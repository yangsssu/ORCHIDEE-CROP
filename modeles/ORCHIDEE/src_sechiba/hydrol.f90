! ===================================================================================================\n
! MODULE        : hydrol
!
! CONTACT       : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE       : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module computes the soil moisture processes on continental points. 
!!
!!\n DESCRIPTION : contains hydrol_main, hydrol_initialize, hydrol_finalise, hydrol_init, 
!!                 hydrol_var_init, hydrol_waterbal, hydrol_alma,
!!                 hydrol_snow, hydrol_vegupd, hydrol_canop, hydrol_flood, hydrol_soil.
!!                 The assumption in this module is that very high vertical resolution is
!!                 needed in order to properly resolve the vertical diffusion of water in
!!                 the soils. Furthermore we have taken into account the sub-grid variability
!!                 of soil properties and vegetation cover by allowing the co-existence of
!!                 different soil moisture columns in the same grid box.
!!                 This routine was originaly developed by Patricia deRosnay.
!!
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S) :
!! - de Rosnay, P., J. Polcher, M. Bruen, and K. Laval, Impact of a physically based soil
!! water flow and soil-plant interaction representation for modeling large-scale land surface
!! processes, J. Geophys. Res, 107 (10.1029), 2002. \n
!! - de Rosnay, P. and Polcher J. (1998) Modeling root water uptake in a complex land surface scheme coupled 
!! to a GCM. Hydrology and Earth System Sciences, 2(2-3):239-256. \n
!! - de Rosnay, P., M. Bruen, and J. Polcher, Sensitivity of surface fluxes to the number of layers in the soil 
!! model used in GCMs, Geophysical research letters, 27 (20), 3329 - 3332, 2000. \n
!! - d’Orgeval, T., J. Polcher, and P. De Rosnay, Sensitivity of the West African hydrological
!! cycle in ORCHIDEE to infiltration processes, Hydrol. Earth Syst. Sci. Discuss, 5, 2251 - 2292, 2008. \n
!! - Carsel, R., and R. Parrish, Developing joint probability distributions of soil water retention
!! characteristics, Water Resources Research, 24 (5), 755 - 769, 1988. \n
!! - Mualem, Y., A new model for predicting the hydraulic conductivity of unsaturated porous
!! media, Water Resources Research, 12 (3), 513 - 522, 1976. \n
!! - Van Genuchten, M., A closed-form equation for predicting the hydraulic conductivity of
!! unsaturated soils, Soil Science Society of America Journal, 44 (5), 892 - 898, 1980. \n
!! - Campoy, A., Ducharne, A., Cheruy, F., Hourdin, F., Polcher, J., and Dupont, J.-C., Response 
!! of land surface fluxes and precipitation to different soil bottom hydrological conditions in a
!! general circulation model,  J. Geophys. Res, in press, 2013. \n
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_sechiba/hydrol.f90 $
!! $Date: 2016-05-11 16:59:14 +0200 (Wed, 11 May 2016) $
!! $Revision: 3432 $
!! \n
! ===============================================================================================\n
MODULE hydrol

  USE ioipsl
  USE xios_orchidee
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE sechiba_io
  USE grid
  USE explicitsnow

!pss:+USE TOPMODEL routines
!  USE extrac_cti
  USE ioipsl_para
  USE init_top
  USE hydro_subgrid
!pss:-


  IMPLICIT NONE

  PRIVATE
  PUBLIC :: hydrol_main, hydrol_initialize, hydrol_finalize, hydrol_clear, hydrol_rotation_update

  !
  ! variables used inside hydrol module : declaration and initialisation
  !
  LOGICAL, SAVE                                   :: first_hydrol_main=.TRUE.  !! Initialisation has to be done one time (true/false)
!$OMP THREADPRIVATE(first_hydrol_main)
  !
  LOGICAL, SAVE                                   :: check_cwrr=.FALSE.      !! To check the water balance (true/false)
!$OMP THREADPRIVATE(check_cwrr)
  LOGICAL, SAVE                                   :: doponds=.FALSE.         !! Reinfiltration flag (true/false)
!$OMP THREADPRIVATE(doponds)

!pss:+
  LOGICAL,SAVE                                    :: TOPM_calcul             !! flag of TOPMODEL usage
!$OMP THREADPRIVATE(TOPM_calcul)
!pss:-

  !
  CHARACTER(LEN=80) , SAVE                        :: var_name                !! To store variables names for I/O
!$OMP THREADPRIVATE(var_name)
  !
  REAL(r_std), PARAMETER                          :: allowed_err =  2.0E-8_r_std
  REAL(r_std), PARAMETER                          :: EPS1 = EPSILON(un)      !! A small number
  ! one dimension array allocated, computed, saved and got in hydrol module
  ! Values per soil type
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: nvan                !! Van Genuchten coeficients n (unitless)
                                                                          ! RK: 1/n=1-m
!$OMP THREADPRIVATE(nvan)                                                 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: avan                !! Van Genuchten coeficients a
                                                                         !!  @tex $(mm^{-1})$ @endtex
!$OMP THREADPRIVATE(avan)                                                
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mcr                 !! Residual volumetric water content
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex
!$OMP THREADPRIVATE(mcr)                                                 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mcs                 !! Saturated volumetric water content
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex
!$OMP THREADPRIVATE(mcs)                                                  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: ks                  !! Hydraulic conductivity at saturation
                                                                         !!  @tex $(mm d^{-1})$ @endtex
!$OMP THREADPRIVATE(ks)                                                  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: pcent               !! Fraction of saturated volumetric soil moisture above 
                                                                         !! which transpir is max (0-1, unitless)
!$OMP THREADPRIVATE(pcent)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mcf                 !! Volumetric water content at field capacity
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex 
!$OMP THREADPRIVATE(mcf)                                                 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mcw                 !! Volumetric water content at wilting point
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex 
!$OMP THREADPRIVATE(mcw)                                                 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mc_awet             !! Vol. wat. cont. above which albedo is cst
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex 
!$OMP THREADPRIVATE(mc_awet)                                             
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mc_adry             !! Vol. wat. cont. below which albedo is cst
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex 
!$OMP THREADPRIVATE(mc_adry)                                             

  ! Values per grid point
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_water_beg    !! Total amount of water at start of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_water_beg)                                       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_water_end    !! Total amount of water at end of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_water_end)                                       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_flux         !! Total water flux @tex $(kg m^{-2})$ @endtex 
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_flux)                                            
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_watveg_beg   !! Total amount of water on vegetation at start of time 
                                                                         !! step @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_watveg_beg)                                      
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_watveg_end   !! Total amount of water on vegetation at end of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_watveg_end)                                      
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_watsoil_beg  !! Total amount of water in the soil at start of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_watsoil_beg)                                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tot_watsoil_end  !! Total amount of water in the soil at end of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tot_watsoil_end)                                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: snow_beg         !! Total amount of snow at start of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(snow_beg)                                            
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: snow_end         !! Total amount of snow at end of time step
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(snow_end)                                           
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: delsoilmoist     !! Change in soil moisture @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(delsoilmoist)                                         
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: delintercept     !! Change in interception storage
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(delintercept)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: delswe           !! Change in SWE @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(delswe)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: swi              !! Integrated Soil Wetness Index with respect to (mcf-mcw)
                                                                         !!  (unitless; can be out of 0-1)
!$OMP THREADPRIVATE(swi)                                        

!pss+
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:) :: mcs_grid              !! Saturation dim kjpindex
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:) :: mcw_grid              !! Wilting point dim kjpindex  
!pss-

  ! array allocated, computed, saved and got in hydrol module
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: mask_veget       !! zero/one when veget fraction is zero/higher (1)
!$OMP THREADPRIVATE(mask_veget)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: mask_soiltile    !! zero/one where soil tile is zero/higher (1)
!$OMP THREADPRIVATE(mask_soiltile)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: humrelv          !! Water stress index for transpiration
                                                                         !! for each soiltile x PFT couple (0-1, unitless)
!$OMP THREADPRIVATE(humrelv)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: vegstressv       !! Water stress index for vegetation growth
                                                                         !! for each soiltile x PFT couple (0-1, unitless)
!$OMP THREADPRIVATE(vegstressv)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:,:):: us               !! Water stress index for transpiration 
                                                                         !! (by soil layer and PFT) (0-1, unitless)
!$OMP THREADPRIVATE(us)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: precisol         !! Throughfall per PFT 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(precisol)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: precisol_ns      !! Throughfall per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(precisol_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ae_ns            !! Bare soil evaporation per soiltile
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(ae_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: evap_bare_lim_ns !! Limitation factor (beta) for bare soil evaporation 
                                                                         !! per soiltile (used to deconvoluate vevapnu)  
                                                                         !!  (0-1, unitless) 
!$OMP THREADPRIVATE(evap_bare_lim_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: free_drain_coef  !! Coefficient for free drainage at bottom
                                                                         !!  (0-1, unitless) 
!$OMP THREADPRIVATE(free_drain_coef)                                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: frac_bare_ns     !! Evaporating bare soil fraction per soiltile
                                                                         !!  (0-1, unitless)
!$OMP THREADPRIVATE(frac_bare_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: rootsink         !! Transpiration sink by soil layer and soiltile
                                                                         !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(rootsink)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: subsnowveg       !! Sublimation of snow on vegetation 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(subsnowveg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: subsnownobio     !! Sublimation of snow on other surface types  
                                                                         !! (ice, lakes,...) @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(subsnownobio)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: snowmelt          !! Quantite de glace fondue
!$OMP THREADPRIVATE(snowmelt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: icemelt          !! Quantite de glace fondue
!$OMP THREADPRIVATE(icemelt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: subsinksoil      !! Excess of sublimation as a sink for the soil
                                                                         !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(subsinksoil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: vegtot           !! Total Total fraction of grid-cell covered by PFTs
                                                                         !! (bare soil + vegetation) (1; 1)
!$OMP THREADPRIVATE(vegtot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: resdist          !! Soiltile values from previous time-step (1; 1)
  
!$OMP THREADPRIVATE(resdist)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: mx_eau_var       !! Maximum water content of the soil @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(mx_eau_var)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: irrig_fin        !! final application of irrigation water
!$OMP THREADPRIVATE(irrig_fin)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: vegstress        !! final application of irrigation water

  ! arrays used by cwrr scheme
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: nroot            !! Normalized root length fraction in each soil layer 
                                                                         !! (0-1, unitless)
                                                                         !! DIM = nvm * nstm * nslm
!$OMP THREADPRIVATE(nroot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: kfact_root       !! Factor to increase Ks towards the surface
                                                                         !! (unitless)
                                                                         !! DIM = kjpindex * nslm * nstm
!$OMP THREADPRIVATE(kfact_root)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: kfact            !! Factor to reduce Ks with depth (unitless)
                                                                         !! DIM = nslm * nscm
!$OMP THREADPRIVATE(kfact)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: zz               !! Depth of the calculation nodes (mm) 
!$OMP THREADPRIVATE(zz)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: dz               !! Internode thickness (mm)
!$OMP THREADPRIVATE(dz)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: dh               !! Layer thickness (mm)
!$OMP THREADPRIVATE(dh)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: mc_lin   !! 50 Vol. Wat. Contents to linearize K and D, for each texture 
                                                                 !!  @tex $(m^{3} m^{-3})$ @endtex
                                                                 !! DIM = imin:imax * nscm
!$OMP THREADPRIVATE(mc_lin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: k_lin    !! 50 values of unsaturated K, for each soil layer and texture
                                                                 !!  @tex $(mm d^{-1})$ @endtex
                                                                 !! DIM = imin:imax * nslm * nscm
!$OMP THREADPRIVATE(k_lin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: d_lin    !! 50 values of diffusivity D, for each soil layer and texture
                                                                 !!  @tex $(mm^2 d^{-1})$ @endtex
                                                                 !! DIM = imin:imax * nslm * nscm
!$OMP THREADPRIVATE(d_lin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: a_lin    !! 50 values of the slope in K=a*mc+b, for each soil layer and texture
                                                                 !!  @tex $(mm d^{-1})$ @endtex
                                                                 !! DIM = imin:imax * nslm * nscm
!$OMP THREADPRIVATE(a_lin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: b_lin    !! 50 values of y-intercept in K=a*mc+b, for each soil layer and texture
                                                                 !!  @tex $(m^{3} m^{-3})$ @endtex
                                                                 !! DIM = imin:imax * nslm * nscm
!$OMP THREADPRIVATE(b_lin)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: humtot   !! Total Soil Moisture @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(humtot)
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION (:)          :: resolv   !! Mask of land points where to solve the diffusion equation
                                                                 !! (true/false)
!$OMP THREADPRIVATE(resolv)

!! linarization coefficients of hydraulic conductivity K (hydrol_soil_coef)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: k        !! Hydraulic conductivity K for each soil layer
                                                                 !!  @tex $(mm d^{-1})$ @endtex
                                                                 !! DIM = (:,nslm)
!$OMP THREADPRIVATE(k)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: kk_moy   !! Mean hydraulic conductivity over soiltiles (mm/d)
!$OMP THREADPRIVATE(kk_moy)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: kk       !! Hydraulic conductivity for each soiltiles (mm/d)
!$OMP THREADPRIVATE(kk)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: a        !! Slope in K=a*mc+b(:,nslm)
                                                                 !!  @tex $(mm d^{-1})$ @endtex
                                                                 !! DIM = (:,nslm)
!$OMP THREADPRIVATE(a)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: b        !! y-intercept in K=a*mc+b
                                                                 !!  @tex $(m^{3} m^{-3})$ @endtex
                                                                 !! DIM = (:,nslm)
!$OMP THREADPRIVATE(b)
!! linarization coefficients of hydraulic diffusivity D (hydrol_soil_coef)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: d        !! Diffusivity D for each soil layer
                                                                 !!  @tex $(mm^2 d^{-1})$ @endtex
                                                                 !! DIM = (:,nslm)
!$OMP THREADPRIVATE(d)
!! matrix coefficients (hydrol_soil_tridiag and hydrol_soil_setup), see De Rosnay (1999), p155-157
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: e        !! Left-hand tridiagonal matrix coefficients
!$OMP THREADPRIVATE(e)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: f        !! Left-hand tridiagonal matrix coefficients
!$OMP THREADPRIVATE(f)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: g1       !! Left-hand tridiagonal matrix coefficients
!$OMP THREADPRIVATE(g1)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ep       !! Right-hand matrix coefficients
!$OMP THREADPRIVATE(ep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: fp       !! Right-hand atrix coefficients
!$OMP THREADPRIVATE(fp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: gp       !! Right-hand atrix coefficients
!$OMP THREADPRIVATE(gp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: rhs      !! Right-hand system
!$OMP THREADPRIVATE(rhs)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: srhs     !! Temporarily stored rhs
!$OMP THREADPRIVATE(srhs)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: tmat             !! Left-hand tridiagonal matrix
!$OMP THREADPRIVATE(tmat)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: stmat            !! Temporarily stored tmat
  !$OMP THREADPRIVATE(stmat)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: water2infilt     !! Water to be infiltrated
                                                                         !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(water2infilt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc              !! Total moisture content per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex 
!$OMP THREADPRIVATE(tmc)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmcr             !! Total moisture constent at residual per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmcr)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmcs             !! Total moisture constent at saturation per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmcs)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter       !! Total moisture in the litter per soiltile
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tmc_litt_mea     !! Total moisture in the litter over the grid
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litt_mea)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter_wilt  !! Total moisture of litter at wilt point per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter_wilt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter_field !! Total moisture of litter at field cap. per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter_field)
!!! A CHANGER DANS TOUT HYDROL: tmc_litter_res et sat ne devraient pas dependre de ji - tdo
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter_res   !! Total moisture of litter at residual moisture per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter_res)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter_sat   !! Total moisture of litter at saturation per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter_sat)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter_awet  !! Total moisture of litter at mc_awet per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter_awet)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tmc_litter_adry  !! Total moisture of litter at mc_adry per soiltile 
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litter_adry)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tmc_litt_wet_mea !! Total moisture in the litter over the grid below which 
                                                                         !! albedo is fixed constant
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litt_wet_mea)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: tmc_litt_dry_mea !! Total moisture in the litter over the grid above which 
                                                                         !! albedo is constant
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(tmc_litt_dry_mea)
  LOGICAL, SAVE                                      :: tmc_init_updated = .FALSE. !! Flag allowing to determine if tmc is initialized.
!$OMP THREADPRIVATE(tmc_init_updated)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: v1               !! Temporary variable (:)
!$OMP THREADPRIVATE(v1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: qflux00          !! Flux at the top of the soil column
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(qflux00)

  !! par type de sol :
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ru_ns            !! Surface runoff per soiltile
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(ru_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: dr_ns            !! Drainage per soiltile
                                                                         !!  @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(dr_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: tr_ns            !! Transpiration per soiltile
!$OMP THREADPRIVATE(tr_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: cvs_over_veg     !! (:,nvm,nstm) old value of corr_veg_soil/veget_max kept 
                                                                         !! from diag to next split 
!$OMP THREADPRIVATE(cvs_over_veg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: corr_veg_soil    !! (:,nvm,nstm) percentage of each veg. type on each soil 
                                                                         !! of each grid point 
!$OMP THREADPRIVATE(corr_veg_soil)
REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: mc                 !! Total volumetric water content at the calculation nodes
                                                                         !! (eg : liquid + frozen)
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex
!$OMP THREADPRIVATE(mc)
   REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: mcl              !! Liquid moisture content
!$OMP THREADPRIVATE(mcl)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: soilmoist        !! (:,nslm) mean of mc across soiltiles
                                                                         !!  @tex $(m^{3} m^{-3})$ @endtex 
!$OMP THREADPRIVATE(soilmoist)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: soil_wet         !! Soil wetness above mcw (0-1, unitless)
!$OMP THREADPRIVATE(soil_wet)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: soil_wet_litter  !! Soil wetness aove mvw in the litter (0-1, unitless)
!$OMP THREADPRIVATE(soil_wet_litter)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: qflux            !! Diffusive water fluxes between soil layers
!$OMP THREADPRIVATE(qflux)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: frac_hydro_diag       !! 
!$OMP THREADPRIVATE(frac_hydro_diag)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: profil_froz_hydro     !! Frozen fraction for each hydrological soil layer
!$OMP THREADPRIVATE(profil_froz_hydro)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: profil_froz_hydro_ns  !! As  profil_froz_hydro per soiltile
!$OMP THREADPRIVATE(profil_froz_hydro_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: temp_hydro            !! Temp profile on hydrological levels
!$OMP THREADPRIVATE(temp_hydro)

!pss:+ TOPMODEL
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: fsat             !! field capacity fraction
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: fwet             !! wetland fraction with WTD = 0 cm
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: fwt1             !! wetland fraction with WTD entre 0 et -3cm
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: fwt2             !! wetland fraction with WTD entre -3cm et -6cm
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: fwt3             !! wetland fraction with WTD entre ... et ...
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: fwt4             !! etc.
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)      :: drunoff        !! runoff de Dunne
!pss:-

!pss:+ TOPMODEL parameter
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZMEAN            !! statistiques de la fonction de distribution des indices topo au sein de chaque maille
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZSTDT
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZSKEW
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZMIN
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZMAX
!!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: NB_PIXE
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZWWILT           !! wilting point
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZWSAT            !! saturation point
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZWFC             !! field capacity
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZD_TOP           !! profondeur de sol pour TOPMODEL 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZM               !! parametre TOPMODEL
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: ZZPAS            !! pas des veceturs d indice topo au sein de chaque maille
! vecteurs calculees par TOPMODEL pour chaque maille (contenu = f(indice seuil); fsat = f(indice seuil); etc.)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ZTAB_FSAT        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ZTAB_WTOP
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ZTAB_FWET
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ZTAB_WTOP_WET
!pss:-


CONTAINS

!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_initialize
!!
!>\BRIEF         Allocate module variables, read from restart file or initialize with default values
!!
!! DESCRIPTION :
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrol_initialize ( kjit,           kjpindex,  index,         rest_id,          &
                                 njsc,           soiltile,  veget,         veget_max,        &
                                 humrel,         vegstress, drysoil_frac,                    &
                                 shumdiag_perma, k_litt,    qsintveg,                        &
                                 evap_bare_lim,  snow,      snow_age,      snow_nobio,       &
!                                 evap_bare_lim,  evap_bare_lim_pft, snow,      snow_age,      snow_nobio,       &
                                 snow_nobio_age, snowrho,   snowtemp,                        &
                                 snowgrain,      snowdz,    snowheat,      fwet_out,         &
                                 totfrac_nobio,  precip_rain, precip_snow, returnflow,       &
                                 reinfiltration, irrigation,tot_melt,      vevapwet,         &
                                 transpir,       vevapnu,   vevapsno,      vevapflo,         &
                                 floodout,       runoff,    drainage,                        &
                                 mc_layh,        mcl_layh,  tmc_layh,                        &
                                 mc_layh_s,      mcl_layh_s, tmc_layh_s)

    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                         :: kjit             !! Time step number 
    INTEGER(i_std), INTENT(in)                         :: kjpindex         !! Domain size
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: index            !! Indeces of the points on the map
    INTEGER(i_std),INTENT (in)                         :: rest_id          !! Restart file identifier
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: njsc             !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std),DIMENSION (kjpindex,nstm), INTENT (in) :: soiltile         !! Fraction of each soil tile (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget            !! Fraction of vegetation type           
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max        !! Max. fraction of vegetation type (LAI -> infty)

    !! 0.2 Output variables
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: humrel         !! Relative humidity
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: vegstress      !! Veg. moisture stress (only for vegetation growth)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: drysoil_frac   !! function of litter wetness
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out)  :: shumdiag_perma !! Percent of porosity filled with water (mc/mcs) used for the thermal computations
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: k_litt         !! litter approximate conductivity
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: qsintveg       !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex), INTENT(out)        :: evap_bare_lim  !! Limitation factor for bare soil evaporation
!    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(out)    :: evap_bare_lim_pft  !! Limitation factor for bare soil evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: snow           !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: snow_age       !! Snow age
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out):: snow_nobio     !! Water balance on ice, lakes, .. [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out):: snow_nobio_age !! Snow age on ice, lakes, ...
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out) :: snowrho        !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out) :: snowtemp       !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out) :: snowgrain      !! Snow grainsize
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out) :: snowdz         !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out) :: snowheat       !! Snow heat content
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (out)  :: mc_layh        !! Volumetric moisture content for each layer in hydrol (liquid+ice) m3/m3
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (out)  :: mcl_layh       !! Volumetric moisture content for each layer in hydrol (liquid) m3/m3
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (out)  :: tmc_layh       !! Total soil moisture content for each layer in hydrol (liquid+ice), mm
    REAL(r_std),DIMENSION (kjpindex,nslm,nstm), INTENT (out)  :: mc_layh_s        !! Volumetric moisture content for each layer in hydrol (liquid+ice) m3/m3
    REAL(r_std),DIMENSION (kjpindex,nslm,nstm), INTENT (out)  :: mcl_layh_s       !! Volumetric moisture content for each layer in hydrol (liquid) m3/m3
    REAL(r_std),DIMENSION (kjpindex,nslm,nstm), INTENT (out)  :: tmc_layh_s       !! Total soil moisture content for each layer in hydrol (liquid+ice), mm
    REAL(r_std),DIMENSION (kjpindex)                     :: soilwetdummy   !! Temporary variable never used


    !! 0.4 Local variables
    LOGICAL(r_std)                                       :: TOPMODEL_CTI
    CHARACTER(LEN=80)                                    :: filename       !! To store file names for I/O
    INTEGER(i_std)                                       :: iml, jml, lml, tml, fid
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:)               :: Zminf, Zmaxf, Zmeanf, Zstdf, Zskewf
    REAL(r_std),ALLOCATABLE,DIMENSION(:)                 :: lon_temp, lat_temp
    REAL(r_std)                                          :: lev(1), pssdate, pssdt
    INTEGER(i_std)                                       :: pssitau(1)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)             :: lat_rel, lon_rel
    INTEGER(r_std)                                       :: ALLOC_ERR
    INTEGER(i_std) :: il, ils, ip, ix, iy, imin, jmin
    REAL(r_std) :: dlon, dlonmin, dlat, dlatmin

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: fwet_out       !! output wetland fraction to change energy or runoff ???!!!
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: totfrac_nobio    !! Total fraction of ice+lakes+...
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: precip_rain      !! Rain precipitation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: precip_snow      !! Snow precipitation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: returnflow       !! Routed water which comes back into the soil (from the 
                                                                             !! bottom) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: reinfiltration   !! Routed water which comes back into the soil (at the 
                                                                             !! top) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: irrigation       !! Water from irrigation returning to soil moisture  
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: tot_melt         !! Total melt 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: vevapwet         !! Interception loss
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: transpir         !! Transpiration
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: vevapnu          !! Bare soil evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: vevapsno         !! Snow evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: vevapflo         !! Floodplain evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)       :: floodout     !! Flux out of floodplains
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)       :: runoff       !! Complete runoff
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)       :: drainage     !! Drainage

!_ ================================================================================================================================

    CALL hydrol_init (kjit, kjpindex, index, rest_id, veget_max, soiltile, &
         humrel, vegstress, snow,       snow_age,   snow_nobio, snow_nobio_age, qsintveg, &
         snowdz, snowgrain, snowrho,    snowtemp,   snowheat, &
         drysoil_frac, evap_bare_lim,  &
!         snowflx,snowcap,   cgrnd_snow, dgrnd_snow, drysoil_frac, evap_bare_lim, evap_bare_lim_pft, &
         fwet_out ) 

    CALL hydrol_var_init (kjpindex, veget, veget_max, &
         soiltile, njsc, mx_eau_var, shumdiag_perma, k_litt, &
         drysoil_frac, qsintveg, mc_layh, mcl_layh, tmc_layh, &
         mc_layh_s, mcl_layh_s, tmc_layh_s) 

!pss+       
       !Config Key   = TOPM_CALCUL
       !Config Desc  = Enable or disable TOPMODEL module
       !Config Def   = False
       !Config If    = OK_SECHIBA
       !Config Help  = Enable or disable TOPMODEL module.
       !Config         
       !Config Units = [-] 
       TOPM_calcul  = .FALSE.
       CALL getin_p('TOPM_CALCUL', TOPM_calcul)
    
       IF (TOPM_calcul) THEN

          TOPMODEL_CTI = .TRUE.
          IF (TOPMODEL_CTI) THEN
            !  Needs to be a configurable variable
            !
            !
            !Config Key   = TOPMODEL_PARAMETERS_FILE
            !Config Desc  = Name of file from which TOPMODEL parameters file are read
            !Config Def   = TOPMODEL_param_1deg.nc
            !Config If    = TOPM_CALCUL and NOT(IMPOSE_VEG)
            !Config Help  = The name of the file to be opened to read the TOPMODEL parameters. 
            !Config         
            !Config Units = [FILE]
            !
            filename = 'TOPMODEL_param_1deg.nc'
            CALL getin_p('TOPMODEL_PARAMETERS_FILE',filename)
            !
            IF (is_root_prc) THEN
               CALL flininfo(filename,iml, jml, lml, tml, fid)
               CALL flinclo(fid)
            ENDIF
            CALL bcast(iml)
            CALL bcast(jml)
            CALL bcast(lml)
            CALL bcast(tml)
            !
            ! soils_param.nc file is 1� soit texture file.
            !
            ALLOC_ERR=-1
            ALLOCATE(lat_rel(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of lat_rel : ",ALLOC_ERR
              STOP 
            ENDIF
            ALLOC_ERR=-1
            ALLOCATE(lon_rel(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of lon_rel : ",ALLOC_ERR
              STOP 
            ENDIF

            ALLOC_ERR=-1
            ALLOCATE(Zminf(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of ZMINf : ",ALLOC_ERR
              STOP 
            ENDIF
            ALLOC_ERR=-1
            ALLOCATE(Zmaxf(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of ZMAXf : ",ALLOC_ERR
              STOP 
            ENDIF
            ALLOC_ERR=-1
            ALLOCATE(Zmeanf(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of ZMEANf : ",ALLOC_ERR
              STOP 
            ENDIF
            ALLOC_ERR=-1
            ALLOCATE(Zstdf(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of ZSTDTf : ",ALLOC_ERR
              STOP 
            ENDIF
            ALLOC_ERR=-1
            ALLOCATE(Zskewf(iml,jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of ZSKEWf : ",ALLOC_ERR
              STOP 
            ENDIF

            ALLOC_ERR=-1
            ALLOCATE (lon_temp(iml),lat_temp(jml), STAT=ALLOC_ERR)
            IF (ALLOC_ERR/=0) THEN
              WRITE(numout,*) "ERROR IN ALLOCATION of lon_temp,lat_temp : ",ALLOC_ERR
              STOP 
            ENDIF
            !
            IF (is_root_prc) CALL flinopen(filename, .FALSE., iml, jml, lml, lon_rel, lat_rel, lev, tml, pssitau, pssdate, pssdt, fid)
            CALL bcast(lon_rel)
            CALL bcast(lat_rel)
            CALL bcast(pssitau)
            CALL bcast(pssdate)
            CALL bcast(pssdt)

            !
            IF (is_root_prc) CALL flinget(fid, 'Zmin', iml, jml, lml, tml, 1, 1, Zminf)
            IF (is_root_prc) CALL flinget(fid, 'Zmax', iml, jml, lml, tml, 1, 1, Zmaxf)
            IF (is_root_prc) CALL flinget(fid, 'Zmean', iml, jml, lml, tml, 1, 1, Zmeanf)
            IF (is_root_prc) CALL flinget(fid, 'Zstdev', iml, jml, lml, tml, 1, 1, Zstdf)
            IF (is_root_prc) CALL flinget(fid, 'Zskew', iml, jml, lml, tml, 1, 1, Zskewf)

            CALL bcast(Zminf)
            CALL bcast(Zmaxf)
            CALL bcast(Zmeanf)
            CALL bcast(Zstdf)
            CALL bcast(Zskewf)
            !
            IF (is_root_prc) CALL flinclo(fid)

        !!!! TOPMODEL parameters 2D into 1D 
               lon_temp(:) = lon_rel(:,1)
               lat_temp(:) = lat_rel(1,:)

               DO ip = 1, kjpindex
                  dlonmin = HUGE(1.)
                  DO ix = 1,iml
                     dlon = MIN( ABS(lalo(ip,2)-lon_temp(ix)), ABS(lalo(ip,2)+360.-lon_temp(ix)), ABS(lalo(ip,2)-360.-lon_temp(ix)) )
                     IF ( dlon .LT. dlonmin ) THEN
                        imin = ix
                        dlonmin = dlon
                     ENDIF
                  ENDDO
                  dlatmin = HUGE(1.)
                  DO iy = 1,jml
                     dlat = ABS(lalo(ip,1)-lat_temp(iy))
                     IF ( dlat .LT. dlatmin ) THEN
                        jmin = iy
                        dlatmin = dlat
                     ENDIF
                  ENDDO
                  ZMIN(ip) = Zminf(imin,jmin)
                  ZMAX(ip) = Zmaxf(imin,jmin)
                  ZMEAN(ip) = Zmeanf(imin,jmin)
                  ZSTDT(ip) = Zstdf(imin,jmin)
                  ZSKEW(ip) = Zskewf(imin,jmin)
               ENDDO

               DEALLOCATE (lon_temp)
               DEALLOCATE (lat_temp)
               DEALLOCATE (Zminf)
               DEALLOCATE (Zmaxf)
               DEALLOCATE (Zmeanf)
               DEALLOCATE (Zstdf)
               DEALLOCATE (Zskewf)
             
             TOPMODEL_CTI = .FALSE.
             write (numout,*) 'STATS CTI OK num1!'
             write (numout,*) 'psstest2'
          ELSE
             write (*,*) 'topmodel data already calculate!'
             write (numout,*) 'psstest3'
          ENDIF
       ELSE
          
          ZMIN(:)=0.
          ZMAX(:)=0.
          ZMEAN(:)=0.
          ZSTDT(:)=0.
          ZSKEW(:)=0.

       ENDIF

        !le deficit utilise pour TOPMODEL va etre calcule par rapport a la saturation
        !ZM(:)=(ZWFC(:)-ZWWILT(:))*ZD_TOP(:)/4.

        !ZM(:) = (mcs du grid_cell - mcw du grid_cell)*zmaxh/4.
        mcs_grid(:) = mcs(1)*soiltile(:,1)+mcs(2)*soiltile(:,2)+mcs(3)*soiltile(:,3)
        mcw_grid(:) = mcw(1)*soiltile(:,1)+mcw(2)*soiltile(:,2)+mcw(3)*soiltile(:,3)
        ZM(:) = ( mcs_grid(:) -  mcw_grid(:) )*zmaxh/4.


       IF(TOPM_calcul) THEN
  
          !2 obtention des differentes fonctions necessaires a TOPMODEL en chaque grid-cell  
          CALL init_top_main(kjpindex, lalo, veget_max, mcw_grid,mcs_grid,zmaxh, ZM,ZMIN, ZMAX, &
               & ZMEAN, ZSTDT, ZSKEW, ZTAB_FSAT, ZTAB_WTOP, ZTAB_FWET, ZTAB_WTOP_WET, ZZPAS)
          
       ELSE

          ZTAB_FSAT=0
          ZTAB_WTOP=0
          ZTAB_FWET=0
          ZTAB_WTOP_WET=0
          ZZPAS=0

       ENDIF

!pss:-
      !! Initialize alma output variables if they were not found in the restart file. This is done in the end of 
      !! hydrol_initialize so that all variables(humtot,..) that will be used are initialized.
      IF (ALL(tot_watveg_beg(:)==val_exp) .OR.  ALL(tot_watsoil_beg(:)==val_exp) .OR. ALL(snow_beg(:)==val_exp)) THEN
            ! The output variable soilwetdummy is not calculated at first call to hydrol_alma.
            CALL hydrol_alma(kjpindex, index, .TRUE., qsintveg, snow, snow_nobio, soilwetdummy)
       END IF

       ! If we check the water balance we first save the total amount of water
       !! X if check_waterbal ==> hydrol_waterbal
       ! init var, just in case is not initialized
       tot_melt(:) = zero
       IF (check_waterbal) THEN
          CALL hydrol_waterbal(kjpindex, index, .TRUE., veget_max, &
               & totfrac_nobio, qsintveg, snow, snow_nobio,&
               & precip_rain, precip_snow, returnflow, reinfiltration, irrigation, tot_melt, &
               & vevapwet, transpir, vevapnu, vevapsno, vevapflo, floodout, runoff,drainage)
       ENDIF
    
  END SUBROUTINE hydrol_initialize


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_main
!!
!>\BRIEF         
!!
!! DESCRIPTION :
!! - called every time step
!! - initialization and finalization part are not done in here
!!
!! - 1 computes snow  ==> hydrol_snow
!! - 2 computes vegetations reservoirs  ==> hydrol_vegupd
!! - 3 computes canopy  ==> hydrol_canop
!! - 4 computes surface reservoir  ==> hydrol_flood
!! - 5 computes soil hydrologie ==> hydrol_soil
!! - X if check_waterbal ==> hydrol_waterbal
!!
!! IMPORTANT NOTICE : The water fluxes are used in their integrated form, over the time step 
!! dt_sechiba, with a unit of kg m^{-2}.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrol_main (kjit, kjpindex, &
       & index, indexveg, indexsoil, indexlayer, indexnbdl, &
       & temp_sol_new, floodout, runoff, drainage, frac_nobio, totfrac_nobio, vevapwet, veget, veget_max, njsc, &
       & qsintmax, qsintveg, vevapnu, vevapnu_pft, vevapsno, vevapflo, snow, snow_age, snow_nobio, snow_nobio_age,  &
       & tot_melt, transpir, precip_rain, precip_snow, returnflow, reinfiltration, irrigation, vegstress_old, transpot, &
       & humrel, vegstress, drysoil_frac, evapot, evapot_penm, evap_bare_lim, flood_frac, flood_res, &
!       & humrel, vegstress, drysoil_frac, evapot, evapot_penm, evap_bare_lim, evap_bare_lim_pft, flood_frac, flood_res, &
       & shumdiag,shumdiag_perma, k_litt, litterhumdiag, soilcap, soiltile, reinf_slope, rest_id, hist_id, hist2_id, soil_deficit, is_crop_soil, &
       & stempdiag, temp_air, pb, u, v, tq_cdrag, pgflux, snowrho,snowtemp, snowgrain,snowdz,snowheat,snowliq,&
       & grndflux,gtemp, tot_bare_soil, soilflxresid, mc_layh, mcl_layh, tmc_layh, &
       & mc_layh_s, mcl_layh_s, tmc_layh_s, drunoff_tot, fwet_out, lambda_snow, cgrnd_snow, dgrnd_snow, temp_sol_add, irrigr, &
       & do_irrig_reservoir, reservoir_dep_max, irrig_dep_threshold, do_awd, awd_dep, lai)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables
  
    INTEGER(i_std), INTENT(in)                         :: kjit             !! Time step number 
    INTEGER(i_std), INTENT(in)                         :: kjpindex         !! Domain size
    INTEGER(i_std),INTENT (in)                         :: rest_id,hist_id  !! _Restart_ file and _history_ file identifier
    INTEGER(i_std),INTENT (in)                         :: hist2_id         !! _history_ file 2 identifier
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: index            !! Indeces of the points on the map
    INTEGER(i_std),DIMENSION (kjpindex*nvm), INTENT (in):: indexveg        !! Indeces of the points on the 3D map for veg
    INTEGER(i_std),DIMENSION (kjpindex*nstm), INTENT (in):: indexsoil      !! Indeces of the points on the 3D map for soil
    INTEGER(i_std),DIMENSION (kjpindex*nslm), INTENT (in):: indexlayer     !! Indeces of the points on the 3D map for soil layers
    INTEGER(i_std),DIMENSION (kjpindex*nslm), INTENT (in):: indexnbdl      !! Indeces of the points on the 3D map for of diagnostic soil layers

    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: precip_rain      !! Rain precipitation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: precip_snow      !! Snow precipitation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: returnflow       !! Routed water which comes back into the soil (from the 
                                                                           !! bottom) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: reinfiltration   !! Routed water which comes back into the soil (at the 
                                                                           !! top) 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: irrigation       !! Water from irrigation returning to soil moisture  
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: vegstress_old    !! vegstress of previous step
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: transpot         !! potential transpiration

    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: temp_sol_new     !! New soil temperature

    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: njsc             !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: frac_nobio     !! Fraction of ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: totfrac_nobio    !! Total fraction of ice+lakes+...
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: soilcap          !! Soil capacity
    REAL(r_std),DIMENSION (kjpindex,nstm), INTENT (in) :: soiltile         !! Fraction of each soil tile (0-1, unitless)
    LOGICAL, DIMENSION (nstm), INTENT (in) :: is_crop_soil                 !! whether soil tile is under cropland
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: vevapwet         !! Interception loss
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget            !! Fraction of vegetation type           
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max        !! Max. fraction of vegetation type (LAI -> infty)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: lai              !! Leaf Area Index
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: qsintmax         !! Maximum water on vegetation for interception
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: transpir         !! Transpiration
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: reinf_slope      !! Slope coef
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: evapot           !! Soil Potential Evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: evapot_penm      !! Soil Potential Evaporation Correction
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: flood_frac       !! flood fraction
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (in) :: stempdiag        !! Diagnostic temp profile from thermosoil
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: temp_air         !! Air temperature
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: u,v              !! Horizontal wind speed
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: tq_cdrag         !! Surface drag coefficient (-)
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: pb               !! Surface pressure
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: pgflux           !! Net energy into snowpack
    REAL(r_std),DIMENSION (kjpindex),INTENT(inout)     :: soilflxresid     !! Energy flux to the snowpack
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: gtemp            !! First soil layer temperature
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: tot_bare_soil    !! Total evaporating bare soil fraction 
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)       :: lambda_snow      !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (in):: cgrnd_snow       !! Integration coefficient for snow numerical scheme
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (in):: dgrnd_snow       !! Integration coefficient for snow numerical scheme
    LOGICAL,DIMENSION (nstm), INTENT (in)              :: do_irrig_reservoir    !! flag of irrigation reservoir for specific soil column
    REAL(r_std),DIMENSION (nstm), INTENT (in)          :: reservoir_dep_max!! maximum depth of irrigation reservoir
    REAL(r_std),DIMENSION (nstm), INTENT (in)          :: irrig_dep_threshold   !! threshold of depth when irrigation occurs
    LOGICAL,DIMENSION (nstm), INTENT (in)              :: do_awd           !! if AWD irrigation
    REAL(r_std),DIMENSION (nstm), INTENT (in)          :: awd_dep          !! water amount (depth) of one irrigation events
    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out) :: vegstress        !! Veg. moisture stress (only for vegetation growth)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: drysoil_frac     !! function of litter wetness
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out):: shumdiag         !! Relative soil moisture in each soil layer 
                                                                           !! with respect to (mcf-mcw)
                                                                           !! (unitless; can be out of 0-1)
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out):: shumdiag_perma   !! Percent of porosity filled with water (mc/mcs) used for the thermal computations 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: k_litt           !! litter approximate conductivity
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: litterhumdiag    !! litter humidity
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: tot_melt         !! Total melt    
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: floodout         !! Flux out of floodplains
    
!pss:+
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)     :: drunoff_tot    !! Dunne runoff 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)     :: fwet_out   !! fwet: to change the balance of energy according to wetland fraction
!pss:-

    !! 0.3 Modified variables

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(inout):: qsintveg         !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex), INTENT(inout)    :: evap_bare_lim    !! Limitation factor (beta) for bare soil evaporation    
!    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(inout) :: evap_bare_lim_pft    !! Limitation factor (beta) for bare soil evaporation    
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(out) :: soil_deficit    !! water deficit to reach IRRIG_FULFILL of holding capacity
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(inout):: humrel           !! Relative humidity
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: vevapnu          !! Bare soil evaporation
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout) :: vevapnu_pft      !! Bare soil evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: vevapsno         !! Snow evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: vevapflo         !! Floodplain evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: flood_res        !! flood reservoir estimate
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: snow             !! Snow mass [kg/m^2]
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)   :: snow_age         !! Snow age
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (inout) :: snow_nobio  !! Water balance on ice, lakes, .. [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (inout) :: snow_nobio_age !! Snow age on ice, lakes, ...
    !! We consider that any water on the ice is snow and we only peforme a water balance to have consistency.
    !! The water balance is limite to + or - 10^6 so that accumulation is not endless

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)         :: runoff       !! Complete surface runoff
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)         :: drainage     !! Drainage
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout) :: snowrho      !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout) :: snowtemp     !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout) :: snowgrain    !! Snow grainsize
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout) :: snowdz       !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout) :: snowheat     !! Snow heat content
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout) :: snowliq      !! Snow liquid content (m)
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)       :: grndflux     !! Net flux into soil W/m2
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT(out)     :: mc_layh      !! Volumetric moisture content for each layer in hydrol(liquid + ice) [m3/m3)]
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT(out)     :: mcl_layh     !! Volumetric moisture content for each layer in hydrol(liquid) [m3/m3]
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT(out)     :: tmc_layh     !! Total soil moisture content for each layer in hydrol(liquid + ice) [mm]
    REAL(r_std),DIMENSION (kjpindex,nslm,nstm), INTENT(out)  :: mc_layh_s      !! Volumetric moisture content for each layer in hydrol(liquid + ice) [m3/m3)]
    REAL(r_std),DIMENSION (kjpindex,nslm,nstm), INTENT(out)  :: mcl_layh_s     !! Volumetric moisture content for each layer in hydrol(liquid) [m3/m3]
    REAL(r_std),DIMENSION (kjpindex,nslm,nstm), INTENT(out)  :: tmc_layh_s     !! Total soil moisture content for each layer in hydrol(liquid + ice) [mm]
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)       :: temp_sol_add !! additional surface temperature due to the melt of first layer
                                                                           !! at the present time-step @tex ($K$) @endtex
    REAL(r_std),DIMENSION (kjpindex,nstm), INTENT (inout)  :: irrigr       !! irrigation reservoir [kg.m-2]
    !REAL(r_std),DIMENSION (kjpindex), INTENT (inout)       :: irrigation_exe   !! executable water from irrigation to soil moisture  


    !! 0.4 Local variables

    INTEGER(i_std)                                     :: jst              !! Index of soil tiles (unitless, 1-3)
    INTEGER(i_std)                                     :: jsl              !! Index of soil layers (unitless)
    INTEGER(i_std)                                     :: ji, jv
    REAL(r_std)                                        :: tempfrac         !! temporary fraction for irrigation
    INTEGER(i_std)                                     :: itopmax          !! Indicating the layer corresponding to 0.1m depth
    REAL(r_std),DIMENSION (kjpindex)                   :: soilwet          !! A temporary diagnostic of soil wetness
    REAL(r_std),DIMENSION (kjpindex)                   :: snowdepth        !! Depth of snow layer
    REAL(r_std),DIMENSION (kjpindex)                   :: njsc_tmp         !! Temporary REAL value for njsc to write it
    REAL(r_std),DIMENSION (kjpindex,nvm)               :: irrig_demand_ratio !! irrigation demand for different PFTs
    REAL(r_std), DIMENSION (kjpindex,nstm)             :: tmc_top          !! Moisture content in the itopmax upper layers, per tile
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm)        :: tmc_pro          !! Moisture content profile, per tile
    REAL(r_std), DIMENSION (kjpindex,nslm)             :: humtot_pro       !! Moisture content profile, for diagnostics
    REAL(r_std), DIMENSION (kjpindex)                  :: humtot_top       !! Moisture content in the itopmax upper layers, for diagnistics
    REAL(r_std), DIMENSION(kjpindex)                   :: histvar          !! Temporary variable when computations are needed
    REAL(r_std), DIMENSION (kjpindex,nvm)              :: frac_bare        !! Fraction(of veget_max) of bare soil in each vegetation type
    INTEGER(i_std), DIMENSION(kjpindex*imax)           :: mc_lin_axis_index
!pss:+
    logical                                           :: filealive, TOPMODEL_CTI
    INTEGER(i_std)                                    :: ind_spe, iet
!pss:-
!pss:+
    CHARACTER(LEN=80) :: filename   !! To store file names for I/O
    INTEGER(i_std) :: il, ils, ip, ix, iy, imin, jmin
    REAL(r_std) :: dlon, dlonmin, dlat, dlatmin
    INTEGER(i_std) :: iml, jml, lml, tml, fid
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:) :: Zminf, Zmaxf, Zmeanf, Zstdf, Zskewf
    REAL(r_std),ALLOCATABLE,DIMENSION(:) :: lon_temp, lat_temp
    REAL(r_std) :: lev(1), pssdate, pssdt
    INTEGER(i_std) :: pssitau(1)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:) :: lat_rel, lon_rel
    INTEGER                  :: ALLOC_ERR
!pss-
    REAL(r_std), DIMENSION(kjpindex)                   :: twbr             !! Grid-cell mean of TWBR Total Water Budget Residu[kg/m2/dt]
    REAL(r_std),DIMENSION (kjpindex,nslm)              :: land_nroot       !! To ouput the grid-cell mean of nroot
    REAL(r_std),DIMENSION (kjpindex,nslm)              :: land_dh          !! To ouput the soil layer thicknes on all grid points [m]
    REAL(r_std),DIMENSION (kjpindex,nslm)              :: land_mcs         !! To ouput the grid-cell mean of mcs
    !REAL(r_std) :: irrig_dosmax_tmp                                        !! temporal case for routing
   

!_ ================================================================================================================================

    !! 3. Shared time step
    IF (printlev>=3) WRITE (numout,*) 'hydrol pas de temps = ',dt_sechiba

!pss:+ 
    !! 3.0 Calculate wetland fractions
        
    IF (TOPM_calcul) THEN
        CALL hydro_subgrid_main(kjpindex, ZTAB_FSAT, ZTAB_WTOP, humtot, profil_froz_hydro, fsat,&
           & ZTAB_FWET,ZTAB_WTOP_WET,fwet, zmaxh, &
           & 1000*(mcs_grid(:)-mcw_grid(:)), fwt1, fwt2, fwt3, fwt4, ZM, ZMIN, ZMAX, ZZPAS, dz)

    ELSE
        fsat(:)=0.0
        fwet(:)=0.0
        fwt1(:)=0.0
        fwt2(:)=0.0
        fwt3(:)=0.0
        fwt4(:)=0.0
    ENDIF

    fwet_out(:)=fwet(:)
!pss:-

    !! Calculate kfact_root
    !! An exponential factor is used to increase ks near the surface depending on the amount of roots in the soil 
    !! through a geometric average over the vegets
    !! This comes from the PhD thesis of d'Orgeval, 2006, p82; d'Orgeval et al. 2008, Eqs. 3-4
    !! (Calibrated against Hapex-Sahel measurements)
    !! Since rev 2916: veget_max/2 is used instead of veget
    kfact_root(:,:,:) = un
    DO jsl = 1, nslm
       DO jv = 2, nvm
          jst = pref_soil_veg(jv)
          DO ji = 1, kjpindex
             IF(soiltile(ji,jst) .GT. min_sechiba ) THEN
                kfact_root(ji,jsl,jst) = kfact_root(ji,jsl,jst) * &
                     & MAX((MAXVAL(ks_usda)/ks(njsc(ji)))**(- veget_max(ji,jv)/2 * (humcste(jv)*zz(jsl)/mille - un)/deux), &
                     un) 

             ENDIF
          ENDDO
       ENDDO
    ENDDO

    ! 
    !! 3.1 Calculate snow processes with explicit method or bucket snow model
    IF (ok_explicitsnow) THEN
       ! Explicit snow model
       IF (printlev>=3) WRITE (numout,*) ' ok_explicitsnow : use multi-snow layer '
       
       CALL explicitsnow_main(kjpindex,    precip_rain,   precip_snow,    temp_air,   pb,       &
                              u,           v,             temp_sol_new,   soilcap,    pgflux,   &
                              frac_nobio,  totfrac_nobio, gtemp,                                &
                              lambda_snow, cgrnd_snow,    dgrnd_snow,                           & 
                              vevapsno,    snow_age,      snow_nobio_age, snow_nobio, snowrho,  &
                              snowgrain,   snowdz,        snowtemp,       snowheat,   snow,     &
                              temp_sol_add,                                                     &
                              snowliq,     subsnownobio,  grndflux,       snowmelt,   tot_melt, &
                              soilflxresid,subsinksoil)
 
    ELSE
       ! Bucket snow model
       CALL hydrol_snow(kjpindex, precip_rain, precip_snow, temp_sol_new, soilcap, &
            frac_nobio, totfrac_nobio, vevapsno, snow, snow_age, snow_nobio, snow_nobio_age, &
            tot_melt, snowdepth,snowmelt)
    END IF
        
    !
    !! 3.2 computes vegetations reservoirs  ==>hydrol_vegupd
    CALL hydrol_vegupd(kjpindex, veget, veget_max, soiltile, qsintveg, resdist, frac_bare)

    !
    !! 3.3 computes canopy  ==>hydrol_canop
    CALL hydrol_canop(kjpindex, precip_rain, vevapwet, veget_max, veget, qsintmax, qsintveg,precisol,tot_melt)

    !
    !! 3.4 computes surface reservoir  ==>hydrol_flood
    CALL hydrol_flood(kjpindex,  vevapflo, flood_frac, flood_res, floodout)

    !
    !! 3.5 computes soil hydrologie ==>hydrol_soil
    !!! calculating ratio of irrigation for each pft at each point
    irrig_demand_ratio(:,:) = zero
!    irrig_totfrac(:) = zero
    DO ji = 1,kjpindex
        DO jv = 2,nvm
            !IF (ok_LAIdev(jv) .AND. (veget_max(ji,jv) .GT. zero)) THEN
            IF (ok_LAIdev(jv) .AND. (lai(ji,jv) .GT. 0)) THEN
                IF (irrig_drip) THEN
                    tempfrac = veget(ji,jv)/veget_max(ji,jv)
                    !Zun: change the condition to if (transpot - precip -
                    !reinfiltration) > 0
                    IF (vegstress_old(ji,jv) .LT. irrig_threshold(jv)) THEN
                    !IF ((vegstress_old(ji,jv) .LT. irrig_threshold(jv)) .AND.  &
                    !    & (transpot(ji,jv)*tempfrac + evapot(ji)*(1-tempfrac) .GT. precip_rain(ji)) ) THEN
        !                irrig_totfrac(ji) = irrig_totfrac(ji) + veget_max(ji,jv)
                        irrig_demand_ratio(ji,jv) = MIN(irrig_dosmax, &
                                                    & MAX(zero, transpot(ji,jv)*tempfrac &
                                                    & - precip_rain(ji) - reinfiltration(ji) &
                                                    & )) * veget_max(ji,jv)
                        !!!! reconsider if evapot or evapot_corr to be used as irrigation demand
                        !!!! if re-infiltration is considered in sechiba, it# {RIVER_ROUTING}
                        !should also be considered here, xuhui
                    ENDIF ! since irrigated ratio is the same across pfts on the same grid, no need to consider
                ELSEIF (do_irrig_reservoir(pref_soil_veg(jv))) THEN ! flooding with reservoir
                    IF ( do_awd(pref_soil_veg(jv))) THEN 
                    ! do AWD irrigation
                        IF ( vegstress_old(ji,jv) .LT. irrig_threshold(jv)) THEN
                            !if this condition meets, do irrigation, otherwise
                            !no irrigation
                            IF ( awd_dep(pref_soil_veg(jv)) .LT. zero) THEN
                            ! use default deficit to define irrigation
                            ! amount    
                                irrig_demand_ratio(ji,jv) = MIN( irrig_dosmax, MAX( zero, soil_deficit(ji,jv) ) ) * veget_max(ji,jv)
                            ELSE
                            ! use defined depth for irrigation water amount
                                irrig_demand_ratio(ji,jv) =  awd_dep(pref_soil_veg(jv)) * veget_max(ji,jv)
                            ENDIF
                        ENDIF
                    ELSEIF ((irrigr(ji,pref_soil_veg(jv)) .LT. irrig_dep_threshold(pref_soil_veg(jv)))) THEN 
                        irrig_demand_ratio(ji,jv) =  (irrig_dep_threshold(pref_soil_veg(jv)) - irrigr(ji,pref_soil_veg(jv))) * veget_max(ji,jv)
                    ENDIF
                ELSEIF ((vegstress_old(ji,jv) .LT. irrig_threshold(jv))) THEN 
                    !flooding without reservoir
                        irrig_demand_ratio(ji,jv) = MIN( irrig_dosmax, MAX( zero, soil_deficit(ji,jv) ) ) * veget_max(ji,jv)
                        !irrig_demand_ratio(ji,jv) = MIN( irrig_threshold(jv) - vegstress_old(ji,jv), MAX( zero, soil_deficit(ji,jv) ) ) * veget_max(ji,jv)
                    !!!! if re-infiltration is considered in sechiba, it
                    !should also be considered here, xuhui
                ENDIF
            ENDIF
        ENDDO

        IF ( SUM(irrig_demand_ratio(ji,:)) .GT. zero ) THEN
            irrig_demand_ratio(ji,:) = irrig_demand_ratio(ji,:) / SUM(irrig_demand_ratio(ji,:))
            
        ENDIF
    ENDDO
!    irrig_fin(:,:) = zero

    CALL hydrol_soil(kjpindex, veget_max, soiltile, njsc, reinf_slope,  &
         transpir, vevapnu, vevapnu_pft, evapot, evapot_penm, runoff, &
         drainage, returnflow, reinfiltration, irrigation, irrig_demand_ratio, &
         tot_melt,evap_bare_lim, shumdiag, shumdiag_perma, &
!         tot_melt,evap_bare_lim, evap_bare_lim_pft, shumdiag, shumdiag_perma, &
         k_litt, litterhumdiag, humrel, vegstress, drysoil_frac, irrig_fin, &
         is_crop_soil, stempdiag,snow,snowdz, tot_bare_soil, u, v, tq_cdrag, &
         mc_layh, mcl_layh, tmc_layh, &
         mc_layh_s, mcl_layh_s, tmc_layh_s, drunoff_tot, fsat, veget, irrigr, &
         do_irrig_reservoir, reservoir_dep_max, irrig_dep_threshold, do_awd, &
         awd_dep,lai)
         !awd_dep, irrig_dosmax_tmp, irrigation_exe)
         !awd_dep, irrigation_exe)

    !WRITE(numout,*)'Zun mc_2:',mc(1,:,6)
    DO ji = 1,kjpindex
        DO jv = 2,nvm
           IF (.NOT. natural(jv)) THEN
               IF (.NOT. is_crop_soil(pref_soil_veg(jv))) THEN
                   STOP 'hydrol irrig soil column'
               ENDIF
               !! soil_deficit(ji,jv) = MAX( zero, irrig_fulfill(jv)*tmcs(ji,4) - tmc(ji,4) ) !mm
               ! note that since crop soil may not necessarily be the fourth
               ! soil colum, this needs to be changed !xuhui 151214
               soil_deficit(ji,jv) = MAX( zero, irrig_fulfill(jv)*tmcs(ji,pref_soil_veg(jv)) - tmc(ji,pref_soil_veg(jv)) ) !mm
               !soil_deficit(ji,jv) = MAX(zero, irrig_fulfill(jv)*pcent(njsc(ji))*(tmcs(ji,pref_soil_veg(jv)) - &
               !& mcw(njsc(ji))*2000) + mcw(njsc(ji))*2000 - tmc(ji,pref_soil_veg(jv)) ) !mm
           ENDIF
        ENDDO
    ENDDO


    ! If we check the water balance we end with the comparison of total water change and fluxes
    !! X if check_waterbal ==> hydrol_waterbal
    IF (check_waterbal) THEN
       CALL hydrol_waterbal(kjpindex, index, .FALSE., veget_max, totfrac_nobio, &
            & qsintveg, snow,snow_nobio, precip_rain, precip_snow, returnflow, reinfiltration, &
            & irrigation, tot_melt, vevapwet, transpir, vevapnu, vevapsno, vevapflo, floodout, runoff, drainage)
    ENDIF

    !! 4 write out file  ==> hydrol_alma/histwrite(*)
    !
    ! If we use the ALMA standards
    CALL hydrol_alma(kjpindex, index, .FALSE., qsintveg, snow, snow_nobio, soilwet)
    

    ! Calcuate the moisture in the upper itopmax layers (humtot_top): 
    ! For ORCHIDEE with nslm=11 and zmaxh=2 this means the upper 10 cm. 
    ! We compute tmc_top as tmc but only for the first itopmax layers. Then we compute a humtot with this variable.
    ! Note: itopmax should depend on the vertical discretization, to be done. 
    itopmax=6
    DO jst=1,nstm
       DO ji=1,kjpindex
          tmc_top(ji,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
          DO jsl = 2, itopmax
             tmc_top(ji,jst) = tmc_top(ji,jst) + dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                  + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
          ENDDO
       ENDDO
    ENDDO
 
    DO jst=1,nstm
       DO ji=1,kjpindex
          tmc_pro(ji,1,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
          DO jsl = 2,nslm-1
             tmc_pro(ji,jsl,jst) =  dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                  + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
          ENDDO
       ENDDO
    ENDDO
    humtot_top(:) = zero
    DO jst=1,nstm
       DO ji=1,kjpindex
          humtot_top(ji) = humtot_top(ji) + soiltile(ji,jst) * tmc_top(ji,jst)
       ENDDO
    ENDDO

    humtot_pro(:,:) = zero
    DO jst=1,nstm
       DO ji=1,kjpindex
           DO jsl=1,nslm-1
             humtot_pro(ji,jsl) = humtot_pro(ji,jsl) + soiltile(ji,jst) * tmc_pro(ji,jsl,jst)
           ENDDO
       ENDDO
    ENDDO

    ! Calculcate the Total Water Budget Residu (in kg/m2 over dt_sechiba)
    ! Consistent with hydrol_waterbal for the use of vegtot
    ! snow_nobio included in delswe
    ! Normally, correct equation if vegtot=1, else...?
    ! Does not include the routing reservoirs, although the flux to/from routing are integrated
    DO ji=1,kjpindex
       twbr(ji) = (vegtot(ji)*delsoilmoist(ji) + delintercept(ji) + delswe(ji)) &
            - ( precip_rain(ji) + precip_snow(ji) + irrigation(ji) + floodout(ji) &
            + returnflow(ji) + reinfiltration(ji) ) &
            + ( runoff(ji) + drainage(ji) + SUM(vevapwet(ji,:)) &
            + SUM(transpir(ji,:)) + vevapnu(ji) + vevapsno(ji) + vevapflo(ji) ) 
    ENDDO
    ! Transform unit from kg/m2/dt to kg/m2/s (or mm/s)
    CALL xios_orchidee_send_field("twbr",twbr/dt_sechiba)
    
    
    ! Calculate land_nroot : grid-cell mean of nroot 
    ! Here use only nroot(jv,1,jsl) with jst=1 as nroot is the same for all soiltile
    ! Do not treat PFT1 because it has no roots
    land_nroot(:,:) = zero
    DO jsl=1,nslm
       DO jv=2,nvm
          DO ji=1,kjpindex
               IF ( vegtot(ji) > min_sechiba ) THEN 
               land_nroot(ji,jsl) = land_nroot(ji,jsl) + veget_max(ji,jv) * nroot(jv,1,jsl) / vegtot(ji) 
            END IF
          END DO
       ENDDO
    ENDDO
    CALL xios_orchidee_send_field("RootDist",land_nroot)   

    DO jsl=1,nslm
       land_dh(:,jsl)=dh(jsl)/mille
    ENDDO
    CALL xios_orchidee_send_field("SoilThick",land_dh)

    land_mcs(:,:) = zero
    DO jsl=1,nslm
       DO jst=1,nstm
          DO ji=1,kjpindex
             land_mcs(ji,jsl) = land_mcs(ji,jsl) + soiltile(ji,jst) * tmcs(ji,jst)
          ENDDO
       ENDDO
    ENDDO
    CALL xios_orchidee_send_field("SoilSat",land_mcs/(zmaxh* mille)) ! in m3/m3
    CALL xios_orchidee_send_field("water2infilt",water2infilt)   

    DO jst = 1, nstm
       ! var_name= "mc_1" ... "mc_3"
       WRITE (var_name,"('moistc_',i1)") jst
       CALL xios_orchidee_send_field(TRIM(var_name),mc(:,:,jst))

       ! var_name= "kfactroot_1" ... "kfactroot_3"
       WRITE (var_name,"('kfactroot_',i1)") jst
       CALL xios_orchidee_send_field(TRIM(var_name),kfact_root(:,:,jst))

       ! var_name= "vegetsoil_1" ... "vegetsoil_3"
       WRITE (var_name,"('vegetsoil_',i1)") jst
       CALL xios_orchidee_send_field(TRIM(var_name),corr_veg_soil(:,:,jst))
    END DO

    CALL xios_orchidee_send_field("evapnu_soil",ae_ns*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("drainage_soil",dr_ns*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("transpir_soil",tr_ns*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("runoff_soil",ru_ns*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("water2infilt",water2infilt*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("humtot_soil",tmc)
    CALL xios_orchidee_send_field("humtot",humtot)
    CALL xios_orchidee_send_field("humtot_pro",humtot_pro)
    CALL xios_orchidee_send_field("mrso",humtot)
    CALL xios_orchidee_send_field("mrsos",humtot_top)
    njsc_tmp(:)=njsc(:)
    CALL xios_orchidee_send_field("soilindex",njsc_tmp)
    CALL xios_orchidee_send_field("humrel",humrel)     
    CALL xios_orchidee_send_field("drainage",drainage*one_day/dt_sechiba) ! [kg m-2 d-1]
    CALL xios_orchidee_send_field("runoff",runoff*one_day/dt_sechiba) ! [kg m-2 d-1]
    CALL xios_orchidee_send_field("mrros",runoff/dt_sechiba) ! [kg m-2 s-1]
    CALL xios_orchidee_send_field("mrro",(runoff+drainage)/dt_sechiba) ! [kg m-2 s-1]
    CALL xios_orchidee_send_field("precisol",precisol*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("rain",precip_rain*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("rain_alma",precip_rain/dt_sechiba)
    CALL xios_orchidee_send_field("irrig_fin",irrig_fin*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("vegstress",vegstress)
    CALL xios_orchidee_send_field("snowf",precip_snow*one_day/dt_sechiba) ! [mm/d]
    CALL xios_orchidee_send_field("snowf_alma",precip_snow/dt_sechiba)    ! [mm/s]
    CALL xios_orchidee_send_field("qsintmax",qsintmax)
    CALL xios_orchidee_send_field("qsintveg",qsintveg)
    CALL xios_orchidee_send_field("CanopInt",SUM(qsintveg(:,:),dim=2))
    CALL xios_orchidee_send_field("SWI",swi)

    histvar(:)=(precip_rain(:)-SUM(precisol(:,:),dim=2))
    CALL xios_orchidee_send_field("prveg",histvar/dt_sechiba)

    IF ( do_floodplains ) THEN
       CALL xios_orchidee_send_field("floodout",floodout*one_day/dt_sechiba)
    END IF

    IF (check_waterbal) THEN
       CALL xios_orchidee_send_field("TotWater",tot_water_end)
       CALL xios_orchidee_send_field("TotWaterFlux",tot_flux*one_day/dt_sechiba)
    END IF

    CALL xios_orchidee_send_field("Qs",runoff/dt_sechiba)
    CALL xios_orchidee_send_field("Qsb",drainage/dt_sechiba)
    CALL xios_orchidee_send_field("Qsm",snowmelt/dt_sechiba)
    CALL xios_orchidee_send_field("SoilMoist",soilmoist)

! Note that vevapsno has been changed compared to enerbil with respect to subsinksoil/cf vevapnu
    CALL xios_orchidee_send_field("SubSnow",vevapsno/dt_sechiba)
    CALL xios_orchidee_send_field("SnowDepth",snowdepth)
    CALL xios_orchidee_send_field("frac_bare",frac_bare)
    CALL xios_orchidee_send_field("frac_bare_ns",frac_bare_ns)

    CALL xios_orchidee_send_field("SoilWet",soilwet)
    CALL xios_orchidee_send_field("RootMoist",tot_watsoil_end)
    CALL xios_orchidee_send_field("DelSoilMoist",delsoilmoist)
    CALL xios_orchidee_send_field("DelSWE",delswe)
    CALL xios_orchidee_send_field("DelIntercept",delintercept)  
    

    IF ( .NOT. almaoutput ) THEN
       CALL histwrite_p(hist_id, 'frac_bare', kjit, frac_bare, kjpindex*nvm, indexveg)

       DO jst=1,nstm
          ! var_name= "mc_1" ... "mc_3"
          WRITE (var_name,"('moistc_',i1)") jst
          CALL histwrite_p(hist_id, TRIM(var_name), kjit,mc(:,:,jst), kjpindex*nslm, indexlayer)

          ! var_name= "kfactroot_1" ... "kfactroot_3"
          WRITE (var_name,"('kfactroot_',i1)") jst
          CALL histwrite_p(hist_id, TRIM(var_name), kjit, kfact_root(:,:,jst), kjpindex*nslm, indexlayer)

          ! var_name= "vegetsoil_1" ... "vegetsoil_3"
          WRITE (var_name,"('vegetsoil_',i1)") jst
          CALL histwrite_p(hist_id, TRIM(var_name), kjit,corr_veg_soil(:,:,jst), kjpindex*nvm, indexveg)
       ENDDO
       CALL histwrite_p(hist_id, 'precip_soil', kjit, precisol_ns, kjpindex*nstm, indexsoil)
       CALL histwrite_p(hist_id, 'evapnu_soil', kjit, ae_ns, kjpindex*nstm, indexsoil)
       CALL histwrite_p(hist_id, 'drainage_soil', kjit, dr_ns, kjpindex*nstm, indexsoil)
       CALL histwrite_p(hist_id, 'transpir_soil', kjit, tr_ns, kjpindex*nstm, indexsoil)
       CALL histwrite_p(hist_id, 'runoff_soil', kjit, ru_ns, kjpindex*nstm, indexsoil)
       CALL histwrite_p(hist_id, 'water2infilt', kjit, water2infilt, kjpindex*nstm, indexsoil)
       CALL histwrite_p(hist_id, 'humtot_soil', kjit, tmc, kjpindex*nstm, indexsoil)
       ! mrso is a perfect duplicate of humtot
       CALL histwrite_p(hist_id, 'humtot', kjit, humtot, kjpindex, index)
       CALL histwrite_p(hist_id, 'mrso', kjit, humtot, kjpindex, index)
       CALL histwrite_p(hist_id, 'mrsos', kjit, humtot_top, kjpindex, index)
       CALL histwrite_p(hist_id, 'humtot_pro', kjit, humtot_pro, kjpindex*nslm, indexlayer)
       njsc_tmp(:)=njsc(:)
       CALL histwrite_p(hist_id, 'soilindex', kjit, njsc_tmp, kjpindex, index)
       CALL histwrite_p(hist_id, 'humrel',   kjit, humrel,   kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'vegstress',   kjit, vegstress, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'soil_deficit', kjit, soil_deficit, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'drainage', kjit, drainage, kjpindex, index)
       ! NB! According to histdef in intersurf, the variables 'runoff' and 'mrros' have different units
       CALL histwrite_p(hist_id, 'runoff', kjit, runoff, kjpindex, index)
       CALL histwrite_p(hist_id, 'mrros', kjit, runoff, kjpindex, index)
       histvar(:)=(runoff(:)+drainage(:))
       CALL histwrite_p(hist_id, 'mrro', kjit, histvar, kjpindex, index)
       CALL histwrite_p(hist_id, 'precisol', kjit, precisol, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'rain', kjit, precip_rain, kjpindex, index)

       histvar(:)=(precip_rain(:)-SUM(precisol(:,:),dim=2))
       CALL histwrite_p(hist_id, 'prveg', kjit, histvar, kjpindex, index)

       CALL histwrite_p(hist_id, 'snowf', kjit, precip_snow, kjpindex, index)
       CALL histwrite_p(hist_id, 'qsintmax', kjit, qsintmax, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'qsintveg', kjit, qsintveg, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'irrig_fin', kjit, irrig_fin, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'SWI', kjit, swi, kjpindex, index)
       CALL histwrite_p(hist_id, 'snowmelt',kjit,snowmelt,kjpindex,index)
       CALL histwrite_p(hist_id, 'shumdiag_perma',kjit,shumdiag_perma,kjpindex*nbdl,indexnbdl)

!pss:+ ! write out wetland fraction and CTI parameters
       CALL histwrite_p(hist_id, 'fsat', kjit, fsat, kjpindex, index)
       CALL histwrite_p(hist_id, 'fwet', kjit, fwet, kjpindex, index)
       CALL histwrite_p(hist_id, 'fwt1', kjit, fwt1, kjpindex, index)
       CALL histwrite_p(hist_id, 'fwt2', kjit, fwt2, kjpindex, index)
       CALL histwrite_p(hist_id, 'fwt3', kjit, fwt3, kjpindex, index)
       CALL histwrite_p(hist_id, 'fwt4', kjit, fwt4, kjpindex, index)
       CALL histwrite_p(hist_id, 'ZMIN', kjit, ZMIN, kjpindex, index)
       CALL histwrite_p(hist_id, 'ZMAX', kjit, ZMAX, kjpindex, index)
       CALL histwrite_p(hist_id, 'ZMEAN', kjit, ZMEAN, kjpindex, index)
       !CALL histwrite_p(hist_id, 'NB_PIXE', kjit, NB_PIXE, kjpindex, index)
       CALL histwrite_p(hist_id, 'ZSTDT', kjit, ZSTDT, kjpindex, index)
       CALL histwrite_p(hist_id, 'ZSKEW', kjit, ZSKEW, kjpindex, index)
!       CALL histwrite_p(hist_id, 'dsg', kjit, dsg, kjpindex*nvm, indexveg)
!       CALL histwrite_p(hist_id, 'dsp', kjit, dsp, kjpindex*nvm, indexveg)
!       CALL histwrite_p(hist_id, 'ZWSAT', kjit, ZWSAT, kjpindex, index)
!       CALL histwrite_p(hist_id, 'ZWWILT', kjit, ZWWILT, kjpindex, index) 
!       CALL histwrite_p(hist_id, 'ZWFC', kjit, ZWFC, kjpindex, index) 
!       CALL histwrite_p(hist_id, 'RU', kjit, ruu_ch, kjpindex, index) 
!       CALL histwrite_p(hist_id, 'mx_eau_var', kjit, mx_eau_var, kjpindex, index)
       CALL histwrite_p(hist_id, 'drunoff_tot', kjit, drunoff_tot, kjpindex, index)
!pss:-

       IF ( river_routing .AND. do_floodplains ) THEN
          CALL histwrite_p(hist_id, 'floodout', kjit, floodout, kjpindex, index)
       ENDIF
       !
       IF ( hist2_id > 0 ) THEN
          DO jst=1,nstm
             ! var_name= "mc_1" ... "mc_3"
             WRITE (var_name,"('moistc_',i1)") jst
             CALL histwrite_p(hist2_id, TRIM(var_name), kjit,mc(:,:,jst), kjpindex*nslm, indexlayer)

             ! var_name= "kfactroot_1" ... "kfactroot_3"
             WRITE (var_name,"('kfactroot_',i1)") jst
             CALL histwrite_p(hist2_id, TRIM(var_name), kjit, kfact_root(:,:,jst), kjpindex*nslm, indexlayer)

             ! var_name= "vegetsoil_1" ... "vegetsoil_3"
             WRITE (var_name,"('vegetsoil_',i1)") jst
             CALL histwrite_p(hist2_id, TRIM(var_name), kjit,corr_veg_soil(:,:,jst), kjpindex*nvm, indexveg)
          ENDDO
          CALL histwrite_p(hist2_id, 'evapnu_soil', kjit, ae_ns, kjpindex*nstm, indexsoil)
          CALL histwrite_p(hist2_id, 'drainage_soil', kjit, dr_ns, kjpindex*nstm, indexsoil)
          CALL histwrite_p(hist2_id, 'transpir_soil', kjit, tr_ns, kjpindex*nstm, indexsoil)
          CALL histwrite_p(hist2_id, 'runoff_soil', kjit, ru_ns, kjpindex*nstm, indexsoil)
          CALL histwrite_p(hist2_id, 'water2infilt', kjit, water2infilt, kjpindex*nstm, indexsoil)
          CALL histwrite_p(hist2_id, 'humtot_soil', kjit, tmc, kjpindex*nstm, indexsoil)
          ! mrso is a perfect duplicate of humtot
          CALL histwrite_p(hist2_id, 'humtot', kjit, humtot, kjpindex, index)
          CALL histwrite_p(hist2_id, 'mrso', kjit, humtot, kjpindex, index)
          CALL histwrite_p(hist2_id, 'mrsos', kjit, humtot_top, kjpindex, index)
          CALL histwrite_p(hist2_id, 'humtot_pro', kjit, humtot_pro, kjpindex*nslm, indexlayer)
          njsc_tmp(:)=njsc(:)
          CALL histwrite_p(hist2_id, 'soilindex', kjit, njsc_tmp, kjpindex, index)
          CALL histwrite_p(hist2_id, 'humrel',   kjit, humrel,   kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'drainage', kjit, drainage, kjpindex, index)
          ! NB! According to histdef in intersurf, the variables 'runoff' and 'mrros' have different units
          CALL histwrite_p(hist2_id, 'runoff', kjit, runoff, kjpindex, index)
          CALL histwrite_p(hist2_id, 'mrros', kjit, runoff, kjpindex, index)
          histvar(:)=(runoff(:)+drainage(:))
          CALL histwrite_p(hist2_id, 'mrro', kjit, histvar, kjpindex, index)

          IF ( river_routing .AND. do_floodplains ) THEN
             CALL histwrite_p(hist2_id, 'floodout', kjit, floodout, kjpindex, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'precisol', kjit, precisol, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'rain', kjit, precip_rain, kjpindex, index)
          CALL histwrite_p(hist2_id, 'snowf', kjit, precip_snow, kjpindex, index)
          CALL histwrite_p(hist2_id, 'snowmelt',kjit,snowmelt,kjpindex,index)
          CALL histwrite_p(hist2_id, 'qsintmax', kjit, qsintmax, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'qsintveg', kjit, qsintveg, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'irrig_fin', kjit, irrig_fin, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'vegstress', kjit, vegstress, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'SWI', kjit, swi, kjpindex, index) 
          !
          IF (check_waterbal) THEN
             CALL histwrite_p(hist2_id, 'TotWater', kjit, tot_water_end, kjpindex, index)
             CALL histwrite_p(hist2_id, 'TotWaterFlux', kjit, tot_flux, kjpindex, index)
          ENDIF
       ENDIF
    ELSE
       CALL histwrite_p(hist_id, 'Snowf', kjit, precip_snow, kjpindex, index)
       CALL histwrite_p(hist_id, 'Rainf', kjit, precip_rain, kjpindex, index)
       CALL histwrite_p(hist_id, 'Qs', kjit, runoff, kjpindex, index)
       CALL histwrite_p(hist_id, 'Qsb', kjit, drainage, kjpindex, index)
       CALL histwrite_p(hist_id, 'Qsm', kjit, snowmelt, kjpindex, index)
       CALL histwrite_p(hist_id, 'DelSoilMoist', kjit, delsoilmoist, kjpindex, index)
       CALL histwrite_p(hist_id, 'DelSWE', kjit, delswe, kjpindex, index)
       CALL histwrite_p(hist_id, 'DelIntercept', kjit, delintercept, kjpindex, index)
       !
       CALL histwrite_p(hist_id, 'SoilMoist', kjit, soilmoist, kjpindex*nslm, indexlayer)
       CALL histwrite_p(hist_id, 'SoilWet', kjit, soilwet, kjpindex, index)
       !
       CALL histwrite_p(hist_id, 'RootMoist', kjit, tot_watsoil_end, kjpindex, index)
       CALL histwrite_p(hist_id, 'SubSnow', kjit, vevapsno, kjpindex, index)
       !
       CALL histwrite_p(hist_id, 'SnowDepth', kjit, snowdepth, kjpindex, index)
       !
       IF ( hist2_id > 0 ) THEN
          CALL histwrite_p(hist2_id, 'Snowf', kjit, precip_snow, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Rainf', kjit, precip_rain, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Qs', kjit, runoff, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Qsb', kjit, drainage, kjpindex, index)
          CALL histwrite_p(hist2_id, 'Qsm', kjit, snowmelt, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelSoilMoist', kjit, delsoilmoist, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelSWE', kjit, delswe, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelIntercept', kjit, delintercept, kjpindex, index)
          !
          CALL histwrite_p(hist2_id, 'SoilMoist', kjit, soilmoist, kjpindex*nslm, indexlayer)
          CALL histwrite_p(hist2_id, 'SoilWet', kjit, soilwet, kjpindex, index)
          !
          CALL histwrite_p(hist2_id, 'RootMoist', kjit, tot_watsoil_end, kjpindex, index)
          CALL histwrite_p(hist2_id, 'SubSnow', kjit, vevapsno, kjpindex, index)
          !
          CALL histwrite_p(hist2_id, 'SnowDepth', kjit, snowdepth, kjpindex, index)
       ENDIF
    ENDIF

    IF (ok_freeze_cwrr) THEN
       CALL histwrite_p(hist_id, 'profil_froz_hydro', kjit,profil_froz_hydro , kjpindex*nslm, indexlayer)
       DO jst=1,nstm
          WRITE (var_name,"('profil_froz_hydro_',i1)") jst
          CALL histwrite_p(hist_id, TRIM(var_name), kjit, profil_froz_hydro_ns(:,:,jst), kjpindex*nslm, indexlayer)
       ENDDO
       CALL histwrite_p(hist_id, 'temp_hydro', kjit,temp_hydro , kjpindex*nslm, indexlayer)
       CALL histwrite_p(hist_id, 'kk_moy', kjit, kk_moy,kjpindex*nslm, indexlayer) ! averaged over soiltiles
    ENDIF

    IF (first_hydrol_main) THEN 
       first_hydrol_main=.FALSE.
    ENDIF
    IF (printlev>=3) WRITE (numout,*) ' hydrol_main Done '

  END SUBROUTINE hydrol_main


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_finalize
!!
!>\BRIEF         
!!
!! DESCRIPTION : This subroutine writes the module variables and variables calculated in hydrol to restart file
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrol_finalize( kjit,           kjpindex, rest_id,  vegstress,  &
                              qsintveg,       humrel,                         &
                              snow,           snow_age, snow_nobio,           &
                              snow_nobio_age, snowrho,  snowtemp,             &
                              snowdz,         snowheat,                       &
                              fwet_out,                                       &
                              snowgrain, drysoil_frac, evap_bare_lim)
!                              snowcap,        snowgrain, drysoil_frac, evap_bare_lim, evap_bare_lim_pft)

    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                           :: kjit           !! Time step number 
    INTEGER(i_std), INTENT(in)                           :: kjpindex       !! Domain size
    INTEGER(i_std),INTENT (in)                           :: rest_id        !! Restart file identifier
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: vegstress      !! Veg. moisture stress (only for vegetation growth)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: qsintveg       !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: humrel
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: snow           !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: snow_age       !! Snow age
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio     !! Water balance on ice, lakes, .. [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio_age !! Snow age on ice, lakes, ...
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowrho        !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowtemp       !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowdz         !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowheat       !! Snow heat content
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)          :: drysoil_frac   !! function of litter wetness
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)          :: evap_bare_lim
!    REAL(r_std),DIMENSION (kjpindex,nvm),INTENT(in)       :: evap_bare_lim_pft

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)       :: fwet_out       !! output wetland fraction to change energy or runoff ???!!!
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(in)   :: snowgrain      !! Snow grain size

    !! 0.4 Local variables
    INTEGER(i_std)                                       :: jst, jsl
   
!_ ================================================================================================================================


    IF (printlev>=3) WRITE (numout,*) ' we have to complete restart file with HYDROLOGIC variables '

    CALL restput_p(rest_id, 'moistc', nbp_glo,  nslm, jst, kjit, mc, 'scatter',  nbp_glo, index_g)
    !
    DO jst=1,nstm
      DO jsl=1,nslm
         ! var_name= "us_1_01" ... "us_3_11"
         WRITE (var_name,"('us_',i1,'_',i2.2)") jst,jsl
         CALL restput_p(rest_id, var_name, nbp_glo,nvm, 1,kjit,us(:,:,jst,jsl),'scatter',nbp_glo,index_g)
      END DO
    END DO
    
    CALL restput_p(rest_id, 'free_drain_coef', nbp_glo,   nstm, 1, kjit,  free_drain_coef, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'water2infilt', nbp_glo,   nstm, 1, kjit,  water2infilt, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'ae_ns', nbp_glo,   nstm, 1, kjit,  ae_ns, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'vegstress', nbp_glo,   nvm, 1, kjit,  vegstress, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow', nbp_glo,   1, 1, kjit,  snow, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_age', nbp_glo,   1, 1, kjit,  snow_age, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_nobio', nbp_glo,   nnobio, 1, kjit,  snow_nobio, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_nobio_age', nbp_glo,   nnobio, 1, kjit,  snow_nobio_age, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'qsintveg', nbp_glo, nvm, 1, kjit,  qsintveg, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'evap_bare_lim_ns', nbp_glo, nstm, 1, kjit,  evap_bare_lim_ns, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'evap_bare_lim', nbp_glo, 1, 1, kjit,  evap_bare_lim, 'scatter',  nbp_glo, index_g)
!    CALL restput_p(rest_id, 'evap_bare_lim_pft', nbp_glo, 1, 1, kjit,  evap_bare_lim_pft, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'resdist', nbp_glo, nstm, 1, kjit,  resdist, 'scatter',  nbp_glo, index_g)       
    CALL restput_p(rest_id, 'drysoil_frac', nbp_glo,   1, 1, kjit, drysoil_frac, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'humrel', nbp_glo,   nvm, 1, kjit,  humrel, 'scatter',  nbp_glo, index_g)

    !pss:+
    !
    var_name= 'fwet_out'
    CALL restput_p(rest_id, var_name, nbp_glo,   1, 1, kjit,  fwet_out, 'scatter',  nbp_glo, index_g)
    !
    !pss:-
    CALL restput_p(rest_id, 'cvs_over_veg', nbp_glo,  nvm, jst, kjit, cvs_over_veg, 'scatter',  nbp_glo, index_g)
    !
    IF ( check_waterbal ) THEN
      var_name= 'tot_water_beg'
      CALL restput_p(rest_id, var_name, nbp_glo,   1, 1, kjit,  tot_water_end, 'scatter', nbp_glo, index_g)
    ENDIF

    CALL restput_p(rest_id, 'tot_watveg_beg', nbp_glo,  1, 1, kjit,  tot_watveg_beg, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'tot_watsoil_beg', nbp_glo, 1, 1, kjit,  tot_watsoil_beg, 'scatter',  nbp_glo, index_g)
    CALL restput_p(rest_id, 'snow_beg', nbp_glo,        1, 1, kjit,  snow_beg, 'scatter',  nbp_glo, index_g)
    
    ! Write variables for explictsnow module to restart file
    IF (ok_explicitsnow) THEN

      CALL explicitsnow_finalize ( kjit,     kjpindex, rest_id,    snowrho,   &
                                    snowtemp, snowdz,   snowheat,   snowgrain)
    END IF

  END SUBROUTINE hydrol_finalize


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_init
!!
!>\BRIEF        Initializations and memory allocation   
!!
!! DESCRIPTION  :
!! - 1 Some initializations
!! - 2 make dynamic allocation with good dimension
!! - 2.1 array allocation for soil textur
!! - 2.2 Soil texture choice
!! - 3 Other array allocation
!! - 4 Open restart input file and read data for HYDROLOGIC process
!! - 5 get restart values if none were found in the restart file
!! - 6 Vegetation array      
!! - 7 set humrelv from us
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!!_ hydrol_init

  SUBROUTINE hydrol_init(kjit, kjpindex, index, rest_id, veget_max, soiltile, &
         humrel, vegstress, snow,       snow_age,   snow_nobio, snow_nobio_age, qsintveg, &
         snowdz, snowgrain, snowrho,    snowtemp,   snowheat, &
         drysoil_frac, evap_bare_lim,  &
!         snowflx,snowcap,   cgrnd_snow, dgrnd_snow, drysoil_frac, evap_bare_lim, evap_bare_lim_pft, &
         fwet_out ) 

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                         :: kjit               !! Time step number 
    INTEGER(i_std), INTENT (in)                         :: kjpindex           !! Domain size
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)    :: index              !! Indeces of the points on the map
    INTEGER(i_std), INTENT (in)                         :: rest_id            !! _Restart_ file identifier 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: veget_max          !! Carte de vegetation max
    REAL(r_std),DIMENSION (kjpindex,nstm), INTENT (in)  :: soiltile           !! Fraction of each soil tile (0-1, unitless)

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)  :: humrel             !! Stress hydrique, relative humidity
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)  :: vegstress          !! Veg. moisture stress (only for vegetation growth)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: snow               !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: snow_age           !! Snow age
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out) :: snow_nobio       !! Snow on ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (out) :: snow_nobio_age   !! Snow age on ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)    :: qsintveg         !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)    :: snowdz           !! Snow depth
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)    :: snowgrain        !! Snow grain size
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)    :: snowheat         !! Snow heat content
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)    :: snowtemp         !! Snow temperature
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(out)    :: snowrho          !! Snow density
!pss:+
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)  :: fwet_out            !! output wetland fraction to change energy or runoff ???!!!
!pss:-
    REAL(r_std),DIMENSION (kjpindex),INTENT(out)          :: drysoil_frac     !! function of litter wetness
    REAL(r_std),DIMENSION (kjpindex),INTENT(out)          :: evap_bare_lim
!    REAL(r_std),DIMENSION (kjpindex,nvm),INTENT(out)          ::evap_bare_lim_pft

    !! 0.3 Modified variable

    !! 0.4 Local variables

    INTEGER(i_std)                                     :: ier                   !! Error code
    INTEGER(i_std)                                     :: ji                    !! Index of land grid cells (1)
    INTEGER(i_std)                                     :: jv                    !! Index of PFTs (1)
    INTEGER(i_std)                                     :: jst                   !! Index of soil tiles (1)
    INTEGER(i_std)                                     :: jsl                   !! Index of soil layers (1)
    INTEGER(i_std)                                     :: jsc                   !! Index of soil texture (1)
    INTEGER(i_std), PARAMETER                          :: error_level = 3       !! Error level for consistency check
                                                                                !! Switch to 2 tu turn fatal errors into warnings  
    REAL(r_std), ALLOCATABLE, DIMENSION (:)            :: free_drain_max        !! Temporary variable for initialization of free_drain_coef 

!_ ================================================================================================================================

    !! 1 Some initializations
    !
    !
    !Config Key   = CHECK_CWRR
    !Config Desc  = Should we check detailed CWRR water balance ?
    !Config Def   = n
    !Config If    = HYDROL_CWRR
    !Config Help  = This parameters allows the user to check
    !Config         the detailed water balance in each time step
    !Config         of CWRR.
    !Config Units = [FLAG]
    !
    check_cwrr = .FALSE.
    CALL getin_p('CHECK_CWRR', check_cwrr)
    !
    !Config Key   = DO_PONDS
    !Config Desc  = Should we include ponds 
    !Config Def   = n
    !Config If    = HYDROL_CWRR
    !Config Help  = This parameters allows the user to ask the model
    !Config         to take into account the ponds and return 
    !Config         the water into the soil moisture. If this is 
    !Config         activated, then there is no reinfiltration 
    !Config         computed inside the hydrol module.
    !Config Units = [FLAG]
    !
    doponds = .FALSE.
    CALL getin_p('DO_PONDS', doponds)


    !! 2 make dynamic allocation with good dimension

    !! 2.1 array allocation for soil texture

    ALLOCATE (nvan(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable nvan','','')

    ALLOCATE (avan(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable avan','','')

    ALLOCATE (mcr(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mcr','','')

    ALLOCATE (mcs(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mcs','','')

    ALLOCATE (ks(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable ks','','')

    ALLOCATE (pcent(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable pcent','','')

    ALLOCATE (mcf(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mcf','','')

    ALLOCATE (mcw(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mcw','','')
    
    ALLOCATE (mc_awet(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mc_awet','','')

    ALLOCATE (mc_adry(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mc_adry','','')
       
    !!__2.2 Soil texture choose

    SELECTCASE (nscm)
    CASE (3)
              
       nvan(:) = nvan_fao(:)       
       avan(:) = avan_fao(:)
       mcr(:) = mcr_fao(:)
       mcs(:) = mcs_fao(:)
       ks(:) = ks_fao(:)
       pcent(:) = pcent_fao(:)
       mcf(:) = mcf_fao(:)
       mcw(:) = mcw_fao(:)
       mc_awet(:) = mc_awet_fao(:)
       mc_adry(:) = mc_adry_fao(:)
    CASE (12)
       
       nvan(:) = nvan_usda(:)
       avan(:) = avan_usda(:)
       mcr(:) = mcr_usda(:)
       mcs(:) = mcs_usda(:)
       ks(:) = ks_usda(:)
       pcent(:) = pcent_usda(:)
       mcf(:) = mcf_usda(:)
       mcw(:) = mcw_usda(:)
       mc_awet(:) = mc_awet_usda(:)
       mc_adry(:) = mc_adry_usda(:)
       
    CASE DEFAULT
       WRITE (numout,*) 'Unsupported soil type classification. Choose between zobler and usda according to the map'
       CALL ipslerr_p(3,'hydrol_init','Unsupported soil type classification. ',&
            'Choose between zobler and usda according to the map','')
    ENDSELECT


    !! 2.3 Read in the run.def the parameters values defined by the user

    !Config Key   = CWRR_N_VANGENUCHTEN
    !Config Desc  = Van genuchten coefficient n
    !Config If    = HYDROL_CWRR
    !Config Def   = 1.89, 1.56, 1.31
    !Config Help  = This parameter will be constant over the entire 
    !Config         simulated domain, thus independent from soil
    !Config         texture.   
    !Config Units = [-]
    CALL getin_p("CWRR_N_VANGENUCHTEN",nvan)

    !! Check parameter value (correct range)
    IF ( ANY(nvan(:) <= zero) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for CWRR_N_VANGENUCHTEN.", &
            &     "This parameter should be positive. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = CWRR_A_VANGENUCHTEN
    !Config Desc  = Van genuchten coefficient a
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.0075, 0.0036, 0.0019
    !Config Help  = This parameter will be constant over the entire 
    !Config         simulated domain, thus independent from soil
    !Config         texture.   
    !Config Units = [1/mm]  
    CALL getin_p("CWRR_A_VANGENUCHTEN",avan)

    !! Check parameter value (correct range)
    IF ( ANY(avan(:) <= zero) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for CWRR_A_VANGENUCHTEN.", &
            &     "This parameter should be positive. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = VWC_RESIDUAL
    !Config Desc  = Residual soil water content
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.065, 0.078, 0.095
    !Config Help  = This parameter will be constant over the entire 
    !Config         simulated domain, thus independent from soil
    !Config         texture.   
    !Config Units = [m3/m3]  
    CALL getin_p("VWC_RESIDUAL",mcr)

    !! Check parameter value (correct range)
    IF ( ANY(mcr(:) < zero) .OR. ANY(mcr(:) > 1.)  ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for VWC_RESIDUAL.", &
            &     "This parameter is ranged between 0 and 1. ", &
            &     "Please, check parameter value in run.def. ")
    END IF

    
    !Config Key   = VWC_SAT
    !Config Desc  = Saturated soil water content
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.41, 0.43, 0.41
    !Config Help  = This parameter will be constant over the entire 
    !Config         simulated domain, thus independent from soil
    !Config         texture.   
    !Config Units = [m3/m3]  
    CALL getin_p("VWC_SAT",mcs)

    !! Check parameter value (correct range)
    IF ( ANY(mcs(:) < zero) .OR. ANY(mcs(:) > 1.) .OR. ANY(mcs(:) <= mcr(:)) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for VWC_SAT.", &
            &     "This parameter should be greater than VWC_RESIDUAL and less than 1. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = CWRR_KS 
    !Config Desc  = Hydraulic conductivity Saturation
    !Config If    = HYDROL_CWRR 
    !Config Def   = 1060.8, 249.6, 62.4
    !Config Help  = This parameter will be constant over the entire 
    !Config         simulated domain, thus independent from soil
    !Config         texture.   
    !Config Units = [mm/d]   
    CALL getin_p("CWRR_KS",ks)

    !! Check parameter value (correct range)
    IF ( ANY(ks(:) <= zero) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for CWRR_KS.", &
            &     "This parameter should be positive. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = WETNESS_TRANSPIR_MAX
    !Config Desc  = Soil moisture above which transpir is max
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.5, 0.5, 0.5
    !Config Help  = This parameter is independent from soil texture for
    !Config         the time being.
    !Config Units = [-]    
    CALL getin_p("WETNESS_TRANSPIR_MAX",pcent)

    !! Check parameter value (correct range)
    IF ( ANY(pcent(:) <= zero) .OR. ANY(pcent(:) > 1.) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for WETNESS_TRANSPIR_MAX.", &
            &     "This parameter should be positive and less or equals than 1. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = VWC_FC 
    !Config Desc  = Volumetric water content field capacity
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.32, 0.32, 0.32
    !Config Help  = This parameter is independent from soil texture for
    !Config         the time being.
    !Config Units = [m3/m3]   
    CALL getin_p("VWC_FC",mcf)

    !! Check parameter value (correct range)
    IF ( ANY(mcf(:) > mcs(:)) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for VWC_FC.", &
            &     "This parameter should be less than VWC_SAT. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = VWC_WP
    !Config Desc  = Volumetric water content Wilting pt
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.10, 0.10, 0.10 
    !Config Help  = This parameter is independent from soil texture for
    !Config         the time being.
    !Config Units = [m3/m3]   
    CALL getin_p("VWC_WP",mcw)

    !! Check parameter value (correct range)
    IF ( ANY(mcw(:) > mcf(:)) .OR. ANY(mcw(:) < mcr(:)) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for VWC_WP.", &
            &     "This parameter should be greater or equal than VWC_RESIDUAL and less or equal than VWC_SAT.", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = VWC_MIN_FOR_WET_ALB
    !Config Desc  = Vol. wat. cont. above which albedo is cst
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.25, 0.25, 0.25
    !Config Help  = This parameter is independent from soil texture for
    !Config         the time being.
    !Config Units = [m3/m3]  
    CALL getin_p("VWC_MIN_FOR_WET_ALB",mc_awet)

    !! Check parameter value (correct range)
    IF ( ANY(mc_awet(:) < 0) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for VWC_MIN_FOR_WET_ALB.", &
            &     "This parameter should be positive. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = VWC_MAX_FOR_DRY_ALB
    !Config Desc  = Vol. wat. cont. below which albedo is cst
    !Config If    = HYDROL_CWRR
    !Config Def   = 0.1, 0.1, 0.1
    !Config Help  = This parameter is independent from soil texture for
    !Config         the time being.
    !Config Units = [m3/m3]   
    CALL getin_p("VWC_MAX_FOR_DRY_ALB",mc_adry)

    !! Check parameter value (correct range)
    IF ( ANY(mc_adry(:) < 0) .OR. ANY(mc_adry(:) > mc_awet(:)) ) THEN
       CALL ipslerr_p(error_level, "hydrol_init.", &
            &     "Wrong parameter value for VWC_MAX_FOR_DRY_ALB.", &
            &     "This parameter should be positive and not greater than VWC_MIN_FOR_WET_ALB.", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !! 3 Other array allocation


    ALLOCATE (mask_veget(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mask_veget','','')

    ALLOCATE (irrig_fin(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable irrig_fin','','')

    !ALLOCATE (vegstress(kjpindex,nvm),stat=ier)
    !IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable irrig_fin','','')

    ALLOCATE (mask_soiltile(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mask_soiltile','','')

    ALLOCATE (humrelv(kjpindex,nvm,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable humrelv','','')

    ALLOCATE (vegstressv(kjpindex,nvm,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable vegstressv','','')

    ALLOCATE (us(kjpindex,nvm,nstm,nslm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable us','','')

    ALLOCATE (precisol(kjpindex,nvm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable precisol','','')

    ALLOCATE (precisol_ns(kjpindex,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable precisol_nc','','')

    ALLOCATE (free_drain_coef(kjpindex,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable free_drain_coef','','')

    ALLOCATE (frac_bare_ns(kjpindex,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable frac_bare_ns','','')

    ALLOCATE (water2infilt(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable water2infilt','','')

    ALLOCATE (ae_ns(kjpindex,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable ae_ns','','')

    ALLOCATE (evap_bare_lim_ns(kjpindex,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable evap_bare_lim_ns','','')

    ALLOCATE (rootsink(kjpindex,nslm,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable rootsink','','')

    ALLOCATE (subsnowveg(kjpindex),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable subsnowveg','','')

    ALLOCATE (subsnownobio(kjpindex,nnobio),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable subsnownobio','','')

    ALLOCATE (snowmelt(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in snowmelt allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrol_init'
    END IF

    ALLOCATE (icemelt(kjpindex),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable icemelt','','')

    ALLOCATE (subsinksoil(kjpindex),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable subsinksoil','','')

    ALLOCATE (mx_eau_var(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mx_eau_var','','')

    ALLOCATE (vegtot(kjpindex),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable vegtot','','')

    ALLOCATE (resdist(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable resdist','','')

    ALLOCATE (humtot(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable humtot','','')

    ALLOCATE (resolv(kjpindex),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable resolv','','')

    ALLOCATE (k(kjpindex,nslm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable k','','')

    IF (ok_freeze_cwrr) THEN
       ALLOCATE (kk_moy(kjpindex,nslm),stat=ier) 
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable kk_moy','','')
       kk_moy(:,:) = 276.48

       ALLOCATE (kk(kjpindex,nslm,nstm),stat=ier) 
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable kk','','')
       kk(:,:,:) = 276.48
    ENDIF

    ALLOCATE (a(kjpindex,nslm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable a','','')

    ALLOCATE (b(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable b','','')

    ALLOCATE (d(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable d','','')

    ALLOCATE (e(kjpindex,nslm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable e','','')

    ALLOCATE (f(kjpindex,nslm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable f','','')

    ALLOCATE (g1(kjpindex,nslm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable g1','','')

    ALLOCATE (ep(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable ep','','')

    ALLOCATE (fp(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable fp','','')

    ALLOCATE (gp(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable gp','','')

    ALLOCATE (rhs(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable rhs','','')

    ALLOCATE (srhs(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable srhs','','')

    ALLOCATE (tmc(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc','','')

    ALLOCATE (tmcs(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmcs','','')

    ALLOCATE (tmcr(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmcr','','')

    ALLOCATE (tmc_litter(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter','','')

    ALLOCATE (tmc_litt_mea(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litt_mea','','')

    ALLOCATE (tmc_litter_res(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter_res','','')

    ALLOCATE (tmc_litter_wilt(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter_wilt','','')

    ALLOCATE (tmc_litter_field(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter_field','','')

    ALLOCATE (tmc_litter_sat(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter_sat','','')

    ALLOCATE (tmc_litter_awet(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter_awet','','')

    ALLOCATE (tmc_litter_adry(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litter_adry','','')

    ALLOCATE (tmc_litt_wet_mea(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litt_wet_mea','','')

    ALLOCATE (tmc_litt_dry_mea(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmc_litt_dry_mea','','')

    ALLOCATE (v1(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable v1','','')

    ALLOCATE (qflux00(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable qflux00','','')

    ALLOCATE (ru_ns(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable ru_ns','','')
    ru_ns(:,:) = zero

    ALLOCATE (dr_ns(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable dr_ns','','')
    dr_ns(:,:) = zero

    ALLOCATE (tr_ns(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tr_ns','','')

    ALLOCATE (cvs_over_veg(kjpindex,nvm,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable cvs_over_veg','','')

    ALLOCATE (corr_veg_soil(kjpindex,nvm,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable corr_veg_soil','','')

    ALLOCATE (mc(kjpindex,nslm,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mc','','')

    ALLOCATE (soilmoist(kjpindex,nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable soilmoist','','')

    ALLOCATE (soil_wet(kjpindex,nslm,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable soil_wet','','')

    ALLOCATE (soil_wet_litter(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable soil_wet_litter','','')

    ALLOCATE (qflux(kjpindex,nslm,nstm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable qflux','','')

    ALLOCATE (tmat(kjpindex,nslm,3),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tmat','','')

    ALLOCATE (stmat(kjpindex,nslm,3),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable stmat','','')

    ALLOCATE (nroot(nvm, nstm, nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable nroot','','')

    ALLOCATE (kfact_root(kjpindex, nslm, nstm), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable kfact_root','','')

    ALLOCATE (kfact(nslm, nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable kfact','','')

    ALLOCATE (zz(nslm+1),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable zz','','')

!jgjg    ALLOCATE (dz(nslm+1),stat=ier)
    ALLOCATE (dz(nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable dz','','')

    ALLOCATE (dh(nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable dh','','')

    ALLOCATE (mc_lin(imin:imax, nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mc_lin','','')

    ALLOCATE (k_lin(imin:imax, nslm, nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable k_lin','','')

    ALLOCATE (d_lin(imin:imax, nslm, nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable d_lin','','')

    ALLOCATE (a_lin(imin:imax, nslm, nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable a_lin','','')

    ALLOCATE (b_lin(imin:imax, nslm, nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable b_lin','','')

!pss+ ! WETALND variables allocation
!pss:+
    
    ALLOCATE (fsat(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in fsat allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (fwet(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in fwet allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (fwt1(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in fwt1 allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    
    ALLOCATE (fwt2(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in fwt2 allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
   
    ALLOCATE (fwt3(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in fwt3 allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (fwt4(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in fwt4 allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (drunoff(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in drunoff allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
       
    ALLOCATE (ZMEAN(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in zmean allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
!    ALLOCATE (NB_PIXE(kjpindex),stat=ier)
!    IF (ier.NE.0) THEN
!        WRITE (numout,*) ' error in mx_eau_var allocation. We stop. We need kjpindex words = ',kjpindex
!        STOP 'hydrolc_init'
!    END IF
    ALLOCATE (ZSTDT(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in zstdt allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    
    ALLOCATE (ZSKEW(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in zskew allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (ZMIN(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in zmin allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    
    ALLOCATE (ZMAX(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in zmax allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF
    
    ALLOCATE (ZM(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in ZM allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (ZZPAS(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in zzpas allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (ZTAB_FSAT(kjpindex,1000),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error inztab_fsat allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (ZTAB_WTOP(kjpindex,1000),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in ztab_wtop allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (ZTAB_FWET(kjpindex,1000),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in ztab_fwet allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

    ALLOCATE (ZTAB_WTOP_WET(kjpindex,1000),stat=ier)
    IF (ier.NE.0) THEN
        WRITE (numout,*) ' error in ztab_wtop_wet allocation. We stop. We need kjpindex words = ',kjpindex
        STOP 'hydrolc_init'
    END IF

!pss+
    ALLOCATE (mcw_grid(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
       WRITE (numout,*) ' error in mcw_grid allocation. We stop. We need kjpindex words = ',kjpindex
       STOP 'hydrol_init'
    END IF

    ALLOCATE (mcs_grid(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
       WRITE (numout,*) ' error in mcs_grid allocation. We stop. We need kjpindex words = ',kjpindex
       STOP 'hydrol_init'
    END IF
!pss-


    IF (ok_freeze_cwrr) THEN
       ALLOCATE (profil_froz_hydro(kjpindex, nslm),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable profil_froz_hydrol','','')
       profil_froz_hydro(:,:) = zero

       ALLOCATE (profil_froz_hydro_ns(kjpindex, nslm, nstm),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable profil_froz_hydro_ns','','')
       profil_froz_hydro_ns(:,:,:) = zero

       ALLOCATE (temp_hydro(kjpindex, nslm),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable temp_hydro','','')
       temp_hydro(:,:) = 280.
    ENDIF

    ALLOCATE (mcl(kjpindex, nslm, nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable mcl','','')

    ALLOCATE (frac_hydro_diag(nslm, nbdl),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable frac_hydro_diag','','')

    !  If we check the water balance we need two more variables
    IF ( check_waterbal ) THEN
       ALLOCATE (tot_water_beg(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_water_beg','','')

       ALLOCATE (tot_water_end(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_water_end','','')

       ALLOCATE (tot_flux(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_flux','','')
    ENDIF

    ! Soil Wetness Index
    ALLOCATE (swi(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable swi','','')

    ALLOCATE (tot_watveg_beg(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_watveg_beg','','')
    
    ALLOCATE (tot_watveg_end(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_watvag_end','','')
    
    ALLOCATE (tot_watsoil_beg(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_watsoil_beg','','')
    
    ALLOCATE (tot_watsoil_end(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable tot_watsoil_end','','')
    
    ALLOCATE (delsoilmoist(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable delsoilmoist','','')
    
    ALLOCATE (delintercept(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable delintercept','','')
    
    ALLOCATE (delswe(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable delswe','','')
    
    ALLOCATE (snow_beg(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable snow_beg','','')
    
    ALLOCATE (snow_end(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable snow_end','','')
    
    !! 4 Open restart input file and read data for HYDROLOGIC process
       IF (printlev>=3) WRITE (numout,*) ' we have to read a restart file for HYDROLOGIC variables'

       CALL ioconf_setatt_p('UNITS', '-')
       CALL restget_p (rest_id, 'moistc', nbp_glo, nslm , jst, kjit, .TRUE., mc, "gather", nbp_glo, index_g)
       !
       CALL ioconf_setatt_p('UNITS', '-')
       DO jst=1,nstm
          DO jsl=1,nslm
             ! var_name= "us_1_01" ... "us_3_11"
             WRITE (var_name,"('us_',i1,'_',i2.2)") jst,jsl
             CALL ioconf_setatt_p('LONG_NAME',var_name)
             CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., us(:,:,jst,jsl), "gather", nbp_glo, index_g)
          END DO
       END DO
       !
       var_name= 'free_drain_coef'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '-')
          CALL ioconf_setatt_p('LONG_NAME','Coefficient for free drainage at bottom of soil')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nstm, 1, kjit, .TRUE., free_drain_coef, "gather", nbp_glo, index_g)
       !
       var_name= 'water2infilt'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '-')
          CALL ioconf_setatt_p('LONG_NAME','Remaining water to be infiltrated on top of the soil')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nstm, 1, kjit, .TRUE., water2infilt, "gather", nbp_glo, index_g)
       !
       var_name= 'ae_ns'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', 'kg/m^2')
          CALL ioconf_setatt_p('LONG_NAME','Bare soil evap on each soil type')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nstm, 1, kjit, .TRUE., ae_ns, "gather", nbp_glo, index_g)
       !
       var_name= 'snow'        
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', 'kg/m^2')
          CALL ioconf_setatt_p('LONG_NAME','Snow mass')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, 1  , 1, kjit, .TRUE., snow, "gather", nbp_glo, index_g)
       !
       var_name= 'snow_age'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', 'd')
          CALL ioconf_setatt_p('LONG_NAME','Snow age')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, 1  , 1, kjit, .TRUE., snow_age, "gather", nbp_glo, index_g)
       !
       var_name= 'snow_nobio'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', 'kg/m^2')
          CALL ioconf_setatt_p('LONG_NAME','Snow on other surface types')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nnobio  , 1, kjit, .TRUE., snow_nobio, "gather", nbp_glo, index_g)
       !
       var_name= 'snow_nobio_age'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', 'd')
          CALL ioconf_setatt_p('LONG_NAME','Snow age on other surface types')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nnobio  , 1, kjit, .TRUE., snow_nobio_age, "gather", nbp_glo, index_g)
       !
       var_name= 'vegstress'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '-')
          CALL ioconf_setatt_p('LONG_NAME','Vegetation growth moisture stress')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., vegstress, "gather", nbp_glo, index_g)
       !
       var_name= 'qsintveg'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', 'kg/m^2')
          CALL ioconf_setatt_p('LONG_NAME','Intercepted moisture')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., qsintveg, "gather", nbp_glo, index_g)

!pss:+ !          
       var_name= 'fwet_out'      
       IF (is_root_prc) THEN
          CALL ioconf_setatt('UNITS', '-')
          CALL ioconf_setatt('LONG_NAME','fwet pr autres routines')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, 1  , 1, kjit, .TRUE.,fwet_out , "gather", nbp_glo, index_g)
!pss:-
       var_name= 'evap_bare_lim_ns'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '?')
          CALL ioconf_setatt_p('LONG_NAME','?')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nstm, 1, kjit, .TRUE., evap_bare_lim_ns, "gather", nbp_glo, index_g)
       CALL setvar_p (evap_bare_lim_ns, val_exp, 'NO_KEYWORD', 0.0)
!       DO jv = 1,nvm
!           evap_bare_lim_pft(:,jv) = evap_bare_lim_ns(:,pref_soil_veg(jv))
!       ENDDO

       var_name= 'resdist'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '-')
          CALL ioconf_setatt_p('LONG_NAME','soiltile values from previous time-step')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nstm, 1, kjit, .TRUE., resdist, "gather", nbp_glo, index_g)
       
       CALL ioconf_setatt_p('UNITS', '-')
       CALL restget_p (rest_id, 'cvs_over_veg', nbp_glo,  nvm, jst, kjit, .TRUE., cvs_over_veg, "gather",  nbp_glo, index_g)
       
       IF ( check_waterbal ) THEN
          var_name= 'tot_water_beg'
          IF (is_root_prc) THEN
             CALL ioconf_setatt_p('UNITS', 'kg/m^2')
             CALL ioconf_setatt_p('LONG_NAME','Previous Total water')
          ENDIF
          CALL restget_p (rest_id, var_name, nbp_glo, 1  , 1, kjit, .TRUE., tot_water_beg, "gather", nbp_glo, index_g)
       ENDIF

       ! Read drysoil_frac. It will be initalized later in hydrol_var_init if the varaible is not find in restart file.
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '')
          CALL ioconf_setatt_p('LONG_NAME','Function of litter wetness')
       ENDIF
       CALL restget_p (rest_id, 'drysoil_frac', nbp_glo, 1  , 1, kjit, .TRUE., drysoil_frac, "gather", nbp_glo, index_g)


    !! 5 get restart values if none were found in the restart file
       !
       !Config Key   = HYDROL_MOISTURE_CONTENT
       !Config Desc  = Soil moisture on each soil tile and levels
       !Config If    = HYDROL_CWRR       
       !Config Def   = 0.3
       !Config Help  = The initial value of mc if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = [m3/m3]
       !
       CALL setvar_p (mc, val_exp, 'HYDROL_MOISTURE_CONTENT', 0.3_r_std)
       !
       !Config Key   = US_INIT
       !Config Desc  = US_NVM_NSTM_NSLM
       !Config If    = HYDROL_CWRR       
       !Config Def   = 0.0
       !Config Help  = The initial value of us (relative moisture) if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = [-]
       !
       DO jsl=1,nslm
          CALL setvar_p (us(:,:,:,jsl), val_exp, 'US_INIT', zero)
       ENDDO
       !
       !Config Key   = FREE_DRAIN_COEF
       !Config Desc  = Coefficient for free drainage at bottom, dimension nstm
       !Config If    = HYDROL_CWRR       
       !Config Def   = 1.0 1.0 1.0
       !Config Help  = The initial value of free drainage coefficient if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = [-]
              
       ALLOCATE (free_drain_max(nstm),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'hydrol_init','Problem in allocate of variable free_drain_max','','')
       free_drain_max(:)=1.0

       CALL setvar_p (free_drain_coef, val_exp, 'FREE_DRAIN_COEF', free_drain_max)
       DEALLOCATE(free_drain_max)

       !
       !Config Key   = WATER_TO_INFILT
       !Config Desc  = Water to be infiltrated on top of the soil
       !Config If    = HYDROL_CWRR    
       !Config Def   = 0.0
       !Config Help  = The initial value of free drainage if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = [mm]
       !
       CALL setvar_p (water2infilt, val_exp, 'WATER_TO_INFILT', zero)
       !
       !Config Key   = EVAPNU_SOIL
       !Config Desc  = Bare soil evap on each soil if not found in restart
       !Config If    = HYDROL_CWRR  
       !Config Def   = 0.0
       !Config Help  = The initial value of bare soils evap if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = [mm]
       !
       CALL setvar_p (ae_ns, val_exp, 'EVAPNU_SOIL', zero)
       !
       !Config Key  = HYDROL_SNOW
       !Config Desc  = Initial snow mass if not found in restart
       !Config If    = OK_SECHIBA
       !Config Def   = 0.0
       !Config Help  = The initial value of snow mass if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units =
       !
       CALL setvar_p (snow, val_exp, 'HYDROL_SNOW', zero)
       !
       !Config Key   = HYDROL_SNOWAGE
       !Config Desc  = Initial snow age if not found in restart
       !Config If    = OK_SECHIBA
       !Config Def   = 0.0
       !Config Help  = The initial value of snow age if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = ***
       !
       CALL setvar_p (snow_age, val_exp, 'HYDROL_SNOWAGE', zero)
       !
       !Config Key   = HYDROL_SNOW_NOBIO
       !Config Desc  = Initial snow amount on ice, lakes, etc. if not found in restart
       !Config If    = OK_SECHIBA
       !Config Def   = 0.0
       !Config Help  = The initial value of snow if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = [mm]
       !
       CALL setvar_p (snow_nobio, val_exp, 'HYDROL_SNOW_NOBIO', zero)
       !
       !Config Key   = HYDROL_SNOW_NOBIO_AGE
       !Config Desc  = Initial snow age on ice, lakes, etc. if not found in restart
       !Config If    = OK_SECHIBA
       !Config Def   = 0.0
       !Config Help  = The initial value of snow age if its value is not found
       !Config         in the restart file. This should only be used if the model is 
       !Config         started without a restart file.
       !Config Units = ***
       !
       CALL setvar_p (snow_nobio_age, val_exp, 'HYDROL_SNOW_NOBIO_AGE', zero)
       !
       !Config Key   = HYDROL_QSV
       !Config Desc  = Initial water on canopy if not found in restart
       !Config If    = OK_SECHIBA
       !Config Def   = 0.0
       !Config Help  = The initial value of moisture on canopy if its value 
       !Config         is not found in the restart file. This should only be used if
       !Config         the model is started without a restart file. 
       !Config Units = [mm]
       !
       CALL setvar_p (qsintveg, val_exp, 'HYDROL_QSV', zero)

       IF (ok_freeze_cwrr) THEN  
          CALL setvar_p (profil_froz_hydro, val_exp, 'NO_KEYWORD', zero)
          CALL setvar_p (profil_froz_hydro_ns, val_exp, 'NO_KEYWORD', zero)
          CALL setvar_p (kk, val_exp, 'NO_KEYWORD', 276.48)
          CALL setvar_p (kk_moy, val_exp, 'NO_KEYWORD', 276.48)
          CALL setvar_p (temp_hydro, val_exp, 'NO_KEYWORD', 280.)
       ENDIF
       
!pss:+
       !
       !Config Key   = HYDROL_FWET
       !Config Desc  = Initial fwet_out if not found in restart
       !Config If    = TOPM_calcul
       !Config Def   = 0.0
       !Config Help  = The initial value of fwet_out if its value 
       !Config         is not found in the restart file. This should only be used if
       !Config         the model is started without a restart file. 
       !Config Units =
       CALL setvar_p (fwet_out, val_exp,'HYDROL_FWET', zero)

!pss:-

    !! 6 Vegetation array      
       !
       ! If resdist is not in restart file, initialize with soiltile
       IF ( MINVAL(resdist) .EQ.  MAXVAL(resdist) .AND. MINVAL(resdist) .EQ. val_exp) THEN
          resdist(:,:) = soiltile(:,:)
       ENDIF
       !
       !  Remember that it is only frac_nobio + SUM(veget_max(,:)) that is equal to 1. Thus we need vegtot
       !
       DO ji = 1, kjpindex
          vegtot(ji) = SUM(veget_max(ji,:))
       ENDDO
       !
       !
       ! compute the masks for veget


       mask_veget(:,:) = 0
       mask_soiltile(:,:) = 0

       DO jst=1,nstm
          DO ji = 1, kjpindex
             IF(soiltile(ji,jst) .GT. min_sechiba) THEN
                mask_soiltile(ji,jst) = 1
             ENDIF
          END DO
       ENDDO
          
       DO jv = 1, nvm
          DO ji = 1, kjpindex
             IF(veget_max(ji,jv) .GT. min_sechiba) THEN
                mask_veget(ji,jv) = 1
             ENDIF
          END DO
       END DO
          
    !! 7. set humrelv from us
! soiltile(ji,jst) * cvs_over_veg(ji,jv,jst) * vegtot(ji) = 1 if PFT jv belongs to soiltile jst
!                                                         = 0 else
! here cvs_over_veg defines the vegetation cover of the previous timestep, consistently with vegstressv and humrelv

       humrelv(:,:,:) = SUM(us,dim=4)
       vegstressv(:,:,:) = humrelv(:,:,:)

       CALL setvar_p (cvs_over_veg, val_exp, 'NO_KEYWORD', un)

       vegstress(:,:) = zero
       
       DO jst=1,nstm
          DO jv=1,nvm
             DO ji=1,kjpindex
                vegstress(ji,jv)=vegstress(ji,jv) + vegstressv(ji,jv,jst) * &
                     & soiltile(ji,jst) * cvs_over_veg(ji,jv,jst) * vegtot(ji)
             END DO
          END DO
       END DO

       ! Read humrel from restart file
       var_name= 'humrel'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '')
          CALL ioconf_setatt_p('LONG_NAME','Relative humidity')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, .TRUE., humrel, "gather", nbp_glo, index_g)

       ! Calculate humrel if it is not found in restart file
       IF (ALL(humrel(:,:)==val_exp)) THEN
          ! set humrel from humrelv, assuming equi-repartition for the first time step
          humrel(:,:) = zero
          DO jst=1,nstm
             DO jv=1,nvm
                DO ji=1,kjpindex
                   humrel(ji,jv)=humrel(ji,jv) + humrelv(ji,jv,jst) * & 
                        & soiltile(ji,jst) * cvs_over_veg(ji,jv,jst) * vegtot(ji)
                   humrel(ji,jv)=MAX(humrel(ji,jv), zero)* mask_veget(ji,jv)           
                END DO
             END DO
          END DO
       END IF

       ! Read evap_bare_lim from restart file
       var_name= 'evap_bare_lim'
       IF (is_root_prc) THEN
          CALL ioconf_setatt_p('UNITS', '')
          CALL ioconf_setatt_p('LONG_NAME','Limitation factor for bare soil evaporation')
       ENDIF
       CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., evap_bare_lim, "gather", nbp_glo, index_g)

       ! Calculate evap_bare_lim if it was not found in the restart file.
       IF ( ALL(evap_bare_lim(:) == val_exp) ) THEN
          DO ji = 1, kjpindex
             evap_bare_lim(ji) =  SUM(evap_bare_lim_ns(ji,:)*vegtot(ji)*soiltile(ji,:))
          ENDDO
       END IF


    ! Read from restart file       
    ! The variables tot_watsoil_beg, tot_watsoil_beg and snwo_beg will be initialized in the end of 
    ! hydrol_initialize if they were not found in the restart file.
       
    var_name= 'tot_watveg_beg'
    IF (is_root_prc) THEN
       CALL ioconf_setatt_p('UNITS', '?')
       CALL ioconf_setatt_p('LONG_NAME','?')
    ENDIF
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., tot_watveg_beg, "gather", nbp_glo, index_g)
    
    var_name= 'tot_watsoil_beg'
    IF (is_root_prc) THEN
       CALL ioconf_setatt_p('UNITS', '?')
       CALL ioconf_setatt_p('LONG_NAME','?')
    ENDIF
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., tot_watsoil_beg, "gather", nbp_glo, index_g)
    
    var_name= 'snow_beg'
    IF (is_root_prc) THEN
       CALL ioconf_setatt_p('UNITS', '?')
       CALL ioconf_setatt_p('LONG_NAME','?')
    ENDIF
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., snow_beg, "gather", nbp_glo, index_g)
       
 
    ! Initialize variables for explictsnow module by reading restart file
    IF (ok_explicitsnow) THEN
       CALL explicitsnow_initialize( kjit,     kjpindex, rest_id,    snowrho,   &
                                     snowtemp, snowdz,   snowheat,   snowgrain)
    END IF

    
    IF (printlev>=3) WRITE (numout,*) ' hydrol_init done '
    
  END SUBROUTINE hydrol_init


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_clear
!!
!>\BRIEF        Deallocate arrays 
!!
!_ ================================================================================================================================
!_ hydrol_clear

  SUBROUTINE hydrol_clear()

    ! Allocation for soiltile related parameters
    IF ( ALLOCATED (nvan)) DEALLOCATE (nvan)
    IF ( ALLOCATED (avan)) DEALLOCATE (avan)
    IF ( ALLOCATED (mcr)) DEALLOCATE (mcr)
    IF ( ALLOCATED (mcs)) DEALLOCATE (mcs)
    IF ( ALLOCATED (ks)) DEALLOCATE (ks)
    IF ( ALLOCATED (pcent)) DEALLOCATE (pcent)
    IF ( ALLOCATED (mcf)) DEALLOCATE (mcf)
    IF ( ALLOCATED (mcw)) DEALLOCATE (mcw)
    IF ( ALLOCATED (mc_awet)) DEALLOCATE (mc_awet)
    IF ( ALLOCATED (mc_adry)) DEALLOCATE (mc_adry)
!pss+
    IF ( ALLOCATED (mcs_grid)) DEALLOCATE (mcs_grid)
    IF ( ALLOCATED (mcw_grid)) DEALLOCATE (mcw_grid)
!pss-
    ! Other arrays
    IF (ALLOCATED (mask_veget)) DEALLOCATE (mask_veget)
    IF (ALLOCATED (irrig_fin)) DEALLOCATE (irrig_fin)
    IF (ALLOCATED (vegstress)) DEALLOCATE (vegstress)
    IF (ALLOCATED (mask_soiltile)) DEALLOCATE (mask_soiltile)
    IF (ALLOCATED (humrelv)) DEALLOCATE (humrelv)
    IF (ALLOCATED (vegstressv)) DEALLOCATE (vegstressv)
    IF (ALLOCATED (us)) DEALLOCATE (us)
    IF (ALLOCATED  (precisol)) DEALLOCATE (precisol)
    IF (ALLOCATED  (precisol_ns)) DEALLOCATE (precisol_ns)
    IF (ALLOCATED  (free_drain_coef)) DEALLOCATE (free_drain_coef)
    IF (ALLOCATED  (frac_bare_ns)) DEALLOCATE (frac_bare_ns)
    IF (ALLOCATED  (water2infilt)) DEALLOCATE (water2infilt)
    IF (ALLOCATED  (ae_ns)) DEALLOCATE (ae_ns)
    IF (ALLOCATED  (evap_bare_lim_ns)) DEALLOCATE (evap_bare_lim_ns)
    IF (ALLOCATED  (rootsink)) DEALLOCATE (rootsink)
    IF (ALLOCATED  (subsnowveg)) DEALLOCATE (subsnowveg)
    IF (ALLOCATED  (subsnownobio)) DEALLOCATE (subsnownobio)
    IF (ALLOCATED  (snowmelt)) DEALLOCATE (snowmelt)
    IF (ALLOCATED  (icemelt)) DEALLOCATE (icemelt)
    IF (ALLOCATED  (subsinksoil)) DEALLOCATE (subsinksoil)
    IF (ALLOCATED  (mx_eau_var)) DEALLOCATE (mx_eau_var)
    IF (ALLOCATED  (vegtot)) DEALLOCATE (vegtot)
    IF (ALLOCATED  (resdist)) DEALLOCATE (resdist)
    IF (ALLOCATED  (tot_water_beg)) DEALLOCATE (tot_water_beg)
    IF (ALLOCATED  (tot_water_end)) DEALLOCATE (tot_water_end)
    IF (ALLOCATED  (tot_flux)) DEALLOCATE (tot_flux)
    IF (ALLOCATED  (tot_watveg_beg)) DEALLOCATE (tot_watveg_beg)
    IF (ALLOCATED  (tot_watveg_end)) DEALLOCATE (tot_watveg_end)
    IF (ALLOCATED  (tot_watsoil_beg)) DEALLOCATE (tot_watsoil_beg)
    IF (ALLOCATED  (tot_watsoil_end)) DEALLOCATE (tot_watsoil_end)
    IF (ALLOCATED  (delsoilmoist)) DEALLOCATE (delsoilmoist)
    IF (ALLOCATED  (delintercept)) DEALLOCATE (delintercept)
    IF (ALLOCATED  (snow_beg)) DEALLOCATE (snow_beg)
    IF (ALLOCATED  (snow_end)) DEALLOCATE (snow_end)
    IF (ALLOCATED  (delswe)) DEALLOCATE (delswe)
    IF (ALLOCATED  (swi)) DEALLOCATE (swi)
    IF (ALLOCATED  (v1)) DEALLOCATE (v1)
    IF (ALLOCATED  (humtot)) DEALLOCATE (humtot)
    IF (ALLOCATED  (resolv)) DEALLOCATE (resolv)
    IF (ALLOCATED  (k)) DEALLOCATE (k)
    IF (ALLOCATED  (kk)) DEALLOCATE (kk)
    IF (ALLOCATED  (kk_moy)) DEALLOCATE (kk_moy)
    IF (ALLOCATED  (a)) DEALLOCATE (a)
    IF (ALLOCATED  (b)) DEALLOCATE (b)
    IF (ALLOCATED  (d)) DEALLOCATE (d)
    IF (ALLOCATED  (e)) DEALLOCATE (e)
    IF (ALLOCATED  (f)) DEALLOCATE (f)
    IF (ALLOCATED  (g1)) DEALLOCATE (g1)
    IF (ALLOCATED  (ep)) DEALLOCATE (ep)
    IF (ALLOCATED  (fp)) DEALLOCATE (fp)
    IF (ALLOCATED  (gp)) DEALLOCATE (gp)
    IF (ALLOCATED  (rhs)) DEALLOCATE (rhs)
    IF (ALLOCATED  (srhs)) DEALLOCATE (srhs)
    IF (ALLOCATED  (tmc)) DEALLOCATE (tmc)
    IF (ALLOCATED  (tmcs)) DEALLOCATE (tmcs)
    IF (ALLOCATED  (tmcr)) DEALLOCATE (tmcr)
    IF (ALLOCATED  (tmc_litter)) DEALLOCATE (tmc_litter)
    IF (ALLOCATED  (tmc_litt_mea)) DEALLOCATE (tmc_litt_mea)
    IF (ALLOCATED  (tmc_litter_res)) DEALLOCATE (tmc_litter_res)
    IF (ALLOCATED  (tmc_litter_wilt)) DEALLOCATE (tmc_litter_wilt)
    IF (ALLOCATED  (tmc_litter_field)) DEALLOCATE (tmc_litter_field)
    IF (ALLOCATED  (tmc_litter_sat)) DEALLOCATE (tmc_litter_sat)
    IF (ALLOCATED  (tmc_litter_awet)) DEALLOCATE (tmc_litter_awet)
    IF (ALLOCATED  (tmc_litter_adry)) DEALLOCATE (tmc_litter_adry)
    IF (ALLOCATED  (tmc_litt_wet_mea)) DEALLOCATE (tmc_litt_wet_mea)
    IF (ALLOCATED  (tmc_litt_dry_mea)) DEALLOCATE (tmc_litt_dry_mea)
    IF (ALLOCATED  (qflux00)) DEALLOCATE (qflux00)
    IF (ALLOCATED  (ru_ns)) DEALLOCATE (ru_ns)
    IF (ALLOCATED  (dr_ns)) DEALLOCATE (dr_ns)
    IF (ALLOCATED  (tr_ns)) DEALLOCATE (tr_ns)
    IF (ALLOCATED  (cvs_over_veg)) DEALLOCATE (cvs_over_veg)
    IF (ALLOCATED  (corr_veg_soil)) DEALLOCATE (corr_veg_soil)
    IF (ALLOCATED  (mc)) DEALLOCATE (mc)
    IF (ALLOCATED  (soilmoist)) DEALLOCATE (soilmoist)
    IF (ALLOCATED  (soil_wet)) DEALLOCATE (soil_wet)
    IF (ALLOCATED  (soil_wet_litter)) DEALLOCATE (soil_wet_litter)
    IF (ALLOCATED  (qflux)) DEALLOCATE (qflux)
    IF (ALLOCATED  (tmat)) DEALLOCATE (tmat)
    IF (ALLOCATED  (stmat)) DEALLOCATE (stmat)
    IF (ALLOCATED  (nroot)) DEALLOCATE (nroot)
    IF (ALLOCATED  (kfact_root)) DEALLOCATE (kfact_root)
    IF (ALLOCATED  (kfact)) DEALLOCATE (kfact)
    IF (ALLOCATED  (zz)) DEALLOCATE (zz)
    IF (ALLOCATED  (dz)) DEALLOCATE (dz)
    IF (ALLOCATED  (dh)) DEALLOCATE (dh)
    IF (ALLOCATED  (mc_lin)) DEALLOCATE (mc_lin)
    IF (ALLOCATED  (k_lin)) DEALLOCATE (k_lin)
    IF (ALLOCATED  (d_lin)) DEALLOCATE (d_lin)
    IF (ALLOCATED  (a_lin)) DEALLOCATE (a_lin)
    IF (ALLOCATED  (b_lin)) DEALLOCATE (b_lin)
    IF (ALLOCATED  (frac_hydro_diag)) DEALLOCATE (frac_hydro_diag)
    
!pss:+ !WETLAND variables
    IF (ALLOCATED  (fsat))  DEALLOCATE (fsat)
    IF (ALLOCATED  (fwet))  DEALLOCATE (fwet)
    IF (ALLOCATED  (fwt1))  DEALLOCATE (fwt1)
    IF (ALLOCATED  (fwt2))  DEALLOCATE (fwt2)
    IF (ALLOCATED  (fwt3))  DEALLOCATE (fwt3)
    IF (ALLOCATED  (fwt4))  DEALLOCATE (fwt4)
    IF (ALLOCATED  (drunoff))  DEALLOCATE (drunoff)
    IF (ALLOCATED  (ZMEAN)) DEALLOCATE (ZMEAN)
!    IF (ALLOCATED  (NB_PIXE)) DEALLOCATE (NB_PIXE)
    IF (ALLOCATED  (ZSTDT)) DEALLOCATE (ZSTDT)
    IF (ALLOCATED  (ZSKEW)) DEALLOCATE (ZSKEW)
    IF (ALLOCATED  (ZMIN)) DEALLOCATE (ZMIN)
    IF (ALLOCATED  (ZMAX)) DEALLOCATE (ZMAX)
    IF (ALLOCATED  (ZM)) DEALLOCATE (ZM)
    IF (ALLOCATED  (ZZPAS)) DEALLOCATE (ZZPAS)
    IF (ALLOCATED  (ZTAB_FSAT)) DEALLOCATE (ZTAB_FSAT)
    IF (ALLOCATED  (ZTAB_WTOP)) DEALLOCATE (ZTAB_WTOP)
    IF (ALLOCATED  (ZTAB_FWET)) DEALLOCATE (ZTAB_FWET)
    IF (ALLOCATED  (ZTAB_WTOP_WET)) DEALLOCATE (ZTAB_WTOP_WET)
!pss:-

  END SUBROUTINE hydrol_clear

!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_tmc_update
!!
!>\BRIEF        This routine updates the soil moisture profiles when the vegetation fraction have changed. 
!!
!! DESCRIPTION  :
!! 
!!    This routine update tmc and mc with variation of veget_max (LAND_USE or DGVM activated)
!! 
!!
!!
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_tmc_update
  SUBROUTINE hydrol_tmc_update ( kjpindex, veget_max, soiltile, qsintveg, resdist )

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                          :: kjpindex      !! domain size
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: veget_max     !! max fraction of vegetation type
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in) :: soiltile      !! Fraction of each soil tile (0-1, unitless)

    !! 0.3 Modified variables
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)   :: qsintveg   !! Amount of water in the canopy interception 
    REAL(r_std), DIMENSION (kjpindex, nstm), INTENT(inout) :: resdist    !! Soiltile from previous time-step

    !! 0.4 Local variables
    INTEGER(i_std)                           :: ji, jv, jst,jsl
    LOGICAL                                  :: soil_upd        !! True if soiltile changed since last time step
    LOGICAL                                  :: error=.FALSE.   !! If true, exit in the end of subroutine
    REAL(r_std), DIMENSION(kjpindex,nstm)    :: vmr             !! Change in soiltile
    REAL(r_std), DIMENSION(kjpindex)         :: vmr_sum
    REAL(r_std), DIMENSION(kjpindex,nslm)    :: mc_dilu         !! Total loss of moisture content
    REAL(r_std), DIMENSION(kjpindex)         :: infil_dilu      !! Total loss for water2infilt
    REAL(r_std), DIMENSION(kjpindex,nstm)    :: tmc_old         !! tmc before calculations
    REAL(r_std), DIMENSION(kjpindex,nstm)    :: water2infilt_old!! water2infilt before calculations
    REAL(r_std), DIMENSION (kjpindex,nvm)    :: qsintveg_old    !! qsintveg before calculations
    REAL(r_std), DIMENSION(kjpindex)         :: test

    !! 0. Check if soiltiles changed since last time step
    soil_upd=SUM(ABS(soiltile(:,:)-resdist(:,:))) .GT. zero
    IF (printlev>=3) WRITE (numout,*) 'soil_upd ', soil_upd

    IF (check_cwrr) THEN
       ! Save soil moisture for later use
       tmc_old(:,:) = tmc(:,:) 
       water2infilt_old(:,:) = water2infilt(:,:)
       qsintveg_old(:,:) = qsintveg(:,:)
    ENDIF

    !! 1. If a PFT has disapperead as result from a veget_max change, 
    !!    then add canopy water to surface water.

    water2infilt(:,:) = zero

    DO ji=1,kjpindex
       DO jv=1,nvm
          IF ((veget_max(ji,jv).LT.min_sechiba).AND.(qsintveg(ji,jv).GT.0.)) THEN
             jst=pref_soil_veg(jv) ! soil tile index
             water2infilt(ji,jst) = water2infilt(ji,jst) + qsintveg(ji,jv)/resdist(ji,jst)
             qsintveg(ji,jv) = zero
          ENDIF
       ENDDO
    ENDDO
    
    !! 2. Compute new soil moisture if soiltile changed due to veget_max's change
    IF (soil_upd) THEN
       !! 2.1 Define the change in soiltile
       vmr(:,:) = soiltile(:,:) - resdist(:,:)  ! resdist is the previous values of soiltiles 

       ! Total area loss by the three soil tiles
       DO ji=1,kjpindex
          vmr_sum(ji)=SUM(vmr(ji,:),MASK=vmr(ji,:).LT.zero)
       ENDDO

       !! 2.2 Shrinking soil tiles
       !! 2.2.1 Total loss of moisture content from the shrinking soil tiles, expressed by soil layer
       mc_dilu(:,:)=zero
       DO jst=1,nstm
          DO jsl = 1, nslm
             DO ji=1,kjpindex
                IF ( vmr(ji,jst) < zero ) THEN
                   mc_dilu(ji,jsl) = mc_dilu(ji,jsl) + mc(ji,jsl,jst) * vmr(ji,jst) / vmr_sum(ji)
                ENDIF
             ENDDO
          ENDDO
       ENDDO

       !! 2.2.2 Total loss of water2inft from the shrinking soil tiles
       infil_dilu(:)=zero
       DO jst=1,nstm
          DO ji=1,kjpindex
             IF ( vmr(ji,jst) < zero ) THEN
                infil_dilu(ji) = infil_dilu(ji) + water2infilt(ji,jst) * vmr(ji,jst) / vmr_sum(ji)
             ENDIF
          ENDDO
       ENDDO

       !! 2.3 Each gaining soil tile gets moisture proportionally to both the total loss and its areal increase 

       ! As the original mc from each soil tile are in [mcr,mcs] and we do weighted avrage, the new mc are in [mcr,mcs]
       ! The case where the soiltile is created (soiltile_old=0) works as the other cases

       ! 2.3.1 Update mc(kjpindex,nslm,nstm) !m3/m3
       DO jst=1,nstm
          DO jsl = 1, nslm
             DO ji=1,kjpindex
                IF ( vmr(ji,jst) > zero ) THEN
                   mc(ji,jsl,jst) = ( mc(ji,jsl,jst) * resdist(ji,jst) + mc_dilu(ji,jsl) * vmr(ji,jst) ) / soiltile(ji,jst)
                   ! NB : soiltile can not be zero for case vmr > zero, see slowproc_veget
                ENDIF
             ENDDO
          ENDDO
       ENDDO
!
!       DO jst=1,nstm
!          IF ( vmr(1,jst) > zero ) THEN
!             WRITE(numout,*) 'zdcheck2 jst=',jst,'soiltile,resdist,vmr',soiltile(1,jst),resdist(1,jst),vmr(1,jst)
!          ENDIF
!       ENDDO
       
       ! 2.3.2 Update water2inft
       DO jst=1,nstm
          DO ji=1,kjpindex
             IF ( vmr(ji,jst) > zero ) THEN !donc soiltile>0     
                water2infilt(ji,jst) = ( water2infilt(ji,jst) * resdist(ji,jst) + infil_dilu(ji) * vmr(ji,jst) ) / soiltile(ji,jst)
             ENDIF !donc resdist>0
          ENDDO
       ENDDO

       ! 2.3.3 Case where soiltile < min_sechiba 
       DO jst=1,nstm
          DO ji=1,kjpindex
             IF ( soiltile(ji,jst) .LT. min_sechiba ) THEN
                water2infilt(ji,jst) = zero
                mc(ji,:,jst) = zero
             ENDIF
          ENDDO
       ENDDO


    ENDIF ! soil_upd 


    !2.3.3 we compute tmc(kjpindex,nstm) and humtot!
    DO jst=1,nstm
       DO ji=1,kjpindex
             tmc(ji,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
             DO jsl = 2,nslm-1
                tmc(ji,jst) = tmc(ji,jst) + dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                     + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
             ENDDO
             tmc(ji,jst) = tmc(ji,jst) + dz(nslm) * (trois*mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
             tmc(ji,jst) = tmc(ji,jst) + water2infilt(ji,jst)
             ! WARNING tmc is increased by water2infilt(ji,jst), but mc is not modified !
       ENDDO
    ENDDO

    humtot(:) = zero
    DO jst=1,nstm
       DO ji=1,kjpindex
          humtot(ji) = humtot(ji) + soiltile(ji,jst) * tmc(ji,jst)
       ENDDO
    ENDDO

    !! 4 check
    IF (check_cwrr) THEN
       DO ji=1,kjpindex
          test(ji) = ABS(SUM(tmc(ji,:)*soiltile(ji,:)) - SUM(tmc_old(ji,:)*resdist(ji,:)) + &
               SUM(qsintveg(ji,:)) - SUM(qsintveg_old(ji,:))) ! sum(soiltile)=1
          IF ( test(ji) .GT.  10.*allowed_err ) THEN
             WRITE(numout,*) 'tmc update WRONG: ji',ji
             WRITE(numout,*) 'tot water avant:',SUM(tmc_old(ji,:)*resdist(ji,:)) + SUM(qsintveg_old(ji,:))
             WRITE(numout,*) 'tot water apres:',SUM(tmc(ji,:)*soiltile(ji,:)) + SUM(qsintveg(ji,:))
             WRITE(numout,*) 'err:',test(ji)
             WRITE(numout,*) 'allowed_err:',allowed_err
             WRITE(numout,*) 'tmc:',tmc(ji,:)
             WRITE(numout,*) 'tmc_old:',tmc_old(ji,:)
             WRITE(numout,*) 'qsintveg:',qsintveg(ji,:)
             WRITE(numout,*) 'qsintveg_old:',qsintveg_old(ji,:)
             WRITE(numout,*) 'SUMqsintveg:',SUM(qsintveg(ji,:))
             WRITE(numout,*) 'SUMqsintveg_old:',SUM(qsintveg_old(ji,:))
             WRITE(numout,*) 'veget_max:',veget_max(ji,:)
             WRITE(numout,*) 'soiltile:',soiltile(ji,:)
             WRITE(numout,*) 'resdist:',resdist(ji,:)
             WRITE(numout,*) 'vmr:',vmr(ji,:)
             WRITE(numout,*) 'vmr_sum:',vmr_sum(ji)
             DO jst=1,nstm
                WRITE(numout,*) 'mc(',jst,'):',mc(ji,:,jst)
             ENDDO
             WRITE(numout,*) 'water2infilt:',water2infilt(ji,:)
             WRITE(numout,*) 'water2infilt_old:',water2infilt_old(ji,:)
             WRITE(numout,*) 'infil_dilu:',infil_dilu(ji)
             WRITE(numout,*) 'mc_dilu:',mc_dilu(ji,:)

             error=.TRUE.
             CALL ipslerr_p(2, 'hydrol_tmc_update', 'Error in water balance', 'We STOP in the end of this subroutine','')
          ENDIF
       ENDDO
    ENDIF

    !! Now that the work is done, update resdist
    resdist(:,:) = soiltile(:,:)

    !
    !!  Exit if error was found previously in this subroutine
    !
    IF ( error ) THEN
       WRITE(numout,*) 'One or more errors have been detected in hydrol_tmc_update. Model stops.'
       CALL ipslerr_p(3, 'hydrol_tmc_update', 'We will STOP now.',&
                  & 'One or several fatal errors were found previously.','')
    END IF

    IF (printlev>=3) WRITE (numout,*) ' hydrol_tmc_update done '

  END SUBROUTINE hydrol_tmc_update


  SUBROUTINE hydrol_rotation_update( ip, kjpindex, rot_matrix, old_veget_max, veget_max, soiltile, qsintveg )

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                          :: ip, kjpindex      !! domain size
    REAL(r_std),DIMENSION (nvm), INTENT (in)            :: old_veget_max     !! max fraction of vegetation type
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: veget_max     !! max fraction of vegetation type
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in) :: soiltile      !! Fraction of each soil tile (0-1, unitless)
    REAL(r_std), DIMENSION (nvm, nvm), INTENT(in)       :: rot_matrix    !! rotation matrix

    !! 0.3 Modified variables
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)   :: qsintveg   !! Amount of water in the canopy interception 
!    REAL(r_std), DIMENSION (kjpindex, nstm), INTENT(inout) :: resdist    !! Soiltile from previous time-step
!  resdist is defined in MODULE hydrol, no need to be put in the argument list

    !! 0.4 Local variables
    INTEGER(i_std)                           :: ji, jv, jst, jst1, jst2, jsl, jsrc, jtar
    LOGICAL                                  :: soil_upd        !! True if soiltile changed since last time step
    LOGICAL                                  :: error=.FALSE.   !! If true, exit in the end of subroutine
!!    REAL(r_std), DIMENSION(kjpindex,nstm)    :: vmr             !! Change in soiltile
!!    REAL(r_std), DIMENSION(kjpindex)         :: vmr_sum
!!    REAL(r_std), DIMENSION(kjpindex,nslm)    :: mc_dilu         !! Total loss of moisture content
!!    REAL(r_std), DIMENSION(kjpindex)         :: infil_dilu      !! Total loss for water2infilt
!!    REAL(r_std), DIMENSION(kjpindex,nstm)    :: tmc_old         !! tmc before calculations
!!    REAL(r_std), DIMENSION(kjpindex,nstm)    :: water2infilt_old!! water2infilt before calculations
!!    REAL(r_std), DIMENSION (kjpindex,nvm)    :: qsintveg_old    !! qsintveg before calculations
!!    REAL(r_std), DIMENSION(kjpindex)         :: test

    REAL(r_std), DIMENSION(nslm,nstm)    :: mc_dilu         !! Total loss of moisture content
    REAL(r_std), DIMENSION(nslm,nstm)    :: mc_old          !! temporary file to store mc
    REAL(r_std), DIMENSION(nstm)         :: infil_dilu      !! Total loss for water2infilt
    REAL(r_std), DIMENSION(nstm)    :: tmc_old         !! tmc before calculations
    REAL(r_std), DIMENSION(nstm)    :: water2infilt_old!! water2infilt before calculations
    REAL(r_std), DIMENSION(nvm)     :: qsintveg_old    !! qsintveg before calculations
    REAL(r_std), DIMENSION(nvm)     :: maxfrac, maxfrac_new
    REAL(r_std), DIMENSION(nstm,nstm) :: rot_matrix_tile !! relative portion of soil tile to be transferred
    REAL(r_std)                     :: test

    !! 0. Check if soiltiles changed since last time step
!    soil_upd=SUM(ABS(soiltile(:,:)-resdist(:,:))) .GT. zero
    maxfrac = old_veget_max(:)
    maxfrac_new = old_veget_max(:)
    rot_matrix_tile(:,:) = 0.0
    DO jsrc = 1,nvm
        DO jtar = 1,nvm
            IF (rot_matrix(jsrc,jtar) .GT. 0.0) THEN
                maxfrac_new(jtar) = maxfrac_new(jtar) + maxfrac(jsrc) * rot_matrix(jsrc,jtar)
                maxfrac_new(jsrc) = maxfrac_new(jsrc) - maxfrac(jsrc) * rot_matrix(jsrc,jtar)
                jst1 = pref_soil_veg(jsrc)
                jst2 = pref_soil_veg(jtar)
                rot_matrix_tile(jst1,jst2) = rot_matrix_tile(jst1,jst2) + &
                    rot_matrix(jsrc,jtar) * maxfrac(jsrc) / resdist(ip, jst1 )
                IF ( (rot_matrix_tile(jst1,jst2) .GT. 1.0) .OR. &
                     (rot_matrix_tile(jst1,jst2) .LE. 0.0) ) THEN
                    !!!! make sure the fraction is in (0,1)
                    WRITE(numout,*) 'pref_soil_veg',pref_soil_veg
                    WRITE(numout,*) 'jsrc, jtar,', jsrc, jtar
                    WRITE(numout,*) 'jst1, jst2,', jst1, jst2
                    WRITE(numout,*) 'maxfrac(jsrc), rot_matrix(jsrc,jtar)', maxfrac(jsrc), rot_matrix(jsrc,jtar)
                    WRITE(numout,*) 'pref_soil_veg(jsrc), resdist(ip,pref_soil_veg(jsrc))', pref_soil_veg(jsrc), resdist(ip,pref_soil_veg(jsrc))
                    STOP 'soiltile error in hydrol_rotation'
                ENDIF
            ENDIF
        ENDDO
    ENDDO

    !!!! check if the vegetation conversion is successful
    IF ( SUM(ABS(maxfrac_new - veget_max(ip,:))) .GT. min_sechiba ) THEN
        WRITE(numout,*) 'maxfrac',maxfrac
        WRITE(numout,*) 'maxfrac_new', maxfrac_new
        WRITE(numout,*) 'veget_max(ip,:)', veget_max(ip,:)
        STOP 'hydrol_rotation: fraction conversion error'
    ENDIF
    IF (printlev>=4) THEN 
        WRITE(numout,*) 'xuhui: hydrol_rotation' 
        WRITE(numout,*) 'resdist(ip,:)', resdist(ip,:)
        WRITE(numout,*) 'soiltile(ip,:)', soiltile(ip,:)
        DO jsrc = 1,nstm 
            WRITE(numout,*) 'jsrc, rot_matrix_tile(jsrc,:)', jsrc, rot_matrix_tile(jsrc,:)
        ENDDO
    ENDIF

    IF (SUM(rot_matrix) .GT. 0) THEN
        soil_upd = .TRUE.
    ENDIF
    IF (printlev>=3) WRITE (numout,*) 'soil_upd ', soil_upd

    IF (check_cwrr) THEN
       ! Save soil moisture for later use
       tmc_old(:) = tmc(ip,:) 
       water2infilt_old(:) = water2infilt(ip,:)
       qsintveg_old(:) = qsintveg(ip,:)
    ENDIF

    !! 1. If a PFT has disapperead as result from a veget_max change, 
    !!    then add canopy water to surface water.
     DO jv=1,nvm
        IF ( (maxfrac_new(jv) .LT. min_sechiba) .AND.  (qsintveg(ip, jv) .GT. 0.0) )  THEN
            jst = pref_soil_veg(jv)
            water2infilt(ip,jst) = water2infilt(ip,jst) + qsintveg(ip,jv)/maxfrac(jv)
            qsintveg(ip,jv) = zero
        ENDIF
     ENDDO
    
    mc_old = mc(ip,:,:)
    water2infilt_old(:) = water2infilt(ip,:)
    !! 2. Compute new soil moisture if soiltile changed
    IF (soil_upd) THEN
        DO jtar = 1,nstm
          mc_dilu(:,:) = zero
          infil_dilu(:) = zero
          IF ( SUM(rot_matrix_tile(:,jtar)) .GT. min_sechiba ) THEN
            DO jsrc = 1,nstm
              IF ( rot_matrix_tile(jsrc,jtar) .GT. min_sechiba ) THEN
                mc_dilu(:,jsrc) = mc_old(:,jsrc)
                infil_dilu(jsrc) = water2infilt_old(jsrc)
              ENDIF ! rot_matrix_tile(jsrc,jtar) > 0
            ENDDO
            !!! actually do the rotation
            mc(ip,:,jtar) = mc_old(:,jtar) * resdist(ip,jtar) * (1.0 - SUM(rot_matrix_tile(jtar,:)))
            water2infilt(ip,jtar) = water2infilt_old(jtar) * resdist(ip,jtar) * (1.0 - SUM(rot_matrix_tile(jtar,:)))
            DO jsrc = 1,nstm
                mc(ip,:,jtar) = mc(ip,:,jtar) + resdist(ip,jsrc) * rot_matrix_tile(jsrc,jtar) * mc_dilu(:,jsrc)
                water2infilt(ip,jtar) = water2infilt(ip,jtar) + resdist(ip,jsrc) * rot_matrix_tile(jsrc,jtar) * infil_dilu(jsrc)
            ENDDO
            IF ( soiltile(ip,jtar) .LE. 0. ) THEN
                WRITE(numout,*) 'jtar, soiltile(ip,jtar)',jtar, soiltile(ip,jtar)
                STOP 'hydrol_rotation_update: target tile has no proportion'
            ENDIF
            mc(ip,:,jtar) = mc(ip,:,jtar) / soiltile(ip,jtar)
            water2infilt(ip,jtar) = water2infilt(ip,jtar) / soiltile(ip,jtar)
          ENDIF ! SUM(rot_matrix_tile(:,jtar)) > 0
        ENDDO

       ! 2.3.3 Case where soiltile < min_sechiba 
       DO jst=1,nstm
          IF ( soiltile(ip,jst) .LT. min_sechiba ) THEN
             water2infilt(ip,jst) = zero
             mc(ip,:,jst) = zero
          ENDIF
       ENDDO

       IF (printlev>=4) THEN
            WRITE(numout,*) 'mc_old(1,:)',mc_old(1,:)
            WRITE(numout,*) 'mc(ip,1,:)',mc(ip,1,:)
            WRITE(numout,*) 'water2infilt_old(:)', water2infilt_old(:)
            WRITE(numout,*) 'water2infilt(ip,:)', water2infilt(ip,:)
       ENDIF


    ENDIF ! soil_upd 


    !2.3.3 we compute tmc(kjpindex,nstm) and humtot!
    DO jst=1,nstm
         tmc(ip,jst) = dz(2) * ( trois*mc(ip,1,jst) + mc(ip,2,jst) )/huit
         DO jsl = 2,nslm-1
            tmc(ip,jst) = tmc(ip,jst) + dz(jsl) * (trois*mc(ip,jsl,jst)+mc(ip,jsl-1,jst))/huit &
                 + dz(jsl+1) * (trois*mc(ip,jsl,jst)+mc(ip,jsl+1,jst))/huit
         ENDDO
         tmc(ip,jst) = tmc(ip,jst) + dz(nslm) * (trois*mc(ip,nslm,jst) + mc(ip,nslm-1,jst))/huit
         tmc(ip,jst) = tmc(ip,jst) + water2infilt(ip,jst)
         ! WARNING tmc is increased by water2infilt(ip,jst), but mc is not modified !
    ENDDO

    humtot(ip) = zero
    DO jst=1,nstm
        humtot(ip) = humtot(ip) + soiltile(ip,jst) * tmc(ip,jst)
    ENDDO

    !! 4 check
    IF (check_cwrr) THEN
!       DO ji=1,kjpindex
        ji = ip
          test = ABS(SUM(tmc(ji,:)*soiltile(ji,:)) - SUM(tmc_old(:)*resdist(ji,:)) + &
               SUM(qsintveg(ji,:)) - SUM(qsintveg_old(:))) ! sum(soiltile)=1
          IF ( test .GT.  allowed_err ) THEN
             WRITE(numout,*) 'hydrol_rotation_update WRONG: ji',ji
             WRITE(numout,*) 'tot water before:',SUM(tmc_old(:)*resdist(ji,:)) + SUM(qsintveg_old(:))
             WRITE(numout,*) 'tot water after:',SUM(tmc(ji,:)*soiltile(ji,:)) + SUM(qsintveg(ji,:))
             WRITE(numout,*) 'err:',test
             WRITE(numout,*) 'allowed_err:',allowed_err
             WRITE(numout,*) 'tmc:',tmc(ji,:)
             WRITE(numout,*) 'tmc_old:',tmc_old(:)
             WRITE(numout,*) 'qsintveg:',qsintveg(ji,:)
             WRITE(numout,*) 'qsintveg_old:',qsintveg_old(:)
             WRITE(numout,*) 'SUMqsintveg:',SUM(qsintveg(ji,:))
             WRITE(numout,*) 'SUMqsintveg_old:',SUM(qsintveg_old(:))
             WRITE(numout,*) 'veget_max:',veget_max(ji,:)
             WRITE(numout,*) 'soiltile:',soiltile(ji,:)
             WRITE(numout,*) 'resdist:',resdist(ji,:)
             DO jst=1,nstm
                WRITE(numout,*) 'mc(',jst,'):',mc(ji,:,jst)
             ENDDO
             WRITE(numout,*) 'water2infilt:',water2infilt(ji,:)
             WRITE(numout,*) 'water2infilt_old:',water2infilt_old(:)

             error=.TRUE.
             CALL ipslerr_p(2, 'hydrol_rotation_update', 'Error in water balance', 'We STOP in the end of this subroutine','')
          ENDIF
!       ENDDO
    ENDIF

    !! Now that the work is done, update resdist
    resdist(:,:) = soiltile(:,:)

    !
    !!  Exit if error was found previously in this subroutine
    !
    IF ( error ) THEN
       WRITE(numout,*) 'One or more errors have been detected in hydrol_tmc_update. Model stops.'
       CALL ipslerr_p(3, 'hydrol_tmc_update', 'We will STOP now.',&
                  & 'One or several fatal errors were found previously.','')
    END IF

    IF (printlev>=3) WRITE (numout,*) ' hydrol_rotation_update done '

  END SUBROUTINE hydrol_rotation_update

!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_var_init
!!
!>\BRIEF        This routine initializes hydrologic parameters to define K and D, and diagnostic hydrologic variables.  
!!
!! DESCRIPTION  :
!! - 1 compute the depths
!! - 2 compute the profile for roots
!! - 3 compute the profile for ksat, a and n Van Genuchten parameter
!! - 4 compute the linearized values of k, a, b and d for the resolution of Fokker Planck equation
!! - 5 water reservoirs initialisation
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_var_init

  SUBROUTINE hydrol_var_init (kjpindex, veget, veget_max, soiltile, njsc, &
       mx_eau_var, shumdiag_perma, k_litt, &
       drysoil_frac, qsintveg, mc_layh, mcl_layh, tmc_layh, mc_layh_s, mcl_layh_s, tmc_layh_s) 

    ! interface description

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    ! input scalar 
    INTEGER(i_std), INTENT(in)                          :: kjpindex      !! Domain size (number of grid cells) (1)
    ! input fields
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: veget_max     !! PFT fractions within grid-cells (1; 1)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: veget         !! Effective fraction of vegetation by PFT (1; 1)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)    :: njsc          !! Index of the dominant soil textural class 
                                                                         !! in the grid cell (1-nscm, unitless) 
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in) :: soiltile      !! Fraction of each soil tile (0-1, unitless)

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: mx_eau_var    !! Maximum water content of the soil 
                                                                         !! @tex $(kg m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out) :: shumdiag_perma!! Percent of porosity filled with water (mc/mcs)
                                                                         !! used for the thermal computations
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: k_litt        !! Mean litter hydraulic conductivity
                                                                         !! @tex $(mm d^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)    :: drysoil_frac  !! function of litter humidity
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out):: mc_layh       !! Volumetric soil moisture content for each layer in hydrol(liquid+ice) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm), INTENT (out):: mc_layh_s   !! Volumetric soil moisture content for each layer in hydrol(liquid+ice) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out):: mcl_layh      !! Volumetric soil moisture content for each layer in hydrol(liquid) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm), INTENT (out):: mcl_layh_s  !! Volumetric soil moisture content for each layer in hydrol(liquid) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out):: tmc_layh      !! Total soil moisture content for each layer in hydrol(liquid+ice) [mm]
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm), INTENT(out)  :: tmc_layh_s  !! total soil moisture content for each layer in hydrol and for each soiltile (mm)

    !! 0.3 Modified variables
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: qsintveg    !! Water on vegetation due to interception
                                                                         !! @tex $(kg m^{-2})$ @endtex  

    !! 0.4 Local variables

    INTEGER(i_std)                                      :: ji, jv        !! Grid-cell and PFT indices (1)
    INTEGER(i_std)                                      :: jst, jsc, jsl !! Soiltile, Soil Texture, and Soil layer indices (1)
    INTEGER(i_std)                                      :: i, jd         !! Index (1)
    REAL(r_std)                                         :: m             !! m=1-1/n (unitless)
    REAL(r_std)                                         :: frac          !! Relative linearized VWC (unitless)
    REAL(r_std)                                         :: avan_mod      !! VG parameter a modified from  exponantial profile
                                                                         !! @tex $(mm^{-1})$ @endtex
    REAL(r_std)                                         :: nvan_mod      !! VG parameter n  modified from  exponantial profile
                                                                         !! (unitless)
    REAL(r_std), DIMENSION(nslm,nscm)                   :: afact, nfact  !! Multiplicative factor for decay of a and n with depth
                                                                         !! (unitless)
    ! parameters for "soil densification" with depth
    REAL(r_std)                                         :: dp_comp       !! Depth at which the 'compacted' value of ksat
                                                                         !! is reached (m)
    REAL(r_std)                                         :: f_ks          !! Exponential factor for decay of ksat with depth 
                                                                         !! @tex $(m^{-1})$ @endtex 
    ! Fixed parameters from fitted relationships
    REAL(r_std)                                         :: n0            !! fitted value for relation log((n-n0)/(n_ref-n0)) = 
                                                                         !! nk_rel * log(k/k_ref) 
                                                                         !! (unitless)
    REAL(r_std)                                         :: nk_rel        !! fitted value for relation log((n-n0)/(n_ref-n0)) = 
                                                                         !! nk_rel * log(k/k_ref) 
                                                                         !! (unitless)
    REAL(r_std)                                         :: a0            !! fitted value for relation log((a-a0)/(a_ref-a0)) = 
                                                                         !! ak_rel * log(k/k_ref) 
                                                                         !! @tex $(mm^{-1})$ @endtex
    REAL(r_std)                                         :: ak_rel        !! fitted value for relation log((a-a0)/(a_ref-a0)) = 
                                                                         !! ak_rel * log(k/k_ref)
                                                                         !! (unitless) 
    REAL(r_std)                                         :: kfact_max     !! Maximum factor for Ks decay with depth (unitless)
    REAL(r_std)                                         :: k_tmp, tmc_litter_ratio
    INTEGER(i_std), PARAMETER                           :: error_level = 3 !! Error level for consistency check
                                                                           !! Switch to 2 tu turn fatal errors into warnings
    INTEGER(i_std)                                      :: jiref           !! To identify the mc_lins where k_lin and d_lin 
                                                                           !! need special treatment

!_ ================================================================================================================================

!!??Aurelien: Les 3 parametres qui suivent pourait peut-être mis dans hydrol_init?
    !
    !
    !Config Key   = CWRR_NKS_N0 
    !Config Desc  = fitted value for relation log((n-n0)/(n_ref-n0)) = nk_rel * log(k/k_ref)
    !Config Def   = 0.95
    !Config If    = HYDROL_CWRR 
    !Config Help  =
    !Config Units = [-]
    n0 = 0.95
    CALL getin_p("CWRR_NKS_N0",n0)

    !! Check parameter value (correct range)
    IF ( n0 < zero ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for CWRR_NKS_N0.", &
            &     "This parameter should be non-negative. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = CWRR_NKS_POWER
    !Config Desc  = fitted value for relation log((n-n0)/(n_ref-n0)) = nk_rel * log(k/k_ref)
    !Config Def   = 0.34
    !Config If    = HYDROL_CWRR 
    !Config Help  =
    !Config Units = [-]
    nk_rel = 0.34
    CALL getin_p("CWRR_NKS_POWER",nk_rel)

    !! Check parameter value (correct range)
    IF ( nk_rel < zero ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for CWRR_NKS_POWER.", &
            &     "This parameter should be non-negative. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = CWRR_AKS_A0 
    !Config Desc  = fitted value for relation log((a-a0)/(a_ref-a0)) = ak_rel * log(k/k_ref)
    !Config Def   = 0.00012
    !Config If    = HYDROL_CWRR 
    !Config Help  =
    !Config Units = [1/mm]
    a0 = 0.00012
    CALL getin_p("CWRR_AKS_A0",a0)

    !! Check parameter value (correct range)
    IF ( a0 < zero ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for CWRR_AKS_A0.", &
            &     "This parameter should be non-negative. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = CWRR_AKS_POWER
    !Config Desc  = fitted value for relation log((a-a0)/(a_ref-a0)) = ak_rel * log(k/k_ref)
    !Config Def   = 0.53
    !Config If    = HYDROL_CWRR 
    !Config Help  =
    !Config Units = [-]
    ak_rel = 0.53
    CALL getin_p("CWRR_AKS_POWER",ak_rel)

    !! Check parameter value (correct range)
    IF ( nk_rel < zero ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for CWRR_AKS_POWER.", &
            &     "This parameter should be non-negative. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = KFACT_DECAY_RATE
    !Config Desc  = Factor for Ks decay with depth
    !Config Def   = 2.0
    !Config If    = HYDROL_CWRR 
    !Config Help  =  
    !Config Units = [1/m]
    f_ks = 2.0
    CALL getin_p ("KFACT_DECAY_RATE", f_ks)

    !! Check parameter value (correct range)
    IF ( f_ks <= zero ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for KFACT_DECAY_RATE.", &
            &     "This parameter should be positive. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = KFACT_STARTING_DEPTH
    !Config Desc  = Depth for compacted value of Ks 
    !Config Def   = 0.3
    !Config If    = HYDROL_CWRR 
    !Config Help  =  
    !Config Units = [m]
    dp_comp = 0.3
    CALL getin_p ("KFACT_STARTING_DEPTH", dp_comp)

    !! Check parameter value (correct range)
    IF ( dp_comp <= zero ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for KFACT_STARTING_DEPTH.", &
            &     "This parameter should be positive. ", &
            &     "Please, check parameter value in run.def. ")
    END IF


    !Config Key   = KFACT_MAX
    !Config Desc  = Maximum Factor for Ks increase due to vegetation
    !Config Def   = 10.0
    !Config If    = HYDROL_CWRR 
    !Config Help  =
    !Config Units = [-]
    kfact_max = 10.0
    CALL getin_p ("KFACT_MAX", kfact_max)

    !! Check parameter value (correct range)
    IF ( kfact_max < 10. ) THEN
       CALL ipslerr_p(error_level, "hydrol_var_init.", &
            &     "Wrong parameter value for KFACT_MAX.", &
            &     "This parameter should be greater than 10. ", &
            &     "Please, check parameter value in run.def. ")
    END IF

    
    !-
    !! 1 Depths are stored in module vertical_soil_var
    !-
    ! Transform from m into mm
    DO jsl=1,nslm
       zz(jsl) = znh(jsl)*mille
       dz(jsl) = dnh(jsl)*mille
       dh(jsl) = dlh(jsl)*mille
    ENDDO
    zz(nslm+1) = zz(nslm)

    ! ** AD ** there are a lot of confusions among soiltiles and texture classes !
    DO jst=1,nstm ! ** AD ** loop on soiltiles ; here it shoud be independent from the soiltiles

       !-
       !! 2 compute the root density profile
       !-
       !! The three following equations concerning nroot computation are derived from the integrals 
       !! of equations C9 to C11 of De Rosnay's (1999) PhD thesis (page 158).
       !! The occasional absence of minus sign before humcste parameter is correct.
       DO jv = 1,nvm
          DO jsl = 2, nslm-1
             nroot(jv,jst,jsl) = (EXP(-humcste(jv)*zz(jsl)/mille)) * &
                     & (EXP(humcste(jv)*dz(jsl)/mille/deux) - &
                     & EXP(-humcste(jv)*dz(jsl+1)/mille/deux))/ &
                     & (EXP(-humcste(jv)*dz(2)/mille/deux) &
                     & -EXP(-humcste(jv)*zz(nslm)/mille))
          ENDDO
       ENDDO
       DO jv=1,nvm
          nroot(jv,jst,1) = zero
          nroot(jv,jst,nslm) = (EXP(humcste(jv)*dz(nslm)/mille/deux) -un) * &
                  & EXP(-humcste(jv)*zz(nslm)/mille) / &
                  & (EXP(-humcste(jv)*dz(2)/mille/deux) &
                  & -EXP(-humcste(jv)*zz(nslm)/mille))
       ENDDO
    ENDDO

       !! An exponential factor is used to increase ks near the surface depending on the amount of roots in the soil 
       !! through a geometric average over the vegets
       !! This comes from the PhD thesis of d'Orgeval, 2006, p82; d'Orgeval et al. 2008, Eqs. 3-4
       !! (Calibrated against Hapex-Sahel measurements)
    kfact_root(:,:,:) = un
    DO jsl = 1, nslm
       DO jv = 2, nvm
          jst = pref_soil_veg(jv)
          DO ji = 1, kjpindex
             IF(soiltile(ji,jst) .GT. min_sechiba) THEN
                kfact_root(ji,jsl,jst) = kfact_root(ji,jsl,jst) * &
                     & MAX((MAXVAL(ks_usda)/ks(njsc(ji)))**(- veget_max(ji,jv)/2 * (humcste(jv)*zz(jsl)/mille - un)/deux), &
                     un) 
             ENDIF
          ENDDO
       ENDDO
    ENDDO
    !-
    !! 3 Compute the profile for ksat, a and n
    !-

    ! For every soil texture
    DO jsc = 1, nscm ! ** AD ** loop on soil textures, using zz defined on soiltiles
       DO jsl=1,nslm
          ! PhD thesis of d'Orgeval, 2006, p81, Eq. 4.38; d'Orgeval et al. 2008, Eq. 2
          ! Calibrated against Hapex-Sahel measurements
          kfact(jsl,jsc) = MIN(MAX(EXP(- f_ks * (zz(jsl)/mille - dp_comp)), un/kfact_max),un)
          ! PhD thesis of d'Orgeval, 2006, p81, Eqs. 4.39; 4.42, and Fig 4.14 
          
          nfact(jsl,jsc) = ( kfact(jsl,jsc) )**nk_rel
          afact(jsl,jsc) = ( kfact(jsl,jsc) )**ak_rel
       ENDDO
    ENDDO

    ! For every soil texture
    DO jsc = 1, nscm ! ** AD ** loop on soil textures, which is good here
       !-
       !! 4 compute the linearized values of k, a, b and d
       !-
       ! Calculate the matrix coef for Dublin model (de Rosnay, 1999; p149)
       ! piece-wise linearised hydraulic conductivity k_lin=alin * mc_lin + b_lin
       ! and diffusivity d_lin in each interval of mc, called mc_lin,
       ! between imin, for residual mcr, and imax for saturation mcs.

       ! We define 51 bounds for 50 bins of mc between mcr and mcs
       mc_lin(imin,jsc)=mcr(jsc)
       mc_lin(imax,jsc)=mcs(jsc)
       DO ji= imin+1, imax-1 ! ji=2,50
          mc_lin(ji,jsc) = mcr(jsc) + (ji-imin)*(mcs(jsc)-mcr(jsc))/(imax-imin)
       ENDDO

       DO jsl = 1, nslm
          ! From PhD thesis of d'Orgeval, 2006, p81, Eq. 4.42
          nvan_mod = n0 + (nvan(jsc)-n0) * nfact(jsl,jsc)
          avan_mod = a0 + (avan(jsc)-a0) * afact(jsl,jsc)
          m = un - un / nvan_mod
          ! We apply Van Genuchten equation for K(theta) based on Ks(z)=ks(jsc) * kfact(jsl,jsc)
          DO ji = imax,imin,-1 
             frac=MIN(un,(mc_lin(ji,jsc)-mcr(jsc))/(mcs(jsc)-mcr(jsc)))
             k_lin(ji,jsl,jsc) = ks(jsc) * kfact(jsl,jsc) * (frac**0.5) * ( un - ( un - frac ** (un/m)) ** m )**2
          ENDDO

          ! k_lin should not be zero, nor too small
          ! We track jiref, the bin under which mc is too small and we may get zero k_lin     
          ji=imax-1
          DO WHILE ((k_lin(ji,jsl,jsc) > 1.e-32) .and. (ji>0))
             jiref=ji
             ji=ji-1
          ENDDO
          DO ji=jiref-1,imin,-1
             k_lin(ji,jsl,jsc)=k_lin(ji+1,jsl,jsc)/10.
          ENDDO
         
          DO ji = imin,imax-1 ! ji=1,50
             ! We deduce a_lin and b_lin based on continuity between segments k_lin = a_lin*mc-lin+b_lin
             a_lin(ji,jsl,jsc) = (k_lin(ji+1,jsl,jsc)-k_lin(ji,jsl,jsc)) / (mc_lin(ji+1,jsc)-mc_lin(ji,jsc))
             b_lin(ji,jsl,jsc)  = k_lin(ji,jsl,jsc) - a_lin(ji,jsl,jsc)*mc_lin(ji,jsc)

             ! We calculate the d_lin for each mc bin, from Van Genuchten equation for D(theta)
             ! d_lin is constant and taken as the arithmetic mean between the values at the bounds of each bin 
             IF (ji.NE.imin .AND. ji.NE.imax-1) THEN
                frac=MIN(un,(mc_lin(ji,jsc)-mcr(jsc))/(mcs(jsc)-mcr(jsc)))
                d_lin(ji,jsl,jsc) =(k_lin(ji,jsl,jsc) / (avan_mod*m*nvan_mod)) *  &
                     ( (frac**(-un/m))/(mc_lin(ji,jsc)-mcr(jsc)) ) * &
                     (  frac**(-un/m) -un ) ** (-m)
                frac=MIN(un,(mc_lin(ji+1,jsc)-mcr(jsc))/(mcs(jsc)-mcr(jsc)))
                d_lin(ji+1,jsl,jsc) =(k_lin(ji+1,jsl,jsc) / (avan_mod*m*nvan_mod))*&
                     ( (frac**(-un/m))/(mc_lin(ji+1,jsc)-mcr(jsc)) ) * &
                     (  frac**(-un/m) -un ) ** (-m)
                d_lin(ji,jsl,jsc) = undemi * (d_lin(ji,jsl,jsc)+d_lin(ji+1,jsl,jsc))
             ELSE IF(ji.EQ.imax-1) THEN
                d_lin(ji,jsl,jsc) =(k_lin(ji,jsl,jsc) / (avan_mod*m*nvan_mod)) * &
                     ( (frac**(-un/m))/(mc_lin(ji,jsc)-mcr(jsc)) ) *  &
                     (  frac**(-un/m) -un ) ** (-m)
             ENDIF
          ENDDO

          ! Special case for ji=imin
          d_lin(imin,jsl,jsc) = d_lin(imin+1,jsl,jsc)/1000.

          ! We adjust d_lin where k_lin was previously adjusted otherwise we might get non-monotonous variations
          ! We don't want d_lin = zero
          DO ji=jiref-1,imin,-1
             d_lin(ji,jsl,jsc)=d_lin(ji+1,jsl,jsc)/10.
          ENDDO

       ENDDO
    ENDDO
    

    !! 5 Water reservoir initialisation
    !
!!$    DO jst = 1,nstm
!!$       DO ji = 1, kjpindex
!!$          mx_eau_var(ji) = mx_eau_var(ji) + soiltile(ji,jst)*&
!!$               &   zmaxh*mille*mcs(njsc(ji))
!!$       END DO
!!$    END DO
!!$    IF (check_CWRR) THEN
!!$       IF ( ANY ( ABS( mx_eau_var(:) - zmaxh*mille*mcs(njsc(:)) ) > min_sechiba ) ) THEN
!!$          ji=MAXLOC ( ABS( mx_eau_var(:) - zmaxh*mille*mcs(njsc(:)) ) , 1)
!!$          WRITE(numout, *) "Erreur formule simplifiÃ©e mx_eau_var ! ", mx_eau_var(ji), zmaxh*mille*mcs(njsc(ji))
!!$          WRITE(numout, *) "err = ",ABS(mx_eau_var(ji) - zmaxh*mille*mcs(njsc(ji)))
!!$          STOP 1
!!$       ENDIF
!!$    ENDIF

    mx_eau_var(:) = zero
    mx_eau_var(:) = zmaxh*mille*mcs(njsc(:)) 

    DO ji = 1,kjpindex 
       IF (vegtot(ji) .LE. zero) THEN
          mx_eau_var(ji) = mx_eau_nobio*zmaxh
          ! Aurelien: what does vegtot=0 mean? is it like frac_nobio=1? But if 0<frac_nobio<1 ???
       ENDIF

    END DO

    ! Compute the litter humidity, shumdiag_perma and fry
    k_litt(:) = zero
    tmc_litt_mea(:) = zero
    tmc_litt_wet_mea(:) = zero
    tmc_litt_dry_mea(:) = zero
    shumdiag_perma(:,:) = zero
    humtot(:) = zero
    tmc(:,:) = zero

    ! Loop on soiltiles to compute the variables (ji,jst)
    DO jst=1,nstm 
       DO ji = 1, kjpindex
          tmcs(ji,jst)=zmaxh* mille*mcs(njsc(ji))
          tmcr(ji,jst)=zmaxh* mille*mcr(njsc(ji))
       ENDDO
    ENDDO
       
    ! The total soil moisture for each soiltile:
    DO jst=1,nstm
       DO ji=1,kjpindex
          tmc(ji,jst)= dz(2) * ( trois*mc(ji,1,jst)+ mc(ji,2,jst))/huit
       END DO
    ENDDO

    DO jst=1,nstm 
       DO jsl=2,nslm-1
          DO ji=1,kjpindex
             tmc(ji,jst) = tmc(ji,jst) + dz(jsl) * ( trois*mc(ji,jsl,jst) + mc(ji,jsl-1,jst))/huit &
                  & + dz(jsl+1)*(trois*mc(ji,jsl,jst) + mc(ji,jsl+1,jst))/huit
          END DO
       END DO
    ENDDO

    DO jst=1,nstm 
       DO ji=1,kjpindex
          tmc(ji,jst) = tmc(ji,jst) +  dz(nslm) * (trois * mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
          tmc(ji,jst) = tmc(ji,jst) + water2infilt(ji,jst)
       ENDDO
    END DO

!JG: hydrol_tmc_update should not be called in the initialization phase. Call of hydrol_tmc_update makes the model restart differenlty.    
!    ! If veget has been updated before restart (with LAND USE or DGVM),
!    ! tmc and mc must be modified with respect to humtot conservation.
!   CALL hydrol_tmc_update ( kjpindex, veget_max, soiltile, qsintveg, resdist )

    ! The litter variables:
    ! level 1
    DO jst=1,nstm 
       DO ji=1,kjpindex
          tmc_litter(ji,jst) = dz(2) * (trois*mc(ji,1,jst)+mc(ji,2,jst))/huit
          tmc_litter_wilt(ji,jst) = dz(2) * mcw(njsc(ji)) / deux
          tmc_litter_res(ji,jst) = dz(2) * mcr(njsc(ji)) / deux
          tmc_litter_field(ji,jst) = dz(2) * mcf(njsc(ji)) / deux
          tmc_litter_sat(ji,jst) = dz(2) * mcs(njsc(ji)) / deux
          tmc_litter_awet(ji,jst) = dz(2) * mc_awet(njsc(ji)) / deux
          tmc_litter_adry(ji,jst) = dz(2) * mc_adry(njsc(ji)) / deux
       ENDDO
    END DO
    ! sum from level 2 to 4
    DO jst=1,nstm 
       DO jsl=2,4
          DO ji=1,kjpindex
             tmc_litter(ji,jst) = tmc_litter(ji,jst) + dz(jsl) * & 
                  & ( trois*mc(ji,jsl,jst) + mc(ji,jsl-1,jst))/huit &
                  & + dz(jsl+1)*(trois*mc(ji,jsl,jst) + mc(ji,jsl+1,jst))/huit
             tmc_litter_wilt(ji,jst) = tmc_litter_wilt(ji,jst) + &
                  &(dz(jsl)+ dz(jsl+1))*& 
                  & mcw(njsc(ji))/deux
             tmc_litter_res(ji,jst) = tmc_litter_res(ji,jst) + &
                  &(dz(jsl)+ dz(jsl+1))*& 
                  & mcr(njsc(ji))/deux
             tmc_litter_sat(ji,jst) = tmc_litter_sat(ji,jst) + &
                  &(dz(jsl)+ dz(jsl+1))* & 
                  & mcs(njsc(ji))/deux
             tmc_litter_field(ji,jst) = tmc_litter_field(ji,jst) + &
                  & (dz(jsl)+ dz(jsl+1))* & 
                  & mcf(njsc(ji))/deux
             tmc_litter_awet(ji,jst) = tmc_litter_awet(ji,jst) + &
                  &(dz(jsl)+ dz(jsl+1))* & 
                  & mc_awet(njsc(ji))/deux
             tmc_litter_adry(ji,jst) = tmc_litter_adry(ji,jst) + &
                  & (dz(jsl)+ dz(jsl+1))* & 
                  & mc_adry(njsc(ji))/deux
          END DO
       END DO
    END DO

    ! Soil wetness profiles (mc-mcw)/(mcs-mcw)
    DO jst=1,nstm 
       DO ji=1,kjpindex
          soil_wet(ji,1,jst) = MIN(un, MAX(zero,&
               &(trois*mc(ji,1,jst) + mc(ji,2,jst) - quatre*mcw(njsc(ji)))&
               & /(quatre*(mcs(njsc(ji))-mcw(njsc(ji)))) ))
    !!??Aurelien: a quoi sert cette ligne?
          humrelv(ji,1,jst) = zero
       ENDDO
    END DO

    DO jst=1,nstm 
       DO jsl=2,nslm-1
          DO ji=1,kjpindex
             soil_wet(ji,jsl,jst) = MIN(un, MAX(zero,&
                  & (trois*mc(ji,jsl,jst) + & 
                  & mc(ji,jsl-1,jst) *(dz(jsl)/(dz(jsl)+dz(jsl+1))) &
                  & + mc(ji,jsl+1,jst)*(dz(jsl+1)/(dz(jsl)+dz(jsl+1))) &
                  & - quatre*mcw(njsc(ji))) / (quatre*(mcs(njsc(ji))-mcw(njsc(ji)))) ))
          END DO
       END DO
    END DO

    DO jst=1,nstm 
       DO ji=1,kjpindex
          soil_wet(ji,nslm,jst) = MIN(un, MAX(zero,&
               & (trois*mc(ji,nslm,jst) &
               & + mc(ji,nslm-1,jst)-quatre*mcw(njsc(ji)))/(quatre*(mcs(njsc(ji))-mcw(njsc(ji)))) ))
       ENDDO
    END DO

    ! Compute the grid averaged values
    DO jst=1,nstm        
       DO ji=1,kjpindex
          !
          IF ( tmc_litter(ji,jst) < tmc_litter_res(ji,jst)) THEN
             i = imin
          ELSE
             tmc_litter_ratio = (tmc_litter(ji,jst)-tmc_litter_res(ji,jst)) / &
                  & (tmc_litter_sat(ji,jst)-tmc_litter_res(ji,jst))
             i= MAX(MIN(INT((imax-imin)*tmc_litter_ratio)+imin , imax-1), imin)
          ENDIF
          ! k_litt is an averaged conductivity for saturated infiltration in the 'litter' layer
          ! This is used for reinfiltration from surface water
          k_tmp = MAX(k_lin(i,1,njsc(ji))*ks(njsc(ji)), zero)
          k_litt(ji) = k_litt(ji) + soiltile(ji,jst) * SQRT(k_tmp)
       ENDDO
    ENDDO

    DO jst=1,nstm        
       DO ji=1,kjpindex
          tmc_litt_wet_mea(ji) =  tmc_litt_wet_mea(ji) + & 
               & tmc_litter_awet(ji,jst)* soiltile(ji,jst)

          tmc_litt_dry_mea(ji) = tmc_litt_dry_mea(ji) + &
               & tmc_litter_adry(ji,jst) * soiltile(ji,jst) 

          tmc_litt_mea(ji) = tmc_litt_mea(ji) + &
               & tmc_litter(ji,jst) * soiltile(ji,jst) 
       ENDDO
    ENDDO

    ! Caluculate frac_hydro_diag for interpolation between hydrological and diagnostic axes
    CALL hydrol_calculate_frac_hydro_diag

    ! Calculate shumdiag_perma
    ! Use resdist instead of soiltile because we here need to have 
    ! shumdiag at the value from previous time step. 
    DO jst=1,nstm 
       DO jd=1,nbdl
          DO ji=1,kjpindex
             DO jsl = 1, nslm
                shumdiag_perma(ji,jd)= shumdiag_perma(ji,jd)  &
                     + mc(ji,jsl,jst) *frac_hydro_diag(jsl,jd) &
                     /mcs(njsc(ji))*resdist(ji,jst)
             END DO
             shumdiag_perma(ji,jd) = MAX(MIN(shumdiag_perma(ji,jd), un), zero) 
          END DO
       END DO
    END DO
    
    ! Calculate soilmoist
    soilmoist(:,:) = zero
    DO jst=1,nstm
       DO ji=1,kjpindex
             soilmoist(ji,1) = soilmoist(ji,1) + soiltile(ji,jst) * &
                  dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
             DO jsl = 2,nslm-1
                soilmoist(ji,jsl) = soilmoist(ji,jsl) + soiltile(ji,jst) * &
                     ( dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                     + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit )
             END DO
             soilmoist(ji,nslm) = soilmoist(ji,nslm) + soiltile(ji,jst) * &
                  dz(nslm) * (trois*mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
       END DO
    END DO


    ! Calclate drysoil_frac if it was not found in the restart file
    IF (ALL(drysoil_frac(:) == val_exp)) THEN
       DO ji=1,kjpindex
          IF ( tmc_litt_wet_mea(ji) - tmc_litt_dry_mea(ji) > zero ) THEN
             drysoil_frac(ji) = un + MAX( MIN( (tmc_litt_dry_mea(ji) - tmc_litt_mea(ji)) / &
                  (tmc_litt_wet_mea(ji) - tmc_litt_dry_mea(ji)), zero), - un)
          ELSE
             drysoil_frac(ji) = zero
          ENDIF
       END DO
    END IF

    profil_froz_hydro(:,:) = 0.0
    profil_froz_hydro_ns(:,:,:) = 0.0
    temp_hydro(:,:) = 0.0
    
    !! Calculate the volumetric soil moisture content (mc_layh and mcl_layh) needed in 
    !! thermosoil for the thermal conductivity. Calculate also total soil moisture content(tmc_layh) 
    !! needed in thermosoil for the heat capacity.
    mc_layh(:,:) = zero
    mcl_layh(:,:) = zero
    tmc_layh(:,:) = zero
    mc_layh_s(:,:,:) = zero
    mcl_layh_s(:,:,:) = zero
    tmc_layh_s(:,:,:) = zero
    
    mc_layh_s = mc
    mcl_layh_s = mc
    DO jst=1,nstm
      DO ji=1,kjpindex
         DO jsl=1,nslm
            mc_layh(ji,jsl) = mc_layh(ji,jsl) + mc(ji,jsl,jst) * soiltile(ji,jst) 
            mcl_layh(ji,jsl) = mc_layh(ji,jsl)
         ENDDO
         tmc_layh_s(ji,1,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit 
         DO jsl = 2,nslm-1
            tmc_layh_s(ji,jsl,jst) = dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit 
         ENDDO
         tmc_layh_s(ji,nslm,jst) = dz(nslm) * (trois*mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
         DO jsl = 1,nslm
            tmc_layh(ji,jsl) = tmc_layh(ji,jsl) + tmc_layh_s(ji,jsl,jst) * soiltile(ji,jst)
         ENDDO
      END DO
    END DO

    IF (printlev>=3) WRITE (numout,*) ' hydrol_var_init done '

  END SUBROUTINE hydrol_var_init


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_snow
!!
!>\BRIEF        This routine computes snow processes. 
!!
!! DESCRIPTION  :
!! - 0 initialisation
!! - 1 On vegetation
!! - 1.1 Compute snow masse
!! - 1.2 Sublimation 
!! - 1.2.1 Check that sublimation on the vegetated fraction is possible.
!! - 1.3. snow melt only if temperature positive
!! - 1.3.1 enough snow for melting or not
!! - 1.3.2 not enough snow
!! - 1.3.3 negative snow - now snow melt
!! - 1.4 Snow melts only on weight glaciers
!! - 2 On Land ice
!! - 2.1 Compute snow
!! - 2.2 Sublimation 
!! - 2.3 Snow melt only for continental ice fraction
!! - 2.3.1 If there is snow on the ice-fraction it can melt
!! - 2.4 Snow melts only on weight glaciers 
!! - 3 On other surface types - not done yet
!! - 4 computes total melt (snow and ice)
!! - 5 computes snow age on veg and ice (for albedo)
!! - 5.1 Snow age on vegetation
!! - 5.2 Snow age on ice
!! - 6 Diagnose the depth of the snow layer
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_snow

  SUBROUTINE hydrol_snow (kjpindex, precip_rain, precip_snow , temp_sol_new, soilcap,&
       & frac_nobio, totfrac_nobio, vevapsno, snow, snow_age, snow_nobio, snow_nobio_age, &
       & tot_melt, snowdepth,snowmelt)

    ! 
    ! interface description

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    ! input scalar 
    INTEGER(i_std), INTENT(in)                               :: kjpindex      !! Domain size
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_rain   !! Rainfall
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_snow   !! Snow precipitation
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: temp_sol_new  !! New soil temperature
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: soilcap       !! Soil capacity
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(in)     :: frac_nobio    !! Fraction of continental ice, lakes, ...
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: totfrac_nobio !! Total fraction of continental ice+lakes+ ...

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: tot_melt      !! Total melt from snow and ice  
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: snowmelt      !! Snow melt
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: snowdepth     !! Snow depth

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapsno      !! Snow evaporation
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: snow          !! Snow mass [Kg/m^2]
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: snow_age      !! Snow age
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: snow_nobio    !! Ice water balance
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: snow_nobio_age!! Snow age on ice, lakes, ...

    !! 0.4 Local variables

    INTEGER(i_std)                               :: ji, jv
    REAL(r_std), DIMENSION (kjpindex)             :: d_age  !! Snow age change
    REAL(r_std), DIMENSION (kjpindex)             :: xx     !! temporary
    REAL(r_std)                                   :: snowmelt_tmp !! The name says it all !
    REAL(r_std)                                   :: snow_d1k !! The amount of snow that corresponds to a 1K cooling

!_ ================================================================================================================================

    !
    ! for continental points
    !

    !
    !!_0 initialisation
    !
    DO jv = 1, nnobio
       DO ji=1,kjpindex
          subsnownobio(ji,jv) = zero
       ENDDO
    ENDDO
    DO ji=1,kjpindex
       subsnowveg(ji) = zero
       snowmelt(ji) = zero
       icemelt(ji) = zero
       subsinksoil(ji) = zero
       tot_melt(ji) = zero
    ENDDO
    !
    !! 1 On vegetation
    !
    DO ji=1,kjpindex
       !
    !! 1.1 Compute snow masse
       !
       snow(ji) = snow(ji) + (un - totfrac_nobio(ji))*precip_snow(ji)
       !
       !
    !! 1.2 Sublimation 
       !      Separate between vegetated and no-veget fractions 
       !      Care has to be taken as we might have sublimation from the
       !      the frac_nobio while there is no snow on the rest of the grid.
       !
       IF ( snow(ji) > snowcri ) THEN
          subsnownobio(ji,iice) = frac_nobio(ji,iice)*vevapsno(ji)
          subsnowveg(ji) = vevapsno(ji) - subsnownobio(ji,iice)
       ELSE
          ! Correction Nathalie - Juillet 2006.
          ! On doit d'abord tester s'il existe un frac_nobio!
          ! Pour le moment je ne regarde que le iice
          IF ( frac_nobio(ji,iice) .GT. min_sechiba) THEN
             subsnownobio(ji,iice) = vevapsno(ji)
             subsnowveg(ji) = zero
          ELSE 
             subsnownobio(ji,iice) = zero
             subsnowveg(ji) = vevapsno(ji)
          ENDIF
       ENDIF
       !
       !
    !! 1.2.1 Check that sublimation on the vegetated fraction is possible.
       !
       IF (subsnowveg(ji) .GT. snow(ji)) THEN
          ! What could not be sublimated goes into subsinksoil
          IF( (un - totfrac_nobio(ji)).GT.min_sechiba) THEN
             subsinksoil (ji) = (subsnowveg(ji) - snow(ji))/ (un - totfrac_nobio(ji))
          END IF
          ! Sublimation is thus limited to what is available
          subsnowveg(ji) = snow(ji)
          snow(ji) = zero
          vevapsno(ji) = subsnowveg(ji) + subsnownobio(ji,iice)
       ELSE
          snow(ji) = snow(ji) - subsnowveg(ji)
       ENDIF
       !
    !! 1.3. snow melt only if temperature positive
       !
       IF (temp_sol_new(ji).GT.tp_00) THEN
          !
          IF (snow(ji).GT.sneige) THEN
             !
             snowmelt(ji) = (un - frac_nobio(ji,iice))*(temp_sol_new(ji) - tp_00) * soilcap(ji) / chalfu0
             !
    !! 1.3.1 enough snow for melting or not
             !
             IF (snowmelt(ji).LT.snow(ji)) THEN
                snow(ji) = snow(ji) - snowmelt(ji)
             ELSE
                snowmelt(ji) = snow(ji)
                snow(ji) = zero
             END IF
             !
          ELSEIF (snow(ji).GE.zero) THEN
             !
    !! 1.3.2 not enough snow
             !
             snowmelt(ji) = snow(ji)
             snow(ji) = zero
          ELSE
             !
    !! 1.3.3 negative snow - now snow melt
             !
             snow(ji) = zero
             snowmelt(ji) = zero
             WRITE(numout,*) 'hydrol_snow: WARNING! snow was negative and was reset to zero. '
             !
          END IF

       ENDIF
    !! 1.4 Snow melts above a threshold
       ! Ice melt only if there is more than a given mass : maxmass_snow,
       ! But the snow cannot melt more in one time step to what corresponds to
       ! a 1K cooling. This will lead to a progressive melting of snow above
       ! maxmass_snow but it is needed as a too strong cooling can destabilise the model.
       IF ( snow(ji) .GT. maxmass_snow ) THEN
          snow_d1k = un * soilcap(ji) / chalfu0
          snowmelt(ji) = snowmelt(ji) + MIN((snow(ji) - maxmass_snow),snow_d1k)
          snow(ji) = snow(ji) - snowmelt(ji)
          IF ( printlev >= 3 ) WRITE (numout,*) "Snow was above maxmass_snow (", maxmass_snow,") and we melted ", snowmelt(ji)
       ENDIF
       
    END DO
    !
    !! 2 On Land ice
    !
    DO ji=1,kjpindex
       !
    !! 2.1 Compute snow
       !
       !!??Aurelien: pkoi mettre precip_rain en dessous? We considere liquid precipitations becomes instantly snow?  
       snow_nobio(ji,iice) = snow_nobio(ji,iice) + frac_nobio(ji,iice)*precip_snow(ji) + &
            & frac_nobio(ji,iice)*precip_rain(ji)
       !
    !! 2.2 Sublimation 
       !      Was calculated before it can give us negative snow_nobio but that is OK
       !      Once it goes below a certain values (-maxmass_snow for instance) we should kill
       !      the frac_nobio(ji,iice) !
       !
       snow_nobio(ji,iice) = snow_nobio(ji,iice) - subsnownobio(ji,iice)
       !
    !! 2.3 Snow melt only for continental ice fraction
       !
       snowmelt_tmp = zero
       IF (temp_sol_new(ji) .GT. tp_00) THEN
          !
    !! 2.3.1 If there is snow on the ice-fraction it can melt
          !
          snowmelt_tmp = frac_nobio(ji,iice)*(temp_sol_new(ji) - tp_00) * soilcap(ji) / chalfu0
          !
          IF ( snowmelt_tmp .GT. snow_nobio(ji,iice) ) THEN
             snowmelt_tmp = MAX( zero, snow_nobio(ji,iice))
          ENDIF
          snowmelt(ji) = snowmelt(ji) + snowmelt_tmp
          snow_nobio(ji,iice) = snow_nobio(ji,iice) - snowmelt_tmp
          !
       ENDIF
       !
    !! 2.4 Snow melts over a threshold
       !   Ice melt only if there is more than a given mass : maxmass_snow, 
       !   But the snow cannot melt more in one time step to what corresponds to
       !   a 1K cooling. This will lead to a progressive melting of snow above
       !   maxmass_snow but it is needed as a too strong cooling can destabilise the model.
       !
       IF ( snow_nobio(ji,iice) .GT. maxmass_snow ) THEN
          snow_d1k = un * soilcap(ji) / chalfu0
          icemelt(ji) = MIN((snow_nobio(ji,iice) - maxmass_snow),snow_d1k)
          snow_nobio(ji,iice) = snow_nobio(ji,iice) - icemelt(ji)

          IF ( printlev >= 3 ) WRITE (numout,*) "Snow was above maxmass_snow ON ICE (", maxmass_snow,") and we melted ", icemelt(ji)
       ENDIF

    END DO

    !
    !! 3 On other surface types - not done yet
    !
    IF ( nnobio .GT. 1 ) THEN
       WRITE(numout,*) 'WE HAVE',nnobio-1,' SURFACE TYPES I DO NOT KNOW'
       WRITE(numout,*) 'CANNOT TREAT SNOW ON THESE SURFACE TYPES'
       CALL ipslerr_p(3,'hydrol_snow','nnobio > 1 not allowded','Cannot treat snow on these surface types.','')
    ENDIF

    !
    !! 4 computes total melt (snow and ice)
    !
    DO ji = 1, kjpindex
       tot_melt(ji) = icemelt(ji) + snowmelt(ji)
    ENDDO

    !
    !! 5 computes snow age on veg and ice (for albedo)
    !
    DO ji = 1, kjpindex
       !
    !! 5.1 Snow age on vegetation
       !
       IF (snow(ji) .LE. zero) THEN
          snow_age(ji) = zero
       ELSE
          snow_age(ji) =(snow_age(ji) + (un - snow_age(ji)/max_snow_age) * dt_sechiba/one_day) &
               & * EXP(-precip_snow(ji) / snow_trans)
       ENDIF
       !
    !! 5.2 Snow age on ice
       !
       ! age of snow on ice: a little bit different because in cold regions, we really
       ! cannot negect the effect of cold temperatures on snow metamorphism any more.
       !
       IF (snow_nobio(ji,iice) .LE. zero) THEN
          snow_nobio_age(ji,iice) = zero
       ELSE
          !
          d_age(ji) = ( snow_nobio_age(ji,iice) + &
               &  (un - snow_nobio_age(ji,iice)/max_snow_age) * dt_sechiba/one_day ) * &
               &  EXP(-precip_snow(ji) / snow_trans) - snow_nobio_age(ji,iice)
          IF (d_age(ji) .GT. min_sechiba ) THEN
             xx(ji) = MAX( tp_00 - temp_sol_new(ji), zero )
             xx(ji) = ( xx(ji) / 7._r_std ) ** 4._r_std
             d_age(ji) = d_age(ji) / (un+xx(ji))
          ENDIF
          snow_nobio_age(ji,iice) = MAX( snow_nobio_age(ji,iice) + d_age(ji), zero )
          !
       ENDIF

    ENDDO

    !
    !! 6 Diagnose the depth of the snow layer
    !

    DO ji = 1, kjpindex
       snowdepth(ji) = snow(ji) /sn_dens
    ENDDO

    IF (printlev>=3) WRITE (numout,*) ' hydrol_snow done '

  END SUBROUTINE hydrol_snow

   
!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_canop
!!
!>\BRIEF        This routine computes canopy processes.
!!
!! DESCRIPTION  :
!! - 1 evaporation off the continents
!! - 1.1 The interception loss is take off the canopy. 
!! - 1.2 precip_rain is shared for each vegetation type
!! - 1.3 Limits the effect and sum what receives soil
!! - 1.4 swap qsintveg to the new value
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_canop

  SUBROUTINE hydrol_canop (kjpindex, precip_rain, vevapwet, veget_max, veget, qsintmax, &
       & qsintveg,precisol,tot_melt)

    ! 
    ! interface description
    !

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                               :: kjpindex    !! Domain size
    ! input fields
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_rain !! Rain precipitation
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)        :: vevapwet    !! Interception loss
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)        :: veget_max   !! max fraction of vegetation type
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)        :: veget       !! Fraction of vegetation type 
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)        :: qsintmax    !! Maximum water on vegetation for interception
    REAL(r_std), DIMENSION  (kjpindex), INTENT (in)          :: tot_melt    !! Total melt

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(out)       :: precisol    !! Water fallen onto the ground (throughfall)

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: qsintveg    !! Water on vegetation due to interception

    !! 0.4 Local variables

    INTEGER(i_std)                                           :: ji, jv
    REAL(r_std), DIMENSION (kjpindex,nvm)                    :: zqsintvegnew

!_ ================================================================================================================================

    ! boucle sur les points continentaux
    ! calcul de qsintveg au pas de temps suivant
    ! par ajout du flux interception loss
    ! calcule par enerbil en fonction
    ! des calculs faits dans diffuco
    ! calcul de ce qui tombe sur le sol
    ! avec accumulation dans precisol
    ! essayer d'harmoniser le traitement du sol nu
    ! avec celui des differents types de vegetation
    ! fait si on impose qsintmax ( ,1) = 0.0
    !
    ! loop for continental subdomain
    !
    !
    !! 1 evaporation off the continents
    !
    !! 1.1 The interception loss is take off the canopy. 
    DO jv=2,nvm
       qsintveg(:,jv) = qsintveg(:,jv) - vevapwet(:,jv)
    END DO

    !     It is raining :
    !! 1.2 precip_rain is shared for each vegetation type
    !     sum (veget (1,nvm)) must be egal to 1-totfrac_nobio.
    !     iniveget computes veget each day
    !
    qsintveg(:,1) = zero
    DO jv=2,nvm
       qsintveg(:,jv) = qsintveg(:,jv) + veget(:,jv) * ((1-throughfall_by_pft(jv))*precip_rain(:))
          ! should consider the veget_max(:,jv) to intercept rain
          ! xuhui 20151216
          !DO ji = 1,kjpindex
          !    IF (veget_max(ji,jv) .GT. zero) THEN ! otherwise, there will be no interception
          !        qsintveg(ji,jv) = qsintveg(ji,jv) + veget(ji,jv) / veget_max(ji,jv) * (1 - throughfall_by_pft(jv)) * precip_rain(ji)
          !    ENDIF
          !ENDDO
    END DO

    !
    !! 1.3 Limits the effect and sum what receives soil
    !
    precisol(:,1)=veget_max(:,1)*precip_rain(:)
    DO jv=2,nvm
       DO ji = 1, kjpindex
          zqsintvegnew(ji,jv) = MIN (qsintveg(ji,jv),qsintmax(ji,jv)) 
          precisol(ji,jv) = (veget(ji,jv)*throughfall_by_pft(jv)*precip_rain(ji)) + &
               qsintveg(ji,jv) - zqsintvegnew (ji,jv) + &
               (veget_max(ji,jv) - veget(ji,jv))*precip_rain(ji)
       ENDDO
    END DO
    !    
    DO jv=1,nvm
       DO ji = 1, kjpindex
          IF (vegtot(ji).GT.min_sechiba) THEN
             precisol(ji,jv) = precisol(ji,jv)+tot_melt(ji)*veget_max(ji,jv)/vegtot(ji)
          ENDIF
       ENDDO
    END DO
    !   
    !
    !! 1.4 swap qsintveg to the new value
    !
    DO jv=2,nvm
       qsintveg(:,jv) = zqsintvegnew (:,jv)
    END DO

    IF (printlev>=3) WRITE (numout,*) ' hydrol_canop done '

  END SUBROUTINE hydrol_canop


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_vegupd
!!
!>\BRIEF        Vegetation update   
!!
!! DESCRIPTION  :
!!   The vegetation cover has changed and we need to adapt the reservoir distribution 
!!   and the distribution of plants on different soil types.
!!   You may note that this occurs after evaporation and so on have been computed. It is
!!   not a problem as a new vegetation fraction will start with humrel=0 and thus will have no
!!   evaporation. If this is not the case it should have been caught above.
!!
!! - 1 Update of vegetation is it needed?
!! - 2 calculate water mass that we have to redistribute
!! - 3 put it into reservoir of plant whose surface area has grown
!! - 4 Soil tile gestion
!! - 5 update the corresponding masks
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_vegupd

  SUBROUTINE hydrol_vegupd(kjpindex, veget, veget_max, soiltile, qsintveg, resdist, frac_bare)


    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    ! input scalar 
    INTEGER(i_std), INTENT(in)                            :: kjpindex 
    ! input fields
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(in)    :: veget            !! New vegetation map
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)     :: veget_max        !! Max. fraction of vegetation type
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in)   :: soiltile         !! Fraction of each soil tile (0-1, unitless)

    !! 0.2 Output variables
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(out)    :: frac_bare        !! Fraction(of veget_max) of bare soil in each vegetation type

    !! 0.3 Modified variables

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: qsintveg         !! Water on old vegetation 
    REAL(r_std), DIMENSION (kjpindex, nstm), INTENT(inout) :: resdist         !! Soiltile from previous time-step

    !! 0.4 Local variables

    INTEGER(i_std)                                 :: ji,jv,jst
    REAL(r_std), DIMENSION(kjpindex)               :: tot_corr_veg_soil

!_ ================================================================================================================================

    !! 1 If veget has been updated at last time step (with LAND USE or DGVM),
    !! tmc and mc must be modified with respect to humtot conservation.
    CALL hydrol_tmc_update ( kjpindex, veget_max, soiltile, qsintveg, resdist )

    ! Remember that it is only frac_nobio + SUM(veget_max(,:)) that is equal to 1. Thus we need vegtot
    DO ji = 1, kjpindex
       vegtot(ji) = SUM(veget_max(ji,:))
    ENDDO

    ! Compute the masks for veget
    
    mask_veget(:,:) = 0
    mask_soiltile(:,:) = 0
    
    DO jst=1,nstm
       DO ji = 1, kjpindex
          IF(soiltile(ji,jst) .GT. min_sechiba) THEN
             mask_soiltile(ji,jst) = 1
          ENDIF
       END DO
    ENDDO
          
    DO jv = 1, nvm
       DO ji = 1, kjpindex
          IF(veget_max(ji,jv) .GT. min_sechiba) THEN
             mask_veget(ji,jv) = 1
          ENDIF
       END DO
    END DO

    ! Compute corr_veg_soil 
    corr_veg_soil(:,:,:) = zero
    DO jv = 1, nvm
       jst = pref_soil_veg(jv)
       DO ji=1,kjpindex
          ! for veget distribution used in sechiba via humrel
          IF (mask_soiltile(ji,jst).GT.0 .AND. vegtot(ji) > min_sechiba) THEN
             corr_veg_soil(ji,jv,jst)=veget_max(ji,jv)/soiltile(ji,jst)
          ENDIF
       ENDDO
    ENDDO

    IF (check_cwrr .AND. first_hydrol_main) THEN
       ! somme(soiltile * corr_veg_soil ) = 1
       tot_corr_veg_soil(:)=zero
       DO jst = 1, nstm
          DO jv = 1,nvm
             DO ji=1,kjpindex
                tot_corr_veg_soil(ji)=tot_corr_veg_soil(ji)+soiltile(ji,jst)*corr_veg_soil(ji,jv,jst)
             ENDDO
          ENDDO
       ENDDO

       DO ji=1,kjpindex
          IF ( ABS( tot_corr_veg_soil(ji) - vegtot(ji) ) > 10*EPS1 ) THEN
             WRITE(numout,*) 'corr_veg_soil SPLIT FALSE:ji=',ji,&
                  tot_corr_veg_soil(ji)
             WRITE(numout,*) 'err',ABS( tot_corr_veg_soil(ji) - vegtot(ji) )
             WRITE(numout,*) 'vegtot',vegtot(ji)
             DO jv=1,nvm
                WRITE(numout,*) 'jv,veget_max,corr_veg_soil',jv,veget_max(ji,jv),corr_veg_soil(ji,jv,:)
             END DO
             CALL ipslerr_p(3, 'hydrol_vegupd', 'Error in tot_corr_veg_soil or vegtot','','')
          ENDIF
       ENDDO
    ENDIF

    ! Calculate frac_bare (previosly done in slowproc_veget)
    DO ji =1, kjpindex
       IF( veget_max(ji,1) .GT. min_sechiba ) THEN
          frac_bare(ji,1) = un
       ELSE
          frac_bare(ji,1) = zero
       ENDIF
    ENDDO
    DO jv = 2, nvm
       DO ji =1, kjpindex
          IF( veget_max(ji,jv) .GT. min_sechiba ) THEN
             frac_bare(ji,jv) = un - veget(ji,jv)/veget_max(ji,jv)
          ELSE
             frac_bare(ji,jv) = zero
          ENDIF
       ENDDO
    ENDDO

    ! Tout dans cette routine est maintenant certainement obsolete (veget_max etant constant) en dehors des lignes 
    ! suivantes et le calcul de frac_bare:
    frac_bare_ns(:,:) = zero
    DO jst = 1, nstm
       DO jv = 1, nvm
          DO ji =1, kjpindex
             IF(vegtot(ji) .GT. min_sechiba) THEN
                frac_bare_ns(ji,jst) = frac_bare_ns(ji,jst) + corr_veg_soil(ji,jv,jst) * frac_bare(ji,jv) / vegtot(ji)
             ENDIF
          END DO
       ENDDO
    END DO

    IF (printlev>=3) WRITE (numout,*) ' hydrol_vegupd done '

  END SUBROUTINE hydrol_vegupd


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_flood
!!
!>\BRIEF        This routine computes the evolution of the surface reservoir (floodplain).  
!!
!! DESCRIPTION  :
!! - 1 Take out vevapflo from the reservoir and transfer the remaining to subsinksoil
!! - 2 Compute the total flux from floodplain floodout (transfered to routing)
!! - 3 Discriminate between precip over land and over floodplain
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_flood

  SUBROUTINE hydrol_flood (kjpindex, vevapflo, flood_frac, flood_res, floodout)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    ! input scalar 
    INTEGER(i_std), INTENT(in)                               :: kjpindex         !!
    ! input fields
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: flood_frac       !! Fraction of floodplains in grid box

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: floodout         !! Flux to take out from floodplains

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: flood_res        !! Floodplains reservoir estimate
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapflo         !! Evaporation over floodplains

    !! 0.4 Local variables

    INTEGER(i_std)                                           :: ji, jv           !! Indices
    REAL(r_std), DIMENSION (kjpindex)                        :: temp             !! 

!_ ================================================================================================================================
    !- 
    !! 1 Take out vevapflo from the reservoir and transfer the remaining to subsinksoil 
    !-
    DO ji = 1,kjpindex
       temp(ji) = MIN(flood_res(ji), vevapflo(ji))
    ENDDO
    DO ji = 1,kjpindex
       flood_res(ji) = flood_res(ji) - temp(ji)
       subsinksoil(ji) = subsinksoil(ji) + vevapflo(ji) - temp(ji)
       vevapflo(ji) = temp(ji)
    ENDDO

    !- 
    !! 2 Compute the total flux from floodplain floodout (transfered to routing) 
    !-
    DO ji = 1,kjpindex
       floodout(ji) = vevapflo(ji) - flood_frac(ji) * SUM(precisol(ji,:))
    ENDDO

    !-
    !! 3 Discriminate between precip over land and over floodplain
    !-
    DO jv=1, nvm
       DO ji = 1,kjpindex
          precisol(ji,jv) = precisol(ji,jv) * (1 - flood_frac(ji))
       ENDDO
    ENDDO 

    IF (printlev>=3) WRITE (numout,*) ' hydrol_flood done'

  END SUBROUTINE hydrol_flood


!! ================================================================================================================================
!! SUBROUTINE 	: hydrol_soil
!!
!>\BRIEF        This routine computes soil processes with CWRR scheme.
!!
!! DESCRIPTION  :
!! - 0 Arrays initialisation
!! -   for each soil type
!! - 1 We compare water2infilt and water2extract to keep only difference
!! - 1.1 add to the first layer
!! - 1.2 filling layers
!! - 2 Before diffusion scheme
!! - 2.1 Some initialisation necessary for the diffusion scheme to work
!! - 2.2 coefficients are computed for the profile of mc before infiltration:
!! - 3 The infiltration is computed 
!! - 4 Coefficient are recomputed for the profile of mc after infiltration:
!! - 5 Prepar the diffusion scheme
!! - 5.1 Set the values for diffusion scheme
!! - 5.2 verifications for a good soil humidity
!! - 5.3 compute matrix coefficients
!! - 6 Resolve diffusion scheme
!! - 6.1 solve equations assuming atmosphere limiting
!! - 6.2 check if really atmosphere limiting
!! - 6.3 Reset the coefficient for diffusion (only used if resolv(ji) = .TRUE.)
!! - 6.4 resolve the equations with new boundary conditions if necessary
!! - 7 close the water balance
!! - 7.1 compute dr_ns with the bottom boundary condition 
!! - 7.2 compute total soil moisture content
!! - 7.3 deduction of upper flux from soil moisture variation and bottom flux
!! - 7.4 deduction of ae_ns and ru_ns:
!! - 8 Special treatment for the unstable cases
!! - 9 Then compute the temporary surface water and correct the outgoing runoff
!! - 10 smooth again
!! - 11 Optional computation of the fluxes 
!! - 12 We make some useful output
!! - 13 before closing the soil water, we check the water balance of soil
!! - 14 sum 3d variables into 2d variables
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_soil

  SUBROUTINE hydrol_soil (kjpindex, veget_max, soiltile, njsc, reinf_slope, &
       & transpir, vevapnu, vevapnu_pft, evapot, evapot_penm, runoff, drainage, &
       & returnflow, reinfiltration, irrigation, irrig_demand_ratio, &
       & tot_melt, evap_bare_lim,  shumdiag, shumdiag_perma,&
       & k_litt, litterhumdiag, humrel,vegstress, drysoil_frac, irrig_fin, &
       & is_crop_soil, stempdiag,snow, &
       & snowdz, tot_bare_soil, u, v, tq_cdrag, mc_layh, mcl_layh, tmc_layh, &
       & mc_layh_s, mcl_layh_s, tmc_layh_s, drunoff_tot, fsat, veget, irrigr, &
       & do_irrig_reservoir, reservoir_dep_max, irrig_dep_threshold, do_awd, &
       & awd_dep, lai)
       !& awd_dep, irrigation_exe)
       !& awd_dep, irrig_dosmax_tmp, irrigation_exe)
    ! 
    ! interface description

    !! 0. Variable and parameter declaration

    !AD*** UNITS have not yet been fully commented; mm = @tex $(kg m^{-2})$ @endtex 

    !! 0.1 Input variables

    ! input scalar 
    INTEGER(i_std), INTENT(in)                               :: kjpindex 
    ! input fields
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: veget_max        !! Map of max vegetation types [-]
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: veget            !! Map of vegetation fraction [-]
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: lai              !! leaf area index [-]
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: njsc             !! Index of the dominant soil textural class 
                                                                                 !!   in the grid cell (1-nscm, unitless)
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in)      :: soiltile         !! Fraction of each soil tile (0-1, unitless)
    LOGICAL, DIMENSION (nstm), INTENT (in)      :: is_crop_soil         !! Whether the tile is under cropland
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)        :: transpir         !! Transpiration [mm]
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)           :: reinf_slope      !! Slope coef
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: returnflow       !! Water returning to the soil from the bottom [mm]
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: reinfiltration   !! Water returning to the top of the soil [mm]
    !REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: irrigation       !! Irrigation [mm]
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)        :: irrig_demand_ratio  !! ratio of irrigation water [unitless,0-1]
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: evapot           !! Potential evaporation [mm]
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: evapot_penm      !! Potential evaporation Penman [mm] 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: tot_melt         !! Total melt from snow and ice [mm]
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (in)       :: stempdiag        !! Diagnostic temp profile from thermosoil
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: snow             !! Snow mass [Kg/m^2]
    REAL(r_std), DIMENSION (kjpindex,nsnow),INTENT(in)       :: snowdz           !! Snow depth [m]
!pss:+
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: fsat             !! fraction of saturation soil 
!pss:-
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: tot_bare_soil    !! Total evaporating bare soil fraction 
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)             :: u,v              !! Horizontal wind speed
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)             :: tq_cdrag         !! Surface drag coefficient 
    LOGICAL,DIMENSION (nstm), INTENT (in)              :: do_irrig_reservoir     !! flag of irrigation reservoir for specific soil column
    REAL(r_std),DIMENSION (nstm), INTENT (in)          :: reservoir_dep_max      !! maximum depth of irrigation reservoir
    REAL(r_std),DIMENSION (nstm), INTENT (in)          :: irrig_dep_threshold    !! threshold of depth when irrigation occurs
    LOGICAL,DIMENSION (nstm), INTENT (in)              :: do_awd                 !! if do awd irrigation
    REAL(r_std),DIMENSION (nstm), INTENT (in)          :: awd_dep                !! water amount (depth) of one irrigation event
    !REAL(r_std), INTENT (in) :: irrig_dosmax_tmp                                 !! temporal case for routing

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: runoff           !! complete surface runoff [mm]
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: drainage         !! complete drainage [mm]
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: evap_bare_lim    !! limitation factor (beta) for bare soil evaporation  
!    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(out)        :: evap_bare_lim_pft !! limitation factor (beta) for bare soil evaporation  
                                                                                 !! on each soil column [mm] 
    REAL(r_std), DIMENSION (kjpindex,nbdl), INTENT (out)     :: shumdiag         !! Relative soil moisture in each soil layer 
                                                                                 !! with respect to (mcf-mcw)
    REAL(r_std), DIMENSION (kjpindex,nbdl), INTENT (out)     :: shumdiag_perma   !! Percent of porosity filled with water (mc/mcs)
                                                                                 !! used for the thermal computations
    REAL(r_std), DIMENSION (kjpindex), INTENT (out)          :: k_litt           !! Litter approximated hydraulic conductivity
    REAL(r_std), DIMENSION (kjpindex), INTENT (out)          :: litterhumdiag    !! Litter diagnosed humidity
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(out)      :: vegstress        !! Veg. moisture stress (only for vegetation 
                                                                                 !! growth) 
    REAL(r_std), DIMENSION (kjpindex), INTENT (out)          :: drysoil_frac     !! Function of the litter humidity
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(out)       :: irrig_fin        !! final application of irrigation [mm]
!pss:+
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: drunoff_tot      !! Dunne runoff
!pss:-
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out)     :: mc_layh          !! Volumetric soil moisture content for each layer in hydrol(liquid + ice) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out)     :: mcl_layh         !! Volumetric soil moisture content for each layer in hydrol(liquid) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out)     :: tmc_layh         !! Total soil moisture content for each layer in hydrol(liquid + ice) [mm]
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm), INTENT (out)  :: mc_layh_s          !! Volumetric soil moisture content for each layer in hydrol(liquid + ice) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm), INTENT (out)  :: mcl_layh_s         !! Volumetric soil moisture content for each layer in hydrol(liquid) [m3/m3]
    REAL(r_std), DIMENSION (kjpindex,nslm,nstm), INTENT (out)  :: tmc_layh_s         !! Total soil moisture content for each layer in hydrol(liquid + ice) [mm]


    !! 0.3 Modified variables

    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapnu          !! Bare soil evaporation
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(inout)     :: vevapnu_pft          !! Bare soil evaporation
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (inout)    :: humrel           !! Relative humidity [0-1, dimensionless]
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT(inout)    :: irrigr           !! irrigation reservoir
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: irrigation       !! Irrigation [mm]
    !REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: irrigation_exe   !! Irrigation [mm]

    !! 0.4 Local variables

    INTEGER(i_std)                                 :: ji, jv, jsl, jst           !! indices
    REAL(r_std), PARAMETER                         :: frac_mcs = 0.66            !! Temporary depth
    REAL(r_std)                                    :: us_tmp                     !! Temporary stress
    REAL(r_std)                                    :: tot_irrig_frac            !! temporary sum of irrigated fraction of vegetation
    REAL(r_std), DIMENSION(kjpindex)               :: temp                       !! Temporary value for fluxes
    REAL(r_std), DIMENSION(kjpindex)               :: tmcold                     !!
    REAL(r_std), DIMENSION(kjpindex)               :: tmcint                     !!
    REAL(r_std), DIMENSION(kjpindex,nslm,nstm)     :: moderwilt                  !!
    REAL(r_std), DIMENSION(kjpindex,nslm)          :: mcint                      !! To save mc values for future use
    LOGICAL, DIMENSION(kjpindex)                   :: is_under_mcr               !! Allows under residual soil moisture due to evap 
    LOGICAL, DIMENSION(kjpindex)                   :: is_over_mcs                !! Allows over saturated soil moisture due to 
                                                                                 !! returnflow 
    REAL(r_std), DIMENSION(kjpindex)               :: sum_rootsink               !! Sum of the root sink
    REAL(r_std), DIMENSION(kjpindex)               :: deltahum,diff              !!
    LOGICAL(r_std), DIMENSION(kjpindex)            :: test                       !!
    REAL(r_std), DIMENSION(kjpindex)               :: tsink                      !!
    REAL(r_std), DIMENSION(kjpindex)               :: water2extract              !! Temporary variable [mm]
    REAL(r_std), DIMENSION(kjpindex)               :: returnflow_soil            !! Water from the routing back to the bottom of 
                                                                                 !! the soil [mm] 
    REAL(r_std), DIMENSION(kjpindex)               :: reinfiltration_soil        !! Water from the routing back to the top of the 
                                                                                 !! soil [mm] 
    REAL(r_std), DIMENSION(kjpindex, nstm)         :: irrigation_soil            !! Water from irrigation returning to soil 
                                                                                 !! moisture for each soil type [mm] 
    REAL(r_std), DIMENSION(kjpindex)               :: flux_infilt                !! Water to infiltrate
                                                                                 !!  @tex $(kg m^{-2})$ @endtex
    REAL(r_std), DIMENSION(kjpindex)               :: flux_bottom                !! Flux at bottom of the soil column
    REAL(r_std), DIMENSION(kjpindex)               :: flux_top                   !! Flux at top of the soil column (for bare soil evap)
    REAL(r_std), DIMENSION(kjpindex)               :: flux_top_res               !! residuel of flux at top and the water in the above reservoir (for bare soil evap)
    !applicable when do_irrig_reservoir = True
    LOGICAL                                        :: error=.FALSE.              !! If true, exit in the end of subroutine
!    REAL(r_std), DIMENSION (kjpindex,nslm,nstm)    :: tmc_layh_s                 !! total soil moisture content for each layer in hydrol and each soiltile (mm)
    REAL(r_std), DIMENSION(kjpindex)               :: mc_rel                     !! first layer relative soil moisture, required for rsoil
    REAL(r_std), DIMENSION(kjpindex,nstm)          :: r_soil_ns	                 !! soil resistance from Oleson et al 2008
    REAL(r_std)                     		   :: speed			 !! magnitude of wind speed required for Aerodynamic resistance 
    REAL(r_std)                                    :: ra			 !! diagnosed aerodynamic resistance
    REAL(r_std), DIMENSION(kjpindex)               :: evap_soil                  !! soil evaporation from Oleson et al 2008
    REAL(r_std), DIMENSION (kjpindex,nstm)         :: irrigrint                  !! initial value of irrigation reservoir depth
    REAL(r_std), DIMENSION (kjpindex,nstm)         :: irrigrvir                  !! virtual irrigation reservoir depth for soil evaporation estimation
    REAL(r_std), DIMENSION(kjpindex,nslm,nstm)     :: mcl_tmp                    !! temporal liquid soil moisture before the bug in 'hydrol_soil_tridiag' fixed
    REAL(r_std), DIMENSION(kjpindex)               :: tmcl_tmp                    !! temporal liquid soil moisture before the bug in 'hydrol_soil_tridiag' fixed
    REAL(r_std), DIMENSION (kjpindex,nvm)          :: irrig_tmp                  !! virtual irrigation reservoir depth for soil evaporation estimation
    LOGICAL, DIMENSION (kjpindex,nstm)             :: trigger_irrig              !! check if irrigation is triggered
    LOGICAL, DIMENSION (kjpindex,nstm)             :: hascrops                   !! check if current soil column has crops: TRUE:has crops; FALSE: no crops

!_ ================================================================================================================================

    !! 0 Initialisation
    !
    !! 0.1 Arrays with DIMENSION(kjpindex)
    !
    returnflow_soil(:) = zero
    reinfiltration_soil(:) = zero
    irrigation_soil(:, :) = zero
    !irrigation_exe(:) = zero
    trigger_irrig(:,:) = .FALSE.
    irrig_tmp(:,:) = zero
    qflux(:,:,:) = zero
    is_under_mcr(:) = .FALSE.
    is_over_mcs(:) = .FALSE.
    flux_infilt(:) = zero
    mc_layh(:,:) = zero
    mcl_layh(:,:) = zero
    tmc_layh(:,:) = zero
    mc_layh_s(:,:,:) = zero
    mcl_layh_s(:,:,:) = zero
    tmc_layh_s(:,:,:) = zero
    IF (ok_freeze_cwrr) THEN
       kk(:,:,:)=zero
       kk_moy(:,:)=zero
    ENDIF
    tmcl_tmp(:) = zero

    IF (ok_freeze_cwrr) THEN
       !
       ! 1.1 Calculate the temperature at hydrological levels
       !
       CALL hydrol_calculate_temp_hydro(kjpindex, stempdiag, snow,snowdz)
    ENDIF
    !
    !! 0.2 Split 2d variables to 3d variables, per soil tile
    !
    CALL hydrol_split_soil (kjpindex, veget_max, soiltile, vevapnu, vevapnu_pft, transpir, humrel, evap_bare_lim, tot_bare_soil)
    !
    !! 0.3 Common variables related to routing, with all return flow applied to the soil surface
    ! AD*** redundancies with first three lines of step 0.1
    ! AD*** why dividing by vegtot if returnflow(ji), reinfiltration(ji),irrigation(ji) are in kg/m2 ??
    ! AD*** ces flux sont-ils supposes constants sur les 3 soiltiles ?
    !
    irrig_fin(:,:) = zero
    DO ji=1,kjpindex
       IF(vegtot(ji).GT.min_sechiba) THEN
          returnflow_soil(ji) = zero
          reinfiltration_soil(ji) = (returnflow(ji) + reinfiltration(ji))/vegtot(ji)
          irrigation_soil(ji,:) = zero 
          tot_irrig_frac = zero
          DO jv=2,nvm
              IF ( ok_LAIdev(jv) .AND. ( irrig_demand_ratio(ji,jv) .GT. zero )) THEN
                  IF (.NOT. is_crop_soil(pref_soil_veg(jv)) ) THEN 
                      ! the demanded irrigation is not in the crop soil column
                      WRITE(numout,*) "pft ", jv, " pref_soil_veg(jv) ", pref_soil_veg(jv), "is not crop soil"
                      STOP "hydrol irrig soiltile"
                  ENDIF
                  tot_irrig_frac = tot_irrig_frac + veget_max(ji,jv)
              ENDIF
          ENDDO          
          DO jv = 2,nvm
              ! old: since we only have one crop soil, we pour all irrigation water into
              ! this soil column
              ! now we have several crop soil tile
              ! we irrigate separately considering soil tiles
              ! assuming each crop has its own tile respectively
              !ZunYin for later use to avoid the problem of hydrol_soil_tridiag
              IF (is_crop_soil(pref_soil_veg(jv)) .AND. (free_drain_coef(ji,pref_soil_veg(jv)) .LT. min_sechiba) .AND. (lai(ji,jv) .GT. 0)) THEN
                  trigger_irrig(ji,pref_soil_veg(jv)) = .TRUE.
              ENDIF

              IF ((irrig_demand_ratio(ji,jv) .GT. zero) .AND. is_crop_soil(pref_soil_veg(jv)) .AND. (lai(ji,jv) .GT. 0)) THEN
                  IF (do_irrig_reservoir(pref_soil_veg(jv))) THEN
                      irrig_tmp(ji,jv) = MIN(MAX(zero, irrig_dep_threshold(pref_soil_veg(jv)) - irrigr(ji,pref_soil_veg(jv))), &
                          & irrigation(ji) * irrig_demand_ratio(ji,jv) / veget_max(ji,jv)) * veget_max(ji,jv)
                  ELSE
                      irrig_tmp(ji,jv) = MIN(irrig_dosmax, irrigation(ji) * irrig_demand_ratio(ji,jv) / veget_max(ji,jv)) * veget_max(ji,jv)
                  ENDIF

                  irrig_fin(ji,jv) = irrig_tmp(ji,jv) / veget_max(ji,jv)
                  irrigation_soil(ji,pref_soil_veg(jv)) = irrigation_soil(ji,pref_soil_veg(jv)) + &
                             & irrig_tmp(ji,jv) / soiltile(ji,pref_soil_veg(jv))

              ENDIF
          ENDDO
       ELSE
          returnflow_soil(ji) = zero
          reinfiltration_soil(ji) = zero
          irrigation_soil(ji,:) = zero
       ENDIF
       !ZunYin correction of irrigation amount
       !irrigation_exe(ji) = SUM(irrig_tmp(ji,:))
    ENDDO

    !! -- START MAIN LOOP OVER SOILTILES
    !!    The called subroutines work on arrays with DIMENSION(kjpindex), recursively used for each soiltile jst
    !
    DO jst = 1,nstm
       !
       !! 0.4. Keep initial values for future check-up
       !
       ! Sum of sinks 
       sum_rootsink(:)=SUM(rootsink(:,:,jst),dim=2)
       DO ji=1,kjpindex
          tsink(ji) = sum_rootsink(ji)+MAX(ae_ns(ji,jst),zero)+subsinksoil(ji)
       ENDDO

       !IF (jst == 6) THEN
       !   WRITE(numout,*)'Zun rootsink_1:',SUM(rootsink(1,:,jst))
       !ENDIF
       !
       ! Total moisture content (including water2infilt) is saved for balance checks at the end
       ! In hydrol_tmc_update, tmc is increased by water2infilt(ji,jst), but mc is not modified !
       tmcold(:) = tmc(:,jst)
       !
       ! The value of mc is kept in mcint (nstm dimension removed), used in the flux computation after diffusion
       DO jsl = 1, nslm
          DO ji = 1, kjpindex
             mcint(ji,jsl) = mask_soiltile(ji,jst) * mc(ji,jsl,jst)
          ENDDO
       ENDDO

       ! Initial total moisture content : tmcint does not include water2infilt, contrarily to tmcold
       DO ji = 1, kjpindex
          tmcint(ji) = dz(2) * ( trois*mcint(ji,1) + mcint(ji,2) )/huit 
       ENDDO
       DO jsl = 2,nslm-1
          DO ji = 1, kjpindex
             tmcint(ji) = tmcint(ji) + dz(jsl) &
                  & * (trois*mcint(ji,jsl)+mcint(ji,jsl-1))/huit &
                  & + dz(jsl+1) * (trois*mcint(ji,jsl)+mcint(ji,jsl+1))/huit
          ENDDO
       ENDDO
       DO ji = 1, kjpindex
          tmcint(ji) = tmcint(ji) + dz(nslm) &
               & * (trois * mcint(ji,nslm) + mcint(ji,nslm-1))/huit
       ENDDO

       !! FIRSTLY, WE CHANGE MC BASED ON EXTERNAL FLUXES, ALL APPLIED AT THE SOIL SURFACE
       !!   Input = water2infilt(ji,jst) + irrigation_soil(ji) + reinfiltration_soil(ji) + precisol_ns(ji,jst)
       !!   Output = MAX(ae_ns(ji,jst),zero) + subsinksoil(ji)
       ! AD*** what is the fate of negative ae_ns ???

       !! 1 Reduces water2infilt and water2extract to their difference + extracts water2extract (including bare soil evap)

       !! 1.1 compares water2infilt and water2extract to keep only difference


       !ZunYin: temporally change ae_ns by multiping (1-vegetfrac)/maxvegetfrac
       !DO ji = 1, kjpindex
       !   mask_vegtot(ji) = 0
       !   IF(vegtot(ji) .GT. min_sechiba) THEN
       !      mask_vegtot(ji) = 1
       !   ENDIF
       !END DO
       
       !ae_ns_tmp(:,jst) = ae_ns(:,jst)

       !DO ji = 1, kjpindex 
       !   ! Here we weight ae_ns by the fraction of bare evaporating soil. 
       !   ! This is given by frac_bare_ns, taking into account bare soil under vegetation
       !   !ae_ns(ji,:) = mask_vegtot(ji) * ae_ns(ji,:) * frac_bare_ns(ji,:)
       !   IF (vegtot(ji) .GT. min_sechiba) THEN
       !       ae_ns_tmp(ji,jst) = ae_ns(ji,jst) * frac_bare_ns(ji,jst)
       !   ELSE
       !       ae_ns_tmp(ji,jst) = zero
       !   ENDIF
       !END DO

       ! Here, water2extract is used as a temporary variable to store the difference the min of water to infiltrate vs evaporate
       DO ji = 1, kjpindex
!          water2extract(ji) = MIN(water2infilt(ji,jst) + irrigation_soil(ji) + reinfiltration_soil(ji), &
!               & MAX(ae_ns(ji,jst),zero) + subsinksoil(ji))
!          IF ( jst .GE. 4 ) THEN ! crop soil
          IF ( is_crop_soil(jst) ) THEN ! crop soil
              water2extract(ji) = MIN(water2infilt(ji,jst) + irrigation_soil(ji,jst) + irrigr(ji,jst) + reinfiltration_soil(ji), &
                   & MAX(ae_ns(ji,jst),zero) + subsinksoil(ji))
              !ZunYin: should check if ae_ns should powered by
              !(1-vegetfrac/maxvegetfrac) or not
              !water2extract(ji) = MIN(water2infilt(ji,jst) + irrigation_soil(ji,jst) + irrigr(ji,jst) + reinfiltration_soil(ji), &
              !     & MAX(ae_ns_tmp(ji,jst),zero) + subsinksoil(ji))
!           ae_ns: bare soil evaporation per soiltile
!           subsinksoil: excess of sublimation as a sink for the soil
          ELSE
               water2extract(ji) = MIN(water2infilt(ji,jst) + reinfiltration_soil(ji), &
                   & MAX(ae_ns(ji,jst),zero) + subsinksoil(ji))
                   !& MAX(ae_ns_tmp(ji,jst),zero) + subsinksoil(ji))
          ENDIF

       ENDDO

       ! The water to infiltrate at the soil surface is either 0, or the difference to what has to be evaporated
       !   - the initial water2infilt (right hand side) results from qsintveg changes with vegetation updates
       !   - irrigation_soil is the input flux to the soil surface from irrigation 
       !   - reinfiltration_soil is the input flux to the soil surface from routing 'including returnflow)
       !   - eventually, water2infilt holds all fluxes to the soil surface except precisol (reduced by water2extract)
       DO ji = 1, kjpindex
!          water2infilt(ji,jst) = water2infilt(ji,jst) + irrigation_soil(ji) + reinfiltration_soil(ji) - water2extract(ji)
!          IF ( jst .GE. 4 ) THEN ! crop soil
          IF ( is_crop_soil(jst) ) THEN ! crop soil
              water2infilt(ji,jst) = water2infilt(ji,jst) + irrigation_soil(ji,jst) + irrigr(ji,jst) + reinfiltration_soil(ji) - water2extract(ji)
          ELSE
              water2infilt(ji,jst) = water2infilt(ji,jst) + reinfiltration_soil(ji) - water2extract(ji)
          ENDIF

       ENDDO       

!!       WRtTE(numout,*) "irrig xuhui: water2infilt(1,4): ", water2infilt(1,4)
             
       ! The water to evaporate from the soil surface is either the difference to what has to be infiltrated, or 0
       !   - subsinksoil is the residual from sublimation is the snowpack is not sufficient
       !   - how are the negative values of ae_ns taken into account ??? 
       DO ji = 1, kjpindex
          water2extract(ji) =  MAX(ae_ns(ji,jst),zero) + subsinksoil(ji) - water2extract(ji)
          !water2extract(ji) =  MAX(ae_ns_tmp(ji,jst),zero) + subsinksoil(ji) - water2extract(ji)
       ENDDO

       !! 1.2 Removes water2extract (including ae_ns) from top layer,
       !!     and from layers below in case it creates under residual value
       !!     This is done by hydrol_soil_smooth ("like for mcs in case of reinfiltration")
       !      ...but where ? AD***???

       DO ji = 1, kjpindex
          mc(ji,1,jst) = mc(ji,1,jst) - water2extract(ji) * deux / dz(2)
          ! mc: total water content in the soiltile
          ! dz: internode thickness
       ENDDO

       CALL hydrol_soil_smooth(kjpindex,jst, njsc, is_under_mcr, is_over_mcs)

       !! 2 Before the diffusion scheme

       !! 2.1 We correct rootsink in the first two layers so that it is not too low in the first layer
       DO ji = 1, kjpindex
          v1(ji,jst) = dz(2)/huit * (trois * mc(ji,1,jst)+ mc(ji,2,jst))
          rootsink(ji,2,jst) = rootsink(ji,2,jst) + MAX(rootsink(ji,1,jst)-v1(ji,jst), zero) 
          rootsink(ji,1,jst) = MIN(rootsink(ji,1,jst),v1(ji,jst))
       ENDDO

       !- Flux at top of the soil column is zero
       flux_top(:) = zero
       flux_top_res(:) = zero

       !! 2.2 Definition of flux_infilt and flux, which corresponds to bare soil evaporation in the top layer
       !!     This flux needs to be set to zero, as ae_ns has already been withdrawn in step 1.2 
       DO ji = 1, kjpindex
          ! Initialise the flux to be infiltrated 
          flux_infilt(ji) = water2infilt(ji,jst) + precisol_ns(ji,jst) 
          ! precisol_ns: throughfall per soil tile
          ! This incoming flux is first dedicated to fill the soil up to mcr (in case needed, see 2.3)
       ENDDO

       !! 2.3 Treat problem with too low soil moisture content in soil column : is_under_mcr
       DO ji = 1, kjpindex
          IF (is_under_mcr(ji)) THEN
             ! If is_under_mcr, the water deficit should be equally split over all soil layers (cf hydrol_soil_smooth)
             ! Calculate minimum mc 
             temp(ji)=mc(ji,1,jst)
             DO jsl=2,nslm
                IF (mc(ji,jsl,jst) .LT. temp(ji)) THEN
                   temp(ji)=mc(ji,jsl,jst)
                END IF
             END DO     
             ! Calculate water deficit
             temp(ji)=mcr(njsc(ji))-temp(ji)
             ! Add to each soil layer
             DO jsl=1,nslm
                mc(ji,jsl,jst) = mc(ji,jsl,jst) + temp(ji)
             END DO
             ! Remove added water from flux_infilt
             flux_infilt(ji)=flux_infilt(ji)-temp(ji)*(zmaxh*mille)
             IF (flux_infilt(ji) .LT. 0.) THEN
                WRITE(numout,*) 'BIG PROBLEM at grid cell ',ji
                WRITE(numout,*) 'No available water in soil column!!!'
                flux_infilt(ji) = 0.
                ! AD *** This violates water conservation, unless the difference is reported to another flux (see 5.3 ?)
             END IF
          END IF
       END DO

       !! 2.4 K and D are computed for the profile of mc before infiltration
       CALL hydrol_soil_coef(kjpindex,jst,njsc)

       !! 2.5 The infiltration is computed 
       CALL hydrol_soil_infilt(kjpindex, jst, njsc, flux_infilt)
       !! 2.6 K and D are recomputed for the profile of mc after infiltration
       CALL hydrol_soil_coef(kjpindex,jst,njsc)

       !! -- SECONDLY, WE UPDATE MC FROM DIFFUSION, INCLUDING DRAINAGE AND ROOTSINK

       !! 3 Prepare for diffusion based on the initial SM profile modified by infiltration and bare soil evaporation
       !!   Thus, flux=0
 
       !! 3.1 Set the tridiagonal matrix coefficients for the diffusion scheme
       CALL hydrol_soil_setup(kjpindex,jst)

       !! 3.2 Verifications for good soil humidity
       !
       !! 3.2.1 resolv serves to only run diffusion if we are not under mcr nor over mcs (for hydrol_soil_tridiag)
       resolv(:) = (mask_soiltile(:,jst) .GT. 0) .AND. & 
               & (.NOT. is_under_mcr(:)) .AND. (.NOT. is_over_mcs(:))    
       !
       !! 3.2.2 Else, we equally spread the transpiration over the layers
       sum_rootsink(:)=SUM(rootsink(:,:,jst),dim=2)
       ! AD***  since flux=0, the next 3 lines are useless
       WHERE (is_over_mcs(:))
          mc(:,1,jst) = mc(:,1,jst) - flux_top(:) * deux / dz(2)
       ENDWHERE
       ! transpiration via root_sink
       DO jsl=1, nslm
          WHERE (is_over_mcs(:))
             mc(:,jsl,jst) = mc(:,jsl,jst) - sum_rootsink(:) / (zmaxh*mille) 
          ENDWHERE
       ENDDO
       ! the rootsink will exacerbate the under_mcr problem
       DO jsl = 1, nslm
          WHERE (is_under_mcr(:))
             mc(:,jsl,jst) = mc(:,jsl,jst) - sum_rootsink(:) / (zmaxh*mille) 
          ENDWHERE
       ENDDO

       IF (ok_freeze_cwrr) THEN
          CALL hydrol_soil_coef(kjpindex,jst,njsc)
          DO ji =1, kjpindex
             DO jsl = 1, nslm
                mcl(ji,jsl,jst)= MIN(mc(ji,jsl,jst),mcr(njsc(ji))+(1-profil_froz_hydro_ns(ji,jsl,jst))*(mc(ji,jsl,jst)-mcr(njsc(ji))))
             ENDDO
          ENDDO
       ELSE
          mcl(:,:,jst)=mc(:,:,jst)
       ENDIF

       !! 3.3 We define the system of linear equations, based on matrix coefficients, 
       !! following the PhD thesis of de Rosnay (1999), p155-157
       ! rhs for right-hand side term; fp for f'; gp for g'; ep for e'; with flux=0 !
       !- First layer
       DO ji = 1, kjpindex
          tmat(ji,1,1) = zero
          tmat(ji,1,2) = f(ji,1)
          tmat(ji,1,3) = g1(ji,1)
          rhs(ji,1)    = fp(ji,1) * mcl(ji,1,jst) + gp(ji,1)*mcl(ji,2,jst) &
               &  - flux_top(ji) - (b(ji,1)+b(ji,2))/deux *(dt_sechiba/one_day) - rootsink(ji,1,jst)
       ENDDO
       !- soil body
       DO jsl=2, nslm-1
          DO ji = 1, kjpindex
             tmat(ji,jsl,1) = e(ji,jsl)
             tmat(ji,jsl,2) = f(ji,jsl)
             tmat(ji,jsl,3) = g1(ji,jsl)
             rhs(ji,jsl) = ep(ji,jsl)*mcl(ji,jsl-1,jst) + fp(ji,jsl)*mcl(ji,jsl,jst) &
                  & +  gp(ji,jsl) * mcl(ji,jsl+1,jst) & 
                  & + (b(ji,jsl-1) - b(ji,jsl+1)) * (dt_sechiba/one_day) / deux & 
                  & - rootsink(ji,jsl,jst) 
          ENDDO
       ENDDO       
       !- Last layer, including drainage
       DO ji = 1, kjpindex
          jsl=nslm
          tmat(ji,jsl,1) = e(ji,jsl)
          tmat(ji,jsl,2) = f(ji,jsl)
          tmat(ji,jsl,3) = zero
          rhs(ji,jsl) = ep(ji,jsl)*mcl(ji,jsl-1,jst) + fp(ji,jsl)*mcl(ji,jsl,jst) &
               & + (b(ji,jsl-1) + b(ji,jsl)*(un-deux*free_drain_coef(ji,jst))) * (dt_sechiba/one_day) / deux &
               & - rootsink(ji,jsl,jst)
       ENDDO
       !- Store the equations in case needed again
       DO jsl=1,nslm
          DO ji = 1, kjpindex
             srhs(ji,jsl) = rhs(ji,jsl)
             stmat(ji,jsl,1) = tmat(ji,jsl,1)
             stmat(ji,jsl,2) = tmat(ji,jsl,2)
             stmat(ji,jsl,3) = tmat(ji,jsl,3) 
          ENDDO
       ENDDO
       
       !! 4 Resolves diffusion based on the initial SM profile modified by infiltration and bare soil evaporation
       
       !! 4.1 Solves diffusion equations, but only in grid-cells where tmc is OK (else, see step 3.2.2)
       !!     The result is an updated mcl profile
       !IF (jst == 5) THEN
       !    tmcl_tmp(:) = 0
       !    DO ji = 1, kjpindex
       !       tmcl_tmp(ji) = dz(2) * ( trois*mcl(ji,1,jst) + mcl(ji,2,jst) )/huit 
       !    ENDDO
       !    DO jsl = 2,nslm-1
       !       DO ji = 1, kjpindex
       !          tmcl_tmp(ji) = tmcl_tmp(ji) + dz(jsl) &
       !               & * (trois*mcl(ji,jsl,jst)+mcl(ji,jsl-1,jst))/huit &
       !               & + dz(jsl+1) * (trois*mcl(ji,jsl,jst)+mcl(ji,jsl+1,jst))/huit
       !       ENDDO
       !    ENDDO
       !    DO ji = 1, kjpindex
       !       tmcl_tmp(ji) = tmcl_tmp(ji) + dz(nslm) &
       !            & * (trois * mcl(ji,nslm,jst) + mcl(ji,nslm-1,jst))/huit
       !    ENDDO

       !    WRITE(numout,*)'Zun 1 mc_b:',mcl(1,:,jst)
       !    WRITE(numout,*)'Zun 1 tmc_b:',tmcl_tmp(1)
       !ENDIF

       mcl_tmp(:,:,jst) = mcl(:,:,jst)
       !ZunYin
       CALL hydrol_soil_tridiag(kjpindex,jst)

       !IF (jst == 5) THEN
       !    tmcl_tmp(:) = 0
       !    DO ji = 1, kjpindex
       !       tmcl_tmp(ji) = dz(2) * ( trois*mcl(ji,1,jst) + mcl(ji,2,jst) )/huit 
       !    ENDDO
       !    DO jsl = 2,nslm-1
       !       DO ji = 1, kjpindex
       !          tmcl_tmp(ji) = tmcl_tmp(ji) + dz(jsl) &
       !               & * (trois*mcl(ji,jsl,jst)+mcl(ji,jsl-1,jst))/huit &
       !               & + dz(jsl+1) * (trois*mcl(ji,jsl,jst)+mcl(ji,jsl+1,jst))/huit
       !       ENDDO
       !    ENDDO
       !    DO ji = 1, kjpindex
       !       tmcl_tmp(ji) = tmcl_tmp(ji) + dz(nslm) &
       !            & * (trois * mcl(ji,nslm,jst) + mcl(ji,nslm-1,jst))/huit
       !    ENDDO

       !    WRITE(numout,*)'Zun 1 mc_a:',mcl(1,:,jst)
       !    WRITE(numout,*)'Zun 1 tmc_a:',tmcl_tmp(1)
       !ENDIF

       !! 4.2 Correct bad moisture contents due to numerical errors before water balance
       ! AD*** shouldn't this be done only where resolv = TRUE ??
       ! AD*** even where the diffusion was solved, don't these corrections create water conservation pbs ??
       DO ji = 1, kjpindex
          !IF ( is_crop_soil(jst) .AND. (free_drain_coef(ji,jst) .LT. min_sechiba) .AND. (MAXVAL(mcl(ji,:,jst)) .GT. mcs(njsc(ji))) &
          !    & .AND. ((irrigation_soil(ji,jst) .GT. min_sechiba) .OR. (irrigr(ji,jst) .GT. min_sechiba))) THEN
          !IF ( is_crop_soil(jst) .AND. (free_drain_coef(ji,jst) .LT. min_sechiba) .AND. (MAXVAL(mcl(ji,:,jst)) .GT. mcs(njsc(ji))) &
          IF (trigger_irrig(ji,jst)) THEN
              mc(ji,:,jst) = mcl_tmp(ji,:,jst)
          ELSE
              DO jsl = 1,nslm
                 IF (mcl(ji,jsl,jst) .LT. mcr(njsc(ji)) .AND. mcl(ji,jsl,jst) .NE. 0.) THEN
                    mcl(ji,jsl,jst)=mcr(njsc(ji))
                 END IF
                 IF (mcl(ji,jsl,jst) .GT. mcs(njsc(ji)) .AND. mcl(ji,jsl,jst) .NE. 0.) THEN
                    mcl(ji,jsl,jst)=mcs(njsc(ji))
                 END IF

                 IF (ok_freeze_cwrr) THEN
                    mc(ji,jsl,jst)=MAX(mcl(ji,jsl,jst), mcl(ji,jsl,jst)+profil_froz_hydro_ns(ji,jsl,jst)*(mc(ji,jsl,jst)-mcr(njsc(ji))))
                 ELSE
                    mc(ji,jsl,jst)=mcl(ji,jsl,jst)
                 ENDIF
              ENDDO
          ENDIF
       ENDDO

       ! Calculation of total soil moisture content (liquid + frozen)
       !IF (ok_freeze_cwrr) THEN
       !   DO ji =1, kjpindex
       !      DO jsl = 1, nslm
       !         mc(ji,jsl,jst)=MAX(mcl(ji,jsl,jst), mcl(ji,jsl,jst)+profil_froz_hydro_ns(ji,jsl,jst)*(mc(ji,jsl,jst)-mcr(njsc(ji))))
       !      ENDDO
       !   ENDDO
       !ELSE
       !   mc(:,:,jst)=mcl(:,:,jst)
       !ENDIF

       !! -- THEN, FLUXES ALLOW TO CLOSE THE WATER BALANCE

       !! 5 Closing water balance

       !! 5.1 Computes dr_ns = bottom boundary condition, consistent with rhs(ji,jsl=nslm)
       DO ji = 1, kjpindex
          dr_ns(ji,jst)=zero
          jsl=nslm
          IF (.NOT. is_under_mcr(ji)) THEN ! AD*** what happens if is_over_mcs ???
             dr_ns(ji,jst) = mask_soiltile(ji,jst)*k(ji,jsl)*free_drain_coef(ji,jst) * (dt_sechiba/one_day)
          ENDIF
       ENDDO
          
       !! 5.2 Computes total soil moisture content
       DO ji = 1, kjpindex
          tmc(ji,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit 
       ENDDO          
       DO jsl = 2,nslm-1
          DO ji = 1, kjpindex
             tmc(ji,jst) = tmc(ji,jst) + dz(jsl) &
                  & * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                  & + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
          ENDDO
       ENDDO
       DO ji = 1, kjpindex
          tmc(ji,jst) = tmc(ji,jst) + dz(nslm) &
               & * (trois * mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
       END DO

       !! 5.3 Diagnostic of upper fluxes from soil moisture variation and bottom flux
       ! AD*** should this be done where resolv=FALSE ?
       ! AD*** shouldn't we initialize run_ns to zero ?

       DO ji = 1, kjpindex
          ! qflux00 balances soil moisture variation and bottom flux+rootsink-returnflow
          ! AD*** why do we use the MIN(tmcs, tmc) ??? doesn't this create conservation flaws ?
          ! AD*** returnflow_soil=0
          !IF (is_crop_soil(jst) .AND. do_irrig_reservoir(jst) .AND. (flux_infilt(ji) .GT. min_sechiba) .AND. (free_drain_coef(ji,jst) .LT. min_sechiba)) THEN
          !    qflux00(ji,jst) = mask_soiltile(ji,jst) * &
          !         & (tmc_tmp(ji)-tmcint(ji)+SUM(rootsink(ji,:,jst))+dr_ns(ji,jst)-returnflow_soil(ji))
          !ELSE
              qflux00(ji,jst) = mask_soiltile(ji,jst) * &
                   & (MIN(tmcs(ji,jst),tmc(ji,jst))-tmcint(ji)+SUM(rootsink(ji,:,jst))+dr_ns(ji,jst)-returnflow_soil(ji))
          ! ENDIF

          ! We should also have that qflux00=precisol_ns+water2infilt-water2extract
          ! unless the adjustements to mc with respect to mcr/mcs created water imbalance
          ! In such a case, the imbalance (whether positive or negative) is reported to surface runoff
          ru_ns(ji,jst) = (precisol_ns(ji,jst) + water2infilt(ji,jst)  &
               & - water2extract(ji) - qflux00(ji,jst)) * mask_soiltile(ji,jst)

          !Zun: calculate irrigation depth irrig and cancel reservior if no
          !crops
          hascrops(ji,:) = .FALSE.
          DO jv = 2,nvm
            IF (is_crop_soil(pref_soil_veg(jv)) .AND. (lai(ji,jv) .GT. 0)) THEN
                hascrops(ji,pref_soil_veg(jv)) = .TRUE.
            ENDIF
          ENDDO

          IF (is_crop_soil(jst) .AND. do_irrig_reservoir(jst)) THEN
              IF (hascrops(ji,jst)) THEN
                  IF (ru_ns(ji,jst) .GT. reservoir_dep_max(jst)) THEN
                      ru_ns(ji,jst) = ru_ns(ji,jst) - reservoir_dep_max(jst)
                      irrigr(ji,jst) = reservoir_dep_max(jst)
                  ELSEIF (ru_ns(ji,jst) .GT. min_sechiba) THEN
                      irrigr(ji,jst) = ru_ns(ji,jst)
                      ru_ns(ji,jst) = zero
                  ELSE
                      irrigr(ji,jst) = zero
                  ENDIF
              ELSE !has reservior but has not crop anymore
                  !irrigr(ji,jst) = zero
                  IF (irrigr(ji,jst) .GT. min_sechiba) THEN
                      IF (ru_ns(ji,jst) .GT. (precisol_ns(ji,jst)+reinfiltration_soil(ji))) THEN
                          irrigr(ji,jst) =  ru_ns(ji,jst) - precisol_ns(ji,jst) - &
                              & reinfiltration_soil(ji)
                          ru_ns(ji,jst) = precisol_ns(ji,jst) + reinfiltration_soil(ji)
                      ELSE
                          irrigr(ji,jst) = zero
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF

          ! AD*** why do we further change ae_ns ??? and why has MAX(ae_ns,0) disappeared (ask A. Campoy)
          ! ae_ns+ru_ns=precisol_ns+irrigation-q0 ?
          ae_ns(ji,jst) = ae_ns(ji,jst) + subsinksoil(ji)
       ENDDO

       !! 6 Special treatment for the unstable cases
       ! AD*** NOT COMMENTED ***

       !! 6.1 Negative runoff

       IF (printlev>=3) THEN
          DO ji = 1, kjpindex
             IF (ru_ns(ji,jst) .LT. zero) THEN
                WRITE (numout,*) 'Negative runoff corrected', ji,jst,ru_ns(ji,jst), mc(ji,1,jst), tmc(ji,jst)
             ENDIF
          ENDDO
       ENDIF

       DO ji = 1, kjpindex
          IF (ru_ns(ji,jst) .LT. zero) THEN
             ! Available water in first layer
             temp(ji)=MAX(((mc(ji,1,jst)-mcr(njsc(ji))) * dz(2) / deux), zero)
             ! Calculate and add maximum water to runoff
             temp(ji)=MIN(temp(ji),ABS(ru_ns(ji,jst)))
             ru_ns(ji,jst)=ru_ns(ji,jst)+temp(ji)
             ! Update water balance
             qflux00(ji,jst) = qflux00(ji,jst) - temp(ji)
             mc(ji,1,jst)=mc(ji,1,jst)-(temp(ji) * deux / dz(2))
          END IF

          ! If still negative runoff, take water from deeper layers
          DO jsl = 2, nslm-1
             IF (ru_ns(ji,jst) .LT. zero) THEN
                temp(ji)= MAX((mc(ji,jsl,jst)-mcr(njsc(ji)))*(dz(jsl)+dz(jsl+1)) / deux, zero)
                temp(ji)=MIN(temp(ji),ABS(ru_ns(ji,jst)))
                ru_ns(ji,jst)=ru_ns(ji,jst)+temp(ji)
                qflux00(ji,jst) = qflux00(ji,jst) - temp(ji)
                mc(ji,jsl,jst)=mc(ji,jsl,jst)-temp(ji) * deux / (dz(jsl)+dz(jsl+1))
             END IF
          ENDDO
          
          ! Last layer
          IF (ru_ns(ji,jst) .LT. zero) THEN
             temp(ji)=MAX((mc(ji,nslm,jst)-mcr(njsc(ji))) * dz(nslm) / deux, zero)
             temp(ji)=MIN(temp(ji),ABS(ru_ns(ji,jst)))
             ru_ns(ji,jst)=ru_ns(ji,jst)+temp(ji)
             qflux00(ji,jst) = qflux00(ji,jst) - temp(ji)
             mc(ji,nslm,jst)=mc(ji,nslm,jst)-temp(ji) * deux / dz(nslm)
          END IF
             
          ! If still negative runoff, take water from bottom drainage
          IF (ru_ns(ji,jst) .LT. zero) THEN
             IF (printlev>=3)  WRITE (numout,*) 'runoff and drainage before correction',&
                  ru_ns(ji,jst),dr_ns(ji,jst)
             dr_ns(ji,jst)=dr_ns(ji,jst)+ru_ns(ji,jst)
             qflux00(ji,jst) = qflux00(ji,jst) + ru_ns(ji,jst)
             ru_ns(ji,jst)= 0.
          END IF
          
       ENDDO

       !! 6.2 Compute total soil moisture content
       DO ji = 1, kjpindex
          tmc(ji,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
       ENDDO
       DO jsl = 2,nslm-1
          DO ji = 1, kjpindex
             tmc(ji,jst) = tmc(ji,jst) + dz(jsl) &
                  & * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                  & + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
          ENDDO
       ENDDO
       DO ji = 1, kjpindex
          tmc(ji,jst) = tmc(ji,jst) + dz(nslm) &
               & * (trois * mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
       END DO

       !! 6.3 Avoid under-precision value for the 3 outward flux
       ! AD*** and the third one ?
       DO ji = 1, kjpindex  
          IF (ABS(ae_ns(ji,jst)).LT.min_sechiba) THEN
             ae_ns(ji,jst) = zero
          ENDIF

          IF(ABS(ru_ns(ji,jst)).LT.min_sechiba) THEN
             ru_ns(ji,jst) = zero
          ENDIF
       ENDDO

       !! 7 Surface runoff : compute temporary surface water and extract from runoff
       IF ( .NOT. doponds ) THEN
          DO ji = 1, kjpindex
             water2infilt(ji,jst) = reinf_slope(ji) * ru_ns(ji,jst)
          ENDDO
       ELSE
          DO ji = 1, kjpindex           
             water2infilt(ji,jst) = zero
          ENDDO
       ENDIF
       !
       DO ji = 1, kjpindex           
          ru_ns(ji,jst) = ru_ns(ji,jst) - water2infilt(ji,jst)
       END DO

       !! 8 Smooth again
       ! Probably not necessary but harmless (Aurelien)
       CALL hydrol_soil_smooth(kjpindex, jst, njsc, is_under_mcr, is_over_mcs)

       !! 9 Optional computation of the fluxes 
       IF ( check_cwrr ) THEN
          CALL hydrol_soil_flux(kjpindex,jst,mcint,returnflow_soil)
       ENDIF

       !! -- SM DIAGNOSTICS FOR OTHER ROUTINES, MODULES, OR NEXT STEP

       !! 10 We make some useful output
       !  Total soil moisture, soil moisture at litter levels, soil wetness...

       ! total soil moisture ! AD*** why include water2infilt here ???
       DO ji=1,kjpindex
          tmc(ji,jst)= dz(2) * (trois*mc(ji,1,jst) + mc(ji,2,jst))/huit
       END DO
       DO jsl=2,nslm-1
          DO ji=1,kjpindex
             tmc(ji,jst) = tmc(ji,jst) + dz(jsl) * ( trois*mc(ji,jsl,jst) + mc(ji,jsl-1,jst))/huit &
                  & + dz(jsl+1)*(trois*mc(ji,jsl,jst) + mc(ji,jsl+1,jst))/huit
          END DO
       END DO
       DO ji=1,kjpindex
          tmc(ji,jst) = tmc(ji,jst) +  dz(nslm) * (trois * mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
          tmc(ji,jst) = tmc(ji,jst) + water2infilt(ji,jst)
       END DO

       ! The litter is the 4 top levels of the soil
       ! Compute various field of soil moisture for the litter (used for stomate and for albedo)

       DO ji=1,kjpindex
          tmc_litter(ji,jst) = dz(2) * ( trois*mc(ji,1,jst)+ mc(ji,2,jst))/huit
       END DO
       ! sum from level 1 to 4
       DO jsl=2,4
          DO ji=1,kjpindex
             tmc_litter(ji,jst) = tmc_litter(ji,jst) + dz(jsl) * & 
                  & ( trois*mc(ji,jsl,jst) + mc(ji,jsl-1,jst))/huit &
                  & + dz(jsl+1)*(trois*mc(ji,jsl,jst) + mc(ji,jsl+1,jst))/huit
          END DO
       END DO

       ! subsequent calculation of soil_wet_litter (tmc-tmcw)/(tmcf-tmcw)

       DO ji=1,kjpindex
          soil_wet_litter(ji,jst) = MIN(un, MAX(zero,&
               & (tmc_litter(ji,jst)-tmc_litter_wilt(ji,jst)) / &
               & (tmc_litter_field(ji,jst)-tmc_litter_wilt(ji,jst)) ))
       END DO

       ! Soil wetness profiles (mc-mcw)/(mcs-mcw)
       ! soil_wet is the ratio of soil moisture to available soil moisture for plant
       ! (ie soil moisture at saturation minus soil moisture at wilting point).

       DO ji=1,kjpindex
          soil_wet(ji,1,jst) = MIN(un, MAX(zero,&
               & (trois*mc(ji,1,jst) + mc(ji,2,jst) - quatre*mcw(njsc(ji)))&
               & /(quatre*(mcs(njsc(ji))-mcw(njsc(ji)))) ))
          humrelv(ji,1,jst) = zero
       END DO
       DO jsl=2,nslm-1
          DO ji=1,kjpindex
             soil_wet(ji,jsl,jst) = MIN(un, MAX(zero,&
                  & (trois*mc(ji,jsl,jst) + & 
                  & mc(ji,jsl-1,jst) *(dz(jsl)/(dz(jsl)+dz(jsl+1))) &
                  & + mc(ji,jsl+1,jst)*(dz(jsl+1)/(dz(jsl)+dz(jsl+1))) &
                  & - quatre*mcw(njsc(ji))) / (quatre*(mcs(njsc(ji))-mcw(njsc(ji)))) ))
          END DO
       END DO
       DO ji=1,kjpindex
          soil_wet(ji,nslm,jst) = MIN(un, MAX(zero,&
               & (trois*mc(ji,nslm,jst) &
               & + mc(ji,nslm-1,jst)-quatre*mcw(njsc(ji)))/(quatre*(mcs(njsc(ji))-mcw(njsc(ji)))) ))
       END DO

       ! Compute the moderation of transpiration due to wilting point
       ! moderwilt is a factor which is zero if soil moisture is below the wilting point
       ! and is un if soil moisture is above the wilting point.

       DO jsl=1,nslm
          DO ji=1,kjpindex
             moderwilt(ji,jsl,jst) = INT( MAX(soil_wet(ji,jsl,jst), zero) + un - min_sechiba )
          END DO
       END DO

       ! Compute the new humrelv to use in sechiba:
       ! loop on each vegetation type
       humrelv(:,1,jst) = zero   

       ! calculation of us for each layer and vegetation type.
       DO jv = 2,nvm
          DO ji=1,kjpindex
             !- Here we make the assumption that roots do not take water from the 1st layer. 
             !- Comment the us=0 if you want to change this.
!             us(ji,jv,jst,1) = moderwilt(ji,1,jst)*MIN(un,((trois*mc(ji,1,jst) + mc(ji,2,jst)) &
!                  & /(quatre*mcs(njsc(ji))*pcent(jst))) )* (un-EXP(-humcste(jv)*dz(2)/mille/deux)) &
!                  & /(un-EXP(-humcste(jv)*zz(nslm)/mille))
             us(ji,jv,jst,1) = zero
             humrelv(ji,jv,jst) = MAX(us(ji,jv,jst,1),zero) ! initialisation, to zero as us(:,:,:,1)=0
          END DO
       ENDDO

       IF (ok_freeze_cwrr) THEN
           CALL hydrol_soil_coef(kjpindex,jst,njsc)
       ENDIF

       DO jsl = 2,nslm-1
          DO jv = 2, nvm
             DO ji=1,kjpindex
                ! Influence of freezing in the soil on the soil moisture
                IF (ok_freeze_cwrr) THEN
                   us_tmp= (trois*(1-profil_froz_hydro_ns(ji,jsl, jst))*mc(ji,jsl,jst)+ &
                        (1-profil_froz_hydro_ns(ji,jsl-1, jst))*mc(ji,jsl-1,jst) &
                        *(dz(jsl)/(dz(jsl)+dz(jsl+1)))+ &
                        (1-profil_froz_hydro_ns(ji,jsl+1, jst))*mc(ji,jsl+1,jst) &
                        *(dz(jsl+1)/(dz(jsl)+dz(jsl+1)))) &
                        /(quatre*mcs(njsc(ji))*pcent(njsc(ji)))

                ELSE
                   !ZunYin
                   !us_tmp= (trois*mc(ji,jsl,jst) + &
                   !     mc(ji,jsl-1,jst)*(dz(jsl)/(dz(jsl)+dz(jsl+1)))+ &
                   !     mc(ji,jsl+1,jst)*(dz(jsl+1)/(dz(jsl)+dz(jsl+1)))) &
                   !     /(quatre*mcs(njsc(ji))*pcent(njsc(ji)))

                   us_tmp= ((trois*mc(ji,jsl,jst) + &
                        mc(ji,jsl-1,jst))*(dz(jsl)/(dz(jsl)+dz(jsl+1)))+ &
                        (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))*(dz(jsl+1)/(dz(jsl)+dz(jsl+1)))) &
                        /(quatre*mcs(njsc(ji))*pcent(njsc(ji)))
                ENDIF

                !IF (jst == 5 .AND. jv == 13) THEN
                !    WRITE(numout,*)'ZUN us_tmp_',jsl,':',us_tmp
                !ENDIF
                ! us_tmp should not be negative
                !!??Aurelien: Useless, here we are sure mc between mcr and mcs
                us_tmp=MAX(us_tmp, zero)
                ! us is computed with a SQRT in order for it to grow more rapidly with soil moisture.
                ! it is not essential
                us(ji,jv,jst,jsl) = moderwilt(ji,jsl,jst) * MIN( un, SQRT(us_tmp) ) * nroot(jv,jst,jsl)
                !
                us(ji,jv,jst,jsl) = MAX(us (ji,jv,jst,jsl), zero)
                
                humrelv(ji,jv,jst) = MAX((humrelv(ji,jv,jst) + us(ji,jv,jst,jsl)),zero)
             END DO
          END DO
       ENDDO

       DO jv = 2, nvm
          DO ji=1,kjpindex

             IF (ok_freeze_cwrr) THEN
                us_tmp = (trois*(1-profil_froz_hydro_ns(ji,nslm, jst))*mc(ji,nslm,jst)  &
                     + (1-profil_froz_hydro_ns(ji,nslm-1, jst))*mc(ji,nslm-1,jst))  &
                     / (quatre*mcs(njsc(ji))*pcent(njsc(ji)))
             ELSE 
                us_tmp = (trois*mc(ji,nslm,jst) + mc(ji,nslm-1,jst))  &
                     / (quatre*mcs(njsc(ji))*pcent(njsc(ji)))
             ENDIF

            !IF (jst == 5 .AND. jv == 13) THEN
            !    WRITE(numout,*)'ZUN us_tmp_11:',us_tmp
            !ENDIF

             ! us_tmp should not be negative
             us_tmp=MAX(us_tmp, zero)
             !
             !us(ji,jv,jst,nslm) =moderwilt(ji,nslm,jst)*  &
             !     & MIN( un, SQRT(us_tmp) ) * nroot(jv,jst,nslm)
             !Zun Yin
             us(ji,jv,jst,nslm) =moderwilt(ji,nslm,jst)*  &
                  & nroot(jv,jst,nslm)

             us(ji,jv,jst,nslm) = MAX(us(ji,jv,jst,nslm), zero)
             humrelv(ji,jv,jst) = MAX(zero,MIN(un, humrelv(ji,jv,jst) + us(ji,jv,jst,nslm)))
             vegstressv(ji,jv,jst) = humrelv(ji,jv,jst)
          END DO
       END DO

        !IF (jst == 5) THEN
        !    !WRITE(numout,*)'ZUN us_13:',us(1,13,5,:)
        !    WRITE(numout,*)'ZUN mc_13:',mc(1,:,5)
        !    WRITE(numout,*)'ZUN mcs:',mcs(njsc(1))
        !ENDIF

       DO jv = 2, nvm
          DO ji = 1, kjpindex
             IF (corr_veg_soil(ji,jv,jst) .LT. min_sechiba) THEN
                humrelv(ji,jv,jst) = zero  
             ENDIF
          END DO
       END DO

       !! We need to turn off evaporation when is_under_mcr
       !! We set us, humrelv and vegstressv to zero in this case
       DO jsl = 1,nslm
          DO jv = 2, nvm
             WHERE (is_under_mcr(:))
                us(:,jv,jst,jsl) = zero
             ENDWHERE
          ENDDO
       ENDDO
       DO jv = 2, nvm
          WHERE (is_under_mcr(:))
             humrelv(:,jv,jst) = zero
             vegstressv(:,jv,jst) = zero 
          ENDWHERE
       ENDDO
       
       !! For consistency in stomate, we also set moderwilt and soil_wet to zero in this case. 
       !! They are used later for shumdiag and shumdiag_perma
       DO jsl = 1,nslm
          WHERE (is_under_mcr(:))
             moderwilt(:,jsl,jst) = zero 
             soil_wet(:,jsl,jst) = zero
          ENDWHERE
       ENDDO

       !! Calculate the volumetric soil moisture content (mc_layh and mcl_layh) needed in 
       !! thermosoil for the thermal conductivity. Calculate also total soil moisture content(tmc_layh) 
       !! needed in thermosoil for the heat capacity.
       mc_layh_s = mc
       mcl_layh_s = mc
       DO ji=1,kjpindex
          DO jsl=1,nslm
             mc_layh(ji,jsl) = mc_layh(ji,jsl) + mc(ji,jsl,jst) * soiltile(ji,jst) 
             mcl_layh(ji,jsl) = mcl_layh(ji,jsl) + mcl(ji,jsl,jst) * soiltile(ji,jst)
          ENDDO
          tmc_layh_s(ji,1,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
          DO jsl = 2,nslm-1
             tmc_layh_s(ji,jsl,jst) = dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                  + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
          ENDDO
          tmc_layh_s(ji,nslm,jst) = dz(nslm) * (trois*mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
          DO jsl = 1,nslm
             tmc_layh(ji,jsl) = tmc_layh(ji,jsl) + tmc_layh_s(ji,jsl,jst) * soiltile(ji,jst)
          ENDDO
       END DO

    !! 10bis before closing the soil water, we check the water balance of soil

       IF(check_cwrr) THEN
          DO ji = 1,kjpindex
             
             deltahum(ji) = (tmc(ji,jst) - tmcold(ji))
             diff(ji)     = precisol_ns(ji,jst)-ru_ns(ji,jst)-dr_ns(ji,jst)-tsink(ji) &
                  & + irrigation_soil(ji,jst) + irrigr(ji,jst) + returnflow_soil(ji) &
                  & + reinfiltration_soil(ji)

             test(ji) = (ABS(deltahum(ji)-diff(ji))*mask_soiltile(ji,jst) .GT. allowed_err)
          ENDDO
             
          DO ji = 1,kjpindex
             IF(test(ji)) THEN
                
                WRITE (numout,*)'CWRR pat: bilan non nul',ji,jst,njsc(ji),deltahum(ji)-diff(ji)
                WRITE (numout,*)'tmc,tmcold,diff',tmc(ji,jst),tmcold(ji),deltahum(ji)
                WRITE(numout,*) 'evapot,evapot_penm,ae_ns',evapot(ji),evapot_penm(ji),ae_ns(ji,jst)
                WRITE (numout,*)'flux_top,ru_ns,qdrain,tsink,q0,precisol',flux_top(ji),ru_ns(ji,jst), &
                     &      dr_ns(ji,jst),tsink(ji),qflux00(ji,jst),precisol_ns(ji,jst)
                WRITE (numout,*)'water2infilt',water2infilt(ji,jst)
                WRITE (numout,*)'soiltile',soiltile(ji,jst)
                WRITE (numout,*)'irrigation, returnflow, reinfiltration', &
                     & irrigation(ji),returnflow_soil(ji),reinfiltration_soil(ji)
                WRITE (numout,*)'mc',mc(ji,:,jst)
                WRITE (numout,*)'qflux',qflux(ji,:,jst)
                WRITE (numout,*)'veget_max', veget_max(ji,:)
                WRITE (numout,*)'k', k(ji,:)
                
                error=.TRUE.
                CALL ipslerr_p(2, 'hydrol_soil', 'We will STOP in the end of this subroutine.',&
                     & 'CWRR water balance check','')
             ENDIF
          ENDDO

          DO ji = 1,kjpindex
             
             IF(MINVAL(mc(ji,:,jst)).LT.-min_sechiba) THEN
                WRITE (numout,*)'CWRR MC NEGATIVE', &
                     & ji,lalo(ji,:),MINLOC(mc(ji,:,jst)),jst,mc(ji,:,jst)
                WRITE (numout,*)'evapot,evapot_penm,ae_ns',evapot(ji),evapot_penm(ji),ae_ns(ji,jst)
                WRITE (numout,*)'flux_top,ru_ns,qdrain,tsink,q0,precisol',flux_top(ji),ru_ns(ji,jst), &
                     &      dr_ns(ji,jst),tsink(ji),qflux00(ji,jst),precisol_ns(ji,jst)
                WRITE (numout,*)'water2infilt',water2infilt(ji,jst)
                WRITE (numout,*)'soiltile',soiltile(ji,jst)
                WRITE (numout,*)'irrigation, returnflow, reinfiltration', &
                     & irrigation(ji),returnflow_soil(ji),reinfiltration_soil(ji)
                WRITE (numout,*)'mc',mc(ji,:,jst)
                WRITE (numout,*)'qflux',qflux(ji,:,jst)
                WRITE (numout,*)'veget_max', veget_max(ji,:)
                WRITE (numout,*)'k', k(ji,:)
                WRITE (numout,*)'soiltile',soiltile(ji,jst)

                error=.TRUE.
                CALL ipslerr_p(2, 'hydrol_soil', 'We will STOP in the end of this subroutine.',&
                     & 'CWRR MC NEGATIVE','')
             ENDIF
          END DO

          DO ji=1,kjpindex
             IF (ru_ns(ji,jst)*soiltile(ji,jst).LT.-min_sechiba) THEN
                WRITE (numout,*) 'Negative runoff', ji,jst, mask_soiltile(ji,jst) 
                WRITE (numout,*) 'mc1, mc2', mc(ji,1,jst), mc(ji,2,jst)
                WRITE (numout,*) 'mcint1, mcint2', mcint(ji,1), mcint(ji,2)
                WRITE (numout,*) 'qflux1, flux_top', qflux(ji,nslm,jst), flux_top(ji)
                WRITE (numout,*) 'is_over_mcs, is_under_mcr, test', &
                     & is_over_mcs(ji), is_under_mcr(ji), tmc(ji,jst)-tmcint(ji)+qflux(ji,nslm,jst)+SUM(rootsink(ji,:,jst))
                WRITE (numout,*)'mc', mc(ji,:,jst)
                WRITE (numout,*)'mcint', mcint(ji,:)
                WRITE (numout,*)'qflux', qflux(ji,:,jst)
                WRITE (numout,*)'rootsink1,evapot_penm,vegtot', rootsink(ji,1,jst), evapot_penm(ji), vegtot(ji)
                WRITE (numout,*) 'ae_ns, tsink, returnflow, reinfiltration, precisol_ns, irrigation, qflux0, ru_ns', &
                     & ae_ns(ji,jst), tsink(ji), returnflow_soil(ji), reinfiltration_soil(ji), &
                     & precisol_ns(ji,jst), irrigation(ji), qflux00(ji,jst), ru_ns(ji,jst)

                error=.TRUE.
                CALL ipslerr_p(2, 'hydrol_soil', 'We will STOP in the end of this subroutine.',&
                     & 'Negative runoff, non-saturated soil','')
             ENDIF
          ENDDO
       ENDIF

       IF (printlev>=3) WRITE (numout,*) ' hydrol_soil done for jst =', jst     

       IF (ok_freeze_cwrr) THEN
          DO ji = 1, kjpindex
             kk_moy(ji,:) =kk_moy(ji,:)+soiltile(ji,jst)*k(ji,:) 
             kk(ji,:,jst)=k(ji,:)
          ENDDO
       ENDIF

    END DO  ! end of loop on soiltile

    !! -- ENDING THE MAIN LOOP ON SOILTILES

    !! 11 Summing 3d variables into 2d variables
    !
    CALL hydrol_diag_soil (kjpindex, veget_max, soiltile, njsc, runoff, drainage, &
         & evapot, vevapnu, returnflow, reinfiltration, irrigation, &
         & shumdiag,shumdiag_perma, k_litt, litterhumdiag, humrel, vegstress, drysoil_frac,tot_melt, & !pss:+
         & drunoff_tot) !pss:-

    !! -- COMPUTING EVAP_BARE_LIM_NS FOR NEXT TIME STEP, WITH A LOOP ON SOILTILES
    !
    !! 12 Calculation of evap_bare_limit : limitation factor for bare soil evaporation
    !
    evap_bare_lim(:) = zero
    evap_bare_lim_ns(:,:) = zero
!    evap_bare_lim_pft(:,:) = zero

    !!_  for each soil tile
    !   
    DO jst = 1,nstm

   !! 12.1 Save mc and tmc for use at the end of the time step
   !!      and calculate tmcint corresponding to mc without water2infilt
       DO jsl = 1, nslm
          DO ji = 1, kjpindex
             mcint(ji,jsl) = mask_soiltile(ji,jst) * mc(ji,jsl,jst)
          ENDDO
       ENDDO

       DO ji = 1, kjpindex
          temp(ji) = tmc(ji,jst)
       ENDDO

       ! Calculate tmc corresponding to mc without water2infilt and save it for later use
       DO ji = 1, kjpindex
          tmcint(ji) = dz(2) * ( trois*mcint(ji,1) + mcint(ji,2) )/huit
       ENDDO
       DO jsl = 2,nslm-1
          DO ji = 1, kjpindex
             tmcint(ji) = tmcint(ji) + dz(jsl) &
                  * (trois*mcint(ji,jsl)+mcint(ji,jsl-1))/huit &
                  + dz(jsl+1) * (trois*mcint(ji,jsl)+mcint(ji,jsl+1))/huit
          ENDDO
       ENDDO
       DO ji = 1, kjpindex
          tmcint(ji) = tmcint(ji) + dz(nslm) &
               * (trois * mcint(ji,nslm) + mcint(ji,nslm-1))/huit
       ENDDO

     !! 12.2 K and D are recomputed for the profile of mc
       CALL hydrol_soil_coef(kjpindex,jst,njsc)

     !! 12.3 Set the values for diffusion scheme
       CALL hydrol_soil_setup(kjpindex,jst)
       resolv(:) = (mask_soiltile(:,jst) .GT. 0)

     !! 12.3 compute matrix coefficients 
     !- estimate maximum evaporation flux_top in mm/step, assuming the water is available
     ! AD*** this should lead to never have evapnu>evapot_penm(ji)
     ! AD*** why isn't there rootsinks ?
     irrigrint(:,:) = irrigr(:,:)
     irrigrvir(:,:) = irrigr(:,:)
    
       DO ji = 1, kjpindex
          IF(vegtot(ji).GT.min_sechiba) THEN
             ! if soil tile has irrigation reservior and there is water in the
             ! reservoir, bare soil evaporation = evapot_penm
             IF (do_irrig_reservoir(jst) .AND. irrigr(ji,jst) .GT. min_sechiba) THEN
                 flux_top(ji) = evapot_penm(ji) * &
                      AINT(frac_bare_ns(ji,jst)+un-min_sechiba)

                 flux_top_res(ji) = MAX(zero, flux_top(ji) - irrigrvir(ji,jst))

                 irrigrvir(ji,jst) = MAX(zero, irrigrvir(ji,jst) - flux_top(ji))

                 !in the case, evaporation is larger than the water in
                 !the reservoir, and the top 4 layer are not saturated. We
                 !calculate double flux top based on soil moisture and
                 !reservoir, then calculate how much water evaporated from soil

                 IF (flux_top_res(ji) .GT. min_sechiba) THEN
                     mc_rel(ji) = tmc_litter(ji,jst)/tmc_litter_sat(ji,jst)
                     r_soil_ns(ji,jst) = exp(8.206 - 4.255 * mc_rel(ji))

                     speed = MAX(0.1, SQRT (u(ji)*u(ji) + v(ji)*v(ji)))
                     IF (speed * tq_cdrag(ji) .GT. min_sechiba) THEN
                         ra = un / (speed * tq_cdrag(ji))
                         evap_soil(ji) = evapot_penm(ji) / (un + r_soil_ns(ji,jst)/ra)
                     ELSE
                         evap_soil(ji) = evapot_penm(ji)
                     ENDIF

                     flux_top_res(ji) = flux_top_res(ji) * evap_soil(ji) * &
                          AINT(frac_bare_ns(ji,jst)+un-min_sechiba) / &
                          flux_top(ji)
                 ENDIF

             ELSE
                 ! We calculate a reduced demand, by means of a soil resistance
                 mc_rel(ji) = tmc_litter(ji,jst)/tmc_litter_sat(ji,jst)
                 ! based on SM in the top 4 soil layers (litter) to smooth variability
                 r_soil_ns(ji,jst) = exp(8.206 - 4.255 * mc_rel(ji))
               
                 ! Aerodynamic resistance
                 ! here 0.1 is min_wind, revised in rev4668, here just keep it simple
                 speed = MAX(0.1, SQRT (u(ji)*u(ji) + v(ji)*v(ji)))
                 IF (speed * tq_cdrag(ji) .GT. min_sechiba) THEN
                     ra = un / (speed * tq_cdrag(ji))
                     evap_soil(ji) = evapot_penm(ji) / (un + r_soil_ns(ji,jst)/ra)
                 ELSE
                     evap_soil(ji) = evapot_penm(ji)
                 ENDIF

                 !evap_soil(ji) = evapot_penm(ji)

                 flux_top(ji) = evap_soil(ji) * &
                      AINT(frac_bare_ns(ji,jst)+un-min_sechiba)
             ENDIF
          ELSE
             flux_top(ji) = zero
          ENDIF
       ENDDO

       IF (ok_freeze_cwrr) THEN
          CALL hydrol_soil_coef(kjpindex,jst,njsc)
          DO ji =1, kjpindex
             DO jsl = 1, nslm
                mcl(ji,jsl,jst)= MIN(mc(ji,jsl,jst),mcr(njsc(ji))+(1-profil_froz_hydro_ns(ji,jsl,jst))*(mc(ji,jsl,jst)-mcr(njsc(ji))))
             ENDDO
          ENDDO
       ELSE
          mcl(:,:,jst)=mc(:,:,jst)
       ENDIF

       !- First layer
       DO ji = 1, kjpindex
          tmat(ji,1,1) = zero
          tmat(ji,1,2) = f(ji,1)
          tmat(ji,1,3) = g1(ji,1)
          IF (do_irrig_reservoir(jst)) THEN
              rhs(ji,1)    = fp(ji,1) * mcl(ji,1,jst) + gp(ji,1)*mcl(ji,2,jst) &
                   - flux_top_res(ji) - (b(ji,1)+b(ji,2))/deux *(dt_sechiba/one_day)
          ELSE
              rhs(ji,1)    = fp(ji,1) * mcl(ji,1,jst) + gp(ji,1)*mcl(ji,2,jst) &
                   - flux_top(ji) - (b(ji,1)+b(ji,2))/deux *(dt_sechiba/one_day)
          ENDIF
       ENDDO
       !- soil body
       DO jsl=2, nslm-1
          DO ji = 1, kjpindex
             tmat(ji,jsl,1) = e(ji,jsl)
             tmat(ji,jsl,2) = f(ji,jsl)
             tmat(ji,jsl,3) = g1(ji,jsl)
             rhs(ji,jsl) = ep(ji,jsl)*mcl(ji,jsl-1,jst) + fp(ji,jsl)*mcl(ji,jsl,jst) &
                  +  gp(ji,jsl) * mcl(ji,jsl+1,jst) &
                  + (b(ji,jsl-1) - b(ji,jsl+1)) * (dt_sechiba/one_day) / deux
          ENDDO
       ENDDO
       !- Last layer
       DO ji = 1, kjpindex
          jsl=nslm
          tmat(ji,jsl,1) = e(ji,jsl)
          tmat(ji,jsl,2) = f(ji,jsl)
          tmat(ji,jsl,3) = zero
          rhs(ji,jsl) = ep(ji,jsl)*mcl(ji,jsl-1,jst) + fp(ji,jsl)*mcl(ji,jsl,jst) &
               + (b(ji,jsl-1) + b(ji,jsl)*(un-deux*free_drain_coef(ji,jst))) * (dt_sechiba/one_day) / deux
       ENDDO
       !- Store the equations for later use (15.5.2)
       DO jsl=1,nslm
          DO ji = 1, kjpindex
             srhs(ji,jsl) = rhs(ji,jsl)
             stmat(ji,jsl,1) = tmat(ji,jsl,1)
             stmat(ji,jsl,2) = tmat(ji,jsl,2)
             stmat(ji,jsl,3) = tmat(ji,jsl,3)
          ENDDO
       ENDDO

       !IF (jst == 5) THEN
       !    WRITE(numout,*)'Zun 2 mc_b:',mcl(1,:,jst)
       !ENDIF
    !! 12.5.1 Resolution with flux=evapot_penm (update mcl)
       CALL hydrol_soil_tridiag(kjpindex,jst)

       !IF (jst == 5) THEN
       !    WRITE(numout,*)'Zun 2 mc_a:',mcl(1,:,jst)
       !ENDIF

    !! 12.5.2 Resolution with mc(1)=mcr
       ! hydrol_soil_tridiag calcule les mc recursivement
       ! a partir du haut en fonction de rhs et tmat. Ci-dessous, on reutilise ces valeurs, sauf pour mc(1)=mcr et tmat associes

       !! Reset the coefficient for diffusion (only used if resolv(ji) = .TRUE.)
       DO ji = 1, kjpindex
          IF (do_irrig_reservoir(jst)) THEN
              resolv(ji) = (mcl(ji,1,jst).LT.(mcr(njsc(ji))).AND.flux_top_res(ji).GT.min_sechiba)
          ELSE
              resolv(ji) = (mcl(ji,1,jst).LT.(mcr(njsc(ji))).AND.flux_top(ji).GT.min_sechiba)
          ENDIF
       ENDDO
       DO jsl=1,nslm
          !- The new condition is to put the upper layer at residual soil moisture
          DO ji = 1, kjpindex
             rhs(ji,jsl) = srhs(ji,jsl)
             tmat(ji,jsl,1) = stmat(ji,jsl,1)
             tmat(ji,jsl,2) = stmat(ji,jsl,2)
             tmat(ji,jsl,3) = stmat(ji,jsl,3)
          END DO
       END DO
       
       DO ji = 1, kjpindex
          tmat(ji,1,2) = un
          tmat(ji,1,3) = zero
          rhs(ji,1) = mcr(njsc(ji))
       ENDDO
       
       !! Resolve the equations with new boundary conditions if necessary (update mcl)
       CALL hydrol_soil_tridiag(kjpindex,jst)

       ! Calculation of total soil moisture content (liquid + frozen)
       IF (ok_freeze_cwrr) THEN
          CALL hydrol_soil_coef(kjpindex,jst,njsc)           
          DO ji =1, kjpindex
             DO jsl = 1, nslm
                mc(ji,jsl,jst)=MAX(mcl(ji,jsl,jst), mcl(ji,jsl,jst)+profil_froz_hydro_ns(ji,jsl,jst)*(mc(ji,jsl,jst)-mcr(njsc(ji))))
             ENDDO
          ENDDO
       ELSE
          mc(:,:,jst)=mcl(:,:,jst)
       ENDIF


       !! Correct bad moisture content due to numerical errors before water balance
       DO ji = 1, kjpindex
          DO jsl = 1,nslm
             IF (mc(ji,jsl,jst) .LT. mcr(njsc(ji)) .AND. mc(ji,jsl,jst) .NE. 0.) THEN
                mc(ji,jsl,jst)=mcr(njsc(ji))
             END IF
             IF (mc(ji,jsl,jst) .GT. mcs(njsc(ji)) .AND. mc(ji,jsl,jst) .NE. 0.) THEN
                mc(ji,jsl,jst)=mcs(njsc(ji))
             END IF
          ENDDO
       ENDDO

    !! 12.6 Water balance

    !! 12.6.1 Compute dr_ns with the bottom boundary condition 

    ! Initialize qflux at bottom of diffusion
       DO ji = 1, kjpindex
          flux_bottom(ji) = mask_soiltile(ji,jst)*k(ji,nslm)*free_drain_coef(ji,jst) * (dt_sechiba/one_day)
       ENDDO

    !! 12.6.2 Compute total soil moisture content
       DO ji = 1, kjpindex
          tmc(ji,jst) = dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
       ENDDO       
       DO jsl = 2,nslm-1
          DO ji = 1, kjpindex
             tmc(ji,jst) = tmc(ji,jst) + dz(jsl) &
                  * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                  + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit
          ENDDO
       ENDDO
       DO ji = 1, kjpindex
          tmc(ji,jst) = tmc(ji,jst) + dz(nslm) &
               * (trois * mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
       END DO
    
    !! 12.6.3 Deduce upper flux from soil moisture variation and bottom flux
    !! TMCi-D-E=TMC   
       ! AD*** what about rootsink ??? + shouldn't we check/force that evap_bare_lim_ns LE evapot_penman ?
       ! AD*** et pourquoi c'est l'humidite totale qui intervient ici ??? 
       ! c'est pour deduire flux au top quand on ne le connait pas : cas ou on impose mc1=mcr
       ! AD*** et si evap_bare_lim_ns<0 ?? car on suppose que tmcint > tmc(new)
       ! water2inflit permet de propager de la ponded water d'un pas de temps a l'autre:
       ! peut-on s'en servir pour creer des cas d'evapnu potentielle ? a gerer dans diffuco ?
       DO ji = 1, kjpindex
           IF (do_irrig_reservoir(jst)) THEN
              evap_bare_lim_ns(ji,jst) = mask_soiltile(ji,jst) * &
                   (irrigrint(ji,jst) - irrigrvir(ji,jst) + tmcint(ji)-tmc(ji,jst)-flux_bottom(ji))
           ELSE
              evap_bare_lim_ns(ji,jst) = mask_soiltile(ji,jst) * &
                   (tmcint(ji)-tmc(ji,jst)-flux_bottom(ji))
           ENDIF
       END DO

       !IF (jst == 5) THEN
       !    WRITE(numout,*)'Zun bare_lim_ns_1:',evap_bare_lim_ns(1,:)
       !ENDIF

    !! 12.7 Determination of evap_bare_lim_ns
       !DO ji = 1, kjpindex
       !   ! Here we weight evap_bare_lim_ns by the fraction of bare evaporating soil. 
       !   ! This is given by frac_bare_ns, taking into account bare soil under vegetation
       !   IF(vegtot(ji) .GT. min_sechiba) THEN
       !      evap_bare_lim_ns(ji,jst) = evap_bare_lim_ns(ji,jst) * frac_bare_ns(ji,jst)
       !   ELSE
       !      evap_bare_lim_ns(ji,jst) = 0.
       !   ENDIF
       !END DO

       !IF (jst == 6) THEN
       !    WRITE(numout,*)'Zun bare_lim_ns_1:',evap_bare_lim_ns(1,6)
       !    WRITE(numout,*)'Zun frac_bare_ns:',frac_bare_ns(1,6)
       !ENDIF

       ! We divide by evapot, which is consistent with diffuco (evap_bare_lim_ns < evapot_penm/evapot)
       ! Further decrease if tmc_litter is below the wilting point
       DO ji=1,kjpindex
          IF ((evapot(ji).GT.min_sechiba) .AND. &
               (tmc_litter(ji,jst).GT.(tmc_litter_wilt(ji,jst)))) THEN
             evap_bare_lim_ns(ji,jst) = evap_bare_lim_ns(ji,jst) / evapot(ji)
             !Noted by Xuhui, evapot_penm is not suitable to use here.
             !evap_bare_lim_ns(ji,jst) = evap_bare_lim_ns(ji,jst) / evapot_penm(ji)
          ELSEIF((evapot(ji).GT.min_sechiba).AND. &
               (tmc_litter(ji,jst).GT.(tmc_litter_res(ji,jst)))) THEN
             evap_bare_lim_ns(ji,jst) =  (un/deux) * evap_bare_lim_ns(ji,jst) / evapot(ji)
             !evap_bare_lim_ns(ji,jst) =  (un/deux) * evap_bare_lim_ns(ji,jst) / evapot_penm(ji)
             ! AD*** ceci, tres arbitraire, non modifie par AC
             ! on peut reduire evap_bare_lim_ns en passant au carre par exemple....
          ELSE
             evap_bare_lim_ns(ji,jst) = zero
          END IF
          evap_bare_lim_ns(ji,jst)=MAX(MIN(evap_bare_lim_ns(ji,jst),1.),0.)
       END DO

       DO ji = 1, kjpindex
          ! Here we weight evap_bare_lim_ns by the fraction of bare evaporating soil. 
          ! This is given by frac_bare_ns, taking into account bare soil under vegetation
          IF(vegtot(ji) .GT. min_sechiba) THEN
             evap_bare_lim_ns(ji,jst) = evap_bare_lim_ns(ji,jst) * frac_bare_ns(ji,jst)
          ELSE
             evap_bare_lim_ns(ji,jst) = 0.
          ENDIF
       END DO
       
       ! Set evap_bare_lim_ns to zero if is_under_mcr (cf us, humrelv, vegstressv)
       WHERE (is_under_mcr(:))
          evap_bare_lim_ns(:,jst) = zero
       ENDWHERE

       !IF (jst == 6) THEN
       !    WRITE(numout,*)'Zun bare_lim_ns_2:',evap_bare_lim_ns(1,6)
       !    WRITE(numout,*)'Zun evapot:',evapot(1)
       !ENDIF

    !! 12.8 Restore mc and tmc
       DO jsl = 1, nslm
          DO ji = 1, kjpindex
             mc(ji,jsl,jst) = mask_soiltile(ji,jst) * mcint(ji,jsl)
          ENDDO
       ENDDO

       DO ji = 1, kjpindex
          tmc(ji,jst) = temp(ji)
       ENDDO

    ENDDO !end loop on tiles

    !! 12.9 Deduction of evap_bare_lim
    DO ji = 1, kjpindex
       evap_bare_lim(ji) =  SUM(evap_bare_lim_ns(ji,:)*vegtot(ji)*soiltile(ji,:))
!       DO jv = 1,nvm
!           evap_bare_lim_pft(ji,jv) = MAX(MIN(evap_bare_lim_ns(ji,pref_soil_veg(jv)), evap_bare_lim_pft(ji,jv)), 0. )
!       ENDDO
    ENDDO

    !! 13 Exit if error was found previously in this subroutine
    !
    IF ( error ) THEN
       WRITE(numout,*) 'One or more errors have been detected in hydrol_soil. Model stops.'
       CALL ipslerr_p(3, 'hydrol_soil', 'We will STOP now.',&
                  & 'One or several fatal errors were found previously.','')
    END IF

  END SUBROUTINE hydrol_soil


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_soil_infilt
!!
!>\BRIEF        Infiltration
!!
!! DESCRIPTION  :
!! - 1 First layer
!! - 2 Infiltration layer by layer 
!! - 2.1 Initialisation
!! - 2.2 Infiltrability of each layer if under a saturated one
!! - 2.3 Compute the mean rate at which water actually infiltrate:
!! - 2.4 From which we deduce the time it takes to fill up the layer or to end the time step...
!! - 2.5 The water enters in the layer
!! - 3 Verification
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_soil_infilt

  SUBROUTINE hydrol_soil_infilt(kjpindex, ins, njsc, flux_infilt)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    ! GLOBAL (in or inout)
    INTEGER(i_std), INTENT(in)                        :: kjpindex        !! Domain size
    INTEGER(i_std), INTENT(in)                        :: ins             !! temporal index of soil tile
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)  :: njsc            !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)    :: flux_infilt     !! Water to infiltrate
                                                                         !!  @tex $(kg m^{-2})$ @endtex

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                :: ji, jsl             !! Indices
    REAL(r_std), DIMENSION (kjpindex)             :: wat_inf_pot         !! infiltrable water in the layer
    REAL(r_std), DIMENSION (kjpindex)             :: wat_inf             !! infiltrated water in the layer
    REAL(r_std), DIMENSION (kjpindex)             :: dt_tmp              !! time remaining before the end of the time step
    REAL(r_std), DIMENSION (kjpindex)             :: dt_inf              !! the time it takes to complete the infiltration in the 
                                                                         !! layer 
    REAL(r_std)                                   :: k_m                 !! the mean conductivity used for the saturated front
    REAL(r_std), DIMENSION (kjpindex)             :: infilt_tmp          !! infiltration rate for the considered layer 
    REAL(r_std), DIMENSION (kjpindex)             :: infilt_tot          !! total infiltration 
    REAL(r_std), DIMENSION (kjpindex)             :: flux_tmp            !! rate at which precip hits the ground

!_ ================================================================================================================================

    ! If data (or coupling with GCM) was available, a parameterization for subgrid rainfall could be performed

    DO ji = 1, kjpindex
       !-
    !_ 1 First layer
       !-
       ! First we fill up the first layer (about 1mm) without any resistance and quasi-immediately
       wat_inf_pot(ji) = MAX((mcs(njsc(ji))-mc(ji,1,ins)) * dz(2) / deux, zero)
       wat_inf(ji) = MIN(wat_inf_pot(ji), flux_infilt(ji))
       mc(ji,1,ins) = mc(ji,1,ins) + wat_inf(ji) * deux / dz(2)
       !
    ENDDO
    !-
    !! 2 Infiltration layer by layer 
    !! 2.1 Initialisation
    ! Initialize a countdown for infiltration during the time-step and the value of potential runoff
    dt_tmp(:) = dt_sechiba / one_day
    infilt_tot(:) = wat_inf(:)
    ! Compute the rate at which water will try to infiltrate each layer
    ! flux_temp is converted here to the same unit as k_m
    flux_tmp(:) = (flux_infilt(:)-wat_inf(:)) / dt_tmp(:)
    !
    DO jsl = 2, nslm-1
       DO ji = 1, kjpindex
    !! 2.2 Infiltrability of each layer if under a saturated one
          ! This is computed by an simple arithmetic average because 
          ! the time step (30min) is not appropriate for a geometric average (advised by Haverkamp and Vauclin)
          k_m = (k(ji,jsl) + ks(njsc(ji))*kfact(jsl-1,njsc(ji))*kfact_root(ji,jsl,ins)) / deux 

          IF (ok_freeze_cwrr) THEN
             IF (temp_hydro(ji, jsl) .LT. ZeroCelsius) THEN
                k_m = k(ji,jsl)
             ENDIF
          ENDIF

    !! 2.3 We compute the mean rate at which water actually infiltrate:
          ! Subgrid: Exponential distribution of k around k_m, but average p directly used 
          ! See d'Orgeval 2006, p 78, but it's not fully clear to me (AD***)

          infilt_tmp(ji) = k_m * (un - EXP(- flux_tmp(ji) / k_m)) 

    !! 2.4 From which we deduce the time it takes to fill up the layer or to end the time step...
          wat_inf_pot(ji) =  MAX((mcs(njsc(ji))-mc(ji,jsl,ins)) * (dz(jsl) + dz(jsl+1)) / deux, zero)
          IF ( infilt_tmp(ji) > min_sechiba) THEN
             dt_inf(ji) =  MIN(wat_inf_pot(ji)/infilt_tmp(ji), dt_tmp(ji))
             ! The water infiltration TIME has to limited by what is still available for infiltration.
             IF ( dt_inf(ji) * infilt_tmp(ji) > flux_infilt(ji)-infilt_tot(ji) ) THEN
                dt_inf(ji) = MAX(flux_infilt(ji)-infilt_tot(ji),zero)/infilt_tmp(ji)
             ENDIF
          ELSE
             dt_inf(ji) = dt_tmp(ji)
          ENDIF

    !! 2.5 The water enters in the layer
          wat_inf(ji) = dt_inf(ji) * infilt_tmp(ji)
          ! bviously the moisture content
          mc(ji,jsl,ins) = mc(ji,jsl,ins) + &
               & wat_inf(ji) * deux / (dz(jsl) + dz(jsl+1))
          ! the time remaining before the next time step
          dt_tmp(ji) = dt_tmp(ji) - dt_inf(ji)
          ! and finally the infilt_tot (which is just used to check if there is a problem, below) 
          infilt_tot(ji) = infilt_tot(ji) + infilt_tmp(ji) * dt_inf(ji)
       ENDDO
    ENDDO
    
    !! 3 Verification
    DO ji = 1, kjpindex
       IF (infilt_tot(ji) .LT. -min_sechiba .OR. infilt_tot(ji) .GT. flux_infilt(ji) + min_sechiba) THEN
          WRITE (numout,*) 'Error in the calculation of infilt tot', infilt_tot(ji)
          WRITE (numout,*) 'k, ji, jst, mc', k(ji,1:2), ji, ins, mc(ji,1,ins)
          CALL ipslerr_p(3, 'hydrol_soil_infilt', 'We will STOP now.','Error in calculation of infilt tot','')
       ENDIF
    ENDDO

  END SUBROUTINE hydrol_soil_infilt


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_soil_smooth
!!
!>\BRIEF        : Modifies the soil moisture profile to avoid over-saturation or under-residual values, 
!!                then diagnoses the points where such "excess" values remain. 
!!
!! DESCRIPTION  :
!! The "excesses" (over-saturation or under-residual) are corrected from top to bottom, by transfer of excesses 
!! to the lower layers. The reverse transfer is performed to smooth any remaining "excess" in the bottom layer.
!! If some "excess" remain afterwards, the entire soil profile is at the threshold value (mcs or mcr), 
!! and the remaining "excess" is necessarily concentrated in the top layer. 
!! This allowing diagnosing the flags is_over_mcs and is_under_mcr. 
!! Eventually, the remaining "excess" is split over the entire profile
! AD*** but I don't undersand the weighting scheme
!! - 1 Avoid over-saturation values
!! - 1.1 smoothing from top to bottom
!! - 1.2 smoothing from bottom to top
!! - 1.3 diagnoses is_over_mcs(ji), and updates the entire profile
!! - 2 Avoid below residual values
!! - 2.1 smoothing from top to bottom
!! - 2.2 smoothing from bottom to top
!! - 2.3 diagnoses is_under_mcr(ji), and updates the entire profile
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_soil_smooth

  SUBROUTINE hydrol_soil_smooth(kjpindex, ins, njsc, is_under_mcr, is_over_mcs)

    !- arguments

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                         :: kjpindex        !! Domain size
    INTEGER(i_std), INTENT(in)                         :: ins             !! Soiltile index (1-nstm, unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: njsc            !! Index of the dominant soil textural class in grid cell 
                                                                          !! (1-nscm, unitless)

    !! 0.2 Output variables

    LOGICAL, DIMENSION(kjpindex), INTENT(out)          :: is_under_mcr    !! Flag diagnosing under residual soil moisture  
    LOGICAL, DIMENSION(kjpindex), INTENT(out)          :: is_over_mcs     !! Flag diagnosing over saturated soil moisture 

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                       :: ji,jsl
    REAL(r_std)                          :: excess
    REAL(r_std), DIMENSION(kjpindex)     :: excessji

!_ ================================================================================================================================       
    !-
    !! 1 Avoid over-saturation values
    !-

    ! in case of over-saturation we put the water where it is possible

    !! 1.1 smoothing from top to bottom
    DO jsl = 1, nslm-2
       DO ji=1, kjpindex
          excess = MAX(mc(ji,jsl,ins)-mcs(njsc(ji)),zero)
          mc(ji,jsl,ins) = mc(ji,jsl,ins) - excess
          mc(ji,jsl+1,ins) = mc(ji,jsl+1,ins) + excess * &
               &  (dz(jsl)+dz(jsl+1))/(dz(jsl+1)+dz(jsl+2))
       ENDDO
    ENDDO

    jsl = nslm-1
    DO ji=1, kjpindex
       excess = MAX(mc(ji,jsl,ins)-mcs(njsc(ji)),zero)
       mc(ji,jsl,ins) = mc(ji,jsl,ins) - excess
       mc(ji,jsl+1,ins) = mc(ji,jsl+1,ins) + excess * &
            &  (dz(jsl)+dz(jsl+1))/dz(jsl+1)
    ENDDO

    jsl = nslm
    DO ji=1, kjpindex
       excess = MAX(mc(ji,jsl,ins)-mcs(njsc(ji)),zero)
       mc(ji,jsl,ins) = mc(ji,jsl,ins) - excess
       mc(ji,jsl-1,ins) = mc(ji,jsl-1,ins) + excess * &
            &  dz(jsl)/(dz(jsl-1)+dz(jsl))
    ENDDO

    !! 1.2 smoothing from bottom to top
    DO jsl = nslm-1,2,-1
       DO ji=1, kjpindex
          excess = MAX(mc(ji,jsl,ins)-mcs(njsc(ji)),zero)
          mc(ji,jsl,ins) = mc(ji,jsl,ins) - excess
          mc(ji,jsl-1,ins) = mc(ji,jsl-1,ins) + excess * &
               &  (dz(jsl)+dz(jsl+1))/(dz(jsl-1)+dz(jsl))
       ENDDO
    ENDDO

    !! 1.3 diagnoses is_over_mcs(ji), and updates the entire profile 
    DO ji=1, kjpindex
       excessji(ji) = mask_soiltile(ji,ins) * MAX(mc(ji,1,ins)-mcs(njsc(ji)),zero)
    ENDDO

    DO ji=1, kjpindex
       mc(ji,1,ins) = mc(ji,1,ins) - excessji(ji) ! then mc(1)=mcs

       is_over_mcs(ji) = (excessji(ji) .GT. min_sechiba)
    ENDDO
    ! the amount of water corresponding to excess in the top soil layer is redistributed in all soil layers
    ! AD*** I don't understand the weighting by dz(2) / (deux * zmaxh*mille)
    DO jsl = 1, nslm
       DO ji=1, kjpindex
          mc(ji,jsl,ins) = mc(ji,jsl,ins) + excessji(ji) * dz(2) / (deux * zmaxh*mille)
       ENDDO
    ENDDO
    ! AD*** it can lead to mc(jsl) > mcs depending on the value of excess
    ! AD*** does it make sense to start the last loop at jsl=1 ?? as mc(1) has already been corrected

    !-
    !! 2 Avoid below residual values
    !-

    ! Smoothes the profile to avoid negative values of punctual soil moisture

    !! 2.1 smoothing from top to bottom
    DO jsl = 1,nslm-2
       DO ji=1, kjpindex
          excess = MAX(mcr(njsc(ji))-mc(ji,jsl,ins),zero)
          mc(ji,jsl,ins) = mc(ji,jsl,ins) + excess
          mc(ji,jsl+1,ins) = mc(ji,jsl+1,ins) - excess * &
               &  (dz(jsl)+dz(jsl+1))/(dz(jsl+1)+dz(jsl+2))
       ENDDO
    ENDDO

    jsl = nslm-1
    DO ji=1, kjpindex
       excess = MAX(mcr(njsc(ji))-mc(ji,jsl,ins),zero)
       mc(ji,jsl,ins) = mc(ji,jsl,ins) + excess
       mc(ji,jsl+1,ins) = mc(ji,jsl+1,ins) - excess * &
            &  (dz(jsl)+dz(jsl+1))/dz(jsl+1)
    ENDDO

    jsl = nslm
    DO ji=1, kjpindex
       excess = MAX(mcr(njsc(ji))-mc(ji,jsl,ins),zero)
       mc(ji,jsl,ins) = mc(ji,jsl,ins) + excess
       mc(ji,jsl-1,ins) = mc(ji,jsl-1,ins) - excess * &
            &  dz(jsl)/(dz(jsl-1)+dz(jsl))
    ENDDO

    !! 2.2 smoothing from bottom to top
    DO jsl = nslm-1,2,-1
       DO ji=1, kjpindex
          excess = MAX(mcr(njsc(ji))-mc(ji,jsl,ins),zero)
          mc(ji,jsl,ins) = mc(ji,jsl,ins) + excess
          mc(ji,jsl-1,ins) = mc(ji,jsl-1,ins) - excess * &
               &  (dz(jsl)+dz(jsl+1))/(dz(jsl-1)+dz(jsl))
       ENDDO
    ENDDO

    !! 2.3 diagnoses is_under_mcr(ji), and updates the entire profile 
    DO ji=1, kjpindex
       excessji(ji) = mask_soiltile(ji,ins) * MAX(mcr(njsc(ji))-mc(ji,1,ins),zero)
    ENDDO
    DO ji=1, kjpindex
       mc(ji,1,ins) = mc(ji,1,ins) + excessji(ji) ! then mc(1)=mcr
       is_under_mcr(ji) = (excessji(ji) .GT. min_sechiba)
    ENDDO

    ! the amount of water corresponding to excess in the top soil layer is redistributed in all soil layers
    ! AD*** I don't understand the weighting by dz(2) / (deux * zmaxh*mille)
    DO jsl = 1, nslm
       DO ji=1, kjpindex
          mc(ji,jsl,ins) = mc(ji,jsl,ins) - excessji(ji) * dz(2) / (deux * zmaxh*mille)
       ENDDO
    ENDDO
    ! AD*** this can lead to mc(jsl) < mcr depending on the value of excess
    ! AD*** does it make sense to start the last loop at jsl=1 ?? as mc(1) has already been corrected

    ! We just make sure that mc remains at 0 where soiltile=0
    DO jsl = 1, nslm
       DO ji=1, kjpindex
          mc(ji,jsl,ins) = mask_soiltile(ji,ins) * mc(ji,jsl,ins)
       ENDDO
    ENDDO
    
  END SUBROUTINE hydrol_soil_smooth


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_soil_flux
!!
!>\BRIEF        This subroutine computes the hydrological fluxes between the different soil layers.
!!
!! DESCRIPTION  :
!! - 1 Initialize qflux from the bottom, with dr_ns
!! - 2 between layer nslm and nslm-1   
!! - 3 we go to top and deduct qflux(1:nslm-2)   
!! - 4 Water balance verification
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_soil_flux

  SUBROUTINE hydrol_soil_flux(kjpindex,ins,mcint,returnflow_soil)
    !
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                         :: kjpindex        !! Domain size
    INTEGER(i_std), INTENT(in)                         :: ins             !! index of soil type
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT(in) :: mcint           !! mc values at the beginning of the time step
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)      :: returnflow_soil !! returnflow

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                     :: jsl,ji
    REAL(r_std), DIMENSION(kjpindex)                   :: temp

!_ ================================================================================================================================    
    !- Compute the flux at every level from bottom to top (using mc and sink values)
    DO ji = 1, kjpindex
    !! 1 Initialize qflux from the bottom, with dr_ns
       jsl = nslm
       qflux(ji,jsl,ins) = dr_ns(ji,ins) - returnflow_soil(ji)
    !!_ between layer nslm and nslm-1   
       jsl = nslm-1
       qflux(ji,jsl,ins) = qflux(ji,jsl+1,ins) & 
            &  + (mc(ji,jsl,ins)-mcint(ji,jsl) &
            &  + trois*mc(ji,jsl+1,ins) - trois*mcint(ji,jsl+1)) &
            &  * (dz(jsl+1)/huit) &
            &  + rootsink(ji,jsl+1,ins) 
    ENDDO
    !! 3 we go to top and deduct qflux(1:nslm-2)   
    DO jsl = nslm-2,1,-1
       DO ji = 1, kjpindex
          qflux(ji,jsl,ins) = qflux(ji,jsl+1,ins) & 
               &  + (mc(ji,jsl,ins)-mcint(ji,jsl) &
               &  + trois*mc(ji,jsl+1,ins) - trois*mcint(ji,jsl+1)) &
               &  * (dz(jsl+1)/huit) &
               &  + rootsink(ji,jsl+1,ins) &
               &  + (dz(jsl+2)/huit) &
               &  * (trois*mc(ji,jsl+1,ins) - trois*mcint(ji,jsl+1) &
               &  + mc(ji,jsl+2,ins)-mcint(ji,jsl+2)) 
       END DO
    ENDDO
    
    !! 4 Water balance verification  
    DO ji = 1, kjpindex
       temp(ji) =  qflux(ji,1,ins) + (dz(2)/huit) &
            &  * (trois* (mc(ji,1,ins)-mcint(ji,1)) + (mc(ji,2,ins)-mcint(ji,2))) &
            &  + rootsink(ji,1,ins)
    ENDDO

    DO ji = 1, kjpindex
       IF (ABS(qflux00(ji,ins)-temp(ji)).GT. deux*min_sechiba) THEN
          WRITE(numout,*) 'Problem in the water balance, qflux computation', qflux00(ji,ins),temp(ji)
          WRITE (numout,*) 'returnflow_soil', returnflow_soil(ji)
          WRITE(numout,*) 'ji', ji, 'jsl',jsl,'ins',ins
          WRITE(numout,*) 'mcint', mcint(ji,:)
          WRITE(numout,*) 'mc', mc(ji,:,ins)
          WRITE (numout,*) 'rootsink', rootsink(ji,1,ins)
          CALL ipslerr_p(3, 'hydrol_soil_flux', 'We will STOP now.',&
               & 'Problem in the water balance, qflux computation','')
       ENDIF
    ENDDO

  END SUBROUTINE hydrol_soil_flux


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_soil_tridiag
!!
!>\BRIEF        This subroutine solves a set of linear equations which has a tridiagonal coefficient matrix. 
!!
!! DESCRIPTION  : It is only applied in the grid-cells where resolv(ji)=TRUE
!! 
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : mcl (global module variable)
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_soil_tridiag 

  SUBROUTINE hydrol_soil_tridiag(kjpindex,ins)

    !- arguments

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                         :: kjpindex        !! Domain size
    INTEGER(i_std), INTENT(in)                         :: ins             !! number of soil type

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                     :: ji,jsl
    REAL(r_std), DIMENSION(kjpindex)                   :: bet
    REAL(r_std), DIMENSION(kjpindex,nslm)              :: gam

!_ ================================================================================================================================
    DO ji = 1, kjpindex

       IF (resolv(ji)) THEN
          bet(ji) = tmat(ji,1,2)
          mcl(ji,1,ins) = rhs(ji,1)/bet(ji)
       ENDIF
    ENDDO

    DO jsl = 2,nslm
       DO ji = 1, kjpindex
          
          IF (resolv(ji)) THEN

             gam(ji,jsl) = tmat(ji,jsl-1,3)/bet(ji)
             bet(ji) = tmat(ji,jsl,2) - tmat(ji,jsl,1)*gam(ji,jsl)
             mcl(ji,jsl,ins) = (rhs(ji,jsl)-tmat(ji,jsl,1)*mcl(ji,jsl-1,ins))/bet(ji)
          ENDIF

       ENDDO
    ENDDO

    DO ji = 1, kjpindex
       IF (resolv(ji)) THEN
          DO jsl = nslm-1,1,-1
             mcl(ji,jsl,ins) = mcl(ji,jsl,ins) - gam(ji,jsl+1)*mcl(ji,jsl+1,ins)
          ENDDO
       ENDIF
    ENDDO

  END SUBROUTINE hydrol_soil_tridiag


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_soil_coef
!!
!>\BRIEF        Computes coef for the linearised hydraulic conductivity 
!! k_lin=a_lin mc_lin+b_lin and the linearised diffusivity d_lin. 
!!
!! DESCRIPTION  :
!! First, we identify the interval i in which the current value of mc is located.
!! Then, we give the values of the linearized parameters to compute 
!! conductivity and diffusivity as K=a*mc+b and d.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_soil_coef
 
  SUBROUTINE hydrol_soil_coef(kjpindex,ins,njsc)

    IMPLICIT NONE
    !
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                        :: kjpindex         !! Domain size
    INTEGER(i_std), INTENT(in)                        :: ins              !! Index of soil type
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)  :: njsc             !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                    :: jsl,ji,i
    REAL(r_std)                                       :: mc_ratio
    REAL(r_std)                                       :: mc_used    !! Used liquid water content
    REAL(r_std)                                       :: x,m
    
!_ ================================================================================================================================

    IF (ok_freeze_cwrr) THEN
    
       ! Calculation of liquid and frozen saturation degrees with respect to residual
       ! x=liquid saturation degree/residual=(mcl-mcr)/(mcs-mcr)
       ! 1-x=frozen saturation degree/residual=(mcf-mcr)/(mcs-mcr) (=profil_froz_hydro)
       
       DO jsl=1,nslm
          DO ji=1,kjpindex
             ! Van Genuchten parameter for thermodynamical calculation
             m = 1. -1./nvan(njsc(ji))
           
             IF ((.NOT. ok_thermodynamical_freezing).OR.(mc(ji,jsl, ins).LT.(mcr(njsc(ji))+min_sechiba))) THEN
                ! Linear soil freezing or soil moisture below residual
                IF (temp_hydro(ji, jsl).GE.(ZeroCelsius+fr_dT/2.)) THEN
                   x=1._r_std
                ELSE IF ( (temp_hydro(ji,jsl) .GE. (ZeroCelsius-fr_dT/2.)) .AND. &
                     (temp_hydro(ji,jsl) .LT. (ZeroCelsius+fr_dT/2.)) ) THEN 
                   x=(temp_hydro(ji, jsl)-(ZeroCelsius-fr_dT/2.))/fr_dT
                ELSE 
                   x=0._r_std
                ENDIF
             ELSE IF (ok_thermodynamical_freezing) THEN
                ! Thermodynamical soil freezing
                IF (temp_hydro(ji, jsl).GE.(ZeroCelsius+fr_dT/2.)) THEN
                   x=1._r_std
                ELSE IF ( (temp_hydro(ji,jsl) .GE. (ZeroCelsius-fr_dT/2.)) .AND. &
                     (temp_hydro(ji,jsl) .LT. (ZeroCelsius+fr_dT/2.)) ) THEN 
                   x=MIN(((mcs(njsc(ji))-mcr(njsc(ji))) &
                        *((1000.*avan(njsc(ji))*(ZeroCelsius+fr_dT/2.-temp_hydro(ji, jsl)) &
                        *lhf/ZeroCelsius/10.)**nvan(njsc(ji))+1.)**(-m)) / &
                        (mc(ji,jsl, ins)-mcr(njsc(ji))),1._r_std)                
                ELSE
                   x=0._r_std
                ENDIF
             ENDIF
             profil_froz_hydro_ns(ji, jsl,ins)=1._r_std-x
             
             ! mc_used is used in the calculation of hydrological properties
             mc_used = mcr(njsc(ji))+x*(mc(ji,jsl, ins)-mcr(njsc(ji))) 
             !
             ! calcul de k based on mc_liq
             !
             i= MAX(imin, MIN(imax-1, INT(imin +(imax-imin)*(mc_used-mcr(njsc(ji)))/(mcs(njsc(ji))-mcr(njsc(ji))))))
             a(ji,jsl) = a_lin(i,jsl,njsc(ji)) * kfact_root(ji,jsl,ins)
             b(ji,jsl) = b_lin(i,jsl,njsc(ji)) * kfact_root(ji,jsl,ins)
             d(ji,jsl) = d_lin(i,jsl,njsc(ji)) * kfact_root(ji,jsl,ins)
             k(ji,jsl) = kfact_root(ji,jsl,ins) * MAX(k_lin(imin+1,jsl,njsc(ji)), &
                  a_lin(i,jsl,njsc(ji)) * mc_used + b_lin(i,jsl,njsc(ji)))
          ENDDO ! loop on grid
       ENDDO
             
    ELSE
       ! .NOT. ok_freeze_cwrr
       DO jsl=1,nslm
          DO ji=1,kjpindex 
             
             mc_ratio = MAX(mc(ji,jsl,ins)-mcr(njsc(ji)), zero)/(mcs(njsc(ji))-mcr(njsc(ji)))
             
             i= MAX(MIN(INT((imax-imin)*mc_ratio)+imin , imax-1), imin)
             a(ji,jsl) = a_lin(i,jsl,njsc(ji)) * kfact_root(ji,jsl,ins) ! in mm/d
             b(ji,jsl) = b_lin(i,jsl,njsc(ji)) * kfact_root(ji,jsl,ins) ! in mm/d
             d(ji,jsl) = d_lin(i,jsl,njsc(ji)) * kfact_root(ji,jsl,ins) ! in mm^2/d
             k(ji,jsl) = kfact_root(ji,jsl,ins) * MAX(k_lin(imin+1,jsl,njsc(ji)), &
                  a_lin(i,jsl,njsc(ji)) * mc(ji,jsl,ins) + b_lin(i,jsl,njsc(ji))) 
          END DO ! loop on grid
       END DO
    ENDIF
    
  END SUBROUTINE hydrol_soil_coef


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_soil_setup
!!
!>\BRIEF        This subroutine computes the matrix coef.  
!!
!! DESCRIPTION  : None 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : matrix coef
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrol_soil_setup(kjpindex,ins)


    IMPLICIT NONE
    !
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                        :: kjpindex          !! Domain size
    INTEGER(i_std), INTENT(in)                        :: ins               !! index of soil type

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std) :: jsl,ji
    REAL(r_std)                        :: temp3, temp4

!_ ================================================================================================================================
    !-we compute tridiag matrix coefficients (LEFT and RIGHT) 
    ! of the system to solve [LEFT]*mc_{t+1}=[RIGHT]*mc{t}+[add terms]: 
    ! e(nslm),f(nslm),g1(nslm) for the [left] vector
    ! and ep(nslm),fp(nslm),gp(nslm) for the [right] vector

    ! w_time=1 (in constantes_soil) indicates implicit computation for diffusion 
    temp3 = w_time*(dt_sechiba/one_day)/deux
    temp4 = (un-w_time)*(dt_sechiba/one_day)/deux

    ! Passage to arithmetic means for layer averages also in this subroutine : Aurelien 11/05/10

    !- coefficient for first layer
    DO ji = 1, kjpindex
       e(ji,1) = zero
       f(ji,1) = trois * dz(2)/huit  + temp3 &
            & * ((d(ji,1)+d(ji,2))/(dz(2))+a(ji,1))
       g1(ji,1) = dz(2)/(huit)       - temp3 &
            & * ((d(ji,1)+d(ji,2))/(dz(2))-a(ji,2))
       ep(ji,1) = zero
       fp(ji,1) = trois * dz(2)/huit - temp4 &
            & * ((d(ji,1)+d(ji,2))/(dz(2))+a(ji,1))
       gp(ji,1) = dz(2)/(huit)       + temp4 &
            & * ((d(ji,1)+d(ji,2))/(dz(2))-a(ji,2))
    ENDDO

    !- coefficient for medium layers

    DO jsl = 2, nslm-1
       DO ji = 1, kjpindex
          e(ji,jsl) = dz(jsl)/(huit)                        - temp3 &
               & * ((d(ji,jsl)+d(ji,jsl-1))/(dz(jsl))+a(ji,jsl-1))

          f(ji,jsl) = trois * (dz(jsl)+dz(jsl+1))/huit  + temp3 &
               & * ((d(ji,jsl)+d(ji,jsl-1))/(dz(jsl)) + &
               & (d(ji,jsl)+d(ji,jsl+1))/(dz(jsl+1)) )

          g1(ji,jsl) = dz(jsl+1)/(huit)                     - temp3 &
               & * ((d(ji,jsl)+d(ji,jsl+1))/(dz(jsl+1))-a(ji,jsl+1))

          ep(ji,jsl) = dz(jsl)/(huit)                       + temp4 &
               & * ((d(ji,jsl)+d(ji,jsl-1))/(dz(jsl))+a(ji,jsl-1))

          fp(ji,jsl) = trois * (dz(jsl)+dz(jsl+1))/huit - temp4 &
               & * ( (d(ji,jsl)+d(ji,jsl-1))/(dz(jsl)) + &
               & (d(ji,jsl)+d(ji,jsl+1))/(dz(jsl+1)) )

          gp(ji,jsl) = dz(jsl+1)/(huit)                     + temp4 &
               & *((d(ji,jsl)+d(ji,jsl+1))/(dz(jsl+1))-a(ji,jsl+1))
       ENDDO
    ENDDO

    !- coefficient for last layer
    DO ji = 1, kjpindex
       e(ji,nslm) = dz(nslm)/(huit)        - temp3 &
            & * ((d(ji,nslm)+d(ji,nslm-1)) /(dz(nslm))+a(ji,nslm-1))
       f(ji,nslm) = trois * dz(nslm)/huit  + temp3 &
            & * ((d(ji,nslm)+d(ji,nslm-1)) / (dz(nslm)) &
            & -a(ji,nslm)*(un-deux*free_drain_coef(ji,ins)))
       g1(ji,nslm) = zero
       ep(ji,nslm) = dz(nslm)/(huit)       + temp4 &
            & * ((d(ji,nslm)+d(ji,nslm-1)) /(dz(nslm))+a(ji,nslm-1))
       fp(ji,nslm) = trois * dz(nslm)/huit - temp4 &
            & * ((d(ji,nslm)+d(ji,nslm-1)) /(dz(nslm)) &
            & -a(ji,nslm)*(un-deux*free_drain_coef(ji,ins)))
       gp(ji,nslm) = zero
    ENDDO

  END SUBROUTINE hydrol_soil_setup

  
!! ================================================================================================================================
!! SUBROUTINE   : hydrol_split_soil
!!
!>\BRIEF        Splits 2d variables into 3d variables, per soiltile (_ns suffix), at the beginning of hydrol
!!
!! DESCRIPTION  :
!! - 1 Split 2d variables into 3d variables, per soiltile
!! - 1.1 Throughfall
!! - 1.2 evaporation
!! - 1.2.1 vevapnu_old
!! - 1.2.2 ae_ns new
!! - 1.3 transpiration
!! - 1.4 root sink
!! - 2 Verification
!! - 2.1 Check of mc
!! - 2.2 Check if the deconvolution is correct and conserves the fluxes
!! - 2.2.1 the precisol and evapnu
!! - 2.2.2 the transpiration and root sink
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_split_soil

  SUBROUTINE hydrol_split_soil (kjpindex, veget_max, soiltile, vevapnu, vevapnu_pft, transpir, humrel, evap_bare_lim, tot_bare_soil)
    ! 
    ! interface description

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                               :: kjpindex
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(in)       :: veget_max        !! max Vegetation map 
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in)      :: soiltile         !! Fraction of each soil tile (0-1, unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)           :: vevapnu          !! Bare soil evaporation
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT (in)      :: vevapnu_pft      !! Bare soil evaporation
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: transpir         !! Transpiration
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: humrel           !! Relative humidity
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: evap_bare_lim    !!   
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: tot_bare_soil    !! Total evaporating bare soil fraction 

    !! 0.4 Local variables

    INTEGER(i_std)                                :: ji, jv, jsl, jst
    REAL(r_std), DIMENSION (kjpindex)             :: vevapnu_old
    REAL(r_std), DIMENSION (kjpindex,nstm)        :: vevapnu_ns      !! Bare soil evaporation
    REAL(r_std), DIMENSION (kjpindex)             :: tmp_check1
    REAL(r_std), DIMENSION (kjpindex)             :: tmp_check2
    REAL(r_std), DIMENSION (kjpindex,nstm)        :: tmp_check3
    LOGICAL                                       :: error=.FALSE. !! If true, exit in the end of subroutine

!_ ================================================================================================================================
    !
    !
    !! 1 Split 2d variables into 3d variables, per soiltile
    !
    ! Reminders:
    !  corr_veg_soil(:,nvm,nstm) = PFT fraction per soiltile in each grid-cell
    !      corr_veg_soil(ji,jv,jst)=veget_max(ji,jv)/soiltile(ji,jst) 
    !  cvs_over_veg(:,nvm,nstm) = old value of corr_veg_soil/veget_max/vegtot, kept from diag to next split 
    !  soiltile(:,nstm) = fraction (of vegtot+totfrac_nobio) covered by each soiltile in a grid-cell (0-[1-fracnobio], unitless)  
    !      soiltile #1 includes nobio 
    !  vegtot(:) = total fraction of grid-cell covered by PFTs (fraction with bare soil + vegetation)
    !  veget_max(:,nvm) = PFT fractions of vegtot+frac_nobio 
    !  veget(:,nvm) =  fractions (of vegtot+frac_nobio) covered by vegetation in each PFT 
    !       BUT veget(:,1)=veget_max(:,1) 
    !  frac_bare(:,nvm) = fraction (of veget_max) with bare soil in each PFT
    !  tot_bare_soil(:) = total evaporating bare soil fraction (=SUM(frac_bare*veget_max))
    !  frac_bare_ns(:,nstm) = evaporating bare soil fraction (of vegtot) per soiltile (defined in hydrol_vegupd)
    ! 
    ! AD *** isn't there a pb in corr_veg_soil as SUM(soiltiles)=totfrac_nobio + vegtot ???
    !
    !! 1.1 Throughfall
    !      We use corr_veg_soil, i.e. the vegetation cover of this timestep, which is normal for interception processes
    precisol_ns(:,:)=zero
    DO jv=1,nvm
       DO jst=1,nstm
          DO ji=1,kjpindex
             IF(veget_max(ji,jv).GT.min_sechiba) THEN
                precisol_ns(ji,jst)=precisol_ns(ji,jst)+precisol(ji,jv)* &
                     !& corr_veg_soil(ji,jv,jst) /vegtot(ji)  ! xuhui 20151217
                     & corr_veg_soil(ji,jv,jst) /vegtot(ji) / veget_max(ji,jv)
             ENDIF
          END DO
       END DO
    END DO
    !
    !
    !! 1.2 Bare soil evaporation

    vevapnu_ns(:,:) = zero
    DO jv = 1,nvm
        DO jst = 1,nstm
            DO ji=1,kjpindex
                IF (veget_max(ji,jv) .GT. min_sechiba) THEN 
                    vevapnu_ns(ji,jst) = vevapnu_ns(ji,jst) + vevapnu_pft(ji,jv)* &
                            & corr_veg_soil(ji,jv,jst) / vegtot(ji) / veget_max(ji,jv)
                ENDIF
            ENDDO
        ENDDO
    ENDDO
    
    !! 1.2.1 vevapnu_old
    vevapnu_old(:)=zero
    DO jst=1,nstm
       DO ji=1,kjpindex
          IF ( vegtot(ji) .GT. min_sechiba) THEN
             vevapnu_old(ji)=vevapnu_old(ji)+ &
                  & ae_ns(ji,jst)*soiltile(ji,jst)*vegtot(ji)
          ENDIF
       END DO
    END DO
    !
    !! 1.2.2 ae_ns new
    DO jst=1,nstm
       DO ji=1,kjpindex
          IF (vevapnu_old(ji).GT.min_sechiba) THEN   
             IF(evap_bare_lim(ji).GT.min_sechiba) THEN       
                ae_ns(ji,jst) = vevapnu(ji) * evap_bare_lim_ns(ji,jst)/evap_bare_lim(ji)
             ELSE
                 !ZunYin definitely vevapnu_old > min_sechiba
                !IF(vevapnu_old(ji).GT.min_sechiba) THEN  
                !   ae_ns(ji,jst)=ae_ns(ji,jst) * vevapnu(ji)/vevapnu_old(ji)
                !ELSE
                !   ae_ns(ji,jst)=zero
                !ENDIF
                ae_ns(ji,jst)=ae_ns(ji,jst) * vevapnu(ji)/vevapnu_old(ji)
             ENDIF
          ELSEIF(frac_bare_ns(ji,jst).GT.min_sechiba) THEN
             IF(evap_bare_lim(ji).GT.min_sechiba) THEN  
                ae_ns(ji,jst) = vevapnu(ji) * evap_bare_lim_ns(ji,jst)/evap_bare_lim(ji)
             ELSE
                IF(tot_bare_soil(ji).GT.min_sechiba) THEN  
                   ae_ns(ji,jst) = vevapnu(ji) * frac_bare_ns(ji,jst)/tot_bare_soil(ji)
                ELSE
                   ae_ns(ji,jst) = zero
                ENDIF
             ENDIF
          ENDIF
!!!!!! we tried here the new column specific soil evaporation
!          ae_ns(ji,jst) = vevapnu_ns(ji,jst)
!!          IF (ae_ns(ji,jst) .LT. zero) THEN
!!              ae_ns(ji,jst) = zero
!!          ENDIF
          precisol_ns(ji,jst)=precisol_ns(ji,jst)+MAX(-ae_ns(ji,jst),zero)
       END DO
    END DO

    !WRITE(numout,*)'Zun bare_lim_ns_final:',evap_bare_lim_ns(1,:)
    !WRITE(numout,*)'Zun bare_lim_final:',evap_bare_lim(1)

    !WRITE(numout,*)'Zun vevapnu:',vevapnu(1)
    WRITE(numout,*)'Zun ae_ns_1:',ae_ns(1,6)
    !WRITE(numout,*)'Zun sum_ae_ns:',SUM(ae_ns(1,:)*frac_bare_ns(1,:))
    !WRITE(numout,*)'Zun sum_ae_ns2:',SUM(ae_ns(1,:)*soiltile(1,:))
    !WRITE(numout,*)'Zun evap_bare_lim_ns:',evap_bare_lim_ns(1,:)
    !WRITE(numout,*)'Zun evap_bare_lim:',evap_bare_lim(1)
    !WRITE(numout,*)'Zun sum_lim_ns:',SUM(evap_bare_lim_ns(1,:)*vegtot(1)*soiltile(1,:)) = evap_bare_lim
    !WRITE(numout,*)'Zun vegtot:',vegtot(1) = 1
    !
    !
    !! 1.3 transpiration
    !ZunYin According to MT4668
    tr_ns(:,:)=zero
    DO jv=1,nvm
       DO jst=1,nstm
          DO ji=1,kjpindex
             IF (humrel(ji,jv).GT.min_sechiba) THEN 
                 tr_ns(ji,jst)=tr_ns(ji,jst)+ corr_veg_soil(ji,jv,jst)*humrelv(ji,jv,jst)* & 
                       & transpir(ji,jv) / (humrel(ji,jv) * soiltile(ji,jst) * vegtot(ji))
             ENDIF
          END DO
       END DO
    END DO
    !DO jv=1,nvm
    !   jst=pref_soil_veg(jv)
    !   DO ji=1,kjpindex
    !      IF ((humrel(ji,jv).GT.min_sechiba) .AND. ((soiltile(ji,jst)*vegtot(ji)) .GT.min_sechiba))THEN 
    !         tr_ns(ji,jst)= tr_ns(ji,jst) &
    !              + transpir(ji,jv) * (humrelv(ji,jv,jst) / humrel(ji,jv)) &
    !              / (soiltile(ji,jst)*vegtot(ji))
    !      ENDIF
    !   END DO
    !END DO

    !
    !
    !! 1.4 root sink
    rootsink(:,:,:)=zero
    !ZunYin according to MT4668
    DO jv=1,nvm
       DO jsl=1,nslm
          DO jst=1,nstm
             DO ji=1,kjpindex
                IF (humrel(ji,jv).GT.min_sechiba) THEN 
                   rootsink(ji,jsl,jst) = rootsink(ji,jsl,jst) &
                        & + cvs_over_veg(ji,jv,jst)* (transpir(ji,jv)*us(ji,jv,jst,jsl))/ &
!                        & + corr_veg_soil(ji,jv,jst)* (transpir(ji,jv)/veget_max(ji,jv)*us(ji,jv,jst,jsl))/ &
                        & humrel(ji,jv)
                END IF
             END DO
          END DO
       END DO
    END DO

    !DO jv=1,nvm
    !   jst=pref_soil_veg(jv)
    !   DO jsl=1,nslm
    !      DO ji=1,kjpindex
    !         IF ((humrel(ji,jv).GT.min_sechiba) .AND. ((soiltile(ji,jst)*vegtot(ji)) .GT. min_sechiba)) THEN 
    !            rootsink(ji,jsl,jst) = rootsink(ji,jsl,jst) &
    !                 & + transpir(ji,jv)*(us(ji,jv,jst,jsl)/humrel(ji,jv)) &
    !                 & / (soiltile(ji,jst) * vegtot(ji))
    !         END IF
    !      END DO
    !   END DO
    !END DO

    !! 2 Verification
    !! 2.1 Check of mc
    IF(check_cwrr) THEN
       DO jsl=1,nslm
          DO jst=1,nstm
             DO ji=1,kjpindex
                IF(mc(ji,jsl,jst).LT.-0.05) THEN
                   WRITE(numout,*) 'CWRR split-----------------------------------------------'
                   WRITE(numout,*) 'ji,jst,jsl',ji,jst,jsl
                   WRITE(numout,*) 'mc',mc(ji,jsl,jst)
                   WRITE(numout,*) 'rootsink,us',rootsink(ji,:,jst),us(ji,:,jst,jsl)
                   WRITE(numout,*) 'corr_veg_soil',corr_veg_soil(ji,:,jst)
                   WRITE(numout,*) 'transpir',transpir(ji,:)
                   WRITE(numout,*) 'veget_max',veget_max(ji,:)
                   WRITE(numout,*) 'cvs_over_veg',cvs_over_veg(ji,:,jst)
                   WRITE(numout,*) 'humrel',humrel(ji,:)
                   WRITE(numout,*) 'humrelv (pour ce jst)',humrelv(ji,:,jst)
                   WRITE(numout,*) 'ae_ns',ae_ns(ji,jst)
                   WRITE(numout,*) 'tr_ns',tr_ns(ji,jst)
                   WRITE(numout,*) 'vevapnuold',vevapnu_old(ji)
                ENDIF
             END DO
          END DO
       END DO
    ENDIF


    !! 2.2 Check if the deconvolution is correct and conserves the fluxes

    IF (check_cwrr) THEN


       tmp_check1(:)=zero
       tmp_check2(:)=zero  

    !! 2.2.1 the precisol and evapnu

       DO jst=1,nstm
          DO ji=1,kjpindex
             tmp_check1(ji)=tmp_check1(ji) + &
                  & (precisol_ns(ji,jst)-MAX(-ae_ns(ji,jst),zero))* &
                  & soiltile(ji,jst)*vegtot(ji)
          END DO
       END DO

       DO jv=1,nvm
          DO ji=1,kjpindex
             tmp_check2(ji)=tmp_check2(ji) + precisol(ji,jv)
          END DO
       END DO


       DO ji=1,kjpindex   

          IF(ABS(tmp_check1(ji)- tmp_check2(ji)).GT.allowed_err) THEN
             WRITE(numout,*) 'PRECISOL SPLIT FALSE:ji=',ji,tmp_check1(ji),tmp_check2(ji)
             WRITE(numout,*) 'err',ABS(tmp_check1(ji)- tmp_check2(ji))
             WRITE(numout,*) 'vegtot',vegtot(ji)

             DO jv=1,nvm
                WRITE(numout,'(a,i2.2,"|",F13.4,"|",F13.4,"|",3(F9.6))') 'jv,veget_max, precisol, corr_veg_soil ',&
                     jv,veget_max(ji,jv),precisol(ji,jv),corr_veg_soil(ji,jv,:)
             END DO

             DO jst=1,nstm
                WRITE(numout,*) 'jst,precisol_ns',jst,precisol_ns(ji,jst)
                WRITE(numout,*) 'soiltile', soiltile(ji,jst)
             END DO

             error=.TRUE.
             CALL ipslerr_p(2, 'hydrol_split_soil', 'We will STOP in the end of this subroutine.',&
                  & 'check_CWRR','PRECISOL SPLIT FALSE')
          ENDIF

       END DO


       tmp_check1(:)=zero

       DO jst=1,nstm
          DO ji=1,kjpindex
             tmp_check1(ji)=tmp_check1(ji) + ae_ns(ji,jst)* &
                  & soiltile(ji,jst)*vegtot(ji)
          END DO
       END DO

       DO ji=1,kjpindex   

          IF(ABS(tmp_check1(ji)- vevapnu(ji)).GT.allowed_err) THEN
             WRITE(numout,*) 'VEVAPNU SPLIT FALSE:ji, Sum(ae_ns), vevapnu =',ji,tmp_check1(ji),vevapnu(ji)
             WRITE(numout,*) 'err',ABS(tmp_check1(ji)- vevapnu(ji))
             WRITE(numout,*) 'ae_ns',ae_ns(ji,:)
             WRITE(numout,*) 'vegtot',vegtot(ji)
             WRITE(numout,*) 'evap_bare_lim, evap_bare_lim_ns',evap_bare_lim(ji), evap_bare_lim_ns(ji,:)
             WRITE(numout,*) 'tot_bare_soil,frac_bare_ns',tot_bare_soil(ji),frac_bare_ns(ji,:)
             WRITE(numout,*) 'vevapnu_old',vevapnu_old(ji)
             DO jst=1,nstm
                WRITE(numout,*) 'jst,ae_ns',jst,ae_ns(ji,jst)
                WRITE(numout,*) 'soiltile', soiltile(ji,jst)
                WRITE(numout,*) 'veget_max/vegtot/soiltile', veget_max(ji,:)/vegtot(ji)/soiltile(ji,jst)
                WRITE(numout,*) "corr_veg_soil",corr_veg_soil(ji,:,jst)
             END DO

             error=.TRUE.
             CALL ipslerr_p(2, 'hydrol_split_soil', 'We will STOP in the end of this subroutine.',&
                  & 'check_CWRR','VEVAPNU SPLIT FALSE')
          ENDIF
       ENDDO

    !! 2.2.2 the transpiration and root sink

       tmp_check1(:)=zero
       tmp_check2(:)=zero  


       DO jst=1,nstm
          DO ji=1,kjpindex
             tmp_check1(ji)=tmp_check1(ji) + tr_ns(ji,jst)* &
                  & soiltile(ji,jst)*vegtot(ji)
          END DO
       END DO

       DO jv=1,nvm
          DO ji=1,kjpindex
             tmp_check2(ji)=tmp_check2(ji) + transpir(ji,jv)
          END DO
       END DO

       DO ji=1,kjpindex   

          IF(ABS(tmp_check1(ji)- tmp_check2(ji)).GT.allowed_err) THEN
             WRITE(numout,*) 'TRANSPIR SPLIT FALSE:ji=',ji,tmp_check1(ji),tmp_check2(ji)
             WRITE(numout,*) 'err',ABS(tmp_check1(ji)- tmp_check2(ji))
             WRITE(numout,*) 'vegtot',vegtot(ji)

             DO jv=1,nvm
                WRITE(numout,*) 'jv,veget_max, transpir',jv,veget_max(ji,jv),transpir(ji,jv)
                DO jst=1,nstm
                   WRITE(numout,*) 'corr_veg_soil:ji,jv,jst',ji,jv,jst,corr_veg_soil(ji,jv,jst)
                END DO
             END DO

             DO jst=1,nstm
                WRITE(numout,*) 'jst,tr_ns',jst,tr_ns(ji,jst)
                WRITE(numout,*) 'soiltile', soiltile(ji,jst)
             END DO

             error=.TRUE.
             CALL ipslerr_p(2, 'hydrol_split_soil', 'We will STOP in the end of this subroutine.',&
                  & 'check_CWRR','TRANSPIR SPLIT FALSE')
          ENDIF

       END DO


       tmp_check3(:,:)=zero

       DO jst=1,nstm
          DO jsl=1,nslm
             DO ji=1,kjpindex
                tmp_check3(ji,jst)=tmp_check3(ji,jst) + rootsink(ji,jsl,jst)
             END DO
          END DO
       ENDDO

       DO jst=1,nstm
          DO ji=1,kjpindex
             IF(ABS(tmp_check3(ji,jst)- tr_ns(ji,jst)).GT.allowed_err) THEN
                WRITE(numout,*) 'ROOTSINK SPLIT FALSE:ji,jst=', ji,jst,&
                     & tmp_check3(ji,jst),tr_ns(ji,jst)
                WRITE(numout,*) 'err',ABS(tmp_check3(ji,jst)- tr_ns(ji,jst))
                WRITE(numout,*) 'HUMREL(jv=1:13)',humrel(ji,:)
                WRITE(numout,*) 'TRANSPIR',transpir(ji,:)
                DO jv=1,nvm 
                   WRITE(numout,*) 'jv=',jv,'us=',us(ji,jv,jst,:)
                ENDDO

                error=.TRUE.
                CALL ipslerr_p(2, 'hydrol_split_soil', 'We will STOP in the end of this subroutine.',&
                  & 'check_CWRR','ROOTSINK SPLIT FALSE')
             ENDIF
          END DO
       END DO

    ENDIF

!! Exit if error was found previously in this subroutine
    IF ( error ) THEN
       WRITE(numout,*) 'One or more errors have been detected in hydrol_split_soil. Model stops.'
       CALL ipslerr_p(3, 'hydrol_split_soil', 'We will STOP now.',&
                  & 'One or several fatal errors were found previously.','')
    END IF

  END SUBROUTINE hydrol_split_soil
  

!! ================================================================================================================================
!! SUBROUTINE   : hydrol_diag_soil
!!
!>\BRIEF        ??
!!
!! DESCRIPTION  :
!! - 1 Apply mask_soiltile
!! - 2 sum 3d variables in 2d variables with fraction of vegetation per soil type
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_diag_soil

  SUBROUTINE hydrol_diag_soil (kjpindex, veget_max, soiltile, njsc, runoff, drainage, &
       & evapot, vevapnu, returnflow, reinfiltration, irrigation, &
       & shumdiag,shumdiag_perma, k_litt, litterhumdiag, humrel, vegstress, drysoil_frac, tot_melt, & !pss:+
       & drunoff_tot) !pss:-

    ! 
    ! interface description

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    ! input scalar 
    INTEGER(i_std), INTENT(in)                               :: kjpindex 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)        :: veget_max       !! Max. vegetation type
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: njsc            !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std), DIMENSION (kjpindex,nstm), INTENT (in)      :: soiltile        !! Fraction of each soil tile (0-1, unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: evapot          !! 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: returnflow      !! Water returning to the deep reservoir
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: reinfiltration  !! Water returning to the top of the soil
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: irrigation      !! Water from irrigation
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: tot_melt        !!

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex), INTENT (out)          :: drysoil_frac    !! Function of litter wetness
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: runoff          !! complete runoff
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: drainage        !! Drainage
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out)      :: shumdiag        !! relative soil moisture
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out)      :: shumdiag_perma  !! Percent of porosity filled with water (mc/mcs) used for the thermal computations
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: k_litt          !! litter cond.
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: litterhumdiag   !! litter humidity
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)       :: humrel          !! Relative humidity
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT(out)      :: vegstress       !! Veg. moisture stress (only for vegetation growth)

!pss:+
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)           :: drunoff_tot          !! Dunne runoff
!pss:-

    !! 0.3 Modified variables 

    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapnu         !!

    !! 0.4 Local variables

    INTEGER(i_std)                                           :: ji, jv, jsl, jst, i, jd
    REAL(r_std), DIMENSION (kjpindex)                        :: mask_vegtot
    REAL(r_std)                                              :: k_tmp, tmc_litter_ratio

!_ ================================================================================================================================
    !
    ! Put the prognostics variables of soil to zero if soiltype is zero

    !! 1 Apply mask_soiltile
    DO jst=1,nstm 
       IF (ok_freeze_cwrr) THEN
           CALL hydrol_soil_coef(kjpindex,jst,njsc)
       ENDIF
       DO ji=1,kjpindex

!!??Aurelien: who and why coment this line
!          IF(soiltile(ji,jst).EQ.zero) THEN

             ae_ns(ji,jst) = ae_ns(ji,jst) * mask_soiltile(ji,jst)
             dr_ns(ji,jst) = dr_ns(ji,jst) * mask_soiltile(ji,jst)
             ru_ns(ji,jst) = ru_ns(ji,jst) * mask_soiltile(ji,jst)
             tmc(ji,jst) =  tmc(ji,jst) * mask_soiltile(ji,jst)
             IF (ok_freeze_cwrr) THEN
                profil_froz_hydro_ns(ji,:,jst)=profil_froz_hydro_ns(ji,:,jst)*mask_soiltile(ji,jst)
             ENDIF !if (ok_freeze_cwrr) then

             DO jv=1,nvm
                humrelv(ji,jv,jst) = humrelv(ji,jv,jst) * mask_soiltile(ji,jst)
                DO jsl=1,nslm
                   us(ji,jv,jst,jsl) = us(ji,jv,jst,jsl)  * mask_soiltile(ji,jst)
                END DO
             END DO

             DO jsl=1,nslm          
                mc(ji,jsl,jst) = mc(ji,jsl,jst)  * mask_soiltile(ji,jst)
             END DO

!          ENDIF

       END DO
    END DO

    runoff(:) = zero
    drainage(:) = zero
    humtot(:) = zero
    shumdiag(:,:)= zero
    shumdiag_perma(:,:)=zero
    k_litt(:) = zero
    litterhumdiag(:) = zero
    tmc_litt_dry_mea(:) = zero
    tmc_litt_wet_mea(:) = zero
    tmc_litt_mea(:) = zero
    humrel(:,:) = zero
    vegstress(:,:) = zero
    swi(:) = zero
    IF (ok_freeze_cwrr) THEN
       profil_froz_hydro(:,:)=zero
    ENDIF
    
    !! 2 sum 3d variables in 2d variables with fraction of vegetation per soil type

    DO ji = 1, kjpindex
       mask_vegtot(ji) = 0
       IF(vegtot(ji) .GT. min_sechiba) THEN
          mask_vegtot(ji) = 1
       ENDIF
    END DO
    
    DO ji = 1, kjpindex 
       ! Here we weight ae_ns by the fraction of bare evaporating soil. 
       ! This is given by frac_bare_ns, taking into account bare soil under vegetation
        ae_ns(ji,:) = mask_vegtot(ji) * ae_ns(ji,:) * frac_bare_ns(ji,:)
       !ZunYin: should multiplied by soiltile to make evapnu = sum(evapnu_soil)
       !ae_ns(ji,:) = mask_vegtot(ji) * ae_ns(ji,:) * soiltile(ji,:)
       !ZunYin: not correct, need to keep evaporation amount/soiltile for later
       !calculation
       !ae_ns(ji,:) = mask_vegtot(ji) * ae_ns(ji,:)
    END DO

    DO jst = 1, nstm
       DO ji = 1, kjpindex 
          drainage(ji) = mask_vegtot(ji) * (drainage(ji) + vegtot(ji)*soiltile(ji,jst) * dr_ns(ji,jst))
          runoff(ji) = mask_vegtot(ji) *  (runoff(ji) +   vegtot(ji)*soiltile(ji,jst) * ru_ns(ji,jst)) &
               & + (1 - mask_vegtot(ji)) * (tot_melt(ji) + irrigation(ji) + returnflow(ji) + reinfiltration(ji))
          humtot(ji) = mask_vegtot(ji) * (humtot(ji) + soiltile(ji,jst) * tmc(ji,jst)) 
          IF (ok_freeze_cwrr) THEN 
             profil_froz_hydro(ji,:)=mask_vegtot(ji) * &
                  (profil_froz_hydro(ji,:) + soiltile(ji,jst) * profil_froz_hydro_ns(ji,:, jst))
          ENDIF
       END DO
    END DO

    ! we add the excess of snow sublimation to vevapnu

    DO ji = 1,kjpindex
       vevapnu(ji) = vevapnu (ji) + subsinksoil(ji)*vegtot(ji)
    END DO

! AD *** en quoi est-ce different de vegstress(ji,jv)=vegstress(ji,jv)+vegstressv(ji,jv,jst)*vegtot(ji) ???
!        à quoi sert vegtot ici ? 
    DO jst=1,nstm
       DO jv=1,nvm
          DO ji=1,kjpindex
             IF(veget_max(ji,jv).GT.min_sechiba) THEN
                vegstress(ji,jv)=vegstress(ji,jv)+vegstressv(ji,jv,jst)*soiltile(ji,jst) &
                     !& * corr_veg_soil(ji,jv,jst) *vegtot(ji)/veget_max(ji,jv)
                     !ZunYin
                     & * corr_veg_soil(ji,jv,jst)/veget_max(ji,jv)
                !vegstress(ji,jv)=vegstress(ji,jv)+vegstressv(ji,jv,jst)
                vegstress(ji,jv)= MAX(vegstress(ji,jv),zero)
             ENDIF
          END DO
       END DO
    END DO

    cvs_over_veg(:,:,:) = zero
    DO jv=1,nvm
       DO ji=1,kjpindex
          IF(veget_max(ji,jv).GT.min_sechiba) THEN
             DO jst=1,nstm
                cvs_over_veg(ji,jv,jst) = corr_veg_soil(ji,jv,jst)/vegtot(ji) / veget_max(ji,jv)
             ENDDO
          ENDIF
       END DO
    END DO

! AD *** en quoi est-ce different de humrel(ji,jv)=humrel(ji,jv)+humrelv(ji,jv,jst) ???
!        pourquoi procède-t-on différemment de vegstress ?
!        y a-t-il vraiment une difference de pas de temps entre corr_veg_soil et cvs_over_veg ?
!        non dans cette routine (step 14 of hydrol)
!        oui si cvs_over_veg est utilisé avant l'appel à cette routine
          !ZunYin according to the MT4668
    DO jst=1,nstm
       DO jv=1,nvm
          DO ji=1,kjpindex
             humrel(ji,jv)=humrel(ji,jv)+humrelv(ji,jv,jst)*soiltile(ji,jst) &
                  & * cvs_over_veg(ji,jv,jst)*vegtot(ji)
             humrel(ji,jv)=MAX(humrel(ji,jv),zero)
          END DO
       END DO
    END DO

    !DO jv=1,nvm
    !   DO ji=1,kjpindex
    !      humrel(ji,jv)=humrel(ji,jv)+humrelv(ji,jv,pref_soil_veg(jv))
    !   END DO
    !END DO


    DO jst=1,nstm
       DO ji=1,kjpindex
          ! We compute here a mean k for the 'litter' used for reinfiltration from floodplains of ponds
          !
          IF ( tmc_litter(ji,jst) < tmc_litter_res(ji,jst)) THEN
             i = imin
          ELSE
             tmc_litter_ratio = (tmc_litter(ji,jst)-tmc_litter_res(ji,jst)) / &
                  & (tmc_litter_sat(ji,jst)-tmc_litter_res(ji,jst))
             i= MAX(MIN(INT((imax-imin)*tmc_litter_ratio)+imin, imax-1), imin)
          ENDIF
          !
          !
          k_tmp = MAX(k_lin(i,1,njsc(ji))*ks(njsc(ji)), zero)
          k_litt(ji) = k_litt(ji) + soiltile(ji,jst) * SQRT(k_tmp)
       ENDDO
    ENDDO

    DO jst=1,nstm        

       DO ji=1,kjpindex
          litterhumdiag(ji) = litterhumdiag(ji) + &
               & soil_wet_litter(ji,jst) * soiltile(ji,jst)

          tmc_litt_wet_mea(ji) =  tmc_litt_wet_mea(ji) + & 
               & tmc_litter_awet(ji,jst)* soiltile(ji,jst)

          tmc_litt_dry_mea(ji) = tmc_litt_dry_mea(ji) + &
               & tmc_litter_adry(ji,jst) * soiltile(ji,jst) 

          tmc_litt_mea(ji) = tmc_litt_mea(ji) + &
               & tmc_litter(ji,jst) * soiltile(ji,jst) 
      END DO

       DO jd=1,nbdl
          DO ji=1,kjpindex
               DO jsl=1,nslm  
                  shumdiag(ji,jd)= shumdiag(ji,jd) + soil_wet(ji,jsl,jst)  &
                       *frac_hydro_diag(jsl,jd)* &
                       ((mcs(njsc(ji))-mcw(njsc(ji)))/(mcf(njsc(ji))-mcw(njsc(ji)))) * &
                       soiltile(ji,jst)
                  
                  shumdiag_perma(ji,jd)= shumdiag_perma(ji,jd)  &
                       + mc(ji,jsl,jst) *frac_hydro_diag(jsl,jd) &
                       /mcs(njsc(ji))*soiltile(ji,jst)
               ENDDO
               shumdiag(ji,jd) = MAX(MIN(shumdiag(ji,jd), un), zero) 
               shumdiag_perma(ji,jd) = MAX(MIN(shumdiag_perma(ji,jd), un), zero) 
          END DO
       END DO

    END DO

    ! Calculate soilmoist
    soilmoist(:,:) = zero
    DO jst=1,nstm
       DO ji=1,kjpindex
             soilmoist(ji,1) = soilmoist(ji,1) + soiltile(ji,jst) * &
                  dz(2) * ( trois*mc(ji,1,jst) + mc(ji,2,jst) )/huit
             DO jsl = 2,nslm-1
                soilmoist(ji,jsl) = soilmoist(ji,jsl) + soiltile(ji,jst) * &
                     ( dz(jsl) * (trois*mc(ji,jsl,jst)+mc(ji,jsl-1,jst))/huit &
                     + dz(jsl+1) * (trois*mc(ji,jsl,jst)+mc(ji,jsl+1,jst))/huit )
             END DO
             soilmoist(ji,nslm) = soilmoist(ji,nslm) + soiltile(ji,jst) * &
                  dz(nslm) * (trois*mc(ji,nslm,jst) + mc(ji,nslm-1,jst))/huit
       END DO
    END DO

    DO ji=1,kjpindex
       swi(ji) = swi(ji) + shumdiag(ji,1) * (dz(2))/(deux*zmaxh*mille)
       
       DO jsl=2,nbdl-1 
          swi(ji) = swi(ji) + shumdiag(ji,jsl) * (dz(jsl)+dz(jsl+1))/(deux*zmaxh*mille)
       ENDDO
       swi(ji) = swi(ji) + shumdiag(ji,nbdl) * (dz(nbdl))/(deux*zmaxh*mille)
    END DO

    DO ji=1,kjpindex
       IF ( tmc_litt_wet_mea(ji) - tmc_litt_dry_mea(ji) > zero ) THEN
          drysoil_frac(ji) = un + MAX( MIN( (tmc_litt_dry_mea(ji) - tmc_litt_mea(ji)) / &
               & (tmc_litt_wet_mea(ji) - tmc_litt_dry_mea(ji)), zero), - un)
       ELSE
          drysoil_frac(ji) = zero
       ENDIF
    END DO

  END SUBROUTINE hydrol_diag_soil  


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_waterbal_init
!!
!>\BRIEF        Initialize variables needed for hydrol_waterbal
!!
!! DESCRIPTION  : Initialize variables needed for hydrol_waterbal
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  SUBROUTINE hydrol_waterbal_init(kjpindex, qsintveg, snow, snow_nobio)
    
    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std), INTENT (in)                          :: kjpindex     !! Domain size
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)    :: qsintveg     !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: snow         !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio   !! Ice water balance
    
    !! 0.2 Local variables
    INTEGER(i_std) :: ji
    REAL(r_std) :: watveg

!_ ================================================================================================================================
    !
    !
    !
    IF ( ALL( tot_water_beg(:) == val_exp ) ) THEN
       ! tot_water_beg was not found in restart file
       DO ji = 1, kjpindex
          watveg = SUM(qsintveg(ji,:))
          tot_water_beg(ji) = humtot(ji)*vegtot(ji) + watveg + snow(ji)&
               & + SUM(snow_nobio(ji,:))
       ENDDO
       tot_water_end(:) = tot_water_beg(:)
       tot_flux(:) = zero
    ELSE 
       tot_water_end(:) = tot_water_beg(:)
       tot_flux(:) = zero
    ENDIF

  END SUBROUTINE hydrol_waterbal_init
!! ================================================================================================================================
!! SUBROUTINE   : hydrol_waterbal 
!!
!>\BRIEF        Checks the water balance.
!!
!! DESCRIPTION  :
!! This routine checks the water balance. First it gets the total
!! amount of water and then it compares the increments with the fluxes.
!! The computation is only done over the soil area as over glaciers (and lakes?)
!! we do not have water conservation.
!! This verification does not make much sense in REAL*4 as the precision is the same as some
!! of the fluxes
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_waterbal

  SUBROUTINE hydrol_waterbal (kjpindex, index, first_call, veget_max, totfrac_nobio, &
       & qsintveg, snow,snow_nobio, precip_rain, precip_snow, returnflow, reinfiltration, irrigation, tot_melt, &
       & vevapwet, transpir, vevapnu, vevapsno, vevapflo, floodout, runoff, drainage)
    !
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                        :: kjpindex     !! Domain size
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: index        !! Indeces of the points on the map
    LOGICAL, INTENT (in)                               :: first_call   !! At which time is this routine called ?
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max    !! Max Fraction of vegetation type 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: totfrac_nobio!! Total fraction of continental ice+lakes+...
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: qsintveg     !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: snow         !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio !!Ice water balance
    !
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: precip_rain  !! Rain precipitation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: precip_snow  !! Snow precipitation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: returnflow   !! Water to the bottom
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: reinfiltration !! Water to the top
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: irrigation   !! Water from irrigation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: tot_melt     !! Total melt
    !
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: vevapwet     !! Interception loss
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: transpir     !! Transpiration
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: vevapnu      !! Bare soil evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: vevapsno     !! Snow evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: vevapflo     !! Floodplains evaporation
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: floodout     !! flow out of floodplains
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: runoff       !! complete runoff
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: drainage     !! Drainage

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std) :: ji
    REAL(r_std) :: watveg, delta_water
    LOGICAL     :: error=.FALSE.  !! If true, exit in the end of subroutine

!_ ================================================================================================================================

    tot_water_end(:) = zero
    tot_flux(:) = zero
    !
    DO ji = 1, kjpindex
       !
       ! If the fraction of ice, lakes, etc. does not complement the vegetation fraction then we do not
       ! need to go any further
       !
       IF ( ABS(un - (totfrac_nobio(ji) + vegtot(ji))) .GT. allowed_err ) THEN
          WRITE(numout,*) 'HYDROL problem in vegetation or frac_nobio on point ', ji
          WRITE(numout,*) 'totfrac_nobio : ', totfrac_nobio(ji)
          WRITE(numout,*) 'vegetation fraction : ', vegtot(ji)

          error=.TRUE.
          CALL ipslerr_p(2, 'hydrol_waterbal', 'We will STOP in the end of hydrol_waterbal.','','')
       ENDIF
    ENDDO

    DO ji = 1, kjpindex
       !
       watveg = SUM(qsintveg(ji,:))
       tot_water_end(ji) = humtot(ji)*vegtot(ji) + watveg + &
            & snow(ji) + SUM(snow_nobio(ji,:))
       !
       tot_flux(ji) =  precip_rain(ji) + precip_snow(ji) + irrigation (ji) - &
            & SUM(vevapwet(ji,:)) - SUM(transpir(ji,:)) - vevapnu(ji) - vevapsno(ji) - vevapflo(ji) + &
            & floodout(ji) - runoff(ji) - drainage(ji) + returnflow(ji) + reinfiltration(ji)
    ENDDO
    
    DO ji = 1, kjpindex
       !
       delta_water = tot_water_end(ji) - tot_water_beg(ji)
       !
       !
       !  Set some precision ! This is a wild guess and corresponds to what works on an IEEE machine
       !  under double precision (REAL*8).
       !
       !
       IF ( ABS(delta_water-tot_flux(ji)) .GT. deux*allowed_err ) THEN
          WRITE(numout,*) '------------------------------------------------------------------------- '
          WRITE(numout,*) 'HYDROL does not conserve water. The erroneous point is : ', ji
          WRITE(numout,*) 'Coord erroneous point', lalo(ji,:)
          WRITE(numout,*) 'The error in mm/s is :', (delta_water-tot_flux(ji))/dt_sechiba, ' and in mm/dt : ', &
               & delta_water-tot_flux(ji)
          WRITE(numout,*) 'delta_water : ', delta_water, ' tot_flux : ', tot_flux(ji)
          WRITE(numout,*) 'Actual and allowed error : ', ABS(delta_water-tot_flux(ji)), allowed_err
          WRITE(numout,*) 'vegtot : ', vegtot(ji)
          WRITE(numout,*) 'precip_rain : ', precip_rain(ji)
          WRITE(numout,*) 'precip_snow : ',  precip_snow(ji)
          WRITE(numout,*) 'Water from routing. Reinfiltration/returnflow/irrigation : ', reinfiltration(ji), &
               & returnflow(ji),irrigation(ji)
          WRITE(numout,*) 'Total water in soil humtot:',  humtot(ji)
          WRITE(numout,*) 'mc:' , mc(ji,:,:)
          WRITE(numout,*) 'Water on vegetation watveg:', watveg
          WRITE(numout,*) 'Snow mass snow:', snow(ji)
          WRITE(numout,*) 'Snow mass on ice snow_nobio:', SUM(snow_nobio(ji,:))
          WRITE(numout,*) 'Melt water tot_melt:', tot_melt(ji)
          WRITE(numout,*) 'evapwet : ', vevapwet(ji,:)
          WRITE(numout,*) 'transpir : ', transpir(ji,:)
          WRITE(numout,*) 'evapnu, evapsno, evapflo: ', vevapnu(ji), vevapsno(ji), vevapflo(ji)
          WRITE(numout,*) 'drainage,runoff,floodout : ', drainage(ji),runoff(ji),floodout(ji)
         ! error=.TRUE.
         ! CALL ipslerr_p(2, 'hydrol_waterbal', 'We will STOP in the end of hydrol_waterbal.','','')
       ENDIF
       !
    ENDDO
    !
    ! Transfer the total water amount at the end of the current timestep top the begining of the next one.
    !
    tot_water_beg = tot_water_end
    !
    
    ! Exit if one or more errors were found
    IF ( error ) THEN
       WRITE(numout,*) 'One or more errors have been detected in hydrol_waterbal. Model stops.'
       CALL ipslerr_p(3, 'hydrol_waterbal', 'We will STOP now.',&
            'One or several fatal errors were found previously.','')
    END IF
    
  END SUBROUTINE hydrol_waterbal


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_alma 
!!
!>\BRIEF        This routine computes the changes in soil moisture and interception storage for the ALMA outputs.  
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
!_ hydrol_alma

  SUBROUTINE hydrol_alma (kjpindex, index, first_call, qsintveg, snow, snow_nobio, soilwet)
    !
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                        :: kjpindex     !! Domain size
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)   :: index        !! Indeces of the points on the map
    LOGICAL, INTENT (in)                               :: first_call   !! At which time is this routine called ?
    !
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: qsintveg     !! Water on vegetation due to interception
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: snow         !! Snow water equivalent
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT (in) :: snow_nobio     !! Water balance on ice, lakes, .. [Kg/m^2]

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: soilwet     !! Soil wetness

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std) :: ji
    REAL(r_std) :: watveg

!_ ================================================================================================================================
    !
    !
    IF ( first_call ) THEN
       ! Initialize variables if they were not found in the restart file

       DO ji = 1, kjpindex
          watveg = SUM(qsintveg(ji,:))
          tot_watveg_beg(ji) = watveg
          tot_watsoil_beg(ji) = humtot(ji)
          snow_beg(ji)        = snow(ji)+ SUM(snow_nobio(ji,:))
       ENDDO

       RETURN

    ENDIF
    !
    ! Calculate the values for the end of the time step
    !
    DO ji = 1, kjpindex
       watveg = SUM(qsintveg(ji,:))
       tot_watveg_end(ji) = watveg
       tot_watsoil_end(ji) = humtot(ji)
       snow_end(ji) = snow(ji)+ SUM(snow_nobio(ji,:))

       delintercept(ji) = tot_watveg_end(ji) - tot_watveg_beg(ji)
       delsoilmoist(ji) = tot_watsoil_end(ji) - tot_watsoil_beg(ji)
       delswe(ji)       = snow_end(ji) - snow_beg(ji)
    ENDDO
    !
    !
    ! Transfer the total water amount at the end of the current timestep top the begining of the next one.
    !
    tot_watveg_beg = tot_watveg_end
    tot_watsoil_beg = tot_watsoil_end
    snow_beg(:) = snow_end(:)
    !
    DO ji = 1,kjpindex
       IF ( mx_eau_var(ji) > 0 ) THEN
          soilwet(ji) = tot_watsoil_end(ji) / mx_eau_var(ji)
       ELSE
          soilwet(ji) = zero
       ENDIF
    ENDDO
    !
  END SUBROUTINE hydrol_alma
  !


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_calculate_temp_hydro
!!
!>\BRIEF         Calculate the temperature at hydrological levels  
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================


  SUBROUTINE hydrol_calculate_temp_hydro(kjpindex, stempdiag, snow,snowdz)

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                             :: kjpindex 
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (in)     :: stempdiag
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)          :: snow
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (in)    :: snowdz


    !! 0.2 Local variables
    
    INTEGER jh, jd, ji
    REAL(r_std) :: snow_h
    REAL(r_std)  :: lev_diag, prev_diag, lev_prog, prev_prog
    REAL(r_std), DIMENSION(nslm,nbdl) :: intfactt
    
    
    DO ji=1,kjpindex
       IF (ok_explicitsnow) THEN 
          !The snow pack is above the surface soil in the new snow model.
          snow_h=0
       ELSE  
          snow_h=snow(ji)/sn_dens
       ENDIF
       
       intfactt(:,:)=0.
       prev_diag = snow_h
       DO jh = 1, nslm
          IF (jh.EQ.1) THEN
             lev_diag = zz(2)/1000./2.+snow_h
          ELSEIF (jh.EQ.nslm) THEN
             lev_diag = zz(nslm)/1000.+snow_h
             
          ELSE
             lev_diag = zz(jh)/1000. &
                  & +(zz(jh+1)-zz(jh))/1000./2.+snow_h
             
          ENDIF
          prev_prog = 0.0
          DO jd = 1, nbdl
             lev_prog = diaglev(jd)
             IF ((lev_diag.GT.diaglev(nbdl).AND. &
                  & prev_diag.LT.diaglev(nbdl)-min_sechiba)) THEN
                lev_diag=diaglev(nbdl)          
             ENDIF
             intfactt(jh,jd) = MAX(MIN(lev_diag,lev_prog)-MAX(prev_diag, prev_prog),&
                  & 0.0)/(lev_diag-prev_diag)
             prev_prog = lev_prog
          ENDDO
          IF (lev_diag.GT.diaglev(nbdl).AND. &
               & prev_diag.GE.diaglev(nbdl)-min_sechiba) intfactt(jh,nbdl)=1.
          prev_diag = lev_diag
       ENDDO
    ENDDO
    
    temp_hydro(:,:)=0.
    DO jd= 1, nbdl
       DO jh= 1, nslm
          DO ji = 1, kjpindex
             temp_hydro(ji,jh) = temp_hydro(ji,jh) + stempdiag(ji,jd)*intfactt(jh,jd)
          ENDDO
       ENDDO
    ENDDO
    
  END SUBROUTINE hydrol_calculate_temp_hydro


!! ================================================================================================================================
!! SUBROUTINE   : hydrol_calculate_frac_hydro_diag
!!
!>\BRIEF         Caluculate frac_hydro_diag for interpolation between hydrological and diagnostic axes
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE hydrol_calculate_frac_hydro_diag

    !! 0.1 Local variables

    INTEGER(i_std) :: jd, jh
    REAL(r_std)    :: prev_hydro, next_hydro, prev_diag, next_diag
    

    frac_hydro_diag(:,:)=0.
    prev_diag = 0.0
    
    DO jd = 1, nbdl 
       
       next_diag = diaglev(jd)
       prev_hydro = 0.0
       DO jh = 1, nslm
          IF (jh.EQ.1) THEN
             next_hydro = zz(2)/1000./2.
          ELSEIF (jh.EQ.nslm) THEN
             next_hydro = zz(nslm)/1000.
          ELSE
             next_hydro = zz(jh)/1000.+(zz(jh+1)-zz(jh))/1000./2.
          ENDIF
          frac_hydro_diag(jh,jd) = MAX(MIN(next_hydro, next_diag)-MAX(prev_hydro, prev_diag), 0.)/(next_diag - prev_diag)
          prev_hydro=next_hydro
       ENDDO
       
       prev_diag = next_diag
    ENDDO

  END SUBROUTINE hydrol_calculate_frac_hydro_diag
  
END MODULE hydrol
