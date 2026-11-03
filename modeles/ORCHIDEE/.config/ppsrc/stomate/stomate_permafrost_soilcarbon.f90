










! =================================================================================================================================
! MODULE       : stomate_permafrost_soilcarbon
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see
! ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Calculate permafrost soil carbon dynamics following POPCRAN by Dmitry Khvorstyanov
!!      
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! SVN          :
!! $HeadURL:
!svn://forge.ipsl.jussieu.fr/orchidee/branches/ORCHIDEE-MICT/ORCHIDEE/src_stomate/stomate_soilcarbon.f90
!$ 
!! $Date: 2013-10-14 15:38:24 +0200 (Mon, 14 Oct 2013) $
!! $Revision: 1536 $
!! \n
!_
!================================================================================================================================

MODULE stomate_permafrost_soilcarbon
  
  ! modules used:
  USE ioipsl_para  
  USE constantes_soil_var
  USE constantes_soil
  USE constantes_var
  USE pft_parameters
  USE stomate_data
  USE grid
  USE mod_orchidee_para
  USE xios_orchidee

  IMPLICIT NONE
  PRIVATE
  PUBLIC deep_carbcycle,permafrost_carbon_clear, microactem
  
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)         :: zf_soil        !! depths of full levels (m)
  !$OMP THREADPRIVATE(zf_soil)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)         :: zi_soil        !! depths of intermediate levels (m)
  !$OMP THREADPRIVATE(zi_soil)
  REAL(r_std), SAVE                                    :: mu_soil
  !$OMP THREADPRIVATE(mu_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)     :: alphaO2_soil
  !$OMP THREADPRIVATE(alphaO2_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)     :: betaO2_soil
  !$OMP THREADPRIVATE(betaO2_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)     :: alphaCH4_soil
  !$OMP THREADPRIVATE(alphaCH4_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)     :: betaCH4_soil
  !$OMP THREADPRIVATE(betaCH4_soil)
  
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:) 	:: heights_snow      !! total thickness of snow levels (m)
  !$OMP THREADPRIVATE(heights_snow)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:,:)     	:: zf_snow           !! depths of full levels (m)
  !$OMP THREADPRIVATE(zf_snow)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:,:)     	:: zi_snow           !! depths of intermediate levels (m)
  !$OMP THREADPRIVATE(zi_snow)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)     	:: zf_snow_nopftdim  !! depths of full levels (m)
  !$OMP THREADPRIVATE(zf_snow_nopftdim)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)     	:: zi_snow_nopftdim  !! depths of intermediate levels (m)
  !$OMP THREADPRIVATE(zi_snow_nopftdim)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: zf_coeff_snow
  !$OMP THREADPRIVATE(zf_coeff_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: zi_coeff_snow
  !$OMP THREADPRIVATE(zi_coeff_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)    :: mu_snow
  !$OMP THREADPRIVATE(mu_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: alphaO2_snow
  !$OMP THREADPRIVATE(alphaO2_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: betaO2_snow
  !$OMP THREADPRIVATE(betaO2_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: alphaCH4_snow
  !$OMP THREADPRIVATE(alphaCH4_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)  :: betaCH4_snow
  !$OMP THREADPRIVATE(betaCH4_snow)

  real(r_std), allocatable, save, dimension(:,:,:)  :: deepC_pftmean        !! Deep soil carbon profiles, mean over all PFTs
  !$OMP THREADPRIVATE(deepC_pftmean)
  
  INTEGER(i_std), SAVE                              :: yr_len = 360
  !$OMP THREADPRIVATE(yr_len)
  !! Arrays related to cryoturbation processes
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: diff_k        !! Diffusion constant (m^2/s)
  !$OMP THREADPRIVATE(diff_k)
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE    :: xe_a
  !$OMP THREADPRIVATE(xe_a)
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE    :: xe_s
  !$OMP THREADPRIVATE(xe_s)
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE    :: xe_p 
  !$OMP THREADPRIVATE(xe_p)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: xc_cryoturb
  !$OMP THREADPRIVATE(xc_cryoturb)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: xd_cryoturb
  !$OMP THREADPRIVATE(xd_cryoturb)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: alpha_a
  !$OMP THREADPRIVATE(alpha_a)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: alpha_s
  !$OMP THREADPRIVATE(alpha_s)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: alpha_p
  !$OMP THREADPRIVATE(alpha_p)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: beta_a
  !$OMP THREADPRIVATE(beta_a)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: beta_s
  !$OMP THREADPRIVATE(beta_s)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE  :: beta_p
  !$OMP THREADPRIVATE(beta_p)
  LOGICAL, DIMENSION(:,:), ALLOCATABLE, SAVE        :: cryoturb_location
  !$OMP THREADPRIVATE(cryoturb_location)
  LOGICAL, DIMENSION(:,:), ALLOCATABLE, SAVE        :: bioturb_location
  !$OMP THREADPRIVATE(bioturb_location)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: airvol_soil
  !$OMP THREADPRIVATE(airvol_soil)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: totporO2_soil              !! total oxygen porosity in the soil
  !$OMP THREADPRIVATE(totporO2_soil)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: totporCH4_soil             !! total methane porosity in the soil
  !$OMP THREADPRIVATE(totporCH4_soil)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: conduct_soil
  !$OMP THREADPRIVATE(conduct_soil)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: diffO2_soil                !! oxygen diffusivity in the soil (m**2/s)
  !$OMP THREADPRIVATE(diffO2_soil)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: diffCH4_soil               !! methane diffusivity in the soil (m**2/s)
  !$OMP THREADPRIVATE(diffCH4_soil)
  REAL(r_std), DIMENSION(:,:,:),ALLOCATABLE, SAVE       :: airvol_snow
  !$OMP THREADPRIVATE(airvol_snow)
  REAL(r_std), DIMENSION(:,:,:),ALLOCATABLE, SAVE       :: totporO2_snow              !! total oxygen porosity in the snow 
  !$OMP THREADPRIVATE(totporO2_snow)
  REAL(r_std), DIMENSION(:,:,:),ALLOCATABLE, SAVE       :: totporCH4_snow             !! total methane porosity in the snow
  !$OMP THREADPRIVATE(totporCH4_snow)
  REAL(r_std), DIMENSION(:,:,:),ALLOCATABLE, SAVE       :: conduct_snow
  !$OMP THREADPRIVATE(conduct_snow)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: diffCH4_snow               !! methane diffusivity in the snow (m**2/s)
  !$OMP THREADPRIVATE(diffCH4_snow)
  REAL(r_std), DIMENSION(:,:,:), ALLOCATABLE, SAVE      :: diffO2_snow                !! oxygen diffusivity in the snow (m**2/s)
  !$OMP THREADPRIVATE(diffO2_snow)
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE        :: altmax_lastyear            !! active layer thickness  
  !$OMP THREADPRIVATE(altmax_lastyear)
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE        :: alt
  !$OMP THREADPRIVATE(alt)
  INTEGER(i_std), DIMENSION(:,:), ALLOCATABLE, SAVE     :: alt_ind !! active layer thickness  
  !$OMP THREADPRIVATE(alt_ind)
  INTEGER(i_std), DIMENSION(:,:),ALLOCATABLE, SAVE      :: altmax_ind !! Maximum over the year active layer thickness
  !$OMP THREADPRIVATE(altmax_ind)
  INTEGER(i_std), DIMENSION(:,:),ALLOCATABLE, SAVE      :: altmax_ind_lastyear
  !$OMP THREADPRIVATE(altmax_ind_lastyear)
  REAL(r_std), DIMENSION(:,:),ALLOCATABLE, SAVE         :: z_root !! Rooting depth
  !$OMP THREADPRIVATE(z_root)
  INTEGER(i_std), DIMENSION(:,:),ALLOCATABLE, SAVE      :: rootlev !! The deepest model level within the rooting depth
  !$OMP THREADPRIVATE(rootlev)
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE, SAVE          :: lalo_global        !! Geogr. coordinates (latitude,longitude) (degrees)
  !$OMP THREADPRIVATE(lalo_global)
  LOGICAL,DIMENSION(:,:),ALLOCATABLE,  SAVE             :: veget_mask_2d      !! whether there is vegetation 
  !$OMP THREADPRIVATE(veget_mask_2d)
  REAL(r_std), PARAMETER                        :: fslow = 37 !16.66667! 36.7785   !  37. Dmitry original   ! facteurs de vitesse pour reservoirs slow et passif
  REAL(r_std), PARAMETER                        :: fpassive = 1617.45 !2372 represents 2000 years for passive at reference of 5 degrees!1617.45 !666.667 !1617.45 !1600. Dmitry original

CONTAINS

!!
!================================================================================================================================
!! SUBROUTINE     : deep_carbcycle
!!
!>\BRIEF          Recalculate vegetation cover and LAI
!!
!!\n DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): None 
!!
!! REFERENCE(S)   : None
!! 
!! FLOWCHART :
!_
!================================================================================================================================

  SUBROUTINE deep_carbcycle(kjpindex, index, itau, time_step, lalo, clay, &
       tsurf, tprof, hslong_in, snow, heat_Zimov, pb, &  
       sfluxCH4_deep, sfluxCO2_deep, &
       deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil, O2_snow, CH4_snow, &
       zz_deep, zz_coef_deep, z_organic, soilc_in, veget_max, &
       rprof, altmax, carbon, carbon_surf, resp_hetero_soil, fbact, fixed_cryoturbation_depth, &
       snowdz,snowrho)

!! 0. Variable and parameter declaration    

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                            :: kjpindex
    REAL(r_std), INTENT(in)                               :: time_step         !! time step in seconds
    INTEGER(i_std), intent(in)                            :: itau              !! time step number
    REAL(r_std),DIMENSION(kjpindex,2),INTENT(in)          :: lalo              !! Geogr. coordinates (latitude,longitude) (degrees)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)          :: pb                !! surface pressure [pa]
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)          :: clay              !! clay content
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in)         :: index             !! Indeces of the points on the map
    REAL(r_std), DIMENSION(kjpindex), INTENT (in)         :: snow              !! Snow mass [Kg/m^2]
    REAL(r_std), DIMENSION(kjpindex,nsnow), INTENT(in)    :: snowdz            !! Snow depth [m]
    REAL(r_std), DIMENSION(kjpindex,nsnow), INTENT(in)    :: snowrho           !! snow density   
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)          :: zz_deep           !! deep vertical profile
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)          :: zz_coef_deep      !! deep vertical profile
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)    :: z_organic         !! depth to organic soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),INTENT (in):: tprof             !! deep temperature profile
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),INTENT (in):: hslong_in         !! deep long term soil humidity profile
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm),INTENT(in) :: soilc_in          !! carbon going into carbon pools  [gC/(m**2 of ground)/day]
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)       :: veget_max         !! Maximum vegetation fraction
    REAL(r_std), DIMENSION (kjpindex,nvm)                 :: veget_max_bg 
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)      :: rprof             !! rooting depth (m)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)          :: tsurf             !! skin temperature  [K]
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in):: fbact
   
    !! 0.2 Output variables
    REAL(r_std), DIMENSION(kjpindex), INTENT(out)             :: sfluxCH4_deep              !! total CH4 flux [g CH4 / m**2 / s]
    REAL(r_std), DIMENSION(kjpindex), INTENT(out)             :: sfluxCO2_deep              !! total CO2 flux [g C / m**2 / s]
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(out)         :: resp_hetero_soil           !! soil heterotrophic respiration (first in gC/day/m**2 of ground )
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT (out)  :: heat_Zimov                 !! Heating associated with decomposition  [W/m**3 soil]
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm), INTENT (out)  :: carbon                     !! vertically-integrated (diagnostic) soil carbon pool: active, slow, or passive, (gC/(m**2 of ground))
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm), INTENT (out)  :: carbon_surf                !! vertically-integrated (diagnostic) soil carbon pool: active, slow, or passive, (gC/(m**2 of ground))

    !! 0.3 Modified variables
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout) :: deepC_a                    !! Active soil carbon (g/m**3)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout) :: deepC_s                    !! Slow soil carbon (g/m**3) 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout) :: deepC_p                    !! Passive soil carbon (g/m**3)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout) :: O2_snow                    !! oxygen in the snow (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout) :: O2_soil                    !! oxygen in the soil (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout) :: CH4_snow                   !! methane in the snow (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout) :: CH4_soil                   !! methane in the soil (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(inout)       :: altmax                     !! active layer thickness (m)
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(inout)        :: fixed_cryoturbation_depth  !! depth to hold cryoturbation to for fixed runs

    !! 0.4 Local variables

    REAL(r_std), DIMENSION(kjpindex)                  :: overburden
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: fluxCH4,febul
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: sfluxCH4    
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: flupmt
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: MT                                 !! depth-integrated methane consumed in methanotrophy
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: MG                                 !! depth-integrated methane released in methanogenesis
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: CH4i                               !! depth-integrated methane
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: CH4ii                              !! depth-integrated initial methane
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: dC1i                               !! depth-integrated oxic decomposition carbon
    REAL(r_std), DIMENSION(kjpindex,nvm)              :: dCi                                !! depth-integrated soil carbon

    REAL(r_std), DIMENSION(kjpindex,nvm)              :: Tref                               !! Ref. temperature for growing season caluculation (C)	
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)        :: deltaCH4g, deltaCH4
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)        :: deltaC1_a,deltaC1_s,deltaC1_p,deltaC2,deltaC3
    REAL(r_std), DIMENSION(kjpindex,ncarb,ndeep,nvm)  :: dc_litter_z
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)        :: CH4ini_soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)        :: hslong                             !! deep long term soil humidity profile
    INTEGER(i_std)                   :: ip, il, itz, iz
    REAL(r_std), SAVE, DIMENSION(3)  :: lhc                       !! specific heat of soil organic matter oxidation (J/kg carbon)
    REAL(r_std), SAVE                :: O2m                       !! oxygen concentration [g/m3] below which there is anoxy 
    LOGICAL, SAVE                    :: ok_methane                !! Is Methanogenesis and -trophy taken into account?
    LOGICAL, SAVE                    :: ok_cryoturb               !! cryoturbate the carbon?
    REAL(r_std), SAVE                :: cryoturbation_diff_k_in   !! input time constant of cryoturbation (m^2/y)
    REAL(r_std), SAVE                :: bioturbation_diff_k_in    !! input time constant of bioturbation (m^2/y)
    REAL(r_std), SAVE                :: tau_CH4troph              !! time constant of methanetrophy (s)
    REAL(r_std), SAVE                :: fbactratio                !! time constant of methanogenesis (ratio to that of oxic)
    LOGICAL, SAVE                    :: firstcall = .TRUE.        !! first call?
    REAL(r_std), SAVE, DIMENSION(2)  :: lhCH4                     !! specific heat of methane transformation  (J/kg) (/ 3.1E6, 9.4E6 /)
    INTEGER(i_std), SAVE             :: frozen_respiration_func
    LOGICAL, SAVE                    :: oxlim                     !! O2 limitation taken into account
    LOGICAL, SAVE                    :: no_pfrost_decomp = .FALSE.!! Whether this is a spinup run
    REAL(r_std), PARAMETER           :: refdep = 0.20_r_std       !! Depth to compute reference temperature for the growing season (m). WH2000 use 0.50
    REAL(r_std), PARAMETER           :: Tgr  = 5.                 !! Temperature when plant growing starts and this becomes constant
    INTEGER(i_std)                   :: month,year,dayno          !! current time parameters
    REAL(r_std)                      :: scnd
    REAL(r_std)                      :: organic_layer_thickness
    REAL(r_std)                      :: fbact_a
    INTEGER(i_std)                   :: ier, iv, m, jv
    CHARACTER(80)                    :: yedoma_map_filename
    REAL(r_std)                      :: yedoma_depth, yedoma_cinit_act, yedoma_cinit_slo, yedoma_cinit_pas
    LOGICAL                          :: reset_yedoma_carbon
    LOGICAL, SAVE                    :: MG_useallCpools = .true.  !! Do we allow all three C pools to feed methanogenesis?
    CHARACTER(LEN=10)                :: part_str                  !! string suffix indicating an index
    REAL(r_std), SAVE                :: max_shum_value = 1.0      !! maximum saturation degree on the thermal axes
    REAL(r_std), DIMENSION(kjpindex) :: alt_pftmean, altmax_pftmean, tsurf_pftmean

    IF (printlev>=3) WRITE(*,*) 'Entering  deep_carbcycle'
    
    !! 0. first call 
    IF ( firstcall ) THEN                
       
       overburden(:)=1.
       !
       !Config Key   = organic_layer_thickness
       !Config Desc  = The thickness of organic layer
       !Config Def   = n
       !Config If    = OK_PC
       !Config Help  = This parameters allows the user to prescibe the organic
       !Config         layer thickness 
       !Config Units = [-]
       !
       organic_layer_thickness = 0.
       CALL getin_p('organic_layer_thickness', organic_layer_thickness)
       z_organic(:) = overburden(:)*organic_layer_thickness

       !
       !Config Key   = OK_METHANE
       !Config Desc  = Is Methanogenesis and -trophy taken into account?
       !Config Def   = n
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [flag]
       !
       CALL getin_p('OK_METHANE',ok_methane)
       !
       !Config Key   = HEAT_CO2_ACT
       !Config Desc  = specific heat of soil organic matter oxidation for active carbon (J/kg carbon)
       !Config Def   = 40.0E6 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [J/Kg]
       !
       CALL getin_p('HEAT_CO2_ACT',lhc(iactive))
       !
       !Config Key   = HEAT_CO2_SLO
       !Config Desc  = specific heat of soil organic matter oxidation for slow
       !Config         carbon pool (J/kg carbon)
       !Config Def   = 30.0E6 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [J/Kg]
       !
       CALL getin_p('HEAT_CO2_SLO',lhc(islow))
       !
       !Config Key   = HEAT_CO2_PAS
       !Config Desc  = specific heat of soil organic matter oxidation for
       !Config         passive carbon pool (J/kg carbon)
       !Config Def   = 10.0E6 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [J/Kg]
       !
       CALL getin_p('HEAT_CO2_PAS',lhc(ipassive))
       !
       !Config Key   = TAU_CH4_TROPH
       !Config Desc  = time constant of methanetrophy
       !Config Def   = 432000 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [s]
       ! 
       CALL getin_p('TAU_CH4_TROPH',tau_CH4troph)
       !
       !Config Key   = TAU_CH4_GEN_RATIO
       !Config Desc  = time constant of methanogenesis (ratio to that of oxic)
       !Config Def   = 9.0 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [-]
       !  
       CALL getin_p('TAU_CH4_GEN_RATIO',fbactratio)
       !
       !Config Key   = O2_SEUIL_MGEN
       !Config Desc  = oxygen concentration below which there is anoxy 
       !Config Def   = 3.0 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [g/m3]
       !  
       CALL getin_p('O2_SEUIL_MGEN',O2m)
       !
       !Config Key   = HEAT_CH4_GEN
       !Config Desc  = specific heat of methanogenesis 
       !Config Def   = 0 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [J/kgC]
       !  
       CALL getin_p('HEAT_CH4_GEN',lhCH4(1))
       !
       !Config Key   = HEAT_CH4_TROPH
       !Config Desc  = specific heat of methanotrophy 
       !Config Def   = 0 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [J/kgC]
       !  
       CALL getin_p('HEAT_CH4_TROPH',lhCH4(2))
       !
       !Config Key   = frozen_respiration_func
       !Config Desc  = which temperature function of carbon consumption
       !Config Def   = 1 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [-]
       ! 
       frozen_respiration_func=1
       CALL getin_p('frozen_respiration_func',frozen_respiration_func)
       !
       !Config Key   = O2_LIMIT
       !Config Desc  = O2 limitation taken into account
       !Config Def   = y 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [flag]
       ! 
       oxlim=.TRUE. 
       CALL getin_p('O2_LIMIT',oxlim)
       !
       !Config Key   = NO_PFROST_DECOMP
       !Config Desc  = whether this is spin-up
       !Config Def   = n 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [flag]
       ! 
       no_pfrost_decomp=.FALSE. 
       CALL getin_p('NO_PFROST_DECOMP',no_pfrost_decomp)
       !
       !Config Key   = cryoturbate
       !Config Desc  = Do we allow for cyoturbation?
       !Config Def   = y 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [flag]
       ! 
       ok_cryoturb=.TRUE.
       CALL getin_p('cryoturbate',ok_cryoturb)
       !
       !Config Key   = cryoturbation_diff_k_in
       !Config Desc  = diffusion constant for cryoturbation 
       !Config Def   = 0.001 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [m2/year]
       !  
       cryoturbation_diff_k_in = .001
       CALL getin_p('cryoturbation_diff_k',cryoturbation_diff_k_in)
       !
       !Config Key   = bioturbation_diff_k_in
       !Config Desc  = diffusion constant for bioturbation 
       !Config Def   = 0.0
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [m2/year]
       !  
       bioturbation_diff_k_in = 0.0001
       CALL getin_p('bioturbation_diff_k',bioturbation_diff_k_in)
       !
       !Config Key   = MG_useallCpools
       !Config Desc  = Do we allow all three C pools to feed methanogenesis?
       !Config Def   = y 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [flag]
       !    
       CALL getin_p('MG_useallCpools', MG_useallCpools)
       !
       !Config Key   = max_shum_value
       !Config Desc  = maximum saturation degree on the thermal axes
       !Config Def   = 1 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [-]
       !   
       max_shum_value=1.0 
       CALL getin_p('max_shum_value',max_shum_value)
       hslong(:,:,:) = MAX(MIN(hslong_in(:,:,:),max_shum_value),zero)
       !

       !!  Arrays allocations

       ALLOCATE (veget_mask_2d(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in veget_mask_2d allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE(lalo_global(kjpindex,2),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in lalo_global allocation. We stop. We need ',kjpindex,' fois ',2,' words = '&
              & , kjpindex*2
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (alt(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in alt allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (altmax_lastyear(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in altmax_lastyear allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF 

       ALLOCATE (alt_ind(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in alt_ind allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (altmax_ind(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in altmax_ind allocation. We stop. We need',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (altmax_ind_lastyear(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in altmax_ind allocation. We stop. We need',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (z_root(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in z_root allocation. We stop. We need',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (rootlev(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in rootlev allocation. We stop. We need',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (heights_snow(kjpindex,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in heights_snow allocation. We stop. We need',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (zf_soil(0:ndeep),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in zf_soil allocation. We stop. We need',ndeep+1,' words = '&
              & , ndeep+1
           STOP 'deep_carbcycle'
       END IF 
       
       ALLOCATE (zi_soil(ndeep),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in zi_soil allocation. We stop. We need',ndeep,' words = '&
              & , ndeep
           STOP 'deep_carbcycle'
       END IF 

       ALLOCATE (zf_snow(kjpindex,0:nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in zf_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow+1, ' fois ',nvm,' words = '&
              & , kjpindex*(nsnow+1)*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (zi_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN           
           WRITE (numout,*) ' error in zi_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (zf_snow_nopftdim(kjpindex,0:nsnow),stat=ier)   
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in zf_snow_nopftdim allocation. We stop. We need', kjpindex, ' fois ',nsnow+1,' words = '&
              & , kjpindex*(nsnow+1)
           STOP 'deep_carbcycle'
       END IF
       
       ALLOCATE (zi_snow_nopftdim(kjpindex,nsnow),stat=ier)
       IF (ier.NE.0) THEN           
           WRITE (numout,*) ' error in zi_snow_nopftdim allocation. We stop. We need', kjpindex, ' fois ',nsnow,' words = '&
              & , kjpindex*nsnow
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (airvol_soil(kjpindex,ndeep,nvm),stat=ier)       
       IF (ier.NE.0) THEN 
           WRITE (numout,*) ' error in airvol_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
           STOP 'deep_carbcycle'
       END IF
       
       ALLOCATE (totporO2_soil(kjpindex,ndeep,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in totporO2_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (totporCH4_soil(kjpindex,ndeep,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in totporCH4_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (conduct_soil(kjpindex,ndeep,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in conduct_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (diffO2_soil(kjpindex,ndeep,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in diffO2_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (diffCH4_soil(kjpindex,ndeep,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in diffCH4_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (airvol_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in airvol_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (totporO2_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in totporO2_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (totporCH4_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in totporCH4_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (conduct_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in conduct_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (diffO2_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in diffO2_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (diffCH4_snow(kjpindex,nsnow,nvm),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in diffCH4_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
              & , kjpindex*nsnow*nvm
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (deepc_pftmean(kjpindex,ndeep,ncarb),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in deepc_pftmean allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',ncarb,' words = '&
              & , kjpindex*ndeep*ncarb
           STOP 'deep_carbcycle'
       END IF

       !! assign values for arrays
       yr_len = NINT(one_year)

       veget_max_bg(:,2:nvm) = veget_max(:,2:nvm)
       veget_max_bg(:,1) = MAX((un - SUM(veget_max(:,2:nvm), 2)), zero)
!!       veget_mask_2d(:,:) = veget_max_bg .GT. EPSILON(zero)
!!       WHERE( ALL((.NOT. veget_mask_2d(:,:)), dim=2) )
!!          veget_mask_2d(:,1) = .TRUE.
!!       END WHERE
       veget_mask_2d(:,:) = .TRUE.

       lalo_global(:,:) = lalo(:,:)
       alt(:,:) = 0
       altmax_lastyear(:,:) = 0
       alt_ind(:,:) = 0
       altmax_ind(:,:) = 0
       altmax_ind_lastyear(:,:) = 0
       z_root(:,:) = 0
       rootlev(:,:) = 0
       ! make sure gas concentrations where not defined by veget_mask are equal
       !to initial conditions
       DO iv = 1, ndeep
          WHERE ( .NOT. veget_mask_2d(:,:) )
             O2_soil(:,iv,:) = O2_init_conc
             CH4_soil(:,iv,:) = CH4_init_conc
          END WHERE
       END DO
       DO iv = 1, nsnow
          WHERE ( .NOT. veget_mask_2d(:,:) )
             O2_snow(:,iv,:) = O2_surf
             CH4_snow(:,iv,:) = CH4_surf
          END WHERE
       END DO

       heights_snow(:,:) = zero
       zf_soil(:) = zero
       zi_soil(:) = zero
       zf_snow(:,:,:) = zero
       zi_snow(:,:,:) = zero
       zf_snow_nopftdim(:,:) = zero
       zi_snow_nopftdim(:,:) = zero
       airvol_soil(:,:,:) = zero
       totporO2_soil(:,:,:) = zero
       totporCH4_soil(:,:,:) = zero
       conduct_soil(:,:,:) = zero
       diffO2_soil(:,:,:) = zero
       diffCH4_soil(:,:,:) = zero
       airvol_snow(:,:,:) = zero
       totporO2_snow(:,:,:) = zero
       totporCH4_snow(:,:,:) = zero
       conduct_snow(:,:,:) = zero
       diffO2_snow(:,:,:) = zero
       diffCH4_snow(:,:,:) = zero

       ! get snow and soil levels
       DO iv = 1, nvm
          heights_snow(:,iv) = SUM(snowdz(:,1:nsnow), 2)
       ENDDO
       ! Calculating intermediate and full depths for snow
       call snowlevels (kjpindex, snowdz, zi_snow, zf_snow, veget_max_bg)

       ! here we need to put the shallow and deep soil levels together to make the complete soil levels. 
       ! This requires pulling in the indices from thermosoil and deepsoil_freeze.
       zi_soil(:) = zz_deep(:)
       zf_soil(1:ndeep) = zz_coef_deep(:)
       zf_soil(0) = 0.

       !    allocate arrays for gas diffusion        !
       !    get diffusion coefficients: heat capacity,
       !    conductivity, and oxygen diffusivity
       
       CALL get_gasdiff (kjpindex,hslong,tprof,snow,airvol_snow, &
            totporO2_snow,totporCH4_snow,diffO2_snow,diffCH4_snow, &
            airvol_soil,totporO2_soil,totporCH4_soil,diffO2_soil,diffCH4_soil, z_organic, snowrho)

       !
       !    initialize soil temperature calculation
       !
       CALL soil_gasdiff_main (kjpindex,time_step,index,'initialize', &	
            pb,tsurf,tprof,diffO2_snow,diffCH4_snow, &
            totporO2_snow,totporCH4_snow,O2_snow,CH4_snow,diffO2_soil,diffCH4_soil, &
            totporO2_soil,totporCH4_soil,O2_soil,CH4_soil, zi_snow, zf_snow)
       
       !
       !    calculate the coefficients
       !
       CALL soil_gasdiff_main (kjpindex,time_step,index,'coefficients', &
            pb,tsurf,tprof,diffO2_snow,diffCH4_snow, &
            totporO2_snow,totporCH4_snow,O2_snow,CH4_snow,diffO2_soil,diffCH4_soil, &
            totporO2_soil,totporCH4_soil,O2_soil,CH4_soil, zi_snow, zf_snow)
       


       CALL itau2ymds(itau, dt_sechiba, year, month, dayno, scnd)
       dayno = (month-1)*30 + dayno
       CALL altcalc (kjpindex, time_step, dayno, scnd, REAL(tprof), zi_soil, alt, alt_ind, altmax, altmax_ind, &
            altmax_lastyear, altmax_ind_lastyear)   

       IF (printlev>=3 ) THEN
          WRITE(*,*) 'deep_carbcycle: finished firstcall calcs'
       ENDIF

       ! reset
       !
       !Config Key   = reset_yedoma_carbon
       !Config Desc  = Do we reset carbon concentrations for yedoma region?
       !Config Def   = n 
       !Config If    = OK_PC
       !Config Help  = 
       !Config         
       !Config Units = [flag]
       ! 
       reset_yedoma_carbon = .false.
       CALL getin_p('reset_yedoma_carbon',reset_yedoma_carbon)

       IF (reset_yedoma_carbon) THEN
          yedoma_map_filename = 'NONE'
          yedoma_depth = zero
          yedoma_cinit_act = zero
          yedoma_cinit_slo = zero
          yedoma_cinit_pas = zero
          !
          !Config Key   = yedoma_map_filename
          !Config Desc  = The filename for yedoma map
          !Config Def   = yedoma_map.nc 
          !Config If    = OK_PC
          !Config Help  = 
          !Config         
          !Config Units = []
          ! 
          CALL getin_p('yedoma_map_filename', yedoma_map_filename)
          !
          !Config Key   = yedoma_depth
          !Config Desc  = The depth for soil carbon in yedoma
          !Config Def   = 20 
          !Config If    = OK_PC
          !Config Help  = 
          !Config         
          !Config Units = [m]
          ! 
          CALL getin_p('yedoma_depth', yedoma_depth)
          !
          !Config Key   = deepC_a_init
          !Config Desc  = Carbon concentration for active soil C pool in yedoma
          !Config Def   = 1790.1  
          !Config If    = OK_PC
          !Config Help  = 
          !Config         
          !Config Units = []
          ! 
          CALL getin_p('deepC_a_init', yedoma_cinit_act)
          !
          !Config Key   = deepC_s_init
          !Config Desc  = Carbon concentration for slow soil C pool in yedoma
          !Config Def   = 14360.8
          !Config If    = OK_PC
          !Config Help  = 
          !Config         
          !Config Units = []
          ! 
          CALL getin_p('deepC_s_init', yedoma_cinit_slo)
          !
          !Config Key   = deepC_p_init
          !Config Desc  = Carbon concentration for passive soil C pool in yedoma
          !Config Def   = 1436
          !Config If    = OK_PC
          !Config Help  = 
          !Config         
          !Config Units = []
          ! 
          CALL getin_p('deepC_p_init', yedoma_cinit_pas)
          ! intialize the yedoma carbon stocks
          CALL initialize_yedoma_carbonstocks(kjpindex, lalo, deepC_a, deepC_s, deepC_p, zz_deep, &
               yedoma_map_filename, yedoma_depth, yedoma_cinit_act,yedoma_cinit_slo, yedoma_cinit_pas, altmax_ind)
       ENDIF


    ENDIF ! firstcall

    ! Prepare values for arrays
    veget_max_bg(:,2:nvm) = veget_max(:,2:nvm)
    veget_max_bg(:,1) = MAX((un - SUM(veget_max(:,2:nvm), 2)), zero)

    ! whether this is a C spin-up; if not, then 
    IF ( .NOT. no_pfrost_decomp ) THEN
    
            IF ( ANY(rootlev(:,:) .GT. ndeep) ) THEN
               WRITE(*,*) 'problems with rootlev:', rootlev
               STOP
            ENDIF
  
            DO iv = 1, nvm
                  heights_snow(:,iv) = SUM(snowdz(:,1:nsnow), 2)
            ENDDO
            !
            ! define initial CH4 value (before the time step)
            CH4ini_soil(:,:,:) = CH4_soil(:,:,:)
  
            ! apply maximum soil wetness criteria to prevent soils from turning to wetlands where they aren't supposed to
            hslong(:,:,:) = MAX(MIN(hslong_in(:,:,:),max_shum_value),zero)
            
            
            ! update the gas profiles
            !
            CALL soil_gasdiff_main (kjpindex, time_step, index, 'diffuse', &
                 pb,tsurf,tprof,diffO2_snow,diffCH4_snow, &
                 totporO2_snow,totporCH4_snow,O2_snow,CH4_snow,diffO2_soil,diffCH4_soil, &
                 totporO2_soil,totporCH4_soil,O2_soil,CH4_soil, zi_snow, zf_snow)
  
            ! get new snow levels and interpolate gases on these levels
            !
            CALL snow_interpol (kjpindex,O2_snow, CH4_snow, zi_snow, zf_snow, veget_max_bg, snowdz)
            
            ! Compute active layer thickness
            CALL itau2ymds(itau, dt_sechiba, year, month, dayno, scnd)
                dayno = (month-1)*30 + dayno
  
            CALL altcalc (kjpindex, time_step, dayno, scnd, REAL(tprof), zi_soil, alt, alt_ind, altmax, altmax_ind, &
                 altmax_lastyear, altmax_ind_lastyear)     
  
            ! list pft-mean alt and altmax for debugging purposes
            IF (printlev>=3) THEN
               alt_pftmean(:) = 0.
               altmax_pftmean(:) = 0.
               tsurf_pftmean(:) = 0.
               DO iv = 1, nvm
                  WHERE ( veget_mask_2d(:,iv) )
                     alt_pftmean(:) = alt_pftmean(:) + alt(:,iv)*veget_max_bg(:,iv)
                     altmax_pftmean(:) = altmax_pftmean(:) + altmax(:,iv)*veget_max_bg(:,iv)
                     tsurf_pftmean(:) = tsurf_pftmean(:) + tprof(:,1,iv)*veget_max_bg(:,iv)
                  END WHERE
               END DO
            END IF
  
            ! Make sure the rooting depth is within the active layer
            
            !need to sort out the rooting depth, by each STOMATE PFT
            WHERE ( altmax_lastyear(:,:) .LT. z_root_max .and. veget_mask_2d(:,:) )
               z_root(:,:) = altmax_lastyear(:,:)
               rootlev(:,:) = altmax_ind_lastyear(:,:) 
            ELSEWHERE ( veget_mask_2d(:,:) )
               z_root(:,:) = z_root_max
               rootlev(:,:) = altmax_ind_lastyear(:,:)  
            ENDWHERE
                
            IF (ok_cryoturb) CALL cryoturbate(kjpindex, time_step, dayno, altmax_ind_lastyear, deepC_a, deepC_s, deepC_p, &
                 'diffuse', cryoturbation_diff_k_in/(one_day*one_year), bioturbation_diff_k_in/(one_day*one_year), &
                 altmax_lastyear, fixed_cryoturbation_depth)
            !
            ! Carbon input into the soil
            !
             CALL carbinput(kjpindex,time_step,itau*time_step,no_pfrost_decomp,tprof,tsurf,hslong,dayno,z_root,altmax_lastyear, &
                  deepC_a, deepC_s, deepC_p, soilc_in, dc_litter_z, z_organic, veget_max_bg, rprof)
             !
  
             CALL permafrost_decomp (kjpindex, time_step, tprof, frozen_respiration_func, airvol_soil, &
                  oxlim, tau_CH4troph, ok_methane, fbactratio, O2m, &
                  totporO2_soil, totporCH4_soil, hslong, clay, &
                  no_pfrost_decomp, deepC_a, deepC_s, deepC_p, deltaCH4g, deltaCH4, deltaC1_a, deltaC1_s, deltaC1_p, deltaC2, &
                  deltaC3, O2_soil, CH4_soil, fbact, MG_useallCpools)
  
             DO ip = 1, kjpindex
                DO iv = 1, nvm
                   IF ( veget_mask_2d(ip,iv) ) THEN
                      ! oxic decomposition
                      heat_Zimov(ip,:,iv) = lhc(iactive)*1.E-3*deltaC1_a(ip,:,iv) + &
                                            lhc(islow)*1.E-3*deltaC1_s(ip,:,iv) + &
                                            lhc(ipassive)*1.E-3*deltaC1_p(ip,:,iv)
                      !
                      ! methanogenesis
                      heat_Zimov(ip,:,iv) = heat_Zimov(ip,:,iv) + lhCH4(1)*1.E-3*deltaC2(ip,:,iv)
                      !
                      ! methanotrophy
                      heat_Zimov(ip,:,iv) = heat_Zimov(ip,:,iv) + lhCH4(2)*1.E-3*deltaCH4(ip,:,iv) *  &
                           totporCH4_soil(ip,:,iv)
                      !
                      heat_Zimov(ip,:,iv) = heat_Zimov(ip,:,iv)/time_step
                      
                      !
                      fluxCH4(ip,iv) = zero
                   ELSE
                      heat_Zimov(ip,:,iv) = zero
                      fluxCH4(ip,iv) = zero
                   ENDIF
                ENDDO
             ENDDO
  
             IF  ( .NOT. firstcall) THEN 
                !
                ! Plant-mediated CH4 transport     
                !
               CALL traMplan(CH4_soil,O2_soil,kjpindex,time_step,totporCH4_soil,totporO2_soil,z_root, &
                    rootlev,Tgr,Tref,hslong,flupmt, &
                    refdep, zi_soil, tprof)
               !	flupmt=zero
               !
               ! CH4 ebullition
               !
               
               CALL ebullition (kjpindex,time_step,tprof,totporCH4_soil,hslong,CH4_soil,febul)
  
               !
            ENDIF 
            
            !
            MT(:,:)=zero   
            MG(:,:)=zero    
            CH4i(:,:)=zero  
            CH4ii(:,:)=zero 
            dC1i(:,:)=zero 
            dCi(:,:)=zero   
            !
            DO ip = 1, kjpindex
               DO iv = 1, nvm
                  IF (  veget_mask_2d(ip,iv) ) THEN
                     DO il=1,ndeep
                        MT(ip,iv) = MT(ip,iv) + deltaCH4(ip,il,iv)*totporCH4_soil(ip,il,iv) * &
                             ( zf_soil(il) - zf_soil(il-1) )
                        MG(ip,iv) = MG(ip,iv) + deltaCH4g(ip,il,iv)*totporCH4_soil(ip,il,iv) * &
                             ( zf_soil(il) - zf_soil(il-1) )
                        CH4i(ip,iv) = CH4i(ip,iv) + CH4_soil(ip,il,iv)*totporCH4_soil(ip,il,iv) * &
                             (zf_soil(il)-zf_soil(il-1))
                        CH4ii(ip,iv) = CH4ii(ip,iv) +  &
                             CH4ini_soil(ip,il,iv)*totporCH4_soil(ip,il,iv) * &
                             (zf_soil(il)-zf_soil(il-1))          
                        dC1i(ip,iv) = dC1i(ip,iv) + (deltaC1_a(ip,il,iv)+deltaC1_s(ip,il,iv)+deltaC1_p(ip,il,iv)) * &
                             ( zf_soil(il) - zf_soil(il-1) )
                        dCi(ip,iv) = dCi(ip,iv) + (deepC_a(ip,il,iv) + deepC_s(ip,il,iv) + deepC_p(ip,il,iv)) * &
                             ( zf_soil(il) - zf_soil(il-1) )
                     END DO
                  ENDIF
               ENDDO
            ENDDO
            
            !
            !
            
            DO ip = 1, kjpindex
               ! Total CH4 flux
               sfluxCH4_deep(ip) = SUM(veget_max_bg(ip,:)*( CH4ii(ip,:)-CH4i(ip,:)+MG(ip,:)-MT(ip,:) ))/time_step
               ! TotalCO2 flux
               sfluxCO2_deep(ip) = SUM(veget_max_bg(ip,:)*( dC1i(ip,:) + MT(ip,:)*(12./16.) ) )/time_step
            END DO
  
            resp_hetero_soil(:,:) = ( dC1i(:,:) + MT(:,:)*(12./16.) ) *one_day/time_step
            sfluxCH4(:,:) = ( CH4ii(:,:)-CH4i(:,:)+MG(:,:)-MT(:,:) ) *one_day/time_step
  
  
            ! calculate coefficients for cryoturbation calculation
            IF (ok_cryoturb) CALL cryoturbate(kjpindex, time_step, dayno, altmax_ind_lastyear, deepC_a, deepC_s, deepC_p, &
                 'coefficients', cryoturbation_diff_k_in/(one_day*one_year),bioturbation_diff_k_in/(one_day*one_year), &
                 altmax_lastyear, fixed_cryoturbation_depth)
  
            ! calculate the coefficients for the next timestep:
            !
            ! get diffusion coefficients: heat capacity,
            !    conductivity, and oxygen diffusivity
            !
            CALL get_gasdiff (kjpindex,hslong,tprof,snow,airvol_snow, &
                 totporO2_snow,totporCH4_snow,diffO2_snow,diffCH4_snow, &
                 airvol_soil,totporO2_soil,totporCH4_soil,diffO2_soil,diffCH4_soil, z_organic, snowrho)
            
            !
            ! calculate the coefficients for the next time step
            !
            CALL soil_gasdiff_main (kjpindex,time_step,index,'coefficients', &
                 pb,tsurf,tprof,diffO2_snow,diffCH4_snow, &
                 totporO2_snow,totporCH4_snow,O2_snow,CH4_snow,diffO2_soil,diffCH4_soil, &
                 totporO2_soil,totporCH4_soil,O2_soil,CH4_soil, zi_snow, zf_snow)
  
            call calc_vert_int_soil_carbon(kjpindex, deepC_a, deepC_s, deepC_p, carbon, carbon_surf, zf_soil)
            IF (printlev>=3) WRITE(*,*) 'after calc_vert_int_soil_carbon'
    ENDIF
    
    ! define pft-mean soil C profile
    deepC_pftmean(:,:,:) = 0._r_std
    do iv = 1, nvm
       do il=1,ndeep
          deepC_pftmean(:,il,iactive)  = deepC_pftmean(:,il,iactive)  + deepC_a(:,il,iv) * veget_max(:,iv)
          deepC_pftmean(:,il,islow)    = deepC_pftmean(:,il,islow)    + deepC_s(:,il,iv) * veget_max(:,iv)
          deepC_pftmean(:,il,ipassive) = deepC_pftmean(:,il,ipassive) + deepC_p(:,il,iv) * veget_max(:,iv)
       end do
    end do

    !history output
    IF ( .NOT. soilc_isspinup ) THEN

       CALL histwrite_p (hist_id_stomate, 'tsurf', itime, tsurf, kjpindex, index)
       CALL histwrite_p (hist_id_stomate, 'fluxCH4', itime, sfluxCH4, kjpindex*nvm, horipft_index)
       CALL histwrite_p (hist_id_stomate, 'febul', itime, (febul*one_day), kjpindex*nvm, horipft_index)
       CALL histwrite_p (hist_id_stomate, 'flupmt', itime, (flupmt*one_day), kjpindex*nvm, horipft_index)
       CALL histwrite_p (hist_id_stomate, 'alt', itime, alt, kjpindex*nvm, horipft_index)
       CALL histwrite_p (hist_id_stomate, 'altmax', itime, altmax, kjpindex*nvm, horipft_index)
       CALL histwrite_p (hist_id_stomate, 'sfluxCH4_deep', itime, sfluxCH4_deep, kjpindex, index)
       CALL histwrite_p (hist_id_stomate, 'sfluxCO2_deep', itime, sfluxCO2_deep, kjpindex, index)
       CALL histwrite_p (hist_id_stomate, 'pb', itime, pb, kjpindex, index)
       call histwrite_p (hist_id_stomate, 'deepC_a_pftmean', itime, deepC_pftmean(:,:,iactive), kjpindex*ndeep, horideep_index)
       call histwrite_p (hist_id_stomate, 'deepC_s_pftmean', itime, deepC_pftmean(:,:,islow), kjpindex*ndeep, horideep_index)
       call histwrite_p (hist_id_stomate, 'deepC_p_pftmean', itime, deepC_pftmean(:,:,ipassive), kjpindex*ndeep, horideep_index)
       DO jv = 1, nvm   
          IF (permafrost_veg_exists(jv)) THEN  !don't bother to write if there are pfts that don't exist in our domain
             WRITE(part_str,'(I2)') jv
             IF (jv < 10) part_str(1:1) = '0'
             IF (writehist_deepC) THEN
                CALL histwrite_p (hist_id_stomate, 'deepC_a_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deepC_a(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deepC_s_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deepC_s(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deepC_p_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deepC_p(:,:,jv), kjpindex*ndeep, horideep_index)
             ENDIF
             IF (writehist_soilgases) THEN
                CALL histwrite_p (hist_id_stomate, 'O2_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, O2_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'CH4_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, CH4_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'O2_snow_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, O2_snow(:,:,jv), kjpindex*nsnow, horisnow_index) 
                CALL histwrite_p (hist_id_stomate, 'CH4_snow_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, CH4_snow(:,:,jv), kjpindex*nsnow, horisnow_index)
             ENDIF
             IF (writehist_deltaC) THEN
                CALL histwrite_p (hist_id_stomate, 'deltaCH4g_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deltaCH4g(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deltaCH4_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deltaCH4(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deltaC1_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deltaC1_a(:,:,jv)+deltaC1_s(:,:,jv)+deltaC1_p(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deltaC2_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deltaC2(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deltaC3_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, deltaC3(:,:,jv), kjpindex*ndeep, horideep_index)
             ENDIF

             IF (writehist_zimovheat) THEN
                CALL histwrite_p (hist_id_stomate, 'heat_Zimov_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, heat_Zimov(:,:,jv), kjpindex*ndeep, horideep_index)
             ENDIF

             IF (writehist_deltaC_litter) THEN
                CALL histwrite_p (hist_id_stomate, 'deltaC_litter_act_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, dc_litter_z(:,iactive,:,jv)/ time_step, kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'deltaC_litter_slo_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, dc_litter_z(:,islow,:,jv)/ time_step, kjpindex*ndeep, horideep_index)
             ENDIF
             !------------------------------  further output for debugging/diagnosing
             
             IF (writehist_gascoeff) THEN
                CALL histwrite_p (hist_id_stomate, 'totporO2_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, totporO2_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'diffO2_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, diffO2_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'alphaO2_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, alphaO2_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'betaO2_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, betaO2_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                
                CALL histwrite_p (hist_id_stomate, 'totporCH4_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, totporCH4_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'diffCH4_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, diffCH4_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'alphaCH4_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, alphaCH4_soil(:,:,jv), kjpindex*ndeep, horideep_index)
                CALL histwrite_p (hist_id_stomate, 'betaCH4_soil_'//part_str(1:LEN_TRIM(part_str)), &
                     itime, betaCH4_soil(:,:,jv), kjpindex*ndeep, horideep_index)
             ENDIF
          END IF
       END DO

    ENDIF
    IF (printlev>=3) WRITE(*,*) 'cdk: leaving  deep_carbcycle'

    IF ( firstcall )  firstcall = .FALSE.


  END SUBROUTINE deep_carbcycle
  
!!
!================================================================================================================================
!! SUBROUTINE   : altcalc
!!
!>\BRIEF        This routine calculate active layer thickness
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : alt
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================  
  SUBROUTINE altcalc (kjpindex,time_step,dayno,scnd, temp, zprof, alt, alt_ind, altmax, altmax_ind, &
        altmax_lastyear, altmax_ind_lastyear)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables

    INTEGER(i_std), INTENT(in) 			                 :: kjpindex
    REAL(r_std), INTENT(in)                                      :: time_step           !! time step in seconds
    INTEGER(i_std), INTENT(in)			                 :: dayno               !! number of the day in the current year
    REAL(r_std), INTENT(in)			                 :: scnd                !! model time & time step
    REAL(r_std), DIMENSION(kjpindex,ndeep, nvm), INTENT(in)      :: temp                !! soil temperature
    REAL(r_std), DIMENSION(ndeep), INTENT(in)   	         :: zprof               !! soil depths (m)

    !! 0.2 Output variables

    REAL(r_std), DIMENSION(kjpindex, nvm), INTENT(out)	         :: alt	                !! active layer thickness  
    INTEGER, DIMENSION(kjpindex, nvm), INTENT(out)	         :: alt_ind	        !! active layer index  
    
    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex, nvm),INTENT(inout)	         :: altmax_lastyear     !! Maximum active-layer thickness
    REAL(r_std), DIMENSION(kjpindex, nvm),INTENT(inout)          :: altmax              !! Maximum active-layer thickness
    INTEGER(i_std), DIMENSION(kjpindex, nvm),INTENT(inout)	 :: altmax_ind          !! Maximum over the year active-layer index
    INTEGER(i_std), DIMENSION(kjpindex, nvm),INTENT(inout)       :: altmax_ind_lastyear !! Maximum over the year active-layer index

    !! 0.4 Local variables

    INTEGER				                         :: ix,iz,il,iv         !! grid indices
    LOGICAL, SAVE 				                 :: firstcall = .TRUE.
    INTEGER, save                                                :: tcounter
    INTEGER(i_std), SAVE  			                 :: id, id2
    LOGICAL, SAVE			                         :: check = .FALSE.
    LOGICAL, SAVE			                         :: newaltcalc = .FALSE.
    LOGICAL, DIMENSION(kjpindex,nvm)                             :: inalt, bottomlevelthawed
    CHARACTER(LEN=16)			                         :: buf  
    INTEGER                                                      :: lev

    
    IF ( firstcall )  THEN

       ! calculate altmax_ind from altmax
       altmax_ind(:,:) = 0
       DO ix = 1, kjpindex
          DO iv = 1, nvm
             IF ( veget_mask_2d(ix,iv) ) THEN
                DO il=1,ndeep
                   IF ( altmax(ix,iv) .GE. zprof(il) ) THEN
                      altmax_ind(ix,iv) = altmax_ind(ix,iv) + 1
                   END IF
                END DO
             END IF
          END DO
       END DO
       altmax_lastyear(:,:) = altmax(:,:)
       altmax_ind_lastyear(:,:) = altmax_ind(:,:)
       firstcall = .FALSE.

       !Config Key   = newaltcalc
       !Config Desc  = calculate alt ?
       !Config Def   = n
       !Config If    = OK_PC
       !Config Help  = 
       !Config Unit  = [flag]
       CALL getin_p('newaltcalc', newaltcalc)

    ELSE
       ! all other timesteps
       IF ( .NOT. newaltcalc ) THEN
          DO ix = 1, kjpindex
             DO iv = 1, nvm
                IF ( veget_mask_2d(ix,iv) ) THEN
                   iz = 1
                   DO WHILE( temp(ix,iz,iv) > ZeroCelsius .AND. iz < ndeep )
                      iz = iz + 1	  
                   END DO
                   IF( iz == 1 ) THEN 
                      ! it means that all is frozen
                      alt(ix,iv) = zero
                   ELSE
                      alt(ix,iv) = zprof(iz-1)
                   END IF
                   alt_ind(ix,iv) = iz-1
                END IF
             END DO
          END DO
       ELSE          
          ! initialize for pfts that don't exist
          alt(:,:) = zprof(ndeep)  
          bottomlevelthawed(:,:) = .FALSE.
          ! start from bottom and work up instead
          WHERE (temp(:,ndeep,:) > ZeroCelsius ) 
             bottomlevelthawed(:,:) = .TRUE.
             alt(:,:) = zprof(ndeep)
             alt_ind(:,:) = ndeep
          END WHERE
          inalt(:,:) = .FALSE.
          DO iz = 1, ndeep - 1
             lev = ndeep - iz
             WHERE ( temp(:,lev,:) > ZeroCelsius .AND. .NOT. inalt(:,:) .AND. .NOT. bottomlevelthawed(:,:) )
                inalt(:,:) = .TRUE.
                alt(:,:) = zprof(lev)
                alt_ind(:,:) = lev
             ELSEWHERE ( temp(:,lev,:) <= ZeroCelsius .AND. inalt(:,:) .AND. .NOT. bottomlevelthawed(:,:) )
                inalt(:,:) = .FALSE.
             END WHERE
          END DO
          WHERE ( .NOT. inalt .AND. .NOT. bottomlevelthawed(:,:) ) 
             alt(:,:) = zero
             alt_ind(:,:) = 0
          END WHERE
       ENDIF

       ! debug
       IF ( check ) THEN
          IF (ANY(alt(:,:) .GT. zprof(ndeep))) THEN
             WRITE(*,*) 'error: alt greater than soil depth.'
          ENDIF
       ENDIF

       ! Maximum over the year active layer thickness
       WHERE ( ( alt(:,:) .GT. altmax(:,:) ) .AND. veget_mask_2d(:,:)  ) 
          altmax(:,:) = alt(:,:)
          altmax_ind(:,:) = alt_ind(:,:)
       ENDWHERE
       
       IF ( .NOT. soilc_isspinup ) THEN
             ! do it on the second timestep, that way when we are writing restart files it is not done before that!
             ! now we are doing daily permafrost calcs, so just run it on the second day.
          IF ( ( dayno .EQ. 2) ) THEN  
             ! Reinitialize ALT_max
             altmax_lastyear(:,:) = altmax(:,:)
             altmax_ind_lastyear(:,:) = altmax_ind(:,:)
             altmax(:,:) = alt(:,:)
             altmax_ind(:,:) = alt_ind(:,:)
          END IF
       ELSE

             ! for spinup, best to set altmax_lastyear to altmax, and not boter to reset since every year is the same, 
             ! and if you try to do so, it doesn't work properly --  06 may 2010
          altmax_lastyear(:,:) = altmax(:,:)
          altmax_ind_lastyear(:,:) = altmax_ind(:,:)
       END IF
    END IF

    IF (printlev>=3) WRITE(*,*) 'leaving  altcalc'
  END SUBROUTINE altcalc
  
!!
!================================================================================================================================
!! SUBROUTINE   : soil_gasdiff_main
!!
!>\BRIEF        This routine calculate oxygen and methane in the snow/soil medium 
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================   
  SUBROUTINE soil_gasdiff_main( kjpindex,time_step,index,action, &
       psol,tsurf,tprof,diffO2_snow,diffCH4_snow, &
       totporO2_snow,totporCH4_snow,O2_snow,CH4_snow,diffO2_soil,diffCH4_soil, &
       totporO2_soil,totporCH4_soil,O2_soil,CH4_soil, zi_snow, zf_snow)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables

    INTEGER(i_std), INTENT(in)                                 :: kjpindex           !! number of grid points 
    REAL(r_std), INTENT(in)                                    :: time_step          !! time step in seconds
    CHARACTER(LEN=*), INTENT(in)                               :: action             !! what to do 
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: psol               !! surface pressure (Pa)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: tsurf              !! Surface temperature (K)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: tprof              !! Soil temperature (K)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: diffO2_snow        !! oxygen diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: diffCH4_snow       !! methane diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: totporO2_snow      !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: totporCH4_snow     !! total CH4 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: diffO2_soil        !! oxygen diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: diffCH4_soil       !! methane diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: totporO2_soil      !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: totporCH4_soil     !! total CH4 porosity (Tans, 1998)
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in)  :: index                          !! Indeces of permafrost points on the map

    !! 0.2  Output variables

    !! 0.3  Modified variables

    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)  :: O2_snow            !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)  :: CH4_snow           !! methane (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: O2_soil            !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: CH4_soil           !! methane (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,0:nsnow,nvm), intent(inout):: zf_snow            !! depths of full levels (m)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), intent(inout)  :: zi_snow            !! depths of intermediate levels (m)
  
    !! 0.4 local variables

    CHARACTER(LEN=20), SAVE        :: last_action = 'not called'
    
    
    ! 1. ensure that we do not repeat actions
    !
    IF ( action .EQ. last_action ) THEN
       !
       WRITE(*,*) 'CANNOT TAKE THE SAME ACTION TWICE: ',TRIM(action)
       STOP
       !
    ENDIF
    !
    ! 2. decide what to do
    !
    IF ( action .EQ. 'initialize' ) THEN
       !
       ! 2.1 initialize
       !
       IF ( TRIM(last_action) .NE. 'not called' ) THEN
          !
          WRITE(*,*) 'SOIL MODEL CANNOT BE INITIALIZED TWICE.'
          STOP
          !
       ENDIF
       !
       CALL soil_gasdiff_alloc( kjpindex )
       !
    ELSEIF ( action .EQ. 'diffuse' ) THEN
       !
       ! 2.2 calculate soil temperatures
       !
       CALL soil_gasdiff_diff( kjpindex,time_step,index,psol,tsurf, O2_snow, CH4_snow, O2_soil, CH4_soil)
       !
    ELSEIF ( action .EQ. 'coefficients' ) THEN
       !
       ! 2.3 calculate coefficients (heat flux and apparent surface heat capacity)
       !
       CALL soil_gasdiff_coeff( kjpindex,time_step,tprof,O2_snow,CH4_snow, &
            diffO2_snow,diffCH4_snow,totporO2_snow,totporCH4_snow,O2_soil,CH4_soil, &
            diffO2_soil,diffCH4_soil,totporO2_soil,totporCH4_soil, zi_snow, zf_snow)
       !
    ELSE
       !
       ! 2.4 do not know this action
       !
       WRITE(*,*) 'DO NOT KNOW WHAT TO DO: ',TRIM(action)
       STOP
       !
    ENDIF
    !
    ! 2.5 keep last action in mind
    !
    last_action = action
    
    IF (printlev>=3) WRITE(*,*) 'leaving  soil_gasdiff_main'
  END SUBROUTINE soil_gasdiff_main
 
!!
!================================================================================================================================
!! SUBROUTINE   : soil_gasdiff_alloc
!!
!>\BRIEF        This routine allocate arrays related to oxygen and methane in the snow/soil medium 
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================   
  SUBROUTINE soil_gasdiff_alloc( kjpindex )
   
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
 
    INTEGER(i_std), INTENT(in)                             :: kjpindex

    !! 0.2 Output variables

    !! 0.3 Modified variables
    
    !! 0.4 local variables

    INTEGER(i_std)                                         :: ier
    
    ! Allocate the variables that need to be saved after soil_gasdiff_coeff

      ALLOCATE (alphaO2_soil(kjpindex,ndeep,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in alphaO2_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
             & , kjpindex*ndeep*nvm
          STOP 'deep_carbcycle'
      END IF
   
      ALLOCATE (betaO2_soil(kjpindex,ndeep,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in betaO2_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
             & , kjpindex*ndeep*nvm
          STOP 'deep_carbcycle'
      END IF
 
      ALLOCATE (alphaCH4_soil(kjpindex,ndeep,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in alphaCH4_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
             & , kjpindex*ndeep*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (betaCH4_soil(kjpindex,ndeep,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in betaCH4_soil allocation. We stop. We need', kjpindex, ' fois ',ndeep, ' fois ',nvm,' words = '&
             & , kjpindex*ndeep*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (alphaO2_snow(kjpindex,nsnow,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in alphaO2_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
             & , kjpindex*nsnow*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (betaO2_snow(kjpindex,nsnow,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in betaO2_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
             & , kjpindex*nsnow*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (alphaCH4_snow(kjpindex,nsnow,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in alphaCH4_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
             & , kjpindex*nsnow*nvm
          STOP 'deep_carbcycle'
      END IF
 
      ALLOCATE (betaCH4_snow(kjpindex,nsnow,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in betaCH4_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
             & , kjpindex*nsnow*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (zf_coeff_snow(kjpindex,0:nsnow,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in zf_coeff_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow+1, ' fois ',nvm,' words = '&
             & , kjpindex*(nsnow+1)*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (zi_coeff_snow(kjpindex,nsnow,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in zi_coeff_snow allocation. We stop. We need', kjpindex, ' fois ',nsnow, ' fois ',nvm,' words = '&
             & , kjpindex*nsnow*nvm
          STOP 'deep_carbcycle'
      END IF

      ALLOCATE (mu_snow(kjpindex,nvm),stat=ier)
      IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in mu_snow allocation. We stop. We need', kjpindex, ' fois ',nvm,' words = '&
             & , kjpindex*nvm
          STOP 'deep_carbcycle'
      END IF

      alphaO2_soil(:,:,:) = zero
      betaO2_soil(:,:,:) = zero
      alphaCH4_soil(:,:,:) = zero
      betaCH4_soil(:,:,:) = zero
      alphaO2_snow(:,:,:) = zero
      betaO2_snow(:,:,:) = zero
      alphaCH4_snow(:,:,:) = zero
      betaCH4_snow(:,:,:) = zero
      zf_coeff_snow(:,:,:) = zero
      zi_coeff_snow(:,:,:) = zero
      mu_snow(:,:) = zero
    
  END SUBROUTINE soil_gasdiff_alloc
 
!!
!================================================================================================================================
!! SUBROUTINE   : soil_gasdiff_coeff
!!
!>\BRIEF        This routine calculate coeff related to gas diffuvisity
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================   
  
  SUBROUTINE soil_gasdiff_coeff( kjpindex,time_step,tprof,O2_snow,CH4_snow, &
       diffO2_snow,diffCH4_snow,totporO2_snow,totporCH4_snow,O2_soil,CH4_soil, &
       diffO2_soil,diffCH4_soil,totporO2_soil,totporCH4_soil, zi_snow, zf_snow)


  !! 0. Variable and parameter declaration

    !! 0.1  Input variables

    INTEGER(i_std), INTENT(in)                                 :: kjpindex            !! number of grid points 
    REAL(r_std), INTENT(in)                                    :: time_step           !! time step in seconds
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: tprof               !! Soil temperature (K)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: diffO2_snow         !! oxygen diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: diffCH4_snow        !! methane diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: totporO2_snow       !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: totporCH4_snow      !! total CH4 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: diffO2_soil         !! oxygen diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: diffCH4_soil        !! methane diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: totporO2_soil       !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: totporCH4_soil      !! total CH4 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: O2_snow             !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: CH4_snow            !! methane (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: O2_soil             !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: CH4_soil            !! methane (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,0:nsnow,nvm), INTENT(in)   :: zf_snow             !! depths of full levels (m)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(in)     :: zi_snow             !! depths of intermediate levels (m)

    !! 0.2  Output variables

    !! 0.3  Modified variables

    !! 0.4 local variables

    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)         :: xcO2_snow,xdO2_snow
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)         :: xcCH4_snow,xdCH4_snow
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)         :: xcO2_soil,xdO2_soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)         :: xcCH4_soil,xdCH4_soil
    INTEGER(i_std)                                     :: il
    REAL(r_std), DIMENSION(kjpindex,nvm)               :: xeO2,xeCH4
    LOGICAL, DIMENSION(kjpindex,nvm)                   :: snow_height_mask_2d
    LOGICAL, SAVE :: firstcall = .true.

    ! loop over materials (soil, snow), beginning at the bottom
    !
    ! 1. define useful variables linked to geometry and physical properties
    !
    ! 1.1 normal levels
    !
    ! default value if inexistent
    xcO2_snow(:,1,:) = xcO2_soil(:,1,:)
    xdO2_snow(:,1,:) = xdO2_soil(:,1,:)
    xcCH4_snow(:,1,:) = xcCH4_soil(:,1,:)
    xdCH4_snow(:,1,:) = xdCH4_soil(:,1,:)
    !
    snow_height_mask_2d(:,:) = ( heights_snow(:,:) .GT. hmin_tcalc )
    !
    DO il = 1,nsnow-1
       !
       WHERE ( snow_height_mask_2d(:,:) .AND. veget_mask_2d(:,:) )
          !
          xcO2_snow(:,il,:) = ( zf_snow(:,il,:) - zf_snow(:,il-1,:) ) * &
               totporO2_snow(:,il,:) / time_step
          xcCH4_snow(:,il,:) = ( zf_snow(:,il,:) - zf_snow(:,il-1,:) ) * &
               totporCH4_snow(:,il,:) / time_step
          !
          xdO2_snow(:,il,:) = diffO2_snow(:,il,:) /  &
               (zi_snow(:,il+1,:)-zi_snow(:,il,:))
          xdCH4_snow(:,il,:) = diffCH4_snow(:,il,:) /  &
               (zi_snow(:,il+1,:)-zi_snow(:,il,:))
          !
       ENDWHERE
    END DO
    !
    DO il = 1,ndeep-1
       !
       WHERE ( veget_mask_2d(:,:) )
          !
          xcO2_soil(:,il,:) = ( zf_soil(il) - zf_soil(il-1) ) * &
               totporO2_soil(:,il,:) / time_step
          xcCH4_soil(:,il,:) = ( zf_soil(il) - zf_soil(il-1) ) * &
               totporCH4_soil(:,il,:) / time_step
          !
          xdO2_soil(:,il,:) = diffO2_soil(:,il,:) /  &
               (zi_soil(il+1)-zi_soil(il))
          xdCH4_soil(:,il,:) = diffCH4_soil(:,il,:) /  &
               (zi_soil(il+1)-zi_soil(il))
          !
       ENDWHERE
       !
    ENDDO
    !
    ! 1.2 for the lower boundary, define a similar geometric variable.
    !
    !snow
    !
    WHERE ( snow_height_mask_2d(:,:) .AND. veget_mask_2d(:,:) ) 
       xcO2_snow(:,nsnow,:) = ( zf_snow(:,nsnow,:) -  &
            zf_snow(:,nsnow-1,:) ) *  &
            totporO2_snow(:,nsnow,:) / time_step
       xdO2_snow(:,nsnow,:) = diffO2_snow(:,nsnow,:) /  &
            ( zi_soil(1) +  &
            zf_snow(:,nsnow,:) - zi_snow(:,nsnow,:) )
       xcCH4_snow(:,nsnow,:) = ( zf_snow(:,nsnow,:) -  &
            zf_snow(:,nsnow-1,:) ) * &
            totporCH4_snow(:,nsnow,:) / time_step
       xdCH4_snow(:,nsnow,:) = diffCH4_snow(:,nsnow,:) /  &
            ( zi_soil(1) +  &
            zf_snow(:,nsnow,:) - zi_snow(:,nsnow,:) )
    ENDWHERE
    !
    ! soil
    ! 
    WHERE (  veget_mask_2d(:,:) ) ! removed heights_soil logic
       xcO2_soil(:,ndeep,:) =  &
            ( zf_soil(ndeep) - zf_soil(ndeep-1) ) * &
            totporO2_soil(:,ndeep,:) / time_step
       xdO2_soil(:,ndeep,:) = diffO2_soil(:,ndeep,:) /  &
            ( zf_soil(ndeep) - zi_soil(ndeep) )
       xcCH4_soil(:,ndeep,:) =  &
            ( zf_soil(ndeep) - zf_soil(ndeep-1) ) * &
            totporCH4_soil(:,ndeep,:) / time_step
       xdCH4_soil(:,ndeep,:) = diffCH4_soil(:,ndeep,:) /  &
            ( zf_soil(ndeep) - zi_soil(ndeep) )
    ENDWHERE    
    !
    ! 1.3 extrapolation factor from first levels to surface
    !
    WHERE ( snow_height_mask_2d(:,:)  .AND. veget_mask_2d(:,:) )
       mu_snow(:,:) = zi_snow(:,1,:) / ( zi_snow(:,2,:) - zi_snow(:,1,:) )
    ELSEWHERE ( veget_mask_2d(:,:) )
       mu_snow(:,:) = .5 ! any value
    ENDWHERE
    !
    mu_soil = zi_soil(1) / ( zi_soil(2) - zi_soil(1) )
    !
    ! 2. bottom level: treatment depends on lower boundary condition
    !
    ! soil
    !
    WHERE ( veget_mask_2d(:,:) ) ! removed heights_soil logic
       !
       xeO2(:,:) = xcO2_soil(:,ndeep,:) + xdO2_soil(:,ndeep-1,:)
       xeCH4(:,:) = xcCH4_soil(:,ndeep,:) + xdCH4_soil(:,ndeep-1,:)
       !
       alphaO2_soil(:,ndeep-1,:) = xdO2_soil(:,ndeep-1,:) / xeO2(:,:)
       alphaCH4_soil(:,ndeep-1,:) = xdCH4_soil(:,ndeep-1,:)  &
            / xeCH4(:,:)
       !
       betaO2_soil(:,ndeep-1,:) =  &
            (xcO2_soil(:,ndeep,:)*O2_soil(:,ndeep,:))/xeO2(:,:)
       betaCH4_soil(:,ndeep-1,:) =  &
            (xcCH4_soil(:,ndeep,:)*CH4_soil(:,ndeep,:))/xeCH4(:,:)
       !
    ENDWHERE
    !
    !snow
    !
    WHERE ( snow_height_mask_2d(:,:) .AND. veget_mask_2d(:,:) )
       !
       ! dernier niveau
       !
       xeO2(:,:) = xcO2_soil(:,1,:) + &
            (1.-alphaO2_soil(:,1,:))*xdO2_soil(:,1,:) +  &
            xdO2_snow(:,nsnow,:)
       xeCH4(:,:) = xcCH4_soil(:,1,:) + &
            (1.-alphaCH4_soil(:,1,:))*xdCH4_soil(:,1,:) + &
            xdCH4_snow(:,nsnow,:)
       !
       alphaO2_snow(:,nsnow,:) = xdO2_snow(:,nsnow,:)/xeO2(:,:)
       alphaCH4_snow(:,nsnow,:) = xdCH4_snow(:,nsnow,:) &
            /xeCH4(:,:)
       !
       betaO2_snow(:,nsnow,:) =  &
            ( xcO2_soil(:,1,:)*O2_soil(:,1,:) + &
            xdO2_soil(:,1,:)*betaO2_soil(:,1,:) ) &
            / xeO2(:,:)
       betaCH4_snow(:,nsnow,:) =  &
            ( xcCH4_soil(:,1,:)*CH4_soil(:,1,:) + &
            xdCH4_soil(:,1,:)*betaCH4_soil(:,1,:) ) &
            / xeCH4(:,:)
       !
       ! avant-dernier niveau
       !
       xeO2(:,:) = xcO2_snow(:,nsnow,:) + &
            (1.-alphaO2_snow(:,nsnow,:))*xdO2_snow(:,nsnow,:) + &
            xdO2_snow(:,nsnow-1,:)
       xeCH4(:,:) = xcCH4_snow(:,nsnow,:) + &
            (1.-alphaCH4_snow(:,nsnow,:))*xdCH4_snow(:,nsnow,:) &
            + xdCH4_snow(:,nsnow-1,:)
       !
       alphaO2_snow(:,nsnow-1,:) =  &
            xdO2_snow(:,nsnow-1,:) / xeO2(:,:)
       alphaCH4_snow(:,nsnow-1,:) =  &
            xdCH4_snow(:,nsnow-1,:) / xeCH4(:,:)
       !
       betaO2_snow(:,nsnow-1,:) = &
            ( xcO2_snow(:,nsnow,:)*O2_snow(:,nsnow,:) + &
            xdO2_snow(:,nsnow,:)*betaO2_snow(:,nsnow,:) ) &
            / xeO2(:,:)
       betaCH4_snow(:,nsnow-1,:) = &
            ( xcCH4_snow(:,nsnow,:)*CH4_snow(:,nsnow,:) + &
            xdCH4_snow(:,nsnow,:)*betaCH4_snow(:,nsnow,:) ) &
            / xeCH4(:,:)
       !
    ELSEWHERE ( veget_mask_2d(:,:) )
       !
       alphaO2_snow(:,nsnow,:) = 1.
       alphaCH4_snow(:,nsnow,:) = 1.
       betaO2_snow(:,nsnow,:) = zero
       betaCH4_snow(:,nsnow,:) = zero
       !
       alphaO2_snow(:,nsnow-1,:) = 1.
       alphaCH4_snow(:,nsnow-1,:) = 1.
       betaO2_snow(:,nsnow-1,:) = zero
       betaCH4_snow(:,nsnow-1,:) = zero
       !
    ENDWHERE
    !    
            
    !
    ! 3. the other levels
    !
    DO il = nsnow-2,1,-1 !snow
       !
       WHERE ( snow_height_mask_2d(:,:) .AND. veget_mask_2d(:,:) )
          !
          xeO2(:,:) = xcO2_snow(:,il+1,:) +  &
               (1.-alphaO2_snow(:,il+1,:))*xdO2_snow(:,il+1,:) + xdO2_snow(:,il,:)
          xeCH4(:,:) = xcCH4_snow(:,il+1,:) +  &
               (1.-alphaCH4_snow(:,il+1,:))*xdCH4_snow(:,il+1,:) +  &
               xdCH4_snow(:,il,:)
          !
          alphaO2_snow(:,il,:) = xdO2_snow(:,il,:) / xeO2(:,:)
          alphaCH4_snow(:,il,:) = xdCH4_snow(:,il,:) / xeCH4(:,:)
          !
          betaO2_snow(:,il,:) =  &
               ( xcO2_snow(:,il+1,:)*O2_snow(:,il+1,:) +  &
               xdO2_snow(:,il+1,:)*betaO2_snow(:,il+1,:) ) / xeO2(:,:)
          betaCH4_snow(:,il,:) =  &
               ( xcCH4_snow(:,il+1,:)*CH4_snow(:,il+1,:) +  &
               xdCH4_snow(:,il+1,:)*betaCH4_snow(:,il+1,:) ) / xeCH4(:,:)
          !
       ELSEWHERE ( veget_mask_2d(:,:) )
          !
          alphaO2_snow(:,il,:) = 1.
          alphaCH4_snow(:,il,:) = 1.
          !
          betaO2_snow(:,il,:) = zero
          betaCH4_snow(:,il,:) = zero
          !
       ENDWHERE
       !
    ENDDO
    !
    DO il = ndeep-2,1,-1 !soil
       !
       WHERE ( veget_mask_2d(:,:) ) !removed heights_soil logic
          !
          xeO2(:,:) = xcO2_soil(:,il+1,:) +  &
               (1.-alphaO2_soil(:,il+1,:))*xdO2_soil(:,il+1,:) + xdO2_soil(:,il,:)
          xeCH4(:,:) = xcCH4_soil(:,il+1,:) +  &
               (1.-alphaCH4_soil(:,il+1,:))*xdCH4_soil(:,il+1,:) +  &
               xdCH4_soil(:,il,:)
          !
          alphaO2_soil(:,il,:) = xdO2_soil(:,il,:) / xeO2(:,:)
          alphaCH4_soil(:,il,:) = xdCH4_soil(:,il,:) / xeCH4(:,:)
          !
          betaO2_soil(:,il,:) =  &
               ( xcO2_soil(:,il+1,:)*O2_soil(:,il+1,:) +  &
               xdO2_soil(:,il+1,:)*betaO2_soil(:,il+1,:) ) / xeO2(:,:)
          betaCH4_soil(:,il,:) =  &
               ( xcCH4_soil(:,il+1,:)*CH4_soil(:,il+1,:) +  &
               xdCH4_soil(:,il+1,:)*betaCH4_soil(:,il+1,:) ) / xeCH4(:,:)
          !
       ENDWHERE
       !
    ENDDO
    !
    ! 4. store thickness of the different levels for all soil types (for security) 
    !
    zf_coeff_snow(:,:,:) = zf_snow(:,:,:)
    zi_coeff_snow(:,:,:) = zi_snow(:,:,:)

    !--hist out for keeping track of these
    IF (firstcall) THEN
       firstcall = .false.
    ELSE
    ENDIF

  END SUBROUTINE soil_gasdiff_coeff
 
!!
!================================================================================================================================
!! SUBROUTINE   : soil_gasdiff_diff
!!
!>\BRIEF        This routine update oxygen and methane in the snow and soil 
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================    
  
  SUBROUTINE soil_gasdiff_diff( kjpindex,time_step,index,pb,tsurf, O2_snow, CH4_snow, O2_soil, CH4_soil)
   
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables

    INTEGER(i_std), INTENT(in)                                 :: kjpindex             !! number of grid points 
    REAL(r_std), INTENT(in)                                    :: time_step            !! time step in seconds
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: pb                   !! Surface pressure
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: tsurf                !! Surface temperature
    INTEGER(i_std),DIMENSION(kjpindex),INTENT(in)              :: index                !! Indeces of the points on the map 
    !! 0.2  Output variables

    !! 0.3  Modified variables

    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)  :: O2_snow              !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)  :: CH4_snow             !! methane (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: O2_soil              !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: CH4_soil             !! methane (g CH4/m**3 air)

    !! 0.4 local variables
 
    INTEGER(i_std)                                             :: it, ip, il, iv
    LOGICAL, DIMENSION(kjpindex,nvm)                           :: snowtop
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: O2sa, CH4sa
    
    !
    ! 1.1 Determine which is the first existing soil type.
    !
    snowtop(:,:) = .FALSE.  
    !
    !ignore snow for now...
    WHERE ( heights_snow(:,:) .GT. hmin_tcalc )
       snowtop(:,:) = .TRUE.
    ENDWHERE
    !
    ! 2.gas diffusion
    !
    ! 2.1 top level
    !
    ! 2.1.1 non-existing
    !
    DO iv = 1, nvm
       O2sa(:,iv) = pb(:)/(RR*tsurf(:)) * O2_surf * wO2
       CH4sa(:,iv) = pb(:)/(RR*tsurf(:)) * CH4_surf * wCH4
    ENDDO
    !
    WHERE ( (.NOT. snowtop(:,:)) .AND. veget_mask_2d(:,:) ) ! it equals 1 (snow) but there is no snow...
       !
       O2_snow(:,1,:) = O2sa(:,:)
       CH4_snow(:,1,:) = CH4sa(:,:)
       !
       O2_soil(:,1,:) = ( O2sa(:,:) + mu_soil*betaO2_soil(:,1,:) ) / &
            ( 1. + mu_soil*(1.-alphaO2_soil(:,1,:)) )
       CH4_soil(:,1,:) = ( CH4sa(:,:) + mu_soil*betaCH4_soil(:,1,:) ) / &
            ( 1. + mu_soil*(1.-alphaCH4_soil(:,1,:)) )
       !
    ENDWHERE
    !
    ! 2.1.2 first existing soil type
    !
    WHERE ( snowtop(:,:) .AND. veget_mask_2d(:,:) )
       !
       O2_snow(:,1,:) = ( O2sa(:,:) + mu_snow(:,:)*betaO2_snow(:,1,:) ) / &
            ( 1. + mu_snow(:,:)*(1.-alphaO2_snow(:,1,:)) )
       CH4_snow(:,1,:) = ( CH4sa(:,:) + mu_snow(:,:)*betaCH4_snow(:,1,:) ) / &
            ( 1. + mu_snow(:,:)*(1.-alphaCH4_snow(:,1,:)) )
       !
       O2_soil(:,1,:) =  &
            alphaO2_snow(:,nsnow,:) * O2_snow(:,nsnow,:) + &
            betaO2_snow(:,nsnow,:)
       CH4_soil(:,1,:) =  &
            alphaCH4_snow(:,nsnow,:) * CH4_snow(:,nsnow,:) + &
            betaCH4_snow(:,nsnow,:)
       ! debug: need to check for weird numbers here!
    ENDWHERE
    !
    ! 2.2 other levels
    !
    DO il = 2, nsnow
       
       WHERE ( veget_mask_2d(:,:) )
          !
          O2_snow(:,il,:) =  &
               alphaO2_snow(:,il-1,:) * O2_snow(:,il-1,:) + &
               betaO2_snow(:,il-1,:)
          CH4_snow(:,il,:) =  &
               alphaCH4_snow(:,il-1,:) * CH4_snow(:,il-1,:) + &
               betaCH4_snow(:,il-1,:)
       END WHERE
    ENDDO
    DO il = 2, ndeep
       
       WHERE ( veget_mask_2d(:,:)  )
          !
          O2_soil(:,il,:) =  &
               alphaO2_soil(:,il-1,:) * O2_soil(:,il-1,:) + &
               betaO2_soil(:,il-1,:)
          CH4_soil(:,il,:) =  &
               alphaCH4_soil(:,il-1,:) * CH4_soil(:,il-1,:) + &
               betaCH4_soil(:,il-1,:)
       END WHERE
    ENDDO

  END SUBROUTINE soil_gasdiff_diff
 
!!
!================================================================================================================================
!! SUBROUTINE   : get_gasdiff
!!
!>\BRIEF        This routine update oxygen and methane in the snow and soil 
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================    
  SUBROUTINE get_gasdiff (kjpindex,hslong,tprof,snow,airvol_snow, &
       totporO2_snow,totporCH4_snow,diffO2_snow,diffCH4_snow, &
       airvol_soil,totporO2_soil,totporCH4_soil,diffO2_soil,diffCH4_soil, z_organic, snowrho)
   
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables

    INTEGER(i_std), INTENT(in)                                 :: kjpindex          !! number of grid points 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: hslong            !! deep long term soil humidity profile 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: tprof             !! Soil temperature (K)      
    REAL(r_std), DIMENSION(kjpindex,nsnow), INTENT(in)         :: snowrho           !! snow density 
    REAL(r_std), DIMENSION(kjpindex),  INTENT (in)             :: snow              !! Snow mass [Kg/m^2]
    REAL(r_std), DIMENSION(kjpindex),   INTENT (in)            :: z_organic         !! depth to organic soil

    !! 0.2  Output variables

    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(out)    :: airvol_soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(out)    :: totporO2_soil     !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(out)    :: totporCH4_soil    !! total CH4 porosity 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(out)    :: diffO2_soil       !! oxygen diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(out)    :: diffCH4_soil      !! methane diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(out)    :: airvol_snow
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(out)    :: totporO2_snow     !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(out)    :: totporCH4_snow    !! total CH4 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(out)    :: diffO2_snow       !! oxygen diffusivity (m**2/s)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(out)    :: diffCH4_snow      !! methane diffusivity (m**2/s)
   
    !! 0.3  Modified variables

    !! 0.4 local variables
 
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                 :: density_snow
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                 :: porosity_snow
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                 :: tortuosity_snow
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: density_soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: porosity_soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: tortuosity_soil
    INTEGER(i_std)                                             :: it,ip, il, iv
    REAL(r_std)                                                :: x, rho_iw
    REAL(r_std)                                                :: csat, fng
    REAL(r_std),  SAVE                                         :: cond_fact 
    LOGICAL, SAVE                                              :: pr_fois=.TRUE.
    
    IF (pr_fois) THEN
       cond_fact=1.
       CALL getin_p('COND_FACT',cond_fact) 
        WRITE(*,*) 'COND_FACT=',cond_fact
       pr_fois=.FALSE.
    ENDIF
    
    !
    ! 1. Three-layers snow model with snow density resolved at each snow layer
    !
    DO iv = 1, nvm 
       density_snow(:,:,iv) = snowrho(:,:)
    ENDDO
    porosity_snow(:,:,:) = (1. - density_snow(:,:,:)/rho_ice )
    tortuosity_snow(:,:,:) = porosity_snow(:,:,:)**(1./3.)     ! based on Sommerfeld et al., GBC, 1996
    diffO2_snow(:,:,:) = diffO2_air * porosity_snow(:,:,:) * tortuosity_snow(:,:,:)
    diffCH4_snow(:,:,:) = diffCH4_air * porosity_snow(:,:,:) * tortuosity_snow(:,:,:)
    airvol_snow(:,:,:) = MAX(porosity_snow(:,:,:),avm)
    totporO2_snow(:,:,:) = airvol_snow(:,:,:)
    totporCH4_snow(:,:,:) = airvol_snow(:,:,:)
    !
    ! 2. soil: depends on temperature and soil humidity
    !
    DO ip = 1, kjpindex
       !
       DO iv = 1, nvm
          !
          IF ( veget_mask_2d(ip,iv) ) THEN
             !
             DO il = 1, ndeep
                !
                ! 2.1 soil dry density, porosity, and dry heat capacity
                !
                porosity_soil(ip,il,iv) = tetasat
                !
                !
                ! 2.2 heat capacity and density as a function of
                !     ice and water content
                !  removed these as we are calculating thermal evolution in the sechiba subroutines
               
                !
                ! 2.3 oxygen diffusivity: soil can get waterlogged,
                !     therefore take soil humidity into account
                !
                tortuosity_soil(ip,il,iv) = 2./3. ! Hillel, 1980
                airvol_soil(ip,il,iv) = porosity_soil(ip,il,iv)*(1.-hslong(ip,il,iv))  
                totporO2_soil(ip,il,iv) = airvol_soil(ip,il,iv) + porosity_soil(ip,il,iv)*BunsenO2*hslong(ip,il,iv)  
                totporCH4_soil(ip,il,iv) = airvol_soil(ip,il,iv) + porosity_soil(ip,il,iv)*BunsenCH4*hslong(ip,il,iv)  
                diffO2_soil(ip,il,iv) = (diffO2_air*airvol_soil(ip,il,iv) + & 
                     diffO2_w*BunsenO2*hslong(ip,il,iv)*porosity_soil(ip,il,iv))*tortuosity_soil(ip,il,iv)  
                diffCH4_soil(ip,il,iv) = (diffCH4_air*airvol_soil(ip,il,iv) + & 
                     diffCH4_w*BunsenCH4*hslong(ip,il,iv)*porosity_soil(ip,il,iv))*tortuosity_soil(ip,il,iv)  
                !
          END DO
       ELSE
          tortuosity_soil(ip,:,iv) = EPSILON(0.)
          airvol_soil(ip,:,iv) =  EPSILON(0.)
          totporO2_soil(ip,:,iv) =  EPSILON(0.)
          totporCH4_soil(ip,:,iv) = EPSILON(0.)
          diffO2_soil(ip,:,iv) = EPSILON(0.)
          diffCH4_soil(ip,:,iv) =  EPSILON(0.)
       END IF
    ENDDO
 ENDDO

END SUBROUTINE get_gasdiff
  
!!
!================================================================================================================================
!! SUBROUTINE   : traMplan
!!
!>\BRIEF        This routine calculates plant-mediated transport of methane
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================    
  SUBROUTINE traMplan(CH4,O2,kjpindex,time_step,totporCH4,totporO2,z_root,rootlev,Tgr,Tref,hslong,flupmt, &
       refdep, zi_soil, tprof)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables
    
    INTEGER(i_std), INTENT(in)			                  :: kjpindex    
    REAL(r_std), INTENT(in)                                       :: time_step      !! time step in seconds
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)        :: totporO2       !! total oxygen porosity
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)        :: totporCH4      !! total methane porosity
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),INTENT(in)         :: tprof          !! soil temperature (K)
    INTEGER(i_std),DIMENSION(kjpindex,nvm),INTENT(in)		  :: rootlev        !! the deepest model level within the rooting depth 
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)		  :: z_root         !! the rooting depth
    REAL(r_std), INTENT(in) 					  :: Tgr            !! Temperature at which plants begin to grow (C)
    REAL(r_std), DIMENSION(ndeep), INTENT(in)                     :: zi_soil        !!  depths at intermediate levels 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)        :: hslong         !! deep soil humidity

    !! 0.2 Output variables

    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(out)             :: flupmt         !! plant-mediated methane flux (g m-2 s-1)

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(inout)            :: Tref           !! Ref. temperature for growing season caluculation (C)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)     :: O2
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)     :: CH4

    !! 0.4 local variables
    REAL(r_std), DIMENSION(kjpindex,nvm)   			  :: CH4atm         !! CH4 atm concentration
    REAL(r_std), DIMENSION(kjpindex,nvm)                          :: dCH4           !! delta CH4 per m3 air
    REAL(r_std), DIMENSION(kjpindex,nvm)   			  :: dO2            !! O2 change
    REAL(r_std), DIMENSION(kjpindex,nvm)	       		  :: fgrow          !! Plant growing state (maturity index)
    REAL(r_std)                            			  :: froot          !! vertical distribution of roots
    REAL(r_std)					                  :: Tmat           !! Temperature at which plants reach maturity (C)
    REAL(r_std), PARAMETER 				          :: La_min = zero
    REAL(r_std), PARAMETER                                        :: La = 4.
    REAL(r_std), PARAMETER 				          :: La_max = La_min + La
    REAL(r_std), PARAMETER 				          :: Tveg = 10      !! Vegetation type control on the plant-mediated transport, Adjustable parameter,
                                                                                    !! but we start from 10 following Walter et al (2001) tundra value 
    REAL(r_std), PARAMETER 				          :: Pox = 0.5      !! fraction of methane oxydized near the roots
    LOGICAL, SAVE 				                  :: firstcall=.TRUE.
    INTEGER(i_std)       				          :: il,ip, iv
    LOGICAL, SAVE			                          :: check = .FALSE.
    REAL(r_std), INTENT(in)			                  :: refdep         !! Depth to compute reference temperature for the growing season (m)
    INTEGER(i_std), SAVE				          :: reflev = 0     !! Level closest to reference depth refdep
   

    IF (firstcall) THEN
       firstcall = .FALSE.

       ! Find the level closest to refdep
       DO il=1,ndeep
          IF (zi_soil(il) .GT. refdep .AND. reflev.EQ.0) reflev = il-1
       ENDDO
       IF (reflev.EQ.0) reflev = ndeep


       IF (check) THEN
          OPEN (28,file='pmt.dat',status='unknown') 
          OPEN (29,file='pmtf.dat',status='unknown')
       ENDIF
    ENDIF

     ! Update seasonal reference temperature trace record  
     WHERE ( veget_mask_2d(:,:) )          
        Tref(:,:) = tprof(:,reflev,:) - ZeroCelsius
     END WHERE

    Tmat = Tgr + 10._r_std
    flupmt(:,:) = zero
    CH4atm(:,:) = zero  


    ! Plant growing state (maturity index)
    WHERE (Tref(:,:).LE.Tgr .AND. veget_mask_2d(:,:) )
       fgrow(:,:) = La_min
    ELSEWHERE (Tref(:,:).GE.Tmat .AND. veget_mask_2d(:,:) )
       fgrow(:,:) = La_max
    ELSEWHERE   ( veget_mask_2d(:,:))
       fgrow(:,:) = La_min + La * (1 - ((Tmat - Tref(:,:))/(Tmat - Tgr))**2)
    ENDWHERE

    DO ip=1,kjpindex
       DO iv = 1, nvm
          IF ( (z_root(ip,iv) .GT. 0.) .AND. veget_mask_2d(ip,iv) ) THEN ! added this to prevent pmt calcs when soil frozen
             DO il=1,rootlev(ip,iv)
                ! vertical distribution of roots
                froot = MAX( 2 * (z_root(ip,iv) - REAL( zi_soil(il) )) / z_root(ip,iv), zero) 
                ! Methane removal from a given depth. We assume that the methane
                ! in air pores is always in equilibrium with that dissolved 
                ! in water-filled pores. If soil humidity is low,
                ! with root water as well
                ! We assume that PMT is proportional to soil humidity
                dCH4(ip,iv) = 0.01_r_std * Tveg * froot * fgrow(ip,iv) * hslong(ip,il,iv) * (CH4(ip,il,iv) - CH4atm(ip,iv))  
                ! No transport if soil concentration is less than atmospheric
                IF (dCH4(ip,iv).LT.CH4atm(ip,iv)) dCH4(ip,iv) = zero 
                ! Strange thing in WH 2001: 0.01*Tveg*froot*fgrow > 1
                ! at Tveg=15, froot&fgrow=max, i.e. more CH4 is taken than available
                ! So need to impose a limitation:
                IF (dCH4(ip,iv).GT.CH4(ip,il,iv)) dCH4(ip,iv) = CH4(ip,il,iv)
                ! Methane concentration is decreased within the root layer:
                
                CH4(ip,il,iv) = CH4(ip,il,iv) - dCH4(ip,iv)
                ! O2 concentration is decreased in reaction with
                ! dCH4*Pox*time_step
                dO2(ip,iv) = dCH4(ip,iv)*Pox * wO2/wCH4 * totporCH4(ip,il,iv)/totporO2(ip,il,iv)
                IF ( dO2(ip,iv).LT.O2(ip,il,iv) ) O2(ip,il,iv) = O2(ip,il,iv) - dO2(ip,iv)
                
                ! CO2 concentration is increased by dCH4(:)*Pox
                
                ! Integration	 
                flupmt(ip,iv) = flupmt(ip,iv) + dCH4(ip,iv)*totporCH4(ip,il,iv)/time_step * (1 - Pox) * &
                     ( zf_soil(il) - zf_soil(il-1) )
             ENDDO
          END IF
       ENDDO
    ENDDO
    
    IF (check) THEN
       WRITE(29,*) flupmt(:,:)
       CALL flush(28) 
       CALL flush(29) 
    END IF
    
  END SUBROUTINE traMplan
  
!!
!================================================================================================================================
!! SUBROUTINE   : ebullition
!!
!>\BRIEF        This routine calculates CH4 ebullition
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
  SUBROUTINE ebullition (kjpindex,time_step,tprof,totporCH4_soil,hslong,Ch4_soil,febul)
   
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables 

    INTEGER(i_std), INTENT(in)                                  :: kjpindex
    REAL(r_std), INTENT(in)                                     :: time_step      !! time step in seconds
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),INTENT(in)       :: tprof          !! soil temperature (K)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)      :: totporCH4_soil !! total methane porosity
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)      :: hslong         !! deep soil humidity

    !! 0.2 Output variables
    
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(out)           :: febul          !! CH4 ebullition

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)   :: Ch4_soil       !! methane 

    !! 0.4 Local variables
    REAL(r_std)                                                 :: dCH4, CH4d
    INTEGER(i_std)                                              :: ip, il, iv
    REAL(r_std)                                                 :: dz
    REAL(r_std), PARAMETER                                      :: tortuosity=2./3. 
    REAL(r_std), PARAMETER                                      :: wsize=0.01
    REAL(r_std), PARAMETER                                      :: CH4wm = 12.E-3 !! CH4 concentration threshold for ebullition (8-16 mg/m3 in Walter&Heimann 2000)
    REAL(r_std)                                                 :: hum

    DO ip=1,kjpindex
       DO iv = 1, nvm
          IF ( veget_mask_2d(ip,iv) ) THEN
             febul(ip,iv) = zero
             IF (hslong(ip,1,iv).GT.ebuthr) THEN
                DO il = ndeep, 1, -1
                   CH4d = Ch4_soil(ip,il,iv) - CH4wm/BunsenCH4
                   IF (CH4d .GT. EPSILON(0.)) THEN  
                      IF (il.GT.1) THEN
                         dz = zi_soil(il) - zi_soil(il-1)
                         hum = ( hslong(ip,il,iv) + hslong(ip,il-1,iv) ) / 2 
                      ELSE
                         dz = zi_soil(1)
                         hum = hslong(ip,1,iv)
                      ENDIF
                      
                      dCH4 = hum**( dz/wsize/tortuosity ) * CH4d
                      dCH4 = CH4d 
                      
                      Ch4_soil(ip,il,iv) = Ch4_soil(ip,il,iv) - dCH4
                      
                   
                      febul(ip,iv) = febul(ip,iv) + dCH4 * totporCH4_soil(ip,il,iv) *  &
                           ( zf_soil(il) - zf_soil(il-1) ) / time_step
                      
                   ENDIF 
                ENDDO
             ENDIF 
          END IF
       ENDDO
    ENDDO
  END SUBROUTINE ebullition
  
!!
!================================================================================================================================
!! SUBROUTINE   : microactem
!!
!>\BRIEF        This routine calculates parameters describing bacterial activity (time constant tau[s]) as a function of temperature
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
  FUNCTION microactem ( temp, frozen_respiration_func, moist_in, i_ind, j_ind, k_ind ) RESULT ( fbact )

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables 
    
    INTEGER(i_std), INTENT(in)                        :: i_ind
    INTEGER(i_std), INTENT(in)                        :: j_ind
    INTEGER(i_std), INTENT(in)                        :: k_ind
    INTEGER(i_std), INTENT(in)                        :: frozen_respiration_func
    REAL, DIMENSION(i_ind, j_ind, k_ind), INTENT(in)  :: moist_in
    REAL, DIMENSION(i_ind, j_ind, k_ind), INTENT(in)  :: temp

    !! 0.2 Output variables 

    !! 0.3 Modified variables

    !! 0.4 Local variables

    REAL, DIMENSION(i_ind, j_ind, k_ind)              :: fbact
    REAL, DIMENSION(i_ind, j_ind, k_ind)              :: tempfunc_result
    REAL, DIMENSION(i_ind, j_ind, k_ind)              :: temp_kelvin
    INTEGER(i_std), PARAMETER                         :: ntconfun = 7
    REAL(r_std), DIMENSION(ntconfun)                  :: tconfun
    REAL(r_std), DIMENSION(ntconfun)                  :: tauconfun
    INTEGER                                           :: itz
    INTEGER                                           :: ii, ij, ik
    REAL, DIMENSION(i_ind, j_ind, k_ind)              :: moistfunc_result
    REAL(r_std), parameter                            :: q10 = 2.0
    REAL(r_std), PARAMETER                            :: stomate_tau = 4.699E6  !4.7304E7 !4.699E6  
    logical, parameter                                :: limit_decomp_moisture = .true.

    temp_kelvin(:,:,:) = temp(:,:,:) + ZeroCelsius
    SELECT CASE(frozen_respiration_func)

    CASE(0) ! this is the standard ORCHIDEE state

       tempfunc_result(:,:,:) = EXP( log(q10) * ( temp_kelvin(:,:,:) - (ZeroCelsius+30.) ) / 10. )
       tempfunc_result(:,:,:) = MIN( 1._r_std, tempfunc_result(:,:,:) )

    CASE(1)  ! cutoff respiration when T < -1C
       WHERE (temp_kelvin(:,:,:) .GT. ZeroCelsius ) ! normal as above
          tempfunc_result(:,:,:) = EXP( log(q10) * ( temp_kelvin(:,:,:) - (ZeroCelsius+30.) ) / 10. )
       ELSEWHERE (temp_kelvin(:,:,:) .GT. ZeroCelsius - 1. )  ! linear dropoff to zero
          tempfunc_result(:,:,:) = (temp_kelvin(:,:,:) - (ZeroCelsius - 1.)) * &
               EXP( log(q10) * ( ZeroCelsius - (ZeroCelsius+30.) ) / 10. )
       ELSEWHERE  ! zero
          tempfunc_result(:,:,:) = EPSILON(0.)
       endwhere

       tempfunc_result(:,:,:) = MAX(MIN( 1._r_std, tempfunc_result(:,:,:) ), EPSILON(0.))

    CASE(2)  ! cutoff respiration when T < -3C
       WHERE (temp_kelvin(:,:,:) .GT. ZeroCelsius ) ! normal as above
          tempfunc_result(:,:,:) = EXP( log(q10) * ( temp_kelvin(:,:,:) - (ZeroCelsius+30.) ) / 10. )
       ELSEWHERE (temp_kelvin(:,:,:) .GT. ZeroCelsius - 3. )  ! linear dropoff to zero
          tempfunc_result(:,:,:) = ((temp_kelvin(:,:,:) - (ZeroCelsius - 3.))/3.) * &
               EXP( log(q10) * ( ZeroCelsius - (ZeroCelsius+30.) ) / 10. )
       ELSEWHERE  ! zero
          tempfunc_result(:,:,:) = EPSILON(0.)
       endwhere

    CASE(3)  ! q10 = 100 when below zero
       WHERE (temp_kelvin(:,:,:) .GT. ZeroCelsius ) ! normal as above
          tempfunc_result(:,:,:) = EXP( log(q10) * ( temp_kelvin(:,:,:) - (ZeroCelsius+30.) ) / 10. )
       ELSEWHERE 
          tempfunc_result(:,:,:) = EXP( log(100.) * ( temp_kelvin(:,:,:) - (ZeroCelsius) ) / 10. ) * &
               EXP( log(q10) * ( -30. ) / 10. )
       endwhere

    CASE(4)  ! q10 = 1000 when below zero
       WHERE (temp_kelvin(:,:,:) .GT. ZeroCelsius ) ! normal as above
          tempfunc_result(:,:,:) = EXP( log(q10) * ( temp_kelvin(:,:,:) - (ZeroCelsius+30.) ) / 10. )
       ELSEWHERE 
          tempfunc_result(:,:,:) = EXP( log(1000.) * ( temp_kelvin(:,:,:) - (ZeroCelsius) ) / 10. ) * &
               EXP( log(q10) * ( -30. ) / 10. )
       endwhere

    CASE DEFAULT
       WRITE(*,*) 'microactem ERROR: frozen_respiration_func not in list: ', frozen_respiration_func
       STOP

    END SELECT
    tempfunc_result(:,:,:) = MAX(MIN( 1._r_std, tempfunc_result(:,:,:) ), EPSILON(0.))

    !---- stomate residence times: -----!
    ! residence times in carbon pools (days)
    !carbon_tau(iactive) = .149 * one_year        !!!!???? 1.5 years
    !carbon_tau(islow) = 5.48 * one_year          !!!!???? 25 years
    !carbon_tau(ipassive) = 241. * one_year       !!!!???? 1000 years
    !-----------------------------------!
    IF ( limit_decomp_moisture ) THEN
       ! stomate moisture control function
       moistfunc_result(:,:,:) = -1.1 * moist_in(:,:,:) * moist_in(:,:,:) + 2.4 * moist_in(:,:,:) - 0.29
       moistfunc_result(:,:,:) = max( 0.25_r_std, min( 1._r_std, moistfunc_result(:,:,:) ) )
    ELSE
       moistfunc_result(:,:,:) = 1._r_std
    ENDIF

    fbact(:,:,:) = stomate_tau/(moistfunc_result(:,:,:) * tempfunc_result(:,:,:))

    
  END FUNCTION microactem
  
  
!!
!================================================================================================================================
!! SUBROUTINE   : snowlevels
!!
!>\BRIEF        This routine calculates depths of full levels and intermediate
!!              levels related to snow pack
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
 
  SUBROUTINE snowlevels( kjpindex, snowdz, zi_snow, zf_snow, veget_max )

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     

    INTEGER(i_std), INTENT(in)                                          :: kjpindex
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)                     :: veget_max     !! maximum vegetation fraction
    REAL(r_std), DIMENSION(kjpindex,nsnow),INTENT(in)                   :: snowdz        !! snow depth

    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex,0:nsnow,nvm), INTENT(inout)         :: zf_snow       !! depths of full levels (m)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)           :: zi_snow       !! depths of intermediate levels (m)

    !! 0.4 Local variables

    REAL(r_std), DIMENSION(kjpindex,nvm)                                :: z_alpha       !! parameter of the geometric series
    INTEGER(i_std)                                                      :: il,it, ix, iv
    INTEGER(i_std)                                                      :: it_beg,it_end
    INTEGER(i_std), PARAMETER                                           :: niter = 10
    REAL(r_std), DIMENSION(kjpindex)                                    :: dxmin
    INTEGER(i_std), DIMENSION(kjpindex)                                 :: imin
    INTEGER(i_std)                                                      :: i,j
    REAL(r_std), DIMENSION(kjpindex,nvm)                                :: xi, xf
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                          :: snowdz_pft 

    snowdz_pft(:,:,:) = 0.0 
    DO il = 1,nsnow 
       DO iv = 1, nvm
          WHERE ( veget_mask_2d(:,iv) ) 
                snowdz_pft(:,il,iv) = snowdz(:,il)
          ENDWHERE
       ENDDO 
    ENDDO
    !
    ! calculate snow discretisation
    !
    WHERE ( veget_mask_2d(:,:) )
       zf_snow(:,0,:) = 0.
    END WHERE
    !
    DO il = 1, nsnow
       IF ( il .EQ. 1 ) THEN
          WHERE ( veget_mask_2d(:,:) )
             
             zi_snow(:,il,:) = snowdz_pft(:,1,:) / 2. 
             
             zf_snow(:,il,:) = snowdz_pft(:,1,:)
        
          END WHERE
       ENDIF

       IF ( il .GT. 1 ) THEN
          WHERE ( veget_mask_2d(:,:) )
             
             zi_snow(:,il,:) = zf_snow(:,il-1,:) + snowdz_pft(:,il,:) / 2 
             
             zf_snow(:,il,:) = SUM(snowdz_pft(:,1:il,:),2)

          END WHERE
       ENDIF

    ENDDO
    
    DO ix = 1, kjpindex
       DO il = 1, nsnow
          zi_snow_nopftdim(ix,il) = SUM(zi_snow(ix,il,:)*veget_max(ix,:))
          zf_snow_nopftdim(ix,il) = SUM(zf_snow(ix,il,:)*veget_max(ix,:))
       END DO
    END DO
    
  END SUBROUTINE snowlevels
 
!!
!================================================================================================================================
!! SUBROUTINE   : snow_interpol
!!
!>\BRIEF        This routine interpolates oxygen and methane into snow layers
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
 
  SUBROUTINE snow_interpol (kjpindex,snowO2, snowCH4, zi_snow, zf_snow, veget_max, snowdz)
    
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     

    INTEGER(i_std), INTENT(in)                                  :: kjpindex
    REAL(r_std), DIMENSION(kjpindex,nsnow), INTENT(in)          :: snowdz       !! snow depth at each layer
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)             :: veget_max    !! maximum vegetation fraction                                                           

    !! 0.2 Output variables                                     
                                                                
    !! 0.3 Modified variables                                   

    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)   :: snowO2       !! snow oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)   :: snowCH4      !! snow methane (g CH4/m**3 air), needed just for num. scheme
    REAL(r_std), DIMENSION(kjpindex,0:nsnow,nvm), INTENT(inout) :: zf_snow      !! depths at full levels
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm), INTENT(inout)   :: zi_snow      !! depths at intermediate levels

    !! 0.4 Local variables
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                  :: isnow        !! index of first old layer that is deeper
    INTEGER(i_std), DIMENSION(kjpindex,nsnow,nvm)               :: i1,i2        !! indices of the layers used for the inter- or extrapolation
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                  :: snowO2o      !! initial snow oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                  :: snowCH4o     !! initial snow methane (g CH4/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,nvm)                        :: dzio         !! initial distance between two levels
    INTEGER(i_std)                                      :: il, it, ip, ill, iv  !! indices
    REAL(r_std), DIMENSION(kjpindex,0:nsnow,nvm)              :: zfo            !! initial depths at full levels
    REAL(r_std), DIMENSION(kjpindex,nsnow,nvm)                :: zio            !! initial depths at intermediate levels    
    
    
    
    ! 1. save old discretisation and temperatures
    
    zio(:,:,:) = zi_snow(:,:,:)
    
    zfo(:,:,:) = zf_snow(:,:,:)
    
    snowO2o(:,:,:) = snowO2(:,:,:)
    snowCH4o(:,:,:) = snowCH4(:,:,:)
    
    ! 2. new discretisation
    
    CALL snowlevels( kjpindex, snowdz, zi_snow, zf_snow, veget_max)
    
    ! 3. for each new intermediate layer, look for the first old intermediate 
    !   layer that is deeper
    
    DO il = 1, nsnow
       
       isnow(:,il,:) = -1
       
       DO ill = nsnow,1,-1
          
          WHERE ( zio(:,ill,:) .GT. zi_snow(:,il,:) .AND. veget_mask_2d(:,:) )
             
             isnow(:,il,:) = ill
             
          ENDWHERE
          
       ENDDO
       
    ENDDO
   
    ! 4. determine which levels to take for the inter- or extrapolation
    

    DO ip = 1, kjpindex
       DO iv = 1, nvm
          IF ( veget_mask_2d(ip,iv) ) THEN
             DO il = 1, nsnow
                !
                IF ( isnow(ip,il,iv) .EQ. 1  ) THEN
                   !
                   ! 4.1 first old layer is below new layer:
                   !       extrapolation from layers 1 and 2
                   !
                   i1(ip,il,iv) = 1
                   i2(ip,il,iv) = 2
                   !
                ELSEIF ( isnow(ip,il,iv) .EQ. -1 ) THEN
                   !
                   ! 4.2 new layer is below last old layer:
                   !       extrapolation from layers nsnow-1 and nsnow
                   !
                   i1(ip,il,iv) = nsnow-1
                   i2(ip,il,iv) = nsnow
                   !
                ELSE
                   !
                   ! 4.3 new layer is between two old layers: interpolation
                   !
                   i1(ip,il,iv) = isnow(ip,il,iv)-1
                   i2(ip,il,iv) = isnow(ip,il,iv)
                   !
                ENDIF
                
             ENDDO
          ENDIF
       ENDDO
    ENDDO
    
    ! 5. inter- or extrapolate
    
    DO ip = 1, kjpindex
       DO iv = 1, nvm
          IF ( veget_mask_2d(ip,iv) ) THEN
             DO il = 1, nsnow
                dzio(ip,iv) = zio(ip,i2(ip,il,iv),iv) - zio(ip,i1(ip,il,iv),iv)
                
                IF ( dzio(ip,iv) .GT. min_stomate ) THEN
                   
                   snowO2(ip,il,iv) =  snowO2o(ip,i1(ip,il,iv),iv) + &
                        ( zi_snow(ip,il,iv) - zio(ip,i1(ip,il,iv),iv) ) / dzio(ip,iv) * &
                        ( snowO2o(ip,i2(ip,il,iv),iv) - snowO2o(ip,i1(ip,il,iv),iv)  )
                   snowCH4(ip,il,iv) =  snowCH4o(ip,i1(ip,il,iv),iv) + &
                        ( zi_snow(ip,il,iv) - zio(ip,i1(ip,il,iv),iv) ) / dzio(ip,iv) * &
                        ( snowCH4o(ip,i2(ip,il,iv),iv) - snowCH4o(ip,i1(ip,il,iv),iv)  )
                   
                ELSE
                   
                   snowO2(ip,il,iv) = snowO2o(ip,i1(ip,il,iv),iv) 
                   snowCH4(ip,il,iv) = snowCH4o(ip,i1(ip,il,iv),iv) 
                   
                ENDIF
                
             ENDDO
          ENDIF
       ENDDO
       
    ENDDO
  END SUBROUTINE snow_interpol
 
!!
!================================================================================================================================
!! SUBROUTINE   : permafrost_carbon_clear
!!
!>\BRIEF        
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
  SUBROUTINE permafrost_carbon_clear()
    IF (ALLOCATED(veget_mask_2d)) DEALLOCATE(veget_mask_2d)
    IF (ALLOCATED(heights_snow)) DEALLOCATE(heights_snow)
    IF (ALLOCATED(zf_soil)) DEALLOCATE(zf_soil)
    IF (ALLOCATED(zi_soil)) DEALLOCATE(zi_soil)
    IF (ALLOCATED(zf_snow)) DEALLOCATE(zf_snow)
    IF (ALLOCATED(zi_snow)) DEALLOCATE(zi_snow)
    IF (ALLOCATED(alphaO2_soil )) DEALLOCATE(alphaO2_soil )
    IF (ALLOCATED(betaO2_soil )) DEALLOCATE(betaO2_soil )
    IF (ALLOCATED(alphaCH4_soil )) DEALLOCATE(alphaCH4_soil )
    IF (ALLOCATED(betaCH4_soil )) DEALLOCATE(betaCH4_soil )
    IF (ALLOCATED(alphaO2_snow )) DEALLOCATE(alphaO2_snow )
    IF (ALLOCATED(betaO2_snow )) DEALLOCATE(betaO2_snow )
    IF (ALLOCATED(alphaCH4_snow )) DEALLOCATE(alphaCH4_snow )
    IF (ALLOCATED(betaCH4_snow )) DEALLOCATE(betaCH4_snow )
    IF (ALLOCATED(zf_coeff_snow )) DEALLOCATE(zf_coeff_snow )
    IF (ALLOCATED(zi_coeff_snow )) DEALLOCATE(zi_coeff_snow )
    IF (ALLOCATED(mu_snow )) DEALLOCATE(mu_snow )
    IF (ALLOCATED(deepc_pftmean )) DEALLOCATE(deepc_pftmean )
    
  END SUBROUTINE permafrost_carbon_clear

!!
!================================================================================================================================
!! SUBROUTINE   : initialize_yedoma_carbonstocks
!!
!>\BRIEF        This routine intialize soil carbon in yedoma region
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     

  SUBROUTINE initialize_yedoma_carbonstocks(kjpindex, lalo, soilc_a, soilc_s, soilc_p, zz_deep, &
       yedoma_map_filename, yedoma_depth, yedoma_cinit_act, yedoma_cinit_slo, yedoma_cinit_pas, altmax_ind)
   
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     
 
    INTEGER(i_std), INTENT(in)                                       :: kjpindex            !! domain size
    REAL(r_std), DIMENSION(kjpindex,2), INTENT(in)                   :: lalo                !! geographic lat/lon
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)                     :: zz_deep             !! deep vertical profile
    CHARACTER(LEN=80), INTENT (in)                                   :: yedoma_map_filename !! yedoma map
    REAL(r_std), INTENT(in)                                          :: yedoma_depth        !! depth of yedoma carbon stock
    REAL(r_std), INTENT(in)                                          :: yedoma_cinit_act    !! initial active soil C concentration 
    REAL(r_std), INTENT(in)                                          :: yedoma_cinit_slo    !! initial slow soil C concentration 
    REAL(r_std), INTENT(in)                                          :: yedoma_cinit_pas    !! initial passive soil C concentration 
    INTEGER(i_std), DIMENSION(kjpindex,nvm),INTENT(in)	             :: altmax_ind          !! Maximum over the year active-layer index

    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)        :: soilc_a             !! active soil C concentration
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)        :: soilc_s             !! slow soil C concentration
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)        :: soilc_p             !! passive soil C concentration

    !! 0.4 Local variables
    REAL(r_std), DIMENSION(kjpindex)                                 :: yedoma
    INTEGER(i_std)                                                   :: il, ils, ip, ix, iy, imin, jmin, ier, iv
    REAL(r_std)                                                      :: dlon, dlonmin, dlat, dlatmin
    INTEGER(i_std)                                                   :: iml, jml, lml, tml, fid
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:)                           :: xx,yy, yedoma_file
    REAL(r_std),ALLOCATABLE,DIMENSION(:)                             :: x,y
    REAL(r_std)                                                      :: lev(1), date, dt
    INTEGER(i_std)                                                   :: itau(1)
    INTEGER(i_std)                                                   :: yedoma_depth_index, iz
    
    ! plus bas, on prend la temperature lue dans un fichier climato si celui-ci existe

    IF ( yedoma_map_filename .EQ. "NONE" ) THEN
       yedoma(:) = zero
    ELSE IF ( yedoma_map_filename .EQ. "EVERYWHERE" ) THEN
       yedoma(:) = 1.
    ELSE
       CALL flininfo(yedoma_map_filename,iml, jml, lml, tml, fid)

       ALLOCATE (yy(iml,jml),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in yy allocation. We stop. We need ',iml,' fois ',jml,' words = '&
              & , iml*jml
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (xx(iml,jml),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in xx allocation. We stop. We need ',iml,'fois ',jml,' words = '&
              & , iml*jml
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (x(iml),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in x allocation. We stop. We need',iml,' words = '&
              & , iml
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (y(jml),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in y allocation. We stop. We need',jml,'words = '&
              & , jml
           STOP 'deep_carbcycle'
       END IF

       ALLOCATE (yedoma_file(iml,jml),stat=ier)
       IF (ier.NE.0) THEN
           WRITE (numout,*) ' error in yedoma_file allocation. We stop. We need ',iml,'fois ',jml,' words = '&
              & , iml*jml
           STOP 'deep_carbcycle'
       END IF

       CALL flinopen (yedoma_map_filename, .FALSE., iml, jml, lml, &
            xx, yy, lev, tml, itau, date, dt, fid)
       CALL flinget (fid, 'yedoma', iml, jml, lml, tml, &
            1, 1, yedoma_file)
       CALL flinclo (fid)
       ! On suppose que le fichier est regulier.
       ! Si ce n'est pas le cas, tant pis. Les temperatures seront mal
       ! initialisees et puis voila. De toute maniere, il faut avoir
       ! l'esprit mal tourne pour avoir l'idee de faire un fichier de
       ! climatologie avec une grille non reguliere.
       x(:) = xx(:,1)
       y(:) = yy(1,:)
       ! prendre la valeur la plus proche
       DO ip = 1, kjpindex
          dlonmin = HUGE(1.)
          DO ix = 1,iml
             dlon = MIN( ABS(lalo(ip,2)-x(ix)), ABS(lalo(ip,2)+360.-x(ix)), ABS(lalo(ip,2)-360.-x(ix)) )
             IF ( dlon .LT. dlonmin ) THEN
                imin = ix
                dlonmin = dlon
             ENDIF
          ENDDO
          dlatmin = HUGE(1.)
          DO iy = 1,jml
             dlat = ABS(lalo(ip,1)-y(iy))
             IF ( dlat .LT. dlatmin ) THEN
                jmin = iy
                dlatmin = dlat
             ENDIF
          ENDDO
          yedoma(ip) = yedoma_file(imin,jmin)
       ENDDO
       DEALLOCATE (yy)
       DEALLOCATE (xx)
       DEALLOCATE (x)
       DEALLOCATE (y)
       DEALLOCATE (yedoma_file)
    ENDIF
    
    yedoma_depth_index = 0
    DO iz = 1, ndeep
       IF (zz_deep(iz) .LE. yedoma_depth ) yedoma_depth_index = yedoma_depth_index + 1
    END DO
    WRITE(*,*) 'yedoma_depth_index ', yedoma_depth_index, ' at depth ', yedoma_depth

    IF ( yedoma_depth_index .GT. 0) THEN
       DO ix = 1, kjpindex
          DO iv = 2, nvm  !!! no yedoma carbon for PFT zero.
             IF ( veget_mask_2d(ix,iv) ) THEN
                DO iz = 1, yedoma_depth_index
                   IF (yedoma(ix) .GT. 0.)  THEN
                      IF ( iz .GE. altmax_ind(ix,iv) ) THEN  !!! only put yedoma carbon at base of and below the active layer
                         soilc_a(ix, iz,iv) = yedoma_cinit_act
                         soilc_s(ix, iz,iv) = yedoma_cinit_slo
                         soilc_p(ix, iz,iv) = yedoma_cinit_pas
                      ELSE
                         soilc_a(ix, iz,iv) = zero
                         soilc_s(ix, iz,iv) = zero
                         soilc_p(ix, iz,iv) = zero
                      ENDIF
                   ELSE
                      soilc_a(ix, iz,iv) = zero
                      soilc_s(ix, iz,iv) = zero
                      soilc_p(ix, iz,iv) = zero
                   END IF
                END DO
             ENDIF
          ENDDO
       ENDDO
    ENDIF

  END SUBROUTINE initialize_yedoma_carbonstocks
!!
!================================================================================================================================
!! SUBROUTINE   : carbinput
!!
!>\BRIEF        This routine calculate carbon input to the soil
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
  SUBROUTINE carbinput(kjpindex,time_step,time,no_pfrost_decomp,tprof,tsurf,hslong, dayno,z_root,altmax, &
       soilc_a, soilc_s, soilc_p, soilc_in, dc_litter_z, z_organic, veget_max, rprof)
   
  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     
 
    INTEGER(i_std), INTENT(in)				             :: kjpindex         !! domain size
    REAL(r_std), INTENT(in)                                          :: time_step        !! time step in seconds
    REAL(r_std), INTENT(in)				             :: time  
    LOGICAL, INTENT(in)				                     :: no_pfrost_decomp  
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)           :: tprof            !! Soil temperature (K)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)		     :: tsurf	         !! Surface temperature (K)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)	     :: hslong           !! deep soil humidity
    INTEGER(i_std), INTENT(in)				             :: dayno            !! current day of year
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)		     :: z_root           !! the rooting depth 
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)		     :: altmax           !! Maximum over the year active-layer thickness
    REAL(r_std), DIMENSION(kjpindex),   INTENT (in)                  :: z_organic        !! depth to organic soil
    REAL(r_std),DIMENSION(kjpindex,nvm),INTENT(in)                   :: veget_max        !! Maximum fraction of vegetation type
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm), INTENT(in)           :: soilc_in         !! quantity of carbon going into carbon pools from litter decomposition (gC/(m**2 of ground)/day)

    !! 0.2 Output variables

    REAL(r_std), DIMENSION(kjpindex,ncarb,ndeep,nvm), INTENT(out)    :: dc_litter_z      !! depth_dependent carbon input due to litter

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)        :: soilc_a          !! active soil C
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)        :: soilc_s          !! slow soil C
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)        :: soilc_p          !! passive soil C

    !! 0.4 Local variables

    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm)                       :: dc_litter        !! depth-integrated carbon input due to litter decomposition
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm)                       :: soilc_in_finite
    REAL(r_std), DIMENSION(kjpindex,nvm)                             :: intdep           !! integral depth of carbon deposition   
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm)                       :: carbinp_correction
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm)                       :: soilc_in_TS
    LOGICAL, SAVE  			                             :: firstcall = .TRUE.
    REAL(r_std), DIMENSION(kjpindex,nvm)  		             :: z_lit            !! litter input e-folding depth
    INTEGER				                             :: il,ic,iv, ix
    LOGICAL, SAVE			                             :: check = .FALSE.
    REAL(r_std), PARAMETER			                     :: dgyrst = 96.
    INTEGER(i_std), SAVE  			                     :: id, id2, id3, id4
    CHARACTER(LEN=16)			                             :: buf  
    INTEGER				                             :: recn
    LOGICAL, SAVE                                                    :: correct_carboninput_vertprof = .TRUE.
    LOGICAL, SAVE                                                    :: new_carbinput_intdepzlit = .FALSE.
    REAL(r_std), DIMENSION(ndeep), SAVE                              :: z_thickness
    REAL(r_std), DIMENSION(ndeep)                                    :: root_prof
    REAL(r_std), SAVE                                                :: minaltmax = 0.1
    REAL(r_std), SAVE                                                :: maxaltmax = 2.
    REAL(r_std), SAVE                                                :: finerootdepthratio = 0.5  !! the ratio of fine root to overall root e-folding depth (for C inputs)
    REAL(r_std), SAVE                                                :: altrootratio = 0.5        !! the maximum ratio of fine root depth to active layer thickness (for C inputs)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)                 :: rprof                     !! root depth (m)
    INTEGER, save                                                    :: tcounter


    
    IF (no_pfrost_decomp) THEN
       !
       ! no carbon input during spinup
       !
       dc_litter(:,:,:) = 0.
       ! 
    ELSE	 
       !
       IF (firstcall) THEN
          
          DO il = 1, ndeep
             z_thickness(il) = zf_soil(il) - zf_soil(il-1) 
          END DO

          !
          !Config Key   = new_carbinput_intdepzlit
          !Config Desc  = 
          !Config Def   = n
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = [flag]
          CALL getin_p('new_carbinput_intdepzlit', new_carbinput_intdepzlit)

          !
          !Config Key   = correct_carboninput_vertprof
          !Config Desc  = 
          !Config Def   = n
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = [flag]
          CALL getin_p('correct_carboninput_vertprof', correct_carboninput_vertprof)


          ! Diagnostic output init
          
          IF (check) THEN
             tcounter = 1
             WRITE(buf,'(I3)') yr_len
             id2 = 0
             CALL fliocrfd ('alt.nc', (/'geo ','veg ','time'/), (/kjpindex, nvm, -1/), id, id2, 'REPLACE')
             CALL fliodefv (id,'time',(/ 3 /),units='seconds since 0000-01-01 00:00:00',v_t=flio_r8)
             CALL flioputa (id,'time','title','time')
             CALL flioputa (id,'time','calendar',TRIM(buf)//'d')	 
             CALL fliodefv (id,'alt',(/ 1,2,3 /),units='m',v_t=flio_r8)

             CALL fliocrfd ('soilc_litterinput.nc', (/'geo ','carb','veg ','time'/), (/kjpindex,ncarb,nvm,-1/), id3, id4, 'REPLACE')
             CALL fliodefv (id3,'time',(/ 4 /),units='seconds since 0000-01-01 00:00:00',v_t=flio_r8)
             CALL flioputa (id3,'time','title','time')
             CALL flioputa (id3,'time','calendar',TRIM(buf)//'d')	 
             CALL fliodefv (id3,'dc_litter',(/ 1,2,3,4 /),units='g C / ts',v_t=flio_r8)
             CALL fliodefv (id3,'soilc_in_TS',(/ 1,2,3,4 /),units='g C / ts',v_t=flio_r8)

 
          ENDIF ! check
          
          firstcall = .FALSE.
          !
       ENDIF ! firstcall
       
       !
       ! 1. Litter input and decomposition
       !
       ! add up the soil carbon from all veg pools, and change units from  (gC/(m**2 of ground)/day) to gC/m^2 per timestep      
       soilc_in_TS(:,:,:) = soilc_in(:,:,:)*time_step/one_day
       
       
       ! 2. Carbon input e-folding depth. We distribute with e-depth = min(z_root,intdep) 
       !	and integral depth = min(altmax,z_org)
       !     ! e-folding depth cannot be greater than integral depth
       
       ! change to make intdep equal to z_root alone
       IF ( .NOT. new_carbinput_intdepzlit ) THEN 
          z_lit(:,:) = z_root(:,:)
          intdep(:,:) = z_root(:,:)
       ELSE
          !change to separate e-folding depths for roots  from total depth over which to integrate
          z_lit(:,:) = MIN(rprof(:,:)*finerootdepthratio, altmax(:,:)*altrootratio)   ! z_lit is the e-folding depth
          intdep(:,:) = MIN(altmax(:,:), maxaltmax)  ! intdep is the maximum depth of integration; 
       ENDIF
       
       ! Litter is decomposed somehow (?) even when alt == 0. To avoid carbon loss,
       ! we distribute this carbon within the first 2 soil layers when alt == 0
       WHERE ( intdep(:,:) .LT. zi_soil(2) ) intdep(:,:) = zi_soil(2) +EPSILON(0.)  
       WHERE ( z_lit(:,:) .LT. zi_soil(2) ) z_lit(:,:) = zi_soil(2)
       
       !
       ! 3. Carbon input. 
       !
       dc_litter_z(:,:,:,:) = zero
       
       dc_litter(:,:,:)=zero
       
       
       DO il = 1, ndeep
          DO ic = 1, ncarb
             
             ! 3.1. from litter.
             
             WHERE ( zi_soil(il) .LT. intdep(:,:) .AND. veget_mask_2d(:,:) )
                dc_litter_z(:,ic,il,:) = soilc_in_TS(:,ic,:) / z_lit(:,:) / ( 1. - EXP( -intdep(:,:) / z_lit(:,:) ) ) &
                     * EXP( -zi_soil(il) / z_lit(:,:) )
             ELSEWHERE 
                dc_litter_z(:,ic,il,:) = zero
             ENDWHERE
             
             dc_litter(:,ic,:) = dc_litter(:,ic,:) + dc_litter_z(:,ic,il,:) * (zf_soil(il)-zf_soil(il-1)) 
             
          ENDDO 
          
       ENDDO 
       
       
       IF ( correct_carboninput_vertprof ) THEN 
          !! correct for the truncated carbon adddition profile here by multiplying by a scalar
          DO ic = 1, ncarb
             WHERE ( dc_litter(:,ic,:) .GT. EPSILON(0.) .AND. veget_mask_2d(:,:) ) 
                carbinp_correction(:,ic,:) = soilc_in_TS(:,ic,:)/dc_litter(:,ic,:)
             ELSEWHERE
                carbinp_correction(:,ic,:) = 0.
             END WHERE
          END DO
          
          dc_litter(:,:,:)=0.
          DO ic = 1, ncarb
             DO il = 1, ndeep
                WHERE ( veget_mask_2d(:,:) )
                   dc_litter_z(:,ic,il,:) = carbinp_correction(:,ic,:)*dc_litter_z(:,ic,il,:)
                END WHERE
                dc_litter(:,ic,:) = dc_litter(:,ic,:) + dc_litter_z(:,ic,il,:) * (zf_soil(il)-zf_soil(il-1)) !! check again
             END DO
          END DO
          
          
       ENDIF
        
       DO il = 1, ndeep
          WHERE ( veget_mask_2d(:,:) ) 
             soilc_a(:,il,:) = soilc_a(:,il,:) + dc_litter_z(:,iactive,il,:)
             soilc_s(:,il,:) = soilc_s(:,il,:) + dc_litter_z(:,islow,il,:)
             soilc_p(:,il,:) = soilc_p(:,il,:) + dc_litter_z(:,ipassive,il,:)
          END WHERE
       END DO
       
       ! Diagnostic output
       
       IF (check) THEN        
          recn = NINT(time/time_step)
          tcounter = tcounter + 1
          WRITE(*,*) 'carbinput check: output to .nc number',recn
          WRITE(*,*) 'time',time
          WRITE(*,*) 'time_step',time_step
          
          CALL flioputv (id,'time', time, (/ tcounter /) ) 
          CALL flioputv (id,'alt', altmax(:,:), start = (/ 1, 1, tcounter /), count = (/ kjpindex, nvm, 1 /) )
          CALL fliosync(id) 
 
          CALL flioputv (id3,'time', time, (/ tcounter /) ) 
          CALL flioputv (id3,'soilc_in_TS', soilc_in_TS(:,:,:), start = (/ 1, 1, 1, tcounter /), &
               count = (/ kjpindex, ncarb, nvm, 1 /) )
          CALL flioputv (id3,'dc_litter', dc_litter(:,:,:), start = (/ 1, 1, 1, tcounter /), &
               count = (/ kjpindex, ncarb, nvm, 1 /) )
          CALL fliosync(id3) 
       ENDIF
           
    ENDIF 

  END SUBROUTINE carbinput

!!
!================================================================================================================================
!! SUBROUTINE   : cryoturbate
!!
!>\BRIEF        This routine calculates cryoturbation process
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     
  
  SUBROUTINE cryoturbate(kjpindex, time_step, dayno, altmax_ind, deepC_a, deepC_s, deepC_p, &
       action, diff_k_const, bio_diff_k_const, altmax_lastyear, fixed_cryoturbation_depth)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     

    INTEGER(i_std), INTENT(in)				       :: kjpindex         !! domain size
    REAL(r_std), INTENT(in)                                    :: time_step        !! time step in seconds
    INTEGER(i_std), INTENT(in)			               :: dayno            !! number of the day in the current year
    INTEGER(i_std), DIMENSION(kjpindex,nvm),INTENT(in)	       :: altmax_ind       !! Maximum over the year active-layer index
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(in)            :: altmax_lastyear  !! Maximum over the year active-layer thickness
    CHARACTER(LEN=*), INTENT(in)                               :: action           !! what to do
    REAL(r_std), INTENT(in)                                    :: diff_k_const
    REAL(r_std), INTENT(in)                                    :: bio_diff_k_const

    !! 0.2 Output variables 

    !! 0.3 Modified variables
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deepC_a          !! soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deepC_s          !! soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deepC_p          !! soil carbon (g/m**3) passive
    REAL(r_std), DIMENSION(kjpindex,nvm),INTENT(inout)         :: fixed_cryoturbation_depth  !! depth to hold cryoturbation to for fixed runs

    !! 0.4 Local variables
    LOGICAL, SAVE			                       :: firstcall = .TRUE.
    LOGICAL, SAVE			                       :: use_new_cryoturbation
    INTEGER, SAVE			                       :: cryoturbation_method
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: deepC_a_old      !! soil carbon (g/m**3) active before timestep
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: deepC_s_old      !! soil carbon (g/m**3) slow before timestep
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: deepC_p_old      !! soil carbon (g/m**3) passive before timestep
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: altC_a_old       !! soil carbon (g/m**2) active integrated over active layer before cryoturbation
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: altC_s_old       !! soil carbon (g/m**2) slow integrated over active layer before cryoturbation
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: altC_p_old       !! soil carbon (g/m**2) passive integrated over active layer before cryoturbation
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: altC_a
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: altC_s
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: altC_p
    INTEGER(i_std), PARAMETER                                  :: n_totakefrom = 3 !! how many surface layers to subtract from in mass balance
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: surfC_totake_a   !! active soil carbon to subtract from surface layers to maintain mass balance (g/m**3) 
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: surfC_totake_s   !! slow soil carbon to subtract from surface layers to maintain mass balance (g/m**3) 
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: surfC_totake_p   !! passive soil carbon to subtract from surface layers to maintain mass balance (g/m**3) 
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: error_a
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: error_s
    REAL(r_std), DIMENSION(kjpindex,nvm)                       :: error_p
    INTEGER(i_std)                                             :: ip, il, ier, iv
    CHARACTER(LEN=20), SAVE                                    :: last_action = 'not called'
    INTEGER(i_std)                                             :: cryoturb_date
    REAL(r_std), SAVE                                          :: max_cryoturb_alt
    REAL(r_std), SAVE                                          :: min_cryoturb_alt
    REAL(r_std), SAVE                                          :: bioturbation_depth
    LOGICAL, SAVE			                       :: reset_fixed_cryoturbation_depth = .FALSE.
    LOGICAL, SAVE			                       :: use_fixed_cryoturbation_depth = .FALSE.
    LOGICAL, SAVE                                              :: adjust_method_new = .TRUE.
    REAL(r_std), DIMENSION(kjpindex,nvm)	               :: cryoturbation_depth
    
    
    ! 1. ensure that we do not repeat actions
    !
    IF ( action .EQ. last_action ) THEN
       !
       WRITE(*,*) 'CANNOT TAKE THE SAME ACTION TWICE: ',TRIM(action)
       STOP
       !
    ENDIF
    
    IF ( action .EQ. 'diffuse' ) THEN
       IF (firstcall) THEN
          
          ! 2. faire les trucs du debut
          
          ! 2.1 allocation des variables
          ALLOCATE (xe_a(kjpindex,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in xe_a allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
             STOP 'deep_carbcycle'
          END IF

          ALLOCATE (xe_s(kjpindex,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in xe_s allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
             STOP 'deep_carbcycle'
          END IF
 
          ALLOCATE (xe_p(kjpindex,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in xe_p allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
             STOP 'deep_carbcycle'
          END IF

          ALLOCATE (xc_cryoturb(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in xc_cryoturb allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF

          ALLOCATE (xd_cryoturb(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in xd_cryoturb allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF

          ALLOCATE (alpha_a(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in alpha_a allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF
	  alpha_a(:,:,:)=0.

          ALLOCATE (alpha_s(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in alpha_s allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF
	  alpha_s(:,:,:)=0.

          ALLOCATE (alpha_p(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in alpha_p allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF
	  alpha_p(:,:,:)=0.

          ALLOCATE (beta_a(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in beta_a allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm 
             STOP 'deep_carbcycle'
          END IF
	  beta_a(:,:,:)=0.

          ALLOCATE (beta_s(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in beta_s allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF
	  beta_s(:,:,:)=0.
          
          ALLOCATE (beta_p(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in beta_p allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF
 	  beta_p(:,:,:)=0.
        
          ALLOCATE (diff_k(kjpindex,ndeep,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in diff_k allocation. We stop. We need ',kjpindex,' fois ',ndeep,' fois ',nvm,' words = '&
              & , kjpindex*ndeep*nvm
             STOP 'deep_carbcycle'
          END IF
          
          ALLOCATE (cryoturb_location(kjpindex,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in cryoturb_location allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
             STOP 'deep_carbcycle'
          END IF

          ALLOCATE (bioturb_location(kjpindex,nvm),stat=ier)
          IF (ier.NE.0) THEN
             WRITE (numout,*) ' error in bioturb_location allocation. We stop. We need ',kjpindex,' fois ',nvm,' words = '&
              & , kjpindex*nvm
             STOP 'deep_carbcycle'
          END IF

           
          cryoturb_location(:,:) = .false.
          use_new_cryoturbation = .false.
          !
          !Config Key   = use_new_cryoturbation
          !Config Desc  = 
          !Config Def   = n
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = [flag]
          CALL getin_p('use_new_cryoturbation', use_new_cryoturbation)
          !
          !Config Key   = cryoturbation_method
          !Config Desc  = 
          !Config Def   = 1
          !Config If    =  OK_PC 
          !Config Help  = 
          !Config Units = []
          cryoturbation_method = 1
          CALL getin_p('cryoturbation_method', cryoturbation_method)
          !
          !Config Key   = max_cryoturb_alt
          !Config Desc  = 
          !Config Def   = 1
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = []
          max_cryoturb_alt = 1.
          CALL getin_p('max_cryoturb_alt',max_cryoturb_alt)
          !
          !Config Key   = min_cryoturb_alt
          !Config Desc  = 
          !Config Def   = 1
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = []
          min_cryoturb_alt = 0.01
          CALL getin_p('min_cryoturb_alt',min_cryoturb_alt)
          !
          !Config Key   = reset_fixed_cryoturbation_depth
          !Config Desc  = 
          !Config Def   = n
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = [flag]
          CALL getin_p('reset_fixed_cryoturbation_depth',reset_fixed_cryoturbation_depth)
          IF (reset_fixed_cryoturbation_depth) THEN
             fixed_cryoturbation_depth = altmax_lastyear
          ENDIF
          !
          !Config Key   = use_fixed_cryoturbation_depth
          !Config Desc  = 
          !Config Def   = n
          !Config If    = OK_PC
          !Config Help  = 
          !Config Units = [flag]
          CALL getin_p('use_fixed_cryoturbation_depth',use_fixed_cryoturbation_depth)
          bioturb_location(:,:) = .false.
          !
          !Config Key   = bioturbation_depth
          !Config Desc  = maximum bioturbation depth 
          !Config Def   = 2
          !Config If    = ok_pc
          !Config Help  = 
          !Config Units = m
          bioturbation_depth = 2.
          CALL getin_p('bioturbation_depth',bioturbation_depth)
          !
          !Config Key   = adjust_method_new 
          !Config Desc  = 
          !Config Def   = y
          !Config If    = ok_pc
          !Config Help  = 
          !Config Units = y/n
          CALL getin_p('adjust_method_new',adjust_method_new)
          
          firstcall = .FALSE.
       ELSE
          ! 1. calculate the total soil carbon in the active layer
          deepC_a_old = deepC_a
          deepC_s_old = deepC_s
          deepC_p_old = deepC_p
          altC_a_old(:,:) = zero
          altC_s_old(:,:) = zero
          altC_p_old(:,:) = zero
          altC_a(:,:) = zero
          altC_s(:,:) = zero
          altC_p(:,:) = zero

          DO ip = 1, kjpindex
             DO iv = 1, nvm
                IF ( cryoturb_location(ip,iv) .OR. bioturb_location(ip,iv) )THEN 
                   ! 1. calculate the total soil carbon in the active layer
                   DO il = 1, ndeep
                      altC_a_old(ip,iv) = altC_a_old(ip,iv) + deepC_a(ip,il,iv)*(zf_soil(il)-zf_soil(il-1))
                      altC_s_old(ip,iv) = altC_s_old(ip,iv) + deepC_s(ip,il,iv)*(zf_soil(il)-zf_soil(il-1))
                      altC_p_old(ip,iv) = altC_p_old(ip,iv) + deepC_p(ip,il,iv)*(zf_soil(il)-zf_soil(il-1))
                   ENDDO
                   
                   ! 2. diffuse the soil carbon                 
                   deepC_a(ip,1,iv) = (deepC_a(ip,1,iv)+mu_soil*beta_a(ip,1,iv)) / (1.+mu_soil*(1.-alpha_a(ip,1,iv)))
                   deepC_s(ip,1,iv) = (deepC_s(ip,1,iv)+mu_soil*beta_s(ip,1,iv)) / (1.+mu_soil*(1.-alpha_s(ip,1,iv)))
                   deepC_p(ip,1,iv) = (deepC_p(ip,1,iv)+mu_soil*beta_p(ip,1,iv)) / (1.+mu_soil*(1.-alpha_p(ip,1,iv)))

                   DO il = 2, ndeep
                      deepC_a(ip,il,iv) = alpha_a(ip,il-1,iv)*deepC_a(ip,il-1,iv) + beta_a(ip,il-1,iv)
                      deepC_s(ip,il,iv) = alpha_s(ip,il-1,iv)*deepC_s(ip,il-1,iv) + beta_s(ip,il-1,iv)
                      deepC_p(ip,il,iv) = alpha_p(ip,il-1,iv)*deepC_p(ip,il-1,iv) + beta_p(ip,il-1,iv)
                   ENDDO

                   ! 3. recalculate the total soil carbon in the active layer
                   DO il = 1, ndeep
                      altC_a(ip,iv) = altC_a(ip,iv) + deepC_a(ip,il,iv)*(zf_soil(il)-zf_soil(il-1))
                      altC_s(ip,iv) = altC_s(ip,iv) + deepC_s(ip,il,iv)*(zf_soil(il)-zf_soil(il-1))
                      altC_p(ip,iv) = altC_p(ip,iv) + deepC_p(ip,il,iv)*(zf_soil(il)-zf_soil(il-1))
                   ENDDO

                   ! 4. subtract the soil carbon in the top layer(s) so that the total carbon content of the active layer is conserved.             
                   ! for now remove this correction term...
                   IF (.NOT. adjust_method_new) THEN
                      surfC_totake_a(ip,iv) = (altC_a(ip,iv)-altC_a_old(ip,iv))/(zf_soil(altmax_ind(ip,iv))-zf_soil(0))
                      surfC_totake_s(ip,iv) = (altC_s(ip,iv)-altC_s_old(ip,iv))/(zf_soil(altmax_ind(ip,iv))-zf_soil(0))
                      surfC_totake_p(ip,iv) = (altC_p(ip,iv)-altC_p_old(ip,iv))/(zf_soil(altmax_ind(ip,iv))-zf_soil(0))
                      deepC_a(ip,1:altmax_ind(ip,iv),iv) = deepC_a(ip,1:altmax_ind(ip,iv),iv) - surfC_totake_a(ip,iv)
                      deepC_s(ip,1:altmax_ind(ip,iv),iv) = deepC_s(ip,1:altmax_ind(ip,iv),iv) - surfC_totake_s(ip,iv)
                      deepC_p(ip,1:altmax_ind(ip,iv),iv) = deepC_p(ip,1:altmax_ind(ip,iv),iv) - surfC_totake_p(ip,iv)
                   ELSE
                      IF (altC_a(ip,iv) .GT. zero) THEN
                         deepC_a(ip,:,iv)=deepC_a(ip,:,iv)*altC_a_old(ip,iv)/altC_a(ip,iv)
                      ENDIF
                      IF (altC_s(ip,iv) .GT. zero) THEN
                         deepC_s(ip,:,iv)=deepC_s(ip,:,iv)*altC_s_old(ip,iv)/altC_s(ip,iv)
                      ENDIF
                      IF (altC_p(ip,iv) .GT. zero) THEN
                         deepC_p(ip,:,iv)=deepC_p(ip,:,iv)*altC_p_old(ip,iv)/altC_p(ip,iv)
                      ENDIF

                   ENDIF

          	   ! Consistency check. Potentially add to STRICT_CHECK flag
                   IF ( ANY(deepC_a(ip,:,iv) .LT. zero) ) THEN
                      WRITE (numout,*) 'cryoturbate: deepC_a<0','ip=',ip,'iv=',iv,'deepC_a=',deepC_a(ip,:,iv)
                      CALL ipslerr_p (3,'cryoturbate','','','')                            
                   ENDIF
                   IF ( ANY(deepC_s(ip,:,iv) .LT. zero) ) THEN
                      WRITE (numout,*) 'cryoturbate: deepC_s<0','ip=',ip,'iv=',iv,'deepC_s=',deepC_s(ip,:,iv)         
                      CALL ipslerr_p (3,'cryoturbate','','','')                            
                   ENDIF
                   IF ( ANY(deepC_p(ip,:,iv) .LT. zero) ) THEN
                      WRITE (numout,*) 'cryoturbate: deepC_p<0','ip=',ip,'iv=',iv,'deepC_p=',deepC_p(ip,:,iv)         
                     CALL ipslerr_p (3,'cryoturbate','','','')                            
                   ENDIF

                ENDIF
             ENDDO
          ENDDO


          !WHERE (deepC_a(:,:,:) .LT. zero)   deepC_a(:,:,:) = zero
          !WHERE (deepC_s(:,:,:) .LT. zero)   deepC_s(:,:,:) = zero
          !WHERE (deepC_p(:,:,:) .LT. zero)   deepC_p(:,:,:) = zero
 
       ENDIF
      
       
    ELSEIF ( action .EQ. 'coefficients' ) THEN
       IF (firstcall) THEN
          WRITE(*,*) 'error: initilaizations have to happen before coefficients calculated. we stop.'
          STOP
       ENDIF

       cryoturb_location(:,:) =  ( altmax_lastyear(:,:) .LT. max_cryoturb_alt ) &
!In the former vertical discretization scheme the first level was at 0.016 cm; now it's only 0.00048 so we set an equivalent threshold directly as a fixed depth of 1 cm,
            .AND. ( altmax_lastyear(:,:) .GE. min_cryoturb_alt ) .AND. veget_mask_2d(:,:)
       IF (use_fixed_cryoturbation_depth) THEN
          cryoturbation_depth(:,:) = fixed_cryoturbation_depth(:,:)
       ELSE
          cryoturbation_depth(:,:) = altmax_lastyear(:,:)
       ENDIF

       bioturb_location(:,:) = ( ( altmax_lastyear(:,:) .GE. min_cryoturb_alt ) .AND. veget_mask_2d(:,:) )

       DO ip = 1, kjpindex
          DO iv = 1,nvm
             IF ( cryoturb_location(ip,iv) ) THEN
                !
                IF (use_new_cryoturbation) THEN
                   SELECT CASE(cryoturbation_method)
                   CASE(1)
                      !
                      DO il = 1, ndeep ! linear dropoff to zero between alt and 2*alt
                         IF ( zi_soil(il) .LE. cryoturbation_depth(ip,iv) ) THEN
                            diff_k(ip,il,iv) = diff_k_const
                         ELSE
                            diff_k(ip,il,iv) = diff_k_const*(un-MAX(MIN((zi_soil(il)/cryoturbation_depth(ip,iv))-un,un),zero))
                         ENDIF
                      END DO
                      !
                   CASE(2)
                      !
                      DO il = 1, ndeep ! exponential dropoff with e-folding distace = alt, below the active layer
                         IF ( zi_soil(il) .LE. cryoturbation_depth(ip,iv) ) THEN
                            diff_k(ip,il,iv) = diff_k_const
                         ELSE
                            diff_k(ip,il,iv) = diff_k_const*(EXP(-MAX((zi_soil(il)/cryoturbation_depth(ip,iv)-un),zero)))
                         ENDIF
                      END DO
                      !
                   CASE(3)
                      !
                      ! exponential dropoff with e-folding distace = alt, starting at surface
                      diff_k(ip,:,iv) = diff_k_const*(EXP(-(zi_soil(:)/cryoturbation_depth(ip,iv))))
                      !
                   CASE(4)
                      !
                      DO il = 1, ndeep ! linear dropoff to zero between alt and 3*alt
                         IF ( zi_soil(il) .LE. cryoturbation_depth(ip,iv) ) THEN
                            diff_k(ip,il,iv) = diff_k_const
                         ELSE
                            diff_k(ip,il,iv) = diff_k_const*(un-MAX(MIN((zi_soil(il)-cryoturbation_depth(ip,iv))/ &
                                 (2.*cryoturbation_depth(ip,iv)),un),zero))
                         ENDIF
                      END DO
                      ! 
                      IF (printlev>=3) WRITE(*,*) 'cryoturb method 4: ip, iv, diff_k(ip,:,iv): ', ip, iv, diff_k(ip,:,iv)
                   CASE(5)
                      !
                      DO il = 1, ndeep ! linear dropoff to zero between alt and 3m
                         IF ( zi_soil(il) .LE. cryoturbation_depth(ip,iv) ) THEN
                            diff_k(ip,il,iv) = diff_k_const
                         ELSE
                            diff_k(ip,il,iv) = diff_k_const*(un-MAX(MIN((zi_soil(il)-cryoturbation_depth(ip,iv))/ &
                                 (3.-cryoturbation_depth(ip,iv)),un),zero))
                         ENDIF
                      END DO
                      !
                      IF (printlev>=3) WRITE(*,*) 'cryoturb method 5: ip, iv, diff_k(ip,:,iv): ', ip, iv, diff_k(ip,:,iv)
                   END SELECT
                   
                   ELSE ! old cryoturbation scheme 
                   !
                   diff_k(ip,1:altmax_ind(ip,iv),iv) = diff_k_const
                   diff_k(ip, altmax_ind(ip,iv)+1,iv) = diff_k_const/10.
                   diff_k(ip, altmax_ind(ip,iv)+2,iv) = diff_k_const/100.
                   diff_k(ip,(altmax_ind(ip,iv)+3):ndeep,iv) = zero
                ENDIF
             ELSE IF ( bioturb_location(ip,iv) ) THEN
                DO il = 1, ndeep
                   IF ( zi_soil(il) .LE. bioturbation_depth ) THEN
                      diff_k(ip,il,iv) = bio_diff_k_const
                   ELSE
                      diff_k(ip,il,iv) = zero
                   ENDIF
                END DO
	     ELSE
                diff_k(ip,:,iv) = zero
             END IF
          END DO
       END DO
       
       DO il = 1,ndeep-1
          WHERE ( cryoturb_location(:,:) .OR. bioturb_location(:,:) )
             xc_cryoturb(:,il,:) = (zf_soil(il)-zf_soil(il-1))  / time_step
             xd_cryoturb(:,il,:) = diff_k(:,il,:) / (zi_soil(il+1)-zi_soil(il))
          endwhere
       ENDDO
       
       WHERE ( cryoturb_location(:,:) .OR. bioturb_location(:,:)  )
          xc_cryoturb(:,ndeep,:) = (zf_soil(ndeep)-zf_soil(ndeep-1))  / time_step
          
          !bottom
          xe_a(:,:) = xc_cryoturb(:,ndeep,:)+xd_cryoturb(:,ndeep-1,:)
          xe_s(:,:) = xc_cryoturb(:,ndeep,:)+xd_cryoturb(:,ndeep-1,:)
          xe_p(:,:) = xc_cryoturb(:,ndeep,:)+xd_cryoturb(:,ndeep-1,:)
          alpha_a(:,ndeep-1,:) = xd_cryoturb(:,ndeep-1,:) / xe_a(:,:)
          alpha_s(:,ndeep-1,:) = xd_cryoturb(:,ndeep-1,:) / xe_s(:,:)
          alpha_p(:,ndeep-1,:) = xd_cryoturb(:,ndeep-1,:) / xe_p(:,:)
          beta_a(:,ndeep-1,:) = xc_cryoturb(:,ndeep,:)*deepC_a(:,ndeep,:) / xe_a(:,:)
          beta_s(:,ndeep-1,:) = xc_cryoturb(:,ndeep,:)*deepC_s(:,ndeep,:) / xe_s(:,:)
          beta_p(:,ndeep-1,:) = xc_cryoturb(:,ndeep,:)*deepC_p(:,ndeep,:) / xe_p(:,:)
       END WHERE

       !other levels
       DO il = ndeep-2,1,-1
          WHERE ( cryoturb_location(:,:) .OR. bioturb_location(:,:) )
             xe_a(:,:) = xc_cryoturb(:,il+1,:) + (1.-alpha_a(:,il+1,:))*xd_cryoturb(:,il+1,:) + xd_cryoturb(:,il,:)
             xe_s(:,:) = xc_cryoturb(:,il+1,:) + (1.-alpha_s(:,il+1,:))*xd_cryoturb(:,il+1,:) + xd_cryoturb(:,il,:)
             xe_p(:,:) = xc_cryoturb(:,il+1,:) + (1.-alpha_s(:,il+1,:))*xd_cryoturb(:,il+1,:) + xd_cryoturb(:,il,:)
             alpha_a(:,il,:) = xd_cryoturb(:,il,:) / xe_a(:,:)
             alpha_s(:,il,:) = xd_cryoturb(:,il,:) / xe_s(:,:)
             alpha_p(:,il,:) = xd_cryoturb(:,il,:) / xe_p(:,:)
             beta_a(:,il,:) = (xc_cryoturb(:,il+1,:)*deepC_a(:,il+1,:)+xd_cryoturb(:,il+1,:)*beta_a(:,il+1,:)) / xe_a(:,:)
             beta_s(:,il,:) = (xc_cryoturb(:,il+1,:)*deepC_s(:,il+1,:)+xd_cryoturb(:,il+1,:)*beta_s(:,il+1,:)) / xe_s(:,:)
             beta_p(:,il,:) = (xc_cryoturb(:,il+1,:)*deepC_p(:,il+1,:)+xd_cryoturb(:,il+1,:)*beta_p(:,il+1,:)) / xe_p(:,:)
          END WHERE
       ENDDO

    ELSE
       !
       ! do not know this action
       !
       CALL ipslerr_p(3, 'cryoturbate', 'DO NOT KNOW WHAT TO DO:', TRIM(action), '')
       !
    ENDIF
    
    ! keep last action in mind
    !
    last_action = action
    
  END  SUBROUTINE cryoturbate

!!
!================================================================================================================================
!! SUBROUTINE   : permafrost_decomp
!!
!>\BRIEF        This routine calculates carbon decomposition
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     

  SUBROUTINE permafrost_decomp (kjpindex, time_step, tprof, Nconfun, airvol_soil, &
       oxlim, tau_CH4troph, ok_methane, fbactratio, O2m, &
       totporO2_soil, totporCH4_soil, hslong, clay, &
       no_pfrost_decomp, deepC_a, deepC_s, deepC_p, deltaCH4g, deltaCH4, deltaC1_a, deltaC1_s, deltaC1_p, deltaC2, &
       deltaC3, O2_soil, CH4_soil, fbact_out, MG_useallCpools)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     

    INTEGER(i_std), INTENT(in)				       :: kjpindex        !! domain size
    REAL(r_std), INTENT(in)                                    :: time_step       !! time step in seconds
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT(in)   :: tprof           !! deep temperature profile
    INTEGER(i_std),  INTENT(in)                                :: Nconfun
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: airvol_soil
    LOGICAL, INTENT(in)                                        :: oxlim           !! O2 limitation taken into account
    REAL(r_std), INTENT(in)                                    :: tau_CH4troph    !! time constant of methanetrophy (s)
    LOGICAL, INTENT(in)                                        :: ok_methane      !! Is Methanogenesis and -trophy taken into account? 
    REAL(r_std), INTENT(in)                                    :: fbactratio      !! time constant of methanogenesis (ratio to that of oxic)
    REAL(r_std), INTENT(in)                                    :: O2m             !! oxygen concentration [g/m3] below which there is anoxy 
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: totporO2_soil   !! total O2 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: totporCH4_soil  !! total CH4 porosity (Tans, 1998)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: hslong          !! deep soil humidity
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: clay            !! clay content
    LOGICAL, INTENT(in)				               :: no_pfrost_decomp!! Whether this is a spinup run
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)     :: fbact_out
    LOGICAL, INTENT(in)                                        :: MG_useallCpools !! Do we allow all three C pools to feed methanogenesis?
    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deepC_a         !! soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deepC_s         !! soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deepC_p         !! soil carbon (g/m**3) passive
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaCH4
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaCH4g
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaC1_a
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaC1_s
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaC1_p
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaC2
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: deltaC3
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: O2_soil         !! oxygen (g O2/m**3 air)
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(inout)  :: CH4_soil        !! methane (g CH4/m**3 air)

    !! 0.4 Local variables

    LOGICAL, SAVE                                              :: firstcall = .TRUE.    
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:,:), SAVE         :: fc              !! flux fractions within carbon pools
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:), SAVE           :: fr              !! fraction of decomposed carbon that goes into the atmosphere
    INTEGER(i_std)                                             :: ier
    REAL(r_std), DIMENSION(3,3)                                :: cflux           !! fluxes between soil carbon reservoirs
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm)                 :: nadd_soil       !! number of moles created / m**3 of air
    REAL(r_std)                                                :: fbact_a,fbact_s, fbact_p,temp
    REAL(r_std)                                                :: fbactCH4_a, fbactCH4_s, fbactCH4_p
    REAL(r_std)                                                :: dC,dCm
    REAL(r_std)                                                :: dCH4,dCH4m,dO2
    INTEGER(i_std)                                             :: il, ip, iv


    IF (firstcall) THEN

       ALLOCATE (fc(kjpindex,3,3,nvm),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in fc allocation. We stop. We need ',kjpindex,' fois ',3,' fois ',3,' fois ',nvm,' words = '&
           & , kjpindex*3*3*nvm
          STOP 'deep_carbcycle'
       END IF
       ALLOCATE (fr(kjpindex,3,nvm),stat=ier)
       IF (ier.NE.0) THEN
          WRITE (numout,*) ' error in fc allocation. We stop. We need ',kjpindex,' fois ',3,' fois ',nvm,' words = '&
           & , kjpindex*3*nvm
          STOP 'deep_carbcycle'
       END IF

       !
       ! calculate carbon flux fractions
       !
       DO iv =1,nvm
          fc(:,iactive,iactive,iv) = 0.0_r_std
          fc(:,iactive,ipassive,iv) = 0.004_r_std
          fc(:,iactive,islow,iv) = 1._r_std - (.85-.68*clay(:)) - fc(:,iactive,ipassive,iv)
          !
          fc(:,islow,islow,iv) = .0_r_std
          fc(:,islow,iactive,iv) = .42_r_std
          fc(:,islow,ipassive,iv) = .03_r_std
          !
          fc(:,ipassive,ipassive,iv) = .0_r_std
          fc(:,ipassive,iactive,iv) = .45_r_std
          fc(:,ipassive,islow,iv) = .0_r_std
          !
          fr(:,:,iv) = 1._r_std-fc(:,:,iactive,iv)-fc(:,:,islow,iv)-fc(:,:,ipassive,iv)
          firstcall = .FALSE.
       END DO
       IF (printlev>=3) THEN
          DO ip = 1,kjpindex
             WRITE(*,*) 'cdk: permafrost_decomp: i, fraction respired gridcell(i) :', ip, fr(ip,:,1)
          END DO
       ENDIF
    ENDIF
    
    !
    ! calculate carbon consumption
    !
    nadd_soil(:,:,:) = zero
    cflux(:,:) = zero

    deltaC1_a(:,:,:) = zero
    deltaC1_s(:,:,:) = zero
    deltaC1_p(:,:,:) = zero
    deltaCH4(:,:,:) = zero
    deltaCH4g(:,:,:) = zero
    deltaC2(:,:,:) = zero
    deltaC3(:,:,:) = zero   
    DO ip = 1, kjpindex
       !
       DO iv = 1, nvm
          !
          IF (  veget_mask_2d(ip,iv) ) THEN
             !
             DO il = 1, ndeep
                !
                ! 1 function that gives carbon residence time as a function of
                !     soil temperature (in seconds)
                !
                temp = tprof(ip,il,iv) - ZeroCelsius
                IF (no_pfrost_decomp) THEN
                   ! no decomposition during spinup
                   fbact_a = HUGE(1.0)
                ELSE
                   fbact_a = fbact_out(ip,il,iv)
                   fbact_a = MAX(fbact_a,time_step)
                ENDIF
                !
                IF ( fbact_a/HUGE(1.) .GT. .1 ) THEN
                   fbact_s = fbact_a
                   fbact_p = fbact_a
                ELSE
                   fbact_s = fbact_a * fslow
                   fbact_p = fbact_a * fpassive
                ENDIF
                !
                ! methanogenesis: first guess, 10 times (fbactratio) slower than oxic
                ! decomposition
                IF ( fbact_a/HUGE(1.) .GT. .1 ) THEN 
                   fbactCH4_a = fbact_a
                   fbactCH4_s = fbact_s
                   fbactCH4_p = fbact_p
                ELSE
                   fbactCH4_a = fbact_a * fbactratio
                   IF ( MG_useallCpools ) THEN
                      fbactCH4_s = fbact_s * fbactratio
                      fbactCH4_p = fbact_p * fbactratio
                   ELSE
                      fbactCH4_s = HUGE(1.0)
                      fbactCH4_p = HUGE(1.0)
                   ENDIF
                ENDIF
                !
                ! 2 oxic decomposition: carbon and oxygen consumption
                !
                ! 2.1 active
                !
                IF (oxlim) THEN
                   dCm = O2_soil(ip,il,iv)*airvol_soil(ip,il,iv)*wC/wO2
                   dC = MIN(deepC_a(ip,il,iv) * time_step/fbact_a,dCm)
                ELSE
                   dC = deepC_a(ip,il,iv) * time_step/fbact_a
                ENDIF

                ! pour actif
                dC = dC * ( 1. - .75 * clay(ip) )

                ! flux vers les autres reservoirs
                cflux(iactive,ipassive) = fc(ip,iactive,ipassive,iv) * dC
                cflux(iactive,islow) = fc(ip,iactive,islow,iv) * dC
                !
                deepC_a(ip,il,iv) = deepC_a(ip,il,iv) - dC
                dO2 = wO2/wC * dC*fr(ip,iactive,iv) / totporO2_soil(ip,il,iv)
                O2_soil(ip,il,iv) = MAX( O2_soil(ip,il,iv) - dO2, zero)
                ! keep delta C * fr in memory (generates energy)
                deltaC1_a(ip,il,iv) = dC*fr(ip,iactive,iv) !!this line!!!
                !
                ! 2.2 slow       
                !
                IF (oxlim) THEN
                   dCm = O2_soil(ip,il,iv)*airvol_soil(ip,il,iv)*wC/wO2
                   dC = MIN(deepC_s(ip,il,iv) * time_step/fbact_s,dCm)
                ELSE
                   dC = deepC_s(ip,il,iv) * time_step/fbact_s
                ENDIF
                ! flux vers les autres reservoirs
                cflux(islow,iactive) = fc(ip,islow,iactive,iv) * dC
                cflux(islow,ipassive) = fc(ip,islow,ipassive,iv) * dC
                !
                deepC_s(ip,il,iv) = deepC_s(ip,il,iv) - dC
                dO2 = wO2/wC * dC*fr(ip,islow,iv) / totporO2_soil(ip,il,iv)
                O2_soil(ip,il,iv) = MAX( O2_soil(ip,il,iv) - dO2, zero)
                ! keep delta C * fr in memory (generates energy)
                deltaC1_s(ip,il,iv) =  dC*fr(ip,islow,iv)
                !
                ! 2.3 passive
                !
                IF (oxlim) THEN
                   dCm = O2_soil(ip,il,iv)*airvol_soil(ip,il,iv)*wC/wO2
                   dC = MIN(deepC_p(ip,il,iv) * time_step/fbact_p,dCm)
                ELSE
                   dC = deepC_p(ip,il,iv) * time_step/fbact_p
                ENDIF
                ! flux vers les autres reservoirs
                cflux(ipassive,iactive) = fc(ip,ipassive,iactive,iv) * dC
                cflux(ipassive,islow) = fc(ip,ipassive,islow,iv) * dC
                !
                deepC_p(ip,il,iv) = deepC_p(ip,il,iv) - dC
                dO2 = wO2/wC * dC*fr(ip,ipassive,iv) / totporO2_soil(ip,il,iv)
                O2_soil(ip,il,iv) = MAX( O2_soil(ip,il,iv) - dO2, zero)
                ! keep delta C * fr in memory (generates energy)
                deltaC1_p(ip,il,iv) =  dC*fr(ip,ipassive,iv)
                !
                !
                ! 3 methanogenesis or methanotrophy
                !   
                !
                IF (ok_methane) THEN
                   !
                   !
                   ! 3.1 active pool methanogenesis
                   dC = deepC_a(ip,il,iv) * time_step / fbactCH4_a * EXP(-O2_soil(ip,il,iv)*(1+hslong(ip,il,iv) * &
                        (BunsenO2-1.)) / O2m ) !DKtest: when commented, no ox lim for MG
                   ! pour actif
                   dC = dC * ( 1. - .75 * clay(ip) )
                   dCH4 = dc*fr(ip,iactive,iv) * wCH4/wC / totporCH4_soil(ip,il,iv)
                   !
                   !
                   ! flux vers les autres reservoirs
                   cflux(iactive,ipassive)=cflux(iactive,ipassive)+fc(ip,iactive,ipassive,iv)*dC
                   cflux(iactive,islow)=cflux(iactive,islow)+fc(ip,iactive,islow,iv)*dC
                   !
                   deepC_a(ip,il,iv) = deepC_a(ip,il,iv) - dC
                   !
                   deltaCH4g(ip,il,iv) = dCH4
                   !
                   CH4_soil(ip,il,iv) = CH4_soil(ip,il,iv) + dCH4
                   ! keep delta C*fr in memory (generates energy)
                   deltaC2(ip,il,iv) = dC*fr(ip,iactive,iv)
                   !
                   ! how many moles of gas / m**3 of air did we generate?
                   ! (methanogenesis generates 1 molecule net if we take
                   !  B -> B' + CH4 ) 
                   nadd_soil(ip,il,iv) = nadd_soil(ip,il,iv) + dCH4/wCH4
                   !
                   !
                   IF ( MG_useallCpools ) THEN
                      !
                      ! 3.2 slow pool methanogenesis  cdk: adding this to allow other carbon pools to participate in MG
                      dC = deepC_s(ip,il,iv) * time_step / fbactCH4_s * EXP(-O2_soil(ip,il,iv)*(1+hslong(ip,il,iv) * &
                           (BunsenO2-1.)) / O2m ) !DKtest: when commented, no ox lim for MG
                      dCH4 = dc*fr(ip,islow,iv) * wCH4/wC / totporCH4_soil(ip,il,iv)
                      !
                      ! flux vers les autres reservoirs
                      cflux(islow,ipassive)=cflux(islow,ipassive)+fc(ip,islow,ipassive,iv)*dC
                      cflux(islow,iactive)=cflux(islow,iactive)+fc(ip,islow,iactive,iv)*dC
                      !
                      deepC_s(ip,il,iv) = deepC_s(ip,il,iv) - dC
                      !
                      deltaCH4g(ip,il,iv) = deltaCH4g(ip,il,iv) + dCH4
                      CH4_soil(ip,il,iv) = CH4_soil(ip,il,iv) + dCH4
                      ! keep delta C*fr in memory (generates energy)
                      deltaC2(ip,il,iv) = deltaC2(ip,il,iv) + dC*fr(ip,islow,iv)
                      !
                      ! how many moles of gas / m**3 of air did we generate?
                      ! (methanogenesis generates 1 molecule net if we take
                      !  B -> B' + CH4 ) 
                      nadd_soil(ip,il,iv) = nadd_soil(ip,il,iv) + dCH4/wCH4
                      !	      
                      !
                      !
                      ! 3.3 passive pool methanogenesis  cdk: adding this to allow other carbon pools to participate in MG
                      dC = deepC_p(ip,il,iv) * time_step / fbactCH4_p * EXP(-O2_soil(ip,il,iv)*(1+hslong(ip,il,iv) * &
                          (BunsenO2-1.)) / O2m ) !DKtest: when commented, no ox lim for MG
                      dCH4 = dc*fr(ip,ipassive,iv) * wCH4/wC / totporCH4_soil(ip,il,iv)
                      !
                      ! flux vers les autres reservoirs
                      cflux(ipassive,islow)=cflux(ipassive,islow)+fc(ip,ipassive,islow,iv)*dC
                      cflux(ipassive,iactive)=cflux(ipassive,iactive)+fc(ip,ipassive,iactive,iv)*dC
                      !
                      deepC_p(ip,il,iv) = deepC_p(ip,il,iv) - dC
                      !
                      deltaCH4g(ip,il,iv) = deltaCH4g(ip,il,iv) + dCH4
                      CH4_soil(ip,il,iv) = CH4_soil(ip,il,iv) + dCH4
                      ! keep delta C*fr in memory (generates energy)
                      deltaC2(ip,il,iv) = deltaC2(ip,il,iv) + dC*fr(ip,ipassive,iv)
                      !
                      ! how many moles of gas / m**3 of air did we generate?
                      ! (methanogenesis generates 1 molecule net if we take
                      !  B -> B' + CH4 ) 
                      nadd_soil(ip,il,iv) = nadd_soil(ip,il,iv) + dCH4/wCH4
                      !	      
                      !
                   ENDIF
                   !
                   ! trophy: 
                   ! no temperature dependence except that T>0��C (Price et
                   ! al, GCB 2003; Koschorrek and Conrad, GBC 1993).
                   ! tau_CH4troph is such that we fall between values of
                   ! soil methane oxidation flux given by these authors.
                   ! 
                   IF ( temp .GE. zero ) THEN
                      !
                      dCH4m = O2_soil(ip,il,iv)/2. * wCH4/wO2 * totporO2_soil(ip,il,iv)/totporCH4_soil(ip,il,iv)
                      !      		dCH4m = CH4_soil(ip,il,iv)  !DKtest - no ox lim to trophy
                      dCH4 = MIN( CH4_soil(ip,il,iv) * time_step/MAX(tau_CH4troph,time_step), dCH4m )
                      CH4_soil(ip,il,iv) = CH4_soil(ip,il,iv) - dCH4
                      dO2 = 2.*dCH4 * wO2/wCH4 * totporCH4_soil(ip,il,iv)/totporO2_soil(ip,il,iv)
                      O2_soil(ip,il,iv) = MAX( O2_soil(ip,il,iv) - dO2, zero)
                      ! keep delta CH4 in memory (generates energy)
                      deltaCH4(ip,il,iv) = dCH4
                      ! carbon (g/m3 soil) transformed to CO2
                      deltaC3(ip,il,iv)=dCH4/wCH4*wC*totporCH4_soil(ip,il,iv)
                      ! how many moles of gas / m**3 of air did we generate?
                      ! (methanotrophy consumes 2 molecules net if we take
                      !  CH4 + 2 O2 -> CO2 + 2 H2O )
                      nadd_soil(ip,il,iv) = nadd_soil(ip,il,iv)-2.*dCH4/wCH4
                      !
                   ENDIF
                   
                ENDIF
               
                ! 4 add fluxes between reservoirs
                
                deepC_a(ip,il,iv)=deepC_a(ip,il,iv)+cflux(islow,iactive)+cflux(ipassive,iactive)
                deepC_s(ip,il,iv)=deepC_s(ip,il,iv)+cflux(iactive,islow)+cflux(ipassive,islow)
                deepC_p(ip,il,iv)=deepC_p(ip,il,iv)+cflux(iactive,ipassive)+cflux(islow,ipassive)
                
             ENDDO
             
          ELSE

          ENDIF
          
       ENDDO
       
    ENDDO
  END SUBROUTINE permafrost_decomp


!!
!================================================================================================================================
!! SUBROUTINE   : calc_vert_int_soil_carbon
!!
!>\BRIEF        This routine calculates carbon decomposition
!!
!! DESCRIPTION : 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : 
!! 
!! REFERENCE(S) : None
!!
!! FLOWCHART11    : None
!! \n
!_
!================================================================================================================================     

  SUBROUTINE calc_vert_int_soil_carbon(kjpindex, deepC_a, deepC_s, deepC_p, carbon, carbon_surf, zf_soil)

  !! 0. Variable and parameter declaration

    !! 0.1  Input variables     

    INTEGER(i_std), INTENT(in)                                :: kjpindex   !! domain size
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)    :: deepC_a    !! active pool deepc
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)    :: deepC_s    !! slow pool deepc
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT(in)    :: deepC_p    !! passive pool deepc
    REAL(r_std), DIMENSION(0:ndeep), INTENT(in)               :: zf_soil    !! depths at full levels
   
    !! 0.2 Output variables

    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm), INTENT (out)  :: carbon     !! vertically-integrated carbon pool: active, slow, or passive, (gC/(m**2 of ground))
    REAL(r_std), DIMENSION(kjpindex,ncarb,nvm),   INTENT (out):: carbon_surf!! vertically-integrated carbon pool to 1 meter: active, slow, or passive,(gC/(m**2 of ground))

    !! 0.3 Modified variables

    !! 0.4 Local variables
    INTEGER(i_std)                                            :: il
    real(r_std), parameter                                    ::  maxdepth=2.!! depth to which we intergrate the carbon for carbon_surf calculation                              

    carbon(:,:,:) = zero
    DO il = 1, ndeep
       WHERE ( veget_mask_2d(:,:) ) 
          carbon(:,iactive,:) = carbon(:,iactive,:) + deepC_a(:,il,:)*(zf_soil(il)-zf_soil(il-1))
          carbon(:,islow,:) = carbon(:,islow,:) + deepC_s(:,il,:)*(zf_soil(il)-zf_soil(il-1))
          carbon(:,ipassive,:) = carbon(:,ipassive,:) + deepC_p(:,il,:)*(zf_soil(il)-zf_soil(il-1))
       END WHERE
    ENDDO

    carbon_surf(:,:,:) = zero
    DO il = 1, ndeep
       if (zf_soil(il-1) .lt. maxdepth ) then
          where ( veget_mask_2d(:,:) ) 
             carbon_surf(:,iactive,:) = carbon_surf(:,iactive,:) + deepC_a(:,il,:)*(min(maxdepth,zf_soil(il))-zf_soil(il-1))
             carbon_surf(:,islow,:) = carbon_surf(:,islow,:) + deepC_s(:,il,:)*(min(maxdepth,zf_soil(il))-zf_soil(il-1))
             carbon_surf(:,ipassive,:) = carbon_surf(:,ipassive,:) + deepC_p(:,il,:)*(min(maxdepth,zf_soil(il))-zf_soil(il-1))
          end where
       endif
    ENDDO

  END SUBROUTINE calc_vert_int_soil_carbon

END MODULE stomate_permafrost_soilcarbon
