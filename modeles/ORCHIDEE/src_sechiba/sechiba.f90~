!  ==============================================================================================================================\n
!  MODULE 	: sechiba
! 
!  CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
!  LICENCE      : IPSL (2006)
!  This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        Structures the calculation of atmospheric and hydrological 
!! variables by calling diffuco_main, enerbil_main, hydrolc_main (or hydrol_main),
!! enerbil_fusion, condveg_main and thermosoil_main. Note that sechiba_main
!! calls slowproc_main and thus indirectly calculates the biogeochemical
!! processes as well.
!!
!!\n DESCRIPTION  : :: shumdiag, :: litterhumdiag and :: stempdiag are not 
!! saved in the restart file because at the first time step because they 
!! are recalculated. However, they must be saved as they are in slowproc 
!! which is called before the modules which calculate them.
!! 
!! RECENT CHANGE(S): None 
!! 
!! REFERENCE(S) : None
!!   
!! SVN     :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_sechiba/sechiba.f90 $ 
!! $Date: 2016-05-09 17:28:04 +0200 (Mon, 09 May 2016) $
!! $Revision: 3420 $
!! \n
!_ ================================================================================================================================
 
MODULE sechiba
 
  USE ioipsl
  USE xios_orchidee
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE diffuco
  USE condveg
  USE enerbil
  USE hydrol
  USE hydrolc
  USE thermosoil
  USE thermosoilc
  USE sechiba_io
  USE slowproc
  USE routing
  use ioipsl_para

  IMPLICIT NONE

  PRIVATE
  PUBLIC sechiba_main, sechiba_initialize, sechiba_clear

  INTEGER(i_std), SAVE                             :: printlev_loc   !! local printlev for this module
!$OMP THREADPRIVATE(printlev_loc)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexveg       !! indexing array for the 3D fields of vegetation
!$OMP THREADPRIVATE(indexveg)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexlai       !! indexing array for the 3D fields of vegetation
!$OMP THREADPRIVATE(indexlai)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexnobio     !! indexing array for the 3D fields of other surfaces (ice,
                                                                     !! lakes, ...)
!$OMP THREADPRIVATE(indexnobio)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexsoil      !! indexing array for the 3D fields of soil types (kjpindex*nstm)
!$OMP THREADPRIVATE(indexsoil)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexgrnd      !! indexing array for the 3D ground heat profiles (kjpindex*ngrnd)
!$OMP THREADPRIVATE(indexgrnd)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexlayer     !! indexing array for the 3D fields of soil layers in CWRR (kjpindex*nslm)
!$OMP THREADPRIVATE(indexlayer)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexnbdl      !! indexing array for the 3D fields of diagnostic soil layers (kjpindex*nbdl)
!$OMP THREADPRIVATE(indexnbdl)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexalb       !! indexing array for the 2 fields of albedo
!$OMP THREADPRIVATE(indexalb)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: indexsnow      !! indexing array for the 3D fields snow layers
!$OMP THREADPRIVATE(indexsnow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: veget          !! Fraction of vegetation type (unitless, 0-1)       
!$OMP THREADPRIVATE(veget)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: veget_max      !! Max. fraction of vegetation type (LAI -> infty, unitless)
!$OMP THREADPRIVATE(veget_max)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: tot_bare_soil  !! Total evaporating bare soil fraction 
!$OMP THREADPRIVATE(tot_bare_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: height         !! Vegetation Height (m)
!$OMP THREADPRIVATE(height)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: totfrac_nobio  !! Total fraction of continental ice+lakes+cities+...
                                                                     !! (unitless, 0-1)
!$OMP THREADPRIVATE(totfrac_nobio)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: floodout       !! Flow out of floodplains from hydrol
!$OMP THREADPRIVATE(floodout)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: runoff         !! Surface runoff calculated by hydrol or hydrolc 
                                                                     !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(runoff)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: drainage       !! Deep drainage calculatedd by hydrol or hydrolc 
                                                                     !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(drainage)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: returnflow     !! Water flow from lakes and swamps which returns to 
                                                                     !! the grid box @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(returnflow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: reinfiltration !! Routed water which returns into the soil
!$OMP THREADPRIVATE(reinfiltration)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: irrigation     !! Irrigation flux taken from the routing reservoirs and 
                                                                     !! being put into the upper layers of the soil 
                                                                     !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(irrigation)
  !REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: irrigation_exe !! real Irrigation flux taken from the routing reservoirs and 
                                                                     !! being put into the upper layers of the soil 
                                                                     !! @tex $(kg m^{-2})$ @endtex
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: irrigr         !! irrigation water reservoir @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(irrigr)

LOGICAL, ALLOCATABLE, SAVE, DIMENSION (:)          :: do_irrig_reservoir !!  if apply irrigation reservoir
                                                                         !! (TRUE or FALSE)
!$OMP THREADPRIVATE(do_irrig_reservoir)

REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: reservoir_dep_max  !! maximum depth of irrigation reservoir (mm)
!$OMP THREADPRIVATE(reservoir_dep_max)

REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: irrig_dep_threshold!! irrigation occurs when reservoir depth is lower than the threshold (mm)
!$OMP THREADPRIVATE(irrig_dep_threshold)

LOGICAL, ALLOCATABLE, SAVE, DIMENSION (:)          :: do_awd             !!  if apply Alternate Wetting and Drying (AWD) irrigation (T/F)
!$OMP THREADPRIVATE(do_awd)

REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: awd_dep            !! water amount (depth) of one irrigation events (mm)
!$OMP THREADPRIVATE(awd_dep)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: emis           !! Surface emissivity (unitless)
!$OMP THREADPRIVATE(emis)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: z0             !! Surface roughness (m)
!$OMP THREADPRIVATE(z0)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: roughheight    !! Effective height for roughness (m)
!$OMP THREADPRIVATE(roughheight)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: roughheight_pft    !! Effective height for roughness (m)
!$OMP THREADPRIVATE(roughheight_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: reinf_slope    !! slope coefficient (reinfiltration) 
!$OMP THREADPRIVATE(reinf_slope)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: shumdiag       !! Mean relative soil moisture in the different levels used 
                                                                     !! by thermosoil.f90 (unitless, 0-1)
!$OMP THREADPRIVATE(shumdiag)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: shumdiag_perma !! Saturation degree of the soil 
!$OMP THREADPRIVATE(shumdiag_perma)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: k_litt         !! litter cond.
!$OMP THREADPRIVATE(k_litt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: litterhumdiag  !! Litter dryness factor (unitless, 0-1)
!$OMP THREADPRIVATE(litterhumdiag)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: stempdiag      !! Temperature which controls canopy evolution (K)
!$OMP THREADPRIVATE(stempdiag)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: qsintveg       !! Water on vegetation due to interception 
                                                                     !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(qsintveg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: tq_cdrag_pft   !! Interception resistance (unitless,0-1)
!$OMP THREADPRIVATE(tq_cdrag_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vbeta2         !! Interception resistance (unitless,0-1)
!$OMP THREADPRIVATE(vbeta2)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vbeta3         !! Vegetation resistance (unitless,0-1)
!$OMP THREADPRIVATE(vbeta3)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vbeta3pot      !! Potential vegetation resistance
!$OMP THREADPRIVATE(vbeta3pot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: gsmean         !! Mean stomatal conductance for CO2 (umol m-2 s-1) 
!$OMP THREADPRIVATE(gsmean) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: cimean         !! STOMATE: mean intercellular CO2 concentration (ppm)
!$OMP THREADPRIVATE(cimean)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vevapwet       !! Interception loss over each PFT 
                                                                     !! @tex $(kg m^{-2} days^{-1})$ @endtex
!$OMP THREADPRIVATE(vevapwet)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: transpir       !! Transpiration @tex $(kg m^{-2} days^{-1})$ @endtex
!$OMP THREADPRIVATE(transpir)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: transpot       !! Potential Transpiration (needed for irrigation)
!$OMP THREADPRIVATE(transpot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: qsintmax       !! Maximum amount of water in the canopy interception 
                                                                     !! reservoir @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(qsintmax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: rveget         !! Surface resistance for the vegetation 
                                                                     !! @tex $(s m^{-1})$ @endtex
!$OMP THREADPRIVATE(rveget)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: rstruct        !! Vegetation structural resistance
!$OMP THREADPRIVATE(rstruct)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: snow_nobio     !! Snow mass of non-vegetative surfaces 
                                                                     !! @tex $(kg m^{-2})$ @endtex
!$OMP THREADPRIVATE(snow_nobio)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: snow_nobio_age !! Snow age on non-vegetative surfaces (days)
!$OMP THREADPRIVATE(snow_nobio_age)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: frac_nobio     !! Fraction of non-vegetative surfaces (continental ice, 
                                                                     !! lakes, ...) (unitless, 0-1)
!$OMP THREADPRIVATE(frac_nobio)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: albedo         !! Surface albedo for visible and near-infrared 
                                                                     !! (unitless, 0-1)
!$OMP THREADPRIVATE(albedo)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:):: assim_param    !! min+max+opt temps, vcmax, vjmax for photosynthesis
!$OMP THREADPRIVATE(assim_param)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: lai            !! Surface foliaire
!$OMP THREADPRIVATE(lai)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: gpp            !! STOMATE: GPP. gC/m**2 of total area
!$OMP THREADPRIVATE(gpp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: temp_growth       !! Growth temperature (Â°C) - Is equal to t2m_month 
!$OMP THREADPRIVATE(temp_growth) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: humrel         !! Relative humidity
!$OMP THREADPRIVATE(humrel)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vegstress      !! Vegetation moisture stress (only for vegetation growth)
!$OMP THREADPRIVATE(vegstress)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vegstress_old  !! Vegetation moisture stress of previous time step
!$OMP THREADPRIVATE(vegstress_old)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:):: frac_age       !! Age efficacity from STOMATE for isoprene 
!$OMP THREADPRIVATE(frac_age)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: soiltile       !! Fraction of each soil tile (0-1, unitless)
!$OMP THREADPRIVATE(soiltile)
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION (:)  :: is_crop_soil   !! whether the soil tile is under cropland
!$OMP THREADPRIVATE(is_crop_soil)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION (:) :: njsc           !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
!$OMP THREADPRIVATE(njsc)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vbeta1         !! Snow resistance 
!$OMP THREADPRIVATE(vbeta1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vbeta4         !! Bare soil resistance
!$OMP THREADPRIVATE(vbeta4)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vbeta4_pft     !! Bare soil resistance
!$OMP THREADPRIVATE(vbeta4_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vbeta5         !! Floodplains resistance
!$OMP THREADPRIVATE(vbeta5)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: soilcap        !!
!$OMP THREADPRIVATE(soilcap)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: soilcap_pft    !!
!$OMP THREADPRIVATE(soilcap_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: soilflx        !!
!$OMP THREADPRIVATE(soilflx)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: soilflx_pft    !!
!$OMP THREADPRIVATE(soilflx_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: temp_sol       !! Soil temperature
!$OMP THREADPRIVATE(temp_sol)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: temp_sol_pft    !! Soil temperature for each pft
!$OMP THREADPRIVATE(temp_sol_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: temp_sol_new_pft    !! Soil temperature
!$OMP THREADPRIVATE(temp_sol_new_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: qsurf          !! near soil air moisture
!$OMP THREADPRIVATE(qsurf)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: flood_res      !! flood reservoir estimate
!$OMP THREADPRIVATE(flood_res)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: flood_frac     !! flooded fraction
!$OMP THREADPRIVATE(flood_frac)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: snow           !! Snow mass [Kg/m^2]
!$OMP THREADPRIVATE(snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: snow_age       !! Snow age
!$OMP THREADPRIVATE(snow_age)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: drysoil_frac   !! Fraction of visibly (albedo) Dry soil (Between 0 and 1)
!$OMP THREADPRIVATE(drysoil_frac)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: rsol           !! resistance to bare soil evaporation
!$OMP THREADPRIVATE(rsol)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: evap_bare_lim  !! Bare soil stress
!$OMP THREADPRIVATE(evap_bare_lim)
!!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: evap_bare_lim_pft  !! Bare soil stress
!!!$OMP THREADPRIVATE(evap_bare_lim_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: soil_deficit   !! water deficit to reach IRRIG_FULFILL
!$OMP THREADPRIVATE(soil_deficit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: co2_flux       !! CO2 flux (gC/m**2 of average ground/s)
!$OMP THREADPRIVATE(co2_flux)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: evapot         !! Soil Potential Evaporation
!$OMP THREADPRIVATE(evapot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: evapot_corr    !! Soil Potential Evaporation Correction (Milly 1992)
!$OMP THREADPRIVATE(evapot_corr)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vevapflo       !! Floodplains evaporation
!$OMP THREADPRIVATE(vevapflo)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vevapsno       !! Snow evaporation
!$OMP THREADPRIVATE(vevapsno)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vevapnu        !! Bare soil evaporation
!$OMP THREADPRIVATE(vevapnu)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vevapnu_pft        !! Bare soil evaporation
!$OMP THREADPRIVATE(vevapnu_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: t2mdiag        !! 2 meter temperature
!$OMP THREADPRIVATE(t2mdiag)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: tot_melt       !! Total melt
!$OMP THREADPRIVATE(tot_melt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: vbeta          !! Resistance coefficient
!$OMP THREADPRIVATE(vbeta)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: vbeta_pft      !! Resistance coefficient for each pft
!$OMP THREADPRIVATE(vbeta_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: valpha         !! Resistance coefficient
!$OMP THREADPRIVATE(valpha)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: fusion         !!
!$OMP THREADPRIVATE(fusion)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: rau            !! Density
!$OMP THREADPRIVATE(rau)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: deadleaf_cover !! Fraction of soil covered by dead leaves
!$OMP THREADPRIVATE(deadleaf_cover)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: ptnlev1        !! 1st level Different levels soil temperature
!$OMP THREADPRIVATE(ptnlev1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: irrig_frac        !!irrigated fraction of the croplands
!$OMP THREADPRIVATE(irrig_frac)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: tot_vegfrac_crop !! Total fraction occcupied by crops (0-1, unitess)
!$OMP THREADPRIVATE(tot_vegfrac_crop)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: mc_layh        !! Volumetric soil moisture for each layer in hydrol(liquid + ice) (m3/m3)
!$OMP THREADPRIVATE(mc_layh)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: mc_layh_s    !! Volumetric soil moisture for each layer in hydrol(liquid + ice) (m3/m3)
!$OMP THREADPRIVATE(mc_layh_s)
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: mc_layh_pft    !! Volumetric soil moisture for each layer in hydrol(liquid + ice) (m3/m3)
!!$OMP THREADPRIVATE(mc_layh_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: mcl_layh       !! Volumetric soil moisture for each layer in hydrol(liquid) (m3/m3)
!$OMP THREADPRIVATE(mcl_layh)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: mcl_layh_s    !! Volumetric soil moisture for each layer in hydrol(liquid) (m3/m3)
!$OMP THREADPRIVATE(mcl_layh_s)
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: mcl_layh_pft    !! Volumetric soil moisture for each layer in hydrol(liquid) (m3/m3)
!!$OMP THREADPRIVATE(mcl_layh_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)  :: tmc_layh       !! Total soil moisture content for each layer in hydrol(liquid + ice) (mm)
!$OMP THREADPRIVATE(tmc_layh)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: tmc_layh_s     !! Total soil moisture content for each layer in hydrol(liquid + ice) (mm)
!$OMP THREADPRIVATE(tmc_layh_s)
!  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: tmc_layh_pft     !! Total soil moisture content for each layer in hydrol(liquid + ice) (mm)
!!$OMP THREADPRIVATE(tmc_layh_pft)

  LOGICAL, SAVE                                    :: l_first_sechiba = .TRUE. !! Flag controlling the intialisation (true/false)
!$OMP THREADPRIVATE(l_first_sechiba)

  ! Variables related to snow processes calculations  

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: snowrho      !! snow density for each layer
!$OMP THREADPRIVATE(snowrho)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: snowheat     !! snow heat content for each layer (J/m2)
!$OMP THREADPRIVATE(snowheat)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: snowgrain    !! snow grain size (m)
!$OMP THREADPRIVATE(snowgrain)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: snowtemp     !! snow temperature profile (K)
!$OMP THREADPRIVATE(snowtemp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: snowdz       !! snow layer thickness (m)
!$OMP THREADPRIVATE(snowdz)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)    :: gtemp        !! soil surface temperature
!$OMP THREADPRIVATE(gtemp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)    :: soilflxresid !! Net flux to the snowpack
!$OMP THREADPRIVATE(soilflxresid)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: pgflux       !! net energy into snow pack
!$OMP THREADPRIVATE(pgflux)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: cgrnd_snow   !! Integration coefficient for snow numerical scheme
!$OMP THREADPRIVATE(cgrnd_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: dgrnd_snow   !! Integration coefficient for snow numerical scheme
!$OMP THREADPRIVATE(dgrnd_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)    :: lambda_snow  !! Coefficient of the linear extrapolation of surface temperature 
                                                                  !! from the first and second snow layers
!$OMP THREADPRIVATE(lambda_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)    :: temp_sol_add !! Additional energy to melt snow for snow ablation case (K)
!$OMP THREADPRIVATE(temp_sol_add)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)    :: sfcfrz       !! snow surface layer frozen fraction
!$OMP THREADPRIVATE(sfcfrz) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:)  :: radsink      !! radiation sink/source (J/m2)
!$OMP THREADPRIVATE(radsink)

  ! Variables related to deep permafrost calculations
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: tdeep          !! deep temperature profile
!$OMP THREADPRIVATE(tdeep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: hsdeep         !! deep soil humidity profile
!$OMP THREADPRIVATE(hsdeep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: heat_Zimov     !! heating associated with decomposition
!$OMP THREADPRIVATE(heat_Zimov)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: sfluxCH4_deep    !! surface flux of CH4 to atmosphere from permafrost
!$OMP THREADPRIVATE(sfluxCH4_deep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: sfluxCO2_deep    !! surface flux of CO2 to atmosphere from permafrost
!$OMP THREADPRIVATE(sfluxCO2_deep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: thawed_humidity  
!$OMP THREADPRIVATE(thawed_humidity)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: depth_organic_soil
!$OMP THREADPRIVATE(depth_organic_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: zz_deep
!$OMP THREADPRIVATE(zz_deep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: zz_coef_deep
!$OMP THREADPRIVATE(zz_coef_deep)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:)   :: soilc_total    !! total  soil carbon for use in thermal calcs
!$OMP THREADPRIVATE(soilc_total)

!pss:+
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: drunoff_tot         !! Surface runoff generated Dune process
!$OMP THREADPRIVATE(drunoff_tot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)    :: fwet_out      !! wetland fraction
!$OMP THREADPRIVATE(fwet_out)
!pss:-
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: rot_cmd
!$OMP THREADPRIVATE(rot_cmd) 
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:)        :: f_rot_sech
!$OMP THREADPRIVATE(f_rot_sech) 
  ! the rotation command matrix: xxxyyzz, xxx: % land fraction change, yy:
  ! source PFT, zz: destination PFT,  dimensions: kjpindex, rot_cmd_max, cyc_rot_max


CONTAINS

!!  =============================================================================================================================
!! SUBROUTINE:    sechiba_initialize
!!
!>\BRIEF	  Initialize all prinicipal modules by calling their "_initialize" subroutines
!!
!! DESCRIPTION:	  Initialize all prinicipal modules by calling their "_initialize" subroutines
!!
!! \n
!_ ==============================================================================================================================

  SUBROUTINE sechiba_initialize( &
       kjit,         kjpij,        kjpindex,     index,      date0,       &
       lalo,         contfrac,     neighbours,   resolution, zlev,        &
       u,            v,            qair,         t2m,        temp_air,    &
       petAcoef,     peqAcoef,     petBcoef,     peqBcoef,                &
       precip_rain,  precip_snow,  lwdown,       swnet,      swdown,      &
       pb,           rest_id,      hist_id,      hist2_id,                &
       rest_id_stom, hist_id_stom, hist_id_stom_IPCC,                     &
       coastalflow,  riverflow,    tsol_rad,     vevapp,       qsurf_out, &
       z0_out,       albedo_out,   fluxsens,     fluxlat,      emis_out,  &
       netco2flux,   fco2_lu,      temp_sol_new,  tq_cdrag)

!! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                               :: kjit              !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)                               :: kjpij             !! Total size of the un-compressed grid 
                                                                                  !! (unitless)
    INTEGER(i_std), INTENT(in)                               :: kjpindex          !! Domain size - terrestrial pixels only 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT (in)                               :: rest_id           !! _Restart_ file identifier (unitless)
    INTEGER(i_std),INTENT (in)                               :: hist_id           !! _History_ file identifier (unitless)
    INTEGER(i_std),INTENT (in)                               :: hist2_id          !! _History_ file 2 identifier (unitless)
    INTEGER(i_std),INTENT (in)                               :: rest_id_stom      !! STOMATE's _Restart_ file identifier 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT (in)                               :: hist_id_stom      !! STOMATE's _History_ file identifier 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT(in)                                :: hist_id_stom_IPCC !! STOMATE's IPCC _history_ file file 
                                                                                  !! identifier (unitless)
    REAL(r_std), INTENT (in)                                 :: date0             !! Initial date (??unit??)
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (in)          :: lalo              !! Geographic coordinates (latitude,longitude)
                                                                                  !! for grid cells (degrees)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: contfrac          !! Fraction of continent in the grid 
                                                                                  !! (unitless, 0-1)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: index             !! Indices of the pixels on the map. 
                                                                                  !! Sechiba uses a reduced grid excluding oceans
                                                                                  !! ::index contains the indices of the 
                                                                                  !! terrestrial pixels only! (unitless)
    INTEGER(i_std), DIMENSION (kjpindex,8), INTENT(in)       :: neighbours        !! Neighboring grid points if land!(unitless)
    REAL(r_std), DIMENSION (kjpindex,2), INTENT(in)          :: resolution        !! Size in x and y of the grid (m)
    
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: u                 !! Lowest level wind speed in direction u 
                                                                                  !! @tex $(m.s^{-1})$ @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: v                 !! Lowest level wind speed in direction v 
                                                                                  !! @tex $(m.s^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: zlev              !! Height of first layer (m)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: qair              !! Lowest level specific humidity 
                                                                                  !! @tex $(kg kg^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: t2m               !! 2m air temperature (K)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: precip_rain       !! Rain precipitation 
                                                                                  !! @tex $(kg m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: precip_snow       !! Snow precipitation 
                                                                                  !! @tex $(kg m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: lwdown            !! Down-welling long-wave flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: swnet             !! Net surface short-wave flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: swdown            !! Down-welling surface short-wave flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: temp_air          !! Air temperature (K)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: petAcoef          !! Coefficients A for T from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: peqAcoef          !! Coefficients A for q from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: petBcoef          !! Coefficients B for T from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: peqBcoef          !! Coefficients B for q from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: pb                !! Surface pressure (hPa)


!! 0.2 Output variables
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: coastalflow       !! Outflow on coastal points by small basins.
                                                                                  !! This is the water which flows in a disperse 
                                                                                  !! way into the ocean
                                                                                  !! @tex $(kg dt_routing^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: riverflow         !! Outflow of the major rivers.
                                                                                  !! The flux will be located on the continental 
                                                                                  !! grid but this should be a coastal point  
                                                                                  !! @tex $(kg dt_routing^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: tsol_rad          !! Radiative surface temperature 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: vevapp            !! Total of evaporation 
                                                                                  !! @tex $(kg m^{-2} days^{-1})$ @endtex
    
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: qsurf_out         !! Surface specific humidity 
                                                                                  !! @tex $(kg kg^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: z0_out            !! Surface roughness (output diagnostic, m)
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (out)         :: albedo_out        !! VIS and NIR albedo (output diagnostic, 
                                                                                  !! unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: fluxsens          !! Sensible heat flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: fluxlat           !! Latent heat flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: emis_out          !! Emissivity (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: netco2flux        !! Sum CO2 flux over PFTs 
                                                                                  !! ??(gC m^{-2} s^{-1})??
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: fco2_lu           !! Land Cover Change CO2 flux 
                                                                                  !! ??(gC m^{-2} s^{-1})??
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: temp_sol_new      !! New ground temperature (K)
!    REAL(r_std),DIMENSION (kjpindex, nvm), INTENT (out)      :: temp_sol_new_pft  !! New ground temperature (K)

!! 0.3 Modified
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)         :: tq_cdrag          !! Surface drag coefficient 
                                                                                  !! @tex $(m.s^{-1})$ @endtex 

!! 0.4 Local variables
    INTEGER(i_std)                                           :: ji, jv		  !! Index (unitless)
    INTEGER(i_std)                                           :: ier
    REAL(r_std), DIMENSION(kjpindex)                         :: histvar           !! Computations for history files (unitless)
    CHARACTER(LEN=80)                                        :: var_name          !! To store variables names for I/O (unitless)
    REAL(r_std), DIMENSION(kjpindex)                         :: frac_snow_veg     !! Snow cover fraction on vegetation, 
                                                                                  !! only for diagnostics (unitless)
    REAL(r_std), DIMENSION(kjpindex,nnobio)                  :: frac_snow_nobio   !! Snow cover fraction on continental ice, lakes, etc
                                                                                  !! only for diagnostics (unitless)

    REAL(r_std), DIMENSION(kjpindex,nslm,nvm)                :: mc_layh_pft
    REAL(r_std), DIMENSION(kjpindex,nslm,nvm)                :: mcl_layh_pft
    REAL(r_std), DIMENSION(kjpindex,nslm,nvm)                :: tmc_layh_pft
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) ' sechiba kjpindex =',kjpindex

    !! 1. Initialize variables on first call

    !! 1.1 Initialize most of sechiba's variables
    CALL sechiba_init (kjit, kjpij, kjpindex, index, rest_id, lalo)
    
    !! 1.3 Initialize stomate's variables
    CALL slowproc_initialize (kjit,     kjpij,          kjpindex,       date0,          &      
                        index,          indexveg,       lalo,           neighbours,     &
                        resolution,     contfrac,       soiltile,       reinf_slope,    &
                        t2m,                                                            &            
                        deadleaf_cover, assim_param,    lai,            frac_age,       &
                        height,         veget,          frac_nobio,     njsc,           & 
                        veget_max,      totfrac_nobio,  qsintmax,       rest_id,        &
                        rest_id_stom,   hist_id_stom,   tot_bare_soil,                  &
                        hist_id_stom_IPCC, co2_flux,    fco2_lu,        temp_growth,    &
                        soilc_total,    thawed_humidity, depth_organic_soil,   heat_Zimov, f_rot_sech )

    netco2flux(:) = zero
    DO jv = 2,nvm
       netco2flux(:) = netco2flux(:) + co2_flux(:,jv)*veget_max(:,jv)
    ENDDO
    
    !!! xuhui: allocation of rot_cmd has to be after slowproc_initialize
    !!! due to possible change of rot_cmd_max after reading rotation map
    ALLOCATE(rot_cmd(kjpindex,rot_cmd_max),stat=ier)
    IF (ier/=0) THEN
        WRITE(numout,*) "ERROR IN ALLOCATION OF rot_cmd: ",ier
        STOP 'sechiba_initialize rot_cmd allocation'
    ENDIF
    rot_cmd(:,:) = 0
    
    !! 1.4 Initialize diffusion coefficients
    CALL diffuco_initialize (kjit,    kjpindex, index,                  &
                             rest_id, lalo,     neighbours, resolution, &
                             rstruct, tq_cdrag, tq_cdrag_pft)
    
    !! 1.5 Initialize remaining variables of energy budget
    CALL enerbil_initialize (kjit,     kjpindex,     index,    rest_id,  &
                             temp_air, qair,                             &
                             temp_sol, temp_sol_pft, temp_sol_new, temp_sol_new_pft, tsol_rad, t2mdiag,  &
                             evapot,   evapot_corr,  qsurf,    fluxsens, &
                             fluxlat,  vevapp )
    

    !! 1.7 Initialize remaining hydrological variables
    IF ( .NOT. hydrol_cwrr ) THEN
       ! 1.7.1 Initialize remaining hydrological variables from Choisnel module (2 soil layers)
       
        CALL hydrolc_initialize( kjit,     kjpindex,     index,          rest_id,   &
                                  veget,     veget_max,  tot_bare_soil,             &
                                  rsol,      drysoil_frac, snow,                    &
                                  snow_age,  snow_nobio,   snow_nobio_age, humrel,  &
                                  vegstress, qsintveg,     shumdiag,       snowrho, &
                                  snowtemp,  snowgrain,      snowdz,  &
                                  snowheat ) 
     
       evap_bare_lim(:) = -un
       k_litt(:) = huit
       
       ! No specific calculation for shumdiag_perma. We assume it to shumdiag.
       shumdiag_perma(:,:)=shumdiag(:,:)
    ELSE
       !! 1.7.2 Initialize remaining hydrological variables from CWRR module (11 soil layers)
          CALL hydrol_initialize (kjit,           kjpindex,  index,         rest_id,          &
                                  njsc,           soiltile,  veget,         veget_max,        &
                                  humrel,         vegstress, drysoil_frac,                    &
                                  shumdiag_perma, k_litt,    qsintveg,                        &
!                                  evap_bare_lim,  evap_bare_lim_pft, snow,      snow_age,      snow_nobio,       &
                                  evap_bare_lim,  snow,      snow_age,      snow_nobio,       &
                                  snow_nobio_age, snowrho,   snowtemp,                        &
                                  snowgrain,      snowdz,    snowheat,                        &
                                  fwet_out,                                   &
                                  totfrac_nobio,  precip_rain, precip_snow, returnflow,       &
                                  reinfiltration, irrigation,tot_melt, vevapwet,              &
                                  transpir,       vevapnu,   vevapsno, vevapflo,              &
                                  floodout,       runoff,    drainage,                        &
                                  mc_layh,        mcl_layh,  tmc_layh,                        &
                                  mc_layh_s,      mcl_layh_s, tmc_layh_s )
    ENDIF
    
    !! 1.9 Initialize surface parameters (emissivity, albedo and roughness)
       CALL condveg_initialize (kjit, kjpindex, index, veget, & 
            veget_max, frac_nobio, totfrac_nobio, &
            lalo, neighbours, resolution, contfrac, rest_id, &
            zlev, drysoil_frac, height, snowdz, snowrho, tot_bare_soil, &
            snow, snow_age, snow_nobio, snow_nobio_age, &
            emis, albedo, z0, roughheight, roughheight_pft, &
            frac_snow_veg, frac_snow_nobio)
    
    !! 1.10 Initialization of soil thermodynamics
    IF (hydrol_cwrr) THEN
       is_crop_soil(:) = .FALSE.
       !!!!! transfer  mc_layh_s,      mcl_layh_s, tmc_layh_s
       !!!!!       to  mc_layh_pft,    mcl_layh_pft, tmc_layh_pft
       DO jv = 1,nvm
            mc_layh_pft(:,:,jv) = mc_layh_s(:,:,pref_soil_veg(jv))
            mcl_layh_pft(:,:,jv) = mcl_layh_s(:,:,pref_soil_veg(jv))
            tmc_layh_pft(:,:,jv) = tmc_layh_s(:,:,pref_soil_veg(jv))
!!            IF (ok_LAIdev(jv) ) THEN
            IF (ok_LAIdev(jv) .OR. (.NOT. natural(jv)) ) THEN !! try that for general crop
                is_crop_soil(pref_soil_veg(jv)) = .TRUE.
            ENDIF
       ENDDO
       IF (printlev >=4) THEN
           WRITE(numout,*) 'xuhui: initialized'
           WRITE(numout,*) 'is_crop_soil', is_crop_soil
           WRITE(numout,*) 'tmc_layh(1,1)', tmc_layh(1,1)
           WRITE(numout,*) 'tmc_layh_pft(1,1,:)', tmc_layh_pft(1,1,:)
           WRITE(numout,*) 'tmc_layh(1,nslm)', tmc_layh(1,nslm)
           WRITE(numout,*) 'tmc_layh_pft(1,nslm,:)', tmc_layh_pft(1,nslm,:)
       ENDIF
       CALL thermosoil_initialize (kjit,  kjpindex, lalo, rest_id, veget_max, &
            shumdiag_perma, snow,               thawed_humidity,  soilc_total, &
            temp_sol_new,   temp_sol_new_pft, depth_organic_soil, stempdiag, &
            soilcap,     soilcap_pft,    soilflx,   soilflx_pft,         gtemp, &
            mc_layh,        mcl_layh,           tmc_layh,   mc_layh_pft, mcl_layh_pft, tmc_layh_pft,      njsc, &
            frac_snow_veg,  frac_snow_nobio,    totfrac_nobio,     &
            snowdz,         snowrho,            snowtemp,          &
            lambda_snow,    cgrnd_snow,         dgrnd_snow,       pb)
    ELSE
       CALL thermosoilc_initialize (kjit,  kjpindex, lalo, rest_id, veget_max, &
            snowdz,        shumdiag_perma, snow,  thawed_humidity,  soilc_total, &
            temp_sol_new,  depth_organic_soil,    stempdiag, &
            soilcap,       soilflx,               gtemp ,    & 
            frac_snow_veg, frac_snow_nobio,       totfrac_nobio,  &
            snowrho,       snowtemp,          &
            lambda_snow,    cgrnd_snow,         dgrnd_snow,       pb)

    END IF

    !! 1.12 Initialize river routing
    IF ( river_routing .AND. nbp_glo .GT. 1) THEN
       !! 1.12.1 Initialize river routing
       CALL routing_initialize( kjit,        kjpindex,       index,                 &
            rest_id,     hist_id,        hist2_id,   lalo,      &
            neighbours,  resolution,     contfrac,   stempdiag, &
            returnflow,  reinfiltration, irrigation, riverflow, &
            coastalflow, flood_frac,     flood_res )
    ELSE
       !! 1.12.2 No routing, set variables to zero
       riverflow(:) = zero
       coastalflow(:) = zero
       returnflow(:) = zero
       reinfiltration(:) = zero
       irrigation(:) = zero
       flood_frac(:) = zero
       flood_res(:) = zero
    ENDIF

    IF (do_fullirr) THEN ! compulsory irrigation
        CALL restget_p(rest_id, 'irrigation', nbp_glo, 1, 1, kjit, .TRUE., irrigation, "gather", nbp_glo, index_g)
          IF (.NOT. (MINVAL(irrigation) < MAXVAL(irrigation) .OR. MAXVAL(irrigation) < val_exp)) THEN
             irrigation(:) = zero
          ENDIF

        CALL restget_p(rest_id, 'irrigr', nbp_glo, nstm, 1, kjit, .TRUE., irrigr, "gather", nbp_glo, index_g)
            IF (.NOT. (MINVAL(irrigr) < MAXVAL(irrigr) .OR. MAXVAL(irrigr) < val_exp)) THEN
                irrigr(:,:) = zero
            ENDIF
    ENDIF

    !! 1.13 Write internal variables to output fields
    z0_out(:) = z0(:)
    emis_out(:) = emis(:)
    albedo_out(:,:) = albedo(:,:) 
    qsurf_out(:) = qsurf(:)

  END SUBROUTINE sechiba_initialize

!! ==============================================================================================================================\n
!! SUBROUTINE 	: sechiba_main
!!
!>\BRIEF        Main routine for the sechiba module performing three functions:
!! calculating temporal evolution of all variables and preparation of output and 
!! restart files (during the last call only)
!!
!!\n DESCRIPTION : Main routine for the sechiba module. 
!! One time step evolution consists of:
!! - call sechiba_var_init to do some initialization,
!! - call slowproc_main to do some daily calculations
!! - call diffuco_main for diffusion coefficient calculation,
!! - call enerbil_main for energy budget calculation,
!! - call hydrolc_main (or hydrol_main) for hydrologic processes calculation,
!! - call enerbil_fusion : last part with fusion,
!! - call condveg_main for surface conditions such as roughness, albedo, and emmisivity,
!! - call thermosoil_main(for cwrr) or thermosoilc_main(for choisnel) for soil thermodynamic calculation,
!! - call sechiba_end to swap previous to new fields.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): Hydrological variables (:: coastalflow and :: riverflow),
!! components of the energy budget (:: tsol_rad, :: vevapp, :: fluxsens, 
!! :: temp_sol_new and :: fluxlat), surface characteristics (:: z0_out, :: emis_out, 
!! :: tq_cdrag and :: albedo_out) and land use related CO2 fluxes (:: netco2flux and 
!! :: fco2_lu)            
!!
!! REFERENCE(S)	: 
!!
!! FLOWCHART    : 
!! \latexonly 
!! \includegraphics[scale = 0.5]{sechibamainflow.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE sechiba_main (kjit, kjpij, kjpindex, index, date0, &
       & ldrestart_read, ldrestart_write, &
       & lalo, contfrac, neighbours, resolution,&
       & zlev, u, v, qair, q2m, t2m, temp_air, epot_air, ccanopy, &
       & tq_cdrag, petAcoef, peqAcoef, petBcoef, peqBcoef, &
       & precip_rain, precip_snow, lwdown, swnet, swdown, coszang, pb, &
       & vevapp, fluxsens, fluxlat, coastalflow, riverflow, netco2flux, fco2_lu, &
       & tsol_rad, temp_sol_new, qsurf_out, albedo_out, emis_out, z0_out, &
       & rest_id, hist_id, hist2_id, rest_id_stom, hist_id_stom, hist_id_stom_IPCC)

!! 0.1 Input variables
    
    INTEGER(i_std), INTENT(in)                               :: kjit              !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)                               :: kjpij             !! Total size of the un-compressed grid 
                                                                                  !! (unitless)
    INTEGER(i_std), INTENT(in)                               :: kjpindex          !! Domain size - terrestrial pixels only 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT (in)                               :: rest_id           !! _Restart_ file identifier (unitless)
    INTEGER(i_std),INTENT (in)                               :: hist_id           !! _History_ file identifier (unitless)
    INTEGER(i_std),INTENT (in)                               :: hist2_id          !! _History_ file 2 identifier (unitless)
    INTEGER(i_std),INTENT (in)                               :: rest_id_stom      !! STOMATE's _Restart_ file identifier 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT (in)                               :: hist_id_stom      !! STOMATE's _History_ file identifier 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT(in)                                :: hist_id_stom_IPCC !! STOMATE's IPCC _history_ file file 
                                                                                  !! identifier (unitless)
    REAL(r_std), INTENT (in)                                 :: date0             !! Initial date (??unit??)
    LOGICAL, INTENT(in)                                      :: ldrestart_read    !! Logical for _restart_ file to read 
                                                                                  !! (true/false)
    LOGICAL, INTENT(in)                                      :: ldrestart_write   !! Logical for _restart_ file to write 
                                                                                  !! (true/false)
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (in)          :: lalo              !! Geographic coordinates (latitude,longitude)
                                                                                  !! for grid cells (degrees)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: contfrac          !! Fraction of continent in the grid 
                                                                                  !! (unitless, 0-1)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: index             !! Indices of the pixels on the map. 
                                                                                  !! Sechiba uses a reduced grid excluding oceans
                                                                                  !! ::index contains the indices of the 
                                                                                  !! terrestrial pixels only! (unitless)
    INTEGER(i_std), DIMENSION (kjpindex,8), INTENT(in)       :: neighbours        !! Neighboring grid points if land!(unitless)
    REAL(r_std), DIMENSION (kjpindex,2), INTENT(in)          :: resolution        !! Size in x and y of the grid (m)
    
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: u                 !! Lowest level wind speed in direction u 
                                                                                  !! @tex $(m.s^{-1})$ @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: v                 !! Lowest level wind speed in direction v 
                                                                                  !! @tex $(m.s^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: zlev              !! Height of first layer (m)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: qair              !! Lowest level specific humidity 
                                                                                  !! @tex $(kg kg^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: q2m               !! 2m specific humidity 
                                                                                  !! @tex $(kg kg^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: t2m               !! 2m air temperature (K)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: precip_rain       !! Rain precipitation 
                                                                                  !! @tex $(kg m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: precip_snow       !! Snow precipitation 
                                                                                  !! @tex $(kg m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: lwdown            !! Down-welling long-wave flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: coszang           !! Cosine of the solar zenith angle (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: swnet             !! Net surface short-wave flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: swdown            !! Down-welling surface short-wave flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: temp_air          !! Air temperature (K)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: epot_air          !! Air potential energy (??J)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: ccanopy           !! CO2 concentration in the canopy (ppm)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: petAcoef          !! Coefficients A for T from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: peqAcoef          !! Coefficients A for q from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: petBcoef          !! Coefficients B for T from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: peqBcoef          !! Coefficients B for q from the Planetary 
                                                                                  !! Boundary Layer
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: pb                !! Surface pressure (hPa)


!! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: coastalflow       !! Outflow on coastal points by small basins.
                                                                                  !! This is the water which flows in a disperse 
                                                                                  !! way into the ocean
                                                                                  !! @tex $(kg dt_routing^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: riverflow         !! Outflow of the major rivers.
                                                                                  !! The flux will be located on the continental 
                                                                                  !! grid but this should be a coastal point  
                                                                                  !! @tex $(kg dt_routing^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: tsol_rad          !! Radiative surface temperature 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: vevapp            !! Total of evaporation 
                                                                                  !! @tex $(kg m^{-2} days^{-1})$ @endtex
    
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: qsurf_out         !! Surface specific humidity 
                                                                                  !! @tex $(kg kg^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: z0_out            !! Surface roughness (output diagnostic, m)
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (out)         :: albedo_out        !! VIS and NIR albedo (output diagnostic, 
                                                                                  !! unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: fluxsens          !! Sensible heat flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: fluxlat           !! Latent heat flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: emis_out          !! Emissivity (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: netco2flux        !! Sum CO2 flux over PFTs 
                                                                                  !! ??(gC m^{-2} s^{-1})??
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)           :: fco2_lu           !! Land Cover Change CO2 flux 
                                                                                  !! ??(gC m^{-2} s^{-1})??

!! 0.3 Modified

    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)         :: tq_cdrag          !! Surface drag coefficient 
                                                                                  !! @tex $(m.s^{-1})$ @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)         :: temp_sol_new      !! New ground temperature (K)

!! 0.4 local variables

    INTEGER(i_std)                                           :: ji, jv		  !! Index (unitless)
    INTEGER(i_std)                                           :: jsrc,jtar     !! Index (unitless)
    INTEGER(i_std)                                           :: stveg, edveg
    REAL(r_std), DIMENSION(kjpindex)                         :: histvar           !! Computations for history files (unitless)
    CHARACTER(LEN=80)                                        :: var_name          !! To store variables names for I/O (unitless)
    REAL(r_std), DIMENSION(kjpindex)                         :: sum_treefrac      !! Total fraction occupied by trees (0-1, uniless) 
    REAL(r_std), DIMENSION(kjpindex)                         :: sum_grassfrac     !! Total fraction occupied by grasses (0-1, unitless)
    REAL(r_std), DIMENSION(nvm)                              :: temp_veget_max
    REAL(r_std), DIMENSION(nvm)                              :: old_veget_max
    REAL(r_std), DIMENSION(nvm,nvm)                          :: matrix_rot     !! temporary matrix for rotation
    REAL(r_std)                                              :: rotprc
    REAL(r_std), DIMENSION(kjpindex)                         :: sum_cropfrac      !! Total fraction occcupied by crops (0-1, unitess)
    REAL(r_std), DIMENSION(kjpindex,nsnow)                   :: snowliq           !! Liquid water content (m)
    REAL(r_std), DIMENSION(kjpindex)                         :: frac_snow_veg     !! Snow cover fraction on vegetation,
                                                                                  !! only for diagnostics (unitless)
    REAL(r_std), DIMENSION(kjpindex,nnobio)                  :: frac_snow_nobio   !! Snow cover fraction on continental ice, lakes, etc
                                                                                  !! only for diagnostics (unitless)
    REAL(r_std), DIMENSION(kjpindex)                         :: grndflux          !! Net energy into soil (W/m2)
    REAL(r_std),DIMENSION (kjpindex)                         :: wspeed            !! Lowest level Wind speed
    REAL(r_std), DIMENSION(kjpindex)                         :: temp_transpot_agr !! potential transpiration by crops need irrigation
    REAL(r_std)                                              :: temp_irrig_need
    REAL(r_std)                                              :: tempfrac
    INTEGER(i_std)                                           :: ier, k

    REAL(r_std), DIMENSION(kjpindex,nslm,nvm)                :: mc_layh_pft
    REAL(r_std), DIMENSION(kjpindex,nslm,nvm)                :: mcl_layh_pft
    REAL(r_std), DIMENSION(kjpindex,nslm,nvm)                :: tmc_layh_pft

    REAL(r_std), DIMENSION(nvm)                              :: temp_temp_sol, temp_soilcap, temp_soilflx
    REAL(r_std), DIMENSION(nvm)                              :: dilu_temp, dilu_soilcap, dilu_soilflx
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) ' sechiba kjpindex =',kjpindex

    !! 1. Initialize variables at each time step
    CALL sechiba_var_init (kjpindex, rau, pb, temp_air) 

    !! 2. Compute diffusion coefficients
    CALL diffuco_main (kjit, kjpindex, index, indexveg, indexlai, u, v, &
         & zlev, z0, roughheight, roughheight_pft, temp_sol, temp_sol_pft, temp_air, temp_growth, rau, tq_cdrag, tq_cdrag_pft, &
         & qsurf, qair, q2m, t2m, pb ,  &
!         & rsol, evap_bare_lim, evap_bare_lim_pft, evapot, evapot_corr, snow, flood_frac, flood_res, frac_nobio, snow_nobio, totfrac_nobio, &
         & rsol, evap_bare_lim, evapot, evapot_corr, snow, flood_frac, flood_res, frac_nobio, snow_nobio, totfrac_nobio, &
         & swnet, swdown, coszang, ccanopy, humrel, veget, veget_max, lai, qsintveg, qsintmax, assim_param, &
         & vbeta, vbeta_pft, valpha, vbeta1, vbeta2, vbeta3, vbeta3pot, vbeta4, vbeta4_pft, vbeta5, gsmean, rveget, rstruct, cimean, gpp, &
         & lalo, neighbours, resolution, ptnlev1, precip_rain, frac_age, tot_bare_soil, &
         & hist_id, hist2_id)
    
!    WRITE(numout,*) 'xuhui: before enerbil_main:'
!    WRITE(numout,*) 'temp_sol(1)',temp_sol(1)
!    WRITE(numout,*) 'temp_sol(1,:)', temp_sol_pft(0,:)
!    WRITE(numout,*) 'vbeta(1)', vbeta(1)
!    WRITE(numout,*) 'vbeta_pft(1,:)', vbeta_pft(1,:)
    !! 3. Compute energy balance
    CALL enerbil_main (kjit, kjpindex, &
         & index, indexveg, zlev, lwdown, swnet, epot_air, temp_air, u, v, petAcoef, petBcoef, &
         & qair, peqAcoef, peqBcoef, pb, rau, vbeta, vbeta_pft, valpha, vbeta1, vbeta2, vbeta3, vbeta3pot, vbeta4, vbeta4_pft,  vbeta5, &
         & emis, soilflx, soilflx_pft,  soilcap, soilcap_pft, tq_cdrag, tq_cdrag_pft, veget_max, humrel, fluxsens, fluxlat, &
         & vevapp, transpir, transpot, vevapnu, vevapnu_pft, vevapwet, vevapsno, vevapflo, t2mdiag, temp_sol, temp_sol_pft, tsol_rad, &
         & temp_sol_new, temp_sol_new_pft, qsurf, evapot, evapot_corr, rest_id, hist_id, hist2_id,&
         & precip_rain,snowdz,snowrho,pgflux,temp_sol_add)

!    WRITE(numout,*) 'xuhui: after enerbil_main:'
!    WRITE(numout,*) 'temp_sol(1)',temp_sol(1)
!    WRITE(numout,*) 'temp_sol(1,:)', temp_sol_pft(1,:)
!    WRITE(numout,*) 'temp_sol_new(1)', temp_sol_new(1)
!    WRITE(numout,*) 'temp_sol_new_pft(1,:)', temp_sol_new_pft(1,:)

    !! 4. Compute hydrology
    IF ( .NOT. hydrol_cwrr ) THEN
       ! 4.1 Water balance from Choisnel module (2 soil layers)
       vegstress_old = vegstress
       CALL hydrolc_main (kjit, kjpindex, index, indexveg, &
            & temp_sol_new, floodout, runoff, drainage, frac_nobio, totfrac_nobio, vevapwet, veget, veget_max,&
            & qsintmax, qsintveg, vevapnu, vevapsno, vevapflo, snow, snow_age, snow_nobio, snow_nobio_age,&
            & tot_melt, transpir, precip_rain, precip_snow, returnflow, reinfiltration, irrigation, vegstress_old, transpot, humrel, &
            & vegstress, rsol, drysoil_frac, evapot, evapot_corr, flood_frac, flood_res, shumdiag, litterhumdiag, &
            & soilcap, rest_id, hist_id, hist2_id, soil_deficit, &
            & temp_air, pb, u, v, pgflux, &
            & snowrho,snowtemp,snowgrain,snowdz,snowheat,snowliq, &
            & grndflux, gtemp, tot_bare_soil, soilflxresid,       &
              lambda_snow, cgrnd_snow, dgrnd_snow, temp_sol_add)

       evap_bare_lim(:) = -un
       k_litt(:) = huit
       
       ! No specific calculation for shumdiag_perma. We assume it to shumdiag.
       shumdiag_perma(:,:)=shumdiag(:,:)
    ELSE
       !! 4.1 Water balance from CWRR module (11 soil layers)
       vegstress_old = vegstress
       CALL hydrol_main (kjit, kjpindex, &
            & index, indexveg, indexsoil, indexlayer, indexnbdl, &
            & temp_sol_new, floodout, runoff, drainage, frac_nobio, totfrac_nobio, vevapwet, veget, veget_max, njsc, &
            & qsintmax, qsintveg, vevapnu, vevapnu_pft, vevapsno, vevapflo, snow, snow_age, snow_nobio, snow_nobio_age,  &
            & tot_melt, transpir, precip_rain, precip_snow, returnflow, reinfiltration, irrigation, vegstress_old, transpot, &
!            & humrel, vegstress, drysoil_frac, evapot, evapot_corr, evap_bare_lim, evap_bare_lim_pft, flood_frac, flood_res, &
            & humrel, vegstress, drysoil_frac, evapot, evapot_corr, evap_bare_lim, flood_frac, flood_res, &
            & shumdiag,shumdiag_perma, k_litt, litterhumdiag, soilcap, soiltile, reinf_slope,&
            & rest_id, hist_id, hist2_id, soil_deficit, is_crop_soil, &
            & stempdiag, temp_air, pb, u, v, tq_cdrag, pgflux, &
            & snowrho, snowtemp, snowgrain, snowdz, snowheat, snowliq, &
            & grndflux, gtemp, tot_bare_soil, &
            & soilflxresid, mc_layh, mcl_layh, tmc_layh, mc_layh_s, mcl_layh_s, tmc_layh_s, &
            & drunoff_tot, fwet_out, lambda_snow, cgrnd_snow, dgrnd_snow, temp_sol_add, irrigr, &
            & do_irrig_reservoir, reservoir_dep_max, irrig_dep_threshold, do_awd, awd_dep, lai)

       rsol(:) = -un

    ENDIF
         
    !! 5. Compute remaining components of the energy balance
    IF ( .NOT. ok_explicitsnow ) THEN 
!!! this means that my code is not compatible with explicit snow due to missing
!soilcap_pft and temp_sol_new_pft when explicitsnow activated
       CALL enerbil_fusion (kjpindex, tot_melt, soilcap, soilcap_pft,  snowdz, &
            temp_sol_new, temp_sol_new_pft, fusion)
    END IF

    !! 6. Compute surface variables (emissivity, albedo and roughness)
    CALL condveg_main (kjit, kjpindex, index,&
         & lalo, neighbours, resolution, contfrac, veget, veget_max, frac_nobio, totfrac_nobio, &
         & zlev, snow, snow_age, snow_nobio, snow_nobio_age, tot_bare_soil, &
         & drysoil_frac, height, snowdz, snowrho, emis, albedo, frac_snow_veg, frac_snow_nobio, &
         & z0, roughheight, roughheight_pft, rest_id, hist_id, hist2_id)

    !! 7. Compute soil thermodynamics
    IF (hydrol_cwrr) THEN      
       !!!!! transfer  mc_layh_s,      mcl_layh_s, tmc_layh_s
       !!!!!       to  mc_layh_pft,    mcl_layh_pft, tmc_layh_pft
       DO jv = 1,nvm
            mc_layh_pft(:,:,jv) = mc_layh_s(:,:,pref_soil_veg(jv))
            mcl_layh_pft(:,:,jv) = mcl_layh_s(:,:,pref_soil_veg(jv))
            tmc_layh_pft(:,:,jv) = tmc_layh_s(:,:,pref_soil_veg(jv))
       ENDDO

       CALL thermosoil_main (kjit, kjpindex, &
            index, indexgrnd, &
            temp_sol_new, temp_sol_new_pft, snow, soilcap, soilcap_pft, soilflx, soilflx_pft, shumdiag_perma, stempdiag, &
            ptnlev1, hist_id, hist2_id, &
            snowdz, snowrho, snowtemp, gtemp, pb, &
            mc_layh, mcl_layh, tmc_layh, mc_layh_pft, mcl_layh_pft, tmc_layh_pft,  njsc, &
            thawed_humidity, depth_organic_soil, heat_Zimov, tdeep, hsdeep,&
            soilc_total, veget_max, &
            frac_snow_veg,frac_snow_nobio,totfrac_nobio, temp_sol_add, &
            lambda_snow, cgrnd_snow, dgrnd_snow) 
    ELSE
       CALL thermosoilc_main (kjit, kjpindex, &
            index, indexgrnd,indexnbdl, &
            temp_sol_new, snow, soilcap, soilflx, shumdiag_perma, stempdiag, &
            ptnlev1, hist_id, hist2_id, &
            snowdz, snowrho, snowtemp, gtemp, pb, &
            thawed_humidity, depth_organic_soil, heat_Zimov, tdeep, hsdeep,&
            soilc_total, veget_max, &
            frac_snow_veg,frac_snow_nobio,totfrac_nobio,temp_sol_add, &
            lambda_snow, cgrnd_snow, dgrnd_snow)
    END IF


    !! 8. Compute river routing 
    IF ( river_routing .AND. nbp_glo .GT. 1) THEN
       !! 8.1 River routing
       CALL routing_main (kjit, kjpindex, index, &
            & lalo, neighbours, resolution, contfrac, totfrac_nobio, veget, veget_max, soil_deficit, floodout, runoff, &
            & drainage, transpot, evapot_corr, vegstress, precip_rain, humrel, k_litt, flood_frac, flood_res, &
            & stempdiag, reinf_slope, returnflow, reinfiltration, irrigation, riverflow, coastalflow, rest_id, hist_id, &
            & hist2_id, irrigr, do_irrig_reservoir, irrig_dep_threshold, reservoir_dep_max, do_awd, awd_dep, lai)
    ELSE
       !! 8.2 No routing, set variables to zero
       riverflow(:) = zero
       coastalflow(:) = zero
       returnflow(:) = zero
       reinfiltration(:) = zero
!       irrigation(:) = zero
       flood_frac(:) = zero
       flood_res(:) = zero

       IF ( do_fullirr ) THEN
           DO ji = 1, kjpindex
               temp_irrig_need = zero
               DO jv = 2,nvm
                   !IF ( veget_max(ji,jv) .GT. 0 ) THEN
                   IF (lai(ji,jv) .GT. 0) THEN
                       tempfrac = veget(ji,jv)/veget_max(ji,jv)
                       IF (tempfrac .LE. 0) THEN
                           tempfrac = 0
                       ENDIF
                       !IF ( ok_LAIdev(jv) .AND. do_irrig_reservoir(pref_soil_veg(jv)) .AND. (irrigr(pref_soil_veg(jv)) .LT. irrig_threshold(jv))  ) THEN
                       !IF ( ok_LAIdev(jv) .AND. do_irrig_reservoir() .AND. (vegstress(ji,jv) .LT. irrig_threshold(jv))  ) THEN
                       IF (ok_LAIdev(jv) .AND. do_irrig_reservoir(pref_soil_veg(jv))) THEN
                        !flooding but with reservior
                            IF (do_awd(pref_soil_veg(jv))) THEN
                                ! do AWD irrigation
                                IF ( vegstress(ji,jv) .LT. irrig_threshold(jv)) THEN
                                    IF ( awd_dep(pref_soil_veg(jv)) .LT. zero) THEN
                                    ! use default deficit to define irrigation
                                    ! amount    
                                         temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                                        & MIN( irrig_dosmax, MAX( zero, &
                                                        & soil_deficit(ji,jv) )) * veget_max(ji,jv)
                                        !temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                        !& awd_dep(pref_soil_veg(jv)) * veget_max(ji,jv)
                                    ELSE
                                    ! use defined depth for irrigation water amount
                                        temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                        & (irrig_dep_threshold(pref_soil_veg(jv)) - irrigr(ji,pref_soil_veg(jv))) * veget_max(ji,jv)
                                    ENDIF
                                ENDIF
                            ELSEIF (irrigr(ji,pref_soil_veg(jv)) .LT. irrig_dep_threshold(pref_soil_veg(jv))) THEN
                                ! not AWD, but continuous irrigation
                                temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                & (irrig_dep_threshold(pref_soil_veg(jv)) - irrigr(ji,pref_soil_veg(jv))) * veget_max(ji,jv)
                            ENDIF
                       ELSEIF (ok_LAIdev(jv) .AND. (vegstress(ji,jv) .LT. irrig_threshold(jv))) THEN
                       !dripping or normal flooding
                           IF (irrig_drip) THEN
                                  temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                                    & MIN(irrig_dosmax, MAX( zero, &
    !                                                & transpot(ji,jv)*tempfrac + evapot_corr(ji)*(1-tempfrac) - precip_rain(ji) ) ) &
                                                    & transpot(ji,jv)*tempfrac  - (precip_rain(ji) + reinfiltration(ji))) ) &
                                                    & * veget_max(ji,jv)
                                                    !!!! reconsider if evapot or evapot_corr to be used as irrigation demand
                                                    !!!! this also affects the calc of irrig_demand_ratio in hydrol.f90
                                                    !!!!! consider adding re-infiltration into this formula, xuhui
                           ELSE ! flooding without irrigation reservoir
                                 temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                                    & MIN( irrig_dosmax, MAX( zero, &
                                                    & soil_deficit(ji,jv) )) * veget_max(ji,jv)
                                 !temp_irrig_need = temp_irrig_need + irrig_frac(ji) * &
                                 !                   & MIN( irrig_threshold(jv) - vegstress(ji,jv), MAX( zero, &
                                 !                   & soil_deficit(ji,jv) )) * veget_max(ji,jv)
                                                    !!!!! consider adding re-infiltration into this formula, xuhui
                           ENDIF
                       ENDIF
                   ENDIF
               ENDDO
               IF (temp_irrig_need .LT. 0) THEN
                   !write(*,*) 'sechiba irrigation: negative irrigation need'
                   temp_irrig_need  = 0
               ENDIF
               irrigation(ji) = temp_irrig_need
           ENDDO
       ELSE
            irrigation(:) = zero
       ENDIF
    ENDIF

    !! 9. Compute slow processes (i.e. 'daily' and annual time step)
       !spitfire
       ! Compute wind speed for fire model
       DO ji = 1, kjpindex
          wspeed(ji) = MAX(min_wind, SQRT (u(ji)*u(ji) + v(ji)*v(ji)))
       ENDDO
       !endspit

    !! 2.9 Compute slow processes (i.e. 'daily' and annual time step)
    ! ::ok_co2 and ::ok_stomate are flags that determine whether the
    ! forcing files are written.
    CALL slowproc_main (kjit, kjpij, kjpindex, date0, &
         index, indexveg, lalo, neighbours, resolution, contfrac, soiltile, &
         t2mdiag, t2mdiag, temp_sol, stempdiag, &
         vegstress, shumdiag, litterhumdiag, precip_rain, precip_snow, &
         !spitfire
         wspeed, &
         !endspit 
         gpp, &
         deadleaf_cover, &
         assim_param, &
         lai, frac_age, height, veget, frac_nobio, njsc, veget_max, totfrac_nobio, qsintmax, &
         rest_id, hist_id, hist2_id, rest_id_stom, hist_id_stom, hist_id_stom_IPCC, &
         co2_flux, fco2_lu, temp_growth,&
         swdown, t2mdiag, evapot_corr, & !added for crops, xuhui
         tdeep, hsdeep, snow, heat_Zimov, pb, &
         sfluxCH4_deep, sfluxCO2_deep, &
         thawed_humidity, depth_organic_soil, zz_deep, zz_coef_deep, &
         soilc_total,snowdz,snowrho, tot_bare_soil, f_rot_sech, rot_cmd)

    !! 9.2 Compute global CO2 flux
    netco2flux(:) = zero
    DO jv = 2,nvm
       netco2flux(:) = netco2flux(:) + co2_flux(:,jv)*veget_max(:,jv)
    ENDDO
    !! 9.3 crop rotation 
    IF (ok_rotate) THEN
       DO ji = 1,kjpindex

         IF (f_rot_sech(ji)) THEN
            matrix_rot(:,:) = zero
            !!! do all the rotation command
            k = 1
            DO WHILE ( (rot_cmd(ji,k) .GT. 0) .AND. (k .LE. rot_cmd_max) )
                CALL sechiba_get_cmd(rot_cmd(ji,k), stveg, edveg, rotprc)
                matrix_rot(stveg,edveg) = rotprc
                k = k+1
            ENDDO
            ! The rotation command in rot_cmd has been well checked by
            ! in stomate_rotate, no further need to check here, xuhui
            !!! converting veget_max
            temp_veget_max = veget_max(ji,:)
            old_veget_max = veget_max(ji,:)
            DO jsrc = 1, nvm
                DO jtar = 1,nvm
                    IF ( (matrix_rot(jsrc,jtar) .GT. zero) .AND. (matrix_rot(jsrc,jtar) .LT. 1.0+min_sechiba) )  THEN
                        temp_veget_max(jsrc) = temp_veget_max(jsrc) - matrix_rot(jsrc,jtar) * veget_max(ji,jsrc)
                        temp_veget_max(jtar) = temp_veget_max(jtar) + matrix_rot(jsrc,jtar) * veget_max(ji,jsrc)
                    ENDIF
                ENDDO
            ENDDO
            WHERE (temp_veget_max(:) .LT. min_sechiba) 
                temp_veget_max(:) = zero
            ENDWHERE
            veget_max(ji,:) = temp_veget_max
            
            !!! transfering soil heat and water storage
            !!!!!!! veget & soiltile simple update
            veget(ji,1) = veget_max(ji,1)
            DO jv = 2,nvm
                veget(ji,jv) = veget_max(ji,jv) * (un - exp( - lai(ji,jv) * ext_coeff(jv) ) )
            ENDDO

            soiltile(ji,:) = zero
            soiltile(ji,1) = totfrac_nobio(ji)
            DO jsrc = 1,nvm
                jv = pref_soil_veg(jsrc)
                soiltile(ji,jv) = soiltile(ji,jv) + veget_max(ji,jsrc)
            ENDDO
            IF ((SUM(soiltile(ji,:)) .LT. 1.0-min_sechiba) .OR.  &
                (SUM(soiltile(ji,:)) .GT. 1.0+min_sechiba) ) THEN
                WRITE(numout,*) 'ji, soiltile(ji,:)', ji, soiltile(ji,:)
                STOP 'sechiba_rotation: sum of soiltile not equal to 1'
            ENDIF
            !!! soil water storage
            CALL hydrol_rotation_update (ji, kjpindex, matrix_rot, old_veget_max, veget_max, soiltile, qsintveg)

            CALL thermosoil_rotation_update (ji, kjpindex, matrix_rot, old_veget_max) !ptn, cgrnd, dgrnd

            !!! sechiba thermol variables: temp_sol_new, soilcap_pft, soilflx_pft, 
            temp_temp_sol = temp_sol_new_pft(ji,:)
            temp_soilcap = soilcap_pft(ji,:)
            temp_soilflx = soilflx_pft(ji,:)

            DO jtar = 1,nvm
                dilu_temp(:) = zero
                dilu_soilcap(:) = zero
                dilu_soilflx(:) = zero
                IF ( SUM(matrix_rot(:,jtar)) .GT. min_sechiba ) THEN
                    DO jsrc = 1,nvm
                        IF ( matrix_rot(jsrc,jtar) .GT. min_sechiba ) THEN
                            dilu_temp(jsrc) = temp_temp_sol(jsrc)
                            dilu_soilcap(jsrc) = temp_soilcap(jsrc)
                            dilu_soilflx(jsrc) = temp_soilflx(jsrc)
                        ENDIF
                    ENDDO
                    temp_sol_new_pft(ji,jtar) = temp_temp_sol(jtar) * old_veget_max(jtar) * (1.0 - SUM(matrix_rot(jtar,:)))
                    soilcap_pft(ji,jtar) = temp_soilcap(jtar) * old_veget_max(jtar) * (1.0 - SUM(matrix_rot(jtar,:)))
                    soilflx_pft(ji,jtar) = temp_soilflx(jtar) * old_veget_max(jtar) * (1.0 - SUM(matrix_rot(jtar,:)))
                    DO jsrc = 1,nvm
                        temp_sol_new_pft(ji,jtar) = temp_sol_new_pft(ji,jtar) + old_veget_max(jsrc) * matrix_rot(jsrc,jtar) * dilu_temp(jsrc)
                        soilcap_pft(ji,jtar) = soilcap_pft(ji,jtar) + old_veget_max(jsrc) * matrix_rot(jsrc,jtar) * dilu_soilcap(jsrc)
                        soilflx_pft(ji,jtar) = soilflx_pft(ji,jtar) + old_veget_max(jsrc) * matrix_rot(jsrc,jtar) * dilu_soilflx(jsrc)
                    ENDDO
                    temp_sol_new_pft(ji,jtar) = temp_sol_new_pft(ji,jtar) / veget_max(ji,jtar)
                    soilcap_pft(ji,jtar) = soilcap_pft(ji,jtar) / veget_max(ji,jtar)
                    soilflx_pft(ji,jtar) = soilflx_pft(ji,jtar) / veget_max(ji,jtar)
                ENDIF
            ENDDO
            !The old temp_sol_new_pft, soilcap_pft, soilflx_pft should be
            !maintained as the old value

            IF (printlev>=4) THEN
                WRITE(numout,*) 'xuhui: debug for sechiba_rotation, ji', ji
                WRITE(numout,*) 'rot_cmd(ji,:)', rot_cmd(ji,:)
                WRITE(numout,*) 'temp_temp_sol:', temp_temp_sol
                WRITE(numout,*) 'temp_sol_new_pft(ji,:)', temp_sol_new_pft(ji,:)
            ENDIF

            !!! remove the executed rotation commands
            f_rot_sech(ji) = .FALSE.
            rot_cmd(ji,:) = 0
         ENDIF  ! f_rot_sech(ji)
       ENDDO ! ji       
    ENDIF !ok_rotate
    !!!!! end rotation, xuhui
 
    !! 10. Update the temperature (temp_sol) with newly computed values
    CALL sechiba_end (kjpindex, temp_sol_new, temp_sol_new_pft, temp_sol, temp_sol_pft)
    !WRITE(numout,*) 'zd sechiba_end 2 ','snowtemp(1,:)',snowtemp(1,:)

   
    !! 11. Write internal variables to output fields
    z0_out(:) = z0(:)
    emis_out(:) = emis(:)
    albedo_out(:,:) = albedo(:,:) 
    qsurf_out(:) = qsurf(:)
 
    !! 12. Write global variables to history files
    sum_treefrac(:) = zero
    sum_grassfrac(:) = zero
    sum_cropfrac(:) = zero
    DO jv = 2, nvm 
       IF (is_tree(jv) .AND. natural(jv)) THEN
          sum_treefrac(:) = sum_treefrac(:) + veget_max(:,jv)
       ELSE IF ((.NOT. is_tree(jv))  .AND. natural(jv)) THEN
          sum_grassfrac(:) = sum_grassfrac(:) + veget_max(:,jv)
       ELSE 
          sum_cropfrac = sum_cropfrac(:) + veget_max(:,jv)
       ENDIF
    ENDDO          

    CALL xios_orchidee_send_field("evapnu",vevapnu*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("snow",snow)
    CALL xios_orchidee_send_field("snowage",snow_age)
    CALL xios_orchidee_send_field("snownobio",snow_nobio)
    CALL xios_orchidee_send_field("snownobioage",snow_nobio_age)
    CALL xios_orchidee_send_field("reinf_slope",reinf_slope)
    CALL xios_orchidee_send_field("soilindex",REAL(njsc, r_std))
    CALL xios_orchidee_send_field("vegetfrac",veget)
    CALL xios_orchidee_send_field("maxvegetfrac",veget_max)
    CALL xios_orchidee_send_field("nobiofrac",frac_nobio)
    CALL xios_orchidee_send_field("soiltile",soiltile)
    CALL xios_orchidee_send_field("rstruct",rstruct)
    IF (ok_co2) CALL xios_orchidee_send_field("gpp",gpp/dt_sechiba)
    CALL xios_orchidee_send_field("nee",co2_flux/dt_sechiba)
    CALL xios_orchidee_send_field("drysoil_frac",drysoil_frac)
    CALL xios_orchidee_send_field("evapflo",vevapflo*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("evapflo_alma",vevapflo/dt_sechiba)
    CALL xios_orchidee_send_field("k_litt",k_litt)
    CALL xios_orchidee_send_field("beta",vbeta)
    CALL xios_orchidee_send_field("vbeta1",vbeta1)
    CALL xios_orchidee_send_field("vbeta2",vbeta2)
    CALL xios_orchidee_send_field("vbeta3",vbeta3)
    CALL xios_orchidee_send_field("vbeta4",vbeta4)
    CALL xios_orchidee_send_field("vbeta5",vbeta5)
    CALL xios_orchidee_send_field("gsmean",gsmean)
    CALL xios_orchidee_send_field("cimean",cimean)
    CALL xios_orchidee_send_field("rveget",rveget)
    CALL xios_orchidee_send_field("rsol",rsol)
 
    histvar(:)=SUM(vevapwet(:,:),dim=2)
    CALL xios_orchidee_send_field("evspsblveg",histvar/dt_sechiba)
    histvar(:)= vevapnu(:)+vevapsno(:)
    CALL xios_orchidee_send_field("evspsblsoi",histvar/dt_sechiba)
    histvar(:)=SUM(transpir(:,:),dim=2)
    CALL xios_orchidee_send_field("tran",histvar/dt_sechiba)
    histvar(:)= sum_treefrac(:)*100*contfrac(:)
    CALL xios_orchidee_send_field("treeFrac",histvar)
    histvar(:)= sum_grassfrac(:)*100*contfrac(:)
    CALL xios_orchidee_send_field("grassFrac",histvar)
    histvar(:)= sum_cropfrac(:)*100*contfrac(:)
    CALL xios_orchidee_send_field("cropFrac",histvar)
    histvar(:)=veget_max(:,1)*100*contfrac(:)
    CALL xios_orchidee_send_field("baresoilFrac",histvar)
    histvar(:)=SUM(frac_nobio(:,1:nnobio),dim=2)*100*contfrac(:)
    CALL xios_orchidee_send_field("residualFrac",histvar)

    CALL xios_orchidee_send_field("tsol_rad",tsol_rad-273.15)
    CALL xios_orchidee_send_field("qsurf",qsurf)
    CALL xios_orchidee_send_field("albedo",albedo)
    CALL xios_orchidee_send_field("emis",emis)
    CALL xios_orchidee_send_field("z0",z0)
    CALL xios_orchidee_send_field("roughheight",roughheight)
    CALL xios_orchidee_send_field("roughheight_pft",roughheight_pft)
    CALL xios_orchidee_send_field("lai",lai)
    histvar(:)=zero   
    DO ji = 1, kjpindex
       IF (SUM(veget_max(ji,:)) > zero) THEN
         DO jv=2,nvm
            histvar(ji) = histvar(ji) + veget_max(ji,jv)*lai(ji,jv)/SUM(veget_max(ji,:))
         END DO
       END IF
    END DO

    CALL xios_orchidee_send_field("LAImean",histvar)
    CALL xios_orchidee_send_field("subli",vevapsno*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("vevapnu",vevapnu*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("vevapnu_alma",vevapnu/dt_sechiba)
    CALL xios_orchidee_send_field("transpir",transpir*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("inter",vevapwet*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("Qf",fusion)
    CALL xios_orchidee_send_field("irrigr",irrigr)
    IF (.NOT. ( river_routing .AND. nbp_glo .GT. 1) ) THEN 
        CALL xios_orchidee_send_field("irrigation",irrigation*one_day/dt_sechiba)
    ENDIF
    !CALL xios_orchidee_send_field("irrigation",irrigation_exe*one_day/dt_sechiba)
    histvar(:)=zero
    DO jv=1,nvm
      histvar(:) = histvar(:) + vevapwet(:,jv)
    ENDDO
    CALL xios_orchidee_send_field("ECanop",histvar/dt_sechiba)
    histvar(:)=zero
    DO jv=1,nvm
      histvar(:) = histvar(:) + transpir(:,jv)
    ENDDO
    CALL xios_orchidee_send_field("TVeg",histvar/dt_sechiba)
    CALL xios_orchidee_send_field("ACond",tq_cdrag)
    CALL xios_orchidee_send_field("ACond_pft",tq_cdrag_pft)

    IF ( .NOT. almaoutput ) THEN
       ! Write history file in IPSL-format
       CALL histwrite_p(hist_id, 'beta', kjit, vbeta, kjpindex, index)
       CALL histwrite_p(hist_id, 'beta_pft', kjit, vbeta_pft, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'z0', kjit, z0, kjpindex, index)
       CALL histwrite_p(hist_id, 'soilflx',kjit, soilflx, kjpindex, index)
       CALL histwrite_p(hist_id, 'soilcap',kjit, soilcap, kjpindex, index)
       CALL histwrite_p(hist_id, 'soilflx_pft',kjit, soilflx_pft, kjpindex, indexveg)
       CALL histwrite_p(hist_id, 'soilcap_pft',kjit, soilcap_pft, kjpindex, indexveg)
       CALL histwrite_p(hist_id, 'roughheight', kjit, roughheight, kjpindex, index)
       CALL histwrite_p(hist_id, 'roughheight_pft', kjit, roughheight_pft, kjpindex, indexveg)
       CALL histwrite_p(hist_id, 'temp_sol_pft', kjit, temp_sol_pft, kjpindex, indexveg)
       CALL histwrite_p(hist_id, 'vegetfrac', kjit, veget, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'maxvegetfrac', kjit, veget_max, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'nobiofrac', kjit, frac_nobio, kjpindex*nnobio, indexnobio)
       CALL histwrite_p(hist_id, 'lai', kjit, lai, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'subli', kjit, vevapsno, kjpindex, index)
       CALL histwrite_p(hist_id, 'evapnu', kjit, vevapnu, kjpindex, index)
       CALL histwrite_p(hist_id, 'evapnu_pft', kjit, vevapnu_pft, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'transpir', kjit, transpir, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'inter', kjit, vevapwet, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'vbeta1', kjit, vbeta1, kjpindex, index)
       CALL histwrite_p(hist_id, 'vbeta2', kjit, vbeta2, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'vbeta3', kjit, vbeta3, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'vbeta4', kjit, vbeta4, kjpindex, index)    
       CALL histwrite_p(hist_id, 'vbeta4_pft', kjit, vbeta4_pft, kjpindex*nvm, indexveg)    
       CALL histwrite_p(hist_id, 'vbeta5', kjit, vbeta5, kjpindex, index)    
       CALL histwrite_p(hist_id, 'drysoil_frac', kjit, drysoil_frac, kjpindex, index)
       CALL histwrite_p(hist_id, 'rveget', kjit, rveget, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'rstruct', kjit, rstruct, kjpindex*nvm, indexveg)

       IF ( .NOT. hydrol_cwrr ) THEN
          CALL histwrite_p(hist_id, 'rsol', kjit, rsol, kjpindex, index)
       ENDIF
       CALL histwrite_p(hist_id, 'snow', kjit, snow, kjpindex, index)
       CALL histwrite_p(hist_id, 'snowage', kjit, snow_age, kjpindex, index)
       CALL histwrite_p(hist_id, 'snownobio', kjit, snow_nobio, kjpindex*nnobio, indexnobio)
       CALL histwrite_p(hist_id, 'snownobioage', kjit, snow_nobio_age, kjpindex*nnobio, indexnobio)
       IF (.NOT. ( river_routing .AND. nbp_glo .GT. 1) ) THEN 
       ! if routing not activated output here
       ! otherwise output is inside routing
           CALL histwrite_p(hist_id, 'irrigation', kjit, irrigation, kjpindex, index)
       ENDIF
       CALL histwrite_p(hist_id, 'irrigr', kjit, irrigr, kjpindex*nstm, indexsoil)

       IF (ok_explicitsnow) THEN
          CALL histwrite_p(hist_id, 'grndflux', kjit, grndflux, kjpindex,index)
          CALL histwrite_p(hist_id, 'snowtemp',kjit,snowtemp,kjpindex*nsnow,indexsnow)
          CALL histwrite_p(hist_id, 'snowliq', kjit,snowliq,kjpindex*nsnow,indexsnow)
          CALL histwrite_p(hist_id, 'snowdz', kjit,snowdz,kjpindex*nsnow,indexsnow)
          CALL histwrite_p(hist_id, 'snowrho', kjit,snowrho,kjpindex*nsnow,indexsnow)
          CALL histwrite_p(hist_id, 'snowgrain',kjit,snowgrain,kjpindex*nsnow,indexsnow)
          CALL histwrite_p(hist_id, 'snowheat',kjit,snowheat,kjpindex*nsnow,indexsnow)
       END IF

       CALL histwrite_p(hist_id, 'pgflux',kjit,pgflux,kjpindex,index)
       CALL histwrite_p(hist_id, 'soiltile',  kjit, soiltile, kjpindex*nstm, indexsoil)
       !
       IF ( hydrol_cwrr ) THEN
          CALL histwrite_p(hist_id, 'soilindex',  kjit, REAL(njsc, r_std), kjpindex, index)
          CALL histwrite_p(hist_id, 'reinf_slope',  kjit, reinf_slope, kjpindex, index)
          CALL histwrite_p(hist_id, 'k_litt', kjit, k_litt, kjpindex, index)
       ENDIF
       IF ( river_routing .AND. do_floodplains ) THEN
          CALL histwrite_p(hist_id, 'evapflo', kjit, vevapflo, kjpindex, index)
          CALL histwrite_p(hist_id, 'flood_frac', kjit, flood_frac, kjpindex, index)
       ENDIF
       IF ( ok_co2 ) THEN
          CALL histwrite_p(hist_id, 'gsmean', kjit, gsmean, kjpindex*nvm, indexveg)    
          CALL histwrite_p(hist_id, 'gpp', kjit, gpp, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist_id, 'cimean', kjit, cimean, kjpindex*nvm, indexveg)    
       ENDIF
       IF ( ok_stomate ) THEN
          CALL histwrite_p(hist_id, 'nee', kjit, co2_flux, kjpindex*nvm, indexveg)    
       ENDIF

       histvar(:)=SUM(vevapwet(:,:),dim=2)
       CALL histwrite_p(hist_id, 'evspsblveg', kjit, histvar, kjpindex, index)

       histvar(:)= vevapnu(:)+vevapsno(:)
       CALL histwrite_p(hist_id, 'evspsblsoi', kjit, histvar, kjpindex, index)

       histvar(:)=SUM(transpir(:,:),dim=2)
       CALL histwrite_p(hist_id, 'tran', kjit, histvar, kjpindex, index)

       histvar(:)= sum_treefrac(:)*100*contfrac(:)
       CALL histwrite_p(hist_id, 'treeFrac', kjit, histvar, kjpindex, index) 

       histvar(:)= sum_grassfrac(:)*100*contfrac(:)
       CALL histwrite_p(hist_id, 'grassFrac', kjit, histvar, kjpindex, index) 

       histvar(:)= sum_cropfrac(:)*100*contfrac(:)
       CALL histwrite_p(hist_id, 'cropFrac', kjit, histvar, kjpindex, index)

       histvar(:)=veget_max(:,1)*100*contfrac(:)
       CALL histwrite_p(hist_id, 'baresoilFrac', kjit, histvar, kjpindex, index)

       histvar(:)=SUM(frac_nobio(:,1:nnobio),dim=2)*100*contfrac(:)
       CALL histwrite_p(hist_id, 'residualFrac', kjit, histvar, kjpindex, index)
    ELSE
       ! Write history file in ALMA format 
       CALL histwrite_p(hist_id, 'vegetfrac', kjit, veget, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'maxvegetfrac', kjit, veget_max, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'nobiofrac', kjit, frac_nobio, kjpindex*nnobio, indexnobio)
       CALL histwrite_p(hist_id, 'Qf', kjit, fusion, kjpindex, index)
       CALL histwrite_p(hist_id, 'ESoil', kjit, vevapnu, kjpindex, index)
       CALL histwrite_p(hist_id, 'EWater', kjit, vevapflo, kjpindex, index)
       CALL histwrite_p(hist_id, 'SWE', kjit, snow, kjpindex, index)
       histvar(:)=zero
       DO jv=1,nvm
          histvar(:) = histvar(:) + transpir(:,jv)
       ENDDO
       CALL histwrite_p(hist_id, 'TVeg', kjit, histvar, kjpindex, index)
       histvar(:)=zero
       DO jv=1,nvm
          histvar(:) = histvar(:) + vevapwet(:,jv)
       ENDDO
       CALL histwrite_p(hist_id, 'ECanop', kjit, histvar, kjpindex, index)
       CALL histwrite_p(hist_id, 'ACond', kjit, tq_cdrag, kjpindex, index)
       CALL histwrite_p(hist_id, 'SnowFrac', kjit, vbeta1, kjpindex, index)
       !
       CALL histwrite_p(hist_id, 'Z0', kjit, z0, kjpindex, index)
       CALL histwrite_p(hist_id, 'EffectHeight', kjit, roughheight, kjpindex, index)
       !
       IF ( river_routing .AND. do_floodplains ) THEN
          CALL histwrite_p(hist_id, 'Qflood', kjit, vevapflo, kjpindex, index)
          CALL histwrite_p(hist_id, 'FloodFrac', kjit, flood_frac, kjpindex, index)
       ENDIF
       !
       IF ( ok_co2 ) THEN
          CALL histwrite_p(hist_id, 'GPP', kjit, gpp, kjpindex*nvm, indexveg)
       ENDIF
       IF ( ok_stomate ) THEN
             CALL histwrite_p(hist_id, 'NEE', kjit, co2_flux, kjpindex*nvm, indexveg)    
       ENDIF
    ENDIF ! almaoutput
    
    !! 13. Write additional output file with higher frequency
    IF ( hist2_id > 0 ) THEN
       IF ( .NOT. almaoutput ) THEN
          ! Write history file in IPSL-format
          CALL histwrite_p(hist2_id, 'tsol_rad', kjit, tsol_rad, kjpindex, index)
          CALL histwrite_p(hist2_id, 'qsurf', kjit, qsurf, kjpindex, index)
          CALL histwrite_p(hist2_id, 'albedo', kjit, albedo, kjpindex*2, indexalb)
          CALL histwrite_p(hist2_id, 'emis', kjit, emis, kjpindex, index)
          CALL histwrite_p(hist2_id, 'beta', kjit, vbeta, kjpindex, index)
          CALL histwrite_p(hist2_id, 'z0', kjit, z0, kjpindex, index)
          CALL histwrite_p(hist2_id, 'roughheight', kjit, roughheight, kjpindex, index)
          CALL histwrite_p(hist2_id, 'roughheight_pft', kjit, roughheight_pft, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'vegetfrac', kjit, veget, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'maxvegetfrac', kjit, veget_max, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'nobiofrac', kjit, frac_nobio, kjpindex*nnobio, indexnobio)
          CALL histwrite_p(hist2_id, 'lai', kjit, lai, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'subli', kjit, vevapsno, kjpindex, index)
          IF ( river_routing .AND. do_floodplains ) THEN
             CALL histwrite_p(hist2_id, 'vevapflo', kjit, vevapflo, kjpindex, index)
             CALL histwrite_p(hist2_id, 'flood_frac', kjit, flood_frac, kjpindex, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'vevapnu', kjit, vevapnu, kjpindex, index)
          CALL histwrite_p(hist2_id, 'transpir', kjit, transpir, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'inter', kjit, vevapwet, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'vbeta1', kjit, vbeta1, kjpindex, index)
          CALL histwrite_p(hist2_id, 'vbeta2', kjit, vbeta2, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'vbeta3', kjit, vbeta3, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'vbeta4', kjit, vbeta4, kjpindex, index)    
          CALL histwrite_p(hist2_id, 'vbeta5', kjit, vbeta5, kjpindex, index)    
          CALL histwrite_p(hist2_id, 'drysoil_frac', kjit, drysoil_frac, kjpindex, index)
          CALL histwrite_p(hist2_id, 'rveget', kjit, rveget, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'rstruct', kjit, rstruct, kjpindex*nvm, indexveg)
          IF ( .NOT. hydrol_cwrr ) THEN
             CALL histwrite_p(hist2_id, 'rsol', kjit, rsol, kjpindex, index)
          ENDIF
          IF (.NOT. ( river_routing .AND. nbp_glo .GT. 1) ) THEN
          ! if routing not activated output here
          ! otherwise output is inside routing
               CALL histwrite_p(hist2_id, 'irrigation', kjit, irrigation, kjpindex,index)
          ENDIF
          CALL histwrite_p(hist2_id, 'irrigr', kjit, irrigr, kjpindex*nstm, indexsoil)
          CALL histwrite_p(hist2_id, 'snow', kjit, snow, kjpindex, index)
          CALL histwrite_p(hist2_id, 'snowage', kjit, snow_age, kjpindex, index)
          CALL histwrite_p(hist2_id, 'snownobio', kjit, snow_nobio, kjpindex*nnobio, indexnobio)
          CALL histwrite_p(hist2_id, 'snownobioage', kjit, snow_nobio_age, kjpindex*nnobio, indexnobio)
          !
          IF (  hydrol_cwrr ) THEN
             CALL histwrite_p(hist2_id, 'soilindex',  kjit, REAL(njsc, r_std), kjpindex, index)
             CALL histwrite_p(hist2_id, 'reinf_slope',  kjit, reinf_slope, kjpindex, index)
          ENDIF
          !
          IF ( ok_co2 ) THEN
             CALL histwrite_p(hist2_id, 'gsmean', kjit, gsmean, kjpindex*nvm, indexveg)    
             CALL histwrite_p(hist2_id, 'gpp', kjit, gpp, kjpindex*nvm, indexveg)
             CALL histwrite_p(hist2_id, 'cimean', kjit, cimean, kjpindex*nvm, indexveg)    
          ENDIF
          IF ( ok_stomate ) THEN
             CALL histwrite_p(hist2_id, 'nee', kjit, co2_flux, kjpindex*nvm, indexveg)    
          ENDIF
       ELSE
          ! Write history file in ALMA format
          CALL histwrite_p(hist2_id, 'vegetfrac', kjit, veget, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'maxvegetfrac', kjit, veget_max, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'nobiofrac', kjit, frac_nobio, kjpindex*nnobio, indexnobio)
          CALL histwrite_p(hist2_id, 'Qf', kjit, fusion, kjpindex, index)
          CALL histwrite_p(hist2_id, 'ESoil', kjit, vevapnu, kjpindex, index)
          IF ( river_routing .AND. do_floodplains ) THEN
             CALL histwrite_p(hist2_id, 'EWater', kjit, vevapflo, kjpindex, index)
             CALL histwrite_p(hist2_id, 'FloodFrac', kjit, flood_frac, kjpindex, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'SWE', kjit, snow, kjpindex, index)
          histvar(:)=zero
          DO jv=1,nvm
             histvar(:) = histvar(:) + transpir(:,jv)
          ENDDO
          CALL histwrite_p(hist2_id, 'TVeg', kjit, histvar, kjpindex, index)
          histvar(:)=zero
          DO jv=1,nvm
             histvar(:) = histvar(:) + vevapwet(:,jv)
          ENDDO
          CALL histwrite_p(hist2_id, 'ECanop', kjit, histvar, kjpindex, index)
          CALL histwrite_p(hist2_id, 'ACond', kjit, tq_cdrag, kjpindex, index)
          CALL histwrite_p(hist2_id, 'SnowFrac', kjit, vbeta1, kjpindex, index)
          IF ( ok_co2 ) THEN
             CALL histwrite_p(hist2_id, 'GPP', kjit, gpp, kjpindex*nvm, indexveg)
          ENDIF
          IF ( ok_stomate ) THEN
             CALL histwrite_p(hist2_id, 'NEE', kjit, co2_flux, kjpindex*nvm, indexveg)    
          ENDIF
       ENDIF ! almaoutput
    ENDIF ! hist2_id

    !! Change the vegetation fractions if a new map was read in slowproc. This is done 
    !! after lcchange has been done in stomatelpj
        !note by xuhui: according to my reading, putting slowproc_change_frac here is
        !logically problematic, because in the end of slowproc, slowproc_veget has cleaned
        !carbon content of PFT with veget_max==0. So the new PFT will always
        !have 0 Carbon content when started, instead of inheriting C of reduced PFTs
    IF (done_stomate_lcchange) THEN
       IF (.NOT. use_age_class) THEN
           CALL slowproc_change_frac(kjpindex, f_rot_sech, lai, &
                                 veget_max, veget, frac_nobio, totfrac_nobio, tot_bare_soil, soiltile)
       ENDIF
       done_stomate_lcchange=.FALSE.
    END IF

    !! 14. If it is the last time step, write restart files
    IF (ldrestart_write) THEN
       CALL sechiba_finalize( &
            kjit,     kjpij,  kjpindex, index,   rest_id, &
            tq_cdrag, vevapp, fluxsens, fluxlat, tsol_rad, &
            netco2flux )
    END IF

  END SUBROUTINE sechiba_main


!!  =============================================================================================================================
!! SUBROUTINE:    sechiba_finalize
!!
!>\BRIEF	  Finalize all modules by calling their "_finalize" subroutines.
!!
!! DESCRIPTION:	  Finalize all modules by calling their "_finalize" subroutines. These subroutines will write variables to 
!!                restart file. 
!!
!! \n
!_ ==============================================================================================================================

  SUBROUTINE sechiba_finalize( &
       kjit,     kjpij,  kjpindex, index,   rest_id, &
       tq_cdrag, vevapp, fluxsens, fluxlat, tsol_rad, &
       netco2flux )

!! 0.1 Input variables    
    INTEGER(i_std), INTENT(in)                               :: kjit              !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)                               :: kjpij             !! Total size of the un-compressed grid 
                                                                                  !! (unitless)
    INTEGER(i_std), INTENT(in)                               :: kjpindex          !! Domain size - terrestrial pixels only 
                                                                                  !! (unitless)
    INTEGER(i_std),INTENT (in)                               :: rest_id           !! _Restart_ file identifier (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: index             !! Indices of the pixels on the map. 
                                                                                  !! Sechiba uses a reduced grid excluding oceans
                                                                                  !! ::index contains the indices of the 
                                                                                  !! terrestrial pixels only! (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: tsol_rad           !! Radiative surface temperature 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: vevapp             !! Total of evaporation 
                                                                                  !! @tex $(kg m^{-2} days^{-1})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: fluxsens           !! Sensible heat flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: fluxlat            !! Latent heat flux 
                                                                                  !! @tex $(W m^{-2})$ @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)           :: tq_cdrag           !! Surface drag coefficient 
                                                                                  !! @tex $(m.s^{-1})$ @endtex 
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)          :: netco2flux         !! Sum CO2 flux over PFTs 

!! 0.2 Local variables
    INTEGER(i_std)                                          :: ji, jv		  !! Index (unitless)
    REAL(r_std), DIMENSION(kjpindex)                        :: histvar            !! Computations for history files (unitless)
    CHARACTER(LEN=80)                                       :: var_name           !! To store variables names for I/O (unitless)


    !! Write restart file for the next simulation from SECHIBA and other modules

    IF (printlev>=3) WRITE (numout,*) 'Write restart file'

    !! 1. Call diffuco_finalize to write restart files
    CALL diffuco_finalize (kjit, kjpindex, rest_id, rstruct, tq_cdrag_pft)
    
    !! 2. Call energy budget to write restart files
    CALL enerbil_finalize (kjit,   kjpindex,    rest_id,            &
                           evapot, evapot_corr, temp_sol, temp_sol_pft,  tsol_rad, &
                           qsurf,  fluxsens,    fluxlat,  vevapp )
    
    !! 3. Call hydrology to write restart files
    IF ( .NOT. hydrol_cwrr ) THEN
       !! 3.1 Call water balance from Choisnel module (2 soil layers) to write restart file
         CALL hydrolc_finalize( kjit,      kjpindex,   rest_id,        snow,  &
                         snow_age,   snow_nobio, snow_nobio_age, humrel,      &
                         vegstress,  qsintveg,   snowrho,        snowtemp,    &
                         snowdz,     snowheat,   snowgrain,                   &
                         drysoil_frac,           rsol,           shumdiag)
       evap_bare_lim(:) = -un
       k_litt(:) = huit
       shumdiag_perma(:,:)=shumdiag(:,:)
    ELSE
       !! 3.2 Call water balance from CWRR module (11 soil layers) to write restart file
        CALL hydrol_finalize( kjit,           kjpindex, rest_id,  vegstress,  &
                        qsintveg,       humrel,         snow,     snow_age, snow_nobio, &
                        snow_nobio_age, snowrho,  snowtemp,             &
                        snowdz,         snowheat,       fwet_out,       &
                        snowgrain,  drysoil_frac, evap_bare_lim)
!                        snowcap,        snowgrain,  drysoil_frac, evap_bare_lim, evap_bare_lim_pft)
       rsol(:) = -un
    ENDIF
    
    !! 4. Call condveg to write surface variables to restart files
    CALL condveg_finalize (kjit, kjpindex, rest_id, z0, roughheight, roughheight_pft)
    
    !! 5. Call soil thermodynamic to write restart files
    IF (hydrol_cwrr) THEN
       CALL thermosoil_finalize (kjit,    kjpindex, rest_id,   gtemp, &
            soilcap, soilcap_pft, soilflx, soilflx_pft, lambda_snow, cgrnd_snow, dgrnd_snow)
    ELSE
       CALL thermosoilc_finalize (kjit,    kjpindex, rest_id,   gtemp, &
            soilcap, soilflx, lambda_snow, cgrnd_snow, dgrnd_snow)
    END IF

    !! 6. Add river routing to restart files  
    IF ( river_routing .AND. nbp_glo .GT. 1) THEN
       !! 6.1 Call river routing to write restart files 
       CALL routing_finalize( kjit, kjpindex, rest_id, flood_frac, flood_res )
    ELSE
       !! 6.2 No routing, set variables to zero
       reinfiltration(:) = zero
       returnflow(:) = zero
!       irrigation(:) = zero
       flood_frac(:) = zero
       flood_res(:) = zero
       IF (do_fullirr) THEN
            CALL restput_p (rest_id, 'irrigation', nbp_glo, 1, 1, kjit, irrigation, 'scatter',  nbp_glo, index_g)
            CALL restput_p(rest_id, 'irrigr', nbp_glo, nstm, 1, kjit,  irrigr, 'scatter',  nbp_glo, index_g)
       ELSE
           irrigation(:) = zero
       ENDIF
    ENDIF

    !! 7. Call slowproc_main to add 'daily' and annual variables to restart file
    print *,'veget_max before slowproc_finalize,',veget_max
    CALL slowproc_finalize (kjit,       kjpindex,  rest_id,  index,      &
                           njsc,       lai,       height,   veget,      &
                           frac_nobio, veget_max, reinf_slope,&
                           zz_deep, zz_coef_deep, thawed_humidity, depth_organic_soil, &
                           assim_param, frac_age )

    ! Compute global CO2 flux !*
    netco2flux(:) = zero
    DO jv = 2,nvm
      netco2flux(:) = netco2flux(:) + co2_flux(:,jv)*veget_max(:,jv)
    ENDDO
    
    IF (printlev>=3) WRITE (numout,*) 'sechiba_finalize done'
    
  END SUBROUTINE sechiba_finalize

  
!! ==============================================================================================================================\n
!! SUBROUTINE 	: sechiba_init
!!
!>\BRIEF        Dynamic allocation of the variables, the dimensions of the 
!! variables are determined by user-specified settings. 
!! 
!! DESCRIPTION  : The domain size (:: kjpindex) is used to allocate the correct
!! dimensions to all variables in sechiba. Depending on the variable, its 
!! dimensions are also determined by the number of PFT's (::nvm), number of 
!! soil types (::nstm), number of non-vegetative surface types (::nnobio),
!! number of soil levels (::ngrnd), number of soil layers in the hydrological 
!! model (i.e. cwrr) (::nslm). Values for these variables are set in
!! constantes_soil.f90 and constantes_veg.f90.\n
!!
!! Memory is allocated for all Sechiba variables and new indexing tables
!! are build making use of both (::kjpij) and (::kjpindex). New indexing tables 
!! are needed because a single pixel can contain several PFTs, soil types, etc.
!! The new indexing tables have separate indices for the different
!! PFTs, soil types, etc.\n
!!
!! RECENT CHANGE(S): None
!! 
!! MAIN OUTPUT VARIABLE(S): Strictly speaking the subroutine has no output 
!! variables. However, the routine allocates memory and builds new indexing 
!! variables for later use.
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================ 

  SUBROUTINE sechiba_init (kjit, kjpij, kjpindex, index, rest_id, lalo)

!! 0.1 Input variables
 
    INTEGER(i_std), INTENT (in)                         :: kjit               !! Time step number (unitless)
    INTEGER(i_std), INTENT (in)                         :: kjpij              !! Total size of the un-compressed grid (unitless)
    INTEGER(i_std), INTENT (in)                         :: kjpindex           !! Domain size - terrestrial pixels only (unitless)
    INTEGER(i_std), INTENT (in)                         :: rest_id            !! _Restart_ file identifier (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)    :: index              !! Indeces of the points on the map (unitless)
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (in)     :: lalo               !! Geographical coordinates (latitude,longitude) 
                                                                              !! for pixels (degrees)
!! 0.2 Output variables

!! 0.3 Modified variables

!! 0.4 Local variables

    INTEGER(i_std)                                      :: ier                !! Check errors in memory allocation (unitless)
    INTEGER(i_std)                                      :: ji, jv             !! Indeces (unitless)
    REAL(r_std), DIMENSION (ngrnd)                      :: dz_tmp             !! Dummy variable
!_ ==============================================================================================================================

!! 1. Initialize variables 
    
    ! Dynamic allocation with user-specified dimensions on first call
    IF (l_first_sechiba) THEN 
       l_first_sechiba=.FALSE.
    ELSE 
       CALL ipslerr_p(3,'sechiba_init',' l_first_sechiba false . we stop ','','')
    ENDIF

    !! Initialize local printlev
    printlev_loc=get_printlev('sechiba')
    

    !! 1.1 Initialize 3D vegetation indexation table
    ALLOCATE (indexveg(kjpindex*nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexveg','','')

    ALLOCATE (indexlai(kjpindex*(nlai+1)),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexlai','','')

    ALLOCATE (indexsoil(kjpindex*nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexsoil','','')

    ALLOCATE (indexnobio(kjpindex*nnobio),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexnobio','','')

    ALLOCATE (indexgrnd(kjpindex*ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexgrnd','','')

    ALLOCATE (indexsnow(kjpindex*nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexsnow','','')

    ALLOCATE (indexlayer(kjpindex*nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexlayer','','')

    ALLOCATE (indexnbdl(kjpindex*nbdl),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexnbdl','','')

    ALLOCATE (indexalb(kjpindex*2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for indexalb','','')

    !! 1.2  Initialize 1D array allocation with restartable value
    ALLOCATE (flood_res(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for flood_res','','')
    flood_res(:) = undef_sechiba

    ALLOCATE (flood_frac(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for kjpindex','','')
    flood_frac(:) = undef_sechiba

    ALLOCATE (snow(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snow','','')
    snow(:) = undef_sechiba

    ALLOCATE (snow_age(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snow_age','','')
    snow_age(:) = undef_sechiba

    ALLOCATE (drysoil_frac(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for drysoil_frac','','')

    ALLOCATE (rsol(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for rsol','','')

    ALLOCATE (evap_bare_lim(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for evap_bare_lim','','')

!    ALLOCATE (evap_bare_lim_pft(kjpindex,nvm),stat=ier)
!    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for evap_bare_lim_pft','','')

    ALLOCATE (soil_deficit(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soil_deficit','','')

    ALLOCATE (evapot(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for evapot','','')
    evapot(:) = undef_sechiba

    ALLOCATE (evapot_corr(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for evapot_corr','','')

    ALLOCATE (humrel(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for humrel','','')
    humrel(:,:) = undef_sechiba

    ALLOCATE (vegstress(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vegstress','','')
    vegstress(:,:) = undef_sechiba

    ALLOCATE (vegstress_old(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vegstress_old','','')
    vegstress_old(:,:) = 1

    ALLOCATE (njsc(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for njsc','','')
    njsc(:)= undef_int

    ALLOCATE (soiltile(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soiltile','','')

    ALLOCATE (is_crop_soil(nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for is_crop_soil','','')

    ALLOCATE (reinf_slope(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for reinf_slope','','')

    ALLOCATE (vbeta1(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta1','','')

    ALLOCATE (vbeta4(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta4','','')

    ALLOCATE (vbeta4_pft(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta4_pft','','')

    ALLOCATE (vbeta5(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta5','','')

    ALLOCATE (soilcap(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soilcap','','')

    ALLOCATE (soilcap_pft(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soilcap_pft','','')

    ALLOCATE (soilflx(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soilflx','','')

    ALLOCATE (soilflx_pft(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soilflx_pft','','')

    ALLOCATE (temp_sol(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for temp_sol','','')
    temp_sol(:) = undef_sechiba

    ALLOCATE (temp_sol_pft(kjpindex, nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for temp_sol_pft','','')
    temp_sol_pft(:,:) = undef_sechiba

    ALLOCATE (temp_sol_new_pft(kjpindex, nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for temp_sol_new_pft','','')
    temp_sol_new_pft(:,:) = undef_sechiba


    ALLOCATE (qsurf(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for qsurf','','')
    qsurf(:) = undef_sechiba

    !! 1.3 Initialize 2D array allocation with restartable value
    ALLOCATE (qsintveg(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for qsintveg','','')
    qsintveg(:,:) = undef_sechiba

    ALLOCATE (tq_cdrag_pft(kjpindex,nvm),stat=ier)
    IF (ier.NE.0) THEN
       WRITE (numout,*) ' error in tq_cdrag_pft allocation. We stop. We need kjpindex x nvm words = ',&
            & kjpindex,' x ' ,nvm, ' = ',kjpindex*nvm
       STOP 'sechiba_init'
    END IF
    ALLOCATE (vbeta_pft(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta_pft','','')

    ALLOCATE (vbeta2(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta2','','')

    ALLOCATE (vbeta3(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta3','','')

    ALLOCATE (vbeta3pot(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta3pot','','')

    ALLOCATE (gsmean(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for gsmean','','')

    ALLOCATE (cimean(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for cimean','','')

    ALLOCATE (gpp(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for gpp','','')
    gpp(:,:) = undef_sechiba

 
    ALLOCATE (temp_growth(kjpindex),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for temp_growth','','')
    temp_growth(:) = undef_sechiba 

    ALLOCATE (veget(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for veget','','')
    veget(:,:)=undef_sechiba

    ALLOCATE (veget_max(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for veget_max','','')

    ALLOCATE (tot_bare_soil(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for tot_bare_soil','','')

    ALLOCATE (lai(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for lai','','')
    lai(:,:)=undef_sechiba

    ALLOCATE (frac_age(kjpindex,nvm,nleafages),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for frac_age','','')
    frac_age(:,:,:)=undef_sechiba

    ALLOCATE (height(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for height','','')
    height(:,:)=undef_sechiba

    ALLOCATE (frac_nobio(kjpindex,nnobio),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for frac_nobio','','')
    frac_nobio(:,:) = undef_sechiba

    ALLOCATE (albedo(kjpindex,2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for albedo','','')

    ALLOCATE (snow_nobio(kjpindex,nnobio),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snow_nobio','','')
    snow_nobio(:,:) = undef_sechiba

    ALLOCATE (snow_nobio_age(kjpindex,nnobio),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snow_nobio_age','','')
    snow_nobio_age(:,:) = undef_sechiba

    ALLOCATE (assim_param(kjpindex,nvm,npco2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for assim_param','','')

    !! 1.4 Initialize 1D array allocation 
! xuhui: +
    ALLOCATE(f_rot_sech(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
       WRITE (numout,*) ' error in fwet_out allocation. We stop. we need kjpindex words = ',kjpindex
       STOP 'sechiba_init'
    END IF
    f_rot_sech(:) = .FALSE.
    
! xuhui: -
!pss:+
    ALLOCATE (fwet_out(kjpindex),stat=ier)
    IF (ier.NE.0) THEN
       WRITE (numout,*) ' error in fwet_out allocation. We stop. we need kjpindex words = ',kjpindex
       STOP 'sechiba_init'
    END IF
    fwet_out(:) = undef_sechiba
!pss:-
!pss:+
    ALLOCATE (drunoff_tot(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for drunoff_tot','','')
    drunoff_tot(:) = zero
!pss:-

    ALLOCATE (vevapflo(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vevapflo','','')
    vevapflo(:)=zero

    ALLOCATE (vevapsno(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vevapsno','','')

    ALLOCATE (vevapnu(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vevapnu','','')

    ALLOCATE (vevapnu_pft(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vevapnu_pft','','')

    ALLOCATE (t2mdiag(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for t2mdiag','','')

    ALLOCATE (totfrac_nobio(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for totfrac_nobio','','')

    ALLOCATE (floodout(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for floodout','','')

    ALLOCATE (runoff(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for runoff','','')

    ALLOCATE (drainage(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for drainage','','')

    ALLOCATE (returnflow(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for returnflow','','')
    returnflow(:) = zero

    ALLOCATE (reinfiltration(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for reinfiltration','','')
    reinfiltration(:) = zero

    ALLOCATE (irrigation(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for irrigation','','')
    irrigation(:) = zero

    !ALLOCATE (irrigation_exe(kjpindex),stat=ier)
    !IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for irrigation_exe','','')
    !irrigation_exe(:) = zero

    ALLOCATE (irrigr(kjpindex,nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Problem in allocate of variable irrigr','','')

    ALLOCATE (do_irrig_reservoir(nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Problem in allocate of variable do_irrig_reservoir','','')
    !Config Key   = DO_IRRIG_RESERVOIR
    !Config Desc  = if active reservoir for irrigation
    !Config If    = DO_IRRIGATION or DO_FULLIRR
    !Config Def   = false/true
    !Config Units = [-]
    do_irrig_reservoir(:) = .FALSE.
    CALL getin_p("DO_IRRIG_RESERVOIR",do_irrig_reservoir)

    ALLOCATE (reservoir_dep_max(nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Problem in allocate of variable reservoir_dep_max','','')
    !Config Key   = RESERVOIR_DEP_MAX
    !Config Desc  = maximum depth of reservoir
    !Config If    = DO_IRRIG_RESERVOIR
    !Config Units = [mm]
    reservoir_dep_max(:) = 0.
    CALL getin_p("RESERVOIR_DEP_MAX",reservoir_dep_max)

    ALLOCATE (irrig_dep_threshold(nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Problem in allocate of variable irrig_dep_threshold','','')
    !Config Key   = IRRIG_DEP_THRESHOLD
    !Config Desc  = threshold when water depth below which irrigation starts
    !Config If    = DO_IRRIG_RESERVOIR
    !Config Units = [mm]
    irrig_dep_threshold(:) = 0.
    CALL getin_p("IRRIG_DEP_THRESHOLD",irrig_dep_threshold)

    ALLOCATE (do_awd(nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Problem in allocate of variable do_awd','','')
    !Config Key   = DO_AWD
    !Config Desc  = if active Alternate Wetting and Drying (AWD) irrigation
    !Config If    = DO_IRRIG_RESERVOIR
    !Config Def   = false/true
    !Config Units = [-]
    do_awd(:) = .FALSE.
    CALL getin_p("DO_AWD",do_awd)

    ALLOCATE (awd_dep(nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Problem in allocate of variable awd_dep','','')
    !Config Key   = AWD_DEP
    !Config Desc  = amount of water (depth,mm) of one irrigation event, if -1,
    !then calculate the irrigation amount based on IRRIG_FULFILL
    !Config If    = DO_AWD
    !Config Units = [mm]
    awd_dep(:) = -1.
    CALL getin_p("AWD_DEP",awd_dep)

          IF ( do_fullirr ) THEN
              ALLOCATE (irrig_frac(kjpindex),stat=ier)
              IF (ier.NE.0) THEN
                 WRITE (numout,*) ' error in irrig_frac allocation. We stop. We need kjpindex words = ',kjpindex
                 STOP 'sechiba_main init'
              END IF
              irrig_frac(:) = 1 ! percentage of cropland irrigated
              !! put here a irrigation map input routine, xuhui
              !!

!              ALLOCATE (tot_vegfrac_crop(kjpindex),stat=ier)
!              IF (ier.NE.0) THEN
!                 WRITE (numout,*) ' error in tot_vegfrac_crop allocation. We stop. We need kjpindex words = ',kjpindex
!                 STOP 'sechiba_main init'
!              END IF
!              DO jv  = 2, nvm
!                 IF ( (jv /= ibare_sechiba) .AND. .NOT.(natural(jv)) ) THEN
!                    tot_vegfrac_crop(:) = tot_vegfrac_crop(:) + veget_max(:,jv)
!                 END IF
!              END DO
          ENDIF

    ALLOCATE (z0(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for z0','','')

    ALLOCATE (roughheight(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for roughheight','','')

    ALLOCATE (roughheight_pft(kjpindex,nvm),stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for roughheight_pft','','')

    ALLOCATE (emis(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for emis','','')

    ALLOCATE (tot_melt(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for tot_melt','','')

    ALLOCATE (valpha(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for valpha','','')

    ALLOCATE (vbeta(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vbeta','','')

    ALLOCATE (fusion(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for fusion','','')

    ALLOCATE (rau(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for rau','','')

    ALLOCATE (deadleaf_cover(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for deadleaf_cover','','')

    ALLOCATE (stempdiag(kjpindex, nbdl),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for stempdiag','','')

    ALLOCATE (co2_flux(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for co2_flux','','')
    co2_flux(:,:)=zero

    ALLOCATE (shumdiag(kjpindex,nbdl),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for shumdiag','','')
    
    ALLOCATE (shumdiag_perma(kjpindex,nbdl),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for shumdiag_perma','','')

    ALLOCATE (litterhumdiag(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for litterhumdiag','','')

    ALLOCATE (ptnlev1(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for ptnlev1','','')

    ALLOCATE (k_litt(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for k_litt','','')

    !! 1.5 Initialize 2D array allocation
    ALLOCATE (vevapwet(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for vevapwet','','')
    vevapwet(:,:)=undef_sechiba

    ALLOCATE (transpir(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for transpir','','')

    ALLOCATE (transpot(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for transpot','','')

    ALLOCATE (qsintmax(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for qsintmax','','')

    ALLOCATE (rveget(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for rveget','','')

    ALLOCATE (rstruct(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for rstruct','','')

    ALLOCATE (pgflux(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for pgflux','','')
    pgflux(:)= 0.0

    ALLOCATE (soilflxresid(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soilflxresid','','')

    ALLOCATE (cgrnd_snow(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for cgrnd_snow','','')
    cgrnd_snow(:,:) = 0

    ALLOCATE (dgrnd_snow(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for dgrnd_snow','','')
    dgrnd_snow(:,:) = 0

    ALLOCATE (lambda_snow(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for lambda_snow','','')
    lambda_snow(:) = 0

    ALLOCATE (snowrho(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snowrho','','')

    ALLOCATE (snowheat(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snowheat','','')

    ALLOCATE (snowgrain(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snowgrain','','')

    ALLOCATE (snowtemp(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snowtemp','','')

    ALLOCATE (snowdz(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for snowdz','','')

    ALLOCATE (temp_sol_add(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for temp_sol_add','','')
    temp_sol_add(:) = 0.0

    ALLOCATE (gtemp(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for gtemp','','')

    !allocate arrays needed for permafrost calculations
    ALLOCATE(tdeep(kjpindex,ndeep,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for tdeep','','')
    tdeep(:,:,:) = 250.

    ALLOCATE(hsdeep(kjpindex,ndeep,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for hsdeep','','')
    hsdeep(:,:,:) = 1.0

    ALLOCATE(heat_Zimov(kjpindex,ndeep,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for heat_Zimov','','')
    heat_Zimov(:,:,:) = zero

  ! 1d arrays (xy)
    ALLOCATE(sfluxCH4_deep(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for sfluxCH4_deep','','')
    ALLOCATE(sfluxCO2_deep(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for sfluxCO2_deep','','')
    ALLOCATE(thawed_humidity(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for thawed_humidity','','')
    ALLOCATE(depth_organic_soil(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for depth_organic_soil','','')
    ! 1d arrays (ndeep)
    ALLOCATE(zz_deep(ndeep),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for zz_deep','','')
    ALLOCATE(zz_coef_deep(ndeep),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for zz_coef_deep','','')
    ALLOCATE(soilc_total(kjpindex,ndeep,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for soilc_total','','')

    ALLOCATE (mc_layh(kjpindex, nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for mc_layh','','')

    ALLOCATE (mcl_layh(kjpindex, nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for mcl_layh','','')

    ALLOCATE (tmc_layh(kjpindex, nslm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for tmc_layh','','')

    ALLOCATE (mc_layh_s(kjpindex, nslm, nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for mc_layh_s','','')

    ALLOCATE (mcl_layh_s(kjpindex, nslm, nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for mcl_layh_s','','')

    ALLOCATE (tmc_layh_s(kjpindex, nslm, nstm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for tmc_layh_s','','')

!    ALLOCATE (mc_layh_pft(kjpindex, nslm, nvm),stat=ier)
!    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for mc_layh_pft','','')

!    ALLOCATE (mcl_layh_pft(kjpindex, nslm, nvm),stat=ier)
!    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for mcl_layh_pft','','')

!    ALLOCATE (tmc_layh_pft(kjpindex, nslm, nvm),stat=ier)
!    IF (ier /= 0) CALL ipslerr_p(3,'sechiba_init','Pb in alloc for tmc_layh_pft','','')

    !! 1.6 Initialize indexing table for the vegetation fields. 
    ! In SECHIBA we work on reduced grids but to store in the full 3D filed vegetation variable 
    ! we need another index table : indexveg, indexsoil, indexnobio and indexgrnd
    DO ji = 1, kjpindex
       !
       DO jv = 1, nlai+1
          indexlai((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !
       DO jv = 1, nvm
          indexveg((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !      
       DO jv = 1, nstm
          indexsoil((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !      
       DO jv = 1, nnobio
          indexnobio((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !
       DO jv = 1, ngrnd
          indexgrnd((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !
       DO jv = 1, nsnow
          indexsnow((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO

       DO jv = 1, nbdl
          indexnbdl((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO

       DO jv = 1, nslm
          indexlayer((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !
       DO jv = 1, 2
          indexalb((jv-1)*kjpindex + ji) = INDEX(ji) + (jv-1)*kjpij + offset_omp - offset_mpi
       ENDDO
       !
    ENDDO
    !
    zz_deep      = znt  ! Vertical depth of the nodes for Thermodynamics
    zz_coef_deep = zlt  ! Vertical depth of the Layers for Thermodynamics
    dz_tmp       = dlt  ! Delta of the Layers for Thermodynamics
    !

!! 2. Read the default value that will be put into variable which are not in the restart file
    CALL ioget_expval(val_exp)
    
    IF (printlev>=3) WRITE (numout,*) ' sechiba_init done '

  END SUBROUTINE sechiba_init
  

!! ==============================================================================================================================\n
!! SUBROUTINE 	: sechiba_clear
!!
!>\BRIEF        Deallocate memory of sechiba's variables
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): None 
!!
!! REFERENCE(S)	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================ 

  SUBROUTINE sechiba_clear (forcing_name,cforcing_name)

    CHARACTER(LEN=100), INTENT(in)           :: forcing_name       !! Name of forcing file (unitless)
    CHARACTER(LEN=100), INTENT(in)           :: cforcing_name      !! Name of forcing file with carbon related variables (unitless)
!_ ================================================================================================================================
    
!! 1. Initialize first run

    l_first_sechiba=.TRUE.

!! 2. Deallocate dynamic variables of sechiba

    IF ( ALLOCATED (indexveg)) DEALLOCATE (indexveg)
    IF ( ALLOCATED (indexlai)) DEALLOCATE (indexlai)
    IF ( ALLOCATED (indexsoil)) DEALLOCATE (indexsoil)
    IF ( ALLOCATED (indexnobio)) DEALLOCATE (indexnobio)
    IF ( ALLOCATED (indexsnow)) DEALLOCATE (indexsnow)
    IF ( ALLOCATED (indexgrnd)) DEALLOCATE (indexgrnd)
    IF ( ALLOCATED (indexlayer)) DEALLOCATE (indexlayer)
    IF ( ALLOCATED (indexnbdl)) DEALLOCATE (indexnbdl)
    IF ( ALLOCATED (indexalb)) DEALLOCATE (indexalb)
    IF ( ALLOCATED (flood_res)) DEALLOCATE (flood_res)
    IF ( ALLOCATED (flood_frac)) DEALLOCATE (flood_frac)
    IF ( ALLOCATED (snow)) DEALLOCATE (snow)
    IF ( ALLOCATED (snow_age)) DEALLOCATE (snow_age)
    IF ( ALLOCATED (drysoil_frac)) DEALLOCATE (drysoil_frac)
    IF ( ALLOCATED (rsol)) DEALLOCATE (rsol)
    IF ( ALLOCATED (evap_bare_lim)) DEALLOCATE (evap_bare_lim)
!    IF ( ALLOCATED (evap_bare_lim_pft)) DEALLOCATE (evap_bare_lim_pft)
    IF ( ALLOCATED (soil_deficit)) DEALLOCATE (soil_deficit)
    IF ( ALLOCATED (evapot)) DEALLOCATE (evapot)
    IF ( ALLOCATED (evapot_corr)) DEALLOCATE (evapot_corr)
    IF ( ALLOCATED (humrel)) DEALLOCATE (humrel)
    IF ( ALLOCATED (vegstress)) DEALLOCATE (vegstress)
    IF ( ALLOCATED (vegstress_old)) DEALLOCATE (vegstress_old)
    IF ( ALLOCATED (soiltile)) DEALLOCATE (soiltile)
    IF ( ALLOCATED (is_crop_soil)) DEALLOCATE (is_crop_soil)
    IF ( ALLOCATED (njsc)) DEALLOCATE (njsc)
    IF ( ALLOCATED (reinf_slope)) DEALLOCATE (reinf_slope)
    IF ( ALLOCATED (vbeta1)) DEALLOCATE (vbeta1)
    IF ( ALLOCATED (vbeta_pft)) DEALLOCATE (vbeta_pft)
    IF ( ALLOCATED (vbeta4)) DEALLOCATE (vbeta4)
    IF ( ALLOCATED (vbeta4_pft)) DEALLOCATE (vbeta4_pft)
    IF ( ALLOCATED (vbeta5)) DEALLOCATE (vbeta5)
    IF ( ALLOCATED (soilcap)) DEALLOCATE (soilcap)
    IF ( ALLOCATED (soilcap_pft)) DEALLOCATE (soilcap_pft)
    IF ( ALLOCATED (soilflx)) DEALLOCATE (soilflx)
    IF ( ALLOCATED (soilflx_pft)) DEALLOCATE (soilflx_pft)
!    IF ( ALLOCATED (snowcap)) DEALLOCATE (snowcap)
!    IF ( ALLOCATED (snowflx)) DEALLOCATE (snowflx)
    IF ( ALLOCATED (temp_sol)) DEALLOCATE (temp_sol)
    IF ( ALLOCATED (temp_sol_pft)) DEALLOCATE (temp_sol_pft)
    IF ( ALLOCATED (qsurf)) DEALLOCATE (qsurf)
    IF ( ALLOCATED (qsintveg)) DEALLOCATE (qsintveg)
    IF ( ALLOCATED (vbeta2))  DEALLOCATE (vbeta2)
    IF ( ALLOCATED (vbeta3)) DEALLOCATE (vbeta3)
    IF ( ALLOCATED (vbeta3pot)) DEALLOCATE (vbeta3pot)
    IF ( ALLOCATED (tq_cdrag_pft))  DEALLOCATE (tq_cdrag_pft)
    IF ( ALLOCATED (gsmean)) DEALLOCATE (gsmean)
    IF ( ALLOCATED (cimean)) DEALLOCATE (cimean)
    IF ( ALLOCATED (gpp)) DEALLOCATE (gpp)
    IF ( ALLOCATED (temp_growth)) DEALLOCATE (temp_growth) 
    IF ( ALLOCATED (veget)) DEALLOCATE (veget)
    IF ( ALLOCATED (veget_max)) DEALLOCATE (veget_max)
    IF ( ALLOCATED (tot_bare_soil)) DEALLOCATE (tot_bare_soil)
    IF ( ALLOCATED (lai)) DEALLOCATE (lai)
    IF ( ALLOCATED (frac_age)) DEALLOCATE (frac_age)
    IF ( ALLOCATED (height)) DEALLOCATE (height)
    IF ( ALLOCATED (roughheight)) DEALLOCATE (roughheight)
    IF ( ALLOCATED (roughheight_pft)) DEALLOCATE (roughheight_pft)
    IF ( ALLOCATED (frac_nobio)) DEALLOCATE (frac_nobio)
    IF ( ALLOCATED (snow_nobio)) DEALLOCATE (snow_nobio)
    IF ( ALLOCATED (snow_nobio_age)) DEALLOCATE (snow_nobio_age)
    IF ( ALLOCATED (assim_param)) DEALLOCATE (assim_param)
    IF ( ALLOCATED (vevapflo)) DEALLOCATE (vevapflo)
    IF ( ALLOCATED (vevapsno)) DEALLOCATE (vevapsno)
    IF ( ALLOCATED (vevapnu)) DEALLOCATE (vevapnu)
    IF ( ALLOCATED (vevapnu_pft)) DEALLOCATE (vevapnu_pft)
    IF ( ALLOCATED (t2mdiag)) DEALLOCATE (t2mdiag)
    IF ( ALLOCATED (totfrac_nobio)) DEALLOCATE (totfrac_nobio)
    IF ( ALLOCATED (floodout)) DEALLOCATE (floodout)
    IF ( ALLOCATED (runoff)) DEALLOCATE (runoff)
    IF ( ALLOCATED (drainage)) DEALLOCATE (drainage)
    IF ( ALLOCATED (reinfiltration)) DEALLOCATE (reinfiltration)
    IF ( ALLOCATED (irrigation)) DEALLOCATE (irrigation)
    IF ( ALLOCATED (irrigr)) DEALLOCATE (irrigr)
    IF ( ALLOCATED (do_irrig_reservoir)) DEALLOCATE (do_irrig_reservoir)
    IF ( ALLOCATED (reservoir_dep_max)) DEALLOCATE (reservoir_dep_max)
    IF ( ALLOCATED (irrig_dep_threshold)) DEALLOCATE (irrig_dep_threshold)
    IF ( ALLOCATED (do_awd)) DEALLOCATE (do_awd)
    IF ( ALLOCATED (awd_dep)) DEALLOCATE (awd_dep)
    IF ( ALLOCATED (irrig_frac)) DEALLOCATE (irrig_frac)
    IF ( ALLOCATED (tot_vegfrac_crop)) DEALLOCATE (tot_vegfrac_crop)
    IF ( ALLOCATED (tot_melt)) DEALLOCATE (tot_melt)
    IF ( ALLOCATED (valpha)) DEALLOCATE (valpha)
    IF ( ALLOCATED (vbeta)) DEALLOCATE (vbeta)
    IF ( ALLOCATED (fusion)) DEALLOCATE (fusion)
    IF ( ALLOCATED (rau)) DEALLOCATE (rau)
    IF ( ALLOCATED (deadleaf_cover)) DEALLOCATE (deadleaf_cover)
    IF ( ALLOCATED (stempdiag)) DEALLOCATE (stempdiag)
    IF ( ALLOCATED (co2_flux)) DEALLOCATE (co2_flux)
    IF ( ALLOCATED (shumdiag)) DEALLOCATE (shumdiag)
    IF ( ALLOCATED (shumdiag_perma)) DEALLOCATE (shumdiag_perma)
    IF ( ALLOCATED (litterhumdiag)) DEALLOCATE (litterhumdiag)
    IF ( ALLOCATED (ptnlev1)) DEALLOCATE (ptnlev1)
    IF ( ALLOCATED (k_litt)) DEALLOCATE (k_litt)
    IF ( ALLOCATED (vevapwet)) DEALLOCATE (vevapwet)
    IF ( ALLOCATED (transpir)) DEALLOCATE (transpir)
    IF ( ALLOCATED (transpot)) DEALLOCATE (transpot)
    IF ( ALLOCATED (qsintmax)) DEALLOCATE (qsintmax)
    IF ( ALLOCATED (rveget)) DEALLOCATE (rveget)
    IF ( ALLOCATED (rstruct)) DEALLOCATE (rstruct)
    IF ( ALLOCATED (snowrho)) DEALLOCATE (snowrho)
    IF ( ALLOCATED (snowgrain)) DEALLOCATE (snowgrain)
    IF ( ALLOCATED (snowtemp)) DEALLOCATE (snowtemp)
    IF ( ALLOCATED (snowdz)) DEALLOCATE (snowdz)
    IF ( ALLOCATED (snowheat)) DEALLOCATE (snowheat)
    IF ( ALLOCATED (cgrnd_snow)) DEALLOCATE (cgrnd_snow)
    IF ( ALLOCATED (dgrnd_snow)) DEALLOCATE (dgrnd_snow)
    IF ( ALLOCATED (lambda_snow)) DEALLOCATE(lambda_snow) 
    IF ( ALLOCATED (gtemp)) DEALLOCATE (gtemp)
    IF ( ALLOCATED (soilflxresid)) DEALLOCATE (soilflxresid)
    IF ( ALLOCATED (pgflux)) DEALLOCATE (pgflux)
    IF ( ALLOCATED (mc_layh)) DEALLOCATE (mc_layh)
    IF ( ALLOCATED (mcl_layh)) DEALLOCATE (mcl_layh)
    IF ( ALLOCATED (tmc_layh)) DEALLOCATE (tmc_layh)
    IF ( ALLOCATED (mc_layh_s)) DEALLOCATE (mc_layh_s)
    IF ( ALLOCATED (mcl_layh_s)) DEALLOCATE (mcl_layh_s)
    IF ( ALLOCATED (tmc_layh_s)) DEALLOCATE (tmc_layh_s)
!    IF ( ALLOCATED (mc_layh_pft)) DEALLOCATE (mc_layh_pft)
!    IF ( ALLOCATED (mcl_layh_pft)) DEALLOCATE (mcl_layh_pft)
!    IF ( ALLOCATED (tmc_layh_pft)) DEALLOCATE (tmc_layh_pft)

!pss:+
    IF ( ALLOCATED (fwet_out)) DEALLOCATE (fwet_out)
    IF ( ALLOCATED (drunoff_tot)) DEALLOCATE (drunoff_tot)
!pss:-

!! 3. Clear all allocated memory

    CALL pft_parameters_clear
    CALL slowproc_clear 
    CALL diffuco_clear 
    CALL enerbil_clear  
    IF ( hydrol_cwrr ) THEN
       CALL hydrol_clear 
       CALL thermosoil_clear
    ELSE
       CALL hydrolc_clear  
       CALL thermosoilc_clear
    ENDIF
    CALL condveg_clear 
    CALL routing_clear

  END SUBROUTINE sechiba_clear


!! ==============================================================================================================================\n
!! SUBROUTINE 	: sechiba_var_init
!!
!>\BRIEF        Calculate air density as a function of air temperature and 
!! pressure for each terrestrial pixel.
!! 
!! RECENT CHANGE(S): None
!! 
!! MAIN OUTPUT VARIABLE(S): air density (::rau, kg m^{-3}).
!! 
!! REFERENCE(S)	: None
!! 
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE sechiba_var_init (kjpindex, rau, pb, temp_air) 

!! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                    :: kjpindex        !! Domain size - terrestrial pixels only (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)  :: pb              !! Surface pressure (hPa)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)  :: temp_air        !! Air temperature (K)
    
!! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out) :: rau             !! Air density @tex $(kg m^{-3})$ @endtex

!! 0.3 Modified variables

!! 0.4 Local variables

    INTEGER(i_std)                                 :: ji              !! Indices (unitless)
!_ ================================================================================================================================
    
!! 1. Calculate intial air density (::rau)
   
    DO ji = 1,kjpindex
       rau(ji) = pa_par_hpa * pb(ji) / (cte_molr*temp_air(ji))
    END DO

    IF (printlev>=3) WRITE (numout,*) ' sechiba_var_init done '

  END SUBROUTINE sechiba_var_init


!! ==============================================================================================================================\n
!! SUBROUTINE 	: sechiba_end
!!
!>\BRIEF        Swap old for newly calculated soil temperature.
!! 
!! RECENT CHANGE(S): None
!! 
!! MAIN OUTPUT VARIABLE(S): soil temperature (::temp_sol; K)
!! 
!! REFERENCE(S)	: None
!! 
!! FLOWCHART    : None
!! \n
!! ================================================================================================================================ 

  SUBROUTINE sechiba_end (kjpindex, temp_sol_new, temp_sol_new_pft, temp_sol, temp_sol_pft)
                         

!! 0.1 Input variables

    INTEGER(i_std), INTENT (in)                       :: kjpindex           !! Domain size - terrestrial pixels only (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)     :: temp_sol_new       !! New soil temperature (K)
    REAL(r_std),DIMENSION (kjpindex, nvm), INTENT (in)     :: temp_sol_new_pft       !! New soil temperature (K)
    
    !! 0.2 Output variables
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)    :: temp_sol           !! Soil temperature (K)
    REAL(r_std),DIMENSION (kjpindex, nvm), INTENT (out)    :: temp_sol_pft           !! Soil temperature (K)

    !! 0.2 Local variables
    INTEGER(i_std)                                    :: ji,jv
!_ ================================================================================================================================
    
!! 1. Swap temperature

    temp_sol(:) = temp_sol_new(:)
    DO jv = 1,nvm
        temp_sol_pft(:,jv) = temp_sol_new_pft(:,jv)
    ENDDO

    IF (printlev>=3) WRITE (numout,*) ' sechiba_end done '

  END SUBROUTINE sechiba_end



  SUBROUTINE sechiba_get_cmd(cmdin, src_rot, tgt_rot, prc_rot)
  ! 0.1 Input
  INTEGER(i_std),INTENT(in)  :: cmdin
  ! 0.2 Output
  INTEGER(i_std),INTENT(out) :: src_rot, tgt_rot
  REAL(r_std),INTENT(out)    :: prc_rot

  ! 0.3 Local variables

  !!!! --------------------------------------------------------------
    IF (cmdin > 1010000 .OR. cmdin < 0 ) THEN
        WRITE(numout,*) 'cmdin, ',cmdin
        STOP 'cmd error in sechiba_get_cmd'
    ENDIF
    IF (cmdin == 0) THEN
        tgt_rot = 0
        src_rot = 0
        
        prc_rot = 0.0
    ELSE
        tgt_rot = MOD(cmdin, 100)
        src_rot = MOD(FLOOR(FLOAT(cmdin)/100), 100)

        prc_rot = FLOAT(FLOOR(FLOAT(cmdin)/10000))/100.0
        IF (printlev >=4) THEN
            WRITE(numout,*) 'xuhui: cmdin, tgt_rot, src_rot, prc_rot', cmdin, src_rot, tgt_rot, prc_rot
        ENDIF
        IF (prc_rot .GT. 1.0 .AND. prc_rot .LT. 1.0+0.01) THEN ! resolve potential  precision issues
            prc_rot = 1.0
        ENDIF
        !!! consistency check
        IF (prc_rot .GT. 1.0) THEN
            WRITE(numout,*) 'percent change larger than 1..., prc_rot',prc_rot
            STOP 'incorrect percent rotation, sechiba_get_cmd'
        ENDIF
        IF ( (tgt_rot .GT. nvm) .OR. ( .NOT. (ok_LAIdev(tgt_rot) .OR. (tgt_rot .EQ. 1)) ) ) THEN
            WRITE(numout,*) 'rotation target error: tgt_rot ', tgt_rot
            WRITE(numout,*) 'nvm, ok_LAIdev', nvm, ok_LAIdev
            STOP 'incorrect rotation target, sechiba_get_cmd'
        ENDIF
        IF ( (src_rot .GT. nvm) .OR. ( .NOT. (ok_LAIdev(src_rot) .OR. (src_rot .EQ. 1)) ) ) THEN
            WRITE(numout,*) 'rotation target error: src_rot ', src_rot
            WRITE(numout,*) 'nvm, ok_LAIdev', nvm, ok_LAIdev
            STOP 'incorrect rotation source, sechiba_get_cmd'
        ENDIF
    ENDIF
  END SUBROUTINE sechiba_get_cmd


END MODULE sechiba
