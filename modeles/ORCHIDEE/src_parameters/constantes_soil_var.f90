! =================================================================================================================================
! MODULE 	: constantes_soil_var
!
! CONTACT       : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE       : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF         "constantes_soil_var" module contains the parameters related to soil and hydrology.
!!
!!\n DESCRIPTION : The non saturated hydraulic properties are defined from the  
!!                 formulations of van Genuchten (1980) and Mualem (1976), combined as  
!!                 explained in d'Orgeval (2006). \n
!!                 The related parameters for three main  
!!                 soil textures (coarse, medium and fine) come from Carsel and Parrish  
!!                 (1988).
!!
!! RECENT CHANGE(S): Sonke Zaehle changed hcrit_litter value according to Shilong Piao from 0.03 to 0.08, 080806
!!
!! REFERENCE(S)	:
!!- Roger A.Pielke, (2002), Mesoscale meteorological modeling, Academic Press Inc. 
!!- Polcher, J., Laval, K., Dümenil, L., Lean, J., et Rowntree, P. R. (1996).
!! Comparing three land surface schemes used in general circulation models. Journal of Hydrology, 180(1-4), 373--394.
!!- Ducharne, A., Laval, K., et Polcher, J. (1998). Sensitivity of the hydrological cycle
!! to the parametrization of soil hydrology in a GCM. Climate Dynamics, 14, 307--327. 
!!- Rosnay, P. de et Polcher, J. (1999). Modelling root water uptake in a complex land surface
!! scheme coupled to a GCM. Hydrol. Earth Syst. Sci., 2(2/3), 239--255.
!!- d'Orgeval, T. et Polcher, J. (2008). Impacts of precipitation events and land-use changes
!! on West African river discharges during the years 1951--2000. Climate Dynamics, 31(2), 249--262. 
!!- Carsel, R. and Parrish, R.: Developing joint probability distributions of soil water
!! retention characteristics, Water Resour. Res.,24, 755–769, 1988.
!!- Mualem Y (1976) A new model for predicting the hydraulic conductivity  
!! of unsaturated porous media. Water Resources Research 12(3):513-522
!!- Van Genuchten M (1980) A closed-form equation for predicting the  
!! hydraulic conductivity of unsaturated soils. Soil Sci Soc Am J, 44(5):892-898
!!
!! SVN          :
!! $HeadURL: $
!! $Date: $
!! $Revision: $
!! \n
!_ ================================================================================================================================

MODULE constantes_soil_var

  USE defprec
  USE vertical_soil_var

  IMPLICIT NONE


  LOGICAL, SAVE             :: check_waterbal=.TRUE.    !! The check the water balance (true/false)
!$OMP THREADPRIVATE(check_waterbal)


  !when refinerottom is not active
  !INTEGER(i_std),PARAMETER  :: ndeep=21
  !when refinerottom active
  INTEGER(i_std),PARAMETER  :: ndeep=90
  REAL(r_std), PARAMETER    :: zalph=1.18
  REAL(r_std), PARAMETER    :: z_deepsoil = 2.          !! depth below which soil humidity is set to fixed values

  !! Number of soil classes

  INTEGER(i_std), PARAMETER :: ntext=3                  !! Number of soil textures (Silt, Sand, Clay)
!  INTEGER(i_std), PARAMETER :: nstm=3                   !! Number of soil tiles (unitless)
!  INTEGER(i_std), PARAMETER :: nstm=6                   !! Number of soil tiles (unitless) 4 wheat 5 maize 6 rice
  INTEGER(i_std), SAVE :: nstm=6                   !! Number of soil tiles (unitless) 4 wheat 5 maize 6 rice
  CHARACTER(LEN=30)         :: soil_classif             !! Type of classification used for the map of soil types.
                                                        !! It must be consistent with soil file given by 
                                                        !! SOILCLASS_FILE parameter.
  INTEGER(i_std), PARAMETER :: nscm_fao=3               !! For FAO Classification (unitless)
  INTEGER(i_std), PARAMETER :: nscm_usda=12             !! For USDA Classification (unitless)
  INTEGER(i_std), SAVE      :: nscm=nscm_fao            !! Default value for nscm
!$OMP THREADPRIVATE(nscm)

  !! Parameters for soil thermodynamics

  REAL(r_std), SAVE :: so_capa_dry = 1.80e+6            !! Dry soil Heat capacity of soils 
                                                        !! @tex $(J.m^{-3}.K^{-1})$ @endtex 
!$OMP THREADPRIVATE(so_capa_dry)
  REAL(r_std), SAVE :: so_cond_dry = 0.40               !! Dry soil Thermal Conductivity of soils
                                                        !! @tex $(W.m^{-2}.K^{-1})$ @endtex
!$OMP THREADPRIVATE(so_cond_dry)
  REAL(r_std), SAVE :: so_capa_wet = 3.03e+6            !! Wet soil Heat capacity of soils 
                                                        !! @tex $(J.m^{-3}.K^{-1})$ @endtex
!$OMP THREADPRIVATE(so_capa_wet)
  REAL(r_std), SAVE :: so_cond_wet = 1.89               !! Wet soil Thermal Conductivity of soils 
                                                        !! @tex $(W.m^{-2}.K^{-1})$ @endtex 
!$OMP THREADPRIVATE(so_cond_wet)
  REAL(r_std), SAVE :: sn_cond = 0.3                    !! Thermal Conductivity of snow 
                                                        !! @tex $(W.m^{-2}.K^{-1})$ @endtex  
!$OMP THREADPRIVATE(sn_cond)
  REAL(r_std), SAVE :: sn_dens = 330.0                  !! Snow density for the soil thermodynamics
                                                        !! (kg/m3)
!$OMP THREADPRIVATE(sn_dens)
  REAL(r_std), SAVE :: sn_capa                          !! Heat capacity for snow 
                                                        !! @tex $(J.m^{-3}.K^{-1})$ @endtex
!$OMP THREADPRIVATE(sn_capa)
  REAL(r_std), PARAMETER :: poros_org = 0.92            !! for now just a number from dmitry's code
  REAL(r_std), PARAMETER :: cond_solid_org = 0.25       !! W/m/K from Farouki via Lawrence and Slater
  REAL(r_std), PARAMETER :: so_cond_dry_org = 0.25      !! W/m/K from Farouki via Lawrence and Slater
  REAL(r_std), PARAMETER :: so_capa_dry_org = 2.5e6     !! J/K/m^3 from Farouki via Lawrence and Slater
  REAL(r_std), SAVE :: water_capa = 4.18e+6             !! Water heat capacity 
                                                        !! @tex $(J.m^{-3}.K^{-1})$ @endtex
!$OMP THREADPRIVATE(water_capa)
  REAL(r_std), SAVE :: brk_capa = 2.0e+6                !! Heat capacity of generic rock
                                                        !! @tex $(J.m^{-3}.K^{-1})$ @endtex
!$OMP THREADPRIVATE(brk_capa)
  REAL(r_std), SAVE :: brk_cond = 3.0                   !! Thermal conductivity of saturated granitic rock
                                                        !! @tex $(W.m^{-1}.K^{-1})$ @endtex
!$OMP THREADPRIVATE(brk_cond)

  !REAL(r_std),PARAMETER :: sn_capa = 2100.0_r_std*sn_dens !! Heat capacity
  !for snow @tex $(J.m^{-3}.K^{-1})$ @endtex
  REAL(r_std), PARAMETER   :: soilc_max =  130000.      !! g/m^3 from lawrence and slater

  !! Specific parameters for the Choisnel hydrology

  REAL(r_std), SAVE :: min_drain = 0.001                !! Diffusion constant for the slow regime
                                                        !! (This is for the diffusion between reservoirs)
                                                        !! @tex $(kg.m^{-2}.dt^{-1})$ @endtex
!$OMP THREADPRIVATE(min_drain)
  REAL(r_std), SAVE :: max_drain = 0.1                  !! Diffusion constant for the fast regime 
                                                        !! @tex $(kg.m^{-2}.dt^{-1})$ @endtex
!$OMP THREADPRIVATE(max_drain)
  REAL(r_std), SAVE :: exp_drain = 1.5                  !! The exponential in the diffusion law (unitless)
!$OMP THREADPRIVATE(exp_drain)
  REAL(r_std), SAVE :: qsintcst = 0.1                   !! Transforms leaf area index into size of interception reservoir
                                                        !! (unitless)
!$OMP THREADPRIVATE(qsintcst)
  REAL(r_std), SAVE :: mx_eau_nobio = 150.              !! Volumetric available soil water capacity in nobio fractions
                                                        !! @tex $(kg.m^{-3} of soil)$ @endtex
!$OMP THREADPRIVATE(mx_eau_nobio)
  REAL(r_std), SAVE :: rsol_cste = 33.E3                !! Constant in the computation of resistance for bare soil evaporation
                                                        !! @tex $(s.m^{-2})$ @endtex
!$OMP THREADPRIVATE(rsol_cste)
  REAL(r_std), SAVE :: hcrit_litter=0.08_r_std          !! Scaling depth for litter humidity (m)
!$OMP THREADPRIVATE(hcrit_litter)

  INTEGER(i_std), SAVE :: SO_DISCRETIZATION_METHOD      !! Soil layer discretization method selected, 0 = thermix, 1 = permafrost  
!$OMP THREADPRIVATE(SO_DISCRETIZATION_METHOD)
  INTEGER(i_std), PARAMETER :: SLD_THERMIX = 0          !! Soil layers discretization constant for thermix method
  INTEGER(i_std), PARAMETER :: SLD_PERMAFROST = 1       !! Soil layers discretization constant for permafrost method

  REAL(r_std), SAVE         :: THKICE = 2.2             !! Ice Thermal Conductivity (W/m/k)
!$OMP THREADPRIVATE(THKICE)
  REAL(r_std), SAVE         :: THKQTZ = 7.7             !! Thermal Conductivity for Quartz (W/m/k)
!$OMP THREADPRIVATE(THKQTZ)
  REAL(r_std), SAVE         :: THKW = 0.57              !! Water Thermal Conductivity (W/m/k)
!$OMP THREADPRIVATE(THKW)

  !! Parameters specific for the CWRR hydrology.

  !!  1. Parameters for FAO Classification

  !! Parameters for soil type distribution

  REAL(r_std),DIMENSION(nscm_fao),SAVE :: soilclass_default_fao = &   !! Default soil texture distribution for fao :
 & (/ 0.28, 0.52, 0.20 /)                                             !! in the following order : COARSE, MEDIUM, FINE (unitless)
!$OMP THREADPRIVATE(soilclass_default_fao)

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: nvan_fao = &            !! Van Genuchten coefficient n (unitless)
 & (/ 1.89_r_std, 1.56_r_std, 1.31_r_std /)                             !  RK: 1/n=1-m

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: avan_fao = &            !! Van Genuchten coefficient a 
  & (/ 0.0075_r_std, 0.0036_r_std, 0.0019_r_std /)                     !!  @tex $(mm^{-1})$ @endtex

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcr_fao = &             !! Residual volumetric water content 
 & (/ 0.065_r_std, 0.078_r_std, 0.095_r_std /)                         !!  @tex $(m^{3} m^{-3})$ @endtex

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcs_fao = &             !! Saturated volumetric water content 
 & (/ 0.41_r_std, 0.43_r_std, 0.41_r_std /)                            !!  @tex $(m^{3} m^{-3})$ @endtex

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: ks_fao = &              !! Hydraulic conductivity at saturation 
 & (/ 1060.8_r_std, 249.6_r_std, 62.4_r_std /)                         !!  @tex $(mm d^{-1})$ @endtex

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: pcent_fao = &           !! Fraction of saturated volumetric soil moisture 
 & (/ 0.8_r_std, 0.8_r_std, 0.8_r_std /)                               !! above which transpir is max (0-1, unitless)

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: free_drain_max_fao = &  !! Max=default value of the permeability coeff  
 & (/ 1.0_r_std, 1.0_r_std, 1.0_r_std /)                               !! at the bottom of the soil (0-1, unitless)

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcf_fao = &             !! Volumetric water content at field capacity
 & (/ 0.32_r_std, 0.32_r_std, 0.32_r_std /)                            !!  @tex $(m^{3} m^{-3})$ @endtex

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mcw_fao = &             !! Volumetric water content Wilting pt
 & (/ 0.065_r_std, 0.078_r_std, 0.095_r_std /)                         !! Phocaides, A. (2007). Handbook on pressurized irrigation techniques, CHAPTER 6: Irrigation scheduling, Food & Agriculture Org

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mc_awet_fao = &         !! Vol. wat. cont. above which albedo is cst 
 & (/ 0.25_r_std, 0.25_r_std, 0.25_r_std /)                            !!  @tex $(m^{3} m^{-3})$ @endtex

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: mc_adry_fao = &         !! Vol. wat. cont. below which albedo is cst
 & (/ 0.1_r_std, 0.1_r_std, 0.1_r_std /)                               !!  @tex $(m^{3} m^{-3})$ @endtex

  REAL(r_std),DIMENSION(nscm_fao),SAVE :: SMCMAX_fao = &               !! porosity
 & (/ 0.41_r_std, 0.43_r_std, 0.41_r_std /)                            !! & (/ 0.434_r_std, 0.439_r_std, 0.465_r_std /) !!noah lsm

  REAL(r_std),SAVE,DIMENSION(nscm_fao) :: QZ_fao = &              !! QUARTZ CONTENT (SOIL TYPE DEPENDENT)
 & (/ 0.60_r_std, 0.40_r_std, 0.35_r_std /)                            !! Peters et al [1998]

  REAL(r_std),PARAMETER,DIMENSION(nscm_fao) :: so_capa_dry_ns_fao = &  !! Dry soil Heat capacity of soils,J.m^{-3}.K^{-1}
 & (/ 1.34e+6_r_std, 1.21e+6_r_std, 1.23e+6_r_std /)                   !! Pielke [2002, 2013]

  !!  2. Parameters for USDA Classification

  !! Parameters for soil type distribution :
  !! Sand, Loamy Sand, Sandy Loam, Silt Loam, Silt, Loam, Sandy Clay Loam, Silty Clay Loam, Clay Loam, Sandy Clay, Silty Clay, Clay

  REAL(r_std),DIMENSION(nscm_usda),SAVE :: soilclass_default_usda = &    !! Default soil texture distribution in the above order :
 & (/ 0.28, 0.52, 0.20, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /)   !! Thus different from "FAO"'s COARSE, MEDIUM, FINE
                                                                         !! which have indices 3,6,9 in the 12-texture vector
  !$OMP THREADPRIVATE(soilclass_default_usda)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: nvan_usda = &            !! Van Genuchten coefficient n (unitless)
 & (/ 2.68_r_std, 2.28_r_std, 1.89_r_std, 1.41_r_std, &                   !  RK: 1/n=1-m
 &    1.37_r_std, 1.56_r_std, 1.48_r_std, 1.23_r_std, &
 &    1.31_r_std, 1.23_r_std, 1.09_r_std, 1.09_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: avan_usda = &            !! Van Genuchten coefficient a 
 & (/ 0.0145_r_std, 0.0124_r_std, 0.0075_r_std, 0.0020_r_std, &          !!  @tex $(mm^{-1})$ @endtex
 &    0.0016_r_std, 0.0036_r_std, 0.0059_r_std, 0.0010_r_std, &
 &    0.0019_r_std, 0.0027_r_std, 0.0005_r_std, 0.0008_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcr_usda = &             !! Residual volumetric water content 
 & (/ 0.045_r_std, 0.057_r_std, 0.065_r_std, 0.067_r_std, &              !!  @tex $(m^{3} m^{-3})$ @endtex
 &    0.034_r_std, 0.078_r_std, 0.100_r_std, 0.089_r_std, &
 &    0.095_r_std, 0.100_r_std, 0.070_r_std, 0.068_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcs_usda = &             !! Saturated volumetric water content 
 & (/ 0.43_r_std, 0.41_r_std, 0.41_r_std, 0.45_r_std, &                  !!  @tex $(m^{3} m^{-3})$ @endtex
 &    0.46_r_std, 0.43_r_std, 0.39_r_std, 0.43_r_std, &
 &    0.41_r_std, 0.38_r_std, 0.36_r_std, 0.38_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: ks_usda = &              !! Hydraulic conductivity at saturation
 & (/ 7128.0_r_std, 3501.6_r_std, 1060.8_r_std, 108.0_r_std, &           !!  @tex $(mm d^{-1})$ @endtex
 &    60.0_r_std, 249.6_r_std, 314.4_r_std, 16.8_r_std, &
 &    62.4_r_std, 28.8_r_std, 4.8_r_std, 48.0_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: pcent_usda = &           !! Fraction of saturated volumetric soil moisture
 & (/ 0.8_r_std, 0.8_r_std, 0.8_r_std, 0.8_r_std, &                      !! above which transpir is max (0-1, unitless)
 &    0.8_r_std, 0.8_r_std, 0.8_r_std, 0.8_r_std, &
 &    0.8_r_std, 0.8_r_std, 0.8_r_std, 0.8_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: free_drain_max_usda = &  !! Max=default value of the permeability coeff 
 & (/ 1.0_r_std, 1.0_r_std, 1.0_r_std, 1.0_r_std, &                      !! at the bottom of the soil (0-1, unitless)
 &    1.0_r_std, 1.0_r_std, 1.0_r_std, 1.0_r_std, &
 &    1.0_r_std, 1.0_r_std, 1.0_r_std, 1.0_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcf_usda = &             !! Volumetric water content at field capacity
 & (/ 0.32_r_std, 0.32_r_std, 0.32_r_std, 0.32_r_std, &                  !!  @tex $(m^{3} m^{-3})$ @endtex
 &    0.32_r_std, 0.32_r_std, 0.32_r_std, 0.32_r_std, &
 &    0.32_r_std, 0.32_r_std, 0.32_r_std, 0.32_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mcw_usda = &             !! Volumetric water content at wilting point
 & (/ 0.10_r_std, 0.10_r_std, 0.10_r_std, 0.10_r_std, &                  !!  @tex $(m^{3} m^{-3})$ @endtex
 &    0.10_r_std, 0.10_r_std, 0.10_r_std, 0.10_r_std, &
 &    0.10_r_std, 0.10_r_std, 0.10_r_std, 0.10_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mc_awet_usda = &         !! Vol. wat. cont. above which albedo is cst
 & (/ 0.25_r_std, 0.25_r_std, 0.25_r_std, 0.25_r_std, &                  !!  @tex $(m^{3} m^{-3})$ @endtex
 &    0.25_r_std, 0.25_r_std, 0.25_r_std, 0.25_r_std, &
 &    0.25_r_std, 0.25_r_std, 0.25_r_std, 0.25_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: mc_adry_usda = &         !! Vol. wat. cont. below which albedo is cst
 & (/ 0.1_r_std, 0.1_r_std, 0.1_r_std, 0.1_r_std, &                      !!  @tex $(m^{3} m^{-3})$ @endtex
 &    0.1_r_std, 0.1_r_std, 0.1_r_std, 0.1_r_std, &
 &    0.1_r_std, 0.1_r_std, 0.1_r_std, 0.1_r_std /)

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: SMCMAX_usda = &          !! porosity
 & (/ 0.43_r_std, 0.41_r_std, 0.41_r_std, 0.45_r_std, &
 &    0.46_r_std, 0.43_r_std, 0.39_r_std, 0.43_r_std, &
 &    0.41_r_std, 0.38_r_std, 0.36_r_std, 0.38_r_std /)
 
  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: QZ_usda = &              !! QUARTZ CONTENT (SOIL TYPE DEPENDENT)
 & (/ 0.92_r_std, 0.82_r_std, 0.60_r_std, 0.25_r_std, &
 &    0.10_r_std, 0.40_r_std, 0.60_r_std, 0.10_r_std, &
 &    0.35_r_std, 0.52_r_std, 0.10_r_std, 0.25_r_std /)                  !! Peters et al [1998]

  REAL(r_std),PARAMETER,DIMENSION(nscm_usda) :: so_capa_dry_ns_usda = &  !! Dry soil Heat capacity of soils,J.m^{-3}.K^{-1}
 & (/ 1.47e+6_r_std, 1.41e+6_r_std, 1.34e+6_r_std, 1.27e+6_r_std, &
 &    1.21e+6_r_std, 1.21e+6_r_std, 1.18e+6_r_std, 1.32e+6_r_std, &
 &    1.23e+6_r_std, 1.18e+6_r_std, 1.15e+6_r_std, 1.09e+6_r_std /)      !! Pielke [2002, 2013]
  
  !! Parameters for the numerical scheme used by CWRR

  INTEGER(i_std), PARAMETER :: imin = 1                                 !! Start for CWRR linearisation (unitless)
  INTEGER(i_std), PARAMETER :: nbint = 50                               !! Number of interval for CWRR linearisation (unitless)
  INTEGER(i_std), PARAMETER :: imax = nbint+1                           !! Number of points for CWRR linearisation (unitless)
  REAL(r_std), PARAMETER    :: w_time = 1.0_r_std                       !! Time weighting for CWRR numerical integration (unitless)


  !! Variables related to soil freezing, in thermosoil : 
  LOGICAL, SAVE        :: ok_Ecorr                    !! Flag for energy conservation correction
  LOGICAL, SAVE        :: ok_freeze_thermix           !! Flag to activate thermal part of the soil freezing scheme
  LOGICAL, SAVE        :: read_reftemp                !! Flag to initialize soil temperature using climatological temperature
  LOGICAL, SAVE        :: read_permafrost_map         !! Read information about ice content, overburden and permafrost type from IPA map
  REAL(r_std), SAVE    :: poros                       !! Soil porosity (from USDA classification, mean value)(-)
  REAL(r_std), SAVE    :: fr_dT                       !! Freezing window (K)

  !! Variables related to soil freezing, in diffuco : 
  LOGICAL, SAVE        ::  ok_snowfact                !! Activate snow smoothering

  !! Variables related to soil freezing, in hydrol : 
  LOGICAL, SAVE        :: ok_freeze_cwrr              !! CWRR freezing scheme by I. Gouttevin
  LOGICAL, SAVE        :: ok_thermodynamical_freezing !! Calculate frozen fraction thermodynamically

  
END MODULE constantes_soil_var
