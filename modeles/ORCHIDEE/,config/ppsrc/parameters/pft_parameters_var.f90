










! =================================================================================================================================
! MODULE       : pft_parameters_var
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2011)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module contains the variables in function of plant funtional type (pft).
!!
!!\n DESCRIPTION: This module contains the declarations for the externalized variables in function of the 
!!                plant foncional type(pft). \n
!!                The module is already USE in module pft_parameters. Therefor no need to USE it seperatly except
!!                if the subroutines in module pft_parameters are not needed.\n
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	: None
!!
!! SVN          :
!! $HeadURL: $
!! $Date: 2016-04-26 13:28:48 +0200 (Tue, 26 Apr 2016) $
!! $Revision: 3386 $
!! \n
!_ ================================================================================================================================

MODULE pft_parameters_var

  USE defprec
  
  IMPLICIT NONE


  !
  ! PFT GLOBAL
  !
  INTEGER(i_std), SAVE :: nvm = 14                              !! Number of vegetation types (2-N, unitless)
!$OMP THREADPRIVATE(nvm)

  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pft_to_mtc  !! Table of conversion : we associate one pft to one metaclass 
                                                                 !! (1-13, unitless)
!$OMP THREADPRIVATE(pft_to_mtc)

  CHARACTER(LEN=34), ALLOCATABLE, SAVE, DIMENSION(:) :: PFT_name !! Description of the PFT (unitless)
!$OMP THREADPRIVATE(PFT_name)

  LOGICAL, SAVE   :: l_first_pft_parameters = .TRUE.             !! To keep first call trace of the module (true/false)
!$OMP THREADPRIVATE(l_first_pft_parameters)

  !
  ! VEGETATION STRUCTURE
  !
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: leaf_tab       !! leaf type (1-4, unitless)
                                                                    !! 1=broad leaved tree, 2=needle leaved tree, 
                                                                    !! 3=grass 4=bare ground 
!$OMP THREADPRIVATE(leaf_tab)

  CHARACTER(len=6), ALLOCATABLE, SAVE, DIMENSION(:) :: pheno_model  !! which phenology model is used? (tabulated) (unitless)
!$OMP THREADPRIVATE(pheno_model)

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: is_tree               !! Is the vegetation type a tree ? (true/false)
!$OMP THREADPRIVATE(is_tree)

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: is_deciduous          !! Is PFT deciduous ? (true/false)
!$OMP THREADPRIVATE(is_deciduous)

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: is_evergreen          !! Is PFT evegreen ? (true/false)
!$OMP THREADPRIVATE(is_evergreen)

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: is_needleleaf         !! Is PFT needleleaf ? (true/false)
!$OMP THREADPRIVATE(is_needleleaf)
 
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: is_tropical           !! Is PFT tropical ? (true/false)
!$OMP THREADPRIVATE(is_tropical)

  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: natural               !! natural? (true/false)
!$OMP THREADPRIVATE(natural)
  CHARACTER(len=5), ALLOCATABLE, SAVE, DIMENSION(:) :: type_of_lai  !! Type of behaviour of the LAI evolution algorithm 
                                                                    !! for each vegetation type.
                                                                    !! Value of type_of_lai, one for each vegetation type :
                                                                    !! mean or interp
!$OMP THREADPRIVATE(type_of_lai)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: veget_ori_fixed_test_1 !! Value for veget_ori for tests in 0-dim simulations 
                                                                         !! (0-1, unitless)
!$OMP THREADPRIVATE(veget_ori_fixed_test_1)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: llaimax                !! laimax for maximum lai see also type of lai 
                                                                         !! interpolation
                                                                         !! @tex $(m^2.m^{-2})$ @endtex 
!$OMP THREADPRIVATE(llaimax)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: llaimin                !! laimin for minimum lai see also type of lai 
                                                                         !! interpolation 
                                                                         !! @tex $(m^2.m^{-2})$ @endtex
!$OMP THREADPRIVATE(llaimin)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: height_presc           !! prescribed height of vegetation.(m)
                                                                         !! Value for height_presc : one for each vegetation type
!$OMP THREADPRIVATE(height_presc)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) ::  rveg_pft              !! Potentiometer to set vegetation resistance (unitless)
                                                                         !! Nathalie on March 28th, 2006 - from Fred Hourdin,
!$OMP THREADPRIVATE(rveg_pft)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: sla                    !! specif leaf area @tex $(m^2.gC^{-1})$ @endtex 
!$OMP THREADPRIVATE(sla)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: availability_fact      !! calculate dynamic mortality in lpj_gap
!$OMP THREADPRIVATE(availability_fact)

  !
  ! EVAPOTRANSPIRATION (sechiba)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: rstruct_const          !! Structural resistance. 
                                                                         !! Value for rstruct_const : one for each vegetation type
                                                                         !! @tex $(s.m^{-1})$ @endtex
!$OMP THREADPRIVATE(rstruct_const)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: kzero                  !! A vegetation dependent constant used in the calculation
                                                                         !! of the surface resistance. 
                                                                         !! Value for kzero one for each vegetation type
                                                                         !! @tex $(kg.m^2.s^{-1})$ @endtex
!$OMP THREADPRIVATE(kzero)

  !
  ! WATER (sechiba)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: wmax_veg  !! Volumetric available soil water capacity in each PFT
                                                            !! @tex $(kg.m^{-3} of soil)$ @endtex
!$OMP THREADPRIVATE(wmax_veg)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: humcste   !! Root profile description for the different vegetation types.
                                                            !! These are the factor in the exponential which gets
                                                            !! the root density as a function of depth 
                                                            !! @tex $(m^{-1})$ @endtex
!$OMP THREADPRIVATE(humcste)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: throughfall_by_pft !! Fraction of rain intercepted by the canopy (0-100, unitless)
!$OMP THREADPRIVATE(throughfall_by_pft)

  !
  ! ALBEDO (sechiba)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: snowa_aged    !! Minimum snow albedo value for each vegetation type
                                                                !! after aging (dirty old snow) (unitless)
                                                                !! Source : Values are from the Thesis of S. Chalita (1992)
!$OMP THREADPRIVATE(snowa_aged)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: snowa_dec     !! Decay rate of snow albedo value for each vegetation type
                                                                !! as it will be used in condveg_snow (unitless)
                                                                !! Source : Values are from the Thesis of S. Chalita (1992)
!$OMP THREADPRIVATE(snowa_dec)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: alb_leaf_vis  !! leaf albedo of vegetation type, visible albedo (unitless)
!$OMP THREADPRIVATE(alb_leaf_vis)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: alb_leaf_nir  !! leaf albedo of vegetation type, near infrared albedo (unitless)
!$OMP THREADPRIVATE(alb_leaf_nir)
  !chaoyue+
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: permafrost_veg_exists!! leaf albedo of vegetation type, near infrared albedo (unitless)
!$OMP THREADPRIVATE(permafrost_veg_exists)
  !chaoyue-

  !
  ! SOIL - VEGETATION
  !
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pref_soil_veg      !! Table which contains the correlation between the soil
                                                                        !! types and vegetation type. Two modes exist :
                                                                        !! 1) pref_soil_veg = 0 then we have an equidistribution
                                                                        !!    of vegetation on soil types
                                                                        !! 2) Else for each pft the prefered soil type is given :
                                                                        !!    1=sand, 2=loan, 3=clay
                                                                        !! This variable is initialized in slowproc.(1-3, unitless)
!$OMP THREADPRIVATE(pref_soil_veg)

  !
  ! PHOTOSYNTHESIS
  !
  !-
  ! 1. CO2
  !-
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: is_c4             !! flag for C4 vegetation types (true/false)
!$OMP THREADPRIVATE(is_c4)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: vcmax_fix     !! values used for vcmax when STOMATE is not activated
                                                                !! @tex $(\mu mol.m^{-2}.s^{-1})$ @endtex
!$OMP THREADPRIVATE(vcmax_fix)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: downregulation_co2_coeff !! Coefficient for CO2 downregulation (unitless)
!$OMP THREADPRIVATE(downregulation_co2_coeff)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: E_KmC         !! Energy of activation for KmC (J mol-1)
!$OMP THREADPRIVATE(E_KmC)                                                               
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: E_KmO         !! Energy of activation for KmO (J mol-1)
!$OMP THREADPRIVATE(E_KmO)          
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: E_gamma_star  !! Energy of activation for gamma_star (J mol-1)
!$OMP THREADPRIVATE(E_gamma_star)    
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: E_Vcmax       !! Energy of activation for Vcmax (J mol-1) 
!$OMP THREADPRIVATE(E_Vcmax)                                                              
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: E_Jmax        !! Energy of activation for Jmax (J mol-1)
!$OMP THREADPRIVATE(E_Jmax) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: aSV           !! a coefficient of the linear regression (a+bT) defining the Entropy term for Vcmax (J K-1 mol-1)
!$OMP THREADPRIVATE(aSV)    
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: bSV           !! b coefficient of the linear regression (a+bT) defining the Entropy term for Vcmax (J K-1 mol-1 °C-1)
!$OMP THREADPRIVATE(bSV)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tphoto_min   !! minimum photosynthesis temperature (deg C)
!$OMP THREADPRIVATE(tphoto_min) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tphoto_max   !! maximum photosynthesis temperature (deg C)
!$OMP THREADPRIVATE(tphoto_max) 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: aSJ           !! a coefficient of the linear regression (a+bT) defining the Entropy term for Jmax (J K-1 mol-1)
!$OMP THREADPRIVATE(aSJ)    
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: bSJ           !! b coefficient of the linear regression (a+bT) defining the Entropy term for Jmax (J K-1 mol-1 °C-1)
!$OMP THREADPRIVATE(bSJ)    
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: D_Vcmax       !! Energy of deactivation for Vcmax (J mol-1)
!$OMP THREADPRIVATE(D_Vcmax)                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: D_Jmax        !! Energy of deactivation for Jmax (J mol-1)
!$OMP THREADPRIVATE(D_Jmax)                                    
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: E_Rd          !! Energy of activation for Rd (J mol-1)
!$OMP THREADPRIVATE(E_Rd)                                      
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: Vcmax25       !! Maximum rate of Rubisco activity-limited carboxylation at 25°C
                                                                !! @tex $(\mu mol.m^{-2}.s^{-1})$ @endtex
!$OMP THREADPRIVATE(Vcmax25)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: arJV          !! a coefficient of the linear regression (a+bT) defining the Jmax25/Vcmax25 ratio (mu mol e- (mu mol CO2)-1)
!$OMP THREADPRIVATE(arJV)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: brJV          !! b coefficient of the linear regression (a+bT) defining the Jmax25/Vcmax25 ratio (mu mol e- (mu mol CO2)-1)
!$OMP THREADPRIVATE(brJV)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: KmC25         !! Michaelis–Menten constant of Rubisco for CO2 at 25°C (ubar)
!$OMP THREADPRIVATE(KmC25)                                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: KmO25         !! Michaelis–Menten constant of Rubisco for O2 at 25°C (ubar)
!$OMP THREADPRIVATE(KmO25)                
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: gamma_star25  !! Ci-based CO2 compensation point in the absence of Rd at 25°C (ubar)
!$OMP THREADPRIVATE(gamma_star25)       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: a1            !! Empirical factor involved in the calculation of fvpd (-)
!$OMP THREADPRIVATE(a1)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: b1            !! Empirical factor involved in the calculation of fvpd (-)
!$OMP THREADPRIVATE(b1)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: g0            !! Residual stomatal conductance when irradiance approaches zero (mol m−2 s−1 bar−1)
!$OMP THREADPRIVATE(g0)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: h_protons     !! Number of protons required to produce one ATP (mol mol-1)
!$OMP THREADPRIVATE(h_protons)                                 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: fpsir         !! Fraction of PSII e− transport rate partitioned to the C4 cycle (-)
!$OMP THREADPRIVATE(fpsir)                                         
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: fQ            !! Fraction of electrons at reduced plastoquinone that follow the Q-cycle (-) - Values for C3 platns are not used
!$OMP THREADPRIVATE(fQ)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: fpseudo       !! Fraction of electrons at PSI that follow  pseudocyclic transport (-) - Values for C3 platns are not used
!$OMP THREADPRIVATE(fpseudo)                                   
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: kp            !! Initial carboxylation efficiency of the PEP carboxylase (mol m−2 s−1 bar−1) 
!$OMP THREADPRIVATE(kp)                                        
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: alpha         !! Fraction of PSII activity in the bundle sheath (-)
!$OMP THREADPRIVATE(alpha)                                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: gbs           !! Bundle-sheath conductance (mol m−2 s−1 bar−1)
!$OMP THREADPRIVATE(gbs)                                       
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: theta         !! Convexity factor for response of J to irradiance (-)
!$OMP THREADPRIVATE(theta)                                     
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: alpha_LL      !! Conversion efficiency of absorbed light into J at strictly limiting light (mol e− (mol photon)−1)
!$OMP THREADPRIVATE(alpha_LL)


  !-
  ! 2. Stomate
  !-
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ext_coeff     !! extinction coefficient of the Monsi&Saeki relationship (1953)
                                                                !! (unitless)
!$OMP THREADPRIVATE(ext_coeff)


  !
  ! ALLOCATION (stomate)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: R0            !! Default root allocation (0-1, unitless)
!$OMP THREADPRIVATE(R0)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: S0            !! Default sapwood allocation (0-1, unitless)
!$OMP THREADPRIVATE(S0)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: L0            !! Default leaf allocation (0-1, unitless)
!$OMP THREADPRIVATE(L0)


  !
  ! RESPIRATION (stomate)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: frac_growthresp  !! fraction of GPP which is lost as growth respiration

!$OMP THREADPRIVATE(frac_growthresp)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: maint_resp_slope  !! slope of maintenance respiration coefficient 
                                                                      !! (1/K, 1/K^2, 1/K^3), used in the code
!$OMP THREADPRIVATE(maint_resp_slope)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: maint_resp_slope_c  !! slope of maintenance respiration coefficient (1/K),
                                                                      !! constant c of aT^2+bT+c , tabulated
!$OMP THREADPRIVATE(maint_resp_slope_c)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: maint_resp_slope_b  !! slope of maintenance respiration coefficient (1/K), 
                                                                      !! constant b of aT^2+bT+c , tabulated
!$OMP THREADPRIVATE(maint_resp_slope_b)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: maint_resp_slope_a  !! slope of maintenance respiration coefficient (1/K), 
                                                                      !! constant a of aT^2+bT+c , tabulated
!$OMP THREADPRIVATE(maint_resp_slope_a)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: coeff_maint_zero  !! maintenance respiration coefficient at 0 deg C, 
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(coeff_maint_zero)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_leaf        !! maintenance respiration coefficient at 0 deg C,
                                                                      !! for leaves, tabulated 
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_leaf)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_sapabove    !! maintenance respiration coefficient at 0 deg C,
                                                                      !! for sapwood above, tabulated
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_sapabove)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_sapbelow    !! maintenance respiration coefficient at 0 deg C,
                                                                      !! for sapwood below, tabulated 
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_sapbelow)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_heartabove  !! maintenance respiration coefficient at 0 deg C
                                                                      !! for heartwood above, tabulated 
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_heartabove)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_heartbelow  !! maintenance respiration coefficient at 0 deg C,
                                                                      !! for heartwood below, tabulated 
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_heartbelow)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_root        !! maintenance respiration coefficient at 0 deg C,
                                                                      !! for roots, tabulated
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_root)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_fruit       !! maintenance respiration coefficient  at 0 deg C,
                                                                      !! for fruits, tabulated
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_fruit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cm_zero_carbres     !! maintenance respiration coefficient at 0 deg C,
                                                                      !! for carbohydrate reserve, tabulated
                                                                      !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex
!$OMP THREADPRIVATE(cm_zero_carbres)

 
  !
  ! FIRE (stomate)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: flam              !! flamability : critical fraction of water holding 
                                                                    !! capacity (0-1, unitless)
!$OMP THREADPRIVATE(flam)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: resist            !! fire resistance (0-1, unitless)
!$OMP THREADPRIVATE(resist)
  !spitfire
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: dens_fuel         !! fuel bulk density
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: f_sh              !! scorch height parameter for crown fire
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: crown_length      !! crown length
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: BTpar1            !! Bark thickness parameter
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: BTpar2            !! Bark thickness parameter
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: r_ck              !! parameter for postfire mortality as a result of crown damage
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: p_ck              !! parameter for postfire mortality as a result of crown damage
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ef_CO2            !! emissions factors
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ef_CO
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ef_CH4
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ef_VOC
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ef_TPM
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ef_NOx
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: me                !! flammability threshold
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: fire_max_cf_100hr       !! Maximum combustion fraction for 100hr and 1000hr fuel for different PFTs
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: fire_max_cf_1000hr       !! Maximum combustion fraction for 100hr and 1000hr fuel for different PFTs
  !endspit

  !
  ! grassland management
  !
  !JCADD
  ! Is the vegetation type a managed grassland ?
  LOGICAL,ALLOCATABLE, SAVE, DIMENSION (:) :: is_grassland_manag
  ! Is the vegetation type a cut grassland for management adaptation ?
  LOGICAL,ALLOCATABLE, SAVE, DIMENSION (:) :: is_grassland_cut
  ! Is the vegetation type a grazed grassland for management adaptation ?
  LOGICAL,ALLOCATABLE, SAVE, DIMENSION (:) :: is_grassland_grazed
  ! Management Intensity reading in MANAGEMENT_MAP
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: management_intensity
  ! Start year of management reading in MANAGEMENT_MAP & GRAZING_MAP
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: management_start
  ! Start year of N deposition reading in DEPOSITION_MAP
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: deposition_start
  ! Number of year that management should be read
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: nb_year_management
  ! maximum specific leaf area (m**2/gC)
  REAL(r_std),  ALLOCATABLE, SAVE, DIMENSION(:) :: sla_max
  ! minimum specific leaf area (m**2/gC)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)  :: sla_min
  !ENDJCADD

  !
  ! FLUX - LUC (Land Use Change)
  !
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: coeff_lcchange_1   !! Coeff of biomass export for the year (unitless)
!$OMP THREADPRIVATE(coeff_lcchange_1)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: coeff_lcchange_10  !! Coeff of biomass export for the decade (unitless)
!$OMP THREADPRIVATE(coeff_lcchange_10)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: coeff_lcchange_100 !! Coeff of biomass export for the century (unitless)
!$OMP THREADPRIVATE(coeff_lcchange_100)
 
 
  !
  ! PHENOLOGY
  !
  !-
  ! 1. Stomate
  !-
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: lai_max_to_happy  !! threshold of LAI below which plant uses carbohydrate reserves
!$OMP THREADPRIVATE(lai_max_to_happy)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: lai_max           !! maximum LAI, PFT-specific @tex $(m^2.m^{-2})$ @endtex
!$OMP THREADPRIVATE(lai_max)

  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pheno_type     !! type of phenology (0-4, unitless)
                                                                    !! 0=bare ground 1=evergreen,  2=summergreen, 
                                                                    !! 3=raingreen,  4=perennial
                                                                    !! For the moment, the bare ground phenotype is not managed, 
                                                                    !! so it is considered as "evergreen"
!$OMP THREADPRIVATE(pheno_type)

  !-
  ! 2. Leaf Onset
  !-
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: pheno_gdd_crit   !! critical gdd,tabulated (C), used in the code
!$OMP THREADPRIVATE(pheno_gdd_crit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pheno_gdd_crit_c   !! critical gdd,tabulated (C), 
                                                                     !! constant c of aT^2+bT+c (unitless)
!$OMP THREADPRIVATE(pheno_gdd_crit_c)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pheno_gdd_crit_b   !! critical gdd,tabulated (C), 
                                                                     !! constant b of aT^2+bT+c (unitless)
!$OMP THREADPRIVATE(pheno_gdd_crit_b)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pheno_gdd_crit_a   !! critical gdd,tabulated (C), 
                                                                     !! constant a of aT^2+bT+c (unitless)
!$OMP THREADPRIVATE(pheno_gdd_crit_a)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pheno_moigdd_t_crit!! Monthly avearage temperature treashold used for C4 grass (C)
!$OMP THREADPRIVATE(pheno_moigdd_t_crit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ngd_crit           !! critical ngd,tabulated. Threshold -5 degrees (days)
!$OMP THREADPRIVATE(ngd_crit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ncdgdd_temp        !! critical temperature for the ncd vs. gdd function
                                                                     !! in phenology (C)
!$OMP THREADPRIVATE(ncdgdd_temp)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: hum_frac           !! critical humidity (relative to min/max) for phenology
                                                                     !! (0-1, unitless)
!$OMP THREADPRIVATE(hum_frac)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: hum_min_time       !! minimum time elapsed since moisture minimum (days)
!$OMP THREADPRIVATE(hum_min_time)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tau_sap            !! sapwood -> heartwood conversion time (days)
!$OMP THREADPRIVATE(tau_sap)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tau_fruit          !! fruit lifetime (days)
!$OMP THREADPRIVATE(tau_fruit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tau_leafinit  !! time to attain the initial foliage using the carbohydrate reserve
!$OMP THREADPRIVATE(tau_leafinit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: ecureuil           !! fraction of primary leaf and root allocation put
                                                                     !! into reserve (0-1, unitless)
!$OMP THREADPRIVATE(ecureuil)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: alloc_min          !! NEW - allocation above/below = f(age) - 30/01/04 NV/JO/PF
!$OMP THREADPRIVATE(alloc_min)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: alloc_max          !! NEW - allocation above/below = f(age) - 30/01/04 NV/JO/PF
!$OMP THREADPRIVATE(alloc_max)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: demi_alloc         !! NEW - allocation above/below = f(age) - 30/01/04 NV/JO/PF
!$OMP THREADPRIVATE(demi_alloc)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: leaflife_tab       !! leaf longevity, tabulated (??units??)
!$OMP THREADPRIVATE(leaflife_tab)

  !-
  ! 3. Senescence
  !-
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: leaffall              !! length of death of leaves,tabulated (days)
!$OMP THREADPRIVATE(leaffall)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: leafagecrit           !! critical leaf age,tabulated (days)
!$OMP THREADPRIVATE(leafagecrit)

  CHARACTER(len=6), ALLOCATABLE, SAVE, DIMENSION(:) :: senescence_type  !! type of senescence,tabulated (unitless)
                                                                        !! List of avaible types of senescence :
                                                                        !! 'cold  ', 'dry   ', 'mixed ', 'none  '
!$OMP THREADPRIVATE(senescence_type)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: senescence_hum        !! critical relative moisture availability for senescence
                                                                        !! (0-1, unitless)
!$OMP THREADPRIVATE(senescence_hum)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: nosenescence_hum      !! relative moisture availability above which there is
                                                                        !! no humidity-related senescence (0-1, unitless)
!$OMP THREADPRIVATE(nosenescence_hum)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: max_turnover_time     !! maximum turnover time for grasses (days)
!$OMP THREADPRIVATE(max_turnover_time)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: min_turnover_time     !! minimum turnover time for grasses (days)
!$OMP THREADPRIVATE(min_turnover_time)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: min_leaf_age_for_senescence  !! minimum leaf age to allow senescence g (days)
!$OMP THREADPRIVATE(min_leaf_age_for_senescence)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: senescence_temp     !! critical temperature for senescence (C),
                                                                        !! used in the code
!$OMP THREADPRIVATE(senescence_temp)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: senescence_temp_c     !! critical temperature for senescence (C), 
                                                                        !! constant c of aT^2+bT+c , tabulated (unitless)
!$OMP THREADPRIVATE(senescence_temp_c)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: senescence_temp_b     !! critical temperature for senescence (C), 
                                                                        !! constant b of aT^2+bT+c , tabulated (unitless)
!$OMP THREADPRIVATE(senescence_temp_b)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: senescence_temp_a     !! critical temperature for senescence (C),
                                                                        !! constant a of aT^2+bT+c , tabulated (unitless)
!$OMP THREADPRIVATE(senescence_temp_a)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: gdd_senescence        !! minimum gdd to allow senescence of crops (days)
!$OMP THREADPRIVATE(gdd_senescence)

  !
  ! DGVM
  !

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: residence_time        !! residence time of trees (y) 
!$OMP THREADPRIVATE(residence_time)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tmin_crit             !! critical tmin, tabulated (C)
!$OMP THREADPRIVATE(tmin_crit)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tcm_crit              !! critical tcm, tabulated (C)
!$OMP THREADPRIVATE(tcm_crit)

  !
  ! Biogenic Volatile Organic Compounds
  !

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_isoprene       !! Isoprene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_isoprene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_monoterpene    !! Monoterpene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_monoterpene)

  REAL(r_std), SAVE :: LDF_mono                                            !! monoterpenes fraction dependancy to light
!$OMP THREADPRIVATE(LDF_mono) 
  REAL(r_std), SAVE :: LDF_sesq                                            !! sesquiterpenes fraction dependancy to light
!$OMP THREADPRIVATE(LDF_sesq)
  REAL(r_std), SAVE :: LDF_meth                                            !! methanol fraction dependancy to light
!$OMP THREADPRIVATE(LDF_meth)
  REAL(r_std), SAVE :: LDF_acet                                            !! acetone fraction dependancy to light
!$OMP THREADPRIVATE(LDF_acet)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_apinene        !! Alfa pinene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_apinene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_bpinene        !! Beta pinene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_bpinene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_limonene       !! Limonene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_limonene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_myrcene        !! Myrcene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_myrcene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_sabinene       !! Sabinene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_sabinene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_camphene       !! Camphene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_camphene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_3carene        !! 3-carene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_3carene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_tbocimene      !! T-beta-ocimene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_tbocimene)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_othermonot     !! Other monoterpenes emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_othermonot)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_sesquiterp     !! Sesquiterpene emission factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_sesquiterp)

  REAL(r_std), SAVE :: beta_mono                                           !! Monoterpenes temperature dependency coefficient 
!$OMP THREADPRIVATE(beta_mono)
  REAL(r_std), SAVE :: beta_sesq                                           !! Sesquiterpenes temperature dependency coefficient
!$OMP THREADPRIVATE(beta_sesq)
  REAL(r_std), SAVE :: beta_meth                                           !! Methanol temperature dependency coefficient 
!$OMP THREADPRIVATE(beta_meth)
  REAL(r_std), SAVE :: beta_acet                                           !! Acetone temperature dependency coefficient
!$OMP THREADPRIVATE(beta_acet)
  REAL(r_std), SAVE :: beta_oxyVOC                                         !! Other oxygenated BVOC temperature dependency coefficient
!$OMP THREADPRIVATE(beta_oxyVOC)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_ORVOC          !! ORVOC emissions factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_ORVOC)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_OVOC           !! OVOC emissions factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_OVOC)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_MBO            !! MBO emissions factor 
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_MBO)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_methanol       !! Methanol emissions factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_methanol)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_acetone        !! Acetone emissions factor 
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_acetone)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_acetal         !! Acetaldehyde emissions factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_acetal)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_formal         !! Formaldehyde emissions factor
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_formal)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_acetic         !! Acetic Acid emissions factor 
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_acetic)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_formic         !! Formic Acid emissions factor 
                                                                           !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_formic)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_no_wet         !! NOx emissions factor soil emissions and 
                                                                           !! exponential dependancy factor for wet soils
                                                                           !! @tex $(ngN.m^{-2}.s^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_no_wet)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: em_factor_no_dry         !! NOx emissions factor soil emissions and
                                                                           !! exponential dependancy factor for dry soils
                                                                           !! @tex $(ngN.m^{-2}.s^{-1})$ @endtex
!$OMP THREADPRIVATE(em_factor_no_dry)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: Larch                    !! Larcher 1991 SAI/LAI ratio (unitless)
!$OMP THREADPRIVATE(Larch)

  !
  ! INTERNAL PARAMETERS USED IN STOMATE_DATA
  !

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: lai_initmin   !! Initial lai for trees/grass 
                                                                !! @tex $(m^2.m^{-2})$ @endtex
!$OMP THREADPRIVATE(lai_initmin)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: bm_sapl   !! sapling biomass @tex $(gC.ind^{-1})$ @endtex
!$OMP THREADPRIVATE(bm_sapl)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: migrate       !! migration speed @tex $(m.year^{-1})$ @endtex
!$OMP THREADPRIVATE(migrate)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: maxdia        !! maximum stem diameter from which on crown area no longer 
                                                                !! increases (m)
!$OMP THREADPRIVATE(maxdia)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: cn_sapl       !! crown of tree when sapling  @tex $(m^2$)$ @endtex
!$OMP THREADPRIVATE(cn_sapl)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: leaf_timecst  !! time constant for leaf age discretisation (days)
!$OMP THREADPRIVATE(leaf_timecst)


!
! WETLAND CH4 methane
!
!pss+
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: sdepth_v  !! soil depth for wetland vegetation types
!$OMP THREADPRIVATE(sdepth_v)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: rdepth_v  !! rooting depth for wetland vegetation types
!$OMP THREADPRIVATE(rdepth_v)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: tveg_v  !! Plant mediated transport efficiency
!$OMP THREADPRIVATE(tveg_v)
!pss-

!!!! cropland parameters


  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: irrig_threshold   !! Value for stress threshold to start irrigation (0-1, vegstress)
!$OMP THREADPRIVATE(irrig_threshold)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: irrig_fulfill     !! Value for fulfilling irrigation demand (0-1)
!$OMP THREADPRIVATE(irrig_fulfill)

  ! 4. STICS
  ! 4.0 
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: ok_LAIdev     !! flag for using the STICS-LAIdev module    
!$OMP THREADPRIVATE(ok_LAIdev)
  ! 4.1 LAIdev module
  CHARACTER(len=3), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codeplante
!$OMP THREADPRIVATE(SP_codeplante)
  CHARACTER(len=3), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stade0
!$OMP THREADPRIVATE(SP_stade0)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_iplt0
!$OMP THREADPRIVATE(SP_iplt0)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_iplt1
!$OMP THREADPRIVATE(SP_iplt1)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_iplt2
!$OMP THREADPRIVATE(SP_iplt2)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_nbox
!$OMP THREADPRIVATE(SP_nbox)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_iwater 
!$OMP THREADPRIVATE(SP_iwater)
  CHARACTER(len=7), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codesimul
!$OMP THREADPRIVATE(SP_codesimul)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codelaitr
!$OMP THREADPRIVATE(SP_codelaitr)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_slamax
!$OMP THREADPRIVATE(SP_slamax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_slamin
!$OMP THREADPRIVATE(SP_slamin)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codeperenne    !! annual crop (1) or perennial crop (2)
!$OMP THREADPRIVATE(SP_codeperenne)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codcueille     !! harvest option: cutting (1) or picking (2)
!$OMP THREADPRIVATE(SP_codcueille)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codegdh        !! hourly (1) or daily (2) calculation of development unit 
!$OMP THREADPRIVATE(SP_codegdh)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codetemp       !! 
!$OMP THREADPRIVATE(SP_codetemp)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_coderetflo
!$OMP THREADPRIVATE(SP_coderetflo)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codeinnact
!$OMP THREADPRIVATE(SP_codeinnact)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codeh2oact
!$OMP THREADPRIVATE(SP_codeh2oact)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stressdev
!$OMP THREADPRIVATE(SP_stressdev)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_innlai
!$OMP THREADPRIVATE(SP_innlai)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_innsenes
!$OMP THREADPRIVATE(SP_innsenes)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codebfroid
!$OMP THREADPRIVATE(SP_codebfroid)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codephot
!$OMP THREADPRIVATE(SP_codephot)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codedormance
!$OMP THREADPRIVATE(SP_codedormance)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codefauche     !! option of cut modes for forage crops: yes (1), no (2)
!$OMP THREADPRIVATE(SP_codefauche)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codetempfauche !! option of the reference temperature to compute cutting sum of temperatures : upvt (1), udevair (2)
!$OMP THREADPRIVATE(SP_codetempfauche)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codlainet
!$OMP THREADPRIVATE(SP_codlainet)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codeindetermin
!$OMP THREADPRIVATE(SP_codeindetermin)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codeinitprec
!$OMP THREADPRIVATE(SP_codeinitprec)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_culturean 
!$OMP THREADPRIVATE(SP_culturean)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_jvc
!$OMP THREADPRIVATE(SP_jvc)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tfroid
!$OMP THREADPRIVATE(SP_tfroid)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_ampfroid 
!$OMP THREADPRIVATE(SP_ampfroid)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_jvcmini
!$OMP THREADPRIVATE(SP_jvcmini)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgmin
!$OMP THREADPRIVATE(SP_tgmin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stpltger
!$OMP THREADPRIVATE(SP_stpltger)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_profsem
!$OMP THREADPRIVATE(SP_profsem)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_propjgermin              !! minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax 
!$OMP THREADPRIVATE(SP_propjgermin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tdmax
!$OMP THREADPRIVATE(SP_tdmax)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_nbjgerlim              !! Threshold number of day after grain imbibition without germination lack // days
!$OMP THREADPRIVATE(SP_nbjgerlim)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_densitesem
!$OMP THREADPRIVATE(SP_densitesem)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_vigueurbat                !!  indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 //
!$OMP THREADPRIVATE(SP_vigueurbat)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codepluiepoquet              !!  option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2
!$OMP THREADPRIVATE(SP_codepluiepoquet)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codehypo
!$OMP THREADPRIVATE(SP_codehypo)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_elmax
!$OMP THREADPRIVATE(SP_elmax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_belong
!$OMP THREADPRIVATE(SP_belong)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_celong
!$OMP THREADPRIVATE(SP_celong)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_nlevlim1             !! number of days after germination decreasing the emerged plants if emergence has not occur // days 
!$OMP THREADPRIVATE(SP_nlevlim1)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_nlevlim2             !! number of days after germination after which the emerged plants are null // days 
!$OMP THREADPRIVATE(SP_nlevlim2)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codrecolte           !! harvest mode : all the plant (1) or just the fruits (2)
!$OMP THREADPRIVATE(SP_codrecolte)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_variete              !! variety number in the technical file // SD
!$OMP THREADPRIVATE(SP_variete)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codegermin           !! option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2)
!$OMP THREADPRIVATE(SP_codegermin)

  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: S_codeulaivernal
!$OMP THREADPRIVATE(S_codeulaivernal)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_swfacmin
!$OMP THREADPRIVATE(SP_swfacmin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_neffmax
!$OMP THREADPRIVATE(SP_neffmax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_nsatrat
!$OMP THREADPRIVATE(SP_nsatrat)

  ! 4.2  STICS:: LAI CALCULATION
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_laiplantule
!$OMP THREADPRIVATE(SP_laiplantule)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_vlaimax
!$OMP THREADPRIVATE(SP_vlaimax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stlevamf
!$OMP THREADPRIVATE(SP_stlevamf)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stamflax
!$OMP THREADPRIVATE(SP_stamflax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_udlaimax
!$OMP THREADPRIVATE(SP_udlaimax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_laicomp
!$OMP THREADPRIVATE(SP_laicomp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_adens                   !! Interplant competition parameter 
!$OMP THREADPRIVATE(SP_adens)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_bdens
!$OMP THREADPRIVATE(SP_bdens)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tcxstop
!$OMP THREADPRIVATE(SP_tcxstop)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tcmax
!$OMP THREADPRIVATE(SP_tcmax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tcmin
!$OMP THREADPRIVATE(SP_tcmin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_dlaimax
!$OMP THREADPRIVATE(SP_dlaimax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_dlaimin
!$OMP THREADPRIVATE(SP_dlaimin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_pentlaimax
!$OMP THREADPRIVATE(SP_pentlaimax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tigefeuil               !! stem (structural part)/leaf proportion // SD  
!$OMP THREADPRIVATE(SP_tigefeuil)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stlaxsen
!$OMP THREADPRIVATE(SP_stlaxsen)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stsenlan
!$OMP THREADPRIVATE(SP_stsenlan)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stlevdrp
!$OMP THREADPRIVATE(SP_stlevdrp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stflodrp
!$OMP THREADPRIVATE(SP_stflodrp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stdrpmat
!$OMP THREADPRIVATE(SP_stdrpmat)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_stdrpdes
!$OMP THREADPRIVATE(SP_stdrpdes)
  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_phyllotherme
!$OMP THREADPRIVATE(SP_phyllotherme)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_lai0
!$OMP THREADPRIVATE(SP_lai0)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tustressmin
!$OMP THREADPRIVATE(SP_tustressmin)



  ! 4.3  STICS:: LAI SENESCENCE  
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_nbfgellev
!$OMP THREADPRIVATE(SP_nbfgellev)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_ratiodurvieI
!$OMP THREADPRIVATE(SP_ratiodurvieI)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_durvieF
!$OMP THREADPRIVATE(SP_durvieF)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_ratiosen
!$OMP THREADPRIVATE(SP_ratiosen)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tdmin
!$OMP THREADPRIVATE(SP_tdmin)
  
  ! 4.4 STICS:: F_humerac

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_sensrsec
!$OMP THREADPRIVATE(SP_sensrsec)

  ! 4.5 STICS:: gel

  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codgellev
!$OMP THREADPRIVATE(SP_codgellev)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codgeljuv
!$OMP THREADPRIVATE(SP_codgeljuv)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_codgelveg
!$OMP THREADPRIVATE(SP_codgelveg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tletale
!$OMP THREADPRIVATE(SP_tletale)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tdebgel
!$OMP THREADPRIVATE(SP_tdebgel)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgellev10
!$OMP THREADPRIVATE(SP_tgellev10)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgellev90
!$OMP THREADPRIVATE(SP_tgellev90)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgeljuv10
!$OMP THREADPRIVATE(SP_tgeljuv10)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgeljuv90
!$OMP THREADPRIVATE(SP_tgeljuv90)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgelveg10
!$OMP THREADPRIVATE(SP_tgelveg10)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_tgelveg90
!$OMP THREADPRIVATE(SP_tgelveg90)


  ! 4.6 STICS:: Photoperiod
 
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_sensiphot
!$OMP THREADPRIVATE(SP_sensiphot)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_phosat
!$OMP THREADPRIVATE(SP_phosat)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: SP_phobase
!$OMP THREADPRIVATE(SP_phobase)
 
  ! 4.7 STICS:: carbon allocation 
  
  CHARACTER(len=3), ALLOCATABLE, SAVE, DIMENSION(:)   :: SP_stoprac
!$OMP THREADPRIVATE(SP_stoprac)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_zracplantule
!$OMP THREADPRIVATE(SP_zracplantule)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codtrophrac
!$OMP THREADPRIVATE(SP_codtrophrac)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_repracpermax
!$OMP THREADPRIVATE(SP_repracpermax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_repracpermin
!$OMP THREADPRIVATE(SP_repracpermin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_krepracperm
!$OMP THREADPRIVATE(SP_krepracperm)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_repracseumax
!$OMP THREADPRIVATE(SP_repracseumax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_repracseumin
!$OMP THREADPRIVATE(SP_repracseumin)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_krepracseu
!$OMP THREADPRIVATE(SP_krepracseu)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codetemprac
!$OMP THREADPRIVATE(SP_codetemprac)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codedyntalle
!$OMP THREADPRIVATE(SP_codedyntalle)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_nbjgrain
!$OMP THREADPRIVATE(SP_nbjgrain)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_maxgs
!$OMP THREADPRIVATE(SP_maxgs)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codgelflo
!$OMP THREADPRIVATE(SP_codgelflo)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_tgelflo10
!$OMP THREADPRIVATE(SP_tgelflo10)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_tgelflo90
!$OMP THREADPRIVATE(SP_tgelflo90)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_cgrain
!$OMP THREADPRIVATE(SP_cgrain)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_cgrainv0
!$OMP THREADPRIVATE(SP_cgrainv0)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_nbgrmax
!$OMP THREADPRIVATE(SP_nbgrmax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_nbgrmin
!$OMP THREADPRIVATE(SP_nbgrmin)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codazofruit
!$OMP THREADPRIVATE(SP_codazofruit)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codeir
!$OMP THREADPRIVATE(SP_codeir)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_vitircarb
!$OMP THREADPRIVATE(SP_vitircarb)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_irmax
!$OMP THREADPRIVATE(SP_irmax)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_vitircarbT
!$OMP THREADPRIVATE(SP_vitircarbT)
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:)     :: SP_codetremp
!$OMP THREADPRIVATE(SP_codetremp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_tminremp
!$OMP THREADPRIVATE(SP_tminremp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_tmaxremp
!$OMP THREADPRIVATE(SP_tmaxremp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_pgrainmaxi
!$OMP THREADPRIVATE(SP_pgrainmaxi)

!! for  dynamic nitrogen processes 

LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:) :: SP_DY_INN     !! flag for activating the dynamic nitrogen processes  
!$OMP THREADPRIVATE(SP_DY_INN)

REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:)        :: SP_avenfert
!$OMP THREADPRIVATE(SP_avenfert)

!!!! end cropland parameters, xuhui


! STOMATE - Age classes

  INTEGER(i_std), SAVE                            :: nvmap          !! The number of PFTs we have if we ignore age classes.
                                                                    !! @tex $-$ @endtex
!$OMP THREADPRIVATE(nvmap) 
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: agec_group     !! The age class group that this PFT belongs to.
                                                                    !! If you're not using age classes, this will just be
                                                                    !! set to the number of the PFT and should be ignored
                                                                    !! in the code.
                                                                    !! @tex $-$ @endtex
!$OMP THREADPRIVATE(nvmap) 
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: age_class_bound!! The age class bounds used to allow age class to move
                                                                    !! to next one.
!$OMP THREADPRIVATE(agec_groups) 
! I do not like the location of these next two variables.  They are computed
! after agec_group is read in.  Ideally, they would be passed around
! as arguments or in a structure, since they are not really
! parameters read in from the input file.
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: start_index    !! Gives the index that this real PFT starts
                                                                    !! on, ignoring age classes
                                                                    !! @tex $-$ @endtex
!$OMP THREADPRIVATE(start_index) 
  INTEGER(i_std), ALLOCATABLE, SAVE, DIMENSION(:) :: nagec_pft      !! The number of age classes for each PFT.
                                                                    !! @tex $-$ @endtex
!$OMP THREADPRIVATE(nagec_pft) 
END MODULE pft_parameters_var
