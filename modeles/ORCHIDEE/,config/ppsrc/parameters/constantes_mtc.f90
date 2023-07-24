










! =================================================================================================================================
! MODULE       : constantes_mtc
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2011)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF         This module contains the standard values of the parameters for the 13 metaclasses of vegetation used by ORCHIDEE.
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): 
!!
!! REFERENCE(S)	: 
!! - Kuppel, S. (2012): Doctoral Thesis, Assimilation de mesures de flux turbulents d'eau et de carbone dans un modèle de la biosphère 
!! continentale 
!! - Kuppel, S., Peylin, P., Chevallier, F., Bacour, C., Maignan, F., and Richardson, A. D. (2012). Constraining a global ecosystem
!! model with multi-site eddy-covariance data, Biogeosciences, 9, 3757-3776, DOI 10.5194/bg-9-3757-2012.
!! - Wohlfahrt, G., M. Bahn, E. Haubner, I. Horak, W. Michaeler, K.Rottmar, U. Tappeiner, and A. Cemusca, 1999: Inter-specific 
!! variation of the biochemical limitation to photosynthesis and related leaf traits of 30 species from mountain grassland 
!! ecosystems under different land use. Plant Cell Environ., 22, 12811296.
!! - Malhi, Y., Doughty, C., and Galbraith, D. (2011). The allocation of ecosystem net primary productivity in tropical forests, 
!! Philosophical Transactions of the Royal Society B-Biological Sciences, 366, 3225-3245, DOI 10.1098/rstb.2011.0062.
!! - Earles, J. M., Yeh, S., and Skog, K. E. (2012). Timing of carbon emissions from global forest clearance, Nature Climate Change, 2, 
!! 682-685, Doi 10.1038/Nclimate1535.
!! - Piao, S. L., Luyssaert, S., Ciais, P., Janssens, I. A., Chen, A. P., Cao, C., Fang, J. Y., Friedlingstein, P., Luo, Y. Q., and 
!! Wang, S. P. (2010). Forest annual carbon cost: A global-scale analysis of autotrophic respiration, Ecology, 91, 652-661, 
!! Doi 10.1890/08-2176.1.
!! - Verbeeck, H., Peylin, P., Bacour, C., Bonal, D., Steppe, K., and Ciais, P. (2011). Seasonal patterns of co2 fluxes in amazon 
!! forests: Fusion of eddy covariance data and the orchidee model, Journal of Geophysical Research-Biogeosciences, 116, 
!! Artn G02018, Doi 10.1029/2010jg001544.
!!
!! SVN          :
!! $HeadURL: $
!! $Date: 2016-04-26 13:28:48 +0200 (Tue, 26 Apr 2016) $
!! $Revision: 3386 $
!! \n
!_ ================================================================================================================================

MODULE constantes_mtc

  USE defprec
  USE constantes

  IMPLICIT NONE

  !
  ! METACLASSES CHARACTERISTICS
  !

  INTEGER(i_std), PARAMETER :: nvmc = 14                         !! Number of MTCS fixed in the code (unitless)

  CHARACTER(len=34), PARAMETER, DIMENSION(nvmc) :: MTC_name = &  !! description of the MTC (unitless)
  & (/ 'bare ground                       ', &          !  1
  &    'tropical  broad-leaved evergreen  ', &          !  2
  &    'tropical  broad-leaved raingreen  ', &          !  3
  &    'temperate needleleaf   evergreen  ', &          !  4
  &    'temperate broad-leaved evergreen  ', &          !  5
  &    'temperate broad-leaved summergreen', &          !  6
  &    'boreal    needleleaf   evergreen  ', &          !  7
  &    'boreal    broad-leaved summergreen', &          !  8
  &    'boreal    needleleaf   summergreen', &          !  9
  &    '          C3           grass      ', &          ! 10
  &    '          C4           grass      ', &          ! 11
  &    '          wheat                   ', &          ! 12
  &    '          maize                   ', &          ! 13
  &    '          rice                    '  /)         ! 14


  !
  ! VEGETATION STRUCTURE
  !
  INTEGER(i_std),PARAMETER, DIMENSION(nvmc) :: leaf_tab_mtc  =  &                 !! leaf type (1-4, unitless)
  & (/  4,   1,   1,   2,   1,   1,   2,   &                                      !! 1=broad leaved tree, 2=needle leaved tree
  &     1,   2,   3,   3,   3,   3,   3   /)                                           !! 3=grass 4=bare ground

  CHARACTER(len=6), PARAMETER, DIMENSION(nvmc) :: pheno_model_mtc  =  &  !! which phenology model is used? (tabulated) 
  & (/  'none  ',   'none  ',   'moi   ',   'none  ',   'none  ',  &
  &     'ncdgdd',   'none  ',   'ncdgdd',   'ngd   ',   'moigdd',  &
  &     'moi_C4',   'moigdd',   'moigdd',   'moigdd'  /) 

  LOGICAL, PARAMETER, DIMENSION(nvmc) :: is_tropical_mtc  =  &                       !! Is PFT tropical ? (true/false)
  & (/ .FALSE.,   .TRUE.,    .TRUE.,    .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,  &
  &    .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.  /)    

!JCADD
  ! Is the vegetation a grassland that can be managed ?
  LOGICAL,PARAMETER, DIMENSION(nvmc) :: is_grassland_manag_mtc   =  &
  & (/ .FALSE., .FALSE.,  .FALSE., .FALSE., .FALSE.,  &
  &    .FALSE.,  .FALSE.,  .FALSE., .FALSE., .FALSE., &
  &    .FALSE., .FALSE., .FALSE.,  .FALSE.  /)
  ! Is the vegetation a grassland that can be cut ?
  LOGICAL,PARAMETER, DIMENSION(nvmc) :: is_grassland_cut_mtc   =  &
  & (/ .FALSE., .FALSE.,  .FALSE., .FALSE., .FALSE.,  &
  &    .FALSE.,  .FALSE.,  .FALSE., .FALSE., .FALSE., &
  &    .FALSE., .FALSE., .FALSE. , .FALSE. /)
  ! Is the vegetation a grassland that can be grazed ?
  LOGICAL,PARAMETER, DIMENSION(nvmc) :: is_grassland_grazed_mtc   =  &
  & (/ .FALSE., .FALSE.,  .FALSE., .FALSE., .FALSE.,  &
  &    .FALSE.,  .FALSE.,  .FALSE., .FALSE., .FALSE., &
  &    .FALSE., .FALSE., .FALSE. , .FALSE. /)
  ! Management Intensity reading in MANAGEMENT_MAP
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: management_intensity_mtc  =  &
  & (/ 0,   0,   0,   0,   0,   0,   0,  &
  &    0,   0,   0,   0,   0,   0,   0  /)
  ! Start year of management reading in MANAGEMENT_MAP & GRAZING_MAP
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: management_start_mtc  =  &
  & (/ 0,   0,   0,   0,   0,   0,   0,  &
  &    0,   0,   0,   0,   0,   0,   0  /)
  ! Start year of N deposition reading in DEPOSITION_MAP
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: deposition_start_mtc  =  &
  & (/ 0,   0,   0,   0,   0,   0,   0,  &
  &    0,   0,   0,   0,   0,   0,   0  /)
  ! Number of year that management should be read
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: nb_year_management_mtc  =  &
  & (/ 0,   0,   0,   0,   0,   0,   0,  &
  &    0,   0,   1,   1,   0,   0,   0  /)
  ! maximum specific leaf area (m**2/gC)
   REAL(r_std), PARAMETER, DIMENSION(nvmc)     ::    sla_max_mtc = &  !m2/gC
(/1.5E-2,1.53E-2, 2.6E-2,9.26E-3, 2E-2, 2.6E-2, 9.26E-3, &
 &                   2.6E-2, 1.9E-2, 2.6E-2,2.6E-2,2.6E-2,2.6E-2, 2.6E-2 /)
  ! miniimum specific leaf area (m**2/gC)
   REAL(r_std), PARAMETER, DIMENSION(nvmc)     ::    sla_min_mtc  = &
(/1.5E-2,1.53E-2, 2.6E-2,9.26E-3, 2E-2, 2.6E-2, 9.26E-3, &
 &                   2.6E-2, 1.9E-2, 2.6E-2,2.6E-2,2.6E-2,2.6E-2, 2.6E-2 /)
!ENDJCADD
  CHARACTER(LEN=5), PARAMETER, DIMENSION(nvmc) :: type_of_lai_mtc  =  &  !! Type of behaviour of the LAI evolution algorithm
  & (/ 'inter', 'inter', 'inter', 'inter', 'inter',  &                   !! for each vegetation type. (unitless)
  &    'inter', 'inter', 'inter', 'inter', 'inter',  &                   !! Value of type_of_lai : mean or interp
  &    'inter', 'inter', 'inter', 'inter' /)

  LOGICAL, PARAMETER, DIMENSION(nvmc) :: natural_mtc =  &                         !! natural?  (true/false)
  & (/ .TRUE.,   .TRUE.,   .TRUE.,   .TRUE.,   .TRUE.,    .TRUE.,   .TRUE.,  &
  &    .TRUE.,   .TRUE.,   .TRUE.,   .TRUE.,   .FALSE.,   .FALSE., .FALSE.  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: veget_ori_fixed_mtc  =  &  !! Value for veget_ori for tests in
  & (/ 0.2,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &                !! 0-dim simulations (0-1, unitless)
  &    0.0,   0.0,   0.8,   0.0,   0.0,   0.0,   0.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: llaimax_mtc  =  &          !! laimax for maximum
  & (/ 0.0,   8.0,   8.0,   4.0,   4.5,   4.5,   4.0,  &                !! See also type of lai interpolation
  &    4.5,   4.0,   2.0,   2.0,   2.0,   2.0,  2.0  /)                       !! @tex $(m^2.m^{-2})$ @endtex

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: llaimin_mtc  = &           !! laimin for minimum lai
  & (/ 0.0,   8.0,   0.0,   4.0,   4.5,   0.0,   4.0,  &                !! See also type of lai interpolation (m^2.m^{-2})
  &    0.0,   0.0,   0.0,   0.0,   0.0,   0.0, 0.0/)                       !! @tex $(m^2.m^{-2})$ @endtex

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: height_presc_mtc  =  &     !! prescribed height of vegetation (m)
  & (/  0.0,   30.0,   30.0,   20.0,   20.0,   20.0,   15.0,  &         !! Value for height_presc : one for each vegetation type
!  &    15.0,   15.0,    0.5,    0.6,    1.0,    1.0,   1.0/)        
  &    15.0,   15.0,    0.5,    0.6,    1.1,    2.1,   1.0/)            ! YANG SU  Grignon data


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: rveg_mtc  =  &             !! Potentiometer to set vegetation resistance (unitless)
  & (/ 1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0,  &                !! Nathalie on March 28th, 2006 - from Fred Hourdin,
  &    1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0   /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: sla_mtc  =  &                       !! specif leaf area @tex $(m^2.gC^{-1})$ @endtex
  & (/ 1.5E-2,   1.53E-2,   2.6E-2,   9.26E-3,     2E-2,   2.6E-2,   9.26E-3,  &
  &    2.6E-2,    1.9E-2,   2.6E-2,    2.6E-2,   2.6E-2,   2.6E-2,   2.6E-2  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: availability_fact_mtc  =  &     !! calculate mortality in lpj_gap
  & (/ undef,   0.14,  0.14,   0.10,   0.10,   0.10,   0.05,  &
  &     0.05,   0.05,  undef,  undef,  undef,  undef, undef  /)


  !
  ! EVAPOTRANSPIRATION (sechiba)
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: rstruct_const_mtc  =  &  !! Structural resistance.
  & (/ 0.0,   25.0,   25.0,   25.0,   25.0,   25.0,   25.0,  &        !! @tex $(s.m^{-1})$ @endtex
  &   25.0,   25.0,    2.5,    2.0,    2.0,    2.0,   2.0   /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: kzero_mtc  =  &                  !! A vegetation dependent constant used in the 
  & (/    0.0,   12.E-5,   12.E-5,   12.E-5,   12.E-5,   25.E-5,   12.E-5,  & !! calculation  of the surface resistance. 
  &    25.E-5,   25.E-5,   30.E-5,   30.E-5,   30.E-5,   30.E-5,  30.E-5  /)           !! @tex $(kg.m^2.s^{-1})$ @endtex


  !
  ! WATER (sechiba)
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: wmax_veg_mtc  =  &        !! Volumetric available soil water capacity in each PFT
  & (/ 150.0,   150.0,   150.0,   150.0,   150.0,   150.0,   150.0,  & !! @tex $(kg.m^{-3} of soil)$ @endtex
  &    150.0,   150.0,   150.0,   150.0,   150.0,   150.0,   150.0  /)         
                                                                      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: humcste_ref4m  =  &       !! Root profile description for the different 
  & (/ 5.0,   0.4,   0.4,   1.0,   0.8,   0.8,   1.0,  &               !! vegetations types. @tex $(m^{-1})$ @endtex
  &    1.0,   0.8,   4.0,   1.0,   4.0,   1.0,   4.0  /)                      !! These are the factor in the exponential which gets       
                                                                       !! the root density as a function of depth
                                                                       !! Values for zmaxh = 4.0  
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: humcste_ref2m  =  &       !! Root profile description for the different
  & (/ 5.0,   0.8,   0.8,   1.0,   0.8,   0.8,   1.0,  &               !! vegetations types.  @tex $(m^{-1})$ @endtex
  &    1.0,   0.8,   4.0,   4.0,   4.0,   4.0,   4.0  /)                      !! These are the factor in the exponential which gets       
                                                                       !! the root density as a function of depth
                                                                       !! Values for zmaxh = 2.0

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: throughfall_by_mtc  =  &  !! Fraction of rain intercepted by the canopy
  & (/ 30.0,   30.0,   30.0,   30.0,   30.0,   30.0,   30.0,  &        !! (0-100, unitless)
  &    30.0,   30.0,   30.0,   30.0,   30.0,   30.0,   30.0  /)


  !
  ! ALBEDO (sechiba)
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: snowa_aged_mtc  =  &    !! Minimum snow albedo value for each vegetation type
  & (/ 0.35,    0.0,    0.0,   0.14,   0.14,   0.14,   0.14,  &      !! after aging (dirty old snow) (unitless)
  &    0.14,   0.14,   0.18,   0.18,   0.18,   0.18,   0.18/)              !! Source : Values are from the Thesis of S. Chalita (1992)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: snowa_dec_mtc  =  &     !! Decay rate of snow albedo value for each vegetation type
  & (/ 0.45,    0.0,    0.0,   0.06,   0.06,   0.11,   0.06,  &      !! as it will be used in condveg_snow (unitless)
  &    0.11,   0.11,   0.52,   0.52,   0.52,   0.52,   0.52/)              !! Source : Values are from the Thesis of S. Chalita (1992)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alb_leaf_vis_mtc  =  &  !! leaf albedo of vegetation type, visible albedo 
  & (/ 0.00,   0.04,   0.06,   0.06,   0.06,   0.06,   0.06,  &      !! (unitless)
  &    0.06,   0.06,   0.10,   0.10,   0.10,   0.10,   0.10  /) 
!  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alb_leaf_vis_mtc  =  &  !! optimized set of parameter values, to be switched to after validation
!  & (/ 0.00,   0.037,  0.047,  0.048,  0.06,   0.06,   0.06,  &      !! (values are different only for PFTs 2,3,4)
!  &    0.06,   0.06,   0.10,   0.10,   0.10,   0.10  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alb_leaf_nir_mtc  =  &  !! leaf albedo of vegetation type, near infrared albedo
  & (/ 0.00,   0.20,   0.22,   0.22,   0.22,   0.22,   0.22,  &      !! (unitless)
  &    0.22,   0.22,   0.30,   0.30,   0.30,   0.30,   0.30  /)
!  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alb_leaf_nir_mtc  =  &  !! optimized set of parameter values, to be switched to after validation
!  & (/ 0.00,   0.218,   0.198,   0.208,  0.197,   0.232,   0.2,  &   !! (for PFTs 2-13)
!  &    0.217,  0.244,   0.246,   0.28,   0.291,   0.245  /)


  !
  ! SOIL - VEGETATION
  !
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: pref_soil_veg_mtc  =  &       !! The soil tile number for each vegetation
  & (/ 1,   2,   2,   2,   2,   2,   2,  &                                    
  &    2,   2,   3,   3,   4,   5,   6  /)                                         
!  &    2,   2,   3,   3,   3,   3,   3  /)      
!  LOGICAL, PARAMETER, DIMENSION(nvmc) :: is_crop_soil = &                !! whether it is a crop soil tile

  !
  ! VEGETATION - AGE CLASSES
  !
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: agec_group_mtc  =  &       !! The age class group that each PFT belongs to.
       (/ 1,   2,   3,   4,   5,   6,   7,  &                                    
       8,   9,   10,   11,   12,   13,  14  /)                                         



  !
  ! PHOTOSYNTHESIS
  !
  !-
  ! 1 .CO2
  !-
  LOGICAL, PARAMETER, DIMENSION(nvmc) :: is_c4_mtc  =  &                            !! flag for C4 vegetation types (true/false)
  & (/ .FALSE.,  .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,  &
  &    .FALSE.,  .FALSE.,   .FALSE.,   .TRUE.,    .FALSE.,   .TRUE.,   .FALSE.  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: vcmax_fix_mtc  =  &     !! values used for vcmax when STOMATE is not
  & (/  0.0,   40.0,   50.0,   30.0,   35.0,   40.0,   30.0,  &      !! activated @tex $(\mu mol.m^{-2}.s^{-1})$ @endtex
  &    40.0,   35.0,   60.0,   60.0,   70.0,   70.0,   70.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: downregulation_co2_coeff_mtc  =  &  !! coefficient for CO2 downregulation 
  & (/  0.0,   0.38,   0.38,   0.28,   0.28,   0.28,   0.22,  &
  &     0.22,  0.22,   0.26,   0.26,   0.26,   0.26,   0.26 /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: E_KmC_mtc  = &            !! Energy of activation for KmC (J mol-1)
  & (/undef,  79430.,  79430.,  79430.,  79430.,  79430.,  79430.,  &  !! See Medlyn et al. (2002) 
  &  79430.,  79430.,  79430.,  79430.,  79430.,  79430.,  79430.  /)  !! from Bernacchi al. (2001)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: E_KmO_mtc  = &            !! Energy of activation for KmO (J mol-1)
  & (/undef,  36380.,  36380.,  36380.,  36380.,  36380.,  36380.,  &  !! See Medlyn et al. (2002) 
  &  36380.,  36380.,  36380.,  36380.,  36380.,  36380.,  36380.  /)           !! from Bernacchi al. (2001)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: E_gamma_star_mtc  = &     !! Energy of activation for gamma_star (J mol-1)
  & (/undef,  37830.,  37830.,  37830.,  37830.,  37830.,  37830.,  &  !! See Medlyn et al. (2002) from Bernacchi al. (2001) 
  &  37830.,  37830.,  37830.,  37830.,  37830.,  37830.,  37830.  /)           !! for C3 plants - We use the same values for C4 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: E_Vcmax_mtc  = &          !! Energy of activation for Vcmax (J mol-1)
  & (/undef,  71513.,  71513.,  71513.,  71513.,  71513.,  71513.,  &  !! See Table 2 of Yin et al. (2009) for C4 plants
  &  71513.,  71513.,  71513.,  67300.,  71513.,  67300.,  71513.  /)           !! and Kattge & Knorr (2007) for C3 plants (table 3)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: E_Jmax_mtc  = &            !! Energy of activation for Jmax (J mol-1)
  & (/undef,  49884.,  49884.,  49884.,  49884.,  49884.,  49884.,  &   !! See Table 2 of Yin et al. (2009) for C4 plants
  &  49884.,  49884.,  49884.,  77900.,  49884.,  77900.,  49884.  /)            !! and Kattge & Knorr (2007) for C3 plants (table 3)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: aSV_mtc     = &            !! a coefficient of the linear regression (a+bT) defining the Entropy term for Vcmax (J K-1 mol-1)
  & (/undef,  668.39,  668.39,  668.39,  668.39,  668.39,  668.39,  &   !! See Table 3 of Kattge & Knorr (2007)
  &  668.39,  668.39,  668.39,  641.64,  668.39,  641.64,  668.39  /)            !! For C4 plants, we assume that there is no
                                                                        !! acclimation and that at for a temperature of 25°C, aSV is the same for both C4 and C3 plants (no strong jusitification - need further parametrization)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: bSV_mtc     = &            !! b coefficient of the linear regression (a+bT) defining the Entropy term for Vcmax (J K-1 mol-1 °C-1)
  & (/undef,   -1.07,   -1.07,   -1.07,   -1.07,   -1.07,   -1.07,  &   !! See Table 3 of Kattge & Knorr (2007)
  &   -1.07,   -1.07,   -1.07,      0.,   -1.07,      0.,   -1.07  /)            !! We assume No acclimation term for C4 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tphoto_min_mtc  =  &  !! minimum photosynthesis temperature (deg C) 
  & (/  undef,   -4.0,    -4.0,   -4.0,   -4.0,   -4.0,   -4.0,  & 
  &      -4.0,   -4.0,    -4.0,   -4.0,   -4.0,   -4.0,   -4.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tphoto_max_mtc  =  &  !! maximum photosynthesis temperature (deg C) 
  & (/  undef,   55.0,    55.0,   55.0,   55.0,   55.0,   55.0,  & 
  &      55.0,   55.0,    55.0,   55.0,   55.0,   55.0,   55.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: aSJ_mtc     = &            !! a coefficient of the linear regression (a+bT) defining the Entropy term for Jmax (J K-1 mol-1)
  & (/undef,  659.70,  659.70,  659.70,  659.70,  659.70,  659.70,  &   !! See Table 3 of Kattge & Knorr (2007)
  &  659.70,  659.70,  659.70,    630.,  659.70,    630.,  659.70  /)            !! and Table 2 of Yin et al. (2009) for C4 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: bSJ_mtc     = &            !! b coefficient of the linear regression (a+bT) defining the Entropy term for Jmax (J K-1 mol-1 °C-1)
  & (/undef,   -0.75,   -0.75,   -0.75,   -0.75,   -0.75,   -0.75,  &   !! See Table 3 of Kattge & Knorr (2007)
  &   -0.75,   -0.75,   -0.75,      0.,   -0.75,      0.,   -0.75  /)            !! We assume no acclimation term for C4 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: D_Vcmax_mtc  = &           !! Energy of deactivation for Vcmax (J mol-1)
  & (/undef, 200000., 200000., 200000., 200000., 200000., 200000.,  &   !! Medlyn et al. (2002) also uses 200000. for C3 plants (same value than D_Jmax)
  & 200000., 200000., 200000., 192000., 200000., 192000., 200000.  /)            !! 'Consequently', we use the value of D_Jmax for C4 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: D_Jmax_mtc  = &            !! Energy of deactivation for Jmax (J mol-1)
  & (/undef, 200000., 200000., 200000., 200000., 200000., 200000.,  &   !! See Table 2 of Yin et al. (2009)
  & 200000., 200000., 200000., 192000., 200000., 192000., 200000.  /)            !! Medlyn et al. (2002) also uses 200000. for C3 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: E_Rd_mtc  = &              !! Energy of activation for Rd (J mol-1)
  & (/undef,  46390.,  46390.,  46390.,  46390.,  46390.,  46390.,  &   !! See Table 2 of Yin et al. (2009)
  &  46390.,  46390.,  46390.,  46390.,  46390.,  46390.,  46390.  /)           

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: Vcmax25_mtc  =  &          !! Maximum rate of Rubisco activity-limited carboxylation at 25°C
  & (/ undef,   65.0,    65.0,    45.0,   45.0,   55.0,   45.0,  &      !! @tex $(\mu mol.m^{-2}.s^{-1})$ @endtex
  &     45.0,   35.0,    70.0,    70.0,   70.0,   70.0,   63.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: arJV_mtc    = &            !! a coefficient of the linear regression (a+bT) defining the Jmax25/Vcmax25 ratio (mu mol e- (mu mol CO2)-1)
  & (/undef,    2.59,    2.59,    2.59,    2.59,    2.59,    2.59,  &   !! See Table 3 of Kattge & Knorr (2007)
  &    2.59,    2.59,    2.59,   1.715,    2.59,   1.715,   2.34  /)            !! For C4 plants, we assume that there is no
                                                                        !! acclimation and that for a temperature of 25°C, aSV is the same for both C4 and C3 plants (no strong jusitification - need further parametrization)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: brJV_mtc    = &            !! b coefficient of the linear regression (a+bT) defining the Jmax25/Vcmax25 ratio ((mu mol e- (mu mol CO2)-1) (°C)-1)
  & (/undef,  -0.035,  -0.035,  -0.035,  -0.035,  -0.035,  -0.035,  &   !! See Table 3 of Kattge & Knorr (2007)
  &  -0.035,  -0.035,  -0.035,      0.,  -0.035,      0.,  -0.030  /)            !! We assume No acclimation term for C4 plants

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: KmC25_mtc  = &             !! Michaelis–Menten constant of Rubisco for CO2 at 25°C (ubar)
  & (/undef,   404.9,   404.9,   404.9,   404.9,  404.9,   404.9,  &    !! See Table 2 of Yin et al. (2009) for C4
  &   404.9,   404.9,   404.9,    650.,   404.9,   650.,   404.9  /)             !! and Medlyn et al (2002) for C3

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: KmO25_mtc  = &             !! Michaelis–Menten constant of Rubisco for O2 at 25°C (ubar)
  & (/undef, 278400., 278400., 278400., 278400., 278400., 278400.,  &   !! See Table 2 of Yin et al. (2009) for C4 plants and Medlyn et al. (2002) for C3
  & 278400., 278400., 278400., 450000., 278400., 450000., 278400.  /)           

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: gamma_star25_mtc  = &      !! Ci-based CO2 compensation point in the absence of Rd at 25°C (ubar)
  & (/undef,   42.75,   42.75,   42.75,   42.75,   42.75,   42.75,  &   !! See Medlyn et al. (2002) for C3 plants - For C4 plants, we use the same value (probably uncorrect)
  &   42.75,   42.75,   42.75,   42.75,   42.75,   42.75,   42.75  /)    

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: a1_mtc  = &                !! Empirical factor involved in the calculation of fvpd (-)
  & (/undef,    0.85,    0.85,    0.85,    0.85,    0.85,  0.85,  &     !! See Table 2 of Yin et al. (2009)
  &    0.85,    0.85,    0.85,    0.85,    0.85,    0.85,  0.85  /)           

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: b1_mtc  = &                !! Empirical factor involved in the calculation of fvpd (-)
  & (/undef,    0.14,    0.14,    0.14,    0.14,    0.14,  0.14,  &     !! See Table 2 of Yin et al. (2009)
  &    0.14,    0.14,    0.14,    0.20,    0.14,    0.20,  0.14  /)           

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: g0_mtc  = &                !! Residual stomatal conductance when irradiance approaches zero (mol CO2 m−2 s−1 bar−1)
  & (/undef, 0.00625, 0.00625, 0.00625, 0.00625, 0.00625, 0.00625,  &   !! Value from ORCHIDEE - No other reference.
  & 0.00625, 0.00625, 0.00625, 0.01875, 0.00625, 0.01875, 0.00625  /)            !! modofy to account for the conversion for conductance to H2O to CO2 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: h_protons_mtc  = &         !! Number of protons required to produce one ATP (mol mol-1)
  & (/undef,      4.,      4.,      4.,      4.,      4.,    4.,  &     !! See Table 2 of Yin et al. (2009) - h parameter
  &      4.,      4.,      4.,      4.,      4.,      4.,    4.  /)           

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: fpsir_mtc = &              !! Fraction of PSII e− transport rate 
  & (/undef,   undef,   undef,   undef,   undef,  undef,  undef,  &     !! partitioned to the C4 cycle (-)
  &   undef,   undef,   undef,     0.4,   undef,    0.4,  undef  /)             !! See Table 2 of Yin et al. (2009) - x parameter        
 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: fQ_mtc = &                 !! Fraction of electrons at reduced plastoquinone 
  & (/undef,   undef,   undef,   undef,   undef,  undef,  undef,  &     !! that follow the Q-cycle (-) - Values for C3 platns are not used
  &   undef,   undef,   undef,      1.,   undef,     1.,  undef  /)             !! See Table 2 of Yin et al. (2009)          

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: fpseudo_mtc = &            !! Fraction of electrons at PSI that follow 
  & (/undef,   undef,   undef,   undef,   undef,  undef,  undef,  &     !! pseudocyclic transport (-) - Values for C3 platns are not used
  &   undef,   undef,   undef,     0.1,   undef,    0.1,  undef  /)             !! See Table 2 of Yin et al. (2009)    

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: kp_mtc = &                 !! Initial carboxylation efficiency of the PEP carboxylase (mol m−2 s−1 bar−1) 
  & (/undef,   undef,   undef,   undef,   undef,  undef,  undef,  &     !! See Table 2 of Yin et al. (2009)
  &   undef,   undef,   undef,     0.7,   undef,    0.7,  undef  /)                 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alpha_mtc = &              !! Fraction of PSII activity in the bundle sheath (-)
  & (/undef,   undef,   undef,   undef,   undef,  undef,  undef,  &     !! See legend of Figure 6 of Yin et al. (2009)
  &   undef,   undef,   undef,     0.1,   undef,    0.1,  undef  /)                 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: gbs_mtc = &                !! Bundle-sheath conductance (mol m−2 s−1 bar−1)
  & (/undef,   undef,   undef,   undef,   undef,  undef,  undef,  &     !! See legend of Figure 6 of Yin et al. (2009)
  &   undef,   undef,   undef,   0.003,   undef,  0.003,  undef  /)    

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: theta_mtc = &              !! Convexity factor for response of J to irradiance (-)
  & (/undef,     0.7,     0.7,     0.7,     0.7,    0.7,    0.7,  &     !! See Table 2 of Yin et al. (2009) 
  &     0.7,     0.7,     0.7,     0.7,     0.7,    0.7,    0.7  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alpha_LL_mtc = &           !! Conversion efficiency of absorbed light into J at strictly limiting light (mol e− (mol photon)−1)
  & (/undef,     0.3,     0.3,     0.3,     0.3,    0.3,    0.3,  &     !! See comment from Yin et al. (2009) after eq. 4
  &     0.3,     0.3,     0.3,     0.3,     0.3,    0.3,    0.3  /)             !! alpha value from Medlyn et al. (2002)   
                                                                        !! 0.093 mol CO2 fixed per mol absorbed photons
                                                                        !! times 4 mol e- per mol CO2 produced
    
  !-
  ! 2 .Stomate
  !-
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: ext_coeff_mtc  =  &     !! extinction coefficient of the Monsi&Saeki
  & (/ 0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,  &             !! relationship (1953) (unitless)
  &    0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5  /)

  !
  ! ALLOCATION (stomate)
  ! 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: R0_mtc = &              !! Default root allocation (0-1, unitless)
  & (/ undef,   0.30,   0.30,   0.30,   0.30,  0.30,    0.30, &
  &     0.30,   0.30,   0.30,   0.30,   0.30,  0.30,    0.30 /)                   

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: S0_mtc = &              !! Default sapwood allocation (0-1, unitless)
  & (/ undef,   0.25,   0.25,   0.30,   0.30,  0.30,    0.30, &
  &     0.30,   0.30,   0.30,   0.30,   0.30,  0.30,    0.30 /)                   

  !
  ! RESPIRATION (stomate)
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: frac_growthresp_mtc  =  &  !! fraction of GPP which is lost as growth respiration
  & (/  undef,   0.28,   0.28,   0.28,   0.28,   0.28,   0.28,  &
  &      0.28,   0.28,   0.28,   0.28,   0.28,   0.28,   0.28  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: maint_resp_slope_c_mtc  =  &  !! slope of maintenance respiration coefficient (1/K),
  & (/  undef,   0.20,   0.20,   0.16,   0.16,   0.16,   0.16,  &          !! constant c of aT^2+bT+c, tabulated
  &      0.16,   0.16,   0.16,   0.12,   0.16,   0.12,   0.16  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: maint_resp_slope_b_mtc  =  &  !! slope of maintenance respiration coefficient (1/K),
  & (/  undef,   0.0,        0.0,   0.0,        0.0,   0.0,   0.0,  &      !! constant b of aT^2+bT+c, tabulated
  &       0.0,   0.0,   -0.00133,   0.0,   -0.00133,   0.0,   -0.00133  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: maint_resp_slope_a_mtc  =  &  !! slope of maintenance respiration coefficient (1/K),
  & (/  undef,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &                !! constant a of aT^2+bT+c, tabulated
  &       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_leaf_mtc  =   &                  !! maintenance respiration coefficient
  & (/   undef,   2.35E-3,   2.62E-3,   1.01E-3,   2.35E-3,   2.62E-3,   1.01E-3,  &  !! at 0 deg C,for leaves, tabulated, 
  &    2.62E-3,   2.05E-3,   2.62E-3,   2.62E-3,   2.62E-3,   2.62E-3,   2.62E-3  /)  !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_sapabove_mtc =  &                !! maintenance respiration coefficient 
  & (/   undef,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,  &  !! at 0 deg C, for sapwood above,
  &    1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4  /)  !! tabulated, @tex $(gC.gC^{-1}.day^{-1})$ @endtex

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_sapbelow_mtc  =  &               !! maintenance respiration coefficient
  & (/   undef,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,  &  !! at 0 deg C, for sapwood below, 
  &    1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4  /)             !! tabulated, @tex $(gC.gC^{-1}.day^{-1})$ @endtex 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_heartabove_mtc  =  &             !! maintenance respiration coefficient
  & (/  undef,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &                           !! at 0 deg C, for heartwood above,
  &       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0  /)                                  !! tabulated, @tex $(gC.gC^{-1}.day^{-1})$ @endtex 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_heartbelow_mtc  =  &             !! maintenance respiration coefficient
  & (/  undef,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &                           !! at 0 deg C, for heartwood below, 
  &       0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0  /)                                  !! tabulated, @tex $(gC.gC^{-1}.day^{-1})$ @endtex 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_root_mtc  =  &                   !! maintenance respiration coefficient
  & (/   undef,   1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3,  &  !! at 0 deg C, for roots, tabulated,
  &    1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3,   1.67E-3  /)             !! @tex $(gC.gC^{-1}.day^{-1})$ @endtex 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_fruit_mtc  =  &                  !! maintenance respiration coefficient
  & (/   undef,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,  &  !! at 0 deg C, for fruits, tabulated,
  &    1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4  /)             !!  @tex $(gC.gC^{-1}.day^{-1})$ @endtex

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: cm_zero_carbres_mtc  =  &                !! maintenance respiration coefficient
  & (/   undef,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,  &  !! at 0 deg C, for carbohydrate reserve,
  &    1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4,   1.19E-4  /)             !! tabulated, @tex $(gC.gC^{-1}.day^{-1})$ @endtex


  !
  ! FIRE (stomate)
  !
  REAL(r_std),PARAMETER, DIMENSION(nvmc) :: flam_mtc  =  &         !! flamability: critical fraction of water 
  & (/  undef,   0.15,   0.25,   0.25,   0.25,   0.25,   0.25,  &  !! holding capacity (0-1, unitless)
  &      0.25,   0.25,   0.25,   0.25,   0.35,   0.35,   0.35  /)

  REAL(r_std),PARAMETER, DIMENSION(nvmc) :: resist_mtc  =  &       !! fire resistance (0-1, unitless)
  & (/ undef,   0.95,   0.90,   0.90,   0.90,   0.90,   0.90,  &
  &    0.90,    0.90,    0.0,    0.0,    0.0,    0.0,   0.0  /) 

!spitfire
  ! dens_fuel : fuel bulk density
  REAL(r_std), SAVE, DIMENSION(nvmc)                     ::  dens_fuel_mtc = (/0.0,   25.,      25.,     25.,     10.,      22.,    25.,  &   !!kgm^{-3}
                                                                       22.,      22.,     2.,      2.,      2.,     2.,     2.   /)
  ! f_SH : scorch height parameter for crown fire
  REAL(r_std), SAVE, DIMENSION(nvmc)                     :: f_sh_mtc = (/ 0.0, 0.1487,    0.061,     0.1,   0.371,    0.094,   0.11,  &
                                                                  0.094,    0.094,      0.,      0.,       0.,     0.,    0.   /)
  ! crown_length : crown length
  REAL(r_std), SAVE, DIMENSION(nvmc)                     :: crown_length_mtc = (/ 0.0, 0.3334,     0.10,  0.3334,  0.3334,   0.3334, 0.6667,  &
                                                                           0.3334,   0.3334,      0.,      0.,       0.,      0.,  0.  /)
  ! param1 :  Bark thickness parameter
  REAL(r_std), SAVE, DIMENSION(nvmc)                     :: BTpar1_mtc = (/ 0.0, 0.0301,   0.1085,  0.0670,  0.0451,   0.0347, 0.0292,  &
                                                                     0.0347,   0.0347,      0.,      0.,       0.,     0.,  0.   /)
  ! param2 :  Bark thickness parameter
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: BTpar2_mtc = (/ 0.0, 0.0281,    0.212,  0.5590,  0.1412,   0.1086, 0.2632,  &
                                                                      0.1086,   0.1086,      0.,      0.,       0.,     0.,  0.   /)
  ! r_ck and p_ck parameter for postfire mortality as a result of crown damage
  ! r_ck
  REAL(r_std), SAVE, DIMENSION(nvmc)                     :: r_ck_mtc = (/  0.0, 0.95,     0.05,    0.95,    0.95,     0.95,   0.95,  &
                                                                   0.95,     0.95,      0.,      0.,       0.,     0.,   0.   /)
  ! p_ck
  REAL(r_std), SAVE, DIMENSION(nvmc)                     :: p_ck_mtc = (/    0.0, 3.,       3.,     3.75,      3.,       3.,     3.,  &
                                                                   3.,       3.,       0.,      0.,       0.,     0.,   0.   /)
  ! emissions factors, unit: g/(kg dry mass burned)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: ef_CO2_mtc = (/ 0.0, 1580.,    1664.,    1568.,   1568.,    1568.,  1568.,  &
                                                                      1568.,    1568.,    1568.,   1664.,    1568.,  1664.,  1568.   /)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: ef_CO_mtc = (/  0.0, 103.,      63.,     106.,    106.,     106.,   106.,  &
                                                                     106.,     106.,     106.,     63.,     106.,    63.,  106.   /)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: ef_CH4_mtc = (/  0.0,  6.8,      2.2,      4.8,     4.8,      4.8,    4.8,  &
                                                                      4.8,      4.8,      4.8,     2.2,      4.8,    2.2, 4.8   /)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: ef_VOC_mtc = (/   0.0, 8.1,      3.4,      5.7,     5.7,      5.7,    5.7,  &
                                                                       5.7,      5.7,      5.7,     3.4,      5.7,    3.4,  5.7   /)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: ef_TPM_mtc = (/   0.0, 8.5,      8.5,     17.6,    17.6,     17.6,   17.6,  &
                                                                      17.6,     17.6,     17.6,     8.5,     17.6,    8.5,   17.6   /)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: ef_NOx_mtc = (/ 0.0, 1.999,     2.54,     3.24,    3.24,     3.24,   3.24,  &
                                                                       3.24,     3.24,     3.24,    2.54,     3.24,   2.54, 3.24   /)
  ! me : flammability threshold
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: me_mtc = (/    1.0, 0.2,     0.3,      0.3,     0.3,      0.3,   0.35,  &
                                                                   0.35,    0.35,      0.2,     0.2,      0.2,    0.2,  0.2   /)

!  ! Maximum combustion fraction for 100hr and 1000hr fuel for different PFTs.
!  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: fire_max_cf_mtc = (/    0.0, 0.48,     0.48,      0.45,     0.45,      0.45,   0.41,  &
!                                                                   0.41,    0.41,      0.85,     0.85,      0.35,    0.35,  0.35   /)
  !! The 100hr and 1000hr maximum CCs are from Table 2 of Leeuwen et al. (2014),
  !! with 100hr corresponding to woody debris of 2.55-7.6cm and 1000hr corresponding
  !! to woody debris of 7.2-20.5cm. The CCs for tropical tress are taken from
  !! Table 2 (b); The CCs for temperate trees are taken from Table 2 (c). As the
  !! The equivalent CCs for woody debris were not reported for boreal forests in 
  !! Table 2 (d), we tentatively use those for temperate forests. The CCs for grassland
  !! are taken from Table 3 by averaging the CCs for savanna and Grassland savanna 
  !! as we don't make distinction between savanna and Grassland savanna in our model
  !! yet. The CCs for crops are set tentatively as zero to indicate the fact that
  !! cropland fires are not simulated in the model.
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: fire_max_cf_100hr_mtc = (/    0.0, 0.65,     0.65,      0.73,     0.73,      0.73,   0.73,  &
                                                                     0.73,    0.73,      0.76,     0.76,      0.,    0.,  0.  /)
  REAL(r_std) , SAVE, DIMENSION(nvmc)                     :: fire_max_cf_1000hr_mtc = (/    0.0, 0.41,     0.41,      0.38,     0.38,      0.38,   0.38,  &
                                                                   0.38,    0.38,      0.76,     0.76,      0.,    0., 0.  /)


!endspit

  !
  ! FLUX - LUC
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: coeff_lcchange_1_mtc  =  &   !! Coeff of biomass export for the year
  & (/  undef,   0.897,   0.897,   0.597,   0.597,   0.597,   0.597,  &   !! (0-1, unitless)
  &     0.597,   0.597,   0.597,   0.597,   0.597,   0.597,   0.597  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: coeff_lcchange_10_mtc  =  &  !! Coeff of biomass export for the decade 
  & (/  undef,   0.103,   0.103,   0.299,   0.299,   0.299,   0.299,  &   !! (0-1, unitless)
  &     0.299,   0.299,   0.299,   0.403,   0.299,   0.403,   0.299  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: coeff_lcchange_100_mtc  =  & !! Coeff of biomass export for the century
  & (/  undef,     0.0,     0.0,   0.104,   0.104,   0.104,   0.104,  &   !! (0-1, unitless)
  &     0.104,   0.104,   0.104,     0.0,   0.104,     0.0,   0.104  /)


  !
  ! PHENOLOGY
  !
  !-
  ! 1. Stomate
  !-
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: lai_max_to_happy_mtc  =  &  !! threshold of LAI below which plant uses carbohydrate reserves
  & (/  undef,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5,  &
  &       0.5,   0.5,   0.5,   0.5,   0.5,   0.5,   0.5  /)

  REAL(r_std), PARAMETER, DIMENSION (nvmc) :: lai_max_mtc  =  &          !! maximum LAI, PFT-specific 
  & (/ undef,   7.0,   7.0,   5.0,   5.0,   5.0,   4.5,  &               !! @tex $(m^2.m^{-2})$ @endtex
  &      4.5,   3.0,   2.5,   2.5,   5.0,   5.0,   5.0  /)

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: pheno_type_mtc  =  &     !! type of phenology (0-4, unitless)
  & (/  0,   1,   3,   1,   1,   2,   1,  &                              !! 0=bare ground 1=evergreen,  2=summergreen, 
  &     2,   2,   4,   4,   2,   3,   2  /)                                   !! 3=raingreen,  4=perennial
  !-
  ! 2. Leaf Onset
  !-
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: pheno_gdd_crit_c_mtc  =  &    !! critical gdd, tabulated (C),
  & (/  undef,   undef,   undef,   undef,   undef,   undef,   undef,  &    !! constant c of aT^2+bT+c
  &     undef,   undef,   320.0,   400.0,   320.0,   700.0,   320.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: pheno_gdd_crit_b_mtc  =  &    !! critical gdd, tabulated (C),
  & (/  undef,   undef,   undef,   undef,   undef,   undef,   undef,  &    !! constant b of aT^2+bT+c
  &     undef,   undef,    6.25,     0.0,    6.25,     0.0,  6.25  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: pheno_gdd_crit_a_mtc  =  &    !! critical gdd, tabulated (C),
  & (/  undef,   undef,     undef,   undef,   undef,   undef,   undef,  &  !! constant a of aT^2+bT+c
  &     undef,   undef,   0.03125,     0.0,  0.0315,   0.0,  0.0315  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: pheno_moigdd_t_crit_mtc  = &  !! temperature threshold for C4 grass(C)
  & (/  undef,   undef,     undef,   undef,   undef,   undef,   undef,  &  
  &     undef,   undef,     undef,    22.0,   undef,   undef,   undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: ngd_crit_mtc  =  &            !! critical ngd, tabulated. 
  & (/  undef,   undef,   undef,   undef,   undef,   undef,   undef,  &    !! Threshold -5 degrees (days)
  &     undef,    17.0,   undef,   undef,   undef,   undef,   undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: ncdgdd_temp_mtc  =  &         !! critical temperature for the ncd vs. gdd 
  & (/  undef,   undef,   undef,   undef,   undef,     5.0,   undef,  &    !! function in phenology (C)
  &       0.0,   undef,   undef,   undef,   undef,   undef,   undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: hum_frac_mtc  =  &            !! critical humidity (relative to min/max) 
  & (/  undef,   undef,   0.5,   undef,   undef,   undef,   undef, &       !! for phenology (unitless)
  &     undef,   undef,   0.5,     0.5,     0.5,     0.5,   0.5  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: hum_min_time_mtc  =  &        !! minimum time elapsed since
  & (/  undef,   undef,   50.0,   undef,   undef,   undef,   undef,  &     !! moisture minimum (days)
  &     undef,   undef,   35.0,    35.0,    75.0,    75.0,   75.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tau_sap_mtc  =  &             !! time (days)  
  & (/  undef,   730.0,   730.0,   730.0,   730.0,   730.0,   730.0,  &
  &     730.0,   730.0,   undef,   undef,   undef,   undef,   undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tau_leafinit_mtc  =  &  !! time to attain the initial foliage using the carbohydrate reserve
  & (/  undef,   10.,   10.,   10.,   10.,   10.,   10.,  &
  &       10.,   10.,   10.,   10.,   10.,   10.,   10.  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tau_fruit_mtc  =  &           !! fruit lifetime (days)
  & (/  undef,  90.0,    90.0,    90.0,    90.0,   90.0,   90.0,  &
  &      90.0,  90.0,   undef,   undef,   undef,   undef,  undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: ecureuil_mtc  =  &            !! fraction of primary leaf and root allocation
  & (/  undef,   0.0,   1.0,   0.0,   0.0,   1.0,   0.0,  &                !! put into reserve (0-1, unitless)
  &       1.0,   1.0,   1.0,   1.0,   1.0,   1.0,   1.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alloc_min_mtc  =  &           !! NEW - allocation above/below = f(age) 
  & (/  undef,   0.2,     0.2,     0.2,     0.2,    0.2,   0.2,  &         !! - 30/01/04 NV/JO/PF
  &       0.2,   0.2,   undef,   undef,   undef,   undef,  undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: alloc_max_mtc  =  &           !! NEW - allocation above/below = f(age) 
  & (/  undef,   0.8,     0.8,     0.8,     0.8,    0.8,   0.8,  &         !! - 30/01/04 NV/JO/PF
  &       0.8,   0.8,   undef,   undef,   undef,   undef,  undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: demi_alloc_mtc  =  &          !! NEW - allocation above/below = f(age) 
  & (/  undef,   5.0,     5.0,     5.0,     5.0,    5.0,   5.0,  &         !! - 30/01/04 NV/JO/PF
  &       5.0,   5.0,   undef,   undef,   undef,   undef,  undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: leaflife_mtc  =  &            !! leaf longevity, tabulated (??units??)
  & (/  undef,   0.5,   2.0,   0.33,   1.0,   2.0,   0.33,  &
  &       2.0,   2.0,   2.0,   2.0,    2.0,   2.0,   2.0  /)
  !-
  ! 3. Senescence
  !-
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: leaffall_mtc  =  &             !! length of death of leaves, tabulated (days)
  & (/  undef,   undef,   10.0,   undef,   undef,   10.0,   undef,  &
  &      10.0,    10.0,   10.0,    10.0,    10.0,   10.0,   10.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: leafagecrit_mtc  =  &          !! critical leaf age, tabulated (days)
  & (/  undef,   730.0,   180.0,   910.0,   730.0,   180.0,   910.0,  &
  &     180.0,   180.0,   120.0,   120.0,    90.0,    90.0,   90.0  /)

  CHARACTER(LEN=6), PARAMETER, DIMENSION(nvmc) :: senescence_type_mtc  =  & !! type of senescence, tabulated (unitless)
  & (/  'none  ',  'none  ',   'dry   ',  'none  ',  'none  ',  &
  &     'cold  ',  'none  ',   'cold  ',  'cold  ',  'mixed ',  &
  &     'mixed ',  'mixed ',   'mixed ',  'mixed '            /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: senescence_hum_mtc  =  &       !! critical relative moisture availability
  & (/  undef,   undef,   0.3,   undef,   undef,   undef,   undef,  &       !! for senescence (0-1, unitless)
  &     undef,   undef,   0.2,     0.2,     0.3,     0.2,   0.3  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: nosenescence_hum_mtc  =  &     !! relative moisture availability above which 
  & (/  undef,   undef,   0.8,   undef,   undef,   undef,   undef,  &       !! there is no humidity-related senescence
  &     undef,   undef,   0.3,     0.3,     0.3,     0.3,   0.3  /)                !! (0-1, unitless)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: max_turnover_time_mtc  =  &    !! maximum turnover time for grasses (days)
  & (/  undef,   undef,   undef,   undef,   undef,   undef,   undef,  &
  &     undef,   undef,    80.0,    80.0,    80.0,    80.0,   80.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: min_turnover_time_mtc  =  &    !! minimum turnover time for grasses (days)
  & (/  undef,   undef,   undef,   undef,   undef,   undef,   undef,  &
  &     undef,   undef,    10.0,    10.0,    10.0,    10.0,   10.0  /)
 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: min_leaf_age_for_senescence_mtc  =  &  !! minimum leaf age to allow 
  & (/  undef,   undef,   90.0,   undef,   undef,   90.0,   undef,  &               !! senescence g (days)
  &      60.0,    60.0,   30.0,    30.0,    30.0,   30.0,   30.0  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: senescence_temp_c_mtc  =  &    !! critical temperature for senescence (C)
  & (/  undef,   undef,    undef,   undef,   undef,   12.0,   undef,  &     !! constant c of aT^2+bT+c, tabulated
  &       7.0,     2.0,   -1.375,     5.0,    5.0,    10.0,   5.0  /)              !! (unitless)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: senescence_temp_b_mtc  =  &    !! critical temperature for senescence (C), 
  & (/  undef,   undef,   undef,   undef,   undef,   0.0,   undef,  &       !! constant b of aT^2+bT+c, tabulated
  &       0.0,     0.0,     0.1,     0.0,     0.0,   0.0,   0.0  /)                !! (unitless)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: senescence_temp_a_mtc  =  &    !! critical temperature for senescence (C), 
  & (/  undef,   undef,     undef,   undef,   undef,   0.0,   undef,  &     !! constant a of aT^2+bT+c, tabulated
  &       0.0,     0.0,   0.00375,     0.0,     0.0,   0.0,   0.0  /)              !! (unitless)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: gdd_senescence_mtc  =  &       !! minimum gdd to allow senescence of crops (days)
  & (/  undef,   undef,    undef,   undef,     undef,    undef,    undef,  &
  &     undef,   undef,    undef,   undef,      950.,    4000.,    950.  /)


  !
  ! DGVM
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: residence_time_mtc  =  &    !! residence time of trees (years)
  & (/  undef,   30.0,   30.0,   40.0,   40.0,   40.0,   80.0,  &
  &      80.0,   80.0,    0.0,    0.0,    0.0,    0.0,    0.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tmin_crit_mtc  =  &
  & (/  undef,     0.0,     0.0,   -30.0,   -14.0,   -30.0,   -45.0,  &  !! critical tmin, tabulated (C)
  &     -45.0,   undef,   undef,   undef,   undef,   undef,   undef  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: tcm_crit_mtc  =  &
  & (/  undef,   undef,   undef,     5.0,    15.5,    15.5,   -8.0,  &   !! critical tcm, tabulated (C)
  &      -8.0,    -8.0,   undef,   undef,   undef,   undef,  undef  /)



  !
  ! Biogenic Volatile Organic Compounds
  !
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_isoprene_mtc = &     !! Isoprene emission factor 
  & (/  0.,    24.,   24.,    8.,   16.,   45.,   8.,  &                    !!
  &    18.,    0.5,   12.,   18.,    5.,    5.,   5.  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_monoterpene_mtc = &  !! Monoterpene emission factor
  & (/   0.,   2.0,    2.0,   1.8,    1.4,    1.6,    1.8,  &               !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &    1.4,    1.8,    0.8,   0.8,    0.22,     0.22,  0.22  /)

  REAL(r_std), PARAMETER :: LDF_mono_mtc = 0.6                                  !! monoterpenes fraction dependancy to light
  REAL(r_std), PARAMETER :: LDF_sesq_mtc = 0.5                                  !! sesquiterpenes fraction dependancy to light
  REAL(r_std), PARAMETER :: LDF_meth_mtc = 0.8                                  !! methanol fraction dependancy to light
  REAL(r_std), PARAMETER :: LDF_acet_mtc = 0.2                                  !! acetone fraction dependancy to light

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_apinene_mtc = &      !! Alfa pinene emission factor percentage
  & (/   0.,   0.395,   0.395,   0.354,   0.463,   0.326,   0.354, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.316,   0.662,   0.231,   0.200,   0.277,   0.277,   0.277 /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_bpinene_mtc = &      !! Beta pinene emission factor  percentage      
  & (/   0.,   0.110,   0.110,   0.146,   0.122,   0.087,   0.146, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.063,   0.150,   0.123,   0.080,   0.154,   0.154,   0.154  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_limonene_mtc = &     !! Limonene emission factor percentage
  & (/   0.,   0.092,   0.092,   0.083,   0.122,   0.061,   0.083, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.071,   0.037,   0.146,   0.280,   0.092,   0.092,   0.092  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_myrcene_mtc = &      !! Myrcene emission factor percentage
  & (/   0.,   0.073,   0.073,   0.050,   0.054,   0.028,   0.050, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.019,   0.025,   0.062,   0.057,   0.046,   0.046,    0.046  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_sabinene_mtc = &     !! Sabinene emission factor percentage
  & (/   0.,   0.073,   0.073,   0.050,   0.083,   0.304,   0.050, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.263,   0.030,   0.065,   0.050,   0.062,   0.062,   0.062  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_camphene_mtc = &     !! Camphene emission factor percentage
  & (/   0.,   0.055,   0.055,   0.042,   0.049,   0.004,   0.042, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.005,   0.023,   0.054,   0.053,   0.031,   0.031,   0.031  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_3carene_mtc = &      !! 3-carene emission factor percentage
  & (/   0.,   0.048,   0.048,   0.175,   0.010,   0.024,   0.175, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.013,   0.042,   0.065,   0.057,   0.200,   0.200,   0.200  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_tbocimene_mtc = &    !! T-beta-ocimene emission factor percentage
  & (/   0.,   0.092,   0.092,   0.054,   0.044,   0.113,   0.054, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.105,   0.028,   0.138,   0.120,   0.031,   0.031,   0.031  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_othermonot_mtc = &   !! Other monoterpenes emission factor percentage
  & (/   0.,   0.062,   0.062,   0.046,   0.054,   0.052,   0.046, &        !! ATTENTION: for each PFT they are PERCENTAGE of monoterpene EF
  &   0.144,   0.003,   0.115,   0.103,   0.108,   0.108,   0.108  /)

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_sesquiterp_mtc = &   !! Sesquiterpene emission factor
  & (/   0.,  0.45,   0.45,   0.13,   0.30,   0.36,   0.15, &               !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &    0.30,  0.25,   0.60,   0.60,   0.08,   0.08,   0.08  /)

  REAL(r_std), PARAMETER :: beta_mono_mtc = 0.10                            !! Monoterpenes temperature dependency coefficient 
  REAL(r_std), PARAMETER :: beta_sesq_mtc = 0.17                            !! Sesquiterpenes temperature dependency coefficient 
  REAL(r_std), PARAMETER :: beta_meth_mtc = 0.08                            !! Methanol temperature dependency coefficient 
  REAL(r_std), PARAMETER :: beta_acet_mtc = 0.10                            !! Acetone temperature dependency coefficient 
  REAL(r_std), PARAMETER :: beta_oxyVOC_mtc = 0.13                          !! Other oxygenated BVOC temperature dependency coefficient 


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_ORVOC_mtc = &        !! ORVOC emissions factor 
  &  (/  0.,    1.5,    1.5,    1.5,    1.5,   1.5,    1.5,  &              !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &     1.5,    1.5,    1.5,    1.5,    1.5,   1.5,    1.5  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_OVOC_mtc = &         !! OVOC emissions factor 
  &  (/  0.,    1.5,    1.5,    1.5,    1.5,   1.5,    1.5,  &              !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &     1.5,    1.5,    1.5,    1.5,    1.5,   1.5,    1.5  /)
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_MBO_mtc = &          !! MBO emissions factor
  & (/     0., 2.e-5, 2.e-5,   1.4, 2.e-5, 2.e-5, 0.14,  &                  !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &     2.e-5, 2.e-5, 2.e-5, 2.e-5, 2.e-5, 2.e-5, 2.e-5  /)  
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_methanol_mtc = &     !! Methanol emissions factor 
  & (/  0.,    0.8,   0.8,   1.8,   0.9,   1.9,   1.8,  &                   !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &    1.8,    1.8,   0.7,   0.9,    2.,     2.,   2.  /)  
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_acetone_mtc = &      !! Acetone emissions factor
  & (/  0.,   0.25,   0.25,   0.30,   0.20,   0.33,   0.30,  &              !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &   0.25,   0.25,   0.20,   0.20,   0.08,   0.08,   0.08  /)
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_acetal_mtc = &       !! Acetaldehyde emissions factor 
  & (/  0.,   0.2,    0.2,     0.2,   0.2,   0.25,   0.25,   0.16,   &      !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &   0.16,   0.12,   0.12,   0.035,   0.020,  0.020  /)  
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_formal_mtc = &       !! Formaldehyde emissions factor
  & (/  0.,   0.04,   0.04,  0.08,    0.04,    0.04,  0.04,  &              !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &   0.04,   0.04,  0.025, 0.025,   0.013,   0.013,  0.013  /)  

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_acetic_mtc = &       !! Acetic Acid emissions factor
  & (/   0.,   0.025,   0.025,   0.025,   0.022,   0.08,   0.025,   &      !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &   0.022,   0.013,   0.012,   0.012,   0.008,   0.008,   0.008  /)  

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: em_factor_formic_mtc = &       !! Formic Acid emissions factor
  & (/  0.,  0.015,  0.015,   0.02,    0.02,   0.025,  0.025,  &            !! @tex $(\mu gC.g^{-1}.h^{-1})$ @endtex
  &  0.015,  0.015,  0.010,  0.010,   0.008,   0.008,   0.008  /)  

  REAL(r_std),PARAMETER, DIMENSION(nvmc) :: em_factor_no_wet_mtc = &        !! NOx emissions factor soil emissions and exponential
  & (/  0.,   2.6,   0.06,   0.03,   0.03,   0.03,   0.03,  &               !! dependancy factor for wet soils
  &  0.03,   0.03,   0.36,   0.36,   0.36,   0.36,   0.36  /)                       !! @tex $(ngN.m^{-2}.s^{-1})$ @endtex

  REAL(r_std),PARAMETER, DIMENSION(nvmc) :: em_factor_no_dry_mtc = &        !! NOx emissions factor soil emissions and exponential
  & (/  0.,   8.60,   0.40,   0.22,   0.22,   0.22,   0.22,  &              !! dependancy factor for dry soils
  &   0.22,   0.22,   2.65,   2.65,   2.65,   2.65,   2.65  /)                      !! @tex $(ngN.m^{-2}.s^{-1})$ @endtex 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: Larch_mtc = &                  !! Larcher 1991 SAI/LAI ratio (unitless)
  & (/   0.,   0.015,   0.015,   0.003,   0.005,   0.005,   0.003,  &
  &   0.005,   0.003,   0.005,   0.005,   0.008,   0.008,   0.008  /)  


!
!WETALND CH4 methane
!
!pss+
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: sdepth_v_mtc  = &
  & (/ 0, 129, 129, 129, 129, 129, 129, 129, 129, 79, 79, 162, 162, 162/)

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: rdepth_v_mtc  = &
  & (/ 0, 64, 64, 64, 64, 64, 64, 64, 64, 39, 39, 81, 81, 81 /)

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: tveg_v_mtc  = & 
  & (/ 0, 1, 1, 1, 1, 1, 1, 1, 1, 10, 10, 15, 15, 15 /)
!pss-

!chaoyue+
  LOGICAL,PARAMETER,DIMENSION(nvmc) :: permafrost_veg_exists_mtc = &  !!! set all as true to avoid masking
   & (/ .TRUE., .TRUE., .TRUE., .TRUE., .TRUE.,  .TRUE., .TRUE., &
   &    .TRUE., .TRUE.,  .TRUE.,  .TRUE., .TRUE., .TRUE., .TRUE./)
!chaoyue-


!!!!! crop parameters, xuhui

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: irrig_threshold_mtc  =  &  !! Value for irrig_threshold determine
  & (/ 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &                !! when irrigation started (0-1, vegstress)
  &    0.0,   0.0,   0.0,   1.0,   1.0,   1.0,   1.0  /)                       !! only useful for non-natural pfts

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: irrig_fulfill_mtc  =  &  !! Value for irrig_fulfill determine
  & (/ 0.0,   0.0,   0.0,   0.0,   0.0,   0.0,   0.0,  &                !! how irrigation demand fulfilled (0-1)
  &    0.0,   0.0,   0.0,   0.9,   0.9,   0.9,   0.9  /)                       !! only useful for non-natural pfts


  !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  !- 
  ! 4. STICS
  !    This part is especially for Stics
  !- 
  ! 4.0 flag in stomate
  LOGICAL, PARAMETER, DIMENSION(nvmc) ::  ok_LAIdev_mtc =  &        !! flag for using the LAIdev module for vegetation types (true/false)
  & (/ .FALSE.,  .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,  &
  &    .FALSE.,  .FALSE.,   .FALSE.,   .FALSE.,    .TRUE.,    .TRUE.,   .TRUE.  /)

  ! INTEGER(i_std), PARAMETER :: crop_nbjmax = 300                         !! Maximum days of a crop cycle simulation
  
  ! 4.1 LAIdev module

  CHARACTER(len = 3), PARAMETER, DIMENSION(nvmc) ::  SP_codeplante_mtc =  &                     !! simulated plante
  & (/ 'snu',  'snu',   'snu',   'snu',   'snu',   'snu',   'snu',  &
  &    'snu',  'snu',   'snu',   'snu',    'ble',   'mai',  'ric'  /)
  CHARACTER(len = 3), PARAMETER, DIMENSION(nvmc) :: SP_stade0_mtc = &
  & (/ 'snu',  'snu',   'snu',   'snu',   'snu',   'snu',   'snu',  &
  &    'snu',  'snu',   'snu',   'lev',    'snu',   'snu',  'lev'  /)


  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_iplt0_mtc  =  &    !! sowing date
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      120,     284,   100, 120 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_nbox_mtc  =  &    !! number of agebox for senescence
  & (/  0,     0,     0,      0,        0,       0,       0,  &    
  &       0,        0,        0,      3,     3,   3,  5 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_iwater_mtc  =  &    !! julian day of the beginning of the simulation
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1, 1 /)      

  CHARACTER(len = 7), PARAMETER, DIMENSION(nvmc) ::  SP_codesimul_mtc =  &                     !! simulated plante
  & (/ 'feuille',  'feuille',  'feuille', 'feuille', 'feuille', 'feuille', 'feuille', &
  &    'feuille',   'feuille', 'feuille', 'culture', 'culture', 'culture',  'culture'/)

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codelaitr_mtc  =  &    !! option of calculation of LAI: undef_int. LAI, 2. Soil cover 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,    1,   1,  1 /)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_slamax_mtc  =  &    !! maximum sla  cm2/g dry mass
  & (/  undef,   undef,   undef,   undef,   undef,   undef,   undef,  &       !! 
  &       undef,     undef,     undef,     500.0,     350.0,  250.0, 500.0  /)     !!  YANG SU from STICS, wheat is 350

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_slamin_mtc  =  &    !! minimum sla
  & (/  undef,   undef,     undef,   undef,   undef,   undef,   undef,  &     !! 
  &       undef,     undef,    undef,     180.0,     180.0,   180.0,   180.0  /)    !! YANG SU from STICS, wheat is 180

  ! STICS:: DEVELOPMENT
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codeperenne_mtc  =  &    !! annual crop (1) or perennial crop (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codcueille_mtc  =  &    !! harvest option: cutting (1) or picking (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codegdh_mtc  =  &    !!  daily (1) or hourly (2) calculation of development unit 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codetemp_mtc  =  &    !!  air temperature (1) or crop_temperature (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,    1,  1,   1 /)      
  
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_coderetflo_mtc  =  &    !! slowing effect of water stress before DRP, YES (1) OR NO (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,      undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     1,   1,   1/)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codeinnact_mtc  =  &    !! slowing effect of nitrogen stress on crop, active (1) OR NOt active (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,   1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codeh2oact_mtc  =  &    !! slowing effect of water stress on crop, YES (1) OR NO (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1/)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stressdev_mtc  =  &    !! maximum phasic delay allowed  due to stresses  // SD
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
  !&     undef,        undef,        undef,      0.20,     0.34,        0.20,   0.20 /)      
  &     undef,        undef,        undef,      0.20,     0.20,        0.20,   0.20 /)  ! YANG SU from STICS

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_innlai_mtc  =  &    !! nitrogen limitation for leaf growth  // SD
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
 ! &     undef,        undef,        undef,      1.0,     0.50,        1.0,   1.0 /)    
  &     undef,        undef,        undef,      1.0,     0.50,        1.0,   1.0 /)    ! YANG SU, index of nitrogen stress active on leaf growth
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_innsenes_mtc  =  &    !! nitrogen limitation for leaf senescence  // SD
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
  &     undef,        undef,        undef,      0.35,     1.0,        1.0,   0.35 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codebfroid_mtc  =  &    !! PARAMETER option of calculation of chilling requirements,  1 (no need), 2 (vernalization), 3 (development stage) 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &           !!  code undef_int (vernalization), 3 (for perennial) 
  &       undef_int,        undef_int,        undef_int,      1,    2,   1,   1 /)      


  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codephot_mtc  =  &    !!  option of plant photoperiodism: yes (1), no (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   2,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codedormance_mtc  =  &    !!option of calculation of dormancy and chilling requirement // code 1/2  
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      3,    3,   3,  3/)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codefauche_mtc  =  &    !! option of cut modes for forage crops: yes (1), no (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,    2,   2,  2 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codetempfauche_mtc  =  &    !!option of the reference temperature to compute cutting sum of temperatures : 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    !!  : upvt (1), udevair (2) 
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1/)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codlainet_mtc  =  &    !!calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI 
  & (/  1,     1,     1,      1,        1,       1,       1,  &    
  &       1,        1,        1,      3,    3,   3,   3 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codeindetermin_mtc  =  &    !!option of  simulation of the leaf growth and fruit growth  
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    !!  indeterminate (2) or determinate (1)
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1/)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codeinitprec_mtc  =  &    !! reinitializing initial status in case of chaining simulations : yes (1), no (2)  
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,    1,   1,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_culturean_mtc  =  &    !! crop status:  1 = over 1 calendar year 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    !! other than 1  = on two calendar years (winter crop in northern hemisphere)
  &       undef_int,        undef_int,        undef_int,      1,     2,   1,  1/)      !! // code 0/1

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_jvc_mtc  =  &    !! Number of vernalizing days // day
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
  &       undef,        undef,        undef,      0.0,    38.0,   0.0,  0.0 /)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tfroid_mtc  =  &    !!optimal temperature for vernalisation // degree C 
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
  &       undef,        undef,        undef,      6.5,     6.5,   6.5,  6.5 /)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_ampfroid_mtc  =  &    !! semi thermal amplitude thermique for vernalising effect // degree C
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
  &       undef,        undef,        undef,      10.,     10.,   10., 10. /)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_jvcmini_mtc  =  &    !! Minimum number of vernalising days  // day
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
  &       undef,        undef,        undef,      undef,     7.,   undef,  undef /)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgmin_mtc  =  &    !!  Minimum threshold temperature used in emergence stage // degree C
  & (/  undef,     undef,     undef,      undef,        undef,       undef,       undef,  &    
 ! &       undef,        undef,        undef,      0.,    0.,   8.,  0. /)      
  &       undef,        undef,        undef,      0.,    0.,   10.,  0. /)  ! YANG SU

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stpltger_mtc  =  &            !!Sum of development allowing germination // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    50.,    50.,    35.0,   50.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_profsem_mtc  =  &            !! Sowing depth //cm 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    3.0,    2.0,   3.0,  3.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_propjgermin_mtc  =  &            !! minimal proportion of the duration P_nbjgerlim 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &  !!  when the temperature is higher than the temperature threshold SP_Tdmax  // %
  &       undef,    undef,    undef,    1.0,    1.0,    1.0,  1.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tdmax_mtc  =  &       !! Maximum threshold temperature for development // degree C 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    30.0,    28.0,    32.0,  25.9  /) ! YANG SU from STICS wheat

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_nbjgerlim_mtc  =  &    !! Threshold number of day after grain imbibition without germination lack // days 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      50,     50,   50,  50 /)      
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_densitesem_mtc  =  &            !! Sowing density  // plants.m-2
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  !&       undef,    undef,    undef,    66.0,    212.,    10.0,  66.0  /) 
  &       undef,    undef,    undef,    66.0,    275.0,    9.0,  66.0  /) ! YANG SU, wheat is between 150 to 400, maize is between 6 to 12

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_vigueurbat_mtc  =  &            !! indicator of plant vigor allowing to emerge through the crust
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    1.0,    1.0,    1.0,  1.0  /) 

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codepluiepoquet_mtc  =  &    !!option to replace rainfall by irrigation at poquet depth 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &   !! in the case of poquet sowing // code 1/2 
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1/)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codehypo_mtc  =  &    !! option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,    1,   1, 2 /)      

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_elmax_mtc  =  &            !! Maximum elongation of the coleoptile in darkness condition // cm /
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    6.5,    8.0,    8.0,  6.5  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_belong_mtc  =  &            !! parameter of the curve of coleoptile elongation // degree.days -1
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.006,   0.012,    0.022,  0.006  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_celong_mtc  =  &            !! parameter of the subsoil plantlet elongation curve 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    2.00,    3.2,    2.04,  2.00  /) 

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_nlevlim1_mtc  =  &  !!number of days after germination decreasing the emerged plants if emergence has not occur 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      10,     10,   10,  10 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_nlevlim2_mtc  =  &    !!number of days after germination after which the emerged plants is 0 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
 ! &       undef_int,        undef_int,        undef_int,      50,     50,   50,  50 /)      
  &       undef_int,        undef_int,        undef_int,      50,     50,   50,  50 /)  ! YANG SU 

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codrecolte_mtc  =  &    !! harvest mode : all the plant (1) or just the fruits (2)
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,    1,  1,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_variete_mtc  =  &    !!variety number in the technical file
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,    1,  1,  1 /)      

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codegermin_mtc  =  &    !! option of simulation of a germination phase 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    !! or a delay at the beginning of the crop (undef_int)   or  direct starting (2)
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1 /)      


  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: S_codeulaivernal_mtc  =  &    !! sensitive or not to vernalization 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    !! 
  &       undef_int,        undef_int,        undef_int,      0,     1,   0,  0 /)      


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_swfacmin_mtc  =  &    !! minimum of drought stress
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
!  &       undef,    undef,    undef,    0.3,    0.3,    0.3,   0.3  /) 
  &       undef,    undef,    undef,    0.3,    0.2,    0.2,   0.3  /)  ! YANG SU

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_neffmax_mtc  =  &    !! maximum nitrogen effect
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.65,    0.65,    0.65,  0.65  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_nsatrat_mtc  =  &    !! maximum nitrogen effect
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.91,    0.91,    0.91,   0.91  /) 


  ! STICS:: LAI CALCULATION

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_laiplantule_mtc  =  &    !! Plantlet Leaf index at the plantation // m2 leaf  m-2 soil 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.0,    1.0,    1.0,   0.0  /) 




  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_udlaimax_mtc  =  &    !! ulai from which the rate of leaf growth decreases 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    3.0,    3.0,    3.0,  3.0  /) ! YANG SU from STICS, wheat is 3.0

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_laicomp_mtc  =  &     !! LAI from which starts competition inbetween plants // m2 m-2 /
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
!  &       undef,    undef,    undef,    0.305,    0.304,    0.7,  0.305  /)    ! YANG SU from STICS, wheat is 0.3
  &       undef,    undef,    undef,    0.305,    0.3,    0.7,  0.305  /)    ! YANG SU from STICS, wheat is 0.3

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_adens_mtc  =  &     !!  Interplant competition parameter
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -0.85,    -0.60,    -0.12,  -0.85  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_bdens_mtc  =  &     !! minimal density from which interplant competition starts // plants m-2
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    2.0,    15.0,    5.0,  2.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tcxstop_mtc  =  &     !!threshold temperature beyond which the foliar growth stops // degree C 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    40.0,    100.0,    35.0,  35.7  /) 
  &       undef,    undef,    undef,    40.0,    40.0,    35.0,  35.7  /)  ! YANG SU, 100 is insane

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tcmax_mtc  =  &            !! Maximum temperature of growth
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    30.0,    40.0,    32.0 ,  24.1 /) 
  &       undef,    undef,    undef,    30.0,    40.0,    32.0 ,  24.1 /)  ! YANG SU

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tcmin_mtc  =  &            !!Minimum temperature of growth
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    13.0,    0.0,    6.0,  11.3  /) 


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_dlaimin_mtc  =  &      !!accelerating parameter for the lai growth rate
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.00,    0.00,    0.0012,  0.00  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_pentlaimax_mtc  =  &    !!parameter of the logistic curve of LAI growth
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    5.5,    5.5,    5.5,  5.5 /) ! YANG SU from STICS


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tigefeuil_mtc  =  &     !! stem (structural part)/leaf proportion // SD 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.5,    0.5,    0.25,  0.5  /) 
 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stlaxsen_mtc  =  &      !!Sum of development units between the stages LEV and AMF // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    390.0,    265.0,    600.0,   390.0  /)  ! YANG SU, GDD from ILAX (maximum leaf area index, end of leaf growth) to sen

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stsenlan_mtc  =  &    !! Sum of development units between the stages AMF and LAX // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    89.0,    620.0,    272.0,   89.0  /) 



  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_vlaimax_mtc  =  &            !!  ULAI at inflection point of the function DELTAI=f(ULAI)
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    1.5,    2.2,    2.2,  1.5 /) 
  &       undef,    undef,    undef,    1.5,    2.2,    1.4,  1.5 /)  ! YANG SU from STICS, wheat is 2.2


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_dlaimax_mtc  =  &     !! accelerating parameter for the lai growth rate for maize 0.0016
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    0.0060,    0.00044,    0.0020,  0.0042 /) ! YANG SU from STICS, wheat is 0.00044
  &       undef,    undef,    undef,    0.0060,    0.00060,    0.00172,  0.0042 /) ! YANG SU from STICS, wheat is 0.00044



  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stlevamf_mtc  =  &      !!Sum of development units between the stages LEV and AMF // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
!  &       undef,    undef,    undef,    571.,    185.0,    225.0,  215.0 /)  
  &       undef,    undef,    undef,    571.,    445.0,    370.0,  215.0 /)  ! YANG SU calibration of Grignon site

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stamflax_mtc  =  &    !! Sum of development units between the stages AMF and LAX // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
!  &       undef,    undef,    undef,    409.,    280.0,    450.0,  475.0  /) !
  &       undef,    undef,    undef,    409.,    295.0,    395.0,  475.0  /) ! YANG SU calibration of Grignon site


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stlevdrp_mtc  =  &      !!Sum of development units between the stages LEV and AMF // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    959.0,    690.0,    960.0,   795.0 /) ! 540 for wheat ! YANG SU during between ILEV to IDRP
 ! &       undef,    undef,    undef,    959.0,    690.0,    900.0,   795.0 /)  ! YANG SU from STICS, wheat is between 692 and 837
  &       undef,    undef,    undef,    959.0,    1030.0,    900.0,   795.0 /)  ! YANG SU grignon calibration, increase this can increase yield
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stflodrp_mtc  =  &    !! Sum of development units between the stages AMF and LAX // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    50.0,    0.0,    250.0,   50.0  /) ! YANG SU during between FLO to DRP

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stdrpmat_mtc  =  &      !! wrong! Sum of development units between the stages LEV and AMF // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  !&       undef,    undef,    undef,    442.0,    940.,    650.0,   380.0  /)    ! 750 for wheat
 ! &       undef,    undef,    undef,    442.0,    450.0,    450.0,   380.0  /)    ! YANG SU, for short Growing Season, from STICS, wheat is 700, during between IDRP and IMAT, phyaiological mature
  &       undef,    undef,    undef,    442.0,    650.0,    370.0,   380.0  /)    ! YANG SU calibration of Grignon site


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_stdrpdes_mtc  =  &    !! Sum of development units between the stages AMF and LAX // degree.days
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  !&       undef,    undef,    undef,    350.0,    750.0,    600.0,  350.0  /) ! YANG SU from STICS, for wheat, IDEBDES =  IMAT
  &       undef,    undef,    undef,    350.0,    600.0,    370.0,  350.0  /) !  YANG SU from STICS, wheat is 700, during between IDRP and IDEBEDS, onset of water dynamics in fruits

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_phyllotherme_mtc  =  &    !! shoot growth 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    120.0,    120.0,    70.0,   120.0  /) ! YANG SU  wheat is 120, this is the thermal duration between the apprarition of two successive leaves on the main stem




  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_lai0_mtc  =  &       !!  initial LAI
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.61,    0.0,    0.0,  0.81  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tustressmin_mtc  =  &       !!rice does not take tustress approach
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.7,    0.7,    0.7,   0.7  /) 
 
  ! STICS:: LAI SENESCENCE

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_nbfgellev_mtc  =  &    !!  leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     2,   50,  2 /)   ! YANG SU from STICS, wheat is 2   

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_ratiodurvieI_mtc  =  &      !!  life span of early leaves expressed as a proportion of 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &   !!  the life span of the last leav    es emitted SP_DURVIEF
  &       undef,    undef,    undef,    0.8,    0.8,    0.8,   0.8  /) 
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_durvieF_mtc  =  &       !!  maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase))
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  !&       undef,    undef,    undef,    480.0,    480.0,    580.0,  545.0  /) 
  &       undef,    undef,    undef,    480.0,    200.0,    580.0,  545.0  /)  ! YANG SU from STICS

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_ratiosen_mtc  =  &     !!  fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.28,    0.8,    0.8,  0.28  /) ! YANG SU from STICS

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tdmin_mtc  =  &       !!Minimum threshold temperature for development // degree C
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    13.0,    0.0,    6.0,  10.8  /) 
  


  ! STICS:: f_humerac 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_sensrsec_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.8,    0.5,    0.5,   0.8  /) 

  ! STICS:: gel 
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codgellev_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     1,   2,  2 /)      


  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codgeljul_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     1,   2,   2 /)      
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codgelveg_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     1,   2,  2 /)      



  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tletale_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -25.0,    -25.0,    -5.0,  -25.0  /) 
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tdebgel_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -4.0,    -4.0,    0.0,   -4.0  /) 
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgellev10_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -4.0,    -4.0,    undef,   -4.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgellev90_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -20.0,    -20.0,    undef,  -20.0  /) 


  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgeljuv10_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -10.0,    -10.0,    undef,   -10.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgeljuv90_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -20.0,    -20.0,    undef,   -20.0  /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgelveg10_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -4.5,    -4.5,    undef,   -4.5  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgelveg90_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -10.0,    -10.0,    undef,   -10.0  /) 


  ! STICS:: Photoperiod

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_sensiphot_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    1.0,    0.0,    1.0,  1.0 /) 
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_phosat_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    20.0,    20.0,    undef,  20.0  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_phobase_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    6.3,    6.3,    undef,  6.3  /) 

  ! STICS:: CARBON ALLOCATION

  CHARACTER(len=3), PARAMETER, DIMENSION(nvmc) :: SP_stoprac_mtc = &  !! description of the MTC (unitless)
  & (/ 'GER', &          !  1
  &    'GER', &          !  2
  &    'GER', &          !  3
  &    'GER', &          !  4
  &    'GER', &          !  5
  &    'GER', &          !  6
  &    'GER', &          !  7
  &    'GER', &          !  8
  &    'GER', &          !  9
  &    'GER', &          ! 10
  &    'SEN', &          ! 11
  &    'SEN', &          ! 12 wheat
  &    'LAX', &          ! 13 maize
  &    'SEN'  /)         ! 14 rice
 
  ! initial root depth for planting plant // cm 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_zracplantule_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    3.0,    3.0,    3.0,  3.0  /) 
  &       undef,    undef,    undef,    3.0,    2.0,    3.0,  3.0  /) ! YANG SU 
  ! trophic effect on root partitioning within the soil // code 1/2/3 
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codtrophrac_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1 /)      
  
  !  maximum of root biomass respective to the total biomass (permanent trophic link) // SD /
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_repracpermax_mtc  =  &                      ! not correct
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.24,    0.4,    0.4,   0.24  /) 


  ! minimum of root biomass respective to the total biomass (permanent trophic link) // SD /
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_repracpermin_mtc  =  &                      ! not correct
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.047,    0.2,    0.2,    0.040  /) 
  !  parameter of biomass root partitioning : evolution of the ratio root/total (permanent trophic link)
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_krepracperm_mtc  =  &                       ! not correct
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    1.27,    1.27,     1.27,   1.27  /) 

  ! maximum of root biomass respective to the total biomass (trophic link by thresholds) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_repracseumax_mtc  =  &                      ! not correct
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.24,    0.4,    0.4,   0.24  /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_repracseumin_mtc  =  &                      ! not correct
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.047,    0.2,    0.2,   0.047  /)      
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_krepracseu_mtc  =  &                        ! not correct
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    1.27,    1.27,    1.27,    1.27  /) 
  
  ! option calculation mode of heat time for the root: with crop temperature (1)  or with soil temperature (2)
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codetemprac_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,   1 /)

  ! Activation of the module simulating tiller dynamic: yes (1), no (2) /      
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codedyntalle_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   2,   1 /)

  ! < // PARAMETER // Period to compute NBGRAIN // days // PARPLT      
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_nbjgrain_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      30,     30,   20,   30 /) 

  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_maxgs_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  !&       undef_int,        undef_int,        undef_int,      300,     300,   235,  300 /)  ! YANG SU
  !&       undef_int,        undef_int,        undef_int,      350,     350,   350,  350 /) 
   &       undef_int,        undef_int,        undef_int,      350,     350,   350,  350 /)

  ! activation of frost at anthesis // code 1/2     1, no, 2, yes 
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codgelflo_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     2,   1,  2 /)      
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgelflo10_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -4.5,    -4.5,    undef,  -4.5 /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tgelflo90_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    -6.5,    -6.5,    undef, -6.5  /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_cgrain_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.035,    0.036,    0.05,  0.035  /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_cgrainv0_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.350,    0.0,    0.111,  0.350  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_nbgrmax_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    40000.0,    30000.0,    4000.0,  40000.0  /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_nbgrmin_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    10000.0,    6000.0,    1500.0,  10000.0  /) 

  ! option of activation of the direct effect of the nitrogen plant status upon the fruit/grain number , 1: no; 2, yes 
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codazofruit_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,   1 /)      
  ! option of computing the ratio grain weight/total biomass: proportional to time(1), proportional to sum temperatures (2)
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codeir_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      2,     1,   1,   2 /)      

  ! Rate of increase of the carbon harvest index // g grain g plant -1 day-1 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_vitircarb_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
 ! &       undef,    undef,    undef,    0.015,    0.085,    0.080,  0.015  /) ! YANG SU STICS wheat is 0.011
  &       undef,    undef,    undef,    0.015,    0.053,    0.0668,  0.015  /) ! YANG SU  calibration grignon site
 !&       undef,    undef,    undef,    0.015,    0.014,    0.020,  0.015  /) ! 0.0107 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_irmax_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
!  &       undef,    undef,    undef,    0.65,    0.95,    0.72,   0.65  /) ! YANG SU STICS wheat is 0.55
  &       undef,    undef,    undef,    0.65,    0.859,    0.86,   0.65  /) ! YANG SU calibration grignon site
  !&       undef,    undef,    undef,    0.65,    0.55,    0.53,   0.65  /) 
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_vitircarbT_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.0012,    0.1,    0.00150,  0.002  /)  ! YANG SU STICS no parameter 
  !&       undef,    undef,    undef,    0.0012,    0.0007,    0.00110,  0.002  /) 

  !  option of heat effect on grain filling: yes (1), no (2) // code 1/2 
  INTEGER(i_std), PARAMETER, DIMENSION(nvmc) :: SP_codetremp_mtc  =  &    
  & (/  undef_int,     undef_int,     undef_int,      undef_int,        undef_int,       undef_int,       undef_int,  &    
  &       undef_int,        undef_int,        undef_int,      1,     1,   1,  1 /)      
  
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tminremp_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    14.0,    0.0,    0.0,  12.8  /) 

  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_tmaxremp_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    38.0,    38.0,    42.0,  33.2  /) 
  
  ! Maximum weight of one grain (at 0% water content) // g // PARPLT // 1, it is depedent on variety (wheat: Thesee, maise: Furio---variety)
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_pgrainmaxi_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    0.03,    0.052,      0.33,  0.03  /) 

  !!===============================SPECIFIC FOR DYNAMIC NITROGEN FOR CROP GROWTH ======================================
  LOGICAL, PARAMETER, DIMENSION(nvmc) :: SP_DY_INN_mtc =  &                     !! flag for using the LAIdev module for vegetation types (true/false)
  & (/ .FALSE.,  .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,   .FALSE.,  &
  &    .FALSE.,  .FALSE.,   .FALSE.,   .FALSE.,    .FALSE.,   .FALSE.,  .FALSE.  /)

  ! An average nitrogen fertilization amount for crop // unit is kg N ha-1 year-1
  REAL(r_std), PARAMETER, DIMENSION(nvmc) :: SP_avenfert_mtc  =  &  
  & (/    undef,    undef,    undef,    undef,    undef,    undef,   undef,  &
  &       undef,    undef,    undef,    150.0,   100.0,      100.0,  150.0  /) 

  ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


!!!!! end crop parameters, xuhui

END MODULE constantes_mtc
