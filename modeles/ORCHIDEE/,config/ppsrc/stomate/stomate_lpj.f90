










! ================================================================================================================================
! MODULE       : stomate_lpj
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Main entry point for daily processes in STOMATE and LPJ (phenology, 
!! allocation, npp_calc, kill, turn, light, establish, crown, cover, lcchange)
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S) : None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/stomate_lpj.f90 $
!! $Date: 2016-04-26 13:28:48 +0200 (Tue, 26 Apr 2016) $
!! $Revision: 3386 $
!! \n
!_ ================================================================================================================================

MODULE stomate_lpj

  ! modules used:

  USE ioipsl_para
  USE xios_orchidee
  USE grid
  USE stomate_data
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE lpj_constraints
  USE lpj_pftinout
  USE lpj_kill
  USE lpj_crown
  USE lpj_fire
  USE lpj_spitfire
  USE lpj_gap
  USE lpj_light
  USE lpj_establish
  USE lpj_cover
  USE stomate_prescribe
  USE stomate_phenology
  USE stomate_alloc
  USE stomate_npp
  USE stomate_turnover
  USE stomate_litter
  USE stomate_soilcarbon
  USE stomate_vmax
  USE stomate_lcchange
  USE stomate_glcchange_fh
  USE stomate_glcchange_SinAgeC_fh
!JCADD
  USE Grassland_Management
!ENDJCADD
!  USE Orch_Write_field_p

  !pss:+
!  USE stomate_cste_wetlands
  USE stomate_wet_ch4_pt_ter_0
  USE stomate_wet_ch4_pt_ter_wet1
  USE stomate_wet_ch4_pt_ter_wet2
  USE stomate_wet_ch4_pt_ter_wet3
  USE stomate_wet_ch4_pt_ter_wet4
  !pss:-



  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC StomateLpj,StomateLpj_clear

  LOGICAL, SAVE                         :: firstcall = .TRUE.             !! first call
!$OMP THREADPRIVATE(firstcall)
!JCADD
  ! flag that enable grazing
  LOGICAL, SAVE :: enable_grazing
!ENDJCADD

CONTAINS


!! ================================================================================================================================
!! SUBROUTINE   : StomateLpj_clear
!!
!>\BRIEF        Re-initialisation of variable
!!
!! DESCRIPTION  : This subroutine reinitializes variables. To be used if we want to relaunch 
!! ORCHIDEE but the routine is not used in current version.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE StomateLpj_clear

    CALL prescribe_clear
    CALL phenology_clear
    CALL npp_calc_clear
    CALL turn_clear
    CALL soilcarbon_clear
    CALL constraints_clear
    CALL establish_clear
    CALL fire_clear
    CALL spitfire_clear
    CALL gap_clear
    CALL light_clear
    CALL pftinout_clear
    CALL alloc_clear
    
    CALL grassmanag_clear
    !pss:+
    CALL ch4_wet_flux_density_clear_0
    CALL ch4_wet_flux_density_clear_wet1
    CALL ch4_wet_flux_density_clear_wet2
    CALL ch4_wet_flux_density_clear_wet3
    CALL ch4_wet_flux_density_clear_wet4
    !pss:-

  END SUBROUTINE StomateLpj_clear


!! ================================================================================================================================
!! SUBROUTINE   : StomateLPJ
!!
!>\BRIEF        Main entry point for daily processes in STOMATE and LPJ, structures the call sequence 
!!              to the different processes such as dispersion, establishment, competition and mortality of PFT's.
!! 
!! DESCRIPTION  : This routine is the main entry point to all processes calculated on a 
!! daily time step. Is mainly devoted to call the different STOMATE and LPJ routines 
!! depending of the ok_dgvm (is dynamic veg used) and lpj_constant_mortality (is background mortality used).
!! It also prepares the cumulative 
!! fluxes or pools (e.g TOTAL_M TOTAL_BM_LITTER etc...)
!!
!! This routine makes frequent use of "weekly", "monthly" and "long term" variables. Quotion is used because
!! by default "weekly" denotes 7 days, by default "monthly" denotes 20 days and by default "Long term" denotes
!! 3 years. dtslow refers to 24 hours (1 day).
!!
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S): All variables related to stomate and required for LPJ dynamic vegetation mode.
!!
!! REFERENCE(S) : 
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudré, J. Ogeé, J. Polcher, P. Friedlingstein, P. Ciais, S. Sitch, 
!! and I. C. Prentice. 2005. A dynamic global vegetation model for studies of the coupled atmosphere-biosphere 
!! system. Global Biogeochemical Cycles 19:GB1015, doi:1010.1029/2003GB002199.
!! - Sitch, S., B. Smith, I. C. Prentice, A. Arneth, A. Bondeau, W. Cramer, J. O. Kaplan, S. Levis, W. Lucht, 
!! M. T. Sykes, K. Thonicke, and S. Venevsky. 2003. Evaluation of ecosystem dynamics, plant geography and 
!! terrestrial carbon cycling in the LPJ dynamic global vegetation model. Global Change Biology 9:161-185.
!!
!! FLOWCHART    : Update with existing flowchart from N Viovy (Jan 19, 2012)
!! \n
!_ ================================================================================================================================
 
  SUBROUTINE StomateLpj (npts, dt_days, &
       lalo, neighbours, resolution, contfrac, &
       clay, herbivores, &
       tsurf_daily, tsoil_daily, t2m_daily, t2m_min_daily, &
       !spitfire
       t2m_max_daily, precip_daily, wspeed_daily, lightn, popd, a_nd, &
       read_observed_ba, observed_ba, &
       read_cf_fine,cf_fine,read_cf_coarse,cf_coarse,read_ratio_flag,ratio_flag,read_ratio,ratio,date,&
       !endspit
       litterhum_daily, soilhum_daily, &
       maxmoiavail_lastyear, minmoiavail_lastyear, &
       gdd0_lastyear, precip_lastyear, &
       moiavail_month, moiavail_week, t2m_longterm, t2m_month, t2m_week, &
       tsoil_month, soilhum_month, &
       gdd_m5_dormance, gdd_from_growthinit, gdd_midwinter, ncd_dormance, ngd_minus5, &
       turnover_longterm, gpp_daily, gpp_week, &
       time_hum_min, maxfpc_lastyear, resp_maint_part, &
       PFTpresent, age, fireindex, firelitter, &
       leaf_age, leaf_frac, biomass, ind, adapted, regenerate, &
       senescence, when_growthinit, &
       litterpart, litter, litter_avail, litter_not_avail, litter_avail_frac, &
       dead_leaves, carbon,carbon_surf, lignin_struc, &
       !spitfire
       ni_acc,fire_numday,fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr, &
       lcc,bafrac_deforest_accu,emideforest_litter_accu,emideforest_biomass_accu,&              
       deforest_litter_remain,deforest_biomass_remain,&
       def_fuel_1hr_remain,def_fuel_10hr_remain,&         
       def_fuel_100hr_remain,def_fuel_1000hr_remain,&   
       !endspit
       veget_max, veget_max_new, npp_longterm, lm_lastyearmax, &
       veget_lastlight, everywhere, need_adjacent, RIP_time, &
       lai, rprof,npp_daily, turnover_daily, turnover_time,&
       control_moist, control_temp, soilcarbon_input, &
       co2_to_bm, co2_fire, resp_hetero, resp_maint, resp_growth, &
       height, deadleaf_cover, vcmax, &
       bm_to_litter, &
       prod10,prod100,flux10, flux100, &
       vegetnew_firstday, glccNetLCC, &
       glccSecondShift,glccPrimaryShift, &
       harvest_matrix, bound_spa, glcc_pft, &
       convflux,cflux_prod10,cflux_prod100, harvest_above, carb_mass_total, &
       fpc_max, Tseason, Tseason_length, Tseason_tmp, &
       Tmin_spring_time, begin_leaves, onset_date, &
       MatrixA,&
!!!! crop variables
       pdlai, slai, & 
       ! for crop allocation
       in_cycle, deltai, dltaisen, ssla, pgrain, deltgrain, reprac, &
       nger, nlev, ndrp,  nlax, &
       c_reserve, c_leafb, nmat, nrec, N_limfert, tday_counter, &
!!!! end crop variables, xuhui
       zz_coef_deep, deepC_a, deepC_s, deepC_p, & !pss:+
       ch4_flux_density_tot_0, ch4_flux_density_dif_0, ch4_flux_density_bub_0, ch4_flux_density_pla_0,&
       ch4_flux_density_tot_wet1,ch4_flux_density_dif_wet1,ch4_flux_density_bub_wet1,ch4_flux_density_pla_wet1,&
       ch4_flux_density_tot_wet2,ch4_flux_density_dif_wet2,ch4_flux_density_bub_wet2,ch4_flux_density_pla_wet2,&
       ch4_flux_density_tot_wet3,ch4_flux_density_dif_wet3,ch4_flux_density_bub_wet3,ch4_flux_density_pla_wet3,&
       ch4_flux_density_tot_wet4,ch4_flux_density_dif_wet4,ch4_flux_density_bub_wet4,ch4_flux_density_pla_wet4,&
       tsurf_year, &!) !pss:-
!JCADD
       wshtotsum, sr_ugb, compt_ugb, nb_ani, grazed_frac, &
       import_yield, sla_age1, t2m_14, sla_calc, snow, day_of_year, &
       when_growthinit_cut, &
       EndOfYear)
!ENDJCADD
    
  !! 0. Variable and parameter declaration

    !! 0.1 input

    INTEGER(i_std), INTENT(in)                                 :: npts                 !! Domain size (unitless)
    REAL(r_std), INTENT(in)                                    :: dt_days              !! Time step of Stomate (days)
    INTEGER(i_std), DIMENSION(npts,8), INTENT(in)              :: neighbours           !! Indices of the 8 neighbours of each grid 
                                                                                       !! point [1=N, 2=NE, 3=E, 4=SE,
                                                                                       !!  5=S, 6=SW, 7=W, 8=NW] 
    REAL(r_std), DIMENSION(npts,2), INTENT(in)                 :: resolution           !! Resolution at each grid point (m)  
                                                                                       !! [1=E-W, 2=N-S] 
    REAL(r_std),DIMENSION(npts,2),INTENT(in)                   :: lalo                 !! Geographical coordinates (latitude,longitude)
                                                                                       !! for pixels (degrees)
    REAL(r_std),DIMENSION (npts), INTENT (in)   :: contfrac          !! Fraction of continent in the grid cell (unitless)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: clay                 !! Clay fraction (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: herbivores           !! Time constant of probability of a leaf to 
                                                                                       !! be eaten by a herbivore (days) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: tsurf_daily          !! Daily surface temperatures (K)
    REAL(r_std), DIMENSION(npts,nbdl), INTENT(in)              :: tsoil_daily          !! Daily soil temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: t2m_daily            !! Daily 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: t2m_min_daily        !! Daily minimum 2 meter temperatures (K)
    !spitfire
    INTEGER(i_std),INTENT(in)                            :: date               !! Date (days) 
    ! daily maximum 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                       :: t2m_max_daily
    ! daily precip (mm/day)
    REAL(r_std), DIMENSION(npts), INTENT(in)                        :: precip_daily
    ! Wind speed 
    REAL(r_std), DIMENSION(npts), INTENT(in)                       :: wspeed_daily
    ! Lightning flash rate
    REAL(r_std), DIMENSION(npts), INTENT(in)                       :: lightn
    ! Population density rate
    REAL(r_std), DIMENSION(npts), INTENT(inout)                    :: popd !popd declared and allocated and input in slowproc.f90
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                           :: read_observed_ba
    ! Observed burned area
    REAL(r_std),DIMENSION (npts), INTENT (in)                      :: observed_ba

    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_cf_coarse
    ! Observed burned area
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: cf_coarse
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_cf_fine
    ! Observed burned area
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: cf_fine
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_ratio
    ! Observed burned area
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: ratio
    ! Flag for read in observed burned area
    LOGICAL, INTENT (in)                                   :: read_ratio_flag
    ! Observed burned area
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: ratio_flag

    !endspit

    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: litterhum_daily      !! Daily litter humidity (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts,nbdl), INTENT(in)              :: soilhum_daily        !! Daily soil humidity (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: maxmoiavail_lastyear !! Last year's maximum moisture availability 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: minmoiavail_lastyear !! Last year's minimum moisture availability 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: gdd0_lastyear        !! Last year's GDD0 (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: precip_lastyear      !! Lastyear's precipitation 
                                                                                       !! @tex $(mm year^{-1})$ @endtex
										       !! to determine if establishment possible
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: moiavail_month       !! "Monthly" moisture availability (0 to 1, 
                                                                                       !! unitless) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: moiavail_week        !! "Weekly" moisture availability 
                                                                                       !! (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: t2m_longterm         !! "Long term" 2 meter reference 
                                                                                       !! temperatures (K) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: t2m_month            !! "Monthly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: t2m_week             !! "Weekly" 2-meter temperatures (K)
    ! "seasonal" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: Tseason
    ! temporary variable to calculate Tseason
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: Tseason_length
    ! temporary variable to calculate Tseason
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: Tseason_tmp

    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: Tmin_spring_time     !! Number of days after begin_leaves (leaf onset) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: onset_date           !! Date in the year at when the leaves started to grow(begin_leaves)

    !pss:+
    REAL(r_std), DIMENSION(npts), INTENT(in)                   :: tsurf_year           ! annual surface temperatures (K)
    !pss:-
    REAL(r_std), DIMENSION(npts,nbdl), INTENT(in)              :: tsoil_month          !! "Monthly" soil temperatures (K)
    REAL(r_std), DIMENSION(npts,nbdl), INTENT(in)              :: soilhum_month        !! "Monthly" soil humidity
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)               :: gdd_m5_dormance      !! Growing degree days (K), threshold -5 deg 
                                                                                       !! C (for phenology) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)               :: gdd_from_growthinit  !! growing degree days, since growthinit for crops
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: gdd_midwinter        !! Growing degree days (K), since midwinter 
                                                                                       !! (for phenology) - this is written to the history files 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)               :: ncd_dormance         !! Number of chilling days (days), since 
                                                                                       !! leaves were lost (for phenology) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)               :: ngd_minus5           !! Number of growing days (days), threshold 
                                                                                       !! -5 deg C (for phenology) 
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(in)        :: turnover_longterm    !! "Long term" turnover rate  
                                                                                       !! @tex $(gC m^{-2} year^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: gpp_daily            !! Daily gross primary productivity  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)       :: gpp_week                !! Mean weekly gross primary productivity 
                                                                                !! @tex $(gC m^{-2} day^{-1})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)               :: time_hum_min         !! Time elapsed since strongest moisture 
                                                                                       !! availability (days) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: maxfpc_lastyear      !! Last year's maximum foliage projected
                                                                                       !! coverage for each natural PFT,
                                                                                       !! @tex $(m^2 m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(in)        :: resp_maint_part      !! Maintenance respiration of different 
                                                                                       !! plant parts  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: fpc_max              !! "Maximal" coverage fraction of a PFT (LAI 
                                                                                       !! -> infinity) on ground  
                                                                                       !! @tex $(m^2 m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(ndeep),   INTENT (in)               :: zz_coef_deep         !! deep vertical profile
    LOGICAL, INTENT(in)                                        :: EndOfYear          !! Flag set to true for the first sechiba time step on the year.    
!!!!! crop variables

    ! FOR CROP---STICS
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                  :: pdlai
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)               :: slai
   
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: in_cycle
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: deltai
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: dltaisen
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)              :: ssla
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: pgrain
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: deltgrain
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: reprac
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)              :: nger
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)              :: nlev
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)              :: ndrp
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)              :: nmat
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)              :: nlax

!    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: N_limfert !!
!    defined already in GM
    INTEGER(i_std), INTENT(in)                                :: tday_counter

!!!!! end crop variables, xuhui

  !! 0.2 Output variables
    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: npp_daily            !! Net primary productivity 
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(out) :: turnover_daily       !! Turnover rates 
                                                                                       !! @tex $(gC m^{-2} year^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: co2_to_bm            !! CO2 taken up from atmosphere when 
                                                                                       !! introducing a new PFT (introduced for 
                                                                                       !! carbon balance closure) 
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: co2_fire             !! Carbon emitted into the atmosphere by 
                                                                                       !! fire (living and dead biomass)  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: resp_hetero          !! Heterotrophic respiration
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: resp_maint           !! Maintenance respiration  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: resp_growth          !! Growth respiration  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    
    REAL(r_std), DIMENSION(npts), INTENT(inout)                :: deadleaf_cover       !! Fraction of soil covered by dead leaves 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: vcmax                !! Maximum rate of carboxylation 
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(out):: bm_to_litter      !! Conversion of biomass to litter 
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    LOGICAL, DIMENSION(npts,nvm), INTENT(out)                  :: begin_leaves         !! signal to start putting leaves on (true/false)

    ! Wetland CH4 methane density
    !pss:+
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_tot_0
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_dif_0
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_bub_0
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_pla_0

    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_tot_wet1
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_dif_wet1
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_bub_wet1
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_pla_wet1

    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_tot_wet2
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_dif_wet2
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_bub_wet2
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_pla_wet2

    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_tot_wet3
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_dif_wet3
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_bub_wet3
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_pla_wet3

    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_tot_wet4
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_dif_wet4
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_bub_wet4
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ch4_flux_density_pla_wet4
    !pss:-

!!!!! crop variables
    ! for crop c pools
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)             :: c_reserve
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)             :: c_leafb

    ! for crop turnover
    INTEGER(i_std), DIMENSION(npts,nvm), INTENT(in)             :: nrec   !harvest date
!!!!! end crop variables, xuhui 

    !! 0.3 Modified variables
    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: height               !! Height of vegetation (m) 
    REAL(r_std), DIMENSION(npts,nlevs), INTENT(inout)          :: control_moist        !! Moisture control of heterotrophic 
                                                                                       !! respiration (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts,nlevs), INTENT(inout)          :: control_temp         !! Temperature control of heterotrophic 
                                                                                       !! respiration, above and below 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(inout)      :: soilcarbon_input     !! Quantity of carbon going into carbon 
                                                                                       !! pools from litter decomposition  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: lai                  !! Leaf area index OF AN INDIVIDUAL PLANT,
										       !! where a PFT contains n indentical plants
										       !! i.e., using the mean individual approach 
                                                                                       !! @tex $(m^2 m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: rprof                !! Prescribed root depth (m) 
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)                :: PFTpresent           !! Tab indicating which PFTs are present in 
                                                                                       !! each pixel 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: age                  !! Age (years)    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: fireindex            !! Probability of fire (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: firelitter           !! Longer term litter above the ground that 
                                                                                       !! can be burned, @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)  :: leaf_age             !! Leaf age (days)
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)  :: leaf_frac            !! Fraction of leaves in leaf age class, 
                                                                                       !! (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: biomass        !! Biomass @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: ind                  !! Density of individuals 
                                                                                       !! @tex $(m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: adapted              !! Adaptation of PFT (killed if too cold) 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: regenerate           !! "Fitness": Winter sufficiently cold for 
                                                                                       !! PFT regeneration ? (0 to 1, unitless) 
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)                :: senescence           !! Flag for setting senescence stage (only 
                                                                                       !! for deciduous trees) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: when_growthinit      !! How many days ago was the beginning of 
                                                                                       !! the growing season (days) 
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)      :: litterpart           !! Fraction of litter above the ground 
                                                                                       !! belonging to different PFTs
                                                                                       !! (0 to 1, unitless)
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs,nelements), INTENT(inout):: litter     !! Metabolic and structural litter, above 
                                                                                       !! and below ground 
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
!JCADD for grazing litter
    REAL(r_std), DIMENSION(npts,nlitt,nvm), INTENT(out):: litter_avail
    REAL(r_std), DIMENSION(npts,nlitt,nvm) , INTENT(out):: litter_not_avail
    REAL(r_std), DIMENSION(npts,nlitt,nvm), INTENT(in):: litter_avail_frac
!ENDJCADD
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)      :: dead_leaves          !! Dead leaves on ground, per PFT, metabolic 
                                                                                       !! and structural,  
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(inout)      :: carbon               !! Carbon pool: active, slow, or passive, 
                                                                                       !! @tex $(gC m^{-2})$ @endtex  
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(inout)      :: carbon_surf
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nlevs), INTENT(inout)      :: lignin_struc         !! Ratio of Lignin/Carbon in structural 
                                                                                       !! litter, above and below ground,  
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: veget_max            !! "Maximal" coverage fraction of a PFT (LAI 
                                                                                       !! -> infinity) on ground 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: veget_max_new       !! "Maximal" coverage fraction of a PFT  
                                                                                       !! (LAI-> infinity) on ground (unitless) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: npp_longterm         !! "Long term" mean yearly primary 
                                                                                       !! productivity 
                                                                                       !! @tex $(gC m^{-2} year^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: lm_lastyearmax       !! Last year's maximum leaf mass, for each 
                                                                                       !! PFT @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: veget_lastlight      !! Vegetation fractions (on ground) after 
                                                                                       !! last light competition  
                                                                                       !! @tex $(m^2 m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: everywhere           !! Is the PFT everywhere in the grid box or 
                                                                                       !! very localized (after its introduction) 
                                                                                       !! (unitless) 
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)                :: need_adjacent        !! In order for this PFT to be introduced, 
                                                                                       !! does it have to be present in an 
                                                                                       !! adjacent grid box? 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: RIP_time             !! How much time ago was the PFT eliminated 
                                                                                       !! for the last time (y) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)            :: turnover_time        !! Turnover_time of leaves for grasses 
                                                                                       !! (days)
    REAL(r_std), DIMENSION(npts,nvm),INTENT(in)             :: vegetnew_firstday        !! New "maximal" coverage fraction of a PFT 
                                                                                       !! (LAI -> infinity) (unitless) 
    REAL(r_std),DIMENSION(npts,12),INTENT(inout)               :: glccNetLCC      
    REAL(r_std),DIMENSION(npts,12),INTENT(inout)               :: glccSecondShift 
    REAL(r_std),DIMENSION(npts,12),INTENT(inout)               :: glccPrimaryShift  
    REAL(r_std), DIMENSION(npts,12),INTENT(inout)              :: harvest_matrix       !! The gross land use change matrix in case 
                                                                                       !! of gross land cover change is simulated.
    REAL(r_std), DIMENSION(npts,nvm),INTENT(inout)              :: bound_spa           !! The gross land use change matrix in case 
                                                                                       !! of gross land cover change is simulated.
    REAL(r_std),DIMENSION(npts,0:10,nwp), INTENT(inout)            :: prod10               !! Products remaining in the 10
                                                                                       !! year-turnover pool after the annual 
                                                                                       !! release for each compartment (10
                                                                                       !! + 1 : input from year of land cover 
                                                                                       !! change) @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts,0:100,nwp), INTENT(inout)           :: prod100              !! Products remaining in the 100 
                                                                                       !! year-turnover pool after the annual 
                                                                                       !! release for each compartment (100 
                                                                                       !! + 1 : input from year of land cover 
                                                                                       !! change) @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts,10,nwp), INTENT(inout)              :: flux10               !! Annual release from the 10
                                                                                       !! year-turnover pool compartments  
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts,100,nwp), INTENT(inout)             :: flux100              !! Annual release from the 100 
                                                                                       !! year-turnover pool compartments  
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts,nwp), INTENT(inout)                 :: convflux             !! Release during first year following land 
                                                                                       !! cover change @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts,nwp), INTENT(inout)                 :: cflux_prod10         !! Total annual release from the 10 
                                                                                       !! year-turnover pool 
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts,nwp), INTENT(inout)                 :: cflux_prod100        !! Total annual release from the 100 
                                                                                       !! year-turnover pool 
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts), INTENT(inout)                :: harvest_above        !! Harvest above ground biomass for 
                                                                                       !! agriculture @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts), INTENT(inout)                :: carb_mass_total      !! Carbon Mass total (soil, litter, veg) 
                                                                                       !! @tex $(gC m^{-2})$ @endtex  
    REAL(r_std), DIMENSION(npts,nvm,nbpools,nbpools), INTENT(inout) :: MatrixA         !! Matrix containing the fluxes  
                                                                                       !! between the carbon pools
                                                                                       !! per sechiba time step 
                                                                                       !! @tex $(gC.m^2.day^{-1})$ @endtex
    REAL(r_std), DIMENSION(npts,ndeep,nvm), INTENT(inout)         :: deepC_a           !! permafrost soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(npts,ndeep,nvm), INTENT(inout)         :: deepC_s           !! permafrost soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(npts,ndeep,nvm), INTENT(inout)         :: deepC_p           !! permafrost soil carbon (g/m**3) passive
!JCADD

!glcc
    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: glcc_pft       !! a temporary variable to hold the fraction of ipft->ivma, i.e., from
                                                                              !! PFT_{ipft} to the youngest age class of MTC_{ivma}
!spitfire
    ! Nesterov index accumulated
    REAL(r_std), DIMENSION(npts), INTENT(inout)                           :: ni_acc
    REAL(r_std), DIMENSION(npts), INTENT(inout)                           :: fire_numday
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)                       :: bafrac_deforest_accu 
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)       :: emideforest_litter_accu 
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout)      :: emideforest_biomass_accu 
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs,nelements), INTENT(inout) :: deforest_litter_remain   !! Vegetmax-weighted remaining litter on the ground for 
                                                                                                      !! deforestation region.
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout)      :: deforest_biomass_remain  !! Vegetmax-weighted remaining biomass on the ground for 
                                                                                                      !! deforestation region.
    ! fuel classes (1, 10, 100, 1000 hours)
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_1hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_10hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_100hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_1000hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: def_fuel_1hr_remain
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: def_fuel_10hr_remain
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: def_fuel_100hr_remain
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: def_fuel_1000hr_remain
    REAL(r_std), DIMENSION(npts)                                         :: d_area_burnt
    REAL(r_std), DIMENSION(npts)                                         :: d_numfire
    REAL(r_std), DIMENSION(npts)                                         :: fc_crown
    ! parameter for potential human-caused ignitions, ignitions ind^{-1}day{-1}, used in lpj_spitfire.f90
    REAL(r_std), DIMENSION(npts), INTENT(in)                             :: a_nd
!endspit
   ! snow mass (kg/m2)
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: snow
    ! "14days" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)         ::  t2m_14
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sla_calc
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  wshtotsum
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sr_ugb
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  compt_ugb
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  nb_ani
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  grazed_frac
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  import_yield
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  sla_age1
    INTEGER(i_std), INTENT(in)                       ::  day_of_year
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  N_limfert
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  when_growthinit_cut
!    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)  ::  resp_hetero_litter_d
!    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(inout)  :: resp_hetero_soil_d
!ENDJCADD
    !! 0.4 Local variables

    REAL(r_std), DIMENSION(npts,nvm,nelements)                  :: tot_bm_to_litter    !! Total conversion of biomass to litter 
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nelements)                  :: tot_live_biomass    !! Total living biomass  
                                                                                       !! @tex $(gC m{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements)           :: bm_alloc            !! Biomass increase, i.e. NPP per plant part 
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nelements)                  :: tot_turnover        !! Total turnover rate  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm)                            :: tot_litter_soil_carb!! Total soil and litter carbon  
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm)                            :: tot_litter_carb     !! Total litter carbon 
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm)                            :: tot_soil_carb       !! Total soil carbon  
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(npts)                                :: carb_mass_variation !! Carbon Mass variation  
                                                                                       !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm)                            :: cn_ind              !! Crown area of individuals 
                                                                                       !! @tex $(m^{2})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm)                            :: woodmass_ind        !! Woodmass of individuals (gC) 
    REAL(r_std), DIMENSION(npts,nvm,nparts)                     :: f_alloc             !! Fraction that goes into plant part 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts)                                :: avail_tree          !! Space availability for trees 
                                                                                       !! (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts)                                :: avail_grass         !! Space availability for grasses 
                                                                                       !! (0 to 1, unitless) 
    INTEGER                                                     :: j,ivm,ivma,ipts,ilev,ilitt
    REAL(r_std),DIMENSION(npts)                                 :: prod10_total        !! Total products remaining in the pool 
                                                                                       !! after the annual release 
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts)                                 :: prod100_total       !! Total products remaining in the pool 
                                                                                       !! after the annual release 
                                                                                       !! @tex $(gC m^{-2})$ @endtex 
    REAL(r_std),DIMENSION(npts)                                 :: cflux_prod_total    !! Total flux from conflux and the 10/100 
                                                                                       !! year-turnover pool 
                                                                                       !! @tex $(gC m^{-2} year^{-1})$ @endtex 
    REAL(r_std),DIMENSION(npts,nvm)                             :: veget_max_tmp       !! "Maximal" coverage fraction of a PFT 
                                                                                       !! (LAI-> infinity) on ground (unitless) 
!!!!! crop
    REAL(r_std), DIMENSION(npts,nvm)                            :: crop_export         !! Cropland export (harvest & straw)
!!!!! end crop, xuhui
    REAL(r_std), DIMENSION(npts,nvm)                            :: mortality           !! Fraction of individual dying this time 
                                                                                       !! step (0 to 1, unitless) 
    REAL(r_std), DIMENSION(npts)                                :: vartmp              !! Temporary variable used to add history
    REAL(r_std), DIMENSION(npts,nvm)                            :: histvar             !! History variables
    REAL(r_std), DIMENSION(npts,nvm)                            :: lcc                 !! land cover change, i.e., loss of each PFT,
                                                                                       !! positive value indicates loss.
    REAL(r_std),DIMENSION(npts,nvm)                             :: deflitsup_total
    REAL(r_std),DIMENSION(npts,nvm)                             :: defbiosup_total
    !glcc
    REAL(r_std), DIMENSION(npts,nvm,nvmap)                      :: glcc_pftmtc    !! a temporary variable to hold the fractions each PFT is going to lose
    REAL(r_std), DIMENSION(npts,12)                             :: glccReal       !! The "real" glcc matrix that we apply in the model
                                                                                  !! after considering the consistency between presribed
                                                                                  !! glcc matrix and existing vegetation fractions.
    REAL(r_std), DIMENSION(npts,12)                              :: IncreDeficit   !! "Increment" deficits, negative values mean that 
                                                                                  !! there are not enough fractions in the source PFTs
                                                                                  !! /vegetations to target PFTs/vegetations. I.e., these
                                                                                  !! fraction transfers are presribed in LCC matrix but
                                                                                  !! not realized.
    REAL(r_std), DIMENSION(npts)                       :: Deficit_pf2yf_final     !! 
    REAL(r_std), DIMENSION(npts)                       :: Deficit_sf2yf_final     !! 
    REAL(r_std), DIMENSION(npts)                       :: pf2yf_compen_sf2yf      !! 
    REAL(r_std), DIMENSION(npts)                       :: sf2yf_compen_pf2yf      !!
    INTEGER :: f2g=1, f2p=2, f2c=3
    INTEGER :: g2f=4, g2p=5, g2c=6, p2f=7, p2g=8, p2c=9, c2f=10, c2g=11, c2p=12
    REAL(r_std)                                                 :: valtmp

!JCADD lcchange of managed grassland
    ! "maximal" coverage fraction of a PFT (LAI -> infinity) on ground
    INTEGER(i_std)                       :: ier
    LOGICAL                               :: l_error =.FALSE.



   REAL(r_std), DIMENSION(npts,7) :: outflux_sta,outflux_end
   REAL(r_std), DIMENSION(npts,3) :: influx_sta,influx_end
   REAL(r_std), DIMENSION(npts,4) :: pool_sta,pool_end
   INTEGER(i_std) :: ind_biomass,ind_litter,ind_soil,ind_prod,ind_co2tobm,ind_gpp,ind_npp,&
                     ind_bm2lit,ind_resph,ind_respm,ind_respg,ind_convf,ind_cflux,ind_fire

!ENDJCADD
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering stomate_lpj'

  !! 1. Initializations

    lcc(:,:) = zero    
    glccReal(:,:) = zero
    glcc_pftmtc(:,:,:) = zero

!JCADD
    IF (firstcall) THEN

        firstcall = .FALSE.

        !Config  Key  = ENABLE_GRAZING
        !Config  Desc = grazing allowed
        !Config  Def  = n
        !Config  Help = flag for choose if you want animals or not.
        !
        enable_grazing = .FALSE.
        CALL getin_p('ENABLE_GRAZING',enable_grazing)
        WRITE (numout,*) 'enable_grazing',enable_grazing
        WRITE (numout,*) 'manage',is_grassland_manag
        WRITE (numout,*) 'cut',is_grassland_cut
        WRITE (numout,*) 'grazed',is_grassland_grazed

        emideforest_biomass_accu(:,:,:,:) = zero
        emideforest_litter_accu(:,:,:,:) = zero
        bafrac_deforest_accu(:,:) = zero


      IF (do_now_stomate_lcchange) THEN
        IF (use_age_class) THEN
          IF (SingleAgeClass) THEN
            CALL gross_glcc_firstday_SinAgeC_fh(npts,veget_max,harvest_matrix, &
                        glccSecondShift,glccPrimaryShift,glccNetLCC,&
                        glccReal,glcc_pft,glcc_pftmtc,IncreDeficit, &
                        Deficit_pf2yf_final, Deficit_sf2yf_final,   &
                        pf2yf_compen_sf2yf, sf2yf_compen_pf2yf)

          ELSE
            CALL gross_glcc_firstday_fh(npts,veget_max,harvest_matrix, &
                        glccSecondShift,glccPrimaryShift,glccNetLCC,&
                        glccReal,glcc_pft,glcc_pftmtc,IncreDeficit, &
                        Deficit_pf2yf_final, Deficit_sf2yf_final,   &
                        pf2yf_compen_sf2yf, sf2yf_compen_pf2yf)
          ENDIF
          ! We put only the conversion of tree->Notree as deforestation
          DO ipts = 1,npts
            DO ivm = 1,nvm
              DO ivma = 1,nvmap
                IF (is_tree(ivm) .AND. .NOT. is_tree(start_index(ivma))) THEN
                  lcc(ipts,ivm) = lcc(ipts,ivm) + glcc_pftmtc(ipts,ivm,ivma)
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ELSE ! (.NOT. use_age_class), i.e., net land cover change.
          ! note here veget_max is the last-year veget_max; vegetnew_firstday
          ! is the veget_max of next year, we need lcc to be >0 where forest
          ! area decreased, i.e., when vegetnew_firstday < veget_max
          lcc(:,:) = veget_max(:,:) - vegetnew_firstday(:,:)
        ENDIF

        IF (.NOT. allow_deforest_fire) lcc(:,:) = zero

        ! we creat the proxy that's needed for deforestation fire simulation.
        DO ipts = 1,npts
          DO ivm = 1,nvm
            deforest_litter_remain(ipts,:,ivm,:,:) = litter(ipts,:,ivm,:,:)*lcc(ipts,ivm)
            deforest_biomass_remain(ipts,ivm,:,:) = biomass(ipts,ivm,:,:)*lcc(ipts,ivm)
            def_fuel_1hr_remain(ipts,ivm,:,:) = fuel_1hr(ipts,ivm,:,:)*lcc(ipts,ivm)
            def_fuel_10hr_remain(ipts,ivm,:,:) = fuel_10hr(ipts,ivm,:,:)*lcc(ipts,ivm)
            def_fuel_100hr_remain(ipts,ivm,:,:) = fuel_100hr(ipts,ivm,:,:)*lcc(ipts,ivm)
            def_fuel_1000hr_remain(ipts,ivm,:,:) = fuel_1000hr(ipts,ivm,:,:)*lcc(ipts,ivm)
          ENDDO
        ENDDO
      ENDIF


    END IF !firstcall
    
    !! 1.1 Initialize variables to zero
    co2_to_bm(:,:) = zero
    co2_fire(:,:) = zero
    npp_daily(:,:) = zero
    resp_maint(:,:) = zero
    resp_growth(:,:) = zero
    harvest_above(:) = zero
    bm_to_litter(:,:,:,:) = zero
    cn_ind(:,:) = zero
    woodmass_ind(:,:) = zero
    turnover_daily(:,:,:,:) = zero
    crop_export(:,:) = zero
    deflitsup_total(:,:) = zero
    defbiosup_total(:,:) = zero
    
    !! 1.2  Initialize variables to veget_max
    veget_max_tmp(:,:) = veget_max(:,:)

    !! 1.3 Calculate some vegetation characteristics
    !! 1.3.1 Calculate some vegetation characteristics 
    !        Calculate cn_ind (individual crown mass) and individual height from
    !        state variables if running DGVM or dynamic mortality in static cover mode
    !??        Explain (maybe in the header once) why you mulitply with veget_max in the DGVM
    !??        and why you don't multiply with veget_max in stomate.
    IF ( ok_dgvm .OR. .NOT.lpj_gap_const_mort) THEN
       IF(ok_dgvm) THEN
          WHERE (ind(:,:).GT.min_stomate)
             woodmass_ind(:,:) = &
                  ((biomass(:,:,isapabove,icarbon)+biomass(:,:,isapbelow,icarbon) &
                  +biomass(:,:,iheartabove,icarbon)+biomass(:,:,iheartbelow,icarbon)) & 
                  *veget_max(:,:))/ind(:,:)
          ENDWHERE
       ELSE
          WHERE (ind(:,:).GT.min_stomate)
             woodmass_ind(:,:) = &
                  (biomass(:,:,isapabove,icarbon)+biomass(:,:,isapbelow,icarbon) &
                  +biomass(:,:,iheartabove,icarbon)+biomass(:,:,iheartbelow,icarbon))/ind(:,:)
          ENDWHERE
       ENDIF

       CALL crown (npts,  PFTpresent, &
            ind, biomass, woodmass_ind, &
            veget_max, cn_ind, height)
    ENDIF

    !! 1.3.2 Prescribe characteristics if the vegetation is not dynamic
    !        IF the DGVM is not activated, the density of individuals and their crown
    !        areas don't matter, but they should be defined for the case we switch on
    !        the DGVM afterwards. At the first call, if the DGVM is not activated, 
    !        impose a minimum biomass for prescribed PFTs and declare them present.
    CALL prescribe (npts, &
         veget_max, dt_days, PFTpresent, everywhere, when_growthinit, &
         biomass, leaf_frac, ind, cn_ind, co2_to_bm)


  !! 2. Climatic constraints for PFT presence and regenerativeness

    !   Call this even when DGVM is not activated so that "adapted" and "regenerate"
    !   are kept up to date for the moment when the DGVM is activated.
    CALL constraints (npts, dt_days, &
         t2m_month, t2m_min_daily,when_growthinit, &
         adapted, regenerate, Tseason)

    
  !! 3. Determine introduction and elimination of PTS based on climate criteria
 
    IF ( ok_dgvm ) THEN
      
       !! 3.1 Calculate introduction and elimination
       CALL pftinout (npts, dt_days, adapted, regenerate, &
            neighbours, veget_max, &
            biomass, ind, cn_ind, age, leaf_frac, npp_longterm, lm_lastyearmax, senescence, &
            PFTpresent, everywhere, when_growthinit, need_adjacent, RIP_time, &
            co2_to_bm, &
            avail_tree, avail_grass, &
!JCADD
            sla_calc)
!ENDJCADD 
       !! 3.2 Reset attributes for eliminated PFTs.
       !     This also kills PFTs that had 0 leafmass during the last year. The message
       !     "... after pftinout" is misleading in this case.
       CALL kill (npts, 'pftinout  ', lm_lastyearmax, &
            ind, PFTpresent, cn_ind, biomass, senescence, RIP_time, &
            lai, age, leaf_age, leaf_frac, npp_longterm, &
            when_growthinit, everywhere, veget_max, bm_to_litter)

       
       !! 3.3 Calculate new crown area and diameter 
       !      Calculate new crown area, diameter and maximum vegetation cover**[No longer used in the subroutine]
       !      unsure whether this is really required
       !      - in theory this could ONLY be done at the END of stomate_lpj
       !      calculate woodmass of individual tree
       WHERE ((ind(:,:).GT.min_stomate))
          WHERE  ( veget_max(:,:) .GT. min_stomate)
             woodmass_ind(:,:) = &
                  ((biomass(:,:,isapabove,icarbon) + biomass(:,:,isapbelow,icarbon) &
                  + biomass(:,:,iheartabove,icarbon) + biomass(:,:,iheartbelow,icarbon))*veget_max(:,:))/ind(:,:)
          ELSEWHERE
             woodmass_ind(:,:) =(biomass(:,:,isapabove,icarbon) + biomass(:,:,isapbelow,icarbon) &
                  + biomass(:,:,iheartabove,icarbon) + biomass(:,:,iheartbelow,icarbon))/ind(:,:)
          ENDWHERE

       ENDWHERE
       
       ! Calculate crown area and diameter for all PFTs (including the newly established)
       CALL crown (npts, PFTpresent, &
            ind, biomass, woodmass_ind, &
            veget_max, cn_ind, height)

    ENDIF
    
  !! 4. Phenology

    !! 4.1 Write values to history file
    !      Current values for ::when_growthinit 
    CALL xios_orchidee_send_field("WHEN_GROWTHINIT",when_growthinit)

    CALL histwrite_p (hist_id_stomate, 'WHEN_GROWTHINIT', itime, when_growthinit, npts*nvm, horipft_index)

    ! Set and write values for ::PFTpresent
    WHERE(PFTpresent)
       histvar=un
    ELSEWHERE
       histvar=zero
    ENDWHERE

    CALL xios_orchidee_send_field("PFTPRESENT",histvar)

    CALL histwrite_p (hist_id_stomate, 'PFTPRESENT', itime, histvar, npts*nvm, horipft_index)

    ! Set and write values for gdd_midwinter
    WHERE(gdd_midwinter.EQ.undef)
       histvar=val_exp
    ELSEWHERE
       histvar=gdd_midwinter
    ENDWHERE

    CALL xios_orchidee_send_field("GDD_MIDWINTER",histvar)

    CALL histwrite_p (hist_id_stomate, 'GDD_MIDWINTER', itime, histvar, npts*nvm, horipft_index)

    ! Set and write values for gdd_m5_dormance
    WHERE(gdd_m5_dormance.EQ.undef)
       histvar=val_exp
    ELSEWHERE
       histvar=gdd_m5_dormance
    ENDWHERE
    
    CALL xios_orchidee_send_field('GDD_M5_DORMANCE',histvar)
    CALL histwrite_p (hist_id_stomate, 'GDD_M5_DORMANCE', itime, histvar, npts*nvm, horipft_index)

    ! Set and write values for ncd_dormance
    WHERE(ncd_dormance.EQ.undef)
       histvar=val_exp
    ELSEWHERE
       histvar=ncd_dormance
    ENDWHERE

    CALL xios_orchidee_send_field("NCD_DORMANCE",histvar)

    CALL histwrite_p (hist_id_stomate, 'NCD_DORMANCE', itime, histvar, npts*nvm, horipft_index)

    !! 4.2 Calculate phenology
    ! WRITE(numout,*) 'slai before lpj_phenology: ',slai(1,12:14)
    CALL phenology (npts, dt_days, PFTpresent, &
         veget_max, &
         t2m_longterm, t2m_month, t2m_week, gpp_daily, &
         maxmoiavail_lastyear, minmoiavail_lastyear, &
         moiavail_month, moiavail_week, &
         gdd_m5_dormance, gdd_midwinter, ncd_dormance, ngd_minus5, &
         senescence, time_hum_min, &
         biomass, leaf_frac, leaf_age, &
         when_growthinit, co2_to_bm, &
         pdlai, slai, deltai, ssla, &
         begin_leaves, &!)
!JCADD
         sla_calc)
!ENDJCADD
!    IF (printlev>=4) THEN
!        WRITE(*,*) 'biomass reserve after phenology is', biomass(:,12:14,icarbres, icarbon)
!    ENDIF

!    IF ( ANY(biomass(1,14,:,icarbon) .lt. 0. ) ) THEN
!        WRITE(numout,*) 'biomass low0 after phenology'
!        WRITE(numout,*) 'biomass(1,14,:,icarbon)',biomass(1,14,:,icarbon)
!    ENDIF
    
  !! 5. Allocate C to different plant parts
    !WRITE(numout,*) 'slai before lpj_alloc: ',slai(1,12:14)
    CALL alloc (npts, dt_days, &
         lai, veget_max, senescence, when_growthinit, &
         moiavail_week, tsoil_month, soilhum_month, &
         biomass, age, leaf_age, leaf_frac, rprof, f_alloc, &!)
         deltai, ssla, & !added for crop by xuhui
!JCADD
         sla_calc, when_growthinit_cut)
!ENDJCADD
!    IF (printlev>=4) THEN
!        WRITE(*,*) 'biomass reserve after alloc is', biomass(:,12:14, icarbres,icarbon)
!    ENDIF

!    IF ( ANY(biomass(1,14,:,icarbon) .lt. 0. ) ) THEN
!        WRITE(numout,*) 'biomass low0 after alloc'
!        WRITE(numout,*) 'biomass(1,14,:,icarbon)',biomass(1,14,:,icarbon)
!    ENDIF

  !! 6. NPP, maintenance and growth respiration
    ! WRITE(numout,*) 'slai before lpj_npp_calc: ',slai(1,12:14)
    !! 6.1 Calculate NPP and respiration terms
    CALL npp_calc (npts, dt_days, &
         PFTpresent, &
         t2m_daily, tsoil_daily, lai, rprof, &
         gpp_daily, f_alloc, bm_alloc, resp_maint_part,&
         biomass, leaf_age, leaf_frac, age, &
         resp_maint, resp_growth, npp_daily, &!)
         ! for crop bm_alloc
!!! crop variables
         in_cycle, deltai, dltaisen, ssla, pgrain, deltgrain, reprac, &
         nger, nlev, ndrp, nlax, nmat, nrec, &
         c_reserve, c_leafb, slai, tday_counter, veget_max, &
!!! end crop, xuhui
!JCADD
         sla_calc, sla_age1,N_limfert)
!ENDJCADD
    IF (printlev>=4) THEN
        WRITE(*,*) 'biomass reserve after npp_calc is', biomass(1,:,icarbres, icarbon)
    ENDIF

!    IF ( ANY(biomass(1,14,:,icarbon) .lt. 0. ) ) THEN
!        WRITE(numout,*) 'biomass low0 after npp_calc'
!        WRITE(numout,*) 'biomass(1,14,:,icarbon)',biomass(1,14,:,icarbon)
!    ENDIF

    !! 6.2 Kill slow growing PFTs in DGVM or STOMATE with constant mortality
    IF ( ok_dgvm .OR. .NOT.lpj_gap_const_mort) THEN
       CALL kill (npts, 'npp       ', lm_lastyearmax,  &
            ind, PFTpresent, cn_ind, biomass, senescence, RIP_time, &
            lai, age, leaf_age, leaf_frac, npp_longterm, &
            when_growthinit, everywhere, veget_max, bm_to_litter)

       !! 6.2.1 Update wood biomass      
       !        For the DGVM
       IF(ok_dgvm) THEN
          WHERE (ind(:,:).GT.min_stomate)
             woodmass_ind(:,:) = &
                  ((biomass(:,:,isapabove,icarbon) + biomass(:,:,isapbelow,icarbon) &
                  + biomass(:,:,iheartabove,icarbon) + biomass(:,:,iheartbelow,icarbon)) & 
                  *veget_max(:,:))/ind(:,:)
          ENDWHERE

       ! For all pixels with individuals
       ELSE
          WHERE (ind(:,:).GT.min_stomate)
             woodmass_ind(:,:) = &
                  (biomass(:,:,isapabove,icarbon) + biomass(:,:,isapbelow,icarbon) &
                  + biomass(:,:,iheartabove,icarbon) + biomass(:,:,iheartbelow,icarbon))/ind(:,:)
          ENDWHERE
       ENDIF ! ok_dgvm

       !! 6.2.2 New crown area and maximum vegetation cover after growth
       CALL crown (npts, PFTpresent, &
            ind, biomass, woodmass_ind,&
            veget_max, cn_ind, height)

    ENDIF ! ok_dgvm
    
  !! 7. fire
    !WRITE(numout,*) 'slai before lpj_fire: ',slai(1,12:14)
    !! 7.1. Burn PFTs
    !CALL fire (npts, dt_days, litterpart, &
    !     litterhum_daily, t2m_daily, lignin_struc, veget_max, &
    !     fireindex, firelitter, biomass, ind, &
    !     litter, dead_leaves, bm_to_litter, &
    !     co2_fire, MatrixA)

!JCADD update available and not available litter for grazing litter
!spitfire

        !disable_fire and allow_deforest_fire are defined as constants in src_parameters/constantes_var.f90
        !disable_fire to DISABLE fire when being TRUE
        !allow_deforest_fire to activate deforestation fire module when being TRUE
        IF(.NOT.disable_fire) THEN
           CALL spitfire(npts, dt_days, veget_max,resolution,contfrac,   &
                PFTpresent,t2m_min_daily,t2m_max_daily,                  &    
                precip_daily,wspeed_daily,soilhum_daily(:,1),            &    
                lightn,litter(:,:,:,:,icarbon),ni_acc,fire_numday,       &
                fuel_1hr(:,:,:,icarbon),fuel_10hr(:,:,:,icarbon),fuel_100hr(:,:,:,icarbon), &    
                fuel_1000hr(:,:,:,icarbon),ind,biomass(:,:,:,icarbon),popd,a_nd,height,     &    
                read_observed_ba, observed_ba, read_cf_fine,cf_fine,                        &
                read_cf_coarse,cf_coarse,read_ratio_flag,                                   &
                ratio_flag,read_ratio,ratio,date,                                           &
                bm_to_litter(:,:,:,icarbon),co2_fire,                                       &
                lcc,bafrac_deforest_accu,emideforest_litter_accu(:,:,:,icarbon),emideforest_biomass_accu(:,:,:,icarbon),&
                deforest_litter_remain(:,:,:,:,icarbon),deforest_biomass_remain(:,:,:,icarbon),&
                def_fuel_1hr_remain(:,:,:,icarbon),def_fuel_10hr_remain(:,:,:,icarbon),&
                def_fuel_100hr_remain(:,:,:,icarbon),def_fuel_1000hr_remain(:,:,:,icarbon))
        ELSE
                ni_acc=0. 
                fire_numday=0.
                 
        ENDIF
!endspit
! after fire burning
  litter_avail(:,:,:) = litter(:,:,:,iabove,icarbon) * &
            litter_avail_frac(:,:,:)
  litter_not_avail(:,:,:) = litter(:,:,:,iabove,icarbon) * &
            (1.0 - litter_avail_frac(:,:,:))
!ENDJCADD
    !! 7.2 Kill PFTs in DGVM
    IF ( ok_dgvm ) THEN

       ! reset attributes for eliminated PFTs
       CALL kill (npts, 'fire      ', lm_lastyearmax, &
            ind, PFTpresent, cn_ind, biomass, senescence, RIP_time, &
            lai, age, leaf_age, leaf_frac, npp_longterm, &
            when_growthinit, everywhere, veget_max, bm_to_litter)

    ENDIF ! ok_dgvm
 
  !! 8. Tree mortality

    ! Does not depend on age, therefore does not change crown area.
    CALL gap (npts, dt_days, &
         npp_longterm, turnover_longterm, lm_lastyearmax, &
         PFTpresent, biomass, ind, bm_to_litter, mortality, t2m_min_daily, Tmin_spring_time, &!)
!JCADD
         sla_calc)
!ENDJCADD

    IF ( ok_dgvm ) THEN

       ! reset attributes for eliminated PFTs
       CALL kill (npts, 'gap       ', lm_lastyearmax, &
            ind, PFTpresent, cn_ind, biomass, senescence, RIP_time, &
            lai, age, leaf_age, leaf_frac, npp_longterm, &
            when_growthinit, everywhere, veget_max, bm_to_litter)

    ENDIF

  !! 10. Leaf senescence, new lai and other turnover processes
!    IF ( ANY(biomass(1,14,:,icarbon) .lt. 0. ) ) THEN
!        WRITE(numout,*) 'biomass low0 before turn'
!        WRITE(numout,*) 'biomass(1,14,:,icarbon)',biomass(1,14,:,icarbon)
!    ENDIF

    CALL turn (npts, dt_days, PFTpresent, &
         herbivores, &
         maxmoiavail_lastyear, minmoiavail_lastyear, &
         moiavail_week,  moiavail_month,t2m_longterm, t2m_month, t2m_week, veget_max, &
         gdd_from_growthinit, leaf_age, leaf_frac, age, lai, biomass, &
         turnover_daily, senescence,turnover_time, &!)
         nrec,crop_export, &
!JCADD
         sla_calc)
!ENDJCADD
!    IF ( ANY(biomass(1,14,:,icarbon) .lt. 0. ) ) THEN
!        WRITE(numout,*) 'biomass low0 after turn'
!        WRITE(numout,*) 'biomass(1,14,:,icarbon)',biomass(1,14,:,icarbon)
!    ENDIF
    !! 11. Light competition
    
    !! If not using constant mortality then kill with light competition
!    IF ( ok_dgvm .OR. .NOT.(lpj_gap_const_mort) ) THEN
    IF ( ok_dgvm ) THEN
 
       !! 11.1 Light competition
       CALL light (npts, dt_days, &
            veget_max, fpc_max, PFTpresent, cn_ind, lai, maxfpc_lastyear, &
            lm_lastyearmax, ind, biomass, veget_lastlight, bm_to_litter, mortality, &!)
!JCADD
         sla_calc)
!ENDJCADD
       
       !! 11.2 Reset attributes for eliminated PFTs
       CALL kill (npts, 'light     ', lm_lastyearmax, &
            ind, PFTpresent, cn_ind, biomass, senescence, RIP_time, &
            lai, age, leaf_age, leaf_frac, npp_longterm, &
            when_growthinit, everywhere, veget_max, bm_to_litter)

    ENDIF

    
  !! 12. Establishment of saplings
    
    IF ( ok_dgvm .OR. .NOT.lpj_gap_const_mort ) THEN

       !! 12.1 Establish new plants
       CALL establish (npts, lalo, dt_days, PFTpresent, regenerate, &
            neighbours, resolution, need_adjacent, herbivores, &
            precip_lastyear, gdd0_lastyear, lm_lastyearmax, &
            cn_ind, lai, avail_tree, avail_grass, npp_longterm, &
            leaf_age, leaf_frac, &
            ind, biomass, age, everywhere, co2_to_bm, veget_max, woodmass_ind, &
            mortality, bm_to_litter, &
!JCADD
            sla_calc)
!ENDJCADD

       !! 12.2 Calculate new crown area (and maximum vegetation cover)
       CALL crown (npts, PFTpresent, &
            ind, biomass, woodmass_ind, &
            veget_max, cn_ind, height)

    ENDIF
!JCADD Grassland_management
    !
    ! 13 calculate grazing by animals or cutting for forage
    !
    IF (enable_grazing) THEN
      WRITE (numout, *) 'enter the grassland management process'
        CALL Main_Grassland_Management(&
           npts, lalo, neighbours, resolution, contfrac, &
           dt_days        , &
           day_of_year    , &
           t2m_daily      , &
           t2m_min_daily  , &
           t2m_14         , &
           tsurf_daily    , &
           snow           , &
           biomass        , &
           bm_to_litter   , &
           litter         , &
           litter_avail   , &
           litter_not_avail , &
           !spitfire
           fuel_1hr(:,:,:,icarbon), &
           fuel_10hr(:,:,:,icarbon), &
           fuel_100hr(:,:,:,icarbon), &
           fuel_1000hr(:,:,:,icarbon), &
           !end spitfire
           .TRUE.         , &
           EndOfYear    , & 
!           ldrestart_read , &
!           ldrestart_write, &
!           index          , &
!           flag_cutting   , &
           when_growthinit_cut , &
           lai,sla_calc,leaf_age,leaf_frac, &
           wshtotsum,sr_ugb,compt_ugb, &
           nb_ani,grazed_frac,import_yield,N_limfert)
    ENDIF
!ENDJCADD
  !! 13. Calculate final LAI and vegetation cover
    
    ![chaoyue] veget_max_tmp is used as veget_max_old in cover SUBROUTINE
    CALL cover (npts, cn_ind, ind, biomass, &
         veget_max, veget_max_tmp, lai, &
         litter, litter_avail, litter_not_avail, carbon, & 
         fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr, &
         turnover_daily, bm_to_litter, &
         co2_to_bm, co2_fire, resp_hetero, resp_maint, resp_growth, gpp_daily, &
         deepC_a, deepC_s,deepC_p)

  !! 14. Update litter pools to account for harvest
 
    ! the whole litter stuff:
    !    litter update, lignin content, PFT parts, litter decay, 
    !    litter heterotrophic respiration, dead leaf soil cover.
    !    No vertical discretisation in the soil for litter decay.\n
    ! added by shilong for harvest
    IF(harvest_agri) THEN  !!! DO NOT activate harves_agri if ok_LAIdev
       CALL harvest(npts, dt_days, veget_max, &
            bm_to_litter, turnover_daily, &
            harvest_above)
    ENDIF

  !! 15. Land cover change

    IF(EndOfYear) THEN
      IF (use_age_class) THEN
        CALL age_class_distr(npts, lalo, resolution, bound_spa, &
             biomass, veget_max, ind, &
             lm_lastyearmax, leaf_frac, co2_to_bm, &
             fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr, &
             everywhere, litter, carbon, lignin_struc, &
             deepC_a, deepC_s, deepC_p, &
             bm_to_litter, PFTpresent, when_growthinit,&
             senescence, npp_longterm, gpp_daily, leaf_age, &
             gdd_from_growthinit, gdd_midwinter, time_hum_min, gdd_m5_dormance, &
             ncd_dormance, moiavail_month, moiavail_week, ngd_minus5, &
             gpp_week, resp_maint, resp_growth, npp_daily)
      ENDIF
    ENDIF

    IF (do_now_stomate_lcchange) THEN
      IF (use_age_class) THEN
        IF (SingleAgeClass) THEN  
          CALL gross_glcchange_SinAgeC_fh (npts, dt_days, harvest_matrix,   &
             glccSecondShift,glccPrimaryShift,glccNetLCC,&
             def_fuel_1hr_remain, def_fuel_10hr_remain,        &
             def_fuel_100hr_remain, def_fuel_1000hr_remain,    &
             deforest_litter_remain, deforest_biomass_remain,  &
             convflux, cflux_prod10, cflux_prod100,                  &
             glccReal, IncreDeficit, glcc_pft, glcc_pftmtc,          &
             veget_max, prod10, prod100, flux10, flux100,            &
             PFTpresent, senescence, moiavail_month, moiavail_week,  &
             gpp_week, ngd_minus5, resp_maint, resp_growth,          & 
             resp_hetero, npp_daily, when_growthinit, npp_longterm,  &
             ind, lm_lastyearmax, everywhere, age,                   &
             co2_to_bm, gpp_daily, co2_fire,                         &
             time_hum_min, gdd_midwinter, gdd_from_growthinit,       &
             gdd_m5_dormance, ncd_dormance,                          &
             lignin_struc, carbon, leaf_frac,                        &
             deepC_a, deepC_s, deepC_p,                              &
             leaf_age, bm_to_litter, biomass, litter,                &
             fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr)

        ! Multiple age class + wood harvest
        ELSE
          CALL gross_glcchange_fh (npts, dt_days, harvest_matrix,   &
             glccSecondShift,glccPrimaryShift,glccNetLCC,&
             def_fuel_1hr_remain, def_fuel_10hr_remain,        &
             def_fuel_100hr_remain, def_fuel_1000hr_remain,    &
             deforest_litter_remain, deforest_biomass_remain,  &
             convflux, cflux_prod10, cflux_prod100,                  &
             glccReal, IncreDeficit, glcc_pft, glcc_pftmtc,          &
             veget_max, prod10, prod100, flux10, flux100,            &
             PFTpresent, senescence, moiavail_month, moiavail_week,  &
             gpp_week, ngd_minus5, resp_maint, resp_growth,          & 
             resp_hetero, npp_daily, when_growthinit, npp_longterm,  &
             ind, lm_lastyearmax, everywhere, age,                   &
             co2_to_bm, gpp_daily, co2_fire,                         &
             time_hum_min, gdd_midwinter, gdd_from_growthinit,       &
             gdd_m5_dormance, ncd_dormance,                          &
             lignin_struc, carbon, leaf_frac,                        &
             deepC_a, deepC_s, deepC_p,                              &
             leaf_age, bm_to_litter, biomass, litter,                &
             fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr)
        ENDIF !(SingleAgeClass)

      ELSE ! .NOT. use_age_class
        IF (allow_deforest_fire) THEN
              ![chaoyue] veget_max is used as the old veget_max in lcchange_deffire
              ! veget_max_new is used as the new veget_max in lcchange_deffire
              CALL lcchange_deffire (npts, dt_days, veget_max, veget_max_new, &
                   biomass, ind, age, PFTpresent, senescence, when_growthinit, everywhere, &        
                   co2_to_bm, bm_to_litter, turnover_daily, bm_sapl, cn_ind, &
                   flux10(:,:,iwplcc),flux100(:,:,iwplcc), &
                   prod10(:,:,iwplcc),prod100(:,:,iwplcc),&
                   convflux(:,iwplcc),&
                   cflux_prod10(:,iwplcc),cflux_prod100(:,iwplcc), leaf_frac,&
                   npp_longterm, lm_lastyearmax, litter, litter_avail, litter_not_avail, &
                   carbon,&
                   deepC_a, deepC_s, deepC_p,&
                   fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr,&
                   lcc,bafrac_deforest_accu,emideforest_litter_accu,emideforest_biomass_accu,&
                   deflitsup_total,defbiosup_total)
        ELSE
              ![chaoyue] veget_max is used as veget_max_old in lcchange_main
              ! veget_max_new is used as the new veget_max in lcchange_main
              CALL lcchange_main (npts, dt_days, veget_max_new, veget_max, &
                   biomass, ind, age, PFTpresent, senescence, when_growthinit, everywhere, &
                   co2_to_bm, bm_to_litter, turnover_daily, bm_sapl, cn_ind,&
                   flux10(:,:,iwplcc),flux100(:,:,iwplcc), &
                   prod10(:,:,iwplcc),prod100(:,:,iwplcc),convflux(:,iwplcc), &
                   cflux_prod10(:,iwplcc),cflux_prod100(:,iwplcc),leaf_frac,&
                   npp_longterm, lm_lastyearmax, litter, litter_avail, litter_not_avail, &
                   carbon, &
                   deepC_a, deepC_s, deepC_p,&
                   fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr)
        ENDIF
      ENDIF ! (use_age_class)

      do_now_stomate_lcchange=.FALSE.

      ! Set the flag done_stomate_lcchange to be used in the end of sechiba_main to update the fractions.
      done_stomate_lcchange=.TRUE.

      ! [Added by chaoyue]
      ! As it's possbile people may forget to check if their land use change
      ! data (either gross or net land cover change allows the conservation of
      ! sum of veget_cov_max (i.e., equal to 1 or
      ! zero), so will adjust them here in case this happens.
      DO ipts = 1,npts
        valtmp = SUM(veget_max(ipts,:))
        IF (valtmp .GT. min_stomate) THEN
          DO j = 1,nvm
            veget_max(ipts,j) = veget_max(ipts,j)/valtmp
          ENDDO
        ELSE
          veget_max(ipts,:) = zero
        ENDIF
      ENDDO

    ENDIF ! do_now_stomate_lcchange

    !MM déplacement pour initialisation correcte des grandeurs cumulées :
    cflux_prod_total(:) = SUM(convflux(:,:) + cflux_prod10(:,:) + cflux_prod100(:,:),DIM=2)
    prod10_total(:)=SUM(SUM(prod10,dim=2),DIM=2)
    prod100_total(:)=SUM(SUM(prod100,dim=2),DIM=2)

    !! 9. Calculate vcmax 

    CALL vmax (npts, dt_days, &
         leaf_age, leaf_frac, &
         vcmax, &!)
         N_limfert)
    
  !! 16. Total heterotrophic respiration

    tot_soil_carb(:,:) = zero
    tot_litter_carb(:,:) = zero
    DO j=1,nvm

       tot_litter_carb(:,j) = tot_litter_carb(:,j) + (litter(:,istructural,j,iabove,icarbon) + &
            &          litter(:,imetabolic,j,iabove,icarbon) + &
            &          litter(:,istructural,j,ibelow,icarbon) + litter(:,imetabolic,j,ibelow,icarbon))

       tot_soil_carb(:,j) = tot_soil_carb(:,j) + (carbon(:,iactive,j) + &
            &          carbon(:,islow,j)+  carbon(:,ipassive,j))

    ENDDO
    tot_litter_soil_carb(:,:) = tot_litter_carb(:,:) + tot_soil_carb(:,:)

!!$     DO k = 1, nelements ! Loop over # elements
!!$        tot_live_biomass(:,:,k) = biomass(:,:,ileaf,k) + biomass(:,:,isapabove,k) + biomass(:,:,isapbelow,k) +&
!!$             &                    biomass(:,:,iheartabove,k) + biomass(:,:,iheartbelow,k) + &
!!$             &                    biomass(:,:,iroot,k) + biomass(:,:,ifruit,k) + biomass(:,:,icarbres,k)
!!$    END DO ! Loop over # elements

    tot_live_biomass(:,:,:) = biomass(:,:,ileaf,:) + biomass(:,:,isapabove,:) + biomass(:,:,isapbelow,:) +&
             &                    biomass(:,:,iheartabove,:) + biomass(:,:,iheartbelow,:) + &
             &                    biomass(:,:,iroot,:) + biomass(:,:,ifruit,:) + biomass(:,:,icarbres,:)


    tot_turnover(:,:,:) = turnover_daily(:,:,ileaf,:) + turnover_daily(:,:,isapabove,:) + &
         &         turnover_daily(:,:,isapbelow,:) + turnover_daily(:,:,iheartabove,:) + &
         &         turnover_daily(:,:,iheartbelow,:) + turnover_daily(:,:,iroot,:) + &
         &         turnover_daily(:,:,ifruit,:) + turnover_daily(:,:,icarbres,:)

    tot_bm_to_litter(:,:,:) = bm_to_litter(:,:,ileaf,:) + bm_to_litter(:,:,isapabove,:) +&
         &             bm_to_litter(:,:,isapbelow,:) + bm_to_litter(:,:,iheartbelow,:) +&
         &             bm_to_litter(:,:,iheartabove,:) + bm_to_litter(:,:,iroot,:) + &
         &             bm_to_litter(:,:,ifruit,:) + bm_to_litter(:,:,icarbres,:)

    carb_mass_variation(:)=-carb_mass_total(:)
    carb_mass_total(:)=SUM((tot_live_biomass(:,:,icarbon)+tot_litter_carb+tot_soil_carb)*veget_max,dim=2) + &
         &                 (prod10_total + prod100_total)
    carb_mass_variation(:)=carb_mass_total(:)+carb_mass_variation(:)
    
  !! 17. Write history

    CALL xios_orchidee_send_field("RESOLUTION_X",resolution(:,1))
    CALL xios_orchidee_send_field("RESOLUTION_Y",resolution(:,2))
    CALL xios_orchidee_send_field("CONTFRAC_STOMATE",contfrac(:))
    CALL xios_orchidee_send_field("T2M_MONTH",t2m_month)
    CALL xios_orchidee_send_field("T2M_WEEK",t2m_week)
    CALL xios_orchidee_send_field("HET_RESP",resp_hetero(:,:))
!JCADD
    CALL xios_orchidee_send_field("T2M_14",t2m_14)
!    CALL xios_orchidee_send_field("LITTER_RESP",resp_hetero_litter_d(:,:))
!    CALL xios_orchidee_send_field("ACTIVE_RESP",resp_hetero_soil_d(:,iactive,:))
!    CALL xios_orchidee_send_field("SLOW_RESP",resp_hetero_soil_d(:,islow,:))
!    CALL xios_orchidee_send_field("PASSIVE_RESP",resp_hetero_soil_d(:,ipassive,:))
    CALL xios_orchidee_send_field("LITTER_STR_AVAIL",litter_avail(:,istructural,:))
    CALL xios_orchidee_send_field("LITTER_MET_AVAIL",litter_avail(:,imetabolic,:))
    CALL xios_orchidee_send_field("LITTER_STR_NAVAIL",litter_not_avail(:,istructural,:))
    CALL xios_orchidee_send_field("LITTER_MET_NAVAIL",litter_not_avail(:,imetabolic,:))
    CALL xios_orchidee_send_field("LITTER_STR_AVAILF",litter_avail_frac(:,istructural,:))
    CALL xios_orchidee_send_field("LITTER_MET_AVAILF",litter_avail_frac(:,imetabolic,:))
    IF (ANY(ok_LAIdev)) CALL xios_orchidee_send_field("N_LIMFERT",N_limfert)
!ENDJCADD
    CALL xios_orchidee_send_field("CO2_FIRE",co2_fire)
    CALL xios_orchidee_send_field("CO2_TAKEN",co2_to_bm)
    CALL xios_orchidee_send_field("LAI",lai)
    CALL xios_orchidee_send_field("VEGET_MAX",veget_max)
    CALL xios_orchidee_send_field("NPP_STOMATE",npp_daily)
    CALL xios_orchidee_send_field("GPP",gpp_daily)
    CALL xios_orchidee_send_field("IND",ind)
    CALL xios_orchidee_send_field("CN_IND",cn_ind)
    CALL xios_orchidee_send_field("WOODMASS_IND",woodmass_ind)
    CALL xios_orchidee_send_field("TOTAL_M",tot_live_biomass)
    CALL xios_orchidee_send_field("MOISTRESS",moiavail_week)
    CALL xios_orchidee_send_field("LEAF_M",biomass(:,:,ileaf,icarbon))
    CALL xios_orchidee_send_field("SAP_M_AB",biomass(:,:,isapabove,icarbon))
    CALL xios_orchidee_send_field("SAP_M_BE",biomass(:,:,isapbelow,icarbon))
    CALL xios_orchidee_send_field("HEART_M_AB",biomass(:,:,iheartabove,icarbon))
    CALL xios_orchidee_send_field("HEART_M_BE",biomass(:,:,iheartbelow,icarbon))
    CALL xios_orchidee_send_field("ROOT_M",biomass(:,:,iroot,icarbon))
    CALL xios_orchidee_send_field("FRUIT_M",biomass(:,:,ifruit,icarbon))
    ! HERE WE ADD A OUTPUT VARIABLE, NAMED CROPYIELD 
    CALL xios_orchidee_send_field("CROPYIELD",biomass(:,:,ifruit,icarbon))
    CALL xios_orchidee_send_field("BIOMYIELD",tot_live_biomass)
    ! END DEFINE
    CALL xios_orchidee_send_field("RESERVE_M",biomass(:,:,icarbres,icarbon))
    CALL xios_orchidee_send_field("TOTAL_TURN",tot_turnover)
    CALL xios_orchidee_send_field("LEAF_TURN",turnover_daily(:,:,ileaf,icarbon))
    CALL xios_orchidee_send_field("MAINT_RESP",resp_maint)
    CALL xios_orchidee_send_field("GROWTH_RESP",resp_growth)
    CALL xios_orchidee_send_field("SAP_AB_TURN",turnover_daily(:,:,isapabove,icarbon))
    CALL xios_orchidee_send_field("ROOT_TURN",turnover_daily(:,:,iroot,icarbon))
    CALL xios_orchidee_send_field("FRUIT_TURN",turnover_daily(:,:,ifruit,icarbon))
    CALL xios_orchidee_send_field("TOTAL_BM_LITTER",tot_bm_to_litter(:,:,icarbon))
    CALL xios_orchidee_send_field("LEAF_BM_LITTER",bm_to_litter(:,:,ileaf,icarbon))
    CALL xios_orchidee_send_field("SAP_AB_BM_LITTER",bm_to_litter(:,:,isapabove,icarbon))
    CALL xios_orchidee_send_field("SAP_BE_BM_LITTER",bm_to_litter(:,:,isapbelow,icarbon))
    CALL xios_orchidee_send_field("HEART_AB_BM_LITTER",bm_to_litter(:,:,iheartabove,icarbon))
    CALL xios_orchidee_send_field("HEART_BE_BM_LITTER",bm_to_litter(:,:,iheartbelow,icarbon))
    CALL xios_orchidee_send_field("ROOT_BM_LITTER",bm_to_litter(:,:,iroot,icarbon))
    CALL xios_orchidee_send_field("FRUIT_BM_LITTER",bm_to_litter(:,:,ifruit,icarbon))
    CALL xios_orchidee_send_field("RESERVE_BM_LITTER",bm_to_litter(:,:,icarbres,icarbon))
    CALL xios_orchidee_send_field("LITTER_STR_AB",litter(:,istructural,:,iabove,icarbon))
    CALL xios_orchidee_send_field("LITTER_MET_AB",litter(:,imetabolic,:,iabove,icarbon))
    CALL xios_orchidee_send_field("LITTER_STR_BE",litter(:,istructural,:,ibelow,icarbon))
    CALL xios_orchidee_send_field("LITTER_MET_BE",litter(:,imetabolic,:,ibelow,icarbon))
    CALL xios_orchidee_send_field("DEADLEAF_COVER",deadleaf_cover)
    CALL xios_orchidee_send_field("TOTAL_SOIL_CARB",tot_litter_soil_carb)
    CALL xios_orchidee_send_field("CARBON_ACTIVE",carbon(:,iactive,:))
    CALL xios_orchidee_send_field("CARBON_SLOW",carbon(:,islow,:))
    CALL xios_orchidee_send_field("CARBON_PASSIVE",carbon(:,ipassive,:))
    CALL xios_orchidee_send_field("LITTERHUM",litterhum_daily)
    CALL xios_orchidee_send_field("TURNOVER_TIME",turnover_time)
    CALL xios_orchidee_send_field("PROD10",prod10)
    CALL xios_orchidee_send_field("FLUX10",flux10)
    CALL xios_orchidee_send_field("PROD100",prod100)
    CALL xios_orchidee_send_field("FLUX100",flux100)
    CALL xios_orchidee_send_field("CONVFLUX",convflux)
    CALL xios_orchidee_send_field("CFLUX_PROD10",cflux_prod10)
    CALL xios_orchidee_send_field("CFLUX_PROD100",cflux_prod100)
    CALL xios_orchidee_send_field("HARVEST_ABOVE",harvest_above)
    CALL xios_orchidee_send_field("VCMAX",vcmax)
    CALL xios_orchidee_send_field("AGE",age)
    CALL xios_orchidee_send_field("HEIGHT",height)
    CALL xios_orchidee_send_field("FIREINDEX",fireindex(:,:))
! ipcc history
     CALL xios_orchidee_send_field("cVeg",SUM(tot_live_biomass(:,:,icarbon)*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cLitter",SUM(tot_litter_carb*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cSoil",SUM(tot_soil_carb*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cProduct",(prod10_total + prod100_total)/1e3)
     CALL xios_orchidee_send_field("cMassVariation",carb_mass_variation/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("lai_ipcc",SUM(lai*veget_max,dim=2)*contfrac)
     CALL xios_orchidee_send_field("gpp_ipcc",SUM(gpp_daily*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("ra",SUM((resp_maint+resp_growth)*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("npp_ipcc",SUM(npp_daily*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("rh",SUM(resp_hetero*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("fFire",SUM(co2_fire*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("fHarvest",harvest_above/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("fLuc",cflux_prod_total/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("nbp",(SUM((gpp_daily-(resp_maint+resp_growth+resp_hetero)-co2_fire) &
            &        *veget_max,dim=2)-cflux_prod_total-harvest_above)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("fVegLitter",SUM((tot_bm_to_litter(:,:,icarbon) + tot_turnover(:,:,icarbon))*&
          veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("fLitterSoil",SUM(SUM(soilcarbon_input,dim=2)*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("cLeaf",SUM(biomass(:,:,ileaf,icarbon)*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cWood",SUM((biomass(:,:,isapabove,icarbon)+biomass(:,:,iheartabove,icarbon))*&
          veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cRoot",SUM(( biomass(:,:,iroot,icarbon) + biomass(:,:,isapbelow,icarbon) + &
          biomass(:,:,iheartbelow,icarbon) )*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cMisc",SUM(( biomass(:,:,icarbres,icarbon) + biomass(:,:,ifruit,icarbon))*&
          veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cLitterAbove",SUM((litter(:,istructural,:,iabove,icarbon)+&
          litter(:,imetabolic,:,iabove,icarbon))*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cLitterBelow",SUM((litter(:,istructural,:,ibelow,icarbon)+&
          litter(:,imetabolic,:,ibelow,icarbon))*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cSoilFast",SUM(carbon(:,iactive,:)*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cSoilMedium",SUM(carbon(:,islow,:)*veget_max,dim=2)/1e3*contfrac)
     CALL xios_orchidee_send_field("cSoilSlow",SUM(carbon(:,ipassive,:)*veget_max,dim=2)/1e3*contfrac)
       DO j=1,nvm
          histvar(:,j)=veget_max(:,j)*contfrac(:)*100
       ENDDO
     CALL xios_orchidee_send_field("landCoverFrac",histvar)
       vartmp(:)=zero
       DO j = 2,nvm
          IF (is_deciduous(j)) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
     CALL xios_orchidee_send_field("treeFracPrimDec",vartmp)
       vartmp(:)=zero
       DO j = 2,nvm
          IF (is_evergreen(j)) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
     CALL xios_orchidee_send_field("treeFracPrimEver",vartmp)
       vartmp(:)=zero
       DO j = 2,nvm
          IF ( .NOT.(is_c4(j)) ) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
     CALL xios_orchidee_send_field("c3PftFrac",vartmp)
       vartmp(:)=zero
       DO j = 2,nvm
          IF ( is_c4(j) ) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
     CALL xios_orchidee_send_field("c4PftFrac",vartmp)
     CALL xios_orchidee_send_field("rGrowth",SUM(resp_growth*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("rMaint",SUM(resp_maint*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("nppLeaf",SUM(bm_alloc(:,:,ileaf,icarbon)*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("nppWood",SUM(bm_alloc(:,:,isapabove,icarbon)*veget_max,dim=2)/1e3/one_day*contfrac)
     CALL xios_orchidee_send_field("nppRoot",SUM(( bm_alloc(:,:,isapbelow,icarbon) + bm_alloc(:,:,iroot,icarbon) )*&
          veget_max,dim=2)/1e3/one_day*contfrac)     


    CALL histwrite_p (hist_id_stomate, 'RESOLUTION_X', itime, &
         resolution(:,1), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'RESOLUTION_Y', itime, &
         resolution(:,2), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CONTFRAC', itime, &
         contfrac(:), npts, hori_index)

    CALL histwrite_p (hist_id_stomate, 'LITTER_STR_AB', itime, &
         litter(:,istructural,:,iabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_MET_AB', itime, &
         litter(:,imetabolic,:,iabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_STR_BE', itime, &
         litter(:,istructural,:,ibelow,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_MET_BE', itime, &
         litter(:,imetabolic,:,ibelow,icarbon), npts*nvm, horipft_index)
!spitfiretest
    !CALL histwrite (hist_id_stomate, 'fuel_1hr_met_b', itime, &
    !     fuel_1hr(:,:,imetabolic,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_1hr_str_b', itime, &
    !     fuel_1hr(:,:,istructural,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_10hr_met_b', itime, &
    !     fuel_10hr(:,:,imetabolic,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_10hr_str_b', itime, &
    !     fuel_10hr(:,:,istructural,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_100hr_met_b', itime, &
    !     fuel_100hr(:,:,imetabolic,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_100hr_str_b', itime, &
    !     fuel_100hr(:,:,istructural,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_1000hr_met_b', itime, &
    !     fuel_1000hr(:,:,imetabolic,icarbon), npts*nvm, horipft_index)
    !CALL histwrite (hist_id_stomate, 'fuel_1000hr_str_b', itime, &
    !     fuel_1000hr(:,:,istructural,icarbon), npts*nvm, horipft_index)
!endspittest

    CALL histwrite_p (hist_id_stomate, 'DEADLEAF_COVER', itime, &
         deadleaf_cover, npts, hori_index)

    CALL histwrite_p (hist_id_stomate, 'TOTAL_SOIL_CARB', itime, &
         tot_litter_soil_carb, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CARBON_ACTIVE', itime, &
         carbon(:,iactive,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CARBON_SLOW', itime, &
         carbon(:,islow,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CARBON_PASSIVE', itime, &
         carbon(:,ipassive,:), npts*nvm, horipft_index)

    CALL histwrite_p (hist_id_stomate, 'CARBON_ACTIVE_SURF', itime, &
         carbon_surf(:,iactive,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CARBON_SLOW_SURF', itime, &
         carbon_surf(:,islow,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CARBON_PASSIVE_SURF', itime, &
         carbon_surf(:,ipassive,:), npts*nvm, horipft_index)

!!!! Wetland CH4 methane
!pss:+
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_TOT_0', itime, &
                    ch4_flux_density_tot_0, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_DIF_0', itime, &
                    ch4_flux_density_dif_0, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_BUB_0', itime, &
                    ch4_flux_density_bub_0, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_PLA_0', itime, &
                    ch4_flux_density_pla_0, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_TOT_wet1', itime, &
                    ch4_flux_density_tot_wet1, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_DIF_wet1', itime, &
                    ch4_flux_density_dif_wet1, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_BUB_wet1', itime, &
                    ch4_flux_density_bub_wet1, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_PLA_wet1', itime, &
                    ch4_flux_density_pla_wet1, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_TOT_wet2', itime, &
                    ch4_flux_density_tot_wet2, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_DIF_wet2', itime, &
                    ch4_flux_density_dif_wet2, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_BUB_wet2', itime, &
                    ch4_flux_density_bub_wet2, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_PLA_wet2', itime, &
                    ch4_flux_density_pla_wet2, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_TOT_wet3', itime, &
                    ch4_flux_density_tot_wet3, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_DIF_wet3', itime, &
                    ch4_flux_density_dif_wet3, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_BUB_wet3', itime, &
                    ch4_flux_density_bub_wet3, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_PLA_wet3', itime, &
                    ch4_flux_density_pla_wet3, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_TOT_wet4', itime, &
                    ch4_flux_density_tot_wet4, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_DIF_wet4', itime, &
                    ch4_flux_density_dif_wet4, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_BUB_wet4', itime, &
                    ch4_flux_density_bub_wet4, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CH4_FLUX_PLA_wet4', itime, &
                    ch4_flux_density_pla_wet4, npts, hori_index)

    CALL histwrite_p (hist_id_stomate, 'TSURF_YEAR', itime, &
                    tsurf_year, npts, hori_index)
!pss:-


    CALL histwrite_p (hist_id_stomate, 'T2M_MONTH', itime, &
         t2m_month, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'T2M_WEEK', itime, &
         t2m_week, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TSEASON', itime, &
         Tseason, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TMIN_SPRING_TIME', itime, &
         Tmin_spring_time, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'ONSET_DATE', itime, &
         onset_date(:,:), npts*nvm, horipft_index)

    CALL histwrite_p (hist_id_stomate, 'HET_RESP', itime, &
         resp_hetero(:,:), npts*nvm, horipft_index)
! JCADD
    CALL histwrite_p(hist_id_stomate ,'T2M_14'   ,itime, &
         t2m_14, npts, hori_index)
!    CALL histwrite (hist_id_stomate, 'LITTER_RESP', itime, &
!         resp_hetero_litter_d(:,:), npts*nvm, horipft_index)
!    CALL histwrite (hist_id_stomate, 'ACTIVE_RESP', itime, &
!         resp_hetero_soil_d(:,iactive,:), npts*nvm, horipft_index)
!    CALL histwrite (hist_id_stomate, 'SLOW_RESP', itime, &
!         resp_hetero_soil_d(:,islow,:), npts*nvm, horipft_index)
!    CALL histwrite (hist_id_stomate, 'PASSIVE_RESP', itime, &
!         resp_hetero_soil_d(:,ipassive,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_STR_AVAIL', itime, &
         litter_avail(:,istructural,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_MET_AVAIL', itime, &
         litter_avail(:,imetabolic,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_STR_NAVAIL', itime, &
         litter_not_avail(:,istructural,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_MET_NAVAIL', itime, &
         litter_not_avail(:,imetabolic,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_STR_AVAILF', itime, &
         litter_avail_frac(:,istructural,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_MET_AVAILF', itime, &
         litter_avail_frac(:,imetabolic,:), npts*nvm, horipft_index)

    CALL histwrite_p (hist_id_stomate, 'N_LIMFERT', itime, &
         N_limfert, npts*nvm, horipft_index)
! ENDJCADD
    CALL histwrite_p (hist_id_stomate, 'FIREINDEX', itime, &
         fireindex(:,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LITTERHUM', itime, &
         litterhum_daily, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CO2_FIRE', itime, &
         co2_fire, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CO2_TAKEN', itime, &
         co2_to_bm, npts*nvm, horipft_index)
    ! land cover change
    CALL histwrite_p (hist_id_stomate, 'CONVFLUX_LCC', itime, &
         convflux(:,iwplcc), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CONVFLUX_HAR', itime, &
         convflux(:,iwphar), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CFLUX_PROD10_LCC', itime, &
         cflux_prod10(:,iwplcc), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CFLUX_PROD10_HAR', itime, &
         cflux_prod10(:,iwphar), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CFLUX_PROD100_LCC', itime, &
         cflux_prod100(:,iwplcc), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CFLUX_PROD100_HAR', itime, &
         cflux_prod100(:,iwphar), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'HARVEST_ABOVE', itime, &
         harvest_above, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'PROD10_LCC', itime, &
         prod10(:,:,iwplcc), npts*11, horip11_index)
    CALL histwrite_p (hist_id_stomate, 'PROD10_HAR', itime, &
         prod10(:,:,iwphar), npts*11, horip11_index)
    CALL histwrite_p (hist_id_stomate, 'PROD100_LCC', itime, &
         prod100(:,:,iwplcc), npts*101, horip101_index)
    CALL histwrite_p (hist_id_stomate, 'PROD100_HAR', itime, &
         prod100(:,:,iwphar), npts*101, horip101_index)
    CALL histwrite_p (hist_id_stomate, 'FLUX10_LCC', itime, &
         flux10(:,:,iwplcc), npts*10, horip10_index)
    CALL histwrite_p (hist_id_stomate, 'FLUX10_HAR', itime, &
         flux10(:,:,iwphar), npts*10, horip10_index)
    CALL histwrite_p (hist_id_stomate, 'FLUX100_LCC', itime, &
         flux100(:,:,iwplcc), npts*100, horip100_index)
    CALL histwrite_p (hist_id_stomate, 'FLUX100_HAR', itime, &
         flux100(:,:,iwphar), npts*100, horip100_index)
    CALL histwrite_p (hist_id_stomate, 'DefLitSurplus', itime, &
         deflitsup_total, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'DefBioSurplus', itime, &
         defbiosup_total, npts*nvm, horipft_index)

    IF (use_bound_spa) THEN
      CALL histwrite_p (hist_id_stomate, 'bound_spa', itime, &
         bound_spa, npts*nvm, horipft_index)
    ENDIF

    IF (do_now_stomate_lcchange) THEN
        CALL histwrite_p (hist_id_stomate, 'LCC', itime, &
             lcc, npts*nvm, horipft_index)
    ENDIF

    CALL histwrite_p (hist_id_stomate, 'LAI', itime, &
         lai, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'FPC_MAX', itime, &
         fpc_max, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'MAXFPC_LASTYEAR', itime, &
         maxfpc_lastyear, npts*nvm, horipft_index) 
    CALL histwrite_p (hist_id_stomate, 'VEGET_MAX', itime, &
         veget_max, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'NPP', itime, &
         npp_daily, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'GPP', itime, &
         gpp_daily, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'IND', itime, &
         ind, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CN_IND', itime, &
         cn_ind, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'WOODMASS_IND', itime, &
         woodmass_ind, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'TOTAL_M', itime, &
         tot_live_biomass(:,:,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LEAF_M', itime, &
         biomass(:,:,ileaf,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'SAP_M_AB', itime, &
         biomass(:,:,isapabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'SAP_M_BE', itime, &
         biomass(:,:,isapbelow,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'HEART_M_AB', itime, &
         biomass(:,:,iheartabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'HEART_M_BE', itime, &
         biomass(:,:,iheartbelow,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'ROOT_M', itime, &
         biomass(:,:,iroot,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'FRUIT_M', itime, &
         biomass(:,:,ifruit,icarbon), npts*nvm, horipft_index)
!!!!! crop variables
    CALL histwrite_p (hist_id_stomate, 'CROPYIELD', itime, &
         biomass(:,:,ifruit,icarbon), npts*nvm, horipft_index)

    CALL histwrite_p (hist_id_stomate, 'BIOMYIELD', itime, &
         biomass(:,:,ileaf,icarbon)+biomass(:,:,isapabove,icarbon) &
        +biomass(:,:,ifruit,icarbon)+biomass(:,:,icarbres,icarbon), npts*nvm,horipft_index)

    CALL histwrite_p (hist_id_stomate, 'CROP_EXPORT', itime, &
         crop_export, npts*nvm, horipft_index)
!!!!! end crop variables, xuhui
    CALL histwrite_p (hist_id_stomate, 'RESERVE_M', itime, &
         biomass(:,:,icarbres,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'TOTAL_TURN', itime, &
         tot_turnover(:,:,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LEAF_TURN', itime, &
         turnover_daily(:,:,ileaf,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'SAP_AB_TURN', itime, &
         turnover_daily(:,:,isapabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'ROOT_TURN', itime, &
         turnover_daily(:,:,iroot,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'FRUIT_TURN', itime, &
         turnover_daily(:,:,ifruit,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'TOTAL_BM_LITTER', itime, &
         tot_bm_to_litter(:,:,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'LEAF_BM_LITTER', itime, &
         bm_to_litter(:,:,ileaf,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'SAP_AB_BM_LITTER', itime, &
         bm_to_litter(:,:,isapabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'SAP_BE_BM_LITTER', itime, &
         bm_to_litter(:,:,isapbelow,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'HEART_AB_BM_LITTER', itime, &
         bm_to_litter(:,:,iheartabove,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'HEART_BE_BM_LITTER', itime, &
         bm_to_litter(:,:,iheartbelow,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'ROOT_BM_LITTER', itime, &
         bm_to_litter(:,:,iroot,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'FRUIT_BM_LITTER', itime, &
         bm_to_litter(:,:,ifruit,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'RESERVE_BM_LITTER', itime, &
         bm_to_litter(:,:,icarbres,icarbon), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'MAINT_RESP', itime, &
         resp_maint, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'GROWTH_RESP', itime, &
         resp_growth, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AGE', itime, &
         age, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'HEIGHT', itime, &
         height, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'MOISTRESS', itime, &
         moiavail_week, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'VCMAX', itime, &
         vcmax, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'TURNOVER_TIME', itime, &
         turnover_time, npts*nvm, horipft_index)
!!DZADD
!    CALL histwrite_p (hist_id_stomate, 'LEAF_FRAC1', itime, leaf_frac(:,:,1), npts*nvm, horipft_index)
!    CALL histwrite_p (hist_id_stomate, 'LEAF_FRAC2', itime, leaf_frac(:,:,2), npts*nvm, horipft_index)
!    CALL histwrite_p (hist_id_stomate, 'LEAF_FRAC3', itime, leaf_frac(:,:,3), npts*nvm, horipft_index)
!    CALL histwrite_p (hist_id_stomate, 'LEAF_FRAC4', itime, leaf_frac(:,:,4), npts*nvm, horipft_index)
!!ENDDZADD

    IF ( hist_id_stomate_IPCC > 0 ) THEN
       vartmp(:)=SUM(tot_live_biomass(:,:,icarbon)*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cVeg", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(tot_litter_carb*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cLitter", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(tot_soil_carb*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cSoil", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=(prod10_total + prod100_total)/1e3
       CALL histwrite_p (hist_id_stomate_IPCC, "cProduct", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=carb_mass_variation/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cMassVariation", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(lai*veget_max,dim=2)*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "lai", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(gpp_daily*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "gpp", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM((resp_maint+resp_growth)*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "ra", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(npp_daily*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "npp", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(resp_hetero*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "rh", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(co2_fire*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "fFire", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=harvest_above/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "fHarvest", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=cflux_prod_total/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "fLuc", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=(SUM((gpp_daily-(resp_maint+resp_growth+resp_hetero)-co2_fire) &
            &        *veget_max,dim=2)-cflux_prod_total-harvest_above)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "nbp", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM((tot_bm_to_litter(:,:,icarbon) + tot_turnover(:,:,icarbon))*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "fVegLitter", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(SUM(soilcarbon_input,dim=2)*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "fLitterSoil", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(biomass(:,:,ileaf,icarbon)*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cLeaf", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM((biomass(:,:,isapabove,icarbon)+biomass(:,:,iheartabove,icarbon))*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cWood", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(( biomass(:,:,iroot,icarbon) + biomass(:,:,isapbelow,icarbon) + biomass(:,:,iheartbelow,icarbon) ) &
            &        *veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cRoot", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(( biomass(:,:,icarbres,icarbon) + biomass(:,:,ifruit,icarbon))*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cMisc", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM((litter(:,istructural,:,iabove,icarbon)+litter(:,imetabolic,:,iabove,icarbon))*&
            veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cLitterAbove", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM((litter(:,istructural,:,ibelow,icarbon)+litter(:,imetabolic,:,ibelow,icarbon))*&
            veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cLitterBelow", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(carbon(:,iactive,:)*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cSoilFast", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(carbon(:,islow,:)*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cSoilMedium", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(carbon(:,ipassive,:)*veget_max,dim=2)/1e3*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "cSoilSlow", itime, &
            vartmp, npts, hori_index)
       DO j=1,nvm
          histvar(:,j)=veget_max(:,j)*contfrac(:)*100
       ENDDO
       CALL histwrite_p (hist_id_stomate_IPCC, "landCoverFrac", itime, &
            histvar, npts*nvm, horipft_index)
       !-
       vartmp(:)=zero
       DO j = 2,nvm
          IF (is_deciduous(j)) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
       CALL histwrite_p (hist_id_stomate_IPCC, "treeFracPrimDec", itime, &
            vartmp, npts, hori_index)
       !-
       vartmp(:)=zero
       DO j = 2,nvm
          IF (is_evergreen(j)) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
       CALL histwrite_p (hist_id_stomate_IPCC, "treeFracPrimEver", itime, &
            vartmp, npts, hori_index)
       !-
       vartmp(:)=zero
       DO j = 2,nvm
          IF ( .NOT.(is_c4(j)) ) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
       CALL histwrite_p (hist_id_stomate_IPCC, "c3PftFrac", itime, &
            vartmp, npts, hori_index)
       !-
       vartmp(:)=zero
       DO j = 2,nvm
          IF ( is_c4(j) ) THEN
             vartmp(:) = vartmp(:) + veget_max(:,j)*contfrac*100
          ENDIF
       ENDDO
       CALL histwrite_p (hist_id_stomate_IPCC, "c4PftFrac", itime, &
            vartmp, npts, hori_index)
       !-
       vartmp(:)=SUM(resp_growth*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "rGrowth", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(resp_maint*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "rMaint", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(bm_alloc(:,:,ileaf,icarbon)*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "nppLeaf", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(bm_alloc(:,:,isapabove,icarbon)*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "nppWood", itime, &
            vartmp, npts, hori_index)
       vartmp(:)=SUM(( bm_alloc(:,:,isapbelow,icarbon) + bm_alloc(:,:,iroot,icarbon) )*veget_max,dim=2)/1e3/one_day*contfrac
       CALL histwrite_p (hist_id_stomate_IPCC, "nppRoot", itime, &
            vartmp, npts, hori_index)

       CALL histwrite_p (hist_id_stomate_IPCC, 'RESOLUTION_X', itime, &
            resolution(:,1), npts, hori_index)
       CALL histwrite_p (hist_id_stomate_IPCC, 'RESOLUTION_Y', itime, &
            resolution(:,2), npts, hori_index)
       CALL histwrite_p (hist_id_stomate_IPCC, 'CONTFRAC', itime, &
            contfrac(:), npts, hori_index)

    ENDIF

    IF (printlev>=4) WRITE(numout,*) 'Leaving stomate_lpj'

  END SUBROUTINE StomateLpj


!! ================================================================================================================================
!! SUBROUTINE   : harvest
!!
!>\BRIEF        Harvest of croplands
!!
!! DESCRIPTION  : To take into account biomass harvest from crop (mainly to take 
!! into account for the reduced litter input and then decreased soil carbon. it is a 
!! constant (40\%) fraction of above ground biomass.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): ::harvest_above the harvested biomass
!!
!! REFERENCE(S) :
!! - Piao, S., P. Ciais, P. Friedlingstein, N. de Noblet-Ducoudre, P. Cadule, N. Viovy, and T. Wang. 2009. 
!!   Spatiotemporal patterns of terrestrial carbon cycle during the 20th century. Global Biogeochemical 
!!   Cycles 23:doi:10.1029/2008GB003339.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE harvest(npts, dt_days, veget_max, &
       bm_to_litter, turnover_daily, &
       harvest_above)

  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER, INTENT(in)                                    :: npts            !! Domain size (unitless) 
    REAL(r_std), INTENT(in)                                :: dt_days         !! Time step (days)                               
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: veget_max       !! new "maximal" coverage fraction of a PFT (LAI -> 
                                                                              !! infinity) on ground @tex $(m^2 m^{-2})$ @endtex 
    
   !! 0.2 Output variables
   
   !! 0.3 Modified variables

    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: bm_to_litter !! [DISPENSABLE] conversion of biomass to litter 
                                                                                     !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: turnover_daily   !! Turnover rates 
                                                                               !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts), INTENT(inout)            :: harvest_above    !! harvest above ground biomass for agriculture 
                                                                               !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    !! 0.4 Local variables

    INTEGER(i_std)                                         :: i, j, k, l, m    !! indices                       
    REAL(r_std)                                            :: above_old        !! biomass of previous time step 
                                                                               !! @tex $(gC m^{-2})$ @endtex 
!_ ================================================================================================================================

  !! 1. Yearly initialisation

    above_old             = zero
    harvest_above         = zero

    DO i = 1, npts
       DO j = 1,nvm
          IF (.NOT. natural(j)) THEN
             above_old = turnover_daily(i,j,ileaf,icarbon) + turnover_daily(i,j,isapabove,icarbon) + &
                  &       turnover_daily(i,j,iheartabove,icarbon) + turnover_daily(i,j,ifruit,icarbon) + &
                  &       turnover_daily(i,j,icarbres,icarbon) + turnover_daily(i,j,isapbelow,icarbon) + &
                  &       turnover_daily(i,j,iheartbelow,icarbon) + turnover_daily(i,j,iroot,icarbon)

             turnover_daily(i,j,ileaf,icarbon) = turnover_daily(i,j,ileaf,icarbon)*frac_turnover_daily
             turnover_daily(i,j,isapabove,icarbon) = turnover_daily(i,j,isapabove,icarbon)*frac_turnover_daily
             turnover_daily(i,j,isapbelow,icarbon) = turnover_daily(i,j,isapbelow,icarbon)*frac_turnover_daily
             turnover_daily(i,j,iheartabove,icarbon) = turnover_daily(i,j,iheartabove,icarbon)*frac_turnover_daily
             turnover_daily(i,j,iheartbelow,icarbon) = turnover_daily(i,j,iheartbelow,icarbon)*frac_turnover_daily
             turnover_daily(i,j,iroot,icarbon) = turnover_daily(i,j,iroot,icarbon)*frac_turnover_daily
             turnover_daily(i,j,ifruit,icarbon) = turnover_daily(i,j,ifruit,icarbon)*frac_turnover_daily
             turnover_daily(i,j,icarbres,icarbon) = turnover_daily(i,j,icarbres,icarbon)*frac_turnover_daily
             harvest_above(i)  = harvest_above(i) + veget_max(i,j) * above_old *(un - frac_turnover_daily)
          ENDIF
       ENDDO
    ENDDO

!!$    harvest_above = harvest_above
  END SUBROUTINE harvest
END MODULE stomate_lpj
