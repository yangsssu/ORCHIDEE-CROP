!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/stomate_io.f90 $ 
!< $Date: 2016-05-09 17:27:44 +0200 (Mon, 09 May 2016) $
!< $Author: xuhui.wang $
!< $Revision: 3419 $
! IPSL (2006)
!  This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
MODULE stomate_io
  !---------------------------------------------------------------------
  !- Not all variables saved in the start files are absolutely necessary.
  !- However, Sechiba's and Stomate's PFTs are not necessarily identical,
  !- and for that case this information needs to be saved.
  !---------------------------------------------------------------------
  USE stomate_data
  USE constantes
  USE constantes_soil
  USE mod_orchidee_para
  USE ioipsl_para 
  !-
  IMPLICIT NONE
  !-
  PRIVATE
  PUBLIC readstart, writerestart, get_reftemp_clear
  !-
  ! first call?
  !-
  LOGICAL,SAVE :: firstcall_io = .TRUE.
!$OMP THREADPRIVATE(firstcall_io)
  !-
  ! reference temperature (K)
  !-
  REAL(r_std),ALLOCATABLE,DIMENSION(:),SAVE :: trefe
!$OMP THREADPRIVATE(trefe)
  !-
CONTAINS
  !-
  !===
  !-
  SUBROUTINE readstart &
       & (npts, index, lalo, resolution, t2m, dt_days, date, &
       &  ind, adapted, regenerate, moiavail_daily, gdd_init_date, litterhum_daily, &
       &  t2m_daily, t2m_min_daily, &
       !spitfire
       &  t2m_max_daily, wspeed_daily,&
       !endspit
       &  tsurf_daily, tsoil_daily, &
       &  soilhum_daily, precip_daily, gpp_daily, npp_daily, &
       &  turnover_daily, moiavail_month, moiavail_week,&
       &  t2m_longterm, tau_longterm, t2m_month, t2m_week,&
       &  tsoil_month, soilhum_month, fireindex, firelitter,  &
       &  maxmoiavail_lastyear, maxmoiavail_thisyear, &
       &  minmoiavail_lastyear, minmoiavail_thisyear, &
       &  maxgppweek_lastyear, maxgppweek_thisyear, &
       &  gdd0_lastyear, gdd0_thisyear, precip_lastyear, precip_thisyear, &
       &  gdd_m5_dormance,  gdd_from_growthinit, gdd_midwinter, ncd_dormance, ngd_minus5, &
       &  PFTpresent, npp_longterm, lm_lastyearmax, lm_thisyearmax, &
       &  maxfpc_lastyear, maxfpc_thisyear, &
       &  turnover_longterm, gpp_week, biomass, resp_maint_part, &
       &  leaf_age, leaf_frac, senescence, when_growthinit, age, &
       &  resp_hetero, resp_maint, resp_growth, co2_fire, co2_to_bm_dgvm, &
       &  veget_lastlight, everywhere, need_adjacent, RIP_time, &
       &  time_hum_min, hum_min_dormance, &
       &  litterpart, litter, dead_leaves, &
       &  carbon, lignin_struc,&
       !spitfire
       &  ni_acc,fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr, & 
       !endspit
       & turnover_time, &
       &  prod10,prod100,flux10, flux100, &
       &  convflux, cflux_prod10, cflux_prod100, bm_to_litter, carb_mass_total,&
       &  Tseason, Tseason_length, Tseason_tmp, &
       &  Tmin_spring_time, begin_leaves, onset_date, &
       &  global_years, ok_equilibrium, nbp_accu, nbp_flux, &
       &  MatrixV, VectorU, previous_stock, current_stock,&
!!!!! crop
       !readstart for variables driving STICS
       & f_crop_recycle, in_cycle, f_sen_lai, st2m_max_daily, wut_cm_daily, wus_cm_daily, evapot_daily, pdbiomass, pdmasec, &
       & masecveg, masec, dltams, gdh_daily, phoi, onarretesomcourdrp,  &
       & nsendltams, nsendltai, nsenpfeuilverte, nsendurvie, nsenndurvie, densiteequiv, &
       & nplt, tursla, ssla, pfeuilverte, bsenlai, &
       & zrac, nrec, nlan, tcult, udevair, udevcult, ndrp, rfvi, nlev, nger, etatvernal, &
       & caljvc, rfpi, upvt, utp, somcour, somcourdrp, somcourutp, tdevelop, somtemp, &
       & somcourfauche, stpltger, R_stamflax, R_stlaxsen, R_stsenlan, stlevflo, nflo, &
       & R_stlevdrp, R_stflodrp, R_stdrpmat, nmat, nlax, nrecbutoir, group, ndebdes, R_stdrpdes, densite, &
       & densitelev, coeflev, densiteger, somelong, somger, humectation, nbjhumec, &
       & somtemphumec, stpltlev, namf, stmatrec, tustress, lai, somfeuille, pdlai, &
       & nbfeuille, reajust, ulai, pdulai, efdensite, tempeff, nstopfeuille, deltai, vmax, nsen, &
       & laisen, pdlaisen, dltaisenat, nsencour, dltamsen, dltaisen, fgellev, &
       & gelee, fstressgel, R_stlevamf, dernier_n, durvieI, durvie, ndebsen, somsenreste, &
       & humrel, swfac, turfac, senfac, mafeuiljaune, msneojaune,&
       & v_dltams, fgelflo, pdircarb, ircarb, nbgrains, pgrain, vitmoy, nbgraingel, pgraingel, &
       & dltags, ftempremp, magrain, pdmagrain, nbj0remp, pdsfruittot, repracmax, repracmin, &
       & kreprac, somtemprac, urac, reprac, nstoprac, c_reserve, c_leafb, gslen, drylen, &
       & nboxmax, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem,box_durage, box_somsenbase, &
!!!!! end crop, xuhui
       &  deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil, O2_snow, CH4_snow, &
       &  thawed_humidity, depth_organic_soil, altmax, fixed_cryoturbation_depth, & !pss+
       &  uo_0, uold2_0, uo_wet1, uold2_wet1, uo_wet2, uold2_wet2, uo_wet3, uold2_wet3, &
       &  uo_wet4, uold2_wet4, tsurf_year, &!) !pss-
!JCADD
       &  wshtotsum, sr_ugb, sla_calc, nb_ani, grazed_frac, &
       &  import_yield, t2m_14, litter_not_avail, assim_param)
!ENDJCADD
    !---------------------------------------------------------------------
    !- read start file
    !---------------------------------------------------------------------
    !-
    ! 0 declarations
    !-
    ! 0.1 input
    !-
    ! Domain size
    INTEGER(i_std),INTENT(in) :: npts
    ! Indices of the points on the map
    INTEGER(i_std),DIMENSION(npts),INTENT(in) :: index
    ! Geogr. coordinates (latitude,longitude) (degrees)
    REAL(r_std),DIMENSION(npts,2),INTENT(in) :: lalo
    ! size in x an y of the grid (m)
    REAL(r_std),DIMENSION(npts,2),INTENT(in) :: resolution
    REAL(r_std),DIMENSION(npts),INTENT(in)   :: t2m                !! 2 m air temperature from forcing file or coupled model (K)
    !-
    ! 0.2 output
    !-
    ! time step of STOMATE in days
    REAL(r_std),INTENT(out) :: dt_days
    ! date (d)
    INTEGER(i_std),INTENT(out) :: date
    ! density of individuals (1/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: ind
    ! Winter too cold? between 0 and 1
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: adapted
    ! Winter sufficiently cold? between 0 and 1
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: regenerate
    ! daily moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: moiavail_daily
    ! date for beginning of gdd count
    REAL(r_std),DIMENSION(npts,2),INTENT(out) :: gdd_init_date
    ! daily litter humidity
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: litterhum_daily
    ! daily 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: t2m_daily
    ! daily minimum 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: t2m_min_daily
    !spitfire
    ! daily maximum 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out) :: t2m_max_daily
    ! daily wind speed(m/s)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: wspeed_daily
    !endspit
    ! daily surface temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: tsurf_daily
    ! daily soil temperatures (K)
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(out) :: tsoil_daily
    ! daily soil humidity
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(out) :: soilhum_daily
    ! daily precipitations (mm/day) (for phenology)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: precip_daily
    ! daily gross primary productivity (gC/m**2/day)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: gpp_daily
    ! daily net primary productivity (gC/m**2/day)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: npp_daily
    ! daily turnover rates (gC/m**2/day)
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(out) :: turnover_daily
    ! "monthly" moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: moiavail_month
    ! "weekly" moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: moiavail_week
    ! "long term" 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: t2m_longterm
    ! "tau_longterm"
    REAL(r_std), INTENT(out)        :: tau_longterm
    ! "monthly" 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: t2m_month
    ! "seasonal" 2 meter temperatures (K) 
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: Tseason
    ! temporary variable to calculate Tseason
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: Tseason_length
    ! temporary variable to calculate Tseason
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: Tseason_tmp

    REAL(r_std),DIMENSION(npts,nvm),INTENT(out)  :: Tmin_spring_time
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out)  :: onset_date
    LOGICAL,DIMENSION(npts,nvm),INTENT(out)      :: begin_leaves

    ! "weekly" 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: t2m_week
    ! "monthly" soil temperatures (K)
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(out) :: tsoil_month
    ! "monthly" soil humidity
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(out) :: soilhum_month
    ! Probability of fire
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: fireindex
    ! Longer term total litter above the ground, gC/m**2 of ground
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: firelitter
    ! last year's maximum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: maxmoiavail_lastyear
    ! this year's maximum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: maxmoiavail_thisyear
    ! last year's minimum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: minmoiavail_lastyear
    ! this year's minimum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: minmoiavail_thisyear
    ! last year's maximum weekly GPP
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: maxgppweek_lastyear
    ! this year's maximum weekly GPP
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: maxgppweek_thisyear
    ! last year's annual GDD0
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: gdd0_lastyear
    ! this year's annual GDD0
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: gdd0_thisyear
    ! last year's annual precipitation (mm/year)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: precip_lastyear
    ! this year's annual precipitation (mm/year)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: precip_thisyear
    ! growing degree days, threshold -5 deg C (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: gdd_m5_dormance
    ! growing degree days, from begin of season
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: gdd_from_growthinit
    ! growing degree days since midwinter (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: gdd_midwinter
    ! number of chilling days since leaves were lost (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: ncd_dormance
    ! number of growing days, threshold -5 deg C (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: ngd_minus5
    ! PFT exists (equivalent to fpc_max > 0 for natural PFTs)
    LOGICAL,DIMENSION(npts,nvm),INTENT(out)    :: PFTpresent
    ! "long term" net primary productivity (gC/m**2/year)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: npp_longterm
    ! last year's maximum leaf mass, for each PFT (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: lm_lastyearmax
    ! this year's maximum leaf mass, for each PFT (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: lm_thisyearmax
    ! last year's maximum fpc for each natural PFT, on ground
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: maxfpc_lastyear
    ! this year's maximum fpc for each PFT,
    ! on *total* ground (see stomate_season)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: maxfpc_thisyear
    ! "long term" turnover rate (gC/m**2/year)
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(out) :: turnover_longterm
    ! "weekly" GPP (gC/day/(m**2 covered)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: gpp_week
    ! biomass (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(out) :: biomass
    ! maintenance resp (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm,nparts),INTENT(out) :: resp_maint_part
    ! leaf age (days)
    REAL(r_std),DIMENSION(npts,nvm,nleafages),INTENT(out) :: leaf_age
    ! fraction of leaves in leaf age class
    REAL(r_std),DIMENSION(npts,nvm,nleafages),INTENT(out) :: leaf_frac
    ! is the plant senescent ? 
    !(only for deciduous trees - carbohydrate reserve)
    LOGICAL,DIMENSION(npts,nvm),INTENT(out) :: senescence
    ! how many days ago was the beginning of the growing season
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: when_growthinit
    ! mean age (years)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: age
    ! heterotrophic respiration (gC/day/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: resp_hetero
    ! maintenance respiration (gC/day/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: resp_maint
    ! growth respiration (gC/day/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: resp_growth
    ! carbon emitted into the atmosphere by fire (living and dead biomass)
    ! (in gC/m**2/time step)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: co2_fire
    ! biomass uptaken (gC/(m**2 of total ground)/day)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: co2_to_bm_dgvm
    ! vegetation fractions (on ground) after last light competition
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: veget_lastlight
    ! is the PFT everywhere in the grid box or very localized
    ! (after its introduction)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: everywhere
    ! in order for this PFT to be introduced,
    ! does it have to be present in an adjacent grid box?
    LOGICAL,DIMENSION(npts,nvm),INTENT(out) :: need_adjacent
    ! How much time ago was the PFT eliminated for the last time (y)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: RIP_time
    ! time elapsed since strongest moisture availability (d)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: time_hum_min
    ! minimum moisture during dormance
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: hum_min_dormance
    ! fraction of litter above the ground belonging to different PFTs
    ! separated for natural and agricultural PFTs.
    REAL(r_std),DIMENSION(npts,nvm,nlitt),INTENT(out) :: litterpart
    ! metabolic and structural litter, natural and agricultural,
    ! above and below ground (gC/m**2)
    REAL(r_std),DIMENSION(npts,nlitt,nvm,nlevs,nelements),INTENT(out):: litter
    ! dead leaves on ground, per PFT, metabolic and structural,
    ! in gC/(m**2 of ground)
    REAL(r_std),DIMENSION(npts,nvm,nlitt),INTENT(out) :: dead_leaves
    ! carbon pool: active, slow, or passive, (gC/m**2)
    REAL(r_std),DIMENSION(npts,ncarb,nvm),INTENT(out) :: carbon
    !spitfire
    ! Nesterov index accumulated
    REAL(r_std), DIMENSION(npts), INTENT(out)                    :: ni_acc
    ! fuel classes (1, 10, 100, 1000 hours)
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(out)               :: fuel_1hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(out)               :: fuel_10hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(out)               :: fuel_100hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(out)               :: fuel_1000hr
    !endspit
    ! ratio Lignine/Carbon in structural litter, above and below ground,(gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm,nlevs),INTENT(out) :: lignin_struc
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out) :: turnover_time

    ! For Spinup matrix resolution
    INTEGER(i_std), INTENT(out) :: global_years   
    LOGICAL, DIMENSION(npts), INTENT(out) :: ok_equilibrium
    REAL(r_std), DIMENSION(npts), INTENT(out) :: nbp_accu  !! Accumulated Net Biospheric Production over the year
    REAL(r_std), DIMENSION(npts), INTENT(out) :: nbp_flux  !! Net Biospheric Production over the year
    !-
    REAL(r_std), DIMENSION(npts,nvm,nbpools,nbpools), INTENT(out) :: MatrixV
    REAL(r_std), DIMENSION(npts,nvm,nbpools), INTENT(out) :: VectorU
    REAL(r_std), DIMENSION(npts,nvm,nbpools), INTENT(out) :: previous_stock
    REAL(r_std), DIMENSION(npts,nvm,nbpools), INTENT(out) :: current_stock    
    REAL(r_std), DIMENSION(npts,nvm,npco2),   INTENT(out) :: assim_param

    !-
    REAL(r_std), DIMENSION(npts,ndeep,nvm),INTENT(inout) :: deepC_a
    REAL(r_std), DIMENSION(npts,ndeep,nvm),INTENT(inout) :: deepC_s
    REAL(r_std), DIMENSION(npts,ndeep,nvm),INTENT(inout) :: deepC_p
    REAL(r_std), DIMENSION(npts,ndeep,nvm),INTENT(inout) :: O2_soil
    REAL(r_std), DIMENSION(npts,ndeep,nvm),INTENT(inout) :: CH4_soil
    REAL(r_std), DIMENSION(npts,nsnow,nvm),INTENT(inout) :: O2_snow
    REAL(r_std), DIMENSION(npts,nsnow,nvm),INTENT(inout) :: CH4_snow
    REAL(r_std), DIMENSION(npts),INTENT(inout)           :: thawed_humidity
    REAL(r_std), DIMENSION(npts),INTENT(inout)           :: depth_organic_soil
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: altmax     !! Active layer thickness (m) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: fixed_cryoturbation_depth !! Depth to hold cryoturbation to for fixed runs 

    !pss:+
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out) :: uo_0
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out)  :: uold2_0
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out) :: uo_wet1
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out)  :: uold2_wet1
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out) :: uo_wet2
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out)  :: uold2_wet2
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out) :: uo_wet3
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out)  :: uold2_wet3
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out) :: uo_wet4
    REAL(r_std),DIMENSION(npts,nvert),INTENT(out)  :: uold2_wet4
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: tsurf_year
    !pss:-



    !-
    ! 0.3 not necessarily output
    !-
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out)    ::  sla_calc
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out)    ::  wshtotsum
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)  ::  sr_ugb
!    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)  ::  compt_ugb
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out)    ::  nb_ani
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)  ::  grazed_frac
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)  ::  import_yield
    REAL(r_std),DIMENSION(npts),INTENT(out)        ::  t2m_14
    REAL(r_std), DIMENSION(npts,nlitt,nvm), INTENT(out)  ::  litter_not_avail
!ENDJCADD
    !-
    ! 0.4 local
    !-
    ! date, real
    REAL(r_std) :: date_real
    ! PFT exists (equivalent to fpc_max > 0 for natural PFTs), real
    REAL(r_std),DIMENSION(npts,nvm) :: PFTpresent_real
    ! is the plant senescent ?
    ! (only for deciduous trees - carbohydrate reserve), real
    REAL(r_std),DIMENSION(npts,nvm) :: senescence_real

    REAL(r_std),DIMENSION(npts,nvm) :: begin_leaves_real
    ! in order for this PFT to be introduced,
    ! does it have to be present in an adjacent grid box? - real
    REAL(r_std),DIMENSION(npts,nvm) :: need_adjacent_real
    REAL(r_std), DIMENSION(1) :: vartmp  !! temporary variable because restget/restput needs an array and not a scalar
    ! To store variables names for I/O
    CHARACTER(LEN=80) :: var_name
    ! string suffix indicating an index
    CHARACTER(LEN=10) :: part_str
    ! string suffix indicating biomass type for senecense
    CHARACTER(LEN=10) :: box_str
    ! string suffix indicating litter type
    CHARACTER(LEN=3),DIMENSION(nlitt) :: litter_str
    ! string suffix indicating level
    CHARACTER(LEN=2),DIMENSION(nlevs) :: level_str
    ! temporary storage
    REAL(r_std),DIMENSION(1) :: xtmp
    ! index
    INTEGER(i_std) :: j,k,l,m,h
    ! reference temperature (K)

    CHARACTER(LEN=1),DIMENSION(nelements) :: element_str   !! string suffix indicating element
    REAL(r_std), DIMENSION(1) :: temp_global_years
    CHARACTER(LEN=6), DIMENSION(nbpools) :: pools_str
    REAL(r_std), DIMENSION(npts) :: ok_equilibrium_real    
    CHARACTER(LEN=80) :: filename   !! filename for reference temerature file
    ! Permafrost carbon processes
    LOGICAL :: read_input_deepC_a
    LOGICAL :: read_input_deepC_s
    LOGICAL :: read_input_deepC_p
    LOGICAL :: read_input_thawed_humidity
    LOGICAL :: read_input_depth_organic_soil
    real(r_std) :: deepC_a_init, deepC_s_init, deepC_p_init
    real(r_std) :: thawed_humidity_input = 0.5
    LOGICAL ::  reset_thawed_humidity = .FALSE.

    ! Wetland CH4
    INTEGER(i_std) :: nivo !pss:+-
    REAL(r_std) :: CH4atmo_CONC !pss:+-

!!!!! crops
    REAL(r_std), DIMENSION(npts, nvm)                                 :: in_cycle_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: f_sen_lai_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: f_crop_recycle_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: onarretesomcourdrp_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: humectation_real        
    !REAL(r_std), DIMENSION(nvm)                                       :: codeulaivernal_real        
    
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nplt_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nrec_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nlan_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: ndrp_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nlev_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nger_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: etatvernal_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nflo_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nmat_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nlax_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nrecbutoir_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: ndebdes_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nbjhumec_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: namf_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nbfeuille_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nstopfeuille_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nsen_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nsencour_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: gelee_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: dernier_n_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: ndebsen_real  
   
    ! carbon allocation local variables
    
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nbj0remp_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nstoprac_real  

    ! growing season local variables 
    
    REAL(r_std), DIMENSION(npts, nvm)                                 :: gslen_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: drylen_real  
!!!!! end crops, xuhui

    ! land cover change variables 
    ! products remaining in the 10/100 year-turnover pool after the annual release for each compartment
    ! (10 or 100 + 1 : input from year of land cover change)
    REAL(r_std),DIMENSION(npts,0:10,nwp),INTENT(out)                           :: prod10
    REAL(r_std),DIMENSION(npts,0:100,nwp),INTENT(out)                          :: prod100
    ! annual release from the 10/100 year-turnover pool compartments
    REAL(r_std),DIMENSION(npts,10,nwp),INTENT(out)                           :: flux10
    REAL(r_std),DIMENSION(npts,100,nwp),INTENT(out)                          :: flux100
    REAL(r_std), DIMENSION(npts,nwp), INTENT(out)                            :: convflux
    REAL(r_std), DIMENSION(npts,nwp), INTENT(out)                            :: cflux_prod10
    REAL(r_std), DIMENSION(npts,nwp), INTENT(out)                            :: cflux_prod100
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(out)         :: bm_to_litter
    REAL(r_std),DIMENSION(npts),INTENT(out)                              :: carb_mass_total
    REAL(r_std),DIMENSION(npts,nvm)                                      :: vcmax_tmp
!!!!! crops

    !LOGICAL, INTENT(OUT)       :: f_crop_init 
    LOGICAL, DIMENSION(npts, nvm), INTENT(OUT)       :: f_crop_recycle 
    LOGICAL, DIMENSION(npts, nvm), INTENT(OUT)       :: in_cycle 
    LOGICAL, DIMENSION(npts, nvm), INTENT(OUT)       :: f_sen_lai 
    ! daily maximum 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(OUT)      :: st2m_max_daily
    ! daily value of soil temperature at the resolution of 1 cm, the second dimension is 3
    ! the three layers around sowing layer
    REAL(r_std), DIMENSION(npts, nvm, 3), INTENT(OUT)    :: wut_cm_daily
    ! daily mean value of soil relative humidity at the resolution of 1 cm, the second dimension is 3
    ! the three layers around sowing layer
    REAL(r_std), DIMENSION(npts, nvm, 3), INTENT(OUT)    :: wus_cm_daily
    ! daily potential evapotranspiration 
    REAL(r_std), DIMENSION(npts), INTENT(OUT)      :: evapot_daily
    ! biomass of previous day, t/ha
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)      :: pdbiomass
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)      :: pdmasec
    ! vegetative biomass
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)      :: masecveg   
    ! aboveground dry matter (t ha-1) 
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)      :: masec
    ! growth rate of plant, it means the delta total biomass increment (t ha-1)
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)      :: dltams
    ! daily gdh calculated according to halfhourly temperature // transmitted from stomate.f90 gdh_daily
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)         :: gdh_daily 
    ! Photoperiod // hours
    REAL(r_std),  DIMENSION(npts), INTENT(OUT)                             :: phoi 

    !  
    LOGICAL, DIMENSION(npts, nvm), INTENT(OUT)           :: onarretesomcourdrp 
    !INTEGER(i_std), DIMENSION(nvm), INTENT(OUT)                           :: codeulaivernal 
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nlevobs    ! the following variables ended with obs are only used for forcing simulation.  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: namfobs    ! the initial value should be always 999
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nfloobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nlanobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nlaxobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nmatobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nrecobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: nsenobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)                :: ndrpobs  

    ! LAIdev SPECIFIC 
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nsendltams
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nsendltai
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nsenpfeuilverte
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nsendurvie
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nsenndurvie
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: densiteequiv
    INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nplt
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: tursla
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: ssla
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: pfeuilverte
    REAL(r_std), DIMENSION(npts, nvm), INTENT(OUT)        :: bsenlai
    
    ! variables are involved in DEVELOPMENT

    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: zrac
    INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nrec
    INTEGER(i_std), DIMENSION(npts, nvm), INTENT(OUT)        :: nlan
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: tcult
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: udevair
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: udevcult
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: ndrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: rfvi
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nlev
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nger
    logical,    DIMENSION(npts, nvm), INTENT(OUT)        :: etatvernal
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: caljvc
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: rfpi
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: upvt
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: utp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somcour
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somcourdrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somcourutp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: tdevelop
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somtemp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somcourfauche
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: stpltger
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stamflax
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stlaxsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stsenlan
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: stlevflo
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nflo
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stlevdrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stflodrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stdrpmat
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nmat
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nlax
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nrecbutoir
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: group
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: ndebdes
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: R_stdrpdes
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: densite
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: densitelev
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: coeflev
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: densiteger
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somelong
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somger
    logical,    DIMENSION(npts, nvm), INTENT(OUT)        :: humectation
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nbjhumec
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somtemphumec
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: stpltlev
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: namf
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: stmatrec
 
    ! these variables are involved in Lai_calculation
     
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: tustress
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: lai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: somfeuille
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: pdlai
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nbfeuille
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: reajust
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: ulai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: pdulai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: efdensite
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: tempeff
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nstopfeuille
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: deltai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: vmax
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: nsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: laisen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: pdlaisen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)        :: dltaisenat

    ! these variables are involved in the LAIsenescence

    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: nsencour
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: dltamsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: dltaisen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: fgellev
    logical,    DIMENSION(npts, nvm), INTENT(OUT)      :: gelee
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: fstressgel
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: R_stlevamf
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: dernier_n
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: durvieI
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: durvie
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: ndebsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: somsenreste

    INTEGER(i_std), INTENT(IN)                             :: nboxmax   
    INTEGER(i_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_ndays    
    REAL(r_std),    DIMENSION(npts, nvm)      :: boxtemp
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_lai
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_lairem
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_tdev
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_biom
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_biomrem
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_durage
    REAL(r_std),    DIMENSION(npts, nvm, nboxmax), INTENT(OUT)      :: box_somsenbase
    

    ! these variables are involved in STRESS calculation
    
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: humrel
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: swfac
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: turfac
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: senfac

    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: mafeuiljaune
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(OUT)      :: msneojaune
    ! these variables are involved in the CARBON ALLOCATION calculation

    ! grain related   
    REAL(r_std),    DIMENSION(npts, nvm, vlength)      ,INTENT(OUT)       :: v_dltams
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: fgelflo
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: pdircarb
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: ircarb
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: nbgrains
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: pgrain
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: vitmoy
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: nbgraingel
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: pgraingel
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: dltags
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: ftempremp
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: magrain
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: pdmagrain
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(OUT)       :: nbj0remp 
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: pdsfruittot

    ! reprac related

    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: repracmax
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: repracmin
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: kreprac
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: somtemprac
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: urac
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: reprac
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(OUT)       :: nstoprac

    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: c_reserve
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(OUT)       :: c_leafb

    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(OUT)       :: gslen 
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(OUT)       :: drylen 

!!!!! xuhui

    !---------------------------------------------------------------------
    IF (printlev >= 3) WRITE(numout,*) 'Entering readstart'

    ! near surface CH4 concentration
    !pss:+
    CH4atmo_CONC=0.0017
    CALL GETIN('CH4atmo_CONC', CH4atmo_CONC)
    !pss:-

    !-
    ! 1 string definitions
    !-
    DO l=1,nlitt
       IF     (l == imetabolic) THEN
          litter_str(l) = 'met'
       ELSEIF (l == istructural) THEN
          litter_str(l) = 'str'
       ELSE
          CALL ipslerr_p(3,'stomate_io readstart', 'Define litter_str','','')
       ENDIF
    ENDDO
    !-
    DO l=1,nlevs
       IF     (l == iabove) THEN
          level_str(l) = 'ab'
       ELSEIF (l == ibelow) THEN
          level_str(l) = 'be'
       ELSE
          CALL ipslerr_p(3,'stomate_io readstart','Define level_str','','')
       ENDIF
    ENDDO

    pools_str(1:nbpools) =(/'str_ab','str_be','met_ab','met_be','actif ','slow  ','passif'/)

    !-
    DO l=1,nelements
       IF     (l == icarbon) THEN
          element_str(l) = ''
!!$       ELSEIF (l == initrogen) THEN
!!$          element_str(l) = '_n'
       ELSE
          CALL ipslerr_p(3,'stomate_io readstart','Define element_str','','')
       ENDIF
    ENDDO
    !-
    ! 2 run control
    !-
    ! 2.2 time step of STOMATE in days
    !-
    IF (is_root_prc) THEN
       var_name = 'dt_days'
       CALL restget (rest_id_stomate, var_name, 1   , 1     , 1, itime, &
            &                 .TRUE., xtmp)
       dt_days = xtmp(1)
       IF (dt_days == val_exp) dt_days = un
    ENDIF
    CALL bcast(dt_days)
    !-
    ! 2.3 date
    !-
    IF (is_root_prc) THEN
       var_name = 'date'
       CALL restget (rest_id_stomate, var_name, 1   , 1     , 1, itime, &
            &                 .TRUE., xtmp)
       date_real = xtmp(1)
       IF (date_real == val_exp) date_real = zero
       date = NINT(date_real)
    ENDIF
    CALL bcast(date)
    !-
    ! 3 daily meteorological variables
    !-
    moiavail_daily(:,:) = val_exp
    var_name = 'moiavail_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., moiavail_daily, 'gather', nbp_glo, index_g)
    IF (ALL(moiavail_daily(:,:) == val_exp)) moiavail_daily(:,:) = zero
    !-
    gdd_init_date(:,:) = val_exp
    var_name = 'gdd_init_date'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 2 , 1, itime, &
         &              .TRUE., gdd_init_date, 'gather', nbp_glo, index_g)
    IF (ALL(gdd_init_date(:,1) == val_exp)) gdd_init_date(:,1) = 365.
    !-
    litterhum_daily(:) = val_exp
    var_name = 'litterhum_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., litterhum_daily, 'gather', nbp_glo, index_g)
    IF (ALL(litterhum_daily(:) == val_exp)) litterhum_daily(:) = zero
    !-
    t2m_daily(:) = val_exp
    var_name = 't2m_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &                .TRUE., t2m_daily, 'gather', nbp_glo, index_g)
    IF (ALL(t2m_daily(:) == val_exp)) t2m_daily(:) = zero
    !-
    t2m_min_daily(:) = val_exp
    var_name = 't2m_min_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &                .TRUE., t2m_min_daily, 'gather', nbp_glo, index_g)
    IF (ALL(t2m_min_daily(:) == val_exp)) t2m_min_daily(:) = large_value
    !spitfire
    !-
    t2m_max_daily(:) = val_exp
    var_name = 't2m_max_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &                .TRUE., t2m_max_daily, 'gather', nbp_glo, index_g)
    IF (ALL(t2m_max_daily(:) == val_exp)) t2m_max_daily(:) = (-1.) * large_value
    !-
    wspeed_daily(:) = val_exp
    var_name = 'wspeed_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &                .TRUE., wspeed_daily, 'gather', nbp_glo, index_g)
    IF (ALL(wspeed_daily(:) == val_exp)) wspeed_daily(:) = large_value
    !endspit
    !-
    tsurf_daily(:) = val_exp
    var_name = 'tsurf_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &                .TRUE., tsurf_daily, 'gather', nbp_glo, index_g)
    ! The initial value is set to the current temperature at 2m
    IF (ALL(tsurf_daily(:) == val_exp)) tsurf_daily(:) = t2m(:)
    !-
    tsoil_daily(:,:) = val_exp
    var_name = 'tsoil_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nbdl, 1, itime, &
         &                .TRUE., tsoil_daily, 'gather', nbp_glo, index_g)
    IF (ALL(tsoil_daily(:,:) == val_exp)) tsoil_daily(:,:) = zero
    !-
    soilhum_daily(:,:) = val_exp
    var_name = 'soilhum_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nbdl, 1, itime, &
         &                .TRUE., soilhum_daily, 'gather', nbp_glo, index_g)
    IF (ALL(soilhum_daily(:,:) == val_exp)) soilhum_daily(:,:) = zero
    !-
    precip_daily(:) = val_exp
    var_name = 'precip_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &                .TRUE., precip_daily, 'gather', nbp_glo, index_g)
    IF (ALL(precip_daily(:) == val_exp)) precip_daily(:) = zero
    !-
    ! 4 productivities
    !-
    gpp_daily(:,:) = val_exp
    var_name = 'gpp_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., gpp_daily, 'gather', nbp_glo, index_g)
    IF (ALL(gpp_daily(:,:) == val_exp)) gpp_daily(:,:) = zero
    !-
    npp_daily(:,:) = val_exp
    var_name = 'npp_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., npp_daily, 'gather', nbp_glo, index_g)
    IF (ALL(npp_daily(:,:) == val_exp)) npp_daily(:,:) = zero
    !-
    turnover_daily(:,:,:,:) = val_exp
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF (k < 10) part_str(1:1) = '0'
          var_name = 'turnover_daily_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
               &                .TRUE., turnover_daily(:,:,k,l), 'gather', nbp_glo, index_g)
          IF (ALL(turnover_daily(:,:,k,l) == val_exp)) &
               &       turnover_daily(:,:,k,l) = zero
       ENDDO
    END DO
    !-
    ! 5 monthly meteorological variables
    !-
    moiavail_month(:,:) = val_exp
    var_name = 'moiavail_month'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., moiavail_month, 'gather', nbp_glo, index_g)
    IF (ALL(moiavail_month(:,:) == val_exp)) moiavail_month(:,:) = zero
    !-
    moiavail_week(:,:) = val_exp
    var_name = 'moiavail_week'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., moiavail_week, 'gather', nbp_glo, index_g)
    IF (ALL(moiavail_week(:,:) == val_exp)) moiavail_week(:,:) = zero
    

    !
    ! Longterm temperature at 2m
    !
    var_name = 't2m_longterm'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., t2m_longterm, 'gather', nbp_glo, index_g)

    IF (ALL(t2m_longterm(:) == val_exp)) THEN
       ! t2m_longterm is not in restart file
       ! It is possible to initialize it from a file REFTEMP_FILE or initialize it to t2m(default). 

       !Config Key   = REFTEMP_FILE
       !Config Desc  = Name of file from which the reference temperature can be read
       !Config If    = OK_STOMATE
       !Config Def   = NONE
       !Config Help  = Specify filename if the initial reference temperature should be 
       !Config         read from file. Otherwise, the reference temperature is calculated. 
       !Config Units = [FILE]
       !---
       filename = 'NONE'
       CALL getin_p('REFTEMP_FILE',filename)
       IF ( filename == 'NONE' ) THEN
          ! The initial value for the reference temperature is set to the current temperature
          t2m_longterm(:)=t2m(:)
          ! Set the counter to 2 time steps
          tau_longterm=2
       ELSE
          ! t2m_longterm will be initialized with the values from REFTEMP_FILE
          CALL get_reftemp(npts, lalo, resolution, filename, t2m_longterm)
          tau_longterm=tau_longterm_max
       END IF
    ELSE
       ! t2m_longterm was in the restart file
       ! Now read tau_longterm
       ! tau_longterm is a scalar, therefor only master process read this value
       IF (is_root_prc) THEN
          CALL restget (rest_id_stomate, 'tau_longterm', 1 ,1  , 1, itime, &
               .TRUE., vartmp)
          IF (vartmp(1) == val_exp) THEN
             ! tau_longterm is not found in restart file. 
             ! This is not normal as t2m_longterm was in restart file. Write a warning and initialize it to tau_longterm_max
             CALL ipslerr(2, 'stomate_io readstart','tau_longterm was not in restart file',&
                  'But t2m_longterm was in restart file','')
             tau_longterm = tau_longterm_max
          ELSE
             tau_longterm = vartmp(1)
          END IF
       ENDIF
       CALL bcast(tau_longterm)

    END IF
    !-
    t2m_month(:) = val_exp
    var_name = 't2m_month'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., t2m_month, 'gather', nbp_glo, index_g)
    IF (ALL(t2m_month(:) == val_exp)) t2m_month(:) = t2m(:)
    
    Tseason(:) = val_exp
    var_name = 'Tseason'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., Tseason, 'gather', nbp_glo, index_g)
    IF (ALL(Tseason(:) == val_exp)) Tseason(:) = t2m(:)
    !-
    Tseason_length(:) = val_exp
    var_name = 'Tseason_length'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., Tseason_length, 'gather', nbp_glo, index_g)
    IF (ALL(Tseason_length(:) == val_exp)) Tseason_length(:) = t2m(:)
    !-
    Tseason_tmp(:) = val_exp
    var_name = 'Tseason_tmp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., Tseason_tmp, 'gather', nbp_glo, index_g)
    IF (ALL(Tseason_tmp(:) == val_exp)) Tseason_tmp(:) = t2m(:)
    !-

    Tmin_spring_time(:,:) = val_exp
    var_name = 'Tmin_spring_time'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &              .TRUE., Tmin_spring_time, 'gather', nbp_glo, index_g)
    IF (ALL(Tmin_spring_time(:,:) == val_exp)) Tmin_spring_time(:,:) = zero
    !-

    onset_date(:,:) = val_exp
    var_name = 'onset_date'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., onset_date, 'gather', nbp_glo, index_g)
    IF (ALL(onset_date == val_exp)) onset_date(:,:) = zero
    !-

    t2m_week(:) = val_exp
    var_name = 't2m_week'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., t2m_week, 'gather', nbp_glo, index_g)
    ! The initial value is set to the current temperature
    IF (ALL(t2m_week(:) == val_exp)) t2m_week(:) = t2m(:)
    
    tsoil_month(:,:) = val_exp
    var_name = 'tsoil_month'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nbdl, 1, itime, &
         &              .TRUE., tsoil_month, 'gather', nbp_glo, index_g)

    ! The initial value is set to the current temperature
    IF (ALL(tsoil_month(:,:) == val_exp)) THEN
       DO l=1,nbdl
          tsoil_month(:,l) = t2m(:)
       ENDDO
    ENDIF
    !-
    soilhum_month(:,:) = val_exp
    var_name = 'soilhum_month'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nbdl, 1, itime, &
         &              .TRUE., soilhum_month, 'gather', nbp_glo, index_g)
    IF (ALL(soilhum_month(:,:) == val_exp)) soilhum_month(:,:) = zero
    !-
    ! 6 fire probability
    !-
    fireindex(:,:) = val_exp
    var_name = 'fireindex'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &              .TRUE., fireindex, 'gather', nbp_glo, index_g)
    IF (ALL(fireindex(:,:) == val_exp)) fireindex(:,:) = zero
    !-
    firelitter(:,:) = val_exp
    var_name = 'firelitter'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &              .TRUE., firelitter, 'gather', nbp_glo, index_g)
    IF (ALL(firelitter(:,:) == val_exp)) firelitter(:,:) = zero
    !-
    ! 7 maximum and minimum moisture availabilities for tropic phenology
    !-
    maxmoiavail_lastyear(:,:) = val_exp
    var_name = 'maxmoistr_last'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., maxmoiavail_lastyear, 'gather', nbp_glo, index_g)
    IF (ALL(maxmoiavail_lastyear(:,:) == val_exp)) &
         &     maxmoiavail_lastyear(:,:) = zero
    !-
    maxmoiavail_thisyear(:,:) = val_exp
    var_name = 'maxmoistr_this'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., maxmoiavail_thisyear, 'gather', nbp_glo, index_g)
    IF (ALL(maxmoiavail_thisyear(:,:) == val_exp)) &
         &     maxmoiavail_thisyear(:,:) = zero
    !-
    minmoiavail_lastyear(:,:) = val_exp
    var_name = 'minmoistr_last'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., minmoiavail_lastyear, 'gather', nbp_glo, index_g)
    IF (ALL(minmoiavail_lastyear(:,:) == val_exp)) &
         &     minmoiavail_lastyear(:,:) = un
    !-
    minmoiavail_thisyear(:,:) = val_exp
    var_name = 'minmoistr_this'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., minmoiavail_thisyear, 'gather', nbp_glo, index_g)
    IF (ALL( minmoiavail_thisyear(:,:) == val_exp)) &
         &     minmoiavail_thisyear(:,:) = un
    !-
    ! 8 maximum "weekly" GPP
    !-
    maxgppweek_lastyear(:,:) = val_exp
    var_name = 'maxgppweek_lastyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., maxgppweek_lastyear, 'gather', nbp_glo, index_g)
    IF (ALL(maxgppweek_lastyear(:,:) == val_exp)) &
         &     maxgppweek_lastyear(:,:) = zero
    !-
    maxgppweek_thisyear(:,:) = val_exp
    var_name = 'maxgppweek_thisyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., maxgppweek_thisyear, 'gather', nbp_glo, index_g)
    IF (ALL(maxgppweek_thisyear(:,:) == val_exp)) &
         &     maxgppweek_thisyear(:,:) = zero
    !-
    ! 9 annual GDD0
    !-
    gdd0_thisyear(:) = val_exp
    var_name = 'gdd0_thisyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., gdd0_thisyear, 'gather', nbp_glo, index_g)
    IF (ALL(gdd0_thisyear(:) == val_exp)) gdd0_thisyear(:) = zero
    !-
    gdd0_lastyear(:) = val_exp
    var_name = 'gdd0_lastyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., gdd0_lastyear, 'gather', nbp_glo, index_g)
    IF (ALL(gdd0_lastyear(:) == val_exp)) gdd0_lastyear(:) = gdd_crit_estab
    !-
    ! 10 annual precipitation
    !-
    precip_thisyear(:) = val_exp
    var_name = 'precip_thisyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., precip_thisyear, 'gather', nbp_glo, index_g)
    IF (ALL(precip_thisyear(:) == val_exp)) precip_thisyear(:) = zero
    !-
    precip_lastyear(:) = val_exp
    var_name = 'precip_lastyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., precip_lastyear, 'gather', nbp_glo, index_g)
    IF (ALL(precip_lastyear(:) == val_exp)) &
         &     precip_lastyear(:) = precip_crit
    !-
    ! 11 derived "biometeorological" variables
    !-
    gdd_m5_dormance(:,:) = val_exp
    var_name = 'gdd_m5_dormance'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., gdd_m5_dormance, 'gather', nbp_glo, index_g)
    IF (ALL(gdd_m5_dormance(:,:) == val_exp)) &
         &     gdd_m5_dormance(:,:) = undef
    !-
    gdd_from_growthinit(:,:) = val_exp
    var_name = 'gdd_from_growthinit'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., gdd_from_growthinit, 'gather', nbp_glo, index_g)
    IF (ALL(gdd_from_growthinit(:,:) == val_exp)) &
         &     gdd_from_growthinit(:,:) = zero
    !-
    gdd_midwinter(:,:) = val_exp
    var_name = 'gdd_midwinter'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., gdd_midwinter, 'gather', nbp_glo, index_g)
    IF (ALL(gdd_midwinter(:,:) == val_exp)) gdd_midwinter(:,:) = undef
    !-
    ncd_dormance(:,:) = val_exp
    var_name = 'ncd_dormance'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., ncd_dormance, 'gather', nbp_glo, index_g)
    IF (ALL(ncd_dormance(:,:) == val_exp)) ncd_dormance(:,:) = undef
    !-
    ngd_minus5(:,:) = val_exp
    var_name = 'ngd_minus5'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., ngd_minus5, 'gather', nbp_glo, index_g)
    IF (ALL(ngd_minus5(:,:) == val_exp)) ngd_minus5(:,:) = zero
    !-
    time_hum_min(:,:) = val_exp
    var_name = 'time_hum_min'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., time_hum_min, 'gather', nbp_glo, index_g)
    IF (ALL(time_hum_min(:,:) == val_exp)) time_hum_min(:,:) = undef
    !-
    hum_min_dormance(:,:) = val_exp
    var_name = 'hum_min_dormance'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., hum_min_dormance, 'gather', nbp_glo, index_g)
    IF (ALL(hum_min_dormance(:,:) == val_exp)) &
         &     hum_min_dormance(:,:) = undef
    !-
    ! 12 Plant status
    !-
    PFTpresent_real(:,:) = val_exp
    var_name = 'PFTpresent'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., PFTpresent_real, 'gather', nbp_glo, index_g)
    IF (ALL(PFTpresent_real(:,:) == val_exp)) PFTpresent_real(:,:) = zero
    WHERE (PFTpresent_real(:,:) >= .5)
       PFTpresent = .TRUE.
    ELSEWHERE
       PFTpresent = .FALSE.
    ENDWHERE
    !-
    ind(:,:) = val_exp
    var_name = 'ind'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., ind, 'gather', nbp_glo, index_g)
    IF (ALL(ind(:,:) == val_exp)) ind(:,:) = zero
    !-
    adapted(:,:) = val_exp
    var_name = 'adapted'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., adapted, 'gather', nbp_glo, index_g)
    IF (ALL(adapted(:,:) == val_exp)) adapted(:,:) = zero
    !-
    regenerate(:,:) = val_exp
    var_name = 'regenerate'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., regenerate, 'gather', nbp_glo, index_g)
    IF (ALL(regenerate(:,:) == val_exp)) regenerate(:,:) = zero
    !-
    npp_longterm(:,:) = val_exp
    var_name = 'npp_longterm'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., npp_longterm, 'gather', nbp_glo, index_g)
    IF (ALL(npp_longterm(:,:) == val_exp)) npp_longterm(:,:) = zero
    !-
    lm_lastyearmax(:,:) = val_exp
    var_name = 'lm_lastyearmax'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., lm_lastyearmax, 'gather', nbp_glo, index_g)
    IF (ALL(lm_lastyearmax(:,:) == val_exp)) lm_lastyearmax(:,:) = zero
    !-
    lm_thisyearmax(:,:) = val_exp
    var_name = 'lm_thisyearmax'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., lm_thisyearmax, 'gather', nbp_glo, index_g)
    IF (ALL(lm_thisyearmax(:,:) == val_exp)) lm_thisyearmax(:,:) = zero
    !-
    maxfpc_lastyear(:,:) = val_exp
    var_name = 'maxfpc_lastyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., maxfpc_lastyear, 'gather', nbp_glo, index_g)
    IF (ALL(maxfpc_lastyear(:,:) == val_exp)) maxfpc_lastyear(:,:) = zero
    !-
    maxfpc_thisyear(:,:) = val_exp
    var_name = 'maxfpc_thisyear'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., maxfpc_thisyear, 'gather', nbp_glo, index_g)
    IF (ALL(maxfpc_thisyear(:,:) == val_exp)) maxfpc_thisyear(:,:) = zero
    !-
    turnover_time(:,:) = val_exp
    var_name = 'turnover_time'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., turnover_time, 'gather', nbp_glo, index_g)
    IF ( ALL( turnover_time(:,:) == val_exp)) turnover_time(:,:) = 100.
    !-
    turnover_longterm(:,:,:,:) = val_exp
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF ( k < 10 ) part_str(1:1) = '0'
          var_name = 'turnover_longterm_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
               &              .TRUE., turnover_longterm(:,:,k,l), 'gather', nbp_glo, index_g)
          IF (ALL(turnover_longterm(:,:,k,l) == val_exp)) &
               &       turnover_longterm(:,:,k,l) = zero
       ENDDO
    END DO
    !-
    gpp_week(:,:) = val_exp
    var_name = 'gpp_week'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &              .TRUE., gpp_week, 'gather', nbp_glo, index_g)
    IF (ALL(gpp_week(:,:) == val_exp)) gpp_week(:,:) = zero
    !-
    biomass(:,:,:,:) = val_exp
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF ( k < 10 ) part_str(1:1) = '0'
          var_name = 'biomass_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
               &                   .TRUE., biomass(:,:,k,l), 'gather', nbp_glo, index_g)
          IF (ALL(biomass(:,:,k,l) == val_exp)) biomass(:,:,k,l) = zero
       ENDDO
    END DO
    !-
    resp_maint_part(:,:,:) = val_exp
    var_name = 'maint_resp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , nparts, itime, &
         &                   .TRUE., resp_maint_part, 'gather', nbp_glo, index_g)
    IF (ALL(resp_maint_part == val_exp)) resp_maint_part(:,:,:) = zero
    !-
    leaf_age(:,:,:) = val_exp
    var_name = 'leaf_age'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , nleafages, itime, &
         &                   .TRUE., leaf_age, 'gather', nbp_glo, index_g)
    IF (ALL(leaf_age == val_exp)) leaf_age(:,:,:) = zero
    !-
    leaf_frac(:,:,:) = val_exp
    var_name = 'leaf_frac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , nleafages, itime, &
         &                  .TRUE., leaf_frac, 'gather', nbp_glo, index_g)
    IF (ALL(leaf_frac == val_exp)) leaf_frac(:,:,:) = zero
    !-
    senescence_real(:,:) = val_exp
    var_name = 'senescence'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., senescence_real, 'gather', nbp_glo, index_g)
    IF (ALL(senescence_real(:,:) == val_exp)) senescence_real(:,:) = zero
    WHERE ( senescence_real(:,:) >= .5 )
       senescence = .TRUE.
    ELSEWHERE
       senescence = .FALSE.
    ENDWHERE

    begin_leaves_real(:,:) = val_exp
    var_name = 'begin_leaves'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., begin_leaves_real, 'gather', nbp_glo, index_g)
    IF (ALL(begin_leaves_real(:,:) == val_exp)) begin_leaves_real(:,:) = zero
    WHERE ( begin_leaves_real(:,:) >= .5 )
       begin_leaves = .TRUE.
    ELSEWHERE
       begin_leaves = .FALSE.
    ENDWHERE
    !-
    when_growthinit(:,:) = val_exp
    var_name = 'when_growthinit'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., when_growthinit, 'gather', nbp_glo, index_g)
    IF (ALL(when_growthinit(:,:) == val_exp)) &
         &     when_growthinit(:,:) = zero
    !-
    age(:,:) = val_exp
    var_name = 'age'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., age, 'gather', nbp_glo, index_g)
    IF (ALL(age(:,:) == val_exp)) age(:,:) = zero
    !-
    ! 13 CO2
    !-
    resp_hetero(:,:) = val_exp
    var_name = 'resp_hetero'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                .TRUE., resp_hetero, 'gather', nbp_glo, index_g)
    IF (ALL(resp_hetero(:,:) == val_exp)) resp_hetero(:,:) = zero
    !-
    resp_maint(:,:) = val_exp
    var_name = 'resp_maint'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., resp_maint, 'gather', nbp_glo, index_g)
    IF (ALL(resp_maint(:,:) == val_exp)) resp_maint(:,:) = zero
    !-
    resp_growth(:,:) = val_exp
    var_name = 'resp_growth'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., resp_growth, 'gather', nbp_glo, index_g)
    IF (ALL(resp_growth(:,:) == val_exp)) resp_growth(:,:) = zero
    !-
    co2_fire(:,:) = val_exp
    var_name = 'co2_fire'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &                .TRUE., co2_fire, 'gather', nbp_glo, index_g)
    IF (ALL(co2_fire(:,:) == val_exp)) co2_fire(:,:) = zero
    !-
    co2_to_bm_dgvm(:,:) = val_exp
    var_name = 'co2_to_bm_dgvm'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &                .TRUE., co2_to_bm_dgvm, 'gather', nbp_glo, index_g)
    IF (ALL(co2_to_bm_dgvm(:,:) == val_exp)) co2_to_bm_dgvm(:,:) = zero
    !-
    ! 14 vegetation distribution after last light competition
    !-
    veget_lastlight(:,:) = val_exp
    var_name = 'veget_lastlight'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., veget_lastlight, 'gather', nbp_glo, index_g)
    IF (ALL(veget_lastlight(:,:) == val_exp)) veget_lastlight(:,:) = zero
    !-
    ! 15 establishment criteria
    !-
    everywhere(:,:) = val_exp
    var_name = 'everywhere'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., everywhere, 'gather', nbp_glo, index_g)
    IF (ALL(everywhere(:,:) == val_exp)) everywhere(:,:) = zero
    !-
    need_adjacent_real(:,:) = val_exp
    var_name = 'need_adjacent'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., need_adjacent_real, 'gather', nbp_glo, index_g)
    IF (ALL(need_adjacent_real(:,:) == val_exp)) &
         &     need_adjacent_real(:,:) = zero
    WHERE ( need_adjacent_real(:,:) >= .5 )
       need_adjacent = .TRUE.
    ELSEWHERE
       need_adjacent = .FALSE.
    ENDWHERE
    !-
    RIP_time(:,:) = val_exp
    var_name = 'RIP_time'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                .TRUE., RIP_time, 'gather', nbp_glo, index_g)
    IF (ALL(RIP_time(:,:) == val_exp)) RIP_time(:,:) = large_value
    !-
    ! 17 litter
    !-
    litterpart(:,:,:) = val_exp
    var_name = 'litterpart'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , nlitt, itime, &
         &                   .TRUE., litterpart, 'gather', nbp_glo, index_g)
    IF (ALL(litterpart == val_exp)) litterpart(:,:,:) = zero
    !-
    litter(:,:,:,:,:) = val_exp
    DO k = 1,nelements
       DO l = 1,nlevs
          DO m = 1,nvm
             WRITE (part_str, '(I2)') m
             IF (m<10) part_str(1:1)='0'
             var_name = 'litter_'//part_str(1:LEN_TRIM(part_str))//'_'//level_str(l)//element_str(k)
             CALL restget_p (rest_id_stomate, var_name, nbp_glo, nlitt , 1, itime, &
                  &                     .TRUE., litter(:,:,m,l,k), 'gather', nbp_glo, index_g)
             IF (ALL(litter(:,:,m,l,k) == val_exp)) litter(:,:,m,l,k) = zero
          ENDDO
       ENDDO
    END DO
    !-
    dead_leaves(:,:,:) = val_exp
    var_name = 'dead_leaves'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , nlitt, itime, &
         &                   .TRUE., dead_leaves, 'gather', nbp_glo, index_g)
    IF (ALL(dead_leaves == val_exp)) dead_leaves(:,:,:) = zero
    !-
    carbon(:,:,:) = val_exp
    var_name = 'carbon'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, ncarb , nvm, itime, &
         &                   .TRUE., carbon, 'gather', nbp_glo, index_g)
    IF (ALL(carbon == val_exp)) carbon(:,:,:) = zero
    !-
    lignin_struc(:,:,:) = val_exp
    var_name = 'lignin_struc'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, nlevs, itime, &
         &     .TRUE., lignin_struc, 'gather', nbp_glo, index_g)
    IF (ALL(lignin_struc == val_exp)) lignin_struc(:,:,:) = zero

     !spitfire
      ni_acc(:) = val_exp
        var_name = 'ni_acc'
        CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
     &              .TRUE., ni_acc, 'gather', nbp_glo, index_g)
        IF (ALL(ni_acc(:) == val_exp)) ni_acc(:) = zero
    
      fuel_1hr(:,:,:,:) = val_exp
      DO k = 1,nelements
        DO l=1,nlitt
          var_name = 'fuel_1hr_'//litter_str(l)//element_str(k)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                  &       .TRUE., fuel_1hr(:,:,l,k), 'gather', nbp_glo, index_g)
          IF (ALL(fuel_1hr(:,:,l,k) == val_exp)) fuel_1hr(:,:,l,k) = 0.0
        ENDDO
      ENDDO

      fuel_10hr(:,:,:,:) = val_exp
      DO k = 1,nelements
        DO l=1,nlitt
          var_name = 'fuel_10hr_'//litter_str(l)//element_str(k)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                  &       .TRUE., fuel_10hr(:,:,l,k), 'gather', nbp_glo, index_g)
          IF (ALL(fuel_10hr(:,:,l,k) == val_exp)) fuel_10hr(:,:,l,k) = 0.0
        ENDDO
      ENDDO

      fuel_100hr(:,:,:,:) = val_exp
      DO k = 1,nelements
        DO l=1,nlitt
          var_name = 'fuel_100hr_'//litter_str(l)//element_str(k)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                  &       .TRUE., fuel_100hr(:,:,l,k), 'gather', nbp_glo, index_g)
          IF (ALL(fuel_100hr(:,:,l,k) == val_exp)) fuel_100hr(:,:,l,k) = 0.0
        ENDDO
      ENDDO

      fuel_1000hr(:,:,:,:) = val_exp
      DO k = 1,nelements
        DO l=1,nlitt
          var_name = 'fuel_1000hr_'//litter_str(l)//element_str(k)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                  &       .TRUE., fuel_1000hr(:,:,l,k), 'gather', nbp_glo, index_g)
          IF (ALL(fuel_1000hr(:,:,l,k) == val_exp)) fuel_1000hr(:,:,l,k) = 0.0
        ENDDO
      ENDDO
     !endspit
    
    !-
    ! 18 land cover change
    !-
    prod10(:,:,:) = val_exp
    var_name = 'prod10'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 11     , nwp, itime, &
         &                .TRUE., prod10, 'gather', nbp_glo, index_g)
    IF (ALL(prod10(:,:,:) == val_exp)) prod10(:,:,:) = zero

    prod100(:,:,:) = val_exp
    var_name = 'prod100'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 101     , nwp, itime, &
         &                .TRUE., prod100, 'gather', nbp_glo, index_g)
    IF (ALL(prod100(:,:,:) == val_exp)) prod100(:,:,:) = zero


    flux10(:,:,:) = val_exp
    var_name = 'flux10'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 10     , nwp, itime, &
         &                .TRUE., flux10, 'gather', nbp_glo, index_g)
    IF (ALL(flux10(:,:,:) == val_exp)) flux10(:,:,:) = zero

    flux100(:,:,:) = val_exp
    var_name = 'flux100'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 100     , nwp, itime, &
         &                .TRUE., flux100, 'gather', nbp_glo, index_g)
    IF (ALL(flux100(:,:,:) == val_exp)) flux100(:,:,:) = zero

    !convflux(:,:) = val_exp
    !var_name = 'convflux'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , nwp, itime, &
    !     &              .TRUE., convflux, 'gather', nbp_glo, index_g)
    !IF (ALL(convflux(:,:) == val_exp)) convflux(:,:) = zero

    !cflux_prod10(:,:) = val_exp
    !var_name = 'cflux_prod10'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , nwp, itime, &
    !     &              .TRUE., cflux_prod10, 'gather', nbp_glo, index_g)
    !IF (ALL(cflux_prod10(:,:) == val_exp)) cflux_prod10(:,:) = zero

    !cflux_prod100(:,:) = val_exp
    !var_name = 'cflux_prod100'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , nwp, itime, &
    !     &              .TRUE., cflux_prod100, 'gather', nbp_glo, index_g)
    !IF (ALL(cflux_prod100(:,:) == val_exp)) cflux_prod100(:,:) = zero

    bm_to_litter(:,:,:,:) = val_exp
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF ( k < 10 ) part_str(1:1) = '0'
          var_name = 'bm_to_litter_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
               &                .TRUE., bm_to_litter(:,:,k,l), 'gather', nbp_glo, index_g)
          IF (ALL(bm_to_litter(:,:,k,l) == val_exp)) bm_to_litter(:,:,k,l) = zero
       ENDDO
    END DO

    carb_mass_total(:) = val_exp
    var_name = 'carb_mass_total'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., carb_mass_total, 'gather', nbp_glo, index_g)
    IF (ALL(carb_mass_total(:) == val_exp)) carb_mass_total(:) = zero

    !Permafrost carbon related
    deepC_a(:,:,:) = val_exp
    var_name = 'deepC_a'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, ndeep, nvm, itime, &
         &               .TRUE., deepC_a, 'gather', nbp_glo, index_g)
    IF (ALL(deepC_a == val_exp)) deepC_a(:,:,:) = zero !deepC_a_init

    deepC_s(:,:,:) = val_exp
    var_name = 'deepC_s'
    CALL restget_p (rest_id_stomate,var_name, nbp_glo, ndeep, nvm, itime, &
         &               .TRUE., deepC_s, 'gather', nbp_glo, index_g)
    IF (ALL(deepC_s == val_exp)) deepC_s(:,:,:) = zero !deepC_s_init

    deepC_p(:,:,:) = val_exp
    var_name = 'deepC_p'
    CALL restget_p (rest_id_stomate,var_name, nbp_glo, ndeep, nvm, itime, &
         &               .TRUE., deepC_p, 'gather', nbp_glo, index_g)
    IF (ALL(deepC_p == val_exp)) deepC_p(:,:,:) = zero !deepC_p_init

    O2_soil(:,:,:) = val_exp
    var_name = 'O2_soil'
    CALL restget_p (rest_id_stomate,var_name, nbp_glo, ndeep, nvm, itime, &
           &               .TRUE., O2_soil, 'gather', nbp_glo, index_g)
    IF (ALL(O2_soil == val_exp)) O2_soil(:,:,:) = O2_init_conc

    CH4_soil(:,:,:) = val_exp
    var_name = 'CH4_soil'
    CALL restget_p (rest_id_stomate,var_name, nbp_glo, ndeep, nvm, itime, &
         &               .TRUE., CH4_soil, 'gather', nbp_glo, index_g)
    IF (ALL(CH4_soil == val_exp)) CH4_soil(:,:,:) = CH4_init_conc

    O2_snow(:,:,:) = val_exp
    var_name = 'O2_snow'
    CALL restget_p (rest_id_stomate,var_name, nbp_glo, nsnow, nvm, itime, &
         &               .TRUE., O2_snow, 'gather', nbp_glo, index_g)
    IF (ALL(O2_snow == val_exp)) O2_snow(:,:,:) = O2_init_conc

    CH4_snow(:,:,:) = val_exp
    var_name = 'CH4_snow'
    CALL restget_p (rest_id_stomate,var_name, nbp_glo, nsnow, nvm, itime, &
         &               .TRUE., CH4_snow, 'gather', nbp_glo, index_g)
    IF (ALL(CH4_snow == val_exp)) CH4_snow(:,:,:) = CH4_init_conc

  thawed_humidity(:) = val_exp
  var_name = 'thawed_humidity'

  CALL getin('reset_thawed_humidity', reset_thawed_humidity)
  if ( reset_thawed_humidity ) then
     CALL getin('thawed_humidity_input', thawed_humidity_input)
     thawed_humidity(:) = thawed_humidity_input
  else
     CALL restget_p (rest_id_stomate,var_name, nbp_glo, 1, 1, itime, &
          &               .TRUE., thawed_humidity, 'gather', nbp_glo, index_g)
     IF (ALL(thawed_humidity(:) == val_exp)) THEN
        thawed_humidity(:) = thawed_humidity_input
        read_input_thawed_humidity = .TRUE.
     ENDIF
  endif


  depth_organic_soil(:) = val_exp
  var_name = 'depth_organic_soil'
  CALL restget_p (rest_id_stomate,var_name, nbp_glo, 1, 1, itime, &
       &               .TRUE., depth_organic_soil, 'gather', nbp_glo, index_g)
  IF (ALL(depth_organic_soil(:) == val_exp)) THEN
     depth_organic_soil(:) = 0.0
     read_input_depth_organic_soil = .TRUE.
  ENDIF

  altmax(:,:) = val_exp
  var_name = 'altmax'
  CALL restget_p (rest_id_stomate,var_name, nbp_glo, nvm, 1, itime, &
       &               .TRUE., altmax, 'gather', nbp_glo, index_g)
  IF (ALL(altmax(:,:) == val_exp)) THEN
     altmax(:,:) = 0.0
  ENDIF

  fixed_cryoturbation_depth(:,:) = val_exp
  var_name = 'fixed_cryoturb_depth'
  CALL restget_p (rest_id_stomate,var_name, nbp_glo, nvm, 1, itime, &
       &               .TRUE., fixed_cryoturbation_depth, 'gather', nbp_glo, index_g)
  IF (ALL(fixed_cryoturbation_depth(:,:) == val_exp)) THEN
     fixed_cryoturbation_depth(:,:) = 0.0
  ENDIF

  !-

!!!!! Wetland CH4 methane
    !pss:+
    uo_0(:,:) = val_exp
    var_name = 'uo_0'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
         &              .TRUE.,uo_0 , 'gather', nbp_glo, index_g)
    IF (ALL(uo_0(:,:) == val_exp)) THEN
       DO nivo=1,nvert
          IF (nivo .LE. ns) THEN
            uo_0(:,nivo) = scmax
         ELSE
            uo_0(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF

   uold2_0(:,:) = val_exp
   var_name = 'uold2_0'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uold2_0 , 'gather', nbp_glo, index_g)
   IF (ALL(uold2_0(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns) THEN
            uold2_0(:,nivo) = scmax
         ELSE
            uold2_0(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF
   
   uo_wet1(:,:) = val_exp
   var_name = 'uo_wet1'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
       &              .TRUE.,uo_wet1 , 'gather', nbp_glo, index_g)
   IF (ALL(uo_wet1(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uo_wet1(:,nivo) = scmax
         ELSE
            uo_wet1(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF
   
   uold2_wet1(:,:) = val_exp
   var_name = 'uold2_wet1'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uold2_wet1 , 'gather', nbp_glo, index_g)
   IF (ALL(uold2_wet1(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uold2_wet1(:,nivo) = scmax
         ELSE
            uold2_wet1(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF
  
   uo_wet2(:,:) = val_exp
   var_name = 'uo_wet2'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uo_wet2 , 'gather', nbp_glo, index_g)
   IF (ALL(uo_wet2(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uo_wet2(:,nivo) = scmax
         ELSE
            uo_wet2(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF

   uold2_wet2(:,:) = val_exp
   var_name = 'uold2_wet2'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uold2_wet2 , 'gather', nbp_glo, index_g)
   IF (ALL(uold2_wet2(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uold2_wet2(:,nivo) = scmax
         ELSE
            uold2_wet2(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF

   uo_wet3(:,:) = val_exp
   var_name = 'uo_wet3'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uo_wet3 , 'gather', nbp_glo, index_g)
   IF (ALL(uo_wet3(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uo_wet3(:,nivo) = scmax
         ELSE
            uo_wet3(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF
   
   uold2_wet3(:,:) = val_exp
   var_name = 'uold2_wet3'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uold2_wet3 , 'gather', nbp_glo, index_g)
   IF (ALL(uold2_wet3(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uold2_wet3(:,nivo) = scmax
         ELSE
            uold2_wet3(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF

   uo_wet4(:,:) = val_exp
   var_name = 'uo_wet4'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uo_wet4 , 'gather', nbp_glo, index_g)
   IF (ALL(uo_wet4(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uo_wet4(:,nivo) = scmax
         ELSE
            uo_wet4(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF

   uold2_wet4(:,:) = val_exp
   var_name = 'uold2_wet4'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvert, 1, itime, &
        &              .TRUE.,uold2_wet4 , 'gather', nbp_glo, index_g)
   IF (ALL(uold2_wet4(:,:) == val_exp)) THEN
      DO nivo=1,nvert
         IF (nivo .LE. ns-10) THEN
            uold2_wet4(:,nivo) = scmax
         ELSE
            uold2_wet4(:,nivo) = CH4atmo_CONC
         ENDIF
      ENDDO
   ENDIF
   
   tsurf_year(:) = val_exp
   var_name = 'tsurf_year'
   CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
        &              .TRUE., tsurf_year, 'gather', nbp_glo, index_g)
   IF (ALL(tsurf_year(:) == val_exp)) tsurf_year(:) = t2m(:)
   
!pss:-
!JCADD
!-
!  sla_calc(:,:) = val_exp
!  var_name = 'sla_calc'
!  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm   , 1, itime, &
! &              .TRUE., sla_calc, 'gather', nbp_glo, index_g)
!  IF (ALL(sla_calc(:,:) == val_exp)) THEN
!     DO j=2,nvm
!        sla_calc(:,j) = sla(j)
!     END DO
!  END IF
  wshtotsum(:,:) = val_exp
  var_name = 'wshtotsum'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
 &              .TRUE., wshtotsum, 'gather', nbp_glo, index_g)
    IF (ALL(wshtotsum(:,:) == val_exp)) wshtotsum(:,:) = zero
!-
  sr_ugb(:,:) = val_exp
  var_name = 'sr_ugb'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
 &              .TRUE., sr_ugb, 'gather', nbp_glo, index_g)
    IF (ALL(sr_ugb(:,:) == val_exp)) sr_ugb(:,:) = zero
!-
  sla_calc(:,:) = val_exp
  var_name = 'sla_calc'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
 &              .TRUE., sla_calc, 'gather', nbp_glo, index_g)
!    IF (ALL(sla_calc(:,:) == val_exp)) sla_calc(:,:) = zero
  IF (ALL(sla_calc(:,:) == val_exp)) THEN
     DO j=1,nvm
        sla_calc(:,j) = sla(j)
     END DO
  END IF
!-
  nb_ani(:,:) = val_exp
  var_name = 'nb_ani'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
 &              .TRUE., nb_ani, 'gather', nbp_glo, index_g)
    IF (ALL(nb_ani(:,:) == val_exp)) nb_ani(:,:) = zero
!-
  grazed_frac(:,:) = val_exp
  var_name = 'grazed_frac'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
 &              .TRUE., grazed_frac, 'gather', nbp_glo, index_g)
    IF (ALL(grazed_frac(:,:) == val_exp)) grazed_frac(:,:) = zero
!-
  import_yield(:,:) = val_exp
  var_name = 'import_yield'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
 &              .TRUE., import_yield, 'gather', nbp_glo, index_g)
    IF (ALL(import_yield(:,:) == val_exp)) import_yield(:,:) = zero

!-
   t2m_14(:) = val_exp
  var_name = 't2m_14'
  CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
 &              .TRUE., t2m_14, 'gather', nbp_glo, index_g)
  IF (ALL(t2m_14(:) == val_exp)) t2m_14(:) = t2m(:)
!

    litter_not_avail(:,:,:) = val_exp
    DO l=1,nlitt
       var_name = 'litter_not_avail_'//litter_str(l)
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
            &                   .TRUE., litter_not_avail(:,l,:), 'gather', nbp_glo, index_g)
       IF (ALL(litter_not_avail(:,l,:) == val_exp)) litter_not_avail(:,l,:) = zero
    ENDDO

!ENDJCADD
!!!!! crops

    !f_crop_init_real = val_exp
    !var_name = 'f_crop_init'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo,   1, 1, itime, &
    !     &                .TRUE., f_crop_init_real, 'gather', nbp_glo, index_g)
    !IF (f_crop_init_real == val_exp) f_crop_init_real = zero
    !WHERE (f_crop_init_real == 1)
    !   f_crop_init = .TRUE.
    !ELSEWHERE
    !   f_crop_init = .FALSE.
    !ENDWHERE
   
    f_crop_recycle_real(:, :) = val_exp
    var_name = 'f_crop_recycle'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                .TRUE., f_crop_recycle_real, 'gather', nbp_glo, index_g)
    IF (ALL(f_crop_recycle_real(:, :) == val_exp)) f_crop_recycle_real(:, :) = zero
    WHERE (f_crop_recycle_real(:, :) == un)
       f_crop_recycle = .TRUE.
    ELSEWHERE
       f_crop_recycle = .FALSE.
    ENDWHERE

    in_cycle_real(:, :) = val_exp
    var_name = 'in_cycle'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                .TRUE., in_cycle_real, 'gather', nbp_glo, index_g)
    IF (ALL(in_cycle_real(:, :) == val_exp)) in_cycle_real(:, :) = zero
    WHERE (in_cycle_real(:, :) == un)
       in_cycle = .TRUE.
    ELSEWHERE
       in_cycle = .FALSE.
    ENDWHERE
    
    f_sen_lai_real(:, :) = val_exp
    var_name = 'f_sen_lai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                .TRUE., f_sen_lai_real, 'gather', nbp_glo, index_g)
    IF (ALL(f_sen_lai_real(:, :) == val_exp)) f_sen_lai_real(:, :) = un
    WHERE (f_sen_lai_real(:, :) == un)
       f_sen_lai = .TRUE.
    ELSEWHERE
       f_sen_lai = .FALSE.
    ENDWHERE
    
    st2m_max_daily(:) = val_exp
    var_name = 'st2m_max_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., st2m_max_daily, 'gather', nbp_glo, index_g)
    IF (ALL(st2m_max_daily(:) == val_exp)) st2m_max_daily(:) = zero

    wut_cm_daily(:, :, :) = val_exp
    DO h = 1, 3 
       WRITE(part_str, '(I2)') h
       IF (h < 4) part_str(1:1) = '0'
       var_name = 'wut_cm_daily_'//part_str(1:LEN_TRIM(part_str))
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
            &              .TRUE., wut_cm_daily(:, :, h), 'gather', nbp_glo, index_g)
       IF (ALL(wut_cm_daily(:, :, h) == val_exp)) wut_cm_daily(:, :, h) = zero
    ENDDO


    wus_cm_daily(:, :, :) = val_exp
    DO h = 1, 3
       WRITE(part_str, '(I2)') h
       IF (h < 4) part_str(1:1) = '0'
       var_name = 'wus_cm_daily_'//part_str(1:LEN_TRIM(part_str))

       CALL restget_p (rest_id_stomate, var_name, nbp_glo,  nvm , 1, itime, &
            &              .TRUE., wus_cm_daily(:, :, h), 'gather', nbp_glo, index_g)
       IF (ALL(wus_cm_daily(:, :, h) == val_exp)) wus_cm_daily(:, :, h) = zero
    ENDDO


    evapot_daily(:) = val_exp
    var_name = 'evapot_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., evapot_daily, 'gather', nbp_glo, index_g)
    IF (ALL(evapot_daily(:) == val_exp)) evapot_daily(:) = zero

    pdbiomass(:, :) = val_exp
    var_name = 'pdbiomass'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdbiomass, 'gather', nbp_glo, index_g)
    IF (ALL(pdbiomass(:, :) == val_exp)) pdbiomass(:, :) = zero

    pdmasec(:, :) = val_exp
    var_name = 'pdmasec'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdmasec, 'gather', nbp_glo, index_g)
    IF (ALL(pdmasec(:, :) == val_exp)) pdmasec(:, :) = zero

    masecveg(:, :) = val_exp
    var_name = 'masecveg'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., masecveg, 'gather', nbp_glo, index_g)
    IF (ALL(masecveg(:, :) == val_exp)) masecveg(:, :) = zero

    masec(:, :) = val_exp
    var_name = 'masec'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., masec, 'gather', nbp_glo, index_g)
    IF (ALL(masec(:, :) == val_exp)) masec(:, :) = zero

    dltams(:, :) = val_exp
    var_name = 'dltams'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., dltams, 'gather', nbp_glo, index_g)
    IF (ALL(dltams(:, :) == val_exp)) dltams(:, :) = zero

    gdh_daily(:, :) = val_exp
    var_name = 'gdh_daily'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., gdh_daily, 'gather', nbp_glo, index_g)
    IF (ALL(gdh_daily(:, :) == val_exp)) gdh_daily(:, :) = zero

    phoi(:) = val_exp
    var_name = 'phoi'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &              .TRUE., phoi, 'gather', nbp_glo, index_g)
    IF (ALL(phoi(:) == val_exp)) phoi(:) = zero

    onarretesomcourdrp_real(:, :) = val_exp
    var_name = 'onarretesomcourdrp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                .TRUE., onarretesomcourdrp_real, 'gather', nbp_glo, index_g)
    IF (ALL(onarretesomcourdrp_real(:, :) == val_exp)) onarretesomcourdrp_real(:, :) = zero
    WHERE (onarretesomcourdrp_real(:, :) == un)
       onarretesomcourdrp = .TRUE.
    ELSEWHERE
       onarretesomcourdrp = .FALSE.
    ENDWHERE

    !codeulaivernal(:) = val_exp
    !var_name = 'codeulaivernal'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
    !     &              .TRUE., codeulaivernal_real, 'gather', nbp_glo, index_g)
    !IF (ALL(codeulaivernal_real(:) == val_exp)) codeulaivernal_real(:) = zero
    !codeulaivernal = INT(codeulaivernal_real)

    !nlevobs(:, :) = val_exp
    !var_name = 'nlevobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nlevobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nlevobs_real(:, :) == val_exp)) nlevobs_real(:, :) = zero
    !nlevobs = INT(nlevobs_real)

    !namfobs(:, :) = val_exp
    !var_name = 'namfobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., namfobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(namfobs_real(:, :) == val_exp)) namfobs_real(:, :) = zero
    !namfobs = INT(namfobs_real)

    !nfloobs(:, :) = val_exp
    !var_name = 'nfloobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nfloobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nfloobs_real(:, :) == val_exp)) nfloobs_real(:, :) = zero
    !nfloobs = INT(nfloobs_real)

    !nlanobs(:, :) = val_exp
    !var_name = 'nlanobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nlanobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nlanobs_real(:, :) == val_exp)) nlanobs_real(:, :) = zero
    !nlanobs = INT(nlanobs_real)

    !nlaxobs(:, :) = val_exp
    !var_name = 'nlaxobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nlaxobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nlaxobs_real(:, :) == val_exp)) nlaxobs_real(:, :) = zero
    !nlaxobs = INT(nlaxobs_real)

    !nmatobs(:, :) = val_exp
    !var_name = 'nmatobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nmatobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nmatobs_real(:, :) == val_exp)) nmatobs_real(:, :) = zero
    !nmatobs = INT(nmatobs_real)

    !nrecobs(:, :) = val_exp
    !var_name = 'nrecobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nrecobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nrecobs_real(:, :) == val_exp)) nrecobs_real(:, :) = zero
    !nrecobs = INT(nrecobs_real)

    !nsenobs(:, :) = val_exp
    !var_name = 'nsenobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nsenobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(nsenobs_real(:, :) == val_exp)) nsenobs_real(:, :) = zero
    !nsenobs = INT(nsenobs_real)


    !ndrpobs(:, :) = val_exp
    !var_name = 'ndrpobs'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., ndrpobs_real, 'gather', nbp_glo, index_g)
    !IF (ALL(ndrpobs_real(:, :) == val_exp)) ndrpobs_real(:, :) = zero
    !ndrpobs = INT(ndrpobs_real)




    nsendltams(:, :) = val_exp
    var_name = 'nsendltams'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsendltams, 'gather', nbp_glo, index_g)
    IF (ALL(nsendltams(:, :) == val_exp)) nsendltams(:, :) = zero

    nsendltai(:, :) = val_exp
    var_name = 'nsendltai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsendltai, 'gather', nbp_glo, index_g)
    IF (ALL(nsendltai(:, :) == val_exp)) nsendltai(:, :) = zero

    nsenpfeuilverte(:, :) = val_exp
    var_name = 'nsenpfeuilverte'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsenpfeuilverte, 'gather', nbp_glo, index_g)
    IF (ALL(nsenpfeuilverte(:, :) == val_exp)) nsenpfeuilverte(:, :) = zero

    nsendurvie(:, :) = val_exp
    var_name = 'nsendurvie'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsendurvie, 'gather', nbp_glo, index_g)
    IF (ALL(nsendurvie(:, :) == val_exp)) nsendurvie(:, :) = zero

    nsenndurvie(:, :) = val_exp
    var_name = 'nsenndurvie'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsenndurvie, 'gather', nbp_glo, index_g)
    IF (ALL(nsenndurvie(:, :) == val_exp)) nsenndurvie(:, :) = zero

    densiteequiv(:, :) = val_exp
    var_name = 'densiteequiv'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., densiteequiv, 'gather', nbp_glo, index_g)
    IF (ALL(densiteequiv(:, :) == val_exp)) densiteequiv(:, :) = zero

    nplt(:, :) = val_exp
    var_name = 'nplt'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nplt_real, 'gather', nbp_glo, index_g)
    IF (ALL(nplt_real(:, :) == val_exp)) nplt_real(:, :) = zero
    nplt = INT(nplt_real)

    tursla(:, :) = val_exp
    var_name = 'tursla'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., tursla, 'gather', nbp_glo, index_g)
    IF (ALL(tursla(:, :) == val_exp)) tursla(:, :) = un

    ssla(:, :) = val_exp
    var_name = 'ssla'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ssla, 'gather', nbp_glo, index_g)
    IF (ALL(ssla(:, :) == val_exp)) ssla(:, :) = zero

    pfeuilverte(:, :) = val_exp
    var_name = 'pfeuilverte'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pfeuilverte, 'gather', nbp_glo, index_g)
    IF (ALL(pfeuilverte(:, :) == val_exp)) pfeuilverte(:, :) = zero

    bsenlai(:, :) = val_exp
    var_name = 'bsenlai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., bsenlai, 'gather', nbp_glo, index_g)
    IF (ALL(bsenlai(:, :) == val_exp)) bsenlai(:, :) = zero

    zrac(:, :) = val_exp
    var_name = 'zrac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., zrac, 'gather', nbp_glo, index_g)
    IF (ALL(zrac(:, :) == val_exp)) zrac(:, :) = zero

    nrec(:, :) = val_exp
    var_name = 'nrec'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nrec_real, 'gather', nbp_glo, index_g)
    IF (ALL(nrec_real(:, :) == val_exp)) nrec_real(:, :) = zero
    nrec = INT(nrec_real)

    nlan(:, :) = val_exp
    var_name = 'nlan'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nlan_real, 'gather', nbp_glo, index_g)
    IF (ALL(nlan_real(:, :) == val_exp)) nlan_real(:, :) = zero
    nlan = INT(nlan_real)

    tcult(:, :) = val_exp
    var_name = 'tcult'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., tcult, 'gather', nbp_glo, index_g)
    IF (ALL(tcult(:, :) == val_exp)) tcult(:, :) = zero

    udevair(:, :) = val_exp
    var_name = 'udevair'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., udevair, 'gather', nbp_glo, index_g)
    IF (ALL(udevair(:, :) == val_exp)) udevair(:, :) = zero

    udevcult(:, :) = val_exp
    var_name = 'udevcult'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., udevcult, 'gather', nbp_glo, index_g)
    IF (ALL(udevcult(:, :) == val_exp)) udevcult(:, :) = zero

    ndrp(:, :) = val_exp
    var_name = 'ndrp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ndrp_real, 'gather', nbp_glo, index_g)
    IF (ALL(ndrp_real(:, :) == val_exp)) ndrp_real(:, :) = zero
    ndrp = INT(ndrp_real)

    rfvi(:, :) = val_exp
    var_name = 'rfvi'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., rfvi, 'gather', nbp_glo, index_g)
    IF (ALL(rfvi(:, :) == val_exp)) rfvi(:, :) = zero

    nlev(:, :) = val_exp
    var_name = 'nlev'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nlev_real, 'gather', nbp_glo, index_g)
    IF (ALL(nlev_real(:, :) == val_exp)) nlev_real(:, :) = zero
    nlev = INT(nlev_real)

    nger(:, :) = val_exp
    var_name = 'nger'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nger_real, 'gather', nbp_glo, index_g)
    IF (ALL(nger_real(:, :) == val_exp)) nger_real(:, :) = zero
    nger = INT(nger_real)

    etatvernal_real(:, :) = val_exp
    var_name = 'etatvernal'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., etatvernal_real, 'gather', nbp_glo, index_g)
    IF (ALL(etatvernal_real(:, :) == val_exp)) etatvernal_real(:, :) = zero
    WHERE (etatvernal_real(:,:) == un)
       etatvernal = .TRUE.
    ELSEWHERE
       etatvernal = .FALSE.
    ENDWHERE
    

    caljvc(:, :) = val_exp
    var_name = 'caljvc'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., caljvc, 'gather', nbp_glo, index_g)
    IF (ALL(caljvc(:, :) == val_exp)) caljvc(:, :) = zero

    rfpi(:, :) = val_exp
    var_name = 'rfpi'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., rfpi, 'gather', nbp_glo, index_g)
    IF (ALL(rfpi(:, :) == val_exp)) rfpi(:, :) = zero

    upvt(:, :) = val_exp
    var_name = 'upvt'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., upvt, 'gather', nbp_glo, index_g)
    IF (ALL(upvt(:, :) == val_exp)) upvt(:, :) = zero

    utp(:, :) = val_exp
    var_name = 'utp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., utp, 'gather', nbp_glo, index_g)
    IF (ALL(utp(:, :) == val_exp)) utp(:, :) = zero

    somcour(:, :) = val_exp
    var_name = 'somcour'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somcour, 'gather', nbp_glo, index_g)
    IF (ALL(somcour(:, :) == val_exp)) somcour(:, :) = zero

    somcourdrp(:, :) = val_exp
    var_name = 'somcourdrp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somcourdrp, 'gather', nbp_glo, index_g)
    IF (ALL(somcourdrp(:, :) == val_exp)) somcourdrp(:, :) = zero

    somcourutp(:, :) = val_exp
    var_name = 'somcourutp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somcourutp, 'gather', nbp_glo, index_g)
    IF (ALL(somcourutp(:, :) == val_exp)) somcourutp(:, :) = zero

    tdevelop(:, :) = val_exp
    var_name = 'tdevelop'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., tdevelop, 'gather', nbp_glo, index_g)
    IF (ALL(tdevelop(:, :) == val_exp)) tdevelop(:, :) = zero

    somtemp(:, :) = val_exp
    var_name = 'somtemp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somtemp, 'gather', nbp_glo, index_g)
    IF (ALL(somtemp(:, :) == val_exp)) somtemp(:, :) = zero

    somcourfauche(:, :) = val_exp
    var_name = 'somcourfauche'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somcourfauche, 'gather', nbp_glo, index_g)
    IF (ALL(somcourfauche(:, :) == val_exp)) somcourfauche(:, :) = zero

    stpltger(:, :) = val_exp
    var_name = 'stpltger'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., stpltger, 'gather', nbp_glo, index_g)
    IF (ALL(stpltger(:, :) == val_exp)) THEN
       DO j= 1, nvm
          stpltger(:, j) = SP_stpltger(j)
       ENDDO
    ENDIF

    R_stlaxsen(:, :) = val_exp
    var_name = 'R_stlaxsen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stlaxsen, 'gather', nbp_glo, index_g)
    IF (ALL(R_stlaxsen(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stlaxsen(:, j) = SP_stlaxsen(j)
       ENDDO
    ENDIF


    R_stamflax(:, :) = val_exp
    var_name = 'R_stamflax'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stamflax, 'gather', nbp_glo, index_g)
    IF (ALL(R_stamflax(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stamflax(:, j) = SP_stamflax(j)
       ENDDO
    ENDIF

    R_stsenlan(:, :) = val_exp
    var_name = 'R_stsenlan'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stsenlan, 'gather', nbp_glo, index_g)
    IF (ALL(R_stsenlan(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stsenlan(:, j) = SP_stsenlan(j)
       ENDDO
    ENDIF

    stlevflo(:, :) = val_exp
    var_name = 'stlevflo'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., stlevflo, 'gather', nbp_glo, index_g)
    IF (ALL(stlevflo(:, :) == val_exp)) THEN
       DO j= 1, nvm
          stlevflo(:, j) = SP_stlevdrp(j) - SP_stflodrp(j)
       ENDDO
    ENDIF

    nflo(:, :) = val_exp
    var_name = 'nflo'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nflo_real, 'gather', nbp_glo, index_g)
    IF (ALL(nflo_real(:, :) == val_exp)) nflo_real(:, :) = zero
    nflo = INT(nflo_real)

    R_stlevdrp(:, :) = val_exp
    var_name = 'R_stlevdrp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stlevdrp, 'gather', nbp_glo, index_g)
    IF (ALL(R_stlevdrp(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stlevdrp(:, j) = SP_stlevdrp(j)
       ENDDO
    ENDIF

    R_stflodrp(:, :) = val_exp
    var_name = 'R_stflodrp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stflodrp, 'gather', nbp_glo, index_g)
    IF (ALL(R_stflodrp(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stflodrp(:, j) = SP_stflodrp(j)
       ENDDO
    ENDIF

    R_stdrpmat(:, :) = val_exp
    var_name = 'R_stdrpmat'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stdrpmat, 'gather', nbp_glo, index_g)
    IF (ALL(R_stdrpmat(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stdrpmat(:, j) = SP_stdrpmat(j)
       ENDDO
    ENDIF

    nmat(:, :) = val_exp
    var_name = 'nmat'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nmat_real, 'gather', nbp_glo, index_g)
    IF (ALL(nmat_real(:, :) == val_exp)) nmat_real(:, :) = zero
    nmat = INT(nmat_real)

    nlax(:, :) = val_exp
    var_name = 'nlax'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nlax_real, 'gather', nbp_glo, index_g)
    IF (ALL(nlax_real(:, :) == val_exp)) nlax_real(:, :) = zero
    nlax = INT(nlax_real)

    nrecbutoir(:, :) = val_exp
    var_name = 'nrecbutoir'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nrecbutoir_real, 'gather', nbp_glo, index_g)
    IF (ALL(nrecbutoir_real(:, :) == val_exp)) nrecbutoir_real(:, :) = 999.0
    nrecbutoir = INT(nrecbutoir_real)

    group(:, :) = val_exp
    var_name = 'group'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., group, 'gather', nbp_glo, index_g)
    IF (ALL(group(:, :) == val_exp)) group(:, :) = zero

    ndebdes(:, :) = val_exp
    var_name = 'ndebdes'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ndebdes_real, 'gather', nbp_glo, index_g)
    IF (ALL(ndebdes_real(:, :) == val_exp)) ndebdes_real(:, :) = zero
    ndebdes = INT(ndebdes_real)

    R_stdrpdes(:, :) = val_exp
    var_name = 'R_stdrpdes'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stdrpdes, 'gather', nbp_glo, index_g)
    IF (ALL(R_stdrpdes(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stdrpdes(:, j) = SP_stdrpdes(j)
       ENDDO
    ENDIF

    densite(:, :) = val_exp
    var_name = 'densite'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., densite, 'gather', nbp_glo, index_g)
    IF (ALL(densite(:, :) == val_exp)) densite(:, :) = zero

    densitelev(:, :) = val_exp
    var_name = 'densitelev'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., densitelev, 'gather', nbp_glo, index_g)
    IF (ALL(densitelev(:, :) == val_exp)) densitelev(:, :) = zero

    coeflev(:, :) = val_exp
    var_name = 'coeflev'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., coeflev, 'gather', nbp_glo, index_g)
    IF (ALL(coeflev(:, :) == val_exp)) coeflev(:, :) = un

    densiteger(:, :) = val_exp
    var_name = 'densiteger'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., densiteger, 'gather', nbp_glo, index_g)
    IF (ALL(densiteger(:, :) == val_exp)) densiteger(:, :) = zero

    somelong(:, :) = val_exp
    var_name = 'somelong'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somelong, 'gather', nbp_glo, index_g)
    IF (ALL(somelong(:, :) == val_exp)) somelong(:, :) = zero

    somger(:, :) = val_exp
    var_name = 'somger'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somger, 'gather', nbp_glo, index_g)
    IF (ALL(somger(:, :) == val_exp)) somger(:, :) = zero

    humectation_real(:, :) = val_exp
    var_name = 'humectation'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                .TRUE., humectation_real, 'gather', nbp_glo, index_g)
    IF (ALL(humectation_real(:, :) == val_exp)) humectation_real(:, :) = zero
    WHERE (humectation_real(:, :) == un)
       humectation = .TRUE.
    ELSEWHERE
       humectation = .FALSE.
    ENDWHERE

    nbjhumec(:, :) = val_exp
    var_name = 'nbjhumec'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nbjhumec_real, 'gather', nbp_glo, index_g)
    IF (ALL(nbjhumec_real(:, :) == val_exp)) nbjhumec_real(:, :) = zero
    nbjhumec = INT(nbjhumec_real)

    somtemphumec(:, :) = val_exp
    var_name = 'somtemphumec'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somtemphumec, 'gather', nbp_glo, index_g)
    IF (ALL(somtemphumec(:, :) == val_exp)) somtemphumec(:, :) = zero

    stpltlev(:, :) = val_exp
    var_name = 'stpltlev'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., stpltlev, 'gather', nbp_glo, index_g)
    IF (ALL(stpltlev(:, :) == val_exp)) stpltlev(:, :) = zero

    namf(:, :) = val_exp
    var_name = 'namf'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., namf_real, 'gather', nbp_glo, index_g)
    IF (ALL(namf_real(:, :) == val_exp)) namf_real(:, :) = zero
    namf = INT(namf_real)

    stmatrec(:, :) = val_exp
    var_name = 'stmatrec'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., stmatrec, 'gather', nbp_glo, index_g)
    IF (ALL(stmatrec(:, :) == val_exp)) stmatrec(:, :) = zero

    tustress(:, :) = val_exp
    var_name = 'tustress'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., tustress, 'gather', nbp_glo, index_g)
    IF (ALL(tustress(:, :) == val_exp)) tustress(:, :) = 1.0

    lai(:, :) = val_exp
    var_name = 'lai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., lai, 'gather', nbp_glo, index_g)
    IF (ALL(lai(:, :) == val_exp)) lai(:, :) = zero

    somfeuille(:, :) = val_exp
    var_name = 'somfeuille'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somfeuille, 'gather', nbp_glo, index_g)
    IF (ALL(somfeuille(:, :) == val_exp)) somfeuille(:, :) = zero

    pdlai(:, :) = val_exp
    var_name = 'pdlai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdlai, 'gather', nbp_glo, index_g)
    IF (ALL(pdlai(:, :) == val_exp)) pdlai(:, :) = zero

    nbfeuille(:, :) = val_exp
    var_name = 'nbfeuille'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nbfeuille_real, 'gather', nbp_glo, index_g)
    IF (ALL(nbfeuille_real(:, :) == val_exp)) nbfeuille_real(:, :) = zero
    nbfeuille = INT(nbfeuille_real)

    reajust(:, :) = val_exp
    var_name = 'reajust'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., reajust, 'gather', nbp_glo, index_g)
    IF (ALL(reajust(:, :) == val_exp)) reajust(:, :) = zero

    ulai(:, :) = val_exp
    var_name = 'ulai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ulai, 'gather', nbp_glo, index_g)
    IF (ALL(ulai(:, :) == val_exp)) ulai(:, :) = zero

    pdulai(:, :) = val_exp
    var_name = 'pdulai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdulai, 'gather', nbp_glo, index_g)
    IF (ALL(pdulai(:, :) == val_exp)) pdulai(:, :) = zero

    efdensite(:, :) = val_exp
    var_name = 'efdensite'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., efdensite, 'gather', nbp_glo, index_g)
    IF (ALL(efdensite(:, :) == val_exp)) efdensite(:, :) = zero

    tempeff(:, :) = val_exp
    var_name = 'tempeff'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., tempeff, 'gather', nbp_glo, index_g)
    IF (ALL(tempeff(:, :) == val_exp)) tempeff(:, :) = zero

    nstopfeuille(:, :) = val_exp
    var_name = 'nstopfeuille'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nstopfeuille_real, 'gather', nbp_glo, index_g)
    IF (ALL(nstopfeuille_real(:, :) == val_exp)) nstopfeuille_real(:, :) = zero
    nstopfeuille = INT(nstopfeuille_real)

    deltai(:, :) = val_exp
    var_name = 'deltai'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., deltai, 'gather', nbp_glo, index_g)
    IF (ALL(deltai(:, :) == val_exp)) deltai(:, :) = zero

    vmax(:, :) = val_exp
    var_name = 'vmax'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., vmax, 'gather', nbp_glo, index_g)
    IF (ALL(vmax(:, :) == val_exp)) vmax(:, :) = zero

    nsen(:, :) = val_exp
    var_name = 'nsen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsen_real, 'gather', nbp_glo, index_g)
    IF (ALL(nsen_real(:, :) == val_exp)) nsen_real(:, :) = zero
    nsen = INT(nsen_real)


    laisen(:, :) = val_exp
    var_name = 'laisen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., laisen, 'gather', nbp_glo, index_g)
    IF (ALL(laisen(:, :) == val_exp)) laisen(:, :) = zero

    pdlaisen(:, :) = val_exp
    var_name = 'pdlaisen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdlaisen, 'gather', nbp_glo, index_g)
    IF (ALL(pdlaisen(:, :) == val_exp)) pdlaisen(:, :) = zero

    dltaisenat(:, :) = val_exp
    var_name = 'dltaisenat'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., dltaisenat, 'gather', nbp_glo, index_g)
    IF (ALL(dltaisenat(:, :) == val_exp)) dltaisenat(:, :) = zero

    nsencour(:, :) = val_exp
    var_name = 'nsencour'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nsencour_real, 'gather', nbp_glo, index_g)
    IF (ALL(nsencour_real(:, :) == val_exp)) nsencour_real(:, :) = zero
    nsencour = INT(nsencour_real)

    dltamsen(:, :) = val_exp
    var_name = 'dltamsen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., dltamsen, 'gather', nbp_glo, index_g)
    IF (ALL(dltamsen(:, :) == val_exp)) dltamsen(:, :) = zero

    dltaisen(:, :) = val_exp
    var_name = 'dltaisen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., dltaisen, 'gather', nbp_glo, index_g)
    IF (ALL(dltaisen(:, :) == val_exp)) dltaisen(:, :) = zero

    fgellev(:, :) = val_exp
    var_name = 'fgellev'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., fgellev, 'gather', nbp_glo, index_g)
    IF (ALL(fgellev(:, :) == val_exp)) fgellev(:, :) = un

    gelee_real(:, :) = val_exp
    var_name = 'gelee'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., gelee_real, 'gather', nbp_glo, index_g)
    IF (ALL(gelee_real(:, :) == val_exp)) gelee_real(:, :) = zero
    WHERE (gelee_real(:,:) == un)
       gelee = .TRUE.
    ELSEWHERE
       gelee = .FALSE.
    ENDWHERE

    fstressgel(:, :) = val_exp
    var_name = 'fstressgel'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., fstressgel, 'gather', nbp_glo, index_g)
    IF (ALL(fstressgel(:, :) == val_exp)) fstressgel(:, :) = zero

    R_stlevamf(:, :) = val_exp
    var_name = 'R_stlevamf'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., R_stlevamf, 'gather', nbp_glo, index_g)
    IF (ALL(R_stlevamf(:, :) == val_exp)) THEN
       DO j= 1, nvm
          R_stlevamf(:, j) = SP_stlevamf(j)
       ENDDO
    ENDIF

    dernier_n(:, :) = val_exp
    var_name = 'dernier_n'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., dernier_n_real, 'gather', nbp_glo, index_g)
    IF (ALL(dernier_n_real(:, :) == val_exp)) dernier_n_real(:, :) = zero
    dernier_n = INT(dernier_n_real)

    durvieI(:, :) = val_exp
    var_name = 'durvieI'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., durvieI, 'gather', nbp_glo, index_g)
    IF (ALL(durvieI(:, :) == val_exp)) durvieI(:, :) = zero

    durvie(:, :) = val_exp
    var_name = 'durvie'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., durvie, 'gather', nbp_glo, index_g)
    IF (ALL(durvie(:, :) == val_exp)) durvie(:, :) = zero

    ndebsen(:, :) = val_exp
    var_name = 'ndebsen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ndebsen_real, 'gather', nbp_glo, index_g)
    IF (ALL(ndebsen_real(:, :) == val_exp)) ndebsen_real(:, :) = zero
    ndebsen = INT(ndebsen_real)

    somsenreste(:, :) = val_exp
    var_name = 'somsenreste'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somsenreste, 'gather', nbp_glo, index_g)
    IF (ALL(somsenreste(:, :) == val_exp)) somsenreste(:, :) = zero

    humrel(:, :) = val_exp
    var_name = 'humrel'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., humrel, 'gather', nbp_glo, index_g)
    IF (ALL(humrel(:, :) == val_exp)) humrel(:, :) = zero

    swfac(:, :) = val_exp
    var_name = 'swfac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., swfac, 'gather', nbp_glo, index_g)
    IF (ALL(swfac(:, :) == val_exp)) swfac(:, :) = un

    turfac(:, :) = val_exp
    var_name = 'turfac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., turfac, 'gather', nbp_glo, index_g)
    IF (ALL(turfac(:, :) == val_exp)) turfac(:, :) = un

    senfac(:, :) = val_exp
    var_name = 'senfac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., senfac, 'gather', nbp_glo, index_g)
    IF (ALL(senfac(:, :) == val_exp)) senfac(:, :) = un
  

    mafeuiljaune(:, :) = val_exp
    var_name = 'mafeuiljaune'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., mafeuiljaune, 'gather', nbp_glo, index_g)
    IF (ALL(mafeuiljaune(:, :) == val_exp)) mafeuiljaune(:, :) = un
    
    msneojaune(:, :) = val_exp
    var_name = 'msneojaune'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., msneojaune, 'gather', nbp_glo, index_g)
    IF (ALL(msneojaune(:, :) == val_exp)) msneojaune(:, :) = un

    box_ndays(:,:,:) = 0
    box_lai(:,:,:) = 0.
    box_lairem(:,:,:) = 0.
    box_tdev(:,:,:) = 0.
    box_biom(:,:,:) = 0.
    box_biomrem(:,:,:) = 0.
    box_durage(:,:,:) = 0.
    box_somsenbase(:,:,:) = 0.
    DO k=1,nboxmax
        WRITE(box_str,'(I2)') k
        IF (k<10) box_str(1:1) = '0'
        var_name = 'box_ndays_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
             &              .TRUE., boxtemp, 'gather', nbp_glo, index_g)
        box_ndays(:,:,k) = INT(boxtemp)
        IF (ALL(boxtemp(:,:) == val_exp)) box_ndays(:,:,k) = 0

        var_name = 'box_lai_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_lai(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_lai(:,:,k) == val_exp)) box_lai(:,:,k) = 0.

        var_name = 'box_lairem_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_lairem(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_lairem(:,:,k) == val_exp)) box_lairem(:,:,k) = 0.

        var_name = 'box_tdev_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_tdev(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_tdev(:,:,k) == val_exp)) box_tdev(:,:,k) = 0.

        var_name = 'box_biom_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_biom(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_biom(:,:,k) == val_exp)) box_biom(:,:,k) = 0.

        var_name = 'box_biomrem_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_biomrem(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_biomrem(:,:,k) == val_exp)) box_biomrem(:,:,k) = 0.

        var_name = 'box_durage_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_durage(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_durage(:,:,k) == val_exp)) box_durage(:,:,k) = 0.

        var_name = 'box_somsenbase_'//box_str(1:LEN_TRIM(box_str))
        CALL restget_p(rest_id_stomate, var_name, nbp_glo, nvm,   1, itime, &
            &               .TRUE., box_somsenbase(:,:,k), 'gather', nbp_glo, index_g)
        IF (ALL(box_somsenbase(:,:,k) == val_exp)) box_somsenbase(:,:,k) = 0.

    ENDDO

 
    ! STICS:: CARBON ALLOCATION
   
    
    v_dltams(:,:,:) = val_exp
    DO k=1,vlength
       WRITE(part_str,'(I2)') k
       IF (k < 10) part_str(1:1) = '0'
       var_name = 'v_dltams_'//part_str(1:LEN_TRIM(part_str))
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
            &                .TRUE., v_dltams(:,:,k), 'gather', nbp_glo, index_g)
       IF (ALL(v_dltams(:,:,k) == val_exp)) &
            &       v_dltams(:,:,k) = zero
    ENDDO

    fgelflo(:, :) = val_exp
    var_name = 'fgelflo'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., fgelflo, 'gather', nbp_glo, index_g)
    IF (ALL(fgelflo(:, :) == val_exp)) fgelflo(:, :) = un

    pdircarb(:, :) = val_exp
    var_name = 'pdircarb'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdircarb, 'gather', nbp_glo, index_g)
    IF (ALL(pdircarb(:, :) == val_exp)) pdircarb(:, :) = zero

    ircarb(:, :) = val_exp
    var_name = 'ircarb'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ircarb, 'gather', nbp_glo, index_g)
    IF (ALL(ircarb(:, :) == val_exp)) ircarb(:, :) = zero

    nbgrains(:, :) = val_exp
    var_name = 'nbgrains'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nbgrains, 'gather', nbp_glo, index_g)
    IF (ALL(nbgrains(:, :) == val_exp)) nbgrains(:, :) = zero

    pgrain(:, :) = val_exp
    var_name = 'pgrain'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pgrain, 'gather', nbp_glo, index_g)
    IF (ALL(pgrain(:, :) == val_exp)) pgrain(:, :) = zero

    vitmoy(:, :) = val_exp
    var_name = 'vitmoy'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., vitmoy, 'gather', nbp_glo, index_g)
    IF (ALL(vitmoy(:, :) == val_exp)) vitmoy(:, :) = zero

    nbgraingel(:, :) = val_exp
    var_name = 'nbgraingel'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nbgraingel, 'gather', nbp_glo, index_g)
    IF (ALL(nbgraingel(:, :) == val_exp)) nbgraingel(:, :) = zero

    pgraingel(:, :) = val_exp
    var_name = 'pgraingel'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pgraingel, 'gather', nbp_glo, index_g)
    IF (ALL(pgraingel(:, :) == val_exp)) pgraingel(:, :) = zero

    dltags(:, :) = val_exp
    var_name = 'dltags'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., dltags, 'gather', nbp_glo, index_g)
    IF (ALL(dltags(:, :) == val_exp)) dltags(:, :) = zero

    ftempremp(:, :) = val_exp
    var_name = 'ftempremp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., ftempremp, 'gather', nbp_glo, index_g)
    IF (ALL(ftempremp(:, :) == val_exp)) ftempremp(:, :) = zero

    magrain(:, :) = val_exp
    var_name = 'magrain'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., magrain, 'gather', nbp_glo, index_g)
    IF (ALL(magrain(:, :) == val_exp)) magrain(:, :) = zero

    pdmagrain(:, :) = val_exp
    var_name = 'pdmagrain'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdmagrain, 'gather', nbp_glo, index_g)
    IF (ALL(pdmagrain(:, :) == val_exp)) pdmagrain(:, :) = zero

    nbj0remp(:, :) = val_exp
    var_name = 'nbj0remp'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nbj0remp_real, 'gather', nbp_glo, index_g)
    IF (ALL(nbj0remp_real(:, :) == val_exp)) nbj0remp_real(:, :) = zero
    nbj0remp = INT(nbj0remp_real)

   
    !nbj0remp(:, :) = val_exp
    !var_name = 'nbj0remp'
    !CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &              .TRUE., nbj0remp, 'gather', nbp_glo, index_g)
    !IF (ALL(nbj0remp(:, :) == val_exp)) nbj0remp(:, :) = zero

    pdsfruittot(:, :) = val_exp
    var_name = 'pdsfruittot'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., pdsfruittot, 'gather', nbp_glo, index_g)
    IF (ALL(pdsfruittot(:, :) == val_exp)) pdsfruittot(:, :) = zero

    repracmax(:, :) = val_exp
    var_name = 'repracmax'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., repracmax, 'gather', nbp_glo, index_g)
    IF (ALL(repracmax(:, :) == val_exp)) repracmax(:, :) = zero

    repracmin(:, :) = val_exp
    var_name = 'repracmin'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., repracmin, 'gather', nbp_glo, index_g)
    IF (ALL(repracmin(:, :) == val_exp)) repracmin(:, :) = zero

    kreprac(:, :) = val_exp
    var_name = 'kreprac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., kreprac, 'gather', nbp_glo, index_g)
    IF (ALL(kreprac(:, :) == val_exp)) kreprac(:, :) = zero

    somtemprac(:, :) = val_exp
    var_name = 'somtemprac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., somtemprac, 'gather', nbp_glo, index_g)
    IF (ALL(somtemprac(:, :) == val_exp)) somtemprac(:, :) = zero

    urac(:, :) = val_exp
    var_name = 'urac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., urac, 'gather', nbp_glo, index_g)
    IF (ALL(urac(:, :) == val_exp)) urac(:, :) = zero

    reprac(:, :) = val_exp
    var_name = 'reprac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., reprac, 'gather', nbp_glo, index_g)
    IF (ALL(reprac(:, :) == val_exp)) reprac(:, :) = zero

    c_reserve(:, :) = val_exp
    var_name = 'c_reserve'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., c_reserve, 'gather', nbp_glo, index_g)
    IF (ALL(c_reserve(:, :) == val_exp)) c_reserve(:, :) = zero
    !c_reserve(:, :) = val_exp


    nstoprac(:, :) = val_exp
    var_name = 'nstoprac'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., nstoprac_real, 'gather', nbp_glo, index_g)
    IF (ALL(nstoprac_real(:, :) == val_exp)) nstoprac_real(:, :) = zero
    nstoprac = INT(nstoprac_real)


    var_name = 'c_leafb'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., c_leafb, 'gather', nbp_glo, index_g)
    IF (ALL(c_leafb(:, :) == val_exp)) c_leafb(:, :) = zero

    gslen(:, :) = val_exp
    var_name = 'gslen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., gslen_real, 'gather', nbp_glo, index_g)
    IF (ALL(gslen_real(:, :) == val_exp)) gslen_real(:, :) = zero
    gslen = INT(gslen_real)

    drylen(:, :) = val_exp
    var_name = 'drylen'
    CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &              .TRUE., drylen_real, 'gather', nbp_glo, index_g)
    IF (ALL(drylen_real(:, :) == val_exp)) drylen_real(:, :) = zero
    drylen = INT(drylen_real)


!!!!! xuhui
 
    !-
    ! 19. Spinup
    !-
!    IF (spinup_analytic) THEN

       IF (is_root_prc) THEN
          temp_global_years(1) = val_exp
          var_name = 'Global_years'
          CALL restget (rest_id_stomate, var_name, 1 ,1  , 1, itime, &
               &                .TRUE., temp_global_years)
          IF(temp_global_years(1) == val_exp) temp_global_years(1) = zero
          global_years = INT(temp_global_years(1))
       ENDIF
       CALL bcast(global_years)

       nbp_accu(:) = val_exp
       var_name = 'nbp_sum'
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
            &              .TRUE., nbp_accu, 'gather', nbp_glo, index_g)
       IF (ALL(nbp_accu(:) == val_exp)) nbp_accu(:) = zero    

       nbp_flux(:) = val_exp
       var_name = 'nbp_flux'
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
            &              .TRUE., nbp_flux, 'gather', nbp_glo, index_g)
       IF (ALL(nbp_flux(:) == val_exp)) nbp_flux(:) = zero     

       !-
       ok_equilibrium_real(:) = val_exp
       var_name = 'ok_equilibrium'
       CALL restget_p (rest_id_stomate, var_name, nbp_glo , 1  , 1, itime, &
            &                .TRUE., ok_equilibrium_real,'gather', nbp_glo, index_g)
       IF (ALL(ok_equilibrium_real(:) == val_exp)) ok_equilibrium_real(:) = zero
       WHERE(ok_equilibrium_real(:) >= 0.5) 
          ok_equilibrium = .TRUE.
       ELSEWHERE
          ok_equilibrium = .FALSE.
       ENDWHERE

       MatrixV(:,:,:,:) = val_exp
       DO k = 1,nbpools
          DO j = 1,nbpools
             WRITE(part_str,'(I2)') k
             IF (k < 10) part_str(1:1) = '0'             
             var_name = 'MatrixV_'//part_str(1:LEN_TRIM(part_str))//'_'//TRIM(pools_str(j))
             CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm , 1, itime, &
                  &                     .TRUE., MatrixV(:,:,k,j), 'gather', nbp_glo, index_g)
          ENDDO
       ENDDO
       ! If nothing is found in the restart file, we initialize each submatrix by identity
       IF (ALL(MatrixV(:,:,:,:) == val_exp))  THEN 
          MatrixV(:,:,:,:) = zero
          DO l = 1,nbpools
             MatrixV(:,:,l,l) = un
          END DO
       END IF

       VectorU(:,:,:)  = val_exp
       var_name = 'Vector_U'
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, nbpools, itime, &
         &     .TRUE., VectorU, 'gather', nbp_glo, index_g)
       IF (ALL(VectorU == val_exp))  VectorU(:,:,:) = zero
       
       previous_stock(:,:,:)  = val_exp
       var_name = 'previous_stock'
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, nbpools, itime, &
            &     .TRUE., previous_stock, 'gather', nbp_glo, index_g)
       IF (ALL(previous_stock == val_exp))  previous_stock = undef_sechiba
       
       current_stock(:,:,:)  = val_exp
       var_name = 'current_stock'
       CALL restget_p (rest_id_stomate, var_name, nbp_glo, nvm, nbpools, itime, &
            &     .TRUE., current_stock, 'gather', nbp_glo, index_g)
       IF (ALL(current_stock == val_exp))  current_stock(:,:,:) = zero
 
         
!    ENDIF ! spinup_matrix_method

    ! Read assim_param from restart file. The initialization of assim_param will 
    ! be done in stomate_var_init if the variable is not in the restart file.
    assim_param(:,:,:)  = val_exp
    var_name = 'assim_param'
    CALL restget_p &
         &    (rest_id_stomate, var_name, nbp_glo, nvm, npco2, itime, &
         &     .TRUE., assim_param, 'gather', nbp_glo, index_g)
 
    IF (printlev >= 4) WRITE(numout,*) 'Leaving readstart'
    !-----------------------
  END SUBROUTINE readstart
  !-
  !===
  !-
  SUBROUTINE writerestart &
       & (npts, index, dt_days, date, &
       &  ind, adapted, regenerate, moiavail_daily, gdd_init_date, litterhum_daily, &
       &  t2m_daily, t2m_min_daily, &
       !spitfire
       &  t2m_max_daily, wspeed_daily, &
       !endspit
       &  tsurf_daily, tsoil_daily, &
       &  soilhum_daily, precip_daily, gpp_daily, npp_daily, &
       &  turnover_daily, moiavail_month, moiavail_week, &
       &  t2m_longterm, tau_longterm, t2m_month, t2m_week, &
       &  tsoil_month, soilhum_month, fireindex, firelitter, &
       &  maxmoiavail_lastyear, maxmoiavail_thisyear, &
       &  minmoiavail_lastyear, minmoiavail_thisyear, &
       &  maxgppweek_lastyear, maxgppweek_thisyear, &
       &  gdd0_lastyear, gdd0_thisyear, precip_lastyear, precip_thisyear, &
       &  gdd_m5_dormance, gdd_from_growthinit, gdd_midwinter, ncd_dormance, ngd_minus5, &
       &  PFTpresent, npp_longterm, lm_lastyearmax, lm_thisyearmax, &
       &  maxfpc_lastyear, maxfpc_thisyear, &
       &  turnover_longterm, gpp_week, biomass, resp_maint_part, &
       &  leaf_age, leaf_frac, senescence, when_growthinit, age, &
       &  resp_hetero, resp_maint, resp_growth, co2_fire, co2_to_bm_dgvm, &
       &  veget_lastlight, everywhere, need_adjacent, RIP_time, &
       &  time_hum_min, hum_min_dormance, &
       &  litterpart, litter, dead_leaves, &
       &  carbon, lignin_struc, & 
       !spitfire
       &  ni_acc,fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr, & 
       !endspit
       & turnover_time, &
       &  prod10,prod100 ,flux10, flux100, &
       &  convflux, cflux_prod10, cflux_prod100, bm_to_litter, carb_mass_total, &
       &  Tseason, Tseason_length, Tseason_tmp, &
       &  Tmin_spring_time, begin_leaves, onset_date, &
       &  global_years, ok_equilibrium, nbp_accu, nbp_flux, &
       &  MatrixV, VectorU, previous_stock, current_stock,&
!!!! crops
       &  f_crop_recycle,in_cycle, f_sen_lai, st2m_max_daily, wut_cm_daily, wus_cm_daily, evapot_daily, pdbiomass, pdmasec, &
       &  masecveg, masec, dltams, gdh_daily, phoi, onarretesomcourdrp,  &
       &  nsendltams, nsendltai, nsenpfeuilverte, nsendurvie, nsenndurvie, densiteequiv, &
       &  nplt, tursla, ssla, pfeuilverte, bsenlai, &
       &  zrac, nrec, nlan, tcult, udevair, udevcult, ndrp, rfvi, nlev, nger, etatvernal, &
       &  caljvc, rfpi, upvt, utp, somcour, somcourdrp, somcourutp, tdevelop, somtemp, &
       &  somcourfauche, stpltger, R_stamflax, R_stlaxsen, R_stsenlan, stlevflo, nflo, &
       &  R_stlevdrp, R_stflodrp, R_stdrpmat, nmat, nlax, nrecbutoir, group, ndebdes, R_stdrpdes, densite, &
       &  densitelev, coeflev, densiteger, somelong, somger, humectation, nbjhumec, &
       &  somtemphumec, stpltlev, namf, stmatrec, tustress, lai, somfeuille, pdlai, &
       &  nbfeuille, reajust, ulai, pdulai, efdensite, tempeff, nstopfeuille, deltai, vmax, nsen, &
       &  laisen, pdlaisen, dltaisenat, nsencour, dltamsen, dltaisen, fgellev, &
       &  gelee, fstressgel, R_stlevamf, dernier_n, durvieI, durvie, ndebsen, somsenreste, &
       &  humrel, swfac, turfac, senfac,mafeuiljaune, msneojaune, &
       &  v_dltams, fgelflo, pdircarb, ircarb, nbgrains, pgrain, vitmoy, nbgraingel, pgraingel, &
       &  dltags, ftempremp, magrain, pdmagrain, nbj0remp, pdsfruittot, repracmax, repracmin, &
       &  kreprac, somtemprac, urac, reprac, nstoprac, c_reserve, c_leafb, gslen, drylen,  &
       &  nboxmax, box_ndays, box_lai, box_lairem, box_tdev, box_biom, box_biomrem, box_durage, box_somsenbase, &
       &  cyc_num, cyc_num_tot,rot_cmd_store, plantdate, plantdate_now, &
!!!! xuhui
       &  deepC_a, deepC_s, deepC_p, O2_soil, CH4_soil, O2_snow, CH4_snow, &
       &  thawed_humidity, depth_organic_soil, altmax, fixed_cryoturbation_depth, & !pss+
       &  uo_0, uold2_0, uo_wet1, uold2_wet1, uo_wet2, uold2_wet2, uo_wet3, uold2_wet3, &
       &  uo_wet4, uold2_wet4, tsurf_year, &!) !pss:-
!JCADD
       &  wshtotsum, sr_ugb, sla_calc, nb_ani, grazed_frac, &
       &  import_yield, t2m_14, litter_not_avail, assim_param)
!ENDJCADD
    !---------------------------------------------------------------------
    !- write restart file
    !---------------------------------------------------------------------
    !-
    ! 0 declarations
    !-
    ! 0.1 input
    !-
    ! Domain size
    INTEGER(i_std),INTENT(in) :: npts
    ! Indices of the points on the map
    INTEGER(i_std),DIMENSION(npts),INTENT(in) :: index
    ! time step of STOMATE in days
    REAL(r_std),INTENT(in) :: dt_days
    ! date (d)
    INTEGER(i_std),INTENT(in) :: date
    ! density of individuals (1/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: ind
    ! Winter too cold? between 0 and 1
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: adapted
    ! Winter sufficiently cold? between 0 and 1
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: regenerate
    ! daily moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: moiavail_daily
    ! gdd init date
    REAL(r_std),DIMENSION(npts,2),INTENT(in) :: gdd_init_date
    ! daily litter humidity
    REAL(r_std),DIMENSION(npts),INTENT(in) :: litterhum_daily
    ! daily 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: t2m_daily
    ! daily minimum 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: t2m_min_daily
    !spitfire
    ! daily maximum 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(out) :: t2m_max_daily
    ! daily wind speed(m/s)
    REAL(r_std),DIMENSION(npts),INTENT(out)      :: wspeed_daily
    !endspit
    ! daily surface temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: tsurf_daily
    ! daily soil temperatures (K)
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(in) :: tsoil_daily
    ! daily soil humidity
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(in) :: soilhum_daily
    ! daily precipitations (mm/day) (for phenology)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: precip_daily
    ! daily gross primary productivity (gC/m**2/day)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: gpp_daily
    ! daily net primary productivity (gC/m**2/day)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: npp_daily
    ! daily turnover rates (gC/m**2/day)
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(in) :: turnover_daily
    ! "monthly" moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: moiavail_month
    ! "weekly" moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: moiavail_week
    ! "long term" 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: t2m_longterm
    ! "tau_longterm"
    REAL(r_std), INTENT(IN)             :: tau_longterm
    ! "monthly" 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: t2m_month
    ! "seasonal" 2 meter temperatures (K) 
    REAL(r_std),DIMENSION(npts),INTENT(in)      :: Tseason
    ! temporary variable to calculate Tseason
    REAL(r_std),DIMENSION(npts),INTENT(in)      :: Tseason_length
    ! temporary variable to calculate Tseason
    REAL(r_std),DIMENSION(npts),INTENT(in)      :: Tseason_tmp

    REAL(r_std),DIMENSION(npts,nvm),INTENT(in)  :: Tmin_spring_time
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in)  :: onset_date
    LOGICAL,DIMENSION(npts,nvm),INTENT(in)      :: begin_leaves

    ! "weekly" 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: t2m_week
    ! "monthly" soil temperatures (K)
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(in) :: tsoil_month
    ! "monthly" soil humidity
    REAL(r_std),DIMENSION(npts,nbdl),INTENT(in) :: soilhum_month
    ! Probability of fire
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: fireindex
    ! Longer term total litter above the ground, gC/m**2 of ground
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: firelitter
    ! last year's maximum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: maxmoiavail_lastyear
    ! this year's maximum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: maxmoiavail_thisyear
    ! last year's minimum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: minmoiavail_lastyear
    ! this year's minimum moisture availability
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: minmoiavail_thisyear
    ! last year's maximum weekly GPP
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: maxgppweek_lastyear
    ! this year's maximum weekly GPP
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: maxgppweek_thisyear
    ! last year's annual GDD0
    REAL(r_std),DIMENSION(npts),INTENT(in) :: gdd0_lastyear
    ! this year's annual GDD0
    REAL(r_std),DIMENSION(npts),INTENT(in) :: gdd0_thisyear
    ! last year's annual precipitation (mm/year)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: precip_lastyear
    ! this year's annual precipitation (mm/year)
    REAL(r_std),DIMENSION(npts),INTENT(in) :: precip_thisyear
    ! growing degree days, threshold -5 deg C (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: gdd_m5_dormance
    ! growing degree days, from begin of season (crops)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: gdd_from_growthinit
    ! growing degree days since midwinter (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: gdd_midwinter
    ! number of chilling days since leaves were lost (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: ncd_dormance
    ! number of growing days, threshold -5 deg C (for phenology)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: ngd_minus5
    ! PFT exists (equivalent to fpc_max > 0 for natural PFTs)
    LOGICAL,DIMENSION(npts,nvm),INTENT(in) :: PFTpresent
    ! "long term" net primary productivity (gC/m**2/year)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: npp_longterm
    ! last year's maximum leaf mass, for each PFT (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: lm_lastyearmax
    ! this year's maximum leaf mass, for each PFT (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: lm_thisyearmax
    ! last year's maximum fpc for each natural PFT, on ground
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: maxfpc_lastyear
    ! this year's maximum fpc for each PFT,
    ! on *total* ground (see stomate_season)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: maxfpc_thisyear
    ! "long term" turnover rate (gC/m**2/year)
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(in) :: turnover_longterm
    ! "weekly" GPP (gC/day/(m**2 covered)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: gpp_week
    ! biomass (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(in) :: biomass
    ! maintenance respiration (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm,nparts),INTENT(in) :: resp_maint_part
    ! leaf age (days)
    REAL(r_std),DIMENSION(npts,nvm,nleafages),INTENT(in) :: leaf_age
    ! fraction of leaves in leaf age class
    REAL(r_std),DIMENSION(npts,nvm,nleafages),INTENT(in) :: leaf_frac
    ! is the plant senescent ?
    ! (only for deciduous trees - carbohydrate reserve)
    LOGICAL,DIMENSION(npts,nvm),INTENT(in) :: senescence
    ! how many days ago was the beginning of the growing season
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: when_growthinit
    ! mean age (years)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: age
    ! heterotrophic respiration (gC/day/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: resp_hetero
    ! maintenance respiration (gC/day/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: resp_maint
    ! growth respiration (gC/day/m**2)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: resp_growth
    ! carbon emitted into the atmosphere by fire (living and dead biomass)
    ! (in gC/m**2/time step)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: co2_fire
    ! biomass uptaken (gC/(m**2 of total ground)/day)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: co2_to_bm_dgvm
    ! vegetation fractions (on ground) after last light competition
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: veget_lastlight
    ! is the PFT everywhere in the grid box or very localized
    ! (after its introduction)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: everywhere
    ! in order for this PFT to be introduced,
    ! does it have to be present in an adjacent grid box?
    LOGICAL,DIMENSION(npts,nvm),INTENT(in) :: need_adjacent
    ! How much time ago was the PFT eliminated for the last time (y)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: RIP_time
    ! time elapsed since strongest moisture availability (d)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: time_hum_min
    ! minimum moisture during dormance
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: hum_min_dormance
    ! fraction of litter above the ground belonging to different PFTs
    REAL(r_std),DIMENSION(npts,nvm,nlitt),INTENT(in) :: litterpart
    ! metabolic and structural litter, above and below ground (gC/m**2)
    REAL(r_std),DIMENSION(npts,nlitt,nvm,nlevs,nelements),INTENT(in) :: litter
    ! dead leaves on ground, per PFT, metabolic and structural,
    ! in gC/(m**2 of ground)
    REAL(r_std),DIMENSION(npts,nvm,nlitt),INTENT(in) :: dead_leaves
    ! carbon pool: active, slow, or passive, (gC/m**2)
    REAL(r_std),DIMENSION(npts,ncarb,nvm),INTENT(in) :: carbon
    ! ratio Lignine/Carbon in structural litter, above and below ground, (gC/m**2)
    REAL(r_std),DIMENSION(npts,nvm,nlevs),INTENT(in) :: lignin_struc
    ! turnover_time of leaves
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in) :: turnover_time

    ! For Spinup matrix resolution
    INTEGER(i_std), INTENT(in) :: global_years   
    LOGICAL, DIMENSION(npts), INTENT(in) :: ok_equilibrium
    REAL(r_std), DIMENSION(npts), INTENT(in) :: nbp_accu  !! Accumulated Net Biospheric Production over the year 
    REAL(r_std), DIMENSION(npts), INTENT(in) :: nbp_flux  !! Net Biospheric Production over the year 
    !-
    REAL(r_std), DIMENSION(npts,nvm,nbpools,nbpools), INTENT(in) :: MatrixV
    REAL(r_std), DIMENSION(npts,nvm,nbpools), INTENT(in) :: VectorU
    REAL(r_std), DIMENSION(npts,nvm,nbpools), INTENT(in) :: previous_stock
    REAL(r_std), DIMENSION(npts,nvm,nbpools), INTENT(in) :: current_stock 
    ! Permafrost carbon related
    real(r_std), DIMENSION(npts,ndeep,nvm),INTENT(inout) :: deepC_a
    real(r_std), DIMENSION(npts,ndeep,nvm),intent(inout) :: deepC_s
    real(r_std), DIMENSION(npts,ndeep,nvm),intent(inout) :: deepC_p
    real(r_std), DIMENSION(npts,ndeep,nvm),intent(inout) :: O2_soil
    real(r_std), DIMENSION(npts,ndeep,nvm),intent(inout) :: CH4_soil
    real(r_std), DIMENSION(npts,nsnow,nvm),intent(inout) :: O2_snow
    real(r_std), DIMENSION(npts,nsnow,nvm),intent(inout) :: CH4_snow
    real(r_std), DIMENSION(npts),intent(inout)           :: thawed_humidity
    real(r_std), DIMENSION(npts),intent(inout)           :: depth_organic_soil
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: altmax
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: fixed_cryoturbation_depth
    REAL(r_std), DIMENSION(npts,nvm,npco2),   INTENT(in) :: assim_param
    ! Wetland CH4 methane
    !pss:+
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in) :: uo_0
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in)  :: uold2_0
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in) :: uo_wet1
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in)  :: uold2_wet1
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in) :: uo_wet2
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in)  :: uold2_wet2
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in) :: uo_wet3
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in)  :: uold2_wet3
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in) :: uo_wet4
    REAL(r_std),DIMENSION(npts,nvert),INTENT(in)  :: uold2_wet4

    REAL(r_std),DIMENSION(npts),INTENT(in) :: tsurf_year
    !pss:-
!JCADD
  REAL(r_std),DIMENSION(npts,nvm),INTENT(in)    :: sla_calc
  REAL(r_std),DIMENSION(npts,nvm),INTENT(in)    :: wshtotsum
  REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  sr_ugb
!  REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  compt_ugb
  REAL(r_std),DIMENSION(npts,nvm),INTENT(in)    :: nb_ani
  REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  grazed_frac
  REAL(r_std), DIMENSION(npts,nvm), INTENT(in)  ::  import_yield
  REAL(r_std),DIMENSION(npts),INTENT(in)        :: t2m_14
  REAL(r_std),DIMENSION(npts,nlitt,nvm),INTENT(in)    :: litter_not_avail
!ENDJCADD

  !spitfire
  ! Nesterov index accumulated
  REAL(r_std), DIMENSION(npts), INTENT(in)                    :: ni_acc
  ! fuel classes (1, 10, 100, 1000 hours)
  REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)               :: fuel_1hr
  REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)               :: fuel_10hr
  REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)               :: fuel_100hr
  REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)               :: fuel_1000hr
  !endspit

    !-
    ! 0.2 local
    !-
    ! date, real
    REAL(r_std) :: date_real
    ! PFT exists (equivalent to fpc_max > 0 for natural PFTs), real
    REAL(r_std),DIMENSION(npts,nvm) :: PFTpresent_real
    ! is the plant senescent ?
    ! (only for deciduous trees - carbohydrate reserve), real
    REAL(r_std),DIMENSION(npts,nvm) :: senescence_real

    REAL(r_std),DIMENSION(npts,nvm) :: begin_leaves_real

    ! in order for this PFT to be introduced,
    ! does it have to be present in an adjacent grid box? - real
    REAL(r_std),DIMENSION(npts,nvm) :: need_adjacent_real
    ! To store variables names for I/O
    CHARACTER(LEN=80) :: var_name
    ! string suffix indicating an index
    CHARACTER(LEN=10) :: part_str
    ! string suffix indicating biomass type crops
    CHARACTER(LEN=10) :: box_str
    ! string suffix indicating litter type
    CHARACTER(LEN=3),DIMENSION(nlitt) :: litter_str
    ! string suffix indicating level
    CHARACTER(LEN=2),DIMENSION(nlevs) :: level_str
    ! temporary storage
    REAL(r_std),DIMENSION(1) :: xtmp
    REAL(r_std), DIMENSION(1) :: vartmp  !! temporary variable because restget/restput needs a variable with DIMESION(:)
    ! index
    INTEGER(i_std) :: j,k,l,m,h 
    CHARACTER(LEN=1),DIMENSION(nelements) :: element_str  !! string suffix indicating element
    REAL(r_std), DIMENSION(1) :: temp_global_years
    CHARACTER(LEN=6),DIMENSION(nbpools) :: pools_str
    REAL(r_std), DIMENSION(npts) :: ok_equilibrium_real    
!!!!! crops

    ! STICS--local
    REAL(r_std), DIMENSION(npts, nvm)                                 :: in_cycle_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: f_sen_lai_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: f_crop_recycle_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: onarretesomcourdrp_real        
    REAL(r_std), DIMENSION(npts, nvm)                                 :: humectation_real        

    !REAL(r_std), DIMENSION(nvm)                                       :: codeulaivernal_real        
    
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nlevobs_real
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: namfobs_real
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nfloobs_real  
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nlanobs_real  
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nlaxobs_real  
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nmatobs_real  
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nrecobs_real  
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: nsenobs_real  
    !REAL(r_std), DIMENSION(npts, nvm)                                 :: ndrpobs_real  

    REAL(r_std), DIMENSION(npts, nvm)                                 :: nplt_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nrec_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nlan_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: ndrp_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nlev_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nger_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: etatvernal_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nflo_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nmat_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nlax_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nrecbutoir_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: ndebdes_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nbjhumec_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: namf_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nbfeuille_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nstopfeuille_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nsen_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nsencour_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: gelee_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: dernier_n_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: ndebsen_real  

    ! carbon allocation local variables
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nbj0remp_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: nstoprac_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: gslen_real  
    REAL(r_std), DIMENSION(npts, nvm)                                 :: drylen_real  
!!!!! xuhui

    ! land cover change variables 
    ! products remaining in the 10/100 year-turnover pool after the annual release for each compartment
    ! (10 or 100 + 1 : input from year of land cover change)
    REAL(r_std),DIMENSION(npts,0:10,nwp),INTENT(in)                           :: prod10
    REAL(r_std),DIMENSION(npts,0:100,nwp),INTENT(in)                          :: prod100
    ! annual release from the 10/100 year-turnover pool compartments
    REAL(r_std),DIMENSION(npts,10,nwp),INTENT(in)                           :: flux10
    REAL(r_std),DIMENSION(npts,100,nwp),INTENT(in)                          :: flux100
    REAL(r_std), DIMENSION(npts,nwp), INTENT(in)                            :: convflux
    REAL(r_std), DIMENSION(npts,nwp), INTENT(in)                            :: cflux_prod10
    REAL(r_std), DIMENSION(npts,nwp), INTENT(in)                            :: cflux_prod100
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(in)         :: bm_to_litter
    REAL(r_std),DIMENSION(npts),INTENT(in)                              :: carb_mass_total

!!!!! crops

    ! STICS--writerestart

    LOGICAL, DIMENSION(npts, nvm), INTENT(IN)       :: in_cycle 
    LOGICAL, DIMENSION(npts, nvm), INTENT(IN)       :: f_sen_lai
    LOGICAL, DIMENSION(npts, nvm), INTENT(IN)       :: f_crop_recycle 
    ! daily maximum 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(IN)      :: st2m_max_daily
    ! daily value of soil temperature at the resolution of 1 cm, the second dimension is 3
    ! the three layers around sowing layer
    REAL(r_std), DIMENSION(npts, nvm, 3), INTENT(IN)    :: wut_cm_daily
    ! daily mean value of soil relative humidity at the resolution of 1 cm, the second dimension is 3
    ! the three layers around sowing layer
    REAL(r_std), DIMENSION(npts, nvm, 3), INTENT(IN)    :: wus_cm_daily
    ! daily potential evapotranspiration 
    REAL(r_std), DIMENSION(npts), INTENT(IN)      :: evapot_daily
    ! biomass of previous day, t/ha
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)      :: pdbiomass
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)      :: pdmasec
    ! vegetative biomass
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)      :: masecveg   
    ! aboveground dry matter (t ha-1) 
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)      :: masec
    ! growth rate of plant, it means the delta total biomass increment (t ha-1)
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)      :: dltams
    ! daily gdh calculated according to halfhourly temperature // transmitted from stomate.f90 gdh_daily
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)         :: gdh_daily 
    ! Photoperiod // hours
    REAL(r_std),  DIMENSION(npts), INTENT(IN)                             :: phoi 

    !  
    LOGICAL, DIMENSION(npts, nvm), INTENT(IN)           :: onarretesomcourdrp 
    !INTEGER(i_std), DIMENSION(nvm), INTENT(IN)                           :: codeulaivernal 
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nlevobs    ! the following variables ended with obs are only used for forcing simulation.  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: namfobs    ! the initial value should be always 999
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nfloobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nlanobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nlaxobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nmatobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nrecobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: nsenobs  
    !INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)                :: ndrpobs  

    ! LAIdev SPECIFIC 
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: nsendltams
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: nsendltai
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: nsenpfeuilverte
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: nsendurvie
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: nsenndurvie
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: densiteequiv
    INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)        :: nplt
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: tursla
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: ssla
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: pfeuilverte
    REAL(r_std), DIMENSION(npts, nvm), INTENT(IN)        :: bsenlai
    
    ! variables are involved in DEVELOPMENT

    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: zrac
    INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)        :: nrec
    INTEGER(i_std), DIMENSION(npts, nvm), INTENT(IN)        :: nlan
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: tcult
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: udevair
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: udevcult
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: ndrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: rfvi
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nlev
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nger
    logical,    DIMENSION(npts, nvm), INTENT(IN)        :: etatvernal
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: caljvc
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: rfpi
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: upvt
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: utp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somcour
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somcourdrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somcourutp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: tdevelop
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somtemp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somcourfauche
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: stpltger
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stamflax
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stlaxsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stsenlan
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: stlevflo
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nflo
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stlevdrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stflodrp
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stdrpmat
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nmat
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nlax
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nrecbutoir
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: group
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: ndebdes
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: R_stdrpdes
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: densite
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: densitelev
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: coeflev
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: densiteger
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somelong
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somger
    logical,    DIMENSION(npts, nvm), INTENT(IN)        :: humectation
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nbjhumec
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somtemphumec
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: stpltlev
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: namf
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: stmatrec
 
    ! these variables are involved in Lai_calculation
     
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: tustress
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: lai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: somfeuille
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: pdlai
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nbfeuille
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: reajust
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: ulai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: pdulai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: efdensite
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: tempeff
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nstopfeuille
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: deltai
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: vmax
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)        :: nsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: laisen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: pdlaisen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)        :: dltaisenat

    ! these variables are involved in the LAIsenescence

    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)      :: nsencour
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: dltamsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: dltaisen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: fgellev
    logical,    DIMENSION(npts, nvm), INTENT(IN)      :: gelee
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: fstressgel
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: R_stlevamf
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)      :: dernier_n
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: durvieI
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: durvie
    INTEGER(i_std),    DIMENSION(npts, nvm), INTENT(IN)      :: ndebsen
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: somsenreste
    INTEGER(i_std), INTENT(IN)     :: nboxmax
    INTEGER(i_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_ndays
    REAL(r_std),   DIMENSION(npts, nvm) :: boxtemp
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_lai
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_lairem
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_tdev
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_biom
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_biomrem
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_durage
    REAL(r_std),   DIMENSION(npts, nvm, nboxmax), INTENT(IN) :: box_somsenbase

    ! these variables are involved in STRESS calculation
    
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: humrel
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: swfac
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: turfac
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: senfac

    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: mafeuiljaune 
    REAL(r_std),    DIMENSION(npts, nvm), INTENT(IN)      :: msneojaune
    ! these variables are involved in the CARBON ALLOCATION calculation

    ! grain related   
    REAL(r_std),    DIMENSION(npts, nvm, vlength)      ,INTENT(IN)       :: v_dltams
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: fgelflo
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: pdircarb
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: ircarb
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: nbgrains
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: pgrain
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: vitmoy
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: nbgraingel
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: pgraingel
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: dltags
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: ftempremp
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: magrain
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: pdmagrain
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(IN)       :: nbj0remp 
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: pdsfruittot

    ! reprac related

    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: repracmax
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: repracmin
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: kreprac
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: somtemprac
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: urac
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: reprac
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(IN)       :: nstoprac 

    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: c_reserve
    REAL(r_std),    DIMENSION(npts, nvm)      ,INTENT(IN)       :: c_leafb
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(IN)       :: gslen 
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(IN)       :: drylen 
    INTEGER(i_std), DIMENSION(npts, nvm)      ,INTENT(IN)       :: cyc_num
    INTEGER(i_std), DIMENSION(npts)           ,INTENT(IN)       :: cyc_num_tot
    INTEGER(i_std), DIMENSION(npts, rot_cmd_max, cyc_rot_max) ,INTENT(IN) :: rot_cmd_store
    INTEGER(i_std), DIMENSION(npts, nvm, cyc_rot_max) ,INTENT(IN) :: plantdate
    INTEGER(i_std), DIMENSION(npts, nvm) ,INTENT(IN) :: plantdate_now
!! some temporary variable in type of real for restput
    REAL(r_std), DIMENSION(npts, nvm)                           :: cyc_num_real
    REAL(r_std), DIMENSION(npts)                                :: cyc_num_tot_real
    REAL(r_std), DIMENSION(npts, rot_cmd_max, cyc_rot_max)      :: rot_cmd_store_real
    REAL(r_std), DIMENSION(npts, nvm, cyc_rot_max)              :: plantdate_real
    REAL(r_std), DIMENSION(npts, nvm)              :: plantdate_now_real
!!!!! xuhui
    !---------------------------------------------------------------------
    IF (printlev >= 3) WRITE(numout,*) 'Entering writerestart'
    !-
    ! 1 string definitions
    !-
    DO l=1,nlitt
       IF     (l == imetabolic) THEN
          litter_str(l) = 'met'
       ELSEIF (l == istructural) THEN
          litter_str(l) = 'str'
       ELSE
          CALL ipslerr_p(3,'stomate_io writerestart','Define litter_str','','')
       ENDIF
    ENDDO
    !-
    DO l=1,nlevs
       IF     (l == iabove) THEN
          level_str(l) = 'ab'
       ELSEIF (l == ibelow) THEN
          level_str(l) = 'be'
       ELSE
          CALL ipslerr_p(3,'stomate_io writerestart','Define level_str','','')
       ENDIF
    ENDDO
    !-
    DO l=1,nelements
       IF     (l == icarbon) THEN
          element_str(l) = ''
!!$       ELSEIF (l == initrogen) THEN
!!$          element_str(l) = '_n'
       ELSE
          CALL ipslerr_p(3,'stomate_io writerestart','Define element_str','','')
       ENDIF
    ENDDO
    !-
    pools_str(1:nbpools) =(/'str_ab','str_be','met_ab','met_be','actif ','slow  ','passif'/)
    !-
    IF (is_root_prc) THEN
       CALL ioconf_setatt_p ('UNITS','-')
       CALL ioconf_setatt_p ('LONG_NAME',' ')
    ENDIF
    !-
    ! 2 run control
    !-
    ! 2.2 time step of STOMATE in days
    !-
    IF (is_root_prc) THEN
       var_name = 'dt_days'
       xtmp(1) = dt_days
       CALL restput (rest_id_stomate, var_name, 1, 1, 1, itime, xtmp)
    ENDIF
    !-
    ! 2.3 date
    !-
    IF (is_root_prc) THEN
       var_name = 'date'
       date_real = REAL(date,r_std)
       xtmp(1) = date_real
       CALL restput (rest_id_stomate, var_name, 1, 1, 1, itime, xtmp)
    ENDIF
    !-
    ! 3 daily meteorological variables
    !-
    var_name = 'moiavail_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                moiavail_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'gdd_init_date'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    2, 1, itime, &
         &              gdd_init_date, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'litterhum_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                litterhum_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 't2m_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                t2m_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 't2m_min_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                t2m_min_daily, 'scatter', nbp_glo, index_g)
    !spitfire
    !-
    var_name = 't2m_max_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                t2m_max_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'wspeed_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                wspeed_daily, 'scatter', nbp_glo, index_g)
    !endspit
    !-
    var_name = 'tsurf_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                tsurf_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'tsoil_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nbdl, 1, itime, &
         &                tsoil_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'soilhum_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nbdl, 1, itime, &
         &                soilhum_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'precip_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                precip_daily, 'scatter', nbp_glo, index_g)

    ! Wetland CH4 methane
    !pss:+
    var_name = 'uo_0'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uo_0, 'scatter', nbp_glo, index_g)
    
    var_name = 'uold2_0'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uold2_0, 'scatter', nbp_glo, index_g)
    
    var_name = 'uo_wet1'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uo_wet1, 'scatter', nbp_glo, index_g)
    
    var_name = 'uold2_wet1'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uold2_wet1, 'scatter', nbp_glo, index_g)
    
    var_name = 'uo_wet2'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uo_wet2, 'scatter', nbp_glo, index_g)
 
    var_name = 'uold2_wet2'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uold2_wet2, 'scatter', nbp_glo, index_g)

    var_name = 'uo_wet3'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
       &              uo_wet3, 'scatter', nbp_glo, index_g)
    
    var_name = 'uold2_wet3'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uold2_wet3, 'scatter', nbp_glo, index_g)
    
    var_name = 'uo_wet4'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uo_wet4, 'scatter', nbp_glo, index_g)
    
    var_name = 'uold2_wet4'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvert, 1, itime, &
         &              uold2_wet4, 'scatter', nbp_glo, index_g)
 
    var_name = 'tsurf_year'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &              tsurf_year, 'scatter', nbp_glo, index_g)
   
    !pss:-


    !-
    ! 4 productivities
    !-
    var_name = 'gpp_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                gpp_daily, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'npp_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                npp_daily, 'scatter', nbp_glo, index_g)
    !-
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF (k < 10) part_str(1:1) = '0'
          var_name = 'turnover_daily_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
               &                   turnover_daily(:,:,k,l), 'scatter', nbp_glo, index_g)
       ENDDO
    END DO
    !-
    ! 5 monthly meteorological variables
    !-
    var_name = 'moiavail_month'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                moiavail_month, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'moiavail_week'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                moiavail_week, 'scatter', nbp_glo, index_g)
    !-
    var_name = 't2m_longterm'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                t2m_longterm, 'scatter', nbp_glo, index_g)
    
    IF (is_root_prc) THEN
       var_name='tau_longterm'
       vartmp(1)=tau_longterm
       CALL restput (rest_id_stomate, var_name, 1, 1, 1, itime, vartmp)
    ENDIF
       

    var_name = 't2m_month'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
                         t2m_month, 'scatter', nbp_glo, index_g)
    

    CALL restput_p (rest_id_stomate, 'Tseason', nbp_glo,    1, 1, itime, &
         Tseason, 'scatter', nbp_glo, index_g)
    
    CALL restput_p (rest_id_stomate, 'Tseason_length', nbp_glo,    1, 1, itime, &
         Tseason_length, 'scatter', nbp_glo, index_g)
    
    CALL restput_p (rest_id_stomate, 'Tseason_tmp', nbp_glo,    1, 1, itime, &
         Tseason_tmp, 'scatter', nbp_glo, index_g)
    
    CALL restput_p (rest_id_stomate, 'Tmin_spring_time', nbp_glo, nvm, 1, itime, &
         Tmin_spring_time, 'scatter', nbp_glo, index_g)
    
    var_name = 'Tseason'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                Tseason, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'Tseason_length'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                Tseason_length, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'Tseason_tmp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                Tseason_tmp, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'Tmin_spring_time'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                Tmin_spring_time, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'onset_date'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                onset_date, 'scatter', nbp_glo, index_g)
    !-

    var_name = 't2m_week'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
         &                t2m_week, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'tsoil_month'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nbdl, 1, itime, &
         &                tsoil_month, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'soilhum_month'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nbdl, 1, itime, &
         &                soilhum_month, 'scatter', nbp_glo, index_g)
    !-
    ! 6 fire probability
    !-
    var_name = 'fireindex'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                fireindex, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'firelitter'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                firelitter, 'scatter', nbp_glo, index_g)
    !-
    ! 7 maximum and minimum moisture availabilities for tropic phenology
    !-
    var_name = 'maxmoistr_last'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                maxmoiavail_lastyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'maxmoistr_this'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                maxmoiavail_thisyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'minmoistr_last'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                minmoiavail_lastyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'minmoistr_this'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                minmoiavail_thisyear, 'scatter', nbp_glo, index_g)
    !-
    ! 8 maximum "weekly" GPP
    !-
    var_name = 'maxgppweek_lastyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                maxgppweek_lastyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'maxgppweek_thisyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                maxgppweek_thisyear, 'scatter', nbp_glo, index_g)
    !-
    ! 9 annual GDD0
    !-
    var_name = 'gdd0_thisyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
         &                gdd0_thisyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'gdd0_lastyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
         &                gdd0_lastyear, 'scatter', nbp_glo, index_g)
    !-
    ! 10 annual precipitation
    !-
    var_name = 'precip_thisyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
         &                precip_thisyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'precip_lastyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
         &                precip_lastyear, 'scatter', nbp_glo, index_g)
    !-
    ! 11 derived "biometeorological" variables
    !-
    var_name = 'gdd_m5_dormance'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                gdd_m5_dormance, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'gdd_from_growthinit'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &              gdd_from_growthinit, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'gdd_midwinter'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                gdd_midwinter, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'ncd_dormance'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                ncd_dormance, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'ngd_minus5'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                ngd_minus5, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'time_hum_min'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                time_hum_min, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'hum_min_dormance'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                hum_min_dormance, 'scatter', nbp_glo, index_g)
    !-
    ! 12 Plant status
    !-
    var_name = 'PFTpresent'
    WHERE ( PFTpresent(:,:) )
       PFTpresent_real = un
    ELSEWHERE
       PFTpresent_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                PFTpresent_real, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'ind'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                ind, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'turnover_time'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                turnover_time, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'adapted'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                adapted, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'regenerate'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                regenerate, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'npp_longterm'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                npp_longterm, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'lm_lastyearmax'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                lm_lastyearmax, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'lm_thisyearmax'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                lm_thisyearmax, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'maxfpc_lastyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                maxfpc_lastyear, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'maxfpc_thisyear'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                maxfpc_thisyear, 'scatter', nbp_glo, index_g)
    !-
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF (k < 10) part_str(1:1) = '0'
          var_name = 'turnover_longterm_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
               &                   turnover_longterm(:,:,k,l), 'scatter', nbp_glo, index_g)
       ENDDO
    END DO
    !-
    var_name = 'gpp_week'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                gpp_week, 'scatter', nbp_glo, index_g)
    !-
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF (k < 10) part_str(1:1) = '0'
          var_name = 'biomass_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
               &                   biomass(:,:,k,l), 'scatter', nbp_glo, index_g)
       ENDDO
    END DO
    !-
    var_name = 'maint_resp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nparts, itime, &
         &                   resp_maint_part, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'leaf_age'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nleafages, itime, &
         &                  leaf_age, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'leaf_frac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nleafages, itime, &
         &                   leaf_frac, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'senescence'
    WHERE ( senescence(:,:) )
       senescence_real = un
    ELSEWHERE
       senescence_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                senescence_real, 'scatter', nbp_glo, index_g)
 
    ! Transform the logical variable begin_leaves to real before writing to restart file
    var_name = 'begin_leaves'
    WHERE ( begin_leaves(:,:) )
       begin_leaves_real = un
    ELSEWHERE
       begin_leaves_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                begin_leaves_real, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'when_growthinit'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                when_growthinit, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'age'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
         &                age, 'scatter', nbp_glo, index_g)
    !-
    ! 13 CO2
    !-
    var_name = 'resp_hetero'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                resp_hetero, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'resp_maint'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                resp_maint, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'resp_growth'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                resp_growth, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'co2_fire'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,  nvm, 1, itime, &
         &                co2_fire, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'co2_to_bm_dgvm'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                co2_to_bm_dgvm, 'scatter', nbp_glo, index_g)
    !-
    ! 14 vegetation distribution after last light competition
    !-
    var_name = 'veget_lastlight'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                veget_lastlight, 'scatter', nbp_glo, index_g)
    !-
    ! 15 establishment criteria
    !-
    var_name = 'everywhere'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                everywhere, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'need_adjacent'
    WHERE (need_adjacent(:,:))
       need_adjacent_real = un
    ELSEWHERE
       need_adjacent_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                need_adjacent_real, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'RIP_time'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
         &                RIP_time, 'scatter', nbp_glo, index_g)
    !-
    ! 17 litter
    !-
    var_name = 'litterpart'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,  nvm, nlitt, itime, &
         &                   litterpart, 'scatter', nbp_glo, index_g)
    !-
    DO k = 1,nelements
       DO l = 1,nlevs
          DO m = 1,nvm
             WRITE (part_str, '(I2)') m
             IF (m<10) part_str(1:1)='0'
             var_name = 'litter_'//part_str(1:LEN_TRIM(part_str))//'_'//level_str(l)//element_str(k)
             CALL restput_p (rest_id_stomate, var_name, nbp_glo, nlitt, 1, itime, &
                  &                     litter(:,:,m,l,k), 'scatter', nbp_glo, index_g)
          ENDDO
       ENDDO
    END DO
    !-
    var_name = 'dead_leaves'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,  nvm, nlitt, itime, &
         &                   dead_leaves, 'scatter', nbp_glo, index_g)
    !-
    var_name = 'carbon'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, ncarb, nvm, itime, &
         &                   carbon, 'scatter', nbp_glo, index_g)
    !spitfire 
       var_name = 'ni_acc'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1,1, itime, &
    &                ni_acc(:), 'scatter', nbp_glo, index_g)

    !-
    DO k = 1,nelements
       DO l=1,nlitt
         var_name = 'fuel_1hr_'//litter_str(l)//element_str(k)
         CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                 &       fuel_1hr(:,:,l,k), 'scatter', nbp_glo, index_g)
       ENDDO
    ENDDO

    DO k = 1,nelements
       DO l=1,nlitt
         var_name = 'fuel_10hr_'//litter_str(l)//element_str(k)
         CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                 &       fuel_10hr(:,:,l,k), 'scatter', nbp_glo, index_g)
       ENDDO
    ENDDO

    DO k = 1,nelements
       DO l=1,nlitt
         var_name = 'fuel_100hr_'//litter_str(l)//element_str(k)
         CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                 &       fuel_100hr(:,:,l,k), 'scatter', nbp_glo, index_g)
       ENDDO
    ENDDO

    DO k = 1,nelements
       DO l=1,nlitt
         var_name = 'fuel_1000hr_'//litter_str(l)//element_str(k)
         CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm  , 1, itime, &
                 &       fuel_1000hr(:,:,l,k), 'scatter', nbp_glo, index_g)
       ENDDO
    ENDDO

    !endspit  
    !-
       var_name = 'lignin_struc'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nlevs, itime, &
            &       lignin_struc, 'scatter', nbp_glo, index_g)
    !-
    ! 18 land cover change
    !-
    var_name = 'prod10'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 11, nwp, itime, &
         &                prod10, 'scatter', nbp_glo, index_g)
    var_name = 'prod100'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 101, nwp, itime, &
         &                prod100, 'scatter', nbp_glo, index_g)
    var_name = 'flux10'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 10, nwp, itime, &
         &                flux10, 'scatter', nbp_glo, index_g)
    var_name = 'flux100'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 100, nwp, itime, &
         &                flux100, 'scatter', nbp_glo, index_g)

    !var_name = 'convflux'
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, nwp, itime, &
    !     &              convflux, 'scatter', nbp_glo, index_g)
    !var_name = 'cflux_prod10'
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, nwp, itime, &
    !     &              cflux_prod10, 'scatter', nbp_glo, index_g)
    !var_name = 'cflux_prod100'
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, nwp, itime, &
    !     &              cflux_prod100, 'scatter', nbp_glo, index_g)
    DO l = 1,nelements
       DO k = 1,nparts
          WRITE(part_str,'(I2)') k
          IF (k < 10) part_str(1:1) = '0'
          var_name = 'bm_to_litter_'//part_str(1:LEN_TRIM(part_str))//element_str(l)
          CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
               &                bm_to_litter(:,:,k,l), 'scatter', nbp_glo, index_g)
       ENDDO
    END DO

    var_name = 'carb_mass_total'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
         &              carb_mass_total, 'scatter', nbp_glo, index_g)

    ! 19 Permafrost carbon related
    var_name = 'deepC_a'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, ndeep, nvm, itime, &
         &               deepC_a, 'scatter', nbp_glo, index_g)
    var_name = 'deepC_s'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, ndeep, nvm, itime, &
         &               deepC_s, 'scatter', nbp_glo, index_g)
    var_name = 'deepC_p'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, ndeep, nvm, itime, &
         &               deepC_p, 'scatter', nbp_glo, index_g)
    var_name = 'O2_soil'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, ndeep, nvm, itime, &
         &               O2_soil, 'scatter', nbp_glo, index_g)
    var_name = 'CH4_soil'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, ndeep, nvm, itime, &
         &               CH4_soil, 'scatter', nbp_glo, index_g)
    var_name = 'O2_snow'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nsnow, nvm, itime, &
         &               O2_snow, 'scatter', nbp_glo, index_g)
    var_name = 'CH4_snow'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nsnow, nvm, itime, &
         &               CH4_snow, 'scatter', nbp_glo, index_g)

    var_name = 'thawed_humidity'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
   &               thawed_humidity, 'scatter', nbp_glo, index_g)

    var_name = 'depth_organic_soil'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
   &               depth_organic_soil, 'scatter', nbp_glo, index_g)

    var_name = 'altmax'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
   &               altmax, 'scatter', nbp_glo, index_g)
   !Isa dbg : fixed_cryoturbation_depth -> fixed_cryoturb_depth
    var_name = 'fixed_cryoturb_depth'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
   &               fixed_cryoturbation_depth, 'scatter', nbp_glo, index_g)

!JCADD
!-
!  var_name = 'sla_calc'
!  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
! &              sla_calc, 'scatter', nbp_glo, index_g)
!-
  var_name = 'wshtotsum'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
 &              wshtotsum, 'scatter', nbp_glo, index_g)
!-
  var_name = 'sr_ugb'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
 &              sr_ugb, 'scatter', nbp_glo, index_g)
!-
  var_name = 'sla_calc'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
 &              sla_calc, 'scatter', nbp_glo, index_g)
!-
  var_name = 'nb_ani'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
 &              nb_ani, 'scatter', nbp_glo, index_g)
!-
  var_name = 'grazed_frac'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
 &              grazed_frac, 'scatter', nbp_glo, index_g)
!-
  var_name = 'import_yield'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
 &              import_yield, 'scatter', nbp_glo, index_g)
!-
  var_name = 't2m_14'
  CALL restput_p (rest_id_stomate, var_name, nbp_glo,    1, 1, itime, &
 &              t2m_14, 'scatter', nbp_glo, index_g)

    DO l=1,nlitt
       var_name = 'litter_not_avail_'//litter_str(l)
       CALL restput_p (rest_id_stomate, var_name, nbp_glo,  nvm, 1, itime, &
            &                   litter_not_avail(:,l,:), 'scatter', nbp_glo, index_g)
    ENDDO
!ENDJCADD
!!!!! crops

    !var_name = 'f_crop_init'
    !WHERE (f_crop_init)
    !   f_crop_init_real = un
    !ELSEWHERE
    !   f_crop_init_real = zero
    !ENDWHERE
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo,   1, 1, itime, &
    !     &                 f_crop_init_real, 'scatter', nbp_glo, index_g)
   
    var_name = 'f_crop_recycle'
    WHERE (f_crop_recycle(:,:))
       f_crop_recycle_real = un
    ELSEWHERE
       f_crop_recycle_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                 f_crop_recycle_real, 'scatter', nbp_glo, index_g)

    var_name = 'in_cycle'
    WHERE (in_cycle(:,:))
       in_cycle_real = un
    ELSEWHERE
       in_cycle_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                 in_cycle_real, 'scatter', nbp_glo, index_g)

    var_name = 'f_sen_lai'
    WHERE (f_sen_lai(:,:))
       f_sen_lai_real = un
    ELSEWHERE
       f_sen_lai_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                 f_sen_lai_real, 'scatter', nbp_glo, index_g)
    
    var_name = 'st2m_max_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &               st2m_max_daily, 'scatter', nbp_glo, index_g)


    DO h = 1, 3   
       WRITE(part_str,'(I2)') h
       IF (h < 4) part_str(1:1) = '0'
       var_name = 'wut_cm_daily_'//part_str(1:LEN_TRIM(part_str))
!       print *, 'WU: VAR_NAME STEMPDIAG',var_name
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &               wut_cm_daily(:, :, h), 'scatter', nbp_glo, index_g)
    ENDDO

    DO h = 1, 3
    
       WRITE(part_str,'(I2)') h
       IF (h < 4) part_str(1:1) = '0'
       var_name = 'wus_cm_daily_'//part_str(1:LEN_TRIM(part_str))
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm   , 1, itime, &
            &               wus_cm_daily(:, :, h), 'scatter', nbp_glo, index_g)
    ENDDO

    var_name = 'evapot_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &               evapot_daily, 'scatter', nbp_glo, index_g)

    var_name = 'pdbiomass'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdbiomass, 'scatter', nbp_glo, index_g)

    var_name = 'pdmasec'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdmasec, 'scatter', nbp_glo, index_g)

    var_name = 'masecveg'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               masecveg, 'scatter', nbp_glo, index_g)

    var_name = 'masec'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               masec, 'scatter', nbp_glo, index_g)

    var_name = 'dltams'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               dltams, 'scatter', nbp_glo, index_g)

    var_name = 'gdh_daily'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               gdh_daily, 'scatter', nbp_glo, index_g)

    var_name = 'phoi'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
         &               phoi, 'scatter', nbp_glo, index_g)

    var_name = 'onarretesomcourdrp'
    WHERE (onarretesomcourdrp(:, :))
       onarretesomcourdrp_real = un
    ELSEWHERE
       onarretesomcourdrp_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                 onarretesomcourdrp_real, 'scatter', nbp_glo, index_g)

    !var_name = 'codeulaivernal'
    !codeulaivernal_real = FLOAT(codeulaivernal)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
    !     &               codeulaivernal_real, 'scatter', nbp_glo, index_g)
  
    !var_name = 'nlevobs'
    !nlevobs_real = FLOAT(nlevobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nlevobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'namfobs'
    !namfobs_real = FLOAT(namfobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               namfobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'nfloobs'
    !nfloobs_real = FLOAT(nfloobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nfloobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'nlanobs'
    !nlanobs_real = FLOAT(nlanobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nlanobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'nlaxobs'
    !nlaxobs_real = FLOAT(nlaxobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nlaxobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'nmatobs'
    !nmatobs_real = FLOAT(nmatobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nmatobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'nrecobs'
    !nrecobs_real = FLOAT(nrecobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nrecobs_real, 'scatter', nbp_glo, index_g)

    !var_name = 'nsenobs'
    !nsenobs_real = FLOAT(nsenobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nsenobs_real, 'scatter', nbp_glo, index_g)


    !var_name = 'ndrpobs'
    !ndrpobs_real = FLOAT(ndrpobs)
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               ndrpobs_real, 'scatter', nbp_glo, index_g)


    var_name = 'nsendltams'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsendltams, 'scatter', nbp_glo, index_g)

    var_name = 'nsendltai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsendltai, 'scatter', nbp_glo, index_g)

    var_name = 'nsenpfeuilverte'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsenpfeuilverte, 'scatter', nbp_glo, index_g)

    var_name = 'nsendurvie'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsendurvie, 'scatter', nbp_glo, index_g)

    var_name = 'nsenndurvie'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsenndurvie, 'scatter', nbp_glo, index_g)

    var_name = 'densiteequiv'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               densiteequiv, 'scatter', nbp_glo, index_g)

    var_name = 'nplt'
    nplt_real = FLOAT(nplt)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nplt_real, 'scatter', nbp_glo, index_g)

    var_name = 'tursla'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               tursla, 'scatter', nbp_glo, index_g)

    var_name = 'ssla'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ssla, 'scatter', nbp_glo, index_g)

    var_name = 'pfeuilverte'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pfeuilverte, 'scatter', nbp_glo, index_g)

    var_name = 'bsenlai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               bsenlai, 'scatter', nbp_glo, index_g)

    var_name = 'zrac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               zrac, 'scatter', nbp_glo, index_g)

    var_name = 'nrec'
    nrec_real = FLOAT(nrec)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nrec_real, 'scatter', nbp_glo, index_g)

    var_name = 'nlan'
    nlan_real = FLOAT(nlan)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nlan_real, 'scatter', nbp_glo, index_g)

    var_name = 'tcult'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               tcult, 'scatter', nbp_glo, index_g)

    var_name = 'udevair'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               udevair, 'scatter', nbp_glo, index_g)

    var_name = 'udevcult'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               udevcult, 'scatter', nbp_glo, index_g)

    var_name = 'ndrp'
    ndrp_real = FLOAT(ndrp)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ndrp_real, 'scatter', nbp_glo, index_g)

    var_name = 'rfvi'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               rfvi, 'scatter', nbp_glo, index_g)

    var_name = 'nlev'
    nlev_real = FLOAT(nlev)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nlev_real, 'scatter', nbp_glo, index_g)

    var_name = 'nger'
    nger_real = FLOAT(nger)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nger_real, 'scatter', nbp_glo, index_g)

    var_name = 'etatvernal'
    WHERE ( etatvernal(:,:) )
       etatvernal_real = un
    ELSEWHERE
       etatvernal_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               etatvernal_real, 'scatter', nbp_glo, index_g)

    var_name = 'caljvc'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               caljvc, 'scatter', nbp_glo, index_g)

    var_name = 'rfpi'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               rfpi, 'scatter', nbp_glo, index_g)

    var_name = 'upvt'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               upvt, 'scatter', nbp_glo, index_g)

    var_name = 'utp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               utp, 'scatter', nbp_glo, index_g)

    var_name = 'somcour'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somcour, 'scatter', nbp_glo, index_g)

    var_name = 'somcourdrp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somcourdrp, 'scatter', nbp_glo, index_g)

    var_name = 'somcourutp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somcourutp, 'scatter', nbp_glo, index_g)

    var_name = 'tdevelop'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               tdevelop, 'scatter', nbp_glo, index_g)

    var_name = 'somtemp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somtemp, 'scatter', nbp_glo, index_g)

    var_name = 'somcourfauche'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somcourfauche, 'scatter', nbp_glo, index_g)

    var_name = 'stpltger'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               stpltger, 'scatter', nbp_glo, index_g)

    var_name = 'R_stlaxsen'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stlaxsen, 'scatter', nbp_glo, index_g)

    var_name = 'R_stamflax'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stamflax, 'scatter', nbp_glo, index_g)

    var_name = 'R_stsenlan'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stsenlan, 'scatter', nbp_glo, index_g)

    var_name = 'stlevflo'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               stlevflo, 'scatter', nbp_glo, index_g)

    var_name = 'nflo'
    nflo_real = FLOAT(nflo)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nflo_real, 'scatter', nbp_glo, index_g)

    var_name = 'R_stlevdrp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stlevdrp, 'scatter', nbp_glo, index_g)

    var_name = 'R_stflodrp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stflodrp, 'scatter', nbp_glo, index_g)

    var_name = 'R_stdrpmat'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stdrpmat, 'scatter', nbp_glo, index_g)

    var_name = 'nmat'
    nmat_real = FLOAT(nmat)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nmat_real, 'scatter', nbp_glo, index_g)

    var_name = 'nlax'
    nlax_real = FLOAT(nlax)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nlax_real, 'scatter', nbp_glo, index_g)

    var_name = 'nrecbutoir'
    nrecbutoir_real = FLOAT(nrecbutoir)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nrecbutoir_real, 'scatter', nbp_glo, index_g)

    var_name = 'group'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               group, 'scatter', nbp_glo, index_g)

    var_name = 'ndebdes'
    ndebdes_real = FLOAT(ndebdes)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ndebdes_real, 'scatter', nbp_glo, index_g)

    var_name = 'R_stdrpdes'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stdrpdes, 'scatter', nbp_glo, index_g)

    var_name = 'densite'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               densite, 'scatter', nbp_glo, index_g)

    var_name = 'densitelev'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               densitelev, 'scatter', nbp_glo, index_g)

    var_name = 'coeflev'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               coeflev, 'scatter', nbp_glo, index_g)

    var_name = 'densiteger'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               densiteger, 'scatter', nbp_glo, index_g)

    var_name = 'somelong'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somelong, 'scatter', nbp_glo, index_g)

    var_name = 'somger'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somger, 'scatter', nbp_glo, index_g)

    var_name = 'humectation'
    WHERE (humectation(:, :))
       humectation_real = un
    ELSEWHERE
       humectation_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo,   nvm, 1, itime, &
         &                 humectation_real, 'scatter', nbp_glo, index_g)

    var_name = 'nbjhumec'
    nbjhumec_real = FLOAT(nbjhumec)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nbjhumec_real, 'scatter', nbp_glo, index_g)

    var_name = 'somtemphumec'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somtemphumec, 'scatter', nbp_glo, index_g)

    var_name = 'stpltlev'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               stpltlev, 'scatter', nbp_glo, index_g)

    var_name = 'namf'
    namf_real = FLOAT(namf)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               namf_real, 'scatter', nbp_glo, index_g)

    var_name = 'stmatrec'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               stmatrec, 'scatter', nbp_glo, index_g)

    var_name = 'tustress'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               tustress, 'scatter', nbp_glo, index_g)

    var_name = 'lai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               lai, 'scatter', nbp_glo, index_g)

    var_name = 'somfeuille'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somfeuille, 'scatter', nbp_glo, index_g)

    var_name = 'pdlai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdlai, 'scatter', nbp_glo, index_g)

    var_name = 'nbfeuille'
    nbfeuille_real = FLOAT(nbfeuille)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nbfeuille_real, 'scatter', nbp_glo, index_g)

    var_name = 'reajust'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               reajust, 'scatter', nbp_glo, index_g)

    var_name = 'ulai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ulai, 'scatter', nbp_glo, index_g)

    var_name = 'pdulai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdulai, 'scatter', nbp_glo, index_g)

    var_name = 'efdensite'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               efdensite, 'scatter', nbp_glo, index_g)

    var_name = 'tempeff'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               tempeff, 'scatter', nbp_glo, index_g)

    var_name = 'nstopfeuille'
    nstopfeuille_real = FLOAT(nstopfeuille)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nstopfeuille_real, 'scatter', nbp_glo, index_g)

    var_name = 'deltai'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               deltai, 'scatter', nbp_glo, index_g)

    var_name = 'vmax'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               vmax, 'scatter', nbp_glo, index_g)

    var_name = 'nsen'
    nsen_real = FLOAT(nsen)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsen_real, 'scatter', nbp_glo, index_g)

    var_name = 'laisen'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               laisen, 'scatter', nbp_glo, index_g)

    var_name = 'pdlaisen'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdlaisen, 'scatter', nbp_glo, index_g)

    var_name = 'dltaisenat'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               dltaisenat, 'scatter', nbp_glo, index_g)

    var_name = 'nsencour'
    nsencour_real = FLOAT(nsencour)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nsencour_real, 'scatter', nbp_glo, index_g)

    var_name = 'dltamsen'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               dltamsen, 'scatter', nbp_glo, index_g)

    var_name = 'dltaisen'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               dltaisen, 'scatter', nbp_glo, index_g)

    var_name = 'fgellev'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               fgellev, 'scatter', nbp_glo, index_g)

    var_name = 'gelee'
    WHERE ( gelee(:,:) )
       gelee_real = un
    ELSEWHERE
       gelee_real = zero
    ENDWHERE
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               gelee_real, 'scatter', nbp_glo, index_g)

    var_name = 'fstressgel'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               fstressgel, 'scatter', nbp_glo, index_g)

    var_name = 'laisen'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               laisen, 'scatter', nbp_glo, index_g)

    var_name = 'R_stlevamf'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               R_stlevamf, 'scatter', nbp_glo, index_g)

    var_name = 'dernier_n'
    dernier_n_real = FLOAT(dernier_n)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               dernier_n_real, 'scatter', nbp_glo, index_g)

    var_name = 'durvieI'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               durvieI, 'scatter', nbp_glo, index_g)

    var_name = 'durvie'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               durvie, 'scatter', nbp_glo, index_g)

    var_name = 'ndebsen'
    ndebsen_real = FLOAT(ndebsen)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ndebsen_real, 'scatter', nbp_glo, index_g)

    var_name = 'somsenreste'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somsenreste, 'scatter', nbp_glo, index_g)

    var_name = 'humrel'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               humrel, 'scatter', nbp_glo, index_g)

    var_name = 'swfac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               swfac, 'scatter', nbp_glo, index_g)

    var_name = 'turfac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               turfac, 'scatter', nbp_glo, index_g)

    var_name = 'senfac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               senfac, 'scatter', nbp_glo, index_g)
  

    var_name = 'mafeuiljaune'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               mafeuiljaune, 'scatter', nbp_glo, index_g)

    var_name = 'msneojaune'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               msneojaune, 'scatter', nbp_glo, index_g)

!    var_name = 'myvar'
!    CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm, nboxmax, itime, &
!        &                myvar, 'scatter', nbp_glo, index_g)

    DO h = 1, nboxmax
        WRITE(box_str, '(I2)') h
        IF (h<10) box_str(1:1) = '0'
        var_name = 'box_ndays_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                REAL(box_ndays(:, :, h)), 'scatter', nbp_glo, index_g)

        var_name = 'box_lai_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_lai(:,:,h), 'scatter', nbp_glo, index_g) 

        var_name = 'box_lairem_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_lairem(:,:,h), 'scatter', nbp_glo, index_g)

        var_name = 'box_tdev_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_tdev(:,:,h), 'scatter', nbp_glo, index_g)

        var_name = 'box_biom_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_biom(:,:,h), 'scatter', nbp_glo, index_g)

        var_name = 'box_biomrem_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_biomrem(:,:,h), 'scatter', nbp_glo, index_g)

        var_name = 'box_durage_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_durage(:,:,h), 'scatter', nbp_glo, index_g)

        var_name = 'box_somsenbase_'//box_str(1:LEN_TRIM(box_str))
        CALL restput_p(rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &                box_somsenbase(:,:,h), 'scatter', nbp_glo, index_g)

    ENDDO

 
    ! STICS:: CARBON ALLOCATION

    DO h = 1, vlength 
       WRITE(part_str,'(I2)') h
       IF (h < 10) part_str(1:1) = '0'
       var_name = 'v_dltams_'//part_str(1:LEN_TRIM(part_str))
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
            &               v_dltams(:, :, h), 'scatter', nbp_glo, index_g)
    ENDDO
 
    var_name = 'fgelflo'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               fgelflo, 'scatter', nbp_glo, index_g)

    var_name = 'pdircarb'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdircarb, 'scatter', nbp_glo, index_g)


    var_name = 'ircarb'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ircarb, 'scatter', nbp_glo, index_g)

    var_name = 'nbgrains'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nbgrains, 'scatter', nbp_glo, index_g)

    var_name = 'pgrain'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pgrain, 'scatter', nbp_glo, index_g)

    var_name = 'vitmoy'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               vitmoy, 'scatter', nbp_glo, index_g)

    var_name = 'nbgraingel'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nbgraingel, 'scatter', nbp_glo, index_g)

    var_name = 'pgraingel'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pgraingel, 'scatter', nbp_glo, index_g)

    var_name = 'dltags'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               dltags, 'scatter', nbp_glo, index_g)

    var_name = 'ftempremp'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               ftempremp, 'scatter', nbp_glo, index_g)

    var_name = 'magrain'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               magrain, 'scatter', nbp_glo, index_g)

    var_name = 'pdmagrain'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdmagrain, 'scatter', nbp_glo, index_g)


    var_name = 'nbj0remp'
    nbj0remp_real = FLOAT(nbj0remp)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nbj0remp_real, 'scatter', nbp_glo, index_g)
 
    !var_name = 'nbj0remp'
    !CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
    !     &               nbj0remp, 'scatter', nbp_glo, index_g)

    var_name = 'pdsfruittot'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               pdsfruittot, 'scatter', nbp_glo, index_g)

    var_name = 'repracmax'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               repracmax, 'scatter', nbp_glo, index_g)

    var_name = 'repracmin'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               repracmin, 'scatter', nbp_glo, index_g)

    var_name = 'kreprac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               kreprac, 'scatter', nbp_glo, index_g)

    var_name = 'somtemprac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               somtemprac, 'scatter', nbp_glo, index_g)

    var_name = 'urac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               urac, 'scatter', nbp_glo, index_g)

    var_name = 'reprac'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               reprac, 'scatter', nbp_glo, index_g)


    var_name = 'nstoprac'
    nstoprac_real = FLOAT(nstoprac)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               nstoprac_real, 'scatter', nbp_glo, index_g)


    var_name = 'c_leafb'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               c_leafb, 'scatter', nbp_glo, index_g)

    var_name = 'c_reserve'
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               c_reserve, 'scatter', nbp_glo, index_g)
    
    var_name = 'gslen'
    gslen_real = FLOAT(gslen)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               gslen_real, 'scatter', nbp_glo, index_g)

    var_name = 'drylen'
    drylen_real = FLOAT(drylen)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
         &               drylen_real, 'scatter', nbp_glo, index_g)
    IF (ok_rotate) THEN
        var_name = 'cyc_num'
        cyc_num_real = FLOAT(cyc_num)
        CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm     , 1, itime, &
             &               cyc_num_real, 'scatter', nbp_glo, index_g)
    
        var_name = 'cyc_num_tot'
        cyc_num_tot_real = FLOAT(cyc_num_tot)
        CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1     , 1, itime, &
             &               cyc_num_tot_real, 'scatter', nbp_glo, index_g)
    
        var_name = 'rot_cmd_store'
        rot_cmd_store_real = FLOAT(rot_cmd_store)
        CALL restput_p (rest_id_stomate, var_name, nbp_glo, rot_cmd_max, cyc_rot_max, itime, &
             &               rot_cmd_store_real, 'scatter', nbp_glo, index_g)
    ENDIF
    var_name = 'plantdate'
    plantdate_real = FLOAT(plantdate)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, cyc_rot_max, itime, &
         &                plantdate_real, 'scatter', nbp_glo, index_g)

    var_name = 'plantdate_now'
    plantdate_now_real = FLOAT(plantdate_now)
    CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1   , itime, &
         &                plantdate_now_real, 'scatter', nbp_glo, index_g)

!!!!! xuhui
 
    !-
    ! 19. Spinup
    !-
!    IF (spinup_analytic) THEN

       IF (is_root_prc) THEN
          temp_global_years(1) = REAL(global_years)
          var_name='Global_years'
          CALL restput (rest_id_stomate, var_name, 1, 1, 1, itime, temp_global_years)
       ENDIF
       
       var_name = 'nbp_sum'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
            &              nbp_accu, 'scatter', nbp_glo, index_g)

       var_name = 'nbp_flux'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
            &              nbp_flux, 'scatter', nbp_glo, index_g)

       var_name = 'ok_equilibrium'
       WHERE(ok_equilibrium(:))
          ok_equilibrium_real = un
       ELSEWHERE
          ok_equilibrium_real = zero
       ENDWHERE
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, 1, 1, itime, &
            &               ok_equilibrium_real, 'scatter', nbp_glo, index_g)
       
       DO k = 1,nbpools
          DO j = 1,nbpools
             WRITE(part_str,'(I2)') k
             IF (k < 10) part_str(1:1) = '0'             
             var_name = 'MatrixV_'//part_str(1:LEN_TRIM(part_str))//'_'//TRIM(pools_str(j))
             CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, 1, itime, &
                  &                MatrixV(:,:,k,j), 'scatter', nbp_glo, index_g)
          ENDDO
       ENDDO
          
       var_name = 'Vector_U'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nbpools, itime, &
            &                VectorU, 'scatter', nbp_glo, index_g)
          
       var_name = 'previous_stock'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nbpools, itime, &
            &                previous_stock, 'scatter', nbp_glo, index_g)
          
       var_name = 'current_stock'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, nbpools, itime, &
            &                current_stock, 'scatter', nbp_glo, index_g)

!    ENDIF !(spinup_analytic)
    !-


       var_name = 'assim_param'
       CALL restput_p (rest_id_stomate, var_name, nbp_glo, nvm, npco2, itime, &
            &                assim_param, 'scatter', nbp_glo, index_g)
       

    IF (printlev >= 4) WRITE(numout,*) 'Leaving writerestart'
    !--------------------------
  END SUBROUTINE writerestart
  !-
  !===
  !-
  SUBROUTINE get_reftemp_clear
    !---------------------------------------------------------------------
    firstcall_io=.TRUE.
    IF (ALLOCATED (trefe)) DEALLOCATE( trefe )
    !-------------------------------
  END SUBROUTINE get_reftemp_clear
  !-
  !===
  !-
  SUBROUTINE get_reftemp (npts, lalo, resolution, filename, tref_out)
    !---------------------------------------------------------------------
    !- read the long-term reference temperature from a boundary condition
    !- file. If the vegetation is dynamic, this field is used to
    !- initialize correctly the (prognostic) long-term reference
    !- temperature (in the case it is not found in the restart file).
    !- If the vegetation is static, the field read here is a real boundary
    !- condition that is not modified by the model.
    !---------------------------------------------------------------------
    !-
    ! 0 declarations
    !-
    ! 0.1 input
    !-
    ! Domain size
    INTEGER(i_std),INTENT(in) :: npts
    ! Geogr. coordinates (latitude,longitude) (degrees)
    REAL(r_std),DIMENSION (npts,2),INTENT(in) :: lalo
    ! size in x an y of the grid (m)
    REAL(r_std),DIMENSION (npts,2),INTENT(in) :: resolution
    CHARACTER(LEN=80), INTENT(IN) :: filename
    !-
    ! 0.2 output
    !-
    ! reference temperature (K)
    REAL(r_std), DIMENSION(npts),INTENT(out) :: tref_out
    !-
    ! 0.3 local
    !-
    INTEGER(i_std),PARAMETER :: nbvmax=200
    INTEGER(i_std) :: &
         &  iml, jml, lml, tml, fid, ib, ip, jp, fopt, ilf, lastjp
    REAL(r_std) :: lev(1), date, dt, coslat
    INTEGER(i_std)                                 :: itau(1)
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:) :: &
         &  lat_rel, lon_rel, lat_ful, lon_ful, tref_file
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:) :: &
         &  loup_rel, lolow_rel, laup_rel, lalow_rel
    REAL(r_std) :: lon_up, lon_low, lat_up, lat_low
    REAL(r_std) :: ax, ay, sgn
    REAL(r_std),DIMENSION(nbvmax) :: area
    REAL(r_std),DIMENSION(nbvmax) :: tt
    REAL(r_std) :: resx, resy
    LOGICAL :: do_again
    !---------------------------------------------------------------------
    !-
    ! 1 If this is the first call, calculate the reference temperature
    !   and keep it in memory
    !-
    IF (firstcall_io) THEN
       !---
       !-- 1.1 only do this once
       !---
       firstcall_io = .FALSE.
       !---
       !-- 1.2 allocate the field
       !---
       ALLOCATE( trefe(npts) )
       !---
       !-- 1.3 read and interpolate the temperature file
       !---
       !-- Needs to be a configurable variable
       !---
       !---
       IF (is_root_prc) CALL flininfo(filename,iml, jml, lml, tml, fid)
       CALL bcast(iml)
       CALL bcast(jml)
       CALL bcast(lml)
       CALL bcast(tml)
       !---
       ALLOCATE (lat_rel(iml,jml))
       ALLOCATE (lon_rel(iml,jml))
       ALLOCATE (laup_rel(iml,jml))
       ALLOCATE (loup_rel(iml,jml))
       ALLOCATE (lalow_rel(iml,jml))
       ALLOCATE (lolow_rel(iml,jml))
       ALLOCATE (lat_ful(iml+2,jml+2))
       ALLOCATE (lon_ful(iml+2,jml+2))
       ALLOCATE (tref_file(iml,jml))
       !---
       IF (is_root_prc) CALL flinopen (filename, .FALSE., iml, jml, lml, &
            &                                   lon_rel, lat_rel, lev, tml, itau, date, dt, fid)
       CALL bcast(lon_rel)
       CALL bcast(lat_rel)
       CALL bcast(itau)
       CALL bcast(date)
       CALL bcast(dt)

       !---
       IF (is_root_prc) CALL flinget (fid, 'temperature', iml, jml, lml, tml, &
            &                                  1, 1, tref_file)
       CALL bcast(tref_file)
       !---
       IF (is_root_prc) CALL flinclo (fid)
       !---
       !-- Duplicate the border assuming we have a global grid
       !-- going from west to east
       !---
       lon_ful(2:iml+1,2:jml+1) = lon_rel(1:iml,1:jml)
       lat_ful(2:iml+1,2:jml+1) = lat_rel(1:iml,1:jml)
       !---
       IF ( lon_rel(iml,1) < lon_ful(2,2)) THEN
          lon_ful(1,2:jml+1) = lon_rel(iml,1:jml)
          lat_ful(1,2:jml+1) = lat_rel(iml,1:jml)
       ELSE
          lon_ful(1,2:jml+1) = lon_rel(iml,1:jml)-360
          lat_ful(1,2:jml+1) = lat_rel(iml,1:jml)
       ENDIF
       !---
       IF ( lon_rel(1,1) > lon_ful(iml+1,2)) THEN
          lon_ful(iml+2,2:jml+1) = lon_rel(1,1:jml)
          lat_ful(iml+2,2:jml+1) = lat_rel(1,1:jml)
       ELSE
          lon_ful(iml+2,2:jml+1) = lon_rel(1,1:jml)+360
          lat_ful(iml+2,2:jml+1) = lat_rel(1,1:jml)
       ENDIF
       !---
       sgn = lat_rel(1,1)/ABS(lat_rel(1,1))
       lat_ful(2:iml+1,1) = sgn*180 - lat_rel(1:iml,1)
       sgn = lat_rel(1,jml)/ABS(lat_rel(1,jml))
       lat_ful(2:iml+1,jml+2) = sgn*180 - lat_rel(1:iml,jml)
       lat_ful(1,1) = lat_ful(iml+1,1)
       lat_ful(iml+2,1) = lat_ful(2,1)
       lat_ful(1,jml+2) = lat_ful(iml+1,jml+2)
       lat_ful(iml+2,jml+2) = lat_ful(2,jml+2)
       !---
       !-- Add the longitude lines to the top and bottom
       !---
       lon_ful(:,1) = lon_ful(:,2)
       lon_ful(:,jml+2) = lon_ful(:,jml+1)
       !---
       !-- Get the upper and lower limits of each grid box
       !---
       DO ip=1,iml
          DO jp=1,jml
             loup_rel(ip,jp) = &
                  &        MAX(0.5*(lon_ful(ip,jp+1)+lon_ful(ip+1,jp+1)), &
                  &            0.5*(lon_ful(ip+1,jp+1)+lon_ful(ip+2,jp+1)))
             lolow_rel(ip,jp) = &
                  &        MIN(0.5*(lon_ful(ip,jp+1)+lon_ful(ip+1,jp+1)), &
                  &            0.5*(lon_ful(ip+1,jp+1)+lon_ful(ip+2,jp+1)))
             laup_rel(ip,jp) = &
                  &        MAX(0.5*(lat_ful(ip+1,jp)+lat_ful(ip+1,jp+1)), &
                  &            0.5*(lat_ful(ip+1,jp+1)+lat_ful(ip+1,jp+2)))
             lalow_rel(ip,jp) = &
                  &        MIN(0.5*(lat_ful(ip+1,jp)+lat_ful(ip+1,jp+1)), &
                  &            0.5*(lat_ful(ip+1,jp+1)+lat_ful(ip+1,jp+2)))
          ENDDO
       ENDDO
       !---
       !-- Now we take each grid point and find out which values
       !-- from the forcing we need to average
       !---
       DO ib=1,npts
          !-----
          resx = resolution(ib,1)
          resy = resolution(ib,2)
          !-----
          do_again = .TRUE.
          !-----
          DO WHILE (do_again)
             !-----
             do_again = .FALSE.
             !-------
             !------ We find the 4 limits of the grid-box.
             !------ As we transform the resolution of the model into longitudes
             !------ and latitudes we do not have the problem of periodicity.
             !------ coslat is a help variable here !
             !-------
             coslat = MAX(COS(lalo(ib,1)*pi/180.),mincos)*pi/180.*R_Earth
             !-------
             lon_up  = lalo(ib,2)+resx/(2.0*coslat)
             lon_low = lalo(ib,2)-resx/(2.0*coslat)
             !-------
             coslat  = pi/180.*R_Earth
             !-------
             lat_up  = lalo(ib,1)+resy/(2.0*coslat)
             lat_low = lalo(ib,1)-resy/(2.0*coslat)
             !-------
             !------ Find the grid boxes from the data that go into
             !------ the model's boxes.
             !------ We still work as if we had a regular grid !
             !------ Well it needs to be localy regular so that
             !------ the longitude at the latitude of the last found point
             !------ is close to the one of the next point.
             !-------
             fopt = 0
             lastjp = 1
             DO ip=1,iml
                !---------
                !-------- Either the center of the data grid point is in the interval
                !-------- of the model grid or the East and West limits of the data
                !-------- grid point are on either sides of the border of the data grid
                !---------
                IF (      lon_rel(ip,lastjp) > lon_low &
                     &            .AND. lon_rel(ip,lastjp) < lon_up &
                     &             .OR. lolow_rel(ip,lastjp) < lon_low &
                     &            .AND. loup_rel(ip,lastjp) > lon_low &
                     &             .OR. lolow_rel(ip,lastjp) < lon_up &
                     &            .AND. loup_rel(ip,lastjp) > lon_up ) THEN
                   DO jp=1,jml
                      !-------------
                      !------------ Now that we have the longitude let us find the latitude
                      !-------------
                      IF (      lat_rel(ip,jp) > lat_low &
                           &                 .AND. lat_rel(ip,jp) < lat_up &
                           &                  .OR. lalow_rel(ip,jp) < lat_low &
                           &                 .AND. laup_rel(ip,jp) > lat_low &
                           &                  .OR. lalow_rel(ip,jp) < lat_up &
                           &                 .AND. laup_rel(ip,jp) > lat_up) THEN
                         lastjp = jp
                         !---------------
                         fopt = fopt + 1
                         IF ( fopt > nbvmax) THEN
                            WRITE(numout,*) &
                                 &                       'Please increase nbvmax in subroutine get_reftemp',ib
                            CALL ipslerr_p(3,'stomate_io','get_reftemp','Please increase nbvmax','')
                         ELSE
                            !-----------------
                            !---------------- Get the area of the fine grid in the model grid
                            !-----------------
                            coslat = MAX(COS(lat_rel(ip,jp)*pi/180.),mincos)
                            ax =  ( MIN(lon_up,loup_rel(ip,jp)) &
                                 &                       -MAX(lon_low,lolow_rel(ip,jp))) &
                                 &                     *pi/180.*R_Earth*coslat
                            ay =  ( MIN(lat_up,laup_rel(ip,jp)) &
                                 &                       -MAX(lat_low,lalow_rel(ip,jp))) &
                                 &                     *pi/180.*R_Earth
                            area(fopt) = ax*ay
                            tt(fopt) = tref_file(ip,jp)
                         ENDIF
                      ENDIF
                   ENDDO
                ENDIF
             ENDDO
             !-------
             !------ Check that we found some points
             !-------
             trefe(ib) = zero
             !-------
             IF (fopt == 0) THEN
                do_again = .TRUE.
                !-------
                !------ increase search radius
                !-------
                resx = resx*2.
                resy = resy*2.
                IF ( resx > 2.*pi*R_Earth .OR. resy > pi*R_Earth ) THEN
                   CALL ipslerr_p(3,'stomate_io','get_reftemp: found no point 1','','')
                ENDIF
             ELSE
                sgn = zero
                !-------
                !------ Compute the average surface air temperature
                !-------
                DO ilf=1,fopt
                   trefe(ib) = trefe(ib) + tt(ilf) * area(ilf)
                   sgn = sgn + area(ilf)
                ENDDO
                !-------
                !------ Normalize the surface
                !-------
                IF (sgn < min_sechiba) THEN
                   do_again = .TRUE.
                   !---------
                   !-------- increase search radius
                   !---------
                   resx = resx * 2.
                   resy = resy * 2.
                   IF ( resx > 2.*pi*R_Earth .OR. resy > pi*R_Earth ) THEN
                      CALL ipslerr_p(3,'stomate_io','get_reftemp: found no point 2','','')
                   ENDIF
                ELSE
                   trefe(ib) = trefe(ib) / sgn
                ENDIF
             ENDIF
          ENDDO
       ENDDO
       !-
       ! transform into Kelvin
       !-
       trefe(:) = trefe(:) + ZeroCelsius
       !-
       ! deallocate
       !-
       DEALLOCATE (lat_rel)
       DEALLOCATE (lon_rel)
       DEALLOCATE (laup_rel)
       DEALLOCATE (loup_rel)
       DEALLOCATE (lalow_rel)
       DEALLOCATE (lolow_rel)
       DEALLOCATE (lat_ful)
       DEALLOCATE (lon_ful)
       DEALLOCATE (tref_file)
    ENDIF
    !-
    ! 2 output the reference temperature
    !-
    tref_out(:) = trefe(:)
    !-------------------------
  END SUBROUTINE get_reftemp
  !-
  !===
  !-
END MODULE stomate_io
