! =================================================================================================================================
! MODULE        : stomate_phenology
!
! CONTACT       : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE       : IPSL (2006). This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module manages the beginning of the growing season (leaf onset).
!!      
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/stomate_phenology.f90 $ 
!! $Date: 2016-06-21 10:19:43 +0200 (Tue, 21 Jun 2016) $
!! $Revision: 3569 $
!! \n
!_ =================================================================================================================================

MODULE stomate_phenology

  ! modules used:
  USE xios_orchidee
  USE ioipsl_para
  USE stomate_data
  USE constantes
  USE pft_parameters

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC phenology,phenology_clear

  ! first call
  LOGICAL, SAVE                                              :: firstcall_all_phenology = .TRUE.
!$OMP THREADPRIVATE(firstcall_all_phenology)
  LOGICAL, SAVE                                              :: firstcall_hum = .TRUE.
!$OMP THREADPRIVATE(firstcall_hum)
  LOGICAL, SAVE                                              :: firstcall_moi = .TRUE.
!$OMP THREADPRIVATE(firstcall_moi)
  LOGICAL, SAVE                                              :: firstcall_humgdd = .TRUE.
!$OMP THREADPRIVATE(firstcall_humgdd)
  LOGICAL, SAVE                                              :: firstcall_moigdd = .TRUE.
  LOGICAL, SAVE                                              :: firstcall_moi_C4 = .TRUE.
!$OMP THREADPRIVATE(firstcall_moigdd)

CONTAINS


!! ================================================================================================================================
!! SUBROUTINE   : phenology_clear
!!
!>\BRIEF          Flags setting   
!!
!! DESCRIPTION  : This subroutine sets flags 
!!                ::firstcall_all_phenology, ::firstcall_hum, ::firstcall_moi, ::firstcall_humgdd, 
!!                ::firstcall_moigdd to .TRUE., and therefore activates section 1.1 of each 
!!                subroutine which writes messages to the output. \n
!!                This subroutine is called at the beginning of ::stomateLpj_clear in the 
!!                ::stomate_lpj module.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::firstcall_all_phenology, ::firstcall_hum, ::firstcall_moi, ::firstcall_humgdd, 
!!                ::firstcall_moigdd
!!
!! REFERENCE(S)  : None
!!
!! FLOWCHART     : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE phenology_clear
    firstcall_all_phenology=.TRUE.
    firstcall_hum=.TRUE.
    firstcall_moi = .TRUE.
    firstcall_humgdd = .TRUE.
    firstcall_moigdd = .TRUE.
    firstcall_moi_C4 = .TRUE.
  END SUBROUTINE phenology_clear


!! ================================================================================================================================
!! SUBROUTINE   : phenology
!!
!>\BRIEF          This subroutine controls the detection of the beginning of the growing season 
!!                (if dormancy has been long enough), leaf onset, given favourable biometeorological 
!!                conditions, and leaf growth and biomass allocation when leaf biomass is low (i.e. 
!!                at the start of the growing season.
!!
!! DESCRIPTION  : This subroutine is called by the module ::stomate_lpj and deals with the beginning of the  
!!                growing season. First it is established whether the beginning of the growing season is
!!                allowed. This occurs if the dormance period has been long enough (i.e. greater 
!!                than a minimum PFT-dependent threshold, specified by ::lowgpp_time), 
!!                AND if the last beginning of the growing season was a sufficiently long time ago 
!!                (i.e. when the growing season length is greater than a minimum threshold, specified
!!                by ::min_growthinit_time, which is defined in this module to be 300 days. \n
!!                The dormancy time-length is represented by the variable 
!!                ::time_lowgpp, which is calculated in ::stomate_season. It is increased by 
!!                the stomate time step when the weekly GPP is lower than a threshold. Otherwise
!!                it is set to zero. \n
!!                ::lowgpp_time is set for each PFT in ::stomate_data from a table of all
!!                PFT values (::lowgpp_time_tab), which is defined in ::stomate_constants. \n
!!                The growing season length is given by ::when_growthinit, which increases
!!                by the stomate time-step at each call to this phenology module, except for when
!!                leaf onset is detected, when it is set to 0. \n
!!                If these two conditions are met, leaf onset occurs if the biometeorological 
!!                conditions are also met. This is determined by the leaf onset models, which are
!!                biome-specific. Each PFT is looped over (ignoring bare soil).
!!                The onset phenology model is selected, (according to the parameter 
!!                ::pheno_model, which is initialised in stomate_data), and called. \n
!!                There are six leaf onset phenology models currently being used by ORCHIDEE. 
!!                These are: 'hum' and 'moi', which are based exclusively on moisture conditions,
!!                'humgdd' and 'moigdd', which are based on both temperature and moisture conditions,
!!                'ncdgdd', which is based on a "chilling" requirement for leaf onset, and 
!!                'ngd', which is based on the number of growing days since the temperature was 
!!                above a certain threshold, to account for the end of soil frost.
!!                Those models which are based mostly on temperature conditions are used for
!!                temperate and boreal biomes, and those which include a moisture condition are used
!!                for tropical biomes. More detail on the biometeorological conditions is provided
!!                in the sections on the individual onset models. \n
!!                The moisture conditions are based on the concept of plant "moisture availability".
!!                This is based on the soil humidity (relative soil moisture), but is moderated by
!!                the root density profile, as per the equation:
!!                \latexonly
!!                \input{phenology_moiavail_eqn1.tex}
!!                \endlatexonly
!!                \n
!!                Although some studies have shown that the length of the photoperiod is important
!!                in determining onset (and senescence) dates, this is not considered in the current
!!                versions of the onset models (Krinner et al., 2005). \n
!!                If conditions are favourable, leaf onset occurs (::begin_leaves is set to TRUE), 
!!                ::when_growthinit is set to 0.0, and the growing season has begun. \n
!!                Following the detection of leaf onset, biomass is allocated from the carbohydrate 
!!                reserves equally to the leaves and roots IF the leaf biomass is lower than a minimum
!!                threshold, which is calculated in this subroutine from the parameter
!!                ::lai_initmin, divided by the specific leaf area (both of which are
!!                PFT-dependent and set in ::stomate_constants). \n
!!                Finally, if biomass is required to be allocated from the carbohydrate reserve 
!!                because the leaf biomass is too low, the leaf age and leaf age distribution is 
!!                re-set. In this case the youngest age class fraction is set to 1 and all other   
!!                leaf age class fractions are set to 0. All leaf ages are set to 0. If there is 
!!                no biomass in the carbohydrate reserve, leaf onset will not occur and the PFT
!!                will disappear from the grid cell (Krinner et al., 2005). \n
!!                This subrouting is called in ::stomate_lpj.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::biomass, 
!!                        ::when_growthinit,
!!                        ::leaf age distribution
!!                        ::leaf fraction
!!
!! REFERENCE(S) :
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{phenology_flowchart.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE phenology (npts, dt, PFTpresent, &
       veget_max, &
       t2m_longterm, t2m_month, t2m_week, gpp, &
       maxmoiavail_lastyear, minmoiavail_lastyear, &
       moiavail_month, moiavail_week, &
       gdd_m5_dormance, gdd_midwinter, ncd_dormance, ngd_minus5, &
       senescence, time_hum_min, &
       biomass, leaf_frac, leaf_age, &
       when_growthinit, co2_to_bm, &
       pdlai, slai, deltai, ssla, & !added for crops, xuhui
       begin_leaves, &!)
!JCADD
       sla_calc)
!ENDJCADD

    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                                          :: npts                 !! Domain size - number of grid 
                                                                                                !! cells (unitless) 
    REAL(r_std), INTENT(in)                                             :: dt                   !! time step (dt_days)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                            :: PFTpresent           !! PFT exists (true/false)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: veget_max            !! "maximal" coverage fraction of a 
                                                                                                !! PFT (LAI -> infinity) on ground 
                                                                                                !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts), INTENT(in)                            :: t2m_longterm         !! "long term" 2 meter reference 
                                                                                                !! temperatures (K) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                            :: t2m_month            !! "monthly" 2-meter temperatures 
                                                                                                !! (K) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                            :: t2m_week             !! "weekly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: gpp                  !! daily gross primary productivity 
                                                                                                !! @tex ($gC m^{-2} of 
                                                                                                !! ground/day$) @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: maxmoiavail_lastyear !! last year's maximum moisture 
                                                                                                !! availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: minmoiavail_lastyear !! last year's minimum moisture 
                                                                                                !! availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: moiavail_month       !! "monthly" moisture availability 
                                                                                                !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: moiavail_week        !! "weekly" moisture availability 
                                                                                                !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: gdd_m5_dormance      !! growing degree days above a 
                                                                                                !! threshold of -5 deg C (C) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)                     :: gdd_midwinter        !! growing degree days, since 
                                                                                                !! midwinter (C) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: ncd_dormance         !! number of chilling days since 
                                                                                                !! leaves were lost (days) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: ngd_minus5           !! number of growing days above a 
                                                                                                !! threshold of -5 deg C (days) 
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                            :: senescence           !! is the plant senescent? (only 
                                                                                                !! for deciduous trees - 
                                                                                                !! carbohydrate reserve) 
                                                                                                !! (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                        :: time_hum_min         !! time elapsed since strongest 
                                                                                                !! moisture availability (days) 
    !!!! added for crops
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: pdlai
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: slai
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: deltai
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: ssla
    !!! end crops, xuhui
    !
    !! 0.2 Ouput variables 
    !

    !
    !! 0.3 Modified variables
    !
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout)    :: biomass              !! biomass @tex ($gC m^{-2} of 
                                                                                                !! ground$) @endtex
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)           :: leaf_frac            !! fraction of leaves in leaf age 
                                                                                                !! class (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)           :: leaf_age             !! leaf age (days)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)                     :: when_growthinit      !! how many days since the 
                                                                                                !! beginning of the growing season 
                                                                                                !! (days) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)                     :: co2_to_bm            !! co2 taken up by carbohydrate 
                                                                                                !! reserve at the beginning of the 
                                                                                                !! growing season @tex ($gC m^{-2} 
                                                                                                !! of total ground/day$) @endtex 
                                                                                                ! NV passge 2D
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)                         :: begin_leaves         !! signal to start putting leaves 
!JCADD
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: sla_calc
!ENDJCADD                                                                                                !! on (true/false) 
    !
    !! 0.4 Local variables
    !
    LOGICAL, DIMENSION(npts,nvm)                                        :: allow_initpheno      !! are we allowed to decalre the 
                                                                                                !! beginning of the growing 
                                                                                                !! season? (true/false) 
    REAL(r_std), DIMENSION(npts)                                        :: bm_wanted            !! biomass we would like to have 
                                                                                                !! @tex ($gC m^{-2} of ground$) 
                                                                                                !! @endtex 
    REAL(r_std), DIMENSION(npts)                                        :: bm_use               !! biomass we use (from 
                                                                                                !! carbohydrate reserve or from 
                                                                                                !! atmosphere) @tex ($gC m^{-2} of 
                                                                                                !! ground$) @endtex
    REAL(r_std), DIMENSION(npts)                                        :: lm_min               !! minimum leaf mass @tex ($gC 
                                                                                                !! m^{-2} of ground$) @endtex 
    LOGICAL(r_std), DIMENSION(npts)                                     :: age_reset            !! does the leaf age distribution 
                                                                                                !! have to be reset? (true/false) 
    INTEGER(i_std)                                                      :: i,j,m                !! indices (unitless)
    REAL(r_std), DIMENSION(npts,nvm)                                    :: histvar              !! controls the history output 
                                                                                                !! level - 0: nothing is written; 
                                                                                                !! 10: everything is written 
                                                                                                !! (0-10, unitless) 

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering phenology'

    !
    !! 1. first call - output message giving the setting of the ::always_init
    !!    and ::min_growthinit_time parameters.
    !

    IF ( firstcall_all_phenology ) THEN

       WRITE(numout,*) 'phenology:'

       WRITE(numout,*) '   > take carbon from atmosphere if carbohydrate' // &
            ' reserve too small (::always_init): ', always_init

       WRITE(numout,*) '   > minimum time since last beginning of a growing' // &
            ' season (d) (::min_growthinit_time): ', min_growthinit_time

       firstcall_all_phenology = .FALSE.

    ENDIF

    !
    !! 2. Detection of the beginning of the growing season.
    !

    !
    !! 2.1 allow detection of the beginning of the growing season if dormance was
    !!     long enough (i.e. when ::time_lowgpp, which is calculated in ::stomate_season, 
    !!     is above a certain PFT-dependent threshold, ::lowgpp_time, 
    !!     which is given in ::stomate_constants),
    !!     AND the last beginning of growing season was a sufficiently long time ago 
    !!     (i.e. when ::when_growthinit, which is calculated in this module, 
    !!     is greater than ::min_growthinit_time, which is declared at the beginning of this module).
    !!     If these conditions are met, allow_initpheno is set to TRUE. Each PFT is looped over.
    !

    allow_initpheno(:,1) = .FALSE.
    DO j = 2,nvm

       WHERE ( when_growthinit(:,j) .GT. min_growthinit_time )
          allow_initpheno(:,j) = .TRUE.
       ELSEWHERE
          allow_initpheno(:,j) = .FALSE.
       ENDWHERE

    ENDDO

    WHERE(allow_initpheno)
       histvar=un
    ELSEWHERE
       histvar=zero
    ENDWHERE

    CALL xios_orchidee_send_field("ALLOW_INITPHENO",histvar)

    CALL histwrite_p (hist_id_stomate, 'ALLOW_INITPHENO', itime, histvar, npts*nvm, horipft_index)

    !
    !! 2.2 increase the ::when_growthinit counter, which gives the number of days since the beginning of the growing season.
    !!     Needed for allocation and for the detection of the beginning of the growing season.
    !

    when_growthinit(:,:) = when_growthinit(:,:) + dt

    !
    !! 3. Leaf onset.
    !!    Check biometeorological conditions using the onset phenological models, 
    !!    which are different for each PFT group (i.e. grass versus tropical etc. 
    !!    See below for more detail on the different models and which PFTs use each model).
    !

    !! - By default: phenology does not start (::begin_leaves set to FALSE).
    begin_leaves(:,:) = .FALSE.

    !! - The onset phenology model is selected, (according to the parameter ::pheno_model, 
    !! which is initialised in stomate_data), and called.
    !! Each PFT is looped over (ignoring bare soil). 
    !! If conditions are favourable, begin_leaves is set to TRUE.
    
    ! parameter used in all the differents models of phenology 
    t_always = ZeroCelsius + t_always_add

    DO j = 2,nvm ! Loop over # PFTs

       SELECT CASE ( pheno_model(j) )

       CASE ( 'hum' )

          CALL pheno_hum (npts, j, PFTpresent, allow_initpheno, &
               moiavail_month, moiavail_week, &
               maxmoiavail_lastyear, minmoiavail_lastyear, &
               begin_leaves)

       CASE ( 'moi' )

          CALL pheno_moi (npts, j, PFTpresent, allow_initpheno, &
               time_hum_min, &
               moiavail_month, moiavail_week, &
               begin_leaves)


       CASE ( 'ncdgdd' )

          CALL pheno_ncdgdd (npts, j, PFTpresent, allow_initpheno, &
               ncd_dormance, gdd_midwinter, &
               t2m_month, t2m_week, begin_leaves)

       CASE ( 'ngd' )

          CALL pheno_ngd (npts, j, PFTpresent, allow_initpheno, ngd_minus5, &
               t2m_month, t2m_week, begin_leaves)

       CASE ( 'humgdd' )

          CALL pheno_humgdd (npts, j, PFTpresent, allow_initpheno, gdd_m5_dormance, &
               maxmoiavail_lastyear, minmoiavail_lastyear, &
               t2m_longterm, t2m_month, t2m_week, &
               moiavail_week, moiavail_month, &
               begin_leaves)

       CASE ( 'moigdd' )

          CALL pheno_moigdd (npts, j, PFTpresent, allow_initpheno, gdd_m5_dormance, &
               time_hum_min, &
               t2m_longterm, t2m_month, t2m_week, &
               moiavail_week, moiavail_month, &
               begin_leaves, pdlai, slai)

       CASE ( 'moi_C4' )

          CALL pheno_moi_C4 (npts, j, PFTpresent, allow_initpheno, gdd_m5_dormance, &
               time_hum_min, &
               t2m_longterm, t2m_month, t2m_week, &
               moiavail_week, moiavail_month, &
               begin_leaves)

       CASE ( 'none' )

          ! no action

       CASE default

          WRITE(numout,*) 'phenology: don''t know how to treat this PFT.'
          WRITE(numout,*) '  number: (::j)',j
          WRITE(numout,*) '  phenology model (::pheno_model(j)) : ',pheno_model(j)
          CALL ipslerr_p(3,'stomate phenology','Cannot treat this PFT','','')

       END SELECT

    ENDDO

    WHERE(begin_leaves)
       histvar=un
    ELSEWHERE
       histvar=zero
    ENDWHERE

    CALL xios_orchidee_send_field("BEGIN_LEAVES",histvar)

    CALL histwrite_p (hist_id_stomate, 'BEGIN_LEAVES', itime, histvar, npts*nvm, horipft_index)

    !
    !! 4. Leaf growth and biomass allocation when leaf biomass is low.
    !!   Leaves start to grow if biometeorological conditions are favourable (::begin_leaves == TRUE) and if
    !!   leaf growth is allowed (::allow_initpheno == TRUE).
    !!   PFTs and then grid cells are looped over.
    !

    DO j = 2,nvm ! Loop over # PFTs

       age_reset(:) = .FALSE.

       DO i = 1, npts

          IF ( begin_leaves(i,j) ) THEN
            IF ( .NOT. ok_LAIdev(j) ) THEN

                 !! 4.1 First minimum biomass is calculated using the following equation:
                 !!     \latexonly
                 !!     \input{phenology_lm_min_eqn2.tex}
                 !!     \endlatexonly
                 !!     \n
    !JCMODIF
    !             lm_min(i) = lai_initmin(j) / sla(j)
                 lm_min(i) = lai_initmin(j) / sla_calc(i,j)
    !ENDJCMODIF
                 !! 4.2 If leaf biomass is lower than the minimum biomass then biomass must be allocated from the carbohydrate 
                 !!     reserves to leaves and roots.
    
                 IF ( biomass(i,j,ileaf,icarbon) .LT. lm_min(i) ) THEN
    
                    !
                    !! 4.2.1 Determine how much biomass is available to use
                    !!       First calculate how much biomass is wanted/required 
                    !!       (::bm_wanted = 2 x the minimum leaf biomass).
                    !
    
                    bm_wanted(i) = 2. * lm_min(i)
    
                    !! 4.2.2 If the biomass in the carbohydrate reserves is less than the required biomass
                    !!       take the required amount of carbon from the atmosphere and put it into the
                    !!       carbohydrate reserve. This only occurs if the parameter ::always_init 
                    !!       (set at beginning of this ::subroutine) is TRUE. Default is FALSE. 
    
                    IF ( always_init .AND. ( biomass(i,j,icarbres,icarbon) .LT. bm_wanted(i) ) ) THEN
                       !NV passage 2D
                       co2_to_bm(i,j) = co2_to_bm(i,j) + ( bm_wanted(i) - biomass(i,j,icarbres,icarbon) ) / dt
    
                       biomass(i,j,icarbres,icarbon) = bm_wanted(i)
    
                    ENDIF
                    
                    !! 4.2.3 The biomass available to use is set to be the minimum of the biomass of the carbohydrate reservoir (if 
                    !! carbon not taken from the atmosphere), and the wanted biomass.
                    bm_use(i) = MIN( biomass(i,j,icarbres,icarbon), bm_wanted(i) )
    
                    !
                    !! 4.2.4 divide the biomass which is available to use equally between the leaves and roots.
                    !
    
                    biomass(i,j,ileaf,icarbon) = biomass(i,j,ileaf,icarbon) + bm_use(i) / 2.
    
                    biomass(i,j,iroot,icarbon) = biomass(i,j,iroot,icarbon) + bm_use(i) / 2.
    
                    !
                    !! 4.2.5 decrease carbohydrate reservoir biomass by the amount that's been allocated to the leaves and roots
                    !
    
                    biomass(i,j,icarbres,icarbon) = biomass(i,j,icarbres,icarbon) - bm_use(i)
    
                    !
                    !! 4.2.6 set reset leaf age distribution (::age_reset) flag. Default is TRUE.
                    !     (done later for better vectorization)
                    !
    
                    age_reset(i) = .TRUE.
    
                 ENDIF  ! leaf mass is very low
            ELSE ! crop STICS
                 !! bm_use(i) = MIN( biomass(i,j,icarbres,icarbon), deltai(i,
                 !j)/ssla(i, j)*10000. ) ! available carbon pools
                 ! problem is that although the growth of biomass is reduced,
                 ! but the lai growth continues and sla does not change. So
                 ! there is a decoupling of the two, xuhui
                 bm_use(i) = deltai(i, j)/ssla(i, j)*2*10000.  ! forcibly giving the lai at the begining of the growing season
                 biomass(i,j,ileaf,icarbon) = biomass(i,j,ileaf,icarbon) + bm_use(i) / 2.   ! this is the first day for leave growth 
                 biomass(i,j,iroot,icarbon) = biomass(i,j,iroot,icarbon) + bm_use(i) / 2.
                 biomass(i,j,icarbres,icarbon) = biomass(i,j,icarbres,icarbon) - bm_use(i)
                 age_reset(i) = .TRUE.
                 IF (printlev>=4) THEN
                     WRITE(numout,*) 'in phenology, the bm_use and biomass is:', bm_use
!                     WRITE(numout,*) 'in phenology, the reserve is:', biomass(:,12:14, icarbres,icarbon)
!                     WRITE(numout,*) 'in phenology, the deltai is:', deltai(:,12:14)
                 ENDIF
            ENDIF ! if no crop

             !
             !! 4.3 reset when_growthinit counter: start of the growing season
             !

             when_growthinit(i,j) = zero

          ENDIF    ! start of the growing season

       ENDDO      ! loop over grid points

       !
       !! 4.4 reset leaf age distribution where necessary (i.e. when age_reset is TRUE)
       !!     simply say that everything is in the youngest age class
       !

       !! 4.4.1 fractions - set the youngest age class fraction to 1 and all other leaf age class fractions to 0.

       WHERE ( age_reset(:) )
          leaf_frac(:,j,1) = un
       ENDWHERE
       DO m = 2, nleafages
          WHERE ( age_reset(:) )
             leaf_frac(:,j,m) = zero
          ENDWHERE
       ENDDO

       !! 4.4.2 ages - set all leaf ages to 0.

       DO m = 1, nleafages
          WHERE ( age_reset(:) )
             leaf_age(:,j,m) = zero
          ENDWHERE
       ENDDO

    ENDDO        ! loop over # PFTs


    IF (printlev>=3) WRITE(numout,*) 'Leaving phenology'

  END SUBROUTINE phenology


!! ================================================================================================================================
!! SUBROUTINE   : pheno_hum 
!!
!>\BRIEF          The 'hum' onset model initiate leaf onset based exclusively on moisture 
!!                availability criteria. 
!!                Currently no PFTs are assigned to this onset model.
!!
!! DESCRIPTION  : This model is for tropical biomes, where temperatures are high but moisture
!!                might be a limiting factor on growth. It is based on leaf onset model 4a in 
!!                Botta et al. (2000), which adopts the approach of Le Roux (1995). \n
!!                Leaf onset occurs if the monthly moisture availability is still quite
!!                low (i.e. lower than the weekly availability), but the weekly availability is 
!!                higher than the critical threshold ::availability_crit (as it reacts faster), 
!!                which indicates the weekly moisture availability is increasing.
!!                OR if the monthly moisture availability is high enough (i.e. above the 
!!                threshold value ::moiavail_always), leaf onset is initiated if this has not 
!!                already happened. This allows vegetation in arid areas to respond to rapidly
!!                changing soil moisture conditions (Krinner et al., 2005). \n
!!                The critical weekly moisture availability threshold (::availability_crit), is
!!                calculated in this subroutine, and is a function of last year's maximum and
!!                minimum moisture availability and the PFT-dependent parameter
!!                ::hum_frac, which specifies how much of last year's available 
!!                moisture is required for leaf onset, as per the equation:
!!                \latexonly
!!                \input{phenology_moi_availcrit_eqn3.tex}
!!                \endlatexonly
!!                \n
!!                ::hum_frac is set for each PFT in ::stomate_data from a table
!!                which contains all the PFT values (::hum_frac_tab) in ::stomate_constants. \n
!!                Last year's maximum and minimum moisture availability and the monthly and 
!!                weekly moisture availability are  
!!                The ::pheno_hum subroutine is called in the subroutine ::phenology. 
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::begin_leaves - specifies whether leaf growth can start.
!!
!! REFERENCE(S) : 
!! - Botta, A., N. Viovy, P. Ciais, P. Friedlingstein and P. Monfray (2000), 
!! A global prognostic scheme of leaf onset using satellite data,
!! Global Change Biology, 207, 337-347.
!! - Le Roux, X. (1995), Etude et modelisation des echanges d'eau et d'energie
!! sol-vegetation-atmosphere dans une savane humide, PhD Thesis, University
!! Pierre et Marie Curie, Paris, France.
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{pheno_hum.png}
!! \endlatexonly
!! \n             
!_ ================================================================================================================================

  SUBROUTINE pheno_hum (npts, j, PFTpresent, allow_initpheno, &
       moiavail_month, moiavail_week, &
       maxmoiavail_lastyear, minmoiavail_lastyear, &
       begin_leaves)

    !
    !! 0. Variable and parameter declarations
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                                             :: npts                  !! Domain size - number of 
                                                                                                    !! grid cells (unitless) 
    INTEGER(i_std), INTENT(in)                                             :: j                     !! PFT index (unitless)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                               :: PFTpresent            !! PFT exists (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                               :: allow_initpheno       !! are we allowed to 
                                                                                                    !! declare the beginning of 
                                                                                                    !! the growing season? 
                                                                                                    !! (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                           :: moiavail_month        !! "monthly" moisture 
                                                                                                    !! availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                           :: moiavail_week         !! "weekly" moisture 
                                                                                                    !! availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                           :: maxmoiavail_lastyear  !! last year's maximum 
                                                                                                    !! moisture availability 
                                                                                                    !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                           :: minmoiavail_lastyear  !! last year's minimum 
                                                                                                    !! moisture availability 
                                                                                                    !! (0-1, unitless)

    !
    !! 0.2 Output variables
    !

    !
    !! 0.3 Modified variables
    !
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)                            :: begin_leaves          !! signal to start putting 
                                                                                                    !! leaves on (true/false) 

    !
    !! 0.4 Local variables
    !
    REAL(r_std)                                                            :: moiavail_always       !! critical monthly 
                                                                                                    !! moisture availability - set 
                                                                                                    !! for tree or grass 
                                                                                                    !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts)                                           :: availability_crit     !! critical weekly moisture 
                                                                                                    !! availability (0-1, unitless)
    INTEGER(i_std)                                                         :: i                     !! index (unitless)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering hum'

    !
    !! 1. Initializations
    !

    !
    !! 1.1 first call - outputs the name of onset model and the moisture availability 
    !!     parameters for tree and grass
    !

    IF ( firstcall_hum ) THEN

       WRITE(numout,*) 'pheno_hum:'
       WRITE(numout,*) '   > moisture availability above which moisture tendency doesn''t matter: '
       WRITE(numout,*) '         trees (::moiavail_always_tree): ', moiavail_always_tree
       WRITE(numout,*) '         grasses (::moiavail_always_grass):', moiavail_always_grass

       firstcall_hum = .FALSE.

    ENDIF

    !
    !! 1.2 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    !! 1.3 check the critical value ::hum_frac is defined. If not, stop.
    !

    IF ( hum_frac(j) .EQ. undef ) THEN

       WRITE(numout,*) 'hum: hum_frac is undefined for PFT (::j)',j
       CALL ipslerr_p(3,'stomate phenology','hum_frac is undefined for this PFT','','')

    ENDIF

    !
    !! 1.4 set the critical monthly moisture availability above which we always detect the beginning of the
    !!     growing season - set as the moisture availability for trees or grass.
    !

    IF ( is_tree(j) ) THEN
       moiavail_always = moiavail_always_tree
    ELSE
       moiavail_always = moiavail_always_grass
    ENDIF

    !
    !! 2. Check if biometeorological conditions are favourable for leaf growth.
    !! The PFT has to be there and start of growing season must be allowed
    !

    DO i = 1, npts

       IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) ) THEN

          !! 2.1 Calculate the critical weekly moisture availability: depends linearly on the last year 
          !! minimum and maximum moisture availabilities, and on the parameter ::hum_frac.

          availability_crit(i) = minmoiavail_lastyear(i,j) + hum_frac(j) * &
               ( maxmoiavail_lastyear(i,j) - minmoiavail_lastyear(i,j) )

          !! 2.2 Determine if growing season should start (if so, ::begin_leaves set to TRUE).
          !!     Leaf onset occurs if the monthly moisture availability is still quite
          !!     low (i.e. lower than the weekly availability), but the weekly availability is 
          !!     already higher than the critical threshold ::availability_crit (as it reacts faster), 
          !!     which indicates the weekly moisture availability is increasing.
          !!     OR if the monthly moisture availability is high enough (i.e. above the threshold value 
          !!     ::moiavail_always), leaf onset is initiated if this has not already happened.

          IF ( ( ( moiavail_week(i,j)  .GE. availability_crit(i) ) .AND. &
               ( moiavail_month(i,j) .LT. moiavail_week(i,j) )   ) .OR. &
               ( moiavail_month(i,j) .GE. moiavail_always )                ) THEN
             begin_leaves(i,j) = .TRUE.
          ENDIF

       ENDIF        ! PFT there and start of growing season allowed

    ENDDO ! end loop over grid points

    IF (printlev>=4) WRITE(numout,*) 'Leaving hum'

  END SUBROUTINE pheno_hum


!! ================================================================================================================================
!! SUBROUTINE   : pheno_moi
!!
!>\BRIEF          The 'moi' onset model (::pheno_moi) initiates leaf onset based exclusively 
!!                on moisture availability criteria. 
!!                It is very similar to the 'hum' onset model but instead of the weekly moisture 
!!                availability being higher than a constant threshold, the condition is that the 
!!                moisture minimum happened a sufficiently long time ago. 
!!                Currently PFT 3 (Tropical Broad-leaved Raingreen) is assigned to this model.
!!
!! DESCRIPTION  : This model is for tropical biomes, where temperatures are high but moisture
!!                might be a limiting factor on growth. It is based on leaf onset model 4b in 
!!                Botta et al. (2000).
!!                Leaf onset begins if the plant moisture availability minimum was a sufficiently  
!!                time ago, as specified by the PFT-dependent parameter ::hum_min_time 
!!                AND if the "monthly" moisture availability is lower than the "weekly"
!!                availability (indicating that soil moisture is increasing).
!!                OR if the monthly moisture availability is high enough (i.e. above the threshold 
!!                value ::moiavail_always), leaf onset is initiated if this has not already 
!!                happened. \n
!!                ::hum_min_time is set for each PFT in ::stomate_data, and is 
!!                defined in the table ::hum_min_time_tab in ::stomate_constants. \n
!!                ::moiavail_always is defined for both tree and grass in this subroutine 
!!                (set to 1. and 0.6 respectively). \n
!!                The ::pheno_moi subroutine is called in the subroutine ::phenology. 
!!
!! RECENT CHANGE(S): None
!!        
!! MAIN OUTPUT VARIABLE(S): ::begin_leaves - specifies whether leaf growth can start.
!!
!! REFERENCE(S) : 
!! - Botta, A., N. Viovy, P. Ciais, P. Friedlingstein and P. Monfray (2000), 
!! A global prognostic scheme of leaf onset using satellite data,
!! Global Change Biology, 207, 337-347.
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{pheno_moi.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE pheno_moi (npts, j, PFTpresent, allow_initpheno, &
       time_hum_min, &
       moiavail_month, moiavail_week, &
       begin_leaves)

    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                               :: npts            !! Domain size - number of grid cells (unitless)
    INTEGER(i_std), INTENT(in)                               :: j               !! PFT index (unitless)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: PFTpresent      !! PFT exists (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: allow_initpheno !! are we allowed to declare the beginning of the 
                                                                                !! growing season? (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: time_hum_min    !! time elapsed since strongest moisture 
                                                                                !! availability (days) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: moiavail_month  !! "monthly" moisture availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: moiavail_week   !! "weekly" moisture availability (0-1, unitless)

    !
    !! 0.2 Output variables
    !
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)              :: begin_leaves    !! signal to start putting leaves on (true/false)

    !
    !! 0.3 Modified variables
    !

    !
    !! 0.4 Local variables
    !
    REAL(r_std)                                              :: moiavail_always                 !! critical moisture availability - 
                                                                                                !! set for tree or grass 
                                                                                                !! (0-1, unitless)
    INTEGER(i_std)                                           :: i                               !! index (unitless)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering moi'

    !
    !! 1. Initializations
    !

    !
    !! 1.1 first call - outputs the name of onset model and the moisture availability 
    !!     parameters for tree and grass
    !

    IF ( firstcall_moi ) THEN

       WRITE(numout,*) 'pheno_moi:'
       WRITE(numout,*) '   > moisture availability above which moisture tendency doesn''t matter: '
       WRITE(numout,*) '         trees (::moiavail_always_tree):', moiavail_always_tree
       WRITE(numout,*) '         grasses (::moiavail_always_grass):', moiavail_always_grass

       firstcall_moi = .FALSE.

    ENDIF

    !
    !! 1.2 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    !! 1.3 check the critical value ::hum_min_time is definded. If not, stop
    !

    IF ( hum_min_time(j) .EQ. undef ) THEN

       WRITE(numout,*) 'moi: hum_min_time is undefined for PFT (::j) ',j
       CALL ipslerr_p(3,'stomate phenology','hum_min_time is undefined for this PFT','','')

    ENDIF

    !
    !! 1.4 set the critical monthly moisture availability above which we always detect the beginning of the
    !!     growing season - set as the moisture availability for trees or grass.
    !

    IF ( is_tree(j) ) THEN
       moiavail_always = moiavail_always_tree
    ELSE
       moiavail_always = moiavail_always_grass
    ENDIF

    !
    !! 2. Check if biometeorological conditions are favourable for leaf growth.
    !! The PFT has to be there and start of growing season must be allowed.
    !

    DO i = 1, npts

       IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) ) THEN
          
          !! 2.1 Determine if growing season should start (if so, ::begin_leaves set to TRUE).
          !!     The favorable season starts if the moisture minimum (::time_hum_min) was a sufficiently long 
          !!     time ago, i.e. greater than the threshold specified by the parameter ::hum_min_time 
          !!     and if the "monthly" moisture availability is lower than the "weekly"
          !!     availability (indicating that soil moisture is increasing).
          !!     OR if the monthly moisture availability is high enough (i.e. above the threshold value 
          !!     ::moiavail_always), initiate the growing season if this has not happened yet.

          IF  ( ( ( moiavail_week(i,j) .GT. moiavail_month(i,j) ) .AND. &
               ( time_hum_min(i,j) .GT. hum_min_time(j) )    ) .OR. &
               ( moiavail_month(i,j) .GE. moiavail_always )                     ) THEN
             begin_leaves(i,j) = .TRUE.
          ENDIF

       ENDIF        ! PFT there and start of growing season allowed

    ENDDO ! end loop over grid points

    IF (printlev>=4) WRITE(numout,*) 'Leaving moi'

  END SUBROUTINE pheno_moi


!! ================================================================================================================================
!! SUBROUTINE   : pheno_humgdd
!!
!>\BRIEF          The 'humgdd' onset model initiates leaf onset based on mixed conditions of 
!!                temperature and moisture availability criteria. 
!!                Currently no PFTs are assigned to this onset model. 
!!
!! DESCRIPTION  : In this model the Growing Degree Day (GDD) model (Chuine, 2000) is combined 
!!                with the 'hum' onset model (::pheno_hum), which has previously been described,
!!                in order to account for dependence on both temperature and moisture conditions 
!!                in warmer climates. \n. 
!!                The GDD model specifies that daily temperatures above a threshold of -5  
!!                degrees C are summed, minus this threshold, giving the GDD, starting from 
!!                the beginning of the dormancy period (::time_lowgpp>0), i.e. since the leaves 
!!                were lost. \n.
!!                The dormancy time-length is represented by the variable 
!!                ::time_lowgpp, which is calculated in ::stomate_season. It is increased by 
!!                the stomate time step when the weekly GPP is lower than a threshold. Otherwise
!!                it is set to zero. \n
!!                Leaf onset begins when the a PFT-dependent GDD-threshold is reached.
!!                In addition there are temperature and moisture conditions.
!!                The temperature condition specifies that the monthly temperature has to be 
!!                higher than a constant threshold (::t_always) OR
!!                the weekly temperature is higher than the monthly temperature.
!!                There has to be at least some moisture. The moisture condition 
!!                is exactly the same as the 'hum' onset model (::pheno_hum), which has already
!!                been described. \n
!!                The GDD (::gdd_m5_dormance) is calculated in ::stomate_season. GDD is set to 
!!                undef if beginning of the growing season detected, i.e. when there is GPP 
!!                (::time_lowgpp>0).
!!                The parameter ::t_always is defined as 10 degrees C in this subroutine, 
!!                as are the parameters ::moisture_avail_tree and ::moisture_avail_grass 
!!                (set to 1 and 0.6 respectively), which are used in the moisture condition 
!!                (see ::pheno_moi onset model description). \n
!!                The PFT-dependent GDD threshold (::gdd_crit) is calculated as in the onset 
!!                model ::pheno_humgdd, using the equation:
!!                \latexonly
!!                \input{phenology_hummoigdd_gddcrit_eqn4.tex}
!!                \endlatexonly
!!                \n
!!                The three GDDcrit parameters (::gdd(j,*)) are set for each PFT in 
!!                ::stomate_data, and three tables defining each of the three critical GDD
!!                parameters for each PFT is given in ::gdd_crit1_tab, ::gdd_crit2_tab and 
!!                ::gdd_crit3_tab in ::stomate_constants. \n
!!                The ::pheno_humgdd subroutine is called in the subroutine ::phenology. 
!!
!! RECENT CHANGES: None
!!                
!! MAIN OUTPUT VARIABLES: ::begin_leaves - specifies whether leaf growth can start
!!
!! REFERENCE(S) : 
!! - Botta, A., N. Viovy, P. Ciais, P. Friedlingstein and P. Monfray (2000), 
!! A global prognostic scheme of leaf onset using satellite data,
!! Global Change Biology, 207, 337-347.
!! - Chuine, I (2000), A unified model for the budburst of trees, Journal of 
!! Theoretical Biology, 207, 337-347.
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{pheno_humgdd.png}
!! \endlatexonly
!! \n             
!_ ================================================================================================================================

  SUBROUTINE pheno_humgdd (npts, j, PFTpresent, allow_initpheno, gdd, &
       maxmoiavail_lastyear, minmoiavail_lastyear, &
       t2m_longterm, t2m_month, t2m_week, &
       moiavail_week, moiavail_month, &
       begin_leaves)

    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                               :: npts                    !! Domain size - number of grid cells 
                                                                                        !! (unitless) 
    INTEGER(i_std), INTENT(in)                               :: j                       !! PFT index (unitless)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: PFTpresent              !! PFT exists (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: allow_initpheno         !! are we allowed to declare the beginning 
                                                                                        !! of the growing season? (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: gdd                     !! growing degree days, calculated since 
                                                                                        !! leaves have fallen (C) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: maxmoiavail_lastyear    !! last year's maximum moisture 
                                                                                        !! availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: minmoiavail_lastyear    !! last year's minimum moisture 
                                                                                        !! availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_longterm            !! "long term" 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_month               !! "monthly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_week                !! "weekly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: moiavail_week           !! "weekly" moisture availability 
                                                                                        !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: moiavail_month          !! "monthly" moisture availability 
                                                                                        !! (0-1, unitless)

    !
    !! 0.2 Output variables
    !
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)              :: begin_leaves            !! signal to start putting leaves on 
                                                                                        !! (true/false) 

    !
    !! 0.3 Modified variables
    !

    !
    !! 0.4 Local variables
    !
    REAL(r_std)                                              :: moiavail_always                 !! critical moisture availability - 
                                                                                                !! set for tree or grass 
                                                                                                !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts)                             :: moiavail_crit                   !! critical moisture availability 
                                                                                                !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts)                             :: tl                              !! long term temperature (C)
    REAL(r_std), DIMENSION(npts)                             :: gdd_crit                        !! critical GDD (C)
    INTEGER(i_std)                                           :: i                               !! index (unitless)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering humgdd'

    !
    !! 1. Initializations
    !

    !
    !! 1.1 first call - outputs the name of the onset model, the values of the  
    !!     moisture availability parameters for tree and grass, and the value of the 
    !!     critical monthly temperature.
    !

    IF ( firstcall_humgdd ) THEN

       WRITE(numout,*) 'pheno_humgdd:'
       WRITE(numout,*) '   > moisture availability above which moisture tendency doesn''t matter: '
       WRITE(numout,*) '         trees (::moiavail_always_tree): ', moiavail_always_tree
       WRITE(numout,*) '         grasses (::moiavail_always_grass): ', moiavail_always_grass
       WRITE(numout,*) '   > monthly temp. above which temp. tendency doesn''t matter: ', &
            t_always

       firstcall_humgdd = .FALSE.

    ENDIF

    !
    !! 1.2 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    !! 1.3 check the critical values ::gdd and ::pheno_crit_hum_frac are defined.
    !!     If not, stop.
    !

    IF ( ANY(pheno_gdd_crit(j,:) .EQ. undef) ) THEN

       WRITE(numout,*) 'humgdd: pheno_gdd_crit is undefined for PFT (::j) ',j
       CALL ipslerr_p(3,'stomate phenology','pheno_gdd_crit is undefined for this PFT','','')

    ENDIF

    IF ( hum_frac(j) .EQ. undef ) THEN

       WRITE(numout,*) 'humgdd: hum_frac is undefined for PFT (::j) ',j
       CALL ipslerr_p(3,'stomate phenology','hum_frac is undefined for this PFT','','')

    ENDIF

    !
    !! 1.4 set the critical moisture availability above which we always detect the beginning of the
    !!     growing season - set as the moisture availability for trees or grass.
    !

    IF ( is_tree(j) ) THEN
       moiavail_always = moiavail_always_tree
    ELSE
       moiavail_always = moiavail_always_grass
    ENDIF

    !
    !! 2. Check if biometeorological conditions are favourable for leaf growth.
    !!   The PFT has to be there, start of growing season must be allowed, 
    !!   and GDD has to be defined.
    !

    DO i = 1, npts

       IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) .AND. &
            ( gdd(i,j) .NE. undef )                           ) THEN

          !! 2.1 Calculate the critical weekly moisture availability: depends linearly on the last year 
          !! minimum and maximum moisture availabilities, and on the parameter ::hum_frac.,
          !! (as in the ::pheno_hum model), as per the equation:

          moiavail_crit(i) = minmoiavail_lastyear(i,j) + hum_frac(j) * &
               ( maxmoiavail_lastyear(i,j) - minmoiavail_lastyear(i,j) )

          !! 2.2 Calculate the critical GDD (::gdd_crit), which is a function of the PFT-dependent 
          !!     critical GDD and the "long term" 2 meter air temperatures.  

          tl(i) =  t2m_longterm(i) - ZeroCelsius
          gdd_crit(i) = pheno_gdd_crit(j,1) + tl(i)*pheno_gdd_crit(j,2) + &
               tl(i)*tl(i)*pheno_gdd_crit(j,3)
          
          !! 2.3 Determine if the growing season should start (if so, ::begin_leaves set to TRUE).
          !!     - Has the critical gdd been reached and is the temperature increasing?
          !!     - Is there at least some humidity/moisture availability?
          !!     This occurs if the critical gdd (::gdd_crit) has been reached 
          !!     AND that is temperature increasing, which is true either if the monthly
          !!     temperature being higher than the threshold ::t_always, OR if the weekly
          !!     temperature is higher than the monthly, 
          !!     AND finally that there is sufficient moisture availability, which is 
          !!     the same condition as for the ::pheno_hum onset model.

          IF ( ( gdd(i,j) .GE. gdd_crit(i) ) .AND. &
               ( ( t2m_week(i) .GT. t2m_month(i) ) .OR. &
               ( t2m_month(i) .GT. t_always )          ) .AND. &
               ( ( ( moiavail_week(i,j)  .GE. moiavail_crit(i) ) .AND. &
               ( moiavail_month(i,j) .LT. moiavail_crit(i) )        ) .OR. &
               ( moiavail_month(i,j) .GE. moiavail_always )                   ) )  THEN
             begin_leaves(i,j) = .TRUE.
          ENDIF

       ENDIF        ! PFT there and start of growing season allowed

    ENDDO ! End loop over grid points

    IF (printlev>=4) WRITE(numout,*) 'Leaving humgdd'

  END SUBROUTINE pheno_humgdd


!! ================================================================================================================================
!! SUBROUTINE   : pheno_moigdd
!!
!>\BRIEF          The 'moigdd' onset model initiates leaf onset based on mixed temperature 
!!                and moisture availability criteria.
!!                Currently PFTs 10 - 13 (C3 and C4 grass, and C3 and C4 agriculture) 
!!                are assigned to this model. 
!!
!! DESCRIPTION  : This onset model combines the GDD model (Chuine, 2000), as described for 
!!                the 'humgdd' onset model (::pheno_humgdd), and the 'moi' model, in order 
!!                to account for dependence on both temperature and moisture conditions in
!!                warmer climates. \n
!!                Leaf onset begins when the a PFT-dependent GDD threshold is reached.
!!                In addition there are temperature and moisture conditions.
!!                The temperature condition specifies that the monthly temperature has to be 
!!                higher than a constant threshold (::t_always) OR
!!                the weekly temperature is higher than the monthly temperature.
!!                There has to be at least some moisture. The moisture condition 
!!                is exactly the same as the 'moi' onset model (::pheno_moi), which has
!!                already been described. \n
!!                GDD is set to undef if beginning of the growing season detected.
!!                As in the ::pheno_humgdd model, the parameter ::t_always is defined as 
!!                10 degrees C in this subroutine, as are the parameters ::moisture_avail_tree
!!                and ::moisture_avail_grass (set to 1 and 0.6 respectively), which are used
!!                in the moisture condition (see ::pheno_moi onset model description). \n
!!                The PFT-dependent GDD threshold (::gdd_crit) is calculated as in the onset 
!!                model ::pheno_humgdd, using the equation:
!!                \latexonly
!!                \input{phenology_hummoigdd_gddcrit_eqn4.tex}
!!                \endlatexonly
!!                \n
!!                where i and j are the grid cell and PFT respectively.
!!                The three GDDcrit parameters (::gdd(j,*)) are set for each PFT in 
!!                ::stomate_data, and three tables defining each of the three critical GDD
!!                parameters for each PFT is given in ::gdd_crit1_tab, ::gdd_crit2_tab and 
!!                ::gdd_crit3_tab in ::stomate_constants. \n
!!                The ::pheno_moigdd subroutine is called in the subroutine ::phenology. 
!!
!! RECENT CHANGE(S): Added temperature threshold for C4 grass (pheno_moigdd_t_crit), Dan Zhu april 2015
!!                
!! MAIN OUTPUT VARIABLE(S): ::begin_leaves - specifies whether leaf growth can start
!!
!! REFERENCE(S) : 
!! - Botta, A., N. Viovy, P. Ciais, P. Friedlingstein and P. Monfray (2000), 
!! A global prognostic scheme of leaf onset using satellite data,
!! Global Change Biology, 207, 337-347.
!! - Chuine, I (2000), A unified model for the budburst of trees, Journal of 
!! Theoretical Biology, 207, 337-347.
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!! - Still et al., Global distribution of C3 and C4 vegetation: Carbon cycle implications, 
!! 2003, Global Biogeochemmical Cycles, DOI: 10.1029/2001GB001807. 
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{pheno_moigdd.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE pheno_moigdd (npts, j, PFTpresent, allow_initpheno, gdd, &
       time_hum_min, &
       t2m_longterm, t2m_month, t2m_week, &
       moiavail_week, moiavail_month, &
       begin_leaves, pdlai, slai)

    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                               :: npts            !! Domain size - number of grid cells (unitless)
    INTEGER(i_std), INTENT(in)                               :: j               !! PFT index (unitless)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: PFTpresent      !! PFT exists (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: allow_initpheno !! are we allowed to decalre the beginning of the 
                                                                                !! growing season? (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: gdd             !! growing degree days, calculated since leaves 
                                                                                !! have fallen (C) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: time_hum_min    !! time elapsed since strongest moisture 
                                                                                !! availability (days) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_longterm    !! "long term" 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_month       !! "monthly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_week        !! "weekly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: moiavail_week   !! "weekly" moisture availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: moiavail_month  !! "monthly" moisture availability (0-1, unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: pdlai
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: slai

    !
    !! 0.2 Output variables
    !

    !
    !! 0.3 Modified variables
    !
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)              :: begin_leaves    !! signal to start putting leaves on (true/false)

    !
    !! 0.4 Local variables
    !
    REAL(r_std)                                              :: moiavail_always                 !! critical moisture availability - 
                                                                                                !! set for tree or grass 
                                                                                                !! (0-1, unitless)
    REAL(r_std), DIMENSION(npts)                             :: tl                              !! long term temperature (C)
    REAL(r_std), DIMENSION(npts)                             :: gdd_crit                        !! critical GDD (C)
    INTEGER(i_std)                                           :: i                               !! index (unitless)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering moigdd'

    !
    !! 1. Initializations
    !

    !
    !! 1.1 first call - outputs the name of the onset model, the values of the  
    !!     moisture availability parameters for tree and grass, and the value of the 
    !!     critical monthly temperature.
    !

    IF ( firstcall_moigdd ) THEN

       WRITE(numout,*) 'pheno_moigdd:'
       WRITE(numout,*) '   > moisture availability above which moisture tendency doesn''t matter: '
       WRITE(numout,*) '         trees (::moiavail_always_tree) :', moiavail_always_tree
       WRITE(numout,*) '         grasses (::moiavail_always_grass) :', moiavail_always_grass
       WRITE(numout,*) '   > monthly temp. above which temp. tendency doesn''t matter (::t_always): ', &
            t_always

       firstcall_moigdd = .FALSE.

    ENDIF

    !
    !! 1.2 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    !! 1.3 check the critical values ::gdd and ::pheno_crit_hum_min_time are defined.
    !!     If not, stop.
    !

    IF ( ANY(pheno_gdd_crit(j,:) .EQ. undef) ) THEN

       WRITE(numout,*) 'moigdd: pheno_gdd_crit is undefined for PFT',j
       CALL ipslerr_p(3,'stomate phenology','pheno_gdd is undefined for this PFT','','')

    ENDIF

    IF ( hum_min_time(j) .EQ. undef ) THEN

       WRITE(numout,*) 'moigdd: hum_min_time is undefined for PFT',j
       CALL ipslerr_p(3,'stomate phenology','hum_min is undefined for this PFT','','')

    ENDIF

    !
    !! 1.4 set the critical moisture availability above which we always detect the beginning of the
    !!     growing season - set as the moisture availability for trees or grass.
    !

    IF ( is_tree(j) ) THEN
       moiavail_always = moiavail_always_tree
    ELSE
       moiavail_always = moiavail_always_grass
    ENDIF

    !
    !! 2. Check if biometeorological conditions are favourable for leaf growth.
    !!    The PFT has to be there, the start of growing season must be allowed, 
    !!    and GDD has to be defined.
    !

    DO i = 1, npts
        IF ( ok_LAIdev(j) ) THEN

            IF ( ( (slai(i, j)-pdlai(i, j)) .gt. zero ) .AND. ( pdlai(i, j) .eq. zero ) ) THEN
                begin_leaves(i, j) = .TRUE.
            ELSE
                begin_leaves(i, j) = .FALSE.
            ENDIF
        ELSE ! natural PFTs
            IF (printlev>=4) THEN 
                WRITE(numout,*),'PFTpresent(i,j)',PFTpresent(i,j)
                WRITE(numout,*) 'allow_initpheno(i,j)', allow_initpheno(i,j)
                WRITE(numout,*) 'gdd(i,j)', gdd(i,j)
           ENDIF

           IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) .AND. &
                ( gdd(i,j) .NE. undef )                           ) THEN
              
              !! 2.1 Calculate the critical GDD (::gdd_crit), which is a function of the PFT-dependent 
              !!     critical GDD and the "long term" 2 meter air temperatures 
              
              tl(i) = t2m_longterm(i) - ZeroCelsius
              gdd_crit(i) = pheno_gdd_crit(j,1) + tl(i)*pheno_gdd_crit(j,2) + &
                   tl(i)*tl(i)*pheno_gdd_crit(j,3)
              IF (printlev>=4) THEN 
                !!! debug output xuhui
                WRITE(numout,*) 'gdd(i,j)', gdd(i,j)
                WRITE(numout,*) 'time_hum_min(i,j)', time_hum_min(i,j)
                WRITE(numout,*) 'gdd_crit(i)', gdd_crit(i)
                WRITE(numout,*) 't_always',t_always
                WRITE(numout,*) 'moiavail_always',moiavail_always
              ENDIF
    
    
              !! 2.2 Determine if the growing season should start (if so, ::begin_leaves set to TRUE).
              !!     This occurs if the critical gdd (::gdd_crit) has been reached 
              !!     AND that is temperature increasing, which is true either if the monthly
              !!     temperature being higher than the threshold ::t_always, OR if the weekly
              !!     temperature is higher than the monthly, 
              !!     AND finally that there is sufficient moisture availability, which is 
              !!     the same condition as for the ::pheno_moi onset model.
              !!     AND when pheno_moigdd_t_crit is set(for C4 grass), if the average temperature threshold is reached

              IF ( ( gdd(i,j) .GE. gdd_crit(i) ) .AND. &
                   ( ( t2m_week(i) .GT. t2m_month(i) ) .OR. &
                     ( t2m_month(i) .GT. t_always )  ) .AND. &
                   ( ( ( time_hum_min(i,j) .GT. hum_min_time(j) ) .AND. &
                     ( moiavail_week(i,j) .GT. moiavail_month(i,j) ) ) .OR. &
                     ( moiavail_month(i,j) .GE. moiavail_always )  ) .AND. &
                   ( ( pheno_moigdd_t_crit(j) == undef ) .OR. &
                     (t2m_month(i) .GT. (ZeroCelsius + pheno_moigdd_t_crit(j))) ) ) THEN
    
                 begin_leaves(i,j) = .TRUE.
                 
              ENDIF
    
           ENDIF        ! PFT there and start of growing season allowed
        ENDIF ! crop PFT    
    ENDDO

    IF (printlev>=4) WRITE(numout,*) 'Leaving moigdd'

  END SUBROUTINE pheno_moigdd


!! ================================================================================================================================
!! SUBROUTINE   : pheno_ncdgdd
!!
!>\BRIEF          The Number of Chilling Days - Growing Degree Day (NCD-GDD) model initiates 
!!                leaf onset if a certain relationship between the number of chilling days (NCD) 
!!                since leaves were lost, and the growing degree days (GDD) since midwinter, is 
!!                fulfilled. 
!!                Currently PFT 6 (Temperate Broad-leaved Summergreen) and PFT 8 (Boreal Broad-
!!                leaved Summergreen) are assigned to this model.
!! 
!! DESCRIPTION  : Experiments have shown that some
!!                species have a "chilling" requirement, i.e. their physiology needs cold 
!!                temperatures to trigger the mechanism that will allow the following budburst 
!!                (e.g. Orlandi et al., 2004). 
!!                An increase in chilling days, defined as a day with a daily mean air
!!                temperature below a PFT-dependent threshold, reduces a plant's GDD demand 
!!                (Cannell and Smith, 1986; Murray et al., (1989); Botta et al., 2000).
!!                The GDD threshold therefore decreases as NCD 
!!                increases, using the following empirical negative explonential law:
!!                \latexonly
!!                \input{phenology_ncdgdd_gddmin_eqn5.tex}
!!                \endlatexonly
!!                \n
!!                The constants used have been calibrated against data CHECK FOR REFERENCE OR PERSON WHO DID UPDATE.
!!                Leaf onset begins if the GDD is higher than the calculated minimum GDD
!!                (dependent upon NCD) AND if the weekly temperature is higher than the monthly 
!!                temperature. This is to ensure the temperature is increasing. \n
!!                The dormancy time-length is represented by the variable 
!!                ::time_lowgpp, which is calculated in ::stomate_season. It is increased by 
!!                the stomate time step when the weekly GPP is lower than a threshold. Otherwise
!!                it is set to zero. \n
!!                The NCD (::ncd_dormance) is calculated in ::stomate_season as  
!!                the number of days with a temperature below a PFT-dependent constant threshold
!!                (::ncdgdd_temp), starting from the beginning of the dormancy period
!!                (::time_lowgpp>0), i.e. since the leaves were lost. \n
!!                The growing degree day sum of the temperatures higher than 
!!                ::ncdgdd_temp (GDD) since midwinter (::gdd_midwinter) 
!!                is also calculated in ::stomate_season.
!!                Midwinter is detected if the monthly temperature is lower than the weekly
!!                temperature AND  the monthly temperature is lower than the long-term
!!                temperature. ::gdd_minter is therefore set to 0 at the beginning of midwinter
!!                and increased with each temperature greater than the PFT-dependent threshold.
!!                When midsummer is detected (the opposite of the above conditions), 
!!                ::gdd_midwinter is set to undef.
!!                CHECK! WHEN TO START OF DORMANCY BEEN MODIFIED FROM BOTTA- ADD IN?
!!                The ::pheno_ncdgdd subroutine is called in the subroutine ::phenology. 
!!
!! RECENT CHANGE(S): None
!!                
!! MAIN OUTPUT VARIABLE(S): ::begin_leaves - specifies whether leaf growth can start
!!
!! REFERENCE(S) :
!! - Botta, A., N. Viovy, P. Ciais, P. Friedlingstein and P. Monfray (2000), 
!! A global prognostic scheme of leaf onset using satellite data,
!! Global Change Biology, 207, 337-347.
!! - Cannell, M.J.R. and R.I. Smith (1986), Climatic warming, spring budburst and
!! frost damage on trees, Journal of Applied Ecology, 23, 177-191.
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!! - Murray, M.B., G.R. Cannell and R.I. Smith (1989), Date of budburst of fifteen 
!! tree species in Britain following climatic warming, Journal of Applied Ecology,
!! 26, 693-700.
!! - Orlandi, F., H. Garcia-Mozo, L.V. Ezquerra, B. Romano, E. Dominquez, C. Galan,
!! and M. Fornaciari (2004), Phenological olive chilling requirements in Umbria
!! (Italy) and Andalusia (Spain), Plant Biosystems, 138, 111-116. 
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{pheno_ncdgdd.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE pheno_ncdgdd (npts, j, PFTpresent, allow_initpheno, &
       ncd_dormance, gdd_midwinter, &
       t2m_month, t2m_week, begin_leaves)

    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                               :: npts            !! Domain size - number of grid cells (unitless)
    INTEGER(i_std), INTENT(in)                               :: j               !! PFT index (unitless)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: PFTpresent      !! PFT exists (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: allow_initpheno !! are we allowed to declare the beginning of the 
                                                                                !! growing season? (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: ncd_dormance    !! number of chilling days since leaves were lost 
                                                                                !! (days) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)          :: gdd_midwinter   !! growing degree days since midwinter (C)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_month       !! "monthly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_week        !! "weekly" 2-meter temperatures (K)

    !
    !! 0.2 Output variables
    !

    !
    !! 0.3 Modified variables
    !
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)              :: begin_leaves    !! signal to start putting leaves on (true/false)

    !
    !! 0.4 Local variables
    !
    INTEGER(i_std)                                           :: i               !! index (unitless)
    REAL(r_std)                                              :: gdd_min         !! critical gdd (C)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering ncdgdd'

    !
    !! 1. Initializations
    !

    !
    !! 1.1 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    !! 1.2 check the critical value ::ncdgdd_temp is defined.
    !!     If not, stop.
    !

    IF ( ncdgdd_temp(j) .EQ. undef ) THEN

       WRITE(numout,*) 'ncdgdd: ncdgdd_temp is undefined for PFT (::j) ',j
       CALL ipslerr_p(3,'stomate phenology','ncdgdd_temp this PFT','','')

    ENDIF

    !
    !! 2. Check if biometeorological conditions are favourable for leaf growth.    
    !!    PFT has to be there and start of growing season must be allowed.
    !

    DO i = 1, npts ! loop over grid points

       IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) .AND. &
            ( gdd_midwinter(i,j) .NE. undef ) .AND. &
            ( ncd_dormance(i,j) .NE. undef )                  ) THEN

          !! 2.1 Calculate the critical gdd, which is related to ::ncd_dormance
          !!     using an empirical negative exponential law as described above.           

          gdd_min = ( gddncd_ref / exp(gddncd_curve*ncd_dormance(i,j)) - gddncd_offset )

          !! 2.2 Determine if the growing season should start (if so, ::begin_leaves set to TRUE).
          !!     This occurs if the critical GDD been reached AND the temperatures are increasing.
          !!     If the growing season has started, ::gdd_midwinter is set to "undef". 

          IF ( ( gdd_midwinter(i,j) .GE. gdd_min ) .AND. &
               ( t2m_week(i) .GT. t2m_month(i) ) ) THEN
             begin_leaves(i,j) = .TRUE.
             gdd_midwinter(i,j)=undef
          ENDIF

       ENDIF        ! PFT there and start of growing season allowed

    ENDDO ! end loop over grid points

    IF (printlev>=4) WRITE(numout,*) 'Leaving ncdgdd'

  END SUBROUTINE pheno_ncdgdd


!! ================================================================================================================================
!! SUBROUTINE   : pheno_ngd
!!
!>\BRIEF          The Number of Growing Days (NGD) leaf onset model initiates leaf onset if the NGD, 
!!                defined as the number of days with temperature above a constant threshold, 
!!                exceeds a critical value.
!!                Currently PFT 9 (Boreal Leedleleaf Summergreen) is assigned to this model. 
!!
!! DESCRIPTION    The NGD model is a variant of the GDD model. The model was proposed by Botta et
!!                al. (2000) for boreal and arctic biomes, and is designed to estimate 
!!                leaf onset after the end of soil frost. 
!!                The NDG (::ngd_minus5) is the number of days with a daily mean air 
!!                temperature of greater than -5 degrees C, 
!!                starting from the beginning of the dormancy period (i.e. time since the leaves 
!!                were lost/GPP below a certain threshold).
!!                Leaf onset begins if the NGD is higher than the PFT-dependent constant threshold, 
!!                ::ngd,  AND if the weekly temperature is higher than the monthly 
!!                temperature. \n
!!                The dormancy time-length is represented by the variable 
!!                ::time_lowgpp, which is calculated in ::stomate_season. It is increased by 
!!                the stomate time step when the weekly GPP is lower than a threshold. Otherwise
!!                it is set to zero. \n
!!                ::ngd_minus5 is also calculated in ::stomate_season. It is initialised at the
!!                beginning of the dormancy period (::time_lowgpp>0), and increased by the 
!!                stomate time step when the temperature > -5 degrees C. \n
!!                ::ngd is set for each PFT in ::stomate_data, and a 
!!                table defining the minimum NGD for each PFT is given in ::ngd_crit_tab
!!                in ::stomate_constants. \n
!!                The ::pheno_ngd subroutine is called in the subroutine ::phenology.      
!!
!! RECENT CHANGE(S): None
!!                
!! MAIN OUTPUT VARIABLE(S): ::begin_leaves - specifies whether leaf growth can start
!!
!! REFERENCE(S) : 
!! - Botta, A., N. Viovy, P. Ciais, P. Friedlingstein and P. Monfray (2000), 
!! A global prognostic scheme of leaf onset using satellite data,
!! Global Change Biology, 207, 337-347. 
!! - Krinner, G., N. Viovy, N. de Noblet-Ducoudre, J. Ogee, J. Polcher, P. 
!! Friedlingstein, P. Ciais, S. Sitch and I.C. Prentice (2005), A dynamic global
!! vegetation model for studies of the coupled atmosphere-biosphere system, Global
!! Biogeochemical Cycles, 19, doi:10.1029/2003GB002199.
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{pheno_ngd.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE pheno_ngd (npts, j, PFTpresent, allow_initpheno, ngd, &
       t2m_month, t2m_week, begin_leaves)

    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                               :: npts            !! Domain size - number of grid cells (unitless)
    INTEGER(i_std), INTENT(in)                               :: j               !! PFT index (unitless)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: PFTpresent      !! PFT exists (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                 :: allow_initpheno !! are we allowed to declare the beginning of the 
                                                                                !! growing season? (true/false) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)             :: ngd             !! growing degree days (C)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_month       !! "monthly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_week        !! "weekly" 2-meter temperatures (K)

    !
    !! 0.2 Output variables
    !

    !
    !! 0.3 Modified variables
    !
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)              :: begin_leaves    !! signal to start putting leaves on (true/false)

    !
    !! 0.4 Local variables
    !
    INTEGER(i_std)                                           :: i               !! index (unitless)

    !! =========================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering ngd'

    !
    !! 1. Initializations
    !

    !
    !! 1.1 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    !! 1.2 check the critical value ::ngd_crit is defined.
    !!     If not, stop.
    !

    IF ( ngd_crit(j) .EQ. undef ) THEN

       WRITE(numout,*) 'ngd: ngd_crit is undefined for PFT (::j) ',j
       CALL ipslerr_p(3,'stomate phenology','ngd_crit is undefined for this PFT','','')

    ENDIF

    !
    !! 2. Check if biometeorological conditions are favourable for leaf growth.
    !!    PFT has to be there and start of growing season must be allowed.
    !

    DO i = 1, npts

       IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) ) THEN

          !! 2.1 Determine if the growing season should start (if so, ::begin_leaves set to TRUE).
          !!     This occurs if the critical NGD has been reached AND are temperatures increasing.

          IF ( ( ngd(i,j) .GE. ngd_crit(j) ) .AND. &
               ( t2m_week(i) .GT. t2m_month(i) )        ) THEN
             begin_leaves(i,j) = .TRUE.
          ENDIF

       ENDIF        ! PFT there and start of growing season allowed

    ENDDO ! end loop over grid points

    IF (printlev>=4) WRITE(numout,*) 'Leaving ngd'

  END SUBROUTINE pheno_ngd
  !
  ! ==============================================================================
  ! DZ modified for C4 grass: mean monthly temperature must be greater than 22
  ! degree for C4 to begin leaves (based on moigdd).
  !

  SUBROUTINE pheno_moi_C4 (npts, j, PFTpresent, allow_initpheno, gdd, &
       time_hum_min, &
       t2m_longterm, t2m_month, t2m_week, &
       moiavail_week, moiavail_month, &
       begin_leaves)

    !
    ! 0 declarations
    !

    ! 0.1 input

    ! Domain size
    INTEGER(i_std), INTENT(in)                                     :: npts
    ! PFT index
    INTEGER(i_std), INTENT(in)                               :: j
    ! PFT exists
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)               :: PFTpresent
    ! are we allowed to decalre the beginning of the growing season?
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)               :: allow_initpheno
    ! growing degree days, calculated since leaves have fallen
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: gdd
    ! time elapsed since strongest moisture availability (d)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: time_hum_min
    ! "long term" 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_longterm
    ! "monthly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_month
    ! "weekly" 2-meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)                 :: t2m_week
    ! "weekly" moisture availability
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: moiavail_week
    ! "monthly" moisture availability
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: moiavail_month

    ! 0.2 output

    ! signal to start putting leaves on
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)              :: begin_leaves
    ! 0.3 local

    ! moisture availability above which moisture tendency doesn't matter
    REAL(r_std)                                              :: moiavail_always
    ! long term temperature, C
    REAL(r_std), DIMENSION(npts)                             :: tl
    ! critical GDD
    REAL(r_std), DIMENSION(npts)                             :: gdd_crit
    ! index
    INTEGER(i_std)                                           :: i

    ! =========================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering moi_C4'

    !
    ! 1 Initializations
    !
    ! 1.1 messages
    !

    IF ( firstcall_moi_C4 ) THEN

       WRITE(numout,*) 'pheno_moi_C4:'
       WRITE(numout,*) '   > moisture availability above which moisture tendency doesn''t matter: '
       WRITE(numout,*) '         trees:', moiavail_always_tree
       WRITE(numout,*) '         grasses:', moiavail_always_grass
       WRITE(numout,*) '   > monthly temp. above which temp. tendency doesn''t matter: ', &
            t_always

       firstcall_moi_C4 = .FALSE.

    ENDIF

    !

    ! 1.1 initialize output
    !

    begin_leaves(:,j) = .FALSE.

    !
    ! 1.2 check the prescribed critical values
    !
    IF ( ANY(pheno_gdd_crit(j,:) .EQ. undef) ) THEN

       WRITE(numout,*) 'moi_C4: pheno_gdd_crit is undefined for PFT',j
       WRITE(numout,*) 'We stop.'
       STOP

    ENDIF

    IF ( hum_min_time(j) .EQ. undef ) THEN

       WRITE(numout,*) 'moi_C4: hum_min_time is undefined for PFT',j
       WRITE(numout,*) 'We stop.'
       STOP

    ENDIF

    !
    ! 1.3 critical moisture availability above which we always detect the
    ! beginning of the
    !     growing season.
    !

    IF ( is_tree(j) ) THEN
       moiavail_always = moiavail_always_tree
    ELSE
       moiavail_always = moiavail_always_grass
    ENDIF

    !
    ! 2 PFT has to be there, start of growing season must be allowed, 
    !   and gdd has to be defined
    !

    DO i = 1, npts

       IF ( PFTpresent(i,j) .AND. allow_initpheno(i,j) .AND. &
            ( gdd(i,j) .NE. undef )                           ) THEN

          ! is critical gdd reached and is temperature increasing?
          ! has enough time gone by since moisture minimum and is moisture
          ! increasing?

          tl(i) = t2m_longterm(i) - ZeroCelsius
          gdd_crit(i) = pheno_gdd_crit(j,1) + tl(i)*pheno_gdd_crit(j,2) + &
               tl(i)*tl(i)*pheno_gdd_crit(j,3)

          IF ( ( gdd(i,j) .GE. gdd_crit(i) ) .AND. &
               ( ( t2m_week(i) .GT. t2m_month(i) ) .OR. &
               ( t2m_month(i) .GT. t_always )          ) .AND. &
               ( ( ( time_hum_min(i,j) .GT. hum_min_time(j) ) .AND. &
               ( moiavail_week(i,j) .GT. moiavail_month(i,j) )    ) .OR. &
               ( moiavail_month(i,j) .GE. moiavail_always )    ) .AND. &
               ( t2m_month(i) .GT. (ZeroCelsius + 22)  )    )  THEN
             begin_leaves(i,j) = .TRUE.
          ENDIF

       ENDIF        ! PFT there and start of growing season allowed

    ENDDO

    IF (printlev>=3) WRITE(numout,*) 'Leaving moi_C4'

  END SUBROUTINE pheno_moi_C4


END MODULE stomate_phenology
