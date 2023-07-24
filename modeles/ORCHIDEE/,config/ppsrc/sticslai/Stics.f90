










!> Module Stics
!> - Description : Stics main module. 
!! This module contains the main subroutine of LAIdev for calculating the LAI of crops
!! This module is built by XCW. // 06/06/2013
!>

module Stics

    USE ioipsl      ! USE the tools of ioipsl
    USE grid        ! USE the module regarding the grid

 
    IMPLICIT NONE 
    PUBLIC driver_stics

    !variable (parameters) declaration
    INTEGER(i_std), PARAMETER :: crop_nbjmax = 300  !!Maximum days of a crop cycle simulation

       CHARACTER(len=3)  :: P_codeplante
       CHARACTER(len=3)  :: P_stade0
       INTEGER(i_std)  :: P_iplt0
       INTEGER(i_std)  :: P_iwater 
       CHARACTER(len=7)  :: P_codesimul
       INTEGER(i_std)  :: P_codelaitr
       REAL(r_std)  :: P_slamax
       REAL(r_std)  :: P_slamin
       INTEGER(i_std)  :: P_codeperenne    !! annual crop (1) or perennial crop (2)
       INTEGER(i_std)  :: P_codcueille     !! harvest option: cutting (1) or picking (2)
       INTEGER(i_std)  :: P_codegdh        !! hourly (1) or daily (2) calculation of development unit 
       INTEGER(i_std)  :: P_codetemp       !! 
       INTEGER(i_std)  :: P_coderetflo
       INTEGER(i_std)  :: P_codeinnact
       INTEGER(i_std)  :: P_codeh2oact
       REAL(r_std)  :: P_stressdev
       REAL(r_std)  :: P_innlai
       REAL(r_std)  :: P_innsenes
       INTEGER(i_std)  :: P_codebfroid
       INTEGER(i_std)  :: P_codephot
       INTEGER(i_std)  :: P_codedormance
       INTEGER(i_std)  :: P_codefauche     !! option of cut modes for forage crops: yes (1) no (2)
       INTEGER(i_std)  :: P_codetempfauche !! option of the reference temperature to compute cutting sum of temperatures : upvt (1) udevair (2)
       INTEGER(i_std)  :: P_codlainet
       INTEGER(i_std)  :: P_codeindetermin
       INTEGER(i_std)  :: P_codeinitprec
       INTEGER(i_std)  :: P_culturean 
       REAL(r_std)  :: P_jvc
       REAL(r_std)  :: P_tfroid
       REAL(r_std)  :: P_ampfroid 
       REAL(r_std)  :: P_jvcmini
       REAL(r_std)  :: P_tgmin
       REAL(r_std)  :: P_stpltger
       REAL(r_std)  :: P_profsem
       REAL(r_std)  :: P_propjgermin        !! minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax 
       REAL(r_std)  :: P_tdmax
       INTEGER(i_std)  :: P_nbjgerlim              !! Threshold number of day after grain imbibition without germination lack // days
       REAL(r_std)  :: P_densitesem
       REAL(r_std)  :: P_vigueurbat                !!  indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 //
       INTEGER(i_std)  :: P_codepluiepoquet              !!  option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2
       INTEGER(i_std)  :: P_codehypo
       REAL(r_std)  :: P_elmax
       REAL(r_std)  :: P_belong
       REAL(r_std)  :: P_celong
       INTEGER(i_std)  :: P_nlevlim1             !! number of days after germination decreasing the emerged plants if emergence has not occur // days 
       INTEGER(i_std)  :: P_nlevlim2             !! number of days after germination after which the emerged plants are null // days 
       INTEGER(i_std)  :: P_codrecolte           !! harvest mode : all the plant (1) or just the fruits (2)
       INTEGER(i_std)  :: P_variete              !! variety number in the technical file // SD
       INTEGER(i_std)  :: P_codegermin           !! option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2)

       INTEGER(i_std)  :: codeulaivernal
       REAL(r_std)  :: P_swfacmin
      
       ! STICS:: LAI CALCULATION
       REAL(r_std)  :: P_laiplantule
       REAL(r_std)  :: P_vlaimax
       REAL(r_std)  :: P_stlevamf
       REAL(r_std)  :: P_stamflax
       REAL(r_std)  :: P_udlaimax
       REAL(r_std)  :: P_laicomp
       REAL(r_std)  :: P_adens                   !! Interplant competition parameter 
       REAL(r_std)  :: P_bdens
       REAL(r_std)  :: P_tcxstop
       REAL(r_std)  :: P_tcmax
       REAL(r_std)  :: P_tcmin
       REAL(r_std)  :: P_dlaimax
       REAL(r_std)  :: P_dlaimin
       REAL(r_std)  :: P_pentlaimax
       REAL(r_std)  :: P_tigefeuil               !! stem (structural part)/leaf proportion // SD  

       REAL(r_std)  :: P_stlaxsen
       REAL(r_std)  :: P_stsenlan                   !! Interplant competition parameter 
       REAL(r_std)  :: P_stlevdrp
       REAL(r_std)  :: P_stflodrp
       REAL(r_std)  :: P_stdrpmat
       REAL(r_std)  :: P_stdrpdes
       REAL(r_std)  :: P_phyllotherme
       REAL(r_std)  :: P_lai0
       REAL(r_std)  :: P_tustressmin

       ! STICS:: LAI SENESCENCE  
       INTEGER(i_std)  :: P_nbfgellev
       INTEGER(i_std)  :: P_maxgs
       REAL(r_std)  :: P_ratiodurvieI
       REAL(r_std)  :: P_durvieF
       REAL(r_std)  :: P_ratiosen
       REAL(r_std)  :: P_tdmin
!       ! Xuhui 140321
!       INTEGER(i_std),SAVE :: doyhistst
!       INTEGER(i_std),SAVE :: hist_sencour
!       INTEGER(i_std),SAVE :: hist_latest
!       REAL(r_std), DIMENSION(crop_nbjmax,5), SAVE :: histgrowth
       ! 5 variables are dltai dltams tdevelop ulai durvie
       ! intial these variables to zeros at Stics_init
       ! end 140321
      
       ! STICS:: F_humerac

       REAL(r_std)  :: P_sensrsec
      
       ! STICS:: GEL
       INTEGER(i_std)  :: P_codgellev
       INTEGER(i_std)  :: P_codgeljuv
       INTEGER(i_std)  :: P_codgelveg
       REAL(r_std)  :: P_tletale
       REAL(r_std)  :: P_tdebgel
       REAL(r_std)  :: P_tgellev10
       REAL(r_std)  :: P_tgellev90
       REAL(r_std)  :: P_tgeljuv10
       REAL(r_std)  :: P_tgeljuv90
       REAL(r_std)  :: P_tgelveg10
       REAL(r_std)  :: P_tgelveg90
       ! STICS:: Photoperiod
       REAL(r_std)  :: P_sensiphot
       REAL(r_std)  :: P_phosat
       REAL(r_std)  :: P_phobase

       INTEGER(i_std) :: P_codesla


contains

    subroutine driver_stics(n,                               &  ! IN
                            in_cycle,                       &  ! INout  
                            f_crop_recycle,                 &  ! INOUT
                            f_sen_lai,                       &  ! INout  
                            t2m_daily,                       &  ! IN  
                            t2m_min_daily,                       &  ! IN  
                            gdh_daily,                       &  ! IN
                            phoi,                            &  ! IN
                            onarretesomcourdrp,              &  ! IN                          
                            stempdiag_cm_daily,              &  ! IN
                            shumdiag_cm_day,                 &  ! IN
!                            nlevobs,                         &  ! IN
!                            namfobs,                         &  ! IN
!                            nfloobs,                         &  ! IN
!                            nlanobs,                         &  ! IN
!                            nlaxobs,                         &  ! IN
!                            nmatobs,                         &  ! IN
!                            nrecobs,                         &  ! IN
!                            nsenobs,                         &  ! IN
!                            ndrpobs,                         &  ! IN
                            dltams,                          &  ! IN
                            eop,                             &  ! IN
                            masec,                           &  ! IN
                            masecveg,                        &  ! IN
                            nsendltams,                      &  ! INOUT
                            nsendltai,                       &
                            nsenpfeuilverte,                 &
                            nsendurvie,                      &
                            nsenndurvie,                     &
                            densiteequiv,                    &
                            nplt,                            &
                            tursla,                          &
                            sla,                             &
                            pfeuilverte,                     &
                            bsenlai,                         &
                            zrac,                            &
                            nrec,                            &
                            nlan,                            &
                            tcult,                           &
                            udevair,                         &
                            udevcult,                        &
                            ndrp,                            &
                            rfvi,                            &
                            nlev,                            &
                            nger,                            &
                            etatvernal,                      &
                            caljvc,                          &
                            rfpi,                            &
                            upvt,                            &
                            utp,                             &
                            somcour,                         &
                            somcourdrp,                      &
                            somcourutp,                      &
                            tdevelop,                        &
                            somtemp,                         &
                            somcourfauche,                   &
                            stpltger,                        &
                            R_stamflax,                      &
                            R_stlaxsen,                      &
                            R_stsenlan,                      &
                            stlevflo,                        &
                            nflo,                            &
                            R_stlevdrp,                      &
                            R_stflodrp,                      &
                            R_stdrpmat,                      &
                            nmat,                            &
                            nlax,                            &
                            nrecbutoir,                      &
                            group,                           &
                            ndebdes,                         &
                            R_stdrpdes,                      &
                            densite,                         &
                            densitelev,                      &
                            coeflev,                         &
                            densiteger,                      &
                            somelong,                        &
                            somger,                          &
                            humectation,                     &
                            nbjhumec,                        &
                            somtemphumec,                    &
                            stpltlev,                        &
                            namf,                            &
                            stmatrec,                        &
                            tustress,                        &
                            slai,                             &
                            somfeuille,                      &
                            pdlai,                           &
                            nbfeuille,                       &
                            reajust,                         &
                            ulai,                            &
                            pdulai,                          &
                            efdensite,                       &
                            tempeff,                         &
                            nstopfeuille,                    &
                            deltai,                          &
                            vmax,                            &
                            nsen,                            &
                            laisen,                          &
                            dltaisenat,                      &
                            nsencour,                        &
                            dltamsen,                        &
                            dltaisen,                        &
                            fgellev,                         &
                            gelee,                           &
                            fstressgel,                      &
                            pdlaisen,                        &
                            R_stlevamf,                      &
                            dernier_n,                       &
                            durvieI,                         &
                            durvie,                          &
                            ndebsen,                         &
                            somsenreste,                     &
                            shumrel,                          &
                            humrel,                          &
                            swfac,                           &
                            turfac,                          &
                            senfac,                           & ! INOUT
                            mafeuiljaune,                     &
                            msneojaune,                       &
                            gslen,                            &
                            drylen,                            &
                            vswc,                             &
                            !! parameters      
                            TP_codeplante,                     &           
                            TP_stade0,                         &
                            TP_iplt0,                          &
                            TP_iwater,                         &
                            TP_codesimul,                      &
                            TP_codelaitr,&
                            TP_slamax,&
                            TP_slamin,&
                            TP_codeperenne,&
                            TP_codcueille,&
                            TP_codegdh,&
                            TP_codetemp,&
                            TP_coderetflo,&
                            TP_codeinnact,&
                            TP_codeh2oact,&
                            TP_stressdev,&
                            TP_innlai,&
                            TP_innsenes,&
                            TP_codebfroid,&
                            TP_codephot,&
                            TP_codedormance,&
                            TP_codefauche,&
                            TP_codetempfauche,&
                            TP_codlainet,&
                            TP_codeindetermin,&
                            TP_codeinitprec,&
                            TP_culturean,&
                            TP_jvc,&
                            TP_tfroid,&
                            TP_ampfroid,&
                            TP_jvcmini,&
                            TP_tgmin,&                                  
                            TP_stpltger,&
                            TP_profsem,&
                            TP_propjgermin,&
                            TP_tdmax,&
                            TP_nbjgerlim,&
                            TP_densitesem,&
                            TP_vigueurbat,&
                            TP_codepluiepoquet,&
                            TP_codehypo,&
                            TP_elmax,&
                            TP_belong,&
                            TP_celong,&
                            TP_nlevlim1,& 
                            TP_nlevlim2,&
                            TP_codrecolte,&
                            TP_variete,&
                            TP_codegermin,&
                            T_codeulaivernal,&
                            TP_swfacmin,&
                            TP_laiplantule,& 
                            TP_vlaimax,&
                            TP_stlevamf,&
                            TP_stamflax,&
                            TP_udlaimax,&     
                            TP_laicomp,&
                            TP_adens,&
                            TP_bdens,&
                            TP_tcxstop,&
                            TP_tcmax,&
                            TP_tcmin,&
                            TP_dlaimax,&
                            TP_dlaimin,&
                            TP_pentlaimax,&
                            TP_tigefeuil,&
                            TP_stlaxsen,&
                            TP_stsenlan,&
                            TP_stlevdrp,&
                            TP_stflodrp,&
                            TP_stdrpmat,&
                            TP_stdrpdes,&
                            TP_phyllotherme,&
                            TP_lai0,&
                            TP_tustressmin,&
                            ! STICS:: SENESCENCE
                            TP_nbfgellev,&
                            TP_maxgs,&
                            TP_ratiodurvieI,&
                            TP_durvieF,&
                            TP_ratiosen,&
                            TP_tdmin,&
                            ! STICS:: F_Humarec
                            TP_sensrsec,&
                            ! STICS:: GEL
                            TP_codgellev,&
                            TP_codgeljuv,&
                            TP_codgelveg,&
                            TP_tletale,&
                            TP_tdebgel,&
                            TP_tgellev10,&
                            TP_tgellev90,&
                            TP_tgeljuv10,&
                            TP_tgeljuv90,&
                            TP_tgelveg10,&
                            TP_tgelveg90,&
                            ! STICS:: Photoperiod
                            TP_sensiphot,&
                            TP_phosat,&
                            TP_phobase,&
                            histgrowth,&
                            hist_sencour, &
                            hist_latest, &
                            doyhistst, &
                            nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, boxbiom, boxbiomrem, boxdurage, boxsomsenbase, &
                            codesla)


       implicit none

       ! DECLARATION
       
       ! 0.1 input
       ! These variables should be transmitted from ORCHIDEE or initialized only once 

       
       integer(i_std), intent(IN)                           :: n    ! simulation date  // julian day 
       logical, intent(inout)                                  :: in_cycle
       logical, intent(inout)                                  :: f_crop_recycle
       logical, intent(inout)                                  :: f_sen_lai
       real(r_std),    intent(IN)                           :: t2m_daily  !> / Mean air temperature of the day // degree C
       real(r_std),    intent(IN)                           :: t2m_min_daily  !> / minimum air temperature of the day // degree C
       real(r_std),    intent(IN)                           :: gdh_daily  !> // daily gdh calculated according to halfhourly temperature // transmitted from stomate.f90 gdh_daily
       real(r_std),    intent(IN)                           :: phoi   !> // OUTPUT // Photoperiod // hours
       logical, intent(IN)                           :: onarretesomcourdrp 
       real(r_std),    intent(IN), dimension(3)             :: stempdiag_cm_daily !> / soil temperature at 1 cm resolution for the sowing depth and neighbour layers // Degree C 
       real(r_std),    intent(IN), dimension(3)             :: shumdiag_cm_day     !> /soil moisture at 1 cm resolution for the sowing depth and neighbour layers // unit m3 m-3 with values ranging 0-1
!!!!! local variable
       integer(i_std)            :: nlevobs    ! the following variables ended with obs are only used for forcing simulation.  
       integer(i_std)            :: namfobs    ! the initial value should be always 999
       integer(i_std)            :: nfloobs  
       integer(i_std)            :: nlanobs  
       integer(i_std)            :: nlaxobs  
       integer(i_std)            :: nmatobs  
       integer(i_std)            :: nrecobs  
       integer(i_std)            :: nsenobs  
       integer(i_std)            :: ndrpobs  
!!!!! local variable
       real(r_std),    intent(IN)                           :: dltams     ! biomass growth rate  // t ha-1 day-1
       real(r_std),    intent(IN)                           :: eop        ! potential evaportranspiration //  mm 
       real(r_std),    intent(IN)                           :: masec        ! aboveground biomass   // t ha-1
       real(r_std),    intent(IN)                           :: masecveg     ! vegetative dry matter // t ha-1
       real(r_std),    intent(IN)                           :: vswc    ! volumetric soil water content

       ! 0.1 input--parameters

       CHARACTER(len=3), INTENT(IN) :: TP_codeplante
       CHARACTER(len=3), INTENT(IN) :: TP_stade0
       INTEGER(i_std), INTENT(IN) :: TP_iplt0
       INTEGER(i_std), INTENT(IN) :: TP_iwater 
       CHARACTER(len=7), INTENT(IN) :: TP_codesimul
       INTEGER(i_std), INTENT(IN) :: TP_codelaitr
       REAL(r_std), INTENT(IN) :: TP_slamax
       REAL(r_std), INTENT(IN) :: TP_slamin
       INTEGER(i_std), INTENT(IN) :: TP_codeperenne    !! annual crop (1) or perennial crop (2)
       INTEGER(i_std), INTENT(IN) :: TP_codcueille     !! harvest option: cutting (1) or picking (2)
       INTEGER(i_std), INTENT(IN) :: TP_codegdh        !! hourly (1) or daily (2) calculation of development unit 
       INTEGER(i_std), INTENT(IN) :: TP_codetemp       !! 
       INTEGER(i_std), INTENT(IN) :: TP_coderetflo
       INTEGER(i_std), INTENT(IN) :: TP_codeinnact
       INTEGER(i_std), INTENT(IN) :: TP_codeh2oact
       REAL(r_std), INTENT(IN) :: TP_stressdev
       REAL(r_std), INTENT(IN) :: TP_innlai
       REAL(r_std), INTENT(IN) :: TP_innsenes
       INTEGER(i_std), INTENT(IN) :: TP_codebfroid
       INTEGER(i_std), INTENT(IN) :: TP_codephot
       INTEGER(i_std), INTENT(IN) :: TP_codedormance
       INTEGER(i_std), INTENT(IN) :: TP_codefauche     !! option of cut modes for forage crops: yes (1), no (2)
       INTEGER(i_std), INTENT(IN) :: TP_codetempfauche !! option of the reference temperature to compute cutting sum of temperatures : upvt (1), udevair (2)
       INTEGER(i_std), INTENT(IN) :: TP_codlainet
       INTEGER(i_std), INTENT(IN) :: TP_codeindetermin
       INTEGER(i_std), INTENT(IN) :: TP_codeinitprec
       INTEGER(i_std), INTENT(IN) :: TP_culturean 
       REAL(r_std), INTENT(IN) :: TP_jvc
       REAL(r_std), INTENT(IN) :: TP_tfroid
       REAL(r_std), INTENT(IN) :: TP_ampfroid 
       REAL(r_std), INTENT(IN) :: TP_jvcmini
       REAL(r_std), INTENT(IN) :: TP_tgmin
       REAL(r_std), INTENT(IN) :: TP_stpltger
       REAL(r_std), INTENT(IN) :: TP_profsem
       REAL(r_std), INTENT(IN) :: TP_propjgermin              !! minimal proportion of the duration TP_nbjgerlim when the temperature is higher than the temperature threshold TP_Tdmax 
       REAL(r_std), INTENT(IN) :: TP_tdmax
       INTEGER(i_std), INTENT(IN) :: TP_nbjgerlim              !! Threshold number of day after grain imbibition without germination lack // days
       REAL(r_std), INTENT(IN) :: TP_densitesem
       REAL(r_std), INTENT(IN) :: TP_vigueurbat                !!  indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 //
       INTEGER(i_std), INTENT(IN) :: TP_codepluiepoquet              !!  option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2
       INTEGER(i_std), INTENT(IN) :: TP_codehypo
       REAL(r_std), INTENT(IN) :: TP_elmax
       REAL(r_std), INTENT(IN) :: TP_belong
       REAL(r_std), INTENT(IN) :: TP_celong
       INTEGER(i_std), INTENT(IN) :: TP_nlevlim1             !! number of days after germination decreasing the emerged plants if emergence has not occur // days 
       INTEGER(i_std), INTENT(IN) :: TP_nlevlim2             !! number of days after germination after which the emerged plants are null // days 
       INTEGER(i_std), INTENT(IN) :: TP_codrecolte           !! harvest mode : all the plant (1) or just the fruits (2)
       INTEGER(i_std), INTENT(IN) :: TP_variete              !! variety number in the technical file // SD
       INTEGER(i_std), INTENT(IN) :: TP_codegermin           !! option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2)

       INTEGER(i_std), INTENT(IN) :: T_codeulaivernal
       REAL(r_std), INTENT(IN)    :: TP_swfacmin


       ! STICS:: LAI CALCULATION
       REAL(r_std), INTENT(IN) :: TP_laiplantule
       REAL(r_std), INTENT(IN) :: TP_vlaimax
       REAL(r_std), INTENT(IN) :: TP_stlevamf
       REAL(r_std), INTENT(IN) :: TP_stamflax
       REAL(r_std), INTENT(IN) :: TP_udlaimax
       REAL(r_std), INTENT(IN) :: TP_laicomp
       REAL(r_std), INTENT(IN) :: TP_adens                   !! Interplant competition parameter 
       REAL(r_std), INTENT(IN) :: TP_bdens
       REAL(r_std), INTENT(IN) :: TP_tcxstop
       REAL(r_std), INTENT(IN) :: TP_tcmax
       REAL(r_std), INTENT(IN) :: TP_tcmin
       REAL(r_std), INTENT(IN) :: TP_dlaimax
       REAL(r_std), INTENT(IN) :: TP_dlaimin
       REAL(r_std), INTENT(IN) :: TP_pentlaimax
       REAL(r_std), INTENT(IN) :: TP_tigefeuil               !! stem (structural part)/leaf proportion // SD  

       REAL(r_std), INTENT(IN) :: TP_stlaxsen
       REAL(r_std), INTENT(IN) :: TP_stsenlan
       REAL(r_std), INTENT(IN) :: TP_stlevdrp
       REAL(r_std), INTENT(IN) :: TP_stflodrp
       REAL(r_std), INTENT(IN) :: TP_stdrpmat
       REAL(r_std), INTENT(IN) :: TP_stdrpdes
       REAL(r_std), INTENT(IN) :: TP_phyllotherme  
       REAL(r_std), INTENT(IN) :: TP_lai0
       REAL(r_std), INTENT(IN) :: TP_tustressmin  
       
       ! STICS:: LAI SENESCENCE  
       INTEGER(i_std), INTENT(IN) :: TP_nbfgellev
       INTEGER(i_std), INTENT(IN) :: TP_maxgs
       REAL(r_std), INTENT(IN) :: TP_ratiodurvieI
       REAL(r_std), INTENT(IN) :: TP_durvieF
       REAL(r_std), INTENT(IN) :: TP_ratiosen
       REAL(r_std), INTENT(IN) :: TP_tdmin
       ! STICS:: F_Humarec
       
       REAL(r_std), INTENT(IN) :: TP_sensrsec
       
       ! STICS:: GEL
       INTEGER(i_std), INTENT(IN) :: TP_codgellev
       INTEGER(i_std), INTENT(IN) :: TP_codgeljuv
       INTEGER(i_std), INTENT(IN) :: TP_codgelveg
       REAL(r_std), INTENT(IN) :: TP_tletale
       REAL(r_std), INTENT(IN) :: TP_tdebgel
       REAL(r_std), INTENT(IN) :: TP_tgellev10
       REAL(r_std), INTENT(IN) :: TP_tgellev90
       REAL(r_std), INTENT(IN) :: TP_tgeljuv10
       REAL(r_std), INTENT(IN) :: TP_tgeljuv90
       REAL(r_std), INTENT(IN) :: TP_tgelveg10
       REAL(r_std), INTENT(IN) :: TP_tgelveg90
  
       ! STICS:: Photoperiod

       REAL(r_std), INTENT(IN) :: TP_sensiphot
       REAL(r_std), INTENT(IN) :: TP_phosat
       REAL(r_std), INTENT(IN) :: TP_phobase
       INTEGER(i_std), INTENT(IN) :: codesla
       REAL(r_std), INTENT(IN) :: humrel
       ! 0.2 inout
       ! these variables should ba all saved in Stomate.f90 for historical interaction

       ! these variables are for laidev specifically
       real, intent(INOUT)        :: nsendltams
       real, intent(INOUT)        :: nsendltai
       real, intent(INOUT)        :: nsenpfeuilverte
       real, intent(INOUT)        :: nsendurvie
       real, intent(INOUT)        :: nsenndurvie
       real, intent(INOUT)        :: densiteequiv
       integer, intent(INOUT)        :: nplt
       real, intent(INOUT)        :: tursla
       real, intent(INOUT)        :: sla
       real, intent(INOUT)        :: pfeuilverte
       real, intent(INOUT)        :: bsenlai
       
       ! variables are involved in DEVELOPMENT

       real,    intent(INOUT)        :: zrac
       integer, intent(INOUT)        :: nrec
       integer, intent(INOUT)        :: nlan
       real,    intent(INOUT)        :: tcult
       real,    intent(INOUT)        :: udevair
       real,    intent(INOUT)        :: udevcult
       integer,    intent(INOUT)        :: ndrp
       real,    intent(INOUT)        :: rfvi
       integer,    intent(INOUT)        :: nlev
       integer,    intent(INOUT)        :: nger
       logical,    intent(INOUT)        :: etatvernal
       real,    intent(INOUT)        :: caljvc
       real,    intent(INOUT)        :: rfpi
       real,    intent(INOUT)        :: upvt
       real,    intent(INOUT)        :: utp
       real,    intent(INOUT)        :: somcour
       real,    intent(INOUT)        :: somcourdrp
       real,    intent(INOUT)        :: somcourutp
       real,    intent(INOUT)        :: tdevelop
       real,    intent(INOUT)        :: somtemp
       real,    intent(INOUT)        :: somcourfauche
       real,    intent(INOUT)        :: stpltger
       real,    intent(INOUT)        :: R_stamflax
       real,    intent(INOUT)        :: R_stlaxsen
       real,    intent(INOUT)        :: R_stsenlan
       real,    intent(INOUT)        :: stlevflo
       integer,    intent(INOUT)        :: nflo
       real,    intent(INOUT)        :: R_stlevdrp
       real,    intent(INOUT)        :: R_stflodrp
       real,    intent(INOUT)        :: R_stdrpmat
       integer,    intent(INOUT)        :: nmat
       integer,    intent(INOUT)        :: nlax
       integer,    intent(INOUT)        :: nrecbutoir
       real,    intent(INOUT)        :: group
       integer,    intent(INOUT)        :: ndebdes
       real,    intent(INOUT)        :: R_stdrpdes
       real,    intent(INOUT)        :: densite
       real,    intent(INOUT)        :: densitelev
       real,    intent(INOUT)        :: coeflev
       real,    intent(INOUT)        :: densiteger
       real,    intent(INOUT)        :: somelong
       real,    intent(INOUT)        :: somger
       logical,    intent(INOUT)        :: humectation
       integer,    intent(INOUT)        :: nbjhumec
       real,    intent(INOUT)        :: somtemphumec
       real,    intent(INOUT)        :: stpltlev
       integer,    intent(INOUT)        :: namf
       real,    intent(INOUT)        :: stmatrec
 
       ! these variables are involved in Lai_calculation
        
       real,    intent(INOUT)        :: tustress
       real,    intent(INOUT)        :: slai
       real,    intent(INOUT)        :: somfeuille
       real,    intent(INOUT)        :: pdlai
       integer,    intent(INOUT)        :: nbfeuille
       real,    intent(INOUT)        :: reajust
       real,    intent(INOUT)        :: ulai
       real,    intent(INOUT)        :: pdulai
       real,    intent(INOUT)        :: efdensite
       real,    intent(INOUT)        :: tempeff
       integer,    intent(INOUT)        :: nstopfeuille
       real,    intent(INOUT)        :: deltai
       real,    intent(INOUT)        :: vmax
       integer,    intent(INOUT)        :: nsen
       real,    intent(INOUT)        :: laisen
       real,    intent(INOUT)        :: dltaisenat
       ! Xuhui 140321
       INTEGER(i_std), intent(INOUT) :: doyhistst
       INTEGER(i_std), intent(INOUT) :: hist_sencour
       INTEGER(i_std), intent(INOUT) :: hist_latest
       REAL(r_std), DIMENSION(300,5), intent(INOUT) :: histgrowth
       !end histgrowth

       ! these variables are involved in the LAIsenescence

       integer,    intent(INOUT)      :: nsencour
       real,    intent(INOUT)      :: dltamsen
       real,    intent(INOUT)      :: dltaisen
       real,    intent(INOUT)      :: fgellev
       logical,    intent(INOUT)      :: gelee
       real,    intent(INOUT)      :: fstressgel
       real,    intent(INOUT)      :: pdlaisen
       real,    intent(INOUT)      :: R_stlevamf
       integer,    intent(INOUT)      :: dernier_n
       real,    intent(INOUT)      :: durvieI
       real,    intent(INOUT)      :: durvie
       integer,    intent(INOUT)      :: ndebsen
       real,    intent(INOUT)      :: somsenreste

       ! compartment senescence module
!boxulai, boxndays, boxlai, boxlairem,boxtdev, boxbiom, boxbiomrem, boxdurage, boxsomsenbase
       integer(i_std), intent(IN)                  :: nbox
       real(r_std), dimension(nbox), intent(INOUT) :: boxulai
       integer(i_std), dimension(nbox), intent(INOUT) :: boxndays
       real(r_std), dimension(nbox), intent(INOUT) :: boxlai
       real(r_std), dimension(nbox), intent(INOUT) :: boxlairem
       real(r_std), dimension(nbox), intent(INOUT) :: boxtdev
       real(r_std), dimension(nbox), intent(INOUT) :: boxbiom
       real(r_std), dimension(nbox), intent(INOUT) :: boxbiomrem
       real(r_std), dimension(nbox), intent(INOUT) :: boxdurage
       real(r_std), dimension(nbox), intent(INOUT) :: boxsomsenbase
       

       ! these variables are involved in STRESS calculation
       
       real,    intent(INOUT)      :: shumrel
       real,    intent(INOUT)      :: swfac
       real,    intent(INOUT)      :: turfac
       real,    intent(INOUT)      :: senfac

       ! these variables are involved in CARBON ALLOCATION PROCESSES
       real,    intent(INOUT)      :: mafeuiljaune      !  Dry matter of yellow leaves // t.ha-1
       real,    intent(INOUT)      :: msneojaune        ! Newly-formed senescent dry matter  // t.ha-1
       integer,    intent(INOUT)      :: gslen
       integer,    intent(INOUT)      :: drylen

       ! assignment 
       nlevobs = 999
       namfobs = 999
       nfloobs = 999
       nlanobs = 999
       nlaxobs = 999
       nmatobs = 999
       nrecobs = 999
       nsenobs = 999
       ndrpobs = 999

       P_codeplante = TP_codeplante
       P_stade0 = TP_stade0
       P_iplt0 = TP_iplt0
       P_iwater = TP_iwater
       P_codesimul = TP_codesimul
       P_codelaitr = TP_codelaitr
       P_slamax = TP_slamax
       P_slamin = TP_slamin
       P_codeperenne = TP_codeperenne
       P_codcueille = TP_codcueille
       P_codegdh = TP_codegdh
       P_codetemp = TP_codetemp
       P_coderetflo = TP_coderetflo
       P_codeinnact = TP_codeinnact
       P_codeh2oact = TP_codeh2oact
       P_stressdev = TP_stressdev
       P_innlai = TP_innlai
       P_innsenes = TP_innsenes
       P_codebfroid = TP_codebfroid
       P_codephot = TP_codephot
       P_codedormance = TP_codedormance
       P_codefauche = TP_codefauche
       P_codetempfauche = TP_codetempfauche
       P_codlainet = TP_codlainet
       P_codeindetermin = TP_codeindetermin
       P_codeinitprec = TP_codeinitprec
       P_culturean = TP_culturean
       P_jvc = TP_jvc
       P_tfroid = TP_tfroid
       P_ampfroid = TP_ampfroid
       P_jvcmini = TP_jvcmini
       P_tgmin = TP_tgmin
       P_stpltger = TP_stpltger
       P_profsem = TP_profsem
       P_propjgermin = TP_propjgermin
       P_tdmax = TP_tdmax
       P_nbjgerlim = TP_nbjgerlim
       P_densitesem = TP_densitesem
       P_vigueurbat = TP_vigueurbat
       P_codepluiepoquet = TP_codepluiepoquet
       P_codehypo = TP_codehypo
       P_elmax = TP_elmax
       P_belong = TP_belong
       P_celong = TP_celong
       P_nlevlim1 = TP_nlevlim1
       P_nlevlim2 = TP_nlevlim2
       P_codrecolte = TP_codrecolte
       P_variete = TP_variete
       P_codegermin = TP_codegermin
       
       codeulaivernal = T_codeulaivernal
       P_swfacmin = TP_swfacmin


       P_laiplantule = TP_laiplantule
       P_vlaimax = TP_vlaimax
       P_stlevamf = TP_stlevamf
       P_stamflax = TP_stamflax
       P_udlaimax = TP_udlaimax
       P_laicomp = TP_laicomp
       P_adens = TP_adens
       P_bdens = TP_bdens
       P_tcxstop = TP_tcxstop
       P_tcmax = TP_tcmax
       P_tcmin = TP_tcmin
       P_dlaimax = TP_dlaimax
       P_dlaimin = TP_dlaimin
       P_pentlaimax = TP_pentlaimax
       P_tigefeuil = TP_tigefeuil
 
       P_stlaxsen = TP_stlaxsen
       P_stsenlan = TP_stsenlan
       P_stlevdrp = TP_stlevdrp
       P_stflodrp = TP_stflodrp
       P_stdrpmat = TP_stdrpmat
       P_stdrpdes = TP_stdrpdes
       P_phyllotherme = TP_phyllotherme
       P_lai0 = TP_lai0
       P_tustressmin = TP_tustressmin
       ! STICS:: SENESCENCE
       P_nbfgellev = TP_nbfgellev
       P_maxgs = TP_maxgs
       P_ratiodurvieI = TP_ratiodurvieI
       P_durvieF = TP_durvieF
       P_ratiosen = TP_ratiosen
       P_tdmin = TP_tdmin
       ! STICS:: F_hemarec
       P_sensrsec = TP_sensrsec
       ! STICS::GEL
       P_codgellev = TP_codgellev
       P_codgeljuv = TP_codgeljuv
       P_codgelveg = TP_codgelveg
       P_tletale = TP_tletale
       P_tdebgel = TP_tdebgel
       P_tgellev10 = TP_tgellev10
       P_tgellev90 = TP_tgellev90
       P_tgeljuv10 = TP_tgeljuv10
       P_tgeljuv90 = TP_tgeljuv90
       P_tgelveg10 = TP_tgelveg10
       P_tgelveg90 = TP_tgelveg90
       ! STICS:: Photoperiod    
       P_sensiphot = TP_sensiphot
       P_phosat = TP_phosat
       P_phobase = TP_phobase

      P_codesla = codesla
        
!       if (.NOT. in_cycle) then
!           doyhistst = 0
!           hist_sencour = 0
!           hist_latest = 0
!           histgrowth(:,:) = 0.
!       endif


       call laidev(n,                               &  ! IN
                   in_cycle,                        &  ! inout
                   f_crop_recycle,                  &  ! INOUT
                   f_sen_lai,                        &  ! inout
                   t2m_daily,                       &  ! IN  
                   t2m_min_daily,                       &  ! IN  
                   gdh_daily,                       &  ! IN
                   phoi,                            &  ! IN
                   onarretesomcourdrp,              &  ! IN                          
                   stempdiag_cm_daily,              &  ! IN
                   shumdiag_cm_day,                 &  ! IN
                   nlevobs,                         &  ! IN
                   namfobs,                         &  ! IN
                   nfloobs,                         &  ! IN
                   nlanobs,                         &  ! IN
                   nlaxobs,                         &  ! IN
                   nmatobs,                         &  ! IN
                   nrecobs,                         &  ! IN
                   nsenobs,                         &  ! IN
                   ndrpobs,                         &  ! IN
                   dltams,                          &  ! IN
                   eop,                             &  ! IN
                   masec,                           &  ! IN
                   masecveg,                        &  ! IN
                   nsendltams,                      &  ! INOUT
                   nsendltai,                       &
                   nsenpfeuilverte,                 &
                   nsendurvie,                      &
                   nsenndurvie,                     &
                   densiteequiv,                    &
                   nplt,                            &
                   tursla,                          &
                   sla,                             &
                   pfeuilverte,                     &
                   bsenlai,                         &
                   zrac,                            &
                   nrec,                            &
                   nlan,                            &
                   tcult,                           &
                   udevair,                         &
                   udevcult,                        &
                   ndrp,                            &
                   rfvi,                            &
                   nlev,                            &
                   nger,                            &
                   etatvernal,                      &
                   caljvc,                          &
                   rfpi,                            &
                   upvt,                            &
                   utp,                             &
                   somcour,                         &
                   somcourdrp,                      &
                   somcourutp,                      &
                   tdevelop,                        &
                   somtemp,                         &
                   somcourfauche,                   &
                   stpltger,                        &
                   R_stamflax,                      &
                   R_stlaxsen,                      &
                   R_stsenlan,                      &
                   stlevflo,                        &
                   nflo,                            &
                   R_stlevdrp,                      &
                   R_stflodrp,                      &
                   R_stdrpmat,                      &
                   nmat,                            &
                   nlax,                            &
                   nrecbutoir,                      &
                   group,                           &
                   ndebdes,                         &
                   R_stdrpdes,                      &
                   densite,                         &
                   densitelev,                      &
                   coeflev,                         &
                   densiteger,                      &
                   somelong,                        &
                   somger,                          &
                   humectation,                     &
                   nbjhumec,                        &
                   somtemphumec,                    &
                   stpltlev,                        &
                   namf,                            &
                   stmatrec,                        &
                   tustress,                        &
                   slai,                             &
                   somfeuille,                      &
                   pdlai,                           &
                   nbfeuille,                       &
                   reajust,                         &
                   ulai,                            &
                   pdulai,                          &
                   efdensite,                       &
                   tempeff,                         &
                   nstopfeuille,                    &
                   deltai,                          &
                   vmax,                            &
                   nsen,                            &
                   laisen,                          &
                   dltaisenat,                      &
                   nsencour,                        &
                   dltamsen,                        &
                   dltaisen,                        &
                   fgellev,                         &
                   gelee,                           &
                   fstressgel,                      &
                   pdlaisen,                        &
                   R_stlevamf,                      &
                   dernier_n,                       &
                   durvieI,                         &
                   durvie,                          &
                   ndebsen,                         &
                   somsenreste,                     &
                   shumrel,                          &
                   humrel,                          &
                   swfac,                           &
                   turfac,                          &
                   senfac,                          &
                   mafeuiljaune,                    &
                   msneojaune,                      &
                   gslen,                           &
                   drylen,                           &
                   vswc, &
                   histgrowth, &
                   hist_sencour, &
                   hist_latest, &
                   doyhistst, &
                   nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, &
                   boxbiom, boxbiomrem,boxdurage, boxsomsenbase)

    
    end subroutine driver_stics

end module Stics
 
 
