










!> Routine develop2
!>
!! Description :
!< (routine qui prend des arguments simples)

! DR 23/07/2012 latitude est pas itilisé car on passe la photoperiode qui est suffisante
!subroutine develop2(phoi,tmax,tmin,tmin_demain,trr,P_codeh2oact,P_codeinitprec,P_codeinnact,codeulaivernal,P_psihucc, &
!                    P_psihumin, P_codcueille,P_codefauche,P_densitesem,P_profsem,P_variete,P_ampfroid,P_belong,P_celong,         &
!                    P_codebfroid,P_codedormance,P_codegdh, P_codegdhdeb,P_codegermin,P_codehypo,P_codeperenne,P_codephot,        &
!                    P_codeplante,P_coderetflo,P_codetemp,coeflev,cu_min,cu_veille, densite,densiteger,densitelev,P_elmax,        &
!                    innlai,P_julvernal,P_jvc,P_jvcmini,P_nbjgerlim,ndrpobs,P_nlevlim1,P_nlevlim2,nlevobs,nplt,onarretesomcourdrp,&
!                    P_phobase,P_phosat,P_potgermi,P_propjgermin,P_q10,P_sensiphot,P_sensrsec,somelong,somger,P_stdordebour,      &
!                    P_stpltger,P_stressdev,P_tcxstop,P_tdmax,P_tdmaxdeb,P_tdmin,P_tdmindeb,P_tfroid,P_tgmin,turfac,P_vigueurbat, &
!                    P_codefente,P_mulchbat,P_pluiebat,P_culturean,nbCouches,dacouche,hucc,humin,hur,jjul,n,nbjanrec,nbjsemis,    &
!                    numcult,tairveille,tcult,tsol,xmlch1,P_codepluiepoquet,P_codetempfauche,humectation,nbjhumec,pluiesemis,     &
!                    somtemphumec,P_codeindetermin,P_codelaitr,P_codlainet,P_dureefruit,namfobs,P_nbcueille,nfloobs,     &
!                    nlanobs, nlaxobs,nmatobs,nrecobs,nsenobs,P_stdrpnou,upobs,P_codemontaison,sioncoupe,                         &
!                    caljvc,cu,demande,etatvernal,hauteur,mafrais,mafraisfeuille,mafraisrec,mafraisres,mafraistige,masec,namf,    &
!                    ndebdorm,ndrp,nfindorm,nflo,nger,nlan,nlev,nrec,nrecbutoir,pdsfruitfrais,rfpi,rfvi,somcour,somcourdrp,       &
!                    somcourfauche,somcourutp,somtemp,stpltlev,tdevelop,udevair,udevcult,upvt,utp,zrac,maxwth,group,ndebdes,      &
!                    nfruit,nlax,nmat,nnou,nsen,R_stamflax,R_stdrpdes,R_stdrpmat,stdrpsen,R_stflodrp,R_stlaxsen,R_stlevamf,       &
!                    R_stlevdrp,stlevflo,stmatrec,R_stsenlan,upvtutil, P_codrecolte,h2orec,P_sucrerec,P_CNgrainrec,P_huilerec,    &
!                    sucre,huile,teaugrain,P_h2ofrvert,P_codeaumin, P_h2ograinmin,P_h2ograinmax,P_deshydbase,   &
!                    CNgrain,P_cadencerec, jdepuisrec,pdsfruit,nbrecolte,nrecint,rdtint,teauint,nbfrint,onestan2,somcourmont,     &
!                    nmontaison,stpltger)

! Here, we modify the develop2 processes. 

subroutine develop2(n, in_cycle, nplt, tair,  gdh_daily, turfac,  phoi, onarretesomcourdrp,  stempdiag_cm_daily, shumdiag_cm_day, lai,            &  !> INPUTS
                    nlevobs, namfobs, nfloobs, nlanobs, nlaxobs, nmatobs, nrecobs, nsenobs, ndrpobs,  nrecbutoir,                                            &  !> INPUTS
                    masec, namf,  ndrp, nflo, nger, nlan, nlev, nrec, etatvernal, caljvc,                                                                 &  !> INOUT
                    upvt, utp, somcour, somcourdrp, somcourfauche, somcourutp, somtemp, zrac,                                                      &  !> INOUT
                    coeflev, somelong, somger, humectation, nbjhumec, somtemphumec, densite, densitelev, nlax, nmat, nsen, stlevflo, ndebdes,      &  !> INOUT 
                    R_stlevamf, R_stamflax, R_stsenlan, R_stlaxsen, R_stlevdrp, R_stflodrp, R_stdrpmat, R_stdrpdes, densiteger,                    &  !> INOUT
                    udevair, udevcult,                                                                                               &  !> INOUT                
                    rfvi, rfpi, tdevelop, stpltger, upvtutil, stmatrec, group, tcult, stpltlev,                                      &   !>INOUT
                    f_crop_recycle, gslen, drylen)   


USE Divers_develop
USE Besoins_en_froid
USE Stics
USE constantes
!USE Messages

implicit none 

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! declaration of variables

! 0.1 input 
 
   
  integer, intent(IN)                           :: n
  logical, intent(inout)                           :: in_cycle
  integer, intent(IN)                           :: nplt  
  real,    intent(IN)                           :: tair !> / Mean air temperature of the day // degree C
  real,    intent(IN)                           :: gdh_daily  !> // daily gdh calculated according to halfhourly temperature // transmitted from stomate.f90 gdh_daily
  real,    intent(IN)                           :: turfac
  real,    intent(IN)                           :: phoi   !> // OUTPUT // Photoperiod // hours
  logical, intent(IN)                           :: onarretesomcourdrp 
  real,    intent(IN), dimension(3)             :: stempdiag_cm_daily !> / soil temperature at 1 cm resolution for the sowing depth and neighbour layers // Degree C 
  real,    intent(IN), dimension(3)             :: shumdiag_cm_day     !> /soil moisture at 1 cm resolution for the sowing depth and neighbour layers // unit m3 m-3 with values ranging 0-1
  real,    intent(IN)                           :: lai   !> leaf area index
  integer, intent(IN)                           :: nlevobs 
  integer, intent(IN)                           :: namfobs  
  integer, intent(IN)                           :: nfloobs  
  integer, intent(IN)                           :: nlanobs  
  integer, intent(IN)                           :: nlaxobs  
  integer, intent(IN)                           :: nmatobs  
  integer, intent(IN)                           :: nrecobs  
  integer, intent(IN)                           :: nsenobs  
  integer, intent(IN)                           :: ndrpobs  
  integer, intent(IN)                        :: nrecbutoir    !// the harvest date imposed, actually, if we simulate the harvest automatically, we do not use it. So, we initialize it as 999.  

! 0.2 inout

  real,    intent(INOUT)                        :: masec 

  integer, intent(INOUT)                        :: namf  
  integer, intent(INOUT)                        :: ndrp  
  integer, intent(INOUT)                        :: nflo  
  integer, intent(INOUT)                        :: nger  
  integer, intent(INOUT)                        :: nlan  
  integer, intent(INOUT)                        :: nlev  
  integer, intent(INOUT)                        :: nrec  
  logical, intent(INOUT)                        :: etatvernal
  real,    intent(INOUT)                        :: caljvc 
  real,    intent(INOUT)                        :: upvt   !> // OUTPUT // Daily development unit  // degree.days
  real,    intent(INOUT)                        :: utp 
  real,    intent(INOUT)                        :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
  real,    intent(INOUT)                        :: somcourdrp 
  real,    intent(INOUT)                        :: somcourfauche  
  real,    intent(INOUT)                        :: somcourutp  
  real,    intent(INOUT)                        :: somtemp   !> // OUTPUT // Sum of temperatures // degree C.j
  real,    intent(INOUT)                        :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,    intent(INOUT)                        :: coeflev 
  real,    intent(INOUT)                        :: somelong 
  real,    intent(INOUT)                        :: somger  
  logical, intent(INOUT)                        :: humectation  
  integer, intent(INOUT)                        :: nbjhumec  
  real,    intent(INOUT)                        :: somtemphumec  
  real,    intent(INOUT)                        :: densite   !>  actual sowing density // plants m-2
  real,    intent(INOUT)                        :: densitelev  
  integer, intent(INOUT)                        :: nlax  
  integer, intent(INOUT)                        :: nmat  
  integer, intent(INOUT)                        :: nsen  
  real,    intent(INOUT)                        :: stlevflo  
  integer, intent(INOUT)                        :: ndebdes 
  real,    intent(INOUT)                        :: R_stlevamf
  real,    intent(INOUT)                        :: R_stamflax
  real,    intent(INOUT)                        :: R_stsenlan
  real,    intent(INOUT)                        :: R_stlaxsen
  real,    intent(INOUT)                        :: R_stlevdrp
  real,    intent(INOUT)                        :: R_stflodrp
  real,    intent(INOUT)                        :: R_stdrpmat
  real,    intent(INOUT)                        :: R_stdrpdes
  real,    intent(INOUT)                        :: densiteger  
  real,    intent(INOUT)                        :: udevair   !> // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  real,    intent(INOUT)                        :: udevcult   !> // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days

  logical, intent(inout)                           :: f_crop_recycle   ! > in levee subroutine, we judge whether or not the crop emerges, if not, the in_cycle shoule be false, and crop recycle
  integer, intent(INOUT)                        :: gslen 
  integer, intent(INOUT)                        :: drylen 




! 0.3 out

  ! DEVELOP

  real,    intent(OUT)                          :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  real,    intent(OUT)                          :: rfpi   !> // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
  real,    intent(OUT)                          :: tdevelop 
  real,    intent(OUT)                          :: stpltger
  real,    intent(OUT)                          :: upvtutil 
  real,    intent(OUT)                          :: stmatrec
  real,    intent(OUT)                          :: group 
  real,    intent(OUT)                          :: tcult   !> / Mean crop temperature of the day // degree C
  real,    intent(OUT)                          :: stpltlev

! 0.4 local Variables
  real :: tdev  




!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!  1. CONDITIONAL STATEMENT AND INITIALIZATION

!: Pour les annuelles => pas de développement avant le semis
!- DR - 03/05/06
!- dans le cas de repousse du semis par decisionsemis on ne commence pas les cumul d'unite

  
  if (P_codeperenne == 1 .and. ((n /= nplt) .and. (in_cycle == .FALSE.))) then
     return
  else
 
     in_cycle = .TRUE. ! enter the cycle
     
     !! FIRST STEP: WE ACCOUNT THE GROWING LENGTH
     
     gslen = gslen + 1 


!:    On remet à zéro les masses sèches, masses fraiches, masses des fruits, la hauteur de la plante, les variables de fixation (et le zrac ?)
!:    Marie et Domi - 10/10/03 - bug signalé par Emmanuelle Sauboua
!-    N'était jamais testé dans 'recolte'

!    comments:
!    reinitialization of some variables, which are related to harvest 
!    P_codcueille: way how to harvest
!    1.  We do not use the process of stics to address biomass production;
!    2.  we also do not consider the root dynamics at this moment


     if (P_codeperenne == 1 .and. P_codcueille == 1) then     ! P_codcueille: way how to harvest, 1, whole plant, 2, only fruits
       if (n == nrec+1) then
          masec             = 0.
          zrac              = 0.
          !mafrais(:)        = 0.
          !pdsfruitfrais(:)  = 0.
          !hauteur(:)        = 0. ! DR - 22/10/03
          !demande(:)        = 0. ! PB - 03/05/2004 - remise à zéro des variables de fixation
          !!: DR - 13/01/06 - remise à zero sinon mafrais ne revient pas nul
          !mafraisfeuille(:) = 0.
          !mafraistige(:)    = 0.
          !mafraisres(:)     = 0.
          !mafraisrec(:)     = 0.
       endif
!:    Pour les cultures annuelles (P_codeperenne=1) moissonnées (P_codcueille=1) : pas de développement après la récolte.
       if (n > nrec .and. nlan > 0 .and. nrec > 0) return ! exit this subroutine
     endif

!:    PB - 11/03/2005 - On remet pdsfruitfrais à zéro après la récolte.
!:    PB - 07/08/2009 - Attention, qd n=1 et nrec=0, cette condition peut être vraie alors qu'on est pas après la récolte.
!                       TODO: Ajouter un test sur nrec est nul ?
     !if (P_codeperenne == 2 .and. P_codcueille == 2 .and. n == (nrec+1) .and. P_codeinitprec == 2) then
     !  pdsfruitfrais(:) = 0.
     !endif

!    TODO : GERER LE CAS DES CULTURES ANNUELLES  A RECOLTES MULTIPLES



!2   . CALCULATION OF  EFFECT TEMPERATURE (calcul udevair/udevcult)
!    -------------------------------------------

     !: daily temperature
     !- NB - 07/07/06: Application du seuil thermique négatif P_TCXSTOP au développement
     
     tcult = tair  ! here, we do not use the crop temperature, later if we use the tcult, this sentence shoul dbe commented.


     if (P_codegdh == 1) then  !
       udevair = calcul_UDev(tair)
       udevcult = calcul_UDev(tcult) 
     endif

!     print *, 'udevair and udevcult in develop is', udevair, udevcult
     
     !: hourly temperature
     if (P_codegdh == 2) then

       !: Pour l'instant que températures air autorisées
       if (P_codetemp == 2) then
         !call EnvoyerMsgHistorique(49)
         stop
       endif

       !: 1) Reconstitution des températures horaires.
       !-    Pour l'instant on n'autorise uniquement le cas par températures air.
       !thor = calcul_TemperaturesHoraires(tmin,tmin_demain,tmax)

       !: 2) calculation gdh 
       !     using hourly temperature, only available for air temperature.
       !     see stomate.f90
       
       udevair = gdh_daily  ! the GDH calculated according to halfhourly temperature is implemented in stomate.f90
       !udevair = calcul_GDH(thor,P_tdmin,P_tdmax)
       udevcult = udevair
      
     endif



     ! NB - le 01/09/06:
     ! effet retard du stress hydrique en phase végétative appliqué directement
     ! sur les udevair (ou udevcult) de façon à ce qu'il agisse sur le développement
     ! P_phénologique et également sur la durée de vie.
     ! modif pour Sophie pour permettre action de P_stressdev pendant tout le cycle et
     ! en n'utilisant qu'un seul des deux stress
     ! DR et ML et La Soso - 15/08/07 - y'avait un bug introduit par sophie et on sait
     ! pas qui. On passait la tout le temps meme quand P_coderetflo=2
     !       if (P_coderetflo == 1.and.P_codeinnact == 1 .or. P_codeh2oact == 1)

     ! considering the water and/or nitrogen limiting effects--xcw
     ! at this moment, we do not consider the Nitrogen processes, nitrogen limitation factor innlai is seted as parameter with a value of 1
     ! the calculation of turfac is done in stress subroutine

     if (P_coderetflo == 1 .and. (P_codeinnact == 1 .or. P_codeh2oact == 1)) then
       if (P_codeplante == 'qui' .or. ndrp == 0) then
         if (P_codetemp == 2) then
           udevcult = udevcult * (P_stressdev * min(turfac,P_innlai) + 1 - P_stressdev)  
         else
           udevair = udevair * (P_stressdev * min(turfac,P_innlai) + 1 - P_stressdev)
         endif
       endif
     endif



!    3. EFFECTS OF VERNALIZATION (calcul de rfvi)
!    ---------------------------------
     if (P_codebfroid == 1) rfvi = 1.0

     !: Calcul de l'effet vernalisation a partir de la germination
     !- ou en cours de culture après la date P_julvernal
     !- calculs données intermédiaires
     if (P_codebfroid == 2) then
       !: set the optional temperature for tdev
       if (P_codetemp == 2) then
         tdev = tcult
       else
         tdev = tair
       endif
      
     ! calculation of rfvi, actually, we only use the subroutine vernalization to calculate the rfvi
     ! Considering vernalization when germination

       call Stics_Develop_bfroid2(nger,                &  
                                  tdev,                &  
                                  rfvi,                &  
                                  etatvernal,          &  
                                  caljvc)                 

      ! call Stics_Develop_bfroid2(jjul,n,P_tfroid,P_ampfroid,P_julvernal,P_jvc,P_jvcmini,P_codeperenne,  &
      !                            nger,namf,numcult,nbjsemis,tdev,P_codemontaison,P_culturean,   &
      !                            P_codeinitprec,nbjanrec,                                     &
      !                            nrecbutoir,rfvi,maxwth,etatvernal,caljvc,onestan2)
     endif
!     print *, 'in develop the rfvi is,', rfvi

 
 !    COMMENTS: P_codebfroid == 3 is only effective for perennial crops;
 !    at this moment, we do not address such crops-----xcwu

 !    if (P_codebfroid == 3) then
 !      call Stics_Develop_bfroid3(P_codedormance,cu_min,cu_veille,n,P_jvc,P_q10,tmin,tmax,thor, &
 !                                 etatvernal,cu,rfvi,ndebdorm,nfindorm,nlev)
 !    endif


!    4. EFFECTS OF PHOTOPERIOD (calcul of rfpi)
!    -----------------------------
     if (P_codephot == 1) then
       !numdate = MOD(jjul,nbjsemis+1)
       !numdate = jjul
       !if (jjul > nbjsemis) numdate = jjul - nbjsemis

       !call photpd(P_latitude,numdate,daylen,phoi)

       !if (ndrp /= 0 .or. n > ndrpobs) then
       if (ndrp /= 0) then
         rfpi = 1.0
       else if (P_codebfroid /= 3 .and. (nlev == 0 .or. n < nlev)) then ! pour les ligneux, photopériode active à partir de la fin de dormance
         rfpi = 1.0
       !else if (P_codebfroid == 3 .and. nfindorm == 0) then
       !  rfpi = 1.0
       else
         ! TODO: on remplace l'appel à photpd par ses résultats. photpd n'est appelé qu'une fois par pas de temps
         rfpi = cRFPI(phoi)
       endif
     else
       rfpi = 1.0
     endif
     
!     print *, 'in develop the rfpi is,', rfpi

!    5. UNIT OF DEVELOPPEMENT (calcul de upvt)
!    ---------------------------------------------------
     if (P_codetemp == 2) then
       ! DR 17/08/06 si on est sur une perenne et qu'une dormance a deja ete faite
       ! on ne fait plus jouer la vernalisation sur le calcul des stades
       ! implicitement n'est utilisé que si on est en enchainement
       
       
       ! the calculation of upvt is done
       if (P_codedormance == 3 .and. nlev > 0) then
         upvt = udevcult * rfpi
       else
         upvt = udevcult * rfpi * rfvi
       endif

       ! option codelaisansvernal la vernalisation ne joue pas sur la ulai
       if (codeulaivernal == 0) utp = udevcult * rfpi
     else
       ! DR 17/08/06 si on est sur une perenne et qu'une dormance a deja ete faite
       ! on ne fait plus jouer la vernalisation sur le calcul des stades
       ! implicitement n'est utilisé que si on est en enchainement
       ! NB le 21/08/07 bug
       if (P_codebfroid == 3 .and. P_codedormance == 3 .and. nlev > 0) then
         upvt = udevair * rfpi
       else
         upvt = udevair * rfpi * rfvi
       endif

       if (codeulaivernal == 0) utp = udevair * rfpi
     endif


     !: calculation of the cumulated units of development between two stages
     somcour = somcour + upvt

!     print *, 'somcour and upvt in development is', somcour, upvt

     !: somcourdrp = cumul d'unité entre deux stades reproducteurs
     !- à partir de la levee
     if (nlev > 0) somcourdrp = somcourdrp + upvt

     ! dr 13/01/06: dans le cas de la prairie on ne cumule plus d'upvt si on coupe apres amf
     ! dr 17/11/05: si on coupe apres amf et avant drp on ne pourra plus faire d'epi
     !              donc on arrete le developpement des stades reproducteurs
     if (P_codefauche == 1 .and. onarretesomcourdrp) then   ! P_codefauche cut mode for forage crop, cut (1) or not (2)
       if (namf /= 0 .and. (ndrp == 0 .or. nflo == 0)) then
         somcourdrp = somcourdrp - upvt
       endif
     endif

     if (codeulaivernal == 0) then
       somcourutp = somcourutp + utp
     endif


     !: calcul d'une somme de températures même
     !- pour les plantes vernalo-photo-sensibles
     !- pour les calculs de sénescence et de nombre  de feuilles
     !-
     !- NB le 08/05 on remplace la somme des températures pour
     !- la sénescence par un P_Q10 pour que le vieillissement soit effectif meme
     !- en conditions froides
     if (P_codetemp == 2) then
       tdevelop = 2.0 ** (udevcult / 10.)
     else
       tdevelop = 2.0 ** (udevair / 10.)
     endif

     somtemp = somtemp + tdevelop
     if (P_codetempfauche == 1) then
       somcourfauche = somcourfauche + upvt
     else
       somcourfauche = somcourfauche + udevair
     endif

!     print *, 'somcour and tdevelop, somtemp in develop is:', somcour, tdevelop, somtemp


!    6. CALCULATION OF THE STATUS
!    -----------------
!    

!    This part is for perennial grassland----xcwu

!!    DR et ML et SYL 16/06/09
!!    calcul de la date de montaison et du jour d'entree en
!!    vernalisation de la prairie perenne
!     if (P_codemontaison == 1)then
!!    ####
!!    entrée en vernalisation des fourrages (pérenne)
!!    NB le 07/03/08
!       if (P_codebfroid == 2 .and. P_codeperenne == 2) then
!         somcourmont = somcourmont + upvt
!       ! PB - 03/08/2010 - je remplace jul par jjul qui correspond à n+P_iwater-1
!         if (jjul+((onestan2-1)*nbjsemis) == P_julvernal) then
!           somcourmont = 0.0
!         endif
!!    ** stade début montaison : après vernalisation
!!    unique cycle reproducteur de l'année
!         if (somcourmont >= R_stlevamf .and. jjul+((onestan2-1)*nbjsemis) > P_julvernal)then
!           nmontaison = n
!           namf = n
!!    DR et ML et SYL 16/06/09 - on supprime nvernal qui ne sert à rien
!!   --         nvernal=0
!           somcourmont=0.0
!           onestan2 = 1
!         endif
!
!!    si la coupe intervient après le stade montaison alors on remet
!!    le stade à 0
!         if (sioncoupe) nmontaison=0
!       endif
!!    ####
!     endif
!!    DR et ML et SYL 16/06/09 FIN

     !: 6.1 STATUS OF GERMINATION AND EMERGENCE
     !: levee
     !: in this subroutine, we know when the crop germinates and emerges. We allowed the winter wheat emerges in the following spring within a reasonable period.
     !: However, if the crop can not emerge within this period, we stop the crop growth (in_cycle) and go into another cropping season (f_crop_recycle). 
     
!     write(*,*) 'Pstade0: ', P_stade0
     if (TRIM(P_stade0) == 'snu') then
       if (nlev == 0) &
         call levee(n,stempdiag_cm_daily, shumdiag_cm_day, nlevobs,                                     & ! INPUTS
                    densiteger,densite,coeflev,densitelev,zrac,                                        & ! INOUT
                    somelong,somger,nlev,nger, humectation,nbjhumec,somtemphumec,somcour,              &
                    in_cycle, f_crop_recycle, nplt)               ! INOUT
     elseif (TRIM(P_stade0) == 'lev') then
       if (nlev==0) then
         nger = n
         nlev = n
!         nger = 0
         coeflev = 1
         densiteger = P_densitesem
         densitelev = densiteger*coeflev
         zrac = P_profsem
         densite = densitelev
       endif
     else
       write(*,*) 'Pstade0 ', P_stade0, ' not recognized'
     endif

!    DR 18/07/2012 je rajoute la germination
       if (n == nger) then
          stpltger = somcour   
          ! on affecte le cumul de température entre le semis et la levée
          ! DR 18/07/2012 pour la germination on affiche juste somcour sans le reinitialiser
          !      somcour = 0.0        ! on remet à zéro le cumul de température courant.
          !      if (P_codeperenne == 1) somcourdrp = 0.0 ! NB le 23/03 pour les pérenne début du décompte drp à la levée de dormance
          !      if (P_codefauche == 2) somcourfauche = 0.0
          !      if (codeulaivernal == 0) somcourutp = 0.0
       endif


     if (nlevobs == 999) then ! pas d'observation pour la levée
       if (n  == nlev) then   ! si on est le jour de la levée
         stpltlev = somcour ! on affecte le cumul de température entre le semis et la levée
         somcour = 0.0        ! on remet à zéro le cumul de température courant.
         if (P_codeperenne == 1) somcourdrp = 0.0 ! NB le 23/03 pour les pérenne début du décompte drp à la levée de dormance
         if (P_codefauche == 2) somcourfauche = 0.0
         if (codeulaivernal == 0) somcourutp = 0.0
       endif
     !else                       ! levée observée, this means that we used the forced LAI, but at this moment we do not use this option
     !  if (n  == nlevobs) then  ! si on est le jour de la levée observée
     !    nlev = nlevobs         ! on force nlev
     !    if (nger <= 0) nger = nlev  ! si la germination n'a pas encore été affectée, on la force au jour de la levée
     !    ! réajustement du parcours de dl
     !    stpltlev = somcour       ! on affecte le cumul de température entre le semis et la levée
     !    somcour = 0.0            ! on remet à zéro le cumul de température courant.'
     !    if (P_codeperenne == 1) somcourdrp = 0.0 ! NB le 23/03 pour les pérenne début du décompte drp à la levée de dormance
     !    if (P_codefauche == 2) somcourfauche = 0.0
     !    if (codeulaivernal == 0) somcourutp = 0.0
     !  endif
     endif



     ! 6.2  STATUS OF VEGETATIVE STAGES

     !: stade amf
     if (namfobs == 999) then
       if (somcour >= R_stlevamf .and. namf == 0 .and. nlev > 0) then
         namf = n
         R_stlevamf = somcour
         somcour = 0.0
         if (codeulaivernal == 0) somcourutp = 0.0
       endif
     !else
     !  if (n == namfobs) then
     !    namf = namfobs
     !    ! réajustement du parcours de dl
     !    R_stlevamf = somcour
     !    somcour=0.0
     !    if (codeulaivernal == 0) somcourutp = 0.0
     !    if (namf < nlev .or. nlev == 0) then
     !      call EnvoyerMsgHistorique(46)
     !      stop
     !    endif
     !  endif
     endif

     !: stade end of leaf onset
     if (nlaxobs == 999) then
       if (somcour >= R_stamflax .and. nlax == 0 .and. namf > 0) then
         nlax = n
         R_stamflax = somcour
         somcour = 0.0
         if (codeulaivernal == 0) somcourutp = 0.0
       endif
     !else
     !  if (n == nlaxobs) then
     !    nlax = nlaxobs
     !    ! réajustement du parcours de dl
     !    R_stamflax = somcour
     !    somcour = 0.0
     !    if (codeulaivernal == 0) somcourutp = 0.0
     !    if (nlax < namf .or. namf == 0) then
     !      call EnvoyerMsgHistorique(41)
     !      stop
     !    endif
     !  endif
     endif
    

     !: stade sen
     !- uniquement si P_codlainet=1
     if (P_codlainet == 1) then
       if (nsenobs == 999) then
         if (somcour >= R_stlaxsen .and. nsen == 0 .and. nlax > 0) then
           nsen = n
           R_stlaxsen = somcour
           somcour = 0.0
           if (codeulaivernal == 0) somcourutp = 0.0
         endif
       !else
       !  if (n == nsenobs) then
       !    ! réajustement du parcours de dl
       !    stdrpsen = somcour
       !    nsen = nsenobs
       !    if (nsen < nlax .or. nlax == 0) then
       !      call EnvoyerMsgHistorique(42)
       !      stop
       !    endif
       !    somcour = 0.0
       !    if (codeulaivernal == 0) somcourutp = 0.0
       !  endif
       endif
     endif

!:    version 4.0 suppression du stade fir


     !: stade lan
     !- NB - le 22/04 - si colainet=2 plus de stade lan
     if (P_codelaitr == 1 .and. P_codlainet == 1 .or. P_codelaitr == 2) then
       if (nlanobs == 999) then
         if (somcour >= R_stsenlan .and. nlan == 0 .and. nsen > 0) then
           nlan = n
           R_stsenlan = somcour
           somcour = 0.0
           if (codeulaivernal == 0)somcourutp = 0.0
         endif
       !else
       !  if (n == nlanobs) then
       !    ! réajustement du parcours de dl
       !    R_stsenlan = somcour
       !    nlan = nlanobs
       !    somcour = 0.0
       !    if (codeulaivernal == 0) somcourutp = 0.0
       !    if (nlan < nsen .or. nsen == 0) then
       !      call EnvoyerMsgHistorique(43)
       !      stop
       !    endif
       !  endif
       endif
     endif


!:    STADES REPRODUCTEURS

     !: stade flo
     if (nfloobs == 999) then
       if (somcourdrp >= stlevflo .and. nflo == 0) then
         nflo = n
         stlevflo = somcourdrp
!-   -      somcourdrp = 0.0 ! domi 04/04/01  pb canne on supprime la remise à zero
       endif
     !else
     !  if (n == nfloobs) then
     !    nflo = nfloobs
     !    ! réajustement du parcours de dl
     !    stlevflo = somcourdrp
     !    ! DR et ML 21/01/08 on teste le pb des sommes de temp foireuses
!-   !-      somcourdrp = 0.0
     !  endif
     endif


     !: stade drp
     if (ndrpobs == 999) then
       if (somcourdrp >= R_stlevdrp .and. ndrp == 0) then
         ndrp = n
!    NB le 29/3
!         R_stlevdrp=somcourdrp
!     domi 04/04/01 on fait un essai
!    DR et ML 21/01/08:
!    SUITE AUX PBS DE calcul des sommes de temp dans le bilan
!    lorsque on force flo ou drp => 2 modifs on enleve la remise à zero de somcourdrp si
!    flo est observe (je me demande bien pourquoi on faisait ca)
!    et R_stflodrp =somcourdrp-stlevflo
!-   -      R_stflodrp=somcourdrp
         R_stflodrp = somcourdrp - stlevflo
         somcourdrp = 0.0
!-   -      if (ndrp  = = nflo .or. nflo == 0) then
!-   -        call EnvoyerMsgHistorique(47)
!-   -        stop
!-   -      endif
       endif
     !else
     !  if (n == ndrpobs) then
     !    ndrp = ndrpobs
     !    ! réajustement du parcours de dl
     !    ! NB le 29/3
     !    !--      R_stlevdrp = somcourdrp
     !    ! domi 04/04/01  essai y'a un pb dans les sommes flo
     !    !--      R_stflodrp = somcourdrp
     !    !--      R_stflodrp = somcourdrp
     !    R_stflodrp = somcourdrp-stlevflo
     !    somcourdrp = 0.0
     !    if (ndrp < nflo .or. nflo == 0) then
     !      call EnvoyerMsgHistorique(47)
     !      stop
     !    endif
     !  endif
     endif

!     !: stade fin de nouaison pour la mise en place des fruits
!     if (P_codeindetermin == 2) then
!       if (somcourdrp >= P_stdrpnou .and. nnou == 0 .and. ndrp > 0) nnou = n
!     endif

     !: stade mat
     if (nmatobs == 999) then
       if (P_codeindetermin == 1) then   ! determinate crop 
         if (somcourdrp >= R_stdrpmat .and. nmat == 0 .and. ndrp > 0) then
           nmat = n
           R_stdrpmat = somcourdrp
           ! grain drying start
           drylen = drylen + 1
!           print *, 'in develop, for nmat do we go here, drylen is', drylen
         endif
       !else
       !  ! pour les indéterminées la maturité finale correspond à l'ensemble des P_nboite-1 vides
       !  ! Nb le 01/05 si P_nbcueille = 1
       !  ! si P_nbcueille = 2 : la maturité correspond au début de remplissage de la dernière boite
       !  if (P_nbcueille == 1) then
!-   - !           if (nbfruit == 0.0 .and. nmat == 0 .and.
       !    ! 12/07/06  DR et IGC nous avons changé la condition de calcul de la date
       !    ! de maturité. Avant il calculait celle-ci en fonction du nombre de grains.
       !    ! Maintenant, on calcule en focntion de la durée de fruits.
       !    if (somcourdrp > P_dureefruit .and. nmat == 0 .and. n > ndrp .and. ndrp > 0) then
       !      nmat = n
       !      R_stdrpmat = somcourdrp
       !    endif
       !  else
       !    if (nfruit > 0.0 .and. nmat == 0 .and. n > ndrp .and. ndrp > 0) then
       !      nmat = n
       !      R_stdrpmat = somcourdrp
       !    endif
       !  endif
       endif
     !else
     !  if (n == nmatobs) then
     !    nmat = nmatobs
     !    ! réajustement du parcours de dl
     !    R_stdrpmat = somcourdrp
     !    if (nmat < ndrp .or. ndrp == 0) then
     !      call EnvoyerMsgHistorique(44,ndrp)
     !      stop
     !    endif
     !  endif
     endif
    
     if (drylen > 0) then 
!        print *, 'in develop, the n and nmat is', n, nmat
        if (n > nmat) then
           drylen = drylen + 1
!           print *, 'in develop, the drylen is increasing', drylen
        endif
     endif


!     print *, 'in develop, the somcour and somcourdrp is:', somcour, somcourdrp


     !: stade rec
     if (nrecobs == 999) then !not forced
       ! détemination de la date de récolte par la teneur en eau des grains à partir de la maturité
!-   -     if (teaugrain == h2ograin .and. nrec == 0) then
!-   -       nrec = n
!-   -       stmatrec = somcourdrp-R_stdrpmat
!-   -       group = gpreco
!-   -       write(*,*) 'rec',n,somcour,stmatrec,upvt
!-   -     endif

       !: Récolte
       !- PB - 18/01/2005 - pas de récolte qd culture fauchée.
       if (P_codefauche /= 1) &   ! option of cut modes for forage crops: yes (1); and no (2)
           WRITE(numout,*) 'nrec before call', nrec
            !WRITE(numout,*) 'nrec(:) before call', nrec(:)

           call recolte(n,ndrp, gslen, drylen, lai,        &   ! INPUT 
                        nmat, nrec,              &   ! INOUT
                        stmatrec,group)       ! OUT
           WRITE(numout,*) 'nrec after call', nrec
            !WRITE(numout,*) 'nrec(:) after call', nrec(:)


     endif


!:    Affectation des bonnes valeurs d'unités de développement
!-    utilisées pour le calcul du LAI
     upvtutil = upvt
     !if (nlevobs /= 999 .and. n <= nlevobs) then
     !  upvtutil = upobs
     !endif

     !if (namfobs /= 999 .and. n <= namfobs .and. n > nlev) then
     !  upvtutil = upobs
     !endif

     !if (nlaxobs /= 999 .and. n <= nlaxobs .and. n > namf) then
     !  upvtutil = upobs
     !endif

     !if (nsenobs /= 999 .and. n <= nsenobs .and. n > nlax) then
     !  upvtutil = upobs
     !endif

     !if (nlanobs /= 999 .and. n <= nlanobs .and. n > nsen) then
     !  upvtutil = upobs
     !endif

     if (nlevobs == 999 .and. nlev == 0) then
       upvtutil = upvt
     endif

     if (namfobs == 999 .and. nlev > 0 .and. namf == 0) then
       upvtutil = upvt
     endif

     if (nlaxobs == 999 .and. namf > 0 .and. nlax == 0) then
       upvtutil = upvt
     endif

     if (nsenobs == 999 .and. nlax > 0 .and. nsen == 0) then
       upvtutil = upvt
     endif

     if (nlanobs == 999 .and. nsen > 0 .and. nlan == 0) then
       upvtutil = upvt
     endif


!    26/09/06 pour inaki on sort le test ici nrecbutoir ne sert qu'a declencher la recolte
!    inaki teste et on voit
!         if (n == nrecbutoir) then
!           if (nrec == 0) then
!             group = -1
!             nrec = nrecbutoir
!           endif
!     endif

!    ** Determination du groupe de precocite et effet de la date butoir
!    DR 06/01/06 ajout du test sur P_culturean = 1 pour les perennes sur une portion de leur cycle
     WRITE(numout,*) 'P_codefauche', P_codefauche
     WRITE(numout,*) 'nrecbutoir', nrecbutoir
     WRITE(numout,*) 'P_codeperenne', P_codeperenne
     WRITE(numout,*) 'P_codeinitprec', P_codeinitprec

     WRITE(numout,*) 'P_culturean', P_culturean


     if (n >= nrecbutoir .and. (P_codeperenne /= 2 .or. P_codeinitprec /= 2 .or. P_culturean == 1)) then
!    NB et IG le 23/09/06 suppression du test pour les cultures sur plusieurs années
!         if (n == nrecbutoir) then
       if (nrec == 0) then
         group = -1
         nrec = nrecbutoir
       endif
       if (nlev == 0) nlev = nrecbutoir
       if (nlev > 0 .and. namf == 0) then
         namf = nrecbutoir
         R_stlevamf = somcour
         somcour = 0.0
       endif
       if (namf > 0 .and. nlax == 0) then
         nlax = nrecbutoir
         R_stamflax = somcour
         somcour = 0.0
       endif
       if (nlax > 0 .and. nsen == 0) then
         nsen = nrecbutoir
         R_stlaxsen = somcour
         somcour = 0.0
       endif
       if (nsen > 0 .and. nlan == 0) then
         nlan = nrecbutoir
         R_stsenlan = somcour
         somcour = 0.0
       endif
!    ** NB le 26/03   (floraison)
       if (nflo == 0) then
         nflo = nrecbutoir
         stlevflo = somcourdrp
         somcourdrp = 0.0
       endif
       if (ndrp == 0) then
         ndrp = nrecbutoir
         R_stlevdrp = somcourdrp
         somcourdrp = 0.0
       endif
!    NB le 25/08/04 traitement debdes en cas de recolte butoir
       if (ndebdes == 0) then
         ndebdes = nrecbutoir
         R_stdrpdes = somcourdrp
       endif
!
       if (ndrp > 0 .and. nmat == 0) then
         nmat = nrecbutoir
!    NB le 25/08/04 réactivation ligne suivante
         R_stdrpmat = somcourdrp
       endif
     endif

endif
return
end subroutine develop2

!======================================================================================!
!======================================================================================!
!======================================================================================!

!> Routine de calcul des besoins en froid pour le P_codebfroid = 2
!>
!! Description : subroutine calculating the cold needed
!<
subroutine Stics_Develop_bfroid2(nger,                &  ! IN
                                 tdev,                &  ! IN
                                 rfvi,                &  ! OUT
                                 etatvernal,          &  ! INOUT
                                 caljvc)                 ! INOUT

USE Besoins_en_froid
USE Stics
!USE Messages

  implicit none

!: ARGUMENTS


! 0.1 INPUT
  !integer, intent(IN)    :: jjul  
  !integer, intent(IN)    :: n  
  !real,    intent(IN)    :: P_tfroid  !> // PARAMETER // optimal temperature for vernalisation // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_ampfroid  !> // PARAMETER // semi thermal amplitude thermique for vernalising effect // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_julvernal  !> // PARAMETER // julian day (between 1 and 365) accounting for the beginning of vernalisation for perennial crops // julian day // PARPLT // 1 
  !real,    intent(IN)    :: P_jvc  !> // PARAMETER // Number of vernalizing days // day // PARPLT // 1
  !real,    intent(IN)    :: P_jvcmini  !> // PARAMETER // Minimum number of vernalising days  // day // PARPLT // 1 
  !integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: nger  
  !integer, intent(IN)    :: namf  
  !integer, intent(IN)    :: numcult     ! number of years of simulation, number of crop seasons,   one-year crop, or two-year crop
  !integer, intent(IN)    :: nbjsemis    ! days number in the sowing year, it is the ith day in the sowing year
  real,    intent(IN)    :: tdev        ! temperature 
  !integer, intent(IN)    :: P_codemontaison  !> // PARAMETER // code to stop the reserve limitation from the stem elongation // code 1/2 // PARAMV6 // 0 
  !integer, intent(IN)    :: P_culturean  !> // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // 
  !integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
  !integer, intent(IN)    :: nbjanrec     ! days number in the harvest year, if the harvest year is different from the sowing year

! 0.2  INOUT
  !integer, intent(INOUT) :: maxwth   ! maximum days for the simulation, 365 or 366 which is determined by the leap or not. But here, we do not consider the leap year, so it is 365
  logical, intent(INOUT) :: etatvernal  ! is it not sensitive to vernalization?  false: sensitive,  ture, non-sensitive 
  real,    intent(INOUT) :: caljvc  
  !integer, intent(INOUT) :: onestan2  

! 0.3  OUT
  !integer, intent(OUT)   :: nrecbutoir  
  real,    intent(OUT)   :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1



!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!>
!> this part is especially for perennial grass----xcwu
! DR et ML et SYL 16/06/09
! prise en cpte de la montaison pour les prairies
!    if (P_codemontaison == 1) then   ! 
!! ####
!! SYL modif le 11/03/08
!! NB le 07/03/08
!      jul = jjul
!      if (jul == nbjsemis) then
!        onestan2 = 2
!      endif
!! DR et ML et SYL 16/06/09 Fin
!    else
!      jul = MOD(jjul,nbjsemis) ! jul = jjul modulo nbjsemis
!    endif
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!


    !!: entrée en vernalisation des herbacées pérennes au jour P_julvernal
    !if ( etatvernal .and. jul == P_julvernal .and. P_jvc > P_jvcmini .and. P_codeperenne == 2) then
    !  etatvernal = .FALSE.
    !endif

    if (.not.etatvernal) then

      !: On active P_julvernal pour les annuelles, qui joue lorqu'il se situe après la germination.
      !- NB et ML - 25/02/04 : à tester sur RGI CIPAN
      !- ML - 21/04/04 : on interdit de démarrer au stade 'dor' et d'avoir jul < P_julvernal pour les cultures perennes
      if ( nger /= 0 ) then

      ! This part is only for perennial grassland
      !! DR et ML et SYL 16/06/09
      !! prise en cpte de la montaison des prairies
      !! ####
      !  if (P_codemontaison == 1)then
      !    numcult_ver = onestan2
      !  else
      !    numcult_ver = numcult
      !  endif

        !if (jul+((numcult_ver-1)*nbjsemis) >= P_julvernal) then
        !--if (jul+((numcult-1)*nbjsemis) >= P_julvernal) then

          !: ML - 28/05/04 : on interdit de démarrer au stade lev et
          !- d'avoir jul > P_julvernal le jour de la levée (début de vernalisation) pour les cultures perennes
         


          ! for annual crop we calculate the vernalization---xcw
          if (P_codeperenne == 1) then
            call Vernalisation(tdev, rfvi, caljvc, etatvernal)
          else
            !if (n == namf .and. jul > P_julvernal .and. jul <= (P_julvernal+P_jvcmini)) then
            !   call EnvoyerMsgHistorique(32)
              stop    
            ! endif
            !if (n > namf) then
            !DR 20/07/2012 pas besoin de jjul
            !  call Vernalisation(tdev,P_jvc,P_jvcmini,P_codeperenne,P_culturean,P_codeinitprec,nbjanrec,P_tfroid,P_ampfroid,    &
            !                      jjul,n, rfvi,nrecbutoir,maxwth,caljvc,etatvernal)
            !                     n, rfvi,nrecbutoir,maxwth,caljvc,etatvernal)
            !endif
          endif
        !else    ! < P_julvernal
        !  if (P_codeperenne == 1) then
        !    rfvi = 1.0
        !  else
        !    !call EnvoyerMsgHistorique(50)
        !    stop
        !  endif
        !endif

      else   ! not  germination yet
        rfvi = 1.0
      endif
    else    ! not sensitive to vernalization
      rfvi = 1.0
    endif

return
end subroutine Stics_Develop_bfroid2

!======================================================================================!
!======================================================================================!
!======================================================================================!


!> Routine de calcul des besoins en froid pour le P_codebfroid = 3
!> It is only effective for perennial crops, such as vineyard
!! Description :
!<
!subroutine Stics_Develop_bfroid3(cu_min,cu_veille,n,tmin,tmax,thor, &
!                                 etatvernal,cu,rfvi,ndebdorm,nfindorm,nlev)
!
!USE Besoins_en_froid
!
!implicit none
!
!!  integer, intent(IN)    :: P_codedormance  !> // PARAMETER // option of calculation of dormancy and chilling requirement // code 1/2 // PARPLT // 0 
!  real,    intent(IN)    :: cu_min  
!  real,    intent(IN)    :: cu_veille  
!  integer, intent(IN)    :: n  
!!  real,    intent(IN)    :: P_jvc  !> // PARAMETER // Number of vernalizing days // day // PARPLT // 1 
!!  real,    intent(IN)    :: P_q10  !> // PARAMETER // P_Q10 used for the dormancy break calculation  // SD // PARPLT // 1 
!  real,    intent(IN)    :: tmin   !> // OUTPUT // Minimum active temperature of air // degree C
!  real,    intent(IN)    :: tmax   !> // OUTPUT // Maximum active temperature of air // degree C
!  real,    intent(IN)    :: thor(24)  
!
!  logical, intent(INOUT) :: etatvernal  
!  real,    intent(INOUT) :: cu  
!  real,    intent(OUT)   :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
!  integer, intent(INOUT) :: ndebdorm  
!  integer, intent(INOUT) :: nfindorm  
!  integer, intent(INOUT) :: nlev  
!
!
!
!    if (.not.etatvernal) then
!      !: Calcul des cu (chill units ?)
!      select case(P_codedormance)
!        case(1,2)
!          call Dormancy_Richardson(thor,n,cu_min,cu_veille,ndebdorm,cu)
!        case(3)
!          call Dormancy_Bidabe(n,ndebdorm,P_q10,tmin,tmax,cu_veille,cu)
!      end select
!
!
!      if (P_codedormance >= 2) then
!
!        !: Cas des calculs de Richardson ou Bidabe
!        if (cu > P_jvc) then
!          rfvi = 1.0
!          !: 17/03/08 : maintenant on garde nfindorm0 si nfin s'est passe annee d'avant
!          !--if (nfindorm == 0) nfindorm=n
!          nfindorm = n
!          etatvernal = .TRUE.
!
!          !: DR - 20/11/06 : On est un peu perplexe , on etait sur d'avoir testé tous les cas.
!          !- Quand on arrive en fin de dormance, si on ne met pas nlev=0 on ne calcule plus les sommes
!          !- d'action chaude de richardson. La date de levée a été stockée dans ilevs
!          nlev = 0
!        else
!          rfvi = 0.0
!        end if
!
!      else
!
!        !: Cas de forçage de la levée de dormance
!        if (n < nfindorm) then
!         rfvi = 0.0
!        else
!          rfvi = 1.0
!          etatvernal = .TRUE.
!        end if
!
!      end if
!
!    else
!
!      !: ML - le 18/10/05 : Cas de la dormance calculee avec Bidabe: on demarre la dormance à ndebdorm
!      if (P_codedormance == 3 .and. n == ndebdorm) then
!        etatvernal = .FALSE.
!        rfvi = 0.0
!      else
!        rfvi = 1.0
!      endif
!
!    endif
!
!
!return
!end subroutine Stics_Develop_bfroid3
