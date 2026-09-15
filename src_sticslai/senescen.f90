! *******************************************************************
!    version 6.0
!    reste pb cultures fauchées à voir avec Domi
!--------------------------------------------------
!    calcul de la senescence de la matiere seche
!    avec eventuellement une partie residuelle pour les cultures fauchees
!
!    calcul de la sénescence du LAI pour l'option LAI brut
! derniere modif 30/05/05
! *******************************************************************
!> In STICS shoot senescence only concerns leaves: dry matter and LAI.  For cut crops, it also affects residual biomass after cutting.
!> - Stics book paragraphe 3.1.2, page 44-46
!> While in the first versions of the model senescence was implicit (Brisson et al., 1998a), it is now explicit, with a clear distinction between natural
!! senescence due to the natural ageing of leaves, and senescence accelerated by stresses (water, nitrogen, frost). The concept of leaf lifespan,
!! used for example by Maas (1993), is applied to the green leaf area and biomass produced. The leaf area and part of the leaf biomass produced on a given day
!! is therefore lost through senescence once the lifetime has elapsed (Duru et al., 1995). This part corresponds to the ratiosen parameter, taking into account
!! the part which was remobilised during its senescence.
!!
!! Calculation of lifespan:
!!
!! The natural lifespan of leaves (durage) is defined by two values: the  lifespan of early leaves, or durvieI (expressed as a proportion of durvieF ) and the
!! lifespan of the last leaves emitted, or durvieF (assumed genotype-dependent). Until the IAMF stage, the natural lifespan, calculated for the day when the
!! leaves are emitted (I0) is durvieI; from IAMF to ILAX, the natural lifespan increases between durvieI and durvieF as a function of the leaf development variable ULAI.
!! Because of water or nitrogen stress, the current lifespan may be shortened if the stress undergone is more intense than previous stresses.
!! Specific stress indices for senescence are introduced (senfac and innsenes).  Frost (fstressgel that can be either fgeljuv or fgelveg) may also reduce or
!! even cancel  lifespan.  In the event of over-fertilisation with nitrogen (inn >1), the foliage lifespan is increased from the IAMF stage up to a maximum
!! given by the durviesupmax parameter.
!! The lifespan of leaves is not expressed in degree.days (like phasic development), because this has the disadvantage of stopping any progression as soon as
!! the temperature is lower than the base temperature (tdmin).  To remedy this problem, the senescence course (somsen) is expressed by cumulated Q10 units
!! (with Q10=2), i.e. an exponential type function.
!! The senescence course between I0 and I is affected by the same cardinal temperatures as phasic development and can be slown down by stresses. The lifespan parameter
!! of the leaf (durvieF) expressed in Q10 units represents about 20% of the same  lifespan expressed in degree.days.
!!
!! Calculation of senescence:
!!
!! Material produced on day I0 disappears by senescence after a period corresponding to durvie(I0). Depending on the evolution of temperature and of lifespan
!! as a function of phenology and stresses, senescence can vary from one day to another and affect several days of production (J=I0, I0+1, …) or, on the contrary,
!! none (durvieE(I0)>somsen(I)).  This principle is applied to the biomass (dltamsen) and leaf area index (ltaisen). In general, the leaf biomass produced
!! does not completely disappear (remobilisation):  the ratiosen (<1) parameter enables the definition of the senescent proportion with reference to production.
!! It is the pfeuilverte ratio which defines the proportion of leaves in the biomass produced.
!! The cumulative senescent foliage is laisen. If the crop is a forage crop benefiting from residual dry matter from the previous cycle (msresiduel parameter),
!! the senescence of residual dry matter (deltamsresen) starts as from cutting.  It occurs at a rate estimated from the relative daily lifespan course and
!! weighted by the remobilisation (ratiosen).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!subroutine senescen(nlev,n,nbjmax,lai,P_codeinnact,P_codeh2oact,senfac,innsenes,P_codlainet,  &
!                    P_codeperenne,nbfeuille,P_nbfgellev,P_codgellev,tcultmin,P_tletale,P_tdebgel, &
!                    P_tgellev90,P_tgellev10,ipl,densitelev,densiteassoc,P_codgeljuv,          &
!                    P_tgeljuv90,P_tgeljuv10,P_codgelveg,P_tgelveg90,P_tgelveg10,masecveg,         &
!                    nstopfeuille,somcour,resperenne,ndrp,nrec,P_QNpltminINN,numcult,      &
!                    P_codeinitprec,ulai,P_vlaimax,durvieI,P_durvieF,inn,P_durviesupmax,         &
!                    P_codestrphot,phoi,P_phobasesen,dltams,P_msresiduel,P_ratiosen,tdevelop,    &
!                    somtemp,pfeuilverte,deltai,P_lai0,                                    &
!                    dernier_n,nsencour,dltaisen,dltamsen,fstressgel,fgellev,gelee,      &
!                    densite,laisen,nlan,R_stsenlan,nsen,R_stlaxsen,namf,nlax,R_stlevamf,      &
!                    R_stamflax,nrecbutoir,mortplante,nst2,mortplanteN,durvie,strphot,     &
!                    msres,dltamsres,ndebsen,somsenreste,msresjaune,mafeuiljaune,        &
!                    msneojaune,dltamstombe,QNplante,P_dltamsminsen,P_dltamsmaxsen,          &
!                    P_alphaphot,strphotveille)

subroutine senescen(nlev,n, lai, senfac,  &
                    nbfeuille,t2m_min_daily, &
                    densitelev,          &
                    masecveg,         &
                    nstopfeuille,somcour,nrec,      &
                    ulai, &
                    tdevelop,    &
                    somtemp,nsenpfeuilverte,nsendltai,nsendurvie, nsendltams, nsenndurvie,         &    ! INPUTS
                    pdlaisen, dernier_n,nsencour,dltaisen,dltamsen,fstressgel,fgellev,gelee,      & !INOUT
                    densite,laisen,nlan,R_stsenlan,nsen,R_stlaxsen,namf,nlax,R_stlevamf,      &
                    R_stamflax,durvie,     &
                    ndebsen,somsenreste, mafeuiljaune, msneojaune,  & !INOUT
                    durvieI, deltai, dltams, &
                    histgrowth, hist_sencour, hist_latest, doyhistst,  &
                    nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, &
                    boxbiom,boxbiomrem, boxdurage, boxsomsenbase)  ! OUTPUT

USE constantes
USE Stics
USE Divers_gel
!USE Messages

  implicit none

! DECLARATION

! 0.1 INPUTS

  integer, intent(IN)    :: nlev  
  integer, intent(IN)    :: n  
  !integer, intent(IN)    :: nbjmax  
  real,    intent(IN)    :: lai   !> // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
  !integer, intent(IN)    :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  !integer, intent(IN)    :: P_codeh2oact  !> // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: senfac   !> // OUTPUT // Water stress index on senescence // 0-1
  !real,    intent(IN)    :: innsenes   !> // OUTPUT // Index of nitrogen stress active on leaf death // P_innmin to 1
  !integer, intent(IN)    :: P_codlainet  !> // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
  !integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  integer, intent(IN)    :: nbfeuille   !> // OUTPUT // Number of leaves on main stem // SD
  !integer, intent(IN)    :: P_nbfgellev  !> // PARAMETER // leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 // PARPLT // 1 
  !integer, intent(IN)    :: P_codgellev  !> // PARAMETER // activation of plantlet frost // code 1/2 // PARPLT // 0 
  real,    intent(IN)      :: t2m_min_daily    !// minimum daily temperature // celsius degree
  !real,    intent(IN)    :: tcultmin  
  !real,    intent(IN)    :: P_tletale  !> // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_tdebgel  !> // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_tgellev90  !> // PARAMETER // temperature corresponding to 90% of frost damage on the plantlet  // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_tgellev10  !> // PARAMETER // temperature corresponding to 10% of frost damage on the plantlet  // degree C // PARPLT // 1
  !integer, intent(IN)    :: ipl         !> number of kinds of  crops 
  real,    intent(IN)    :: densitelev  
  !real,    intent(IN)    :: densiteassoc  
  !integer, intent(IN)    :: P_codgeljuv  !> // PARAMETER // activation of LAI frost at the juvenile stadge // code 1/2 // PARPLT // 0 
  !real,    intent(IN)    :: P_tgeljuv90  !> // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_tgeljuv10  !> // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
  !integer, intent(IN)    :: P_codgelveg  !> // PARAMETER // activation of LAI frost at adult stage // code 1/2 // PARPLT // 0 
  !real,    intent(IN)    :: P_tgelveg90  !> // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
  !real,    intent(IN)    :: P_tgelveg10  !> // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
  real,    intent(IN)    :: masecveg   !> // OUTPUT // Vegetative dry matter // t.ha-1
  integer, intent(IN)    :: nstopfeuille  
  real,    intent(IN)    :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
  !real,    intent(IN)    :: resperenne   !> // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1 
  !integer, intent(IN)    :: ndrp  
  integer, intent(IN)    :: nrec  
  !real,    intent(IN)    :: P_QNpltminINN  !> // PARAMETER // minimal amount of nitrogen in the plant allowing INN computing // kg ha-1 // PARAM // 1 
  !integer, intent(IN)    :: numcult  
  !integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
  real,    intent(IN)    :: ulai  !>   // OUTPUT // Daily relative development unit for LAI // 0-3
  !real,    intent(IN)    :: P_vlaimax  !> // PARAMETER // ULAI at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
  !real,    intent(IN)    :: P_durvieF  !> // PARAMETER // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1 
  !real,    intent(IN)    :: inn   !> // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
  !real,    intent(IN)    :: P_durviesupmax  !> // PARAMETER // proportion of additional lifespan due to an overfertilization // SD // PARPLT // 1 
  !integer, intent(IN)    :: P_codestrphot  !> // PARAMETER // activation of the photoperiodic stress on lifespan : yes (1), no (2) // code 1/2 // PARPLT // 0 
  !real,    intent(IN)    :: phoi   !> // OUTPUT // Photoperiod // hours
  !real,    intent(IN)    :: P_phobasesen  !> // PARAMETER // photoperiod under which the photoperiodic stress is activated on the leaf lifespan // heures // PARPLT // 1 
  !real,    intent(IN)    :: dltams(nbjmax)  !>   // OUTPUT // Growth rate of the plant  // t ha-1.j-1
  !real,    intent(IN)    :: P_msresiduel  !> // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1 
  !real,    intent(IN)    :: P_ratiosen  !> // PARAMETER // fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 // PARPLT // 1 
  real,    intent(IN)    :: tdevelop
  real,    intent(IN)    :: somtemp   !> // OUTPUT // Sum of temperatures // degree C.j
  !real,    intent(IN)    :: pfeuilverte(nbjmax) !>  // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
  !real,    intent(IN)    :: deltai(nbjmax)  !>   // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
  !real,    intent(IN)    :: P_lai0  !> // PARAMETER // Initial leaf area index // m2 m-2 // INIT // 1 
  !real,    intent(IN)    :: dltamstombe  
  !real,    intent(IN)    :: QNplante   !> // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1
  !real,    intent(IN)    :: P_dltamsminsen  !> // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARAM // 1 
  !real,    intent(IN)    :: P_dltamsmaxsen  !> // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1 
  !real,    intent(IN)    :: P_alphaphot  !> // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARAM // 1 
  real,     intent(IN)    :: nsenpfeuilverte
  real,     intent(IN)    :: nsendltai 
  real,     intent(IN)    :: nsendurvie 
  real,     intent(IN)    :: nsendltams 
  real,     intent(IN)    :: nsenndurvie 
  real,     intent(IN)    :: deltai
  real,     intent(IN)    :: dltams


! 0. 2 INOUT

  real,     intent(INOUT) :: pdlaisen    !>  senescence leaf area index of previous day // m2 leafs  m-2 soil 
  integer, intent(INOUT) :: dernier_n  
  integer, intent(INOUT) :: nsencour  
  real,    intent(INOUT) :: dltaisen   !> // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
  real,    intent(INOUT) :: dltamsen   !> // OUTPUT // Senescence rate // t ha-1 j-1
  real,    intent(INOUT) :: fstressgel   !> // OUTPUT // Frost index on the LAI // 0-1
  real,    intent(INOUT) :: fgellev  
  logical, intent(INOUT) :: gelee  
  real,    intent(INOUT) :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(INOUT) :: laisen    ! // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
  integer, intent(INOUT) :: nlan  
  real,    intent(INOUT) :: R_stsenlan  !> // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1 
  integer, intent(INOUT) :: nsen  
  real,    intent(INOUT) :: R_stlaxsen  !> // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1 
  integer, intent(INOUT) :: namf  
  integer, intent(INOUT) :: nlax  
  real,    intent(INOUT) :: R_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
  real,    intent(INOUT) :: R_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
  !integer, intent(INOUT) :: nrecbutoir  
  !integer, intent(INOUT) :: mortplante  
  !integer, intent(INOUT) :: nst2  
  !integer, intent(INOUT) :: mortplanteN  
  real,    intent(INOUT) :: durvie !<   // OUTPUT // Actual life span of the leaf surface //  degree C
  !real,    intent(INOUT) :: msres  
  !real,    intent(INOUT) :: dltamsres  
  integer, intent(INOUT) :: ndebsen  
  real,    intent(INOUT) :: somsenreste  
  !real,    intent(INOUT) :: msresjaune   !> // OUTPUT // Senescent residual dry matter  // t.ha-1
  real,    intent(INOUT) :: mafeuiljaune   !> // OUTPUT // Dry matter of yellow leaves // t.ha-1
  real,    intent(INOUT) :: msneojaune   !> // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
  !real,    intent(INOUT) :: strphotveille  

  real, dimension(300,5), intent(INOUT) :: histgrowth
  integer, intent(INOUT)                :: hist_sencour
  integer, intent(INOUT)                :: hist_latest
  integer, intent(INOUT)                :: doyhistst

! compartment senescence module
!boxulai, boxndays, boxlai, boxlairem,boxtdev, boxbiom, boxbiomrem, boxdurage,
!boxsomsenbase
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


! 0.3 OUTPUT

  !real,    intent(OUT)   :: strphot  
  real,    intent(OUT)    :: durvieI  

! 0.4 LOCAL VARIABLES

  integer :: i  !>  
  integer :: ii
  integer :: ncompteur  !>  
  integer :: debut  !>  
  integer :: fin  
  !real    :: vitsenres  !>  
  real    :: durage  !>  
  real    :: durviesup  !>  
  real    :: senstress  !>  
  real    :: somsen  !>  
  !real    :: tgel10  !>
  !real    :: tgel90
  !real    :: msresTMP  
  real   :: dtdevrat
  real   :: tempdays, templaisen, tempamsen
  logical :: myprint



! 1. whether or not go into this subroutine
      !:On shunte ce programme si la plante n'est pas levée ou si le lai est nul
      ! If the emergence of crop did not occur, the value of nsencour is always 0
      myprint = .false.
      if (nlev == 0) then
        ! *- 07/09/06 DR et NB et IGC (la sainte famille)
        ! *- apres un 2 jours d'errements nadine a été notre lumiere !!
        ! *- Qu'elle en soit louée !
        ! *- initialisation de nsencour au debut du cycle


        nsencour = n  !initialization of nsencour 

        return
      endif
        if (P_codlainet==3) then ! accumulated part for compartment senescence
            if (ulai < 1 .or. ulai > 3) then
                write(*,*) 'xuhui: ulai: ', ulai, ', likely a bug'
            elseif (ulai==3) then
!                write(*,*) 'xuhui: ulai==3 with deltai ',deltai
            else
                ii = nbox
                do while (ulai < boxulai(ii))
                    ii = ii - 1
                enddo
                if (ii < 1) then
                    write(*,*) 'xuhui: bug: ii<1'
                    ii = 1
                endif
!                write(*,*) 'xuhui: ulai ',ulai,' ii ', ii
!                write(*,*) 'xuhui: boxulai ',boxulai
                if (boxndays(ii) == 0) then ! the 1st leaf in this pool
                    boxsomsenbase(ii) = somtemp
                    boxlai(ii) = boxlai(ii) + P_lai0/real(nbox) ! equally distributed the original lai to all boxes
!                    if (ii == 1) then  ! put P_lai0 in the 1st box
!                        boxlai(ii) = boxlai(ii) + P_lai0
!                    endif
                endif
                boxndays(ii) = boxndays(ii) + 1
                boxlai(ii) = boxlai(ii) + deltai
                boxlairem(ii) = boxlairem(ii) + deltai
                boxbiom(ii) = boxbiom(ii) + dltams
                boxbiomrem(ii) = boxbiomrem(ii) + dltams
                boxtdev(ii) = boxtdev(ii) + tdevelop
            endif
        endif
 

      !: PB - 08/03/2005 - si encore de la matière jaune on continue la senescence
     
      ! in our model, we do not consider the processes regarding the yellow leaves, so the variable mafeuiljaune is not necessary

      !if (lai <= 0 .and. mafeuiljaune <= 0.) then
      if (lai <= 0) then
        dltaisen = 0.
        dltamsen = 0.
        return
      endif


! 2. calculation of the lifetime for leaves
 
! ** calcul de la durée de vie
! *---------------------------
! *-  a) calcul du LAI net : on utilise version 4 avec une durée de vie fixe (stlevsenms)
! *-  b) calcul du LAI brut: la durée de vie est fonction de l'age de la plante
! *-                         (durée de vie potentielle x facteurs de stress)
! *-   durée de vie potentielle des feuilles (durage) calculée dans calai
! *-   facteurs de stress eau et azote calculés à partir de senfac (transpi.for)
! *-   et innsenes (absorn.for)
! *-------------------------------------------------------------------
! *- spécifiques de la sénescence


   ! 2. 1 Comments: calculation of senstress,
   ! if we chose the direct calculation of LAI, the senstress is 1. 
   ! calculation of senfac is implemented in stress process


      if (P_codeinnact == 1 .and. P_codeh2oact == 1) then
        senstress = min(senfac,P_innsenes)
      endif

      if (P_codeinnact == 2 .and. P_codeh2oact == 1) then
        senstress = min(senfac,1.)
      endif

      if (P_codeinnact == 1 .and. P_codeh2oact == 2) then
        senstress = min(1.,P_innsenes)
      endif

      if (P_codeinnact == 2 .and. P_codeh2oact == 2) senstress = 1.

      !if (P_codlainet == 1) senstress = 1.



   ! 2. 2 ** calcul du STRESS lié au GEL  (utilisation de la fonction GEL.for)
   ! *-------------------------------------------------------------------
   ! *- GEL entre levée et plantule diminue la densité de population uniquement pour les annuelles

      if (P_codeperenne == 1)  then
        if (nbfeuille <= P_nbfgellev) then
          !fgellev = GEL(P_codgellev,tcultmin,P_tletale,P_tdebgel,P_tgellev90,P_tgellev10)
          fgellev = GEL(P_codgellev, t2m_min_daily, P_tgellev90, P_tgellev10)
          if (fgellev < 1.) gelee = .TRUE.    ! we must consider the effects of gel
          !if (ipl == 1) then     ! only consider one kind of plant
            densite = min(densite, densitelev*fgellev)
          !else
          !  densite = min(densite, (densitelev + densiteassoc)*fgellev)
          !endif
        endif
      endif

      ! ** GEL de l'appareil végétatif à deux stades: juvénil (avant AMF) et adulte (après AMF)
      if (namf <= 0) then
        fstressgel = GEL(P_codgeljuv, t2m_min_daily, P_tgeljuv90, P_tgeljuv10)
      else
        fstressgel = GEL(P_codgelveg, t2m_min_daily, P_tgelveg90, P_tgelveg10)
      endif
      if (fstressgel < 1.) gelee = .TRUE.

 
 
      ! ** GEL LETAL pour la plante
      if (fstressgel <= 0.) then  ! frost damage occurs
        dltaisen  = lai
        dltamsen  = masecveg   ! daily dry matter
        !laisen(1) = laisen(0) + dltaisen ! consider the former day
        laisen = pdlaisen + dltaisen


        ! ** affectation des stades
        if (nstopfeuille > 0 .and. nlan == 0) then
          nlan = n
          R_stsenlan = somcour
          if (nsen == 0) then
            nsen = n
            R_stlaxsen = somcour
            R_stsenlan = 0.
          endif
        else
          ! comments: resperenne is only for perennial crops
          ! From the reptair process, we know that the resperenne is 0 for annual determinant crops
          
          if (resperenne <= 0.) then  ! resperenne is global constance
            if (namf == 0) then
              namf = n
              nlax = n
              nsen = n
              nlan = n
              R_stlevamf = somcour
              R_stamflax = 0.
              R_stlaxsen = 0.
              R_stsenlan = 0.
            endif
            if (nlax == 0 .and. namf > 0) then
              nlax = n
              nsen = n
              nlan = n
              R_stamflax = somcour
              R_stlaxsen = 0.
              R_stsenlan = 0.
            endif
            if (nsen == 0 .and. nlax > 0) then
              nsen = n
              nlan = n
              R_stlaxsen = somcour
              R_stsenlan = 0.
            endif
          endif
        endif
        !if (resperenne <= 0.) then
        !  call EnvoyerMsgHistorique(1167)
        !  !: modif - NB - 20/09 - mort le jour d'apres pour écriture rapport
        !  !nrecbutoir = n+1
        !  !mortplante = 1
        !  !: pour non plantade dans bilan.for - TODO : à mettre ici ou dans bilan ?
        !  if (ndrp == 0) nst2 = 1
        !endif
        goto 30
      endif

      if (n == nlev .and. P_codeperenne == 1) then
        nsencour = nlev
      endif

      ! *- DR et NB - 25/08/08 - s'il n'y a plus d'azote dans la plante on la fait mourir
      ! DR 11/04/2012 pour le prairie on a des pbs car quand on coupe on a plus d'azote dans la  plante
      ! donc je mets < au lieu de <=
      
      ! Comments: at this moment, the crop are not N limited, so QNplante >= p_qnpltmininn

      !if ((namf > 0 .and. nrec == 0) .and. QNplante < P_QNpltminINN) then
      !  call EnvoyerMsgHistorique(2099)
      !! modif NB 20/09 mort le jour d'apres pour écriture rapport
      !  nrecbutoir  = n+1
      !  mortplanteN = 1
      !! pour non plantade dans bilan.for
      !  if (ndrp == 0) nst2 = 1
      !endif

! *- NB - le 22/12/2003 - pb senescence perenne vu avec FR
! *- PB - 05/01/2005 - mise en commentaire jusqu'à nouvel ordre.
! *- Sinon cela perturbe le calcul de la sénescence lors de l'enchainement des pérennes.
      !if (n == nlev .and. P_codeperenne == 2 .and. numcult == 1) then
      !  nsencour = 1
      !endif

! ** calcul de la durée de vie
! *----------------------------
! *-  nsenscour = jour où la biomasse sénescente a été produite
! *-  durage = durée de vie potentielle fonction de la P_phénologie
! *-  (varie entre durvieI et P_durvieF; durvieI avant lax)

      debut = nsencour
      if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. nsencour > n) then
        fin = dernier_n
      else
        fin = n
        dernier_n = n
      endif

! calculation of the durvieI
      !durvieI = P_ratiodurvieI * P_durvieF(P_variete)
      durvieI = P_ratiodurvieI * P_durvieF   !(P_variete)
      !do i = debut, fin
      
      if (P_codlainet==1) then
            if (ulai <= P_vlaimax) then
              durage = durvieI   ! where is the durvieI??   p(ipl)%durvieI = p(ipl)%P_ratiodurvieI * p(ipl)%P_durvieF(itk(ipl)%P_variete), it seems a parameter
            else
              durage = durvieI + ((ulai - P_vlaimax) / (3. - P_vlaimax)) * (P_durvieF - durvieI)
            endif
    
            !! ** application des stress qui raccourcissent la durée de vie
            !if (i < fin) then
            !  durvie(i) = min(durvie(i), durage * min(senstress, fstressgel))
            !else
              durvie = durage * min(senstress, fstressgel)
            !endif
    
            ! ** augmentation de durée de vie maxi en cas de surfertilisation azotée
            !if (namf > 0 .and. i > namf .and. P_codlainet == 2) then
            !  if (inn > 1.) then
            !    durviesup = P_durvieF * min(P_durviesupmax, (inn - 1.))
    !	!		write(*,*)n,'durviesup',P_durvieF,durviesup
            !  else
            !    durviesup = 0.
            !  endif
            !else
            !  durviesup = 0.
            !endif
    
            durviesup = 0.
       
            durvie = durvie + durviesup
    
            ! domi 04/01/06 pour inaki insertion du stress photoperiodique
            ! NB le 22/06/06 introduction dans la boucle des "i"
    
            ! at this moment, we do not consider the stress of photoperiod on senescence---xcw
            !if (P_codestrphot == 1 .and. nlax /= 0) then
            !  if (phoi < P_phobasesen) then
            !    call stressphot(dltams(n), P_dltamsminsen, P_dltamsmaxsen, P_alphaphot, strphot, strphotveille)
            !    durvie(i) = durvie(i) * strphot
            !  endif
            !endif
    
          !end do
    
    
          !if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. debut > n) then
          !  debut = 1
          !  fin = n
          !  goto 5
          !endif
    
          ! ** la base de temps de la sénescence est calculée dans develop.for
          ! *- tdevelop(n) est un P_Q10 sommé dans la variable somtemp
    
          ! ** pour les cultures fauchées : senescence de la matiere seche résiduelle
          ! *- qui demarre dès la coupe
          ! *- PB - 25/01/2004 - ajout de msresTMP et dltamsres pour calculer le delta msres(n-1)<->msres(n)
          ! *- et ce, pour pallier à un problème dans le calcul de msresjaune pour les pérennes enchainées fauchées
          !if (msres > 0.) then
          !  vitsenres = P_msresiduel / durvieI
          !  msresTMP = msres - (P_ratiosen * vitsenres * tdevelop(n))
          !  msresTMP = max(0.,msresTMP)
          !  dltamsres = max(0., msres - msresTMP)
          !  msres = msresTMP
          !else
          !  dltamsres = 0.
          !endif
    
    
    
          ! ** pour les autres cultures ou pour la matière sèche néoformée
    
    
          ! ** DOMI VOIR POUR PRAIRIE UTILISATION DE somcourfauche
          ! ** 2/ senescence de la matiere seche neoformee qui demarre au stade
          ! *- debut de senescence de la matiere seche identifie par le parcours
          ! *- de developpement depuis la levee : stlevsenms
          ! *- on calcule vitsen le jour du début de sénescence : ndebsen
    
          !if (somtemp >= durvie(n)) then
          if (somtemp >= durvie) then
            if (ndebsen == 0) then ! premier jour de senescence
              ndebsen = n
              nsencour = nlev
    
              !dltamsen =  dltams(nsencour) * P_ratiosen * pfeuilverte(nsencour)
              dltamsen = nsendltams * P_ratiosen * nsenpfeuilverte     
    
              if (P_codlainet == 2) then
                dltaisen = nsendltai + P_lai0
              endif
    
            ! ** enlever le reste du LAI si deltai(nsencour) = 0.
    
            else  ! jours suivants
              somsen = somsenreste
              dltamsen = 0.
              ncompteur = 0
              if (P_codlainet == 2) dltaisen = 0.
    
    ! ** calcul de la somme de température depuis le dernier jour de sénescence
    ! *- PB - 05/01/2005 - ajout d'un test pour l'enchainement des pérennes. On calcul somsen
    ! *- à partir du nsencour du cycle précédent jusqu'à la fin du cycle précédent et
    ! *- on l'ajoute à la somme des températures depuis le début du cycle courant jusqu'au jour n
              !if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. nsencour > n) then
              !  do i = nsencour+1, dernier_n
              !    somsen = somsen + tdevelop(i)
              !  end do
              !  do i = 1, n
              !    somsen = somsen + tdevelop(i)
              !  end do
              !else
                !do i = nsencour+1, n
                !  somsen = somsen + tdevelop(i)
                !end do
                if (nsencour == nlev .and. nsencour /= 0) then
                   somsen = somsen + tdevelop
                endif
              !endif
    
    
              !if (somsen > durvie(nsencour+1)) then
              if (somsen > nsenndurvie) then
20              nsencour = nsencour + 1
    
    
    ! *- PB - 05/01/2004 - ajout d'un test lors de l'enchainement des pérennes pour que nsencour
    ! *- ne dépasse pas dernier_n. Si nsencour>dernier_n alors on a atteint la fin du cycle précédent,
    ! *- et donc on doit repartir du début du cycle courant. Soit nsencour = 1
               ! if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. nsencour > dernier_n) then
               !   nsencour = 1
               ! endif
    
                !somsen = somsen - durvie(nsencour)
                somsen = somsen - nsendurvie
                if (somsen < 0.) then
                  somsen = 0.
                  nsencour = nsencour - 1
                  goto 40 ! TODO : a priori goto 40 pourrait être remplacé par goto 30s
                endif
    
                !dltamsen = dltamsen + (dltams(nsencour) * P_ratiosen * pfeuilverte(nsencour))
                dltamsen = dltamsen + (nsendltams * P_ratiosen * nsenpfeuilverte)
                ncompteur = ncompteur+1
                if (P_codlainet == 2) then
                  !dltaisen = dltaisen + deltai(nsencour)
                  dltaisen = dltaisen + nsendltai
                endif
    
                ! ** si la durée de vie est raccourcie, on peut "faire mourir" plusieurs jours de production
                ! *- PB - 10/03/2005 - Petite modificatoin, on compare nsencour à dernier_n et non plus à n.
     
       
                !if (somsen > durvie(nsencour) .and. nsencour < dernier_n) goto 20
                if (somsen >  nsendurvie .and. nsencour < dernier_n) goto 20
    
                somsenreste = somsen
              else
                dltamsen = 0.
                if (P_codlainet == 2) dltaisen = 0.
              endif
40            continue
            endif
          else
            dltamsen = 0.
            if (P_codlainet == 2) dltaisen = 0.
          endif
    
    elseif (P_codlainet==2) then
!        dltaisen = 0.
!        write(*,*) 'new senescence process, xuhui' 
        IF (ulai <= P_vlaimax) THEN
            durage = durvieI
        ELSE
            durage = durvieI + ((ulai - P_vlaimax) / (3. - P_vlaimax)) * (P_durvieF - durvieI)
        ENDIF
        durvie = durage * min(senstress, fstressgel)
        ! after calculation of durvie, record it
        histgrowth(hist_latest,5) = durvie
        ! update all previous durvie with minimum values
        if (hist_latest > hist_sencour) then
            do ii = hist_sencour, hist_latest-1
                histgrowth(ii,5) = min(histgrowth(ii,5),durvie)
            end do
        endif
            !!!! this part need to be done when we have Nitrogen stress
    !           DO ii = hist_sencour, hist_latest
    !               if (namf > 0 .and. i > namf .and. P_codlainet == 2) then
    !                 if (inn > 1.) then
    !                   durviesup = P_durvieF * min(P_durviesupmax, (inn - 1.))
    !       !           write(*,*)n,'durviesup',P_durvieF,durviesup
    !                 else
    !                   durviesup = 0.
    !                 endif
    !               else
    !                 durviesup = 0.
    !               endif
    !               durvie(i) = durvie(i) + durviesup
    !           ENDDO
        
        !IF (somtemp >= durvie) THEN ! histgrowth(hist_seencour+1,5)
        IF (somtemp >= histgrowth(hist_sencour+1,5)) THEN ! histgrowth(hist_seencour+1,5)
        ! considering durvie(1..n-1) is smaller or equal to durvie(n),
        ! this is a conservative choice to start senescence a period of full
        ! senescense
        ! a radical one should be set as >= durvie(nsencour)
            IF (ndebsen ==0) THEN
                ndebsen = n
    !           dltamsen = histgrowth(hist_sencour, 2) * P_ratiosen *
    !           pfeuilverte(nsencour)
                !!! dltamsen needs to reconsidered since pfeuilverte is not yet
                !in  histgrowth
                IF (P_codlainet == 2) THEN
                    dltaisen = histgrowth(hist_sencour,1) ! + P_lai0
                    !!! initial lai will never be senescened because we do not
                    !know when it is initiated
                ENDIF
            ELSE
                ! somsen = somsenreste
                somsen = 0
                dltamsen = 0.
                IF (P_codlainet == 2) THEN
                    dltaisen = 0.
                ENDIF
                DO ii = hist_sencour+1, n
                    somsen = somsen + histgrowth(ii,3)
                ENDDO
                
                IF (somsen < histgrowth(hist_sencour+1, 5) ) THEN
                    dltamsen = 0.
                    IF (P_codlainet == 2) THEN
                        dltaisen = 0.
                    ENDIF
                ELSE
                    DO WHILE (somsen >= histgrowth(hist_sencour+1, 5) .and. hist_sencour+1 <= hist_latest)
                        hist_sencour = hist_sencour + 1
                        somsen = somsen - histgrowth(hist_sencour,3)!tdevelop(hist_sencour)
    !                   dltamsen = dltamsen + (histgrowth(hist_sencour, 2) *
    !                   P_ratiosen * pfeuilverte(nsencour))
                        !!! dltamsen needs to reconsidered since pfeuilverte is
                        !not yet in  histgrowth
                        IF (P_codlainet == 2) THEN
                            dltaisen = dltaisen + histgrowth(hist_sencour, 1)
                        ENDIF
                        somsenreste = somsen
                    ENDDO
                    ! somsenreste = somsen - histgrowth(hist_sencour,5)
                    !IF ( somsenreste<0 ) THEN
                    !    write(*,*) 'xuhui: somesenreste<0, likely a bug'
                    !    write(*,*) 'xuhui: somsen : ',somsen, 'durvie(hist_sencour) : ', histgrowth(hist_sencour,5)
                    !ENDIF
                ENDIF
            ENDIF   
        ENDIF

        IF (myprint) THEN
            write(*,*) 'xuhui: ---------------------'
            write(*,*) 'xuhui: day, ', n
            write(*,*) 'xuhui: hist_sencour, ',hist_sencour
            write(*,*) 'xuhui: hist_latest, ',hist_latest
            write(*,*) 'xuhui: dltaisen, ',dltaisen
            write(*,*) 'xuhui: dltai, ',histgrowth(hist_latest,1)
            write(*,*) 'xuhui: somtemp, ',somtemp
            write(*,*) 'xuhui: durvie, ',durvie
            write(*,*) 'xuhui: somsen, ',somsen
            write(*,*) 'xuhui: ---------------------'
        ENDIF
    elseif (P_codlainet==3) then
        IF (ulai <= P_vlaimax) THEN
            durage = durvieI
        ELSE
            durage = durvieI + ((ulai - P_vlaimax) / (3. -P_vlaimax)) * (P_durvieF - durvieI)
        ENDIF
        durvie = durage * min(senstress, fstressgel)
        do ii = 1, nbox
            if (boxndays(ii) == 0) then
                ! do nothing because of no leaf
            else
                if (boxdurage(ii) == 0.) then
                    boxdurage(ii) = durvie
                else
                    boxdurage(ii) = min(durvie, boxdurage(ii))
                endif
            endif
        enddo
        ! senescence condition
        dltaisen = 0.
        dltamsen = 0.
        do ii = 1, nbox
            if (boxndays(ii) > 0 .and. somtemp - boxsomsenbase(ii)  >= boxdurage(ii)) then !senescence part of this compartment
                if (ndebsen ==0) then
                    ndebsen = n
                    write(*,*) 'xuhui: debsen, ', ndebsen
                endif
!                dlairat = boxlai(ii)/real(boxndays(ii))
!                damsrat = boxbiom(ii)/real(boxndays(ii))
                dtdevrat = boxtdev(ii)/real(boxndays(ii))
                
                tempdays = (somtemp - boxsomsenbase(ii) - boxdurage(ii)) / dtdevrat
                if (boxlairem(ii) >= 0.) then ! we have at least sth to be senesced
                    if (tempdays >= boxndays(ii)) then
                        templaisen = boxlai(ii)
                    else
                        templaisen = tempdays/real(boxndays(ii))*boxlai(ii)
                    endif
                    if (templaisen + boxlairem(ii) - boxlai(ii) > 0.) then
                        dltaisen = dltaisen + templaisen + boxlairem(ii) - boxlai(ii)
                        boxlairem(ii) = boxlairem(ii) - dltaisen
                    else
                        dltaisen = dltaisen + 0.
                    endif
                endif
                if (boxbiomrem(ii) >= 0.) then
                    if (tempdays >= boxndays(ii)) then 
                        !leaf:biomass ratio (feuilverte) also change with time, 
                        ! considering including a compartment feuilverte in
                        ! later version
                        tempamsen = boxbiom(ii) * P_ratiosen * nsenpfeuilverte 
                    else
                        tempamsen = tempdays/real(boxndays(ii)) * boxbiom(ii) * P_ratiosen * nsenpfeuilverte
                    endif
                    if (tempamsen + (boxbiomrem(ii) - boxbiom(ii)) * P_ratiosen * nsenpfeuilverte > 0.) then
                        dltamsen = dltamsen + tempamsen + (boxbiomrem(ii) - boxbiom(ii)) * P_ratiosen * nsenpfeuilverte
                    else
                        dltamsen = dltamsen + 0.
                    endif
                endif
            endif
        enddo

        IF (myprint) THEN
            write(*,*) 'xuhui: ---------------------'
            write(*,*) 'xuhui: day, ', n
            write(*,*) 'xuhui: ulai, ',ulai
            write(*,*) 'xuhui: dltai, ',deltai
            write(*,*) 'xuhui: dltaisen, ',dltaisen
            write(*,*) 'xuhui: somtemp, ',somtemp
            write(*,*) 'xuhui: boxndays, ',boxndays
            write(*,*) 'xuhui: boxdurage, ',boxdurage
            write(*,*) 'xuhui: boxlai, ',boxlai
            write(*,*) 'xuhui: boxtdev, ',boxtdev
            write(*,*) 'xuhui: boxlairem, ',boxlairem
            write(*,*) 'xuhui: boxsomsenbase, ', boxsomsenbase
    !        write(*,*) 'xuhui: somsen, ',somsen
            write(*,*) 'xuhui: ---------------------'
        ENDIF
    endif
    
30        continue
      ! **      msresjaune   = matière sénescente résiduelle
      ! *-      msneojaune   = matière sénescente néoformée (limitée au végétatif)
      ! *-      mafeuiljaune = matière sénescente cumulée
      ! *- Hyp: msneojaune   = mafeuiljaune
      
      !msresjaune = msresjaune + dltamsres

      ! PB&Inaki - 08/03/2005 : On enlève les feuilles tombées de la matière jaune
      
      ! the purpose of dltamstombe is for recycling of biomass, but at this moment we do not consider this process---xcw
      
      !mafeuiljaune = mafeuiljaune + dltamsen - dltamstombe

      mafeuiljaune = mafeuiljaune + dltamsen
      msneojaune = mafeuiljaune

! Storage of the laisen information, assignment of the pervious day laisen
      
      pdlaisen = laisen

return
end subroutine senescen
 
 
