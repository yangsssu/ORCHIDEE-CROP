










!    *****************************
!     calcul de l'indice foliaire
!           N.Brisson, R. Roche
!    *****************************
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> calculation of the LAI
!> - Stics book paragraphe 3.1.1, page 40-44
!>
!! In STICS, leaf area growth is driven by phasic development, temperature and stresses. An empirical plant density-dependent function represents inter-plant
!! competition. For indeterminate plants, trophic competition is taken into account through a trophic stress index, while for determinate plants a maximal
!! expansion rate threshold is calculated to avoid unrealistic leaf expansion.
!! The calculation of leaf growth rate (deltai in m2m-2 d-1) is broken down: a first calculation of the LAI growth rate (in m2 plant-1 degree-day-1) describes
!! a logistic curve, related to the ILEV, IAMF and ILAX phenological stages. This value is then multiplied by the effective crop temperature, the plant density
!! combined with a density factor, supposed to stand for inter-plant competition, that is characteristic for the variety, and the water and nitrogen stress indices.
!!
!! The phasic development function is comparable to that of the model PUTU (Singels and Jagger, 1991), i.e. a logistic function with dlaimaxbrut as asymptote
!! and pentlaimax as the slope at the inflection point. It is driven by a normalized leaf development unit (ULAI) equal to 1 at ILEV and 3 at ILAX.
!! At the end of the juvenile stage (IAMF), it is equal to vlaimax when the inflection of the dynamics (point of maximal rate) also occurs.
!! Between the stages ILEV, IAMF and ILAX, the model performs linear interpolation based on development units (upvt) which include all the environmental effects
!! on phasic development. As the ILAX stage approaches, it is possible to introduce a gradual decline in growth rate using the udlaimax parameter
!!- the ULAI value beyond which there is a decline in the leaf growth rate. If udlaimax=3 it has no effect and the leaf stops growing at once at ILAX.
!!
!! The thermal function relies on crop temperature and cardinal temperatures (tcmin and tcmax) which differ from the ones used for the phasic development.
!! The extreme threshold tcxstop is the same as for development.
!!
!! The density function is active solely after a given LAI threshold occurs (laicomp parameter) if the plant density (densite in plant m-2 possibly decreased
!! by early frost) is greater than the bdens threshold, below which plant leaf area is assumed independent of density.  Beyond this density value, leaf area
!! per plant decreases exponentially.  The adens parameter represents the ability of a plant to withstand increasing densities.  It depends on the species
!! and may depend on the variety.  For branching or tillering plants, adens represents the plant’s branching or tillering ability (e. g. wheat or pea).
!! For single-stem plants, adens represents competition between plant leaves within a given stand (e.g. maize or sunflower).
!!
!! Water and nitrogen affect leaf growth as limiting factors, i.e. stress indices whose values vary between 0 and 1. Water (turfac) and nitrogen deficits (innlai)
!! are assumed to interact, justifying the use of the more severe of the two stresses. Meanwhile at the whole plant level the water-logging stress index is assumed
!! to act independently
!!
!!
!!
!> -    Features of determinate crops
!!   Failure to account for trophic aspects in the calculation of leaf growth may cause problems when the radiation intercepted by the crop is insufficient to
!!   ensure leaf expansion (e.g. for crops under a tree canopy or crops growing in winter).  Consequently, from the IAMF stage, we have introduced a trophic effect
!!  to calculate the definitive LAI growth rate in the form of a maximum threshold for leaf expansion (deltaimaxi in m2m-2d-1) using the notion of the maximum
!!   leaf expansion allowed per unit of biomass accumulated in the plant (sbvmax in cm2 g-1) and the daily biomass accumulation (dltams in t.ha-1day-1 possibly
!!   complemented by remobilized reserve remobilj). sbvmax is calculated using the slamax and tigefeuil parameters.
!!
!> -    Features of indeterminate crops
!!   It has been possible to test the robustness of the above formalisation on a variety of crops, including crops where there is an overlap between the
!!   vegetative phase and the reproductive phase (soybean and flax for example).  However, when trophic competition between leaves and fruits is a driving force
!!   for the production and management of the crop (for example tomato, sugarbeet), this formalisation is unsuitable.  We therefore calculate the deltai variable
!!   so as to take trophic monitoring into consideration in the case of crops described as ‘indeterminate’, by introducing a trophic stress index (splai).
!!   As a consequence, the LAI can decrease markedly during the theoretical growth phase if the crop is experiencing severe stresses during the harvested
!!   organs filling phase.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c



subroutine calai_(turfac,                                                                     & ! IN
                  udevair,udevcult,n,                                                         & ! IN
                  nplt,nlev,namf,nlax,ndrp,nrec,upvt,upvtutil,somcour,somcourutp,             & ! IN
                  densite,tcult,                                                              & ! IN
                  fstressgel,dltamsen,sla,                                                    & ! IN
                  dltams, densiteequiv, bsenlai,                                              & ! IN
                  pdlaisen, lai, pdlai, pdulai, somfeuille, nbfeuille, nsen, nlan,            & ! INOUT
                  reajust,  ulai, vmax,                                                       & ! INOUT
                  dltaisenat, laisen, dltaisen, R_stsenlan,                                   & ! INOUT
                  tustress, efdensite, nstopfeuille, deltai, R_stlaxsen, tempeff, &
                  histgrowth, hist_sencour, hist_latest, doyhistst) !, &
!                  nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, &
!                  boxbiom,boxbiomrem, boxdurage, boxsomsenbase)

USE Stics
USE Besoins_en_froid
implicit none

! 0 DECLARATION

! 0.1 INPUTS

!integer, intent(IN)    :: nbjmax !> la taille des tableaux journaliers  
!integer, intent(IN)    :: P_codeinnact  !> // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
!integer, intent(IN)    :: P_codeh2oact  !> // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0 
!integer, intent(IN)    :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
!integer, intent(IN)    :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
!integer, intent(IN)    :: P_codcueille  !> // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0 
!integer, intent(IN)    :: P_codlainet  !> // PARAMETER // option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
!integer, intent(IN)    :: codeulaivernal  !>// the value is either 0 (there is not a vernalization) or 1 (there is an effect of vernalization).
!integer, intent(IN)    :: P_codeindetermin  !> // PARAMETER // option of  simulation of the leaf growth and fruit growth : indeterminate (2) or determinate (1) // code 1/2 // PARPLT // 0 
!integer, intent(IN)    :: P_codeinitprec  !> // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0 
!integer, intent(IN)    :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
!integer, intent(IN)    :: P_codetemp  !> // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0 
real,    intent(IN)    :: turfac   !> // OUTPUT // Index of turgescence water stress  // 0-1
!real,    intent(IN)    :: innlai   !> // OUTPUT // Index of nitrogen stress active on leaf growth // P_innmin à 1
!real,    intent(IN)    :: P_laiplantule  !> // PARAMETER // Plantlet Leaf index at the plantation // m2 leaf  m-2 soil // PARPLT // 1 
!real,    intent(IN)    :: P_phyllotherme  !> // PARAMETER // thermal duration between the apparition of two successive leaves on the main stem // degree C day // PARPLT // 1
real,    intent(IN)    :: udevair   !> // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
real,    intent(IN)    :: udevcult   !> // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
!real,    intent(IN)    :: P_dlaimaxbrut  !> // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1 
!real,    intent(IN)    :: P_udlaimax  !> // PARAMETER // ulai from which the rate of leaf growth decreases  // SD // PARPLT // 1 
integer, intent(IN)    :: n  
!integer, intent(IN)    :: numcult  
integer, intent(IN)    :: nplt  
integer, intent(IN)    :: nlev  
integer, intent(IN)    :: namf  
integer, intent(IN)    :: nlax  
integer, intent(IN)    :: ndrp  
integer, intent(IN)    :: nrec  
real,    intent(IN)    :: upvt   !> // OUTPUT // Daily development unit  // degree.days (calculated in stics_diverse.f90)
real,    intent(IN)    :: upvtutil  ! (calculated in stics_develop.f90)
!real,    intent(IN)    :: P_vlaimax  !> // PARAMETER // ULAI  at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
!real,    intent(IN)    :: P_laicomp  !> // PARAMETER // LAI from which starts competition inbetween plants // m2 m-2 // PARPLT // 1 
real,    intent(IN)    :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
real,    intent(IN)    :: somcourutp  
!real,    intent(IN)    :: P_stlevamf  !> // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1 
!real,    intent(IN)    :: P_stamflax  !> // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1 
real,    intent(IN)    :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
!real,    intent(IN)    :: P_adens  !> // PARAMETER // Interplant competition parameter // SD // PARPLT // 1 
!real,    intent(IN)    :: P_bdens  !> // PARAMETER // minimal density from which interplant competition starts // plants m-2 // PARPLT // 1 
!real,    intent(IN)    :: P_tcxstop  !> // PARAMETER // threshold temperature beyond which the foliar growth stops // degree C // PARPLT // 1
real,    intent(IN)    :: tcult   !> // OUTPUT // Crop surface temperature (daily average) // degree C
!real,    intent(IN)    :: P_tcmin  !> // PARAMETER // Minimum temperature of growth // degree C // PARPLT // 1
!real,    intent(IN)    :: P_tcmax  !> // PARAMETER // Maximum temperature of growth // degree C // PARPLT // 1
!real,    intent(IN)    :: P_pentlaimax  !> // PARAMETER // parameter of the logistic curve of LAI growth  // SD // PARPLT // 1 
!real,    intent(IN)    :: P_dlaimin  !> // PARAMETER // accelerating parameter for the lai growth rate // SD // PARAMV6/PLT // 1 
!real,    intent(IN)    :: exolai   !> // OUTPUT // Index for excess water active on growth in biomass // 0-1   ! at this moment, we do not consider the effects of water logging on LAI
!real,    intent(IN)    :: P_splaimin  !> // PARAMETER // Minimal value of ratio sources/sinks for the leaf growth  // between 0 and 1 // PARPLT // 1 
!real,    intent(IN)    :: P_splaimax  !> // PARAMETER // maximal sources/sinks value allowing the trophic stress calculation for leaf growing // SD // PARPLT // 1 
!real,    intent(IN)    :: P_slamin  !> // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1 
!real,    intent(IN)    :: P_slamax  !> // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1 
!real,    intent(IN)    :: P_tigefeuil  !> // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1 
!real,    intent(IN)    :: P_tustressmin  !> // PARAMETER //  threshold stress (min(turfac,inns)) under which there is an effect on the LAI (supplementary senescence by ratio at the natural senescence) 
real,    intent(IN)    :: fstressgel   !> // OUTPUT // Frost index on the LAI // 0-1
real,    intent(IN)    :: dltamsen   !> // OUTPUT // Senescence rate // t ha-1 j-1
real,    intent(IN)    :: sla   !> // OUTPUT // Specific surface area // cm2 g-1
real,    intent(IN)    :: dltams                ! (n-1)    // OUTPUT // Growth rate of the plant  // t ha-1.j-1
real,    intent(IN)    :: densiteequiv  !densite equivalente calculée chaque jour
real,    intent(IN)    :: bsenlai      ! lai value at date nsen, when the sen begin 

! 0.2 INOUT

real,    intent(INOUT) :: pdlaisen          ! senescence leaf area index of the previous day // Leaf area index (table) // m2 leafs  m-2 soil// 
real,    intent(INOUT) :: lai            ! (n), (n-1), (nsen), (nlan)    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil// in this version, there is no time dimension
real,    intent(INOUT) :: pdlai          ! leaf area index of the previous day // Leaf area index (table) // m2 leafs  m-2 soil// 
real,    intent(INOUT) :: pdulai          ! leaf area index of the previous day // Leaf area index (table) // m2 leafs  m-2 soil// 
real,    intent(INOUT) :: somfeuille  
integer, intent(INOUT) :: nbfeuille   !> // OUTPUT // Number of leaves on main stem // SD
integer, intent(INOUT) :: nsen  
integer, intent(INOUT) :: nlan  
!real,    intent(INOUT) :: P_dlaimax  !> // PARAMETER // Maximum rate of the setting up of LAI // m2 leaf plant-1 degree d-1 // PARPLT // 1 
real,    intent(INOUT) :: reajust  
real,    intent(INOUT) :: ulai         !> // Daily relative development unit for LAI // 0-3
real,    intent(INOUT) :: vmax  
real,    intent(INOUT) :: dltaisenat  
real,    intent(INOUT) :: laisen        ! (n), (n-1)    // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
real,    intent(INOUT) :: dltaisen   !> // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
real,    intent(INOUT) :: R_stsenlan  !> // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1 

  real, dimension(300,5), intent(INOUT) :: histgrowth
  integer, intent(INOUT)                :: hist_sencour
  integer, intent(INOUT)                :: hist_latest
  integer, intent(INOUT)                :: doyhistst

! 0.3 OUT

real,    intent(OUT)   :: tustress   !> // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
real,    intent(OUT)   :: efdensite  
integer, intent(OUT)   :: nstopfeuille  
real,    intent(OUT)   :: deltai                ! (n)    --(0:nbjmax)    // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
!real,    intent(OUT)   :: splai   !> // OUTPUT // Pool/sink ratio applied to leaves // 0-1
!real,    intent(OUT)   :: fpv   !> // OUTPUT // Sink strength of developing leaves // g.j-1.m-2
real,    intent(OUT)   :: R_stlaxsen  !> // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1 
real,    intent(OUT)   :: tempeff   !> // OUTPUT // Efficient temperature for growth // degree C

! 0.4 LOCAL VARIABLES
real :: deltaimaxi  
real :: sbvmin  
real :: sbvmax 

real :: tdevelop 
!integer :: ii

!: 0. Assignments of the historical variables
!---------------------------------------------------------------
    
    ! assignment of the historical variables
    pdlai = lai
    pdulai = ulai
    pdlaisen = laisen


!: 1. CALCULATION THE STRESS FACTOR (no problem)
!--------------------------------------

    if (P_codeinnact == 1 .and. P_codeh2oact == 1) then
      tustress = min(turfac,P_innlai)
    endif
    if (P_codeinnact == 2 .and. P_codeh2oact == 1) then
      tustress = min(turfac,1.0)
    endif
    if (P_codeinnact == 1 .and. P_codeh2oact == 2) then
      tustress = min(1.0,P_innlai)
    endif
    if (P_codeinnact == 2 .and. P_codeh2oact == 2) then
      tustress = 1.
    endif

!print *, 'wu: in calai process the tustress is', tustress

!: 2. INITIALIZATIONS OF SOME VARIABLES BEFORE EMERGENCE (done)
!-------------------------------------------------

!: Dans le cas des cultures annuelles (P_codeperenne=1), trois cas:
!
!- a) pour les cultures qui se plantent (P_codehypo=2) et admettent un
!-    temps de latence avant le demarrage de la croissance (P_codegermin=1):
!-    initialisation du lai a P_laiplantule a partir de la plantation;
!-    et calcul de QNPlante par courbe de dilution
!-
!- b) pour les cultures qui se sement (P_codehypo=1):
!-    initialisation du lai a 0 a partir du semis
!-
!- c) pour les cultures qui se plantent (P_codehypo=2) et demarrent directement (P_codegermin=2):
!-    initialisation du lai a P_laiplantule a partir de la levee qui est aussi la plantation
!-------------------------------------------------
! in the case of annual crop, 3 cases:
! 
! a) for crops that are planted (P_codehypo = 2) and admit a latency before startup growth (P_codegermin = 1):
!    initialization lai P_laiplantule from planting; and calculating by dilution curve QNPlante
!
! b) for crops that are sowed  (P_codehypo = 1): initialization lai 0 from sowing
!
! c) for crops that are planted (P_codehypo = 2) and been booted directly (P_codegermin = 2):
!    initialization lai P_laiplantule from the levee
!------------------------------------------------


!    print *, 'dltams before calculate the lai is', dltams
!    print *, 'in calai the nsen is,', nsen

    if (nlev == 0) then

      if (P_codeperenne == 1 .and. n >= nplt) then  !- NPLT étant non nul dès le depart de la simulation, on test n>=nplt
        if (P_codehypo == 2 .and. P_codegermin == 1) then ! a)
          !lai(n) = P_laiplantule
          lai = P_laiplantule
          return
        else ! b)
          lai = 0.
          !lai(n) = 0.
          return
        endif
      else
        !lai(n) = 0.0
        lai = 0.0
        return
        !: TODO - le somfeuille=0 après le return est inutile, il n'est jamais exécuté. BUG ?
        somfeuille = 0.
      endif
    endif

    ! le jour de la levée, si plantation par plantule, on force le lai du jour précédent
    if (n == nlev .and. P_codehypo == 2 .and. namf == 0) then
      !lai(n-1) = P_laiplantule
      pdlai = P_laiplantule   ! // pdlai lai of previous day
    endif

    !: à la récolte (nrec+1)
    !: 
    if (nrec > 0 .and. n > nrec) then
      ! En cas de moisson, on enlève les feuilles.
      if (P_codcueille == 1) then ! havest the whole plant (1) or only the fruit (2)
        !lai(n) = 0.
        lai = 0.
        somfeuille = 0.
        return
      endif
    endif

    !: Calcul du nombre de feuilles
    !: calculation of number of leaves
    if (P_codetemp == 2) then ! p_codetemp == 2 indicate that we use the crop temperature
      call CalculNombreDeFeuilles(nlax,udevcult,somfeuille,nbfeuille)
    else
      call CalculNombreDeFeuilles(nlax,udevair,somfeuille,nbfeuille)
    endif

!    print *, 'wu: in calai process the nlev, ndrp, nlax, nsen are', nlev, ndrp, nlax, nsen
!    print *, 'wu: in calai process the nbfeuille is', nbfeuille

    !: si option LAI brut+ sénescence
    !if (P_codlainet == 2) P_dlaimax = P_dlaimaxbrut

    !: 2a) calcul de reajust : distance de développement pour réajuster
    !-     les parcours observés

    ! reinitialization of the sum of upvt
    if (n == nlev) reajust = 0.0
    if (n == namf) reajust = 0.0
    if (n == nlax) reajust = 0.0
    if (n == nsen) reajust = 0.0
    if (n == nlan) reajust = 0.0

    reajust = reajust + upvt - upvtutil 
   
!    print *, 'wu: in calai process the reajust is', reajust
    

    !: 2b) lai de départ non nul pour les cultures qui se plantent
    !--      if (n == nlev .and. P_codehypo == 2 .and. namf == 0) then
    !--        lai(n-1) = P_laiplantule
    !--!: NB - le 21/04 - sbv remplacé par slavert
    !--        ms(n-1) = lai(n-1)/P_slamin*100.0
    !--!: NB - ajout de P_QNplante0 - le 21/04
    !--        QNplante = (P_adilmax * ms(n-1) * 10) + P_QNplante0
    !--        QNplantetot = QNplante
    !--        inns =  1.0
    !--        inn  =  1.0
    !--        innlai  =  1.0
    !--      endif


!: 3. CALCUL DE ULAI
!-------------------
    !: 3a) calcul de ulai entre lev et amf
    !- domi - 29/03 la vernalisation ne joue pas sur ulai si codeulaivernal = 1
    if (nlev > 0 .and. namf == 0) then
      if (codeulaivernal == 1) then
        !ulai(n) = 1 + (P_vlaimax - 1) * (somcour + reajust) / (P_stlevamf + reajust)
        ulai = 1 + (P_vlaimax - 1) * (somcour + reajust) / (P_stlevamf + reajust)
      else
        if (somcourutp <= P_stlevamf) then
!          print *, 'in lai the somcourutp is,', somcourutp
          !ulai(n) = 1 + (P_vlaimax - 1) * (somcourutp + reajust) / (P_stlevamf + reajust)
          ulai = 1 + (P_vlaimax - 1) * (somcourutp + reajust) / (P_stlevamf + reajust)
        else
          !ulai(n) = ulai(n-1)
          ulai = pdulai
        endif
      endif
    endif
  

!    print *, 'wu: in calai process the ulai is', ulai

    !: 3b) calcul de ulai entre amf et lax
   
    if (namf > 0 .and. nlax == 0) then
      
      if (codeulaivernal == 1) then
        !ulai(n) = P_vlaimax + (3 - P_vlaimax) * (somcour + reajust) / (P_stamflax + reajust)
        ulai = P_vlaimax + (3 - P_vlaimax) * (somcour + reajust) / (P_stamflax + reajust)
      else
        if (somcourutp <= P_stamflax) then
          !ulai(n) = P_vlaimax + (3 - P_vlaimax) * (somcourutp + reajust) / (P_stamflax + reajust)
          ulai = P_vlaimax + (3 - P_vlaimax) * (somcourutp + reajust) / (P_stamflax + reajust)         
 
        else
          !ulai(n) = ulai(n-1)
          ulai = pdulai

        endif
      endif

    endif

!    print *, 'wu: in calai process the ulai is', ulai

    !: 3c) ulai maximal atteint à drp pour les indéterminées
    if (P_codeindetermin == 2 .and. ndrp > 0) then
      !ulai(n) = P_udlaimax
      ulai = P_udlaimax
    else
      if (nlax > 0) ulai = P_udlaimax     !ulai(n) = P_udlaimax
    endif

   

!: 4. CALCUL du DELTAI BRUT
!--------------------------

    !: 4a) calcul du facteur densité efdensite actif à partir de P_laicomp
    !- calcul de l'effet densité sur la mise en place du LAI pour les stics-plante

    !: calculation of the effective density, to address the density effects on LAI development

    efdensite = 1.
    !if (ulai(n) > 1.) then
    if (ulai > 1.) then 
      !if (lai(n-1) < P_laicomp) then
      if (pdlai < P_laicomp) then
        efdensite = 1.
      else
        if (densite == 0) then
          efdensite = 0.0
        else
          ! domi - 02/07/2002 - pb si gel total densite  = 0 et pb de log(0)
          ! efdensite = min(1.0,(exp(P_adens * (log(densite / P_bdens)))))
          ! DR 12/09/2012 on prend la densite equivalente pour calculer l'effet densite et uniquement pour ca !
          efdensite = min(1.0,(exp(P_adens * (log(densiteequiv / P_bdens)))))
        endif
      endif
    else
      ! domi - 02/07/2002 - pb si gel total densite  = 0 et pb de log(0)
      if (densite == 0) then
        efdensite = 0.0
      else
        !efdensite = min(1.0,(exp(P_adens * log(densite / P_bdens))))
        !DR 12/09/2012 on prend la densite equivalente pour calculer l'effet densite et uniquement pour ca !
        efdensite = min(1.0,(exp(P_adens * log(densiteequiv / P_bdens))))
      endif
    endif

    !if (ulai(n) == 1.0) efdensite = 1
    if (ulai == 1.0) efdensite = 1.

!    print *, 'wu: in calai process the efdensite is', efdensite

    ! ** 4b) température efficace de croissance: limitation par P_tcmax
    ! NB le 13/06/06
    ! introduction d'une température maximale d'arrêt de croissance
    ! foliaire P_Tcxstop à l'occasion étude Françoise sur fourrages méditerranéens
    !      P_tcxstop = 30.0

    !: addressing the effective temperature


    if (P_tcxstop >= 100.0) then
      if (tcult > P_tcmax) then
        tempeff = max(0.,(P_tcmax - P_tcmin))
      else
        tempeff = max(0.,(tcult - P_tcmin))
      endif
    else
      if (tcult > P_tcmax) then
        tempeff = (P_tcmax - P_tcmin) / (-P_tcxstop + P_tcmax) * (tcult - P_tcxstop)
        tempeff = max(0.,tempeff)
      else
        tempeff = max(0.,(tcult - P_tcmin))
      endif
    endif

!    print *, 'wu: in calai process the tempeff is', tempeff

    !: 4c) calcul du deltai
    !- quelle que soit l'option LAI choisie, LAI net ou LAI brut :
    !-  * la croissance s'arrête à LAX pour les déterminées
    !-  * la croissance s'arrête à SEN pour les indéterminées
    if (P_codeindetermin == 1) nstopfeuille = nlax
    if (P_codeindetermin == 2) nstopfeuille = nsen

    if (P_codlainet == 2) nstopfeuille = nlax
    if (P_codlainet == 3) nstopfeuille = nlax

!    print *, 'wu: in calai process the nstopfeuille is', nstopfeuille 
!    print *, 'wu: in calai process the densite is', densite
    if ( nstopfeuille .eq. 0 ) then
 
!    print *, 'wu: in calai process, do we go in? if so the ulai is', ulai

      !: modif le 23/07/01
      !- entre ulai = 1 et ulai = P_udlaimax, la vitesse est croissante
      !- NB - le 13/12

      !if (ulai(n) <= P_udlaimax .or. P_udlaimax == 3.0) then
      if (ulai <= P_udlaimax .or. P_udlaimax == 3.0) then

        ! DR 07/03/08 integration des modifs du 28/02/07 P_dlaimax n'etait actif
        ! et introduction de P_codedlaimin
        ! NB et FR le 28/02/07
        ! introduction d'une ordonnée à l'origine pour la croissance des feuilles
        ! DR on vire le P_codedlaimin et on met P_dlaimin dans le fichier plante
  
        deltai = P_dlaimax * efdensite * densite &
                  * (P_dlaimin + (1.0 - P_dlaimin) / (1 + exp(P_pentlaimax * (P_vlaimax - ulai))))
        vmax = deltai
      else
        !: entre P_udlaimax = 1 et ulai = 3(lax), la vitesse est décroissante
        !- sauf pour les indéterminées
        ! DR 24/01/2011 on a un pb dans l'equation la puissance multiplie tout (eq 3.2 bouquin)

        !deltai = vmax * (1 - (ulai(n) - P_udlaimax) / (3.0 - P_udlaimax))**2
        deltai = vmax * (1 - (ulai - P_udlaimax) / (3.0 - P_udlaimax))**2

      endif




      !: NB - le 28/05 - on lève toujours
      !- 10/01 - ajout d'un test sur l'enchainement des perennes
      ! TODO: voir si on pourrait remplacer ce test par un booléen qui dirait si la plante a levé ou pas. ex: *if (isLevee) then*

      !if (n > nlev .or. (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1)) then
      if (nlev > 0 .or. (P_codeperenne == 2 .and. P_codeinitprec == 2)) then

        !: NB le 07/06 introduction de exolai
        !: At this moment, we do not consider the limiting effects of exolai--xcwu
 
        deltai = deltai * tempeff * tustress * exolai

      endif

      !: la croissance foliaire des plantes indéterminées dépend
      !- du rapport source/puits des organes végétatifs

      ! At this moment, we do not consider the perennial crop or indeterminant crops---xcw

      !if (P_codeindetermin == 2) then

      !  splai = (sourcepuits - P_splaimin) / (P_splaimax - P_splaimin)

      !  splai = max(splai,0.0)
      !  splai = min(splai,1.0)

      !  ! NB le 21/04 recalcul d'un sbv approprié
      !  sbvmin = P_slamin / (1.0 + P_tigefeuil)
      !  fpv = deltai * splai * 1e4 / sbvmin
      !  deltai  =  deltai*splai

      !else
      
      if (P_codeindetermin == 1) then       ! determinant crop case
  
        !: limitation si le rayonnement est vraiment insuffisant
        !- après AMF (cas des cultures associées)
        !- NB le 21/04 recalcul d'un sbv approprié
        !- NB le 15/05/02 calcul de fpv pour les pérennes

        sbvmin = P_slamin / (1.0 + P_tigefeuil)
        !fpv = deltai * 1e4 / sbvmin
        sbvmax = P_slamax / (1.0 + P_tigefeuil)

        !: ajout des remobilisations possible pour faire redémarrer le
        !- lai en cas de couvert complètement sec
        !- NB le 13/06/06
        !- NB le 27/06/06 attention delatremobil était déjà compté dans dltams
        
        ! at this moment, we do not consider the remobilization,
        ! we adjust the process
        ! original process as following:
        ! deltaimaxi = (dltams + remobilj) * sbvmax / 100.
        ! adjusted processes as following:

        deltaimaxi = dltams * sbvmax / 100. ! unit in m2 m-2
    
!        print *, 'in calai the dltams and sbvmax and deltaimaxi is:', dltams, sbvmax, deltaimaxi
!        print *, 'in calai the dltams and sbvmin and deltaimini is:', dltams * sbvmin / 100.    
        if (namf > 0) then
          deltai = min(deltai,deltaimaxi)
          !deltai = deltai   ! test
        endif
      endif
      ! fin nstopfeuille = 0
    else
      deltai = 0.0
    endif


!    print *, 'xuhui: n ', n
!    print *, 'xuhui: nlev ', nlev
! xuhui: record the historical growth of lai for calculation of senescence
    if (P_codlainet==2) then 
        if (nlev > 0) then ! leaf growth have initiated
            if (doyhistst == 0 .and. hist_sencour == 0) then ! history has not been intialized
                doyhistst = n
                hist_sencour = 1
                hist_latest = 1
!                print *, 'xuhui: histgrowth initiated'
            else  ! history already initialized
                hist_latest = hist_latest + 1
!                print *, 'xuhui: histlatest: ',hist_latest
            endif
    !        histgrowth(hist_latest,6) = n
            histgrowth(hist_latest,1) = deltai
            if (hist_latest>1) then
                histgrowth(hist_latest-1,2) = dltams  !! dltams in this routin is from yesterday
            endif
            if (P_codetemp == 2) then
                tdevelop = 2.0 ** (udevcult / 10.)
            else
                tdevelop = 2.0 ** (udevair / 10.)
            endif
            ! according to STICS book, tdevelop should be 2.0 **
            ! (udevcult*(stressdev*min(turfac, innlai) + 1 - stressdev)/10.)
            ! however, the code of v6.0 did not have this process, considering
            ! adding it into the calculation
            histgrowth(hist_latest,3) = tdevelop
            histgrowth(hist_latest,4) = ulai
            ! histgrowth(:,5) is durvie, added in the senescence subroutine
!            print *, 'xuhui: histgrowth in ',hist_latest, ': ', histgrowth(hist_latest,:)
        endif
    elseif (P_codlainet==3) then
!        if (ulai < 1 .or. ulai > 3) then
!            write(*,*) 'xuhui: ulai: ', ulai, ', likely a bug'
!        else 
!            ii = nbox
!            do while (ulai < ulaibox(ii))
!                ii = ii - 1
!            enddo
!            if (ii < 1) then
!                write(*,*) 'xuhui: bug: ii<1'
!                ii = 1
!            endif 
!            boxndays(ii) = boxndays(ii) + 1
!            boxlai(ii) = boxlai(ii) + deltai
!            boxlairem(ii) = boxlairem(ii) + deltai
!            boxbiom(ii) = boxbiom(ii) + dltams
!            boxbiomrem(ii) = boxbiomrem(ii) + dltams
!            if (P_codetemp == 2) then
!                tdevelop = 2.0 ** (udevcult / 10.)
!            else
!                tdevelop = 2.0 ** (udevair / 10.)
!            endif
!            boxtdev(ii) = boxtdev(ii) + tdevelop
!        endif
    endif
! end xuhui

!: 5. PHASE DE PLATEAU  (si option lainet et culture determinée)
!---------------------------------------------------------------

    if (P_codlainet == 1 .and. P_codeindetermin == 1) then
      if (nlax > 0 .and. nsen == 0) deltai = 0.0
    endif

!: 6. Calcul de la senescence rapide par ajustement a la récolte
!- entre sen et lan  pour les options 1 et 2
!: calculation the lai according to two options
!---------------------------------------------------------------

    if (P_codlainet <= 1) then ! the P_codlainet is either 1 or 2, so here it is the direct calculation of LAI

          !lai(n) =  lai(n-1) + deltai - dltaisenat  ! the calculation of dltaisenat is shown below
          lai = pdlai + deltai - dltaisenat
    

          !if (n == nsen) lai(n) = lai(n-1)
          if (n == nsen) lai = pdlai    ! in this date, the senescence is 0
  
          if (nsen > 0 .and. nlan == 0.) then

            !lai(n) = lai(nsen) * (1 - ((somcour+reajust) / (R_stsenlan+reajust)))  ! 
            lai = bsenlai * (1 - ((somcour+reajust) / (R_stsenlan+reajust + 80.0)))     ! there is decreasing lai, 80 is an offset
            
  
            !dltaisenat = lai(n-1) - lai(n)
            dltaisenat = pdlai - lai
            !if (lai(n) <= 0.0 .and. nlan == 0)  nlan = n
            if (lai <= 0.0 .and. nlan == 0) nlan = n
            !if (n == nlan) lai(nlan) = 0.0
            if (n == nlan) lai = 0.0    ! if n == nlan, lai(nlan) is the same as lai(n) , so lai = 0.0

          endif
     else
          !: option LAI brut la sénescence (dltaisen) est calculé dans le spg senescen
          !lai(n) = lai(n-1) + deltai - dltaisen  ! we do not consider the net calculation, so...
          lai = pdlai + deltai - dltaisen
     endif

    !if (lai(n) < 0.) then
    if (lai < 0.0) then
      !lai(n) = 0.
      lai = 0.0

      ! DR 06/09/06 y'a un soucis car alors laisen ne prend pas en compte la derniere valeur de lai
      !--dltaisen = 0.0
    endif

    ! NB - le 22/04 - à cause précisison dans les soustractions
    !if (lai(n) <= 1e-4 .and. nstopfeuille > 0) then
    !  lai(n) = 0.
    !endif

    if (lai <= 1e-4 .and. nstopfeuille > 0) then
       lai = 0.0
    endif

!: 7. senescence foliaire due à des stress  pour l'option LAInet
!: Addressing the senescence of lai, see senescen.f90 
!---------------------------------------------------------------

    !if (P_codlainet == 1 .and. lai(n) > 0.0) then
    if (P_codlainet == 1 .and. lai > 0.0) then
   
      if (tustress < P_tustressmin .or. tcult < P_tcmin .or. fstressgel < 1.0) then    ! there seems to be an additional part of senescence for LAI, representing by dltamsen/100*sla
        ! bug dans l'opération NB le 08/05

        dltaisen = dltamsen * sla * 100.   ! unit in m2 m-2
        

        ! how can we calculate the sla, it is calculated in biomass allocation processes; ---repartir.f90
        ! the calculation of sla is determined by the tursla. 

        !lai(n) = lai(n) - dltaisen
        lai = lai - dltaisen
        dltaisen = dltaisen + dltaisenat
        lai = max(lai, 0.0)
  
      else
        dltaisen = dltaisenat
      endif
    endif

    !laisen(n) = laisen(n-1) + dltaisen
    laisen = pdlaisen + dltaisen 


    !: NB le 22/04: calcul du stade lan pour P_codlainet = 2
    !if (lai(n) <= 0 .and. nlan == 0 .and. nlax > 0) then
    if (lai <= 0.0 .and. nlan == 0 .and. nlax > 0) then
      if (nsen == 0) then
        nsen = n
        R_stlaxsen = somcour

      endif
      nlan = n
      R_stsenlan = somcour
      !lai(n) = 0.0      ! 07/06
      lai = 0.0
      dltaisen = 0.0
      dltaisenat = 0.0 ! NB le 11/06
    endif
   

return
end subroutine calai_
