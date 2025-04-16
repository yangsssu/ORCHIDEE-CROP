!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!
!> Purpose of this subroutine: addressing the shoot/root partitioning <!
!> Author: Xiuchen Wu                                                 <!
!> Date  : 31/07/2013                                                 <!


subroutine reprac_calc(n, nsen, nlax, nflo, nmat, nrec, nplt, in_cycle, nger, tcult, nlev, &      ! IN
                     zrac, repracmax, repracmin, kreprac, somtemprac, urac, reprac, nstoprac, &     ! inout
                  P_stoprac, P_codeperenne, P_codehypo, P_codegermin,P_zracplantule, P_profsem, P_codtrophrac,  &
                  P_repracpermax, P_repracpermin, P_krepracperm, P_repracseumax, P_repracseumin, P_krepracseu,  &
                  P_codetemprac, P_tcmin, P_tcmax, P_codedyntalle, P_tcxstop, P_stlevamf, P_stamflax)     ! parameter

! USE Stics

! DECLARATION PART
!
! 0.0  INPUT  
!  

integer, intent(IN)                :: n
integer, intent(IN)                :: nsen
integer, intent(IN)                :: nlax
integer, intent(IN)                :: nflo
integer, intent(IN)                :: nmat
integer, intent(IN)                :: nrec
integer, intent(IN)                :: nplt
logical, intent(IN)                :: in_cycle
integer, intent(IN)                :: nger
real,    intent(IN)                :: tcult
integer, intent(IN)                :: nlev
character, intent(in)              :: P_stoprac !//parameter, stage when root stop growth
integer, intent(in)                :: P_codeperenne   ! // parameter
integer, intent(in)                :: P_codehypo   ! // parameter

integer, intent(in)                :: P_codegermin   ! // parameter
real,    intent(in)                :: P_zracplantule ! // parameter
real,    intent(in)                :: P_profsem      ! // parameter
integer, intent(in)                :: P_codtrophrac   ! // parameter

real,    intent(in)                :: P_repracpermax     ! // parameter
real,    intent(in)                :: P_repracpermin      ! // parameter
real,    intent(in)                :: P_krepracperm      ! // parameter

real,    intent(in)                :: P_repracseumax     ! // parameter
real,    intent(in)                :: P_repracseumin      ! // parameter
real,    intent(in)                :: P_krepracseu      ! // parameter

integer, intent(in)                :: P_codetemprac   ! // parameter
real,    intent(in)                :: P_tcmin      ! // parameter
real,    intent(in)                :: P_tcmax      ! // parameter
integer, intent(in)                :: P_codedyntalle   ! // parameter
real,    intent(in)                :: P_tcxstop      ! // parameter

real,    intent(in)                :: P_stlevamf      ! // parameter
real,    intent(in)                :: P_stamflax     ! // parameter



!
! 1.0  INOUT

real,    intent(INOUT)             :: zrac
real,    intent(INOUT)             :: repracmax
real,    intent(INOUT)             :: repracmin
real,    intent(INOUT)             :: kreprac
real,    intent(INOUT)             :: somtemprac
real,    intent(INOUT)             :: urac
real,    intent(INOUT)             :: reprac
real,    intent(INOUT)             :: nstoprac



! 2.0  OUT

! 3.0  LOCAL

real :: znonli
real :: dtj


! zrac should be initialized as 0
! when nger ~= 0, meaning that the germination occurs, we assign the zrac as P_profsem


     !>> FIRST 
     ! we determine the nstoprac, this variable is dependent on P_stoprac

     if (P_stoprac == 'sen' .or. P_stoprac == 'SEN') nstoprac = nsen
     if (P_stoprac == 'lax' .or. P_stoprac == 'LAX') nstoprac = nlax
     if (P_stoprac == 'flo' .or. P_stoprac == 'FLO') nstoprac = nflo
     if (P_stoprac == 'mat' .or. P_stoprac == 'MAT') nstoprac = nmat
     if (P_stoprac == 'rec' .or. P_stoprac == 'REC') nstoprac = nrec


     !>> SECOND
     ! adjustment of the root depth accoring to the stages 
     ! but note that in this version, we do not calculate the growth of root

     if (P_codeperenne == 1 .and. (n >= nplt .or. in_cycle == .true.)) then ! annual crop and in the growing cycle
       if (P_codehypo == 2 .and. P_codegermin == 1) then
         znonli = P_zracplantule + P_profsem
         zrac = znonli
         !return
       else
         znonli = P_profsem
         zrac = znonli
         !return
       endif
     else
       znonli = 0.
       zrac = znonli
     endif
     ! now we adjust the zrac 

     
     !>> THIRD
     ! we get the three parameters for calculating the REPRAC variable
     ! There are three cases:
     ! 1. codtrophrac is 1
     ! 2. codtrophrac is 2
     ! 3. codtrophrac is 3

     if (P_codtrophrac == 1) then 
       repracmax = P_repracpermax
       repracmin = P_repracpermin
       kreprac   = P_krepracperm
     endif
     if (P_codtrophrac == 2) then
       repracmax = P_repracseumax
       repracmin = P_repracseumin
       kreprac   = P_krepracseu
     endif
     if (P_codtrophrac == 3) then
       repracmax = 0.
       repracmin = 0.
       kreprac   = 0.
     endif

 
     !>> FOURTH
     ! if the germination did not occur, we do not need the shoot/root partitioning calculation, so the reprac keeps the initialization value
     if (nger == 0) return
    

     
     !>> FIFTH
     ! we calculate the reprac


     !if (n >= nger .and. nger > 0 .and. zrac > 0.) then
     if (in_cycle == .true. .and. nger > 0 .and. zrac > 0) then ! if in growing cycle, and already germinate and depth of root front is larger than 0
       if (P_codetemprac == 1) then ! using the crop temperature
         dtj = max(tcult - P_tcmin, 0.)
         dtj = min(dtj, P_tcmax - P_tcmin)

       ! DR et ML et SYL 15/06/09
       ! ************************
       ! introduction de la fin des modifications de Sylvain (nadine et FR)
       ! dans le cadre du projet PERMED
         if (P_codedyntalle == 1) then
       ! ####
       ! NB le 06/03/2008 introduction de l'effet négatif des températures élevées
       ! sur les fonctions racinaires qui utilisent "dtj"
           if (tcult > P_tcmax .and. P_tcxstop < 100) then
             dtj = (P_tcmax-P_tcmin)/(-P_tcxstop+P_tcmax)*(tcult-P_tcxstop)
             dtj = max(dtj,0.)
           endif
         endif
       ! ####

       !else ! using the soil temperature
       !  dtj(n) = max(tsol(int(zrac)) - P_tgmin, 0.)
       !  dtj(n) = min(dtj(n) ,P_tcmax - P_tgmin)

       !! ####
       !! NB le 06/03/2008 introduction de l'effet négatif des températures élevées
       !! sur les fonctions racinaires qui utilisent "dtj"
       !  if(P_codedyntalle == 1)then
       !    if (tsol(int(zrac)) > P_tcmax .and. P_tcxstop < 100) then
       !      dtj(n) = (P_tcmax-P_tcmin)/(-P_tcxstop+P_tcmax)*(tsol(int(zrac))-P_tcxstop)
       !      dtj(n) = max(dtj(n),0.)
       !    endif
       !  endif
       !! ####

       endif
     else 
       dtj = 0.
     endif

      ! addressing the cumulated effective temperature of root


      somtemprac = somtemprac + dtj
      if (n == nlev) somtemprac = 0.
      if (nstoprac == 0) then
        !: Définition d'une unité de dl racinaire urac
        !- somme de degré.jours racine
        urac = min(1. + (2. * somtemprac / (P_stlevamf + P_stamflax)), 3.)
        if (nlev == 0) urac = 1.
        !: la longueur totale de racine émise par jour (rlj)
        !- suit une logistique de façon similaire au LAI
        !- NB - le 28/10/01:
        !- 07/02/08: on integre ENFIN les modifs de Samuel (vive lui)
        !- essai de rendre nouvrac continu pour Samuel
        !- 20/02/07:
        !--if (int(zrac)-int(zrac-deltaz) <= int(deltaz)) then
        !--  multlvfront = int(deltaz)
        !--else
        !--  multlvfront = int(deltaz)+1
        !--endif
        !--if (nlev == 0) multlvfront = max0(1,multlvfront)
        !--nouvrac = P_lvfront*multlvfront
        ! nouvrac = P_lvfront*deltaz

        !: Introduction de l'indice de stress de densité racinaire idzrac / NB - le 06/06
        !efanoxd = 1.-(1.-idzrac)*P_sensanox

        !: NB - 10/03/02:
        !- Calcul du facteur densité efdensite actif à partir de P_laicomp
        !- Calcul de l'effet densité sur la mise en place du LAI pour les stics-plante
        !efdensite = 1.
        !if (urac >= 1.) then
        !  if (lai_veille < P_laicomp) then
        !    efdensite = 1.
        !  else
        !    !: domi - 02/07/2002: pb si GEL total densite = 0 et pb de log(0)
        !    if (densite == 0) then
        !      efdensite = 0.
        !    else
        !      efdensite = min(exp(P_adens * (log(densite / P_bdens))), 1.)
        !    endif
        !  endif
        !else
        !  efdensite = min(exp(P_adens * (log(densite / P_bdens))),1.)
        !endif

        !rlj = (P_draclong / (1. + exp(5.5 * (P_vlaimax - urac))) * efdensite * densite *dtj(n) * efanoxd) + (nouvrac * 1.e4)

        !: De la germination à la levée, il n'y a qu'une croissance du front racinaire
        !- Nb - le 17/02/2003: suppression de coderac
        if (P_codtrophrac /= 3) then
          !: Option trophique
          ! *- par calcul d'une fonction de répartition reprac = souterrain/total
          ! *- la longueur de racine au niveau du front est soustraite de la
          ! *- longueur produite
          ! ** ML le 25/05/2007 - les paramètres initiaux repracmin, repracmax et kreprac
          ! *- ont changé de noms dans le fichier plante: P_repracpermin, P_repracpermax et P_krepracperm
          ! *- si la liaison trophique est permanente (P_codtrophrac = 1) et
          ! *- P_repracseumin, P_repracseumax et P_krepracseu si la liaison est par seuils (P_codtrophrac = 2)
!!!          if (P_codtrophrac == 1) then
!!!            repracmax = P_repracpermax
!!!            repracmin = P_repracpermin
!!!            kreprac = P_krepracperm
!!!          endif
!!!
!!!          if (P_codtrophrac == 2) then
!!!            repracmax = P_repracseumax
!!!            repracmin = P_repracseumin
!!!            kreprac = P_krepracseu
!!!          endif

          reprac = (repracmax-repracmin) * (exp(-kreprac * (urac - 1.))) + repracmin

          !: PB - 06/01/2005: On multiplie par P_longsperac plutot que diviser (modifs Nadine du 30/12/2004)
          !rlj1 = reprac/(1.-reprac) * P_longsperac * 1.e2 * dltams

          !if (rlj1 < nouvrac * 1.e4) nouvrac = rlj1 * 1.e-4
          !if (P_codtrophrac == 1) then
          !  rlj = rlj1
          !else
          !  if (rlj >= rlj1) rlj = rlj1
          !endif
        endif

        !if (nlev == 0) rlj = nouvrac*1.e4

      !else
      !  rlj = 0.
      endif
     
     
      ! Here we get the reprac, which is an important variable addressing the above/below ground biomass partitioning.    
      ! we end our subroutine 

end subroutine reprac_calc
