










!*-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!> - Stics book paragraphe 3.1.1, page 40-44
!> - In STICS, leaf area growth is driven by phasic development, temperature and stresses. An empirical plant density-dependent function represents inter-plant
!! competition. For indeterminate plants, trophic competition is taken into account through a trophic stress index, while for determinate plants a maximal
!! expansion rate threshold is calculated to avoid unrealistic leaf expansion.
!! The calculation of leaf growth rate (deltai in m2m-2 d-1) is broken down: a first calculation of the LAI growth rate (in m2 plant-1 degree-day-1) describes
!! a logistic curve, related to the ILEV, IAMF and ILAX phenological stages. This value is then multiplied by the effective crop temperature, the plant density
!! combined with a density factor, supposed to stand for inter-plant competition, that is characteristic for the variety, and the water and nitrogen stress indices.
!!
!! At the end of the juvenile stage (IAMF), it is equal to vlaimax when the inflection of the dynamics (point of maximal rate) also occurs.
!! Between the stages ILEV, IAMF and ILAX, the model performs linear interpolation based on development units (upvt) which include all the environmental effects
!! on phasic development. As the ILAX stage approaches, it is possible to introduce a gradual decline in growth rate using the udlaimax parameter
!! - the ULAI value beyond which there is a decline in the leaf growth rate. If udlaimax=3 it has no effect and the leaf stops growing at once at ILAX.
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
!
!
!
!> -	Features of determinate crops
!!   Failure to account for trophic aspects in the calculation of leaf growth may cause problems when the radiation intercepted by the crop is insufficient to
!!   ensure leaf expansion (e.g. for crops under a tree canopy or crops growing in winter).  Consequently, from the IAMF stage, we have introduced a trophic effect
!!  to calculate the definitive LAI growth rate in the form of a maximum threshold for leaf expansion (deltaimaxi in m2m-2d-1) using the notion of the maximum
!!   leaf expansion allowed per unit of biomass accumulated in the plant (sbvmax in cm2 g-1) and the daily biomass accumulation (dltams in t.ha-1day-1 possibly
!!   complemented by remobilized reserve remobilj). sbvmax is calculated using the slamax and tigefeuil parameters.
!!
!> -	Features of indeterminate crops
!!   It has been possible to test the robustness of the above formalisation on a variety of crops, including crops where there is an overlap between the
!!   vegetative phase and the reproductive phase (soybean and flax for example).  However, when trophic competition between leaves and fruits is a driving force
!!   for the production and management of the crop (for example tomato, sugarbeet), this formalisation is unsuitable.  We therefore calculate the deltai variable
!!   so as to take trophic monitoring into consideration in the case of crops described as ‘indeterminate’, by introducing a trophic stress index (splai).
!!   As a consequence, the LAI can decrease markedly during the theoretical growth phase if the crop is experiencing severe stresses during the harvested
!!   organs filling phase.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!TODO: modify the structure into the same manner as ORCHIDEE

! XCW 23/05/2013 modified
! subroutine laidev(sc,p,pg,itk,t,soil,c,sta)

 subroutine laidev(n,                               &  ! IN
                   in_cycle,                        &  ! INout
                   f_crop_recycle,                  &  ! INOUT
                   f_sen_lai,                        &  ! INout
                   tair,                            &  ! IN  
                   t2m_min_daily,                            &  ! IN  
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
                   nsendltai,                       &  ! INOUT
                   nsenpfeuilverte,                 &  ! INOU&
                   nsendurvie,                      &  ! INOU&
                   nsenndurvie,                     &  ! INOU&
                   densiteequiv,                    &  ! INOU&
                   nplt,                            &  ! INOU&
                   tursla,                          &  ! INOU&
                   sla,                             &  ! INOU&
                   pfeuilverte,                     &  ! INOU&
                   bsenlai,                         &  ! INOU&
                   zrac,                            &  ! INOU&
                   nrec,                            &  ! INOU&
                   nlan,                            &  ! INOU&
                   tcult,                           &  ! INOU&
                   udevair,                         &  ! INOU&
                   udevcult,                        &  ! INOU&
                   ndrp,                            &  ! INOU&
                   rfvi,                            &  ! INOU&
                   nlev,                            &  ! INOU&
                   nger,                            &  ! INOU&
                   etatvernal,                      &  ! INOU&
                   caljvc,                          &  ! INOU&
                   rfpi,                            &  ! INOU&
                   upvt,                            &  ! INOU&
                   utp,                             &  ! INOU&
                   somcour,                         &  ! INOU&
                   somcourdrp,                      &  ! INOU&
                   somcourutp,                      &  ! INOU&
                   tdevelop,                        &  ! INOU&
                   somtemp,                         &  ! INOU&
                   somcourfauche,                   &  ! INOU&
                   stpltger,                        &  ! INOU&
                   R_stamflax,                      &  ! INOU&
                   R_stlaxsen,                      &  ! INOU&
                   R_stsenlan,                      &  ! INOU&
                   stlevflo,                        &  ! INOU&
                   nflo,                            &  ! INOU&
                   R_stlevdrp,                      &  ! INOU&
                   R_stflodrp,                      &  ! INOU&
                   R_stdrpmat,                      &  ! INOU&
                   nmat,                            &  ! INOU&
                   nlax,                            &  ! INOU&
                   nrecbutoir,                      &  ! INOU&
                   group,                           &  ! INOU&
                   ndebdes,                         &  ! INOU&
                   R_stdrpdes,                      &  ! INOU&
                   densite,                         &  ! INOU&
                   densitelev,                      &  ! INOU&
                   coeflev,                         &  ! INOU&
                   densiteger,                      &  ! INOU&
                   somelong,                        &  ! INOU&
                   somger,                          &  ! INOU&
                   humectation,                     &  ! INOU&
                   nbjhumec,                        &  ! INOU&
                   somtemphumec,                    &  ! INOU&
                   stpltlev,                        &  ! INOU&
                   namf,                            &  ! INOU&
                   stmatrec,                        &  ! INOU&
                   tustress,                        &  ! INOU&
                   lai,                             &  ! INOU&
                   somfeuille,                      &  ! INOU&
                   pdlai,                           &  ! INOU&
                   nbfeuille,                       &  ! INOU&
                   reajust,                         &  ! INOU&
                   ulai,                            &  ! INOU&
                   pdulai,                          &  ! INOU&
                   efdensite,                       &  ! INOU&
                   tempeff,                         &  ! INOU&
                   nstopfeuille,                    &  ! INOU&
                   deltai,                          &  ! INOU&
                   vmax,                            &  ! INOU&
                   nsen,                            &  ! INOU&
                   laisen,                          &  ! INOU&
                   dltaisenat,                      &  ! INOU&
                   nsencour,                        &  ! INOU&
                   dltamsen,                        &  ! INOU&
                   dltaisen,                        &  ! INOU&
                   fgellev,                         &  ! INOU&
                   gelee,                           &  ! INOU&
                   fstressgel,                      &  ! INOU&
                   pdlaisen,                        &  ! INOU&
                   R_stlevamf,                      &  ! INOU&
                   dernier_n,                       &  ! INOU&
                   durvieI,                         &  ! INOU&
                   durvie,                          &  ! INOU&
                   ndebsen,                         &  ! INOU&
                   somsenreste,                     &  ! INOU&
                   shumrel,                          &  ! INOU&
                   humrel,                          &  ! INOU&
                   swfac,                           &  ! INOU&
                   turfac,                          &  ! INOU&
                   senfac,                          &  ! INOUT
                   mafeuiljaune,                    &  ! INOUT
                   msneojaune,                      &
                   gslen,                           &
                   drylen,                           &
                   vswc, &
                   histgrowth, &
                   hist_sencour, &
                   hist_latest, &
                   doyhistst, &
                   nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, &
                   boxbiom,boxbiomrem, boxdurage, boxsomsenbase)           ! INOUT 


  USE Stics 
  USE Besoins_en_froid
  USE netcdf
  USE ioipsl

  IMPLICIT NONE

 
  ! declaration of variables

  ! 0.1 input 
  
  integer, intent(IN)                           :: n  
  logical, intent(inout)                           :: in_cycle
  logical, intent(inout)                           :: f_crop_recycle
  
  logical, intent(inout)                           :: f_sen_lai
  real,    intent(IN)                           :: tair !> / Mean air temperature of the day // degree C
  real,    intent(IN)                           :: t2m_min_daily !> / Minimum air temperature of the day // degree C
  real,    intent(IN)                           :: gdh_daily  !> // daily gdh calculated according to halfhourly temperature // transmitted from stomate.f90 gdh_daily
  real,    intent(IN)                           :: phoi   !> // OUTPUT // Photoperiod // hours
  logical, intent(IN)                           :: onarretesomcourdrp 
  real,    intent(IN), dimension(3)             :: stempdiag_cm_daily !> / soil temperature at 1 cm resolution for the sowing depth and neighbour layers // Degree C 
  real,    intent(IN), dimension(3)             :: shumdiag_cm_day     !> /soil moisture at 1 cm resolution for the sowing depth and neighbour layers // unit m3 m-3 with values ranging 0-1
  integer(i_std), intent(IN)                           :: nlevobs 
  integer, intent(IN)                           :: namfobs  
  integer, intent(IN)                           :: nfloobs  
  integer, intent(IN)                           :: nlanobs  
  integer, intent(IN)                           :: nlaxobs  
  integer, intent(IN)                           :: nmatobs  
  integer, intent(IN)                           :: nrecobs  
  integer, intent(IN)                           :: nsenobs  
  integer, intent(IN)                           :: ndrpobs  
  real,    intent(IN)                           :: dltams     !> // growth rate of the plant // t ha-1 day-1 (difference of total biomass)
  real,    intent(IN)                           :: eop        !> // potential evanpotranspiration 
  real,    intent(IN)                           :: masec      !> // above ground drymatter // t ha-1
  real,    intent(IN)                           :: masecveg   !> // vegetative dry matter  // t ha-1
  real,    intent(IN)                           :: vswc   !> daily humrel data
  real,    intent(IN)                           :: humrel

! 0.2 inout
 
  ! these variables are for laidev specifically
  integer, intent(INOUT)        :: nsendltams
  integer, intent(INOUT)        :: nsendltai
  integer, intent(INOUT)        :: nsenpfeuilverte
  integer, intent(INOUT)        :: nsendurvie
  integer, intent(INOUT)        :: nsenndurvie
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
  real,    intent(INOUT)        :: lai
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
   

! 0.4 local Variables
  !real                          :: tdev  
  real                          :: upvtutil
  real                          :: tempdev

 !  do i = 1, P_nbplantes   !possible multi species for intercropping.
 !     on pourrait utiliser un pointeur pour raccourcir les écritures.
 !     Ex : p = plante(i). Et au lieu de p(i), on aurait juste p.
 !   
 !     comments: adjustment of LAI, but it is only effective for the option of forcing LAI. 
 !     there are two options for treating lai, 1. forced, 2. calculated.  ---xcwu	
    
 !     if (lge(P_codesimul,'feuille') .eqv. .TRUE.) then
 !         call recalcullevee(n, nlev, nlevobs, nlaxobs, lai(1:n+1), &
 !                            ! tauxcouv(1:n+1),
 !                            & P_codelaitr &
 !                            !,estDominante
 !                            & )
 !     endif


    if (P_codeplante /= 'snu') then    ! 'snu' is bare soil.
    !ens = AS   ! AS is a scalar, the value is 1. 


    ! 1. In senescence process, we need the dltams and dltai at date of  nsencour (specific date). 
    !    but we should know that the variable nsencour is calculated in the latter process in our laidev module.
    !    So here , we can only use the data at the date of nsencour + 1 
     
         if (P_culturean /= 1 .AND. nsencour /= 0 .AND. nsencour > 365) then 
   
            if (n == nsencour - 365 + 1) then
               nsendltams = dltams
               nsendltai  = deltai
               nsenpfeuilverte = pfeuilverte 
               nsendurvie = durvie
            endif
      
 
            ! We need the durvie of the following day after nsencour
            if (n == nsencour- 365 + 2) then
               nsenndurvie = durvie
            endif
         endif
         
         if (P_culturean == 1 .AND. nsencour /= 0) then 
            if (n == nsencour + 1) then
               nsendltams = dltams
               nsendltai  = deltai
               nsenpfeuilverte = pfeuilverte 
               nsendurvie = durvie
            endif
 
            ! We need the durvie of the following day after nsencour
            if (n == nsencour + 2) then
               nsenndurvie = durvie
            endif
         endif

    ! 2. DEVELOPMENT OF PLANTS   
    !>>>> DEVELOPMENT PROCESSES<<<<<<<<<<<<<!
    !>>>> MODIFIED 05/17/13<<<<<<<<<<<<<<<<<!
         
         ! SET THE NPLT
         ! P_iwater //julian day of the beginning of the simulation // jour julien

         nplt = P_iplt0 - P_iwater + 1  ! although nplt is changed each day, but it is actually a constance if the sowing date is determined.


         call develop2(n, in_cycle, nplt, tair,  gdh_daily, turfac,  phoi, onarretesomcourdrp,  stempdiag_cm_daily, shumdiag_cm_day,lai,           &  !> INPUTS
                       nlevobs, namfobs, nfloobs, nlanobs, nlaxobs, nmatobs, nrecobs, nsenobs, ndrpobs, nrecbutoir,                  &  !> INPUTS
                       masec, namf,  ndrp, nflo, nger, nlan, nlev, nrec, etatvernal, caljvc,                                         &  !> INOUT
                       upvt, utp, somcour, somcourdrp, somcourfauche, somcourutp, somtemp, zrac,                                                      &  !> INOUT
                       coeflev, somelong, somger, humectation, nbjhumec, somtemphumec, densite, densitelev, nlax, nmat, nsen, stlevflo, ndebdes,      &  !> INOUT 
                       R_stlevamf, R_stamflax, R_stsenlan, R_stlaxsen, R_stlevdrp, R_stflodrp, R_stdrpmat, R_stdrpdes, densiteger,                    &  !> INOUT
                       udevair, udevcult, rfvi, rfpi, tdevelop, stpltger, upvtutil, stmatrec, group, tcult, stpltlev,                                 &
                       f_crop_recycle, gslen, drylen)                                   !> OUT

    
       ! DR 12/09/2012 on pense que il y avait un pb sur le calcul de deltalai qui pour les CAS etait calculé à partir de la densite equivalente
       ! cette densite equivalente ne doit etre utilisée que pour le calcul de efdensite (en cas de cas et pour la plante 2 ...)
       ! si les 2 plantes sont arrivées à levee on recalcule la densite equivalente

       ! comments: at this moment, we do not consider the intercropping system---xcwu
       !
       ! if(sc%P_nbplantes.gt.1 .and. i==2)then
       !       if((p(1)%nlev>0 .and. p(2)%nlev>0) .and. (p(1)%nlev==sc%n.or.p(2)%nlev==sc%n))then
              !TODO: commenté ou pas ça ?
       !            if (p(1)%nplt > p(2)%nplt) then
       !             call EnvoyerMsgHistorique('culture principale semée après culture associée')
       !           endif
       !    call F_densite_equiv(i,p(1)%densite,p(2)%densite,p(i)%estDominante,p(1)%P_bdens ,p(2)%P_bdens,&
       !     p(1)%densiteequiv,p(2)%densiteequiv)
       ! le jour de la levee la desnite de la plante 2 est celle qu'on vient de recalculer (densite equivalente)
       !     p(2)%densitelev=p(2)%densite
       ! endif
       ! endif
       ! si on a une seule plante on reaffecte la densite equivalente au cas ou il y aurait eu une modif sur la densite courante (gel ou autre )
        
 
       ! We consider that the crop system is homogenous and is without intercropping system, So the densiteequiv is equal to densite 
   
        densiteequiv = densite


       ! on décompose les valeurs AOAS calculées dans develop en AO et AS
       !  p(i)%rdtint(sc%AO,p(i)%nbrecolte-1) = p(i)%rdtint(sc%AOAS,p(i)%nbrecolte-1) * p(i)%surf(sc%AO)
       !  p(i)%rdtint(sc%AS,p(i)%nbrecolte-1) = p(i)%rdtint(sc%AOAS,p(i)%nbrecolte-1) * p(i)%surf(sc%AS)

       !  p(i)%nfruit(sc%AO,p(i)%P_nboite) = p(i)%nfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AO)
       !  p(i)%nfruit(sc%AS,p(i)%P_nboite) = p(i)%nfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AS)

       !  p(i)%pdsfruit(sc%AO,p(i)%P_nboite) = p(i)%pdsfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AO)
       !  p(i)%pdsfruit(sc%AS,p(i)%P_nboite) = p(i)%pdsfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AS)

       !#if DEBUG == 1
       !        if (iand(sc%develop,4) >0) call develop_debug_read_output(1232,sc,p,itk,i)
       !        if (iand(sc%develop,8) >0) call develop_debug_write_output(1233,sc,p,itk,i)
       !        if (iand(sc%develop,16) >0) call develop_debug_test_output(1234,sc,p,itk,i)
       !#endif

       ! dr - 17/11/05: pb de recolte
       ! dr et ml - 06/12/05 : c'etait idiot pour les perennes de forcer la recolte des 2 cultures en meme temps!!
     

       ! this part for the harvest of the cropping system,
       ! at this moment, we do not consider the cropping system, so we comment them
       !    if (itk(i)%P_coderecolteassoc == 1) then
       !      if (p(i)%nrec /= 0 .and. ALL(p(:)%P_codeperenne == 1))then
       !        p(:)%nrec = p(i)%nrec
       !      endif
       !    endif
    
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! 3. Record the lai at nsen
    !    After all of the processes relating to the calculation of daily LAI, we got the daily LAI.
    !    Specially, we need record the lai value on the date when the sen stage begin.  
       
          if (nsen > 0 .and. nlan == 0 .and. f_sen_lai) then 
                          
             bsenlai = lai     ! bsenlai is the lai at the date when the sen starts
           
             f_sen_lai = .FALSE.  
          endif

    ! 4. CALCULATION OF LAI   
    !>>>> MODIFIED 05/17/13<<<<<<<<<<<<<<<<<!

        if (lge(P_codesimul,'feuille') .eqv. .FALSE.) then    ! If the LAI is not forced.
          if (P_codelaitr == 1) then  ! if the calculation of LAI is true LAI instead of soil cover

            
            !do ens = sc%AS,sc%AO
            !  if (surf > 0.) then  ! here we do not consider whether or not there are crop!
            !    sc%ens = ens       ! we also do not need the intercropping system

            ! Calcul the  LAI by calling calai subroutine

            !call calai_(P_codeinnact,P_codeh2oact,P_codehypo,P_codegermin,P_codcueille,P_codlainet,codeulaivernal,      & !IN
            !            P_codeindetermin,P_codeinitprec,P_codeperenne,P_codetemp,turfac,innlai,                         &
            !            P_laiplantule,P_phyllotherme,udevair,udevcult,P_dlaimaxbrut,P_udlaimax,n,numcult,nbjmax,        &
            !            nplt,nlev,namf,nlax,ndrp,nrec,upvt,upvtutil,P_vlaimax,P_laicomp,somcour,somcourutp,             &
            !            P_stlevamf,P_stamflax,densite,P_adens,P_bdens,P_tcxstop,tcult,P_tcmin,P_tcmax,P_pentlaimax,     &
            !            P_dlaimin,exolai,sourcepuits,P_splaimin,P_splaimax,P_slamin,P_slamax,P_tigefeuil,P_tustressmin, &
            !            fstressgel,dltamsen,sla,remobilj,dltams,                                                        & !IN
            !            tustress,efdensite,nstopfeuille,deltai,splai,fpv,P_stlaxsen,tempeff,                            & !OUT
            !            lai,somfeuille,nbfeuille,nsen,nlan,P_dlaimax,reajust,ulai,vmax,dltaisenat,laisen,               & !INOUT
            !            dltaisen,P_stsenlan,densiteequiv)

            call calai_(turfac,                                                                     & ! IN
                        udevair,udevcult,n,                                                         & ! IN
                        nplt,nlev,namf,nlax,ndrp,nrec,upvt,upvtutil,somcour,somcourutp,             & ! IN
                        densite,tcult,                                                              & ! IN
                        fstressgel,dltamsen,sla,                                                    & ! IN
                        dltams, densiteequiv, bsenlai,                                              & ! IN
                        pdlaisen, lai, pdlai, pdulai, somfeuille, nbfeuille, nsen, nlan,            & ! INOUT
                        reajust,  ulai, vmax,                                                       & ! INOUT
                        dltaisenat, laisen, dltaisen, R_stsenlan,                                   & ! INOUT
                        tustress, efdensite, nstopfeuille, deltai, R_stlaxsen, tempeff,             &
                        histgrowth, hist_sencour, hist_latest, doyhistst)!,&
!                        nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, &
!                        boxbiom,boxbiomrem, boxdurage, boxsomsenbase)               ! OUT

       
            ! Rognage & Effeuillage, seulement si lai > 0
            ! ATTENTION! on travaille sur masec(n-1) car à ce moment de la boucle journalière masec(n) n'a pas encore été calculé...
            !     if (p(i)%lai(ens,sc%n) > 0.0) then
            !       if (itk(i)%P_codrognage == 2) then
            !         call rognage(itk(i)%P_codcalrogne,p(i)%hauteur(ens),p(i)%largeur,   & ! IN
            !                      itk(i)%P_hautrogne,itk(i)%P_margerogne,itk(i)%P_largrogne, &
            !                      p(i)%dfol,p(i)%P_hautbase,itk(i)%P_interrang,            &
            !                      p(i)%sla(ens),p(i)%P_tigefeuil,itk(i)%P_biorognem,sc%n,  &
            !           p(i)%nrogne,p(i)%CNplante(ens),         & DR 20/07/2012 on a plus besoin du Cnplante il est calculé apres
            !                      p(i)%nrogne,                                             &
            !                      p(i)%lairogne(ens),p(i)%biorogne(ens),                   & ! INOUT & OUT
            !                      p(i)%lai(ens,sc%n),p(i)%masec(ens,sc%n-1),               &
            !                      p(i)%varrapforme,p(i)%P_forme,p(i)%biorognecum(ens),     &
            !                      p(i)%lairognecum(ens))

            !       ! Cumuls AO/AS
            !         !p(i)%lairogne(ens)
            !         !p(i)%biorogne(ens)
            !         p(i)%lai(sc%aoas,sc%n) = p(i)%lai(sc%as,sc%n) * p(i)%surf(sc%as)              &
            !                                + p(i)%lai(sc%ao,sc%n) * p(i)%surf(sc%ao)
            !         p(i)%masec(sc%aoas,sc%n-1) = p(i)%masec(sc%as,sc%n-1) * p(i)%surf(sc%as)      &
            !                                    + p(i)%masec(sc%ao,sc%n-1) * p(i)%surf(sc%ao)
            !         !p(i)%varrapforme
            !         !p(i)%P_forme
            !         !p(i)%biorognecum(ens)
            !         !p(i)%lairognecum(ens)

            !         ! NB le 23/05 recyclage de la biomasse rognée
            !         ! PB - 08/08/2009 - Pas besoin d'apporter des résidus de rognage tous les jours.
            !         !                   On conditionne l'apport en résidus, si il y a de la masse rognée.

            !         if (sc%n == p(i)%nrogne .and. p(i)%biorogne(ens) > 0.) then
            !           soil%itrav1 = 1
            !           soil%itrav2 = 1
            !           sc%ires = 2
            !           Cfeupc  =  42.
            !           CNrogne = Cfeupc / p(i)%CNplante(ens)
            !          
            !           !Bruno 06/2012 - ajout quantites de C et N tombees au sol apres rognage
            !           Crogne=p(i)%biorogne(ens)*Cfeupc*10.
            !           Nrogne=Crogne/CNrogne
            !           p(i)%QCrogne = p(i)%QCrogne + Crogne
            !           p(i)%QNrogne = p(i)%QNrogne + Nrogne


            !          !EC 0!6/08/2012 Ajout du code ires des feuilles rognées
            !          sc%ires = 2
            !          call ResidusApportSurfaceSol(p(i)%biorogne(ens),Cfeupc,-1.e-10,sc%ires,pg%P_CNresmin(sc%ires),  & ! IN
            !                 pg%P_CNresmax(sc%ires),1.,1.,pg%P_Qmulchdec(sc%ires),sc%nbCouchesSol,pg%nbResidus,   &
            !                 itk(i)%nap,sc%airg(sc%n+1),CNrogne,sc%Cnondec(1:10),sc%Nnondec(1:10),                &
            !                 sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus), &
            !                 sc%QCapp,sc%QNapp,sc%QCresorg,sc%QNresorg)                                                 ! INOUT

            !         Wr=0.
            !         if (CNrogne > 0.) Wr=1./CNrogne
            !         call ResiduParamDec(pg%P_awb(sc%ires),pg%P_bwb(sc%ires),pg%P_cwb(sc%ires),pg%P_CroCo(sc%ires),  &
            !            pg%P_akres(sc%ires),pg%P_bkres(sc%ires),pg%P_ahres(sc%ires),pg%P_bhres(sc%ires),             &
            !            sc%Wb(sc%ires),sc%kres(sc%ires),sc%hres(sc%ires),Wr)
            !         endif

            !       endif


            !      Comments----xcwu
            !      The removal of leaves seems only effective for indeterminate crops, so at this moment we do not consider this processes,
            !      because we mainly focus on the determinate crops.
            !      So, this process is commented. 

            !       if (P_codeffeuil /= 1) then  ! if there is no thinning management! it seems to be only effective for indeterminate crops, such as grape etc
            !         call effeuill(P_codcaleffeuil,P_laidebeff,deltai(ens,sc%n), &
            !                       P_effeuil,sla(ens),n,neffeuil,  &
            !                       P_laieffeuil, P_codetransrad,P_hautmax,P_khaut,     &
            !                       dfol,P_largrogne,P_codhauteff,largeur,      &
            !                       lai(ens,sc%n),masec(ens,sc%n-1),P_hautbase,        & ! INOUT
            !                       varrapforme,bioeffcum(ens),laieffcum(ens))

            !       ! Cumuls AO/AS
            !         p(i)%lai(sc%aoas,sc%n) = p(i)%lai(sc%as,sc%n) * p(i)%surf(sc%as)              &
            !                                  + p(i)%lai(sc%ao,sc%n) * p(i)%surf(sc%ao)
            !         p(i)%masec(sc%aoas,sc%n-1) = p(i)%masec(sc%as,sc%n-1) * p(i)%surf(sc%as)      &
            !                                      + p(i)%masec(sc%ao,sc%n-1) * p(i)%surf(sc%ao)

            !       endif
                endif
              endif
      !      end do
      !     else
      !      call TauxRecouvrement(sc,pg,p(i),itk(i))
      !    endif
      !  endif
      !endif


    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! 4. Senescence processes for LAI
    !    This process is belong to GROWTH module in STICS
    !    In this subroutine, we mainly calculate the senescenced lai and biomass
    !    In detail, see senescence.f90 and the red book
    
         call senescen(nlev,n, lai, senfac,  &
                       nbfeuille,t2m_min_daily, &
                       densitelev,          &
                       masecveg,         &
                       nstopfeuille,somcour,nrec,      &
                       ulai, &
                       tdevelop,    &
                       somtemp,nsenpfeuilverte,nsendltai,nsendurvie, nsendltams, nsenndurvie,          &    ! INPUTS
                       pdlaisen, dernier_n,nsencour,dltaisen,dltamsen,fstressgel,fgellev,gelee,      & !INOUT
                       densite,laisen,nlan,R_stsenlan,nsen,R_stlaxsen,namf,nlax,R_stlevamf,      &
                       R_stamflax,durvie,     &
                       ndebsen,somsenreste, mafeuiljaune, msneojaune,        &
                       durvieI,deltai,dltams, &
                       histgrowth, hist_sencour, hist_latest, doyhistst,  &
                       nbox, boxulai, boxndays, boxlai, boxlairem,boxtdev, &
                       boxbiom,boxbiomrem, boxdurage, boxsomsenbase)               ! OUT
!)  ! OUTPUT
    
    ! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! 5. Calculation of sla ---xcw
    !    In stics, the calculation of sla is in reptair.f90, The process reptair is belong to GROWTH in STICS
    !    the reptair process is called after the senescence 
    
         ! calculation of tursla, the slowing effects of water stress on sla
         ! turstress is calculated in Stics_Calcul_Lai.f90
    
         tursla = (tursla+tustress)/2.
         IF (P_codesla==1) THEN
             sla = min(tursla * P_slamax, P_slamin)
             if ((lge(P_codesimul,'feuille') .eqv. .FALSE.).and.(sla > P_slamax  )) then
                sla = P_slamax
             endif
             if (sla < P_slamin) sla = P_slamin
         ELSEIF (P_codesla==2) THEN
            if ((n <= nlev+1) .or. (nlev==0)) then ! before or start leafing
               sla = P_slamax
            else if ( ndrp == 0 ) then
               tempdev = min(1. + (3. * somcourdrp / (P_stlevamf + P_stamflax)), 4.)
               if (tursla>1) then
                   tursla = 1
               else if (tursla<0) then
                   tursla = 0
               endif
               tursla = (1-tursla)/2 + 0.5 !tursla ~ [0.5 1]
               sla = (P_slamax-P_slamin) * (exp(-1.5 * (tempdev - 1.))) * tursla + P_slamin
            else if (ndrp >0 .and. nmat==0) then
               sla = sla ! after ndrp, there is no imposed change of sla 
            else
               ! do nothing, keep the same sla until harvest
            endif
             
         ELSE
             WRITE(numout,*) 'codesla not recognized: ',P_codesla
             stop
         ENDIF

    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! 6. Calculation of pfeuilverte
    !    In stics, the calculation of pfeuilverte is in reptair.f90
    !    the reptair process is called after the senescence 
    !    masec == aboveground dry matter (t ha-1)
    !    dltams == growth rate of plant (t ha-1 day-1)    
    
     
         if (masec <= 0.0) then
            pfeuilverte = 0.0
         else
            if (dltams > 0.) then
               pfeuilverte = (deltai/P_slamax*1e2) / dltams
               pfeuilverte = min(1.0,pfeuilverte)
            else
               pfeuilverte = 0.0
            endif
         endif

    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    ! 8.  Calculation of the water stress on LAI, turfac
          call stress(n,                  &  ! IN
                      nrec,               &  
                      lai,                & 
                      eop,                &
                      shumdiag_cm_day,    &  ! IN 
                      vswc,               &  ! IN                    
                      humrel,             &  ! IN
                      shumrel,             &  ! OUT
                      swfac,              &  ! INOUT)   ! calculate the turfac stress (water stress)
                      turfac,             &
                      senfac)                 

    ! This part is for debugiing 

   ! write(*,*) 'wu: DOY is',n
   ! write(*,*) 'wu: nplt, nger, nlev, ndrp, nlax, nsen is ', nplt, nger, nlev, ndrp, nlax, nsen
   ! write(*,*) 'wu: upvt is ', upvt
   ! write(*,*) 'wu: somcour is ', somcour
   !  
   ! write(*,*) 'wu: udevair is ', udevair
   ! write(*,*) 'wu: rfvi and rfpi is ', rfvi, rfpi
   ! write(*,*) 'wu: efdensity and tempeff and nstopfeille is ', efdensite, tempeff,nstopfeuille
   ! write(*,*) 'wu: dltai and dltaisen is ', deltai, dltaisen
   ! write(*,*) 'wu: ulai and tustress  is ', ulai, tustress



    endif
return
end
