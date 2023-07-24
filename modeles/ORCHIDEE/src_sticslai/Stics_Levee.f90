!! ****************************************************************
!> Calculation of the emergence. Authors : C. Durr and G. Richard.
!> - Programmation: N. Brisson.
!> - last modification 20/02/07 : l'humectation de la graine se fait en fonction d'un potentiel : humecgraine en MPa.
!> - Stics book paragraphe 2.2.2, page 21-27
!!
!> In STICS, the emergence phase is broken down into three subphases: seed imbibition, followed by germination and lastly, shoot elongation.
!! The soil physical conditions influence not only the duration of emergence but also the number of emerged plants, in particular in dry conditions or when
!! there is a surface crust.
!> - Moistening :
!!   Seed moistening can be regarded as a passive process starting at a species-dependent water potential prevailing in the seed bed (potgermi in MPa).
!!   The relationship from Clapp and Hornberger (1978), parameterized by the characteristic soil water contents of field capacity and wilting point, was used
!!   to convert potgermi into water content (in function "humpotsol", described in the module Divers_develop.f90). Once the seed is moistened, it has a limited
!!   number of days of autotrophy  (nbjgrauto) due to its reserves. This number has a species-dependent component (nbjgerlim) but also a thermal one, since
!!   it is thought that at low temperature (i.e the average soil temperature in the seed bed, from the beginning of moistening), respiration processes and
!!   the consumption of reserves are slower (the minimum at high temperature is propjgermin x nbjgermin). When the temperature is lower than the
!!   germination base temperature, tgmin, then the day number is maximal (nbjgerlim).
!> - Germination :
!!   Germination is achieved when the growing degree-days from planting in the seed bed (somger) reaches a given threshold (stpltger), with a condition as to
!!   the dryness of the soil. Soil moisture in the seedbed influences germination through the "humirac" function (described in the module Divers.f90).
!!   If the seedbed dries out, it may delay germination significantly. This does not impair grain viability as long as the grain has not already imbibed water.
!!   If however the soil water content has been high enough to allow grain moistening, grain viability is reduced. To account for this effect, we relied on
!!   Bradford�s (1990, 2002) work showing that too long a time for germination after moistening reduces the germination rate if the number of days of
!!   moistening (nbjhumec) is higher than a plant- and temperature-dependent threshold duration (nbjgrauto). It is assumed that germination occurs
!!   (IGER being the germination day) but at a reduced plant density (ratio between density of germinated plants, densiteger, to sowing density, densitesem)
!!   proportional to the thermal time deficit.
!> - Subsoil plantlet growth :
!!   Germination initiates the growth of the root and then of the shoot. The growth rate of the shoot is assumed to be a logistic function of soil degree-days
!!   that may slow down with unsuitable soil moisture (humirac). Emergence occurs when elongation (elong) is greater than sowing depth (profsem).
!> - Influence of soil crusting on emergence :
!!   The density reduction law is specific to the crusting phenomenon but analogous to the other constraint law (water content and temperature-dependent)
!!   with a minimum threshold corresponding to the vigueurbat parameter : if vigueurbat is greater than 0, which means that when the soil is crusted a
!!   proportion of plants succeed in emerging, the crusting coeflev function is less effective than the water content and temperature-dependent coeflev function.
!!   The combination of both relationships is made dynamically by calculating the daily derivatives of both laws: if the current day is a "battance=0" day
!!   (battance is calculated in the module Stics_Battance.f90) the density reduction is done according to the crusting coeflev law.
!>
!! For woody plants which have perennial dormancy, ILEV stage corresponds to the budbreak stage (for this calculation hourly temperatures are reconstituted
!! in the module Stics_Debour.f90).
!------------------------------
subroutine levee(n, tsol, hsol, nlevobs,                                                             & ! INPUTS
                 densiteger,densite,coeflev,densitelev,zrac,                                        & ! INOUT
                 somelong,somger,nlev,nger, humectation,nbjhumec,somtemphumec,somcour,              & ! INOUT
                 in_cycle, f_crop_recycle, nplt)                ! INOUT

USE Stics
USE Besoins_en_froid
USE Divers_water
!USE Messages



implicit none


! 0.1 INPUT


  integer, intent(IN) :: n
  real,    intent(IN), dimension(3) :: tsol  !> // soil temperature with resolution of 1 cm, the dimension is 3, we consider the sowing depth and his upper and lower neighbours // degree C
  real,    intent(IN), dimension(3) :: hsol  !> // soil moisture with resolution of 1 cm, the dimension is 3, we consider the sowing depth and his upper and lower neighbours // 
  integer, intent(IN) :: nlevobs 
  integer, intent(IN) :: nplt  
  
 

  !integer, intent(IN) :: P_codeperenne  !> // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0 
  !integer, intent(IN) :: P_codebfroid  !> // PARAMETER // option of calculation of chilling requirements // code 1/2 // PARPLT // 0 
  !integer, intent(IN) :: P_codegdhdeb  !> // PARAMETER // option of calculation of the bud break date in hourly or daily growing degrees  // code 1/2 // PARPLT // 0 
  !integer, intent(IN) :: P_codetemp  !> // PARAMETER // option calculation mode of heat time for the plant : with air temperature (1)  or crop temperature (2) // code 1/2 // PARPLT // 0 
  !integer, intent(IN) :: P_codegermin  !> // PARAMETER // option of simulation of a germination phase or a delay at the beginning of the crop (1) or  direct starting (2) // code 1/2 // PARPLT // 0 
  !integer, intent(IN) :: P_codefente  !> // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0 
  !integer, intent(IN) :: P_codepluiepoquet  !> // PARAMETER // option to replace rainfall by irrigation at poquet depth in the case of poquet sowing // code 1/2 // PARAMV6 // 0 
  !integer, intent(IN) :: P_codehypo  !> // PARAMETER // option of simulation of a  phase of hypocotyl growth (1) or planting of plantlets (2) // code 1/2 // PARPLT // 0 
  !integer, intent(IN) :: P_nbjgerlim  !> // PARAMETER // Threshold number of day after grain imbibition without germination lack // days // PARPLT // 1 
  !real,    intent(IN) :: tmin   !> // OUTPUT // Minimum active temperature of air // degree C
  !real,    intent(IN) :: tmin_demain  
  !real,    intent(IN) :: tmax   !> // OUTPUT // Maximum active temperature of air // degree C
  !integer, intent(IN) :: n  
  !integer, intent(IN) :: nrec  
  !integer, intent(IN) :: P_nlevlim1  !> // PARAMETER // number of days after germination decreasing the emerged plants if emergence has not occur // days // PARPLT // 1 
  !integer, intent(IN) :: P_nlevlim2  !> // PARAMETER // number of days after germination after which the emerged plants are null // days // PARPLT // 1 
  !real,    intent(IN) :: P_tdmindeb  !> // PARAMETER // minimal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  !real,    intent(IN) :: P_tdmaxdeb  !> // PARAMETER // maximal thermal threshold for hourly calculation of phasic duration between dormancy and bud breaks // degree C // PARPLT // 1
  !real,    intent(IN) :: rfvi   !> // OUTPUT // Slowing effect of the vernalization on plant development // 0-1
  !real,    intent(IN) :: rfpi   !> // OUTPUT // Slowing effect of the photoperiod on plant development  // 0-1
  !real,    intent(IN) :: P_profsem  !> // PARAMETER // Sowing depth // cm // PARTEC // 1 
  !real,    intent(IN) :: P_stdordebour  !> // PARAMETER // phasic duration between the dormancy break and the bud break  // degree.days // PARPLT // 1 
  !real,    intent(IN) :: P_tgmin  !> // PARAMETER // Minimum threshold temperature used in emergence stage // degree C // PARPLT // 1
  !real,    intent(IN) :: P_stpltger  !> // PARAMETER // Sum of development allowing germination // degree.days // PARPLT // 1 
  !real,    intent(IN) :: P_sensrsec  !> // PARAMETER // root sensitivity to drought (1=insensitive) // SD // PARPLT // 1 
  !real,    intent(IN) :: P_psihucc  !> // PARAMETER // soil potential corresponding to field capacity  // Mpa // PARAM // 1 
  !real,    intent(IN) :: P_psihumin  !> // PARAMETER // soil potential corresponding to wilting point // Mpa // PARAM // 1 
  !real,    intent(IN) :: P_potgermi  !> // PARAMETER // humidity threshold from which seed humectation occurs, expressed in soil water potential  // Mpa // PARPLT // 1 
  !real,    intent(IN) :: P_tdmax  !> // PARAMETER // Maximum threshold temperature for development // degree C // PARPLT // 1
  !real,    intent(IN) :: P_propjgermin  !> // PARAMETER // minimal proportion of the duration P_nbjgerlim when the temperature is higher than the temperature threshold P_Tdmax  // % // PARPLT // 1 
  !real,    intent(IN) :: P_densitesem  !> // PARAMETER // Sowing density  // plants.m-2 // PARTEC // 1 
  !real,    intent(IN) :: pluiesemis  
  !real,    intent(IN) :: P_pluiebat  !> // PARAMETER // minimal rain quantity for the crust occurrence // mm day-1 // PARSOL // 1 
  !real,    intent(IN) :: P_mulchbat  !> // PARAMETER // mulch depth from which a crust occurs // cm // PARSOL // 1 
  !real,    intent(IN) :: xmlch1   !> // OUTPUT // Thickness of mulch created by evaporation from the soil // cm
  !real,    intent(IN) :: P_vigueurbat  !> // PARAMETER // indicator of plant vigor allowing to emerge through the crust  // between 0 and 1 // PARPLT // 1 
  !real,    intent(IN) :: P_celong  !> // PARAMETER // parameter of the subsoil plantlet elongation curve // SD // PARPLT // 1 
  !real,    intent(IN) :: P_belong  !> // PARAMETER // parameter of the curve of coleoptile elongation // degree.days -1 // PARPLT // 1 
  !real,    intent(IN) :: P_elmax  !> // PARAMETER // Maximum elongation of the coleoptile in darkness condition // cm // PARPLT // 1 
  !integer, intent(IN) :: nbCouches  
  !real,    intent(IN) :: tsol(0:nbCouches)  
  !real,    intent(IN) :: hur(nbCouches)  
  !real,    intent(IN) :: humin(nbCouches)  
  !real,    intent(IN) :: hucc(nbCouches)  
  !real,    intent(IN) :: trr   !> // OUTPUT // Rainfall  // mm.day-1
  !real,    intent(IN) :: dacouche(0:nbCouches)  



  ! 0.2 INOUT
 
  !real,    intent(INOUT) :: udevair   !> // OUTPUT // Effective temperature for the development, computed with TAIR // degree.days
  !real,    intent(INOUT) :: udevcult   !> // OUTPUT // Effective temperature for the development, computed with TCULT // degree.days
  !real,    intent(INOUT) :: upvt   !> // OUTPUT // Daily development unit  // degree.days
  real,    intent(INOUT) :: densiteger  
  real,    intent(INOUT) :: densite   !> // OUTPUT // Actual sowing density // plants.m-2
  real,    intent(INOUT) :: coeflev  
  real,    intent(INOUT) :: densitelev  
  real,    intent(INOUT) :: zrac   !> // OUTPUT // Depth reached by root system // cm
  real,    intent(INOUT) :: somelong  
  real,    intent(INOUT) :: somger  
  integer, intent(INOUT) :: nlev  
  integer, intent(INOUT) :: nger  
  logical, intent(INOUT) :: humectation  
  integer, intent(INOUT) :: nbjhumec  
  real,    intent(INOUT) :: somtemphumec  
  real,    intent(INOUT) :: somcour   !> // OUTPUT // Cumulated units of development between two stages // degree.days
  logical, intent(INOUT) :: in_cycle !> in this subroutine, we also judge whether or not we should stop the crop cycle due to no emergence, if the coeflev is becoming zero, then the in_cycle should be setted as FALSE, indicating exiting this cycle

  logical, intent(INOUT) :: f_crop_recycle !> if the crop can not emerge, then the in_cycle should be setted as FALSE, indicating exiting this cycle, and recycle crop


  ! 0.4 local variables

  !integer :: i  !>  
  !integer :: isem  !>  
  !integer :: isembas  !>  
  !integer :: isemhaut  
  !! NB le 19/01/08
  !real :: hn  !>  
  !real :: hx  !>  
  
  real :: udevlev  
  real :: temphumec  
  real :: levbat  
  real :: nbjgrauto  
  integer :: icompte 
  real :: elong  !>  
  real :: humsol  !> 




!: FUNCTIONS 
  ! real :: humirac ! module Divers
  ! real :: battance  
  ! real :: humpotsol  



! 25/04/2012 DR et IGC le retour ..., pb si on force la levee pour la vigne on passe dans le caluvle de la germination et on met une densit� � 0
! en gros il faut decomposer le test en 2 lignes
!      if (P_codeperenne == 2 .and. P_codebfroid == 3 .and. nlevobs == 999) then

! for perennial crop which are suffering P_codebroid
      if (P_codeperenne == 2 .and. P_codebfroid == 3 ) then
        !if(nlevobs == 999)then
        !!: PB - 15/12/2008: Pour bien faire, je pense qu'on pourrait facilement d�porter le calcul des udev et upvt pour debour dans develop.
        !!- Ca all�gerait l'�criture de levee et �a serait sans doute tout autant logique.
        !  if (P_codegdhdeb == 2) call debour(P_codegdhdeb,P_codetemp,tmax,tmin,tmin_demain,P_tdmindeb,P_tdmaxdeb,rfvi,rfpi, &
        !                                 upvt,udevair,udevcult)
        !  somelong = somelong + upvt
!       ! write(70,*) '2.',somelong,upvt
        !  if (somelong > P_stdordebour) then
        !    nlev = n
        !    somelong = 0.
        !  ! DR 05/08/08 si on est � debourrement on initialise qnplante � P_qnplante0 ,
        !  ! apres essai y'a pb ca plante au bout de 12 ans � voir avec nadine
        !  !    qnplante(ipl,1,n-1) = P_qnplante0
        !  endif
        !  return
        !else
          return
        !endif
      endif

!      print *, 'in levee, the tsoil is', tsol(:)

      !: La germination
      !- calcul de la germination m�me si nlev observ� pour le d�marrage de la vernalisation
      !isem = int(P_profsem)
      udevlev = tsol(2) - P_tgmin  ! there are 3 layers for tsol, the second layer means the sowing depth
      if (udevlev <= 0.0) udevlev = 0.0

      write(*,*) 'nplt, udevlev, nger', nplt, udevlev, nger

      !: Rajout domi - 15/09/97:
      !- Test sur isem-1 et isem+1 pour qu'ils restent dans l'intervalle [1,200]
      !- Calcul de la germination si P_codegermin = 1
      !- domi - 14/12/00 - on passe la profondeur de sol de 200 � 1000
      !- TODO: Remplacer le 1000 par nbCouches

      !isembas = isem-1
      !isemhaut = isem+1
      !if (isembas < 1) isembas = 1
      !if (isembas > 1000) isembas = 1000
      !if (isemhaut < 1) isemhaut = 1
      !if (isemhaut > 1000) isemhaut = 1000
      if (P_codegermin == 1) then
        !humsol = (hur(isem) + hur(isemhaut) + hur(isembas)) / 3
        humsol = (hsol(2) + hsol(1) + hsol(3)) / 3.0 ! although the code is not elegant but it is clear

        !hn = (humin(isem) + humin(isemhaut) + humin(isembas)) / 3
        !hx = (hucc(isem) + hucc(isemhaut) + hucc(isembas)) / 3

!        print *, 'tsol and humsol in levee is', tsol(2), humsol 

        somger = somger + (udevlev * F_humirac(humsol))
        write(*,*) 'udevlev * F_humirac(humsol)', udevlev * F_humirac(humsol)
        write(*,*) 'somger ', somger

        if (somger >= P_stpltger .and. nger == 0) then
          nger = n
          somelong = somger - P_stpltger
          zrac = P_profsem
        endif
      
!        print *, 'in levee, the somger and somlong is,', somger, somelong 
   
      ! NB le 18/08/07 : si lev�e observ�e et germination post�rieure � la lev�e alors for�age germination
      
      ! we do not use the forced emergence data  
      
        !if (nlevobs /= 999) then
        !  if (nger > 0 .and. nlevobs < nger) then
        !    nger = nlevobs
        !  endif
        !endif

        !: NB le 11/04/05 introduction manques � la germination
        !- Sophie Lebonvallet
        !- humectation de la graine
        !- initialisation de humectation,nbjhumec


        if (somger < P_stpltger .and. nger == 0) then
          !whether or not the humidity is larger than the potential soil humidity 
          !if (humsol >= humpotsol(P_psihucc,P_psihumin,hn,hx,dacouche(isem),P_potgermi,P_codefente)) then
          if (humsol >= 1) then
            if (.not. humectation ) humectation = .TRUE.
          endif
          if (humectation) then
            nbjhumec = nbjhumec+1 ! accumulated days for humectation
            somtemphumec = tsol(2) + somtemphumec
            temphumec = somtemphumec / nbjhumec
            ! on calcule le nombre de jours limite d'autotrophie de la graine en fonction de la temp�rature
            nbjgrauto = (P_propjgermin - 1.0) / (P_tdmax - P_tgmin) * temphumec + 1 + (1.0 - P_propjgermin) * P_tgmin / &
                        (P_tdmax - P_tgmin)
            if (temphumec < P_tgmin) nbjgrauto = 1.0
            if (temphumec > P_tdmax) nbjgrauto = P_propjgermin
          else
            somtemphumec = 0.
            nbjgrauto = 1.0
          endif
          nbjgrauto = nbjgrauto*P_nbjgerlim
          if (nbjhumec >= nbjgrauto) then
            nger = n
            densiteger = P_densitesem * somger / P_stpltger
            ! ajout de test NB le 09/08/05
            if (densiteger > P_densitesem) densiteger = P_densitesem
          else
            densiteger = P_densitesem
          endif
        endif
        ! DR 06/02/07 on a pas acces � la densit� � germination pour sophie
        ! pour l'instant je l'affecte dans densite mais c'est � voir avce nad
        densite = densiteger
      else
        nger = n
        zrac = P_profsem
      endif


       
      ! introduction battance NB le 12/05/05
      ! bug nrec > nrec (NB 19/01/08)
      !levbat = battance(n,nplt,nrec,P_codeperenne,pluiesemis,trr,P_pluiebat,P_mulchbat,xmlch1,elong,P_profsem)
      ! Nb le 19/01/08 : pas de battance avec le semis en poquet
      !--if (P_vigueurbat == 1.0) levbat = 1.0


      ! in this version, we do not consider the levbat effects on germination
      if (P_vigueurbat == 1.0 .or. P_codepluiepoquet == 1) levbat = 1.0


      !: �longation
      !- NB - 17/08/07 : il faut pouvoir calculer la densit� lev�e quel que soit
      !-                 nlev obs ou cal
     
      if (nlevobs == 999) then
        if (nger > 0 .and. nlev == 0) then
          if (P_codehypo == 2) then
            nlev = n
          else
            ! From the codes of this part, it calculates the average soil moisture in specific layers. It also calculates the Hn and Hx. 
            ! but in this version, we just use the soil moisture at sowing depth to replace it. ---xcw
   
            !humsol = 0.
            !hn = 0.
            !hx = 0.
            ! NB le 19/01/08 compteur pour moyenne correcte
            !icompte = 0
            !do i = isembas,max(int(zrac),isemhaut)
            !  humsol = humsol + hur(i)
            !  hn = hn + humin(i)
            !  hx = hx + hucc(i)
            !  icompte = icompte + 1
            !end do
            !humsol = humsol / icompte
            
            ! we just show that the humsol is same to the average soil moisture of the 3 layers around sowing depth
            humsol = humsol
            !hn = hn / icompte
            !hx = hx / icompte

            ! introduction battance NB le 12/05/05
            somelong  =  somelong + (udevlev * levbat * F_humirac(humsol))
            elong = P_elmax * (1 - exp(-(P_belong * somelong)**P_celong))
  
!            print *, 'in levee, the elong is,', elong
            write(*,*) 'elong,',elong
       
            if (elong >= P_profsem) nlev = n
          endif
        endif
      endif
      
!      print *, 'in levee before calculating coeflev, P_nlevlim1 P_nlevlim2, levbat, coeflev are', P_nlevlim1, P_nlevlim2, levbat, coeflev

      !: Diminution de la densit� lev�e en fonction du d�lai germination-lev�e
      !- param�tres � mettre dans *.plt 18/12/01
      
      ! FOR WINTER WHEAT, THE GROWING SEASON CAN COVER TWO YEAS, SO IN THE NEXT YEAR WE SHOULD NOT 
      ! REPLACE THE COEFLEV AS 1.0. 
      ! WE ALSO SET A CONDITION THAT IF THE COEFLEV IS NEGATIVE, WE SET IT AS 0.
      

      if ( nger > 0 ) then
         if ( nger > nplt ) then !especially for crops covering two years, here in the current year the crop germinated     
           if (((n - nger) < P_nlevlim1) .AND. ((n - nger) >= 0 )) then ! 
             coeflev = 1.0
           else
             if ( P_nlevlim1 < P_nlevlim2 ) then
               if ( levbat == 1. ) then
                 if ( n - nger >= 0 ) then
                    coeflev = coeflev + (1. / float((P_nlevlim1 - P_nlevlim2)))
                 else  ! for winter wheat, it emerges in the following spring
                    coeflev = coeflev + (1. / float((P_nlevlim1 - P_nlevlim2))) * 1.06  ! largely reduction of coeflev
                 endif
                 ! IF THE COEFLEV IS BECOMING NEGATIVE, WE SHOULD SET IT AS 0.
                 if ( coeflev <= 0.0 ) then
                    coeflev = 0.0
                    in_cycle = .FALSE.
                    f_crop_recycle = .TRUE.
                 endif       
               else
                 coeflev = coeflev + (1. - P_vigueurbat) /float( (P_nlevlim1-P_nlevlim2))
                 ! THE SAME AS ABOVE, IF THE COEFLEV IS BECOMING NEGATIVE, WE SHOULD SET IT AS ZERO
                 if ( coeflev <= 0.0 ) then
                    coeflev = 0.0
                    in_cycle = .FALSE.
                    f_crop_recycle = .TRUE.
                 endif
 
               endif
             else
               if ( levbat == 1. ) then
                 coeflev = 0.0

               !: ML 11/12/07 y'avait un bug: on ne peut pas avoir coeflev = P_vigueurbat apr�s P_nlevlim2
               !- si � la fois la battance et les conditions d'humidit� et de temp�rature ont
               !- frein� la lev�e
               !--else
               !--  coeflev = P_vigueurbat
               endif
             endif
           endif
         else  ! especially for crops covering two years, for one-year crop it will not occur. for spring germination, the effective densite should be largely reduced
           coeflev = 1.0 + (1. / float((P_nlevlim1 - P_nlevlim2))) * ( (365-nplt-P_nlevlim1) + n*1.06 ) ! calculation of the coeflev until crop emerges in spring
           if ( coeflev <= 0.0 ) then
              coeflev = 0.0
              in_cycle = .FALSE.
              f_crop_recycle = .TRUE.
           endif
         endif
      endif 

!      print *, 'in levee the densiteger, coeflev is', densiteger, coeflev  
   
      
      !: R�duction de densit�
      !- introduction de l'effet battance le 13/05/05
      !- modif NB le 18/08/07
      if (n == nlev .or. n == nlevobs) then
        densitelev = densiteger * coeflev
        densite = densitelev
        if (densite <= 0.) then
          !call EnvoyerMsgHistorique(400)
          !: ML le 21/09/04 lorsque la densite de levee est nulle, on n arrete
          !- pas la simulation (equivaut a un sol nu)
          !--stop
          !- domi 16/05/06 j'essaie de mettre la densite �z�ro
          densite = 0.0
        endif

      endif

      !: 29/01/04 - bug pour affichage pheno dans bilan:
      !- somcour etait mal affecte (NB et ML)
      if (n >= nger .and. nger > 0) then
        somcour = P_stpltger + somelong
      else
        somcour = somger
      endif

return
end subroutine levee
