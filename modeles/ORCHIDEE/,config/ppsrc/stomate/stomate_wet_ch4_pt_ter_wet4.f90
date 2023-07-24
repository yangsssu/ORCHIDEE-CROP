










  !Bruno Ringeval; routines de calcul des densites de flux de CH4 a l interface surface/atmo
  !routines inspirées TRES fortement du modèle de Walter et al.
  !j indique quand c'est possible les routines du modele initial de Walter a laquelle appartiennent les differents "bouts" ci-dessous
  !modifications principales: modif du calcul du substrat de la methanogenèse
  !couplage avec ORCHIDEE

  !Cette routine calcule les densites de flux de CH4 pour un wetlands dont la water table depth est constante dans le temps et situee à 10cm EN-DESSOUS de la surface du sol
  !=> simplification par rapport au modele original (WTD variable) ; les parties inutiles ont ete mises en commentaires quand cela a ete possible (ces parties sont differentes dans stomate_*_ter_0.f90 et stomate_*_ter_m10.f90)
  !Dans cette routine, il y a un calcul d oxydation (sur la partie 0;-10cm)
 
MODULE stomate_wet_ch4_pt_ter_wet4
  ! modules used:
  USE ioipsl
!pss+
!  USE stomate_cste_wetlands
!  USE stomate_constants
  USE constantes
  USE constantes_soil
  USE pft_parameters
!pss-

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC ch4_wet_flux_density_wet4,ch4_wet_flux_density_clear_wet4

    ! first call
    LOGICAL, SAVE                                              :: firstcall = .TRUE.

CONTAINS

  SUBROUTINE ch4_wet_flux_density_clear_wet4
    firstcall=.TRUE.
  END SUBROUTINE ch4_wet_flux_density_clear_wet4


  SUBROUTINE ch4_wet_flux_density_wet4 ( npts,stempdiag,tsurf,tsurf_year,veget_max,veget,&
     & carbon_surf,lai,uo,uold2,ch4_flux_density_tot, ch4_flux_density_dif, & 
     & ch4_flux_density_bub, ch4_flux_density_pla, catm)  


!BR: certaines variables du modele de Walter ne sont pas "consistent" avec
!celles d ORCHIDEE: rprof (root depth, rprof), 
!pr le moment conserve le calcul fait au sein du modele de Walter
!le travail de coéhrence entre les 2 modèles a été fait pour le LAI (j ai enlevé
!le calcul du LAI au sein du modele de Walter)

    !
    ! 0 declarations
    !

    ! 0.1 input

    ! Domain size
    INTEGER(i_std), INTENT(in)                                 :: npts
     ! natural space
    !REAL(r_std), DIMENSION(npts), INTENT(in)                    :: space_nat
    ! Soil temperature (K)
!    REAL(r_std),DIMENSION (npts,nbdl), INTENT (in)              :: tsoil

    REAL(r_std),DIMENSION (npts,nbdl), INTENT (in)              :: stempdiag

    ! "maximal" coverage fraction of a PFT (LAI -> infinity) on nat/agri ground
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: veget_max
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: veget
    ! carbon pool: active, slow, or passive, natural and agricultural (gC/m**2 of
    !   natural or agricultural ground)
    !pss+++++
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(in)        :: carbon_surf
!    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(in)        :: carbon
    !pss-----

    ! temperature (K) at the surface
    REAL(r_std), DIMENSION(npts), INTENT(in)                           :: tsurf

!modif bruno
    ! temperature (K) at the surface
    REAL(r_std), DIMENSION(npts), INTENT(in)                           :: tsurf_year
!end modif bruno

    ! LAI
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)               :: lai


    REAL(r_std),INTENT (in)              :: catm

    ! 0.2 modified fields

!BR: a faire: rajouter le carbone du sol dans les variables modifiees

    REAL(r_std), DIMENSION(npts,nvert), INTENT(inout)                      :: uo
    REAL(r_std), DIMENSION(npts,nvert), INTENT(inout)                      :: uold2

    ! 0.3 output

    ! density flux of methane calculated for entire pixel (gCH4/dt/m**2)
    REAL(r_std), DIMENSION(npts), INTENT(out)             :: ch4_flux_density_tot
    REAL(r_std), DIMENSION(npts), INTENT(out)             :: ch4_flux_density_dif
    REAL(r_std), DIMENSION(npts), INTENT(out)             :: ch4_flux_density_bub
    REAL(r_std), DIMENSION(npts), INTENT(out)             :: ch4_flux_density_pla

    ! 0.4 local

    ! ajout
    REAL(r_std), DIMENSION(npts)                 :: substrat
    REAL(r_std)                                   :: mwater

    ! Index
    INTEGER(i_std)                                              :: i
    INTEGER(i_std)                                              :: j
    INTEGER(i_std), DIMENSION(npts)                            :: j_point    
    INTEGER(i_std)                                              :: ipoint

    !variables appelees dans l ancien Scalc
    !Vcalc.var
    INTEGER(i_std)                               :: nsoil
    INTEGER(i_std)                               :: nroot
!    INTEGER(i_std)                               :: ihelp
    !Cmodel.cb
!new dimension
    REAL(r_std), DIMENSION(npts)                  :: rcmax
    REAL(r_std),DIMENSION(npts, ns)               :: forg
    !on definit uo  dans modified fields: doit etre conserve d une iteration a l autre
    !Cread_carbon.cb
    !new dimension
    INTEGER(i_std),DIMENSION(npts)               :: ibare
    INTEGER(i_std),DIMENSION(npts)               :: iroot
    INTEGER(i_std),DIMENSION(npts)               :: isoil
!new dimension
    REAL(r_std),DIMENSION(npts)                   :: tveg
!new dimension
    REAL(r_std),DIMENSION(npts)                   :: tmean
!    REAL(r_std),DIMENSION(npts)                   :: std
!    REAL(r_std),DIMENSION(npts)                   :: std3
!    REAL(r_std),DIMENSION(npts)                   :: std4
!    REAL(r_std),DIMENSION(npts)                   :: std5
    !new dimension
    REAL(r_std),DIMENSION(npts)                   :: quotient
    REAL(r_std)                                   :: ihelp
!redondance et pas meme dimension    
!new dimension
    REAL(r_std),DIMENSION(npts,nvert)                 :: root
!new dimension
    REAL(r_std),DIMENSION(npts,ns)                 :: sou
    !ancien Soutput
!new dimension
    REAL(r_std),DIMENSION(npts)                                   :: fdiffday
    REAL(r_std),DIMENSION(npts)                                     :: fluxbub
    REAL(r_std),DIMENSION(npts)                                    :: fluxplant
    REAL(r_std),DIMENSION(npts)                                     :: fluxtot
    !ancien Vmodel.var --> s y rapporter pour voir les variables que l on a supprime
!new dimension
    INTEGER(i_std),DIMENSION(npts)               :: nw
    INTEGER(i_std),DIMENSION(npts)               :: n0
    INTEGER(i_std),DIMENSION(npts)               :: n1
    INTEGER(i_std)                               :: n2
    INTEGER(i_std)                               :: n3
!new dimension
    INTEGER(i_std),DIMENSION(npts)               :: nwater
    INTEGER(i_std)                               :: nthaw
!redondance    INTEGER(i_std)                               :: nroot
    INTEGER(i_std),DIMENSION(npts)                               :: j1
    INTEGER(i_std),DIMENSION(npts)                               :: j2
    INTEGER(i_std),DIMENSION(npts)                               :: j3
!new dimension
    INTEGER(i_std),DIMENSION(npts)                               :: ijump
    INTEGER(i_std),DIMENSION(npts)                               :: ndim
    INTEGER(i_std)                               :: iday
!new dimension    
    REAL(r_std),DIMENSION(npts)                   :: rwater
    REAL(r_std)                                   :: diffsw
    REAL(r_std)                                   :: diffsa
    REAL(r_std)                                   :: diffwa
!new dimension    
    REAL(r_std),DIMENSION(npts)                                   :: diffup
    REAL(r_std),DIMENSION(npts)                                   :: diffdo
    REAL(r_std)                                   :: amhalf
    REAL(r_std)                                   :: aphalf
    REAL(r_std)                                   :: source
    REAL(r_std)                                   :: rexp
    REAL(r_std)                                   :: q
    REAL(r_std)                                   :: p
!new dimension    
    REAL(r_std),DIMENSION(npts)                                   :: rvmax
    REAL(r_std)                                   :: aold1
    REAL(r_std)                                   :: aold2
    REAL(r_std)                                   :: aold3
    REAL(r_std)                                   :: aold4
    REAL(r_std)                                   :: diffgrdo
    REAL(r_std)                                   :: diffgrup
    REAL(r_std)                                   :: aoldm2
    REAL(r_std)                                   :: aoldm1
    REAL(r_std)                                   :: aoldn
!new dimension    
    REAL(r_std),DIMENSION(npts)                                   :: negconc
    REAL(r_std)                                   :: cdiff
!new dimension    
    REAL(r_std),DIMENSION(npts)                                   :: tmax
    REAL(r_std)                                   :: cplant
!new dimension    
    REAL(r_std),DIMENSION(npts)                                    :: tplant
!new dimension
    REAL(r_std),DIMENSION(npts)                   :: fgrow
    REAL(r_std)                                   :: cbub
!new dimension
    REAL(r_std),DIMENSION(npts)                                   :: wpro
    REAL(r_std),DIMENSION(npts)                                   :: tout
    REAL(r_std),DIMENSION(npts)                                   :: goxid
    REAL(r_std),DIMENSION(npts)                                   :: dplant
    REAL(r_std)                                   :: fdiffgrad
    REAL(r_std)                                   :: wtdiff
    REAL(r_std)                                   :: uwp0
    REAL(r_std)                                   :: uwp1
    REAL(r_std)                                   :: uwp2
    REAL(r_std)                                   :: xgrow
!new dimension
    REAL(r_std),DIMENSION(npts,nvert,nvert)                          :: a 
    REAL(r_std),DIMENSION(npts,nvert)                                :: b
    REAL(r_std),DIMENSION(npts,nvert)                                :: uout
!new dimension
!    REAL(r_std),DIMENSION(npts,nvert)                           :: uold2
    REAL(r_std),DIMENSION(npts,nvert)                                :: tria
    REAL(r_std),DIMENSION(npts,nvert)                                :: trib
    REAL(r_std),DIMENSION(npts,nvert)                                :: tric
    REAL(r_std),DIMENSION(npts,nvert)                                :: trir
    REAL(r_std),DIMENSION(npts,nvert)                                :: triu
!new dimension
    REAL(r_std),DIMENSION(npts,nvert)                           :: fpart
    REAL(r_std),DIMENSION(npts,nday)                                :: flplant
!new dimension
    REAL(r_std),DIMENSION(npts,nday)                                :: flbub
    REAL(r_std),DIMENSION(npts,nday)                                :: fdifftime
!new dimension
    REAL(r_std),DIMENSION(npts,nday)                                :: flbubdiff
    REAL(r_std),DIMENSION(npts)                               :: fluxbubdiff
    REAL(r_std),DIMENSION(npts)                                :: fbubdiff
    REAL(r_std),DIMENSION(npts)                                :: fluxdiff
!    REAL(r_std)                                                :: pwater

!variable necessaire depuis l incoporation de la subrooutine tridag (cf. fin du programme) au code
    REAL(r_std),DIMENSION(npts,500)                :: gam
    REAL(r_std),DIMENSION(npts)                                     :: bet
!    REAL(r_std),DIMENSION(npts)                                     :: sum_veget
    REAL(r_std),DIMENSION(npts)                                     :: sum_veget_nat
    REAL(r_std),DIMENSION(npts)                                     :: sum_veget_nat_ss_nu
!alpha est le pourcentage de carbone labile pouvant subir la methanogenese
    REAL(r_std),DIMENSION(npts)                                     :: alpha_PFT
    REAL(r_std),DIMENSION(3)                                     :: repart_PFT  !1:boreaux, 2:temperes, 3:tropciaux


!!si je veux rajouter le cas ou je peux avoir de l eau au-dessus du sol 
!=>  je dois modifier la valeur max d indices des boucles  n2,n3=f(ipoint)



    !2   Ancien Sread
    !attribue les variables d'ORCHIDEE a celles de Walter
    sum_veget_nat(:)=0.
    DO ipoint = 1,npts
      DO j=1,nvm
        IF ( natural(j) ) THEN ! for natural PFTs
          sum_veget_nat(ipoint) = sum_veget_nat(ipoint) + veget_max(ipoint,j)
        ENDIF
      END DO
    END DO
    sum_veget_nat_ss_nu(:)=0.
    DO ipoint = 1,npts
      DO j=2,nvm
        IF ( natural(j) ) THEN ! for natural PFTs
          sum_veget_nat_ss_nu(ipoint) = sum_veget_nat_ss_nu(ipoint) + veget_max(ipoint,j)
        ENDIF
      END DO
    END DO
    !l indice de boucle va jusque 11 pour ne prendre en compte que les PFT naturels
    !! a changer en etiquette "naturels"

    !arrondi par defaut (pas de round)
    ibare(:) = INT(100*veget_max(:,1))

    !introduit une variation du alpha selon le type de PFT
!!!!pss adapt pft_to_mtc style, need to know whether PFT is boreal, temperate or
!!!!tropical PFT
    repart_PFT(:) = zero
    DO ipoint = 1,npts   
      DO j = 2,nvm
        IF ( pft_to_mtc(j)==4 .OR. pft_to_mtc(j)==5 .OR. pft_to_mtc(j)==6 ) THEN
        !temperate
        repart_PFT(2) = repart_PFT(2) + veget_max(ipoint,j)
        ELSEIF ( is_tropical(j) ) THEN
        !tropical
        repart_PFT(3) = repart_PFT(3) + veget_max(ipoint,j)
        ELSE
        !boreal
        repart_PFT(1) = repart_PFT(1) + veget_max(ipoint,j)
        ENDIF
      END DO
      IF ((repart_PFT(1) .GT. repart_PFT(2)) .AND. (repart_PFT(1) .GE. repart_PFT(3))) THEN
          alpha_PFT(ipoint)=alpha_CH4(1)
      ELSEIF ((repart_PFT(2) .GT. repart_PFT(1)) .AND. (repart_PFT(2) .GE. repart_PFT(3))) THEN
          alpha_PFT(ipoint)=alpha_CH4(2)
      ELSEIF ((repart_PFT(3) .GE. repart_PFT(1)) .AND. (repart_PFT(3) .GE. repart_PFT(2))) THEN
          alpha_PFT(ipoint)=alpha_CH4(3)
      elseif ((repart_PFT(2) .EQ. repart_PFT(1)) .AND. &
         & ((repart_PFT(1) .GE. repart_PFT(3)) .OR. (repart_PFT(2) .GE. repart_PFT(3)))) THEN
          alpha_PFT(ipoint)=alpha_CH4(1)
      ENDIF   


    END DO

    iroot(:) = 0
    quotient(:) = 0
    DO i=1,11
      DO ipoint = 1,npts
        iroot(ipoint) = iroot(ipoint) + veget_max(ipoint,i)*rdepth_v(i)*tveg_v(i)
        quotient(ipoint) = quotient(ipoint) + veget_max(ipoint,i)*tveg_v(i)
      ENDDO
    END DO

    DO ipoint = 1,npts
      IF ((veget_max(ipoint,1) .LE. 0.95) .AND. (sum_veget_nat_ss_nu(ipoint) .NE. 0.)) THEN
          iroot(ipoint) = iroot(ipoint)/quotient(ipoint)
      ELSE
          iroot(ipoint) = 0.0
      ENDIF
    ENDDO
    !non necessaire de diviser numerateur et denominateur par sum_veget_nat

    isoil(:)=0
    DO i=1,11
      DO ipoint = 1,npts
        isoil(ipoint)=isoil(ipoint)+veget_max(ipoint,i)*sdepth_v(i)
      ENDDO
    ENDDO
    DO ipoint = 1,npts
      IF (sum_veget_nat(ipoint) .NE. 0.) THEN
          isoil(ipoint) = isoil(ipoint)/sum_veget_nat(ipoint)
      ENDIF
    ENDDO


    tveg(:)=0
    DO  i=1,11
      DO ipoint = 1,npts
        tveg(ipoint) = tveg(ipoint)+veget_max(ipoint,i)*tveg_v(i)
      ENDDO
    ENDDO
    DO ipoint = 1,npts
      IF (sum_veget_nat(ipoint) .NE. 0.) THEN
          tveg(ipoint) = tveg(ipoint)/sum_veget_nat(ipoint)
      ENDIF
    ENDDO
    tveg(:) = 0.5*tveg(:)/15.

    !ATTENTION: TRES IMPORTANT
    !il n est pas clair dans la publi de Walter et al. si Tmean varie au cours du temps ou non
    !=>cf. ma these
    !cf. stomate_season.f90         
    DO ipoint = 1,npts
      tmean(ipoint) = tsurf_year(ipoint)-273.16
    ENDDO

    !std,std3,std4,std5: supprimees lors du couplage avec ORCHIDEE => prend directement les temperatures
    !du sol d'ORCHIDEE

!    pwater=-21
    rwater(:) = pwater_wet4
    !nwater = NINT(rwater)
    DO ipoint = 1,npts
      nwater(ipoint) = NINT(rwater(ipoint))
    ENDDO
   
    
    substrat(:) = 0.0
    DO i=1,11
      DO ipoint = 1,npts
        substrat(ipoint) = substrat(ipoint) + veget_max(ipoint,i)*carbon_surf(ipoint,1,i)
      ENDDO
    ENDDO
    DO ipoint = 1,npts
      IF (sum_veget_nat(ipoint) .NE. 0.) THEN
          substrat(ipoint) = substrat(ipoint)/sum_veget_nat(ipoint)
      ENDIF
    ENDDO



!impose valeurs par defaut pour eviter que le modele s arrete dans le cas d un grid-cell 
!où iroot,isoil et tveg seraient nuls
    WHERE ((veget_max(:,1) .GT. 0.95) .OR. (sum_veget_nat_ss_nu(:) .LE. 0.1)) 
        iroot(:)=1*64*1/1
        isoil(:)=129
        tveg(:)=0.5*1/15.
    ENDWHERE

    !************************************
    !**ANCIEN Scalc**********************
    !************************************

    rcmax(:)=scmax+(ibare(:)/100.)*scmax

    forg(:,:)=0.

    DO i=1,ns
        DO ipoint = 1,npts
        nsoil=ns-isoil(ipoint)
        nroot=ns-iroot(ipoint)
        ihelp=(nroot-nsoil)+1
        IF ((i .GE. nsoil) .AND. (i .LE. nroot)) THEN
            ihelp=ihelp-1
            forg(ipoint,i)=EXP((-(ihelp*1.))/10.)
        ELSEIF (i .GE. nroot+1) THEN
            forg(ipoint,i)=1.
        ENDIF
      ENDDO
    ENDDO



!**************ANCIEN Smodel

    !1   Initialisation
    !
    !
    !

    IF ( firstcall ) THEN
        
        ! initialize tridiagonal coefficient matrix 

        b(:,:)=0.
        a(:,:,:)=0.

! initialize fpart(i) (see below)
! Bunsen solubility coefficient (for air-water interface)
! (transformation coefficient for concentration<-->partial pressure

        fpart(:,:)=1.
        
!partie du code initial qui initialisait le profil de concentration en CH4
!-> cf. stomate_io
!dans stomate_io, je ne connais pas rcmax => remplace par scmax
!!$        DO i=1,nvert
!!$          DO ipoint = 1,npts
!!$            mwater=ns+NINT(pwater)
!!$            IF (i .LE. mwater) THEN
!!$                uo(ipoint,i)=rcmax(ipoint)
!!$            ELSE
!!$                uo(ipoint,i)=catm
!!$            ENDIF
!!$            uold2(ipoint,i)=uo(ipoint,i)
!!$          ENDDO
!!$        ENDDO

      !mwater=ns+NINT(pwater)

        firstcall = .FALSE.
    ENDIF

    b(:,:)=0.
    a(:,:,:)=0.
 

    j_point(:)=0
    DO i=1,ns
      DO ipoint = 1,npts
        nroot=ns-iroot(ipoint)
        IF (i .LE. nroot-1) THEN
            !(a): no plant transport below rooting depth nroot
            root(ipoint,i)=0.
        ELSEIF ((i .GE. nroot) .and. (iroot(ipoint) .EQ. 0)) THEN
            root(ipoint,i)=0
        ELSEIF ((i .GE. nroot) .and. (iroot(ipoint) .NE. 0)) THEN
            ! (b): efficiency of plant-mediated transportlinearly increasing with 
            ! decreasing soil depth (gets larger when going up)
            j_point(ipoint)=j_point(ipoint)+1
            root(ipoint,i)=j_point(ipoint)*2./iroot(ipoint)
        ENDIF
      ENDDO
    ENDDO

    ! (c): no plant transport above soil surface
    DO i=ns+1,nvert
      DO ipoint = 1,npts
        root(ipoint,i)=0.
      ENDDO
    ENDDO

!! define vertical root distribution root(ns) for plant-mediated transport
!! (a): no plant transport below rooting depth nroot
!    nroot=ns-iroot
!    DO i=1,nroot-1
!      root(i)=0.
!    ENDDO
!! (b): efficiency of plant-mediated transportlinearly increasing with 
!! decreasing soil depth (gets larger when going up)
!    j=0
!    DO i=nroot,ns
!      IF (iroot .NE. 0) THEN 
!          j=j+1
!          root(i)=j*2./iroot
!      ELSE
!          root(i)=0.
!      ENDIF
!    ENDDO
!! (c): no plant transport above soil surface
!    DO i=ns+1,nvert
!      root(i)=0.
!    ENDDO

!ancienne subroutine Sinter**********************************************
! interpolates Tsoil ==> value for each cm
! interpolate soil temperature from input soil layers to 1cm-thick
! model soil layers ==> sou(i)


!plus d interpolation necessaire entre les temperatures du sol de differents niveaux mises en entree du modele de Walter
!met directement les temperatures des differentes couches de sol d'ORCHIDEE
!ATTENTION si dpu_cste varie!? => a changer
    DO i=1,75
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,10)-273.16
      ENDDO
    ENDDO
    DO i=76,113
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,9)-273.16
      ENDDO
    ENDDO
    DO i=114,131
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,8)-273.16
      ENDDO
    ENDDO
    DO i=132,141
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,7)-273.16
      ENDDO
    ENDDO
    DO i=142,146
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,6)-273.16
      ENDDO
    ENDDO
    DO i=147,148
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,5)-273.16
      ENDDO
    ENDDO

    sou(:,149)=stempdiag(:,4)-273.16

    DO i=150,ns
      DO ipoint = 1,npts
        sou(ipoint,i)=stempdiag(ipoint,3)-273.16
      ENDDO
    ENDDO



!!$    DO i=1,71
!!$      DO ipoint = 1,npts
!!$        sou(ipoint,i)=std(ipoint)+((193.+i)/265.)*(std5(ipoint)-std(ipoint))
!!$      ENDDO
!!$    ENDDO
!!$
!!$    j=0
!!$    DO i=72,137
!!$      j=j+1
!!$      DO ipoint = 1,npts
!!$        sou(ipoint,i)=std5(ipoint)+((j-1.)/66.)*(std4(ipoint)-std5(ipoint))
!!$      ENDDO
!!$    ENDDO
!!$
!!$    j=0
!!$    DO i=138,149
!!$      j=j+1
!!$      DO ipoint = 1,npts
!!$        sou(ipoint,i)=std4(ipoint)+((j-1.)/12.)*(std3(ipoint)-std4(ipoint))
!!$      ENDDO
!!$    ENDDO
!!$
!!$    DO i=150,ns
!!$      DO ipoint = 1,npts
!!$        sou(ipoint,i)=std3(ipoint)
!!$      ENDDO
!!$    ENDDO


!!ancienne subroutine Sinter**********************************************
!! interpolates Tsoil ==> value for each cm
!! interpolate soil temperature from input soil layers to 1cm-thick
!! model soil layers ==> sou(i)
!       do i=1,71
!          sou(i)=std+((193.+i)/265.)*(std5-std)
!       enddo
!       j=0
!       do i=72,137
!          j=j+1
!          sou(i)=std5+((j-1.)/66.)*(std4-std5)
!       enddo
!       j=0
!       do i=138,149
!          j=j+1
!          sou(i)=std4+((j-1.)/12.)*(std3-std4)
!       enddo
!       do i=150,ns
!          sou(i)=std3
!       enddo
!!************************************************************************

!utilise le LAI calcule par ORCHIDEE plutot que de calculer son propre LAI comme dans le modele
!de Walter initial

! define fgrow: growing stage of plants (LAI, after (Dickinson, 1993))
! different thresholds for regions with different annual mean temperature 
! tmean
!       if (tmean .le. 5.) then
!          xgrow=2.
!       else
!          xgrow=7.
!       endif
!       if (sou(101) .le. xgrow) then
!          fgrow=0.0001
!       else
!! maximum for regions with large tmean
!          if (sou(101) .ge. (xgrow+10.)) then
!             fgrow=4.
!          else
!             fgrow=0.0001+3.999*(1-((((xgrow+10.)-sou(101))/10.)**2.))
!          endif
!      endif

    !WRITE(*,*) 'veget', veget
    !WRITE(*,*) 'veget_max', veget_max
        
    fgrow(:) = 0.0
    DO i=1,11
      DO ipoint = 1,npts
        fgrow(ipoint) = fgrow(ipoint) + veget_max(ipoint,i)*lai(ipoint,i)
      ENDDO
    ENDDO
    DO ipoint = 1,npts
      IF (sum_veget_nat(ipoint) .NE. 0.) THEN
          fgrow(ipoint) = fgrow(ipoint)/sum_veget_nat(ipoint)
      ELSE
          fgrow(ipoint) = 0.0
      ENDIF
    ENDDO


! calculation of n0,n1,n2 / determine coefficients of diffusion
! n0: lower boundary (soil depth or thaw depth)
! n1: water table (if water<soil surface), else soil surface
! n2: soil surface ( "     "        "   ), "    water table
! n3: upper boundary (fixed: n3=n2+4)
! global model: NO thaw depth (set nthaw=max soil depth [=150])
! for 1-dim model: read thaw depth in Sread.f & comment line 'nthaw=150' out
! instead write: 'nthat=pthaw(itime)'
      !nthaw=150
    nthaw=150 !modifie nthaw car j ai modiife ns dans stomate_cste_wetlands.f90

    DO ipoint = 1,npts
      n0(ipoint)=ns-MIN(isoil(ipoint),nthaw)
      nw(ipoint)=MAX(n0(ipoint),ns+nwater(ipoint))
      nw(ipoint)=MIN(nw(ipoint),(nvert-4))
    ENDDO


    DO ipoint = 1,npts ! boucle la plus externe

!le cas suivant n arrive jamais!!!: la WTD est toujours en-dessous ou au niveau du sol 
!au niveau de la vectorisation=>n2 vaut toujours ns
! if water table is above soil surface
!      IF (nw(ipoint) .GT. ns) THEN
!          n1(ipoint)=ns
!          n2(ipoint)=nw(ipoint)
!          diffdo(ipoint)=diffsw
!          diffup(ipoint)=diffwa
!          rvmax(ipoint)=0.
!      ENDIF

!!!psstry+
       ! define diffusion coefficients (after Penman)
       ! soil air
         diffsa=diffair*0.66*rpv
       ! soil water
         diffsw=0.0001*diffsa
       ! standing water
         diffwa=0.0001*diffair
!!!psstry-

! if water table is below soil surface
      IF (nw(ipoint) .LT. ns) THEN
          n1(ipoint)=nw(ipoint)
          n2=ns
          diffdo(ipoint)=diffsw
          diffup(ipoint)=diffsa
          rvmax(ipoint)=xvmax
      ENDIF

      !WRITE(*,*) 'n2','ipoint',ipoint, n2, n0(ipoint), ns, n1(ipoint), n3

!!$! if water table is at soil surface
!!$      IF (nw(ipoint) .EQ. ns) THEN
!!$          n1(ipoint)=nw(ipoint)
!!$          n2=nw(ipoint)
!!$          diffdo(ipoint)=diffsw
!!$          diffup(ipoint)=diffsw
!!$          rvmax(ipoint)=0.    
!!$      endif
    ENDDO

    !n3 est constant finalement car pour tous les pixels n2=ns et ns=171(ou151)
    n3=n2+4

    !ijump(:)=1
    DO ipoint = 1,npts
! make sure that n0<n1<n2
      IF ((n0(ipoint) .EQ. n2) .OR. (n0(ipoint)+1 .EQ. n2)) THEN
          ijump(ipoint)=0
      ELSE
          ijump(ipoint)=1
      ENDIF
      IF ((n1(ipoint) .EQ. n0(ipoint)) .AND. (n2 .GT. n0(ipoint)+1)) THEN
          n1(ipoint)=n0(ipoint)+1
          diffdo(ipoint)=diffup(ipoint)
          sou(ipoint,n0(ipoint))=0.
          sou(ipoint,n1(ipoint))=0.
      ENDIF
    ENDDO


! Bunsen solubility coefficient (air-water interface)
! (transformation coefficient for concentration<-->partial pressure
! transformtion in water (do i=1,nw) and air (do i=nw+1,n))

!inverse l ordre des boucles pour ameliorer le vectorissation
    DO i=1,nvert
      DO ipoint = 1,npts
        IF (i .LE. nw(ipoint))THEN
            fpart(ipoint,i)=1.
        ELSE
            fpart(ipoint,i)=1./23.
        ENDIF
      ENDDO
    ENDDO


!    DO i=1,nw
!      fpart(i)=1.
!    ENDDO
!    DO i=nw+1,nvert
!      fpart(i)=1./23.
!    ENDDO



! calculate total daily production [wpro] / oxidation rate [goxid], 
! difference in conc(day)-conc(day-1) [tout] / 
! daily amount of CH4 transported through plants [dplant]
! necessary for calculation of diffusive flux from mass balance
    DO ipoint = 1,npts
      wpro(ipoint)=0.
      goxid(ipoint)=0.
      tout(ipoint)=0.
      dplant(ipoint)=0.
      negconc(ipoint)=0.
    ENDDO

! if water table falls (i.e. nw (new water tabel) < nwtm1 (old water table)):
    wtdiff=0.
!supprime boucle if sur ijump: dans l approche choisie WTD=cste au cours du temps
!perform calculation only if n0<n1<n2
!!!      IF (ijump .EQ. 1) THEN

! update old u-values after change of water table/thaw depth
! water-air interface: conc ==> partial pressure
! transform from concentration into partial pressure unit

    DO i=1,n3   
      DO ipoint = 1,npts
        IF (i .GE. n0(ipoint)) THEN
            uo(ipoint,i)=uo(ipoint,i)*fpart(ipoint,i) 
        ENDIF
      ENDDO
    ENDDO
    


!
! solve diffusion equation (Crank-Nicholson)
! Schwarz, Numerische Mathematik, Teubner, Stuttgart, p.476)
! Press et al., Numerical Recepies, FORTRAN, p. 838)
!*************************************************************

! left hand side coefficient matrix:
!************************************

! left boundary


    p=0.
    DO ipoint = 1,npts
      amhalf=diffdo(ipoint)
      aphalf=diffdo(ipoint)
      a(ipoint,n0(ipoint),n0(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
      a(ipoint,n0(ipoint),n0(ipoint)+1)=-rkh*(aphalf+amhalf)
    ENDDO


! inner points
    DO ipoint = 1,npts
      j1(ipoint)=n0(ipoint)-1
      j2(ipoint)=j1(ipoint)+1
      j3(ipoint)=j2(ipoint)+1 
    ENDDO

    DO  i=1,n2 !idem que precedement: sur-calcul
      DO ipoint = 1,npts
        IF ((i .GE. n0(ipoint)+1) .AND. (i .LE. n1(ipoint)-1)) THEN
            j1(ipoint)=j1(ipoint)+1
            j2(ipoint)=j2(ipoint)+1
            j3(ipoint)=j3(ipoint)+1
            amhalf=diffdo(ipoint)
            aphalf=diffdo(ipoint)
            p=0.
            a(ipoint, i,j1(ipoint))=-rkh*amhalf
            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
            a(ipoint,i,j3(ipoint))=-rkh*aphalf
        ENDIF
      ENDDO
    ENDDO
  
!      j1=n0-1
!      j2=j1+1
!      j3=j2+1      
!      do i=n0+1,n1-1
!         j1=j1+1
!         j2=j2+1
!         j3=j3+1
!         amhalf=diffdo
!         aphalf=diffdo
!         p=0.
!         a(i,j1)=-rkh*amhalf
!         a(i,j2)=2.+rkh*(amhalf+aphalf-h**2*p)
!         a(i,j3)=-rkh*aphalf
!      enddo


    diffgrdo=0.57*0.66*rpv
    diffgrup=0.0248*0.66*rpv

!les parties suivantes sont differentes entre stomate_wet_ch4_pt_ter_0 et stomate_wet_ch4_pt_ter_m10

    DO i = 1,n3-1
      !sur-calcul: au depart j avais  = n0,n3-1
      !mais les nouvelles bornes choisies permettent d ameliorer le vectorisation
      DO ipoint = 1,npts

        !1er CAS: (nw(ipoint) .LT. ns-1)
        !diffusion coefficients water-air INTERFACE (soil water --> soil air)        
        !par defaut ici on a toujours (nw(ipoint) .LT. ns-1)

        IF (i .EQ. n1(ipoint)) THEN
            !i=n1
            j1(ipoint)=n1(ipoint)-1
            j2(ipoint)=n1(ipoint)
            j3(ipoint)=n1(ipoint)+1
            amhalf=diffdo(ipoint)
            aphalf=diffgrdo
            p=0.
            a(ipoint,i,j1(ipoint))=-rkh*amhalf
            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
            a(ipoint,i,j3(ipoint))=-rkh*aphalf 
        ELSEIF (i .EQ. n1(ipoint)+1) THEN
            !i=n1+1
            j1(ipoint)=n1(ipoint)
            j2(ipoint)=n1(ipoint)+1
            j3(ipoint)=n1(ipoint)+2
            amhalf=diffgrup
            aphalf=diffup(ipoint)
            p=0.
            a(ipoint,i,j1(ipoint))=-rkh*amhalf
            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
            a(ipoint,i,j3(ipoint))=-rkh*aphalf 
        ELSEIF ((i .GE. n1(ipoint)+2) .AND. (i .LE. n2-1)) THEN            
            !DO i=n1+2,n2-1
            j1(ipoint)=j1(ipoint)+1
            j2(ipoint)=j2(ipoint)+1
            j3(ipoint)=j3(ipoint)+1
            amhalf=diffup(ipoint)
            aphalf=diffup(ipoint)
            p=0.
            a(ipoint,i,j1(ipoint))=-rkh*amhalf
            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
            a(ipoint,i,j3(ipoint))=-rkh*aphalf
            !ENDDO        
        ELSEIF (i .EQ. n2) THEN
            !i=n2
            j1(ipoint)=n2-1
            j2(ipoint)=n2
            j3(ipoint)=n2+1
            amhalf=diffup(ipoint)
            aphalf=diffair
            p=0.
            a(ipoint,i,j1(ipoint))=-rkh*amhalf
            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
            a(ipoint,i,j3(ipoint))=-rkh*aphalf          
        ELSEIF ((i .GE. n2+1) .AND. (i .LE. n3-1)) THEN
            !DO i=n2+1,n3-1
            j1(ipoint)=j1(ipoint)+1
            j2(ipoint)=j2(ipoint)+1
            j3(ipoint)=j3(ipoint)+1
            amhalf=diffair
            aphalf=diffair
            p=0.
            a(ipoint,i,j1(ipoint))=-rkh*amhalf
            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
            a(ipoint,i,j3(ipoint))=-rkh*aphalf
            !ENDDO

!!$            !2eme CAS: (nw .EQ. ns-1)
!!$            ! diffusion coefficients water-air interface (soil water --> soil air)
!!$        ELSEIF ((nw(ipoint) .EQ. ns-1) .AND. (i .EQ. n1(ipoint))) THEN       
!!$            !i=n1
!!$            j1(ipoint)=n1(ipoint)-1
!!$            j2(ipoint)=n1(ipoint)
!!$            j3(ipoint)=n1(ipoint)+1
!!$            amhalf=diffdo(ipoint)
!!$            aphalf=diffgrdo
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf                
!!$        ELSEIF ((nw(ipoint) .EQ. ns-1) .AND. (i .EQ. n2)) THEN    
!!$            !i=n2
!!$            j1(ipoint)=n2-1
!!$            j2(ipoint)=n2
!!$            j3(ipoint)=n2+1
!!$            amhalf=diffgrup
!!$            aphalf=diffup(ipoint)
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf         
!!$        ELSEIF ((nw(ipoint) .EQ. ns-1) .AND. (i .EQ. n2+1)) THEN
!!$            !i=n2+1
!!$            j1(ipoint)=n2
!!$            j2(ipoint)=n2+1
!!$            j3(ipoint)=n2+2
!!$            amhalf=diffup(ipoint)
!!$            aphalf=diffair
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf
!!$        ELSEIF  ((nw(ipoint) .EQ. ns-1) .AND. (i .GE. n2+2) .AND. (i .LE. n3-1)) THEN  
!!$            !DO i=n2+2,n3-1
!!$            j1(ipoint)=j1(ipoint)+1
!!$            j2(ipoint)=j2(ipoint)+1
!!$            j3(ipoint)=j3(ipoint)+1
!!$            amhalf=diffair
!!$            aphalf=diffair
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf
!!$            !enddo
!!$
!!$            !3eme CAS: (nw .EQ. ns)
!!$            ! diffusion coefficients water-air interface (soil water --> soil air)
!!$        ELSEIF ((nw(ipoint) .EQ. ns) .AND. (i .EQ. n1(ipoint))) THEN
!!$            !i=n1
!!$            j1(ipoint)=n1(ipoint)-1
!!$            j2(ipoint)=n1(ipoint)
!!$            j3(ipoint)=n1(ipoint)+1
!!$            amhalf=diffdo(ipoint)
!!$            aphalf=diffgrdo
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf 
!!$        ELSEIF ((nw(ipoint) .EQ. ns) .AND. (i .EQ. n1(ipoint)+1)) THEN
!!$            !i=n1+1
!!$            j1(ipoint)=n1(ipoint)
!!$            j2(ipoint)=n1(ipoint)+1
!!$            j3(ipoint)=n1(ipoint)+2
!!$            amhalf=diffgrup
!!$            aphalf=diffair
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf              
!!$        ELSEIF ((nw(ipoint) .EQ. ns) .AND. (i .GE. n1(ipoint)+2) .AND. (i .LE. n3-1)) THEN
!!$            ! do i=n1+2,n3-1
!!$            j1(ipoint)=j1(ipoint)+1
!!$            j2(ipoint)=j2(ipoint)+1
!!$            j3(ipoint)=j3(ipoint)+1
!!$            amhalf=diffair
!!$            aphalf=diffair
!!$            p=0.
!!$            a(ipoint,i,j1(ipoint))=-rkh*amhalf
!!$            a(ipoint,i,j2(ipoint))=2.+rkh*(amhalf+aphalf-h**2*p)
!!$            a(ipoint,i,j3(ipoint))=-rkh*aphalf
!!$            !ENDDO
            
            !4eme CAS: (nw .GE. ns): non traite dans ORCHIDEE! la  WTD n est jamais au-dessus de la surface
            !de toute facon pas d oxydation dans ce cas dans le modele de Wlater (juste modif du transport)
        ENDIF
      ENDDO
    ENDDO


! right boundary

    DO ipoint = 1,npts
      amhalf=diffair
      aphalf=diffair
      p=0.
      a(ipoint,n3,n3-1)=-rkh*amhalf
      a(ipoint,n3,n3)=2.+rkh*(amhalf+aphalf-h**2*p)
    ENDDO
    
        
!
! begin of time loop (time steps per day)
!*****************************************

      DO iday=1,nday



!*****************************************

! right hand side coefficient matrix:
!************************************

! left boundary
   
        DO ipoint = 1,npts
          i=n0(ipoint)
          amhalf=diffdo(ipoint)
          aphalf=diffdo(ipoint)
          p=0.
! q10 dependent production rate (no production if T<0oC, i.e.sou(i)<=0oC)
! forg(i): vertical distribution of substrate in soil (calculated in Scalc.f)
! fin(itime): describes seasonality of NPP
! rq10: Q10 value of production rate (from para.h)
! r0pl: tuning parameter sr0pl (scaled in Scalc.f)
          IF (sou(ipoint,n0(ipoint)) .GT. 0.) THEN
              rexp=(sou(ipoint,n0(ipoint))-MAX(0.,tmean(ipoint)))/10.
!             source=forg(n0)*r0pl*(rq10**rexp)*fin(itime)
!             source=forg(n0)*(rq10**rexp)*carbon(itime)*(0.001239567)
!             source=forg(n0)*(rq10**rexp)*carbon(itime)*r0pl*(0.0012)
!             source=5.0*(rq10**rexp)
              source=forg(ipoint,n0(ipoint))*(rq10**rexp)*substrat(ipoint)*alpha_PFT(ipoint)
          ELSE
              source=0.
          ENDIF
          q=source
          wpro(ipoint)=wpro(ipoint)+q
! transform from concentration into partial pressure unit
          q=q*fpart(ipoint,i)
          aold1=2.-rkh*(amhalf+aphalf-h**2*p)
          aold2=rkh*(aphalf+amhalf)
          aold3=2.*rk*q
          b(ipoint,n0(ipoint))=aold1*uo(ipoint,n0(ipoint))+aold2*uo(ipoint,n0(ipoint)+1)+aold3
        ENDDO

        !DO ipoint =1,npts
        !  IF (iday .EQ. 2) THEN
        !      WRITE(*,*) 'matrice b apres left boundary',b(ipoint,:)
        !  ENDIF
        !ENDDO


! inner points (from left to right)

        !do i=n0+1,n1-1
        DO i = 1,n2 !sur calcul
          DO ipoint = 1,npts
            IF ((i .GE. n0(ipoint)+1) .AND. (i .LE. n1(ipoint)-1)) THEN
                amhalf=diffdo(ipoint)
                aphalf=diffdo(ipoint)
                p=0.
! q10 dependent production rate (no production if T<0oC, i.e.sou(i)<=0oC)
! forg(i): vertical distribution of substrate in soil (calculated in Scalc.f)
! fin(itime): describes seasonality of NPP
! rq10: Q10 value of production rate (from para.h)
! r0pl: tuning parameter sr0pl (scaled in Scalc.f)
                IF (sou(ipoint,i) .GT. 0.) THEN
                    rexp=(sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.
!              source=forg(i)*r0pl*(rq10**rexp)*fin(itime)
!              source=forg(i)*(rq10**rexp)*carbon(itime)*(0.001239567)
                    !modifie le calcul de la production --> utilise le carbone comme proxi du substrat
                    source=forg(ipoint,i)*(rq10**rexp)*substrat(ipoint)*alpha_PFT(ipoint) 
                ELSE
                    source=0.
                ENDIF
                q=source
                wpro(ipoint)=wpro(ipoint)+q
! transform from concentration into partial pressure unit
                q=q*fpart(ipoint,i)
                aold1=rkh*amhalf
                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
                aold3=rkh*aphalf
                aold4=2.*rk*q
                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
            ENDIF
          ENDDO
        ENDDO

        !DO ipoint =1,npts
        !  IF (iday .EQ. 2) THEN
        !      WRITE(*,*) 'matrice b apres inner point avant cas',b(ipoint,:)
        !  ENDIF
        !ENDDO

        diffgrdo=0.57*0.66*rpv
        diffgrup=0.0248*0.66*rpv

!toujours vrai ici: nw(ipoint) .LT. ns-1)

        DO i = 1,n3-1          !sur-calcul: au depart j avais  i = n0,n3-1
          DO ipoint = 1,npts

            !1er CAS: (nw(ipoint) .LT. ns-1)
            !diffusion coefficients water-air INTERFACE (soil water --> soil air)
!!$            IF ((nw(ipoint) .LT. ns-1) .AND. (i .EQ. n1(ipoint))) THEN
            
           IF (i .EQ. n1(ipoint)) THEN
 ! diffusion coefficients water-air interface (soil water --> soil air)
                !i=n1
                amhalf=diffdo(ipoint)
                aphalf=diffgrdo
                p=0.
! q10 dependent production rate (no production if T<0oC, i.e.sou(i)<=0oC)
! forg(i): vertical distribution of substrate in soil (calculated in Scalc.f)
! fin(itime): describes seasonality of NPP
! rq10: Q10 value of production rate (from para.h)
! r0pl: tuning parameter sr0pl (scaled in Scalc.f)
                IF (sou(ipoint,i) .GT. 0.) THEN
                    rexp=(sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.
                    !source=forg(i)*r0pl*(rq10**rexp)*fin(itime)
                    source=forg(ipoint,i)*(rq10**rexp)*substrat(ipoint)*alpha_PFT(ipoint)
                ELSE
                    source=0.
                ENDIF
                q=source
                wpro(ipoint)=wpro(ipoint)+q
! transform from concentration into partial pressure unit
                q=q*fpart(ipoint,i)
                aold1=rkh*amhalf
                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
                aold3=rkh*aphalf     
                aold4=2.*rk*q
                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
            

            ELSEIF (i .EQ. n1(ipoint)+1) THEN
                !i=n1+1
                amhalf=diffgrup
                aphalf=diffup(ipoint)
                p=0.
! oxidation following Michaelis-Menten kinetics
                IF (uo(ipoint,i) .GT. 0.) THEN
! transform from partial pressure into concentration unit 
                    uo(ipoint,i)=uo(ipoint,i)/fpart(ipoint,i)
! Michaelis Menten Equation
                    q=-rvmax(ipoint)*uo(ipoint,i)/(uo(ipoint,i)+rkm)
! temperature dependence (oxq10 from para.h)
                    q=(q/oxq10)*(oxq10**((sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.))
                    goxid(ipoint)=goxid(ipoint)+q
! transform from concentration into partial pressure unit
                    uo(ipoint,i)=uo(ipoint,i)*fpart(ipoint,i)
! in case of negative concentrations: no oxidation
! (can occur if number of time steps per day (nday) is small and water
! table varies strongly) (help: increase nday (or change numerical scheme!))
                ELSE
                    q=0.
                    goxid(ipoint)=goxid(ipoint)+q
                ENDIF
! transform from concentration into partial pressure unit
                q=q*fpart(ipoint,i)
                aold1=rkh*amhalf
                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
                aold3=rkh*aphalf     
                aold4=2.*rk*q
!!!! pssdebug
!                write(2, *) 'pssdebug, i, aold1, aold2, aold3, aold4, uo(ipoint, i-1), uo(ipoint, i), uo(ipoint, i+1)', i, aold1, aold2, aold3, aold4, uo(ipoint, i-1), uo(ipoint, i), uo(ipoint, i+1)
!!!! pssdebug
                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
                    

            ELSEIF ((i .GE. n1(ipoint)+2) .AND. (i .LE. n2-1)) THEN
                !do i=n1+2,n2-1
                amhalf=diffup(ipoint)
                aphalf=diffup(ipoint)
                p=0.
! oxidation following Michaelis-Menten kinetics
                IF (uo(ipoint,i) .GT. 0.) THEN
! transform from partial pressure into concentration unit
                    uo(ipoint,i)=uo(ipoint,i)/fpart(ipoint,i)
! Michaelis Menten Equation
                    q=-rvmax(ipoint)*uo(ipoint,i)/(uo(ipoint,i)+rkm)
! temperature dependence (oxq10 from para.h)
                    q=(q/oxq10)*(oxq10**((sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.))
                    goxid(ipoint)=goxid(ipoint)+q
! transform from concentration into partial pressure unit
                    uo(ipoint,i)=uo(ipoint,i)*fpart(ipoint,i)
! in case of negative concentrations: no oxidation
! (can occur if number of time steps per day (nday) is small and water
! table varies strongly) (help: increase nday (or change numerical scheme!))
                ELSE
                    q=0.
                    goxid(ipoint)=goxid(ipoint)+q
                ENDIF
! transform from concentration into partial pressure unit
                q=q*fpart(ipoint,i)
                aold1=rkh*amhalf
                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
                aold3=rkh*aphalf
                aold4=2.*rk*q
                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4

            ELSEIF (i .EQ. n2) THEN
                !i=n2
                amhalf=diffup(ipoint)
                aphalf=diffair
                p=0.
! oxidation following Michaelis-Menten kinetics
                IF (uo(ipoint,i) .GT. 0.) THEN
! transform from partial pressure into concentration unit
                    uo(ipoint,i)=uo(ipoint,i)/fpart(ipoint,i)
! Michaelis Menten Equation
                    q=-rvmax(ipoint)*uo(ipoint,i)/(uo(ipoint,i)+rkm)
! temperature dependence (oxq10 from para.h)
                    q=(q/oxq10)*(oxq10**((sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.))
                    goxid(ipoint)=goxid(ipoint)+q
! transform from concentration into partial pressure unit
                    uo(ipoint,i)=uo(ipoint,i)*fpart(ipoint,i)
! in case of negative concentrations: no oxidation
! (can occur if number of time steps per day (nday) is small and water
! table varies strongly) (help: increase nday (or change numerical scheme!))
                ELSE
                    q=0.
                    goxid(ipoint)=goxid(ipoint)+q
                ENDIF
! transform from concentration into partial pressure unit
                q=q*fpart(ipoint,i)
                aold1=rkh*amhalf
                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
                aold3=rkh*aphalf     
                aold4=2.*rk*q
                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4

            ELSEIF ((i .GE. n2+1) .AND. (i .LE. n3-1)) THEN
                !DO i=n2+1,n3-1
                amhalf=diffair
                aphalf=diffair
                p=0.
                q=0.
                aold1=rkh*amhalf
                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
                aold3=rkh*aphalf
                aold4=2.*rk*q
                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4

            ENDIF
          ENDDO
        ENDDO

!!$
!!$
!!$        DO i = 1,n3-1          !sur-calcul: au depart j avais mis i = n0,n3-1
!!$          DO ipoint = 1,npts
!!$
!!$            !2nd CAS: (nw(ipoint) .EQ. ns)
!!$            !diffusion coefficients water-air INTERFACE (soil water --> soil air)
!!$            IF ((nw(ipoint) .EQ. ns) .AND. (i .EQ. n1(ipoint))) THEN
!!$                !i=n1
!!$                amhalf=diffdo(ipoint)
!!$                aphalf=diffgrdo
!!$                p=0.
!!$! q10 dependent production rate (no production if T<0oC, i.e.sou(i)<=0oC)
!!$! forg(i): vertical distribution of substrate in soil (calculated in Scalc.f)
!!$! fin(itime): describes seasonality of NPP
!!$! rq10: Q10 value of production rate (from para.h)
!!$! r0pl: tuning parameter sr0pl (scaled in Scalc.f)
!!$                IF (sou(ipoint,i) .GT. 0.) THEN
!!$                    rexp=(sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.
!!$!              source=forg(i)*r0pl*(rq10**rexp)*fin(itime)
!!$!              source=forg(i)*(rq10**rexp)*carbon(itime)*(0.001239567)
!!$!            source=5.0*(rq10**rexp)
!!$                    source=forg(ipoint,i)*(rq10**rexp)*substrat(ipoint)*alpha_CH4
!!$                ELSE
!!$                    source=0.
!!$                ENDIF
!!$                q=source
!!$                wpro(ipoint)=wpro(ipoint)+q
!!$! transform from concentration into partial pressure unit
!!$                q=q*fpart(ipoint,i)
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf     
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$
!!$            ELSEIF ((nw(ipoint) .EQ. ns) .AND. (i .EQ. n1(ipoint)+1)) THEN
!!$                !i=n1+1
!!$                amhalf=diffgrup
!!$                aphalf=diffair
!!$                p=0.
!!$                q=0.
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf     
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$           
!!$            ELSEIF ((nw(ipoint) .EQ. ns) .AND. (i .GE. n1(ipoint)+2) .AND. (i .LE. n3-1)) THEN
!!$                !DO i=n1+2,n3-1
!!$                amhalf=diffair
!!$                aphalf=diffair
!!$                p=0.
!!$                q=0.
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$                !ENDDO
!!$
!!$            ENDIF
!!$          ENDDO
!!$        ENDDO
!!$
!!$



!!$        DO i = 1,n3-1          !sur-calcul: au depart j avais mis i = n0,n3-1
!!$          DO ipoint = 1,npts
!!$
!!$            !3eme CAS: (nw(ipoint) .EQ. ns-1)
!!$            !diffusion coefficients water-air INTERFACE (soil water --> soil air)
!!$            IF ((nw(ipoint) .EQ. ns-1) .AND. (i .EQ. n1(ipoint))) THEN
!!$                !i=n1
!!$                amhalf=diffdo(ipoint)
!!$                aphalf=diffgrdo
!!$                p=0.
!!$! q10 dependent production rate (no production if T<0oC, i.e.sou(i)<=0oC)
!!$! forg(i): vertical distribution of substrate in soil (calculated in Scalc.f)
!!$! fin(itime): describes seasonality of NPP
!!$! rq10: Q10 value of production rate (from para.h)
!!$! r0pl: tuning parameter sr0pl (scaled in Scalc.f)
!!$                IF (sou(ipoint,i) .GT. 0.) THEN
!!$                    rexp=(sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.
!!$                    !source=forg(i)*r0pl*(rq10**rexp)*fin(itime)
!!$                    source=forg(ipoint,i)*(rq10**rexp)*substrat(ipoint)*alpha_CH4
!!$                ELSE
!!$                    source=0.
!!$                ENDIF
!!$                q=source
!!$                wpro(ipoint)=wpro(ipoint)+q
!!$! transform from concentration into partial pressure unit
!!$                q=q*fpart(ipoint,i)
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf     
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$
!!$
!!$            ELSEIF ((nw(ipoint) .EQ. ns-1) .AND. (i .EQ. n2)) THEN
!!$                !i=n2
!!$                amhalf=diffgrup
!!$                aphalf=diffup(ipoint)
!!$                p=0.
!!$! oxidation following Michaelis-Menten kinetics
!!$                IF (uo(ipoint,i) .GT. 0.) THEN
!!$! transform from partial pressure into concentration unit
!!$                    uo(ipoint,i)=uo(ipoint,i)/fpart(ipoint,i)
!!$! Michaelis Menten Equation
!!$                    q=-rvmax(ipoint)*uo(ipoint,i)/(uo(ipoint,i)+rkm)
!!$! temperature dependence (oxq10 from para.h)
!!$                    q=(q/oxq10)*(oxq10**((sou(ipoint,i)-MAX(0.,tmean(ipoint)))/10.))
!!$                    goxid(ipoint)=goxid(ipoint)+q
!!$! transform from concentration into partial pressure unit
!!$                    uo(ipoint,i)=uo(ipoint,i)*fpart(ipoint,i)
!!$! in case of negative concentrations: no oxidation
!!$! (can occur if number of time steps per day (nday) is small and water
!!$! table varies strongly) (help: increase nday (or change numerical scheme!))
!!$                ELSE  
!!$                    q=0.
!!$                    goxid(ipoint)=goxid(ipoint)+q
!!$                ENDIF
!!$                ! transform from concentration into partial pressure unit
!!$                q=q*fpart(ipoint,i)
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf     
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$
!!$            ELSEIF ((nw(ipoint) .EQ. ns-1) .AND. (i .EQ. n2+1)) THEN
!!$                !i=n2+1
!!$                amhalf=diffup(ipoint)
!!$                aphalf=diffair
!!$                p=0.
!!$                q=0.
!!$                goxid(ipoint)=goxid(ipoint)+q
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf     
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$                
!!$            ELSEIF ((nw(ipoint) .EQ. ns-1) .AND. (i .GE. n2+2) .AND. (i .LE. n3-1)) THEN             
!!$                
!!$                !DO i=n2+2,n3-1
!!$                amhalf=diffair
!!$                aphalf=diffair
!!$                p=0.
!!$                q=0.
!!$                aold1=rkh*amhalf
!!$                aold2=2.-rkh*(amhalf+aphalf-h**2*p)
!!$                aold3=rkh*aphalf
!!$                aold4=2.*rk*q
!!$                b(ipoint,i)=aold1*uo(ipoint,i-1)+aold2*uo(ipoint,i)+aold3*uo(ipoint,i+1)+aold4
!!$                !ENDDO
!!$
!!$            ENDIF
!!$          ENDDO
!!$        ENDDO

        !4eme CAS: (nw .GE. ns): non traite! ma water table ne va jamais au-dessus de la
        !surface en ce qui me concerne. de toute facon, jamais d oxydation dans Walter dans 
        !la colonne d eau au-dessus de la surface: cala modifie juste le transport
        


        !DO ipoint =1,npts
        !  IF (iday .EQ. 2) THEN
        !      WRITE(*,*) 'matrice b apres inner point apres cas',b(ipoint,:)
        !  ENDIF
        !ENDDO

! right boundary
        DO ipoint = 1,npts
          amhalf=diffair
          aphalf=diffair
          p=0.
          q=0.
          aoldm2=rkh*amhalf
          aoldm1=2.-rkh*(amhalf+aphalf-h**2*p)
          aoldn=2.*(rkh*aphalf*catm+rk*q)
          b(ipoint,n3)=aoldm2*uo(ipoint,n3-1)+aoldm1*uo(ipoint,n3)+aoldn
        ENDDO




!*********************************************************
! solution of tridiagonal system using subroutine tridag
! calculate NEW concentration profiles uo(i)


        !DO i=n0,n3
        DO i=1,n3
          DO ipoint =1,npts
            IF (i .GE. n0(ipoint)) THEN
                tria(ipoint,i-n0(ipoint)+1)=0.
                trib(ipoint,i-n0(ipoint)+1)=0.
                tric(ipoint,i-n0(ipoint)+1)=0.
                trir(ipoint,i-n0(ipoint)+1)=0.
                triu(ipoint,i-n0(ipoint)+1)=0.
            ENDIF
          ENDDO
        enddo


        !do i=n0,n3
        DO i=1,n3
          DO ipoint =1,npts
            IF (i .GE. n0(ipoint)) THEN
                trib(ipoint,i-n0(ipoint)+1)=a(ipoint,i,i)
            ENDIF
          ENDDO
        ENDDO

        !do i=n0+1,n3
        DO i=1,n3
          DO ipoint =1,npts
            IF (i .GE. n0(ipoint)+1) THEN
                tria(ipoint,i-n0(ipoint)+1)=a(ipoint,i,i-1)
            ENDIF
          ENDDO
        ENDDO

        !do i=n0,n3-1      
        DO i=1,n3-1
          DO ipoint =1,npts
            IF (i .GE. n0(ipoint)) THEN
                tric(ipoint,i-n0(ipoint)+1)=a(ipoint,i,i+1)
            ENDIF
          ENDDO
        ENDDO

        ! do i=n0,n3
        DO i=1,n3
          DO ipoint =1,npts
            IF (i .GE. n0(ipoint)) THEN
                trir(ipoint,i-n0(ipoint)+1)=b(ipoint,i)
            ENDIF
          ENDDO
        ENDDO
 
        DO ipoint =1,npts
          ndim(ipoint)=n3-n0(ipoint)+1
        ENDDO

        !WRITE(*,*) ' '
        !CALL tridiag(tria,trib,tric,trir,triu,ndim)
        !REMPLACEMENT DE LA ROUTINE TRIDAG
        !WRITE(*,*) ' '

        DO ipoint =1,npts
          bet(ipoint)=trib(ipoint,1)
          triu(ipoint,1)=trir(ipoint,1)/bet(ipoint)
        ENDDO

        !DO j=2,ndim
        !enleve j=2,500 et met a la place j=2,n3
        DO j=2,n3
          DO ipoint =1,npts
            IF (j .LE. ndim(ipoint)) THEN
                gam(ipoint,j)=tric(ipoint,j-1)/bet(ipoint)
!                bet(ipoint)=trib(ipoint,j)-tria(ipoint,j)*gam(ipoint,j)
                IF ((trib(ipoint,j)-tria(ipoint,j)*gam(ipoint,j)) .EQ. 0) THEN
                    bet(ipoint)=trib(ipoint,j)-tria(ipoint,j)*gam(ipoint,j) + 10
                ELSE
                    bet(ipoint)=trib(ipoint,j)-tria(ipoint,j)*gam(ipoint,j)
                ENDIF
                triu(ipoint,j)=(trir(ipoint,j)-tria(ipoint,j)*triu(ipoint,j-1))/bet(ipoint)
            ENDIF
          ENDDO
        ENDDO
        
        !DO j=ndim-1,1,-1
        DO j=n3-1,1,-1
          DO ipoint =1,npts  
            IF (j .LE. ndim(ipoint)-1) THEN
                triu(ipoint,j)=triu(ipoint,j)-gam(ipoint,j+1)*triu(ipoint,j+1)
            ENDIF
          ENDDO
        ENDDO      
        !FIN DE REMPLACEMENT DE LA ROUTINE TRIDAG


        !DO i=n0,n3
        DO i=1,n3
          DO ipoint =1,npts  
            IF (i .GE. n0(ipoint)) THEN
                uo(ipoint,i)=triu(ipoint,i-n0(ipoint)+1)
! if (due to small number of time steps per day and strong variation in
! water table (i.e. large change in vertical profile of diffusion coefficient)
! negative methane concentrations occur: set to zero
! should usually not happen - can be avoided by increasing nday (from 24 to
! 240 or 2400 or ...) or possibly using a different numerical scheme
                IF (uo(ipoint,i) .LT. 0.) THEN
                    negconc(ipoint)=negconc(ipoint)-(uo(ipoint,i)/fpart(ipoint,i))
                ENDIF
                uo(ipoint,i)=MAX(0.,triu(ipoint,i-n0(ipoint)+1))
! transform from partial pressure into concentration unit
                uo(ipoint,i)=uo(ipoint,i)/fpart(ipoint,i)
            ENDIF
          ENDDO
        ENDDO
     


!la nouvelle valeur de uo va etre utilisée comme valeur initiale au pas de temps suivant....
!************************************
!************************************


!**************************************************************

! plant-mediated transport:
!**************************
        
        DO ipoint =1,npts  
          tplant(ipoint)=0.
        ENDDO
        
        !DO i=n0,n3
        DO i=1,n3
          DO ipoint =1,npts  
            IF (i .GE. n0(ipoint)) THEN
! calculated fraction of methane that enters plants:
! tveg: vegetation type from Sread.f
! dveg: scaling factor from para.h
! fgrow: growing stage of plants as calculated above (LAI)
! root(i): vertical root distribution as calculated above
                cplant=MIN(tveg(ipoint)*dveg*fgrow(ipoint)*root(ipoint,i)*10.*rk,1.)
! amount (concentration) of methane that enters plants
                cplant=cplant*uo(ipoint,i)
                dplant(ipoint)=dplant(ipoint)+cplant
! amount (concentration) of methane that remains in soil
                uo(ipoint,i)=uo(ipoint,i)-cplant
! only fraction (1.-pox) of methane entering plants is emitted
! to the atmosphere - the rest (pox) is oxidized in the rhizosphere
                tplant(ipoint)=tplant(ipoint)+cplant*(1.-pox)
            ENDIF
          ENDDO
        ENDDO
! transform amount (concentration) into flux unit (mg/m^2*d)
        flplant(:,iday)=tplant(:)*funit

        !DO ipoint =1,npts
        ! IF (iday .EQ. 1) THEN
        !     WRITE(*,*) 'tveg',tveg,'fgrow',fgrow
        !     WRITE(*,*) 'root',root
        ! ENDIF
        !ENDDO       

        !DO ipoint =1,npts
        ! IF (iday .EQ. 1) THEN
        !     WRITE(*,*) 'matrice uo apres plante',uo
        ! ENDIF
        ! ENDDO


! bubble transport:
!******************

        DO ipoint =1,npts  
          tmax(ipoint)=0.
        ENDDO

        !DO i=n0,nw
        DO i=1,n2
          DO ipoint =1,npts  
            IF ((i .GE. n0(ipoint)) .AND. (i .LE. nw(ipoint))) THEN
! methane in water: if concentration above threshold concentration rcmax 
! (calculated in Scalc.f): bubble formation
                cdiff=uo(ipoint,i)-rcmax(ipoint)
                cbub=MAX(0.,cdiff)
                tmax(ipoint)=tmax(ipoint)+cbub
                uo(ipoint,i)=MIN(uo(ipoint,i),rcmax(ipoint))
            ENDIF
          ENDDO
        ENDDO
     

!test Estelle
!       IF (tmax .GT. 0.) then
!       WRITE(*,*) 'check', tmax, n0, nw, uo(nw), cdiff
!       ENDIF
! if water table is above soil surface: bubble flux is emitted directly 
! to atmosphere (mg/m^2*d)

        DO ipoint =1,npts  
          IF (nw(ipoint) .GE. ns) THEN
              flbub(ipoint,iday)=tmax(ipoint)*funit
              flbubdiff(ipoint,iday)=0.
              tmax(ipoint)=0.
! if water table is below soil surface:
          ELSE
              flbub(ipoint,iday)=0.
              flbubdiff(ipoint,iday)=0.
! 1. if water table is 5 or less cm below soil surface: flbubdiff is
! calculated in the same way as flbub and added to the total flux (mg/m^2*d)
              IF (nw(ipoint) .GE. ns-5) THEN
                  flbubdiff(ipoint,iday)=tmax(ipoint)*funit
                  tmax(ipoint)=0.
              ENDIF
          ENDIF
        ENDDO

!test Estelle
!       IF (flbub(iday) .gt. 0.) then
!       WRITE(*,*) 'buble', iday,flbub(iday),tmax, cdiff, cbub, nw
!       ENDIF 


! 2. if water table is more than 5 cm below soil surface: no bubble flux
! is directly emitted to the atmosphere, BUT methane amount from bubble is
! added to the first layer above the soil surface
! (the distinction more/less than 5 cm below soil has been made, because
! adding tmax to uo(nw+1) if the water table is less than 5cm below the soil 
! surface 'disturbed' the system too much and more time steps per day (larger
! nday) is needed) (this solution avoids this)
        DO ipoint =1,npts  
          uo(ipoint,nw(ipoint)+1)=uo(ipoint,nw(ipoint)+1)+tmax(ipoint)
        ENDDO

        !DO ipoint =1,npts
        ! IF (iday .EQ. 1) THEN
        !     WRITE(*,*) 'matrice uo apres bubble',uo
        ! ENDIF
        !ENDDO

! calculate diffusive flux from the concentration gradient (mg/m^2*d):
!**********************************************************************
        DO ipoint =1,npts  
          fdifftime(ipoint,iday)=((uo(ipoint,n3-2)-uo(ipoint,n3-1))/0.1)*38.4*diffair
        ENDDO

! transform from concentration into partial pressure unit
       !do i=n0,n3
        DO i=1,n3
         DO ipoint =1,npts  
           IF (i .GE. n0(ipoint)) THEN
               uo(ipoint,i)=uo(ipoint,i)*fpart(ipoint,i)
           ENDIF
         ENDDO
       enddo
       


      ! DO ipoint =1,npts
      !   IF (iday .EQ. 1) THEN
      !       WRITE(*,*) 'matrice uo FIN TIMELOOP',uo
      !   ENDIF
      ! ENDDO

!
! end of (iday=1,nday) loop, i.e. time step per day
!***************************************************

     ENDDO

!***************************************************


! water-air interface
! transform from partial pressure into concentration unit
       !do i=n0,n3
     DO i=1,n3
       DO ipoint =1,npts  
         IF (i .GE. n0(ipoint)) THEN
             uo(ipoint,i)=uo(ipoint,i)/fpart(ipoint,i)
         ENDIF
       ENDDO
     ENDDO


! calculate conc kept in soil - 
! necessary to calculate fluxdiff (diffusive flux from mass balance)
     DO i=1,nvert
       DO ipoint =1,npts  
         uout(ipoint,i)=uo(ipoint,i)
       ENDDO
     ENDDO

     !do i=n0,n3
     DO i=1,n3
       DO ipoint =1,npts  
         IF (i .GE. n0(ipoint)) THEN
             tout(ipoint)=tout(ipoint)+(uout(ipoint,i)-uold2(ipoint,i))
         ENDIF
       ENDDO
     ENDDO
 
     !DO ipoint =1,npts  
     !  tout(ipoint)=0.
     !ENDDO
     
     tout(:)=0.

     !do i=n0,n3
     DO i=1,n3
       DO ipoint =1,npts  
         IF (i .GE. n0(ipoint)) THEN
             tout(ipoint)=tout(ipoint)+(uout(ipoint,i)-uold2(ipoint,i))
         ENDIF
       ENDDO
     ENDDO


     DO i=1,nvert
       DO ipoint =1,npts  
         uold2(ipoint,i)=uout(ipoint,i)
       ENDDO
     ENDDO


!*****************************
!vire concout

! write CH4conc to varible concout after each time interval (at iday=nday)
! concout(i,itime) is needed to write CH4conc profiles in Soutput.f
!      do i=1,n3
!          concout(i,itime)=uout(i)
!      enddo
!      do i=n3+1,n
! transform from partial pressure into concentration unit
!          concout(i,itime)=catm/fpart(i)
!      enddo

! calculate daily means of bubble flux, fluxbubdiff, plant flux, and
! diffusive flux (from concentration gradient) (mg/m^2*d)

!*******************************
!la j enleve la dimension temps des sorties...
!*******************************



     DO ipoint =1,npts  
       fluxbub(ipoint)=0.
       fluxbubdiff(ipoint)=0.
       fluxplant(ipoint)=0.
       fdiffday(ipoint)=0.
     ENDDO


     DO i=1,nday
       DO ipoint =1,npts  
         fluxbub(ipoint)=fluxbub(ipoint)+flbub(ipoint,i)
         fluxbubdiff(ipoint)=fluxbubdiff(ipoint)+flbubdiff(ipoint,i)
         fluxplant(ipoint)=fluxplant(ipoint)+flplant(ipoint,i)
         fdiffday(ipoint)=fdiffday(ipoint)+fdifftime(ipoint,i)
       ENDDO
     ENDDO
         
     fluxbub(:)=fluxbub(:)/nday
     fluxbubdiff(:)=fluxbubdiff(:)/nday
     fluxplant(:)=fluxplant(:)/nday
     fdiffday(:)=fdiffday(:)/nday
         
  
!
! calculate diffusive flux ( from mass balance):
!************************************************
! use amounts in concentration unit and tranform in flux unit (mg/m^2*d)
! wpro: amount of methane produced during entire day
! goxid: amount of methane oxidized during entire day
! mutiply by rk (depending on number of time steps per day)
! dplant: amount of methane that entered plants during entire day
!  (includes both: methane emitted through plants AND oxidized in rhizosphere)
! tout: additional methane stored in soil during entire day
! wtdiff: see above
! fluxbub: bubble flux (if water table > soil surface)
! bubble flux (if water table less than 5 cm below soil surface (see above))
! negconc: see above
     fluxdiff(:)= ((rk*(wpro(:)+goxid(:)) - dplant(:)-tout(:)-wtdiff)/nday)*funit &
        & - fluxbub(:) - fluxbubdiff(:) + negconc(:)*(funit/nday)

! fluxbubdiff is added to diffusive flux (mg/m^2*d)

     
     fluxdiff(:) = fluxdiff(:)+fluxbubdiff(:)
     fdiffday(:) = fdiffday(:)+fluxbubdiff(:)


!enleve boucle if sur ihump

!      ELSE
!          fluxdiff=0.
!          fdiffday=0.
!          fluxbub=0.
!          fluxplant=0.
!          fluxtot=0.
!          !do i=1,n
!          !   concout(i,itime)=0.
!          !enddo
!
!! end of (if (ijump .eq. 1)) 'loop'
!!***********************************
!      ENDIF





! to calculate wtdiff (see above)
! define uwp1

!non necessaire: la WTD ne se modifie pas d un pas de tps a l autre 

!         uwp0=uo(ipoint,nw)
!         uwp1=uo(ipoint,nw+1)
!         uwp2=uo(ipoint,nw+2)



! calculate total flux fluxtot (using diffusive flux from mass balance)
! and sum of diffusive flux from mass balance and bubble flux (for plots)
! (mg/m^2*d)
!      do i=1,ntime

     fluxtot(:)=fluxdiff(:)+fluxbub(:)+fluxplant(:)
     fbubdiff(:)=fluxdiff(:)+fluxbub(:)


     DO ipoint =1,npts
       ch4_flux_density_dif(ipoint) = fluxdiff(ipoint)
       ch4_flux_density_bub(ipoint) = fluxbub(ipoint)
       ch4_flux_density_pla(ipoint) = fluxplant(ipoint)  
     ENDDO


     WHERE (fluxtot(:) .LE. 0.0)
         ch4_flux_density_tot(:) = 0.0 
     ELSEWHERE
         ch4_flux_density_tot(:) = fluxtot(:)
     endwhere
     
     !pour contrebalancer valeur par defaut mise pour les grid-cells ou veget_max=0
     WHERE ((veget_max(:,1) .LE. 0.95) .AND. (sum_veget_nat_ss_nu(:) .GT. 0.1)) 
         ch4_flux_density_tot(:) = ch4_flux_density_tot(:)
     ELSEWHERE
         ch4_flux_density_tot(:) = 0.0 
     ENDWHERE



END SUBROUTINE ch4_wet_flux_density_wet4
!********************************************************************
!!$
!!$SUBROUTINE tridag(a,b,c,r,u,n)
!!$!
!!$! Press et al., Numerical Recipes, FORTRAN, p.43
!!$!************************************************
!!$
!!$    ! Domain size
!!$    INTEGER(i_std), INTENT(in)                                 :: n
!!$    REAL(r_std),DIMENSION(n), INTENT(in)                        :: a
!!$    REAL(r_std),DIMENSION(n), INTENT(in)                        :: b
!!$    REAL(r_std),DIMENSION(n), INTENT(in)                       :: c
!!$    REAL(r_std),DIMENSION(n), INTENT(in)                       :: r
!!$    REAL(r_std),DIMENSION(n), INTENT(out)                       :: u
!!$    INTEGER(stnd),PARAMETER                           :: nmax=500
!!$    INTEGER(i_std)                                 :: j
!!$    REAL(r_std),DIMENSION(nmax)                :: gam
!!$    REAL(r_std)                                    :: bet
!!$
!!$      bet=b(1)
!!$
!!$      u(1)=r(1)/bet
!!$
!!$      do j=2,n
!!$        gam(j)=c(j-1)/bet
!!$        bet=b(j)-a(j)*gam(j)
!!$        u(j)=(r(j)-a(j)*u(j-1))/bet
!!$      enddo
!!$
!!$      do j=n-1,1,-1
!!$        u(j)=u(j)-gam(j+1)*u(j+1)
!!$      enddo
!!$
!!$END SUBROUTINE tridag


END MODULE stomate_wet_ch4_pt_ter_wet4
