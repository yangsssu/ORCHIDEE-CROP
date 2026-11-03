










! Excution of cutting
MODULE Fauche

  ! modules used:

  USE fonctions_PaSim
  USE constantes_PaSim
  USE stomate_data
  USE constantes
  USE pft_parameters
  USE ioipsl
  ! USE Balances 

  IMPLICIT NONE
  REAL(r_std), SAVE                 :: mem_tjulian
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE :: wshtotsumprev

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!! CUTTING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! Excute the same process for grids where flag_cutting=1
  SUBROUTINE cutting_spa(&
     npts              , &
     tjulian           , &
     flag_cutting      , &
     wshtotcutinit     , &
     lcutinit          , &
     wsh               , &
     wshtot            , &
     wr                , &
     c                 , &
     n                 , &
     napo              , &
     nsym              , &
     fn                , &
     t                 , &
     nel               , &
     biomass           , &
     devstage          , &
     regcount          , &
     wshcutinit        , &
     gmean             , &
     wc_frac           , &
     wnapo             , &
     wnsym             , &
     wgn               , &
     tasum             , &
     tgrowth           , &
     loss              , &
     lossc             , &
     lossn             , &
     tlossstart        , &
     lai               , &
     tcut              , &
     tcut_modif        , &
     wshtotsum         , &
     controle_azote_sum)         

    ! 
    ! 0 declarations
    !

    ! 0.1 input

    INTEGER(i_std)                              , INTENT(in)    :: npts
    INTEGER(i_std)                              , INTENT(in)  :: tjulian
    INTEGER(i_std), DIMENSION(npts,nvm)         , INTENT(in)    :: flag_cutting
    REAL(r_std), DIMENSION(npts,nvm,ncut)       , INTENT(in)    :: wshtotcutinit
    REAL(r_std), DIMENSION(npts,nvm,ncut)       , INTENT(in)    :: lcutinit
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: wsh  
    ! total dry mass of shoots
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: wshtot   
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: wr     
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: c  
    ! substrat C concentration(kg C/kg)
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: n   
    ! substrat N concentration (kg N/kg)
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: napo 
    ! n concentration of apoplast (kg N m-2)
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: nsym 
    ! n concentration of symplast (kg N m-2)
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: fn  
    ! structral N concentration kgN/kg
    INTEGER(i_std)                              , INTENT(in)    :: t   
    ! time (d)
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(in)    :: nel  
    ! net lactation energy (mj/kg)
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements)     , INTENT(inout)   :: biomass  
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(inout) :: devstage
    ! stade of developpment of plant (-)
    INTEGER(i_std), DIMENSION(npts,nvm)         , INTENT(inout) :: regcount
    ! number of cut realized(-)
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: wshcutinit
    REAL(r_std), DIMENSION(npts,nvm,ngmean)     , INTENT(out)   :: gmean
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: wc_frac
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: wnapo
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: wnsym
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: wgn
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: tasum
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: tgrowth
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: loss
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: lossc
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: lossn
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(out)   :: tlossstart
    REAL(r_std), DIMENSION(npts,nvm)            , INTENT(inout)   :: lai 
    ! leaf area index of an individual plant
    REAL(r_std), DIMENSION(npts,nvm,ncut), INTENT(out)   :: tcut     
    ! date of cutting
    REAL(r_std), DIMENSION(npts,nvm,ncut), INTENT(out)   :: tcut_modif
    Real(r_std), DIMENSION(npts,nvm)     , intent(inout) :: wshtotsum  
    ! yield = total (substrate + structural) shoot dry matter (kg DM m-2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: controle_azote_sum

    ! local
    REAL(r_std), PARAMETER       :: yieldloss    = 0.05
    REAL(r_std), DIMENSION(npts,nvm) :: proportion_wsh, proportion_llam
    INTEGER(i_std)                     :: h, i, j
    REAL(r_std), DIMENSION(npts,nvm) :: wlam          ! leaf mass (kg m-2)
    REAL(r_std), DIMENSION(npts,nvm) :: wst           ! stem mass (kg m-2)
    REAL(r_std), DIMENSION(npts,nvm) :: wear          ! ear mass (kg m-2)
    REAL(r_std), DIMENSION(npts,nvm) :: wshtotreg  ! yield of regrowth
    REAL(r_std), DIMENSION(npts,nvm) :: w_postecut

    DO j=2,nvm
      WHERE(flag_cutting(:,j) .EQ. 1) 
        !transform biomass to dry matter kgDM/m2
        wlam(:,j) = ( biomass(:,j,ileaf,icarbon)/(1000*CtoDM) ) / &
                 & (1.0 + (mc /12.0)*c(:,j) + (mn /14.0)*n(:,j) )
        wst(:,j)  = ( biomass(:,j,isapabove,icarbon)/(1000*CtoDM) ) / &
                 & (1.0 + (mc /12.0)*c(:,j) + (mn /14.0)*n(:,j) )
        wear(:,j) = ( biomass(:,j,ifruit,icarbon)/(1000*CtoDM) ) / &
                 & (1.0 + (mc /12.0)*c(:,j) + (mn /14.0)*n(:,j) )
      END WHERE
!070904 AIG definitions
!
! # wshcutinit: residual structural shoot dry mass
! the less, it equals simulated structural shoot dry mass in input file (when it is less
! than the value in input file)
! in such a case, wshcutinit=wsh=wlam+wstem and proportion_wsh=1 
! the more, it equals residual structural shoot dry mass
! # wshtotcutinit: residual total shoot dry mass (in input file)
!   in the CALL of the subroutine cutting_spa in module 'plantes', wshtotcutinit_Pl 
!   replaces wshtotcutinit which is an argument of the subroutine cutting_spa
!   wshtotcutinit_Pl >=0.12
! # proportion_wsh: proportion of the residual in comparison with the structural shoot 
! dry mass simulated
! # proportion_llam : proportion of the residual in comparison with the structural leaf
! aera simulated

      DO i=1,npts
        IF (flag_cutting(i,j) .EQ. 1) THEN
          wshcutinit(i,j) = MIN (wsh(i,j), wshtotcutinit(i,j,regcount(i,j) + 1) / &
                           & (1.0 + (mc/12.0)*c(i,j) + (mn/14.0)*n(i,j)))
        END IF
      END DO
      WHERE (flag_cutting(:,j) .EQ. 1) 
        WHERE (ABS(wlam(:,j) + wst(:,j)) .GT. 10e-15) 

          proportion_wsh(:,j)  = wshcutinit(:,j)/(wlam(:,j) + wst(:,j))

        ELSEWHERE
        !JCmodif 070904 AIG 
        ! I think there is one error here
        ! If ABS(wlam + wst)< minimum_vital, there is not enough shoot biomass for the cut
        ! to occur, so that proportion_wsh =1 and any dry mass remain the same value(see after)
            !proportion_wsh(:)  = 0.0
          proportion_wsh(:,j)  = 1.0
        !ENDJCmodif 070904 AIG end
        END WHERE
      END WHERE

    ! 070725 AIG confirm
    !-----------------------------
    ! calculation of new biomasses, LAI and substrate concentrations for each component
    ! of different ages after cutting
    !-----------------------------

      WHERE (flag_cutting(:,j) .EQ. 1) 

        wlam(:,j)   = proportion_wsh(:,j) * wlam(:,j)
        wst(:,j)    = proportion_wsh(:,j) * wst(:,j)
        wear(:,j)   = 0.0
        wc_frac(:,j)     = c(:,j)    * (wr(:,j) + wshcutinit(:,j))
        wnapo(:,j)  = napo(:,j) * (wr(:,j) + wshcutinit(:,j))
        wnsym(:,j)  = nsym(:,j) * (wr(:,j) + wshcutinit(:,j))
        wgn(:,j)    = fn(:,j)   * (wr(:,j) + wshcutinit(:,j))
        w_postecut(:,j)=wlam(:,j)+wst(:,j)
        wlam(:,j)=0.1*w_postecut(:,j)
        wst(:,j)=0.9*w_postecut(:,j)

        WHERE((devstage(:,j) .LT. devsecond) .AND. (regcount(:,j) .EQ. 1))
            devstage(:,j) = 0.0
        ELSEWHERE 
            devstage(:,j) = 2.0
        END WHERE

        ! 070725 AIG confirm
        !-----------------------------
        ! reinitialization of air sum temperature and counter for regrowth after last cutting
        !-----------------------------

        ! reinitialize accumulated temperature (>5 degree)
        tasum(:,j)  = 0.0
        
        ! time of growthing = 0
        tgrowth(:,j)  = 0.0
      END WHERE

       ! 070725 AIG confirm
        !-----------------------------
        ! calculations of 
        ! - biomass and substrate losses
        ! - total yield(without in or_pa) and LAI
        ! after last cutting   
        !-----------------------------

      DO i=1,npts

        IF (flag_cutting(i,j) .EQ. 1) THEN
          !ernteverluste, ms 1999
          !constant  yieldloss  = 0.05
          loss(i,j)        = MAX (0.0, wshtot(i,j) - wshtotcutinit(i,j,regcount(i,j)+1)) * yieldloss 
          ! lossc(i)       = (c(i) + fcsh) * (wsh(i) - wshcutinit(i)) * yieldloss 
          lossc(i,j)       = loss(i,j)*CtoDM!*8
          lossn(i,j)       = loss(i,j)* (n(i,j) + fn(i,j))
          ! lossn(i)       = (n(i) + fn(i))   * (wsh(i) - wshcutinit(i)) * yieldloss 
          tlossstart(i,j)  = t 

          ! yield of regrowth
          wshtotreg(i,j) = MAX(0.0 ,wshtot(i,j) - wshtotcutinit(i,j,regcount(i,j)+1)) * (1 - yieldloss)
        END IF
      END DO

      ! RUN WITH GRASS AUTOGESTION (SATURANT OR NONLIMITANT)
      ! it is important to set it for auto cut
      IF ((f_autogestion .EQ. 1) .OR. (f_postauto .NE. 0) .OR. &
        & (f_autogestion .EQ. 3) .OR. (f_autogestion .EQ. 4)) THEN
        DO i=1,npts
          IF  (flag_cutting(i,j) .EQ. 1) THEN   
            tcut(i,j,regcount(i,j))       = tjulian
            tcut_modif(i,j,regcount(i,j)) = tjulian
          END IF
        END DO 
      END IF

      DO i=1,npts
        IF (flag_cutting(i,j) .EQ. 1) THEN
          ! annulation gmean
          gmean(i,j,:) = 0.0

          ! increase count number of cut
          regcount(i,j)  = regcount(i,j)  + 1 
        END IF
      END DO

        ! 070725 AIG confirm
        !-----------------------------
        ! calculations of yield, total LAI, total carbon and nitrogen and nel 
        ! after regrowth
        !-----------------------------

      WHERE(flag_cutting(:,j) .EQ. 1) 
        wshtotsum(:,j)     = wshtotsum(:,j) + wshtotreg(:,j)
        biomass(:,j,ileaf,icarbon)     = (wlam(:,j) * 1000*CtoDM) * (1.0 + (mc /12.0)*c(:,j) + &
                                        & (mn /14.0)*n(:,j) )
        biomass(:,j,isapabove,icarbon) = (wst(:,j)  * 1000*CtoDM) * (1.0 + (mc /12.0)*c(:,j) + &
                                        & (mn /14.0)*n(:,j) )
        biomass(:,j,ifruit,icarbon)    = (wear(:,j) * 1000*CtoDM) * (1.0 + (mc /12.0)*c(:,j) + &
                                        & (mn /14.0)*n(:,j) )

      END WHERE
      IF (f_autogestion .LT. 2) THEN
        controle_azote_sum(:,j)=controle_azote_sum(:,j)+wshtotsum(:,j)-wshtotsumprev(:,j)
      ENDIF

        wshtotsumprev(:,j)=wshtotsum(:,j)
    END DO ! nvm

  END SUBROUTINE cutting_spa

END MODULE Fauche
