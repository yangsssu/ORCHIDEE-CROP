MODULE applic_plant

! liste of functions calculated 
! - devstage
! - tgrowth
! - dndfi  
  USE fonctions_PaSim
  USE constantes_PaSim
  USE stomate_data
  USE constantes
  USE ioipsl
  USE ioipsl_para
!  USE parallel

  IMPLICIT NONE

  LOGICAL, SAVE :: first_call = .TRUE. 

  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tamean1
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tamean2
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tamean3
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tamean4
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tamean5
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tamean6
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tameand
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tameanw
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tacumm
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tacummprev
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tsoilcumm
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tsoilcummprev
  REAL(r_std), DIMENSION(:), ALLOCATABLE, SAVE :: tsoilmeand
  REAL(r_std), DIMENSION(:,:), ALLOCATABLE, SAVE :: tcut0

CONTAINS

  SUBROUTINE Main_appl_pre_animal(&
     npts                  , &
     dt                    , &
     tjulian               , &
     ta                    , &
     tsoil                 , &
     new_day               , &
     new_year              , &
     regcount              , &
     tcut                  , &
     devstage              , &
     tgrowth               )
            
    INTEGER (i_std)                      , INTENT(in)  :: npts     
    LOGICAL                              , INTENT(in)  :: new_day 
    LOGICAL                              , INTENT(in)  :: new_year     
    REAL(r_std)                          , INTENT(in)  :: dt       
    INTEGER(i_std)                       , INTENT(in)  :: tjulian 
    REAL(r_std), DIMENSION(npts)         , INTENT(in)  :: ta   
    ! air temperature (K)
    REAL(r_std), DIMENSION(npts)          , INTENT(in)  :: tsoil
    ! soil surface temperature 
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in)  :: tcut
    INTEGER(i_std)   , DIMENSION(npts,nvm) , INTENT(in)  :: regcount
    REAL(r_std), DIMENSION(npts,nvm)       , INTENT(out) :: devstage
    ! state of developpement of growth
    REAL(r_std), DIMENSION(npts,nvm)       , INTENT(out) :: tgrowth 
    ! time from last cut (d)

    INTEGER(i_std) :: ier
    REAL(r_std), DIMENSION(npts)      :: xtmp_npts


    IF (first_call) THEN 
      first_call = .FALSE. 

      ALLOCATE (tamean1        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tamean1', 'Not enough memory', '')
      ALLOCATE (tamean2        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tamean2', 'Not enough memory', '')
      ALLOCATE (tamean3        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tamean3', 'Not enough memory', '')
      ALLOCATE (tamean4        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tamean4', 'Not enough memory', '')
      ALLOCATE (tamean5        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tamean5', 'Not enough memory', '')
      ALLOCATE (tamean6        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tamean6', 'Not enough memory', '')
      ALLOCATE (tameand        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tameand', 'Not enough memory', '')
      ALLOCATE (tameanw        (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tameanw', 'Not enough memory', '')
      ALLOCATE (tacumm         (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tacumm', 'Not enough memory', '')
      ALLOCATE (tacummprev     (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tacummprev', 'Not enough memory', '')
      ALLOCATE (tsoilcumm      (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tsoilcumm', 'Not enough memory', '')
      ALLOCATE (tsoilcummprev  (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tsoilcummprev', 'Not enough memory', '')
      ALLOCATE (tsoilmeand     (npts), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tsoilmeand', 'Not enough memory', '')
      ALLOCATE (tcut0          (npts,nvm), stat=ier)
      IF (ier /= 0) CALL ipslerr_p(3, 'Main_appl_pre_animal', 'tcut0', 'Not enough memory', '')

      tamean1(:)         = 273.0
      tamean2(:)         = 273.0
      tamean3(:)         = 273.0
      tamean4(:)         = 273.0
      tamean5(:)         = 273.0
      tamean6(:)         = 273.0
      tameand(:)         = 273.0
      tameanw(:)         = 0.0
      tacumm(:)          = 0.0
      tacummprev(:)      = 0.0
      tsoilcumm(:)       = 0.0
      tsoilcummprev(:)   = 0.0
      tsoilmeand(:)      = 273.0
      tcut0(:,:)         = 0.0

    END IF

    IF (new_year) THEN
        tcut0(:,:) = 0.0
    END IF 

    CALL cal_devstage(npts, dt, ta, tsoil, new_day, new_year, regcount, devstage)
    CALL cal_tgrowth(npts, dt, devstage, tjulian, new_day, new_year, regcount, tcut, tgrowth)


  END SUBROUTINE Main_appl_pre_animal



  SUBROUTINE cal_devstage(npts,dt,ta,tsoil,new_day, new_year, regcount, devstage)

    INTEGER (i_std)                   , INTENT(in)  :: npts 
    REAL(r_std)                 , INTENT(in)  :: dt        
    REAL(r_std), DIMENSION(npts), INTENT(in)  :: ta       
    REAL(r_std), DIMENSION(npts), INTENT(in)  :: tsoil   
    LOGICAL                    , INTENT(in)  :: new_day 
    LOGICAL                    , INTENT(in)  :: new_year 
    INTEGER(i_std)   , DIMENSION(npts,nvm), INTENT(in)  :: regcount 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out) :: devstage 

    INTEGER(i_std) :: i,j

    CALL Euler_funct(npts,dt,ta,tacumm)
    CALL Euler_funct(npts,dt,tsoil,tsoilcumm)

    CALL histwrite(hist_id_stomate ,'TSOILCUMM',itime,tsoilcumm, npts, hori_index)


    IF (new_day) THEN

      tamean1(:)  = tamean2(:) 
      tamean2(:)  = tamean3(:) 
      tamean3(:)  = tamean4(:) 
      tamean4(:)  = tamean5(:) 
      tamean5(:)  = tamean6(:) 
      tamean6(:)  = tameand(:) 

      tameand(:) = tacumm(:) - tacummprev(:) 
      tacummprev(:) = tacumm(:) 
      
      tameanw(:)  = (&
         tamean1(:) + &
         tamean2(:) + &
         tamean3(:) + &
         tamean4(:) + &
         tamean5(:) + &
         tamean6(:) + &
         tameand(:))/7.0
      
      tsoilmeand(:) = tsoilcumm(:) - tsoilcummprev(:) 
      tsoilcummprev(:) = tsoilcumm(:)
       
      DO j=2,nvm
        DO i=1,npts
          
          IF ((devstage(i,j) .LE. 0.0) .AND. ( (tameanw(i) .GT. trep) .OR. &
            & (regcount(i,j) .EQ. 2) ) ) THEN
              
              devstage(i,j) = MAX(0.0, tameand(i) - tbase)/tasumrep 
              
          ELSEIF ((devstage(i,j) .GT. 0.0) .AND. (tsoilmeand(i) .GT. tbase) .AND. &
            & (devstage(i,j) .LT. 2.0) ) THEN
              
              devstage(i,j) = devstage(i,j) + MAX(0.0, tameand(i) - tbase)/tasumrep 
          
          ELSE
              devstage(i,j) = devstage(i,j) 
              
          ENDIF
        END DO ! npts 
      END DO ! nvm 
    END IF

    IF (new_year) THEN 

      devstage(:,:) = 0.0

    END IF

  END SUBROUTINE cal_devstage



  SUBROUTINE cal_tgrowth(npts, dt, devstage, tjulian, new_day, new_year, regcount, tcut, tgrowth)
    
    INTEGER(i_std)                        , INTENT(in)  :: npts     
    REAL(r_std)                           , INTENT(in)  :: dt        
    INTEGER(i_std)                        , INTENT(in)  :: tjulian    ! julien day (d)
    LOGICAL                               , INTENT(in)  :: new_day   
    LOGICAL                               , INTENT(in)  :: new_year   
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(in)  :: devstage   ! state of developpement 
    INTEGER(i_std)   , DIMENSION(npts,nvm), INTENT(in)  :: regcount   ! number of cut
    REAL(r_std), DIMENSION(npts,nvm,nstocking), INTENT(in)  :: tcut   ! cut date
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(out) :: tgrowth    ! regrowth time after last cut (d)
    
    INTEGER(i_std) :: i,j

    IF (new_day) THEN

      ! TGROWTH
      !(robson, m. j. et al., 1988)
      DO j=2,nvm  
        WHERE ((devstage(:,j) .GT. 0.0) .AND. (tcut0(:,j) .LE. 0.0)) 
            
          tcut0(:,j)  = FLOAT(tjulian) 
            
        END WHERE
      END DO   
        
      DO j=2,nvm
        DO i=1,npts
          IF ((regcount(i,j) .EQ. 1) .AND. (tcut0(i,j) .LE. 0.0)) THEN
              
            tgrowth(i,j)  = 0.0
              
          ELSEIF (regcount(i,j) .EQ. 1) THEN
              
            tgrowth(i,j) = tjulian  - tcut0(i,j) 
              
          ELSE
              
            tgrowth(i,j) = tjulian - tcut(i,j,regcount(i,j)-1) 
              
          ENDIF
        END DO ! npts
      END DO ! nvm 
    END IF

    IF (new_year) THEN 

      tgrowth(:,:) = 0.0

    END IF


  END SUBROUTINE cal_tgrowth


END MODULE applic_plant
