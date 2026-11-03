










MODULE Fertilisation 

  USE fonctions_PaSim
  USE constantes_PaSim
  USE pft_parameters
  USE constantes
  USE ioipsl

  IMPLICIT NONE
  REAL(r_std ), PARAMETER :: c2Nsolidmanure   = 15.0
  REAL(r_std ), PARAMETER :: c2Nslurry        = 10.0
  ! Nitrogen flux from slurry and manure to strcutural SOM pool (kg N m-2 d-1)
  REAL(r_std ), ALLOCATABLE, SAVE, DIMENSION(:,:)   :: fnOrganicFertstruct
  ! flag for verify that without twice fertilisation at same time
  LOGICAL     , ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: tfert_verif 
  LOGICAL     , ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: tfert_verif2  ! idem
  LOGICAL     , ALLOCATABLE, SAVE, DIMENSION(:,:,:) :: tfert_verif3  ! idem


CONTAINS

  ! two function of fertilization
  ! one is for each time
  ! the other one is for strategy spatialization of fertilization in management


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!  FERTILISATION a chaque pas de temps
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE fertilisation_pas_temps(&
     npts                           , &
     fertcount                      , &
     dt                             , &
     tjulian                        , &
     deltat                         , &
     tfert                          , &
     Nliquidmanure                  , &
     Nslurry                        , &
     Nsolidmanure                   , &
     fcOrganicFertmetabolic         , &
     fcOrganicFertstruct            , &
     fnOrganicFerturine             , &
     fnOrganicFertmetabolic         , &
     c2nratiostruct)

    INTEGER(i_std)                          , INTENT(in)     :: npts               
     ! Number of spatial points
    INTEGER(i_std), DIMENSION(npts,nvm)     , INTENT(inout)  :: fertcount           
    ! counter for fertilizer application (-)
    REAL(r_std)                             , INTENT(in)     :: dt
    INTEGER(i_std)                          , INTENT(in)     :: tjulian              
    ! Julian day (-)
    REAL(r_std)                             , INTENT(in)     :: deltat
    REAL(r_std), DIMENSION(npts,nvm, nfert) , INTENT(in)     :: tfert               
    ! zeitpunkt der duengung h (1,..,nfert) (d)
    REAL(r_std), DIMENSION(npts,nvm, nfert) , INTENT(in)     :: Nliquidmanure        
    ! Nitrogen in liquid manure (kg N m-2) (lisier)
    REAL(r_std), DIMENSION(npts,nvm, nfert) , INTENT(in)     :: Nslurry              
    ! Nitrogen in slurry (kg N m-2) (boues)
    REAL(r_std), DIMENSION(npts,nvm, nfert) , INTENT(in)     :: Nsolidmanure         
    ! Nitrogen in solid manure (kg N m-2)
    REAL(r_std), DIMENSION(npts,nvm)        , INTENT(out)    :: fcOrganicFertmetabolic 
    ! Carbon flux from slurry and manure to metabolic SOM pool (kg c m-2 d-1)
    REAL(r_std), DIMENSION(npts,nvm)        , INTENT(out)    :: fcOrganicFertstruct  
    ! Carbon flux from slurry and manure to structural SOM pool (kg c m-2 d-1)
    REAL(r_std), DIMENSION(npts,nvm)        , INTENT(out)    :: fnOrganicFerturine   
    ! Nitrogen flux from slurry and liquid manure (kg N m-2 d-1)
    REAL(r_std), DIMENSION(npts,nvm)        , INTENT(out)    :: fnOrganicFertmetabolic
    ! Nitrogen flux from Organic ferilization to metabolic SOM pool (kg N m-2 d-1)
    REAL(r_std), DIMENSION(npts,nvm)        , INTENT(in)     :: c2nratiostruct

    ! variables locales
    INTEGER :: i,j
    REAL(r_std), DIMENSION(npts,nvm) :: fnfert_resultat1   
    REAL(r_std), DIMENSION(npts,nvm) :: fnfert_resultat2  
    REAL(r_std), DIMENSION(npts,nvm) :: fnfert_resultat3  

    fnfert_resultat1 = 0.0
    fnfert_resultat2 = 0.0
    fnfert_resultat3 = 0.0

    !fertilization
    !application rates
          fcOrganicFertmetabolic(:,:)  = 0.0
          fcOrganicFertstruct(:,:)     = 0.0
          fnOrganicFerturine(:,:)      = 0.0
          fnOrganicFertmetabolic(:,:)  = 0.0
          fnOrganicFertstruct(:,:)     = 0.0
    ! engrais de ferme ! valeu estimated based on fertilisation 
    ! in chambers ms 1999
    DO j = 2, nvm
      DO i= 1, npts
        IF (fertcount(i,j) .GT. 0) THEN
          CALL fnfert(1, fnfert_resultat1(i,j), tjulian, deltat, &
             tfert(i,j,fertcount(i,j)), tapplvg, Nliquidmanure(i,j,fertcount(i,j)))
          CALL fnfert(1, fnfert_resultat2(i,j), tjulian, deltat, &
             tfert(i,j,fertcount(i,j)), tapplka, Nslurry(i,j,fertcount(i,j)))
          CALL fnfert(1, fnfert_resultat3(i,j), tjulian, deltat, &
             tfert(i,j,fertcount(i,j)), tapplmist, Nsolidmanure(i,j,fertcount(i,j)))

          fcOrganicFertmetabolic(i,j)  =  &
             fvgcmetabolic * 15 * fnfert_resultat1(i,j) + &
             fkacmetabolic * 15 * fnfert_resultat2(i,j) + &
             fmistcmetabolic * c2Nsolidmanure * fnfert_resultat3(i,j)
          fcOrganicFertstruct(i,j)  =  &
             (1.0 - fvgcmetabolic) * 15 * fnfert_resultat1(i,j) + &
             (1.0 - fkacmetabolic) * 15 * fnfert_resultat2(i,j) + &
             (1.0 - fmistcmetabolic) * c2Nsolidmanure *fnfert_resultat3(i,j)

          fnOrganicFerturine(i,j)  =  fvgnurine*fnfert_resultat1(i,j) + fkanurine*fnfert_resultat2(i,j)

          fnOrganicFertmetabolic(i,j)  = &
             (1.0 - fvgnurine)*fnfert_resultat1(i,j) + &
             (1.0 - fkanurine)*fnfert_resultat2(i,j) + &
             fnfert_resultat3(i,j) - fcOrganicFertstruct(i,j)/c2nratiostruct(i,j)  !ms 1999

          fnOrganicFertstruct(i,j)  = fcOrganicFertstruct(i,j)/c2nratiostruct(i,j)

         ELSE
          fcOrganicFertmetabolic(i,j)  = 0.0
          fcOrganicFertstruct(i,j)     = 0.0
          fnOrganicFerturine(i,j)      = 0.0
          fnOrganicFertmetabolic(i,j)  = 0.0
          fnOrganicFertstruct(i,j)     = 0.0
         ENDIF
      END DO ! i npts
    END DO ! j nvm

  END SUBROUTINE fertilisation_pas_temps


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! FNFERT 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Fnfert(&
     npts           , &
     Fnfertform     , &
     tjulianform    , &
     deltatform     , &
     tfertform      , &
     tapplform      , &
     Nfertform)

    INTEGER (i_std)                   , INTENT(in)  :: npts
    INTEGER(i_std)                 , INTENT(in)  :: tjulianform
    REAL(r_std)                 , INTENT(in)  :: deltatform
    REAL(r_std)                 , INTENT(in)  :: tapplform
    REAL(r_std), DIMENSION(npts), INTENT(in)  :: tfertform
    REAL(r_std), DIMENSION(npts), INTENT(in)  :: Nfertform
    REAL(r_std), DIMENSION(npts), INTENT(out) :: Fnfertform

    ! variables locales :
    INTEGER(i_std) :: i


    IF (deltatform .EQ. 0.0) THEN
        Fnfertform(:) = 0.0
    ELSE
      WHERE ((tjulianform + deltatform) .LE. tfertform(:)) 

        Fnfertform(:) = 0.0

      ELSEWHERE ((tjulianform .LE. tfertform(:)) .AND. ((tjulianform + deltatform) .GT. tfertform(:))) 

        Fnfertform(:) = &
          Nfertform(:)*(MIN((tjulianform + deltatform - tfertform(:)), tapplform)) / &
          deltatform/tapplform

      ELSEWHERE (tjulianform .LT. (tfertform(:) + tapplform)) 

        Fnfertform(:) = &
          Nfertform(:)*(MIN((tfertform(:) + tapplform - tjulianform),deltatform)) / &
          deltatform/tapplform

      ELSEWHERE

        Fnfertform(:) = 0.0
      END WHERE
    ENDIF

  END SUBROUTINE Fnfert

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! SPATIALISATION DE LA FERTILISATION GEREE PAR MANAGEMENT
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Fertilisation_spa(&
     npts                  , &
     flag_fertilisation    , &
     fertcount_start       , &
     tjulian               , &
     tfert                 , &
     nfertnittotprevyear   , &
     nfertammtotprevyear   , &
     nfertnit              , &
     nfertamm              , &
     fertcount             , &
     nfertammtot           , &
     nfertnittot           , &
     nfertammtotyear       , &
     nfertnittotyear       , &
     controle_azote_sum    , &
     controle_azote_sum_mem)

    INTEGER (i_std)                         , INTENT(in)    :: npts
    INTEGER(i_std)   , DIMENSION(npts,nvm)      , INTENT(in)    :: flag_fertilisation
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(inout)    :: nfertnittotprevyear
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(inout)    :: nfertammtotprevyear
    REAL(r_std), DIMENSION(npts,nvm,nfert), INTENT(in)    :: nfertnit
    REAL(r_std), DIMENSION(npts,nvm,nfert), INTENT(in)    :: nfertamm
    INTEGER(i_std), DIMENSION(npts,nvm)      , INTENT(inout) :: fertcount
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(inout) :: nfertammtot
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(inout) :: nfertnittot
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(out)   :: nfertammtotyear
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(out)   :: nfertnittotyear
    INTEGER     , DIMENSION(npts,nvm)      , INTENT(in)    :: fertcount_start
    INTEGER(i_std)                       , INTENT(in)    :: tjulian       ! Julian day (-)
    REAL(r_std), DIMENSION(npts,nvm,nfert), INTENT(in)    :: tfert
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(inout) :: controle_azote_sum
    REAL(r_std), DIMENSION(npts,nvm)      , INTENT(out)   :: controle_azote_sum_mem

    INTEGER(i_std) :: i,j


    IF (blabla_pasim) PRINT *, 'PASIM main grassland : call fertilisation_spa'

    DO j = 2, nvm

      DO i=1,npts

        IF (flag_fertilisation(i,j) .EQ. 1) THEN 
          !counter for fertilizer application
          fertcount(i,j)  = fertcount(i,j)  + 1

          !mineral fertilization
          nfertammtot(i,j)  = nfertammtot(i,j)  + nfertamm(i,j,fertcount(i,j))
          nfertnittot(i,j)  = nfertnittot(i,j)  + nfertnit(i,j,fertcount(i,j))
          
          nfertammtotyear(i,j)  = nfertammtot(i,j)  - nfertammtotprevyear(i,j) 
          nfertnittotyear(i,j)  = nfertnittot(i,j)  - nfertnittotprevyear(i,j) 

        END IF
      END DO
    END DO
    !*****RUN NONLIMITANT
    IF (f_nonlimitant .EQ. 1.) THEN
      DO j = 2, nvm
        DO i=1,npts
          IF (flag_fertilisation(i,j) .EQ. 1) THEN

            controle_azote_sum_mem(i,j) = controle_azote_sum(i,j)

            IF ((tjulian .GE. tfert(i,j,fertcount_start(i,j)))  .AND. &
               (tjulian .LE. tfert(i,j,fertcount_start(i,j))+0.9) .AND. &
               (.NOT. (tfert_verif3(i,j,fertcount_start(i,j))) )) THEN

              tfert_verif3(i,j,fertcount_start(i,j))= .TRUE.
              controle_azote_sum(i,j) = 0.

            ENDIF
          END IF
        END DO
      END DO
    ENDIF

  END SUBROUTINE Fertilisation_spa

END MODULE Fertilisation
