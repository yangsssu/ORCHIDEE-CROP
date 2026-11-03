










MODULE fonctions_PaSim

  USE constantes_PaSim
  USE constantes

  ! Used functions
  ! Euler_funct
  ! linreg_pasim

  IMPLICIT NONE

  PUBLIC :: Euler_funct

  INTEGER(i_std), SAVE :: emplacement 
  ! permet de se souvenir du dernier emplacement dans l'interpolation

CONTAINS

  !!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! EULER
  !!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Euler_funct(npts, dt, dY, Y)
    ! Y(n+1) = Y(n) + h*F(tn,Y(n)) 
    ! d'ou : 
    ! Y = Yp + h*dY
    ! ou h = dt

    INTEGER(i_std), INTENT(in)                   :: npts    
    REAL(R_std), INTENT(in)                      :: dt      
    REAL(R_std), DIMENSION(npts), INTENT(in)     :: dY     
    ! result
    REAL(R_std), DIMENSION(npts), INTENT(inout)  :: Y

    Y(:) = Y(:) + dt*dY(:)

  END SUBROUTINE Euler_funct


  SUBROUTINE Euler_X(npts, ncol, dt, dY, Y)
    ! matrix
    INTEGER(i_std), INTENT(in)   :: npts 
    INTEGER(i_std), INTENT(in)   :: ncol ! number colonne of matrix
    REAL(R_std), INTENT(in)      :: dt   
    REAL(R_std), DIMENSION(npts, ncol) :: dY   
    REAL(R_std), DIMENSION(npts, ncol) :: Y    

    INTEGER(i_std) :: i

    DO i=1, ncol
      Y(:,i) = Y(:,i) + dt*dY(:,i)
    END DO

  END SUBROUTINE Euler_X

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!      FTSIGM
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE fTsigm(npts, Tform, &
     T0form, T0pform, qftform, &
     fTsigmform)

    ! functiong of temperature equation 3.11
    INTEGER(i_std), INTENT(in)                :: npts
    REAL(R_std), DIMENSION(npts), INTENT(in)  :: Tform
    REAL(R_std), INTENT(in)                   :: T0form
    REAL(R_std), INTENT(in)                   :: T0pform
    REAL(R_std), INTENT(in)                   :: qftform
    REAL(r_std), DIMENSION(npts), INTENT(out) :: fTsigmform
    
    INTEGER(i_std) :: i

    DO i=1, npts
      IF ((Tform(i) .GT. T0form) .AND. (Tform(i) .LT. T0pform)) THEN
          fTsigmform(i) = ((Tform(i) - T0form)**qftform)*(T0pform - Tform(i))/ &
             (((293.0 - T0form)**qftform)*(T0pform - 293.0)) 
      ELSE
          fTsigmform(i) = 0.0
      ENDIF
    ENDDO

  END SUBROUTINE fTsigm


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!!!!!!!!!!!! INTERPOLATION EXTRAPOLATION
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE interpolation_extrapolation (npts, nbpoints, table, absc, nouv_annee, resultat)

! Principe suivi : cette fonction n'est appelée que pour faire l'interpolation 
!sur les fichiers météo (ta, ea, pa, iatmtot, nh3a, co2, u). 
! Elle est appelée à chaque pas de temps. Il faut faire deux remarques : 
! 1)- pour tous les points de simulations l'interpolation se fait dans 
!le même intervalle de temps pour un fichier. 
! 2)- Tous les fichiers météo étant construit sur le même format, 
!ils font tous leur interpolation dans le même intervalle pour un pas de temps donné. 
! Nous allons donc conserver en mémoire le dernier emplacement 
!(dans quel intervalle) d'interpolation, en sachant que la suivante sera soit au même 
!endroit soit dans l'intervalle suivant. 

    INTEGER(i_std), INTENT(in) :: npts
    INTEGER(i_std), INTENT(in) :: nbpoints  
    REAL(r_std), DIMENSION(npts,2,nbpoints), INTENT(in) :: table
    REAL(r_std), INTENT(in)  :: absc
    LOGICAL, intent(in) :: nouv_annee
    REAL(r_std), DIMENSION(npts), INTENT(out) :: resultat

    INTEGER(i_std) :: i
    INTEGER(i_std) :: j
    LOGICAL, DIMENSION(npts) :: calcul

    calcul(:) = .FALSE. 

    ! remarquons que tous les tableaux appelant cette fonction sont au même niveau pour l'interpolation
    ! remarquons que pour i=1,npts tous les points sont interpolés au même endroit

    IF (nouv_annee) THEN   ! if it's the firts call during a new_year
        
      ! table has npts line, but all points are the make on the same model,
      ! so we search on point 1 and it's the same on all other points
      IF ((absc .GE. table(1,1,1)) .AND. (absc .LE. table(1,1,nbpoints))) THEN  

        ! if table (1,2,i+1) = -999 it's the end of the array table
        DO i=1,nbpoints-1        
          IF ((absc .GE. table(1,1,i)) .AND. (absc .LE. table(1,1,i+1)) .AND.&
            (table(1,2,i+1).NE. -999.0)) THEN
                
            calcul(:) = .TRUE. 
            resultat(:) = table(:,2,i)*(table(:,1,i+1)-absc)/(table(:,1,i+1)-table(:,1,i)) + &
                   table(:,2,i+1)*(absc - table(:,1,i))/(table(:,1,i+1)-table(:,1,i))
            emplacement = i
                
          ELSE IF ((table(1,2,i+1).EQ. -999.0).AND. (calcul(1) .EQ. .FALSE.)) THEN
            calcul(:) = .TRUE.
            resultat(:) = table(:,2,i-1)*(table(:,1,i)-absc)/ &
                   (table(:,1,i)-table(:,1,i-1)) + &
                   table(:,2,i)*(absc - table(:,1,i-1))/&
                   (table(:,1,i)-table(:,1,i-1))
            emplacement = i

          END IF
        END DO
      ELSE 
        IF (absc .LT. table(1,1,1)) THEN

          calcul(:) = .TRUE.
          resultat(:) = table(:,2,1)*(table(:,1,2)-absc)/(table(:,1,2)-table(:,1,1)) + &
                 table(:,2,2)*(absc - table(:,1,1))/(table(:,1,2)-table(:,1,1))
          emplacement = 0
              
        ELSE 

          calcul(:) = .TRUE.
          resultat(:) = table(:,2,nbpoints-1)*(table(:,1,nbpoints)-absc)/ &
                 (table(:,1,nbpoints)-table(:,1,nbpoints-1)) + &
                 table(:,2,nbpoints)*(absc - table(:,1,nbpoints-1))/&
                 (table(:,1,nbpoints)-table(:,1,nbpoints-1))
          emplacement = nbpoints
              
        END IF
      END IF
    ELSE              ! si ce n'est pas le premier appel de l'année
      ! dans ce cas nous connaissons le dernier emplacement utilisé. Nous repartons de là
      IF ((emplacement .NE. 0) .AND. (emplacement .NE. nbpoints)) THEN
        IF ((absc .GE. table(1,1,emplacement)) .AND. (absc .LE. table(1,1,emplacement+1)) .AND.&
             (table(1,2,emplacement+1).NE. -999.0)) THEN
              ! emplacement ne change pas

          calcul(:) = .TRUE. 
          resultat(:) = &
                table(:,2,emplacement)*(table(:,1,emplacement+1)-absc)/&
                (table(:,1,emplacement+1)-table(:,1,emplacement)) + &
                 table(:,2,emplacement+1)*(absc - table(:,1,emplacement))/&
                 (table(:,1,emplacement+1)-table(:,1,emplacement))
              
          
        ELSE IF ((table(1,2,emplacement+1).EQ. -999.0).AND. (calcul(1) .EQ. .FALSE.)) THEN
              !emplacement ne change pas
          calcul(:) = .TRUE.
          resultat(:) = table(:,2,emplacement-1)*(table(:,1,emplacement)-absc)/ &
                 (table(:,1,emplacement)-table(:,1,emplacement-1)) + &
                 table(:,2,emplacement)*(absc - table(:,1,emplacement-1))/&
                 (table(:,1,emplacement)-table(:,1,emplacement-1))

           resultat(:) = table(:,2,emplacement)
              
        ELSE IF (emplacement .NE. nbpoints-1) THEN
          ! dans ce cas là l'interpolation est dans la case suivante
          emplacement = emplacement + 1
          IF ((absc .GE. table(1,1,emplacement)) .AND. (absc .LE. table(1,1,emplacement+1)) .AND.&
               (table(1,2,emplacement+1).NE. -999.0)) THEN
            ! emplacement ne change pas
            calcul(:) = .TRUE. 
            resultat(:) = &
                     table(:,2,emplacement)*(table(:,1,emplacement+1)-absc)/&
                     (table(:,1,emplacement+1)-table(:,1,emplacement)) + &
                     table(:,2,emplacement+1)*(absc - table(:,1,emplacement))/&
                     (table(:,1,emplacement+1)-table(:,1,emplacement))
              
                  
          ELSE IF ((table(1,2,emplacement+1).EQ. -999.0).AND. (calcul(1) .EQ. .FALSE.)) THEN
            !emplacement ne change pas
            calcul(:) = .TRUE.
            resultat(:) = table(:,2,emplacement-1)*(table(:,1,emplacement)-absc)/ &
                     (table(:,1,emplacement)-table(:,1,emplacement-1)) + &
                     table(:,2,emplacement)*(absc - table(:,1,emplacement-1))/&
                     (table(:,1,emplacement)-table(:,1,emplacement-1))

            resultat(:) = table(:,2,emplacement)
                  
          END IF
              
        ELSE IF (emplacement .EQ. nbpoints -1) then
          emplacement = emplacement + 1
          calcul(:) = .TRUE.
          resultat(:) = table(:,2,nbpoints-1)*(table(:,1,nbpoints)-absc)/ &
               (table(:,1,nbpoints)-table(:,1,nbpoints-1)) + &
               table(:,2,nbpoints)*(absc - table(:,1,nbpoints-1))/&
               (table(:,1,nbpoints)-table(:,1,nbpoints-1))
          emplacement = nbpoints
            

        END IF
      ELSE IF (emplacement .EQ. 0) THEN
        IF (absc .LT. table(1,1,1) ) THEN
          ! emplacement ne change pas
          calcul(:) = .TRUE.
          resultat(:) = table(:,2,1)*(table(:,1,2)-absc)/(table(:,1,2)-table(:,1,1)) + &
               table(:,2,2)*(absc - table(:,1,1))/(table(:,1,2)-table(:,1,1))
          emplacement = 0
        ELSE
          emplacement = 1
          calcul(:) = .TRUE. 
          resultat(:) = &
               table(:,2,emplacement)*(table(:,1,emplacement+1)-absc)/&
               (table(:,1,emplacement+1)-table(:,1,emplacement)) + &
               table(:,2,emplacement+1)*(absc - table(:,1,emplacement))/&
               (table(:,1,emplacement+1)-table(:,1,emplacement))
        END IF

      ELSE IF (emplacement .EQ. nbpoints)THEN

        calcul(:) = .TRUE.
        resultat(:) = table(:,2,nbpoints-1)*(table(:,1,nbpoints)-absc)/ &
           (table(:,1,nbpoints)-table(:,1,nbpoints-1)) + &
           table(:,2,nbpoints)*(absc - table(:,1,nbpoints-1))/&
           (table(:,1,nbpoints)-table(:,1,nbpoints-1))
        emplacement = nbpoints
      ELSE
        STOP 'erreur avec l''interpolation'

      END IF

    ENDIF

  END SUBROUTINE interpolation_extrapolation

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! PM
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE Pm(npts, Pmform,alphaform,Tlform, Co2curr, pm20, pmn, pmc)

    INTEGER(i_std), INTENT(in) :: npts
    REAL(R_std), DIMENSION(npts), INTENT(in) :: pm20
    REAL(R_std), DIMENSION(npts), INTENT(in) :: pmc
    REAL(R_std), DIMENSION(npts), INTENT(in) :: pmn
    REAL(R_std), DIMENSION(npts), INTENT(out) :: Pmform
    REAL(R_std), DIMENSION(npts), INTENT(out) :: alphaform
    REAL(R_std), DIMENSION(npts), INTENT(in)  :: Tlform
    REAL(R_std), DIMENSION(npts), INTENT(in)  :: Co2curr
    

    REAL(R_std)  :: PmT0p
    REAL(R_std), DIMENSION(npts) :: PmT
    REAL(R_std), DIMENSION(npts) :: Vcmax
    REAL(R_std), DIMENSION(npts) :: Vomax
    REAL(R_std), DIMENSION(npts) :: Kc
    REAL(R_std), DIMENSION(npts) :: Ko
    REAL(R_std), DIMENSION(npts) :: Oi
    REAL(R_std), DIMENSION(npts) :: Ci
    REAL(R_std), DIMENSION(npts) :: Ci350
    REAL(R_std), DIMENSION(npts) :: GAMMAstar
    REAL(R_std), DIMENSION(npts) :: Amax
    REAL(R_std), DIMENSION(npts) :: Amax350
    REAL(R_std), DIMENSION(npts) :: PmCO2
    REAL(R_std), DIMENSION(npts) :: Eps
    REAL(R_std), DIMENSION(npts) :: Eps350
    REAL(R_std), DIMENSION(npts) :: alphaCO2
    REAL(R_std), DIMENSION(npts) :: Tlcel
    
    REAL(R_std), PARAMETER :: pmqft        = 1.5
    REAL(R_std), PARAMETER :: pmtopt       = 303.0
    REAL(R_std), PARAMETER :: pmt0         = 273.0




    !Abhaengigkeit von Klimaaenderung (CO2 & T)


    Vcmax = 98.0*EXP(68000.0*(Tlform-298.15)/ (298.15*Tlform*Rgas))*SQRT(Tlform/298.15)
    Vomax = 0.21*Vcmax
    Kc    = 460.0*EXP(65800*(Tlform-298.15)/(298.15*Tlform*Rgas))*SQRT(Tlform/298.15)
    Ko    = 330.0*EXP(1400*(Tlform-298.15)/(298.15*Tlform*Rgas))*SQRT(Tlform/298.15)
    Tlcel = Tlform - 273.15
    Oi    = 210*(0.047 - 0.0013087*Tlcel + 0.000025603*Tlcel**2 - 0.00000021441*Tlcel**3)/0.026934
    Ci    = 0.7*CO2curr*((1.6740 - 0.061294*Tlcel + 0.0011688*Tlcel**2 - 0.0000088741*Tlcel**3)/0.73547)
    Ci350 = 0.7*350.0*((1.6740 - 0.061294*Tlcel + 0.0011688*Tlcel**2 - 0.0000088741*Tlcel**3)/0.73547)
    GAMMAstar = 0.5*Vomax*Kc*Oi/(Vcmax*Ko)
    Amax    = (Ci    - GAMMAstar)*Vcmax/(Ci    + Kc*(1.0 + Oi/Ko))
    Amax350 = (Ci350 - GAMMAstar)*Vcmax/(Ci350 + Kc*(1.0 + Oi/Ko))
    PmCO2 = Vcmaxadap*Amax/Amax350
    Eps    = (ABSORvl/2.1)*(Ci    - GAMMAstar)/ (4.5*Ci    + 10.5*GAMMAstar)
    Eps350 = (ABSORvl/2.1)*(Ci350 - GAMMAstar)/ (4.5*Ci350 + 10.5*GAMMAstar)
    alphaCO2 = Eps/Eps350

    alphaform = alpha350*alphaCO2

    !Abhaengigkeit von der Temperatur

    PmT0p = ((1.0 + Pmqft)*PmTopt - PmT0)/Pmqft
    CALL fTsigm(npts, Tlform, PmT0, PmT0p, Pmqft, PmT)

    Pmform = Pm20*PmN*PmC*PmCO2*PmT

  END SUBROUTINE Pm


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !  Fonctions calculant es et s : la pression de vapeur à saturation
  ! et sa dérivée
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  SUBROUTINE es_fonct(npts,temp, resultat)

    INTEGER(i_std), INTENT(in) :: npts
    REAL(r_std), DIMENSION(npts), INTENT(in)  :: temp
    REAL(r_std), DIMENSION(npts), INTENT(out) :: resultat


    resultat = 0.6112*EXP(17.67*(temp/(temp+246.2)))

  END SUBROUTINE es_fonct


  SUBROUTINE s_fonct(npts,temp, resultat)
    
    INTEGER(i_std), INTENT(in) :: npts
    REAL(r_std), DIMENSION(npts), INTENT(in)  :: temp
    REAL(r_std), DIMENSION(npts), INTENT(out) :: resultat
    
    REAL(r_std), DIMENSION(npts) :: es_cal

    CALL es_fonct(npts, temp, es_cal)
    resultat = es_cal*17.67*246.2/(temp+246.2)**2
  END SUBROUTINE s_fonct

  ! Subroutine linreg_pasim for autogestion

  SUBROUTINE linreg_pasim( npts, MaxElems, xx, yy, nDataPairs, misVal, &
     muX, muY, sigX, sigY, a, b, cor )

    INTEGER(i_std), INTENT(in)                                :: npts
    INTEGER(i_std), INTENT(in)                                :: MaxElems
    REAL(r_std), DIMENSION(npts,MaxElems), INTENT(in) :: xx      
    ! IN :x-data
    REAL(r_std), DIMENSION(npts,MaxElems), INTENT(in) :: yy       
    ! IN :y-data
    INTEGER(i_std), INTENT(in)                        :: nDataPairs 
    ! IN : number o valid x-y pairs
    REAL(r_std), INTENT(in)                           :: misVal  
    ! IN :missing value
    REAL(r_std), DIMENSION(npts), INTENT(out)         :: muX, muY 
    ! OUT: meanof x and y
    REAL(r_std), DIMENSION(npts), INTENT(out)         :: sigX, sigY
    ! OUT:standard deviation of x and y
    REAL(r_std), DIMENSION(npts), INTENT(out)         :: a, b    
    ! OUT:parameters of least-square                                                               
    !      fit y=a*x+b,
    REAL(r_std), DIMENSION(npts), INTENT(out)         :: cor     
    ! OUT:correlation between x and y

    INTEGER(i_std)     , DIMENSION(npts)  ::  nn
    INTEGER(i_std) ::  ii, i
    REAL(r_std), DIMENSION(npts)  ::  sxy


    ! initialize variables
    nn   = 0
    muX  = 0.0
    muY  = 0.0
    sigX = 0.0
    sigY = 0.0
    sxy  = 0.0

    DO ii=1,nDataPairs

      WHERE ((xx(:,ii).NE.misVal).AND.(yy(:,ii).NE.misVal))
        nn(:)   = nn(:) + 1
        muX(:)  = muX(:) + xx(:,ii)
        muY(:)  = muY(:) + yy(:,ii)
        sigX(:) = sigX(:) + xx(:,ii)*xx(:,ii)
        sigY(:) = sigY(:) + yy(:,ii)*yy(:,ii)
      END WHERE

    END DO
    DO i=1,npts
      IF (nn(i) .GT. 1) THEN
        muX(i)  = muX(i)/nn(i)
        muY(i)  = muY(i)/nn(i)
        sigX(i) = sigX(i)-nn(i)*muX(i)*muX(i)
        sigY(i) = sigY(i)-nn(i)*muY(i)*muY(i)

        IF (sigX(i) .LT. 0.0)  sigX(i) = 0.0
        IF (sigY(i) .LT. 0.0)  sigY(i) = 0.0

        DO ii=1,nDataPairs
          IF ((xx(i,ii).NE.misVal) .AND. (yy(i,ii) .NE. misVal)) THEN
              sxy(i) = sxy(i)+(xx(i,ii)-muX(i))*(yy(i,ii)-muY(i))
          END IF
        END DO

        IF ((sigX(i).GT.0.0).AND.(sigY(i).GT.0.0)) THEN
            a(i)   = sxy(i)/sigX(i)
            b(i)   = muY(i) - a(i)*muX(i)
            cor(i) = sxy(i)/SQRT(sigX(i)*sigY(i))
        ELSE
            a(i)   = misVal
            b(i)   = misVal
            cor(i) = misVal
        END IF

        sigX(i) = SQRT(sigX(i)/REAL(nn(i)-1))
        sigY(i) = SQRT(sigY(i)/REAL(nn(i)-1))

      ELSE

        muX(i)  = misVal
        muY(i)  = misVal
        sigX(i) = misVal
        sigY(i) = misVal
        a(i)    = misVal
        b(i)    = misVal
        cor(i)  = misVal

      END IF
    END DO


  END SUBROUTINE linreg_pasim  ! LinReg

END MODULE fonctions_PaSim







