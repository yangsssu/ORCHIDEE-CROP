










! =================================================================================================================================
! MODULE       : weather
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module simulates weather.
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	:
!!
!! SVN          :
!! $HeadURL: $
!! $Date: $
!! $Revision: $
!! \n
!_ ================================================================================================================================

MODULE weather

  USE netcdf
!-
  USE defprec
  USE ioipsl_para
  USE constantes
  USE mod_orchidee_para

  USE solar
  USE grid, ONLY : year,month,day,sec
!-
  IMPLICIT NONE
!-
  PRIVATE
  PUBLIC weathgen_main, weathgen_domain_size, weathgen_init, weathgen_read_file, weathgen_qsat_2d
!
! Only for root proc
  INTEGER, SAVE                              :: iim_file, jjm_file, llm_file, ttm_file !(unitless)
  INTEGER,DIMENSION(:,:),SAVE,ALLOCATABLE    :: ncorr !(unitless)
  INTEGER,DIMENSION(:,:,:),SAVE,ALLOCATABLE  :: icorr,jcorr !(unitless)
  INTEGER,SAVE                               :: i_cut, n_agg !(unitless)

  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xinwet   !! climatological wet days + anomaly @tex $(days.month^{-1})$ @endtex
  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xinprec  !! climatological precipition + anomaly @tex $(mm.day^{-1})$ @endtex
  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xint     !! climatological temp + anomaly (C)
  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xinq     !! climatological relative humidity + anomaly (0-1, unitless)
  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xinwind  !! climatological wind speed + anomaly @tex $(m.s^{-1})$ @endtex
  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xincld   !! climatological cloudiness + anomaly (0-1, unitless)
  REAL,DIMENSION(:,:),SAVE,ALLOCATABLE :: xintrng  !! climatological temp range + anomaly (C)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: xintopo  !! topography (m)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: lat_land !! latitudes of land points (unitless)
!-
! daily values
!-
  REAL,SAVE :: julian_last
  INTEGER,DIMENSION(:),SAVE,ALLOCATABLE :: iwet    !! flag for wet day / dry day (0-1, unitless)
!-
! today's values (m0 means "minus 0")
!-
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: psurfm0  !! surface pressure today(Pa)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: cloudm0  !! cloud fraction today (0-1, unitless)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: tmaxm0   !! maximum daily temperature today (K)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: tminm0   !! minimum daily temperature today (K)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: qdm0     !! daily average specific humidity today (kg_h2o/kg_air)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: udm0     !! daily average wind speed today  @tex $(m.s^{-1})$ @endtex
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: precipm0 !! daily precitation today @tex $(mm.day^{-1})$ @endtex
!-
! yesterday's values (m1 means "minus 1")
!-
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: psurfm1  !! surface pressure yesterday (Pa)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: cloudm1  !! cloud fraction yesterday (0-1, unitless)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: tmaxm1   !! maximum daily temperature yesterday (K)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: tminm1   !! minimum daily temperature yesterday (K)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: qdm1     !! daily average specific humidity yesterday (kg_h2o/kg_air)
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: udm1     !! daily average wind speed  yesterday  @tex $(m.s^{-1})$ @endtex 
  REAL,DIMENSION(:),SAVE,ALLOCATABLE   :: precipm1 !! daily precitation  yesterday @tex $(mm.day^{-1})$ @endtex 
!-
! other
!-
  INTEGER,SAVE                  :: ipprec        !! statistical (0) or prescribed (1) daily values

  LOGICAL,SAVE                  :: precip_exact  !! respect monthly precipitation
  INTEGER,DIMENSION(31,12),SAVE :: jour_precip


  INTEGER,PARAMETER      :: seedsize_max = 300  !! max size of random seed
  LOGICAL,SAVE           :: dump_weather
  CHARACTER(LEN=20),SAVE :: dump_weather_file
  LOGICAL,SAVE           :: gathered
  INTEGER,SAVE           :: dump_id

!
  REAL,PARAMETER         :: zero_t=273.15   !! Absolute zero
! 
  REAL,PARAMETER :: pir = pi/180.
  REAL,PARAMETER :: rair = 287.
!-
  INTEGER,PARAMETER :: nmon = 12  !! Number of months

!
  CHARACTER(LEN=3),DIMENSION(12) :: cal = &             !! Months of the year (unitless)
 &  (/ 'JAN','FEB','MAR','APR','MAY','JUN', &
 &     'JUL','AUG','SEP','OCT','NOV','DEC' /)
  INTEGER,DIMENSION(12),SAVE :: ndaypm = &              !! Number of days per month (noleap year) (unitless)
 &  (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  INTEGER,SAVE :: soldownid, rainfid, snowfid, lwradid, &
 &                tairid, qairid,  psolid, uid, vid, &
 &                time_id, timestp_id
!-
  INTEGER,SAVE :: n_rtp = nf90_real4      !! Parameters for NETCDF 
!- 
  INTEGER  :: ALLOC_ERR                   !! Flag for dynamic allocations 
!-
  CHARACTER(LEN=20),SAVE :: calendar_str   !! Calendar type
!-
  INTEGER, DIMENSION(:), ALLOCATABLE, SAVE :: kindex_w  !! Land points index
  INTEGER, SAVE                            :: nbindex_w  !! Land points index
!-
  INTEGER, PARAMETER :: termcol = 100     !! Plot of projection file => grid, number of column on terminal
!80
  CONTAINS
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : daily
!!
!>\BRIEF          This routine generates daily weather conditions from monthly-mean
!! climatic parameters.
!!
!! DESCRIPTION  :  Specifically, this routine generates daily values of 
!!                 - daily total precipitation \n
!!                 - daily maximum temperature \n
!!                 - daily minimum temperature \n
!!                 - daily average cloud cover \n
!!                 - daily average relative humidity \n
!!                 - daily average wind speed \n
!!                 In order to generate daily weather conditions, the model uses
!!                 a series of 'weather generator' approaches,
!!                 which generate random combinations of weather conditions
!!                 based upon the climatological conditions in general. \n
!!                 This weather generator is based upon the so-called Richardson
!!                 weather generator. \n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::psurf, ::cloud, ::tmax, ::tmin, ::qd, ::ud, ::precip 
!!
!! REFERENCE(S) :
!! -  Geng, S., F.W.T. Penning de Vries, and L. Supit, 1985:  A simple
!! method for generating rainfall data, Agricultural and Forest
!! Meteorology, 36, 363-376.
!! -  Richardson, C. W. and Wright, D. A., 1984: WGEN: A model for
!! generating daily weather variables: U. S. Department of
!! Agriculture, Agricultural Research Service. 
!! - Richardson, C., 1981: Stochastic simulation of daily
!! precipitation, temperature, and solar radiation. Water Resources
!! Research 17, 182-190. 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE daily &
 &  (npoi, imonth, iday, cloud, tmax, tmin, precip, qd, ud, psurf)

!-
! in & out: global variables
!-

! INTEGER,INTENT(INOUT):: iwet(npoi) !! wet day / dry day flag!*
!-

  !! 0. Parameters and variables declaration

  REAL,PARAMETER :: rair = 287.     !! Molecular weight of one mole of dry air @tex $(g.mol{-1})$ @endtex
  REAL,PARAMETER :: grav = 9.81     !! Acceleration of the gravity @tex $(m.s^{-2})$ @endtex
  REAL,PARAMETER :: pi = 3.1415927  !! pi

  !! 0.1 Input variables

  INTEGER,INTENT(IN) :: npoi         !! total number of land points
  INTEGER,INTENT(IN) :: imonth, iday

  !! 0.2 Output variables

  REAL,INTENT(OUT) :: psurf(npoi)   !! surface pressure (Pa)
  REAL,INTENT(OUT) :: cloud(npoi)   !! cloud fraction (0-1, unitless)
  REAL,INTENT(OUT) :: tmax(npoi)    !! maximum daily temperature (K)
  REAL,INTENT(OUT) :: tmin(npoi)    !! minimum daily temperature (K)
  REAL,INTENT(OUT) :: qd(npoi)      !! daily average specific humidity (kg_h2o/kg_air)
  REAL,INTENT(OUT) :: ud(npoi)      !! daily average wind speed  @tex $(m.s^{-1})$ @endtex
  REAL,INTENT(OUT) :: precip(npoi)  !! daily precitation @tex $(mm.day^{-1})$ @endtex

  !! 0.4 Local variables

  REAL :: td(npoi)                                 !! daily average temperature (K)
  REAL,allocatable,save,dimension(:,:) ::  xstore  !! weather generator 'memory' matrix
  REAL :: ee(3), r(3), rr(npoi,3)
  REAL :: alpha(npoi), rndnum, pwd, pww
  REAL :: beta(npoi)
  REAL :: pwet(npoi)
  REAL :: rwork
  REAL :: omcloud, omqd, omtmax
  REAL :: cloudm, cloudw, cloudd
  REAL :: cloude(npoi), clouds(npoi)
  REAL :: tmaxd, tmaxw, tmaxm
  REAL :: tminm
  REAL :: tmins(npoi), tmaxs(npoi)
  REAL :: tmine(npoi), tmaxe(npoi)
  REAL :: qdm(npoi),qdd(npoi),qde(npoi),qdw(npoi),qdup(npoi),qdlow(npoi)
  INTEGER :: i,j,k
  REAL :: amn,b1,b2,b3,eud,rn,rn1,rn2,rn3,rn4,s1,s2,s12,x1,y, z(npoi)
  REAL :: aa(npoi),ab(npoi),tr1(npoi), tr2(npoi)
  REAL :: tdm,trngm,tdum
  REAL :: qsattd(npoi)
  INTEGER :: it1w, it2w
  REAL :: dt
  REAL :: rainpwd(npoi)
  INTEGER :: not_ok(npoi)
  INTEGER :: count_not_ok,count_not_ok_g
  LOGICAL,SAVE :: firstcall_weather_daily = .TRUE.
  INTEGER,save :: npoi0
  REAL :: xx
  REAL, DIMENSION(3,3) :: a,b              !! define autocorrelation matrices for Richardson generator
                                           !!
                                           !! note that this matrix should be based upon a statistical
                                           !! analysis of regional weather patterns
                                           !!
                                           !! for global simulations, we use 'nominal' values

  LOGICAL :: Warning_aa_ab(npoi), Warning_iwet(npoi) !! Warnings
!---------------------------------------------------------------------
!-
! initial setup for daily climate calculations
!-
  a(1,:) = (/ 0.600,  0.500,  0.005 /)
  a(2,:) = (/ 0.010,  0.250,  0.005 /)
  a(3,:) = (/ 0.020,  0.125,  0.250 /)
!-
  b(1,:) = (/ 0.500,  0.250, -0.250 /)
  b(2,:) = (/ 0.000,  0.500,  0.250 /)
  b(3,:) = (/ 0.000,  0.000,  0.500 /)
!-
! GK240100
  IF (firstcall_weather_daily) THEN
    firstcall_weather_daily = .FALSE.
    ALLOC_ERR=-1
    ALLOCATE(xstore(npoi,3), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) THEN
      WRITE(numout,*) "ERROR IN ALLOCATION of xstore : ",ALLOC_ERR
      STOP 
    ENDIF
    xstore(:,:) = zero
    npoi0 = npoi
  ELSE IF (npoi /= npoi0) THEN
    WRITE(numout,*) 'Domain size old, new: ',npoi0,npoi
    STOP 'WG Daily: Problem: Domain has changed since last call'
  ENDIF
!-
! define working variables
!-
  rwork = (cte_grav/rair/0.0065)
!-
! 'omega' parameters used to calculate differences in expected
! climatic parameters on wet and dry days
!
! following logic of weather generator used in the EPIC crop model
!
! omcloud -- cloud cover
! omqd    -- humidity
! omtmax  -- maximum temperature
!-
  omcloud = 0.90    ! originally 0.90
  omqd    = 0.50    ! originally 0.50
  omtmax  = 0.75    ! originally 0.75
!-
! calculate weighting factors used in interpolating climatological
! monthly-mean input values to daily-mean values
!-
! this is a simple linear interpolation technique that takes into
! account the length of each month
!-
  IF (ipprec == 0) THEN
    IF (REAL(iday) < REAL(ndaypm(imonth)+1)/2.0) then
      it1w = imonth-1
      it2w = imonth
      dt   = (REAL(iday)-0.5)/ndaypm(imonth)+0.5
    ELSE
      it1w = imonth
      it2w = imonth+1
      dt   = (REAL(iday)-0.5)/ndaypm(imonth)-0.5
    ENDIF
    if (it1w <  1)  it1w = 12
    if (it2w > 12)  it2w = 1
  ELSE
     dt = -1.
     it1w = -1
     it2w = -1
  ENDIF
!-
  IF (ipprec == 0) THEN
!---
!-- use weather generator to create daily statistics
!---
! (1) determine if today will rain or not
!---
!-- calculate monthly-average probability of rainy day
!---
    DO i=1,npoi
      pwet(i) = xinwet(i,imonth)/ndaypm(imonth)
    ENDDO
!---
    IF (.NOT.precip_exact) THEN
!-----
!---- (1.1) following Geng et al.
!-----
      IF (is_root_prc) THEN
         CALL random_number (rndnum)
      ENDIF
      CALL bcast(rndnum)
!-----
      DO i=1,npoi
        IF (xinprec(i,imonth) > 1.e-6) THEN
!---------
!-------- implement simple first-order Markov-chain precipitation
!-------- generator logic based on Geng et al. (1986),
!-------- Richardson and Wright (1984), and Richardson (1981)
!---------
!-------- basically, this allows for the probability of today being
!-------- a wet day (a day with measureable precipitation)
!-------- to be a function of what yesterday was (wet or dry)
!---------
!-------- the logic here is that it is more likely that a wet day
!-------- will follow another wet day -- allowing
!-------- for 'storm events' to persist
!---------
!-------- estimate the probability of a wet day after a dry day
!---------
          pwd = 0.75*pwet(i)
!---------
!-------- estimate the probability of a wet day after a wet day
!---------
          pww = 0.25+pwd
!---------
!-------- decide if today is a wet day or a dry day
!-------- using a random number
!---------
!-------- call random_number(rndnum) ! done before
!---------
          IF (iwet(i) == 0) then
            IF (rndnum <= pwd) iwet(i) = 1
          ELSE
            IF (rndnum > pww) iwet(i) = 0
          ENDIF
        ELSE
          iwet(i) = 0
        ENDIF
      ENDDO
    ELSE
!-----
!---- (1.2) preserving the monthly number of precip days
!----       and monthly precip
!-----
      DO i=1,npoi
        IF (ABS(xinwet(i,imonth)) < 32.) THEN
          IF (xinprec(i,imonth) > 1.e-6) THEN
            IF (   jour_precip(iday,imonth) &
 &              <= NINT(MAX(1.,xinwet(i,imonth))) ) THEN
              iwet(i) = 1
            ELSE
              iwet(i) = 0
            ENDIF
          ELSE
           iwet(i) = 0
          ENDIF
        ENDIF
      ENDDO
    ENDIF
!---
!-- (2) determine today's precipitation amount
!---
    IF (.not.precip_exact) THEN
       Warning_aa_ab(:)=.FALSE.
       Warning_iwet(:)=.FALSE.
!-----
!---- (2.1) following Geng et al.
!-----
      aa(:) = zero
      ab(:) = zero
      tr2(:)= zero
      tr1(:)= zero
      beta(:) = un
      DO i=1,npoi
!-------
!------ initialize daily precipitation to zero
!-------
        precip(i) = zero
!-------
        IF (xinprec(i,imonth) > 1.e-6) THEN
!---------
!-------- if it is going to rain today
!---------
          IF (iwet(i) == 1) THEN
!-----------
!---------- calculate average rainfall per wet day
!-----------
            rainpwd(i) = xinprec(i,imonth) &
 &                      *ndaypm(imonth)/MAX(0.1,xinwet(i,imonth))
!-----------
!---------- randomly select a daily rainfall amount
!---------- from a probability density function of rainfall
!----------
!---------- method i --
!-----------
!---------- use the following technique from Geng et al. and Richardson
!---------- to distribute rainfall probabilities
!-----------
!---------- pick a random rainfall amount
!---------- from a two-parameter gamma function distribution function
!-----------
!---------- estimate two parameters for gamma function
!---------- (following Geng et al.)
!-----------
            beta(i) = MAX(1.0,-2.16+1.83*rainpwd(i))
            alpha(i) = rainpwd(i)/beta(i)
!-----------
!---------- determine daily precipitation amount
!---------- from gamma distribution function
!---------- (following WGEN code of Richardson and Wright (1984))
!-----------
            IF (ABS(1.-alpha(i)) < 1.e-6) THEN
              alpha(i) = 1.e-6*(alpha(i)/ABS(alpha(i)))
            ENDIF
            aa(i) = 1.0/alpha(i)
            ab(i) = 1.0/(1.0-alpha(i))
!-----------
            IF ( (ABS(aa(i)) < 1.e-6) .OR. (ABS(ab(i)) < 1.e-6) ) THEN
               Warning_aa_ab(:)=.TRUE.
            ENDIF
            tr1(i) = exp(-18.42/aa(i))
            tr2(i) = exp(-18.42/ab(i))
          ENDIF
        ELSE
          IF (iwet(i) == 1) THEN
            Warning_iwet(i)=.TRUE.
          ENDIF
        ENDIF
      ENDDO

      DO i=1,npoi
         IF ( Warning_aa_ab(i) ) THEN
            WRITE(numout,*) ' ATTENTION, aa ou ab:'
            WRITE(numout,*) ' aa, ab = ',aa(i),ab(i)
            WRITE(numout,*) '   alpha, rainpwd, beta =', &
 &                       alpha(i),rainpwd(i),beta(i)
         ENDIF
         IF ( Warning_iwet(i) ) THEN
            WRITE(numout,*) ' ATTENTION, iwet = 1 alors que xinprec = 0)'
            WRITE(numout,*) '   xinprec, iwet = ',xinprec(i,imonth),iwet(i)
          ENDIF
      ENDDO
!-----
      where ( iwet(:) == 1 )
        not_ok(:) = 1
      elsewhere
        not_ok(:) = 0
      endwhere
!-----
      count_not_ok_g=0
      count_not_ok=SUM(not_ok(:))
      CALL reduce_sum(count_not_ok,count_not_ok_g)
      CALL bcast(count_not_ok_g)
!-
      z(:) = zero
      DO WHILE (count_not_ok_g > 0)
        IF (is_root_prc) THEN
        CALL random_number (rn1)
        CALL random_number (rn2)
        ENDIF
        CALL bcast(rn1)
        CALL bcast(rn2)

        DO i=1,npoi
          IF ((iwet(i) == 1).AND.(not_ok(i) == 1)) then
            IF ( (rn1-tr1(i)) <= zero ) THEN
              s1 = zero
            ELSE
              s1 = rn1**aa(i)
            ENDIF
!-----------
            IF ((rn2-tr2(i)) <= zero) THEN
              s2 = zero
            ELSE
              s2 = rn2**ab(i)
            ENDIF
!-----------
            s12 = s1+s2
            IF ((s12-1.0) <= zero) THEN
              z(i) = s1/s12
              not_ok(i) = 0
            ENDIF
          ENDIF
        ENDDO

        count_not_ok_g=0
        count_not_ok=SUM(not_ok(:))
        CALL reduce_sum(count_not_ok,count_not_ok_g)
        CALL bcast(count_not_ok_g)
      ENDDO
!-----
      IF (is_root_prc) THEN
         CALL random_number (rn3)
      ENDIF
      CALL bcast(rn3)
!      WRITE(*,*) mpi_rank,"rn3=",rn3
!-----
      DO i=1,npoi
        IF (iwet(i) == 1) then
          precip(i) = -z(i)*log(rn3)*beta(i)
        ENDIF
      ENDDO
!-----
!---- method ii --
!-----
!---- here we use a one-parameter Weibull distribution function
!---- following the analysis of Selker and Haith (1990)
!-----
!---- Selker, J.S. and D.A. Haith, 1990: Development and testing
!---- of single- parameter precipitation distributions,
!---- Water Resources Research, 11, 2733-2740.
!-----
!---- this technique seems to have a significant advantage over other
!---- means of generating rainfall distribution functions
!-----
!---- by calibrating the Weibull function to U.S. precipitation records,
!---- Selker and Haith were able to establish the following relationship
!-----
!---- the cumulative probability of rainfall intensity x is given as:
!-----
!---- f(x) = 1.0-exp(-(1.191 x / rainpwd)**0.75)
!-----
!---- where x       : rainfall intensity
!----       rainpwd : rainfall per wet day
!-----
!---- using transformation method, take uniform deviate and convert
!---- it to a random number weighted by the following Weibull function
!-----
!----    call random_number(rndnum)
!-----
!----    precip(i) = rainpwd / 1.191*(-log(1.0-rndnum))**1.333333
!-----
!---- bound daily precipitation to "REAListic" range
!-----
      DO i=1,npoi
        IF (iwet(i) == 1) THEN
!---------
!-------- lower end is determined by definition of a 'wet day'
!-------- (at least 0.25 mm of total precipitation)
!---------
!-------- upper end is to prevent ibis from blowing up
!---------
          precip(i) = MAX(precip(i),  0.25) ! min =   0.25 mm/day
          precip(i) = MIN(precip(i),150.00) ! max = 150.00 mm/day
        ENDIF
      ENDDO
    ELSE
!-----
!---- (2.2) preserving the monthly number of precip days
!----       and monthly precip
!-----
      DO i=1,npoi
!-------
!------ Correction Nathalie. C'est abs(xinwet) < 32 qu'il faut tester
!------ et non pas abs(xinprec(i,imonth)) < 32.
!-------
!------ IF ( (iwet(i) == 1).and.(abs(xinprec(i,imonth)) < 32.) ) THEN
        IF ( (iwet(i) == 1).and.(abs(xinwet(i,imonth)) < 32.) ) THEN
          precip(i) = xinprec(i,imonth)*REAL(ndaypm(imonth)) &
 &                   /REAL(NINT(MAX(1.,xinwet(i,imonth))))
        ELSE
          precip(i) = zero
        ENDIF
      ENDDO
    ENDIF
!---
!-- (3) estimate expected minimum and maximum temperatures
!---
    DO i=1,npoi
!-----
!---- first determine the expected maximum and minimum temperatures
!---- (from climatological means) for this day of the year
!-----
!---- mean daily mean temperature (K)
      tdm = xint(i,it1w)+dt*(xint(i,it2w)-xint(i,it1w))+zero_t
!---- mean daily temperature range (K)
      trngm = xintrng(i,it1w)+dt*(xintrng(i,it2w)-xintrng(i,it1w))
!---- mean minimum and maximum temperatures
      tmaxm = tdm+0.56*trngm
      tminm = tdm-0.44*trngm
!-----
!---- modify maximum temperatures for wet and dry days
!-----
      IF (pwet(i) /= zero) THEN
        tmaxd = tmaxm+pwet(i)*omtmax*trngm
        tmaxw = tmaxd-        omtmax*trngm
      ELSE
        tmaxd = tmaxm
        tmaxw = tmaxm
      ENDIF
!-----
!---- set the 'expected' maximum and minimum temperatures for today
!-----
!---- note that the expected minimum temperatures are the same for
!---- both wet and dry days
!-----
      if (iwet(i) == 0)  tmaxe(i) = tmaxd
      if (iwet(i) == 1)  tmaxe(i) = tmaxw
!-----
      tmine(i) = tminm
!-----
!---- estimate variability in minimum and maximum temperatures
!-----
!---- tmaxs : standard deviation in maximum temperature (K)
!---- tmins : standard deviation in minimum temperature (K)
!-----
!---- Regression is based on analysis of 2-m air temperature data
!---- from the NCEP/NCAR reanalysis (1958-1997) for 294 land points
!---- over central North America
!---- (24N-52N, 130W-60W, 0.5-degree resolution):
!---- Daily max and min temperatures were calculated for each
!---- land point from daily mean temperature and temperature range.
!---- Anomalies were calculated
!---- by subtracting similar max and min temperatures calculated from
!---- monthly mean temperature and range (interpolated to daily).
!---- The 40 years of anomalies were then binned by month
!---- and the standard deviation calculated for each month.
!---- The 294 x 12 standard deviations were then regressed
!---- against the 3528 long-term monthly mean temperatures.
!-----
!---- note: the values are bound to be greater than 1.0 K
!----       (at the very least they must be bound
!----        so they don't go below zero)
!-----
      tmaxs(i) = MAX(1.0,-0.0713*(tdm-zero_t)+4.89)
      tmins(i) = MAX(1.0,-0.1280*(tdm-zero_t)+5.73)
    ENDDO
!---
!-- (4) estimate expected cloud cover
!---
!---
!-- the formulation of dry and wet cloud cover has been
!-- derived from the weather generator used in the epic crop model
!---
    DO i=1,npoi
!-----
!---- cloudm : mean cloud cover for today
!---- cloudd : dry day cloud cover
!---- cloudw : dry day cloud cover
!---- cloude : expected cloud cover today
!-----
!---- mean cloud cover (%)
!-----
      cloudm = xincld(i,it1w)+dt*(xincld(i,it2w)-xincld(i,it1w))
!-----
!---- convert from percent to fraction
!-----
      cloudm = cloudm/100.0
!-----
!---- adjust cloud cover depending on dry day / rainy day
!---- following logic of the EPIC weather generator code
!-----
      IF (pwet(i) /= zero) THEN
        cloudd = (cloudm-pwet(i)*omcloud)/(un-pwet(i)*omcloud)
        cloudd = MIN(un,MAX(zero,cloudd))
        cloudw = (cloudm-(un-pwet(i))*cloudd)/pwet(i)
      ELSE
        cloudd = cloudm
        cloudw = cloudm
      ENDIF
      IF (iwet(i) == 0)  cloude(i) = cloudd
      IF (iwet(i) == 1)  cloude(i) = cloudw
!-----
!---- estimate variability in cloud cover for wet and dry days
!---- following numbers proposed by Richardson
!-----
!---- clouds : standard deviation of cloud fraction
!-----
      IF (iwet(i) == 0)  clouds(i) = 0.24*cloude(i)
      IF (iwet(i) == 1)  clouds(i) = 0.48*cloude(i)
    ENDDO
!---
! (5) determine today's temperatures and cloud cover using
!     first-order serial autoregressive technique
!---
!-- use the Richardson (1981) weather generator approach to simulate the
!-- daily values of minimum / maximum temperature and cloud cover
!---
!-- following the implementation of the Richardson WGEN weather
!-- generator used in the EPIC crop model
!---
!-- this approach uses a multivariate generator, which assumes that the
!-- perturbation of minimum / maximum temperature and cloud cover are
!-- normally distributed and that the serial correlation of each
!-- variable may be described by a first-order autoregressive model
!---
!-- generate standard deviates for weather generator
!---
    DO j=1,3
      ee(j) = 9999.
      DO WHILE (ee(j) > 2.5)
        IF (is_root_prc) THEN
           CALL random_number (rn1)
           CALL random_number (rn2)
        ENDIF
        CALL bcast(rn1)
        CALL bcast(rn2)
        ee(j) = SQRT(-2.0*LOG(rn1))*COS(6.283185*rn2)
      ENDDO
    ENDDO
!---
!-- zero out vectors
!---
    r(1:3)  = zero
    rr(1:npoi,1:3) = zero
!---
!-- update working vectors
!---
    do j=1,3
      do k=1,3
        r(j) = r(j)+b(j,k)*ee(j)
      enddo
    enddo
!---
    do j=1,3
      do k=1,3
        do i=1,npoi
          rr(i,j) = rr(i,j)+a(j,k)*xstore(i,k)
        enddo
      enddo
    enddo
!---
!-- solve for x() perturbation vector and save current vector
!-- into the xim1() storage vector (saved for each point)
!---
    do j=1,3
      do i=1,npoi
        xstore(i,j) = r(j)+rr(i,j)
      enddo
    enddo
!---
!-- determine today's minimum and maximum temperature
!--
    do i=1,npoi
      tmax(i) = tmaxe(i)+tmaxs(i)*xstore(i,1)
      tmin(i) = tmine(i)+tmins(i)*xstore(i,2)
!-----
!---- if tmin > tmax, then switch the two around
!-----
      if (tmin(i) > tmax(i)) then
        tdum    = tmax(i)
        tmax(i) = tmin(i)
        tmin(i) = tdum
      ENDIF
!---- daily average temperature
      td(i) = 0.44*tmax(i)+0.56*tmin(i)
!---- determine today's cloud cover
      cloud(i) = cloude(i)+clouds(i)*xstore(i,3)
!---- constrain cloud cover to be between 0 and 100%
      cloud(i) = MAX(zero,MIN(un,cloud(i)))
    enddo
!---
!-- (6) estimate today's surface atmospheric pressure
!---
    do i=1,npoi
!-----
!---- simply a function of the daily average temperature
!---- and topographic height -- nothing fancy here
!-----
      psurf(i) = 101325.0*(td(i)/(td(i)+0.0065*xintopo(i)))**rwork
    enddo
!---
!-- (7) estimate today's relative humidity
!---
    IF (is_root_prc) THEN
       CALL random_number (rn)
    ENDIF
    CALL bcast(rn)
!---
    CALL weathgen_qsat (npoi,td,psurf,qsattd)
!---
    do i=1,npoi
!-----
!---- the formulation of dry and wet relative humidities has been
!---- derived from the weather generator used in the epic crop model
!-----
!---- qdm : mean relative humidity
!---- qdd : dry day relative humidity
!---- qdw : rainy day relative humidity
!---- qde : expected relative humidity (based on wet/dry decision)
!-----
!---- mean relative humidity (%)
      qdm(i) = xinq(i,it1w)+dt*(xinq(i,it2w)-xinq(i,it1w))
!---- convert from percent to fraction
      qdm(i) = qdm(i)/100.0
!-----
!---- adjust humidity depending on dry day / rainy day
!---- following logic of the EPIC weather generator code
!-----
      if (pwet(i) /= zero) then
        qdd(i) = (qdm(i)-pwet(i)*omqd)/(un-pwet(i)*omqd)
        if (qdd(i) < 0.2) then
          qdd(i) = 0.2
          if (qdd(i) > qdm(i)) qdm(i) = qdd(i)
        ENDIF
        qdd(i) = MIN(un,qdd(i))
        qdw(i) = (qdm(i)-(un-pwet(i))*qdd(i))/pwet(i)
      ELSE
        qdd(i) = qdm(i)
        qdw(i) = qdm(i)
      ENDIF
!-----
      if (iwet(i) == 0)  qde(i) = qdd(i)
      if (iwet(i) == 1)  qde(i) = qdw(i)
!-----
!---- estimate lower and upper bounds of humidity distribution function
!---- following logic of the EPIC weather generator code
!-----
      xx = exp(qde(i))
      qdup(i)  = qde(i)+(un-qde(i))*xx/euler
      qdlow(i) = qde(i)*(un-1./xx)
!-----
!---- randomlly select humidity from triangular distribution function
!---- following logic of the EPIC weather generator code
!-----
!---- call random_number(rn) ! GK done before
!-----
      y  = 2.0/(qdup(i)-qdlow(i))
!-----
      b3 = qde(i)-qdlow(i)
      b2 = qdup(i)-qde(i)
      b1 = rn/y
!-----
      x1 = y*b3/2.0
!-----
      if (rn > x1) then
        qd(i) = qdup(i)-sqrt (b2*b2-2.0*b2*(b1-0.5*b3))
      ELSE
        qd(i) = qdlow(i)+sqrt (2.0*b1*b3)
      ENDIF
!-----
!---- adjust daily humidity to conserve monthly mean values
!-----
!---- note that this adjustment sometimes gives rise to humidity
!---- values greater than 1.0 -- which is corrected below
!-----
      amn = (qdup(i)+qde(i)+qdlow(i))/3.0
      qd(i) = qd(i)*qde(i)/amn
!-----
!---- constrain daily average relative humidity
!-----
      qd(i) = MAX(0.30,MIN(qd(i),0.99))
!-----
!---- convert from relative humidity to specific humidity at
!---- daily mean temperature
!-----
      qd(i) = qd(i)*qsattd(i)
    enddo
!---
!-- (8) estimate today's daily average wind speed
!---
    IF (is_root_prc) THEN
        CALL random_number (rn4)
    ENDIF
    CALL bcast(rn4)

    DO i=1,npoi
!-----
!---- first estimate the expected daily average wind speed (from monthly
!---- means)
!-----
      eud = xinwind(i,it1w)+dt*(xinwind(i,it2w)-xinwind(i,it1w))
!-----
!---- following logic of the EPIC weather generator
!---- select random wind speed following this equation
!-----
!---- call random_number(rn4)
!-----
      ud(i) = 1.13989*eud*(-log(rn4))**0.30
!---- constrain daily wind speeds to be between 2.5 and 10.0 m/sec
      ud(i) = MAX(2.5,MIN(ud(i),10.0))
    ENDDO
  ELSE
!---
!-- use REAL daily climate data
!---
    DO i=1,npoi
!-----
!---- use basic daily climate data, converting units
!-----
!---- daily total precipitation
      precip(i) = xinprec(i,imonth)
!---- daily average temperatures
      td(i) = xint(i,imonth)+zero_t
      trngm = MIN(44.0,xintrng(i,imonth))
!-----
      tmax(i) = td(i)+0.56*trngm
      tmin(i) = td(i)-0.44*trngm
!---- daily average cloud cover
      cloud(i) = xincld(i,imonth)*0.01
!---- daily average specific humidity
      qd(i) = xinq(i,imonth)
!---- daily average wind speed
      ud(i) = xinwind(i,imonth)
!-----
!---- compute surface atmospheric pressure
!-----
      psurf(i) = 101325.0*(td(i)/(td(i)+0.0065*xintopo(i)))**rwork
    ENDDO
  ENDIF
!-------------------
END SUBROUTINE daily
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : diurnal
!!
!>\BRIEF          This routine interpolates values from the!* 
!! subroutine daily into instantaneous values for simulating a diurnal cycle.
!!
!! DESCRIPTION  :  
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):  ::fira, ::solad, ::ua, ::ta, ::qa, ::raina, ::snowa, ::rh.
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE diurnal &
& (npoi, nband, time, jday, plens, startp, endp, latitude, &
&  cloud, tmax, tmin, precip, qd, ud, psurf, &
&  fira, solad, solai, ua, ta, qa, raina, snowa, rh)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-

  !! 0. Parameter and variables declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN) :: npoi    !! number of grid points (unitless)
  INTEGER,INTENT(IN) :: nband   !! number of visible bands (unitless)
  REAL,INTENT(IN) :: time
  INTEGER, INTENT(IN) :: jday
  REAL,INTENT(IN) :: plens,startp,endp
  REAL,INTENT(IN) :: latitude(npoi)   !! latitude in degrees
  REAL,INTENT(IN) :: cloud(npoi)      !! cloud fraction (0-1, unitless)
  REAL,INTENT(IN) :: tmax(npoi)       !! maximum daily temperature (K)
  REAL,INTENT(IN) :: tmin(npoi)       !! maximum daily temperature (K)
  REAL,INTENT(IN) :: precip(npoi)     !! daily precitation @tex $(mm.day^{-1})$ @endtex 
  REAL,INTENT(IN) :: qd(npoi)         !! daily average specific humidity (kg_h2o/kg_air)
  REAL,INTENT(IN) :: ud(npoi)         !! daily average wind speed @tex $(m.s^{-1})$ @endtex 
  REAL,INTENT(IN) :: psurf(npoi)      !! surface pressure (Pa)

  !! 0.2 Output variables

  REAL,INTENT(OUT) :: fira(npoi)          !! incoming ir flux  @tex $(W.m^{-2})$ @endtex
  REAL,INTENT(OUT) :: solad(npoi,nband)   !! direct downward solar flux @tex $(W.m^{-2})$ @endtex
  REAL,INTENT(OUT) :: solai(npoi,nband)   !! diffuse downward solar flux @tex $(W.m^{-2})$ @endtex
  REAL,INTENT(OUT) :: ua(npoi)            !! wind speed @tex $(m.s^{-1})$ @endtex 
  REAL,INTENT(OUT) :: ta(npoi)            !! air temperature (K)
  REAL,INTENT(OUT) :: qa(npoi)            !! specific humidity (kg_h2o/kg_air)
  REAL,INTENT(OUT) :: raina(npoi)         !! rainfall rate @tex $(mm.day^{-1})$ @endtex 
  REAL,INTENT(OUT) :: snowa(npoi)         !! snowfall rate @tex $(mm.day^{-1})$ @endtex 
  REAL,INTENT(OUT) :: rh(npoi)            !! relative humidity (0-1, unitless)

  !! 0.4 Local variables

  REAL,SAVE      :: step
  REAL :: xl,so,xllp,xee,xse
  REAL :: xlam,dlamm,anm,ranm,ranv,anv,tls,rlam
  REAL :: sd,cd,deltar,delta,Dis_ST,ddt
!-
  REAL :: coszen(npoi)                      !! cosine of solar zenith angle
  REAL :: rtime
  REAL :: sw,frac,gamma,qmin,qmax,qsa,emb,ea,ec,dtair,dtcloud,rn
  REAL :: trans(npoi), fdiffuse(npoi), qsatta(npoi), qsattmin(npoi)
  INTEGER :: i,ib
  INTEGER,SAVE :: npoi0
  LOGICAL,SAVE :: firstcall_weather_diurnal = .TRUE.

!---------------------------------------------------------------------
! GK240100
  IF (firstcall_weather_diurnal) THEN
    firstcall_weather_diurnal = .FALSE.
    npoi0 = npoi
  ELSE IF (npoi /= npoi0) THEN
    WRITE(numout,*) 'Domain size old, new: ',npoi0,npoi
    STOP 'WG Diurnal: Problem: Domain has changed since last call'
  ENDIF

! calculate time in hours
  rtime = time/3600.0

  CALL downward_solar_flux(npoi,latitude,calendar_str,REAL(jday),rtime,cloud,nband,solad,solai)
!-
! temperature calculations
!-
!---
!-- assign hourly temperatures using tmax and tmin
!-- following Environmental Biophysics, by Campbell and Norman, p.23
!---
!-- this function fits a fourier series to the diurnal temperature cycle
!-- note that the maximum temperature occurs at 2:00 pm local solar time
!---
!-- note that the daily mean value of gamma is 0.44,
!-- so td = 0.44*tmax+0.56*tmin,  instead of
!--    td = 0.50*tmax+0.50*tmin
!---
  gamma = 0.44-0.46*SIN(    pi/12.0*rtime+0.9) &
              +0.11*SIN(2.0*pi/12.0*rtime+0.9)
  DO i=1,npoi
    ta(i) = tmax(i)*gamma+tmin(i)*(1.0-gamma)
  ENDDO
!-
! humidity calculations
!-
  CALL weathgen_qsat (npoi,tmin,psurf,qsattmin)
  CALL weathgen_qsat (npoi,ta,psurf,qsatta)
!-
  DO i=1,npoi
!---
!-- adjust specific humidity against daily minimum temperatures
!---
!-- To do this, qa is written as an approximate sine function
!-- (same as ta) to preserve the daily mean specific humidity,
!-- while also preventing rh from exceeding 99% at night
!---
!-- Note that the daily mean RH is *not* preserved, and therefore the
!-- output RH will be slightly different from what was read in.
!---
!-- first adjust so that maximum RH cannot exceed 99% at night
!---
    qmin = MIN(qd(i),0.99*qsattmin(i))
    qmax = (qd(i)-0.56*qmin)/0.44
!---
!-- if needed, adjust again to 99% at other times of the day (in which
!-- case the daily mean *specific* humidity is also not preserved)
!---
    qsa  = 0.99*qsatta(i)
!---
!-- calculate the hourly specific humidity, using the above adjustments
!---
    qa(i) = MIN(qsa,qmax*gamma+qmin*(1.0-gamma))
!---
!-- calculate the hourly relative humidity
!--
    rh(i) = 100.*qa(i)/qsatta(i)
  ENDDO
!-
! wind speed calculations
!-
  IF (is_root_prc) THEN
     CALL random_number (rn)
  ENDIF
  CALL bcast(rn)
!-
  DO i=1,npoi
!---
!-- following logic of the EPIC weather generator
!-- select random wind speed following this equation
!---
!-- call random_number(rn) ! done before
!---
    ua(i) = 1.13989*ud(i)*(-log(rn))**0.30
!---
!-- fix wind speeds to always be above 2.5 m/sec and below 10.0 m/sec
!---
    ua(i) = MAX(2.5,MIN(10.0,ua(i)))
  ENDDO
!-
! ir flux calculations
!-
  DO i=1,npoi
!---
!-- clear-sky emissivity as a function of water vapor pressure
!-- and atmospheric temperature
!---
!-- calculate the ir emissivity of the clear sky
!-- using equation from idso (1981) water resources res., 17, 295-304
!---
    emb = 0.01*(psurf(i)*qa(i)/(0.622+qa(i)))
    ea  = 0.70+5.95e-5*emb*EXP(1500.0/ta(i))
!---
!-- assign the ir emissivity of clouds
!-- (assume they are ~black in the ir)
!---
    ec = 0.950
!---
!-- assign the temperature difference of emissions (air+cloud) from
!-- the surface air temperature
!---
    dtair   = zero
    dtcloud = zero
!---
!-- total downward ir is equal to the sum of:
!---
!-- (1) clear sky contribution to downward ir radiation flux
!-- (2) cloud contribution to downward ir radiation flux
!---
    fira(i) = (1.-cloud(i))*ea*c_stefan*(ta(i)-dtair)**4 &
 &           +cloud(i)*ec*c_stefan*(ta(i)-dtcloud)**4
  ENDDO
!-
! snow and rain calculations
!-
  DO i=1,npoi
!---
!-- reset snow and rain to zero
!---
    snowa(i) = zero
    raina(i) = zero
!---
!-- if precipitation event then calculate
!---
    IF (time >= startp .and. time < endp) then
!-----
!---- for rain / snow partitioning, make it all rain if
!---- ta > 2.5 C and all snow if ta <= 2.5 C
!-----
!---- reference:
!-----
!---- Auer, A. H., 1974: The rain versus snow threshold temperatures,
!---- Weatherwise, 27, 67.
!-----
      IF (ta(i)-273.15 > 2.5) then
        raina(i) = precip(i)/plens
      ELSE
        snowa(i) = precip(i)/plens
      ENDIF
    ENDIF
  ENDDO
!---------------------
END SUBROUTINE diurnal
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : weathgen_main
!!
!>\BRIEF        This subroutine manages the calls to the different subroutines of the weather module. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::tair, ::pb, ::qair, ::swdown, ::rainf, ::snowf, ::u, ::v, ::lwdown
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_main &
& (itau, istp, filename, force_id, iim, jjm, nband, &
&  rest_id, lrstread, lrstwrite, &
&  limit_west, limit_east, limit_north, limit_south, &
&  zonal_res, merid_res, lon, lat, date0, dt_force, &
&  kindex, nbindex, &
&  swdown, rainf, snowf, tair, u, v, qair, pb, lwdown)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN)                  :: itau,istp
  CHARACTER(LEN=*),INTENT(IN)         :: filename
  INTEGER,INTENT(IN)                  :: force_id
  INTEGER,INTENT(IN)                  :: iim,jjm
  INTEGER,INTENT(IN)                  :: nband
  INTEGER,INTENT(IN)                  :: rest_id
  LOGICAL,INTENT(IN)                  :: lrstread, lrstwrite
  REAL,INTENT(IN)                     :: limit_west,limit_east
  REAL,INTENT(IN)                     :: limit_north,limit_south
  REAL,INTENT(IN)                     :: zonal_res,merid_res
  REAL,INTENT(IN)                     :: date0,dt_force
  REAL,DIMENSION(iim,jjm),INTENT(IN)  :: lon,lat  !! longitude, latitude

  !! 0.2 Output variables 

  REAL,DIMENSION(iim,jjm),INTENT(OUT) :: &
 &  tair,pb,qair,swdown,rainf,snowf, u,v,lwdown

  !! 0.3 Mofified variables

  INTEGER,INTENT(INOUT)                    :: nbindex
  INTEGER,DIMENSION(iim*jjm),INTENT(INOUT) :: kindex

  !! 0.4 Local variables

  REAL, ALLOCATABLE, DIMENSION(:,:) :: &
 &  tair_g,pb_g,qair_g,swdown_g,rainf_g,snowf_g, u_g,v_g,lwdown_g
  REAL,DIMENSION(nbindex) :: &
 &  tairl,pbl,qairl,swdownl,rainfl,snowfl, ul,vl,lwdownl
  INTEGER :: i,j,ij
!---------------------------------------------------------------------
  IF (lrstread) THEN
    CALL weathgen_begin ( &
     & dt_force,itau, date0, &
     & rest_id,iim,jjm, &
     & lon,lat,tair,pb,qair,kindex,nbindex)
        RETURN
  ELSE
    CALL weathgen_get &
 &   (itau, date0, dt_force, nbindex, nband, lat_land, &
 &    swdownl, rainfl, snowfl, tairl, ul, vl, qairl, pbl, lwdownl)

    tair(:,:)=val_exp
    qair(:,:)=val_exp
    pb(:,:)=val_exp
    DO ij=1,nbindex
       j = (((kindex(ij)-1)/iim) + 1)
       i = (kindex(ij) - (j-1)*iim)
       !
       swdown(i,j)=swdownl(ij)
       rainf(i,j)=rainfl(ij)
       snowf(i,j)=snowfl(ij)
       tair(i,j)=tairl(ij)
       u(i,j)=ul(ij)
       v(i,j)=vl(ij)
       qair(i,j)=qairl(ij)
       pb(i,j)=pbl(ij)
       lwdown(i,j)=lwdownl(ij)
    ENDDO
!---
    IF (dump_weather) THEN
      ALLOCATE(tair_g(iim_g,jjm_g))
      ALLOCATE(pb_g(iim_g,jjm_g))
      ALLOCATE(qair_g(iim_g,jjm_g))
      ALLOCATE(swdown_g(iim_g,jjm_g))
      ALLOCATE(rainf_g(iim_g,jjm_g))
      ALLOCATE(snowf_g(iim_g,jjm_g))
      ALLOCATE(u_g(iim_g,jjm_g))
      ALLOCATE(v_g(iim_g,jjm_g))
      ALLOCATE(lwdown_g(iim_g,jjm_g))

      CALL gather2D_mpi(tair, tair_g)
      CALL gather2D_mpi(pb, pb_g)
      CALL gather2D_mpi(qair, qair_g)
      CALL gather2D_mpi(swdown, swdown_g)
      CALL gather2D_mpi(rainf, rainf_g)
      CALL gather2D_mpi(snowf, snowf_g)
      CALL gather2D_mpi(u, u_g)
      CALL gather2D_mpi(v, v_g)
      CALL gather2D_mpi(lwdown, lwdown_g)
      IF (is_root_prc) THEN
         CALL weathgen_dump &
 &           (itau, dt_force, iim_g, jjm_g, nbp_glo, index_g, lrstwrite, &
 &            swdown_g, rainf_g, snowf_g, tair_g, u_g, v_g, qair_g, pb_g, lwdown_g)
      ENDIF
    ENDIF
!---
    IF (lrstwrite) THEN
      CALL weathgen_restwrite (rest_id,istp,iim,jjm,nbindex,kindex)
    ENDIF
  ENDIF
!---------------------------
END SUBROUTINE weathgen_main
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : weathgen_init 
!!
!>\BRIEF         This subroutine initializes weather variables.
!!
!! DESCRIPTION  : None 
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):  ::kindex, ::nbindex
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_init &
     & (filename,dt_force,force_id,iim,jjm, &
     &  zonal_res,merid_res,lon,lat,kindex,nbindex)
!!$,iind,jind)
  !---------------------------------------------------------------------
  IMPLICIT NONE
  !-

  !! 0. Variables and parameters declaration

  REAL,PARAMETER  :: fcrit = .5       !! Minimum land fraction on original grid

  !! 0.1 Input variables

  CHARACTER(LEN=*),INTENT(IN)         :: filename
  REAL,INTENT(IN)                     :: dt_force
  INTEGER,INTENT(IN)                  :: iim, jjm
  REAL,INTENT(IN)                     :: zonal_res,merid_res
  REAL,DIMENSION(iim,jjm),INTENT(IN)  :: lon,lat

  !! 0.2 Output variables  

  INTEGER,INTENT(OUT)                 :: nbindex
  INTEGER,DIMENSION(iim*jjm),INTENT(OUT)  :: kindex
!!$  INTEGER,DIMENSION(iim),INTENT(OUT) :: iind
!!$  INTEGER,DIMENSION(jjm),INTENT(OUT) :: jind

  !! 0.3 Modified variables 

  INTEGER,INTENT(INOUT)               :: force_id

  !! 0.4 Local variables

  REAL,DIMENSION(:),ALLOCATABLE         :: lon_file, lon_temp
  REAL,DIMENSION(:,:),ALLOCATABLE       :: nav_lon, nav_lat
  REAL,DIMENSION(:),ALLOCATABLE         :: lat_file, lat_temp
  REAL,DIMENSION(:,:),ALLOCATABLE       :: lsm_file
  REAL     :: xextent_file, yextent_file, xres_file, yres_file
  INTEGER  :: i, j, plotstep
  REAL,DIMENSION(iim,jjm)               :: mask
  CHARACTER(LEN=1),DIMENSION(0:1)       :: maskchar
  CHARACTER(LEN=30)                     :: var_name
  REAL     :: x_cut
  REAL,DIMENSION(iim) :: tmp_lon
  REAL,DIMENSION(jjm) :: tmp_lat

!_ ================================================================================================================================

  !-
  ! 0. messages, initialisations
  !-
  WRITE(numout,*) &
       &  'weathgen_init: Minimum land fraction on original grid =',fcrit
  CALL ioget_calendar(calendar_str)
  !-
  ! 1. on lit les longitudes et latitudes de la grille de depart
  !    ainsi que le masque terre-ocean
  !-
  CALL flinclo(force_id)
  CALL flininfo (filename,iim_file,jjm_file,llm_file,ttm_file,force_id)
  !-
  ALLOC_ERR=-1
  ALLOCATE(nav_lon(iim_file,jjm_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
     WRITE(numout,*) "ERROR IN ALLOCATION of nav_lon : ",ALLOC_ERR
     STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(nav_lat(iim_file,jjm_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
     WRITE(numout,*) "ERROR IN ALLOCATION of nav_lat : ",ALLOC_ERR
     STOP 
  ENDIF
  !-
  var_name='nav_lon'
  CALL flinget (force_id,var_name,iim_file,jjm_file,0,0,1,1,nav_lon)
  var_name='nav_lat'
  CALL flinget (force_id,var_name,iim_file,jjm_file,0,0,1,1,nav_lat)
  !-

  ALLOC_ERR=-1
  ALLOCATE(lon_file(iim_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
     WRITE(numout,*) "ERROR IN ALLOCATION of lon_file : ",ALLOC_ERR
     STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(lat_file(jjm_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
     WRITE(numout,*) "ERROR IN ALLOCATION of lat_file : ",ALLOC_ERR
     STOP 
  ENDIF
  !-
  DO i=1,iim_file
     lon_file(i) = nav_lon(i,1)
  ENDDO
  DO j=1,jjm_file
     lat_file(j) = nav_lat(1,j)
  ENDDO
!-

  ALLOC_ERR=-1
  ALLOCATE(lsm_file(iim_file,jjm_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of lsm_file : ",ALLOC_ERR
    STOP 
  ENDIF
!-
  var_name='lsmera'
  CALL flinget (force_id,var_name,iim_file,jjm_file,0,0,1,1,lsm_file)
!-
! 2. La resolution du modele ne doit pas etre superieure
!    a celle de la grille de depart
!-
  xextent_file = lon_file(iim_file)-lon_file(1)
  yextent_file = lat_file(1)-lat_file(jjm_file)
  xres_file = xextent_file/REAL(iim_file-1)
  yres_file = yextent_file/REAL(jjm_file-1)
!-
  IF (xres_file > zonal_res) THEN
    WRITE(numout,*) 'Zonal resolution of model grid too fine.'
    WRITE(numout,*) 'Resolution of original data (deg): ', xres_file
    STOP
  ENDIF
!-
  IF (yres_file > merid_res) THEN
    WRITE(numout,*) 'Meridional resolution of model grid too fine.'
    WRITE(numout,*) 'Resolution of original data (deg): ', yres_file
    STOP
  ENDIF
!-
! 3. On verifie la coherence des coordonnees de depart et d'arrivee.
!    Sinon, il faut deplacer une partie du monde (rien que ca).
!-
  i_cut = 0
!-

  ALLOC_ERR=-1
  ALLOCATE(lon_temp(iim_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of lon_temp : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(lat_temp(jjm_file), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of lat_temp : ",ALLOC_ERR
    STOP 
  ENDIF
!-
  IF (lon(iim,1) > lon_file(iim_file)) THEN
!-- A l'Est de la region d'interet, il n'y a pas de donnees
!-- le bout a l'Ouest de la region d'interet est deplace de 360 deg
!-- vers l'Est
    x_cut = lon(1,1)
    DO i=1,iim_file
      IF (lon_file(i) < x_cut) i_cut = i
    ENDDO
    IF ((i_cut < 1).OR.(i_cut >= iim_file)) THEN
        STOP 'Cannot find longitude for domain shift'
    ENDIF
!---
    lon_temp(1:iim_file-i_cut-1) = lon_file(i_cut:iim_file)
    lon_temp(iim_file-i_cut:iim_file) = lon_file(1:i_cut+1)+360.
    lon_file(:) = lon_temp(:)
  ELSEIF (lon(1,1) < lon_file(1)) THEN
!-- A l'Ouest de la region d'interet, il n'y a pas de donnees
!-- le bout a l'Est de la region d'interet est deplace de 360 deg
!-- vers l'Ouest
    x_cut = lon(iim,1)
    DO i=1,iim_file
      IF (lon_file(i) < x_cut) i_cut = i
    ENDDO
    IF ( ( i_cut < 1 ) .OR. ( i_cut >= iim_file ) ) THEN
        STOP 'Cannot find longitude for domain shift'
    ENDIF
!---
    lon_temp(1:iim_file-i_cut-1) = lon_file(i_cut:iim_file) -360.
    lon_temp(iim_file-i_cut:iim_file) = lon_file(1:i_cut+1)
    lon_file(:) = lon_temp(:)
  ENDIF
!-
  DEALLOCATE (lon_temp)
  DEALLOCATE (lat_temp)
!-
  IF (    (lon(1,1) < lon_file(1)) &
 &    .OR.(     (lon(iim,1) > lon_file(iim_file)) &
 &         .AND.(lon(iim,1) > lon_file(1)+360.) ) ) THEN
    WRITE(numout,*) lon(:,1)
    WRITE(numout,*)
    WRITE(numout,*) lon_file(:)
    STOP 'weathgen_init: cannot find coherent x-coordinates'
  ENDIF
!-
  IF (i_cut /= 0) THEN
    CALL shift_field (iim_file,jjm_file,i_cut,lsm_file)
  ENDIF
!-
! 4. Verification
!-
  WRITE(numout,*)
  WRITE(numout,*) 'Input File: (Shifted) Global Land-Sea Mask'
  WRITE(numout,*)
  maskchar(0) = '-'
  maskchar(1) = 'X'
  plotstep = INT(REAL(iim_file-1)/termcol)+1
  DO j=1,jjm_file,plotstep
    DO i=1,iim_file,plotstep
      WRITE(numout,'(a1,$)') maskchar(NINT(lsm_file(i,j)))
    ENDDO
    WRITE(numout,*)
  ENDDO
  WRITE(numout,*)
!-
! 5. Prepare interpolation from fine grid land points to model grid
!-
! 5.1 how many grid points of the original grid fall into one grid
!     box of the model grid?
!-
  n_agg = NINT((zonal_res/xres_file*merid_res/yres_file )+1.)
!-
! au diable l'avarice !
!-
  n_agg = n_agg*2
!-
! 5.2 Allocate arrays where we store information about which
!     (and how many) points on the original grid fall
!     into which box on the model grid
!-

  ALLOC_ERR=-1
  ALLOCATE(ncorr(iim,jjm), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of ncorr : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(icorr(iim,jjm,n_agg), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of icorr : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(jcorr(iim,jjm,n_agg), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of jcorr : ",ALLOC_ERR
    STOP 
  ENDIF
!-
! 6. Land-Ocean Mask on model grid
!-
  tmp_lon = lon(:,1)
  tmp_lat = lat(1,:)

  CALL mask_c_o &
 &  (iim_file, jjm_file, lon_file, lat_file, lsm_file, fcrit, &
 &   iim, jjm, zonal_res, merid_res, n_agg, tmp_lon, tmp_lat, &
! &   iim, jjm, zonal_res, merid_res, n_agg, lon(:,1), lat(1,:), &
 &   mask, ncorr, icorr, jcorr)
!-
  WRITE(numout,*) 'Land-Sea Mask on Model Grid'
  WRITE(numout,*)
  plotstep = INT(REAL(iim-1)/termcol)+1
  DO j=1,jjm,plotstep
    DO i=1,iim,plotstep
      WRITE(numout,'(a1,$)') maskchar(NINT(mask(i,j)))
    ENDDO
    WRITE(numout,*)
  ENDDO
  WRITE(numout,*)
!-
! 7. kindex table.
!-
  nbindex = 0
  DO j=1,jjm
    DO i=1,iim
      IF (NINT(mask(i,j)) == 1) THEN
        nbindex = nbindex+1
        kindex(nbindex) = (j-1)*iim+i
      ENDIF
    ENDDO
  ENDDO
  nbindex_w = nbindex
  ALLOC_ERR=-1
  ALLOCATE(kindex_w(nbindex_w), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of kindex_w : ",ALLOC_ERR
    STOP 
  ENDIF
  kindex_w(:)=kindex(1:nbindex)
!-
  IF ( nbindex == 0 ) THEN
    WRITE(numout,*) 'Sorry, you are in the middle of the ocean. Check your coordinates.'
    STOP 
  ELSE
    WRITE(numout,*) 'Number of continental points: ',nbindex
  ENDIF

!-
! 10. clean up
!-
  DEALLOCATE (lon_file)
  DEALLOCATE (lat_file)
  DEALLOCATE (lsm_file)

END SUBROUTINE weathgen_init

!! ================================================================================================================================
!! SUBROUTINE   : weathgen_read_file
!!
!>\BRIEF         This subroutine allocates variables and reads files.  
!!
!! DESCRIPTION  :  
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): 
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_read_file &
     & (force_id,iim,jjm)
  !---------------------------------------------------------------------
  IMPLICIT NONE
  !-

  !! 0. Variables and parameters declaration

  REAL,PARAMETER  :: fcrit = .5

  !! 0.1 Input variables 

  INTEGER,INTENT(IN)                  :: force_id
  INTEGER,INTENT(IN)                  :: iim, jjm

  !! 0.4 Local variables

  CHARACTER(LEN=30)               :: var_name

  INTEGER,DIMENSION(iim*jjm)  :: kindex
  INTEGER                     :: nbindex

  REAL,DIMENSION(iim*jjm)     :: xchamp
  REAL,DIMENSION(iim*jjm,nmon)  :: xchampm

!_ ================================================================================================================================

  kindex(:)=0

  nbindex=nbp_loc
  CALL scatter(index_g,kindex)
  kindex(1:nbindex)=kindex(1:nbindex)-(jj_begin-1)*iim_g

!-

  ALLOC_ERR=-1
  ALLOCATE(lat_land(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of lat_land : ",ALLOC_ERR
    STOP 
  ENDIF

!-
! 8 topography
!-

  ALLOC_ERR=-1
  ALLOCATE(xintopo(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xintopo : ",ALLOC_ERR
    STOP 
  ENDIF
!-
  var_name='altitude'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,1,i_cut, &
                     iim,jjm,n_agg,ncorr,icorr,jcorr,xchamp)
  xintopo(:)=xchamp(kindex(1:nbindex))
!-
! 9. Allocate and read the monthly fields
!-

  ALLOC_ERR=-1
  ALLOCATE(xinwet(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xinwet : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='prs'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                     iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xinwet(:,:)=xchampm(kindex(1:nbindex),:)
  !-

  ALLOC_ERR=-1
  ALLOCATE(xinprec(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xinprec : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='prm'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                     iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xinprec(:,:)=xchampm(kindex(1:nbindex),:)
!-
  
  ALLOC_ERR=-1
  ALLOCATE(xint(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xint : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='t2m'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                     iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xint(:,:)=xchampm(kindex(1:nbindex),:)
!-

  ALLOC_ERR=-1
  ALLOCATE(xinq(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xinq : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='r2m'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                     iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xinq(:,:)=xchampm(kindex(1:nbindex),:)
!-

  ALLOC_ERR=-1
  ALLOCATE(xinwind(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xinwind : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='uv10m'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                     iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xinwind(:,:)=xchampm(kindex(1:nbindex),:)
!-

  ALLOC_ERR=-1
  ALLOCATE(xincld(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xincld : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='tc'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                       iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xincld(:,:)=xchampm(kindex(1:nbindex),:)
!-

  ALLOC_ERR=-1
  ALLOCATE(xintrng(nbindex,nmon), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xintrng : ",ALLOC_ERR
    STOP 
  ENDIF
  var_name='t2a'
  CALL weather_read (force_id,var_name,iim_file,jjm_file,nmon,i_cut, &
                       iim,jjm,n_agg,ncorr,icorr,jcorr,xchampm)
  xintrng(:,:)=xchampm(kindex(1:nbindex),:)
!-
! 10. clean up
!-
  IF (is_root_prc) THEN
     DEALLOCATE (ncorr)
     DEALLOCATE (icorr)
     DEALLOCATE (jcorr)
  ENDIF

!-
! 12. Allocate space for daily mean values
!-

  ALLOC_ERR=-1
  ALLOCATE(iwet(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of iwet : ",ALLOC_ERR
    STOP 
  ENDIF
!-

  ALLOC_ERR=-1
  ALLOCATE(psurfm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of psurfm0 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(cloudm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of cloudm0 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(tmaxm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of tmaxm0 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(tminm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of tminm0 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(qdm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of qdm0 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(udm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of udm0 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(precipm0(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of precipm0 : ",ALLOC_ERR
    STOP 
  ENDIF
!-

  ALLOC_ERR=-1
  ALLOCATE(psurfm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of psurfm1 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(cloudm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of cloudm1 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(tmaxm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of tmaxm1 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(tminm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of tminm1 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(qdm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of qdm1 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(udm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of udm1 : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(precipm1(nbindex), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of precipm1 : ",ALLOC_ERR
    STOP 
  ENDIF
END SUBROUTINE weathgen_read_file
!
!=
!

!! ================================================================================================================================
!! SUBROUTINE   : weathgen_begin
!!
!>\BRIEF         This subroutines initializes variables or reads restart values. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::nbindex, ::tair, ::pb, ::qair, ::kindex
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_begin ( &
     & dt_force,itau, date0, &
     & rest_id,iim,jjm, &
     & lon,lat,tair,pb,qair,kindex,nbindex)
  !---------------------------------------------------------------------
  IMPLICIT NONE

  !-

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  REAL,INTENT(IN)                     :: dt_force, date0
  INTEGER,INTENT(IN)                  :: itau
  INTEGER,INTENT(IN)                  :: rest_id
  INTEGER,INTENT(IN)                  :: iim, jjm
  REAL,DIMENSION(iim,jjm),INTENT(IN)  :: lon,lat

  !! 0.2 Output variables 

  INTEGER,INTENT(OUT)                     :: nbindex
  INTEGER,DIMENSION(iim*jjm),INTENT(OUT)  :: kindex
  REAL,DIMENSION(iim,jjm),INTENT(OUT)     :: tair,pb,qair

  !! 0.4 Local variables

  INTEGER  :: i, j, ij
  REAL     :: val_exp
  REAL,DIMENSION(iim*jjm)               :: xchamp
  REAL,DIMENSION(iim_g*jjm_g)           :: xchamp_g
  CHARACTER(LEN=30)                     :: var_name
  REAL,DIMENSION(1)                     :: jullasttab
  REAL,DIMENSION(seedsize_max)          :: seed_in_file
  INTEGER,DIMENSION(:), ALLOCATABLE     :: seed_in_proc
  INTEGER  :: seedsize, iret
  INTEGER  :: nlonid, nlatid, nlonid1, nlatid1, tdimid1, varid
  INTEGER  :: ndim, dims(4)
  CHARACTER(LEN=30) :: assoc
  CHARACTER(LEN=70) :: str70
  CHARACTER(LEN=80) :: stamp
  INTEGER  :: yy_b, mm_b, dd_b, hh, mm
  REAL     :: ss
  CHARACTER(LEN=10) :: today, att
  INTEGER  :: nlandid1, nlandid, nlevid, nlevid1
  REAL     :: lev_max, lev_min
  REAL     :: height_lev1
  INTEGER  :: imois
  REAL     :: xx, td

!_ ================================================================================================================================

  kindex(:)=0

  nbindex=nbp_loc
  CALL scatter(index_g,kindex)
  kindex(1:nbindex)=kindex(1:nbindex)-(jj_begin-1)*iim_g
!-
! 13. Prescribed or statistical values?
!-
  !Config Key   = IPPREC
  !Config Desc  = Use prescribed values
  !Config If    = ALLOW_WEATHERGEN
  !Config Def   = 0
  !Config Help  = If this is set to 1, the weather generator
  !Config         uses the monthly mean values for daily means.
  !Config         If it is set to 0, the weather generator
  !Config         uses statistical relationships to derive daily
  !Config         values from monthly means.
  !Config Units = [-] 
  ipprec = 0
  CALL getin_p ('IPPREC',ipprec)
  WRITE(numout,*) 'IPPREC: ',ipprec
!-
! 14. Do we want exact monthly precipitations even with ipprec=0 ?
!-
  !Config Key   = WEATHGEN_PRECIP_EXACT
  !Config Desc  = Exact monthly precipitation
  !Config If    = ALLOW_WEATHERGEN
  !Config Def   = n
  !Config Help  = If this is set to y, the weather generator
  !Config         will generate pseudo-random precipitations
  !Config         whose monthly mean is exactly the prescribed one.
  !Config         In this case, the daily precipitation (for rainy
  !Config         days) is constant (that is, some days have 0 precip,
  !Config         the other days have precip=Precip_month/n_precip,
  !Config         where n_precip is the prescribed number of rainy days
  !Config         per month).
  !Config Units = [FLAG]
  precip_exact = .FALSE.
  CALL getin_p ('WEATHGEN_PRECIP_EXACT',precip_exact)
  WRITE(numout,*) 'PRECIP_EXACT: ',precip_exact

!-
! 15. Read restart file
!-
  CALL ioget_expval (val_exp)
!-
  var_name= 'julian'
  IF (is_root_prc) THEN
     CALL restget (rest_id,var_name,1,1,1,itau,.TRUE.,jullasttab)
     IF (jullasttab(1) == val_exp) THEN
        jullasttab(1) = itau2date(itau-1, date0, dt_force)
     ENDIF
  ENDIF
  CALL bcast(jullasttab)
  julian_last = jullasttab(1)
!-
  var_name= 'seed'
  IF (is_root_prc) &
       CALL restget (rest_id,var_name,seedsize_max, &
 &              1,1,itau,.TRUE.,seed_in_file)
  CALL bcast(seed_in_file)
  IF (ALL(seed_in_file(:) == val_exp)) THEN
!---
!-- there is no need to reinitialize the random number generator as
!-- this does not seem to be a restart
!---
    CONTINUE
  ELSE
!---
!-- reinitialize the random number generator
!---
     IF (is_root_prc) &
          CALL RANDOM_SEED( SIZE = seedsize )
     CALL bcast(seedsize)
     IF (seedsize > seedsize_max) THEN
        STOP 'weathgen_begin: increase seedsize_max'
     ENDIF
  
     IF (precip_exact) THEN
!---
!-- preparer un tableau utilise pour determiner s'il pleut ou pas
!-- (en fct. du nombre de jours de pluie par mois).
!---
        IF (is_root_prc) THEN
           DO imois=1,12
              CALL permute (ndaypm(imois),jour_precip(:,imois))
           ENDDO
        ENDIF
       CALL bcast(jour_precip)
    ENDIF

    ALLOC_ERR=-1
    ALLOCATE(seed_in_proc(seedsize), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) THEN
      WRITE(numout,*) "ERROR IN ALLOCATION of seed_in_proc : ",ALLOC_ERR
      STOP 
    ENDIF
    seed_in_proc(1:seedsize) = NINT( seed_in_file(1:seedsize) )
    CALL RANDOM_SEED (PUT = seed_in_proc)
    DEALLOCATE( seed_in_proc )
 ENDIF
!-
  var_name= 'iwet'
  IF (is_root_prc) THEN
    CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
    IF (ALL(xchamp_g(:) == val_exp)) THEN
      xchamp_g(:) = zero
    ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  iwet(:) = NINT(xchamp(kindex(1:nbindex)))
!-
  var_name= 'psurfm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 100000.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  psurfm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'cloudm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = .5
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  cloudm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'tmaxm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 285.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  tmaxm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'tminm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 275.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  tminm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'qdm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 1.E-03
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  qdm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'udm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 2.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  udm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'precipm0'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 1.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  precipm0(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'psurfm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 100000.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  psurfm1(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'cloudm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = .5
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  cloudm1(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'tmaxm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 285.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  tmaxm1(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'tminm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 275.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  tminm1(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'qdm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 1.E-03
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  qdm1(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'udm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 2.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  udm1(:) = xchamp(kindex(1:nbindex))
!-
  var_name= 'precipm1'
  IF (is_root_prc) THEN
     CALL restget (rest_id, var_name, iim_g, jjm_g, 1, itau, .TRUE., xchamp_g)
     IF (ALL(xchamp_g(:) == val_exp)) THEN
        xchamp_g(:) = 1.
     ENDIF
  ENDIF
  CALL scatter2D_mpi(xchamp_g,xchamp)
  precipm1(:) = xchamp(kindex(1:nbindex))

!-
! 16. We still need instantaneous tair, qair, and the surface pressure
!     We take daily mean values read from the restart file
!-
!!$  tair(:,:)=280.
!!$  qair(:,:)=1.E-03
!!$  pb(:,:)=101325
  tair(:,:)=val_exp
  qair(:,:)=val_exp
  pb(:,:)=val_exp
  xx = cte_grav/rair/0.0065
  DO ij=1,nbindex
     j = ((kindex(ij)-1)/iim) + 1
     i = kindex(ij) - (j-1)*iim
     
     lat_land(ij) = lat(i,j)
     
     td =  (tmaxm0(ij)+tminm0(ij))/2.
     tair(i,j) = td
     qair(i,j) = qdm1(ij)
     pb(i,j) = 101325.*(td/(td+0.0065*xintopo(ij)))**xx
  ENDDO
!-
! 17. We can write a forcing file for Orchidee
!     from this weather Generator run.
!-
  !Config Key   = DUMP_WEATHER
  !Config Desc  = Write weather from generator into a forcing file
  !Config If    = ALLOW_WEATHERGEN  
  !Config Def   = n
  !Config Help  = This flag makes the weather generator dump its
  !Config         generated weather into a forcing file which can
  !Config         then be used to get the same forcing on different
  !Config         machines. This only works correctly if there is
  !Config         a restart file (otherwise the forcing at the first
  !Config         time step is slightly wrong).
  !Config Units = [FLAG]
  dump_weather = .FALSE.
  CALL getin_p ('DUMP_WEATHER',dump_weather)
!-
  IF (dump_weather .AND. is_root_prc) THEN
!---
!-- Initialize the file
!---
     !Config Key   = DUMP_WEATHER_FILE
     !Config Desc  = Name of the file that contains the weather from generator
     !Config If    = DUMP_WEATHER
     !Config Def   = weather_dump.nc
     !Config Help  =
     !Config Units = [FILE]
    dump_weather_file = 'weather_dump.nc'
    CALL getin ('DUMP_WEATHER_FILE',dump_weather_file)
!---
    !Config Key   = DUMP_WEATHER_GATHERED
    !Config Desc  = Dump weather data on gathered grid
    !Config If    = DUMP_WEATHER
    !Config Def   = y
    !Config Help  = If 'y', the weather data are gathered for all land points.
    !Config Units = [FLAG]
    gathered = .TRUE.
    CALL getin ('DUMP_WEATHER_GATHERED',gathered)
!---
    iret = NF90_CREATE (TRIM(dump_weather_file),NF90_CLOBBER,dump_id)
!---
!-- Dimensions
!---
    iret = NF90_DEF_DIM (dump_id,'x',iim_g,nlonid1)
    iret = NF90_DEF_DIM (dump_id,'y',jjm_g,nlatid1)
    iret = NF90_DEF_DIM (dump_id,'z',  1,nlevid1)
!---
    IF (gathered) THEN
      iret = NF90_DEF_DIM (dump_id,'land',nbp_glo,nlandid1)
    ENDIF
    iret = NF90_DEF_DIM (dump_id,'tstep',NF90_UNLIMITED,tdimid1)
!---
!-- Coordinate variables
!---
    dims(1:2) = (/ nlonid1, nlatid1 /)
!---
    iret = NF90_DEF_VAR (dump_id,'nav_lon',n_rtp,dims(1:2),nlonid)
    iret = NF90_PUT_ATT (dump_id,nlonid,'units',"degrees_east")
    iret = NF90_PUT_ATT (dump_id,nlonid,'valid_min',MINVAL(lon_g))
    iret = NF90_PUT_ATT (dump_id,nlonid,'valid_max',MAXVAL(lon_g))
    iret = NF90_PUT_ATT (dump_id,nlonid,'long_name',"Longitude")
!---
    iret = NF90_DEF_VAR (dump_id,'nav_lat',n_rtp,dims(1:2),nlatid)
    iret = NF90_PUT_ATT (dump_id,nlatid,'units',"degrees_north")
    iret = NF90_PUT_ATT (dump_id,nlatid,'valid_min',MINVAL(lat_g))
    iret = NF90_PUT_ATT (dump_id,nlatid,'valid_max',MAXVAL(lat_g))
    iret = NF90_PUT_ATT (dump_id,nlatid,'long_name',"Latitude")
!---
    height_lev1 = 10.
    !Config Key   = HEIGHT_LEV1_DUMP
    !Config Desc  = 
    !Config If    = DUMP_WEATHER
    !Config Def   = 10.
    !Config Help  = 
    !Config Units = [m]
    CALL getin ('HEIGHT_LEV1_DUMP',height_lev1)
    lev_min = height_lev1
    lev_max = height_lev1
!---
    iret = NF90_DEF_VAR (dump_id,'level',n_rtp,(/ nlevid1 /),nlevid)
    iret = NF90_PUT_ATT (dump_id,nlevid,'units',"m")
    iret = NF90_PUT_ATT (dump_id,nlevid,'valid_min',lev_min)
    iret = NF90_PUT_ATT (dump_id,nlevid,'valid_max',lev_max)
    iret = NF90_PUT_ATT (dump_id,nlevid,'long_name',"Vertical levels")
!---
    IF (gathered) THEN
      iret = NF90_DEF_VAR (dump_id,'land',NF90_INT,(/ nlandid1 /),nlandid)
      iret = NF90_PUT_ATT (dump_id,nlandid,'compress',"y x")
    ENDIF
!---
!-- Store the time axes.
!---
    iret = NF90_DEF_VAR (dump_id,'time',n_rtp,tdimid1,time_id)

    yy_b=0
    mm_b=1
    dd_b=1
    hh=00
    mm=00
    ss=0.

    WRITE (str70,7000) yy_b, mm_b, dd_b, hh, mm, INT(ss)
    iret = NF90_PUT_ATT (dump_id,time_id,'units',TRIM(str70))
    iret = NF90_PUT_ATT (dump_id,time_id,'calendar',TRIM(calendar_str))
    iret = NF90_PUT_ATT (dump_id,time_id,'title','Time')
    iret = NF90_PUT_ATT (dump_id,time_id,'long_name','Time axis')
    WRITE(str70,7001) yy_b, cal(mm_b), dd_b, hh, mm, INT(ss)
    iret = NF90_PUT_ATT (dump_id,time_id,'time_origin',TRIM(str70))
!---
!-- Time steps
!---
    iret = NF90_DEF_VAR (dump_id,'timestp',NF90_INT,tdimid1,timestp_id)
    WRITE(str70,7002) yy_b, mm_b, dd_b, hh, mm, INT(ss)
    iret = NF90_PUT_ATT (dump_id,timestp_id,'units',TRIM(str70))
    iret = NF90_PUT_ATT (dump_id,timestp_id,'title','Time steps')
    iret = NF90_PUT_ATT (dump_id,timestp_id,'tstep_sec',dt_force)
    iret = NF90_PUT_ATT &
 &           (dump_id,timestp_id,'long_name','Time step axis')
    WRITE(str70,7001) yy_b, cal(mm_b), dd_b, hh, mm, INT(ss)
    iret = NF90_PUT_ATT (dump_id,timestp_id,'time_origin',TRIM(str70))
!---
7000 FORMAT('seconds since ',I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2)
7001 FORMAT(' ',I4.4,'-',A3,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2)
7002 FORMAT('timesteps since ', I4.4,'-',I2.2,'-',I2.2,' ',I2.2,':',I2.2,':',I2.2)
!---
!-- The variables in the file are declared and their attributes
!-- written.
!---
    IF (gathered) THEN
      ndim = 2
      dims(1:2) = (/ nlandid1, tdimid1 /)
      assoc = 'time (nav_lat nav_lon)'
    ELSE
      ndim = 3
      dims(1:3) = (/ nlonid1, nlatid1, tdimid1 /)
      assoc = 'time nav_lat nav_lon'
    ENDIF
!---
    iret = NF90_DEF_VAR (dump_id,'SWdown',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','W/m^2')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Surface incident shortwave radiation')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    soldownid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'Rainf',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','kg/m^2s')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Rainfall rate')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    rainfid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'Snowf',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','kg/m^2s')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Snowfall rate')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    snowfid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'LWdown',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','W/m^2')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Surface incident longwave radiation')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    lwradid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'PSurf',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','Pa')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Surface pressure')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    psolid = varid
!---
!-- 3D Variables to be written
!---
    IF (gathered) THEN
      ndim = 3
      dims(1:3) = (/ nlandid1, nlevid1, tdimid1 /)
      assoc = 'time level (nav_lat nav_lon)'
    ELSE
      ndim = 4
      dims(1:4) = (/ nlonid1, nlatid1, nlevid1, tdimid1 /)
      assoc = 'time level nav_lat nav_lon'
    ENDIF
!---
    iret = NF90_DEF_VAR (dump_id,'Tair',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TZYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','K')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Near surface air temperature')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    tairid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'Qair',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TZYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','kg/kg')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Near surface specific humidity')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    qairid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'Wind_N',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TZYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','m/s')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Near surface northward wind component')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    uid = varid
!---
    iret = NF90_DEF_VAR (dump_id,'Wind_E',n_rtp,dims(1:ndim),varid)
    iret = NF90_PUT_ATT (dump_id,varid,'axis','TZYX')
    iret = NF90_PUT_ATT (dump_id,varid,'units','m/s')
    iret = NF90_PUT_ATT (dump_id,varid,'long_name', &
 &                       'Near surface eastward wind component')
    iret = NF90_PUT_ATT (dump_id,varid,'associate',TRIM(assoc))
    iret = NF90_PUT_ATT (dump_id,varid,'missing_value',undef_sechiba)
    vid = varid
!---
!-- Global attributes
!---
    CALL DATE_AND_TIME (today, att)
    stamp = "WG, date: "//TRIM(today)//" at "//TRIM(att)
    iret = NF90_PUT_ATT (dump_id,NF90_GLOBAL,'Conventions',"GDT 1.2")
    iret = NF90_PUT_ATT (dump_id,NF90_GLOBAL,'file_name', &
 &                       TRIM(dump_weather_file))
    iret = NF90_PUT_ATT (dump_id,NF90_GLOBAL,'production',TRIM(stamp))
!---
!-- Finish the definition phase
!---
    iret = NF90_ENDDEF (dump_id)
!---
!--    Write coordinates
!---
    iret = NF90_PUT_VAR (dump_id,nlonid,lon_g)
    IF (iret /= NF90_NOERR) THEN
       WRITE(numout,*) iret
       CALL ipslerr_p (3,'weathgen_begin', &
 &          'Could not put variable nav_lon in the file : ', &
 &          TRIM(dump_weather_file),'(Solution ?)')
    ENDIF
    iret = NF90_PUT_VAR (dump_id,nlatid,lat_g)
    IF (iret /= NF90_NOERR) THEN
       WRITE(numout,*) iret
       CALL ipslerr_p (3,'weathgen_begin', &
 &          'Could not put variable nav_lat in the file : ', &
 &          TRIM(dump_weather_file),'(Solution ?)')
    ENDIF
    iret = NF90_PUT_VAR (dump_id,nlevid,height_lev1)
    IF (iret /= NF90_NOERR) THEN
       WRITE(numout,*) iret
       CALL ipslerr_p (3,'weathgen_begin', &
 &          'Could not put variable level in the file : ', &
 &          TRIM(dump_weather_file),'(Solution ?)')
    ENDIF
!---
    IF (gathered) THEN
       iret = NF90_PUT_VAR (dump_id,nlandid,index_g(1:nbp_glo))
       IF (iret /= NF90_NOERR) THEN
          WRITE(numout,*) iret
          CALL ipslerr_p (3,'weathgen_begin', &
 &          'Could not put variable land in the file : ', &
 &          TRIM(dump_weather_file),'(Solution ?)')
       ENDIF
    ENDIF
!---
 ENDIF ! dump_weather
!-----------------------------
END SUBROUTINE weathgen_begin
!-
!===
!-
!! ================================================================================================================================
!! SUBROUTINE   : weathgen_get
!!
!>\BRIEF        This subroutine initializes forcing values. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::swdown, ::raina, ::snowa, ::tair, ::u, ::v, ::qair, ::psurf, ::lwdown
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_get &
& (itau, date0, dt_force, nbindex, nband, lat, &
&  swdown, raina, snowa, tair, u, v, qair, psurf, lwdown)
!---------------------------------------------------------------------
  IMPLICIT NONE

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN)               :: itau      !! number of time step
  REAL,INTENT(IN)                  :: date0     !! date when itau was 0
  REAL,INTENT(IN)                  :: dt_force  !! time step (s)
  INTEGER,INTENT(IN)               :: nbindex   !! number of land points
  INTEGER,INTENT(IN)               :: nband     !! number of visible bands
  REAL,DIMENSION(nbindex),INTENT(IN)  :: lat    !! latitude (deg)

  !! 0.2 Output variables  

  REAL,DIMENSION(nbindex,nband),INTENT(OUT) :: swdown        !! Downward solar radiation @tex $(W.m^{-2})$ @endtex
  REAL,DIMENSION(nbindex),INTENT(OUT)       :: raina, snowa  !! precipitations @tex $(mm.day^{-1})$ @endtex 
  REAL,DIMENSION(nbindex),INTENT(OUT)       :: tair          !! air temperature (K)
  REAL,DIMENSION(nbindex),INTENT(OUT)       :: u,v           !! wind speed @tex $(m.s^{-1})$ @endtex
  REAL,DIMENSION(nbindex),INTENT(OUT)       :: qair          !! air humidity @tex $(kg_h2o.kg_air^{-1})$ @endtex
  REAL,DIMENSION(nbindex),INTENT(OUT)       :: psurf         !! Surface pressure (Pa)
  REAL,DIMENSION(nbindex),INTENT(OUT)       :: lwdown        !! Downward long wave radiation @tex $(W.m^{-2})$ @endtex

  !! 0.4 Local variables

  REAL,DIMENSION(nbindex)        :: cloud, tmax, tmin, precipd, qd, ud
  REAL,DIMENSION(nbindex)        :: rh
  REAL,DIMENSION(nbindex,nband)  :: solai, solad
  REAL                        :: julian, jur
  REAL                        :: x
  INTEGER                     :: yy, mm, dd
  REAL                        :: ss, plens, time

!_ ================================================================================================================================

!-
! 1. get a reduced julian day
!-
  julian = itau2date(itau-1, date0, dt_force)
!S. Zaehle, test: solar noon at 12 o'clock!
!  julian = itau2date(itau, date0, dt_force)
  CALL ju2ymds (julian, yy, mm, dd, ss)
  CALL ymds2ju (yy,1,1,0.0, jur)
  julian = julian-jur
  CALL ju2ymds (julian, yy, mm, dd, ss)
!-
! 2. daily values
!-
  IF (INT(julian_last) /= INT(julian)) THEN
!-- today's values become yesterday's values
    cloudm1(:) = cloudm0(:)
    tmaxm1(:) = tmaxm0(:)
    tminm1(:) = tminm0(:)
    precipm1(:) = precipm0(:)
    qdm1(:) = qdm0(:)
    udm1(:) = udm0(:)
    psurfm1(:) = psurfm0(:)
!-- we have to get new daily values
!!$    WRITE(*,*) mpi_rank, "weathgen_get : date ",yy, mm, dd, ss
!!$    WRITE(*,*) mpi_rank, "weathgen_get : grid date ",year, month, day, sec
    CALL daily (nbindex, mm, dd, cloudm0, tmaxm0, tminm0, &
 &              precipm0, qdm0, udm0, psurfm0)
  ENDIF
!-
! 3. interpolate daily values
!    (otherwise we get ugly temperature jumps at midnight)
!-
  x = (julian-INT(julian))
!-
  cloud(:) = (1.-x)*cloudm1(:)+x*cloudm0(:)
  tmax(:) = (1.-x)*tmaxm1(:)+x*tmaxm0(:)
  tmin(:) = (1.-x)*tminm1(:)+x*tminm0(:)
  precipd(:) = (1.-x)*precipm1(:)+x*precipm0(:)
  qd(:) = (1.-x)*qdm1(:)+x*qdm0(:)
  ud(:) = (1.-x)*udm1(:)+x*udm0(:)
  psurf(:) = (1.-x)*psurfm1(:)+x*psurfm0(:)
!-
! 4. read instantaneous values
!-
  plens = one_day/dt_force
  time = (julian-REAL(INT(julian)))*one_day
!-
  CALL diurnal (nbindex, nband, time, NINT(julian), plens, 0., one_day, &
 &              lat, cloud, tmax, tmin, precipd, qd, ud, psurf, &
 &              lwdown, solad, solai, u, tair, qair, raina, snowa, rh)
!-
  raina(:) = raina(:)/dt_force
  snowa(:) = snowa(:)/dt_force
!-
  swdown(:,:) = solad(:,:)+solai(:,:)
!-
  v(:) = zero
!-
! 5. Store date
!-
  julian_last = julian
!--------------------------
END SUBROUTINE weathgen_get
!-
!===
!-
!! ================================================================================================================================
!! SUBROUTINE   : weathgen_restwrite
!!
!>\BRIEF         This subroutine writes data in the restart file.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_restwrite (rest_id,itau,iim,jjm,nbindex,kindex)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN)  :: rest_id,itau,iim,jjm,nbindex
  INTEGER,DIMENSION(iim*jjm),INTENT(IN) :: kindex

  !! 0.4 Local variables

  CHARACTER(LEN=30)                 :: var_name
  INTEGER                           :: i,j,ij
  REAL,DIMENSION(1)                 :: jullasttab
  REAL,DIMENSION(seedsize_max)      :: seed_in_file
  INTEGER,DIMENSION(:),ALLOCATABLE  :: seed_in_proc
  INTEGER                           :: seedsize
  REAL                              :: rndnum
  REAL,DIMENSION(iim*jjm)           :: xchamp
  REAL,DIMENSION(iim_g*jjm_g)       :: xchamp_g

!_ ================================================================================================================================

  var_name= 'julian'
  jullasttab(:) = julian_last
  IF (is_root_prc) CALL restput (rest_id, var_name, 1, 1, 1, itau, jullasttab)
!-
  IF (is_root_prc) THEN
     CALL RANDOM_SEED( SIZE = seedsize )
     IF (seedsize > seedsize_max) THEN
        STOP 'weathgen_restwrite: increase seedsize_max'
     ENDIF
  ENDIF
  CALL bcast(seedsize)

  IF (is_root_prc) THEN
     ALLOC_ERR=-1
     ALLOCATE(seed_in_proc(seedsize), STAT=ALLOC_ERR)
     IF (ALLOC_ERR/=0) THEN
        WRITE(numout,*) "ERROR IN ALLOCATION of seed_in_proc : ",ALLOC_ERR
        STOP 
     ENDIF
!-
     CALL RANDOM_SEED (GET = seed_in_proc)
!-
     seed_in_file(1:seedsize) = REAL(seed_in_proc(1:seedsize))
!-
! fill in the seed up to seedsize_max
! (useful in the case we restart on
!  a machine with a longer seed vector:
!  we do not want a degenerated seed)
!-
     DO i=seedsize+1,seedsize_max
        CALL RANDOM_NUMBER( rndnum )
        seed_in_file(i) = 100000.*rndnum
     ENDDO
  ENDIF
  CALL bcast (seed_in_file)
!-
  IF (is_root_prc) THEN
     DEALLOCATE( seed_in_proc )
!-
     var_name= 'seed'
     CALL restput (rest_id,var_name,seedsize_max,1,1,itau,seed_in_file)
  ENDIF
!-

  xchamp(:) = val_exp
  
!!$  DO j=1,jjm
!!$    DO i=1,iim
!!$      ij = (j-1)*iim+i
!!$      xchamp(i,j) = REAL(iwet(ij))
!!$    ENDDO
!!$  ENDDO
  DO ij=1,nbindex
     xchamp(kindex(ij)) = REAL(iwet(ij))
  ENDDO
  var_name= 'iwet'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = psurfm0(ij)
  ENDDO
  var_name= 'psurfm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = cloudm0(ij)
  ENDDO
  var_name= 'cloudm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = tmaxm0(ij)
  ENDDO
  var_name= 'tmaxm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = tminm0(ij)
  ENDDO
  var_name= 'tminm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = qdm0(ij)
  ENDDO
  var_name= 'qdm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = udm0(ij)
  ENDDO
  var_name= 'udm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = precipm0(ij)
  ENDDO
  var_name= 'precipm0'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = psurfm1(ij)
  ENDDO
  var_name= 'psurfm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = cloudm1(ij)
  ENDDO
  var_name= 'cloudm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = tmaxm1(ij)
  ENDDO
  var_name= 'tmaxm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = tminm1(ij)
  ENDDO
  var_name= 'tminm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = qdm1(ij)
  ENDDO
  var_name= 'qdm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = udm1(ij)
  ENDDO
  var_name= 'udm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!-
  DO ij=1,nbindex
    xchamp(kindex(ij)) = precipm1(ij)
  ENDDO
  var_name= 'precipm1'
  CALL gather2D_mpi(xchamp,xchamp_g)
  IF (is_root_prc) THEN
     CALL restput (rest_id, var_name, iim_g, jjm_g, 1, itau, xchamp_g)
  ENDIF
!--------------------------------
END SUBROUTINE weathgen_restwrite
!-
!===
!-
!! ================================================================================================================================
!! SUBROUTINE   : weathgen_data
!!
!>\BRIEF         This subroutine reads data from an netcdf file. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::champout
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weather_read &
& (force_id,nomvar,iim_file,jjm_file,n3,i_cut, &
&  iim,jjm,n_agg,ncorr,icorr,jcorr,champout)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-

  !! 0. Variables and parameters declaration
  
  !! 0.1 Input variables

  INTEGER,INTENT(IN)                          :: force_id
  CHARACTER(LEN=*),INTENT(IN)                 :: nomvar
  INTEGER,INTENT(IN)                          :: iim_file,jjm_file
  INTEGER,INTENT(IN)                          :: n3
  INTEGER,INTENT(IN)                          :: i_cut
  INTEGER,INTENT(IN)                          :: iim,jjm
  INTEGER,INTENT(IN)                          :: n_agg
  INTEGER,DIMENSION(:,:),INTENT(IN)           :: ncorr
  INTEGER,DIMENSION(:,:,:),INTENT(IN)         :: icorr,jcorr

  !! 0.2 Output variables 

  REAL,DIMENSION(iim*jjm,n3),INTENT(OUT)      :: champout

  !! 0.4 Local variables

  REAL,DIMENSION(iim_file,jjm_file,n3)        :: champ_file
  REAL,ALLOCATABLE,DIMENSION(:,:)             :: champout_g
  INTEGER                                     :: i,j,ij,l,m

!_ ================================================================================================================================

  WRITE(numout,*) 'Lecture ',TRIM(nomvar)
!-
  IF (is_root_prc) THEN
     ALLOCATE(champout_g(iim_g*jjm_g,n3))
     IF ( n3 == 1 ) THEN
        CALL flinget (force_id,nomvar(1:LEN_TRIM(nomvar)), &
             &                iim_file, jjm_file, 0, 0,  1, 1, champ_file)
     ELSE
        DO l=1,n3
           CALL flinget &
                &      (force_id,nomvar(1:LEN_TRIM(nomvar)), &
                &       iim_file, jjm_file, 0, n3,  l, l, champ_file(:,:,l))
        ENDDO
     ENDIF
     ! shift if necessary
     IF (i_cut /= 0) THEN
        DO l=1,n3
           CALL shift_field (iim_file,jjm_file,i_cut,champ_file(:,:,l))
        ENDDO
     ENDIF
     ! interpolate onto the model grid
     DO l=1,n3
        DO j=1,jjm_g
           DO i=1,iim_g
              ij = i+(j-1)*iim_g
              champout_g(ij,l) = zero
              DO m=1,ncorr(i,j)
                 champout_g(ij,l) = champout_g(ij,l) &
        &                        +champ_file(icorr(i,j,m),jcorr(i,j,m),l)
              ENDDO
              champout_g(ij,l) = champout_g(ij,l)/REAL(ncorr(i,j))
           ENDDO
        ENDDO
     ENDDO
!$     DO l=1,n3
!$        DO j=1,jjm_g
!$           WRITE(numout,*) j,(/ ( champout_g((j-1)*iim_g+i,l), i=1,iim_g ) /)
!$        ENDDO
!$     ENDDO
  ELSE
     ALLOCATE(champout_g(1,1))
  ENDIF
  CALL scatter(champout_g,champout)
!#ifndef 1
!  champout(:,:)=champout_g(:,:)
!#else
!  CALL orch_scatter2D_mpi_rgen(champout_g,champout,SIZE(champout_g, 1),SIZE(champout_g, 2))
!#endif

!$  DO l=1,n3
!$     DO j=1,jjm
!$        WRITE(numout,*) j,(/ ( champout((j-1)*iim_g+i,l), i=1,iim_g ) /)
!$     ENDDO
!$  ENDDO
  !----------------------------
END SUBROUTINE weather_read
!-
!===
!-
!! ================================================================================================================================
!! SUBROUTINE   : weathgen_dump
!!
!>\BRIEF        This subroutine "dumps" data before writing an netcdf file.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_dump &
& (itau, dt_force, iim, jjm, nbindex, kindex, lrstwrite, &
&  swdown, rainf, snowf, tair, u, v, qair, pb, lwdown )
!---------------------------------------------------------------------
  IMPLICIT NONE
!-

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN)                     :: itau
  REAL,INTENT(IN)                        :: dt_force
  INTEGER,INTENT(IN)                     :: iim,jjm
  INTEGER,INTENT(IN)                     :: nbindex
  LOGICAL,INTENT(IN)                     :: lrstwrite
  INTEGER,DIMENSION(iim*jjm),INTENT(IN)  :: kindex
  REAL,DIMENSION(iim*jjm),INTENT(IN)     :: &
 &  swdown, rainf, snowf, tair, u, v, qair, pb, lwdown

  !! 0.4 Local variables

  INTEGER                 :: iret,ndim
  INTEGER,DIMENSION(4)    :: corner,edges
  REAL,DIMENSION(iim*jjm) :: var_gather

!_ ================================================================================================================================

!-
! time dimension
!-
  iret = NF90_PUT_VAR (dump_id,timestp_id,(/ REAL(itau) /), &
 &                     start=(/ itau /),count=(/ 1 /))
  iret = NF90_PUT_VAR (dump_id,time_id,(/ REAL(itau)*dt_force /), &
 &                     start=(/ itau /),count=(/ 1 /))
!-
! 2D variables: pas de dimension verticale
!-
  IF (gathered) THEN
    ndim = 2
    corner(1:2) = (/ 1, itau /)
    edges(1:2) = (/ nbindex, 1 /)
  ELSE
    ndim = 3
    corner(1:3) = (/ 1, 1, itau /)
    edges(1:3) = (/ iim, jjm, 1 /)
  ENDIF
!-
  CALL gather_weather (iim*jjm,nbindex,kindex,swdown, var_gather)
  iret = NF90_PUT_VAR (dump_id,soldownid, var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,rainf,  var_gather)
  iret = NF90_PUT_VAR (dump_id,rainfid,   var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,snowf,  var_gather)
  iret = NF90_PUT_VAR (dump_id,snowfid,   var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,pb,     var_gather)
  iret = NF90_PUT_VAR (dump_id,psolid,    var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,lwdown, var_gather)
  iret = NF90_PUT_VAR (dump_id,lwradid,   var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
!-
! 3D variables
!-
  IF (gathered) THEN
    ndim = 3
    corner(1:3) = (/ 1, 1, itau /)
    edges(1:3) = (/ nbindex, 1, 1 /)
  ELSE
    ndim = 4
    corner(1:4) = (/ 1, 1, 1, itau /)
    edges(1:4) = (/ iim, jjm, 1, 1 /)
  ENDIF
!-
  CALL gather_weather (iim*jjm,nbindex,kindex,u,    var_gather)
  iret = NF90_PUT_VAR (dump_id,uid,    var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,v,    var_gather)
  iret = NF90_PUT_VAR (dump_id,vid,    var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,tair, var_gather)
  iret = NF90_PUT_VAR (dump_id,tairid, var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
  CALL gather_weather (iim*jjm,nbindex,kindex,qair, var_gather)
  iret = NF90_PUT_VAR (dump_id,qairid, var_gather, &
 &         start=corner(1:ndim), count=edges(1:ndim))
!-
  IF (lrstwrite) THEN
    iret = NF90_CLOSE (dump_id)
  ENDIF
!---------------------------
END SUBROUTINE weathgen_dump
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : gather_weather
!!
!>\BRIEF        This subroutine determines which points are not in the computational domain and
!! creates a mask for these points. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::var_out
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE gather_weather (iimjjm, nbindex, kindex, var_in, var_out)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN)                     :: iimjjm,nbindex
  INTEGER,DIMENSION(iimjjm),INTENT(IN)   :: kindex
  REAL,DIMENSION(iimjjm),INTENT(IN)      :: var_in

  !! 0.2 Output variables  

  REAL,DIMENSION(iimjjm),INTENT(OUT)     :: var_out

  !! 0.4 Local variables

  INTEGER                                :: i
  LOGICAL,SAVE                           :: firstcall_gather_weather = .TRUE.
  INTEGER,SAVE                           :: nb_outside
  INTEGER,ALLOCATABLE,SAVE,DIMENSION(:)  :: outside

!_ ================================================================================================================================

  IF (firstcall_gather_weather) THEN
!---
!-- determine which points are not in the computational domain and
!-- create a mask for these points
!---
    firstcall_gather_weather = .FALSE.
  
    ALLOC_ERR=-1
    ALLOCATE(outside(iimjjm), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) THEN
      WRITE(numout,*) "ERROR IN ALLOCATION of outside : ",ALLOC_ERR
      STOP 
    ENDIF
    outside(:) = zero
    nb_outside = 0
    DO i=1,iimjjm
      IF ( ALL( kindex(:) /= i ) ) THEN
        nb_outside = nb_outside+1
        outside(nb_outside) = i
      ENDIF
    ENDDO
  ENDIF
!-
  IF ( gathered ) THEN
    DO i=1,nbindex
      var_out(i) = var_in(kindex(i))
    ENDDO
  ELSE
    var_out(:) = var_in(:)
    DO i=1,nb_outside
      var_out(outside(i)) = undef_sechiba
    ENDDO
  ENDIF
!--------------------
END SUBROUTINE gather_weather
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : shift_shield
!!
!>\BRIEF        
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::champ
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE shift_field (im,jm,i_cut,champ)
!---------------------------------------------------------------------

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER,INTENT(IN)                  :: im,jm,i_cut

  !! 0.3 Modified variables

  REAL,DIMENSION(im,jm),INTENT(INOUT) :: champ

  !! 0.4 Local variables

  REAL,DIMENSION(im,jm)               :: champ_temp

!_ ================================================================================================================================

  IF ( (i_cut >= 1).AND.(i_cut <= im) ) THEN
    champ_temp(1:im-i_cut-1,:) = champ(i_cut:im,:)
    champ_temp(im-i_cut:im,:)  = champ(1:i_cut+1,:)
    champ(:,:) = champ_temp(:,:)
  ENDIF
!-------------------------
END SUBROUTINE shift_field
!-
!===
!-
!! ================================================================================================================================
!! SUBROUTINE   : weathgen_domain_size
!!
!>\BRIEF        This subroutine determines the size of the domain.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::iim, ::jjm, ::limit_west, ::limit_east, ::limit_north, ::limit_south
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_domain_size &
& (limit_west,limit_east,limit_north,limit_south, &
&  zonal_res,merid_res,iim,jjm)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-
  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  REAL,INTENT(IN)     :: zonal_res,merid_res

  !! 0.2 Output variables 

  INTEGER,INTENT(OUT) :: iim,jjm

  !! 0.3 Modified variables

  REAL,INTENT(INOUT)  :: limit_west,limit_east,limit_north,limit_south

!_ ================================================================================================================================

  IF (limit_west > limit_east)  limit_east = limit_east+360.
!-
  IF (    (limit_west >= limit_east) &
 &    .OR.(limit_east > 360.) &
 &    .OR.(limit_west < -180.) &
 &    .OR.(limit_east-limit_west > 360.) ) THEN
    WRITE(numout,*) 'PROBLEME longitudes.'
    WRITE(numout,*) 'Limites Ouest, Est: ',limit_west,limit_east
    STOP
  ENDIF
!-
  IF (    (limit_south < -90.) &
 &    .OR.(limit_north > 90.) &
 &    .OR.(limit_south >= limit_north ) ) THEN
    WRITE(numout,*) 'PROBLEME latitudes.'
    WRITE(numout,*) 'Limites Nord, Sud: ',limit_north,limit_south
    STOP
  ENDIF
!-
  IF (    (zonal_res <= 0. ) &
 &    .OR.(zonal_res > limit_east-limit_west) ) THEN
    WRITE(numout,*) 'PROBLEME resolution zonale.'
    WRITE(numout,*) 'Limites Ouest, Est, Resolution: ', &
 &             limit_west,limit_east,zonal_res
    STOP
  ENDIF
!-
  IF (    (merid_res <= 0.) &
 &    .OR.(merid_res > limit_north-limit_south) ) THEN
    WRITE(numout,*) 'PROBLEME resolution meridionale.'
    WRITE(numout,*) 'Limites Nord, Sud, Resolution: ', &
 &             limit_north,limit_south,merid_res
    STOP
  ENDIF
!-
  iim = NINT(MAX((limit_east-limit_west)/zonal_res,1.))
  jjm = NINT(MAX((limit_north-limit_south)/merid_res,1.))
!-
  WRITE(numout,*) 'Domain size: iim, jjm = ', iim, jjm
!----------------------------------
END SUBROUTINE weathgen_domain_size
!-
!===
!-

!! ================================================================================================================================
!! FUNCTION     : tsat1 
!!
!>\BRIEF         This function computes the saturated temperature for vapor. 
!!
!! DESCRIPTION :  functions tsatl,tsati are used below so that Lowe's
!!                polyomial for liquid is used if t gt 273.16, or for ice if
!!                t lt 273.16. also impose range of validity for Lowe's polys.\n
!!
!! RECENT CHANGE(S): None
!!
!! RETURN VALUE : tsat
!!
!! REFERENCE(S) : 
!! - Lowe PR, 1977, An Approximating Polynomial for the Computation of Saturation
!! Vapor Pressure, Journal of Applied Meteorology.16,101-103.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

FUNCTION tsatl (t) RESULT (tsat)

  !! 0. Variables and parameters declaration
  
  !! 0.1 Input variables

  REAL,INTENT(IN) :: t

  !! 0.2 Result
  REAL            :: tsat

!_ ================================================================================================================================

  tsat = MIN(100.,MAX(t-zero_t,zero))
!-----------------
END FUNCTION tsatl
!-
!===
!-
!! ================================================================================================================================
!! FUNCTION     : tsati 
!!
!>\BRIEF         This function computes the saturated temperature for ice. 
!!
!! DESCRIPTION :  functions tsatl,tsati are used below so that Lowe's
!!                polyomial for liquid is used if t gt 273.16, or for ice if
!!                t lt 273.16. also impose range of validity for Lowe's polys.\n
!!
!! RECENT CHANGE(S): None
!!
!! RETURN VALUE : tsat
!!
!! REFERENCE(S) : 
!! - Lowe PR, 1977, An Approximating Polynomial for the Computation of Saturation
!! Vapor Pressure, Journal of Applied Meteorology.16,101-103.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

FUNCTION tsati (t) RESULT (tsat)

  !! 0. Variables and parameters declaration
  
  !! 0.1 Input variables

  REAL,INTENT(IN) :: t

  !! 0.2 Result
  REAL            :: tsat

!_ ================================================================================================================================

  tsat = MAX(-60.,MIN(t-zero_t,zero))

!-----------------
END FUNCTION tsati
!-
!===
!-

!! ================================================================================================================================
!! FUNCTION     : esat
!!
!>\BRIEF         This function computes specific humidity with the polynomials of Lowe.
!!
!! DESCRIPTION : The function esat is svp in n/m**2, with t in deg k.
!!               (100 * lowe's poly since 1 mb = 100 n/m**2.). \n
!!               Polynomials for svp(t), d(svp)/dt over water and ice 
!!
!! RECENT CHANGE(S): None
!!
!! RETURN VALUE : esatout
!!
!! REFERENCE(S) : 
!! - Lowe PR, 1977, An Approximating Polynomial for the Computation of Saturation
!! Vapor Pressure, Journal of Applied Meteorology.16,101-103.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

FUNCTION esat (t) RESULT (esatout)

  !! 0. Variables and parameters declaration

  REAL,PARAMETER :: &                                                    !! polynomials for svp(t), d(svp)/dt over water and ice are from
 & asat0 = 6.1078000,    asat1 = 4.4365185e-1, asat2 = 1.4289458e-2, &   !! lowe(1977),jam,16,101-103.
 & asat3 = 2.6506485e-4, asat4 = 3.0312404e-6, asat5 = 2.0340809e-8, &
 & asat6 = 6.1368209e-11, &
 & bsat0 = 6.1091780,    bsat1 = 5.0346990e-1, bsat2 = 1.8860134e-2, &
 & bsat3 = 4.1762237e-4, bsat4 = 5.8247203e-6, bsat5 = 4.8388032e-8, &
 & bsat6 = 1.8388269e-10

  !! 0.1 Input variables

  REAL,INTENT(IN) :: t

  !! 0.2 Result

  REAL             :: esatout

  !! 0.4 Local variables

  REAL             :: x

!_ ================================================================================================================================

  IF (t >= zero_t) THEN
    x = asat0
  ELSE
    x = bsat0
  ENDIF
!-
  esatout = 100.* &
    ( x &
     +tsatl(t)*(asat1+tsatl(t)*(asat2+tsatl(t)*(asat3 &
     +tsatl(t)*(asat4+tsatl(t)*(asat5+tsatl(t)* asat6))))) &
     +tsati(t)*(bsat1+tsati(t)*(bsat2+tsati(t)*(bsat3 &
     +tsati(t)*(bsat4+tsati(t)*(bsat5+tsati(t)* bsat6)))))  )
!----------------
END FUNCTION esat
!-
!===
!-
! ================================================================================================================================
!! FUNCTION     : qsat
!!
!>\BRIEF         This function computes the saturation specific humidity. 
!!
!! DESCRIPTION :  statement function qsat is saturation specific humidity,
!! with svp e and ambient pressure p in n/m**2. impose an upper
!! limit of 1 to avoid spurious values for very high svp
!! and/or small p
!!
!! RECENT CHANGE(S): None
!!
!! RETURN VALUE : qsatout
!!
!! REFERENCE(S) : 
!! - Lowe PR, 1977, An Approximating Polynomial for the Computation of Saturation
!! Vapor Pressure, Journal of Applied Meteorology.16,101-103.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

FUNCTION qsat (e,p) RESULT (qsatout)

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  REAL, INTENT(IN) :: e,p

  !! 0.2 Result
  REAL             :: qsatout

!_ ================================================================================================================================

  qsatout = 0.622*e/MAX(p-(1.0-0.622)*e,0.622*e)
!----------------
END FUNCTION qsat
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : weathgen_qsat
!!
!>\BRIEF        This subroutine calculates the saturation vapor pressure with the polynomials of Lowe. 
!!
!! DESCRIPTION  : Vectorized version of functions esat and qsat.
!!                statement function esat is svp in n/m**2, with t in deg K.\n
!!               (100 * lowe's poly since 1 mb = 100 n/m**2.)\n
!!               Polynomials for svp(t), d(svp)/dt over water 
!!               and ice are from Lowe(1977),jam,16,101-103 \n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::qsat
!!
!! REFERENCE(S) :
!! - Lowe PR, 1977, An Approximating Polynomial for the Computation of Saturation 
!! Vapor Pressure, Journal of Applied Meteorology.16,101-103.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_qsat (npoi,t,p,qsat)


  !! 0. Variables and parameters declaration

 !-
 ! polynomials for svp(t), d(svp)/dt over water and ice
 ! are from lowe(1977),ja
  REAL,PARAMETER :: &
 & asat0 = 6.1078000,    asat1 = 4.4365185e-1, asat2 = 1.4289458e-2, &   !! polynomials for svp(t), d(svp)/dt over water and ice
 & asat3 = 2.6506485e-4, asat4 = 3.0312404e-6, asat5 = 2.0340809e-8, &
 & asat6 = 6.1368209e-11, &
 & bsat0 = 6.1091780,    bsat1 = 5.0346990e-1, bsat2 = 1.8860134e-2, &
 & bsat3 = 4.1762237e-4, bsat4 = 5.8247203e-6, bsat5 = 4.8388032e-8, &
 & bsat6 = 1.8388269e-10

  !! 0.1 Input variables

  INTEGER,INTENT(IN)              :: npoi
  REAL,DIMENSION(npoi),INTENT(IN) :: t,p

  !! 0.2 Output variables 

  REAL,DIMENSION(npoi),INTENT(OUT):: qsat

  !! 0.4 Local variables

  REAL,DIMENSION(npoi) :: x, tl, ti, e

!_ ================================================================================================================================

  WHERE (t(:) > zero_t)
    x(:) = asat0
  ELSEWHERE
    x(:) = bsat0
  ENDWHERE
!-
  tl(:) = MIN(100.,MAX(t(:)-zero_t,zero))
  ti(:) = MAX(-60.,MIN(t(:)-zero_t,zero))
!-
  e(:) =  100.* &
    ( x(:) &
     +tl(:)*(asat1+tl(:)*(asat2+tl(:)*(asat3 &
     +tl(:)*(asat4+tl(:)*(asat5+tl(:)* asat6))))) &
     +ti(:)*(bsat1+ti(:)*(bsat2+ti(:)*(bsat3 &
     +ti(:)*(bsat4+ti(:)*(bsat5+ti(:)* bsat6)))))  )
!-
  qsat(:) = 0.622*e(:)/MAX(p(:)-(1.0-0.622)*e(:),0.622*e(:))
!---------------------------
END SUBROUTINE weathgen_qsat
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : weathgen_qsat_2d
!!
!>\BRIEF        This subroutine calculates the saturation vapor pressure with the polynomials of Lowe. 
!!
!! DESCRIPTION  : Vectorized version of functions esat and qsat.
!!                statement function esat is svp in n/m**2, with t in deg K.\n
!!               (100 * lowe's poly since 1 mb = 100 n/m**2.)\n
!!               Polynomials for svp(t), d(svp)/dt over water 
!!               and ice are from Lowe(1977),jam,16,101-103 \n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::qsat
!!
!! REFERENCE(S) :
!! - Lowe PR, 1977, An Approximating Polynomial for the Computation of Saturation 
!! Vapor Pressure, Journal of Applied Meteorology.16,101-103.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE weathgen_qsat_2d (iim,jjm,t,p,qsat)

  !! 0. Parameters and variable declaration

  !! 0.1 Input variables

  INTEGER, INTENT(IN) :: iim
  INTEGER, INTENT(IN) :: jjm
  REAL,DIMENSION(iim,jjm),INTENT(IN)  :: t,p

   !! 0.2 Output variables
   REAL,DIMENSION(iim,jjm),INTENT(OUT) :: qsat

   !! 0.4 Local variables

   REAL,DIMENSION(iim,jjm) :: x, tl, ti, e

  REAL,PARAMETER :: &
 & asat0 = 6.1078000,    asat1 = 4.4365185e-1, asat2 = 1.4289458e-2, &
 & asat3 = 2.6506485e-4, asat4 = 3.0312404e-6, asat5 = 2.0340809e-8, &
 & asat6 = 6.1368209e-11, &
 & bsat0 = 6.1091780,    bsat1 = 5.0346990e-1, bsat2 = 1.8860134e-2, &
 & bsat3 = 4.1762237e-4, bsat4 = 5.8247203e-6, bsat5 = 4.8388032e-8, &
 & bsat6 = 1.8388269e-10

!_ ================================================================================================================================

  WHERE (t(:,:) > zero_t)
    x(:,:) = asat0
  ELSEWHERE
    x(:,:) = bsat0
  ENDWHERE
!-
  tl(:,:) = MIN(100.,MAX(t(:,:)-zero_t,0.))
  ti(:,:) = MAX(-60.,MIN(t(:,:)-zero_t,0.))
!-
  e(:,:) =  100.* &
    ( x(:,:) &
     +tl(:,:)*(asat1+tl(:,:)*(asat2+tl(:,:)*(asat3 &
     +tl(:,:)*(asat4+tl(:,:)*(asat5+tl(:,:)* asat6))))) &
     +ti(:,:)*(bsat1+ti(:,:)*(bsat2+ti(:,:)*(bsat3 &
     +ti(:,:)*(bsat4+ti(:,:)*(bsat5+ti(:,:)* bsat6)))))  )
!-
  qsat(:,:) = 0.622*e(:,:)/MAX(p(:,:)-(1.0-0.622)*e(:,:),0.622*e(:,:))
!---------------------------
END SUBROUTINE weathgen_qsat_2d


!! ================================================================================================================================
!! SUBROUTINE   : mask_c_o 
!!
!>\BRIEF        This subroutine computes a field which indicated if the point is terrestrial or oceanic.
!!
!! DESCRIPTION  : From a mask field, we make an indicator mask Earth/ocean. Earth : 1 ; ocean : 0.
!!
!! RECENT CHANGE(S): z.x.li,01/04/1994 : creation of the subroutine 
!!
!! MAIN OUTPUT VARIABLE(S): ::mask, ::ncorr, ::icorr
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE mask_c_o &
& (imdep, jmdep, xdata, ydata, mask_in, fcrit, &
&  imar, jmar, zonal_res, merid_res, n_agg, x, y, mask, &
&  ncorr, icorr, jcorr)

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER :: imdep,jmdep
  REAL :: xdata(imdep),ydata(jmdep)
  REAL :: mask_in(imdep,jmdep)
  REAL :: fcrit
  INTEGER :: imar,jmar
  REAL :: zonal_res,merid_res
  INTEGER :: n_agg
  REAL :: x(imar),y(jmar)

  !! 0.2 Output variables 

  REAL, INTENT(OUT) :: mask(imar,jmar)
  INTEGER :: ncorr(imar,jmar)
  INTEGER,DIMENSION(imar,jmar,n_agg) :: icorr,jcorr

  !! 0.4 Local variables 

  INTEGER i, j, ii, jj
  REAL a(imar),b(imar),c(jmar),d(jmar)
  INTEGER num_tot(imar,jmar), num_oce(imar,jmar)
  REAL,ALLOCATABLE :: distans(:)
  INTEGER ij_proche(1),i_proche,j_proche
!-
  INTEGER,DIMENSION(imar,jmar) :: ncorr_oce , ncorr_land
  INTEGER,DIMENSION(imar,jmar,n_agg) :: &
 &  icorr_oce, jcorr_oce , icorr_land, jcorr_land
!-
  INTEGER imdepp
  REAL,ALLOCATABLE :: xdatap(:)
  REAL,ALLOCATABLE :: mask_inp(:,:)
  LOGICAL :: extend

!_ ================================================================================================================================

  ncorr(:,:) = 0
  icorr(:,:,:) = -1; jcorr(:,:,:) = -1
  ncorr_land(:,:) = 0
  icorr_land(:,:,:) = -1; jcorr_land(:,:,:) = -1
  ncorr_oce(:,:) = 0
  icorr_oce(:,:,:) = -1; jcorr_oce(:,:,:) = -1
! do we have to extend the domain (-x...-x+360)?
  IF ( xdata(1)+360.-xdata(imdep) > 0.01 ) THEN
    extend = .TRUE.
    imdepp = imdep+1
  ELSE
    extend = .FALSE.
    imdepp = imdep
  ENDIF
!-

  ALLOC_ERR=-1
  ALLOCATE(xdatap(imdepp), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of xdatap : ",ALLOC_ERR
    STOP 
  ENDIF

  ALLOC_ERR=-1
  ALLOCATE(mask_inp(imdepp,jmdep), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of mask_inp : ",ALLOC_ERR
    STOP 
  ENDIF
!-
  xdatap(1:imdep) = xdata(1:imdep)
  mask_inp(1:imdep,:) = mask_in(1:imdep,:)
!-
  IF (extend) THEN
    xdatap(imdepp) = xdatap(1)+360.
    mask_inp(imdepp,:) = mask_inp(1,:)
  ENDIF
!-

  ALLOC_ERR=-1
  ALLOCATE(distans(imdepp*jmdep), STAT=ALLOC_ERR)
  IF (ALLOC_ERR/=0) THEN
    WRITE(numout,*) "ERROR IN ALLOCATION of distans : ",ALLOC_ERR
    STOP 
  ENDIF
! Definition des limites des boites de la grille d'arrivee.
  IF (imar > 1) THEN
    a(1) = x(1)-(x(2)-x(1))/2.0
    b(1) = (x(1)+x(2))/2.0
    DO i=2,imar-1
      a(i) = b(i-1)
      b(i) = (x(i)+x(i+1))/2.0
    ENDDO
    a(imar) = b(imar-1)
    b(imar) = x(imar)+(x(imar)-x(imar-1))/2.0
  ELSE
    a(1) = x(1)-zonal_res/2.
    b(1) = x(1)+zonal_res/2.
  ENDIF
!-
  IF (jmar > 1) THEN
    c(1) = y(1)-(y(2)-y(1))/2.0
    d(1) = (y(1)+y(2))/2.0
    DO j=2,jmar-1
      c(j) = d(j-1)
      d(j) = (y(j)+y(j+1))/2.0
    ENDDO
    c(jmar) = d(jmar-1)
    d(jmar) = y(jmar)+(y(jmar)-y(jmar-1))/2.0
  ELSE
    c(1) = y(1)-merid_res/2.
    d(1) = y(1)+merid_res/2.
  ENDIF
!-
  num_oce(1:imar,1:jmar) = 0
  num_tot(1:imar,1:jmar) = 0
!-
!  .....  Modif  P. Le Van ( 23/08/95 )  ....
!-
  DO ii=1,imar
    DO jj=1,jmar
      DO i=1,imdepp
        IF (    (     (xdatap(i)-a(ii) >= 1.e-5) &
 &               .AND.(xdatap(i)-b(ii) <= 1.e-5) ) &
 &          .OR.(     (xdatap(i)-a(ii) <= 1.e-5) &
 &               .AND.(xdatap(i)-b(ii) >= 1.e-5) ) ) THEN
          DO j=1,jmdep
            IF (    (     (ydata(j)-c(jj) >= 1.e-5) &
 &                   .AND.(ydata(j)-d(jj) <= 1.e-5) ) &
 &              .OR.(     (ydata(j)-c(jj) <= 1.e-5) &
 &                   .AND.(ydata(j)-d(jj) >= 1.e-5) ) ) THEN
              num_tot(ii,jj) = num_tot(ii,jj)+1
              IF (mask_inp(i,j) < 0.5) THEN
                num_oce(ii,jj) = num_oce(ii,jj)+1
!-------------- on a trouve un point oceanique. On le memorise.
                ncorr_oce(ii,jj) = ncorr_oce(ii,jj)+1
                IF ((i == imdepp).AND.extend) THEN
                  icorr_oce(ii,jj,ncorr_oce(ii,jj)) = 1
                ELSE
                  icorr_oce(ii,jj,ncorr_oce(ii,jj)) = i
                ENDIF
                jcorr_oce(ii,jj,ncorr_oce(ii,jj)) = j
              ELSE
!-------------- on a trouve un point continental. On le memorise.
                ncorr_land(ii,jj) = ncorr_land(ii,jj)+1
                IF ((i == imdepp).AND.extend) THEN
                  icorr_land(ii,jj,ncorr_land(ii,jj)) = 1
                ELSE
                  icorr_land(ii,jj,ncorr_land(ii,jj)) = i
                ENDIF
                jcorr_land(ii,jj,ncorr_land(ii,jj)) = j
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDDO
  ENDDO
!-
  DO i=1,imar
    DO j=1,jmar
      IF (num_tot(i,j) > 0) THEN
        IF (    (     (num_oce(i,j) == 0) &
 &               .AND.(num_tot(i,j) > 0) ) &
 &          .OR.(     (num_oce(i,j) > 0) &
 &               .AND.(   REAL(num_oce(i,j)) &
 &                     <= REAL(num_tot(i,j))*(1.-fcrit) ) ) ) THEN
          mask(i,j) = un
          ncorr(i,j) = ncorr_land(i,j)
          icorr(i,j,:) = icorr_land(i,j,:)
          jcorr(i,j,:) = jcorr_land(i,j,:)
        ELSE
          mask(i,j) = zero
          ncorr(i,j) = ncorr_oce(i,j)
          icorr(i,j,:) = icorr_oce(i,j,:)
          jcorr(i,j,:) = jcorr_oce(i,j,:)
        ENDIF
      ELSE
        CALL dist_sphe(x(i),y(j),xdatap,ydata,imdepp,jmdep,distans)
        ij_proche(:) = MINLOC(distans)
        j_proche = (ij_proche(1)-1)/imdepp+1
        i_proche = ij_proche(1)-(j_proche-1)*imdepp
        mask(i,j) = mask_inp(i_proche,j_proche)
        IF ( (i_proche == imdepp).AND.extend)  i_proche=1
        ncorr(i,j) = 1
        icorr(i,j,1) = i_proche
        jcorr(i,j,1) = j_proche
      ENDIF
    ENDDO
  ENDDO
!----------------------
END SUBROUTINE mask_c_o
!-
!===
!-
!! ================================================================================================================================
!! SUBROUTINE   : dist_sphe
!!
!>\BRIEF        This subroutine computes the minimum distance between two points on Earth
!! according the great circle. \n
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): Laurent Li, 30/12/1996 : creation of this subroutine
!!
!! MAIN OUTPUT VARIABLE(S): ::distance
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE dist_sphe (rf_lon,rf_lat,rlon,rlat,im,jm,distance)

  !! 0. Variables and parameters declaration

  !! 0.1 Input variables

  INTEGER :: im, jm    !! dimensions
  REAL    :: rf_lon       !! longitude of the referenced point (degrees)
  REAL    :: rf_lat       !! latitude of the referenced point (degrees)
  REAL    :: rlon(im), rlat(jm) !! longitude and latitude of points

  !! 0.2 Output variables

  REAL :: distance(im,jm) !! distances in meters (m)

  !! 0.4 Local variables

  REAL :: rlon1, rlat1
  REAL :: rlon2, rlat2
  REAL :: dist
  REAL :: pa, pb, p
  INTEGER :: i,j

!_ ================================================================================================================================

  DO j=1,jm
    DO i=1,im
      rlon1=rf_lon
      rlat1=rf_lat
      rlon2=rlon(i)
      rlat2=rlat(j)
      pa = pi/2.0-rlat1*pir ! dist. entre pole n et point a
      pb = pi/2.0-rlat2*pir ! dist. entre pole n et point b
!-----
      p = (rlon1-rlon2)*pir ! angle entre a et b (leurs meridiens)
!-----
      dist = ACOS( COS(pa)*COS(pb)+SIN(pa)*SIN(pb)*COS(p))
      dist = R_Earth*dist      
      distance(i,j) = dist
    ENDDO
  ENDDO
!-----------------------
END SUBROUTINE dist_sphe
!-
!===
!-

!! ================================================================================================================================
!! SUBROUTINE   : permute
!!
!>\BRIEF         This subroutine initializes an array of size n by random integers between 1 and n.  
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::ordre
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE permute (n,ordre)
!---------------------------------------------------------------------

  !! 0. Variables and parameters declaration 

  !! 0.1 Input variables

  INTEGER,INTENT(IN) :: n   !! size of the array

  !! 0.2 Output variables 

  INTEGER,DIMENSION(n),INTENT(OUT) :: ordre

  !! 0.4 Local variables

  INTEGER,DIMENSION(n) :: restant
  INTEGER :: ipique, i, n_rest
  REAL    :: rndnum

!_ ================================================================================================================================

  n_rest = n
  restant(:) = (/ (i, i=1,n) /)
!-
  DO i=1,n
    CALL random_number (rndnum)
    ipique = INT(rndnum*n_rest)+1
    ordre(i) = restant(ipique)
    restant(ipique:n_rest-1) = restant(ipique+1:n_rest)
    n_rest = n_rest-1
  ENDDO
!---------------------
END SUBROUTINE permute
!-
!===
!-----------------
END MODULE weather
