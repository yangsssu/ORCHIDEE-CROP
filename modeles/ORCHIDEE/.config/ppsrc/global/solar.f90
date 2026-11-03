










! =================================================================================================================================
! MODULE       : solar
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2011)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module define solar 
!!
!!\n DESCRIPTION: 
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	: None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_global/solar.f90 $
!! $Date: 2015-11-16 14:26:03 +0100 (Mon, 16 Nov 2015) $
!! $Revision: 3026 $
!! \n
!_ ================================================================================================================================

MODULE solar

  USE defprec
  USE constantes
  USE ioipsl_para 

  IMPLICIT NONE



CONTAINS


!! ================================================================================================================================
!! SUBROUTINE   : solarang
!!
!>\BRIEF         This subroutine computes the solar angle according to the method used by GSWP and developed by J.C. Morill.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::csang
!!
!! REFERENCE(S) : See for further details :
!! http://www.atmo.arizona.edu/~morrill/swrad.html
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE solarang (julian, julian0, iim, jjm, lon, lat, csang)

    USE calendar

    IMPLICIT NONE

    !! 0. Parameters and variables declaration
    
    !! 0.1 Input variables

    INTEGER, INTENT(in)                      :: iim, jjm        !!
    REAL, INTENT(in)                         :: julian          !!
    REAL, INTENT(in)                         :: julian0         !!
    REAL, DIMENSION(iim,jjm), INTENT(in)     :: lon, lat        !!

    !! 0.2 Output variables

    REAL, DIMENSION(iim,jjm), INTENT(out)    :: csang           !!

    !! 0.4 Local variables

    REAL                                     :: gamma           !!
    REAL                                     :: dec             !!
    REAL                                     :: decd            !!
    REAL                                     :: et              !!
    REAL                                     :: gmt             !!
    REAL                                     :: le              !!
    REAL                                     :: ls              !!
    REAL                                     :: lcorr           !!
    REAL                                     :: latime          !!
    REAL                                     :: omega           !!
    REAL                                     :: omegad          !!
    REAL                                     :: llatd, llat     !!
    INTEGER                                  :: igmt            !!
    INTEGER                                  :: ilon, ilat      !!
    INTEGER, SAVE, ALLOCATABLE, DIMENSION(:) :: zone            !!
!$OMP THREADPRIVATE(zone)
    REAL, SAVE, ALLOCATABLE, DIMENSION(:)    :: lhour           !!
!$OMP THREADPRIVATE(lhour)
    !
    LOGICAL                                  :: check = .FALSE. !!
!$OMP THREADPRIVATE(check)

!_ ================================================================================================================================

    IF (check) WRITE(numout,*) 'We get the right calendar information'
    !-
    !- 1) Day angle gamma
    !-
    !   gamma = 2.*pi*MOD(julian,one_year)/one_year
    gamma = 2.*pi*(julian-julian0)/one_year
    !-
    !- 2) Solar declination (assumed constant for a 24 hour period)  page 7
    !-    in radians
    !-
    IF (check) WRITE(numout,*) 'Solar declination'
    !
    dec = ( 0.006918-0.399912*COS(gamma)+0.070257*SIN(gamma) &
         &       -0.006758*COS(2*gamma)+0.000907*SIN(2*gamma)      &
         &       -0.002697*COS(3*gamma)+0.00148*SIN(3*gamma))
    decd = dec*(180/pi)
    !-
    !- maximum error 0.0006 rad (<3'), leads to error
    !- of less than 1/2 degree in zenith angle
    !-
    IF (check) WRITE(numout,*) 'Equation of time'
    !- 3)  Equation of time  page 11
    !-
    et = ( 0.000075+0.001868*COS(gamma)-0.032077*SIN(gamma)&
         &      -0.014615*COS(2*gamma)-0.04089*SIN(2*gamma))*229.18
    !-
    !- Get the time zones for the current time
    !-
    gmt = 24.*(julian-INT(julian))
    IF (.NOT.ALLOCATED(zone))  ALLOCATE(zone(iim))
    IF (.NOT.ALLOCATED(lhour)) ALLOCATE(lhour(iim))
    !-
    !igmt = NINT(gmt)
    IF (check) WRITE(numout,*) 'Get time zone'
    CALL time_zone(gmt, iim, jjm, lon, zone, lhour)
    !-
    !- Start a loop through the grid
    !-
    IF (check) WRITE(numout,*) 'Start a loop through the grid'
    DO ilon=1,iim
       !---
       !--- 4) Local apparent time  page 13
       !---
       !--- ls     standard longitude (nearest 15 degree meridian)
       !--- le     local longtitude
       !--- lhour  local standard time
       !--- latime local apparent time
       !--- lcorr  longitudunal correction (minutes)
       !---
       le = lon(ilon,1)
       ls = ((zone(ilon)-1)*15)-180.
       lcorr = 4.*(ls-le)*(-1)
       latime = lhour(ilon)+lcorr/60.+et/60.
       IF (latime <  0.) latime = latime+24
       IF (latime > 24.) latime = latime-24
       !---
       !--- 5) Hour angle omega  page 15
       !---
       !--- hour angle is zero at noon, positive in the morning
       !--- It ranges from 180 to -180
       !---
       !--- omegad is omega in degrees, omega is in radians
       !---
       omegad = (latime-12.)*(-15.)
       omega  = omegad*pi/180.
       !---
       DO ilat=1,jjm
          !-----
          !----- 6)  Zenith angle  page 15
          !-----
          !----- csang cosine of zenith angle (radians)
          !----- llatd =  local latitude in degrees
          !----- llat  =  local latitude in radians
          !-----
          llatd = lat(1,ilat)
          llat  = llatd*pi/180.
          csang(ilon,ilat) = &
               &  MAX(zero,SIN(dec)*SIN(llat)+COS(dec)*COS(llat)*COS(omega))
       ENDDO
    ENDDO
    !----------------------
  END SUBROUTINE solarang


!! ================================================================================================================================
!! SUBROUTINE   : time_zone
!!
!>\BRIEF         
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::zone, ::lhour
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE time_zone (gmt, iim, jjm, lon, zone, lhour)

    IMPLICIT NONE

    !! 0. Parameters and variables declaration
    
    !! 0.1 Input variables

    INTEGER, INTENT(in)                   :: iim, jjm        !!
    REAL, DIMENSION(iim,jjm), INTENT(in)  :: lon             !!
    REAL, INTENT(in)                      :: gmt             !!

    !! 0.2 Output variables

    INTEGER, DIMENSION(iim), INTENT(out)  :: zone            !!
    REAL, DIMENSION(iim), INTENT(out)     :: lhour           !!

    !! 0.4 Local variables

    INTEGER                               :: deg             !!
    !!??   REAL :: deg
    INTEGER                               :: i, ilon, change !! Indices

!_ ================================================================================================================================

    DO ilon=1,iim
       !---
       !--- Convert longitude index to longtitude in degrees
       !---
       deg = lon(ilon,1)
       !---
       !--- Determine into which time zone (15 degree interval) the
       !--- longitude falls.
       !---
       DO i=1,25
          IF (deg < (-187.5+(15*i))) THEN
             zone(ilon) = i
             IF (zone(ilon) == 25)   zone(ilon) = 1
             EXIT
          ENDIF
       ENDDO
       !---
       !--- Calculate change (in number of hours) from GMT time to
       !--- local hour.  Change will be negative for zones < 13 and
       !--- positive for zones > 13.
       !---
       !--- There is also a correction for lhour < 0 and lhour > 23
       !--- to lhour between 0 and 23.
       !---
       IF (zone(ilon) < 13) THEN
          change = zone(ilon)-13
          lhour(ilon) = gmt+change
       ENDIF
       !---
       IF (zone(ilon) == 13) THEN
          lhour(ilon) = gmt
       ENDIF
       !---
       IF (zone(ilon) > 13) THEN
          change = zone(ilon)-13
          lhour(ilon) = gmt+change
       ENDIF
       IF (lhour(ilon) <  0) lhour(ilon) = lhour(ilon)+24
       IF (lhour(ilon) >= 24) lhour(ilon) = lhour(ilon)-24
       !---
    ENDDO
    !-----------------------
  END SUBROUTINE time_zone



!! ================================================================================================================================
!! SUBROUTINE   : downward_solar_flux
!!
!>\BRIEF         
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ::solad, ::solai
!!
!! REFERENCE(S) :See for further details :
!! http://www.atmo.arizona.edu/~morrill/swrad.html
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE downward_solar_flux (npoi,latitude,calendar_str,jday,rtime,cloud,nband,solad,solai)

    IMPLICIT NONE

    !! 0. Parameter and variables declaration
    
    !! 0.1 Input variables
    
    INTEGER, INTENT(in)                      :: npoi               !! number of grid points (unitless)
    REAL, DIMENSION(npoi), INTENT(in)        :: latitude           !! latitude in degrees
    CHARACTER(LEN=20),INTENT(in)             :: calendar_str       !! Calendar type
    REAL, INTENT(in)                         :: jday
    REAL,INTENT(in)                          :: rtime
    REAL, DIMENSION(npoi), INTENT(in)        :: cloud              !! cloud fraction (0-1, unitless)
    INTEGER,INTENT(in)                       :: nband              !! number of visible bands (unitless)

    !! 0.2 Output variables

    REAL, DIMENSION(npoi,nband), INTENT(out) :: solad              !! direct downward solar flux 
                                                                   !! @tex $(W.m^{-2})$ @endtex
    REAL, DIMENSION(npoi,nband), INTENT(out) :: solai              !! diffuse downward solar flux 
                                                                   !! @tex $(W.m^{-2})$ @endtex

    !! 0.4 Local variables

    ! Parametres orbitaux:

    REAL, SAVE                               :: ecc                !! Eccentricity
!$OMP THREADPRIVATE(ecc)
    REAL, SAVE                               :: perh               !! Longitude of perihelie
!$OMP THREADPRIVATE(perh)
    REAL, SAVE                               :: xob                !! obliquity
!$OMP THREADPRIVATE(xob)
    REAL, PARAMETER                          :: pir = pi/180.      !!
    REAL, SAVE                               :: step               !!
!$OMP THREADPRIVATE(step)
    REAL                                     :: xl                 !!
    REAL                                     :: so                 !!
    REAL                                     :: xllp               !!
    REAL                                     :: xee                !!
    REAL                                     :: xse                !!  
    REAL                                     :: xlam               !!
    REAL                                     :: dlamm              !!
    REAL                                     :: anm                !!
    REAL                                     :: ranm               !!
    REAL                                     :: ranv               !!
    REAL                                     :: anv                !!
    REAL                                     :: tls                !!
    REAL                                     :: rlam               !!
    REAL                                     :: sd                 !!
    REAL                                     :: cd                 !!
    REAL                                     :: deltar             !!
    REAL                                     :: delta              !!
    REAL                                     :: Dis_ST             !!  
    REAL                                     :: ddt                !!
    REAL                                     :: orbit              !!
    REAL                                     :: angle              !!
    REAL                                     :: xdecl              !!
    REAL                                     :: xlat               !!
    REAL, DIMENSION(npoi)                    :: trans              !!
    REAL, DIMENSION(npoi)                    :: fdiffuse           !!
    REAL, DIMENSION(npoi)                    :: coszen             !! cosine of solar zenith angle
    REAL                                     :: sw                 !!
    REAL                                     :: frac               !!
    INTEGER                                  :: i,ib               !!
    LOGICAL, SAVE                            :: firstcall_solar = .TRUE. !!
!$OMP THREADPRIVATE(firstcall_solar)

!_ ================================================================================================================================
    
    IF (firstcall_solar) THEN
       IF ( TRIM(calendar_str) .EQ. 'gregorian' ) THEN  
          step = 1.0
       ELSE
          step = one_year/365.2425
       ENDIF
       !-
       ! Read Orbital Parameters
       !-

       !Config Key   = ECCENTRICITY
       !Config Desc  = Use prescribed values
       !Config If    = ALLOW_WEATHERGEN
       !Config Def   = 0.016724
       !Config Help  =
       !Config Units = [-]
       ecc = 0.016724
       CALL getin_p ('ECCENTRICITY',ecc)
       WRITE(numout,*) 'ECCENTRICITY: ',ecc
       !
       !Config Key  = PERIHELIE
       !Config Desc = Use prescribed values
       !Config If   = ALLOW_WEATHERGEN
       !Config Def  = 102.04
       !Config Help  =
       !Config Units = [-]
       perh = 102.04
       CALL getin_p ('PERIHELIE',perh)
       WRITE(numout,*) 'PERIHELIE: ',perh
       !
       !Config Key  = OBLIQUITY
       !Config Desc = Use prescribed values
       !Config If   = ALLOW_WEATHERGEN
       !Config Def  = 23.446
       !Config Help  =
       !Config Units = [Degrees]
       xob = 23.446
       CALL getin_p ('OBLIQUITY',xob)
       WRITE(numout,*) 'OBLIQUITY: ',xob

       firstcall_solar = .FALSE.
    ENDIF

    !-
    ! calendar and orbital calculations
    !-

    !-
    ! calculate the earth's orbital angle (around the sun) in radians
    orbit = 2.0*pi*jday/365.2425
    !-
    ! calculate the solar hour angle in radians
    angle = 2.0*pi*(rtime-12.0)/24.0
    !-
    ! calculate the current solar declination angle
    ! ref: global physical climatology, hartmann, appendix a
    !
    ! xdecl = 0.006918 &
    !        -0.399912*cos(orbit) &
    !        +0.070257*sin(orbit) &
    !        -0.006758*cos(2.0*orbit) &
    !        +0.000907*sin(2.0*orbit) &
    !        -0.002697*cos(3.0*orbit) &
    !        +0.001480*sin(3.0*orbit)
    !
    ! calculate the effective solar constant,
    ! including effects of eccentricity
    ! ref: global physical climatology, hartmann, appendix a
    !
    ! sw = 1370.*( 1.000110 &
    !             +0.034221*cos(orbit) &
    !             +0.001280*sin(orbit) &
    !             +0.000719*cos(2.0*orbit) &
    !             +0.000077*sin(2.0*orbit))
    !
    ! correction Marie-France Loutre
    !
    !    orbital parameters
    !
    !    ecc = 0.016724
    !    perh = 102.04
    !    xob = 23.446
    !-
    xl = perh+180.0
    ! so : sinus of obliquity
    so = sin(xob*pir)
    !-
    xllp = xl*pir
    xee  = ecc*ecc
    xse  = sqrt(1.0d0-xee)
    ! xlam : true long. sun for mean long. = 0
    xlam = (ecc/2.0+ecc*xee/8.0d0)*(1.0+xse)*sin(xllp)-xee/4.0 &
         &      *(0.5+xse)*sin(2.0*xllp)+ecc*xee/8.0*(1.0/3.0+xse) &
         &      *sin(3.0*xllp)
    xlam  =2.0d0*xlam/pir
    ! dlamm : mean long. sun for ma-ja
    dlamm =xlam+(INT(jday)-79)*step
    anm   = dlamm-xl
    ranm  = anm*pir
    xee   = xee*ecc
    ! ranv : anomalie vraie    (radian)
    ranv = ranm+(2.0*ecc-xee/4.0)*sin(ranm)+5.0/4.0*ecc*ecc &
         &      *sin(2.0*ranm)+13.0/12.0*xee*sin(3.0*ranm)
    ! anv  : anomalie vraie    (degrees)
    anv  = ranv/pir
    ! tls  : longitude vraie   (degrees)
    tls  = anv+xl
    ! rlam : longitude vraie   (radian)
    rlam = tls*pir
    ! sd and cd: cosinus and sinus of solar declination angle (delta)
    ! sinus delta = sin (obl)*sin(lambda) with lambda = real longitude
    ! (Phd. thesis of Marie-France Loutre, ASTR-UCL, Belgium, 1993)
    sd    = so*sin(rlam)
    cd    = sqrt(1.0d0-sd*sd)
    ! delta : Solar Declination (degrees, angle sun at equator)
    deltar = atan(sd/cd)
    delta  = deltar/pir
    !-
    ! Eccentricity Effect
    !-
    Dis_ST  =(1.0-ecc*ecc)/(1.0+ecc*cos(ranv))
    ! ddt :    1 / normalized earth's sun distance
    ddt = 1.0/Dis_ST
    !-
    ! Insolation normal to the atmosphere (W/m2)
    !-
    sw  = ddt *ddt *1370.d0
    !-
    xdecl = deltar
    !-
    ! solar calculations
    !-
    do i=1,npoi
       !---
       !-- calculate the latitude in radians
       !---
       xlat = latitude(i)*pi/180.0
       !---
       !-- calculate the cosine of the solar zenith angle
       !---
       coszen(i) = MAX(0.0, (sin(xlat)*sin(xdecl) &
            &                      + cos(xlat)*cos(xdecl)*cos(angle)))
       !---
       !-- calculate the solar transmission through the atmosphere
       !-- using simple linear function of tranmission and cloud cover
       !---
       !-- note that the 'cloud cover' data is typically obtained from
       !-- sunshine hours -- not direct cloud observations
       !---
       !-- where, cloud cover = 1 - sunshine fraction
       !---
       !-- different authors present different values for the slope and
       !-- intercept terms of this equation
       !---
       !-- Friend, A: Parameterization of a global daily weather generator
       !-- for terrestrial ecosystem and biogeochemical modelling,
       !-- Ecological Modelling
       !---
       !-- Spitters et al., 1986: Separating the diffuse and direct component
       !-- of global radiation and its implications for modeling canopy
       !-- photosynthesis, Part I: Components of incoming radiation,
       !-- Agricultural and Forest Meteorology, 38, 217-229.
       !---
       !-- A. Friend       : trans = 0.251+0.509*(1.0-cloud(i))
       !-- Spitters et al. : trans = 0.200+0.560*(1.0-cloud(i))
       !---
       !-- we are using the values from A. Friend
       !---
       trans(i) = 0.251+0.509*(1.0-cloud(i))
       !---
       !-- calculate the fraction of indirect (diffuse) solar radiation
       !-- based upon the cloud cover
       !---
       !-- note that these relationships typically are measured for either
       !-- monthly or daily timescales, and may not be exactly appropriate
       !-- for hourly calculations -- however, in ibis, cloud cover is fixed
       !-- through the entire day so it may not make much difference
       !---
       !-- method i --
       !---
       !-- we use a simple empirical relationships
       !-- from Nikolov and Zeller (1992)
       !---
       !-- Nikolov, N. and K.F. Zeller, 1992:  A solar radiation algorithm
       !-- for ecosystem dynamics models, Ecological Modelling, 61, 149-168.
       !---
       fdiffuse(i) = 1.0045+trans(i)*( 0.0435+trans(i) &
            &                                 *(-3.5227+trans(i)*2.6313))
       !---
       IF (trans(i) > 0.75) fdiffuse(i) = 0.166
       !---
       !-- method ii --
       !---
       !-- another method was suggested by Spitters et al. (1986) based on
       !-- long-term data from the Netherlands
       !--
       !-- Spitters et al., 1986: Separating the diffuse and direct component
       !-- of global radiation and its implications for modeling canopy
       !-- photosynthesis, Part I: Components of incoming radiation,
       !-- Agricultural and Forest Meteorology, 38, 217-229.
       !---
       !--       if ((trans == 0.00).and.(trans < 0.07)) then
       !--         fdiffuse = 1.0
       !--       else if ((trans >= 0.07).and.(trans < 0.35)) then
       !--         fdiffuse = 1.0-2.3*(trans-0.07)**2
       !--       else if ((trans >= 0.35).and.(trans < 0.75)) then
       !--         fdiffuse = 1.33-1.46*trans
       !--       ELSE
       !--         fdiffuse = 0.23
       !--       endif
       !---
    ENDDO
    !-----------------------
    !-
    ! do for each waveband
    !-
    DO ib=1,nband
       !---
       !-- calculate the fraction in each waveband
       !---
       !-- GK010200
       IF (nband == 2) then
          frac = 0.46+0.08*REAL(ib-1)
       ELSE
          frac = 1./REAL(nband)
       ENDIF
       !---
       DO i=1,npoi
          !-----
          !---- calculate the direct and indirect solar radiation
          !-----
          solad(i,ib) = sw*coszen(i)*frac*trans(i)*(1.-fdiffuse(i))
          solai(i,ib) = sw*coszen(i)*frac*trans(i)*fdiffuse(i)
       ENDDO
    ENDDO


  END SUBROUTINE downward_solar_flux

 
END MODULE solar
