
!! This module define variables for the grid to gathered points.
!!
!! @call sechiba_main
!! @Version : $Revision: 3357 $, $Date: 2016-04-11 15:03:33 +0200 (Mon, 11 Apr 2016) $
!! 
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_global/grid.f90 $
!< $Date: 2016-04-11 15:03:33 +0200 (Mon, 11 Apr 2016) $
!< $Author: albert.jornet $
!< $Revision: 3357 $
!!
!! @author Marie-Alice Foujols, Jan Polcher and Martial Mancip
!! 
!!
!f90doc MODULEgrid
MODULE grid

  USE defprec
  USE constantes
  USE mod_orchidee_para

  IMPLICIT NONE

  !
  ! PARAMETERS
  ! default resolution (m)
  REAL(r_std), PARAMETER :: default_resolution = 250000.
  !
  ! VARIABLES
  !
  ! Global map or not.
  ! There is little change that if iim <=2 and jjm <= 2 that we have global grid.
  ! Furthermore using the second line allows to avoid pole problems for global grids
  LOGICAL, SAVE                                      :: global = .TRUE.
!$OMP THREADPRIVATE(global)
  !
  !-
  !- Variable to help describe the grid
  !- once the points are gathered.
  !-
  !! Limits of the domain
  REAL(r_std), SAVE                                   :: limit_west, limit_east, &
       &                                                limit_north, limit_south
!$OMP THREADPRIVATE(limit_west, limit_east, limit_north, limit_south)
  !-
  !! Geographical coordinates
  REAL(r_std), ALLOCATABLE, DIMENSION (:,:), SAVE     :: lalo
!$OMP THREADPRIVATE(lalo)
  !! index  of land points
  INTEGER, ALLOCATABLE, DIMENSION (:), SAVE          :: ilandindex,jlandindex
!$OMP THREADPRIVATE(ilandindex, jlandindex)
  !- 
  !! Fraction of continents.
  REAL(r_std), ALLOCATABLE, DIMENSION (:), SAVE       :: contfrac
!$OMP THREADPRIVATE(contfrac)
  !
  ! indices of the 4 neighbours of each grid point (1=N, 2=E, 3=S, 4=W)
  !   a zero or negative index means that this neighbour is not a land point
  INTEGER(i_std), ALLOCATABLE, DIMENSION (:,:), SAVE :: neighbours
!$OMP THREADPRIVATE(neighbours)
  !
  ! resolution at each grid point in m (1=E-W, 2=N-S)
  ! (size in x an y of the grid)
  REAL(r_std), ALLOCATABLE, DIMENSION (:,:), SAVE     :: resolution
!$OMP THREADPRIVATE(resolution)
  REAL(r_std), DIMENSION(2), SAVE :: min_resol,max_resol
!$OMP THREADPRIVATE(min_resol, max_resol)
  REAL(r_std), ALLOCATABLE, DIMENSION (:), SAVE     :: area
!$OMP THREADPRIVATE(area)
  !
  !
  ! Get the direction of the grid
  !
  CHARACTER(LEN=2), DIMENSION(2), SAVE, PRIVATE      :: grid_dir
!$OMP THREADPRIVATE(grid_dir)
  !
  ! Rose gives the geographical direction for the various index increments
  ! The following corespondences exist
  !                       WE&NS  WE&SN and so on !
  ! rose(1) = i+0 & j-1    NN     SS
  ! rose(2) = i+1 & j-1    NE     SE
  ! rose(3) = i+1 & j+0    EE     EE
  ! rose(4) = i+1 & j+1    SE     NE
  ! rose(5) = i+0 & j+1    SS     NN
  ! rose(6) = i-1 & j+1    SW     NW
  ! rose(7) = i-1 & j+0    WW     WW
  ! rose(8) = i-1 & j-1    NW     SW
  INTEGER(i_std), DIMENSION(8), SAVE, PRIVATE        :: rose
!$OMP THREADPRIVATE(rose)
  !
  ! The calendar
  CHARACTER(LEN=20), SAVE         :: calendar_str
!$OMP THREADPRIVATE(calendar_str)
  !
  ! The date
  REAL(r_std), SAVE                     :: in_julian
!$OMP THREADPRIVATE(in_julian)
  ! Diff with day 0
  REAL(r_std), SAVE                     :: julian_diff
!$OMP THREADPRIVATE(julian_diff)
  !
  INTEGER(i_std), SAVE                  :: year, month, day
!$OMP THREADPRIVATE(year, month, day)
  REAL(r_std), SAVE                     :: sec
!$OMP THREADPRIVATE(sec)
  !
  ! month_len (d)
  INTEGER(i_std), SAVE                  :: month_len
!$OMP THREADPRIVATE(month_len)
  !
  ! year length (d)
  INTEGER(i_std), SAVE            :: year_length=0
!$OMP THREADPRIVATE(year_length)
  !
  ! Ration between calendar year in days (ie 360d or 365d ...) to gregorian year length 
  REAL(r_std), SAVE :: year_spread
!$OMP THREADPRIVATE(year_spread)
  !
CONTAINS
  !
  !f90doc CONTAINS
  ! 
  !
  SUBROUTINE init_grid ( npts )
    !
    ! 0 interface
    !
    IMPLICIT NONE
    !
    ! 0.1 input  !
    
    ! Domain size
    INTEGER(i_std), INTENT(in)                                 :: npts !! Number of local continental points 
    !
    !  Create the internal coordinate table
    !
    IF ( (.NOT.ALLOCATED(lalo))) THEN
       ALLOCATE(lalo(npts,2))
       lalo(:,:) = val_exp
    ENDIF
    !-
    !- Store variable to help describe the grid
    !- once the points are gathered.
    !-
    IF ( (.NOT.ALLOCATED(neighbours))) THEN
       ALLOCATE(neighbours(npts,8))
       neighbours(:,:) = -999999
    ENDIF
    IF ( (.NOT.ALLOCATED(resolution))) THEN
       ALLOCATE(resolution(npts,2))
       resolution(:,:) = val_exp
    ENDIF
    IF ( (.NOT.ALLOCATED(area))) THEN
       ALLOCATE(area(npts))
       area(:) = val_exp
    ENDIF
    !
    !- Store the fraction of the continents only once so that the user
    !- does not change them afterwards.
    !
    IF ( (.NOT.ALLOCATED(contfrac))) THEN
       ALLOCATE(contfrac(npts))
       contfrac(:) = val_exp
    ENDIF
    !
    ! Allocation of index coordinates
    IF (.NOT. ALLOCATED(ilandindex)) THEN
       ALLOCATE(ilandindex(npts),jlandindex(npts))
       ilandindex(:) = -10000000
       jlandindex(:) = -10000000
    ENDIF
    !
  END SUBROUTINE init_grid

  SUBROUTINE grid_stuff (npts_glo, iim, jjm, grid_lon, grid_lat, kindex)
    !
    ! 0 interface
    !
    IMPLICIT NONE
    !
    ! 0.1 input  !
    
    ! Domain size
    INTEGER(i_std), INTENT(in)                                 :: npts_glo
    ! Size of cartesian grid
    INTEGER(i_std), INTENT(in)                                 :: iim, jjm
    ! Longitudes on cartesian grid
    REAL(r_std), DIMENSION(iim,jjm), INTENT(in)                 :: grid_lon
    ! Latitudes on cartesian grid
    REAL(r_std), DIMENSION(iim,jjm), INTENT(in)                 :: grid_lat
    ! Index of land point on 2D map (in local position)
    INTEGER(i_std), DIMENSION(:), INTENT(in)                :: kindex    
    !
    ! 0.3 local
    !
    ! Index of land point on 2D map (in global position)
    INTEGER, ALLOCATABLE, DIMENSION (:)                        :: index_p
    !
    ! which STOMATE point corresponds to the given point on the cartesian grid
    INTEGER(i_std), DIMENSION(iim,jjm)                   :: correspondance
    ! cosine of the latitude
    REAL(r_std)                                           :: coslat
    ! number of points where default resolution is used
    INTEGER(i_std)                                       :: ndefault_lon, ndefault_lat
    ! Indices
    INTEGER(i_std)                                       :: i,ip,jp, imm1, imp1, imm1l, imp1l, ii
    !
    ! =========================================================================
    
    IF ( printlev >= 4 ) WRITE(numout,*) 'Entering grid_stuff'

    ! default resolution
    IF ( printlev >=2 ) WRITE(numout,*) 'grid stuff: default resolution (m): ',default_resolution
    !
    !-
    IF (is_root_prc) THEN
       ! Check if we have a global map or not.
       ! There is little change that if iim <=2 and jjm <= 2 that we have global grid.
       ! Furthermore using the second line allows to avoid pole problems for global grids
       IF (iim <= 2 .OR. jjm <= 2) THEN
          global = .FALSE.
       ELSE
          ! We assume here that the longitude is in increasing order and in degrees.
          IF ( grid_lon(iim,2)-grid_lon(1,2) >= 360. - (grid_lon(2,2)-grid_lon(1,2)) ) THEN
             global = .TRUE.
          ELSE
             global = .FALSE.
          ENDIF
       ENDIF
       !
       ! Get the direction of the grid
       !
       IF ( iim > 1 ) THEN
          IF ( grid_lon(1,1) <= grid_lon(2,1) ) THEN
             grid_dir(1) = 'WE'
          ELSE
             grid_dir(1) = 'EW'
          ENDIF
       ELSE
          grid_dir(1) = 'WE'
       ENDIF
       !
       IF ( jjm > 1 ) THEN
          IF ( grid_lat(1,1) >= grid_lat(1,2) ) THEN
             grid_dir(2) = 'NS'
          ELSE
             grid_dir(2) = 'SN'
          ENDIF
       ELSE
          grid_dir(2) = 'NS'
       ENDIF
       !
       !! WRITE(numout,*) 'Longitude direction :', grid_dir(1)
       !! WRITE(numout,*) 'Latitude  direction :', grid_dir(2)
       !
       ndefault_lon = 0
       ndefault_lat = 0
       ! initialize output
       neighbours_g(:,:) = -1
       resolution_g(:,:) = zero
       min_resol(:) = 1.e6
       max_resol(:) = moins_un
       
       correspondance(:,:) = -1
       DO i = 1, npts_glo          
          !
          ! 1 find numbers of the latitude and longitude of each point
          !
          
          ! index of latitude
          jp = INT( (index_g(i)-1) /iim ) + 1
          
          ! index of longitude
          ip = index_g(i) - ( jp-1 ) * iim
          !
          !correspondance(ip,jp) = kindex(i)
          !
          correspondance(ip,jp) = i

       ENDDO
  
       !
       ! Get the "wind rose" for the various orientation of the grid
       !
       IF ( grid_dir(1) .EQ. 'WE' .AND.  grid_dir(2) .EQ. 'NS' ) THEN
          rose(1) = 1
          rose(2) = 2
          rose(3) = 3
          rose(4) = 4
          rose(5) = 5
          rose(6) = 6
          rose(7) = 7
          rose(8) = 8
       ELSE IF ( grid_dir(1) .EQ. 'EW' .AND.  grid_dir(2) .EQ. 'NS' ) THEN
          rose(1) = 1
          rose(2) = 8
          rose(3) = 7
          rose(4) = 6
          rose(5) = 5
          rose(6) = 4
          rose(7) = 3
          rose(8) = 2
       ELSE IF ( grid_dir(1) .EQ. 'WE' .AND.  grid_dir(2) .EQ. 'SN' ) THEN
          rose(1) = 5
          rose(2) = 4
          rose(3) = 3
          rose(4) = 2
          rose(5) = 1
          rose(6) = 8
          rose(7) = 7
          rose(8) = 6
       ELSE IF ( grid_dir(1) .EQ. 'EW' .AND.  grid_dir(2) .EQ. 'SN' ) THEN
          rose(1) = 5
          rose(2) = 6
          rose(3) = 7
          rose(4) = 8
          rose(5) = 1
          rose(6) = 2
          rose(7) = 3
          rose(8) = 4
       ELSE
          WRITE(numout,*) 'We can not be here'
          CALL ipslerr_p(3,'grid_stuff','We can not be here','','')
       ENDIF

       DO i = 1, npts_glo

          ! index of latitude
          jp = INT( (index_g(i)-1) /iim ) + 1
          
          ! index of longitude
          ip = index_g(i) - ( jp-1 ) * iim
          
          !
          ! 2 resolution
          !
          
          !
          ! 2.1 longitude
          !
          
          ! prevent infinite resolution at the pole
          coslat = MAX( COS( grid_lat(ip,jp) * pi/180. ), mincos )     
          IF ( iim .GT. 1 ) THEN
             
             IF ( ip .EQ. 1 ) THEN
                resolution_g(i,1) = &
                     ABS( grid_lon(ip+1,jp) - grid_lon(ip,jp) ) * &
                     pi/180. * R_Earth * coslat
             ELSEIF ( ip .EQ. iim ) THEN
                resolution_g(i,1) = &
                     ABS( grid_lon(ip,jp) - grid_lon(ip-1,jp) ) * &
                     pi/180. * R_Earth * coslat
             ELSE
                resolution_g(i,1) = &
                     ABS( grid_lon(ip+1,jp) - grid_lon(ip-1,jp) )/2. *&
                     pi/180. * R_Earth * coslat
             ENDIF
             
          ELSE
             
             resolution_g(i,1) = default_resolution
             
             ndefault_lon = ndefault_lon + 1

          ENDIF

          !
          ! 2.2 latitude
          !
          
          IF ( jjm .GT. 1 ) THEN

             IF ( jp .EQ. 1 ) THEN
                resolution_g(i,2) = &
                     ABS( grid_lat(ip,jp) - grid_lat(ip,jp+1) ) * &
                     pi/180. * R_Earth
             ELSEIF ( jp .EQ. jjm ) THEN
                resolution_g(i,2) = &
                     ABS( grid_lat(ip,jp-1) - grid_lat(ip,jp) ) * &
                     pi/180. * R_Earth
             ELSE
                resolution_g(i,2) = &
                     ABS( grid_lat(ip,jp-1) - grid_lat(ip,jp+1) )/2. *&
                     pi/180. * R_Earth
             ENDIF
             
          ELSE
             
             resolution_g(i,2) = default_resolution
             
             ndefault_lat = ndefault_lat + 1
             
          ENDIF
          min_resol(1) = MIN(resolution_g(i,1),min_resol(1))
          min_resol(2) = MIN(resolution_g(i,2),min_resol(2))
          max_resol(1) = MAX(resolution_g(i,1),max_resol(1))
          max_resol(2) = MAX(resolution_g(i,2),max_resol(2))

          area_g(i) = resolution_g(i,1)*resolution_g(i,2)

          !
          ! 3 find neighbours 
          !     
          imm1 = 0
          IF ( ip .GT. 1 ) THEN
             imm1 = ip - 1
          ELSEIF ( global ) THEN
             imm1 = iim
          ENDIF
          
          imp1 = 0
          IF ( ip .LT. iim ) THEN
             imp1 = ip + 1
          ELSEIF ( global ) THEN
             imp1 = 1
          ENDIF
          !
          ! East and West
          !
          IF ( imp1 > 0 ) THEN
             neighbours_g(i,rose(3)) = correspondance(imp1,jp)
          ELSE
             neighbours_g(i,rose(3)) = -1
          ENDIF
          IF ( imm1 > 0 ) THEN
             neighbours_g(i,rose(7)) = correspondance(imm1,jp)
          ELSE
             neighbours_g(i,rose(7)) = -1
          ENDIF
          !
          ! North
          !
          IF ( jp .GT. 1 ) THEN

             neighbours_g(i,rose(1)) = correspondance(ip,jp-1)
             IF ( imp1 > 0 ) THEN
                neighbours_g(i,rose(2)) = correspondance(imp1,jp-1)
             ELSE
                neighbours_g(i,rose(2)) = -1
             ENDIF
             IF ( imm1 > 0 ) THEN
                neighbours_g(i,rose(8)) = correspondance(imm1,jp-1)
             ELSE
                neighbours_g(i,rose(8)) = -1
             ENDIF

          ELSE
             IF ( global ) THEN
                
                ! special treatment for the pole if we are really in a 2d grid
                
                IF ( ( iim .GT. 1 ) .AND. ( jjm .GT. 1 ) ) THEN
                   !
                   ii = MOD(ip+iim/2-1,iim)+1
                   imm1l = ii - 1
                   IF ( imm1l .LT. 1 ) imm1l = iim
                   imp1l = ii + 1
                   IF ( imp1l .GT. iim ) imp1l = 1
                   !
                   IF ( ABS( ( grid_lat(ip,jp) ) - 90. ) .LT. min_sechiba ) THEN
                      ! the grid point sits exactly on the pole. The neighbour is situated
                   ! at a lower latitude.
                      neighbours_g(i,rose(1)) = correspondance( ii, jp+1 )
                      neighbours_g(i,rose(2)) = correspondance( imm1l, jp+1 )
                      neighbours_g(i,rose(8)) = correspondance( imp1l, jp+1 )
                   ELSE
                      ! look across the North Pole
                      neighbours_g(i,rose(1)) = correspondance( ii, jp )
                      neighbours_g(i,rose(2)) = correspondance( imm1l, jp )
                      neighbours_g(i,rose(8)) = correspondance( imp1l, jp )
                   ENDIF
                ENDIF

             ELSE

                neighbours_g(i,rose(1)) = -1
                neighbours_g(i,rose(2)) = -1
                neighbours_g(i,rose(8)) = -1
                
             ENDIF

          ENDIF
          
          ! South
          IF ( jp .LT. jjm ) THEN

             neighbours_g(i,rose(5)) = correspondance(ip,jp+1)
             IF ( imp1 > 0 ) THEN
                neighbours_g(i,rose(4)) = correspondance(imp1,jp+1)
             ELSE
                neighbours_g(i,rose(4)) = -1
             ENDIF
             IF ( imm1 > 0 ) THEN
                neighbours_g(i,rose(6)) = correspondance(imm1,jp+1)
             ELSE
                neighbours_g(i,rose(6)) = -1
             ENDIF

          ELSE
             
             IF ( global ) THEN

                ! special treatment for the pole if we are really in a 2d grid
                
                IF ( ( iim .GT. 1 ) .AND. ( jjm .GT. 1 ) ) THEN
                   !
                   ii = MOD(ip+iim/2-1,iim)+1
                   imm1l = ii - 1
                   IF ( imm1l .LT. 1 ) imm1l = iim
                   imp1l = ii + 1
                   IF ( imp1l .GT. iim ) imp1l = 1
                   !
                   IF ( ( ABS( grid_lat(ip,jp) ) - 90. ) .LT. min_sechiba ) THEN
                      ! the grid point sits exactly on the pole. The neighbour is situated
                      ! at a lower latitude.
                      neighbours_g(i,rose(5)) = correspondance( ii, jp-1 )
                      neighbours_g(i,rose(4)) = correspondance( imm1l, jp-1 )
                      neighbours_g(i,rose(6)) = correspondance( imp1l, jp-1 )
                   ELSE
                      ! look across the South Pole
                      neighbours_g(i,rose(5)) = correspondance( ii, jp )
                      neighbours_g(i,rose(4)) = correspondance( imm1l, jp )
                      neighbours_g(i,rose(6)) = correspondance( imp1l, jp )
                   ENDIF
                ENDIF

             ELSE
                
                neighbours_g(i,rose(5)) = -1
                neighbours_g(i,rose(4)) = -1
                neighbours_g(i,rose(6)) = -1
                
             ENDIF
          ENDIF

       ENDDO

       IF ( printlev >= 2 ) THEN
          WRITE(numout,*) '  > Total number of points: ',npts_glo
          WRITE(numout,*) '  > Using default zonal resolution at',ndefault_lon,' points.'
          WRITE(numout,*) '  > Using default meridional resolution at',ndefault_lat,' points.'
       ENDIF
       !
    ENDIF ! (root_prc)

    CALL scatter(neighbours_g,neighbours)
    CALL scatter(resolution_g,resolution)
    CALL scatter(area_g,area)
    CALL bcast(min_resol)
    CALL bcast(max_resol)
    CALL bcast(resolution_g)
    CALL bcast(neighbours_g)
    IF ( printlev >=4 ) THEN
       WRITE(numout,*) '  > resolution  = ',resolution
       WRITE(numout,*) '  > rose = ',rose
       WRITE(numout,*) '  > neighbours  = ',neighbours
    ENDIF
    IF ( printlev >= 3 ) WRITE(numout,*) 'Leaving grid_stuff'
    
  END SUBROUTINE grid_stuff
  !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE grid
