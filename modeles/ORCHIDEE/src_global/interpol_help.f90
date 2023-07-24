!
! Aggregation routines. These routines allow to interpolate from the finer grid on which the 
! surface parameter is available to the coarser one of the model.
!
! The routines work for the fine data on a regular lat/lon grid. This grid can come in as either
! a rank2 array or a vector. Two procedure exist which require slightly different input fields.
!
!
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_global/interpol_help.f90 $
!< $Date: 2016-04-21 10:05:24 +0200 (Thu, 21 Apr 2016) $
!< $Author: albert.jornet $
!< $Revision: 3378 $
!
!
MODULE interpol_help

  ! Modules used :

  USE constantes
  USE mod_orchidee_para

  IMPLICIT NONE

  PRIVATE
  PUBLIC aggregate, aggregate_p
  !
  INTERFACE aggregate
     MODULE PROCEDURE aggregate_2d, aggregate_vec
  END INTERFACE
  !
  INTERFACE aggregate_p
     MODULE PROCEDURE aggregate_2d_p, aggregate_vec_p
  END INTERFACE
  !
  LOGICAL, PARAMETER                              :: check_grid=.TRUE.
  !
CONTAINS
  !
  ! This routing will get for each point of the coarse grid the
  ! indexes of the finer grid and the area of overlap. 
  ! This routine is designed for a fine grid which is regular in lat/lon.
  !
  SUBROUTINE aggregate_2d (nbpt, lalo, neighbours, resolution, contfrac, &
       &                iml, jml, lon_rel, lat_rel, mask, callsign, &
       &                incmax, indinc, areaoverlap, ok, opt_nbpt_start, opt_nbpt_end)

    USE grid, ONLY : global

    !
    ! INPUT
    ! 
    INTEGER(i_std), INTENT(in)   :: nbpt                 ! Number of points for which the data needs to be interpolated
    REAL(r_std), INTENT(in)       :: lalo(nbpt,2)         ! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)   :: neighbours(nbpt,8)   ! Vector of neighbours for each grid point (1=N, 2=E, 3=S, 4=W)
    REAL(r_std), INTENT(in)       :: resolution(nbpt,2)   ! The size in km of each grid-box in X and Y
    REAL(r_std), INTENT(in)       :: contfrac(nbpt)       ! Fraction of land in each grid box.
    INTEGER(i_std), INTENT(in)   :: iml, jml             ! Size of the finer grid
    REAL(r_std), INTENT(in)       :: lon_rel(iml, jml)    ! Longitudes for the finer grid
    REAL(r_std), INTENT(in)       :: lat_rel(iml, jml)    ! Latitudes for the finer grid
    INTEGER(i_std), INTENT(in)   :: mask(iml, jml)       ! Mask which retains only the significative points
                                                         ! of the fine grid.
    CHARACTER(LEN=*), INTENT(in) :: callsign             ! Allows to specify which variable is beeing treated
    INTEGER(i_std), INTENT(in)    :: incmax              ! Maximum point of the fine grid we can store.
    INTEGER(i_std), OPTIONAL, INTENT(in)    :: opt_nbpt_start            ! Input Start grid cell interpolation 
    INTEGER(i_std), OPTIONAL, INTENT(in)    :: opt_nbpt_end              ! Input End grid cell interpolation
    !
    ! Output
    !
    INTEGER(i_std), INTENT(out)  :: indinc(:,:,:)
    REAL(r_std), INTENT(out)      :: areaoverlap(:,:)
    LOGICAL, OPTIONAL, INTENT(out)      :: ok            ! return code
    !
    ! Local Variables
    !
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:) :: lat_ful, lon_ful
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:) :: loup_rel, lolow_rel, laup_rel, lalow_rel
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:) :: searchind
    REAL(r_std) :: lon_up, lon_low, lat_up, lat_low
    REAL(r_std) :: coslat, ax, ay, sgn, lonrel, lolowrel, louprel
    INTEGER(i_std) :: fopt, fopt_max, ip, jp, ib, i, itmp, iprog, nbind
    REAL(r_std) :: domain_minlon,domain_maxlon,domain_minlat,domain_maxlat
    INTEGER(i_std) :: minLon(1), maxLon(1)
    INTEGER(i_std) :: nbpt_start            ! Start grid cell interpolation 
    INTEGER(i_std) :: nbpt_end              ! End grid cell interpolation
    INTEGER(i_std) :: landpoint_idx 

    INTEGER                  :: ALLOC_ERR
    LOGICAL :: err_fopt
    err_fopt = .FALSE.
    !
    ! Some inital assignmens
    !
    areaoverlap(:,:) = moins_un
    indinc(:,:,:) = zero

    ALLOCATE (laup_rel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of laup_rel','','')

    ALLOCATE (loup_rel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of loup_rel','','')

    ALLOCATE (lalow_rel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of lalow_rel','','')

    ALLOCATE (lolow_rel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of lolow_rel','','')

    ALLOCATE (lat_ful(iml+2,jml+2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of lat_ful','','')

    ALLOCATE (lon_ful(iml+2,jml+2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of lon_ful','','')

    ALLOCATE (searchind(iml*jml,2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_2d', 'ERROR IN ALLOCATION of searchind','','')

    nbpt_start = 1
    nbpt_end = nbpt
    IF (PRESENT(opt_nbpt_start) .AND. PRESENT(opt_nbpt_end)) THEN
        nbpt_start = opt_nbpt_start
        nbpt_end = opt_nbpt_end
    ENDIF

    IF (PRESENT(ok)) ok = .TRUE.
    !IF (PRESENT(ok) .AND. ok) ok = .TRUE.
    !
    !    Duplicate the border assuming we have a global grid going from west to east
    !
    lon_ful(2:iml+1,2:jml+1) = lon_rel(1:iml,1:jml)
    lat_ful(2:iml+1,2:jml+1) = lat_rel(1:iml,1:jml)
    !
    IF ( lon_rel(iml,1) .LT. lon_ful(2,2)) THEN
       lon_ful(1,2:jml+1) = lon_rel(iml,1:jml)
       lat_ful(1,2:jml+1) = lat_rel(iml,1:jml)
    ELSE
       lon_ful(1,2:jml+1) = lon_rel(iml,1:jml)-360
       lat_ful(1,2:jml+1) = lat_rel(iml,1:jml)
    ENDIF

    IF ( lon_rel(1,1) .GT. lon_ful(iml+1,2)) THEN
       lon_ful(iml+2,2:jml+1) = lon_rel(1,1:jml)
       lat_ful(iml+2,2:jml+1) = lat_rel(1,1:jml)
    ELSE
       lon_ful(iml+2,2:jml+1) = lon_rel(1,1:jml)+360
       lat_ful(iml+2,2:jml+1) = lat_rel(1,1:jml)
    ENDIF
    !
    sgn = lat_rel(1,1)/ABS(lat_rel(1,1))
    lat_ful(2:iml+1,1) = sgn*180 - lat_rel(1:iml,1)
    sgn = lat_rel(1,jml)/ABS(lat_rel(1,jml))
    lat_ful(2:iml+1,jml+2) = sgn*180 - lat_rel(1:iml,jml)
    lat_ful(1,1) = lat_ful(iml+1,1)
    lat_ful(iml+2,1) = lat_ful(2,1)
    lat_ful(1,jml+2) = lat_ful(iml+1,jml+2)
    lat_ful(iml+2,jml+2) = lat_ful(2,jml+2)
    !
    ! Add the longitude lines to the top and bottom
    !
    lon_ful(:,1) = lon_ful(:,2) 
    lon_ful(:,jml+2) = lon_ful(:,jml+1) 
    !
    !  Get the upper and lower limits of each grid box
    !
    DO ip=1,iml
       DO jp=1,jml
          !
          loup_rel(ip,jp) =MAX(0.5*(lon_ful(ip,jp+1)+lon_ful(ip+1,jp+1)),&
               & 0.5*(lon_ful(ip+1,jp+1)+lon_ful(ip+2,jp+1)))
          lolow_rel(ip,jp) =MIN(0.5*(lon_ful(ip,jp+1)+lon_ful(ip+1,jp+1)),&
               & 0.5*(lon_ful(ip+1,jp+1)+lon_ful(ip+2,jp+1)))
          laup_rel(ip,jp) =MAX(0.5*(lat_ful(ip+1,jp)+lat_ful(ip+1,jp+1)),&
               & 0.5*(lat_ful(ip+1,jp+1)+lat_ful(ip+1,jp+2)))
          lalow_rel(ip,jp) =MIN(0.5*(lat_ful(ip+1,jp)+lat_ful(ip+1,jp+1)),&
               & 0.5*(lat_ful(ip+1,jp+1)+lat_ful(ip+1,jp+2)))
          !
       ENDDO
    ENDDO
    IF (check_grid) THEN
       WRITE(numout,*) "================================"
       WRITE(numout,*) "interpol_aggregate_2d : "
       WRITE(numout,*) "lalo(:,1) :",lalo(:,1)
       WRITE(numout,*) "lalo(:,2) :",lalo(:,2)
       WRITE(numout,*) "Map meshes : "
       WRITE(numout,*) "lat read(1,:) :",lat_rel(1,:)
       WRITE(numout,*) "lat_ful(1,:) :",lat_ful(1,:)
       WRITE(numout,*) "lat_ful(2,:) :",lat_ful(2,:)
       WRITE(numout,*) "lalow_rel(1,:) :",lalow_rel(1,:)
       WRITE(numout,*) "laup_rel(1,:) :",laup_rel(1,:)
       WRITE(numout,*) "================================"
       WRITE(numout,*) "lon read(:,1) :",lon_rel(:,1)
       WRITE(numout,*) "lon_ful(:,1) :",lon_ful(:,1)
       WRITE(numout,*) "lon_ful(:,2) :",lon_ful(:,2)
       WRITE(numout,*) "lolow_rel(:,1) :",lolow_rel(:,1)
       WRITE(numout,*) "loup_rel(:,1) :",loup_rel(:,1)
       WRITE(numout,*) "================================"
    ENDIF
    !
    !
    !  To speedup calculations we will get the limits of the domain of the 
    !  coarse grid and select all the points of the fine grid which are potentialy
    !  in this domain.
    !
    !
    minLon = MINLOC(lalo(1:nbpt,2))
    coslat = MAX(COS(lalo(minLon(1),1) * pi/180. ), mincos )*pi/180. * R_Earth
    domain_minlon = lalo(minLon(1),2) - resolution(minLon(1),1)/(2.0*coslat)
    maxLon = MAXLOC(lalo(1:nbpt,2))
    coslat = MAX(COS(lalo(maxLon(1),1) * pi/180. ), mincos )*pi/180. * R_Earth
    domain_maxlon = lalo(maxLon(1),2) + resolution(maxLon(1),1)/(2.0*coslat)
    !
    coslat = pi/180. * R_Earth
    domain_minlat = MINVAL(lalo(1:nbpt,1)) - resolution(maxLon(1),2)/(2.0*coslat)
    domain_maxlat = MAXVAL(lalo(1:nbpt,1)) + resolution(maxLon(1),2)/(2.0*coslat)
    !
    IF (check_grid) THEN
       WRITE(numout,*) "indices min/max of longitude :",minLon,maxLon, &
            & "; longitude min/max : ",lalo(minLon,1),lalo(maxLon,1)
       WRITE(numout,*) "Domain for coarse grid :"
       WRITE(numout,*) '(',domain_minlat,',',domain_minlon,')',&
   &                   '(',domain_maxlat,',',domain_maxlon,')'
       WRITE(numout,*) "================================"
    ENDIF
    !
    ! we list a first approximation of all point we will need to 
    ! scan to fill our coarse grid.
    !
    IF ( global ) THEN
       ! Here we do the entire globe
       WRITE(numout,*) 'In aggregate_p : do interpolation to global model domain'
       nbind=0
       DO ip=1,iml
          DO jp=1,jml
             IF (mask(ip,jp) == 1 ) THEN
                nbind = nbind + 1
 !               nbind =  1

                searchind(nbind,1) = ip
                searchind(nbind,2) = jp
             ENDIF
          ENDDO
       ENDDO
       !
    ELSE
       ! Now we get a limited number of points
       WRITE(numout,*) 'In aggregate_p : do interpolation to regional model domain'
       nbind=0
       DO ip=1,iml
          DO jp=1,jml
             IF ( loup_rel(ip,jp) >= domain_minlon .AND. lolow_rel(ip,jp) <= domain_maxlon .AND.&
               &  laup_rel(ip,jp) >= domain_minlat .AND. lalow_rel(ip,jp) <= domain_maxlat ) THEN
                IF (mask(ip,jp) == 1 ) THEN
                   nbind = nbind + 1
!                   nbind =  1
                   searchind(nbind,1) = ip
                   searchind(nbind,2) = jp
                ENDIF
             ENDIF
          ENDDO
       ENDDO
    ENDIF
    !
    WRITE(numout,*) 'We will work with ', nbind, ' points of the fine grid'
    !
    WRITE(numout,*) 'searchind(:,:) : ', searchind(:,:) 
    WRITE(numout,*) 'Aggregate_2d : ', callsign
#ifdef INTERPOL_ADVANCE
    WRITE(numout,'(2a40)')'0%--------------------------------------', &
         & '------------------------------------100%'
#endif
    !
    !   Now we take each grid point and find out which values from the forcing we need to average
    !
    fopt_max = -1
    DO ib = nbpt_start, nbpt_end
       landpoint_idx = ib - nbpt_start + 1
       !
       !   Give a progress meter
       !
#ifdef INTERPOL_ADVANCE
       iprog = NINT(REAL(ib,r_std)/REAL(nbpt,r_std)*79.) - NINT(REAL(ib-1,r_std)/REAL(nbpt,r_std)*79.)
       IF ( iprog .NE. 0 ) THEN
          WRITE(numout,'(a1,$)') 'x'
       ENDIF
#endif
       !
       !  We find the 4 limits of the grid-box. As we transform the resolution of the model
       !  into longitudes and latitudes we do not have the problem of periodicity.
       !  coslat is a help variable here !
       !
       coslat = MAX(COS(lalo(ib,1) * pi/180. ), mincos )*pi/180. * R_Earth
       !
       lon_up = lalo(ib,2) + resolution(ib,1)/(2.0*coslat) 
       lon_low =lalo(ib,2) - resolution(ib,1)/(2.0*coslat) 
       !
       coslat = pi/180. * R_Earth
       !
       lat_up =lalo(ib,1) + resolution(ib,2)/(2.0*coslat) 
       lat_low =lalo(ib,1) - resolution(ib,2)/(2.0*coslat) 
       !
       !  Find the grid boxes from the data that go into the model's boxes
       !  We still work as if we had a regular grid ! Well it needs to be localy regular so
       !  so that the longitude at the latitude of the last found point is close to the one 
       !  of the next point.
       !
       fopt = zero
       !
       DO i=1,nbind
          !
          ip = searchind(i,1)
          jp = searchind(i,2)
          !
          !  Either the center of the data grid point is in the interval of the model grid or
          !  the East and West limits of the data grid point are on either sides of the border of
          !  the data grid.
          !
          !  To do that correctly we have to check if the grid box sits on the date-line.
          !
          IF ( lon_low < -180.0 ) THEN
             ! -179 -> -179
             ! 179 -> -181
             lonrel = MOD( lon_rel(ip,jp) - 360.0, 360.0)
             lolowrel = MOD( lolow_rel(ip,jp) - 360.0, 360.0)
             louprel = MOD( loup_rel(ip,jp) - 360.0, 360.0)
             !
          ELSE IF ( lon_up > 180.0 ) THEN
             ! -179 -> 181
             !  179 -> 179
             lonrel = MOD( lon_rel(ip,jp) + 360., 360.0)
             lolowrel = MOD( lolow_rel(ip,jp) + 360., 360.0)
             louprel = MOD( loup_rel(ip,jp) + 360., 360.0)
          ELSE
             lonrel = lon_rel(ip,jp)
             lolowrel = lolow_rel(ip,jp)
             louprel = loup_rel(ip,jp)
          ENDIF
          !
          !
          !
          IF ( lonrel > lon_low .AND. lonrel < lon_up .OR. &
               & lolowrel < lon_low .AND.  louprel > lon_low .OR. &
               & lolowrel < lon_up  .AND.  louprel > lon_up ) THEN
             !
             ! Now that we have the longitude let us find the latitude
             !
             IF ( lat_rel(ip,jp) > lat_low .AND. lat_rel(ip,jp) < lat_up .OR. &
                  & lalow_rel(ip,jp) < lat_low .AND. laup_rel(ip,jp) > lat_low .OR.&
                  & lalow_rel(ip,jp) < lat_up .AND. laup_rel(ip,jp) > lat_up) THEN
                   !
                fopt = fopt + 1
                IF ( fopt > incmax) THEN
                   err_fopt=.TRUE.
                   EXIT
                ELSE
                   !
                   ! If we sit on the date line we need to do the same transformations as above.
                   ! 
                   IF ( lon_low < -180.0 ) THEN
                      lolowrel = MOD( lolow_rel(ip,jp) - 360.0, 360.0)
                      louprel = MOD( loup_rel(ip,jp) - 360.0, 360.0)
                      !
                   ELSE IF ( lon_up > 180.0 ) THEN
                      lolowrel = MOD( lolow_rel(ip,jp) + 360., 360.0)
                      louprel = MOD( loup_rel(ip,jp) + 360., 360.0)
                   ELSE
                      lolowrel = lolow_rel(ip,jp)
                      louprel = loup_rel(ip,jp)
                   ENDIF
                   !
                   ! Get the area of the fine grid in the model grid
                   !
                   coslat = MAX( COS( lat_rel(ip,jp) * pi/180. ), mincos )
                   ax = (MIN(lon_up,louprel)-MAX(lon_low, lolowrel))*pi/180. * R_Earth * coslat
                   ay = (MIN(lat_up, laup_rel(ip,jp))-MAX(lat_low,lalow_rel(ip,jp)))*pi/180. * R_Earth
                   !
                   areaoverlap(landpoint_idx, fopt) = ax*ay
                   indinc(landpoint_idx, fopt, 1) = ip
                   indinc(landpoint_idx, fopt, 2) = jp
                   !
                   ! If this point was 100% within the grid then we can de-select it from our
                   ! list as it can not be in another mesh of the coarse grid.
                   !
                   IF ( louprel < lon_up .AND. lolowrel > lon_low .AND. &
                     &  laup_rel(ip,jp) < lat_up .AND. lalow_rel(ip,jp) > lat_low ) THEN
                      searchind(i,1) = 0
                      searchind(i,2) = 0
                   ENDIF
                   !
                ENDIF
             ENDIF       ! IF lat
          ENDIF          ! IF lon
       ENDDO

       IF (err_fopt) THEN
          WRITE(numout,*) 'Working on variable :', callsign
          WRITE(numout,*) 'Reached value ', fopt,' for fopt on point', ib, lalo(ib,2), lalo(ib,1)
          CALL ipslerr_p(2,'aggregate_2d', &
               'Working on variable :'//callsign, &
               'Reached incmax value for fopt.',&
               'Please increase incmax in subroutine calling aggregate')                   
          IF (PRESENT(ok)) THEN
             ok = .FALSE.
             RETURN
          ELSE
             CALL ipslerr_p(3,'aggregate_2d','Stop now','','')
          ENDIF
       ENDIF
       fopt_max = MAX ( fopt, fopt_max )
       !
       ! De-select the marked points
       !
       itmp = nbind
       nbind = 0
       DO i=1,itmp
          IF ( searchind(i,1) > 0 .AND. searchind(i,2) > 0 ) THEN
             nbind = nbind + 1
             searchind(nbind,1) = searchind(i,1)
             searchind(nbind,2) = searchind(i,2)
          ENDIF
       ENDDO
       !
    ENDDO
    !
    DO ib=nbpt_start, nbpt_end
       DO fopt=1,incmax
          IF (( indinc(landpoint_idx,fopt,1) == 0 .AND. indinc(landpoint_idx,fopt,2) > 0) .OR.&
               & ( indinc(landpoint_idx,fopt,2) == 0 .AND. indinc(landpoint_idx,fopt,1) > 0) ) THEN
             WRITE(*,*) "aggregate_2d PROBLEM : point =",ib, fopt," Indicies = ", &
                  & indinc(landpoint_idx,fopt,1), indinc(landpoint_idx,fopt,2), areaoverlap(landpoint_idx,fopt)
          ENDIF
       ENDDO
    ENDDO


    WRITE(numout,*) ""
    WRITE(numout,*) "aggregate_2D nbvmax = ",incmax, "max used = ",fopt_max
    !
    ! Do some memory management.
    !
    DEALLOCATE (laup_rel)
    DEALLOCATE (loup_rel)
    DEALLOCATE (lalow_rel)
    DEALLOCATE (lolow_rel)
    DEALLOCATE (lat_ful)
    DEALLOCATE (lon_ful)
    DEALLOCATE (searchind)
    !
    ! Close the progress meter
    !
    WRITE(numout,*) '    '
    !
  END SUBROUTINE aggregate_2d

  !
  ! This routing will get for each point of the coarse grid the
  ! indexes of the finer grid and the area of overlap. 
  ! This routine is designed for a fine grid which is regular in meters along lat lon axes.
  !
  SUBROUTINE aggregate_vec (nbpt, lalo, neighbours, resolution, contfrac, &
       &                iml, lon_rel, lat_rel, resol_lon, resol_lat, callsign, &
       &                incmax, indinc, areaoverlap, ok)
    !
    ! INPUT
    ! 
    INTEGER(i_std), INTENT(in)   :: nbpt                 ! Number of points for which the data needs to be interpolated
    REAL(r_std), INTENT(in)       :: lalo(nbpt,2)         ! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)   :: neighbours(nbpt,8)   ! Vector of neighbours for each grid point (1=N, 2=E, 3=S, 4=W)
    REAL(r_std), INTENT(in)       :: resolution(nbpt,2)   ! The size in km of each grid-box in X and Y
    REAL(r_std), INTENT(in)       :: contfrac(nbpt)       ! Fraction of land in each grid box.
    INTEGER(i_std), INTENT(in)   :: iml                  ! Size of the finer grid
    REAL(r_std), INTENT(in)       :: lon_rel(iml)         ! Longitudes for the finer grid
    REAL(r_std), INTENT(in)       :: lat_rel(iml)         ! Latitudes for the finer grid
    REAL(r_std), INTENT(in)       :: resol_lon, resol_lat ! Resolution in meters of the fine grid
    CHARACTER(LEN=*), INTENT(in) :: callsign             ! Allows to specify which variable is beeing treated
    INTEGER(i_std), INTENT(in)    :: incmax              ! Maximum point of the fine grid we can store.
    !
    ! Output
    !
    INTEGER(i_std), INTENT(out)  :: indinc(nbpt,incmax)
    REAL(r_std), INTENT(out)      :: areaoverlap(nbpt,incmax)
    LOGICAL, OPTIONAL, INTENT(out)      :: ok            ! return code
    !
    ! Local Variables
    !
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:) :: searchind
    REAL(r_std) :: lon_up, lon_low, lat_up, lat_low
    REAL(r_std) :: coslat, ax, ay, lonrel, lolowrel, louprel
    REAL(r_std) :: latrel, lauprel, lalowrel
    INTEGER(i_std), DIMENSION(nbpt) :: fopt
    INTEGER(i_std) :: fopt_max, not_found_fopt
    INTEGER(i_std) :: ip, ib, i, j, itmp, iprog, nbind, pp, ipp
    REAL(r_std) :: domain_minlon,domain_maxlon,domain_minlat,domain_maxlat
    REAL(r_std) :: minlon, minlat, mini
    INTEGER(i_std) :: ff(1), incp
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:) :: fine_ind
    INTEGER(i_std) :: pos_pnt(5)
    INTEGER                  :: ALLOC_ERR
    !
    LOGICAL :: err_fopt
    err_fopt = .FALSE.
    !
    ! Some inital assignmens
    !
    areaoverlap(:,:) = moins_un
    indinc(:,:) = zero

    ALLOCATE (searchind(iml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_vec','ERROR IN ALLOCATION of searchbind','','')

    IF (PRESENT(ok)) ok = .TRUE.
    !
    !  To speedup calculations we will get the limits of the domain of the 
    !  coarse grid and select all the points of the fine grid which are potentialy
    !  in this domain.
    !
    !
    ff = MINLOC(lalo(:,2))
    coslat = MAX(COS(lalo(ff(1),1) * pi/180. ), mincos )*pi/180. * R_Earth
    domain_minlon = lalo(ff(1),2) - resolution(ff(1),1)/(2.0*coslat)
    ff = MAXLOC(lalo(:,2))
    coslat = MAX(COS(lalo(ff(1),1) * pi/180. ), mincos )*pi/180. * R_Earth
    domain_maxlon = lalo(ff(1),2) + resolution(ff(1),1)/(2.0*coslat)
    !
    coslat = pi/180. * R_Earth
    domain_minlat = MINVAL(lalo(:,1)) - resolution(ff(1),2)/(2.0*coslat)
    domain_maxlat = MAXVAL(lalo(:,1)) + resolution(ff(1),2)/(2.0*coslat)
    !
    ! Find appropriate resolution for index table
    !
    ff=MINLOC(resolution(:,1))
    coslat = MAX(COS(lalo(ff(1),1) * pi/180. ), mincos )*pi/180. * R_Earth
    minlon=resolution(ff(1),1)/(2.0*coslat)
    ff=MINLOC(resolution(:,2))
    coslat = pi/180. * R_Earth
    minlat=resolution(ff(1),2)/(2.0*coslat)
    mini=MIN(minlon, minlat)
    !
    ! This interpolation only works if the model grid is coarser than the data grid
    !
    IF (MINVAL(resolution(:,1)) < resol_lon .OR. MINVAL(resolution(:,2)) < resol_lat) THEN
       WRITE(numout,*) " === WARNING == "
       WRITE(numout,*) "Resolution minima of the model (lon, lat) : ", &
            & MINVAL(resolution(:,1)), MINVAL(resolution(:,2))
       WRITE(numout,*) "Resolution of the file to be interpolated (fine grid) : ", resol_lon, resol_lat
       WRITE(numout,*) "This interpolation assumes that we aggregate from a fine grid to a coarser grid"
       WRITE(numout,*) "In the data submitted it apears that the model is runing on a finer grid than the data"
    ENDIF
    !
    incp = 10
    IF (mini < 0.1) THEN
       incp=100
    ELSE IF (mini < 0.01) THEN
       incp = 1000
    ENDIF
    !
    ! Allocate the needed memory for fine_ind
    !
    ALLOCATE (fine_ind(NINT(domain_minlon*incp)-2:NINT(domain_maxlon*incp)+2, &
         & NINT(domain_minlat*incp)-2:NINT(domain_maxlat*incp)+2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR/=0) CALL ipslerr_p(3,'aggregate_vec','ERROR IN ALLOCATION of find_ind','','')
    !
    ! Generate a quick access table for the coarse grid
    !
    fine_ind(:,:) = zero
    !
    DO ib=1,nbpt
       coslat = MAX(COS(lalo(ib,1) * pi/180. ), mincos )*pi/180. * R_Earth
       !
       lon_up = lalo(ib,2) + resolution(ib,1)/(2.0*coslat) 
       lon_low =lalo(ib,2) - resolution(ib,1)/(2.0*coslat) 
       !
       coslat = pi/180. * R_Earth
       !
       lat_up =lalo(ib,1) + resolution(ib,2)/(2.0*coslat) 
       lat_low =lalo(ib,1) - resolution(ib,2)/(2.0*coslat) 
       !
       fine_ind(NINT(lon_low*incp):NINT(lon_up*incp),NINT(lat_low*incp):NINT(lat_up*incp))=ib
       !
    ENDDO
    !
    WRITE(numout,*) 'Domaine LON range : ', domain_minlon, domain_maxlon
    WRITE(numout,*) 'Domaine LAT range : ', domain_minlat, domain_maxlat
    !
    ! we list a first approximation of all point we will need to 
    ! scan to fill our coarse grid.
    !
    IF ( domain_minlon <= -179.5 .AND. domain_maxlon >= 179.5 .AND. &
      &  domain_minlat <= -89.5  .AND. domain_maxlat >= 89.5 ) THEN
       ! Here we do the entire globe
       nbind=0
       DO ip=1,iml
          nbind = nbind + 1
          searchind(nbind) = ip
       ENDDO
       !
    ELSE
       ! Now we get a limited number of points
       nbind=0
       DO ip=1,iml
          ! Compute the limits of the meshes of the fine grid
          coslat = MAX(COS(lat_rel(ip) * pi/180. ), mincos )*pi/180. * R_Earth
          louprel = MIN(lon_rel(ip) + resol_lon/(2.0*coslat), 180.)
          lolowrel = MAX(lon_rel(ip) - resol_lon/(2.0*coslat), -180.)
          coslat = pi/180. * R_Earth
          lauprel = MIN(lat_rel(ip) + resol_lat/(2.0*coslat), 90.)
          lalowrel = MAX(lat_rel(ip) - resol_lat/(2.0*coslat), -90.)
          !
          IF ( louprel >= domain_minlon .AND. lolowrel <= domain_maxlon .AND.&
            &  lauprel >= domain_minlat .AND. lalowrel <= domain_maxlat ) THEN
             nbind = nbind + 1
             searchind(nbind) = ip
          ENDIF
       ENDDO
    ENDIF
    !
    WRITE(numout,*) 'We will work with ', nbind, ' points of the fine grid and ', nbpt, 'for the coarse grid'
    !
    WRITE(numout,*) 'Aggregate_vec : ', callsign
    !
    !   Now we take each grid point and find out which values from the forcing we need to average
    !
    fopt(:) = zero
    fopt_max = -1
    !
    !
    !
    loopnbind : DO i=1,nbind
       !
       !
       ip = searchind(i)
       !
       !  Either the center of the data grid point is in the interval of the model grid or
       !  the East and West limits of the data grid point are on either sides of the border of
       !  the data grid.
       !
       lonrel = lon_rel(ip)
       coslat = MAX(COS(lat_rel(ip) * pi/180. ), mincos )*pi/180. * R_Earth
       louprel = MIN(lon_rel(ip) + resol_lon/(2.0*coslat), domain_maxlon)
       lolowrel = MAX(lon_rel(ip) - resol_lon/(2.0*coslat), domain_minlon)
       !
       latrel = lat_rel(ip)
       coslat = pi/180. * R_Earth
       lauprel = MIN(lat_rel(ip) + resol_lat/(2.0*coslat), domain_maxlat)
       lalowrel = MAX(lat_rel(ip) - resol_lat/(2.0*coslat), domain_minlat)
       !
       !
       pos_pnt(:) = zero
       ipp = zero
       pp = fine_ind(NINT(lonrel*incp),NINT(latrel*incp))
       !
       IF (COUNT(pos_pnt(:) == pp) == zero ) THEN 
          pos_pnt(ipp+1) = pp
          ipp = ipp + 1
       ENDIF
       pp = fine_ind(NINT(louprel*incp),NINT(lauprel*incp))
       !
       IF (COUNT(pos_pnt(:) == pp) == zero ) THEN 
          pos_pnt(ipp+1) = pp
          ipp = ipp + 1
       ENDIF
       pp = fine_ind(NINT(louprel*incp),NINT(lalowrel*incp))
       !
       IF (COUNT(pos_pnt(:) == pp) == zero ) THEN 
          pos_pnt(ipp+1) = pp
          ipp = ipp + 1
       ENDIF
       pp = fine_ind(NINT(lolowrel*incp),NINT(lauprel*incp))
       !
       IF (COUNT(pos_pnt(:) == pp) == zero ) THEN 
          pos_pnt(ipp+1) = pp
          ipp = ipp + 1
       ENDIF
       pp = fine_ind(NINT(lolowrel*incp),NINT(lalowrel*incp))
       !
       IF (COUNT(pos_pnt(:) == pp) == zero ) THEN 
          pos_pnt(ipp+1) = pp
          ipp = ipp + 1
       ENDIF
       !
       !
       IF ( ipp > zero ) THEN
          !
          DO pp=1,ipp
             ib = pos_pnt(pp)
             !
             !  We find the 4 limits of the grid-box. As we transform the resolution of the model
             !  into longitudes and latitudes we do not have the problem of periodicity.
             !  coslat is a help variable here !
             !
             coslat = MAX(COS(lalo(ib,1) * pi/180. ), mincos )*pi/180. * R_Earth
             !
             lon_up = lalo(ib,2) + resolution(ib,1)/(2.0*coslat) 
             lon_low =lalo(ib,2) - resolution(ib,1)/(2.0*coslat) 
             !
             coslat = pi/180. * R_Earth
             !
             lat_up =lalo(ib,1) + resolution(ib,2)/(2.0*coslat) 
             lat_low =lalo(ib,1) - resolution(ib,2)/(2.0*coslat) 
             !
             IF ( lonrel > lon_low .AND. lonrel < lon_up .OR. &
                  & lolowrel < lon_low .AND.  louprel > lon_low .OR. &
                  & lolowrel < lon_up  .AND.  louprel > lon_up ) THEN
                !
                ! Now that we have the longitude let us find the latitude
                !             
                IF ( latrel > lat_low .AND. latrel < lat_up .OR. &
                     & lalowrel < lat_low .AND. lauprel > lat_low .OR.&
                     & lalowrel < lat_up .AND. lauprel > lat_up) THEN
                   !
                   fopt(ib) = fopt(ib) + 1
                   fopt_max = MAX ( fopt(ib), fopt_max )
                   !
                   IF ( fopt(ib) > incmax) THEN
                      err_fopt=.TRUE.
                      EXIT loopnbind
                   ELSE
                      !
                      ! Get the area of the fine grid in the model grid
                      !
                      coslat = MAX( COS( lat_rel(ip) * pi/180. ), mincos )
                      ax = (MIN(lon_up,louprel)-MAX(lon_low,lolowrel))*pi/180. * R_Earth * coslat
                      ay = (MIN(lat_up,lauprel)-MAX(lat_low,lalowrel))*pi/180. * R_Earth
                      !
                      areaoverlap(ib, fopt(ib)) = ax*ay
                      indinc(ib, fopt(ib)) = ip
                      !
                   ENDIF
                ENDIF
             ENDIF
          ENDDO
       ENDIF
    ENDDO loopnbind
    !
    IF (err_fopt) THEN
       WRITE(numout,*) 'Reached value ', fopt(ib),' for fopt on point', ib
       CALL ipslerr_p(2,'aggregate_vec (nbpt < nbind)', &
            'Working on variable :'//callsign, &
            'Reached incmax value for fopt.',&
            'Please increase incmax in subroutine calling aggregate')
       IF (PRESENT(ok)) THEN
          ok = .FALSE.
          RETURN
       ELSE
          CALL ipslerr_p(3,'aggregate_vec','Stop now','','')
       ENDIF
    ENDIF
    !
    WRITE(numout,*) 
    not_found_fopt = COUNT(fopt(:) .EQ. zero)
    WRITE(numout,*) "aggregate_vec : ",not_found_fopt, &
         & "did not find any corresponding data in the input file."
    WRITE(numout,*) "aggregate_vec : This is ", not_found_fopt/FLOAT(nbpt)*100., &
         & " % of the grid"
    WRITE(numout,*) "aggregate_vec : nbvmax = ",incmax, "max used = ",fopt_max
    !
    ! Do some memory management.
    !
    DEALLOCATE (searchind)
    DEALLOCATE (fine_ind)
    !
    ! Close the progress meter
    !
    WRITE(numout,*) '    '
    !
  END SUBROUTINE aggregate_vec
!
!

  SUBROUTINE aggregate_vec_p(nbpt, lalo, neighbours, resolution, contfrac,          &
       &                 iml, lon_ful, lat_ful, resol_lon, resol_lat, callsign, &
       &                 nbvmax, sub_index, sub_area, ok)
    
    IMPLICIT NONE
    
    INTEGER(i_std), INTENT(in)   :: nbpt                 
    REAL(r_std), INTENT(in)       :: lalo(nbpt,2)        
    INTEGER(i_std), INTENT(in)   :: neighbours(nbpt,8)   
    REAL(r_std), INTENT(in)       :: resolution(nbpt,2)   
    REAL(r_std), INTENT(in)       :: contfrac(nbpt)       
    INTEGER(i_std), INTENT(in)   :: iml                 
    REAL(r_std), INTENT(in)       :: lon_ful(iml)         
    REAL(r_std), INTENT(in)       :: lat_ful(iml)         
    REAL(r_std), INTENT(in)       :: resol_lon, resol_lat 
    CHARACTER(LEN=*), INTENT(in) :: callsign             
    INTEGER(i_std), INTENT(in)   :: nbvmax             
    INTEGER(i_std), INTENT(out)  :: sub_index(nbpt,nbvmax)
    REAL(r_std), INTENT(out)      :: sub_area(nbpt,nbvmax) 
    LOGICAL, OPTIONAL, INTENT(out)      :: ok            ! return code

    INTEGER(i_std)  :: sub_index_g(nbp_glo,nbvmax)
    REAL(r_std)       :: sub_area_g(nbp_glo,nbvmax)
        
    IF (is_root_prc) CALL aggregate(nbp_glo, lalo_g, neighbours_g, resolution_g, contfrac_g, &
   &                                  iml, lon_ful, lat_ful, resol_lon, resol_lat, callsign,   &
   &                                  nbvmax, sub_index_g, sub_area_g, ok)

    CALL BCAST(ok)
    CALL scatter(sub_index_g,sub_index)
    CALL scatter(sub_area_g,sub_area)
   
   
  END SUBROUTINE aggregate_vec_p

  SUBROUTINE aggregate_2d_p(nbpt, lalo, neighbours, resolution, contfrac,          &
       &                 iml, jml, lon_ful, lat_ful, mask, callsign, &
       &                 nbvmax, sub_index, sub_area, ok)
    
    IMPLICIT NONE
    
    INTEGER(i_std), INTENT(in)   :: nbpt                 
    REAL(r_std), INTENT(in)       :: lalo(nbpt,2)        
    INTEGER(i_std), INTENT(in)   :: neighbours(nbpt,8)   
    REAL(r_std), INTENT(in)       :: resolution(nbpt,2)   
    REAL(r_std), INTENT(in)       :: contfrac(nbpt)       
    INTEGER(i_std), INTENT(in)   :: iml,jml                 
    REAL(r_std), INTENT(in)       :: lon_ful(iml,jml)         
    REAL(r_std), INTENT(in)       :: lat_ful(iml,jml)         
    INTEGER(i_std), INTENT(in)   :: mask(iml, jml)
    CHARACTER(LEN=*), INTENT(in) :: callsign             
    INTEGER(i_std), INTENT(in)   :: nbvmax             
    INTEGER(i_std), INTENT(out)  :: sub_index(nbpt,nbvmax,2)
    REAL(r_std), INTENT(out)      :: sub_area(nbpt,nbvmax) 
    LOGICAL, OPTIONAL, INTENT(out)      :: ok            ! return code
    INTEGER(i_std)                 :: nbp_start, nbp_end

    nbp_start = nbp_mpi_para_begin(mpi_rank)
    nbp_end = nbp_mpi_para_end(mpi_rank)
    
    CALL aggregate_2d(nbp_glo, lalo_g, neighbours_g, resolution_g, contfrac_g, &
   &                   iml, jml, lon_ful, lat_ful, mask, callsign,   &
   &                   nbvmax, sub_index, sub_area, ok,          &
   &                   nbp_start, nbp_end)
   
  END SUBROUTINE aggregate_2d_p
!
END MODULE interpol_help
