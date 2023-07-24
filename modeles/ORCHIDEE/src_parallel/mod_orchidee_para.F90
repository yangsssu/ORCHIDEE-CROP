! Initialization of parallel for MPI and OpenMP.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/Attic/mod_orchidee_para.F90,v 1.1.2.4 2008/11/05 10:14:05 ssipsl Exp $
!- 

MODULE mod_orchidee_para

  USE mod_orchidee_para_var
  USE mod_orchidee_mpi_data
  USE mod_orchidee_omp_data
  USE mod_orchidee_transfert_para
    
CONTAINS
    
  SUBROUTINE Init_orchidee_para(communicator)
    IMPLICIT NONE
    INTEGER,OPTIONAL,INTENT(in) :: communicator	

    CALL Init_orchidee_omp


    IF ( PRESENT(communicator) ) THEN
       CALL Init_orchidee_mpi(communicator)
    ELSE
       CALL Init_orchidee_mpi
    ENDIF


    IF (is_mpi_root .AND. is_omp_root) THEN
       is_root_prc=.TRUE.
    ELSE
       is_root_prc=.FALSE.
    ENDIF
  END SUBROUTINE Init_orchidee_para
    
  
  SUBROUTINE Init_orchidee_data_para_driver(nbp,kindex_glo)

    IMPLICIT NONE
    INTEGER,INTENT(IN) :: nbp
    INTEGER,INTENT(IN) :: kindex_glo(nbp)
      
    INTEGER :: first_point
    INTEGER :: last_point
    INTEGER :: nbp_loc
    INTEGER :: nbp_loc_para(0:mpi_size-1)
    INTEGER,ALLOCATABLE :: kindex_loc(:)
    INTEGER :: offset
    INTEGER :: i
    
      
    last_point=0
    
    CALL read_load_balance(nbp,nbp_loc_para)    
    
    DO i=0,mpi_rank
       nbp_loc=nbp_loc_para(i)
       First_point=last_point+1
       Last_point=last_point+nbp_loc
    ENDDO
      
    ALLOCATE(kindex_loc(nbp_loc))
    DO i=1,nbp_loc
       kindex_loc(i)=kindex_glo(i+First_Point-1)
    ENDDO
    
    IF (mpi_rank==0) THEN
       offset=0
    ELSE
       offset=kindex_glo(First_point-1)-MOD(kindex_glo(First_point-1),iim_g)
    ENDIF
    
    kindex_loc(:)=kindex_loc(:)-offset
    
    CALL Init_orchidee_data_para(nbp_loc,kindex_loc,offset,omp_size,omp_rank,MPI_COMM_ORCH)
    CALL Set_stdout_file('out_orchidee')
    CALL ipslnlf(new_number=numout)
    !    
  END SUBROUTINE Init_orchidee_data_para_driver
    
  
  SUBROUTINE Init_orchidee_data_para(nbp,kindex,arg_offset,arg_omp_size,arg_omp_rank,COMM)

    IMPLICIT NONE
    INTEGER,INTENT(IN)     :: nbp
    INTEGER,INTENT(IN)     :: kindex(nbp)
    INTEGER,INTENT(IN)     :: arg_offset
    INTEGER,INTENT(IN)     :: arg_omp_size
    INTEGER,INTENT(IN)     :: arg_omp_rank
    INTEGER,INTENT(IN)     :: COMM
    
    INTEGER,SAVE              :: arg_nbp_mpi
    INTEGER,ALLOCATABLE,SAVE  :: kindex_mpi(:)
    
    offset=arg_offset 
    CALL init_orchidee_omp_data(arg_omp_size,arg_omp_rank,nbp,offset)
    
    IF (is_omp_root) THEN
       arg_nbp_mpi=SUM(nbp_omp_para_nb(:))
       ALLOCATE(kindex_mpi(arg_nbp_mpi))
    ENDIF

    CALL barrier2_omp()
    kindex_mpi(nbp_omp_begin:nbp_omp_end)=kindex(:)+offset
    CALL barrier2_omp()
      
    IF (is_omp_root) THEN      
       kindex_mpi(:)=kindex_mpi(:)-offset
       CALL init_orchidee_mpi_data(arg_nbp_mpi,kindex_mpi,offset,COMM)
       nbp_glo=SUM(nbp_mpi_para(:))
    ENDIF
    CALL barrier2_omp()

    nbp_loc=nbp
    
    IF (is_mpi_root .AND. is_omp_root) THEN
       is_root_prc=.TRUE.
    ELSE
       is_root_prc=.FALSE.
    ENDIF
    
    CALL Test_orchidee_para
    
  END SUBROUTINE Init_orchidee_data_para

  SUBROUTINE set_grid_glo(arg_nbp_lon,arg_nbp_lat,arg_nbp_glo)
    IMPLICIT NONE

    INTEGER(i_std), INTENT(IN) :: arg_nbp_lon
    INTEGER(i_std), INTENT(IN) :: arg_nbp_lat
    INTEGER(i_std), INTENT(IN),OPTIONAL :: arg_nbp_glo
    iim_g=arg_nbp_lon
    jjm_g=arg_nbp_lat
    IF (PRESENT(arg_nbp_glo)) nbp_glo=arg_nbp_glo
  END SUBROUTINE set_grid_glo
  
  SUBROUTINE Allocate_grid_glo
    IMPLICIT NONE
  
    ALLOCATE(resolution_g(nbp_glo,2),area_g(nbp_glo),lalo_g(nbp_glo,2), &
         &   neighbours_g(nbp_glo,8),contfrac_g(nbp_glo),index_g(nbp_glo))
    ALLOCATE(lon_g(iim_g, jjm_g), lat_g(iim_g, jjm_g), zlev_g(iim_g, jjm_g))
  
  END SUBROUTINE Allocate_grid_glo
  
    
  SUBROUTINE Set_stdout_file(filename)

    IMPLICIT NONE

    CHARACTER(len=*), INTENT(IN) :: filename
    CHARACTER(len=255) :: fileout
    CHARACTER(len=4)  :: num_mpi
    CHARACTER(len=4)  :: num_omp
    INTEGER,PARAMETER :: base_numout=100
    INTEGER           :: ierr

    IF (is_ok_mpi) THEN
       WRITE(num_mpi,'(I4.4)') mpi_rank
    ENDIF
    
    IF (is_ok_omp) THEN
       WRITE(num_omp,'(I4.4)') omp_rank
    ENDIF
    
     
    IF (is_ok_mpi .AND. is_ok_omp) THEN
       fileout=TRIM(filename)//'_'//num_mpi//'.'//num_omp
       numout=base_numout+omp_rank
    ELSE IF (is_ok_mpi .AND. (.NOT. is_ok_omp)) THEN
       fileout=TRIM(filename)//'_'//num_mpi
       numout=base_numout
    ELSE IF ((.NOT. is_ok_mpi) .AND. is_ok_omp) THEN
       fileout=TRIM(filename)//'_'//num_omp
       numout=base_numout+omp_rank
    ELSE
       fileout=TRIM(filename)
       numout=base_numout
    ENDIF
!$OMP CRITICAL  
    WRITE(*,*) "Set_stdout_file (rank ",mpi_rank,omp_rank,"), id output :",numout
!$OMP END CRITICAL
    
    OPEN(UNIT=numout,FILE=TRIM(fileout),ACTION='write',STATUS='unknown',FORM='formatted',IOSTAT=ierr) 
    IF (ierr /= 0) THEN
#ifdef CPP_PARA
       CALL MPI_FINALIZE(ierr)
#endif
       WRITE(*,*) "In Set_stdout_file : Erreur can't open file ", filename
       STOP 1
    ENDIF
 
!$OMP CRITICAL  
    WRITE(numout,*) "Set_stdout_file (rank ",mpi_rank,omp_rank,"), id output :",numout
!$OMP END CRITICAL

    CALL Init_numout_omp(numout)

  END SUBROUTINE Set_stdout_file
      
      
  SUBROUTINE Test_orchidee_para

    IMPLICIT NONE

    INTEGER,PARAMETER :: dimsize=3
    REAL :: Array(nbp_loc,dimsize)
    REAL :: Array_glo(nbp_glo,dimsize)
    REAL :: Array_glo_tmp(nbp_glo,dimsize)
    REAL :: Array2D_loc(iim_g,jj_nb)
    REAL :: Array2D_glo(iim_g,jjm_g)
    REAL :: sum1,sum2,sum3
    
    INTEGER :: i,j
    
    DO j=1,dimsize
       DO i=1,nbp_loc
          Array(i,j)=10*j+omp_rank+i*1000
       ENDDO
    ENDDO
      
    CALL gather(Array,Array_glo)
    CALL bcast(Array_glo)
    CALL scatter(Array_glo,array)
    CALL gather(array,array_glo_tmp)
    CALL bcast(array_glo_tmp)    
    WRITE(*,*) "Test parallelism (rank ",mpi_rank,omp_rank,"), Sould be 0 :",SUM(array_glo-array_glo_tmp)

    sum1=SUM(array)
    CALL reduce_sum(sum1,sum2)
    CALL bcast(sum2)
    sum3=SUM(array_glo)
    WRITE(*,*) "Test parallelism (rank ",mpi_rank,omp_rank,"), Sould be 0 :",sum3-sum2
    
    IF (is_omp_root) THEN
       DO j=1,jjm_g
          DO i=1,iim_g
             Array2D_glo(i,j)=(j-1)*iim_g+i
          ENDDO
       ENDDO
        
       array2D_loc(:,:)=0
       CALL scatter2D_mpi(array2D_glo,array2D_loc)
       array2D_glo(:,:)=0
       CALL gather2D_mpi(array2D_loc,array2D_glo)
       CALL bcast_mpi(array2D_glo)
       sum1=SUM(array2D_glo)
       sum2=SUM(array2D_loc)
       CALL reduce_sum_mpi(sum2,sum3)
       CALL bcast_mpi(sum3)
       
       WRITE(*,*) "Test parallelism (rank ",mpi_rank,omp_rank,"), Sould be 0 :",sum3-sum1
    ENDIF
    CALL barrier2_omp()

  END SUBROUTINE  Test_orchidee_para
  
END MODULE mod_orchidee_para
