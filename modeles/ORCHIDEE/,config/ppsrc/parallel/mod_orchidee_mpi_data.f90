










! Definition and allocation of parallel datas for MPI.
! Initialization of parallel or sequentiel IOs.
! Definition of Load Balancing functions.

!-
!- $Header: $
!- 

MODULE mod_orchidee_mpi_data

!-
  USE defprec
  USE ioipsl
  USE xios_orchidee
  USE mod_orchidee_para_var

  IMPLICIT NONE

!-
! Redefinition of MPI function if not second underscore in MPI library.
! One must use -DMPI_SECOND__ in precompilation to activate those definitions.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/src_parallel.h,v 1.3 2009/06/24 10:15:00 ssipsl Exp $
!-
















































































































































































































!-
!-

CONTAINS
 
  SUBROUTINE Init_orchidee_mpi(communicator)


    IMPLICIT NONE
    INTEGER, OPTIONAL, INTENT(in) :: communicator
    INTEGER :: COMM
    INTEGER :: ierr
    
    INCLUDE 'mpif.h'

    !Config Key   = XIOS_ORCHIDEE_OK
    !Config Desc  = Use XIOS for writing diagnostics file
    !Config If    = 
    !Config Def   = y
    !Config Help  = If XIOS_ORCHIDEE_OK=y then no output with IOIPSL can be done
    !Config Units = [FLAG]
    CALL getin('XIOS_ORCHIDEE_OK',xios_orchidee_ok)
    WRITE(numout,*)'In init_ochidee_mpi : xios_orchidee_ok=',xios_orchidee_ok

    IF ( xios_orchidee_ok ) THEN
       CALL MPI_INIT(ierr)
       CALL xios_orchidee_comm_init(COMM)
    ELSE IF ( PRESENT(communicator) ) THEN
       COMM=communicator
    ELSE
       CALL MPI_INIT(ierr)
       COMM=MPI_COMM_WORLD
    ENDIF
    CALL MPI_COMM_SIZE(COMM,mpi_size,ierr)
    CALL MPI_COMM_RANK(COMM,mpi_rank,ierr)
    is_ok_mpi=.TRUE.
    
    mpi_rank_root=0

    IF (mpi_rank==mpi_rank_root) THEN 
      is_mpi_root=.TRUE.
    ELSE
      is_mpi_root=.FALSE.
    ENDIF
  
    CALL Init_const_mpi(COMM)
      
  END SUBROUTINE Init_orchidee_mpi



  SUBROUTINE Init_orchidee_mpi_data(arg_nbp_mpi,arg_kindex_mpi,arg_offset_mpi,COMM)

  IMPLICIT NONE
    INCLUDE 'mpif.h'
    INTEGER, INTENT(IN) :: arg_nbp_mpi
    INTEGER, INTENT(IN) :: arg_kindex_mpi(arg_nbp_mpi)
    INTEGER, INTENT(IN) :: arg_offset_mpi
    INTEGER, INTENT(IN) :: COMM

    INTEGER :: i
  
    INTEGER :: ierr
    is_ok_mpi=.TRUE.

    ! Initialization of MPI_COMM_ORCH 
    CALL init_const_mpi(COMM)
    IF (is_ok_mpi) THEN    
      CALL MPI_COMM_SIZE(MPI_COMM_ORCH,mpi_size,ierr)    
      CALL MPI_COMM_RANK(MPI_COMM_ORCH,mpi_rank,ierr)
    ELSE
      mpi_size=1
      mpi_rank=0
    ENDIF   
    
    IF (mpi_rank == 0) THEN
      mpi_rank_root = 0
      is_mpi_root = .true.
    ENDIF
    
    ALLOCATE(nbp_mpi_para(0:mpi_size-1))
    ALLOCATE(nbp_mpi_para_begin(0:mpi_size-1))
    ALLOCATE(nbp_mpi_para_end(0:mpi_size-1))    
    ALLOCATE(jj_para_nb(0:mpi_size-1))
    ALLOCATE(jj_para_begin(0:mpi_size-1))
    ALLOCATE(jj_para_end(0:mpi_size-1))
    ALLOCATE(ii_para_begin(0:mpi_size-1))
    ALLOCATE(ii_para_end(0:mpi_size-1))    
    ALLOCATE(ij_para_nb(0:mpi_size-1))
    ALLOCATE(ij_para_begin(0:mpi_size-1))
    ALLOCATE(ij_para_end(0:mpi_size-1))
    

    nbp_mpi=arg_nbp_mpi
    ALLOCATE(kindex_mpi(nbp_mpi))
    kindex_mpi(:)=arg_kindex_mpi(:)
    
    offset_mpi=arg_offset_mpi
    
    IF (is_ok_mpi) THEN
      CALL MPI_AllGather(nbp_mpi,1,MPI_INT_ORCH,nbp_mpi_para,1,MPI_INT_ORCH,MPI_COMM_ORCH,ierr)
    ELSE
      nbp_mpi_para(0)=nbp_mpi
    ENDIF
    
    nbp_mpi_para_begin(0)=1
    nbp_mpi_para_end(0)=nbp_mpi_para(0)
    DO i=1,mpi_size-1
      nbp_mpi_para_begin(i)=nbp_mpi_para_end(i-1)+1
      nbp_mpi_para_end(i)=nbp_mpi_para_begin(i)+nbp_mpi_para(i)-1
    ENDDO
    nbp_mpi_begin=nbp_mpi_para_begin(mpi_rank)
    nbp_mpi_end=nbp_mpi_para_end(mpi_rank)
    
    
    IF (mpi_rank==mpi_size-1) THEN
      ij_end=iim_g*jjm_g
    ELSE
      ij_end=kindex_mpi(nbp_mpi)+offset_mpi
    ENDIF

    IF (is_ok_mpi) THEN    
      CALL MPI_Allgather(ij_end,1,MPI_INT_ORCH,ij_para_end,1,MPI_INT_ORCH,MPI_COMM_ORCH,ierr)
    ELSE
      ij_para_end(0)=ij_end
    ENDIF
    
    ij_para_begin(0)=1
    ij_para_nb(0)=ij_para_end(0)-ij_para_begin(0)+1
    
    DO i=1,mpi_size-1
      ij_para_begin(i)=ij_para_end(i-1)+1
      ij_para_nb(i)=ij_para_end(i)-ij_para_begin(i)+1
    ENDDO
    
    DO i=0,mpi_size-1
      jj_para_begin(i)=(ij_para_begin(i)-1)/iim_g + 1
      jj_para_end(i)=(ij_para_end(i)-1)/iim_g + 1
      jj_para_nb(i)=jj_para_end(i)-jj_para_begin(i)+1
          
      ii_para_begin(i)=MOD(ij_para_begin(i)-1,iim_g)+1
      ii_para_end(i)=MOD(ij_para_end(i)-1,iim_g)+1
    ENDDO

   
    ij_nb=ij_para_nb(mpi_rank)
    ij_begin=ij_para_begin(mpi_rank)
    ij_end=ij_para_end(mpi_rank)
        
    jj_nb=jj_para_nb(mpi_rank)
    jj_begin=jj_para_begin(mpi_rank)
    jj_end=jj_para_end(mpi_rank)
    
    ii_begin=ii_para_begin(mpi_rank)
    ii_end=ii_para_end(mpi_rank)
        
      
    CALL print_mpi_data
  
    
  END SUBROUTINE Init_orchidee_mpi_data
  
  SUBROUTINE init_const_mpi(COMM)

  IMPLICIT NONE
    INTEGER :: COMM

    INCLUDE 'mpif.h'
    
    MPI_COMM_ORCH=COMM
    
    IF (i_std==i_4) THEN
       MPI_INT_ORCH=MPI_INTEGER4
    ELSEIF (i_std==i_8) THEN
       MPI_INT_ORCH=MPI_INTEGER8
    ELSE
       MPI_INT_ORCH=MPI_INTEGER
    ENDIF
         
    IF (r_std==r_4) THEN
       MPI_REAL_ORCH=MPI_REAL4
    ELSEIF (r_std==r_8) THEN
       MPI_REAL_ORCH=MPI_REAL8
    ELSE
       MPI_REAL_ORCH=MPI_REAL
    ENDIF

  END SUBROUTINE init_const_mpi

  SUBROUTINE Finalize_mpi

  IMPLICIT NONE
  include 'mpif.h'  
  INTEGER :: ierr

  CALL xios_orchidee_finalize

  CALL MPI_FINALIZE(ierr)
   
  END SUBROUTINE Finalize_mpi
  
  SUBROUTINE print_mpi_data

  IMPLICIT NONE
    
    WRITE(numout,*) '==== MPI DOMAIN ===='
    WRITE(numout,*) '     ----------     '
    WRITE(numout,*) 'mpi_size',mpi_size
    WRITE(numout,*) 'mpi_rank',mpi_rank
    WRITE(numout,*) 'is_mpi_root',is_mpi_root
    WRITE(numout,*) 'mpi_rank_root',mpi_rank_root

    WRITE(numout,*) 'nbp_mpi_begin=',nbp_mpi_begin
    WRITE(numout,*) 'nbp_mpi_end  =',nbp_mpi_end
    WRITE(numout,*) 'nbp_mpi=',nbp_mpi
          
    WRITE(numout,*) 'ij_begin=',ij_begin
    WRITE(numout,*) 'ij_end=',ij_end
    WRITE(numout,*) 'ij_nb=',ij_nb
    WRITE(numout,*) 'jj_begin=',jj_begin
    WRITE(numout,*) 'jj_end=',jj_end
    WRITE(numout,*) 'jj_nb=',jj_nb	
    WRITE(numout,*) 'ii_begin=',ii_begin
    WRITE(numout,*) 'ii_end=',ii_end
    
    WRITE(numout,*) 'offset_mpi',offset_mpi
    WRITE(numout,*) 'nbp_mpi_para_begin=',nbp_mpi_para_begin
    WRITE(numout,*) 'nbp_mpi_para_end  =',nbp_mpi_para_end
    WRITE(numout,*) 'nbp_mpi_para=',nbp_mpi_para
          
    WRITE(numout,*) 'ij_para_begin=',ij_para_begin
    WRITE(numout,*) 'ij_para_end=',ij_para_end
    WRITE(numout,*) 'ij_para_nb=',ij_para_nb
    WRITE(numout,*) 'jj_para_begin=',jj_para_begin
    WRITE(numout,*) 'jj_para_end=',jj_para_end
    WRITE(numout,*) 'jj_para_nb=',jj_para_nb	
    WRITE(numout,*) 'ii_para_begin=',ii_para_begin
    WRITE(numout,*) 'ii_para_end=',ii_para_end
  
  END SUBROUTINE print_mpi_data
  
 SUBROUTINE Read_Load_balance(NbPoints,Nbpoints_loc)

    IMPLICIT NONE
    INTEGER,INTENT(IN)  :: NbPoints
    INTEGER,INTENT(OUT) :: Nbpoints_loc(0:mpi_size-1)
    INTEGER :: i,s
    INTEGER :: ierr
    
    CHARACTER(len=255)  :: filename='Load_balance_orchidee.dat'
    INTEGER :: j
    INTEGER :: unit_number=10

    OPEN(UNIT=unit_number,FILE=trim(filename),STATUS='old',FORM='formatted',IOSTAT=ierr) 
    Nbpoints_loc(:) = 0

    s=0
    IF (ierr==0) THEN
       i=0
       !- Reading for any balancing file (even with a bad structure)
       DO WHILE (i < mpi_size .AND. ierr == 0) 
          READ (unit_number,*,IOSTAT=ierr) j,Nbpoints_loc(i)
          s=s+Nbpoints_loc(i)
          i=i+1
       ENDDO
       CLOSE(unit_number)
    ENDIF
    
    !- Correction of bad balancing file (or an empty file) => same nb of points for each procs
    IF (ierr/=0 .OR. s/=Nbpoints) THEN
       DO i=0,mpi_size-1
          Nbpoints_loc(i)=Nbpoints/mpi_size
          IF (MOD(Nbpoints,mpi_size) > i) Nbpoints_loc(i)=Nbpoints_loc(i)+1
       ENDDO
    ENDIF
    
  END SUBROUTINE Read_Load_balance
  
  SUBROUTINE Write_Load_balance(times)
    IMPLICIT NONE
    REAL,INTENT(IN) :: times
  
    CHARACTER(len=255)  :: filename='Load_balance_orchidee.dat'
    INTEGER :: unit_number=10
    INTEGER :: i,ierr
    REAL :: All_Times(0:mpi_size-1)
    REAL :: average
    REAL :: efficiency
    INTEGER :: dp,S
    INTEGER :: New_nbpoints(0:mpi_size-1)
    
    WRITE(numout,*) 'time',times

    IF (is_ok_mpi) THEN
      CALL MPI_GATHER(times,1,MPI_REAL_ORCH,All_times,1,MPI_REAL_ORCH,mpi_rank_root,MPI_COMM_ORCH,ierr)
    ELSE
      All_times(:)=times
    ENDIF
    
    IF (is_mpi_root) WRITE(numout,*) 'ALL_times',All_times

    IF (is_mpi_root) THEN
     
       OPEN(UNIT=unit_number,FILE=trim(filename),STATUS='replace',FORM='formatted',IOSTAT=ierr)
       
       average=sum(All_times(:))/mpi_size
       DO i=0,mpi_size-1
          efficiency=All_times(i)/nbp_mpi_para(i)
          New_nbpoints(i)=Nbp_mpi_para(i)-(All_times(i)-average)/efficiency
       ENDDO
       
       S=sum(new_nbpoints(:))
       dp=nbp_glo-S
       
       IF ( dp > 0 ) THEN
          DO WHILE ( dp > 0 )
             New_nbpoints(MOD(dp,mpi_size))=New_nbpoints(MOD(dp,mpi_size))+1
             dp=dp-1
          ENDDO
       ELSE
          dp=-dp
          DO WHILE ( dp > 0 )
             New_nbpoints(MOD(dp,mpi_size))=New_nbpoints(MOD(dp,mpi_size))-1
             dp=dp-1
          ENDDO
       ENDIF
       

       ! If this algorithm diverge, we use previous repartition.
       IF ( ANY(New_nbpoints(:) .LE. 0) ) THEN
          New_nbpoints(:)=Nbp_mpi_para(:)
       ENDIF
       
       DO i=0,mpi_size-1
          WRITE(Unit_number,*) i,New_nbpoints(i)
       ENDDO
       CLOSE(Unit_number)
    ENDIF

  END SUBROUTINE Write_Load_Balance
  
END MODULE mod_orchidee_mpi_data

! Overlapp of MPI functions not present in some MPI implementations.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/mpi_dummy.h,v 1.2 2007/06/12 08:04:26 ssipsl Exp $
!-
