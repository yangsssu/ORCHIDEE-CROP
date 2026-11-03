










! Definition and allocation of parallel datas for OpenMP.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/Attic/mod_orchidee_omp_data.F90,v 1.1.2.3 2008/11/05 10:08:25 ssipsl Exp $
!- 

MODULE mod_orchidee_omp_data

!-
  USE defprec
  USE ioipsl
  USE mod_orchidee_para_var

  IMPLICIT NONE

CONTAINS

    SUBROUTINE barrier2_omp()

    IMPLICIT NONE

!$OMP BARRIER
!$OMP BARRIER

  END SUBROUTINE barrier2_omp



  
  SUBROUTINE Init_orchidee_omp
  IMPLICIT NONE
  
    is_ok_omp=.FALSE.


    IF (is_ok_omp) THEN
      STOP 'Open MP is not yet implemented for driver'
    ELSE
      omp_size=1
      omp_rank=0
      is_omp_root=.TRUE.
    ENDIF

  END SUBROUTINE Init_orchidee_omp

  SUBROUTINE Init_numout_omp(numout)
    INTEGER, INTENT(in) :: numout
    numout_omp=numout
  END SUBROUTINE Init_numout_omp


  SUBROUTINE Init_orchidee_omp_data(arg_omp_size,arg_omp_rank,arg_nbp_omp,arg_offset_omp)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: arg_omp_size
    INTEGER, INTENT(IN) :: arg_omp_rank
    INTEGER, INTENT(IN) :: arg_nbp_omp
    INTEGER, INTENT(IN) :: arg_offset_omp
    
    INTEGER    :: i
    
    
    IF (arg_omp_rank==0) THEN
      is_omp_root=.TRUE.
    ELSE
      is_omp_root=.FALSE.
    ENDIF
    
    is_ok_omp=.FALSE.

    IF (is_omp_root) omp_size=arg_omp_size

    CALL barrier2_omp()

     omp_rank=arg_omp_rank
    
    IF (is_omp_root) THEN 
      ALLOCATE(nbp_omp_para_nb(0:omp_size-1))
      ALLOCATE(nbp_omp_para_begin(0:omp_size-1))
      ALLOCATE(nbp_omp_para_end(0:omp_size-1))
    ENDIF
    
    CALL barrier2_omp()
    nbp_omp_para_nb(omp_rank)=arg_nbp_omp
    CALL barrier2_omp()
    
    IF (is_omp_root) THEN

      nbp_omp_para_begin(0)=1
      nbp_omp_para_end(0)=nbp_omp_para_nb(0)

      DO i=1,omp_size-1
        nbp_omp_para_begin(i)=nbp_omp_para_end(i-1)+1
        nbp_omp_para_end(i)=nbp_omp_para_begin(i)+nbp_omp_para_nb(i)-1
      ENDDO

    ENDIF

    CALL barrier2_omp()
     
    nbp_omp=nbp_omp_para_nb(omp_rank)
    nbp_omp_begin=nbp_omp_para_begin(omp_rank)
    nbp_omp_end=nbp_omp_para_end(omp_rank)
    
    offset_omp=arg_offset_omp          
    CALL Print_omp_data
    
    CALL Init_synchro_omp()
    
  END SUBROUTINE Init_orchidee_omp_data

  SUBROUTINE print_omp_data
  IMPLICIT NONE

!$OMP CRITICAL  
  PRINT *,'--------> ORCHIDEE TASK ',omp_rank
  PRINT *,'omp_size =',omp_size
  PRINT *,'omp_rank =',omp_rank
  PRINT *,'is_omp_root =',is_omp_root
  PRINT *,'offset_omp',offset_omp
  PRINT *,'nbp_omp_para_nb =',nbp_omp_para_nb
  PRINT *,'nbp_omp_para_begin =',nbp_omp_para_begin
  PRINT *,'nbp_omp_para_end =',nbp_omp_para_end    
  PRINT *,'nbp_omp =',nbp_omp
  PRINT *,'nbp_omp_begin =',nbp_omp_begin
  PRINT *,'nbp_omp_end =',nbp_omp_end    
!$OMP END CRITICAL

  END SUBROUTINE print_omp_data

  SUBROUTINE Init_synchro_omp
  IMPLICIT NONE
    
    IF (is_omp_root) THEN
      ALLOCATE(proc_synchro_omp(0:omp_size-1))
      proc_synchro_omp(:)=.FALSE.

      IF ( check_all_transfert ) THEN
         ALLOCATE(omp_function(0:omp_size-1))
         omp_function(:)=-1
      ENDIF
    ENDIF
    CALL barrier2_omp()

  END SUBROUTINE Init_Synchro_omp
  
  SUBROUTINE Synchro_omp
  IMPLICIT NONE
    INTEGER iter
    LOGICAL, PARAMETER :: check=.TRUE.
    INTEGER, PARAMETER :: iter_max=1
    INTEGER, PARAMETER :: print_iter=1

    proc_synchro_omp(omp_rank)=.TRUE.
    CALL barrier2_omp()

    iter=0
    DO WHILE (.NOT. ALL(proc_synchro_omp))
       iter=iter+1
       IF ( mod(iter,print_iter) == 0 ) THEN
          IF (numout_omp > 0) THEN
             WRITE(numout_omp,*) "ORCHIDEE SYNCHRO OMP : iter ",iter," rank ",omp_rank," wait for ",proc_synchro_omp
          ELSE
             WRITE(*,*) "ORCHIDEE SYNCHRO OMP : iter ",iter," rank ",omp_rank," wait for ",proc_synchro_omp
          ENDIF
       ENDIF
       IF (check) THEN
          IF (iter > iter_max) THEN
             IF (numout_omp > 0) THEN
                WRITE(numout_omp,*) "TOO MUCH WAIT in Synchro_Omp !! iter ",iter," rank ",omp_rank," wait for ",proc_synchro_omp
                WRITE(numout_omp,*) "We stop here"
                WRITE(numout_omp,*) "omp_function : ",omp_function(:)
             ELSE
                WRITE(*,*) "TOO MUCH WAIT in Synchro_Omp !! iter ",iter," rank ",omp_rank," wait for ",proc_synchro_omp
                WRITE(*,*) "We stop here"
                WRITE(*,*) "omp_function : ",omp_function(:)
             ENDIF
             CALL MPI_ABORT(4)
             STOP 'Fatal error from ORCHIDEE : Synchro_Omp failed'
          ENDIF
       ENDIF
    CALL barrier2_omp()
    ENDDO
    CALL barrier2_omp()
    proc_synchro_omp(omp_rank)=.FALSE.
    CALL barrier2_omp()

   END SUBROUTINE Synchro_omp

   SUBROUTINE print_omp_function ()

     IF ( check_all_transfert ) THEN
        CALL barrier2_omp()
        IF (numout_omp > 0) THEN
           WRITE(numout_omp,*) omp_rank,&
                " : ",omp_fct_name(omp_previous),'->',omp_fct_name(omp_function(omp_rank))
           IF (MINVAL(omp_function(:)).LT.MAXVAL(omp_function(:))) &
                WRITE(numout_omp,*) "!!! OMP ERROR : NO MORE SYNCHRO  !!!  ",omp_function(:)
        ELSE
           WRITE(*,*) omp_rank,&
                " : ",omp_fct_name(omp_previous),'->',omp_fct_name(omp_function(omp_rank))
           IF (MINVAL(omp_function(:)).LT.MAXVAL(omp_function(:))) &
                WRITE(*,*) "!!! OMP ERROR : NO MORE SYNCHRO  !!!  ",omp_function(:)
        ENDIF
        CALL barrier2_omp()
     ENDIF

  END SUBROUTINE print_omp_function


END MODULE mod_orchidee_omp_data
