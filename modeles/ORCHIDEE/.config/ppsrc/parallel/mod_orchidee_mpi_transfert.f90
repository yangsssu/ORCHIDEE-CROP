










! Low level MPI parallel communication encapsulations for ORCHIDEE.

!-
!- $Header: $
!-

MODULE mod_orchidee_mpi_transfert
  !-
  USE defprec
  USE mod_orchidee_para_var
  USE timer
  !-
  IMPLICIT NONE
  !-
! Redefinition of MPI function if not second underscore in MPI library.
! One must use -DMPI_SECOND__ in precompilation to activate those definitions.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/src_parallel.h,v 1.3 2009/06/24 10:15:00 ssipsl Exp $
!-
















































































































































































































  !-

  INTERFACE bcast_mpi
     MODULE PROCEDURE bcast_mpi_c, bcast_mpi_c1,                           &
          bcast_mpi_i,bcast_mpi_i1,bcast_mpi_i2,bcast_mpi_i3,bcast_mpi_i4, &
          bcast_mpi_r,bcast_mpi_r1,bcast_mpi_r2,bcast_mpi_r3,bcast_mpi_r4, &
          bcast_mpi_l,bcast_mpi_l1,bcast_mpi_l2,bcast_mpi_l3,bcast_mpi_l4
  END INTERFACE

  INTERFACE scatter_mpi
     MODULE PROCEDURE scatter_mpi_i,scatter_mpi_i1,scatter_mpi_i2,scatter_mpi_i3, &
          scatter_mpi_r,scatter_mpi_r1,scatter_mpi_r2,scatter_mpi_r3, &
          scatter_mpi_l,scatter_mpi_l1,scatter_mpi_l2,scatter_mpi_l3
  END INTERFACE

  INTERFACE gather_mpi_s
     MODULE PROCEDURE gather_mpi_is, &
          gather_mpi_rs, &
          gather_mpi_ls
  END INTERFACE

  INTERFACE gather_mpi
     MODULE PROCEDURE gather_mpi_i,gather_mpi_i1,gather_mpi_i2,gather_mpi_i3, &
          gather_mpi_r,gather_mpi_r1,gather_mpi_r2,gather_mpi_r3, &
          gather_mpi_l,gather_mpi_l1,gather_mpi_l2,gather_mpi_l3  
  END INTERFACE

  INTERFACE scatter2D_mpi
     MODULE PROCEDURE scatter2D_mpi_i,scatter2D_mpi_i1,scatter2D_mpi_i2,scatter2D_mpi_i3, &
          scatter2D_mpi_r0,scatter2D_mpi_r,scatter2D_mpi_r1,scatter2D_mpi_r2,scatter2D_mpi_r3, &
          scatter2D_mpi_l,scatter2D_mpi_l1,scatter2D_mpi_l2,scatter2D_mpi_l3
  END INTERFACE

  INTERFACE gather2D_mpi
     MODULE PROCEDURE gather2D_mpi_i,gather2D_mpi_i1,gather2D_mpi_i2,gather2D_mpi_i3, &
          gather2D_mpi_r0,gather2D_mpi_r,gather2D_mpi_r1,gather2D_mpi_r2,gather2D_mpi_r3, &
          gather2D_mpi_l,gather2D_mpi_l1,gather2D_mpi_l2,gather2D_mpi_l3
  END INTERFACE

  INTERFACE reduce_sum_mpi
     MODULE PROCEDURE reduce_sum_mpi_i,reduce_sum_mpi_i1,reduce_sum_mpi_i2,reduce_sum_mpi_i3,reduce_sum_mpi_i4, &
          reduce_sum_mpi_r,reduce_sum_mpi_r1,reduce_sum_mpi_r2,reduce_sum_mpi_r3,reduce_sum_mpi_r4
  END INTERFACE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Broadcast --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! -- Les chaine de charactère -- !!

  SUBROUTINE bcast_mpi_c(var)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: Var
    CHARACTER(LEN=len(Var)),DIMENSION(1) :: Var1

    IF (is_mpi_root) &
         Var1(1)=Var
    CALL orch_bcast_mpi_cgen(Var1,1)
    Var=Var1(1)
  END SUBROUTINE bcast_mpi_c

  SUBROUTINE bcast_mpi_c1(var)
  IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: Var(:)
   
    CALL orch_bcast_mpi_cgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_c1


  !! -- Les entiers -- !!

  SUBROUTINE bcast_mpi_i(var)
    IMPLICIT NONE
    INTEGER(i_std),INTENT(INOUT) :: Var
    INTEGER(i_std),DIMENSION(1) :: Var1

    IF (is_mpi_root) &
         Var1(1)=Var
    CALL orch_bcast_mpi_igen(Var1,1)
    Var=Var1(1)
  END SUBROUTINE bcast_mpi_i

  SUBROUTINE bcast_mpi_i1(var)
    IMPLICIT NONE
    INTEGER(i_std),INTENT(INOUT) :: Var(:)

    CALL orch_bcast_mpi_igen(Var,size(Var))
  END SUBROUTINE bcast_mpi_i1

  SUBROUTINE bcast_mpi_i2(var)
    IMPLICIT NONE
    INTEGER(i_std),INTENT(INOUT) :: Var(:,:)

    CALL orch_bcast_mpi_igen(Var,size(Var))
  END SUBROUTINE bcast_mpi_i2

  SUBROUTINE bcast_mpi_i3(var)
    IMPLICIT NONE
    INTEGER(i_std),INTENT(INOUT) :: Var(:,:,:)

    CALL orch_bcast_mpi_igen(Var,size(Var))
  END SUBROUTINE bcast_mpi_i3

  SUBROUTINE bcast_mpi_i4(var)
    IMPLICIT NONE
    INTEGER(i_std),INTENT(INOUT) :: Var(:,:,:,:)

    CALL orch_bcast_mpi_igen(Var,size(Var))
  END SUBROUTINE bcast_mpi_i4


  !! -- Les reels -- !!

  SUBROUTINE bcast_mpi_r(var)
    IMPLICIT NONE
    REAL(r_std),INTENT(INOUT) :: Var
    REAL(r_std),DIMENSION(1) :: Var1

    IF (is_mpi_root) &
         Var1(1)=Var
    CALL orch_bcast_mpi_rgen(Var1,1)
    Var=Var1(1)
  END SUBROUTINE bcast_mpi_r

  SUBROUTINE bcast_mpi_r1(var)
    IMPLICIT NONE
    REAL(r_std),INTENT(INOUT) :: Var(:)

    CALL orch_bcast_mpi_rgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_r1

  SUBROUTINE bcast_mpi_r2(var)
    IMPLICIT NONE
    REAL(r_std),INTENT(INOUT) :: Var(:,:)

    CALL orch_bcast_mpi_rgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_r2

  SUBROUTINE bcast_mpi_r3(var)
    IMPLICIT NONE
    REAL(r_std),INTENT(INOUT) :: Var(:,:,:)

    CALL orch_bcast_mpi_rgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_r3

  SUBROUTINE bcast_mpi_r4(var)
    IMPLICIT NONE
    REAL(r_std),INTENT(INOUT) :: Var(:,:,:,:)

    CALL orch_bcast_mpi_rgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_r4

  !! -- Les booleans -- !!

  SUBROUTINE bcast_mpi_l(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var
    LOGICAL,DIMENSION(1) :: Var1
    IF (is_mpi_root) &
         Var1(1)=Var
    CALL orch_bcast_mpi_lgen(Var1,1)
    Var=Var1(1)
  END SUBROUTINE bcast_mpi_l

  SUBROUTINE bcast_mpi_l1(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:)

    CALL orch_bcast_mpi_lgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_l1

  SUBROUTINE bcast_mpi_l2(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:)

    CALL orch_bcast_mpi_lgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_l2

  SUBROUTINE bcast_mpi_l3(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:,:)

    CALL orch_bcast_mpi_lgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_l3

  SUBROUTINE bcast_mpi_l4(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:,:,:)

    CALL orch_bcast_mpi_lgen(Var,size(Var))
  END SUBROUTINE bcast_mpi_l4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Scatter   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE scatter_mpi_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(nbp_glo) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(nbp_mpi) :: VarOut


    INTEGER(i_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_igen(VarIn,Varout,SIZE(VarIn,1),1)
    ELSE
       CALL orch_scatter_mpi_igen(dummy,Varout,1,1)
    ENDIF

  END SUBROUTINE scatter_mpi_i

  SUBROUTINE scatter_mpi_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarOut,2)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_igen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2))
    ELSE
       CALL orch_scatter_mpi_igen(dummy,Varout,1,SIZE(VarOut,2))
    ENDIF

  END SUBROUTINE scatter_mpi_i1

  SUBROUTINE scatter_mpi_i2(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarOut,2)*SIZE(VarOut,3)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_igen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2)*SIZE(VarOut,3))
    ELSE
       CALL orch_scatter_mpi_igen(dummy,Varout,1,SIZE(VarOut,2)*SIZE(VarOut,3))
    ENDIF
  END SUBROUTINE scatter_mpi_i2

  SUBROUTINE scatter_mpi_i3(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_igen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4))
    ELSE
       CALL orch_scatter_mpi_igen(dummy,Varout,1,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4))
    ENDIF

  END SUBROUTINE scatter_mpi_i3


  SUBROUTINE scatter_mpi_r(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:) :: VarOut


    REAL(r_std),DIMENSION(1) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_rgen(VarIn,Varout,SIZE(VarIn,1),1)
    ELSE
       CALL orch_scatter_mpi_rgen(dummy,Varout,1,1)
    ENDIF
  END SUBROUTINE scatter_mpi_r

  SUBROUTINE scatter_mpi_r1(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarOut,2)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_rgen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2))
    ELSE
       CALL orch_scatter_mpi_rgen(dummy,Varout,1,SIZE(VarOut,2))
    ENDIF

  END SUBROUTINE scatter_mpi_r1

  SUBROUTINE scatter_mpi_r2(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarOut,2)*SIZE(VarOut,3)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_rgen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2)*SIZE(VarOut,3))
    ELSE
       CALL orch_scatter_mpi_rgen(dummy,Varout,1,SIZE(VarOut,2)*SIZE(VarOut,3))
    ENDIF

  END SUBROUTINE scatter_mpi_r2

  SUBROUTINE scatter_mpi_r3(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_rgen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4))
    ELSE
       CALL orch_scatter_mpi_rgen(dummy,Varout,1,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4))
    ENDIF
  END SUBROUTINE scatter_mpi_r3


  SUBROUTINE scatter_mpi_l(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    LOGICAL,DIMENSION(1) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_lgen(VarIn,Varout,SIZE(VarIn,1),1)
    ELSE
       CALL orch_scatter_mpi_lgen(dummy,Varout,1,1)
    ENDIF
  END SUBROUTINE scatter_mpi_l

  SUBROUTINE scatter_mpi_l1(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarOut,2)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_lgen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2))
    ELSE
       CALL orch_scatter_mpi_lgen(dummy,Varout,1,SIZE(VarOut,2))
    ENDIF
  END SUBROUTINE scatter_mpi_l1

  SUBROUTINE scatter_mpi_l2(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarOut,2)*SIZE(VarOut,3)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_lgen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2)*SIZE(VarOut,3))
    ELSE
       CALL orch_scatter_mpi_lgen(dummy,Varout,1,SIZE(VarOut,2)*SIZE(VarOut,3))
    ENDIF
  END SUBROUTINE scatter_mpi_l2

  SUBROUTINE scatter_mpi_l3(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4)) :: dummy

    IF (is_mpi_root) THEN
       CALL orch_scatter_mpi_lgen(VarIn,Varout,SIZE(VarIn,1),SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4))
    ELSE
       CALL orch_scatter_mpi_lgen(dummy,Varout,1,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4))
    ENDIF
  END SUBROUTINE scatter_mpi_l3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Gather   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE gather_mpi_is(VarIn, VarOut)

    IMPLICIT NONE

    INCLUDE 'mpif.h'

    INTEGER(i_std),INTENT(IN) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:) :: VarOut

    INTEGER(i_std) :: nb,i,index_para,rank
    INTEGER(i_std) :: ierr
    LOGICAL :: flag=.FALSE.
    LOGICAL, PARAMETER :: check=.FALSE.


    IF (timer_state(timer_mpi)==running) THEN
      flag=.TRUE.
    ELSE
      flag=.FALSE.
    ENDIF
    
    IF (flag) CALL suspend_timer(timer_mpi)

    IF (check) &
         WRITE(numout,*) "gather_mpi_is VarIn=",VarIn    

    CALL MPI_GATHER(VarIn,1,MPI_INT_ORCH,VarOut,1,MPI_INT_ORCH,mpi_rank_root,MPI_COMM_ORCH,ierr)

    IF (check) &
         WRITE(numout,*) "gather_mpi_is VarOut=",VarOut
    IF (flag) CALL resume_timer(timer_mpi)
  END SUBROUTINE gather_mpi_is

  SUBROUTINE gather_mpi_rs(VarIn, VarOut)

    IMPLICIT NONE

    INCLUDE 'mpif.h'

    REAL(r_std),INTENT(IN) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:) :: VarOut

    INTEGER(i_std) :: nb,i,index_para,rank
    INTEGER(i_std) :: ierr
    LOGICAL :: flag=.FALSE.
    LOGICAL, PARAMETER :: check=.FALSE.


    IF (timer_state(timer_mpi)==running) THEN
      flag=.TRUE.
    ELSE
      flag=.FALSE.
    ENDIF
    
    IF (flag) CALL suspend_timer(timer_mpi)

    IF (check) &
         WRITE(numout,*) "gather_mpi_rs VarIn=",VarIn    

    CALL MPI_GATHER(VarIn,1,MPI_REAL_ORCH,VarOut,1,MPI_REAL_ORCH,mpi_rank_root,MPI_COMM_ORCH,ierr)

    IF (check) &
         WRITE(numout,*) "gather_mpi_rs VarOut=",VarOut

    IF (flag) CALL resume_timer(timer_mpi)
  END SUBROUTINE gather_mpi_rs

  SUBROUTINE gather_mpi_ls(VarIn, VarOut)

    IMPLICIT NONE

    INCLUDE 'mpif.h'

    LOGICAL,INTENT(IN) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    INTEGER(i_std) :: nb,i,index_para,rank
    INTEGER(i_std) :: ierr
    LOGICAL :: flag=.FALSE.
    LOGICAL, PARAMETER :: check=.FALSE.


    IF (timer_state(timer_mpi)==running) THEN
      flag=.TRUE.
    ELSE
      flag=.FALSE.
    ENDIF
    
    IF (flag) CALL suspend_timer(timer_mpi)

    IF (check) &
         WRITE(numout,*) "gather_mpi_ls VarIn=",VarIn    

    CALL MPI_GATHER(VarIn,1,MPI_LOGICAL,VarOut,1,MPI_LOGICAL,mpi_rank_root,MPI_COMM_ORCH,ierr)

    IF (check) &
         WRITE(numout,*) "gather_mpi_ls VarOut=",VarOut
    IF (flag) CALL resume_timer(timer_mpi)
  END SUBROUTINE gather_mpi_ls

!!!!! --> Les entiers

  SUBROUTINE gather_mpi_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:) :: VarOut

    INTEGER(i_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_igen(VarIn,VarOut,SIZE(VarOut,1),1)
    ELSE
       CALL orch_gather_mpi_igen(VarIn,dummy,1,1)
    ENDIF

  END SUBROUTINE gather_mpi_i

!!!!!

  SUBROUTINE gather_mpi_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarIn,2)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_igen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2))
    ELSE
       CALL orch_gather_mpi_igen(VarIn,dummy,1,SIZE(VarIn,2))
    ENDIF

  END SUBROUTINE gather_mpi_i1

!!!!!

  SUBROUTINE gather_mpi_i2(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarIn,2)*SIZE(VarIn,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_igen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2)*SIZE(VarIn,3))
    ELSE
       CALL orch_gather_mpi_igen(VarIn,dummy,1,SIZE(VarIn,2)*SIZE(VarIn,3))
    ENDIF

  END SUBROUTINE gather_mpi_i2

!!!!!

  SUBROUTINE gather_mpi_i3(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_igen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4))
    ELSE
       CALL orch_gather_mpi_igen(VarIn,dummy,1,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4))
    ENDIF

  END SUBROUTINE gather_mpi_i3

!!!!! --> Les reels

  SUBROUTINE gather_mpi_r(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1),1)
    ELSE
       CALL orch_gather_mpi_rgen(VarIn,dummy,1,1)
    ENDIF

  END SUBROUTINE gather_mpi_r

!!!!!

  SUBROUTINE gather_mpi_r1(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarIn,2)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2))
    ELSE
       CALL orch_gather_mpi_rgen(VarIn,dummy,1,SIZE(VarIn,2))
    ENDIF

  END SUBROUTINE gather_mpi_r1

!!!!!

  SUBROUTINE gather_mpi_r2(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarIn,2)*SIZE(VarIn,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2)*SIZE(VarIn,3))
    ELSE
       CALL orch_gather_mpi_rgen(VarIn,dummy,1,SIZE(VarIn,2)*SIZE(VarIn,3))
    ENDIF

  END SUBROUTINE gather_mpi_r2

!!!!!

  SUBROUTINE gather_mpi_r3(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4))
    ELSE
       CALL orch_gather_mpi_rgen(VarIn,dummy,1,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4))
    ENDIF

  END SUBROUTINE gather_mpi_r3

!!!!! --> Les booleen

  SUBROUTINE gather_mpi_l(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    LOGICAL,DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1),1)
    ELSE
       CALL orch_gather_mpi_lgen(VarIn,dummy,1,1)      
    ENDIF

  END SUBROUTINE gather_mpi_l

!!!!!

  SUBROUTINE gather_mpi_l1(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarIn,2)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2))
    ELSE
       CALL orch_gather_mpi_lgen(VarIn,dummy,1,SIZE(VarIn,2))
    ENDIF

  END SUBROUTINE gather_mpi_l1

!!!!!

  SUBROUTINE gather_mpi_l2(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarIn,2)*SIZE(VarIn,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2)*SIZE(VarIn,3))
    ELSE
       CALL orch_gather_mpi_lgen(VarIn,dummy,1,SIZE(VarIn,2)*SIZE(VarIn,3))
    ENDIF

  END SUBROUTINE gather_mpi_l2

!!!!!

  SUBROUTINE gather_mpi_l3(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1),SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4))
    ELSE
       CALL orch_gather_mpi_lgen(VarIn,dummy,1,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4))
    ENDIF

  END SUBROUTINE gather_mpi_l3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Scatter2D   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE scatter2D_mpi_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_igen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),1)
    ELSE
       CALL orch_scatter2D_mpi_igen(dummy,VarOut,1,1)
    ENDIF


  END SUBROUTINE scatter2D_mpi_i

  SUBROUTINE scatter2D_mpi_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarOut,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_igen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3))
    ELSE
       CALL orch_scatter2D_mpi_igen(dummy,VarOut,1,SIZE(VarOut,3))
    ENDIF

  END SUBROUTINE scatter2D_mpi_i1

  SUBROUTINE scatter2D_mpi_i2(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarOut,3)*SIZE(VarOut,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_igen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3)*SIZE(VarOut,4))
    ELSE
       CALL orch_scatter2D_mpi_igen(dummy,VarOut,1,SIZE(VarOut,3)*SIZE(VarOut,4))
    ENDIF


  END SUBROUTINE scatter2D_mpi_i2

  SUBROUTINE scatter2D_mpi_i3(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:,:)  :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_igen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5))
    ELSE
       CALL orch_scatter2D_mpi_igen(dummy,VarOut,1,SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5))
    ENDIF

  END SUBROUTINE scatter2D_mpi_i3


  SUBROUTINE scatter2D_mpi_r0(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_rgen(VarIn,VarOut,SIZE(VarIn,1),1)
    ELSE
       CALL orch_scatter2D_mpi_rgen(dummy,VarOut,1,1)      
    ENDIF


  END SUBROUTINE scatter2D_mpi_r0

  SUBROUTINE scatter2D_mpi_r(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL(r_std),INTENT(INOUT),DIMENSION(:,:) :: VarOut

    REAL(r_std),DIMENSION(1,1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_rgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),1)
    ELSE
       CALL orch_scatter2D_mpi_rgen(dummy,VarOut,1,1)
    ENDIF

  END SUBROUTINE scatter2D_mpi_r

  SUBROUTINE scatter2D_mpi_r1(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarOut,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_rgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3))
    ELSE
       CALL orch_scatter2D_mpi_rgen(dummy,VarOut,1,SIZE(VarOut,3))
    ENDIF

  END SUBROUTINE scatter2D_mpi_r1

  SUBROUTINE scatter2D_mpi_r2(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL(r_std),INTENT(INOUT),DIMENSION(:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarOut,3)*SIZE(VarOut,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_rgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3)*SIZE(VarOut,4))
    ELSE
       CALL orch_scatter2D_mpi_rgen(dummy,VarOut,1,SIZE(VarOut,3)*SIZE(VarOut,4))
    ENDIF

  END SUBROUTINE scatter2D_mpi_r2

  SUBROUTINE scatter2D_mpi_r3(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:,:)  :: VarIn
    REAL(r_std),INTENT(INOUT),DIMENSION(:,:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_rgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5))
    ELSE
       CALL orch_scatter2D_mpi_rgen(dummy,VarOut,1,SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5))
    ENDIF

  END SUBROUTINE scatter2D_mpi_r3


  SUBROUTINE scatter2D_mpi_l(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    LOGICAL,DIMENSION(1,1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_lgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),1)
    ELSE
       CALL orch_scatter2D_mpi_lgen(dummy,VarOut,1,1)
    ENDIF

  END SUBROUTINE scatter2D_mpi_l

  SUBROUTINE scatter2D_mpi_l1(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(INOUT),DIMENSION(:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarOut,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_lgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3))
    ELSE
       CALL orch_scatter2D_mpi_lgen(dummy,VarOut,1,SIZE(VarOut,3))
    ENDIF

  END SUBROUTINE scatter2D_mpi_l1

  SUBROUTINE scatter2D_mpi_l2(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(INOUT),DIMENSION(:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarOut,3)*SIZE(VarOut,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_lgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3)*SIZE(VarOut,4))
    ELSE
       CALL orch_scatter2D_mpi_lgen(dummy,VarOut,1,SIZE(VarOut,3)*SIZE(VarOut,4))
    ENDIF

  END SUBROUTINE scatter2D_mpi_l2

  SUBROUTINE scatter2D_mpi_l3(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:,:)  :: VarIn
    LOGICAL,INTENT(INOUT),DIMENSION(:,:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_scatter2D_mpi_lgen(VarIn,VarOut,SIZE(VarIn,1)*SIZE(VarIn,2),SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5))
    ELSE
       CALL orch_scatter2D_mpi_lgen(dummy,VarOut,1,SIZE(VarOut,3)*SIZE(VarOut,4)*SIZE(VarOut,5))
    ENDIF

  END SUBROUTINE scatter2D_mpi_l3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Gather2D   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE gather2D_mpi_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_igen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),1)
    ELSE
       CALL orch_gather2D_mpi_igen(VarIn,dummy,1,1)
    ENDIF

  END SUBROUTINE gather2D_mpi_i

  SUBROUTINE gather2D_mpi_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarIn,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_igen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3))
    ELSE
       CALL orch_gather2D_mpi_igen(VarIn,dummy,1,SIZE(VarIn,3))
    ENDIF

  END SUBROUTINE gather2D_mpi_i1

  SUBROUTINE gather2D_mpi_i2(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarIn,3)*SIZE(VarIn,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_igen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3)*SIZE(VarIn,4))
    ELSE
       CALL orch_gather2D_mpi_igen(VarIn,dummy,1,SIZE(VarIn,3)*SIZE(VarIn,4))
    ENDIF

  END SUBROUTINE gather2D_mpi_i2

  SUBROUTINE gather2D_mpi_i3(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:,:) :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1,SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_igen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5))
    ELSE
       CALL orch_gather2D_mpi_igen(VarIn,dummy,1,SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5))
    ENDIF

  END SUBROUTINE gather2D_mpi_i3


  SUBROUTINE gather2D_mpi_r0(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1),1)
    ELSE
       CALL orch_gather2D_mpi_rgen(VarIn,dummy,1,1)
    ENDIF

  END SUBROUTINE gather2D_mpi_r0

  SUBROUTINE gather2D_mpi_r(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    REAL(r_std),DIMENSION(1,1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),1)
    ELSE
       CALL orch_gather2D_mpi_rgen(VarIn,dummy,1,1)
    ENDIF

  END SUBROUTINE gather2D_mpi_r

  SUBROUTINE gather2D_mpi_r1(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarIn,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3))
    ELSE
       CALL orch_gather2D_mpi_rgen(VarIn,dummy,1,SIZE(VarIn,3))
    ENDIF

  END SUBROUTINE gather2D_mpi_r1

  SUBROUTINE gather2D_mpi_r2(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarIn,3)*SIZE(VarIn,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3)*SIZE(VarIn,4))
    ELSE
       CALL orch_gather2D_mpi_rgen(VarIn,dummy,1,SIZE(VarIn,3)*SIZE(VarIn,4))
    ENDIF

  END SUBROUTINE gather2D_mpi_r2

  SUBROUTINE gather2D_mpi_r3(VarIn, VarOut)

    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:,:) :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1,SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_rgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5))
    ELSE
       CALL orch_gather2D_mpi_rgen(VarIn,dummy,1,SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5))
    ENDIF

  END SUBROUTINE gather2D_mpi_r3


  SUBROUTINE gather2D_mpi_l(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    LOGICAL,DIMENSION(1,1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),1)
    ELSE
       CALL orch_gather2D_mpi_lgen(VarIn,dummy,1,1)
    ENDIF

  END SUBROUTINE gather2D_mpi_l

  SUBROUTINE gather2D_mpi_l1(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarIn,3)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3))
    ELSE
       CALL orch_gather2D_mpi_lgen(VarIn,dummy,1,SIZE(VarIn,3))
    ENDIF

  END SUBROUTINE gather2D_mpi_l1

  SUBROUTINE gather2D_mpi_l2(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarIn,3)*SIZE(VarIn,4)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3)*SIZE(VarIn,4))
    ELSE
       CALL orch_gather2D_mpi_lgen(VarIn,dummy,1,SIZE(VarIn,3)*SIZE(VarIn,4))
    ENDIF

  END SUBROUTINE gather2D_mpi_l2

  SUBROUTINE gather2D_mpi_l3(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(1,SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5)) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_gather2D_mpi_lgen(VarIn,VarOut,SIZE(VarOut,1)*SIZE(VarOut,2),SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5))
    ELSE
       CALL orch_gather2D_mpi_lgen(VarIn,dummy,1,SIZE(VarIn,3)*SIZE(VarIn,4)*SIZE(VarIn,5))
    ENDIF

  END SUBROUTINE gather2D_mpi_l3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des reduce_sum   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE reduce_sum_mpi_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN)  :: VarIn
    INTEGER(i_std),INTENT(OUT) :: VarOut

    INTEGER(i_std),DIMENSION(1) :: Var1
    INTEGER(i_std),DIMENSION(1) :: Var2
    INTEGER(i_std),DIMENSION(1) :: dummy

    Var1(1)=VarIn
    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_igen(Var1,Var2,1)
       VarOut=Var2(1)
    ELSE
       CALL orch_reduce_sum_mpi_igen(Var1,dummy,1)
       VarOut=VarIn
    ENDIF
  END SUBROUTINE reduce_sum_mpi_i

  SUBROUTINE reduce_sum_mpi_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:)  :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:) :: VarOut

    INTEGER(i_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_igen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_igen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_i1

  SUBROUTINE reduce_sum_mpi_i2(VarIn, VarOut)
    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:)  :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_igen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_igen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_i2

  SUBROUTINE reduce_sum_mpi_i3(VarIn, VarOut)
    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:)  :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_igen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_igen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_i3

  SUBROUTINE reduce_sum_mpi_i4(VarIn, VarOut)
    IMPLICIT NONE

    INTEGER(i_std),INTENT(IN),DIMENSION(:,:,:,:)  :: VarIn
    INTEGER(i_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    INTEGER(i_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_igen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_igen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_i4


  SUBROUTINE reduce_sum_mpi_r(VarIn, VarOut)
    IMPLICIT NONE

    REAL(r_std),INTENT(IN)  :: VarIn
    REAL(r_std),INTENT(OUT) :: VarOut

    REAL(r_std),DIMENSION(1) :: Var1
    REAL(r_std),DIMENSION(1) :: Var2
    REAL(r_std),DIMENSION(1) :: dummy


    Var1(1)=VarIn
    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_rgen(Var1,Var2,1)
       VarOut=Var2(1)
    ELSE
       CALL orch_reduce_sum_mpi_rgen(Var1,dummy,1)
       VarOut=VarIn
    ENDIF

  END SUBROUTINE reduce_sum_mpi_r

  SUBROUTINE reduce_sum_mpi_r1(VarIn, VarOut)
    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:)  :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_rgen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_rgen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_r1

  SUBROUTINE reduce_sum_mpi_r2(VarIn, VarOut)
    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:)  :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_rgen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_rgen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_r2

  SUBROUTINE reduce_sum_mpi_r3(VarIn, VarOut)
    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:)  :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_rgen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_rgen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_r3

  SUBROUTINE reduce_sum_mpi_r4(VarIn, VarOut)
    IMPLICIT NONE

    REAL(r_std),INTENT(IN),DIMENSION(:,:,:,:)  :: VarIn
    REAL(r_std),INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    REAL(r_std),DIMENSION(1) :: dummy


    IF (is_mpi_root) THEN
       CALL orch_reduce_sum_mpi_rgen(VarIn,Varout,SIZE(VarIn))
    ELSE
       CALL orch_reduce_sum_mpi_rgen(VarIn,dummy,SIZE(VarIn))      
    ENDIF

  END SUBROUTINE reduce_sum_mpi_r4


END MODULE mod_orchidee_mpi_transfert


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DEFINITION DES FONCTIONS DE TRANSFERT GENERIQUES !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE orch_bcast_mpi_cgen(var,nb)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  CHARACTER(LEN=*),DIMENSION(nb),INTENT(INOUT) :: Var
  INTEGER(i_std),INTENT(IN) :: nb

  INCLUDE 'mpif.h'

  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.
  
  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF
  
  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_cgen before bcast Var",Var    
  IF (flag) CALL suspend_timer(timer_mpi)
  CALL MPI_BCAST(Var,nb*LEN(Var(1)),MPI_CHARACTER,mpi_rank_root,MPI_COMM_ORCH,ierr)
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_cgen after bcast Var",Var

END SUBROUTINE orch_bcast_mpi_cgen

SUBROUTINE orch_bcast_mpi_igen(var,nb)
  USE mod_orchidee_para_var
  USE timer
  IMPLICIT NONE

  INTEGER(i_std),DIMENSION(nb),INTENT(INOUT) :: Var
  INTEGER(i_std),INTENT(IN) :: nb

  INCLUDE 'mpif.h'

  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_igen before bcast Var",Var
  CALL MPI_BCAST(Var,nb,MPI_INT_ORCH,mpi_rank_root,MPI_COMM_ORCH,ierr)
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_igen after bcast Var",Var    

END SUBROUTINE orch_bcast_mpi_igen

SUBROUTINE orch_bcast_mpi_rgen(var,nb)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  REAL(r_std),DIMENSION(nb),INTENT(INOUT) :: Var
  INTEGER(i_std),INTENT(IN) :: nb

  INCLUDE 'mpif.h'

  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_rgen before bcast Var",Var
  IF (flag) CALL suspend_timer(timer_mpi)
  CALL MPI_BCAST(Var,nb,MPI_REAL_ORCH,mpi_rank_root,MPI_COMM_ORCH,ierr)
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_rgen after bcast Var",Var

END SUBROUTINE orch_bcast_mpi_rgen

SUBROUTINE orch_bcast_mpi_lgen(var,nb)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  LOGICAL,DIMENSION(nb),INTENT(INOUT) :: Var
  INTEGER(i_std),INTENT(IN) :: nb

  INCLUDE 'mpif.h'

  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.


  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_lgen before bcast Var",Var
  IF (flag) CALL suspend_timer(timer_mpi)
  CALL MPI_BCAST(Var,nb,MPI_LOGICAL,mpi_rank_root,MPI_COMM_ORCH,ierr)
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_bcast_mpi_lgen after bcast Var",Var

END SUBROUTINE orch_bcast_mpi_lgen


SUBROUTINE orch_scatter_mpi_igen(VarIn, VarOut, nbp, dimsize)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: nbp
  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN),DIMENSION(nbp,dimsize) :: VarIn
  INTEGER(i_std),INTENT(OUT),DIMENSION(nbp_mpi,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: counts
  INTEGER(i_std),DIMENSION(dimsize*nbp_glo) :: VarTmp

  INTEGER(i_std) :: nb,i,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_Para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        displs(rank)=Index_Para-1
        counts(rank)=nb*dimsize
        DO i=1,dimsize
           VarTmp(Index_para:Index_para+nb-1)=VarIn(nbp_mpi_para_begin(rank):nbp_mpi_para_end(rank),i)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
     IF (check) THEN
        WRITE(numout,*) "orch_scatter_mpi_igen VarIn",VarIn
        WRITE(numout,*) "orch_scatter_mpi_igen VarTmp",VarTmp
     ENDIF
  ENDIF

  CALL MPI_SCATTERV(VarTmp,counts,displs,MPI_INT_ORCH,VarOut,nbp_mpi*dimsize,   &
       MPI_INT_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_scatter_mpi_igen VarOut",VarOut

END SUBROUTINE orch_scatter_mpi_igen

SUBROUTINE orch_scatter_mpi_rgen(VarIn, VarOut, nbp, dimsize)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp
  REAL(r_std),INTENT(IN),DIMENSION(nbp,dimsize) :: VarIn
  REAL(r_std),INTENT(OUT),DIMENSION(nbp_mpi,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: counts
  REAL(r_std),DIMENSION(dimsize*nbp_glo) :: VarTmp

  INTEGER(i_std) :: nb,i,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
        DO i=1,dimsize
           VarTmp(Index_para:Index_para+nb-1)=VarIn(nbp_mpi_para_begin(rank):nbp_mpi_para_end(rank),i)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
     IF (check) THEN
        WRITE(numout,*) "orch_scatter_mpi_rgen VarIn",VarIn
        WRITE(numout,*) "orch_scatter_mpi_rgen VarTmp",VarTmp
     ENDIF
  ENDIF

  CALL MPI_SCATTERV(VarTmp,counts,displs,MPI_REAL_ORCH,VarOut,nbp_mpi*dimsize,   &
       MPI_REAL_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_scatter_mpi_rgen VarOut",VarOut

END SUBROUTINE orch_scatter_mpi_rgen

SUBROUTINE orch_scatter_mpi_lgen(VarIn, VarOut, nbp, dimsize)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp
  LOGICAL,INTENT(IN),DIMENSION(nbp,dimsize) :: VarIn
  LOGICAL,INTENT(OUT),DIMENSION(nbp_mpi,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: counts
  LOGICAL,DIMENSION(dimsize*nbp_glo) :: VarTmp

  INTEGER(i_std) :: nb,i,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
        DO i=1,dimsize
           VarTmp(Index_para:Index_para+nb-1)=VarIn(nbp_mpi_para_begin(rank):nbp_mpi_para_end(rank),i)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
     IF (check) THEN
        WRITE(numout,*) "orch_scatter_mpi_lgen VarIn",VarIn
        WRITE(numout,*) "orch_scatter_mpi_lgen VarTmp",VarTmp
     ENDIF
  ENDIF

  CALL MPI_SCATTERV(VarTmp,counts,displs,MPI_LOGICAL,VarOut,nbp_mpi*dimsize,   &
       MPI_LOGICAL,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_scatter_mpi_lgen VarOut",VarOut

END SUBROUTINE orch_scatter_mpi_lgen

SUBROUTINE orch_gather_mpi_igen(VarIn, VarOut, nbp, dimsize)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp
  INTEGER(i_std),INTENT(IN),DIMENSION(nbp_mpi,dimsize) :: VarIn
  INTEGER(i_std),INTENT(OUT),DIMENSION(nbp,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: counts
  INTEGER(i_std),DIMENSION(dimsize*nbp_glo) :: VarTmp

  INTEGER(i_std) :: nb,i,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
	Index_para=Index_para+nb*dimsize
     ENDDO
     IF (check) &
          WRITE(numout,*) "orch_gather_mpi_igen nbp_mpi_para, displs, counts,Index_Para-1",nbp_mpi_para, displs, counts,Index_Para-1

  ENDIF

  CALL MPI_GATHERV(VarIn,nbp_mpi*dimsize,MPI_INT_ORCH,VarTmp,counts,displs,   &
       MPI_INT_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)


  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        DO i=1,dimsize
           VarOut(nbp_mpi_para_begin(rank):nbp_mpi_para_end(rank),i)=VarTmp(Index_para:Index_para+nb-1)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
  ENDIF
  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_igen VarOut=",VarOut
  IF (flag) CALL resume_timer(timer_mpi)

END SUBROUTINE orch_gather_mpi_igen

SUBROUTINE orch_gather_mpi_rgen(VarIn, VarOut, nbp, dimsize)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp
  REAL(r_std),INTENT(IN),DIMENSION(nbp_mpi,dimsize) :: VarIn
  REAL(r_std),INTENT(OUT),DIMENSION(nbp,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: counts
  REAL(r_std),DIMENSION(dimsize*nbp_glo) :: VarTmp

  INTEGER(i_std) :: nb,i,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
	Index_para=Index_para+nb*dimsize
     ENDDO
     IF (check) &
          WRITE(numout,*) "orch_gather_mpi_rgen nbp_mpi_para, displs, counts,Index_Para-1",nbp_mpi_para, displs, counts,Index_Para-1

  ENDIF

  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_rgen VarIn=",VarIn    
  CALL MPI_GATHERV(VarIn,nbp_mpi*dimsize,MPI_REAL_ORCH,VarTmp,counts,displs,   &
       MPI_REAL_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_rgen dimsize,VarTmp=",dimsize,VarTmp

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        DO i=1,dimsize
           VarOut(nbp_mpi_para_begin(rank):nbp_mpi_para_end(rank),i)=VarTmp(Index_para:Index_para+nb-1)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
  ENDIF
  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_rgen VarOut=",VarOut
  IF (flag) CALL resume_timer(timer_mpi)

END SUBROUTINE orch_gather_mpi_rgen

SUBROUTINE orch_gather_mpi_lgen(VarIn, VarOut, nbp, dimsize)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp
  LOGICAL,INTENT(IN),DIMENSION(nbp_mpi,dimsize) :: VarIn
  LOGICAL,INTENT(OUT),DIMENSION(nbp,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1) :: counts
  LOGICAL,DIMENSION(dimsize*nbp_glo) :: VarTmp

  INTEGER(i_std) :: nb,i,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.


  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
	Index_para=Index_para+nb*dimsize
     ENDDO
     IF (check) &
          WRITE(numout,*) "orch_gather_mpi_lgen nbp_mpi_para, displs, counts,Index_Para-1",nbp_mpi_para, displs, counts,Index_Para-1
  ENDIF

  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_lgen VarIn=",VarIn    
  CALL MPI_GATHERV(VarIn,nbp_mpi*dimsize,MPI_LOGICAL,VarTmp,counts,displs,   &
       MPI_LOGICAL,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_lgen dimsize,VarTmp=",dimsize,VarTmp

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=nbp_mpi_para(rank)
        DO i=1,dimsize
           VarOut(nbp_mpi_para_begin(rank):nbp_mpi_para_end(rank),i)=VarTmp(Index_para:Index_para+nb-1)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
  ENDIF
  IF (check) &
       WRITE(numout,*) "orch_gather_mpi_lgen VarOut=",VarOut
  IF (flag) CALL resume_timer(timer_mpi)

END SUBROUTINE orch_gather_mpi_lgen


SUBROUTINE orch_scatter2D_mpi_igen(VarIn, VarOut, nbp2D, dimsize)
  USE mod_orchidee_para_var, iim=>iim_g,jjm=>jjm_g
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp2D
  INTEGER(i_std),INTENT(IN),DIMENSION(nbp2D,dimsize) :: VarIn
  INTEGER(i_std),INTENT(OUT),DIMENSION(iim*jj_nb,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1)      :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1)      :: counts
  INTEGER(i_std),DIMENSION(dimsize*iim*jjm)   :: VarTmp1
  INTEGER(i_std),DIMENSION(ij_nb,dimsize)     :: VarTmp2

  INTEGER(i_std) :: nb,i,ij,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
        DO i=1,dimsize
           VarTmp1(Index_para:Index_para+nb-1)=VarIn(ij_para_begin(rank):ij_para_end(rank),i)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
     IF (check) THEN
        WRITE(numout,*) "orch_scatter2D_mpi_igen VarIn",VarIn
        WRITE(numout,*) "orch_scatter2D_mpi_igen VarTmp1",VarTmp1
     ENDIF
  ENDIF

  CALL MPI_SCATTERV(VarTmp1,counts,displs,MPI_INT_ORCH,VarTmp2,ij_nb*dimsize,   &
       MPI_INT_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_scatter2D_mpi_igen VarTmp2",VarTmp2

  DO i=1,dimsize
     DO ij=1,ij_nb
        VarOut(ij+ii_begin-1,i)=VarTmp2(ij,i)
     ENDDO
  ENDDO
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_scatter2D_mpi_igen VarOut",VarOut

END SUBROUTINE orch_scatter2D_mpi_igen

SUBROUTINE orch_scatter2D_mpi_rgen(VarIn, VarOut, nbp2D, dimsize)
  USE mod_orchidee_para_var, iim=>iim_g,jjm=>jjm_g
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp2D
  REAL(r_std),INTENT(IN),DIMENSION(nbp2D,dimsize) :: VarIn
  REAL(r_std),INTENT(INOUT),DIMENSION(iim*jj_nb,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1)      :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1)      :: counts
  REAL(r_std),DIMENSION(dimsize*iim*jjm)   :: VarTmp1
  REAL(r_std),DIMENSION(ij_nb,dimsize)     :: VarTmp2

  INTEGER(i_std) :: nb,i,ij,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
        DO i=1,dimsize
           VarTmp1(Index_para:Index_para+nb-1)=VarIn(ij_para_begin(rank):ij_para_end(rank),i)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
     IF (check) THEN
        WRITE(numout,*) "orch_scatter2D_mpi_rgen VarIn",VarIn
        WRITE(numout,*) "orch_scatter2D_mpi_rgen VarTmp1",VarTmp1
     ENDIF
  ENDIF
  nb=ij_nb*dimsize
  IF (check) &
       WRITE(numout,*) "ij_nb*dimsize",ij_nb*dimsize

  CALL MPI_SCATTERV(VarTmp1,counts,displs,MPI_REAL_ORCH,VarTmp2,nb,   &
       MPI_REAL_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_scatter2D_mpi_rgen VarTmp2",VarTmp2

  DO i=1,dimsize
     DO ij=1,ij_nb
        VarOut(ij+ii_begin-1,i)=VarTmp2(ij,i)
     ENDDO
  ENDDO

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_scatter2D_mpi_rgen VarOut",VarOut

END SUBROUTINE orch_scatter2D_mpi_rgen

SUBROUTINE orch_scatter2D_mpi_lgen(VarIn, VarOut, nbp2D, dimsize)
  USE mod_orchidee_para_var, iim=>iim_g,jjm=>jjm_g
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp2D
  LOGICAL,INTENT(IN),DIMENSION(nbp2D,dimsize) :: VarIn
  LOGICAL,INTENT(INOUT),DIMENSION(iim*jj_nb,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1)      :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1)      :: counts
  LOGICAL,DIMENSION(dimsize*iim*jjm)   :: VarTmp1
  LOGICAL,DIMENSION(ij_nb,dimsize)     :: VarTmp2

  INTEGER(i_std) :: nb,i,ij,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
        DO i=1,dimsize
           VarTmp1(Index_para:Index_para+nb-1)=VarIn(ij_para_begin(rank):ij_para_end(rank),i)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
     IF (check) THEN
        WRITE(numout,*) "orch_scatter2D_mpi_lgen VarIn",VarIn
        WRITE(numout,*) "orch_scatter2D_mpi_lgen VarTmp1",VarTmp1
     ENDIF
  ENDIF

  CALL MPI_SCATTERV(VarTmp1,counts,displs,MPI_LOGICAL,VarTmp2,ij_nb*dimsize,   &
       MPI_LOGICAL,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_scatter2D_mpi_lgen VarTmp2",VarTmp2

  DO i=1,dimsize
     DO ij=1,ij_nb
        VarOut(ij+ii_begin-1,i)=VarTmp2(ij,i)
     ENDDO
  ENDDO
  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_scatter2D_mpi_lgen VarOut",VarOut

END SUBROUTINE orch_scatter2D_mpi_lgen


SUBROUTINE orch_gather2D_mpi_igen(VarIn, VarOut, nbp2D, dimsize)
  USE mod_orchidee_para_var, iim=>iim_g,jjm=>jjm_g
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp2D
  INTEGER(i_std),INTENT(IN),DIMENSION(iim*jj_nb,dimsize)  :: VarIn
  INTEGER(i_std),INTENT(OUT),DIMENSION(nbp2D,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1)    :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1)    :: counts
  INTEGER(i_std),DIMENSION(ij_nb,dimsize)   :: VarTmp1
  INTEGER(i_std),DIMENSION(dimsize*iim*jjm) :: VarTmp2

  INTEGER(i_std) :: nb,i,ij,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL,PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1

     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
	Index_para=Index_para+nb*dimsize
     ENDDO
     IF (check) &
          WRITE(numout,*) "orch_gather2D_mpi_igen nbp_mpi_para, displs, counts,Index_Para-1",nbp_mpi_para, displs, counts,Index_Para-1
  ENDIF
  DO i=1,dimsize
     DO ij=1,ij_nb
        VarTmp1(ij,i)=VarIn(ij+ii_begin-1,i)
     ENDDO
  ENDDO

  IF (check) THEN
     WRITE(numout,*) "orch_gather2D_mpi_igen VarIn=",VarIn    
     WRITE(numout,*) "orch_gather2D_mpi_igen VarTmp1=",VarTmp1
  ENDIF
  CALL MPI_GATHERV(VarTmp1,ij_nb*dimsize,MPI_INT_ORCH,VarTmp2,counts,displs,   &
       MPI_INT_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_gather2D_mpi_igen VarTmp2=",VarTmp2

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        DO i=1,dimsize
           VarOut(ij_para_begin(rank):ij_para_end(rank),i)=VarTmp2(Index_para:Index_para+nb-1)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
  ENDIF

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_gather2D_mpi_igen VarOut=",VarOut

END SUBROUTINE orch_gather2D_mpi_igen

SUBROUTINE orch_gather2D_mpi_rgen(VarIn, VarOut, nbp2D,dimsize)
  USE mod_orchidee_para_var, iim=>iim_g,jjm=>jjm_g
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp2D
  REAL(r_std),INTENT(IN),DIMENSION(iim*jj_nb,dimsize)  :: VarIn
  REAL(r_std),INTENT(OUT),DIMENSION(nbp2D,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1)    :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1)    :: counts
  REAL(r_std),DIMENSION(ij_nb,dimsize)   :: VarTmp1
  REAL(r_std),DIMENSION(dimsize*iim*jjm) :: VarTmp2

  INTEGER(i_std) :: nb,i,ij,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL,PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1

     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
	Index_para=Index_para+nb*dimsize
     ENDDO
     IF (check) &
          WRITE(numout,*) "orch_gather2D_mpi_rgen nbp_mpi_para, displs, counts,Index_Para-1",nbp_mpi_para, displs, counts,Index_Para-1
  ENDIF

  DO i=1,dimsize
     DO ij=1,ij_nb
        VarTmp1(ij,i)=VarIn(ij+ii_begin-1,i)
     ENDDO
  ENDDO

  IF (check) THEN
     WRITE(numout,*) "orch_gather2D_mpi_rgen VarIn=",VarIn    
     WRITE(numout,*) "orch_gather2D_mpi_rgen VarTmp1=",VarTmp1
  ENDIF
  CALL MPI_GATHERV(VarTmp1,ij_nb*dimsize,MPI_REAL_ORCH,VarTmp2,counts,displs,   &
       MPI_REAL_ORCH,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_gather2D_mpi_rgen VarTmp2=",VarTmp2

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        DO i=1,dimsize
           VarOut(ij_para_begin(rank):ij_para_end(rank),i)=VarTmp2(Index_para:Index_para+nb-1)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
  ENDIF

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_gather2D_mpi_rgen VarOut=",VarOut

END SUBROUTINE orch_gather2D_mpi_rgen

SUBROUTINE orch_gather2D_mpi_lgen(VarIn, VarOut, nbp2D, dimsize)
  USE mod_orchidee_para_var, iim=>iim_g,jjm=>jjm_g
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),INTENT(IN) :: dimsize
  INTEGER(i_std),INTENT(IN) :: nbp2D
  LOGICAL,INTENT(IN),DIMENSION(iim*jj_nb,dimsize)  :: VarIn
  LOGICAL,INTENT(OUT),DIMENSION(nbp2D,dimsize) :: VarOut

  INCLUDE 'mpif.h'

  INTEGER(i_std),DIMENSION(0:mpi_size-1)    :: displs
  INTEGER(i_std),DIMENSION(0:mpi_size-1)    :: counts
  LOGICAL,DIMENSION(ij_nb,dimsize)   :: VarTmp1
  LOGICAL,DIMENSION(dimsize*iim*jjm) :: VarTmp2

  INTEGER(i_std) :: nb,i,ij,index_para,rank
  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL,PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (flag) CALL suspend_timer(timer_mpi)

  IF (is_mpi_root) THEN
     Index_para=1

     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        displs(rank)=Index_para-1
        counts(rank)=nb*dimsize
	Index_para=Index_para+nb*dimsize
     ENDDO
     IF (check) &
          WRITE(numout,*) "orch_gather2D_mpi_lgen nbp_mpi_para, displs, counts,Index_Para-1",nbp_mpi_para, displs, counts,Index_Para-1
  ENDIF

  DO i=1,dimsize
     DO ij=1,ij_nb
        VarTmp1(ij,i)=VarIn(ij+ii_begin-1,i)
     ENDDO
  ENDDO

  IF (check) THEN
     WRITE(numout,*) "orch_gather2D_mpi_lgen VarIn=",VarIn    
     WRITE(numout,*) "orch_gather2D_mpi_lgen VarTmp1=",VarTmp1
  ENDIF
  CALL MPI_GATHERV(VarTmp1,ij_nb*dimsize,MPI_LOGICAL,VarTmp2,counts,displs,   &
       MPI_LOGICAL,mpi_rank_root, MPI_COMM_ORCH,ierr)
  IF (check) &
       WRITE(numout,*) "orch_gather2D_mpi_lgen VarTmp2=",VarTmp2

  IF (is_mpi_root) THEN
     Index_para=1
     DO rank=0,mpi_size-1
        nb=ij_para_nb(rank)
        DO i=1,dimsize
           VarOut(ij_para_begin(rank):ij_para_end(rank),i)=VarTmp2(Index_para:Index_para+nb-1)
           Index_para=Index_para+nb
        ENDDO
     ENDDO
  ENDIF

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_gather2D_mpi_lgen VarOut=",VarOut

END SUBROUTINE orch_gather2D_mpi_lgen

SUBROUTINE orch_reduce_sum_mpi_igen(VarIn,VarOut,nb)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  INTEGER(i_std),DIMENSION(nb),INTENT(IN) :: VarIn
  INTEGER(i_std),DIMENSION(nb),INTENT(OUT) :: VarOut    
  INTEGER(i_std),INTENT(IN) :: nb

  INCLUDE 'mpif.h'

  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (check) &
       WRITE(numout,*) "orch_reduce_sum_mpi_igen VarIn",VarIn
  IF (flag) CALL suspend_timer(timer_mpi)

  CALL MPI_REDUCE(VarIn,VarOut,nb,MPI_INT_ORCH,MPI_SUM,mpi_rank_root,MPI_COMM_ORCH,ierr)

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_reduce_sum_mpi_igen VarOut",VarOut

END SUBROUTINE orch_reduce_sum_mpi_igen

SUBROUTINE orch_reduce_sum_mpi_rgen(VarIn,VarOut,nb)
  USE mod_orchidee_para_var
  USE timer

  IMPLICIT NONE

  REAL(r_std),DIMENSION(nb),INTENT(IN) :: VarIn
  REAL(r_std),DIMENSION(nb),INTENT(OUT) :: VarOut    
  INTEGER(i_std),INTENT(IN) :: nb

  INCLUDE 'mpif.h'

  INTEGER(i_std) :: ierr
  LOGICAL :: flag=.FALSE.
  LOGICAL, PARAMETER :: check=.FALSE.

  IF (timer_state(timer_mpi)==running) THEN
     flag=.TRUE.
  ELSE
     flag=.FALSE.
  ENDIF

  IF (check) &
       WRITE(numout,*) "orch_reduce_sum_mpi_rgen VarIn",VarIn
  IF (flag) CALL suspend_timer(timer_mpi)

  CALL MPI_REDUCE(VarIn,VarOut,nb,MPI_REAL_ORCH,MPI_SUM,mpi_rank_root,MPI_COMM_ORCH,ierr)

  IF (flag) CALL resume_timer(timer_mpi)
  IF (check) &
       WRITE(numout,*) "orch_reduce_sum_mpi_rgen VarOut",VarOut

END SUBROUTINE orch_reduce_sum_mpi_rgen

