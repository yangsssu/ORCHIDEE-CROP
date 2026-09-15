! Low level OpenMP parallel communication encapsulations for ORCHIDEE.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/Attic/mod_orchidee_omp_transfert.F90,v 1.1.2.2 2008/08/29 14:01:40 ssipsl Exp $
!-

MODULE mod_orchidee_omp_transfert
  !-
  USE mod_orchidee_omp_data
  USE ioipsl
  !-
  IMPLICIT NONE

  PRIVATE
  
#ifdef CPP_OMP
  ! Check OpenMP buffer sizes increase.
  LOGICAL, PARAMETER :: check_size = .FALSE.

  INTEGER,PARAMETER :: grow_factor=1.5
  INTEGER,PARAMETER :: size_min=1024
  PUBLIC size_min

  INTEGER(i_std),SAVE,ALLOCATABLE,DIMENSION(:) :: omp_ibuffer
  INTEGER,SAVE                            :: size_i=0
  LOGICAL,SAVE,ALLOCATABLE,DIMENSION(:) :: omp_lbuffer
  INTEGER,SAVE                            :: size_l=0
  REAL(r_std),SAVE,ALLOCATABLE,DIMENSION(:) :: omp_rbuffer
  INTEGER,SAVE                            :: size_r=0
  CHARACTER(len=size_min), SAVE,ALLOCATABLE,DIMENSION(:) :: omp_cbuffer
  INTEGER,SAVE                            :: size_c=0

#endif

  INTERFACE bcast_omp
     MODULE PROCEDURE bcast_omp_c, bcast_omp_c1,                                       &
          bcast_omp_i,bcast_omp_i1,bcast_omp_i2,bcast_omp_i3,bcast_omp_i4, &
          bcast_omp_r,bcast_omp_r1,bcast_omp_r2,bcast_omp_r3,bcast_omp_r4, &
          bcast_omp_l,bcast_omp_l1,bcast_omp_l2,bcast_omp_l3,bcast_omp_l4
  END INTERFACE

  INTERFACE scatter_omp
     MODULE PROCEDURE scatter_omp_i,scatter_omp_i1,scatter_omp_i2,scatter_omp_i3, &
          scatter_omp_r,scatter_omp_r1,scatter_omp_r2,scatter_omp_r3, &
          scatter_omp_l,scatter_omp_l1,scatter_omp_l2,scatter_omp_l3
  END INTERFACE


  INTERFACE gather_omp
     MODULE PROCEDURE gather_omp_i0,gather_omp_i,gather_omp_i1,gather_omp_i2,gather_omp_i3, &
          gather_omp_r0,gather_omp_r,gather_omp_r1,gather_omp_r2,gather_omp_r3, &
          gather_omp_l0,gather_omp_l,gather_omp_l1,gather_omp_l2,gather_omp_l3  
  END INTERFACE


  INTERFACE reduce_sum_omp
     MODULE PROCEDURE reduce_sum_omp_i,reduce_sum_omp_i1,reduce_sum_omp_i2,reduce_sum_omp_i3,reduce_sum_omp_i4, &
          reduce_sum_omp_r,reduce_sum_omp_r1,reduce_sum_omp_r2,reduce_sum_omp_r3,reduce_sum_omp_r4
  END INTERFACE

  PUBLIC  bcast_omp,scatter_omp,gather_omp,reduce_sum_omp

CONTAINS

  SUBROUTINE check_buffer_c(buff_size)
    IMPLICIT NONE
    INTEGER :: buff_size

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 72 
      CALL print_omp_function()
    ENDIF
#ifdef CPP_OMP
    CALL barrier2_omp()
    IF (is_omp_root) THEN
       IF (buff_size>size_c) THEN
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) "ORCHIDEE OMP; buffer for strings : old_size, new_size"
             ELSE
               WRITE(*,*) "ORCHIDEE OMP; buffer for strings : old_size, new_size"
             ENDIF
             IF (ALLOCATED(omp_cbuffer)) THEN
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) SIZE(omp_cbuffer)
                ELSE
                  WRITE(*,*) SIZE(omp_cbuffer)
                ENDIF
             ELSE
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) 0
                ELSE
                  WRITE(*,*) 0
                ENDIF
             ENDIF
          ENDIF
          IF (ALLOCATED(omp_cbuffer)) DEALLOCATE(omp_cbuffer)
          size_c=MAX(size_min,INT(grow_factor*buff_size))
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) size_c
             ELSE
               WRITE(*,*) size_c
             ENDIF
          ENDIF
          ALLOCATE(omp_cbuffer(size_c))
       ENDIF
    ENDIF
    CALL barrier2_omp()

#endif

    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE check_buffer_c

  SUBROUTINE check_buffer_i(buff_size)
    IMPLICIT NONE
    INTEGER :: buff_size

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 1 
      CALL print_omp_function()
    ENDIF
#ifdef CPP_OMP
    CALL barrier2_omp()

    IF (is_omp_root) THEN
       IF (buff_size>size_i) THEN
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) "ORCHIDEE OMP; buffer for integers : old_size, new_size"
             ELSE
               WRITE(*,*) "ORCHIDEE OMP; buffer for integers : old_size, new_size"
             ENDIF
             IF (ALLOCATED(omp_ibuffer)) THEN
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) SIZE(omp_ibuffer)
                ELSE
                  WRITE(*,*) SIZE(omp_ibuffer)
                ENDIF
             ELSE
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) 0
                ELSE
                  WRITE(*,*) 0
                ENDIF
             ENDIF
          ENDIF
          IF (ALLOCATED(omp_ibuffer)) DEALLOCATE(omp_ibuffer)
          size_i=MAX(size_min,INT(grow_factor*buff_size))
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) size_i
             ELSE
               WRITE(*,*) size_i
             ENDIF
          ENDIF
          ALLOCATE(omp_ibuffer(size_i))
       ENDIF
    ENDIF
    CALL barrier2_omp()

#endif

    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE check_buffer_i
  
  SUBROUTINE check_buffer_r(buff_size)
    IMPLICIT NONE
    INTEGER :: buff_size

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 2
      CALL print_omp_function()
    ENDIF
#ifdef CPP_OMP
    CALL barrier2_omp()

    IF (is_omp_root) THEN
       IF (buff_size>size_r) THEN
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) "ORCHIDEE OMP; buffer for reals : old_size, new_size"
             ELSE
               WRITE(*,*) "ORCHIDEE OMP; buffer for reals : old_size, new_size"
             ENDIF
             IF (ALLOCATED(omp_rbuffer)) THEN
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) SIZE(omp_rbuffer)
                ELSE
                  WRITE(*,*) SIZE(omp_rbuffer)
                ENDIF
             ELSE
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) 0
                ELSE
                  WRITE(*,*) 0
                ENDIF
             ENDIF
          ENDIF
          IF (ALLOCATED(omp_rbuffer)) DEALLOCATE(omp_rbuffer)
          size_r=MAX(size_min,INT(grow_factor*buff_size))
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) size_r
             ELSE
               WRITE(*,*) size_r
             ENDIF
          ENDIF
          ALLOCATE(omp_rbuffer(size_r))
       ENDIF
    ENDIF
    CALL barrier2_omp()

#endif

    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE check_buffer_r
  
  SUBROUTINE check_buffer_l(buff_size)
    IMPLICIT NONE
    INTEGER :: buff_size

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 3
      CALL print_omp_function()
    ENDIF
#ifdef CPP_OMP
    CALL barrier2_omp()

    IF (is_omp_root) THEN
       IF (buff_size>size_l) THEN
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) "ORCHIDEE OMP; buffer for logicals : old_size, new_size"
             ELSE
               WRITE(*,*) "ORCHIDEE OMP; buffer for logicals : old_size, new_size"
             ENDIF
             IF (ALLOCATED(omp_lbuffer)) THEN
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) SIZE(omp_lbuffer)
                ELSE
                  WRITE(*,*) SIZE(omp_lbuffer)
                ENDIF
             ELSE
                IF (numout_omp > 0) THEN
                  WRITE(numout_omp,*) 0
                ELSE
                  WRITE(*,*) 0
                ENDIF
             ENDIF
          ENDIF
          IF (ALLOCATED(omp_lbuffer)) DEALLOCATE(omp_lbuffer)
          size_l=MAX(size_min,INT(grow_factor*buff_size))
          IF ( check_size ) THEN
             IF (numout_omp > 0) THEN
               WRITE(numout_omp,*) size_l
             ELSE
               WRITE(*,*) size_l
             ENDIF
          ENDIF
          ALLOCATE(omp_lbuffer(size_l))
       ENDIF
    ENDIF
    CALL barrier2_omp()

#endif

    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE check_buffer_l
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Broadcast --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !! -- Les chaine de charactère -- !!

  SUBROUTINE bcast_omp_c(Var)
    IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: Var
    CHARACTER(LEN=len(Var)),DIMENSION(1) :: Var1

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 4
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    IF (is_omp_root) &
         Var1(1)=Var
    CALL check_buffer_c(1)
    CALL orch_bcast_omp_cgen(Var1,1,omp_cbuffer)
    Var=Var1(1)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_c

  SUBROUTINE bcast_omp_c1(Var)
    IMPLICIT NONE
    CHARACTER(LEN=*),DIMENSION(:),INTENT(INOUT) :: Var

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 4
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_c(size(Var))
    CALL orch_bcast_omp_cgen(Var,size(Var),omp_cbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_c1

  !! -- Les entiers -- !!

  SUBROUTINE bcast_omp_i(var1)
    IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var1

    INTEGER,DIMENSION(1) :: Var

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 5
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    IF (is_omp_root) &
         Var(1)=Var1
    CALL check_buffer_i(1)
    CALL orch_bcast_omp_igen(Var,1,omp_ibuffer)
    Var1=Var(1)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_i

  SUBROUTINE bcast_omp_i1(var)
    IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 6
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_i(size(Var))
    CALL orch_bcast_omp_igen(Var,SIZE(Var),omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_i1

  SUBROUTINE bcast_omp_i2(var)
    IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 7
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_i(size(Var))
    CALL orch_bcast_omp_igen(Var,SIZE(Var),omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_i2

  SUBROUTINE bcast_omp_i3(var)
    IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:,:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 8
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_i(size(Var))
    CALL orch_bcast_omp_igen(Var,SIZE(Var),omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_i3

  SUBROUTINE bcast_omp_i4(var)
    IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:,:,:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)= 9
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_i(size(Var))
    CALL orch_bcast_omp_igen(Var,SIZE(Var),omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_i4


  !! -- Les reels -- !!

  SUBROUTINE bcast_omp_r(var)
    IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var

    REAL,DIMENSION(1) :: Var1

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=10
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    IF (is_omp_root) &
         Var1(1)=Var
    CALL check_buffer_r(1)
    CALL orch_bcast_omp_rgen(Var1,1,omp_rbuffer)
    Var=Var1(1)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_r

  SUBROUTINE bcast_omp_r1(var)
    IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=11
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_r(size(Var))
    CALL orch_bcast_omp_rgen(Var,SIZE(Var),omp_rbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_r1

  SUBROUTINE bcast_omp_r2(var)
    IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=12
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_r(size(Var))
    CALL orch_bcast_omp_rgen(Var,SIZE(Var),omp_rbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_r2

  SUBROUTINE bcast_omp_r3(var)
    IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:,:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=13
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_r(size(Var))
    CALL orch_bcast_omp_rgen(Var,SIZE(Var),omp_rbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_r3

  SUBROUTINE bcast_omp_r4(var)
    IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:,:,:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=14
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_r(size(Var))
    CALL orch_bcast_omp_rgen(Var,SIZE(Var),omp_rbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_r4

  !! -- Les booleans -- !!

  SUBROUTINE bcast_omp_l(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var

    LOGICAL,DIMENSION(1) :: Var1

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=15
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    IF (is_omp_root) &
         Var1(1)=Var
    CALL check_buffer_l(1)
    CALL orch_bcast_omp_lgen(Var1,1,omp_lbuffer)
    Var=Var1(1)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_l

  SUBROUTINE bcast_omp_l1(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=16
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_l(size(Var))
    CALL orch_bcast_omp_lgen(Var,SIZE(Var),omp_lbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_l1

  SUBROUTINE bcast_omp_l2(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=17
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_l(size(Var))
    CALL orch_bcast_omp_lgen(Var,SIZE(Var),omp_lbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_l2

  SUBROUTINE bcast_omp_l3(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=18
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_l(size(Var))
    CALL orch_bcast_omp_lgen(Var,SIZE(Var),omp_lbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_l3

  SUBROUTINE bcast_omp_l4(var)
    IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:,:,:)

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=19
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    RETURN
#else
    CALL check_buffer_l(size(Var))
    CALL orch_bcast_omp_lgen(Var,SIZE(Var),omp_lbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE bcast_omp_l4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Scatter   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE scatter_omp_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=20
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_i(size(VarIn))   
    CALL orch_scatter_omp_igen(VarIn,Varout,1,omp_ibuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_i

  SUBROUTINE scatter_omp_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=21
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarIn))   
    CALL orch_scatter_omp_igen(VarIn,Varout,SIZE(VarOut,2),omp_ibuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_i1

  SUBROUTINE scatter_omp_i2(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=22
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else    
    CALL check_buffer_i(size(VarIn))   
    CALL orch_scatter_omp_igen(VarIn,Varout,SIZE(VarOut,2)*SIZE(VarOut,3),omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_i2

  SUBROUTINE scatter_omp_i3(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=23
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else    
    CALL check_buffer_i(size(VarIn))   
    CALL orch_scatter_omp_igen(VarIn,Varout,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4),omp_ibuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_i3


  SUBROUTINE scatter_omp_r(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=24
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_scatter_omp_rgen(VarIn,Varout,1,omp_rbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_r

  SUBROUTINE scatter_omp_r1(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=25
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_scatter_omp_rgen(VarIn,Varout,SIZE(VarOut,2),omp_rbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_r1

  SUBROUTINE scatter_omp_r2(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=26
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_scatter_omp_rgen(VarIn,Varout,SIZE(VarOut,2)*SIZE(VarOut,3),omp_rbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_r2

  SUBROUTINE scatter_omp_r3(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=27
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_scatter_omp_rgen(VarIn,Varout,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4),omp_rbuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_r3


  SUBROUTINE scatter_omp_l(VarIn, VarOut)

    IMPLICIT NONE
    LOGICAL,INTENT(IN),DIMENSION(:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=28
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_l(size(VarIn))   
    CALL orch_scatter_omp_lgen(VarIn,Varout,1,omp_lbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_l

  SUBROUTINE scatter_omp_l1(VarIn, VarOut)

    IMPLICIT NONE
    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=29
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_l(size(VarIn))   
    CALL orch_scatter_omp_lgen(VarIn,Varout,SIZE(VarOut,2),omp_lbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_l1

  SUBROUTINE scatter_omp_l2(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=30
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_l(size(VarIn))   
    CALL orch_scatter_omp_lgen(VarIn,Varout,SIZE(VarOut,2)*SIZE(VarOut,3),omp_lbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_l2

  SUBROUTINE scatter_omp_l3(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=31
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_l(size(VarIn))   
    CALL orch_scatter_omp_lgen(VarIn,Varout,SIZE(VarOut,2)*SIZE(VarOut,3)*SIZE(VarOut,4),omp_lbuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE scatter_omp_l3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Gather   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE gather_omp_i0(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN)               :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=32
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn
    RETURN
#else
    CALL check_buffer_i(size(VarOut))   
    CALL orch_gather_omp_simple_igen(VarIn,Varout,omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_i0

!!!!! --> Les entiers

  SUBROUTINE gather_omp_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=33
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_i(size(VarOut))   
    CALL orch_gather_omp_igen(VarIn,Varout,1,omp_ibuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_i


  SUBROUTINE gather_omp_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=34
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarOut))   
    CALL orch_gather_omp_igen(VarIn,Varout,SIZE(VarIn,2),omp_ibuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_i1


  SUBROUTINE gather_omp_i2(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=35
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarOut))   
    CALL orch_gather_omp_igen(VarIn,Varout,SIZE(VarIn,2)*SIZE(VarIn,3),omp_ibuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_i2


  SUBROUTINE gather_omp_i3(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=36
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarOut))   
    CALL orch_gather_omp_igen(VarIn,Varout,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4),omp_ibuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_i3

!!!!! --> Les reels

  SUBROUTINE gather_omp_r0(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN)               :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=37
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn
    RETURN
#else
    CALL check_buffer_r(size(VarOut))   
    CALL orch_gather_omp_simple_rgen(VarIn,Varout,omp_rbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_r0

  SUBROUTINE gather_omp_r(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=38
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_r(size(VarOut))   
    CALL orch_gather_omp_rgen(VarIn,Varout,1,omp_rbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_r


  SUBROUTINE gather_omp_r1(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=39
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarOut))   
    CALL orch_gather_omp_rgen(VarIn,Varout,SIZE(VarIn,2),omp_rbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_r1


  SUBROUTINE gather_omp_r2(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=40
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarOut))   
    CALL orch_gather_omp_rgen(VarIn,Varout,SIZE(VarIn,2)*SIZE(VarIn,3),omp_rbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_r2


  SUBROUTINE gather_omp_r3(VarIn, VarOut)

    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=41
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarOut))   
    CALL orch_gather_omp_rgen(VarIn,Varout,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4),omp_rbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_r3

!!!!! --> Les booleen

  SUBROUTINE gather_omp_l0(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN)               :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=42
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn
    RETURN
#else
    CALL check_buffer_l(size(VarOut))   
    CALL orch_gather_omp_simple_lgen(VarIn,Varout,omp_lbuffer)
#endif
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_l0

  SUBROUTINE gather_omp_l(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=43
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_l(size(VarOut))   
    CALL orch_gather_omp_lgen(VarIn,Varout,1,omp_lbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_l


  SUBROUTINE gather_omp_l1(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=44
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_l(size(VarOut))   
    CALL orch_gather_omp_lgen(VarIn,Varout,SIZE(VarIn,2),omp_lbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_l1


  SUBROUTINE gather_omp_l2(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=45
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_l(size(VarOut))   
    CALL orch_gather_omp_lgen(VarIn,Varout,SIZE(VarIn,2)*SIZE(VarIn,3),omp_lbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_l2


  SUBROUTINE gather_omp_l3(VarIn, VarOut)

    IMPLICIT NONE

    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=46
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_l(size(VarOut))   
    CALL orch_gather_omp_lgen(VarIn,Varout,SIZE(VarIn,2)*SIZE(VarIn,3)*SIZE(VarIn,4),omp_lbuffer)
#endif    
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE gather_omp_l3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des reduce_sum   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE reduce_sum_omp_i(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN)  :: VarIn
    INTEGER,INTENT(OUT) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=47
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut=VarIn
    RETURN
#else
    CALL check_buffer_i(1)   
    CALL orch_reduce_sum_omp_igen(VarIn,Varout,1,omp_ibuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_i

  SUBROUTINE reduce_sum_omp_i1(VarIn, VarOut)

    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=48
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_i(size(VarIn))   
    CALL orch_reduce_sum_omp_igen(VarIn,Varout,SIZE(VarIn),omp_ibuffer)
#endif   
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_i1

  SUBROUTINE reduce_sum_omp_i2(VarIn, VarOut)
    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=49
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarIn))   
    CALL orch_reduce_sum_omp_igen(VarIn,Varout,SIZE(VarIn),omp_ibuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_i2

  SUBROUTINE reduce_sum_omp_i3(VarIn, VarOut)
    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:,:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=50
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarIn))   
    CALL orch_reduce_sum_omp_igen(VarIn,Varout,SIZE(VarIn),omp_ibuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_i3

  SUBROUTINE reduce_sum_omp_i4(VarIn, VarOut)
    IMPLICIT NONE

    INTEGER,INTENT(IN),DIMENSION(:,:,:,:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=51
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_i(size(VarIn))   
    CALL orch_reduce_sum_omp_igen(VarIn,Varout,SIZE(VarIn),omp_ibuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_i4


  SUBROUTINE reduce_sum_omp_r(VarIn, VarOut)
    IMPLICIT NONE

    REAL,INTENT(IN)  :: VarIn
    REAL,INTENT(OUT) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=52
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut=VarIn
    RETURN
#else
    CALL check_buffer_r(1)   
    CALL orch_reduce_sum_omp_rgen(VarIn,Varout,1,omp_rbuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_r

  SUBROUTINE reduce_sum_omp_r1(VarIn, VarOut)
    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=53
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:)=VarIn(:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_reduce_sum_omp_rgen(VarIn,Varout,SIZE(VarIn),omp_rbuffer)
#endif   
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_r1

  SUBROUTINE reduce_sum_omp_r2(VarIn, VarOut)
    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=54
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:)=VarIn(:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_reduce_sum_omp_rgen(VarIn,Varout,SIZE(VarIn),omp_rbuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_r2

  SUBROUTINE reduce_sum_omp_r3(VarIn, VarOut)
    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:,:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=55
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_reduce_sum_omp_rgen(VarIn,Varout,SIZE(VarIn),omp_rbuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_r3

  SUBROUTINE reduce_sum_omp_r4(VarIn, VarOut)
    IMPLICIT NONE

    REAL,INTENT(IN),DIMENSION(:,:,:,:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    IF ( check_all_transfert ) THEN
      omp_previous=omp_function(omp_rank)
      omp_function(omp_rank)=56
      CALL print_omp_function()
    ENDIF
#ifndef CPP_OMP
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
#else
    CALL check_buffer_r(size(VarIn))   
    CALL orch_reduce_sum_omp_rgen(VarIn,Varout,SIZE(VarIn),omp_rbuffer)
#endif  
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_r4

END MODULE mod_orchidee_omp_transfert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef CPP_OMP

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! DEFINITION DES FONCTIONS DE TRANSFERT GENERIQUES !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE orch_bcast_omp_cgen(Var,Nb,Buff)
  USE mod_orchidee_omp_data
  USE mod_orchidee_omp_transfert, ONLY : size_min

  IMPLICIT NONE
  INTEGER,INTENT(IN)              :: Nb 
  CHARACTER(LEN=*),DIMENSION(Nb),INTENT(INOUT) :: Var
  CHARACTER(LEN=*),DIMENSION(Nb),INTENT(INOUT) :: Buff
  INTEGER :: i
  LOGICAL, PARAMETER :: check=.FALSE.

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=57
    CALL print_omp_function()
  ENDIF

  IF (check) THEN
     IF (numout_omp > 0) THEN
        WRITE(numout_omp,*) "orch_bcast_omp_cgen before bcast Var",Var
     ELSE
        WRITE(*,*) "orch_bcast_omp_cgen before bcast Var",Var
     ENDIF
  ENDIF

  IF (is_omp_root) THEN
     IF ( len(Var) > size_min ) &
          CALL ipslerr (3,'orch_bcast_omp_cgen', &
          &          'Error with omp_cbuffer.', 'len(Var) > size_min', &
          &          '(Increase size_min in mod_orchidee_omp_transfert.)')
     DO i=1,Nb
        Buff(i)=TRIM(Var(i))
     ENDDO
  ENDIF

  CALL barrier2_omp()

  DO i=1,Nb
     Var(i)=Buff(i)
  ENDDO
  CALL barrier2_omp()
      
  IF (check) THEN
     IF (numout_omp > 0) THEN
        WRITE(numout_omp,*) "orch_bcast_omp_cgen after bcast Var",Var
     ELSE
        WRITE(*,*) "orch_bcast_omp_cgen after bcast Var",Var
     ENDIF
  ENDIF

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_bcast_omp_cgen



SUBROUTINE orch_bcast_omp_igen(Var,Nb,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  INTEGER,DIMENSION(Nb),INTENT(INOUT) :: Var
  INTEGER,DIMENSION(Nb),INTENT(INOUT) :: Buff
  INTEGER,INTENT(IN) :: Nb  

  INTEGER :: i

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=58
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) THEN
     DO i=1,Nb
        Buff(i)=Var(i)
     ENDDO
  ENDIF

  CALL barrier2_omp()


  DO i=1,Nb
     Var(i)=Buff(i)
  ENDDO

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_bcast_omp_igen



SUBROUTINE orch_bcast_omp_rgen(Var,Nb,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  REAL,DIMENSION(Nb),INTENT(INOUT) :: Var
  REAL,DIMENSION(Nb),INTENT(INOUT) :: Buff
  INTEGER,INTENT(IN) :: Nb

  INTEGER :: i

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=59
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) THEN
     DO i=1,Nb
        Buff(i)=Var(i)
     ENDDO
  ENDIF

  CALL barrier2_omp()

  DO i=1,Nb
     Var(i)=Buff(i)
  ENDDO

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_bcast_omp_rgen



SUBROUTINE orch_bcast_omp_lgen(Var,Nb,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  LOGICAL,DIMENSION(Nb),INTENT(INOUT) :: Var
  LOGICAL,DIMENSION(Nb),INTENT(INOUT) :: Buff
  INTEGER,INTENT(IN) :: Nb

  INTEGER :: i

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=60
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) THEN
     DO i=1,Nb
        Buff(i)=Var(i)
     ENDDO
  ENDIF

  CALL barrier2_omp()

  DO i=1,Nb
     Var(i)=Buff(i)
  ENDDO

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_bcast_omp_lgen



SUBROUTINE orch_scatter_omp_igen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data
  USE mod_orchidee_para_var, ONLY : nbp_mpi 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  INTEGER,INTENT(IN),DIMENSION(nbp_mpi,dimsize) :: VarIn
  INTEGER,INTENT(OUT),DIMENSION(nbp_omp,dimsize) :: VarOut
  INTEGER,INTENT(INOUT),DIMENSION(nbp_mpi,dimsize) :: Buff

  INTEGER :: i,ij

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=61
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) THEN
     DO i=1,dimsize
        DO ij=1,nbp_mpi
           Buff(ij,i)=VarIn(ij,i)
        ENDDO
     ENDDO
  ENDIF

  CALL barrier2_omp()

  DO i=1,dimsize
     DO ij=1,nbp_omp
        VarOut(ij,i)=Buff(nbp_omp_begin-1+ij,i)
     ENDDO
  ENDDO

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_scatter_omp_igen


SUBROUTINE orch_scatter_omp_rgen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  USE mod_orchidee_para_var, ONLY : nbp_mpi 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  REAL,INTENT(IN),DIMENSION(nbp_mpi,dimsize) :: VarIn
  REAL,INTENT(OUT),DIMENSION(nbp_omp,dimsize) :: VarOut
  REAL,INTENT(INOUT),DIMENSION(nbp_mpi,dimsize) :: Buff

  INTEGER :: i,ij

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=62
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) THEN
     DO i=1,dimsize
        DO ij=1,nbp_mpi
           Buff(ij,i)=VarIn(ij,i)
        ENDDO
     ENDDO
  ENDIF

  CALL barrier2_omp()

  DO i=1,dimsize
     DO ij=1,nbp_omp
        VarOut(ij,i)=Buff(nbp_omp_begin-1+ij,i)
     ENDDO
  ENDDO

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_scatter_omp_rgen


SUBROUTINE orch_scatter_omp_lgen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  USE mod_orchidee_para_var, ONLY : nbp_mpi 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  LOGICAL,INTENT(IN),DIMENSION(nbp_mpi,dimsize) :: VarIn
  LOGICAL,INTENT(OUT),DIMENSION(nbp_omp,dimsize) :: VarOut
  LOGICAL,INTENT(INOUT),DIMENSION(nbp_mpi,dimsize) :: Buff

  INTEGER :: i,ij

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=63
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) THEN
     DO i=1,dimsize
        DO ij=1,nbp_mpi
           Buff(ij,i)=VarIn(ij,i)
        ENDDO
     ENDDO
  ENDIF

  CALL barrier2_omp()

  DO i=1,dimsize
     DO ij=1,nbp_omp
        VarOut(ij,i)=Buff(nbp_omp_begin-1+ij,i)
     ENDDO
  ENDDO

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_scatter_omp_lgen



SUBROUTINE orch_gather_omp_simple_igen(VarIn,VarOut,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  INTEGER,INTENT(IN)                            :: VarIn
  INTEGER,INTENT(OUT),DIMENSION(0:omp_size-1)   :: VarOut
  INTEGER,INTENT(INOUT),DIMENSION(0:omp_size-1) :: Buff

  Buff(omp_rank)=VarIn

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=64
    CALL print_omp_function()
  ENDIF

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     VarOut(0:omp_size-1)=Buff(0:omp_size-1)
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_gather_omp_simple_igen

SUBROUTINE orch_gather_omp_igen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  USE mod_orchidee_para_var, ONLY : nbp_mpi 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  INTEGER,INTENT(IN),DIMENSION(nbp_omp,dimsize) :: VarIn
  INTEGER,INTENT(OUT),DIMENSION(nbp_mpi,dimsize) :: VarOut
  INTEGER,INTENT(INOUT),DIMENSION(nbp_mpi,dimsize) :: Buff

  INTEGER :: i,ij

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=65
    CALL print_omp_function()
  ENDIF

  DO i=1,dimsize
     DO ij=1,nbp_omp
        Buff(nbp_omp_begin-1+ij,i)=VarIn(ij,i)
     ENDDO
  ENDDO

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     DO i=1,dimsize
        DO ij=1,nbp_mpi
           VarOut(ij,i)=Buff(ij,i)
        ENDDO
     ENDDO
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_gather_omp_igen


SUBROUTINE orch_gather_omp_simple_rgen(VarIn,VarOut,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  REAL,INTENT(IN)                            :: VarIn
  REAL,INTENT(OUT),DIMENSION(0:omp_size-1)   :: VarOut
  REAL,INTENT(INOUT),DIMENSION(0:omp_size-1) :: Buff

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=66
    CALL print_omp_function()
  ENDIF

  Buff(omp_rank)=VarIn

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     VarOut(0:omp_size-1)=Buff(0:omp_size-1)
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_gather_omp_simple_rgen


SUBROUTINE orch_gather_omp_rgen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  USE mod_orchidee_para_var, ONLY : nbp_mpi 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  REAL,INTENT(IN),DIMENSION(nbp_omp,dimsize) :: VarIn
  REAL,INTENT(OUT),DIMENSION(nbp_mpi,dimsize) :: VarOut
  REAL,INTENT(INOUT),DIMENSION(nbp_mpi,dimsize) :: Buff

  INTEGER :: i,ij

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=67
    CALL print_omp_function()
  ENDIF

  DO i=1,dimsize
     DO ij=1,nbp_omp
        Buff(nbp_omp_begin-1+ij,i)=VarIn(ij,i)
     ENDDO
  ENDDO

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     DO i=1,dimsize
        DO ij=1,nbp_mpi
           VarOut(ij,i)=Buff(ij,i)
        ENDDO
     ENDDO
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_gather_omp_rgen


SUBROUTINE orch_gather_omp_simple_lgen(VarIn,VarOut,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  LOGICAL,INTENT(IN)                            :: VarIn
  LOGICAL,INTENT(OUT),DIMENSION(0:omp_size-1)   :: VarOut
  LOGICAL,INTENT(INOUT),DIMENSION(0:omp_size-1) :: Buff

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=68
    CALL print_omp_function()
  ENDIF

  Buff(omp_rank)=VarIn

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     VarOut(0:omp_size-1)=Buff(0:omp_size-1)
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_gather_omp_simple_lgen


SUBROUTINE orch_gather_omp_lgen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  USE mod_orchidee_para_var, ONLY : nbp_mpi 
  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  LOGICAL,INTENT(IN),DIMENSION(nbp_omp,dimsize) :: VarIn
  LOGICAL,INTENT(OUT),DIMENSION(nbp_mpi,dimsize) :: VarOut
  LOGICAL,INTENT(INOUT),DIMENSION(nbp_mpi,dimsize) :: Buff

  INTEGER :: i,ij

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=69
    CALL print_omp_function()
  ENDIF

  DO i=1,dimsize
     DO ij=1,nbp_omp
        Buff(nbp_omp_begin-1+ij,i)=VarIn(ij,i)
     ENDDO
  ENDDO

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     DO i=1,dimsize
        DO ij=1,nbp_mpi
           VarOut(ij,i)=Buff(ij,i)
        ENDDO
     ENDDO
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_gather_omp_lgen



SUBROUTINE orch_reduce_sum_omp_igen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  INTEGER,INTENT(IN),DIMENSION(dimsize) :: VarIn
  INTEGER,INTENT(OUT),DIMENSION(dimsize) :: VarOut
  INTEGER,INTENT(INOUT),DIMENSION(dimsize) :: Buff

  INTEGER :: i

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=70
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) Buff(:)=0

  CALL barrier2_omp()

!$OMP CRITICAL     
  DO i=1,dimsize
     Buff(i)=Buff(i)+VarIn(i)
  ENDDO
!$OMP END CRITICAL

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     DO i=1,dimsize
        VarOut(i)=Buff(i)
     ENDDO
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_reduce_sum_omp_igen


SUBROUTINE orch_reduce_sum_omp_rgen(VarIn,VarOut,dimsize,Buff)
  USE mod_orchidee_omp_data

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: dimsize
  REAL,INTENT(IN),DIMENSION(dimsize) :: VarIn
  REAL,INTENT(OUT),DIMENSION(dimsize) :: VarOut
  REAL,INTENT(INOUT),DIMENSION(dimsize) :: Buff

  INTEGER :: i

  IF ( check_all_transfert ) THEN
    omp_previous=omp_function(omp_rank)
    omp_function(omp_rank)=71
    CALL print_omp_function()
  ENDIF

  IF (is_omp_root) Buff(:)=0

  CALL barrier2_omp()

!$OMP CRITICAL     
  DO i=1,dimsize
     Buff(i)=Buff(i)+VarIn(i)
  ENDDO
!$OMP END CRITICAL

  CALL barrier2_omp()

  IF (is_omp_root) THEN
     DO i=1,dimsize
        VarOut(i)=Buff(i)
     ENDDO
  ENDIF

  CALL barrier2_omp()

  IF ( check_all_transfert ) &
      omp_function(omp_rank)=omp_previous
END SUBROUTINE orch_reduce_sum_omp_rgen

#endif
