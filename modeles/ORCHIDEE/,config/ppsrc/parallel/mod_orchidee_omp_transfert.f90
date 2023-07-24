










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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut(:)=VarIn
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut(:)=VarIn
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut(:)=VarIn
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut=VarIn
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
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
    VarOut=VarIn
    RETURN
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
    VarOut(:)=VarIn(:)
    RETURN
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
    VarOut(:,:)=VarIn(:,:)
    RETURN
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
    VarOut(:,:,:)=VarIn(:,:,:)
    RETURN
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
    VarOut(:,:,:,:)=VarIn(:,:,:,:)
    RETURN
    IF ( check_all_transfert ) &
        omp_function(omp_rank)=omp_previous
  END SUBROUTINE reduce_sum_omp_r4

END MODULE mod_orchidee_omp_transfert

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

