










! High level MPI/OpenMP parallel communication encapsulations for ORCHIDEE.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/Attic/mod_orchidee_transfert_para.F90,v 1.1.2.2 2008/08/29 14:01:40 ssipsl Exp $
!-

MODULE mod_orchidee_transfert_para

  USE mod_orchidee_para_var
  USE mod_orchidee_mpi_transfert
  USE mod_orchidee_omp_transfert 
  


  INTERFACE bcast
    MODULE PROCEDURE bcast_c, bcast_c1,                           &
                     bcast_i,bcast_i1,bcast_i2,bcast_i3,bcast_i4, &
                     bcast_r,bcast_r1,bcast_r2,bcast_r3,bcast_r4, &
		     bcast_l,bcast_l1,bcast_l2,bcast_l3,bcast_l4
  END INTERFACE

  INTERFACE scatter
    MODULE PROCEDURE scatter_i,scatter_i1,scatter_i2,scatter_i3, &
                     scatter_r,scatter_r1,scatter_r2,scatter_r3, &
		     scatter_l,scatter_l1,scatter_l2,scatter_l3
  END INTERFACE

  
  INTERFACE gather
    MODULE PROCEDURE gather_i,gather_i1,gather_i2,gather_i3, &
                     gather_r,gather_r1,gather_r2,gather_r3, &
		     gather_l,gather_l1,gather_l2,gather_l3  
  END INTERFACE
  

  INTERFACE reduce_sum
    MODULE PROCEDURE reduce_sum_i,reduce_sum_i1,reduce_sum_i2,reduce_sum_i3,reduce_sum_i4, &
                     reduce_sum_r,reduce_sum_r1,reduce_sum_r2,reduce_sum_r3,reduce_sum_r4
  END INTERFACE 

   
CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Broadcast --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! -- Les chaine de charactère -- !!

  SUBROUTINE bcast_c(var)
  IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: Var
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_c

  SUBROUTINE bcast_c1(var)
  IMPLICIT NONE
    CHARACTER(LEN=*),INTENT(INOUT) :: Var(:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_c1

!! -- Les entiers -- !!
  
  SUBROUTINE bcast_i(var)
  IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_i

  SUBROUTINE bcast_i1(var)
  IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_i1


  SUBROUTINE bcast_i2(var)
  IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:,:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_i2


  SUBROUTINE bcast_i3(var)
  IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:,:,:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_i3


  SUBROUTINE bcast_i4(var)
  IMPLICIT NONE
    INTEGER,INTENT(INOUT) :: Var(:,:,:,:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_i4

 
!! -- Les reels -- !!
  
  SUBROUTINE bcast_r(var)
  IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var

    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_r

  SUBROUTINE bcast_r1(var)
  IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_r1


  SUBROUTINE bcast_r2(var)
  IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:,:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_r2


  SUBROUTINE bcast_r3(var)
  IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:,:,:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_r3


  SUBROUTINE bcast_r4(var)
  IMPLICIT NONE
    REAL,INTENT(INOUT) :: Var(:,:,:,:)
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_r4 


!! -- Les booleens -- !!
  
  SUBROUTINE bcast_l(var)
  IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_l

  SUBROUTINE bcast_l1(var)
  IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:)
   
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_l1


  SUBROUTINE bcast_l2(var)
  IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:)
   
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_l2


  SUBROUTINE bcast_l3(var)
  IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:,:)
    
    IF (is_omp_root) CALL bcast_mpi(Var)
    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_l3


  SUBROUTINE bcast_l4(var)
  IMPLICIT NONE
    LOGICAL,INTENT(INOUT) :: Var(:,:,:,:)
     
    IF (is_omp_root) CALL bcast_mpi(Var)

    CALL bcast_omp(Var)
    
  END SUBROUTINE bcast_l4


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Scatter   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE scatter_i(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut

    INTEGER,DIMENSION(nbp_mpi) :: Var_tmp
    
    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_i


  SUBROUTINE scatter_i1(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:) :: VarOut

    INTEGER,DIMENSION(nbp_mpi,SIZE(Varout,2)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_i1


  SUBROUTINE scatter_i2(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    INTEGER,DIMENSION(nbp_mpi,SIZE(Varout,2),SIZE(Varout,3)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_i2


  SUBROUTINE scatter_i3(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    INTEGER,DIMENSION(nbp_mpi,SIZE(Varout,2),SIZE(Varout,3),SIZE(Varout,4)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,VarOut)
    
  END SUBROUTINE scatter_i3


  SUBROUTINE scatter_r(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut

    REAL,DIMENSION(nbp_mpi) :: Var_tmp
    
    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_r


  SUBROUTINE scatter_r1(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    REAL,DIMENSION(nbp_mpi,SIZE(Varout,2)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_r1


  SUBROUTINE scatter_r2(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    REAL,DIMENSION(nbp_mpi,SIZE(Varout,2),SIZE(Varout,3)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_r2


  SUBROUTINE scatter_r3(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    REAL,DIMENSION(nbp_mpi,SIZE(Varout,2),SIZE(Varout,3),SIZE(Varout,4)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,VarOut)
    
  END SUBROUTINE scatter_r3
  
  

  SUBROUTINE scatter_l(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut

    LOGICAL,DIMENSION(nbp_mpi) :: Var_tmp
    
    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_l


  SUBROUTINE scatter_l1(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut

    LOGICAL,DIMENSION(nbp_mpi,SIZE(Varout,2)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_l1


  SUBROUTINE scatter_l2(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    LOGICAL,DIMENSION(nbp_mpi,SIZE(Varout,2),SIZE(Varout,3)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,Varout)
    
  END SUBROUTINE scatter_l2


  SUBROUTINE scatter_l3(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut

    LOGICAL,DIMENSION(nbp_mpi,SIZE(Varout,2),SIZE(Varout,3),SIZE(Varout,4)) :: Var_tmp

    
    IF (is_omp_root) CALL scatter_mpi(VarIn,Var_tmp)
    CALL scatter_omp(Var_tmp,VarOut)
    
  END SUBROUTINE scatter_l3



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des Gather   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 
!!!!! --> Les entiers

  SUBROUTINE gather_i(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut
    
    INTEGER, DIMENSION(nbp_mpi) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,Varout)
  
  END SUBROUTINE gather_i


  SUBROUTINE gather_i1(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:) :: VarOut
    
    INTEGER, DIMENSION(nbp_mpi,SIZE(VarIn,2)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,Varout)

  
  END SUBROUTINE gather_i1


  SUBROUTINE gather_i2(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    INTEGER, DIMENSION(nbp_mpi,SIZE(VarIn,2),SIZE(VarIn,3)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_i2


  SUBROUTINE gather_i3(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut
    
    INTEGER, DIMENSION(nbp_mpi,SIZE(VarIn,2),SIZE(VarIn,3),SIZE(VarIn,4)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)

  
  END SUBROUTINE gather_i3


!!!!! --> Les reels

  SUBROUTINE gather_r(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut
    
    REAL, DIMENSION(nbp_mpi) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
 
  END SUBROUTINE gather_r


  SUBROUTINE gather_r1(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:) :: VarOut
    
    REAL, DIMENSION(nbp_mpi,SIZE(VarIn,2)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)

  
  END SUBROUTINE gather_r1


  SUBROUTINE gather_r2(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    REAL, DIMENSION(nbp_mpi,SIZE(VarIn,2),SIZE(VarIn,3)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_r2


  SUBROUTINE gather_r3(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut
    
    REAL, DIMENSION(nbp_mpi,SIZE(VarIn,2),SIZE(VarIn,3),SIZE(VarIn,4)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_r3


!!!!! --> Les booleens

  SUBROUTINE gather_l(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:) :: VarOut
    
    LOGICAL, DIMENSION(nbp_mpi) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_l


  SUBROUTINE gather_l1(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:) :: VarOut
    
    LOGICAL, DIMENSION(nbp_mpi,SIZE(VarIn,2)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_l1


  SUBROUTINE gather_l2(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    LOGICAL, DIMENSION(nbp_mpi,SIZE(VarIn,2),SIZE(VarIn,3)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_l2


  SUBROUTINE gather_l3(VarIn, VarOut)
  IMPLICIT NONE
  
    LOGICAL,INTENT(IN),DIMENSION(:,:,:,:) :: VarIn
    LOGICAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut
    
    LOGICAL, DIMENSION(nbp_mpi,SIZE(VarIn,2),SIZE(VarIn,3),SIZE(VarIn,4)) :: Var_tmp
    
    CALL gather_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL gather_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE gather_l3


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Definition des reduce_sum   --> 4D   !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Les entiers

  SUBROUTINE reduce_sum_i(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN)  :: VarIn
    INTEGER,INTENT(OUT) :: VarOut
    
    INTEGER             :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_i  


  SUBROUTINE reduce_sum_i1(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:) :: VarOut
    
    INTEGER,DIMENSION(SIZE(VarIn))   :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_i1  


  SUBROUTINE reduce_sum_i2(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:) :: VarOut
    
    INTEGER,DIMENSION(SIZE(VarIn,1),SIZE(VarIn,2)) :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)

  END SUBROUTINE reduce_sum_i2  
  

  SUBROUTINE reduce_sum_i3(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:,:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    INTEGER,DIMENSION(SIZE(VarIn,1),SIZE(VarIn,2),SIZE(VarIn,3))  :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_i3  


  SUBROUTINE reduce_sum_i4(VarIn, VarOut)
  IMPLICIT NONE
  
    INTEGER,INTENT(IN),DIMENSION(:,:,:,:)  :: VarIn
    INTEGER,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut
    
    INTEGER,DIMENSION(SIZE(VarIn,1),SIZE(VarIn,2),SIZE(VarIn,3),SIZE(VarIn,4))  :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_i4  


! Les reels

  SUBROUTINE reduce_sum_r(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN)  :: VarIn
    REAL,INTENT(OUT) :: VarOut
    
    REAL             :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_r  


  SUBROUTINE reduce_sum_r1(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:) :: VarOut
    
    REAL,DIMENSION(SIZE(VarIn))   :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_r1  


  SUBROUTINE reduce_sum_r2(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:) :: VarOut
    
    REAL,DIMENSION(SIZE(VarIn,1),SIZE(VarIn,2)) :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_r2  
  

  SUBROUTINE reduce_sum_r3(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:,:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:) :: VarOut
    
    REAL,DIMENSION(SIZE(VarIn,1),SIZE(VarIn,2),SIZE(VarIn,3))  :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_r3  


  SUBROUTINE reduce_sum_r4(VarIn, VarOut)
  IMPLICIT NONE
  
    REAL,INTENT(IN),DIMENSION(:,:,:,:)  :: VarIn
    REAL,INTENT(OUT),DIMENSION(:,:,:,:) :: VarOut
    
    REAL,DIMENSION(SIZE(VarIn,1),SIZE(VarIn,2),SIZE(VarIn,3),SIZE(VarIn,4))  :: Var_tmp
           
    CALL reduce_sum_omp(VarIn,Var_tmp)
    
    IF (is_omp_root) CALL reduce_sum_mpi(Var_tmp,VarOut)
  
  END SUBROUTINE reduce_sum_r4  

   
END MODULE mod_orchidee_transfert_para

