!! This subroutines initialize a variable or an array
!! with a variable or an array of smaller rank
!! - i is for integer interface - r for real interface
!! - 0 is for a scalar - 1 for a 1D array - 2 for a 2D array
!! Thee right routines is automatically called depending type of input variable
!! This initialisation is done only if the value of input field is egal to val_exp
!!
!! If a key word is provided which is not equal to "NO_KEYWORD" or "NOKEYWORD" then
!! we try to find the value to fill in in the configuration file.
!!
!! @author Marie-Alice Foujols and Jan Polcher
!! @Version : $Revision: 1540 $, $Date: 2013-10-15 18:16:19 +0200 (Tue, 15 Oct 2013) $
!! 
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/shushi.peng/ORCHIDEE/src_sechiba/sechiba_io_p.f90 $
!< $Date: 2013-10-15 18:16:19 +0200 (Tue, 15 Oct 2013) $
!< $Author: josefine.ghattas $
!< $Revision: 1540 $
!! IPSL (2006)
!!  This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!!
MODULE sechiba_io_p

  USE defprec
  USE constantes
  USE ioipsl
  USE ioipsl_para
  USE mod_orchidee_para

  IMPLICIT NONE

  INTERFACE setvar_p
    MODULE PROCEDURE i0setvar_p, i10setvar_p, i20setvar_p, i11setvar_p, i21setvar_p, i22setvar_p
    MODULE PROCEDURE r0setvar_p, r10setvar_p, r20setvar_p, r11setvar_p, r21setvar_p, r22setvar_p, r30setvar_p
  END INTERFACE

!
! mettre la l'interface des routines utilisees:
!
! restget/put/ini histbeg/def flinopen/close
!

LOGICAL, SAVE                  :: long_print_setvar_p=.FALSE.  !! change to true to have more information
!$OMP THREADPRIVATE(long_print_setvar_p)

CONTAINS 

!! Interface for integer scalar to scalar.
SUBROUTINE i0setvar_p (var, val_exp, key_wd, val_put)

  INTEGER(i_std), INTENT(inout)                   :: var                  !! Integer scalar to modify
  INTEGER(i_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd               !! The Key word we will look for
  INTEGER(i_std), INTENT(in)                      :: val_put              !! Initial value to stored

  INTEGER(i_std)                                  :: val_tmp
  INTEGER(i_std)                                  :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
  
  IF (long_print_setvar_p) WRITE (numout,*) "i0setvar :", key_wd, val_exp, val_put

  val_tmp = val_put

  IF ( var == val_exp ) THEN 
     IF ( is_key <= 0 ) THEN
        CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var = val_tmp
  END IF
  
END SUBROUTINE i0setvar_p


!! Interface for initialising an 1D integer array with a scalar integer.
SUBROUTINE i10setvar_p (var, val_exp, key_wd, val_put)

  INTEGER(i_std), DIMENSION(:), INTENT(inout)     :: var                  !! 1D integer array to modify
  INTEGER(i_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd               !! The Key word we will look for
  INTEGER(i_std), INTENT(in)                      :: val_put              !! Scalar value to stored
  
  INTEGER(i_std)                                  :: val_tmp
  INTEGER(i_std)                                  :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))

  IF (long_print_setvar_p) WRITE (numout,*) "i10setvar :", key_wd, val_exp, val_put

  val_tmp = val_put

  IF ( ALL( var(:) == val_exp ) ) THEN
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:) = val_tmp
  END IF
  
END SUBROUTINE i10setvar_p


!! Interface for initialising an 1D array integer with an other 1D array integer.
SUBROUTINE i11setvar_p (var, val_exp, key_wd, val_put, is_grid)
  
  INTEGER(i_std), DIMENSION(:), INTENT(inout)     :: var                 !! 1D integer array to modify
  INTEGER(i_std), INTENT(in)                      :: val_exp             !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd               !! The Key word we will look for
  INTEGER(i_std), DIMENSION(:), INTENT(in)        :: val_put             !! 1D integer array to stored
  LOGICAL,        OPTIONAL                        :: is_grid              !! Parameter present indicates a setvar for a grid variable 

  INTEGER(i_std), ALLOCATABLE,DIMENSION(:)        :: val_tmp
  INTEGER(i_std), ALLOCATABLE,DIMENSION(:)        :: val_tmp_g
  INTEGER(i_std)                                  :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
  
  IF (long_print_setvar_p) WRITE (numout,*) "i11setvar :", key_wd, val_exp, SIZE(val_put), val_put(1)

  ALLOCATE(val_tmp(SIZE(val_put)))
  val_tmp(:) = val_put(:)

  IF ( ALL( var(:) == val_exp ) ) THEN 
     IF ( is_key <= 0 ) THEN
        IF (PRESENT(is_grid) ) THEN
           IF (is_root_prc) THEN
              ALLOCATE( val_tmp_g(nbp_glo) )
           ELSE
              ALLOCATE( val_tmp_g(1) )
           ENDIF
           CALL gather( val_tmp,val_tmp_g )
           IF (is_root_prc) &
              CALL getin(key_wd,  val_tmp_g)
           CALL scatter( val_tmp,val_tmp_g )
           DEALLOCATE( val_tmp_g )
        ELSE
           CALL getin_p(key_wd,  val_tmp)
        ENDIF
     ENDIF
     var(:) = val_tmp (:)
  END IF

  DEALLOCATE(val_tmp)
  
END SUBROUTINE i11setvar_p


!! Interface for initialising an 2D array integer with a scalar integer.
SUBROUTINE i20setvar_p (var, val_exp, key_wd, val_put)
  
  INTEGER(i_std), DIMENSION(:,:), INTENT(inout)   :: var                  !! 2D integer array to modify
  INTEGER(i_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd               !! The Key word we will look for
  INTEGER(i_std), INTENT(in)                      :: val_put              !! Scalar value to stored

  INTEGER(i_std)                                  :: val_tmp
  INTEGER(i_std)                                  :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
  
  !
  ! this subroutine set val_put value to var if var is constant
  ! 
  !
  IF (long_print_setvar_p) WRITE (numout,*) "i20setvar :", key_wd, val_exp, val_put

  val_tmp = val_put

  IF ( ALL( var(:,:) == val_exp ) ) THEN 
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:,:) = val_tmp
  END IF
  
END SUBROUTINE i20setvar_p


!! Interface for initialising an 2D array integer with an 1D array integer.
!! Row or column depending size of 1D array to stored.
!!
!! example: 1D 1,2,3     2D is 1, 2, 3,
!!                             1, 2, 3
!!
!!
!! example: 1D 1,2,3     2D is 1, 1,
!!                             2, 2,
!!                             3, 3
!!
SUBROUTINE i21setvar_p (var, val_exp, key_wd, val_put, is_grid)
  
  INTEGER(i_std), DIMENSION(:,:), INTENT(inout)   :: var                  !! 2D integer array to modify
  INTEGER(i_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd               !! The Key word we will look for
  INTEGER(i_std), DIMENSION(:), INTENT(in)        :: val_put              !! 1D integer array to stored
  LOGICAL,        OPTIONAL                        :: is_grid              !! Parameter present indicates a setvar for a grid variable 
  
  INTEGER(i_std), ALLOCATABLE,DIMENSION(:)        :: val_tmp
  INTEGER(i_std)                                  :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))

  ! test if the 1D array dimension is compatible with first or second 
  ! dimension of the 2D array

  IF (long_print_setvar_p) WRITE (numout,*) "i21setvar :", key_wd, val_exp, val_put

  ALLOCATE(val_tmp(SIZE(val_put)))
  val_tmp(:) = val_put(:)

  IF (SIZE(val_put)==SIZE(var,1)) THEN 
      !
      ! example: 1D 1.,2.,3.     2D is 1., 2., 3.,
      !                                1., 2., 3.
      !
      IF ( ALL( var(:,:) == val_exp ) ) THEN 
         IF ( is_key <= 0 ) THEN
           CALL getin_p(key_wd,  val_tmp)
         ENDIF
         var(:,:) = SPREAD(val_tmp(:),2,SIZE(var,1))
      END IF
  ELSEIF (SIZE(val_put)==SIZE(var,2)) THEN 
      !
      ! example: 1D 1.,2.,3.     2D is 1., 1.,
      !                                2., 2.,
      !                                3., 3.
      !
      IF ( ALL( var(:,:) == val_exp ) ) THEN 
         IF ( is_key <= 0 ) THEN
           CALL getin_p(key_wd,  val_tmp)
         ENDIF
         var(:,:) = SPREAD(val_tmp(:),1,SIZE(var,1))
      END IF
  ELSE 
      WRITE (numout,*) ' incompatible dimension var and val_put'
      WRITE (numout,*) ' var     ', SIZE(var,1), SIZE(var,2)
      WRITE (numout,*) ' val_put ', SIZE(val_put)
      STOP 'setvar'
  END IF

  DEALLOCATE(val_tmp)
  
END SUBROUTINE i21setvar_p

!! Interface for initialising an 2D array integer with an other 2D array integer.
SUBROUTINE i22setvar_p (var, val_exp, key_wd, val_put, is_grid)
  
  INTEGER(i_std), DIMENSION(:,:), INTENT(inout)   :: var                 !! 2D integer array to modify
  INTEGER(i_std), INTENT(in)                      :: val_exp             !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd              !! The Key word we will look for
  INTEGER(i_std), DIMENSION(:,:), INTENT(in)      :: val_put             !! 2D integer array to stored
  LOGICAL,        OPTIONAL                        :: is_grid              !! Parameter present indicates a setvar for a grid variable 

  INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)     :: val_tmp
  INTEGER(i_std)                                  :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
  
  IF (long_print_setvar_p) WRITE (numout,*) "i21setvar :", key_wd, val_exp, SIZE(val_put), val_put(1,1)

  ALLOCATE(val_tmp(SIZE(val_put,DIM=1),SIZE(val_put,DIM=2)))
  val_tmp(:,:) = val_put(:,:)

  IF ( ALL(var(:,:) == val_exp ) ) THEN
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:,:) = val_tmp(:,:)
  END IF

  DEALLOCATE(val_tmp)
  
END SUBROUTINE i22setvar_p


!! Interface for scalar to scalar real
SUBROUTINE r0setvar_p (var, val_exp, key_wd, val_put)
  
  REAL(r_std), INTENT(inout)                   :: var                  !! Real scalar to modify
  REAL(r_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                   :: key_wd               !! The Key word we will look for
  REAL(r_std), INTENT(in)                      :: val_put              !! Initial value to stored
  
  REAL(r_std)                                  :: val_tmp
  INTEGER(i_std)                                     :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))

  IF (long_print_setvar_p) WRITE (numout,*) "r0setvar :", key_wd, val_exp, val_put

  val_tmp = val_put

  IF ( var==val_exp ) THEN 
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var = val_tmp
  END IF
  
END SUBROUTINE r0setvar_p


!! Interface for initialising an 1D real array with a scalar real.
SUBROUTINE r10setvar_p (var, val_exp, key_wd, val_put)
  
  REAL(r_std), DIMENSION(:), INTENT(inout)     :: var                  !! 1D real array to modify
  REAL(r_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                   :: key_wd               !! The Key word we will look for
  REAL(r_std), INTENT(in)                      :: val_put              !! Scalar value to stored
   
  REAL(r_std)                                  :: val_tmp
  INTEGER(i_std)                                     :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
 
  IF (long_print_setvar_p) WRITE (numout,*) "r10setvar :", key_wd, val_exp, val_put

  val_tmp = val_put

  IF ( ALL( var(:) == val_exp ) ) THEN 
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:) = val_tmp
  END IF
  
END SUBROUTINE r10setvar_p


!! Interface for initialising an 1D array real with an other 1D array real.
SUBROUTINE r11setvar_p (var, val_exp, key_wd, val_put, is_grid)
  
  REAL(r_std), DIMENSION(:), INTENT(inout)     :: var                 !! 1D real array to modify
  REAL(r_std), INTENT(in)                      :: val_exp             !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                   :: key_wd              !! The Key word we will look for
  REAL(r_std), DIMENSION(:), INTENT(in)        :: val_put             !! 1D integer array to stored
  LOGICAL,        OPTIONAL                        :: is_grid              !! Parameter present indicates a setvar for a grid variable 

  REAL(r_std), ALLOCATABLE,DIMENSION(:)        :: val_tmp
  INTEGER(i_std)                                     :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
   
  IF (long_print_setvar_p) WRITE (numout,*) "r11setvar :", key_wd, val_exp, SIZE(val_put), val_put(1)

  ALLOCATE(val_tmp(SIZE(val_put)))
  val_tmp(:) = val_put(:)

  IF ( ALL( var(:) == val_exp ) ) THEN 
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:) = val_tmp (:)
  END IF

  DEALLOCATE(val_tmp)
  
END SUBROUTINE r11setvar_p


!! Interface for initialising an 2D array real with a scalar real.
SUBROUTINE r20setvar_p (var, val_exp, key_wd, val_put)
  
  ! interface for scalar to 2D array real

  REAL(r_std), DIMENSION(:,:), INTENT(inout)   :: var                  !! 2D integer array to modify
  REAL(r_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                   :: key_wd                  !! The Key word we will look for
  REAL(r_std), INTENT(in)                      :: val_put              !! Scalar value to stored
 
  REAL(r_std)                                  :: val_tmp  
  INTEGER(i_std)                                     :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
 
  IF (long_print_setvar_p) WRITE (numout,*) "r20setvar :", key_wd, val_exp, val_put

  val_tmp = val_put

  IF ( ALL( var(:,:) == val_exp ) ) THEN 
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:,:) = val_tmp
  END IF
  
END SUBROUTINE r20setvar_p


!! Interface for initialising an 2D array real with an 1D array real.
!! Row or column depending size of 1D array to stored.
!!
!! example: 1D 1.,2.,3.     2D is 1., 2., 3.,
!!                                1., 2., 3.
!!
!!
!! example: 1D 1.,2.,3.     2D is 1., 1.,
!!                                2., 2.,
!!                                3., 3.
!!
SUBROUTINE r21setvar_p (var, val_exp, key_wd, val_put, is_grid)
  
  ! interface for 1D array to 2D array real

  REAL(r_std), DIMENSION(:,:), INTENT(inout)   :: var                  !! 2D real array to modify
  REAL(r_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                   :: key_wd               !! The Key word we will look for
  REAL(r_std), DIMENSION(:), INTENT(in)        :: val_put              !! 1D real array to stored
  LOGICAL,        OPTIONAL                        :: is_grid              !! Parameter present indicates a setvar for a grid variable 

  REAL(r_std), ALLOCATABLE,DIMENSION(:)        :: val_tmp
  INTEGER(i_std)                                     :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))
  
  ! test if the 1D array dimension is compatible with first or second 
  ! dimension of the 2D array

  IF (long_print_setvar_p) WRITE (numout,*) "r21setvar :", key_wd, val_exp, SIZE(val_put), val_put(1)

  ALLOCATE(val_tmp(SIZE(val_put)))
  val_tmp(:) = val_put(:)

  IF (SIZE(val_put)==SIZE(var,1)) THEN 
      !
      ! example: 1D 1.,2.,3.     2D is 1., 2., 3.,
      !                                1., 2., 3.
      !
      IF ( ALL( var(:,:) == val_exp ) ) THEN 
         IF ( is_key <= 0 ) THEN
           CALL getin_p(key_wd,  val_tmp)
         ENDIF
         var(:,:) = SPREAD(val_tmp(:),2,SIZE(var,1))
      END IF
  ELSEIF (SIZE(val_put)==SIZE(var,2)) THEN 
      !
      ! example: 1D 1.,2.,3.     2D is 1., 1.,
      !                                2., 2.,
      !                                3., 3.
      !
      IF ( ALL( var(:,:) == val_exp ) ) THEN 
         IF ( is_key <= 0 ) THEN
           CALL getin_p(key_wd,  val_tmp)
         ENDIF
         var(:,:) = SPREAD(val_tmp(:),1,SIZE(var,1))
      END IF
  ELSE 
      WRITE (numout,*) ' incompatible dimension var and val_put'
      WRITE (numout,*) ' var     ', SIZE(var,1), SIZE(var,2)
      WRITE (numout,*) ' val_put ', SIZE(val_put)
      STOP 'setvar'
  END IF

  DEALLOCATE(val_tmp)
  
END SUBROUTINE r21setvar_p


!! Interface for initialising a real 2D array with a scalar parameter from run.def or an other 2D array in argument
SUBROUTINE r22setvar_p (var, val_exp, key_wd, val_put)
  
  ! interface for 2D array to 2D array real
  ! If a key_wd is set, it is supposed to be a scalar parameter will be read from run.def
  ! It is not possible to read a 2D variable from run.def

  REAL(r_std), DIMENSION(:,:), INTENT(inout)   :: var                 !! 2D real array to modify
  REAL(r_std), INTENT(in)                      :: val_exp             !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                 :: key_wd              !! The Key word we will look for
  REAL(r_std), DIMENSION(:,:), INTENT(in)      :: val_put             !! 2D integer array to stored
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: val_tmp
  REAL(r_std)                                  :: val_scal            !! Temporary variable to read a scalar value from run.def
  INTEGER(i_std)                               :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))

  IF (long_print_setvar_p) WRITE (numout,*) "r22setvar :", key_wd, val_exp, SIZE(val_put), val_put(1,1)

  ALLOCATE(val_tmp(SIZE(val_put,DIM=1),SIZE(val_put,DIM=2)))
  val_tmp(:,:) = val_put(:,:)

  IF ( ALL( var(:,:) == val_exp ) ) THEN 
     IF ( is_key <= 0 ) THEN
        ! This case only read a scalar value with getin
        val_scal=val_exp
        CALL getin_p(key_wd, val_scal)
        ! If a value was found in run.def, then set val_tmp to this value.
        IF (val_scal/=val_exp) val_tmp(:,:)=val_scal 
     ENDIF
     var(:,:) = val_tmp(:,:)
  END IF

  DEALLOCATE(val_tmp)
  
END SUBROUTINE r22setvar_p

!! Interface for initialising an 3D array real with a scalar real.
SUBROUTINE r30setvar_p (var, val_exp, key_wd, val_put)

  ! interface for scalar to 3D array real

  REAL(r_std), DIMENSION(:,:,:), INTENT(inout) :: var                  !! 3D integer array to modify
  REAL(r_std), INTENT(in)                      :: val_exp              !! Exceptional value
  CHARACTER(LEN=*), INTENT(in)                :: key_wd               !! The Key word we will look for
  REAL(r_std), INTENT(in)                      :: val_put              !! Scalar value to stored

  REAL(r_std)                                  :: val_tmp 
  INTEGER(i_std)                              :: is_key

  is_key = MAX(INDEX(key_wd, 'NO_KEYWORD'), INDEX(key_wd, 'NOKEYWORD'))

  IF (long_print_setvar_p) WRITE(numout,*) 'r30setvar',val_exp, val_put

  val_tmp = val_put

  IF ( ALL( var(:,:,:) == val_exp ) ) THEN
     IF ( is_key <= 0 ) THEN
       CALL getin_p(key_wd,  val_tmp)
     ENDIF
     var(:,:,:) = val_tmp
  END IF

END SUBROUTINE r30setvar_p

END MODULE sechiba_io_p
