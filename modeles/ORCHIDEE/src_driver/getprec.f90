!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_driver/getprec.f90 $ 
!< $Date: 2012-10-26 16:32:10 +0200 (Fri, 26 Oct 2012) $
!< $Author: josefine.ghattas $
!< $Revision: 1042 $
!-
PROGRAM getprec
!---------------------------------------------------------------------
!- This program verifies that that the number representation
!- between the different components of the model are compatible
!---------------------------------------------------------------------
  USE defprec
!-
  IMPLICIT NONE
!-
  INTEGER :: i
  REAL    :: r
!-
  INTEGER :: range_int,range_real,precision_real
!---------------------------------------------------------------------
  range_int  = RANGE(i)
  range_real = RANGE(r)
  precision_real = PRECISION(r)
!-
  WRITE(*,*) 'The following ranges and precisions are standard'
  WRITE(*,*) 'on this computer with your compiler options :'
  WRITE(*,*) ' INTEGER range     :',range_int
  WRITE(*,*) ' REAL    range     :',range_real
  WRITE(*,*) ' REAL    precision :',precision_real
!-
  WRITE(*,*) 'The corresponding kinds are :'
  WRITE(*,*) ' KIND for integer  :', &
 & SELECTED_INT_KIND(range_int)
  WRITE(*,*) ' KIND for real     :', &
 & SELECTED_REAL_KIND(precision_real,range_real)
!-
  WRITE(*,*) 'We test if this corresponds to what is used'
  WRITE(*,*) 'in various parts of the code :'
!-
! Real :
!-
  IF (SELECTED_REAL_KIND(precision_real,range_real) /= r_std) THEN
    WRITE(*,*) ' REAL : ERROR, the wrong kind is specified.'
    WRITE(*,*) ' Use the value above.'
  ELSE
    WRITE(*,*) ' REAL    : OK'
  ENDIF
!-
! Integer :
!-
  IF (SELECTED_INT_KIND(range_int) /= i_std) THEN
    WRITE(*,*) ' INTEGER : ERROR, the wrong kind is specified.'
    WRITE(*,*) ' Use the value above.'
  ELSE
    WRITE(*,*) ' INTEGER : OK'
  ENDIF
!------------------
END PROGRAM getprec
