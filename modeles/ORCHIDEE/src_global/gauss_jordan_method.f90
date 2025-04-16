! =================================================================================================================================
! MODULE       : matrix_resolution
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2011)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF         This module solves a linear system using the Gauss-Jordan elimination method. It also calculates relative errors
!! globally and for the passive pools.        
!! 
!!
!!\n DESCRIPTION:  This module solves a linear system AX = B with the Gauss Jordan elimination method
!!                 (chosen because the system has no particular properties). \n
!!                 The code has originally picked up in Numerical recipes in Fortran 90. \n
!!                 We simplified the code in our case because we solve a basic (7,7) matrix for each point and each pft
!!                 (so we have npts*nvm*(7,7) systems to solve each time we call this routine). \n
!!                 We also calculate relative for biomass and passive carbon pools in order to test the threshold error. \n
!!
!! RECENT CHANGE(S): Didier Solyga - add subroutine for calculating relative error for passive pool.
!!
!! REFERENCE(S)	: None
!!
!! SVN          :
!! $HeadURL: $
!! $Date: $
!! $Revision:  $
!! \n
!_ ================================================================================================================================

MODULE matrix_resolution

  ! modules used 

  USE ioipsl ! for precision
  USE constantes

  IMPLICIT NONE

  CONTAINS
    

!! ================================================================================================================================
!! SUBROUTINE   : gauss-jordan_method 
!!
!>\BRIEF          This subroutine resolves a linear system by the Gauss-Jordan method.
!! (inversion of the system - complexity O(n^3)) .
!!
!! DESCRIPTION  : 
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): vector_b contains the solution of the system.
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

    SUBROUTINE gauss_jordan_method(n,matrix_a,vector_b)

      IMPLICIT NONE

      !! 0. Variables and parameters declaration    

      !! 0.1 Input variables 
 
      INTEGER(i_std), INTENT(in) :: n  !! size of the system (1-N, unitless)

      !! 0.3 Modified variables

      REAL(r_std), DIMENSION(n,n), INTENT(inout) :: matrix_a   !! Matrix A of the linear system A.X = B
      REAL(r_std), DIMENSION(n), INTENT(inout)   :: vector_b   !! Vector B in the linear system A.X = B
      
      !! 0.4 Local Variables

      INTEGER(i_std) :: i,col,row,j,k,ii,jj        !! index (unitless)
      INTEGER(i_std), DIMENSION(n) :: index_pivot  !! vector containing the pivot index
      INTEGER(i_std), DIMENSION(n) :: index_col    !! vector containing the columns index
      INTEGER(i_std), DIMENSION(n) :: index_row    !! vector containing the rows index
      REAL(r_std) :: pivot_max,inv_pivot,temp      !! temporary variables
      
!_ ================================================================================================================================
      
      !! Initialization
      index_pivot(:) = 0
      col = 0
      row = 0
      
      !! Search the pivot (strategy of full pivoting)
      !! We search the greatest pivot (in order to reduce errors)
      DO i = 1,n     
         pivot_max = 0.
         DO  j = 1,n
            IF(index_pivot(j) /= 1) THEN
               DO k = 1,n
                  IF(index_pivot(k) .EQ. 0) THEN
                     IF(ABS(matrix_a(j,k)) .GE. pivot_max) THEN  
                        pivot_max = ABS(matrix_a(j,k))           
                        row = j
                        col = k
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDDO

         IF (col .EQ. 0) THEN
            CALL ipslerr_p (3,'gauss_jordan_method','Method failed.','','')
         ENDIF

         index_pivot(col)=index_pivot(col) + 1

         !! We exchange the rows and the lines if needed
         IF(row /= col) THEN
            DO j = 1,n
               temp = matrix_a(row,j)
               matrix_a(row,j) = matrix_a(col,j)
               matrix_a(col,j) = temp
            ENDDO
            temp = vector_b(row)
            vector_b(row) = vector_b(col)
            vector_b(col) = temp
         ENDIF
         index_row(i) = row
         index_col(i) = col
         IF(matrix_a(col,col) .EQ. 0.) STOP 'the matrix A is not inversible'
         inv_pivot = 1./matrix_a(col,col)
         DO j = 1,n
            matrix_a(col,j) = matrix_a(col,j) * inv_pivot 
         ENDDO
         vector_b(col) = vector_b(col) * inv_pivot
         DO ii = 1,n
            IF(ii /= col) THEN
               temp = matrix_a(ii,col)
               matrix_a(ii,col) = 0.
               DO jj = 1,n
                  matrix_a(ii,jj) = matrix_a(ii,jj) - matrix_a(col,jj)*temp
               ENDDO
               vector_b(ii) = vector_b(ii) - vector_b(col)*temp
            ENDIF
         ENDDO
      ENDDO
      
      DO j = n,1,-1
         IF(index_row(j) /= index_col(j)) THEN
            DO i = 1,n
               temp = matrix_a(i,index_row(j))
               matrix_a(i,index_row(j)) = matrix_a(i,index_col(j)) 
               matrix_a(i,index_col(j)) = temp
          ENDDO
       ENDIF
    ENDDO
    
    
  END SUBROUTINE gauss_jordan_method


!! ================================================================================================================================
!! SUBROUTINE   : error_L1_passive
!!
!>\BRIEF          This subroutine calculates relative errors of a vector by taking the relative error for the passive pool.
!!
!! DESCRIPTION  : 
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): flag is to true if the maximum relative error is less than a threshold chosen by the user.
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

 SUBROUTINE error_L1_passive(npts,nb_veget, nb_pools, current_value, previous_value, veget_max, criterion, flag)
  
    IMPLICIT NONE

    !! 0. Parameters and variables declaration
    
    !! 0.1 Input variables    
    
    INTEGER(i_std), INTENT(in) :: npts                                             !! Number of continental grid cells (unitless)
    INTEGER(i_std), INTENT(in) :: nb_veget                                         !! Number of vegetation types (2-N, unitless)
    INTEGER(i_std), INTENT(in) :: nb_pools                                         !! Number of carbon pools (unitless)
    REAL(r_std), DIMENSION(npts,nb_veget,nb_pools), INTENT(in) :: current_value    !! Previous values of carbon pools obtained
                                                                                   !! by matrix resolution (gC.m^{-2})
    REAL(r_std), DIMENSION(npts,nb_veget,nb_pools), INTENT(in) :: previous_value   !! Current values of carbon pools  obtained
                                                                                   !! by matrix resolution (gC.m^{-2})   
    REAL(r_std), DIMENSION(npts,nb_veget), INTENT(in) :: veget_max                 !! Fraction of vegetation (0-1, uniless)
    REAL(r_std), INTENT(in) :: criterion                                           !! Threshold for the relativ error (0-1, unitless) 
    
    !! 0.2 Output variables
    
    LOGICAL, DIMENSION(npts), INTENT(out) :: flag   !! Logical array used only inside this subroutine (true/false)
    
    !! 0.4 Local variables
    
    INTEGER(i_std) :: j                                      !! Index (unitless)
    REAL(r_std), DIMENSION(npts) :: previous_passive_stock   !! Previous value of total passive carbon (gC) 
    REAL(r_std), DIMENSION(npts) :: current_passive_stock    !! Current value of total passive carbon (gC)
    REAL(r_std), DIMENSION(npts) :: error_global             !! Temporary arrays containing the relative error for each grid cell 
                                                             !! (unitless)         
    REAL(r_std), DIMENSION(npts) :: temp_diff                !! Working array storing difference values between previous_passive_stock
                                                             !! and current_passive_stock (gC)

!_ ================================================================================================================================

    !! Initialize flag and error_global
    flag(:) = .FALSE.
    error_global(:) = zero

    !! Calculation previous_passive_stock
    previous_passive_stock(:) = zero
    DO j = 1, nb_veget
       previous_passive_stock(:) = previous_passive_stock(:) + previous_value(:,j,ipassive_pool)*veget_max(:,j)
    ENDDO

    !! Calculation current_passive_stock
    current_passive_stock(:) = zero
    DO j = 1, nb_veget
       current_passive_stock(:) = current_passive_stock(:) + current_value(:,j,ipassive_pool)*veget_max(:,j)
    ENDDO
    
    !! We calculate for the error for the passive pool for each pixel
    temp_diff(:) = zero
    temp_diff(:) =  current_passive_stock(:) - previous_passive_stock(:) 
    WHERE ( previous_passive_stock(:) >  min_stomate )
       error_global(:) = 100.*ABS(temp_diff(:))/previous_passive_stock(:) 
    ELSEWHERE
       error_global(:) = ABS(temp_diff(:))
    ENDWHERE

    !! if the criterion is reached, we can mark the point 
    WHERE (error_global(:) <= criterion)
       flag = .TRUE.
    ENDWHERE
       
  END SUBROUTINE error_L1_passive       


END MODULE matrix_resolution
