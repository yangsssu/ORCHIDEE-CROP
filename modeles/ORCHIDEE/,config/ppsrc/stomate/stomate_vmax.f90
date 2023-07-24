










! =================================================================================================================================
! MODULE 	: stomate_vmax
!
! CONTACT	: orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      	: IPSL (2006). This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        calculates the leaf efficiency.
!!	
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! SVN		:
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/stomate_vmax.f90 $ 
!! $Date: 2016-04-26 13:28:48 +0200 (Tue, 26 Apr 2016) $
!! $Revision: 3386 $
!! \n
!_ =================================================================================================================================

MODULE stomate_vmax

  ! modules used:

  USE ioipsl_para
  USE stomate_data
  USE constantes
  USE pft_parameters

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC vmax, vmax_clear

  ! first call
  LOGICAL, SAVE                                              :: firstcall_vmax = .TRUE.
!$OMP THREADPRIVATE(firstcall_vmax)
!JCADD
  LOGICAL, SAVE                                           :: ok_Nlim = .FALSE.
!ENDJCADD
CONTAINS

!! ================================================================================================================================
!! SUBROUTINE	: vmax_clear
!!
!>\BRIEF	  Flag setting 
!!
!!\n DESCRIPTION: This subroutine sets flags ::firstcall_vmax, to .TRUE., and therefore activates   
!!		  section 1.1 of the ::vmax subroutine which writes messages to the output. \n
!!		  This subroutine is called at the end of the subroutine ::stomate_clear, in the 
!!		  module ::stomate.
!!
!! RECENT CHANGE(S):None
!!
!! MAIN OUTPUT VARIABLE(S): ::firstcall_vmax
!!
!! REFERENCE(S)  : None 
!!
!! FLOWCHART     : None
!! \n		  
!_ =================================================================================================================================

  SUBROUTINE vmax_clear
    firstcall_vmax=.TRUE.
  END SUBROUTINE vmax_clear



!! ================================================================================================================================
!! SUBROUTINE    : vmax
!!
!>\BRIEF         This subroutine computes vcmax photosynthesis parameters 
!! given optimal vcmax parameter values and a leaf age-related efficiency.
!!
!! DESCRIPTION (functional, design, flags): 
!! Leaf age classes are introduced to take into account the fact that photosynthetic activity depends on leaf age
!! (Ishida et al., 1999). There are \f$nleafages\f$ classes (constant defined in stomate_constants.f90).
!! This subroutine first calculates the new age of each leaf age-class based on fraction of leaf 
!! that goes from one to another class.                                              
!! Then calculation of the new fraction of leaf in each class is performed.      
!! Last, leaf efficiency is calculated for each PFT and for each leaf age class.
!! vcmax is defined as vcmax25 and vjmax_opt weighted by a mean leaf
!! efficiency. vcmax25 is PFT-dependent constants defined in constants_mtc.f90.
!!
!! This routine is called once at the beginning by stomate_var_init and then at each stomate time step by stomateLpj.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): vcmax
!!
!! REFERENCE(S)	: 
!! - Ishida, A., A. Uemura, N. Koike, Y. Matsumoto, and A. Lai Hoe (1999),
!! Interactive effects of leaf age and self-shading on leaf structure, photosynthetic
!! capacity and chlorophyll fluorescence in the rain forest tree,
!! dryobalanops aromatica, Tree Physiol., 19, 741-747
!!
!! FLOWCHART    : None
!!
!! REVISION(S)	: None
!! \n
!_ ================================================================================================================================

  SUBROUTINE vmax (npts, dt, &
       leaf_age, leaf_frac, &
       vcmax, &!)
!JCADD
       N_limfert)
!ENDJCADD
    !
    !! 0. Variable and parameter declaration
    !

    !
    !! 0.1 Input variables
    !
    INTEGER(i_std), INTENT(in)                                 :: npts                    !! Domain size (unitless)
    REAL(r_std), INTENT(in)                                    :: dt                      !! time step of stomate (days)

    !
    !! 0.2 Output variables 
    !
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)              :: vcmax                   !! Maximum rate of carboxylation 
                                                                                          !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex

    !
    !! 0.3 Modified variables
    !
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)  :: leaf_age                !! Leaf age (days)
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout)  :: leaf_frac               !! fraction of leaves in leaf age 
                                                                                          !! classes 
                                                                                          !! (unitless)
!JCADD
    ! N fertilization limitation factor for grassland Vcmax and SLA
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: N_limfert
!ENDJCADD
    !
    !! 0.4 Local variables
    !
    REAL(r_std), DIMENSION(npts)                               :: leaf_efficiency         !! leaf efficiency (vcmax/vcmax25)
                                                                                          !! (unitless)
    REAL(r_std), DIMENSION(npts,nvm,nleafages)                 :: d_leaf_frac             !! turnover between age classes
                                                                                          !! (unitless)
    REAL(r_std), DIMENSION(npts,nleafages)                     :: leaf_age_new            !! new leaf age (days)
    REAL(r_std), DIMENSION(npts)                               :: sumfrac                 !! sum of leaf age fractions, 
                                                                                          !! for normalization
                                                                                          !! (unitless)
    REAL(r_std), DIMENSION(npts)                               :: rel_age                 !! relative leaf age (age/critical age)
                                                                                          !! (unitless)
    INTEGER(i_std)                                             :: j,m                     !! indices (unitless)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering vmax'

    !
    !! 1 Initialization
    !

    !
    !! 1.1 first call: info about flags and parameters.
    !

    IF ( firstcall_vmax ) THEN
!JCADD
      ok_Nlim=.false.
      CALL GETIN ('N_limitation',ok_Nlim)
      WRITE(numout,*) 'N_limitation',ok_Nlim
!ENDJCADD
       WRITE(numout,*) 'vmax:'

       WRITE(numout,*) '   > offset (minimum vcmax/vmax_opt):' , vmax_offset
       WRITE(numout,*) '   > relative leaf age at which vmax reaches vcmax_opt:', leafage_firstmax 
       WRITE(numout,*) '   > relative leaf age at which vmax falls below vcmax_opt:', leafage_lastmax
       WRITE(numout,*) '   > relative leaf age at which vmax reaches its minimum:', leafage_old

       firstcall_vmax = .FALSE.

    ENDIF

    !
    !! 1.2 initialize output
    !

    vcmax(:,:) = zero

    !
    !! 2 leaf age: general increase and turnover between age classes.
    !

    !
    !! 2.1 increase leaf age
    !
!
!! The age of the leaves in each leaf-age-class increases by 1 time step.
    DO m = 1, nleafages	! Loop over # leaf age classes 
       DO j = 2,nvm	! Loop over # PFTs
          WHERE ( leaf_frac(:,j,m) .GT. min_stomate )

             leaf_age(:,j,m) = leaf_age(:,j,m) + dt
             
          ENDWHERE
       ENDDO	! Loop over # PFTs 

    ENDDO	! Loop over # leaf age classes

    !
    !! 2.2 turnover between leaf age classes
    !     d_leaf_frac(:,:,m) = what leaves m-1 and goes into m
    !

    DO j = 2,nvm	! Loop over # PFTs

       !! 2.2.1 fluxes

       !! nothing goes into first age class
       d_leaf_frac(:,j,1) = zero

       !! for others age classes (what goes from m-1 to m)
       DO m = 2, nleafages 
!! leaf_timecst is defined in stomate_constants.f90 as the quotient of the critical leaf age per the number of age classes.
!! The critical leaf age is a PFT-dependent constant defined in stomate_constants.f90, that represents the leaf life span.
!! This time constant (leaf_timecst) determines the turnover between the nleafages different leaf age classes
!! (see section [118] in Krinner et al. (2005)).
          d_leaf_frac(:,j,m) = leaf_frac(:,j,m-1) * dt/leaf_timecst(j)

       ENDDO

       !! 2.2.2 new leaf age in class
       !!       new age = ( old age * (old fraction - fractional loss) + fractional increase * age of the source class ) / new fraction
       !!       The leaf age of the youngest class (m=1) is updated into stomate_alloc          
       leaf_age_new(:,:) = zero

       DO m = 2, nleafages-1       ! Loop over age classes
	!! For all age classes except first and last 
          WHERE ( d_leaf_frac(:,j,m) .GT. min_stomate )

             leaf_age_new(:,m) = ( ( (leaf_frac(:,j,m)- d_leaf_frac(:,j,m+1)) * leaf_age(:,j,m) )  + &
                  ( d_leaf_frac(:,j,m) * leaf_age(:,j,m-1) ) ) / &
                  ( leaf_frac(:,j,m) + d_leaf_frac(:,j,m)- d_leaf_frac(:,j,m+1) )

          ENDWHERE

       ENDDO       ! Loop over age classes

	!! For last age class, there is no leaf fraction leaving the class. 

       WHERE ( d_leaf_frac(:,j,nleafages) .GT. min_stomate )

          leaf_age_new(:,nleafages) = ( ( leaf_frac(:,j,nleafages) * leaf_age(:,j,nleafages) )  + &
               ( d_leaf_frac(:,j,nleafages) * leaf_age(:,j,nleafages-1) ) ) / &
               ( leaf_frac(:,j,nleafages) + d_leaf_frac(:,j,nleafages) )

       ENDWHERE

       DO m = 2, nleafages       ! Loop over age classes

          WHERE ( d_leaf_frac(:,j,m) .GT. min_stomate )

             leaf_age(:,j,m) = leaf_age_new(:,m)

          ENDWHERE

       ENDDO       ! Loop over age classes

       !! 2.2.3 calculate new fraction

       DO m = 2, nleafages       ! Loop over age classes

          ! where the change comes from
          leaf_frac(:,j,m-1) = leaf_frac(:,j,m-1) - d_leaf_frac(:,j,m)

          ! where it goes to
          leaf_frac(:,j,m) = leaf_frac(:,j,m) + d_leaf_frac(:,j,m)

       ENDDO       ! Loop over age classes

       !! 2.2.4 renormalize fractions in order to prevent accumulation 
       !       of numerical errors

       ! correct small negative values

       DO m = 1, nleafages
          leaf_frac(:,j,m) = MAX( zero, leaf_frac(:,j,m) )
       ENDDO

       ! total of fractions, should be very close to one where there is leaf mass

       sumfrac(:) = zero

       DO m = 1, nleafages       ! Loop over age classes

          sumfrac(:) = sumfrac(:) + leaf_frac(:,j,m)

       ENDDO       ! Loop over age classes

       ! normalize

       DO m = 1, nleafages       ! Loop over age classes

          WHERE ( sumfrac(:) .GT. min_stomate )

             leaf_frac(:,j,m) = leaf_frac(:,j,m) / sumfrac(:) 

          ELSEWHERE

             leaf_frac(:,j,m) = zero

          ENDWHERE

       ENDDO       ! Loop over age classes

    ENDDO         ! Loop over PFTs

    !
    !! 3 calculate vmax as a function of the age
    !

    DO j = 2,nvm

       vcmax(:,j) = zero

       ! sum up over the different age classes
       IF (ok_dgvm .AND. pheno_type(j)==1 .AND. leaf_tab(j)==2) THEN
          ! pheno_typ=evergreen and leaf_tab=needleleaf
          vcmax(:,j) = Vcmax25(j)

       ELSE 
          ! for deciduous tree
          DO m = 1, nleafages       ! Loop over age classes

             !
             !! 3.1 efficiency in each of the age classes
             !!     it varies from vmax_offset to 1 
             !!     linearly increases from vmax_offset to 1 for 0 < rel_age < leafage_firstmax
             !!     is 1 when leafage_firstmax < rel_age < leafage_lastmax
             !!     linearly decreases from 1 to vmax_offset for leafage_lastmax < rel_age < leafage_firstmax
             !!     vmax_offset for rel_age >= leafage_old
             !!     (Ishida et al., 1999)
             rel_age(:) = leaf_age(:,j,m) / leafagecrit(j)

             leaf_efficiency(:) = MAX( vmax_offset, MIN( un, &
                  vmax_offset + (un - vmax_offset) * rel_age(:) / leafage_firstmax, &
                  un - (un - vmax_offset) * ( rel_age(:) - leafage_lastmax ) / &
                  ( leafage_old - leafage_lastmax ) ) )

             !
             !! 3.2 add to mean vmax
             !             
             IF (ok_Nlim .OR. ok_LAIdev(j)) THEN
                vcmax(:,j) = vcmax(:,j) + Vcmax25(j) * N_limfert(:,j)*leaf_efficiency(:) * leaf_frac(:,j,m)
             ELSE
                vcmax(:,j) = vcmax(:,j) + vcmax25(j) * leaf_efficiency(:) * leaf_frac(:,j,m)
             ENDIF
          ENDDO ! loop over age classes
       ENDIF

    ENDDO       ! loop over PFTs

    IF (printlev>=4) WRITE(numout,*) 'Leaving vmax'

  END SUBROUTINE vmax

END MODULE stomate_vmax
