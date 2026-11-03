










! =================================================================================================================================
! MODULE       : lpj_kill
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF         Kills natural PFTs with low biomass and low number of individuals. 
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	:
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_kill.f90 $
!! $Date: 2015-11-16 14:26:03 +0100 (Mon, 16 Nov 2015) $
!! $Revision: 3026 $
!! \n
!_ ================================================================================================================================


MODULE lpj_kill

  ! modules used:

  USE ioipsl_para
  USE stomate_data
  USE pft_parameters
  USE constantes

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC kill

CONTAINS

!! ================================================================================================================================
!! SUBROUTINE  : kill
!!
!>\BRIEF       Kills natural pfts that have low biomass or low number of individuals, 
!! returns biomass to litter pools,  and resets biomass to zero.  
!!
!! DESCRIPTION : Kills natural PFTS. The PFT vegetation characteristics are
!! reinitialized. Kill is either done in DGVM mode or in non-DGVM mode
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): senescence, PFTpresent, cn_ind, ind, RIP_time, age, when_growthinit, 
!! everywhere,beget, veget_max, npp_longterm, biomass, bm_to_litter, leaf_age, leaf_frac
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE kill (npts, whichroutine, lm_lastyearmax, &
       ind, PFTpresent, cn_ind, biomass, senescence, RIP_time, &
       lai, age, leaf_age, leaf_frac, npp_longterm, &
       when_growthinit, everywhere, veget_max, bm_to_litter)

 !! 0. Variable and parameter description

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                                :: npts            !! Domain size (unitless)
    CHARACTER(LEN=10), INTENT(in)                             :: whichroutine    !! Message (unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: lai             !! [DISPENSABLE] leaf area index OF AN INDIVIDUAL PLANT 
                                                                                 !! @tex $(m^2 m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: lm_lastyearmax  !! Last year's maximum leaf mass, for each PFT 
                                                                                 !! @tex $(gC.m^{-2})$ @endtex

    !! 0.2 Output variables


    !! 0.3 Modified variables

    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)               :: senescence      !! Is the plant senescent? (only for deciduous 
                                                                                 !! trees - carbohydrate reserve) (true/false)
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)               :: PFTpresent      !! Is pft there (true/false)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: cn_ind          !! Crown area of individuals 
                                                                                 !! @tex $(m^2)$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: ind             !! Number of individuals 
                                                                                 !! @tex $(m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: RIP_time        !! How much time ago was the PFT eliminated for
                                                                                 !! the last time (y)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: age             !! Mean age (years)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: when_growthinit !! How many days ago was the beginning of the 
                                                                                 !! growing season (days)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: everywhere      !! Is the PFT everywhere in the grid box or very
                                                                                 !! localized (after its introduction)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: veget_max       !! "Maximal" coverage fraction of a PFT (LAI -> 
                                                                                 !! infinity) on ground (unitless;0-1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: npp_longterm    !! "Long term" (default = 3-year) net primary 
                                                                                 !! productivity 
                                                                                 !! @tex $(gC.m^{-2} year^{-1})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout) :: leaf_age        !! Leaf age (days)
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout) :: leaf_frac       !! Fraction of leaves in leaf age class 
                                                                                 !! (unitless;0-1)
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: biomass       !! Biomass @tex $(gC.m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: bm_to_litter  !! Conversion of biomass to litter 
                                                                                      !! @tex $(gC.m^{-2} day^{-1})$ @endtex

    !! 0.4 Local variables

    INTEGER(i_std)                                            :: j,m             !! Indices       (unitless)
    LOGICAL, DIMENSION(npts)                                  :: was_killed      !! Bookkeeping   (true/false)

!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering kill'

 !! 1. Kill PFTs

    ! Kill plants if number of individuals or last year's leaf mass is close to zero.
    ! the "was_killed" business is necessary for a more efficient code on the VPP processor
    DO j = 2,nvm ! loop plant functional types

       was_killed(:) = .FALSE.

       IF ( natural(j) ) THEN

          !! 1.1 Kill natural PFTs when DGVM is activated
          IF ( ok_dgvm ) THEN
             WHERE ( PFTpresent(:,j) .AND. &
                  ( ( ind(:,j) .LT. min_stomate ) .OR. &
                  ( lm_lastyearmax(:,j) .LT. min_stomate ) ) )
             
                was_killed(:) = .TRUE.
             
             ENDWHERE
          
          ELSE

             !! 1.2 Kill natural PFTs when running STOMATE without DGVM

             !! 1.2.1 Kill PFTs
             WHERE ( PFTpresent(:,j) .AND. & 
                  (biomass(:,j,icarbres,icarbon) .LE.zero .OR. & 
                  biomass(:,j,iroot,icarbon).LT.-min_stomate .OR. biomass(:,j,ileaf,icarbon).LT.-min_stomate ).AND. & 
                  ind(:,j).GT. zero)

                was_killed(:) = .TRUE.

             ENDWHERE

             !! 1.2.2 Overwrite long-term NPP for grasses
             IF(.NOT.is_tree(j).AND..NOT.lpj_gap_const_mort)THEN
                WHERE ( was_killed(:) )

                   npp_longterm(:,j) = 500.

                ENDWHERE
             ENDIF

          ENDIF ! ok_dgvm

          !! 1.3 Bookkeeping for PFTs that were killed
          !      For PFTs that were killed, return biomass pools to litter 
          !      and update biomass pools to zero
          IF ( ANY( was_killed(:) ) ) THEN
          
             DO m = 1,nelements

                WHERE ( was_killed(:) )

                   bm_to_litter(:,j,ileaf,m) = bm_to_litter(:,j,ileaf,m) + biomass(:,j,ileaf,m)
                   bm_to_litter(:,j,isapabove,m) = bm_to_litter(:,j,isapabove,m) + biomass(:,j,isapabove,m)
                   bm_to_litter(:,j,isapbelow,m) = bm_to_litter(:,j,isapbelow,m) + biomass(:,j,isapbelow,m)
                   bm_to_litter(:,j,iheartabove,m) = bm_to_litter(:,j,iheartabove,m) + &
                                                     biomass(:,j,iheartabove,m)
                   bm_to_litter(:,j,iheartbelow,m) = bm_to_litter(:,j,iheartbelow,m) + &
                                                     biomass(:,j,iheartbelow,m)
                   bm_to_litter(:,j,iroot,m) = bm_to_litter(:,j,iroot,m) + biomass(:,j,iroot,m)
                   bm_to_litter(:,j,ifruit,m) = bm_to_litter(:,j,ifruit,m) + biomass(:,j,ifruit,m)
                   bm_to_litter(:,j,icarbres,m) = bm_to_litter(:,j,icarbres,m) + biomass(:,j,icarbres,m)
                   
                   biomass(:,j,ileaf,m) = zero
                   biomass(:,j,isapabove,m) = zero
                   biomass(:,j,isapbelow,m) = zero
                   biomass(:,j,iheartabove,m) = zero
                   biomass(:,j,iheartbelow,m) = zero
                   biomass(:,j,iroot,m) = zero
                   biomass(:,j,ifruit,m) = zero
                   biomass(:,j,icarbres,m) = zero

                ENDWHERE   ! was_killed

             END DO

            !! 1.4 Update veget_max in DGVM
            !      Update veget_max in DGVM for killed PFTs and reset RIP_time
            IF (ok_dgvm) THEN

                WHERE ( was_killed(:) )
                   PFTpresent(:,j) = .FALSE.

                   veget_max(:,j) = zero
                   
                   RIP_time(:,j) = zero

                ENDWHERE  ! was_killed

            ENDIF ! ok_dgvm

            !! 1.5 Reinitialize vegetation characteristics in DGVM and STOMATE
            !      Reinitialize number of individuals, crown area and age
            WHERE ( was_killed(:) )

                ind(:,j) = zero

                cn_ind(:,j) = zero

                senescence(:,j) = .FALSE.

                age(:,j) = zero

                when_growthinit(:,j) = large_value 

                everywhere(:,j) = zero

!                veget(:,j) = zero
!MM à imposer ?!
!                lai(:,j) = zero

             ENDWHERE   ! was_killed

             !! 1.6 Update leaf ages
             DO m = 1, nleafages

                WHERE ( was_killed(:) )

                   leaf_age(:,j,m) = zero 
                   leaf_frac(:,j,m) = zero 

                ENDWHERE ! was_killed

             ENDDO

             !! 1.7 Print sub routine messages
             IF ( printlev>=2 ) THEN

                WRITE(numout,*) 'kill: eliminated ',PFT_name(j)
                WRITE(numout,*) '  at ',COUNT( was_killed(:) ),' points after '//whichroutine

             ENDIF

          ENDIF     ! PFT must be killed at at least one place

       ENDIF       ! PFT is natural

    ENDDO         ! loop over PFTs

    IF (printlev>=4) WRITE(numout,*) 'Leaving kill'

  END SUBROUTINE kill

END MODULE lpj_kill
