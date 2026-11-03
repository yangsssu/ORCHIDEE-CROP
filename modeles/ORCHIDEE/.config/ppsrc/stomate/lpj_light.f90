










! =================================================================================================================================
! MODULE       : lpj_light
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
!                This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Light competition within a PFT
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	:
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_light.f90 $
!! $Date: 2015-11-16 14:26:03 +0100 (Mon, 16 Nov 2015) $
!! $Revision: 3026 $
!! \n
!_ ================================================================================================================================

MODULE lpj_light

  ! modules used:
  USE xios_orchidee
  USE ioipsl_para
  USE constantes
  USE stomate_data

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC light, light_clear

  LOGICAL, SAVE                                            :: firstcall_light = .TRUE.             !! first call
!$OMP THREADPRIVATE(firstcall_light)

CONTAINS

!! ================================================================================================================================
!! SUBROUTINE   : light_clear
!!
!>\BRIEF          Activation
!!
!_ ================================================================================================================================

  SUBROUTINE light_clear
    firstcall_light=.TRUE.
  END SUBROUTINE light_clear


!! ================================================================================================================================
!! SUBROUTINE 	: light
!!
!>\BRIEF         Light competition within a PFT
!!
!! DESCRIPTION  : This module kills PFTs based on light competition
!! 
!! Here, fpc ("foilage projected cover") takes into account the minimum fraction
!! of space covered by trees through branches etc. This is done to prevent strong relative
!! changes of FPC from one day to another for deciduous trees at the beginning of their
!! growing season, which would yield too strong cutbacks.\n
!!
!! fpc is now always calculated from lm_lastyearmax*sla, since the aim of this DGVM is 
!! to represent community ecology effects; seasonal variations in establishment related to phenology
!! may be relevant, but beyond the scope of a 1st generation DGVM.\n
!!
!! If agriculture is present, fpc can never reach 1.0 since natural veget_max < 1.0. To
!! correct for this, ::ind must be recalculated to correspond to the natural density.
!! since ::ind is expressed in m^{-2} grid cell, this can be achieved by dividing individual 
!! density by the agricultural fraction.\n
!!
!! The flow in the routine is different for ::ok_dgvm. When ::ok_dgvm is true
!! the following processes are considered:
!!
!! No competition between woody pfts (height of individuals is not considered).
!! Exception: when one woody pft is overwhelming (i.e. fpc > fpc_crit). In that
!! case, eliminate all other woody pfts and reduce dominant pft to fpc_crit.
!! Age of individuals is not considered. In reality, light competition would more
!! easily kill young individuals, thus increasing the mean age of the stand.
!! Exclude agricultural pfts from competition.\n
!!
!! When ::ok_dgvm is false then light competition is calculated for the static case if the mortality is not 
!! assumed to be constant. The following processes are considered: XXX
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): ind, biomass, veget_lastlight, bm_to_litter, mortality
!!
!! REFERENCES   : 
!! - Sitch, S., B. Smith, et al. (2003), Evaluation of ecosystem dynamics,
!! plant geography and terrestrial carbon cycling in the LPJ dynamic 
!! global vegetation model, Global Change Biology, 9, 161-185.\n
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE light (npts, dt, &
       veget_max, fpc_max, PFTpresent, cn_ind, lai, maxfpc_lastyear, &
       lm_lastyearmax, ind, biomass, veget_lastlight, bm_to_litter, mortality, &
!JCADD
       sla_calc)
!ENDJCADD


 !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                             :: npts                     !! Domain size (unitless)      
    REAL(r_std), INTENT(in)                                :: dt                       !! Time step (days)      
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)               :: PFTpresent               !! TRUE if pft is present (true/false)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: cn_ind                   !! Crown area of individuals 
                                                                                       !! @tex $(m^2)$ @endtex  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: lai                      !! Leaf area index OF AN INDIVIDUAL PLANT 
                                                                                       !! @tex $(m^2 m^{-2})$ @endtex   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: maxfpc_lastyear          !! Last year's maximum fpc for each natural 
                                                                                       !! PFT(unitless)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: lm_lastyearmax           !! Last year's maximum leafmass for each 
                                                                                       !! natural PFT 
                                                                                       !! @tex $(gC m^2 s^{-1})$ @endtex   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: veget_max                !! Last year's maximum fpc for each natural 
                                                                                       !! PFT (unitless;0-1)   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: fpc_max                  !! Last year's maximum fpc for each natural 
                                                                                       !! PFT (unitless)    
!JCADD
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: sla_calc
!ENDJCADD
    !! 0.2 Output variables

    !! 0.3 Modified variables
   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)        :: ind                      !! Number of individuals 
                                                                                       !! @tex $(m^{-2})$ @endtex    
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: biomass        !! Biomass @tex $(gCm^{-2})$ @endtex    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)        :: veget_lastlight          !! Vegetation cover after last light 
                                                                                       !! competition (unitless;0-1)     
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: bm_to_litter   !! Biomass transfer to litter per timestep
                                                                                       !! @tex $(gCm^{-2})$ @endtex    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)        :: mortality                !! Fraction of individuals that died this 
                                                                                       !! time step per dt (unitless;0-1)   

    !! 0.4 Local variables

    LOGICAL, PARAMETER                                     :: annual_increase = .TRUE. !! For diagnosis of fpc increase, compare 
                                                                                       !! today's fpc to last year's  Maximum (T)
                                                                                       !! or to fpc of last time step (F) 
    INTEGER(i_std)                                         :: i,j,k,m                  !! Index (unitless)    
    REAL(r_std), DIMENSION(npts)                           :: sumfpc                   !! Total natural fpc, sum of all the PFTs
                                                                                       !! (unitless)   
    REAL(r_std), DIMENSION(npts)                           :: fracnat                  !! Fraction of natural vegetation within a 
                                                                                       !! grid cell (unitless;0-1)   
    REAL(r_std)                                            :: sumfpc_wood              !! Total natural woody fpc (unitless)   
    REAL(r_std)                                            :: sumdelta_fpc_wood        !! Change in total woody fpc (unitless)   
    REAL(r_std)                                            :: maxfpc_wood              !! Maximum wood fpc (unitless)   
    INTEGER(i_std)                                         :: optpft_wood              !! Which woody pft is maximum (unitless)   
    REAL(r_std)                                            :: sumfpc_grass             !! Total natural grass fpc (unitless)   
    REAL(r_std), DIMENSION(npts,nvm)                       :: fpc_nat                  !! This year's foliage projected cover on 
                                                                                       !! natural part of the grid cell 
                                                                                       !! @tex $(m^2)$ @endtex
    REAL(r_std), DIMENSION(nvm)                            :: deltafpc                 !! fpc change within last year (unitless)    
    REAL(r_std)                                            :: reduct                   !! Relative change of number of individuals
                                                                                       !! for trees (ind)    
    REAL(r_std), DIMENSION(nvm)                            :: survive                  !! Fraction of plants that survive 
                                                                                       !! (unitless;0-1)     
    REAL(r_std), DIMENSION(npts)                           :: fpc_real                 !! FPC for static mode (unitless)     
    REAL(r_std), DIMENSION(npts)                           :: lai_ind                  !! FPC mortality for static mode     
    REAL(r_std)                                            :: sumfpc_grass2            !! New total grass fpc     
    REAL(r_std), DIMENSION(npts,nvm)                       :: light_death              !! Fraction of plants that dies each day 
                                                                                       !! @tex $(day^{-1})$ @endtex     
    REAL(r_std)                                            :: fpc_dec                  !! Relative change of number of individuals 
                                                                                       !! for trees
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering light'

    
 !! 1. Write diagnostics to out_orchidee files
  
    IF ( firstcall_light ) THEN

       WRITE(numout,*) 'light:'

       WRITE(numout,*) '   > For trees, minimum fraction of crown area covered'
       WRITE(numout,*) '       (due to its branches etc.)', min_cover

       WRITE(numout,*) '   > for diagnosis of fpc increase, compare today''s fpc'
       IF ( annual_increase ) THEN
          WRITE(numout,*) '     to last year''s maximum.'
       ELSE
          WRITE(numout,*) '     to fpc of the last time step.'
       ENDIF

       firstcall_light = .FALSE.

    ENDIF

!! 2. Light competition in DGVM

    IF (ok_dgvm) THEN
             
       !! 2.1 Calculate natural part of the grid cell
       fracnat(:) = un
       DO j = 2,nvm
          IF ( .NOT. natural(j) ) THEN
             fracnat(:) = fracnat(:) - veget_max(:,j)
          ENDIF
       ENDDO
       
       !! 2.2 Calculate fpc on natural part of grid cell
       fpc_nat(:,:) = zero
       fpc_nat(:,ibare_sechiba) = un

       DO j = 2, nvm ! loop over #PFTs


          !! 2.2.1 Natural PFTs
          IF ( natural(j) ) THEN
   
             !!?? it seems that the treatment below for trees and grasses are the same? so there is no necessity to use IF...ELSE...ENDIF structure?
	     !!?? CODE SHOULD BE CLEANED UP BELOW

             !! 2.2.1.1 Trees
             IF ( is_tree(j) ) THEN

                ! !! 2.1.1.1 trees: minimum cover due to stems, branches etc.
                !          DO i = 1, npts
                !             IF (lai(i,j) == val_exp) THEN
                !                fpc_nat(i,j) = cn_ind(i,j) * ind(i,j)
                !             ELSE
                !                fpc_nat(i,j) = cn_ind(i,j) * ind(i,j) * &
                !                     MAX( ( 1._r_std - exp( -lai(i,j) * ext_coeff(j) ) ), min_cover )
                !             ENDIF
                !          ENDDO
                !NV : modif from S. Zaehle version : fpc is based on veget_max, not veget.

                WHERE(fracnat(:).GE.min_stomate)

                   !            WHERE(LAI(:,j) == val_exp)
                   !               fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:)
                   !            ELSEWHERE
                   !               fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:) * &
                   !                    MAX( ( 1._r_std - exp( - lm_lastyearmax(:,j) * sla(j) * ext_coeff(j) ) ), min_cover )
                   !            ENDWHERE

                   fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:)
                ENDWHERE

             ELSE

                !NV : modif from S. Zaehle version : fpc is based on veget_max, not veget.
		!!?? DO GRASSES HAVE CROWNS?
                
                !! 2.2.1.1 Grasses
                WHERE(fracnat(:).GE.min_stomate)

                   !            WHERE(LAI(:,j) == val_exp)
                   !               fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:)
                   !            ELSEWHERE
                   !               fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:) * &
                   !                    ( 1._r_std - exp( - lm_lastyearmax(:,j) * sla(j) * ext_coeff(j) ) )
                   !            ENDWHERE

                   fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:)
                ENDWHERE

!!!$                ! 2.1.1.2 bare ground 
!!!$                IF (j == ibare_sechiba) THEN
!!!$                   fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) 
!!!$
!!!$                   ! 2.1.1.3 grasses
!!!$                ELSE
!!!$                   DO i = 1, npts
!!!$                      IF (lai(i,j) == val_exp) THEN
!!!$                         fpc_nat(i,j) = cn_ind(i,j) * ind(i,j)
!!!$                      ELSE
!!!$                         fpc_nat(i,j) = cn_ind(i,j) * ind(i,j) * &
!!!$                              ( 1._r_std - exp( -lai(i,j) * ext_coeff(j) ) )
!!!$                      ENDIF
!!!$                   ENDDO
!!!$                ENDIF

             ENDIF  ! tree/grass

          ELSE

             !! 2.2.2 Agricultural PFTs
             !        Agriculural PFTs are not present on natural part
             fpc_nat(:,j) = zero

          ENDIF    ! natural/agricultural

       ENDDO

       
       !! 2.3 Total fpc for grid point
       sumfpc(:) = zero
       DO j = 2,nvm

          !S. Zaehle bug correction MERGE: need to subtract agricultural area!
          sumfpc(:) = sumfpc(:) + fpc_nat(:,j)
       ENDDO

       
       !! 2.4 Light competition

       light_death(:,:) = zero

       DO i = 1, npts ! S. Zaehle why this loop and not a vector statement ?

          !! 2.4.1 Dense canopy 
          IF ( sumfpc(i) .GT. fpc_crit ) THEN

             ! 2.4.1.1 fpc change for each pft
             ! There are two possibilities: either we compare today's fpc with the fpc after the last
             ! time step, or we compare it to last year's maximum fpc of that PFT. In the first case,
             ! the fpc increase will be strong for seasonal PFTs at the beginning of the growing season.
             ! As for trees, the cutback is proportional to this increase, this means that seasonal trees
             ! will be disadvantaged compared to evergreen trees. In the original LPJ model, with its 
             ! annual time step, the second method was used (this corresponds to annual_increase=.TRUE.)

             IF ( annual_increase ) THEN
                deltafpc(:) = MAX( (fpc_nat(i,:)-maxfpc_lastyear(i,:)),  zero )
             ELSE
                deltafpc(:) = MAX( (fpc_nat(i,:)-veget_lastlight(i,:)),  zero )
             ENDIF

             !! 2.4.1.2 Default survival
             survive(:) = un

             
             !! 2.4.1.3 Determine some characteristics of the fpc distribution
             sumfpc_wood = zero
             sumdelta_fpc_wood = zero
             maxfpc_wood = zero
             optpft_wood = 0
             sumfpc_grass = zero

             DO j = 2,nvm ! loop over #PFTs

                !! 2.4.1.3.1 Natural pfts
                IF ( natural(j) ) THEN

                   !! 2.4.1.3.1.1 Trees
                   IF ( is_tree(j) ) THEN

                      ! total woody fpc
                      sumfpc_wood = sumfpc_wood + fpc_nat(i,j)

                      ! how much did the woody fpc increase
                      sumdelta_fpc_wood = sumdelta_fpc_wood + deltafpc(j)

                      ! which woody pft is preponderant
                      IF ( fpc_nat(i,j) .GT. maxfpc_wood ) THEN

                         optpft_wood = j

                         maxfpc_wood = fpc_nat(i,j)

                      ENDIF

                   ELSE

                      !! 2.4.1.3.1.2 Grasses
                      ! total (natural) grass fpc
                      sumfpc_grass = sumfpc_grass + fpc_nat(i,j)

                   ENDIF   ! tree or grass

                ENDIF   ! natural

             ENDDO  ! loop over pfts

             !! 2.4.1.4 Wood outcompetes grass
             !          Light competition where wood outcompetes grasses
             
             !S. Zaehle           IF (sumfpc_wood .GE. fpc_crit ) THEN
             !
             !! 3.2.1 all allowed natural space is covered by wood:
             !!       cut back trees to fpc_crit.
             !!       Original DGVM: kill grasses. Modified: we let a very
             !!       small fraction of grasses survive.
             !

             DO j = 2,nvm ! Loop over #PFTs

                ! only present and natural pfts compete
                IF ( PFTpresent(i,j) .AND. natural(j) ) THEN

                   !! 2.4.1.4.1 Trees
                   IF ( is_tree(j) ) THEN

                      ! no single woody pft is overwhelming
                      ! (original DGVM: tree_mercy = 0.0 )
                      ! The reduction rate is proportional to the ratio deltafpc/fpc.
                      IF (sumfpc_wood .GE. fpc_crit .AND. fpc_nat(i,j) .GT. min_stomate .AND. & 
                           sumdelta_fpc_wood .GT. min_stomate) THEN

                         ! reduct = MIN( ( ( deltafpc(j)/sumdelta_fpc_wood * &
                         !     (sumfpc_wood-fpc_crit) ) / fpc_nat(i,j) ), &
                         !     ( 1._r_std - 0.01 ) ) ! (0.01 = tree_mercy previously)

                         !!? difficult to fully understand but doesn't look so simple
                         reduct = un - MIN((fpc_nat(i,j)-(sumfpc_wood-fpc_crit) & 
                              * deltafpc(j)/sumdelta_fpc_wood)/fpc_nat(i,j), un )

                      ELSE

                         ! tree fpc didn't icrease or it started from nothing
                         reduct = zero

                      ENDIF
                   ELSE

                      !! 2.4.1.4.2 Grasses 
                      !            Let a very small fraction survive (the sum of all
                      !            grass individuals may make up a maximum cover of
                      !            grass_mercy [for lai -> infinity]).
                      !            In the original DGVM, grasses were killed in that case,
                      !            corresponding to grass_mercy = 0.
                      !

                      IF(sumfpc_grass .GE. un-MIN(fpc_crit,sumfpc_wood).AND. & 
                           sumfpc_grass.GE.min_stomate) THEN

                         fpc_dec = (sumfpc_grass - un + MIN(fpc_crit,sumfpc_wood))*fpc_nat(i,j)/sumfpc_grass

                         reduct = fpc_dec
                      ELSE 
                         reduct = zero
                      ENDIF
                   ENDIF   ! tree or grass

                   survive(j) = un - reduct
                ENDIF     ! pft there and natural

             ENDDO       ! loop over pfts

             !S. Zaehle
!!!$          ELSE
!!!$
!!!$             !
!!!$             ! 3.2.2 not too much wood so that grasses can subsist
!!!$             !
!!!$
!!!$             ! new total grass fpc
!!!$             sumfpc_grass2 = fpc_crit - sumfpc_wood
!!!$
!!!$             DO j = 2,nvm
!!!$
!!!$                ! only present and natural PFTs compete
!!!$
!!!$                IF ( PFTpresent(i,j) .AND. natural(j) ) THEN
!!!$
!!!$                   IF ( is_tree(j) ) THEN
!!!$
!!!$                      ! no change for trees
!!!$
!!!$                      survive(j) = 1.0
!!!$
!!!$                   ELSE
!!!$
!!!$                      ! grass: fractional loss is the same for all grasses
!!!$
!!!$                      IF ( sumfpc_grass .GT. min_stomate ) THEN
!!!$                         survive(j) = sumfpc_grass2 / sumfpc_grass
!!!$                      ELSE
!!!$                         survive(j)=  zero
!!!$                      ENDIF
!!!$
!!!$                   ENDIF
!!!$
!!!$                ENDIF    ! pft there and natural
!!!$
!!!$             ENDDO       ! loop over pfts
!!!$
!!!$          ENDIF    ! sumfpc_wood > fpc_crit

             
             !! 2.4.1.5 Update biomass and litter pools
             
             DO j = 2,nvm ! Loop over #PFTs

                ! Natural PFTs
                IF ( PFTpresent(i,j) .AND. natural(j) ) THEN

                   bm_to_litter(i,j,:,:) = bm_to_litter(i,j,:,:) + &
                        biomass(i,j,:,:) * ( un - survive(j) )

                   biomass(i,j,:,:) = biomass(i,j,:,:) * survive(j)

                   !? We are in a section where ok_dgvm is already at TRUE: No need to test it again
                   IF ( ok_dgvm ) THEN
                      ind(i,j) = ind(i,j) * survive(j)
                   ENDIF

                   ! fraction of plants that dies each day. 
                   ! exact formulation: light_death(i,j) = un - survive(j) / dt
                   light_death(i,j) = ( un - survive(j) ) / dt

                ENDIF      ! pft there and natural

             ENDDO        ! loop over pfts

          ENDIF      ! sumfpc > fpc_crit

       ENDDO        ! loop over grid points

       
       !! 2.5 Recalculate fpc for natural PFTs
       !      Recalculate fpc on natural part of the grid cell for next light competition
       DO j = 2,nvm ! loop over #PFT

          !! 2.5.1 Natural PFTs
          IF ( natural(j) ) THEN
 
             !! 2.5.1.1 Trees
             IF ( is_tree(j) ) THEN

                DO i = 1, npts

                   !NVMODIF         
                   !    IF (lai(i,j) == val_exp) THEN
                   !                veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) 
                   !             ELSE
                   !                veget_lastlight(i,j) = &
                   !                     cn_ind(i,j) * ind(i,j) * &
                   !                     MAX( ( un - exp( -lai(i,j) * ext_coeff(j) ) ), min_cover )
                   !             ENDIF
                   !!                veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j)
 
                   IF (lai(i,j) == val_exp) THEN
                      veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) 
                   ELSE
                      veget_lastlight(i,j) = &
                           cn_ind(i,j) * ind(i,j) * &
!JCMODIF
!                           MAX( ( un - EXP( - lm_lastyearmax(i,j) * sla(j) * ext_coeff(j) ) ), min_cover )
                           MAX( ( un - EXP( - lm_lastyearmax(i,j) * sla_calc(i,j) * ext_coeff(j) ) ), min_cover )
!ENDJCMODIF
                   ENDIF
                ENDDO

             ELSE

                !! 2.5.1.2 Grasses
                DO i = 1, npts

                   !NVMODIF         
                   !            IF (lai(i,j) == val_exp) THEN
                   !                veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) 
                   !             ELSE
                   !                veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) * &
                   !                     ( un - exp( -lai(i,j) * ext_coeff(j) ) )
                   !             ENDIF
                   !!veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) 

                   IF (lai(i,j) == val_exp) THEN
                      veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) 
                   ELSE
                      veget_lastlight(i,j) = cn_ind(i,j) * ind(i,j) * &
!JCMODIF
!                           ( un - exp( - lm_lastyearmax(i,j) * sla(j) * ext_coeff(j) ) )
                           ( un - exp( - lm_lastyearmax(i,j) * sla_calc(i,j) * ext_coeff(j) ) )
!ENDJCMODIF
                   ENDIF
                ENDDO
             ENDIF    ! tree/grass

          ELSE

             !! 2.5.2 Agricultural PFTs
             !        Agricultural PFTs are not present on the natural part of the grid point
             veget_lastlight(:,j) = zero

          ENDIF  ! natural/agricultural

       ENDDO ! # PFTs

    ELSE ! ok_dgvm

 !! 3. Light competition in stomate (without DGVM)

       light_death(:,:) = zero

       DO j = 2, nvm 

          IF ( natural(j) ) THEN

	     !! NUMBERING BELOW SHOULD BE 5.0 or 4.3
             !! 2.1.1 natural PFTs, in the one PFT only case there needs to be no special case for grasses,
             !! neither a redistribution of mortality (delta fpc)
             
             !! 3.1 XXX
             WHERE( ind(:,j)*cn_ind(:,j) .GT. min_stomate ) 
!JCMODIF
!                lai_ind(:) = sla(j) * lm_lastyearmax(:,j) / ( ind(:,j) * cn_ind(:,j) )
                lai_ind(:) = sla_calc(:,j) * lm_lastyearmax(:,j) / ( ind(:,j) * cn_ind(:,j) )
!ENDJCMODIF
             ELSEWHERE
                lai_ind(:) = zero
             ENDWHERE

             fpc_nat(:,j) =  cn_ind(:,j) * ind(:,j) * & 
                  MAX( ( un - exp( - ext_coeff(j) * lai_ind(:) ) ), min_cover )

             WHERE(fpc_nat(:,j).GT.fpc_max(:,j))

                light_death(:,j) = MIN(un, un - fpc_max(:,j)/fpc_nat(:,j)) 

             ENDWHERE

             !! 3.2 Update biomass and litter pools
             DO m = 1,nelements
                DO k=1,nparts
                   
                   bm_to_litter(:,j,k,m) = bm_to_litter(:,j,k,m) + light_death(:,j)*biomass(:,j,k,m)
                   biomass(:,j,k,m) = biomass(:,j,k,m) - light_death(:,j)*biomass(:,j,k,m)
                   
                ENDDO
             END DO

             !! 3.3 Update number of individuals
             ind(:,j) = ind(:,j)-light_death(:,j)*ind(:,j)

          ENDIF
       ENDDO

       light_death(:,:) = light_death(:,:)/dt

    ENDIF ! ok_dgvm

   
 !! 4. Write history files
    CALL xios_orchidee_send_field("LIGHT_DEATH",light_death)

    CALL histwrite_p (hist_id_stomate, 'LIGHT_DEATH', itime, &
         light_death, npts*nvm, horipft_index)

    IF (printlev>=4) WRITE(numout,*) 'Leaving light'

  END SUBROUTINE light

END MODULE lpj_light
