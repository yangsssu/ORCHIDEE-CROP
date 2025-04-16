! =================================================================================================================================
! MODULE       : lpj_establish
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        Establish pft's
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	: 
!! - Sitch, S., B. Smith, et al. (2003), Evaluation of ecosystem dynamics,
!!        plant geography and terrestrial carbon cycling in the LPJ dynamic 
!!        global vegetation model, Global Change Biology, 9, 161-185.\n
!! - Haxeltine, A. and I. C. Prentice (1996), BIOME3: An equilibrium
!!        terrestrial biosphere model based on ecophysiological constraints, 
!!        resource availability, and competition among plant functional types,
!!        Global Biogeochemical Cycles, 10(4), 693-709.\n
!! - Smith, B., I. C. Prentice, et al. (2001), Representation of vegetation
!!        dynamics in the modelling of terrestrial ecosystems: comparing two
!!        contrasting approaches within European climate space,
!!        Global Ecology and Biogeography, 10, 621-637.\n
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_establish.f90 $
!! $Date: 2016-03-30 13:33:20 +0200 (Wed, 30 Mar 2016) $
!! $Revision: 3328 $
!! \n
!_ ================================================================================================================================

MODULE lpj_establish

  ! modules used:
  USE xios_orchidee
  USE ioipsl_para
  USE stomate_data
  USE constantes

  IMPLICIT NONE

  ! private & public routines
  PRIVATE
  PUBLIC establish,establish_clear

  LOGICAL, SAVE                          :: firstcall_establish = .TRUE.           !! first call
!$OMP THREADPRIVATE(firstcall_establish)
CONTAINS


!! ================================================================================================================================
!! SUBROUTINE   : fire_clear 
!!
!>\BRIEF       Set the firstcall_establish flag to .TRUE. and activate initialization
!_ ================================================================================================================================

  SUBROUTINE establish_clear
    firstcall_establish = .TRUE.
  END SUBROUTINE establish_clear


! =================================================================================================================================
! SUBROUTINE   : establish 
!
!>\BRIEF       Calculate sstablishment of new woody PFT and herbaceous PFTs
!!
!! DESCRIPTION : Establishments of new woody and herbaceous PFT are simulated. 
!! Maximum establishment rate (0.12) declines due to competition for light (space).
!! There are two establishment estimates: one for the for DGVM and one for the 
!! static cases.\n
!! In the case of DGVM, competitive process of establishment for the area of 
!! available space is represented using more detailed description compared with static 
!! one. Biomass and distribution of plant age are updated on the basis of changes 
!! in number of individuals. Finally excess sapwood of is converted to heartwood.
!!
!! \latexonly
!! \input{equation_lpj_establish.tex}
!! \endlatexonly
!! \n
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)    :
!! Smith, B., I. C. Prentice, et al. (2001), Representation of vegetation
!!    dynamics in the modelling of terrestrial ecosystems: comparing two
!!    contrasting approaches within European climate space,
!!    Global Ecology and Biogeography, 10, 621-637.
!!
!! FLOWCHART       : 
!! \latexonly
!! \includegraphics[scale = 0.7]{establish.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================
 
  SUBROUTINE establish (npts, lalo, dt, PFTpresent, regenerate, &
       neighbours, resolution, need_adjacent, herbivores, &
       precip_annual, gdd0, lm_lastyearmax, &
       cn_ind, lai, avail_tree, avail_grass,  npp_longterm, &
       leaf_age, leaf_frac, &
       ind, biomass, age, everywhere, co2_to_bm,veget_max, woodmass_ind, &
       mortality, bm_to_litter, &
!JCADD
       sla_calc)
!ENDJCADD 

    !! 0. Variable and parameter declaration
    
    !! 0.1 Input variables
    
    INTEGER(i_std), INTENT(in)                                :: npts            !! Domain size - number of pixels (dimensionless)    
    REAL(r_std), INTENT(in)                                   :: dt              !! Time step of vegetation dynamics for stomate 
                                                                                 !! (days)            
    REAL(r_std),DIMENSION(npts,2),INTENT(in)                  :: lalo            !! Geographical coordinates (latitude,longitude) 
                                                                                 !! for pixels (degrees) 
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                  :: PFTpresent      !! Is pft there (unitless)    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: regenerate      !! Winter sufficiently cold (unitless)   
    INTEGER(i_std), DIMENSION(npts,8), INTENT(in)             :: neighbours      !! indices of the 8 neighbours of each grid point 
                                                                                 !! (unitless);  
                                                                                 !! [1=N, 2=NE, 3=E, 4=SE, 5=S, 6=SW, 7=W, 8=NW]  
    REAL(r_std), DIMENSION(npts,2), INTENT(in)                :: resolution      !! resolution at each grid point (m); 1=E-W, 2=N-S     
    LOGICAL, DIMENSION(npts,nvm), INTENT(in)                  :: need_adjacent   !! in order for this PFT to be introduced, does it
                                                                                 !! have to be present in an adjacent grid box?  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: herbivores      !! time constant of probability of a leaf to 
                                                                                 !! be eaten by a herbivore (days)     
    REAL(r_std), DIMENSION(npts), INTENT(in)                  :: precip_annual   !! annual precipitation (mm year^{-1}) 
    REAL(r_std), DIMENSION(npts), INTENT(in)                  :: gdd0            !! growing degree days (degree C)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: lm_lastyearmax  !! last year's maximum leaf mass for each PFT 
                                                                                 !! (gC m^{-2 })
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: cn_ind          !! crown area of individuals (m^2)        
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: lai             !! leaf area index OF an individual plant 
                                                                                 !! (m^2 m^{-2})           
    REAL(r_std), DIMENSION(npts), INTENT(in)                  :: avail_tree      !! space availability for trees (unitless)     
    REAL(r_std), DIMENSION(npts), INTENT(in)                  :: avail_grass     !! space availability for grasses (unitless)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: npp_longterm    !! longterm NPP, for each PFT (gC m^{-2})   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: veget_max       !! "maximal" coverage fraction of a PFT 
                                                                                 !! (LAI -> infinity) on ground (unitless)
    REAL(r_std), DIMENSION(npts,nvm),INTENT(in)               :: mortality       !! Fraction of individual dying this time 
                                                                                 !! step (0 to 1, unitless) 

    !! 0.2 Output variables
    
    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout) :: leaf_age        !! leaf age (days)     
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout) :: leaf_frac       !! fraction of leaves in leaf age class (unitless)    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: ind             !! Number of individuals (individuals m^{-2})          
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout):: biomass   !! biomass (gC m^{-2 })     
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: age             !! mean age (years)       
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: everywhere      !! is the PFT everywhere in the grid box or very 
                                                                                 !! localized (unitless)    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: co2_to_bm       !! biomass up take for establishment i.e. 
                                                                                 !! pseudo-photosynthesis (gC m^{-2} day^{-1}) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: woodmass_ind    !! woodmass of the individual, needed to calculate
                                                                                 !! crownarea in lpj_crownarea (gC m^{-2 })  
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout)    :: bm_to_litter    !!Biomass transfer to litter
!JCADD
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: sla_calc
!ENDJCADD

    !! 0.4 Local variables

    REAL(r_std)                                               :: tau_eatup       !! time during which a sapling can be entirely 
                                                                                 !! eaten by herbivores (days) 
    REAL(r_std), DIMENSION(npts,nvm)                          :: fpc_nat         !! new fpc, foliage projective cover: fractional
                                                                                 !! coverage (unitless)       
    REAL(r_std), DIMENSION(npts)                              :: estab_rate_max_climate_tree  !! maximum tree establishment rate, 
                                                                                              !! based on climate only (unitless)  
    REAL(r_std), DIMENSION(npts)                              :: estab_rate_max_climate_grass !! maximum grass establishment rate,
                                                                                              !! based on climate only (unitless)
    REAL(r_std), DIMENSION(npts)                              :: estab_rate_max_tree          !! maximum tree establishment rate, 
                                                                                              !! based on climate and fpc 
                                                                                              !! (unitless) 
    REAL(r_std), DIMENSION(npts)                              :: estab_rate_max_grass         !! maximum grass establishment rate,
                                                                                              !! based on climate and fpc 
                                                                                              !! (unitless) 
    REAL(r_std), DIMENSION(npts)                              :: sumfpc          !! total natural fpc (unitless)
    REAL(r_std), DIMENSION(npts)                              :: fracnat         !! total fraction occupied by natural 
                                                                                 !! vegetation (unitless)  
    REAL(r_std), DIMENSION(npts)                              :: sumfpc_wood     !! total woody fpc (unitless)    
    REAL(r_std), DIMENSION(npts)                              :: spacefight_grass!! for grasses, measures the total concurrence 
                                                                                 !! for available space (unitless)
    REAL(r_std), DIMENSION(npts,nvm)                          :: d_ind           !! change in number of individuals per time step 
                                                                                 !! (individuals m^{-2} day{-1})         
    REAL(r_std), DIMENSION(npts)                              :: bm_new          !! biomass increase (gC m^{-2 })        
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements)         :: biomass_old     !! Save the original biomass passed into the subroutine 
    REAL(r_std), DIMENSION(npts)                              :: bm_non          !! Non-effective establishment: the "virtual" saplings that die instantly
    REAL(r_std), DIMENSION(npts)                              :: bm_eff          !! Effective (or real) establishment  
    REAL(r_std), DIMENSION(npts)                              :: dia             !! stem diameter (m)    
    REAL(r_std), DIMENSION(npts)                              :: b1              !! temporary variable           
    REAL(r_std), DIMENSION(npts)                              :: woodmass        !! woodmass of an individual (gC m^{-2})  
    REAL(r_std), DIMENSION(npts)                              :: leaf_mass_young !! carbon mass in youngest leaf age class 
                                                                                 !! (gC m^{-2})
    REAL(r_std), DIMENSION(npts)                              :: factor          !! reduction factor for establishment if many 
                                                                                 !! trees or grasses are present (unitless)  
    REAL(r_std), DIMENSION(npts)                              :: total_bm_c      !! Total carbon mass for all pools (gC m^{-2})    
    REAL(r_std), DIMENSION(npts,nelements)                    :: total_bm_sapl   !! Total sappling biomass for all pools 
                                                                                 !! (gC m^{-2})  
    REAL(r_std), DIMENSION(npts,nelements)                    :: total_bm_sapl_non !! total non-effective sapling biomass 

    INTEGER(i_std)                                            :: nfrontx         !! from how many sides is the grid box invaded
                                                                                 !! (unitless?)   
    INTEGER(i_std)                                            :: nfronty         !! from how many sides is the grid box invaded
                                                                                 !! (unitless?)   
    REAL(r_std), DIMENSION(npts)                              :: vn              !! flow due to new individuals veget_max after 
                                                                                 !! establishment, to get a proper estimate of 
                                                                                 !! carbon and nitrogen 
    REAL(r_std), DIMENSION(npts)                              :: lai_ind         !! lai on each PFT surface (m^2 m^{-2})   
    REAL(r_std), DIMENSION(npts)                              :: veget_max_tree  !! Sum of veget_max for the pft's which are trees
    INTEGER(i_std)                                            :: nbtree          !! Number of PFT's which are trees
    INTEGER(i_std)                            :: i,j,k,l,m,ipts,ipart,ilit       !! indices (unitless)       
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering establish'

  !! 1. messages and initialization

    ! Assumption: time durasion that sapling is completely eaten by hervioures is a half year?   
    ! No reference
    tau_eatup = one_year/2.
    
    ! Calculate the sum of the vegetation over the tree pft's and the number of pft's which are trees
    veget_max_tree(:) = 0.0
    nbtree=0
    DO j = 1, nvm 
       IF (is_tree(j)) THEN
          veget_max_tree(:) = veget_max_tree(:) + veget_max(:,j)
          nbtree = nbtree + 1
       END IF
    END DO
    ! Set nbtree=1 to avoid zero division later if there are no tree PFT's.
    ! For that case veget_max_tree=0 so there will not be any impact.
    IF (nbtree == 0) nbtree=1

    !! 1.1 First call only
    IF ( firstcall_establish ) THEN

       WRITE(numout,*) 'establish:'

       WRITE(numout,*) '   > time during which a sapling can be entirely eaten by herbivores (d): ', &
            tau_eatup

       firstcall_establish = .FALSE.

    ENDIF

  !! 2. recalculate fpc

    IF (ok_dgvm) THEN
       fracnat(:) = un

       !! 2.1 Only natural part of the grid cell
       do j = 2,nvm ! Loop over # PFTs
          
          IF ( .NOT. natural(j) ) THEN
             fracnat(:) = fracnat(:) - veget_max(:,j)
          ENDIF
       ENDDO ! Loop over # PFTs
       
       sumfpc(:) = zero

       !! 2.2 Total natural fpc on grid
       !      The overall fractional coverage of a PFT in a grid is calculated here.
       !      FPC is related to mean individual leaf area index by the Lambert-Beer law.
       !      See Eq. (1) in tex file.\n
       DO j = 2,nvm ! Loop over # PFTs
          IF ( natural(j) ) THEN
             WHERE(fracnat(:).GT.min_stomate .AND. lai(:,j) == val_exp) 
                fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:)
             ELSEWHERE(fracnat(:).GT.min_stomate .AND. lai(:,j) /= val_exp) 
                fpc_nat(:,j) = cn_ind(:,j) * ind(:,j) / fracnat(:) & 
!JCMODIF
!                   * ( un - exp( - lm_lastyearmax(:,j) * sla(j) * ext_coeff(j) ) )
                    * ( un - exp( - lm_lastyearmax(:,j) * sla_calc(:,j) * ext_coeff(j) ) )
!ENDJCMODIF
             ENDWHERE

             WHERE ( PFTpresent(:,j) )
                sumfpc(:) = sumfpc(:) + fpc_nat(:,j)
             ENDWHERE
          ELSE

             fpc_nat(:,j) = zero

          ENDIF

       ENDDO ! Loop over # PFTs
       
       !! 2.3 Total woody fpc on grid and number of regenerative tree pfts
       !      Total woody FPC increases by adding new FPC.
       !      Under the condition that temperature in last winter is higher than a threshold, 
       !      woody plants is exposed in higher competitive environment.
       sumfpc_wood(:) = zero

       DO j = 2,nvm ! Loop over # PFTs

          IF ( is_tree(j) .AND. natural(j) ) THEN

             ! total woody fpc
             WHERE ( PFTpresent(:,j) )
                sumfpc_wood(:) = sumfpc_wood(:) + fpc_nat(:,j)
             ENDWHERE

          ENDIF

       ENDDO ! Loop over # PFTs

       !! 2.4 Total number of natural grasses on grid\n
       !     Grass increment equals 'everywhere'\n
       spacefight_grass(:) = zero

       DO j = 2,nvm ! Loop over # PFTs

          IF ( .NOT. is_tree(j) .AND. natural(j) ) THEN

             ! Count a PFT fully only if it is present on a grid.
             WHERE ( PFTpresent(:,j) )
                spacefight_grass(:) = spacefight_grass(:) + everywhere(:,j)
             ENDWHERE

          ENDIF

       ENDDO ! Loop over # PFTs

       !! 2.5 Maximum establishment rate, based on climate only\n
       WHERE ( ( precip_annual(:) .GE. precip_crit ) .AND. ( gdd0(:) .GE. gdd_crit_estab ) )

          estab_rate_max_climate_tree(:) = estab_max_tree ! 'estab_max_*'; see 'stomate_constants.f90'
          estab_rate_max_climate_grass(:) = estab_max_grass

       ELSEWHERE

          estab_rate_max_climate_tree(:) = zero
          estab_rate_max_climate_grass(:) = zero

       ENDWHERE

       !! 2.6 Reduce maximum tree establishment rate if many trees are present.
       !      In the original DGVM, this is done using a step function which yields a
       !      reduction by factor 4 if sumfpc_wood(i) .GT.  fpc_crit - 0.05.
       !      This can lead to small oscillations (without consequences however).
       !      Here, a steady linear transition is used between fpc_crit-0.075 and
       !      fpc_crit-0.025.
       !      factor(:) = 1. - 15. * ( sumfpc_wood(:) - (fpc_crit-.075))
       !      factor(:) = MAX( 0.25_r_std, MIN( 1._r_std, factor(:)))
       !      S. Zaehle modified according to Smith et al. 2001
       !      See Eq. (2) in header
       factor(:)=(un - exp(- establish_scal_fact * (un - sumfpc_wood(:))))*(un - sumfpc_wood(:))
       estab_rate_max_tree(:) = estab_rate_max_climate_tree(:) * factor(:)

       !! 2.7 Modulate grass establishment rate.
       !      If canopy is not closed (fpc < fpc_crit-0.05), normal establishment.
       !      If canopy is closed, establishment is reduced by a factor 4.
       !      Factor is linear between these two bounds.
       !      This is different from the original DGVM where a step function is
       !      used at fpc_crit-0.05 (This can lead to small oscillations,
       !      without consequences however).
       !      factor(:) = 1. - 15. * ( sumfpc(:) - (fpc_crit-.05))
       !      factor(:) = MAX( 0.25_r_std, MIN( 1._r_std, factor(:)))
       !      estab_rate_max_grass(:) = estab_rate_max_climate_grass(:) * factor(:)
       !      S. Zaehle modified to true LPJ formulation, grasses are only allowed in the
       !      fpc fraction not occupied by trees..., 080806
       !      estab_rate_max_grass(:)=MAX(0.98-sumfpc(:),zero)
       !      See Eq. (3) in header
       estab_rate_max_grass(:) = MAX(MIN(estab_rate_max_climate_grass(:), max_tree_coverage - sumfpc(:)),zero)

       !! 2.8 Longterm grass NPP for competition between C4 and C3 grasses
       !      to avoid equal veget_max, the idea is that more reestablishment
       !      is possible for the more productive PFT
       factor(:) = min_stomate
       DO j = 2,nvm ! Loop over # PFTs
          IF ( natural(j) .AND. .NOT.is_tree(j)) & 
               factor(:) = factor(:) + npp_longterm(:,j) * &
!JCMODIF
!               lm_lastyearmax(:,j) * sla(j)
               lm_lastyearmax(:,j) * sla_calc(:,j)
!ENDJCMODIF
       ENDDO ! Loop over # PFTs

       !! 2.9 Establish natural PFTs
       d_ind(:,:) = zero

       DO j = 2,nvm ! Loop over # PFTs

          IF ( natural(j) ) THEN ! only for natural PFTs

             !! 2.9.1 PFT expansion across the grid box. Not to be confused with areal coverage.
             IF ( treat_expansion ) THEN

                ! only treat plants that are regenerative and present and still can expand
                DO i = 1, npts ! Loop over # pixels - domain size

                   IF ( PFTpresent(i,j) .AND. &
                        ( everywhere(i,j) .LT. un ) .AND. &
                        ( regenerate(i,j) .GT. regenerate_crit ) ) THEN

                      ! from how many sides is the grid box invaded (separate x and y directions
                      ! because resolution may be strongly anisotropic)
                      ! For the moment we only look into 4 direction but that can be expanded (JP) 
                      nfrontx = 0
                      IF ( neighbours(i,3) .GT. 0 ) THEN
                         IF ( everywhere(neighbours(i,3),j) .GT. 1.-min_stomate ) nfrontx = nfrontx+1
                      ENDIF
                      IF ( neighbours(i,7) .GT. 0 ) THEN
                         IF ( everywhere(neighbours(i,7),j) .GT. 1.-min_stomate ) nfrontx = nfrontx+1
                      ENDIF

                      nfronty = 0
                      IF ( neighbours(i,1) .GT. 0 ) THEN
                         IF ( everywhere(neighbours(i,1),j) .GT. 1.-min_stomate ) nfronty = nfronty+1
                      ENDIF
                      IF ( neighbours(i,5) .GT. 0 ) THEN
                         IF ( everywhere(neighbours(i,5),j) .GT. 1.-min_stomate ) nfronty = nfronty+1
                      ENDIF
                      
                      everywhere(i,j) = &
                           everywhere(i,j) + migrate(j) * dt/one_year * &
                           ( nfrontx / resolution(i,1) + nfronty / resolution(i,2) )
                      
                      IF ( .NOT. need_adjacent(i,j) ) THEN
                         
                         ! in that case, we also assume that the PFT expands from places within
                         ! the grid box (e.g., oasis).
                         ! What is this equation? No reference.
                         everywhere(i,j) = &
                              everywhere(i,j) + migrate(j) * dt/one_year * &
                              2. * SQRT( pi*everywhere(i,j)/(resolution(i,1)*resolution(i,2)) )

                      ENDIF

                      everywhere(i,j) = MIN( everywhere(i,j), un )

                   ENDIF

                ENDDO ! Loop over # pixels - domain size

             ENDIF ! treat expansion?

             !! 2.9.2 Establishment rate
             !      - Is lower if the PFT is only present in a small part of the grid box
             !        (after its introduction), therefore multiplied by "everywhere".
             !      - Is divided by the number of PFTs that compete ("spacefight").
             !      - Is modulated by space availability (avail_tree, avail_grass).

             !! 2.9.2.1 present and regenerative trees
             IF ( is_tree(j) ) THEN

                WHERE ( PFTpresent(:,j) .AND. ( regenerate(:,j) .GT. regenerate_crit ) )
                   d_ind(:,j) = estab_rate_max_tree(:)*everywhere(:,j) * &
                        avail_tree(:) * dt/one_year
                ENDWHERE

             !! 2.9.2.2 present and regenerative grasses
             ELSE

                WHERE ( PFTpresent(:,j) .AND. ( regenerate(:,j) .GT. regenerate_crit )  & 
                     .AND.factor(:).GT.min_stomate .AND. spacefight_grass(:).GT. min_stomate) 
                   
                   d_ind(:,j) = estab_rate_max_grass(:)*everywhere(:,j)/spacefight_grass(:) * &
                                MAX(min_stomate,npp_longterm(:,j)*lm_lastyearmax(:,j) * &
                                sla_calc(:,j)/factor(:)) * fracnat(:) * dt/one_year
                ENDWHERE
                
             ENDIF  ! tree/grass
             
          ENDIF ! if natural
       ENDDO  ! Loop over # PFTs
       
  !! 3. Lpj establishment in static case 

    !     Lpj establishment in static case, S. Zaehle 080806, account for real LPJ dynamics in
    !     prescribed vegetation, i.e. population dynamics within a given area of the grid cell.
    ELSE !IF(control%ok_dgvm)

       d_ind(:,:) = zero

       DO j = 2,nvm ! Loop over # PFTs

          WHERE(ind(:,j)*cn_ind(:,j).GT.min_stomate)
!JCMODIF
!             lai_ind(:) = sla(j) * lm_lastyearmax(:,j)/(ind(:,j)*cn_ind(:,j))
             lai_ind(:) = sla_calc(:,j) * lm_lastyearmax(:,j)/(ind(:,j)*cn_ind(:,j))
!ENDJCMODIF
          ELSEWHERE
             lai_ind(:) = zero
          ENDWHERE

          !! 3.1 For natural woody PFTs
          IF ( natural(j) .AND. is_tree(j)) THEN 

             ! See Eq. (4) in tex file.            
             fpc_nat(:,j) =  MIN(un, cn_ind(:,j) * ind(:,j) * & 
                  MAX( ( un - exp( - ext_coeff(j) * lai_ind(:) ) ), min_cover ) )


             WHERE (veget_max(:,j).GT.min_stomate.AND.ind(:,j).LE.2.)

                !! 3.1.1 Only establish into growing stands 
                !        Only establish into growing stands, ind can become very
                !        large in the static mode because LAI is very low in poor 
                !        growing conditions, favouring continuous establishment. 
                !        To avoid this a maximum IND is set. BLARPP: This should be
                !        replaced by a better stand density criteria.
                factor(:)=(un - exp(-establish_scal_fact * (un - fpc_nat(:,j))))*(un - fpc_nat(:,j))

                estab_rate_max_tree(:) = estab_max_tree * factor(:) 

                !! 3.1.2 do establishment for natural PFTs\n
                d_ind(:,j) = MAX( zero, estab_rate_max_tree(:) * dt/one_year)

             ENDWHERE

             !S. Zaehle: quickfix: to simulate even aged stand, uncomment the following lines...
             !where (ind(:,j) .LE. min_stomate)
             !d_ind(:,j) = 0.1 !MAX( 0.0, estab_rate_max_tree(:) * dt/one_year)
             WHERE (veget_max(:,j).GT.min_stomate .AND. ind(:,j).EQ.zero)
                d_ind(:,j) = ind_0_estab
             ENDWHERE

          !! 3.2 For natural grass PFTs
          ELSEIF ( natural(j) .AND. .NOT.is_tree(j)) THEN 

             WHERE (veget_max(:,j).GT.min_stomate)

                fpc_nat(:,j) =  cn_ind(:,j) * ind(:,j) * & 
                     MAX( ( un - exp( - ext_coeff(j) * lai_ind(:) ) ), min_cover )

                d_ind(:,j) = MAX(zero , (un - fpc_nat(:,j)) * dt/one_year )

             ENDWHERE

             WHERE (veget_max(:,j).GT.min_stomate .AND. ind(:,j).EQ. zero)
                d_ind(:,j) = ind_0_estab 
             ENDWHERE

          ENDIF

       ENDDO ! Loop over # PFTs

    ENDIF ! DGVM OR NOT

  !! 4. Biomass calculation

    DO j = 2,nvm ! Loop over # PFTs

       IF ( natural(j) ) THEN ! only for natural PFTs

          !! 4.1 Herbivores reduce establishment rate
          !      We suppose that saplings are vulnerable during a given time after establishment.
          !      This is taken into account by preventively reducing the establishment rate.
          IF ( ok_herbivores ) THEN

             d_ind(:,j) = d_ind(:,j) * EXP( - tau_eatup/herbivores(:,j) )

          ENDIF

          !! 4.2 Total biomass.
          !      Add biomass only if d_ind, over one year, is of the order of ind.
          !      save old leaf mass to calculate leaf age
          leaf_mass_young(:) = leaf_frac(:,j,1) * biomass(:,j,ileaf,icarbon)

          ! total biomass of existing PFT to limit biomass added from establishment
          total_bm_c(:) = zero

          DO k = 1, nparts
             total_bm_c(:) = total_bm_c(:) + biomass(:,j,k,icarbon)
          ENDDO
          IF(ok_dgvm) THEN
             vn(:) = veget_max(:,j)
          ELSE
             vn(:) = un
          ENDIF

          !! 4.3 Woodmass calculation

          !! 4.3.1 with DGVM
          IF(ok_dgvm) THEN

             ! S. Zaehle calculate new woodmass_ind and veget_max after establishment (needed for correct scaling!)
             ! essential correction for MERGE!
             IF(is_tree(j))THEN
                DO i=1,npts ! Loop over # pixels - domain size
                   IF((d_ind(i,j)+ind(i,j)).GT.min_stomate) THEN

                      IF((total_bm_c(i).LE.min_stomate) .OR. (veget_max(i,j) .LE. min_stomate)) THEN

                         ! new wood mass of PFT
                         woodmass_ind(i,j) = &
                              (((biomass(i,j,isapabove,icarbon) + biomass(i,j,isapbelow,icarbon) &
                              + biomass(i,j,iheartabove,icarbon) + biomass(i,j,iheartbelow,icarbon))*veget_max(i,j)) &
                              + (bm_sapl(j,isapabove,icarbon) + bm_sapl(j,isapbelow,icarbon) &
                              + bm_sapl(j,iheartabove,icarbon) + bm_sapl(j,iheartbelow,icarbon))*d_ind(i,j))/(ind(i,j) + d_ind(i,j))

                      ELSE
 
                         ! new biomass is added to the labile pool, hence there is no change 
                         ! in CA associated with establishment
                         woodmass_ind(i,j) = &
                              & (biomass(i,j,isapabove,icarbon) + biomass(i,j,isapbelow,icarbon) &
                              & +biomass(i,j,iheartabove,icarbon) + biomass(i,j,iheartbelow,icarbon))*veget_max(i,j) &
                              & /(ind(i,j) + d_ind(i,j))

                      ENDIF

                      ! new diameter of PFT
                      dia(i) = (woodmass_ind(i,j)/(pipe_density*pi/4.*pipe_tune2)) &
                           &                **(1./(2.+pipe_tune3))
                      vn(i) = (ind(i,j) + d_ind(i,j))*pipe_tune1*MIN(dia(i),maxdia(j))**pipe_tune_exp_coeff

                   ENDIF
                ENDDO ! Loop over # pixels - domain size
             ELSE ! for grasses, cnd=1, so the above calculation cancels
                vn(:) = ind(:,j) + d_ind(:,j)
             ENDIF

          !! 4.3.2 without DGVM (static)\n
          ELSE 
             DO i=1,npts ! Loop over # pixels - domain size
                IF(is_tree(j).AND.(d_ind(i,j)+ind(i,j)).GT.min_stomate) THEN
                   IF(total_bm_c(i).LE.min_stomate) THEN

                      ! new wood mass of PFT
                      woodmass_ind(i,j) = &
                           & (((biomass(i,j,isapabove,icarbon) + biomass(i,j,isapbelow,icarbon) &
                           & + biomass(i,j,iheartabove,icarbon) + biomass(i,j,iheartbelow,icarbon))) &
                           & + (bm_sapl(j,isapabove,icarbon) + bm_sapl(j,isapbelow,icarbon) &
                           & + bm_sapl(j,iheartabove,icarbon) + bm_sapl(j,iheartbelow,icarbon))*d_ind(i,j))/(ind(i,j)+d_ind(i,j))

                   ELSE
 
                      ! new biomass is added to the labile pool, hence there is no change 
                      ! in CA associated with establishment
                      woodmass_ind(i,j) = &
                           & (biomass(i,j,isapabove,icarbon) + biomass(i,j,isapbelow,icarbon) &
                           & + biomass(i,j,iheartabove,icarbon) + biomass(i,j,iheartbelow,icarbon)) &
                           & /(ind(i,j) + d_ind(i,j))

                   ENDIF
                ENDIF
             ENDDO ! Loop over # pixels - domain size

             vn(:) = un ! cannot change in static!, and veget_max implicit in d_ind

          ENDIF

          !! 4.4 total biomass of PFT added by establishment defined over veget_max ...
          total_bm_sapl(:,:) = zero
          total_bm_sapl_non(:,:) = zero
          biomass_old(:,j,:,:)=biomass(:,j,:,:)
          DO k = 1, nparts ! Loop over # litter tissues (nparts=8); see 'stomate_constants.f90'
             WHERE(d_ind(:,j).GT.min_stomate.AND.total_bm_c(:).GT.min_stomate.AND.vn(:).GT.min_stomate)

                total_bm_sapl(:,icarbon) = total_bm_sapl(:,icarbon) + & 
                     bm_sapl(j,k,icarbon) * d_ind(:,j) / vn(:)
                ! non-effective establishment
                total_bm_sapl_non(:,icarbon) = total_bm_sapl_non(:,icarbon) + &
                     bm_sapl(j,k,icarbon) * (ind(:,j)+d_ind(:,j))*mortality(:,j) / vn(:)

             ENDWHERE
          ENDDO ! Loop over # litter tissues

!Dan Zhu modification: there is a problem here, if DGVM is activated, co2_to_bm will never reach
!0 due to establishment, where d_ind is still large at equilibrium (=ind*mortality). So we
!need to subtract it from litter (not biomass, because the
!corresponding biomass has been lost in lpj_gap).

          !! 4.5 Update biomass at each component
          DO k = 1, nparts ! Loop over # litter tissues

             bm_new(:) = zero
             bm_non(:) = zero
             bm_eff(:) = zero

             ! first ever establishment, C flows
             DO l=1, npts
                IF( d_ind(l,j).GT.min_stomate .AND. &
                   total_bm_c(l).LE.min_stomate .AND. &
                   vn(l).GT.min_stomate) THEN

                   bm_new(l) = d_ind(l,j) * bm_sapl(j,k,icarbon) / vn(l)
                   biomass(l,j,k,icarbon) = biomass(l,j,k,icarbon) + bm_new(l)

                   ! bm_to_litter minus the 'non-effective' establishment (mortality), but cannot be less than 0
                   IF ((veget_max_tree(l) .GT. 0.1) .AND. (veget_max(l,j) .LT. veget_max_tree(l)/nbtree) ) THEN

                      bm_non(l) = MIN( biomass(l,j,k,icarbon)+bm_to_litter(l,j,k,icarbon), &
                        (ind(l,j)+d_ind(l,j))*mortality(l,j) * bm_sapl(j,k,icarbon)/vn(l) )
                      bm_eff(l) = MIN( npp_longterm(l,j)/one_year, bm_new(l)-bm_non(l) )
                      bm_non(l) = MIN( biomass(l,j,k,icarbon)+bm_to_litter(l,j,k,icarbon), bm_new(l)-bm_eff(l) )

                      co2_to_bm(l,j)=co2_to_bm(l,j) + bm_new(l) - bm_non(l)
                      IF ( bm_to_litter(l,j,k,icarbon) .LT. bm_non(l) ) THEN
                        biomass(l,j,k,icarbon) = biomass(l,j,k,icarbon) - ( bm_non(l) - bm_to_litter(l,j,k,icarbon) )
                      ENDIF
                      bm_to_litter(l,j,k,icarbon) = bm_to_litter(l,j,k,icarbon) - MIN(bm_to_litter(l,j,k,icarbon), bm_non(l) )

                   ELSE

                      bm_non(l) = MIN( bm_to_litter(l,j,k,icarbon), (ind(l,j)+d_ind(l,j))*mortality(l,j) * bm_sapl(j,k,icarbon)/vn(l) )
                      co2_to_bm(l,j)=co2_to_bm(l,j) + bm_new(l)/dt - bm_non(l)/dt
                      bm_to_litter(l,j,k,icarbon)=bm_to_litter(l,j,k,icarbon)- bm_non(l)
                  ENDIF

                ENDIF
             ENDDO

             ! establishment into existing population, C flows
             DO ipts=1, npts
                IF (d_ind(ipts,j).GT.min_stomate.AND.total_bm_c(ipts).GT.min_stomate) THEN

                   bm_new(ipts) = total_bm_sapl(ipts,icarbon) * biomass_old(ipts,j,k,icarbon) / total_bm_c(ipts)
                   biomass(ipts,j,k,icarbon) = biomass(ipts,j,k,icarbon) + bm_new(ipts)

                   IF ((veget_max_tree(ipts) .GT. 0.1) .AND. (veget_max(ipts,j) .LT. veget_max_tree(ipts)/nbtree) ) THEN
                      bm_non(ipts) = MIN( biomass(ipts,j,k,icarbon)+bm_to_litter(ipts,j,k,icarbon), &
                           total_bm_sapl_non(ipts,icarbon) *biomass_old(ipts,j,k,icarbon)/total_bm_c(ipts) )
                      bm_eff(ipts) = MIN( npp_longterm(ipts,j)/one_year, bm_new(ipts)-bm_non(ipts) )
                      bm_non(ipts) = MAX( zero, MIN( biomass(ipts,j,k,icarbon)+bm_to_litter(ipts,j,k,icarbon)-min_stomate, &
                           bm_new(ipts)-bm_eff(ipts) ) )

                      co2_to_bm(ipts,j)=co2_to_bm(ipts,j) + bm_new(ipts) - bm_non(ipts)
                      IF ( bm_to_litter(ipts,j,k,icarbon) .LT. bm_non(ipts) ) THEN
                         biomass(ipts,j,k,icarbon) = biomass(ipts,j,k,icarbon) - ( bm_non(ipts) - bm_to_litter(ipts,j,k,icarbon) )
                      ENDIF
                      bm_to_litter(ipts,j,k,icarbon) = bm_to_litter(ipts,j,k,icarbon) - MIN(bm_to_litter(ipts,j,k,icarbon), bm_non(ipts) )
  
                   ELSE

                      bm_non(ipts) = MIN( bm_to_litter(ipts,j,k,icarbon), total_bm_sapl_non(ipts,icarbon) *biomass_old(ipts,j,k,icarbon)/total_bm_c(ipts) )
                      co2_to_bm(ipts,j) = co2_to_bm(ipts,j) + bm_new(ipts)/dt - bm_non(ipts)/dt
                      bm_to_litter(ipts,j,k,icarbon)=bm_to_litter(ipts,j,k,icarbon)- bm_non(ipts)
                   ENDIF

                ENDIF
             ENDDO
             
          ENDDO ! Loop over # litter tissues

          DO ipts = 1,npts
            DO ipart = 1,nparts
              IF (bm_to_litter(ipts,j,ipart,icarbon) .LT. 0.0 ) THEN
                WRITE(numout,*) 'Negative bm_to_litter at lat', lalo(ipts,1), 'lon', lalo(ipts,2)
                WRITE(numout,*) 'PFT number', j , 'biomass part', ipart 
                WRITE(numout,*) 'ipts', ipts
                CALL ipslerr_p(3,'establish','something wrong in establish/gap.','','')
              ENDIF
              IF (biomass(ipts,j,ipart,icarbon) .LT. 0.0 ) THEN
                WRITE(numout,*) 'Negative biomass at lat', lalo(ipts,1), 'lon', lalo(ipts,2)
                WRITE(numout,*) 'PFT number', j , 'biomass part', ipart 
                WRITE(numout,*) 'ipts', ipts
                CALL ipslerr_p(3,'establish','something wrong in establish/gap.','','')
              ENDIF
            END DO 
          END DO

          !! 4.6 Decrease leaf age in youngest class if new leaf biomass is higher than old one.
          WHERE ( d_ind(:,j) * bm_sapl(j,ileaf,icarbon) .GT. min_stomate )
 
             ! reset leaf ages. Should do a real calculation like in the npp routine, 
             ! but this case is rare and not worth messing around.
             ! S. Zaehle 080806, added real calculation now, because otherwise leaf_age/leaf_frac
             ! are not initialised for the calculation of vmax, and hence no growth at all.
             ! logic follows that of stomate_npp.f90, just that it's been adjusted for the code here
             leaf_age(:,j,1) = leaf_age(:,j,1) * leaf_mass_young(:) / &
                  ( leaf_mass_young(:) + d_ind(:,j) * bm_sapl(j,ileaf,icarbon) )

          ENDWHERE

          leaf_mass_young(:) = leaf_mass_young(:) + d_ind(:,j) * bm_sapl(j,ileaf,icarbon)   

          !! 4.7 Youngest class: new mass in youngest class divided by total new mass
          WHERE ( biomass(:,j,ileaf,icarbon) .GT. min_stomate )
             ! new age class fractions (fraction in youngest class increases)
             leaf_frac(:,j,1) = leaf_mass_young(:) / biomass(:,j,ileaf,icarbon)

          ENDWHERE

          !! 4.8 Other classes: old mass in leaf age class divided by new mass
          DO m = 2, nleafages

             WHERE ( biomass(:,j,ileaf,icarbon) .GT. min_stomate )

                leaf_frac(:,j,m) = leaf_frac(:,j,m) * & 
                     ( biomass(:,j,ileaf,icarbon) + d_ind(:,j) * bm_sapl(j,ileaf,icarbon) ) /  biomass(:,j,ileaf,icarbon)

             ENDWHERE

          ENDDO

          !! 4.9 Update age and number of individuals
          WHERE ( d_ind(:,j) .GT. min_stomate )

             age(:,j) = age(:,j) * ind(:,j) / ( ind(:,j) + d_ind(:,j) )

             ind(:,j) = ind(:,j) + d_ind(:,j)

          ENDWHERE

          !! 4.10 Convert excess sapwood to heartwood
          !!      No longer done : supressed by S. Zaehle given that the LPJ logic of carbon allocation was 
          !!      contradictory to SLAVE allocation. See CVS tag 1_5 for initial formulation. 


       ENDIF ! natural

    ENDDO ! Loop over # PFTs

  !! 5. history

    d_ind = d_ind / dt

    CALL xios_orchidee_send_field("IND_ESTAB",d_ind)
    CALL xios_orchidee_send_field("ESTABTREE",estab_rate_max_tree)
    CALL xios_orchidee_send_field("ESTABGRASS",estab_rate_max_grass)

    CALL histwrite_p (hist_id_stomate, 'IND_ESTAB', itime, d_ind, npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'ESTABTREE', itime, estab_rate_max_tree, npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'ESTABGRASS', itime, estab_rate_max_grass, npts, hori_index)
!!DZADD
!    CALL histwrite_p (hist_id_stomate, 'EST_LEAF_FRAC1', itime, leaf_frac(:,:,1), npts*nvm, horipft_index)
!    CALL histwrite_p (hist_id_stomate, 'EST_LEAF_FRAC2', itime, leaf_frac(:,:,2), npts*nvm, horipft_index)
!    CALL histwrite_p (hist_id_stomate, 'EST_LEAF_FRAC3', itime, leaf_frac(:,:,3), npts*nvm, horipft_index)
!    CALL histwrite_p (hist_id_stomate, 'EST_LEAF_FRAC4', itime, leaf_frac(:,:,4), npts*nvm, horipft_index)
!!ENDDZADD

    IF (printlev>=4) WRITE(numout,*) 'Leaving establish'

  END SUBROUTINE establish

END MODULE lpj_establish
