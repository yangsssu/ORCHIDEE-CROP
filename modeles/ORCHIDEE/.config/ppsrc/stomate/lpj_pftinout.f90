










! =================================================================================================================================
! MODULE       : lpj_pftinout
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Introduce and eliminate PFT's from pixel
!!
!! \n DESCRIPTION: None
!!
!! RECENT CHANGE(S) : None 
!!
!! REFERENCE(S) :
!! - Sitch, S., B. Smith, et al. (2003), Evaluation of ecosystem dynamics,
!!        plant geography and terrestrial carbon cycling in the LPJ dynamic 
!!        global vegetation model, Global Change Biology, 9, 161-185.\n
!! - Thonicke, K., S. Venevsky, et al. (2001), The role of fire disturbance 
!!        for global vegetation dynamics: coupling fire into a Dynamic Global
!!        Vegetation Model, Global Ecology and Biogeography, 10, 661-677.\n
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
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_pftinout.f90 $
!! $Date: 2015-11-16 14:26:03 +0100 (Mon, 16 Nov 2015) $
!! $Revision: 3026 $
!! \n
!_ ==============================================================================================================================

MODULE lpj_pftinout

  ! modules used:

  USE ioipsl_para
  USE stomate_data
  USE pft_parameters
  USE constantes

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC pftinout,pftinout_clear

  LOGICAL, SAVE                       :: firstcall_pftinout = .TRUE.                !! first call
!$OMP THREADPRIVATE(firstcall_pftinout)

CONTAINS


!! ================================================================================================================================
!! SUBROUTINE  : pftinout_clear
!!
!>\BRIEF       Set flag ::firstcall_pftinout to true and initialize the variables
!_ ================================================================================================================================

  SUBROUTINE pftinout_clear
    firstcall_pftinout = .TRUE.
  END SUBROUTINE pftinout_clear

!! ================================================================================================================================ 
!! SUBROUTINE  : pftinout
!! 
!>\BRIEF       Introduce and eliminate PFT's from pixel
!!
!! DESCRIPTION**3 : Introduction and elimination of PFTs on the basis of climate condition.
!! For natural and woody PFTs the foliage projected coverage is calculated as follows:
!! \latexonly
!!  \input{equation_lpj_pftinout.tex}
!! \endlatexonly
!! \n
!! where FPC is foliage projective cover (::fpc_nat), CN crown area (::cn_ind, 
!! @tex $ m^{2} $ @endtex), IND number of individuals (::ind, @tex $ m^{-2} $ @endtex,
!! FRAC total fraction occupied by natural vegetation (::fracnat),
!! @tex $ LM_{rm max} $ @endtex maximum leaf mass in last year
!! (::lm_lastyearmax, @tex $ g C m^{-2} $ @endtex), SLA specific leaf area (sla,
!! @tex $ m^{2} (g C)^{-1} $ @endtex), and coff coefficient (::ext_coeff). ::ext_coeff 
!! describes a property of the canopy (i.e. law of Lambert-Beer) and is defined in **2
!!
!! The foliage projective cover feeds into the calculation of the space available for 
!! expansion of existing and dispersion of new PFTs within a gridbox. In turn, available
!! space is use to calculate the number of individuals with a PFT. 
!!
!! Saplings are introduced under the condition that winter temperature is 
!! moderate, plant age is older than 1.25, (and for some PFTs at least one adjacent grid 
!! box exists for expansion), new saplings are introduced for narural PFT. In the simulation of 
!! agricultural grassland, if target PFT does not exist in the gridbox, it is introduced 
!! regardless of climate condition. When a new PFT is introduced CO_2 is taken from the
!! atmosphere to account for CO_2 present in the seed and required by the germinated seeds
!! to establish a sapling. These initial phases in ontology are not accounted for. However,
!! by taking this small amount of CO2 from the atmosphere, mass balance closure for C is
!! preserved. 
!!
!! PFTs are eliminated under the condition that they are no longer adapted to the critical 
!! temperatures in winter. When a PFT is eliminated its number of indiviuals is set to zero and
!! the rest of the elimination process is taken care of in lpj_kill.f90.
!! 
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): :: avail_tree (space availability for trees, unitless),   
!! :: avail_grass (space availability for grasses, unitless), :: biomass (biomass, \f$gC m^{-2}\f$) 
!! and :: ind (density of individuals, \f$m^{-2}\f$)    
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    :
!! \latexonly
!!   \includegraphics[scale = 0.6]{pftinout.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================






  SUBROUTINE pftinout (npts, dt, adapted, regenerate, &
       neighbours, veget_max, &
       biomass, ind, cn_ind, age, leaf_frac, npp_longterm, lm_lastyearmax, senescence, &
       PFTpresent, everywhere, when_growthinit, need_adjacent, RIP_time, &
       co2_to_bm, &
       avail_tree, avail_grass, &!)
!JCADD
       sla_calc)
!ENDJCADD
    
  !! 0. Variable and parameter declaration
    
    !! 0.1 Input variables
    
    INTEGER(i_std), INTENT(in)                                :: npts            !! Domain size - number of pixels (unitless)            
    REAL(r_std), INTENT(in)                                   :: dt              !! Time step of vegetation dynamics for stomate
                                                                                 !! (days)               
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: adapted         !! Winter not too cold (unitless)   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: regenerate      !! Winter sufficiently cold (unitless)  
    INTEGER(i_std), DIMENSION(npts,8), INTENT(in)             :: neighbours      !! Indices of the 8 neighbours of each grid point
                                                                                 !! (unitless); 1=N, 2=NE, 3=E, 4=SE, 5=S, 6=SW, 
                                                                                 !! 7=W, 8=NW       
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: veget_max       !! "maximal" coverage fraction of a PFT (LAI ->
                                                                                 !! infinity) on ground (unitless)        

    !! 0.2 Output variables

    REAL(r_std), DIMENSION(npts), INTENT(out)                 :: avail_tree      !! Space availability for trees (unitless)   
    REAL(r_std), DIMENSION(npts), INTENT(out)                 :: avail_grass     !! Space availability for grasses (unitless)      


    !! 0.3 Modified variables   

    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: biomass  !! Biomass (gC m^{-2})      
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: ind             !! Density of individuals (m^{-2})
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)              :: cn_ind          !! Crown area of individuals (m^2)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: age             !! Mean age (years)            
    REAL(r_std), DIMENSION(npts,nvm,nleafages), INTENT(inout) :: leaf_frac       !! Fraction of leaves in leaf age class (unitless)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: npp_longterm    !! "long term" net primary productivity 
                                                                                 !! (gC m^{-2} year^{-1})   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: lm_lastyearmax  !! Last year's maximum leaf mass, for each PFT 
                                                                                 !! (gC m^{-2})  
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)               :: senescence      !! Plant senescent for deciduous trees; .FALSE. 
                                                                                 !! if PFT is introduced or killed      
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)               :: PFTpresent      !! PFT exists    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: everywhere      !! is the PFT everywhere in the grid box or very 
                                                                                 !! localized (unitless)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: when_growthinit !! how many days ago was the beginning of the 
                                                                                 !! growing season (days) 
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)               :: need_adjacent   !! in order for this PFT to be introduced, does it
                                                                                 !! have to be present in an adjacent grid box?   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: RIP_time        !! How much time ago was the PFT eliminated for 
                                                                                 !! the last time (years)    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: co2_to_bm       !! biomass uptaken (gC m^{-2} day^{-1})        

!JCADD
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)           :: sla_calc
!ENDJCADD
    !! 0.4 Local variables

    REAL(r_std), DIMENSION(npts)                              :: avail           !! availability           
    INTEGER(i_std)                                            :: i,j,m           !! indices      
    REAL(r_std), DIMENSION(npts)                              :: sumfrac_wood    !! total woody vegetation cover     
    INTEGER(i_std), DIMENSION(npts)                           :: n_present       !! number of adjacent grid cells where PFT is 
                                                                                 !! ubiquitous        
    LOGICAL, DIMENSION(npts)                                  :: can_introduce   !! we can introduce this PFT    
    REAL(r_std), DIMENSION(npts)                              :: fracnat         !! no real need for dimension(ntps) except for 
                                                                                 !! vectorisation          
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering pftinout'

  !! 1. Messages    

    IF ( firstcall_pftinout ) THEN

       WRITE(numout,*) 'pftinout: Minimum space availability: ', min_avail

       firstcall_pftinout = .FALSE.

    ENDIF

  !! 2. Total woody fpc and space avaibility on grid

    ! Only natural part of the grid cell\n
    ! S. Zaehle bug correction MERGE: need to subtract agricultural area!
    ! fraction of agricultural surface

    !! 2.1 only natural PFT
    fracnat(:) = un
    DO j = 2,nvm ! Loop over # PFTs
       IF ( .NOT. natural(j) ) THEN
          fracnat(:) = fracnat(:) - veget_max(:,j)
       ENDIF
    ENDDO ! Loop over # PFTs

    !! 2.2 Total woody fractional plant cover
    sumfrac_wood(:) = zero
    DO j = 2,nvm ! Loop over # PFTs
       
       ! S. Zaehle problem here: agriculture, not convinced that this representation of LPJ is correct
       ! if agriculture is present, ind must be recalculated to correspond to the natural density...
       ! since ind is per grid cell, can be achived by discounting for agricultura fraction
       IF ( natural(j).AND.is_tree(j) ) THEN
          WHERE(fracnat(:).GT.min_stomate) 
                sumfrac_wood(:) = sumfrac_wood(:) + cn_ind(:,j) * ind(:,j) / fracnat(:) &
!JCMODIF 
!                     * ( un - exp( - lm_lastyearmax(:,j) * sla(j) * ext_coeff(j) ) )
                     * ( un - exp( - lm_lastyearmax(:,j) * sla_calc(:,j) * ext_coeff(j) ) )
!ENDJCMODIF
                !lai changed to lm_last
          ENDWHERE
       ENDIF
    ENDDO ! Loop over # PFTs

    !! 2.3 Space availability
    avail_grass(:) = MAX( ( un - sumfrac_wood(:) ), min_avail )
    avail_tree(:) = MAX( ( fpc_crit - sumfrac_wood(:) ), min_avail )

  !! 3. Time since last elimination (y)

    RIP_time = RIP_time + dt / one_year

  !! 4. Agicultural PFTs

    ! Agricultural PFTs are only present if they are prescribed
    DO j = 2,nvm ! Loop over # PFTs
       
       IF ( .NOT. natural(j) ) THEN
          
          IF (printlev>=4) WRITE(numout,*) 'pftinout: Agricultural PFTs'
          
          !! 4.1 Agricultural trees
          !      Agricultural trees are not treated for the moment
          IF ( is_tree(j) ) THEN

             CALL ipslerr_p(3,'pftinout','Agricultural trees not treated.','','')

          !! 4.2 Initialization of agricultural grass lands
          !      Initialize parameter values of prescribed agricultural PFTs
          ELSE

             DO i = 1, npts ! Loop over # pixels - domain size

                IF ( ( veget_max(i,j) .GT. min_stomate ) .AND. ( .NOT. PFTpresent(i,j) ) ) THEN

                   ! prescribed, but not yet there.
                   ind(i,j) = veget_max(i,j)
                   biomass(i,j,:,:) = bm_sapl(j,:,:) * ind(i,j) /veget_max(i,j) 
                   co2_to_bm(i,j) =  co2_to_bm(i,j) +SUM( biomass(i,j,:,icarbon) ) / dt
                   PFTpresent(i,j) = .TRUE.
                   everywhere(i,j) = un
                   senescence(i,j) = .FALSE.
                   age(i,j) = zero

                ENDIF  ! prescribed, but PFT not yet present

             ENDDO ! Loop over # pixels - domain size

          ENDIF

       ENDIF ! not natural

    ENDDO ! Loop over # PFTs

  !! 5 Eliminate PFTs

    DO j = 2,nvm ! Loop over # PFTs

       ! only for natural PFTs
       IF ( natural(j) ) THEN

          ! Number of individuals are set to zero in the condition of cold winter   
          ! 'adapted_crit' critical value for being adapted = 1.-(1./euler); see 'stomate_constants.f90'
          WHERE (  PFTpresent(:,j) .AND. ( adapted(:,j) .LT. adapted_crit ) )

             ! PFT there, but not adapted any more (ex: winter too cold): kill
             ! set number of individuals to zero - rest will be done in lpj_kill
             ind(:,j) = zero

          ENDWHERE

       ENDIF ! natural

    ENDDO ! Loop over # PFTs

  !! 6. Introduce PFTs

    DO j = 2,nvm ! Loop over # PFTs

       IF ( natural(j) ) THEN

          ! space availability for this PFT
          IF ( is_tree(j) ) THEN
             avail(:) = avail_tree(:)
          ELSE
             avail(:) = avail_grass(:)
          ENDIF
          
          !! 6.1 Check if PFT not present but (adapted and regenerative)     
          can_introduce(:) = .FALSE.
          
          DO i = 1, npts ! Loop over # pixels - domain size

             IF ( .NOT. PFTpresent(i,j) .AND. &
                  ( adapted(i,j) .GT. adapted_crit ) .AND. &
                  ( regenerate(i,j) .GT. regenerate_crit )  ) THEN

                ! Seed are available nearby
                IF ( need_adjacent(i,j) ) THEN

                   !! 6.1.1 Climate allows introduction of the PFT but dispersion requires the 
                   !        presence of seeds. Seed are considered available if at least one
                   !        neighbouring pixel is entirely invaded by the PFT. If that condition is
                   !        satisfied, the PFT can establish in the new pixel.
                   ! Count number of totally invaded neighbours.
                   ! no loop so that it can vectorize
                   n_present(i) = 0
                   IF ( neighbours(i,1) .GT. 0 ) THEN
                      IF ( everywhere(neighbours(i,1),j) .GE. un-min_stomate ) THEN
                         n_present(i) = n_present(i)+1
                      ENDIF
                   ENDIF
                   IF ( neighbours(i,3) .GT. 0 ) THEN
                      IF ( everywhere(neighbours(i,3),j) .GE. un-min_stomate ) THEN
                         n_present(i) = n_present(i)+1
                      ENDIF
                   ENDIF
                   IF ( neighbours(i,5) .GT. 0 ) THEN
                      IF ( everywhere(neighbours(i,5),j) .GE. un-min_stomate ) THEN
                         n_present(i) = n_present(i)+1
                      ENDIF
                   ENDIF
                   IF ( neighbours(i,7) .GT. 0 ) THEN
                      IF ( everywhere(neighbours(i,7),j) .GE. un-min_stomate ) THEN
                         n_present(i) = n_present(i)+1
                      ENDIF
                   ENDIF

                   IF ( n_present(i) .GT. 0 ) THEN

                      ! PFT is ubiquitous in at least one adjacent grid box
                      can_introduce(i) = .TRUE.

                   ENDIF

                ELSE

                   !! 6.1.2 No seed (trees) required for dispersion
                   !        The PFT can establish without the presence of seed trees in 
                   !        neighbouring pixels.
                   can_introduce(i) = .TRUE.

                ENDIF ! do we have to look at the neighbours?

             ENDIF ! we'd like to introduce the PFT

          ENDDO ! Loop over # pixels - domain size 

          !! 6.2 Has the PFT been eliminated lately?
          !      Additional test whether the PFT has been eliminated lately, i.e.
          !      less than 1.25 years ago. Do not only take full years as success of
          !      introduction, as introduction might depend on season.
          WHERE ( RIP_time(:,j) .LT. RIP_time_min )

             ! PFT was eliminated lately - cannot reintroduce
             can_introduce(:) = .FALSE.

          ENDWHERE

          !! 6.3 Introduce that PFT where possible
          !      "can_introduce" means that it either exists in neighbouring grid boxes 
          !      or that we do not look at neighbours, that it has not been eliminated 
          !      lately, and, of course, that the climate is good for that PFT.
          WHERE ( can_introduce(:) )
             
             PFTpresent(:,j) = .TRUE.
             
             senescence(:,j) = .FALSE.
             
             ! introduce at least a few saplings, even if canopy is closed
             ! initial density of individuals (ind_0) = 0.02, see 'stomate_constant.f90'
             ind(:,j) = ind_0 * (dt/one_year) * avail(:)
             
             WHERE(veget_max(:,j) .GT. min_stomate)
                biomass(:,j,ileaf,icarbon) = bm_sapl(j,ileaf,icarbon) * ind(:,j) /veget_max(:,j)
                biomass(:,j,isapabove,icarbon) = bm_sapl(j,isapabove,icarbon) * ind(:,j) /veget_max(:,j)
                biomass(:,j,isapbelow,icarbon) = bm_sapl(j,isapbelow,icarbon) * ind(:,j)/veget_max(:,j)
                biomass(:,j,iheartabove,icarbon) = bm_sapl(j,iheartabove,icarbon) * ind(:,j)/veget_max(:,j)
                biomass(:,j,iheartbelow,icarbon) = bm_sapl(j,iheartbelow,icarbon) * ind(:,j)/veget_max(:,j)
                biomass(:,j,iroot,icarbon) = bm_sapl(j,iroot,icarbon) * ind(:,j)/veget_max(:,j)
                biomass(:,j,ifruit,icarbon) = bm_sapl(j,ifruit,icarbon) * ind(:,j)/veget_max(:,j)
                biomass(:,j,icarbres,icarbon) = bm_sapl(j,icarbres,icarbon) * ind(:,j)/veget_max(:,j)
             ELSEWHERE              
                biomass(:,j,ileaf,icarbon) = bm_sapl(j,ileaf,icarbon) * ind(:,j)
                biomass(:,j,isapabove,icarbon) = bm_sapl(j,isapabove,icarbon) * ind(:,j)
                biomass(:,j,isapbelow,icarbon) = bm_sapl(j,isapbelow,icarbon) * ind(:,j)
                biomass(:,j,iheartabove,icarbon) = bm_sapl(j,iheartabove,icarbon) * ind(:,j)
                biomass(:,j,iheartbelow,icarbon) = bm_sapl(j,iheartbelow,icarbon) * ind(:,j)
                biomass(:,j,iroot,icarbon) = bm_sapl(j,iroot,icarbon) * ind(:,j)
                biomass(:,j,ifruit,icarbon) = bm_sapl(j,ifruit,icarbon) * ind(:,j)
                biomass(:,j,icarbres,icarbon) = bm_sapl(j,icarbres,icarbon) * ind(:,j)
             END WHERE
            
             co2_to_bm(:,j) = &
                  co2_to_bm(:,j) +  &
                  ( biomass(:,j,ileaf,icarbon) + biomass(:,j,isapabove,icarbon) + &
                  biomass(:,j,isapbelow,icarbon) + biomass(:,j,iheartabove,icarbon) + &
                  biomass(:,j,iheartbelow,icarbon) + biomass(:,j,iroot,icarbon) + &
                  biomass(:,j,ifruit,icarbon) + biomass(:,j,icarbres,icarbon) )/dt

             when_growthinit(:,j) = large_value

             age(:,j) = zero

             ! all leaves are young
             leaf_frac(:,j,1) = un

             ! non-zero "long term" npp and last year's leaf mass for saplings -
             ! so they won't be killed off by gap or kill
             npp_longterm(:,j) = npp_longterm_init

             lm_lastyearmax(:,j) = bm_sapl(j,ileaf,icarbon) * ind(:,j)

          ENDWHERE    ! we can introduce the PFT

          !! 6.4 Expansion of the PFT within the grid box 
          !      PFT expansion/dispersion to a new grid box should not be confused with 
          !      expansion in areal coverage
          IF ( treat_expansion ) THEN

             WHERE ( can_introduce(:) )

                ! low value at the beginning
                everywhere(:,j) = everywhere_init
             ENDWHERE

          ELSE

             ! expansion is not treated
             WHERE ( can_introduce(:) )
                everywhere(:,j) = un
             ENDWHERE

          ENDIF ! treat expansion

       ENDIF ! only natural PFTs

    ENDDO ! Loop over # PFTs

  !! 7. If a PFT has been present once in a grid box, we suppose that it will survive

    !   If a PFT has been present once in a grid box, we suppose that it will survive
    !   in isolated places (e.g., an oasis) within that grid box, even if it gets
    !   officially eliminated from it later. That means that if climate becomes favorable
    !   again, it will not need to get seeds from adjacent grid cells.
    WHERE ( PFTpresent )
       need_adjacent = .FALSE.
    ENDWHERE

    IF (printlev>=4) WRITE(numout,*) 'Leaving pftinout'

  END SUBROUTINE pftinout

END MODULE lpj_pftinout
