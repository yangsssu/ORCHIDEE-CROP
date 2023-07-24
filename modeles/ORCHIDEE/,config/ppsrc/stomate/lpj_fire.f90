










! =================================================================================================================================
! MODULE        : lpj_fire
!
! CONTACT	: orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      	: IPSL (2006). This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF         Calculates the probability of fire and its effects on the carbon cycle
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! SVN           :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_fire.f90 $ 
!! $Date: 2015-11-16 14:26:03 +0100 (Mon, 16 Nov 2015) $
!! $Revision: 3026 $
!! \n
!_ ================================================================================================================================

MODULE lpj_fire

  ! modules used:
  USE xios_orchidee
  USE stomate_data
  USE ioipsl_para 
  USE pft_parameters
  USE constantes

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC fire,fire_clear

  LOGICAL, SAVE                   :: firstcall_fire = .TRUE.        !! first call 
!$OMP THREADPRIVATE(firstcall_fire)

CONTAINS


!! ================================================================================================================================
!!  SUBROUTINE   : fire_clear 
!!
!>\BRIEF        Set the firstcall_fire flag to .TRUE. and activate initialization
!! 
!_ ================================================================================================================================

  SUBROUTINE fire_clear
    firstcall_fire = .TRUE.
  END SUBROUTINE fire_clear


!! ================================================================================================================================
!! SUBROUTINE     : fire 
!!
!>\BRIEF          Calculate fire index and fraction of area burned by comparing 
!! daily litter moisture with prescribed moisture of fire inhibition. If daily 
!! moisture is below the precribed threshold, allow fire disturbance and 
!! calculate CO_2 emissions from fire. The main algorithm follows Thonicke et al. 
!! (2001)
!!
!! DESCRIPTION	: Fire occurs when the three basic prerequisites are met: 
!! sufficient fuel on the ground, relatively dry fuel (fuel moisture lower than 
!! the moisture of extinction), and presence of ignition sources.  The flag 
!! ::disable_fire is used to use this fire module or not. While here in the module the 
!! ignition source is not explicitely represented. It's assumed 
!! that the ignition source is always available, i.e., if a sufficient amount 
!! of dead fuel exists with a moisture content below the moisture of fire 
!! inhibition, then both live and dead fuel will start to burn.\n
!!
!! The module completes the following tasks:
!! 1. Calculates daily fire index, long term fire index and available ground 
!! litter for burning.\n
!! \latexonly
!! \input{lpj_fire_1.tex}  
!! \endlatexonly
!! Where, m is the daily litter moisture, m_e is the moisture of extinction, p(m) 
!! is probability of fire (i.e. the daily fire index).\n
!! 
!! \latexonly
!! \input{lpj_fire_2.tex}  
!! \endlatexonly
!! Where, s is the long term fire index and A(s) is the annual fire fraction.\n
!! 2. Calculates annual fire fraction, then transform to daily fraction in the 
!! same time step of stomate.\n
!! 3. Ground litter and grass live biomass (in growing season and except root and carbon 
!! reserve pool) are  burned at full fire fraction.\n
!! 4. Fire fraction for tree PFT are compromised by prescribed fire resistence.
!! Tree live biomass are consumed using this compromised fire fraction and consumption
!! fraction for different biomass parts. In the case of activation of dynamic 
!! vegetation, tree individual density is updated after fire. \n 
!! 5. For all types of fuel (either ground litter or live biomass) that are 
!! burned, there is a certain fraction of “residue” that are not completely 
!! burned. The remaining part is transferred to (in case of biomass) or
!! remains (in case of ground litter) as litter.\n
!! 6. Update the biomass, litter or dead leaves pool after burning.
!! 7. If the flag SPINUP_ANALYTIC is set to true, the matrix A is updated following
!! Lardy (2011).
!! 
!! RECENT CHANGE(S): April 2015: Black carbon calculations is removed. The black carbon was not conserved.
!!
!! MAIN OUTPUT VARIABLE(S): ::co2_fire or the carbon emitted into the atmosphere 
!! by fire, including both living and dead biomass (gC m^{-2} dtslow^{-1})$ @endtex
!!
!! REFERENCES    :
!! - Thonicke K., Venevsky S., Sitch S., and Cramer W. (2001) The role of fire 
!! disturbance for global vegetation dynamics: coupling fire into a Dynamic 
!! Global Vegetation Model, Global Ecology & Biogeography, 10, 661-677.
!! - Kuhlbusch et al. JGR 101, 23651-23665, 1996
!! - Kuhlbusch & Crutzen, GBC 9, 491-501, 1995
!! - Lardy, R, et al., A new method to determine soil organic carbon equilibrium,
!! Environmental Modelling & Software (2011), doi:10.1016|j.envsoft.2011.05.016
!!
!! FLOWCHART     : None
!! \n 
!_ ================================================================================================================================
  
  SUBROUTINE fire (npts, dt, litterpart, &
       litterhum_daily, t2m_daily, lignin_struc,veget_max, &
       fireindex, firelitter, biomass, ind, &
       litter, dead_leaves, bm_to_litter, &
       co2_fire, MatrixA)

  !! 0. Variable and parameter declarations

    !! 0.1 Input variables
   
    INTEGER(i_std), INTENT(in)                                   :: npts                !! Domain size - number of pixels 
                                                                                        !! (unitless)   
    REAL(r_std), INTENT(in)                                      :: dt                  !! Time step of the simulations for stomate 
                                                                                        !! (days)   
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)           :: litterpart          !! [DISPENSABLE] Fraction of litter above 
                                                                                        !! the ground belonging to different PFTs 
                                                                                        !! ?? this variable is used but might be 
                                                                                        !! redundant (with value of always 1) ?? 
                                                                                        !! To check but probably litterpart to be 
                                                                                        !! removed. Probably a residual from 
                                                                                        !! nat/agri before merge   
    REAL(r_std), DIMENSION(npts), INTENT(in)                     :: litterhum_daily     !! Daily litter moisture (unitless, 0-1)   
    REAL(r_std), DIMENSION(npts), INTENT(in)                     :: t2m_daily           !! Daily 2 meter temperature (K)   
    REAL(r_std), DIMENSION(npts,nvm,nlevs), INTENT(in)           :: lignin_struc        !! Ratio Lignin/Carbon in structural above 
                                                                                        !! and below ground litter 
                                                                                        !! @tex $(gC gC^{-1})$ @endtex 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                 :: veget_max           !! Maximum fraction of vegetation type including 
                                                                                        !! non-biological fraction (0-1, unitless) 

    !! 0.2 Output variables
    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)                :: co2_fire            !! Carbon emitted into the atmosphere by 
                                                                                        !! fire, including both living and dead 
                                                                                        !! biomass 
                                                                                        !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    !! 0.3 Modified variables
    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)              :: fireindex           !! Probability of fire; in the code means 
                                                                                        !! long term fire index (unitless, 0-1)   
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)              :: firelitter          !! Total natural litter 
                                                                                        !! available for burning above the ground 
                                                                                        !! @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout)  :: biomass        !! Biomass @tex $(gC m^{-2})$ @endtex
    REAl(r_std), DIMENSION(npts,nvm), INTENT(inout)                   :: ind            !! Density of individuals 
                                                                                        !! @tex $(m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs,nelements), INTENT(inout) :: litter     !! Metabolic and structural litter, above
                                                                                        !! and below ground 
                                                                                        !! @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: dead_leaves         !! Dead leaves on ground, per PFT, 
                                                                                        !! metabolic and structural 
                                                                                        !! @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout):: bm_to_litter     !! Biomass entering the litter pool 
                                                                                        !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm,nbpools,nbpools), INTENT(inout) :: MatrixA          !! Matrix containing the fluxes between the carbon pools
                                                                                        !! here, it is called only if disable_fire = n
                                                                                        !! once a day 
                                                                                        !! @tex $(gC.m^2.day^{-1})$ @endtex
   

    !! 0.4 Local variables

    REAL(r_std), DIMENSION(npts)                                  :: fire_disturb       !! Actual fire disturbance fraction 
                                                                                        !! after consideration of inherent fire 
                                                                                        !! resistance of different PFTs 
                                                                                        !! (unitless, 0-1)  
    REAL(r_std), DIMENSION(npts,nvm)                              :: firedeath          !! In case of activation of dynamic 
                                                                                        !! vegetation, the daily fraction of 
                                                                                        !! burned individuals (unitless, 0-1)  
    REAL(r_std), DIMENSION(npts)                                  :: moistlimit         !! Moisture for fire inhibition per PFT                                                                                         !! 
                                                                                        !! (unitless, 0-1) ; temporary variable 
                                                                                        !! for each PFT in the loop over #PFT ? 
                                                                                        !! it's not fire inhibition, it's 
                                                                                        !! "moisture of extinction" by the original
                                                                                        !! reference paper, and a more "fire 
                                                                                        !! professional name" it's better to change
                                                                                        !! the name of moistlimit 
    REAL(r_std), DIMENSION(npts)                                  :: litter_above       !! Total aboveground litter per PFT                                                                                         !! 
                                                                                        !! @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm)                              :: fireindex_daily    !! Daily fire index (unitless, 0-1) 
    REAL(r_std), DIMENSION(npts, nvm)                             :: firefrac           !! Daily fire burning fraction on ground 
                                                                                        !! (unitless, 0-1) 
    REAL(r_std), DIMENSION(npts)                                  :: struc_residual     !! Fraction of structural litter not burned
                                                                                        !! (thus residual), depending on strutural 
                                                                                        !! litter lignin content  (unitless, 0-1) 
    REAL(r_std), DIMENSION(npts)                                  :: residue            !! Fuel (either biomass or litter) not 
                                                                                        !! burned  @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts)                                  :: x                  !! Intermediate variable
    REAL(r_std), DIMENSION(npts)                                  :: aff                !! Annual fire fraction (unitless, 0-1) 
    INTEGER(i_std)                                                :: j,k,m              !! Indeces
!_ ================================================================================================================================

    IF (printlev>=3) WRITE(numout,*) 'Entering fire'

 !! 1. Initialization

    IF ( firstcall_fire ) THEN

       !! 1.1 Fraction to CO_2
       !      What fraction of the plant biomass compartment, if burned, is transformed 
       !      into CO_2 released to the atmosphere?
       !??! the fraction for heartabove seems too big, it's not clear if this value is correct. 

       !! 1.2 print control messages
       WRITE(numout,*) 'fire:'

       WRITE(numout,*) '   > temporal inertia of fire probability (d): ',tau_fire

       WRITE(numout,*) '   > fraction of burned biomass that becomes CO2:'
       WRITE(numout,*) '     leaves: ', co2frac(ileaf)
       WRITE(numout,*) '     sap above ground: ', co2frac(isapabove)
       WRITE(numout,*) '     sap below ground: ', co2frac(isapbelow)
       WRITE(numout,*) '     heartwood above ground: ', co2frac(iheartabove)
       WRITE(numout,*) '     heartwood below ground: ', co2frac(iheartbelow)
       WRITE(numout,*) '     roots: ', co2frac(iroot)
       WRITE(numout,*) '     fruits: ', co2frac(ifruit)
       WRITE(numout,*) '     carbohydrate reserve: ', co2frac(icarbres)

       WRITE(numout,*) '   > critical litter quantity (gC/m**2): ',litter_crit
       WRITE(numout,*) '   > We calculate a fire probability on agricultural ground, but'
       WRITE(numout,*) '       the effective fire fraction is zero.'

       firstcall_fire = .FALSE.

    ENDIF

    !! 1.4 Initialize output
    co2_fire(:,:) = zero
    firedeath(:,:) = zero
    fireindex_daily(:,:) = zero
    firefrac(:,:) = zero

 !! 2. Determine fire probability
 
    ! Calculate probability that crops are not burned for the moment
    ! Calculate long-term aboveground litter
 
    ! a long loop over PFT
    DO j = 2,nvm !loop over #PFT

       !! 2.1 Total above ground litter
       !      Total litter above the ground for the PFT under consideration
       litter_above(:) = litter(:,imetabolic,j,iabove,icarbon) + &
            litter(:,istructural,j,iabove,icarbon)


       !! 2.2 Calculate the PFT litter amount weighted by moisture 
       ! If litter moisture is higher than moisture of extinction, fire is not possible.
       moistlimit(:) = zero

       WHERE ( (t2m_daily(:) .GT. ZeroCelsius) .AND. (litter_above(:) .GT. min_stomate) )
       !?? the calculation here is redundant? as the part before flam(j) is 1?
       !?? see first comment.. litterpart to be removed ?
          moistlimit(:) = &
               ( litterpart(:,j,imetabolic)*litter(:,imetabolic,j,iabove,icarbon) + &
               litterpart(:,j,istructural)*litter(:,istructural,j,iabove,icarbon)  ) / &
               litter_above(:) * flam(j)

       ENDWHERE

       !! 2.3 Calculate daily fire index 
       !      Calculate daily fire index
       !      \latexonly
       !        \input{lpj_fire_1.tex}  
       !      \endlatexonly
       !      Where, m is the daily litter moisture, m_e is the moisture of extinction, p(m) 
       !      is probability of fire (i.e. the daily fire index).\n
       WHERE ( moistlimit(:) .GT. min_stomate )
          x(:) = litterhum_daily(:)/moistlimit(:)
          fireindex_daily(:,j) = EXP( - pi * x(:) * x(:) )
       ELSEWHERE
          fireindex_daily(:,j) = zero
       ENDWHERE

       !! 2.4 Calculate long-term fire index 
       !      Calculate long-term fire index which is the mean probability of fire
       fireindex(:,j) = ((tau_fire - dt) * fireindex(:,j) + (dt) * fireindex_daily(:,j)) / tau_fire

       !! 2.5 Calculate long term aboveground litter that are available for burning
       firelitter(:,j) = &
            ( ( tau_fire-dt ) * firelitter(:,j) + dt * litter_above(:) ) / tau_fire

  !! 3. Calculate fire fraction from litter and fireindex

       !! 3.1 Fire fraction from long term fire index for natural PFTs
       !  Transform the annual fire fraction to daily fraction. This is done by assuming that
       !  each day the fire fraction is the same. 
       aff(:) = firefrac_func (npts, fireindex(:,j))

       ! annual_fire_fraction = 1. - ( 1. - daily_fire_fraction )**365
       ! Thus, daily_fire_fraction = 1. - ( 1. - annual_fire_fraction )**(1/365)
       ! If annual firefrac<<1, then firefrac_daily = firefrac * dt/one_year
       ! This approximation avoids numerical problems.
       ! the variable 'un' is 1
       IF(.NOT.disable_fire.AND.natural(j))THEN
          WHERE ( aff(:) .GT. 0.1 )
             firefrac(:,j) = un - ( un - aff(:) ) ** (dt/one_year)
          ELSEWHERE
             firefrac(:,j) = aff(:) * dt/one_year
          ENDWHERE
       ELSE
          firefrac(:,j) = zero
       ENDIF

       ! No fire if litter is below critical value
       WHERE ( firelitter(:,j) .LT. litter_crit )
          firefrac(:,j) = zero
       ENDWHERE

       ! However, there is a minimum fire extent
       firefrac(:,j) = MAX( 0.001_r_std * dt/one_year, firefrac(:,j) )

       ! if FIRE_DISABLE flag is set no fire
       IF (disable_fire) firefrac = zero
       
       !! 3.2 For agricultural ground, no fire is burned for the moment
       IF ( .NOT. natural(j)) firefrac(:,j) = zero
       
  !! 4. Determine fire impact

       ! Calculate fire disturbance fraction for each PFT, and fire emissions due 
       ! to grasses.
   
       !! 4.1 Tree and grass live biomass 
       ! Tree live biomass is not burned in fire. 
       ! However, in the dynamic vegetation module, tree individual density 
       ! will be updated after fire. The fraction of tree individuals that are 
       ! supposed to die in fire is the fire fraction multiplied by the tree PFT fire 
       ! resistance which reflect survivorship of different tree PFTs during fire.  
       IF ( is_tree(j) ) THEN

          !! 4.1.1 Disturban,ce factor for trees 
          !        Trees are disturbed over the whole year. A resistance factor is  
          !        used to reflect survivorship of different tree PFTs during fire. 
          fire_disturb(:) = ( un - resist(j) ) * firefrac(:,j)

       ELSE

          !! 4.1.2 Disturbance factor for grasses 
          !        Grass is not disturbed outside the growing season thus grass biomass 
          !        is only burned during the growing season.
          WHERE ( biomass(:,j,ileaf,icarbon) .GT. min_stomate )

             fire_disturb(:) = ( un - resist(j) ) * firefrac(:,j)

          ELSEWHERE

             fire_disturb(:) = zero

          ENDWHERE

       ENDIF

       !! 4.2 Burn live biomass
       !      The burned part goes to either the atmposhere or litter
       DO k = 1, nparts

          ! for tree PFT, all biomass compartments are burned.
          ! for grass biomass compartments, all are burned except root and carbon reserve
          ! IF concerning PFT is tree; OR (the PFT is grass, but not root or carbon reserve biomass); then it's burned.
          IF ( .NOT. ( ( .NOT. is_tree(j) ) .AND. ( ( k.EQ.iroot ) .OR. ( k.EQ.icarbres ) ) ) ) THEN

             !! 4.2.1 Fraction to the atmosphere.
             co2_fire(:,j) =  co2_fire(:,j)+ biomass(:,j,k,icarbon) * fire_disturb(:) * co2frac(k) / dt

             !! 4.2.2 Determine the residue
             !        Residual is expressed in gC/m^2
             residue(:) = biomass(:,j,k,icarbon) * fire_disturb(:) * ( un - co2frac(k) )

             !! 4.2.2.3 The rest (largest part) of the residue becomes litter.
             bm_to_litter(:,j,k,icarbon) = bm_to_litter(:,j,k,icarbon) + residue(:)
          ENDIF

       ENDDO

      
       !! 4.3 Update biomass pool after burning
       
       !! 4.3.1 Decrease biomass amount except for grass roots and carbon reserve.
       DO m = 1, nelements ! Loop over # elements
          
          DO k = 1, nparts

             ! **2
             IF ( .NOT. ( ( .NOT. is_tree(j) ) .AND. ( ( k.EQ.iroot ) .OR. ( k.EQ.icarbres) ) ) ) THEN
                
                biomass(:,j,k,m) = ( un - fire_disturb(:) ) * biomass(:,j,k,m)
                
             ENDIF

          ENDDO
          
       END DO ! End Loop over # elements


       !! 4.3.2 If vegetation is dynamic, then decrease the density of tree individuals.
       IF ( (ok_dgvm .OR. .NOT.lpj_gap_const_mort) .AND. is_tree(j) ) THEN

          firedeath(:,j) = fire_disturb(:) / dt

          ind(:,j) = ( un - fire_disturb(:) ) * ind(:,j)

       ENDIF

    ENDDO      ! loop over #PFT

  !! 5. Burn litter

    !   All litter (either forest or grass) is burned throughout the whole year
    !   Metabolic litter is totally burned. Burning fraction of structural litter is related 
    !   with its lignin content. The burned part either goes to the atmosphere
    !   or remains in litter as unburned residue.
    DO j = 2,nvm  !loop over #PFT

       !! 5.1 Burn exposed metabolic litter
       !      Exposed metabolic litter burns completely and goes directly into the atmosphere as CO2.

       !! 5.1.1 CO2 flux from litter burning
       !        Flux expressed in gC m^{-2}dtslow^{-1}
       co2_fire(:,j) = co2_fire(:,j) + litter(:,imetabolic,j,iabove,icarbon) * &
            firefrac(:,j) / dt

       !! 5.1.2 Decrease metabolic litter
       DO m = 1,nelements
          litter(:,imetabolic,j,iabove,m) = litter(:,imetabolic,j,iabove,m) * &
               ( un - firefrac(:,j) )
       END DO
    
       !! 5.2 Burning exposed structural litter 
       !      The fraction that goes to the atmosphere is related to its lignin content. The remaining unburned 
       !      residues remain as litter.

       !! 5.2.1 Incomplete burning of exposed structural litter  
       !        Fraction of structural litter that does not burn completly. This fraction depends on lignin 
       !        content (lignin_struc).
       struc_residual(:) = fire_resist_struct * lignin_struc(:,j,iabove)

       !! 5.2.2 CO2 flux from litter burning 
       !  Flux expressed in gC m^{-2}dtslow^{-1}
       co2_fire(:,j) = co2_fire(:,j) + &
            litter(:,istructural,j,iabove,icarbon) * firefrac(:,j) * &
            ( un - struc_residual(:) )/ dt

       !! 5.2.3 Determine residue 
       !        The residue is litter that undergoes fire, but is not transformed into CO2
!!       IF (ok_dgvm .OR. .NOT.lpj_gap_const_mort) THEN
!!          residue(:) = firefrac(:,j) * struc_residual(:)
!!       ELSE
          residue(:) = litter(:,istructural,j,iabove,icarbon) * firefrac(:,j) * &
               struc_residual(:)
!!       ENDIF

       !! 5.2.6 TResidue remaining litter
       !        The rest (largest part) of the residue remains litter. Remaining 
       !        litter is the sum of this and of the litter which has not undergone a fire.
       litter(:,istructural,j,iabove,icarbon) = &
            litter(:,istructural,j,iabove,icarbon) * ( un - firefrac(:,j) ) + &
            residue(:)

       !! 5.2.7 Update matrix A for analytical spin-up (only if SPINUP_ANALYTIC is activated)

       IF (spinup_analytic) THEN
          
          ! litter structural above 
          MatrixA(:,j,istructural_above,istructural_above) =  MatrixA(:,j,istructural_above,istructural_above) - firefrac(:,j) &
                                                                +  firefrac(:,j)* struc_residual(:)
          
          ! litter_metabolic above
          MatrixA(:,j,imetabolic_above,imetabolic_above) = MatrixA(:,j,imetabolic_above,imetabolic_above) - firefrac(:,j)
          
          !DS! This is the exact formulation. The above is a simplified formulation.
          !          MatrixA(:,j,istructural_above,istructural_above) =  MatrixA(:,j,istructural_above,istructural_above)*(1. - firefrac(:,j) + &
          !                                                             firefrac(:,j)* struc_residual(:)
          !          MatrixA(:,j,imetabolic_above,imetabolic_above) = MatrixA(:,j,imetabolic_above,imetabolic_above)*(1 - firefrac(:,j))
          
       ENDIF !(spinup_analytic)

    ENDDO  !  loop over #PFT
   
   !! 5.3 Update exposed dead leaves (leaf litter) on the ground
   !      Dead leaves are supposed to burn completely in fire even their structural part.
    DO j = 2,nvm

       DO k = 1, nlitt
          dead_leaves(:,j,k) = dead_leaves(:,j,k) * ( un - firefrac(:,j) )
       ENDDO

    ENDDO

  !! 6. write out history variables

    ! output in 1/dtslow
    firefrac(:,:) = firefrac(:,:) / dt

    CALL xios_orchidee_send_field("FIREFRAC",firefrac(:,:))
    CALL xios_orchidee_send_field("FIREDEATH",firedeath(:,:))

    CALL histwrite_p (hist_id_stomate, 'FIREFRAC', itime, &
         firefrac(:,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'FIREDEATH', itime, &
         firedeath(:,:), npts*nvm, horipft_index)

    IF (printlev>=4) WRITE(numout,*) 'Leaving fire'

  END SUBROUTINE fire


!! ================================================================================================================================
!! FUNCTION     : firefrac_func
!!
!>\BRIEF        Calculate the annual fire fraction using long term fire index
!! 
!! DESCRIPTION	: None
!!
!! RECENT CHANGE(S): None
!!
!! RETURN VALUE : annual fire fraction (::firefrac_result)
!!
!! REFERENCES   :
!! - Thonicke K., Venevsky S., Sitch S., and Cramer W. (2001) The role of fire 
!! disturbance for global vegetation dynamics: coupling fire into a Dynamic 
!! Global Vegetation Model, Global Ecology & Biogeography, 10, 661-677.\n
!!
!! FLOWCHART    : None
!! \n 
!_ ================================================================================================================================
   
  FUNCTION firefrac_func (npts, x) RESULT (firefrac_result)

!! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                 :: npts             !! Domain size (unitless)
    REAL(r_std), DIMENSION(npts), INTENT(in)   :: x                !! fire index (unitless, 0-1)

    !! 0.2 Output variables

    REAL(r_std), DIMENSION(npts)               :: firefrac_result  !! fire fraction (unitless, 0-1)

    !! 0.3 Modified variables

    !! 0.4 Local variables
    REAL(r_std), DIMENSION(npts)               :: xm1              !! intermediate variable
!_ ================================================================================================================================

!! 1. Function

    xm1(:) = x(:) - 1.

    ! this equation is from Thonicke et al. (2001) equation (9), it use the fire index as input to calculate annual fire fraction.
    ! but with different parameters with the source literature.
    !! \latexonly
    !! \input{lpj_fire_2.tex}  
    !! \endlatexonly
    !! Where, s is the long term fire index and A(s) is the annual fire fraction.\n
    !??! here we use a different parameter with K. Thonicke et al. (2001)
    !??! it's not clear if these parameters are correct.
    firefrac_result(:) = &
         &     x(:) * EXP( xm1(:) / ( -firefrac_coeff(4)*xm1(:)*xm1(:)*xm1(:) + &
         &     firefrac_coeff(3)*xm1(:)*xm1(:) + &
         &     firefrac_coeff(2)*xm1(:) +  &      
         &     firefrac_coeff(1) ) )

  END FUNCTION firefrac_func

END MODULE lpj_fire
