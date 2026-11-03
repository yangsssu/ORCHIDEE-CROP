










! =================================================================================================================================
! MODULE       : stomate_soilcarbon
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Calculate soil dynamics largely following the Century model
!!	
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! SVN		:
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/stomate_soilcarbon.f90 $ 
!! $Date: 2016-01-24 15:03:23 +0100 (Sun, 24 Jan 2016) $
!! $Revision: 3149 $
!! \n
!_ ================================================================================================================================

MODULE stomate_soilcarbon

  ! modules used:

  USE ioipsl_para
  USE stomate_data
  USE constantes

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC soilcarbon,soilcarbon_clear

  ! Variables shared by all subroutines in this module
  
  LOGICAL, SAVE    :: firstcall_soilcarbon = .TRUE.   !! Is this the first call? (true/false)
!$OMP THREADPRIVATE(firstcall_soilcarbon)

CONTAINS


!! ================================================================================================================================
!!  SUBROUTINE   : soilcarbon_clear
!!
!>\BRIEF        Set the flag ::firstcall_soilcarbon to .TRUE. and as such activate sections 1.1.2 and 1.2 of the subroutine soilcarbon 
!! (see below).
!! 
!_ ================================================================================================================================
  
  SUBROUTINE soilcarbon_clear
    firstcall_soilcarbon=.TRUE.
  ENDSUBROUTINE soilcarbon_clear


!! ================================================================================================================================
!!  SUBROUTINE   : soilcarbon
!!
!>\BRIEF        Computes the soil respiration and carbon stocks, essentially 
!! following Parton et al. (1987).
!!
!! DESCRIPTION	: The soil is divided into 3 carbon pools, with different 
!! characteristic turnover times : active (1-5 years), slow (20-40 years) 
!! and passive (200-1500 years).\n
!! There are three types of carbon transferred in the soil:\n
!! - carbon input in active and slow pools from litter decomposition,\n
!! - carbon fluxes between the three pools,\n
!! - carbon losses from the pools to the atmosphere, i.e., soil respiration.\n
!!
!! The subroutine performs the following tasks:\n
!!
!! Section 1.\n
!! The flux fractions (f) between carbon pools are defined based on Parton et 
!! al. (1987). The fractions are constants, except for the flux fraction from
!! the active pool to the slow pool, which depends on the clay content,\n
!! \latexonly
!! \input{soilcarbon_eq1.tex}
!! \endlatexonly\n
!! In addition, to each pool is assigned a constant turnover time.\n
!!
!! Section 2.\n
!! The carbon input, calculated in the stomate_litter module, is added to the 
!! carbon stock of the different pools.\n
!!
!! Section 3.\n
!! First, the outgoing carbon flux of each pool is calculated. It is 
!! proportional to the product of the carbon stock and the ratio between the 
!! iteration time step and the residence time:\n
!! \latexonly
!! \input{soilcarbon_eq2.tex}
!! \endlatexonly
!! ,\n
!! Note that in the case of crops, the additional multiplicative factor 
!! integrates the faster decomposition due to tillage (following Gervois et 
!! al. (2008)).
!! In addition, the flux from the active pool depends on the clay content:\n
!! \latexonly
!! \input{soilcarbon_eq3.tex}
!! \endlatexonly
!! ,\n
!! Each pool is then cut from the carbon amount corresponding to each outgoing
!! flux:\n
!! \latexonly
!! \input{soilcarbon_eq4.tex}
!! \endlatexonly\n
!! Second, the flux fractions lost to the atmosphere is calculated in each pool
!! by subtracting from 1 the pool-to-pool flux fractions. The soil respiration 
!! is then the summed contribution of all the pools,\n
!! \latexonly
!! \input{soilcarbon_eq5.tex}
!! \endlatexonly\n
!! Finally, each carbon pool accumulates the contribution of the other pools:
!! \latexonly
!! \input{soilcarbon_eq6.tex}
!! \endlatexonly
!!
!! Section 4.\n
!! If the flag SPINUP_ANALYTIC is set to true, the matrix A is updated following
!! Lardy (2011).
!!
!! RECENT CHANGE(S): None
!! 
!! MAIN OUTPUTS VARIABLE(S): carbon, resp_hetero_soil
!!
!! REFERENCE(S)   :
!! - Parton, W.J., D.S. Schimel, C.V. Cole, and D.S. Ojima. 1987. Analysis of 
!! factors controlling soil organic matter levels in Great Plains grasslands. 
!! Soil Sci. Soc. Am. J., 51, 1173-1179.
!! - Gervois, S., P. Ciais, N. de Noblet-Ducoudre, N. Brisson, N. Vuichard, 
!! and N. Viovy (2008), Carbon and water balance of European croplands 
!! throughout the 20th century, Global Biogeochem. Cycles, 22, GB2022, 
!! doi:10.1029/2007GB003018.
!! - Lardy, R, et al., A new method to determine soil organic carbon equilibrium,
!! Environmental Modelling & Software (2011), doi:10.1016|j.envsoft.2011.05.016
!!
!! FLOWCHART    :
!! \latexonly
!! \includegraphics[scale=0.5]{soilcarbon_flowchart.jpg}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE soilcarbon (npts, dt, clay, &
       soilcarbon_input, control_temp, control_moist, &
       carbon, &
       resp_hetero_soil, &
       MatrixA)
!JCADD
!       resp_hetero_soil_part)
!ENDJCADD

!! 0. Variable and parameter declaration

    !! 0.1 Input variables
    
    INTEGER(i_std), INTENT(in)                            :: npts             !! Domain size (unitless)
    REAL(r_std), INTENT(in)                               :: dt               !! Time step 
    REAL(r_std), DIMENSION(npts), INTENT(in)              :: clay             !! Clay fraction (unitless, 0-1) 
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(in)    :: soilcarbon_input !! Amount of carbon going into the carbon pools from litter decomposition \f$(gC m^{-2} day^{-1})$\f
    REAL(r_std), DIMENSION(npts,nlevs), INTENT(in)        :: control_temp     !! Temperature control of heterotrophic respiration (unitless: 0->1)
    REAL(r_std), DIMENSION(npts,nlevs), INTENT(in)        :: control_moist    !! Moisture control of heterotrophic respiration (unitless: 0.25->1)

    !! 0.2 Output variables
    
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)         :: resp_hetero_soil !! Soil heterotrophic respiration \f$(gC m^{-2} dt^{-1})$\f

    !! 0.3 Modified variables
    
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(inout) :: carbon             !! Soil carbon pools: active, slow, or passive, \f$(gC m^{2})$\f
    REAL(r_std), DIMENSION(npts,nvm,nbpools,nbpools), INTENT(inout) :: MatrixA  !! Matrix containing the fluxes between the carbon pools
                                                                                !! per sechiba time step 
                                                                                !! @tex $(gC.m^2.day^{-1})$ @endtex
!JCADD
    !! 0.4 Local variables
    REAL(r_std), SAVE, DIMENSION(ncarb)                   :: carbon_tau       !! Residence time in carbon pools (days)
!$OMP THREADPRIVATE(carbon_tau)
    REAL(r_std), DIMENSION(npts,ncarb,ncarb)              :: frac_carb        !! Flux fractions between carbon pools 
                                                                              !! (second index=origin, third index=destination) 
                                                                              !! (unitless, 0-1)
    REAL(r_std), DIMENSION(npts,ncarb)                    :: frac_resp        !! Flux fractions from carbon pools to the atmosphere (respiration) (unitless, 0-1)
    REAL(r_std), DIMENSION(npts,ncarb,nelements)          :: fluxtot          !! Total flux out of carbon pools \f$(gC m^{2})$\f
    REAL(r_std), DIMENSION(npts,ncarb,ncarb,nelements)    :: flux             !! Fluxes between carbon pools \f$(gC m^{2})$\f
    CHARACTER(LEN=7), DIMENSION(ncarb)                    :: carbon_str       !! Name of the carbon pools for informative outputs (unitless)
    INTEGER(i_std)                                        :: k,kk,m,j         !! Indices (unitless)

!_ ================================================================================================================================

    !! printlev is the level of diagnostic information, 0 (none) to 4 (full)
    IF (printlev>=3) WRITE(numout,*) 'Entering soilcarbon' 

!! 1. Initializations

    !! 1.1 Get soil "constants"
    !! 1.1.1 Flux fractions between carbon pools: depend on clay content, recalculated each time
    ! From active pool: depends on clay content
    frac_carb(:,iactive,iactive) = zero
    frac_carb(:,iactive,ipassive) = frac_carb_ap
    frac_carb(:,iactive,islow) = un - (metabolic_ref_frac - active_to_pass_clay_frac*clay(:)) - frac_carb(:,iactive,ipassive)

    ! 1.1.1.2 from slow pool

    frac_carb(:,islow,islow) = zero
    frac_carb(:,islow,iactive) = frac_carb_sa
    frac_carb(:,islow,ipassive) = frac_carb_sp

    ! From passive pool
    frac_carb(:,ipassive,ipassive) = zero
    frac_carb(:,ipassive,iactive) = frac_carb_pa
    frac_carb(:,ipassive,islow) = frac_carb_ps

    IF ( firstcall_soilcarbon ) THEN

        !! 1.1.2 Residence times in carbon pools (days)
        carbon_tau(iactive) = carbon_tau_iactive * one_year       ! 1.5 years. This is same as CENTURY. But, in Parton et al. (1987), it's weighted by moisture and temperature dependences.
        carbon_tau(islow) = carbon_tau_islow * one_year          ! 25 years. This is same as CENTURY. But, in Parton et al. (1987), it's weighted by moisture and temperature dependences.
        carbon_tau(ipassive) = carbon_tau_ipassive * one_year       ! 1000 years. This is same as CENTURY. But, in Parton et al. (1987), it's weighted by moisture and temperature dependences.
        
        !! 1.2 Messages : display the residence times  
        carbon_str(iactive) = 'active'
        carbon_str(islow) = 'slow'
        carbon_str(ipassive) = 'passive'
        
        WRITE(numout,*) 'soilcarbon:'
        
        WRITE(numout,*) '   > minimal carbon residence time in carbon pools (d):'
        DO k = 1, ncarb ! Loop over carbon pools
          WRITE(numout,*) '(1, ::carbon_str(k)):',carbon_str(k),' : (1, ::carbon_tau(k)):',carbon_tau(k)
        ENDDO
        
        WRITE(numout,*) '   > flux fractions between carbon pools: depend on clay content'
        
        firstcall_soilcarbon = .FALSE.
        
    ENDIF

    !! 1.3 Set soil respiration to zero
    resp_hetero_soil(:,:) = zero
! JCADD
!    resp_hetero_soil_part(:,:,:) = zero
! ENDJCADD
!! 2. Update the carbon stocks with the different soil carbon input

    carbon(:,:,:) = carbon(:,:,:) + soilcarbon_input(:,:,:) * dt

!! 3. Fluxes between carbon reservoirs, and to the atmosphere (respiration) \n

    !! 3.1. Determine the respiration fraction : what's left after
    ! subtracting all the 'pool-to-pool' flux fractions
    ! Diagonal elements of frac_carb are zero
    !    VPP killer:
    !     frac_resp(:,:) = 1. - SUM( frac_carb(:,:,:), DIM=3 )
    frac_resp(:,:) = un - frac_carb(:,:,iactive) - frac_carb(:,:,islow) - &
         frac_carb(:,:,ipassive) 

    !! 3.2. Calculate fluxes

    DO m = 2,nvm ! Loop over # PFTs

      !! 3.2.1. Flux out of pools

      DO k = 1, ncarb ! Loop over carbon pools from which the flux comes
        
        ! Determine total flux out of pool
        ! S.L. Piao 2006/05/05 - for crop multiply tillage factor of decomposition
        ! Not crop
         IF ( natural(m) ) THEN
            fluxtot(:,k,icarbon) = dt/carbon_tau(k) * carbon(:,k,m) * &
                  control_moist(:,ibelow) * control_temp(:,ibelow)
         ! C3 crop
          ELSEIF ( (.NOT. natural(m)) .AND. (.NOT. is_c4(m)) ) THEN
             fluxtot(:,k,icarbon) = dt/carbon_tau(k) * carbon(:,k,m) * &
                  control_moist(:,ibelow) * control_temp(:,ibelow) * flux_tot_coeff(1)
          ! C4 Crop   
          ELSEIF ( (.NOT. natural(m)) .AND. is_c4(m) ) THEN
             fluxtot(:,k,icarbon) = dt/carbon_tau(k) * carbon(:,k,m) * &
                  control_moist(:,ibelow) * control_temp(:,ibelow) * flux_tot_coeff(2)
        ENDIF
        ! END - S.L. Piao 2006/05/05 - for crop multiply tillage factor of decomposition

        ! Carbon flux from active pools depends on clay content
        IF ( k .EQ. iactive ) THEN
            fluxtot(:,k,icarbon) = fluxtot(:,k,icarbon) * ( un - flux_tot_coeff(3) * clay(:) )
        ENDIF
        
        ! Update the loss in each carbon pool
        carbon(:,k,m) = carbon(:,k,m) - fluxtot(:,k,icarbon)
        
        ! Fluxes towards the other pools (k -> kk)
        DO kk = 1, ncarb ! Loop over the carbon pools where the flux goes
          flux(:,k,kk,icarbon) = frac_carb(:,k,kk) * fluxtot(:,k,icarbon)
        ENDDO
        
      ENDDO ! End of loop over carbon pools
      
      !! 3.2.2 respiration
      !       VPP killer:
      !       resp_hetero_soil(:,m) = SUM( frac_resp(:,:) * fluxtot(:,:), DIM=2 ) / dt
      
      resp_hetero_soil(:,m) = &
         ( frac_resp(:,iactive) * fluxtot(:,iactive,icarbon) + &
         frac_resp(:,islow) * fluxtot(:,islow,icarbon) + &
         frac_resp(:,ipassive) * fluxtot(:,ipassive,icarbon)  ) / dt
!JCADD
!       resp_hetero_soil_part(:,iactive,m) = &
!            frac_resp(:,iactive) * fluxtot(:,iactive,icarbon)/dt
!       resp_hetero_soil_part(:,islow,m) = &
!            frac_resp(:,islow) * fluxtot(:,islow,icarbon)/dt
!       resp_hetero_soil_part(:,ipassive,m) = &
!            frac_resp(:,ipassive) * fluxtot(:,ipassive,icarbon)/dt
!ENDJCADD      
      !! 3.2.3 add fluxes to active, slow, and passive pools
      !       VPP killer:
      !       carbon(:,:,m) = carbon(:,:,m) + SUM( flux(:,:,:), DIM=2 )
      
      DO k = 1, ncarb ! Loop over carbon pools
        carbon(:,k,m) = carbon(:,k,m) + &
           flux(:,iactive,k,icarbon) + flux(:,ipassive,k,icarbon) + flux(:,islow,k,icarbon)
      ENDDO ! Loop over carbon pools
      
    ENDDO ! End loop over PFTs
    
 !! 4. (Quasi-)Analytical Spin-up
    
    !! 4.1.1 Finish to fill MatrixA with fluxes between soil pools
    
    IF (spinup_analytic) THEN

       DO m = 2,nvm 

          ! flux leaving the active pool
          MatrixA(:,m,iactive_pool,iactive_pool) = moins_un * &
               dt/carbon_tau(iactive) * &
               control_moist(:,ibelow) * control_temp(:,ibelow) * &
               ( 1. - flux_tot_coeff(3) * clay(:)) 

          ! flux received by the active pool from the slow pool
          MatrixA(:,m,iactive_pool,islow_pool) =  frac_carb(:,islow,iactive)*dt/carbon_tau(islow) * &
               control_moist(:,ibelow) * control_temp(:,ibelow)

          ! flux received by the active pool from the passive pool
          MatrixA(:,m,iactive_pool,ipassive_pool) =  frac_carb(:,ipassive,iactive)*dt/carbon_tau(ipassive) * &
               control_moist(:,ibelow) * control_temp(:,ibelow) 

          ! flux received by the slow pool from the active pool
          MatrixA(:,m,islow_pool,iactive_pool) =  frac_carb(:,iactive,islow) *&
               dt/carbon_tau(iactive) * &
               control_moist(:,ibelow) * control_temp(:,ibelow) * &
               ( 1. - flux_tot_coeff(3) * clay(:) ) 

          ! flux leaving the slow pool
          MatrixA(:,m,islow_pool,islow_pool) = moins_un * &
               dt/carbon_tau(islow) * &
               control_moist(:,ibelow) * control_temp(:,ibelow)

          ! flux received by the passive pool from the active pool
          MatrixA(:,m,ipassive_pool,iactive_pool) =  frac_carb(:,iactive,ipassive)* &
               dt/carbon_tau(iactive) * &
               control_moist(:,ibelow) * control_temp(:,ibelow) *&
               ( 1. - flux_tot_coeff(3) * clay(:) )

          ! flux received by the passive pool from the slow pool
          MatrixA(:,m,ipassive_pool,islow_pool) =  frac_carb(:,islow,ipassive) * &
               dt/carbon_tau(islow) * &
               control_moist(:,ibelow) * control_temp(:,ibelow)

          ! flux leaving the passive pool
          MatrixA(:,m,ipassive_pool,ipassive_pool) =  moins_un * &
               dt/carbon_tau(ipassive) * &
               control_moist(:,ibelow) * control_temp(:,ibelow)      


          IF ( (.NOT. natural(m)) .AND. (.NOT. is_c4(m)) ) THEN ! C3crop

             ! flux leaving the active pool
             MatrixA(:,m,iactive_pool,iactive_pool) = MatrixA(:,m,iactive_pool,iactive_pool) * &
                  flux_tot_coeff(1)  

             ! flux received by the active pool from the slow pool
             MatrixA(:,m,iactive_pool,islow_pool)= MatrixA(:,m,iactive_pool,islow_pool) * &
                  flux_tot_coeff(1) 

             ! flux received by the active pool from the passive pool
             MatrixA(:,m,iactive_pool,ipassive_pool) = MatrixA(:,m,iactive_pool,ipassive_pool) * &
                  flux_tot_coeff(1)    

             ! flux received by the slow pool from the active pool
             MatrixA(:,m,islow_pool,iactive_pool) =  MatrixA(:,m,islow_pool,iactive_pool) * &
                  flux_tot_coeff(1) 

             ! flux leaving the slow pool
             MatrixA(:,m,islow_pool,islow_pool) = MatrixA(:,m,islow_pool,islow_pool) * &
                  flux_tot_coeff(1)     

             ! flux received by the passive pool from the active pool
             MatrixA(:,m,ipassive_pool,iactive_pool) = MatrixA(:,m,ipassive_pool,iactive_pool) * &
                  flux_tot_coeff(1)

             ! flux received by the passive pool from the slow pool
             MatrixA(:,m,ipassive_pool,islow_pool) = MatrixA(:,m,ipassive_pool,islow_pool) * &
                  flux_tot_coeff(1)

             ! flux leaving the passive pool
             MatrixA(:,m,ipassive_pool,ipassive_pool) =  MatrixA(:,m,ipassive_pool,ipassive_pool) *&
                  flux_tot_coeff(1)

          ENDIF ! (.NOT. natural(m)) .AND. (.NOT. is_c4(m)) 


          IF ( (.NOT. natural(m)) .AND. is_c4(m) ) THEN ! C4crop

             ! flux leaving the active pool
             MatrixA(:,m,iactive_pool,iactive_pool) = MatrixA(:,m,iactive_pool,iactive_pool) * &
                  flux_tot_coeff(2)  

            ! flux received by the active pool from the slow pool
             MatrixA(:,m,iactive_pool,islow_pool)= MatrixA(:,m,iactive_pool,islow_pool) * &
                  flux_tot_coeff(2) 

             ! flux received by the active pool from the passive pool
             MatrixA(:,m,iactive_pool,ipassive_pool) = MatrixA(:,m,iactive_pool,ipassive_pool) * &
                  flux_tot_coeff(2)    

             ! flux received by the slow pool from the active pool
             MatrixA(:,m,islow_pool,iactive_pool) =  MatrixA(:,m,islow_pool,iactive_pool) * &
                  flux_tot_coeff(2) 

             ! flux leaving the slow pool
             MatrixA(:,m,islow_pool,islow_pool) = MatrixA(:,m,islow_pool,islow_pool) * &
                  flux_tot_coeff(2)     

             ! flux received by the passive pool from the active pool
             MatrixA(:,m,ipassive_pool,iactive_pool) = MatrixA(:,m,ipassive_pool,iactive_pool) * &
                  flux_tot_coeff(2)

             ! flux received by the passive pool from the slow pool
             MatrixA(:,m,ipassive_pool,islow_pool) = MatrixA(:,m,ipassive_pool,islow_pool) * &
                  flux_tot_coeff(2)

             ! flux leaving the passive pool
             MatrixA(:,m,ipassive_pool,ipassive_pool) =  MatrixA(:,m,ipassive_pool,ipassive_pool) * &
                  flux_tot_coeff(2)

          ENDIF ! (.NOT. natural(m)) .AND. is_c4(m) 

          IF (printlev>=4) WRITE(numout,*)'Finish to fill MatrixA'

       ENDDO ! Loop over # PFTS


       ! 4.2 Add Identity for each submatrix(7,7) 

       DO j = 1,nbpools
          MatrixA(:,:,j,j) = MatrixA(:,:,j,j) + un 
       ENDDO

    ENDIF ! (spinup_analytic)

    IF (printlev>=4) WRITE(numout,*) 'Leaving soilcarbon'
    
  END SUBROUTINE soilcarbon

END MODULE stomate_soilcarbon
