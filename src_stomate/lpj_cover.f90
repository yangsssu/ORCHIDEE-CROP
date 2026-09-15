! =================================================================================================================================
! MODULE       : lpj_cover
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
!                This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        Recalculate vegetation cover and LAI
!!
!!\n DESCRIPTION : None
!!
!! RECENT CHANGE(S) : Including permafrost carbon
!!
!! REFERENCE(S) : 
!!        Sitch, S., B. Smith, et al. (2003), Evaluation of ecosystem dynamics,
!!        plant geography and terrestrial carbon cycling in the LPJ dynamic 
!!        global vegetation model, Global Change Biology, 9, 161-185.\n
!!        Smith, B., I. C. Prentice, et al. (2001), Representation of vegetation
!!        dynamics in the modelling of terrestrial ecosystems: comparing two
!!        contrasting approaches within European climate space,
!!        Global Ecology and Biogeography, 10, 621-637.\n
!!
!! SVN :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_cover.f90 $
!! $Date: 2016-03-16 16:58:51 +0100 (Wed, 16 Mar 2016) $
!! $Revision: 3275 $
!! \n
!_ ================================================================================================================================

MODULE lpj_cover

  ! modules used:

  USE ioipsl_para
  USE stomate_data
  USE pft_parameters
  USE constantes_soil_var

  IMPLICIT NONE

  ! private & public routines

  PRIVATE
  PUBLIC cover

CONTAINS

!! ================================================================================================================================
!! SUBROUTINE     : lpj_cover
!!
!>\BRIEF          Recalculate vegetation cover and LAI
!!
!!\n DESCRIPTION : Veget_max is first renewed here according to newly calculated foliage biomass in this calculation step 
!! Then, litter, soil carbon, and biomass are also recalcuted with taking into account the changes in Veget_max (i.e. delta_veg)
!! Grid-scale fpc (foliage projected coverage) is calculated to obtain the shadede ground area by leaf's light capture
!! Finally, grid-scale fpc is adjusted not to exceed 1.0
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : ::lai (leaf area index, @tex $(m^2 m^{-2})$ @endtex), 
!! :: veget (fractional vegetation cover, unitless)
!!
!! REFERENCE(S)   : None
!! 
!! FLOWCHART :
!! \latexonly 
!!     \includegraphics[scale=0.5]{lpj_cover_flowchart.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

  SUBROUTINE cover (npts, cn_ind, ind, biomass, &
       veget_max, veget_max_old, lai, & 
       litter, litter_avail, litter_not_avail, carbon, &
       fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr, &
       turnover_daily, bm_to_litter, &
       co2_to_bm, co2_fire, resp_hetero, resp_maint, resp_growth, gpp_daily, &
       deepC_a, deepC_s, deepC_p)

!! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                                  :: npts             !! Domain size (unitless)  
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                :: cn_ind           !! Crown area 
                                                                                    !! @tex $(m^2)$ @endtex per individual
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                :: ind              !! Number of individuals 
                                                                                    !! @tex $(m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                :: veget_max_old    !! "Maximal" coverage fraction of a PFT (LAI-> 
                                                                                    !! infinity) on ground at beginning of time 

    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)             :: lai                 !! Leaf area index OF AN INDIVIDUAL PLANT 
                                                                                       !! @tex $(m^2 m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs,nelements), INTENT(inout) :: litter    !! Metabolic and structural litter, above and 
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_1hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_10hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_100hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(inout)                 :: fuel_1000hr
                                                                                       !! below ground @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nlitt,nvm), INTENT(inout):: litter_avail
    REAL(r_std), DIMENSION(npts,nlitt,nvm) , INTENT(inout):: litter_not_avail
    REAL(r_std), DIMENSION(npts,ncarb,nvm), INTENT(inout)             :: carbon        !! Carbon pool: active, slow, or passive 
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: biomass        !! Biomass @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)                  :: veget_max      !! "Maximal" coverage fraction of a PFT (LAI->
                                                                                       !! infinity) on ground (unitless)
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: turnover_daily !! Turnover rates (gC m^{-2} day^{-1})
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(inout) :: bm_to_litter   !! Conversion of biomass to litter 
                                                                                       !! @tex $(gC m^{-2} day^{-1})$ @endtex
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: co2_to_bm             !! biomass up take for establishment           
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: co2_fire
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: resp_hetero
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: resp_maint
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: resp_growth
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)           :: gpp_daily

    REAL(r_std), DIMENSION(npts,ndeep,nvm), INTENT(inout)         :: deepC_a           !! Permafrost soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(npts,ndeep,nvm), INTENT(inout)         :: deepC_s           !! Permafrost soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(npts,ndeep,nvm), INTENT(inout)         :: deepC_p           !! Permafrost soil carbon (g/m**3) passive

    !! 0.4 Local variables

    INTEGER(i_std)                                              :: i,j,k,m               !! Index (unitless)
    REAL(r_std), DIMENSION(npts,nlitt,nlevs,nelements)          :: dilu_lit              !! Litter dilution @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,ncarb)                          :: dilu_soil_carbon      !! Soil Carbondilution 
                                                                                         !! @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nparts,nelements)               :: dilu_bio              !! Biomass dilution 
    REAL(r_std), DIMENSION(npts)                                :: dilu_TCarbon
    REAL(r_std), DIMENSION(npts,nparts,nelements)               :: dilu_turnover_daily
    REAL(r_std), DIMENSION(npts,nparts,nelements)               :: dilu_bm_to_litter
    REAL(r_std), DIMENSION(npts)                                :: dilu_co2flux_new
    REAL(r_std), DIMENSION(npts)                                :: dilu_gpp_daily
    REAL(r_std), DIMENSION(npts)                                :: dilu_resp_growth
    REAL(r_std), DIMENSION(npts)                                :: dilu_resp_maint
    REAL(r_std), DIMENSION(npts)                                :: dilu_resp_hetero
    REAL(r_std), DIMENSION(npts)                                :: dilu_co2_to_bm
    REAL(r_std), DIMENSION(npts)                                :: dilu_co2_fire
    REAL(r_std), DIMENSION(npts,nvm)                            :: TCarbon
    REAL(r_std), DIMENSION(npts,nvm)                            :: co2flux_new
    REAL(r_std), DIMENSION(npts,nvm)                            :: co2flux_old
    REAL(r_std), DIMENSION(npts,ncarb,nvm)                       :: carbon_old
    REAL(r_std),DIMENSION(npts,ndeep,ncarb)                     :: dilu_soil_carbon_vertres !!vertically-resolved Soil Carbondilution (gC/m²)

    REAL(r_std), DIMENSION(nvm)                                 :: delta_veg        !! Conversion factors (unitless)
    REAL(r_std), DIMENSION(nvm)                                 :: reduct           !! Conversion factors (unitless)
    REAL(r_std)                                                 :: delta_veg_sum    !! Conversion factors (unitless)
    REAL(r_std)                                                 :: diff             !! Conversion factors (unitless)
    REAL(r_std)                                                 :: sr               !! Conversion factors (unitless)
    REAL(r_std), DIMENSION(npts)                                :: frac_nat         !! Conversion factors (unitless)
    REAL(r_std), DIMENSION(npts)                                :: sum_vegettree    !! Conversion factors (unitless)
    REAL(r_std), DIMENSION(npts)                                :: sum_vegetgrass   !! Conversion factors (unitless) 
    REAL(r_std), DIMENSION(npts)                                :: sum_veget_natveg !! Conversion factors (unitless)
    REAL(r_std), DIMENSION(npts)                                :: vartmp           !! Temporary variable used to add history
    REAL(r_std), DIMENSION(npts,nlitt,nelements)                :: dilu_f1hr        !! Litter dilution @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nlitt,nelements)                :: dilu_f10hr       !! Litter dilution @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nlitt,nelements)                :: dilu_f100hr      !! Litter dilution @tex $(gC m^{-2})$ @endtex
    REAL(r_std), DIMENSION(npts,nlitt,nelements)                :: dilu_f1000hr     !! Litter dilution @tex $(gC m^{-2})$ @endtex
!_ ================================================================================================================================

 !! 1. If the vegetation is dynamic, calculate new maximum vegetation cover for natural plants
  
    IF ( ok_dgvm ) THEN

       !! 1.1  Calculate initial values of vegetation cover
       frac_nat(:) = un
       sum_veget_natveg(:) = zero
       veget_max(:,ibare_sechiba) = un
       co2flux_new = undef
       co2flux_old = undef
       TCarbon = undef

       carbon_old(:,:,:)=carbon(:,:,:)

       DO j = 2,nvm ! loop over PFTs

          IF ( natural(j) ) THEN
        
             ! Summation of individual tree crown area to get total foliar projected coverage
             veget_max(:,j) = ind(:,j) * cn_ind(:,j)
             sum_veget_natveg(:) = sum_veget_natveg(:) + veget_max(:,j)

          ELSE
             
             !fraction occupied by agriculture needs to be substracted for the DGVM
             !this is used below to constrain veget for natural vegetation, see below
             frac_nat(:) = frac_nat(:) - veget_max(:,j)

          ENDIF

       ENDDO ! loop over PFTs

       DO i = 1, npts ! loop over grid points
      
          ! Recalculation of vegetation projected coverage when ::frac_nat was below ::sum_veget_natveg
          ! It means that non-natural vegetation will recover ::veget_max as natural vegetation
          IF (sum_veget_natveg(i) .GT. frac_nat(i) .AND. frac_nat(i) .GT. min_stomate) THEN

             DO j = 2,nvm ! loop over PFTs
                IF( natural(j) ) THEN
                   veget_max(i,j) =  veget_max(i,j) * frac_nat(i) / sum_veget_natveg(i)
                ENDIF
             ENDDO ! loop over PFTs

          ENDIF
       ENDDO ! loop over grid points
    
       ! Renew veget_max of bare soil as 0 to difference of veget_max (ibare_sechiba) 
       ! to current veget_max
       DO j = 2,nvm ! loop over PFTs
          veget_max(:,ibare_sechiba) = veget_max(:,ibare_sechiba) - veget_max(:,j)
       ENDDO ! loop over PFTs
       veget_max(:,ibare_sechiba) = MAX( veget_max(:,ibare_sechiba), zero )

       !! 1.2 Calculate carbon fluxes between PFTs to maintain mass balance
       !! Assure carbon closure when veget_max changes(delta_veg): if veget_max of some PFTs decrease, we use "dilu" to 
       !! record the corresponding lost in carbon (biomass, litter, soil carbon, gpp, respiration etc.) for 
       !! these PFTs, and re-allocate "dilu" to those PFTs with increasing veget_max.
       DO i = 1, npts ! loop over grid points
          
          ! Calculate the change in veget_max between previous time step and current time step
          delta_veg(:) = veget_max(i,:)-veget_max_old(i,:)
          delta_veg_sum = SUM(delta_veg,MASK=delta_veg.LT.zero)

          dilu_lit(i,:,:,:) = zero
          dilu_f1hr(i,:,:) = zero
          dilu_f10hr(i,:,:) = zero
          dilu_f100hr(i,:,:) = zero
          dilu_f1000hr(i,:,:) = zero
          dilu_soil_carbon(i,:) = zero
          dilu_soil_carbon_vertres(i,:,:) = zero

          dilu_bio(i,:,:) = zero
          dilu_TCarbon(i)=zero

          dilu_turnover_daily(i,:,:)=zero
          dilu_bm_to_litter(i,:,:)=zero
          dilu_co2flux_new(i)=zero
          dilu_gpp_daily(i)=zero
          dilu_resp_growth(i)=zero
          dilu_resp_maint(i)=zero
          dilu_resp_hetero(i)=zero
          dilu_co2_to_bm(i)=zero
          dilu_co2_fire(i)=zero

          ! Calculate TCarbon: total carbon including biomass, litter and soil carbon, as well as "today's" turnover and 
          ! bm_to_litter due to mortality, because today's turnover and bm_to_litter are not yet added into "litter" until tomorrow. 
          DO j=1, nvm
                TCarbon(i,j)=SUM(biomass(i,j,:,icarbon))+SUM(carbon(i,:,j))+SUM(litter(i,:,j,:,icarbon))+SUM(turnover_daily(i,j,:,icarbon))+SUM(bm_to_litter(i,j,:,icarbon))
                co2flux_old(i,j)=resp_maint(i,j)+resp_growth(i,j)+resp_hetero(i,j)+co2_fire(i,j)-co2_to_bm(i,j)-gpp_daily(i,j)
                co2flux_new(i,j)=resp_maint(i,j)+resp_growth(i,j)+resp_hetero(i,j)+co2_fire(i,j)-co2_to_bm(i,j)-gpp_daily(i,j)
          ENDDO

          DO j=1, nvm ! loop over PFTs
             IF ( delta_veg(j) < -min_stomate ) THEN
                dilu_lit(i,:,:,:) =  dilu_lit(i,:,:,:) + delta_veg(j) * litter(i,:,j,:,:) / delta_veg_sum
                dilu_f1hr(i,:,:) =  dilu_f1hr(i,:,:) + delta_veg(j) * fuel_1hr(i,j,:,:) / delta_veg_sum
                dilu_f10hr(i,:,:) =  dilu_f10hr(i,:,:) + delta_veg(j) * fuel_10hr(i,j,:,:) / delta_veg_sum
                dilu_f100hr(i,:,:) =  dilu_f100hr(i,:,:) + delta_veg(j) * fuel_100hr(i,j,:,:) / delta_veg_sum
                dilu_f1000hr(i,:,:) =  dilu_f1000hr(i,:,:) + delta_veg(j) * fuel_1000hr(i,j,:,:) / delta_veg_sum
                dilu_soil_carbon(i,:) =  dilu_soil_carbon(i,:) + delta_veg(j) * carbon(i,:,j) / delta_veg_sum
                dilu_TCarbon(i)= dilu_TCarbon(i) + delta_veg(j) * TCarbon(i,j) / delta_veg_sum
                dilu_turnover_daily(i,:,:)=dilu_turnover_daily(i,:,:)+delta_veg(j)*turnover_daily(i,j,:,:)/delta_veg_sum
                dilu_bm_to_litter(i,:,:)=dilu_bm_to_litter(i,:,:)+delta_veg(j)*bm_to_litter(i,j,:,:)/delta_veg_sum
                dilu_co2flux_new(i)=dilu_co2flux_new(i)+delta_veg(j)*co2flux_old(i,j)/delta_veg_sum
                dilu_gpp_daily(i)=dilu_gpp_daily(i)+delta_veg(j)*gpp_daily(i,j)/delta_veg_sum
                dilu_resp_growth(i)=dilu_resp_growth(i)+delta_veg(j)*resp_growth(i,j)/delta_veg_sum
                dilu_resp_maint(i)=dilu_resp_maint(i)+delta_veg(j)*resp_maint(i,j)/delta_veg_sum
                dilu_resp_hetero(i)=dilu_resp_hetero(i)+delta_veg(j)*resp_hetero(i,j)/delta_veg_sum
                dilu_co2_to_bm(i)=dilu_co2_to_bm(i)+delta_veg(j)*co2_to_bm(i,j)/delta_veg_sum
                dilu_co2_fire(i)=dilu_co2_fire(i)+delta_veg(j)*co2_fire(i,j)/delta_veg_sum
             ENDIF
          ENDDO ! loop over PFTs

          DO j=1, nvm ! loop over PFTs
             IF ( delta_veg(j) > min_stomate) THEN

                ! Dilution of reservoirs
                ! Recalculate the litter and soil carbon with taking into accout the change in 
                ! veget_max (delta_veg)
                ! Litter
                litter(i,:,j,:,:)=(litter(i,:,j,:,:) * veget_max_old(i,j) + dilu_lit(i,:,:,:) * delta_veg(j)) / veget_max(i,j)
                fuel_1hr(i,j,:,:)=(fuel_1hr(i,j,:,:) * veget_max_old(i,j) + dilu_f1hr(i,:,:) * delta_veg(j)) / veget_max(i,j)
                fuel_10hr(i,j,:,:)=(fuel_10hr(i,j,:,:) * veget_max_old(i,j) + dilu_f10hr(i,:,:) * delta_veg(j)) / veget_max(i,j)
                fuel_100hr(i,j,:,:)=(fuel_100hr(i,j,:,:) * veget_max_old(i,j) + dilu_f100hr(i,:,:) * delta_veg(j)) / veget_max(i,j)
                fuel_1000hr(i,j,:,:)=(fuel_1000hr(i,j,:,:) * veget_max_old(i,j) + dilu_f1000hr(i,:,:) * delta_veg(j)) / veget_max(i,j)
                !JCADD available and not available litter for grazing
                ! only not available litter change, available litter will not
                ! change, because tree litter can not be eaten
               IF (is_grassland_manag(j) .AND. is_grassland_grazed(j)) THEN
                 litter_avail(i,:,j) = litter_avail(i,:,j) * veget_max_old(i,j) / veget_max(i,j)
                 litter_not_avail(i,:,j) = litter(i,:,j,iabove,icarbon) - litter_avail(i,:,j)
               ENDIF
                !ENDJCADD    
                !IF ( ok_pc ) THEN
                !   deepC_a(i,:,j)=(deepC_a(i,:,j) * veget_max_old(i,j) + &
                !        dilu_soil_carbon_vertres(i,:,iactive) * delta_veg(j)) / veget_max(i,j)
                !   deepC_s(i,:,j)=(deepC_s(i,:,j) * veget_max_old(i,j) + &
                !        dilu_soil_carbon_vertres(i,:,islow) * delta_veg(j)) / veget_max(i,j)
                !   deepC_p(i,:,j)=(deepC_p(i,:,j) * veget_max_old(i,j) + &
                !        dilu_soil_carbon_vertres(i,:,ipassive) * delta_veg(j)) / veget_max(i,j)
                !ENDIF
                ! Soil carbon
                carbon(i,:,j)=(carbon(i,:,j) * veget_max_old(i,j) + dilu_soil_carbon(i,:) * delta_veg(j)) / veget_max(i,j)
                IF ( ok_pc ) THEN
                   IF (carbon_old(i,iactive,j) .GT. min_stomate) THEN
                      deepC_a(i,:,j)=deepC_a(i,:,j)*carbon(i,iactive,j)/carbon_old(i,iactive,j)
                   ENDIF
                   IF (carbon_old(i,islow,j) .GT. min_stomate) THEN
                      deepC_s(i,:,j)=deepC_s(i,:,j)*carbon(i,islow,j)/carbon_old(i,islow,j)
                   ENDIF
                   IF (carbon_old(i,ipassive,j) .GT. min_stomate) THEN
                      deepC_p(i,:,j)=deepC_p(i,:,j)*carbon(i,ipassive,j)/carbon_old(i,ipassive,j)
                   ENDIF
                ENDIF

                !biomass(i,j,:,:)=(biomass(i,j,:,:) * veget_max_old(i,j) + dilu_bio(i,:,:) * delta_veg(j)) / veget_max(i,j)
                TCarbon(i,j)=(TCarbon(i,j) * veget_max_old(i,j) + dilu_TCarbon(i) * delta_veg(j)) / veget_max(i,j)

                turnover_daily(i,j,:,:)=(turnover_daily(i,j,:,:)*veget_max_old(i,j)+dilu_turnover_daily(i,:,:)*delta_veg(j))/veget_max(i,j)
                bm_to_litter(i,j,:,:)=(bm_to_litter(i,j,:,:)*veget_max_old(i,j)+dilu_bm_to_litter(i,:,:)*delta_veg(j))/veget_max(i,j)
                co2flux_new(i,j)=(co2flux_old(i,j)*veget_max_old(i,j)+dilu_co2flux_new(i)*delta_veg(j))/veget_max(i,j)
                gpp_daily(i,j)=(gpp_daily(i,j)*veget_max_old(i,j)+dilu_gpp_daily(i)*delta_veg(j))/veget_max(i,j)
                resp_growth(i,j)=(resp_growth(i,j)*veget_max_old(i,j)+dilu_resp_growth(i)*delta_veg(j))/veget_max(i,j)
                resp_maint(i,j)=(resp_maint(i,j)*veget_max_old(i,j)+dilu_resp_maint(i)*delta_veg(j))/veget_max(i,j)
                resp_hetero(i,j)=(resp_hetero(i,j)*veget_max_old(i,j)+dilu_resp_hetero(i)*delta_veg(j))/veget_max(i,j)
                co2_to_bm(i,j)=(co2_to_bm(i,j)*veget_max_old(i,j)+dilu_co2_to_bm(i)*delta_veg(j))/veget_max(i,j)
                co2_fire(i,j)=(co2_fire(i,j)*veget_max_old(i,j)+dilu_co2_fire(i)*delta_veg(j))/veget_max(i,j)

             ENDIF

             IF(veget_max(i,j).GT.min_stomate) THEN

                ! Correct biomass densities to conserve mass
                ! since it's defined on veget_max
                biomass(i,j,:,:) = biomass(i,j,:,:) * veget_max_old(i,j) / veget_max(i,j)

             ENDIF

          ENDDO ! loop over PFTs
      ENDDO ! loop over grid points

      vartmp(:)=SUM(co2flux_new*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tCO2FLUX", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(co2flux_old*veget_max_old,dim=2)
      CALL histwrite_p (hist_id_stomate, "tCO2FLUX_OLD", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(TCarbon*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tCARBON", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(gpp_daily*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tGPP", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(resp_growth*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tRESP_GROWTH", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(resp_maint*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tRESP_MAINT", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(resp_hetero*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tRESP_HETERO", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(co2_to_bm*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tCO2_TAKEN", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(co2_fire*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tCO2_FIRE", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(biomass(:,:,:,icarbon),dim=3)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tBIOMASS", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(SUM(litter(:,:,:,:,icarbon),dim=4),dim=2)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tLITTER", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(fuel_1hr(:,:,:,icarbon),dim=3)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tFUEL1HR", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(fuel_10hr(:,:,:,icarbon),dim=3)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tFUEL10HR", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(fuel_100hr(:,:,:,icarbon),dim=3)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tFUEL100HR", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(fuel_1000hr(:,:,:,icarbon),dim=3)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tFUEL1000HR", itime, vartmp, npts, hori_index)
      vartmp(:)=SUM(SUM(carbon,dim=2)*veget_max,dim=2)
      CALL histwrite_p (hist_id_stomate, "tSOILC", itime, vartmp, npts, hori_index)

      IF ( ok_pc ) THEN
        vartmp(:)=SUM(SUM(deepC_a,dim=2)*veget_max,dim=2)
        CALL histwrite_p (hist_id_stomate, "tDEEPCa", itime, vartmp, npts, hori_index)
        vartmp(:)=SUM(SUM(deepC_s,dim=2)*veget_max,dim=2)
        CALL histwrite_p (hist_id_stomate, "tDEEPCs", itime, vartmp, npts, hori_index)
        vartmp(:)=SUM(SUM(deepC_p,dim=2)*veget_max,dim=2)
        CALL histwrite_p (hist_id_stomate, "tDEEPCp", itime, vartmp, npts, hori_index)
      ENDIF

   ENDIF

  END SUBROUTINE cover

END MODULE lpj_cover
