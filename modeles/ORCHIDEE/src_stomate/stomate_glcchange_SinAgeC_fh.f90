! =================================================================================================================================
! MODULE       : stomate_lcchange_fh
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       This module is a copy of stomate_lcchange. It includes the forestry 
!              harvest.
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): Including permafrost carbon
!!
!! REFERENCE(S)	: None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/albert.jornet/ORCHIDEE-MICT/src_stomate/stomate_lcchange.f90 $
!! $Date: 2015-07-30 15:38:45 +0200 (Thu, 30 Jul 2015) $
!! $Revision: 2847 $
!! \n
!_ ================================================================================================================================


MODULE stomate_glcchange_SinAgeC_fh

  ! modules used:
  
  USE ioipsl_para
  USE stomate_data
  USE pft_parameters
  USE constantes
  USE constantes_soil_var
  
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC gross_glcc_firstday_SinAgeC_fh, gross_glcchange_SinAgeC_fh
  
CONTAINS

! ================================================================================================================================
!! SUBROUTINE   : harvest_forest
!!
!>\BRIEF        : Handle forest harvest before its legacy is transferred to 
!                 newly initialized youngest-age-class PFT.
!!
!>\DESCRIPTION  
!_ ================================================================================================================================
  !!++TEMP++ biomass,veget_frac are not used because the remaining biomass to be 
  !! harvested is calculated within the deforestation fire module.
  SUBROUTINE harvest_forest (npts,ipts,ivm,biomass,frac,    &
                litter, deforest_biomass_remain,&
                fuel_1hr,fuel_10hr,&
                fuel_100hr,fuel_1000hr,&
                lignin_struc,&
                bm_to_litter_pro,convflux,prod10,prod100,&
                litter_pro, fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, &
                fuel_1000hr_pro, lignin_content_pro)


    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                       :: npts
    INTEGER, INTENT(in)                                       :: ipts
    INTEGER, INTENT(in)                                       :: ivm
    REAL(r_std), INTENT(in)                                   :: frac   !! the fraction of land covered by forest to be deforested
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: biomass      !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: fuel_1hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: fuel_10hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: fuel_100hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: fuel_1000hr
    REAL(r_std), DIMENSION(:,:,:,:,:), INTENT(in)             :: litter   !! Vegetmax-weighted remaining litter on the ground for 
                                                                                                      !! deforestation region.
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: deforest_biomass_remain  !! Vegetmax-weighted remaining biomass on the ground for 
                                                                                                      !! deforestation region.
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)         :: lignin_struc     !! ratio Lignine/Carbon in structural litter,
                                                                             !! above and below ground

    !! 0.2 Modified variables
    REAL(r_std), DIMENSION(:,:), INTENT(inout)               :: bm_to_litter_pro    !! conversion of biomass to litter 
                                                                              !! @tex ($gC m^{-2} day^{-1}$) @endtex
    REAL(r_std), DIMENSION(:), INTENT(inout)                 :: convflux         !! release during first year following land cover
                                                                                  !! change

    REAL(r_std), DIMENSION(npts,0:10), INTENT(inout)            :: prod10          !! products remaining in the 10 year-turnover
                                                                              !! pool after the annual release for each 
                                                                              !! compartment (10 + 1 : input from year of land
                                                                              !! cover change)
    REAL(r_std), DIMENSION(npts,0:100), INTENT(inout)           :: prod100         !! products remaining in the 100 year-turnover
                                                                              !! pool after the annual release for each 
                                                                              !! compartment (100 + 1 : input from year of land
                                                                              !! cover change)

    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)          :: litter_pro
    REAL(r_std), DIMENSION(:,:), INTENT(inout)            :: fuel_1hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(inout)            :: fuel_10hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(inout)            :: fuel_100hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(inout)            :: fuel_1000hr_pro
    REAL(r_std), DIMENSION(:),INTENT(inout)               :: lignin_content_pro



    !! 0.4 Local variables
    REAL(r_std)                                              :: above
      
    ! harvest of aboveground sap- and heartwood biomass after taking into
    ! account of deforestation fire
    IF (allow_deforest_fire) THEN
      above = deforest_biomass_remain(ipts,ivm,isapabove,icarbon)+ &
            deforest_biomass_remain(ipts,ivm,iheartabove,icarbon)
      convflux(ipts)  = convflux(ipts) + 0
      prod10(ipts,0)  = prod10(ipts,0) + 0.4*above
      prod100(ipts,0) = prod100(ipts,0) + 0.6*above
    ELSE
      above = (biomass(ipts,ivm,isapabove,icarbon)+ &
          biomass(ipts,ivm,iheartabove,icarbon))*frac
      convflux(ipts)  = convflux(ipts) + coeff_lcchange_1(ivm) * above
      prod10(ipts,0)  = prod10(ipts,0) + coeff_lcchange_10(ivm) * above 
      prod100(ipts,0) = prod100(ipts,0) + coeff_lcchange_100(ivm) * above 
    ENDIF
  
    ! the transfer of dead biomass to litter
    bm_to_litter_pro(isapbelow,:) = bm_to_litter_pro(isapbelow,:) +  &
                      biomass(ipts,ivm,isapbelow,:)*frac
    bm_to_litter_pro(iheartbelow,:) = bm_to_litter_pro(iheartbelow,:) + &
                      biomass(ipts,ivm,iheartbelow,:)*frac
    bm_to_litter_pro(iroot,:) = bm_to_litter_pro(iroot,:) + &
                      biomass(ipts,ivm,iroot,:)*frac
    bm_to_litter_pro(ifruit,:) = bm_to_litter_pro(ifruit,:) + &
                      biomass(ipts,ivm,ifruit,:)*frac
    bm_to_litter_pro(icarbres,:) = bm_to_litter_pro(icarbres,:) + &
                      biomass(ipts,ivm,icarbres,:)*frac
    bm_to_litter_pro(ileaf,:) = bm_to_litter_pro(ileaf,:) + &
                      biomass(ipts,ivm,ileaf,:)*frac

    !update litter_pro
    litter_pro(:,:,:) = litter_pro(:,:,:) + litter(ipts,:,ivm,:,:)*frac
    fuel_1hr_pro(:,:) = fuel_1hr_pro(:,:) + fuel_1hr(ipts,ivm,:,:)*frac
    fuel_10hr_pro(:,:) = fuel_10hr_pro(:,:) + fuel_10hr(ipts,ivm,:,:)*frac 
    fuel_100hr_pro(:,:) = fuel_100hr_pro(:,:) + fuel_100hr(ipts,ivm,:,:)*frac
    fuel_1000hr_pro(:,:) = fuel_1000hr_pro(:,:) + fuel_1000hr(ipts,ivm,:,:)*frac
    !don't forget to hanle litter lignin content
    lignin_content_pro(:)= lignin_content_pro(:) + &
      litter(ipts,istructural,ivm,:,icarbon)*frac*lignin_struc(ipts,ivm,:)

  END SUBROUTINE harvest_forest
  
! ================================================================================================================================
!! SUBROUTINE   : harvest_herb
!!
!>\BRIEF        : Handle herbaceous PFT clearing before its legacy is transferred to 
!                 newly initialized youngest-age-class PFT.
!!
!>\DESCRIPTION  
!_ ================================================================================================================================
  SUBROUTINE harvest_herb (ipts,ivm,biomass,veget_frac,bm_to_litter_pro)

    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                       :: ipts
    INTEGER, INTENT(in)                                       :: ivm
    REAL(r_std), INTENT(in)                                   :: veget_frac   !! the fraction of land covered by herbaceous PFT to be cleared
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: biomass      !! biomass @tex ($gC m^{-2}$) @endtex

    !! 0.2 Modified variables
    REAL(r_std), DIMENSION(:,:), INTENT(inout)                :: bm_to_litter_pro    



    ! the transfer of dead biomass to litter
    bm_to_litter_pro(:,:) = bm_to_litter_pro(:,:) + biomass(ipts,ivm,:,:)*veget_frac

  END SUBROUTINE harvest_herb


! ================================================================================================================================
!! SUBROUTINE   : initialize_proxy_pft
!!
!>\BRIEF        Initialize a proxy new youngest age class PFT.
!!
!>\DESCRIPTION  Initialize a proxy new youngest age class PFT that will be 
!!              merged with existing yongest age class, or fill the empty
!!              niche of the youngest age class PFT.
!_ ================================================================================================================================
  SUBROUTINE initialize_proxy_pft(ipts,ipft_young_agec,veget_max_pro,       &
                 biomass_pro, co2_to_bm_pro, ind_pro, age_pro,              & 
                 senescence_pro, PFTpresent_pro,                            &
                 lm_lastyearmax_pro, everywhere_pro, npp_longterm_pro,      &
                 leaf_frac_pro,leaf_age_pro)

    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                  :: ipts              !! 
    INTEGER, INTENT(in)                                  :: ipft_young_agec   !! index of the concerned youngest-age-class PFT
    REAL(r_std), INTENT(in)                              :: veget_max_pro     !! fraction of grid cell land area that's to be occupied

    !! 0.2 Modified variables
    REAL(r_std), INTENT(inout)                           :: co2_to_bm_pro

    !! 0.3 Output variables
    REAL(r_std), DIMENSION(:,:), INTENT(out)             :: biomass_pro     !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:), INTENT(out)               :: leaf_frac_pro   !! fraction of leaves in leaf age class 
    REAL(r_std), DIMENSION(:), INTENT(out)               :: leaf_age_pro    !! fraction of leaves in leaf age class 
    REAL(r_std), INTENT(out)     :: age_pro, ind_pro, lm_lastyearmax_pro
    REAL(r_std), INTENT(out)                             :: npp_longterm_pro 
    REAL(r_std), INTENT(out)                             :: everywhere_pro  !! is the PFT everywhere in the grid box or very 
    LOGICAL, INTENT(out)                                 :: senescence_pro  !! plant senescent (only for deciduous trees) Set
                                                                            !! to .FALSE. if PFT is introduced or killed
    LOGICAL, INTENT(out)                                 :: PFTpresent_pro  !! Is pft there (unitless)

    !! 0.4 Local variables
    !REAL(r_std), DIMENSION(npts,nvm)                     :: when_growthinit !! how many days ago was the beginning of the 
    !                                                                        !! growing season (days)

    REAL(r_std), DIMENSION(nparts,nelements)               :: bm_new          !! biomass increase @tex ($gC m^{-2}$) @endtex
    REAL(r_std) :: cn_ind,ind
    INTEGER  :: i,j,k,l

    ! -Note-
    ! This part of codes are copied from the original lcchange_main subroutine
    ! that initialize a new PFT.

    i=ipts
    j=ipft_young_agec

    !! Initialization of some variables
    leaf_frac_pro(:) = zero 
    leaf_age_pro(:) = zero 
    
    !! Initial setting of new establishment
    IF (is_tree(j)) THEN
       ! cn_sapl(j)=0.5; stomate_data.f90
       cn_ind = cn_sapl(j) 
    ELSE
       cn_ind = un
    ENDIF
    ind = veget_max_pro / cn_ind
    ind_pro = ind*veget_max_pro
    PFTpresent_pro = .TRUE.
    senescence_pro = .FALSE.
    everywhere_pro = 1.*veget_max_pro
    age_pro = zero

    ! large_value = 1.E33_r_std
    ! when_growthinit(i,j) = large_value 
    leaf_frac_pro(1) = 1.0 * veget_max_pro
    leaf_age_pro(1) = 1.0 * veget_max_pro   !This was not included in original lcchange_main subroutine
    npp_longterm_pro = npp_longterm_init * veget_max_pro
    lm_lastyearmax_pro = bm_sapl(j,ileaf,icarbon) * ind * veget_max_pro
    
    !!  Update of biomass in each each carbon stock component (leaf, sapabove, sapbelow,
    !>  heartabove, heartbelow, root, fruit, and carbres)\n
    DO k = 1, nparts ! loop over # carbon stock components, nparts = 8; stomate_constant.f90 
      DO l = 1,nelements ! loop over # elements
        biomass_pro(k,l) = ind * bm_sapl(j,k,l)
      END DO ! loop over # elements
      co2_to_bm_pro = co2_to_bm_pro + ind * bm_sapl(j,k,icarbon)
    ENDDO ! loop over # carbon stock components
    
  END SUBROUTINE initialize_proxy_pft

! ================================================================================================================================
!! SUBROUTINE   sap_take
!!
!>\BRIEF       : Take the sapling biomass of the new PFTs from the existing biomass, otherwise
!                take from co2_to_bm
!!
!>\DESCRIPTION  
!_ ================================================================================================================================
  SUBROUTINE sap_take (ipts,ivma,veget_max,biomass_pro,biomass,co2_to_bm_pro)

    INTEGER, INTENT(in)                                  :: ipts               !! 
    INTEGER, INTENT(in)                                  :: ivma
    REAL(r_std), DIMENSION(:,:), INTENT(in)              :: veget_max          !! "maximal" coverage fraction of a PFT (LAI ->
    REAL(r_std), DIMENSION(:,:), INTENT(in)              :: biomass_pro        !! biomass @tex ($gC m^{-2}$) @endtex

    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: biomass            !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std), INTENT(inout)                           :: co2_to_bm_pro

    
    REAL(r_std), DIMENSION(nparts,nelements)             :: biomass_total      !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std)                             :: bm_org,bmpro_share
    INTEGER                                 :: i,ivm,ipart
    
    biomass_total(:,:) = zero
    bm_org = zero
    bmpro_share = zero

    DO i = 1,nagec_pft(ivma)
      ivm = start_index(ivma)+i-1
      IF (veget_max(ipts,ivm) .GT. min_stomate) THEN
        biomass_total = biomass_total + biomass(ipts,ivm,:,:)*veget_max(ipts,ivm)
      ENDIF
    ENDDO
  
    DO ipart = 1, nparts
      IF (biomass_total(ipart,icarbon) .GT. biomass_pro(ipart,icarbon)) THEN
        co2_to_bm_pro = co2_to_bm_pro - biomass_pro(ipart,icarbon)
        !treat each PFT of the MTC
        DO i = 1,nagec_pft(ivma)
          ivm = start_index(ivma)+i-1
          IF (veget_max(ipts,ivm) .GT. min_stomate) THEN
            bm_org = biomass(ipts,ivm,ipart,icarbon) * veget_max(ipts,ivm)
            bmpro_share = bm_org/biomass_total(ipart,icarbon) * biomass_pro(ipart,icarbon)
            biomass(ipts,ivm,ipart,icarbon) = (bm_org - bmpro_share)/veget_max(ipts,ivm)
          ENDIF
        ENDDO
      ENDIF
    ENDDO
    
  END SUBROUTINE

! ================================================================================================================================
!! SUBROUTINE   collect_legacy_pft
!!
!>\BRIEF       : Collect the legacy variables that are going to be included
!                in the newly initialized PFT.
!!
!>\DESCRIPTION  
!_ ================================================================================================================================
  SUBROUTINE collect_legacy_pft(npts, ipts, ivma, glcc_pftmtc,    &
                biomass, bm_to_litter, carbon, litter,            &
                deepC_a, deepC_s, deepC_p,                        &
                fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr,     &
                lignin_struc, co2_to_bm, gpp_daily, npp_daily,    &
                resp_maint, resp_growth, resp_hetero, co2_fire,   &
                def_fuel_1hr_remain, def_fuel_10hr_remain,        &
                def_fuel_100hr_remain, def_fuel_1000hr_remain,    &
                deforest_litter_remain, deforest_biomass_remain,  &
                veget_max_pro, carbon_pro, lignin_struc_pro, litter_pro, &
                deepC_a_pro, deepC_s_pro, deepC_p_pro,            &
                fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, fuel_1000hr_pro, &
                bm_to_litter_pro, co2_to_bm_pro, gpp_daily_pro,   &
                npp_daily_pro, resp_maint_pro, resp_growth_pro,   &
                resp_hetero_pro, co2_fire_pro,                    &
                convflux,prod10,prod100)

    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                 :: npts               !! Domain size - number of pixels (unitless)
    INTEGER, INTENT(in)                                 :: ipts               !! Domain size - number of pixels (unitless)
    INTEGER, INTENT(in)                                 :: ivma               !! Index for metaclass
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)           :: glcc_pftmtc        !! a temporary variable to hold the fractions each PFT is going to lose
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)         :: biomass            !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)         :: bm_to_litter       !! Transfer of biomass to litter 
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)           :: carbon             !! carbon pool: active, slow, or passive 
                                                                              !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)           :: deepC_a            !! Permafrost soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)           :: deepC_s            !! Permafrost soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)           :: deepC_p            !! Permafrost soil carbon (g/m**3) passive
    REAL(r_std), DIMENSION(:,:,:,:,:), INTENT(in)       :: litter             !! metabolic and structural litter, above and 
                                                                              !! below ground @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)         :: fuel_1hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)         :: fuel_10hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)         :: fuel_100hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)         :: fuel_1000hr
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)           :: lignin_struc       !! ratio Lignine/Carbon in structural litter,
                                                                              !! above and below ground
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: co2_to_bm          !! biomass uptaken 
                                                                              !! @tex ($gC m^{-2} day^{-1}$) @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: gpp_daily          !! Daily gross primary productivity  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: npp_daily          !! Net primary productivity 
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: resp_maint         !! Maintenance respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: resp_growth        !! Growth respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: resp_hetero        !! Heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(in)             :: co2_fire           !! Heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: def_fuel_1hr_remain
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: def_fuel_10hr_remain
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: def_fuel_100hr_remain
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: def_fuel_1000hr_remain
    REAL(r_std), DIMENSION(:,:,:,:,:), INTENT(in)             :: deforest_litter_remain   !! Vegetmax-weighted remaining litter on the ground for 
                                                                                                      !! deforestation region.
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(in)               :: deforest_biomass_remain  !! Vegetmax-weighted remaining biomass on the ground for 
                                                                                                      !! deforestation region.

    !! 0.2 Output variables
    REAL(r_std), DIMENSION(:), INTENT(out)              :: carbon_pro
    REAL(r_std), DIMENSION(:), INTENT(out)              :: deepC_a_pro
    REAL(r_std), DIMENSION(:), INTENT(out)              :: deepC_s_pro
    REAL(r_std), DIMENSION(:), INTENT(out)              :: deepC_p_pro
    REAL(r_std), DIMENSION(:), INTENT(out)              :: lignin_struc_pro   !! ratio Lignine/Carbon in structural litter
                                                                              !! above and below ground
    REAL(r_std), DIMENSION(:,:,:), INTENT(out)          :: litter_pro
    REAL(r_std), DIMENSION(:,:), INTENT(out)            :: fuel_1hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(out)            :: fuel_10hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(out)            :: fuel_100hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(out)            :: fuel_1000hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(out)            :: bm_to_litter_pro
    REAL(r_std), INTENT(out)     :: veget_max_pro, co2_to_bm_pro
    REAL(r_std), INTENT(out)     :: gpp_daily_pro, npp_daily_pro
    REAL(r_std), INTENT(out)     :: resp_maint_pro, resp_growth_pro
    REAL(r_std), INTENT(out)     :: resp_hetero_pro, co2_fire_pro

    !! 0.3 Modified variables
    REAL(r_std), DIMENSION(:,:), INTENT(inout)                 :: convflux      !! release during first year following land cover
                                                                              !! change

    REAL(r_std), DIMENSION(npts,0:10,nwp), INTENT(inout)         :: prod10        !! products remaining in the 10 year-turnover
                                                                              !! pool after the annual release for each 
                                                                              !! compartment (10 + 1 : input from year of land
                                                                              !! cover change)
    REAL(r_std), DIMENSION(npts,0:100,nwp), INTENT(inout)        :: prod100       !! products remaining in the 100 year-turnover
                                                                              !! pool after the annual release for each 
                                                                              !! compartment (100 + 1 : input from year of land
                                                                              !! cover change)

    !! 0.4 Local variables
    REAL(r_std), DIMENSION(nlevs)                  :: lignin_content_pro
    REAL(r_std)                                    :: frac
    INTEGER                                        :: ivm


    ! All *_pro variables collect the legacy pools/fluxes of the ancestor
    ! PFTs for the receiving youngest age class. All *_pro variables 
    ! represent the quantity weighted by the fraction of ancestor contributing
    ! PFTs.
    ! Exceptions:
    ! lignin_struc_pro:: the ratio of lignin content in structural litter.

    veget_max_pro=zero
    carbon_pro(:)=zero
    deepC_a_pro(:)=zero
    deepC_s_pro(:)=zero
    deepC_p_pro(:)=zero
    lignin_struc_pro(:)=zero
    lignin_content_pro(:)=zero
    litter_pro(:,:,:)=zero
    fuel_1hr_pro(:,:)=zero
    fuel_10hr_pro(:,:)=zero
    fuel_100hr_pro(:,:)=zero
    fuel_1000hr_pro(:,:)=zero
    bm_to_litter_pro(:,:)=zero
    co2_to_bm_pro=zero
    gpp_daily_pro=zero
    npp_daily_pro=zero
    resp_maint_pro=zero
    resp_growth_pro=zero
    resp_hetero_pro=zero
    co2_fire_pro=zero

    DO ivm = 1,nvm
      frac = glcc_pftmtc(ipts,ivm,ivma)
      IF (frac>zero) THEN
        veget_max_pro = veget_max_pro+frac

        IF (is_tree(ivm)) THEN
          IF (is_tree(start_index(ivma))) THEN
            CALL harvest_forest (npts,ipts,ivm,biomass,frac,    &
                litter, deforest_biomass_remain,&
                fuel_1hr,fuel_10hr,&
                fuel_100hr,fuel_1000hr,&
                lignin_struc,&
                bm_to_litter_pro,convflux(:,iwphar),prod10(:,:,iwphar),prod100(:,:,iwphar),&
                litter_pro, fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, &
                fuel_1000hr_pro, lignin_content_pro)
          ELSE
            CALL harvest_forest (npts,ipts,ivm,biomass,frac,    &
                litter, deforest_biomass_remain,&
                fuel_1hr,fuel_10hr,&
                fuel_100hr,fuel_1000hr,&
                lignin_struc,&
                bm_to_litter_pro,convflux(:,iwplcc),prod10(:,:,iwplcc),prod100(:,:,iwplcc),&
                litter_pro, fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, &
                fuel_1000hr_pro, lignin_content_pro)
          ENDIF
        ELSE
          CALL harvest_herb(ipts,ivm,biomass,frac,   &
                  bm_to_litter_pro)
          litter_pro(:,:,:) = litter_pro(:,:,:) + litter(ipts,:,ivm,:,:)*frac
          fuel_1hr_pro(:,:) = fuel_1hr_pro(:,:) + fuel_1hr(ipts,ivm,:,:)*frac
          fuel_10hr_pro(:,:) = fuel_10hr_pro(:,:) + fuel_10hr(ipts,ivm,:,:)*frac
          fuel_100hr_pro(:,:) = fuel_100hr_pro(:,:) + fuel_100hr(ipts,ivm,:,:)*frac
          fuel_1000hr_pro(:,:) = fuel_1000hr_pro(:,:) + fuel_1000hr(ipts,ivm,:,:)*frac
          !don't forget to hanle litter lignin content
          lignin_content_pro(:)= lignin_content_pro(:) + &
            litter(ipts,istructural,ivm,:,icarbon)*lignin_struc(ipts,ivm,:)*frac
        ENDIF

        !! scalar variables to be accumulated and inherited
        !! by the destination PFT
        bm_to_litter_pro(:,:) = bm_to_litter_pro(:,:) + &
              bm_to_litter(ipts,ivm,:,:)*frac
        carbon_pro(:) = carbon_pro(:)+carbon(ipts,:,ivm)*frac
        deepC_a_pro(:) = deepC_a_pro(:)+deepC_a(ipts,:,ivm)*frac
        deepC_s_pro(:) = deepC_s_pro(:)+deepC_s(ipts,:,ivm)*frac
        deepC_p_pro(:) = deepC_p_pro(:)+deepC_p(ipts,:,ivm)*frac
        co2_to_bm_pro = co2_to_bm_pro + co2_to_bm(ipts,ivm)*frac

        gpp_daily_pro = gpp_daily_pro + gpp_daily(ipts,ivm)*frac
        npp_daily_pro = npp_daily_pro + npp_daily(ipts,ivm)*frac
        resp_maint_pro = resp_maint_pro + resp_maint(ipts,ivm)*frac
        resp_growth_pro = resp_growth_pro + resp_growth(ipts,ivm)*frac
        resp_hetero_pro = resp_hetero_pro + resp_hetero(ipts,ivm)*frac
        co2_fire_pro = co2_fire_pro + co2_fire(ipts,ivm)*frac
      ENDIF
    ENDDO

    WHERE (litter_pro(istructural,:,icarbon) .GT. min_stomate)
      lignin_struc_pro(:) = lignin_content_pro(:)/litter_pro(istructural,:,icarbon)
    ENDWHERE

  END SUBROUTINE collect_legacy_pft


! ================================================================================================================================
!! SUBROUTINE   gross_lcchange
!!
!>\BRIEF       : Apply gross land cover change.
!!
!>\DESCRIPTION  
!_ ================================================================================================================================
  SUBROUTINE gross_glcchange_SinAgeC_fh (npts, dt_days, harvest_matrix,   &
               glccSecondShift,glccPrimaryShift,glccNetLCC,&
               def_fuel_1hr_remain, def_fuel_10hr_remain,        &
               def_fuel_100hr_remain, def_fuel_1000hr_remain,    &
               deforest_litter_remain, deforest_biomass_remain,  &
               convflux, cflux_prod10, cflux_prod100,                  &
               glccReal, IncreDeficit, glcc_pft, glcc_pftmtc,          &
               veget_max, prod10, prod100, flux10, flux100,            &
               PFTpresent, senescence, moiavail_month, moiavail_week,  &
               gpp_week, ngd_minus5, resp_maint, resp_growth,          & 
               resp_hetero, npp_daily, when_growthinit, npp_longterm,  &
               ind, lm_lastyearmax, everywhere, age,                   &
               co2_to_bm, gpp_daily, co2_fire,                         &
               time_hum_min, gdd_midwinter, gdd_from_growthinit,       &
               gdd_m5_dormance, ncd_dormance,                          &
               lignin_struc, carbon, leaf_frac,                        &
               deepC_a, deepC_s, deepC_p,                              &
               leaf_age, bm_to_litter, biomass, litter,                &
               fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr)
  
    IMPLICIT NONE

    !! 0.1 Input variables

    INTEGER, INTENT(in)                                  :: npts             !! Domain size - number of pixels (unitless)
    REAL(r_std), INTENT(in)                              :: dt_days          !! Time step of vegetation dynamics for stomate
    REAL(r_std), DIMENSION (npts,12),INTENT(in)        :: glccSecondShift     !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                              !! used.
    REAL(r_std), DIMENSION (npts,12),INTENT(in)        :: glccPrimaryShift    !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                              !! used.
    REAL(r_std), DIMENSION (npts,12),INTENT(in)        :: glccNetLCC          !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                              !! used.
    REAL(r_std), DIMENSION (npts,12),INTENT(in)          :: harvest_matrix             !! 
                                                                             !! 

    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)                 :: def_fuel_1hr_remain
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)                 :: def_fuel_10hr_remain
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)                 :: def_fuel_100hr_remain
    REAL(r_std), DIMENSION(npts,nvm,nlitt,nelements), INTENT(in)                 :: def_fuel_1000hr_remain
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs,nelements), INTENT(in) :: deforest_litter_remain   !! Vegetmax-weighted remaining litter on the ground for 
                                                                                                      !! deforestation region.
    REAL(r_std), DIMENSION(npts,nvm,nparts,nelements), INTENT(in)      :: deforest_biomass_remain  !! Vegetmax-weighted remaining biomass on the ground for 
                                                                                                      !! deforestation region.


    !! 0.2 Output variables
    REAL(r_std), DIMENSION(npts,nwp), INTENT(out)            :: convflux         !! release during first year following land cover
                                                                             !! change
    REAL(r_std), DIMENSION(npts,nwp), INTENT(out)            :: cflux_prod10     !! total annual release from the 10 year-turnover
                                                                             !! pool @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(npts,nwp), INTENT(out)            :: cflux_prod100    !! total annual release from the 100 year-
    REAL(r_std), DIMENSION(npts,12), INTENT(inout)       :: glccReal         !! The "real" glcc matrix that we apply in the model
                                                                             !! after considering the consistency between presribed
                                                                             !! glcc matrix and existing vegetation fractions.
    REAL(r_std), DIMENSION(npts,4), INTENT(inout)        :: IncreDeficit     !! "Increment" deficits, negative values mean that 
                                                                             !! there are not enough fractions in the source PFTs
                                                                             !! /vegetations to target PFTs/vegetations. I.e., these
                                                                             !! fraction transfers are presribed in LCC matrix but
                                                                             !! not realized.
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: glcc_pft         !! Loss of fraction in each PFT
    REAL(r_std), DIMENSION(npts,nvm,nvmap), INTENT(inout):: glcc_pftmtc      !! a temporary variable to hold the fractions each PFT is going to lose
                                                                             !! i.e., the contribution of each PFT to the youngest age-class of MTC

    !! 0.3 Modified variables
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)      :: veget_max        !! "maximal" coverage fraction of a PFT (LAI ->
                                                                             !! infinity) on ground (unitless) 
    REAL(r_std), DIMENSION(npts,0:10,nwp), INTENT(inout)     :: prod10           !! products remaining in the 10 year-turnover
                                                                             !! pool after the annual release for each 
                                                                             !! compartment (10 + 1 : input from year of land
                                                                             !! cover change)
    REAL(r_std), DIMENSION(npts,0:100,nwp), INTENT(inout)    :: prod100          !! products remaining in the 100 year-turnover
                                                                             !! pool after the annual release for each 
                                                                             !! compartment (100 + 1 : input from year of land
                                                                             !! cover change)
    REAL(r_std), DIMENSION(npts,10,nwp), INTENT(inout)       :: flux10           !! annual release from the 10/100 year-turnover 
                                                                             !! pool compartments
    REAL(r_std), DIMENSION(npts,100,nwp), INTENT(inout)      :: flux100          !! annual release from the 10/100 year-turnover
                                                                             !! pool compartments
    LOGICAL, DIMENSION(:,:), INTENT(inout)               :: PFTpresent       !! Tab indicating which PFTs are present in 
                                                                             !! each pixel
    LOGICAL, DIMENSION(:,:), INTENT(inout)               :: senescence       !! Flag for setting senescence stage (only 
                                                                             !! for deciduous trees)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: moiavail_month   !! "Monthly" moisture availability (0 to 1, 
                                                                             !! unitless) 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: moiavail_week    !! "Weekly" moisture availability 
                                                                             !! (0 to 1, unitless)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: gpp_week         !! Mean weekly gross primary productivity 
                                                                             !! @tex $(gC m^{-2} day^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: ngd_minus5       !! Number of growing days (days), threshold 
                                                                             !! -5 deg C (for phenology)   
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: resp_maint       !! Maintenance respiration  
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: resp_growth      !! Growth respiration  
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: resp_hetero      !! Heterotrophic respiration  
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: npp_daily        !! Net primary productivity 
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: when_growthinit  !! How many days ago was the beginning of 
                                                                             !! the growing season (days)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: npp_longterm     !! "Long term" mean yearly primary productivity 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: ind              !! Number of individuals at the stand level
                                                                             !! @tex $(m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: lm_lastyearmax   !! last year's maximum leaf mass for each PFT 
                                                                             !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: everywhere       !! is the PFT everywhere in the grid box or 
                                                                             !! very localized (after its introduction) (?)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: age              !! mean age (years)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: co2_to_bm        !! CO2 taken from the atmosphere to get C to create  
                                                                             !! the seedlings @tex (gC.m^{-2}dt^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: gpp_daily        !! Daily gross primary productivity  
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: co2_fire         !! Fire carbon emissions
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: time_hum_min     !! Time elapsed since strongest moisture 
                                                                             !! availability (days) 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: gdd_midwinter    !! Growing degree days (K), since midwinter 
                                                                             !! (for phenology) - this is written to the
                                                                             !!  history files 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: gdd_from_growthinit !! growing degree days, since growthinit 
                                                                             !! for crops
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: gdd_m5_dormance  !! Growing degree days (K), threshold -5 deg 
                                                                             !! C (for phenology)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)           :: ncd_dormance     !! Number of chilling days (days), since 
                                                                             !! leaves were lost (for phenology) 
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: lignin_struc     !! ratio Lignine/Carbon in structural litter,
                                                                             !! above and below ground
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: carbon           !! carbon pool: active, slow, or passive 
                                                                             !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: deepC_a          !! Permafrost soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: deepC_s          !! Permafrost soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: deepC_p          !! Permafrost soil carbon (g/m**3) passive
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: leaf_frac        !! fraction of leaves in leaf age class (unitless;0-1)
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)         :: leaf_age         !! Leaf age (days)
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: bm_to_litter     !! Transfer of biomass to litter 
                                                                             !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: biomass          !! Stand level biomass @tex $(gC.m^{-2})$ @endtex
    REAL(r_std), DIMENSION(:,:,:,:,:), INTENT(inout)     :: litter           !! metabolic and structural litter, above and 
                                                                             !! below ground @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: fuel_1hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: fuel_10hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: fuel_100hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)       :: fuel_1000hr

    !! 0.4 Local variables
    REAL(r_std), DIMENSION(nparts,nelements)             :: bm_to_litter_pro !! conversion of biomass to litter 
                                                                             !! @tex ($gC m^{-2} day^{-1}$) @endtex
    REAL(r_std), DIMENSION(nparts,nelements)             :: biomass_pro      !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std)                                          :: veget_max_pro    !! "maximal" coverage fraction of a PFT (LAI ->
                                                                             !! infinity) on ground (unitless) 
    REAL(r_std), DIMENSION(ncarb)                        :: carbon_pro       !! carbon pool: active, slow, or passive 
                                                                             !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(ndeep)                        :: deepC_a_pro      !! Permafrost carbon pool: active, slow, or passive 
                                                                             !! @tex ($gC m^{-3}$) @endtex
    REAL(r_std), DIMENSION(ndeep)                        :: deepC_s_pro      !! Permafrost carbon pool: active, slow, or passive 
                                                                             !! @tex ($gC m^{-3}$) @endtex
    REAL(r_std), DIMENSION(ndeep)                        :: deepC_p_pro      !! Permafrost carbon pool: active, slow, or passive 
                                                                             !! @tex ($gC m^{-3}$) @endtex
    REAL(r_std), DIMENSION(nlitt,nlevs,nelements)        :: litter_pro       !! metabolic and structural litter, above and 
                                                                             !! below ground @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(nlitt,nelements)              :: fuel_1hr_pro
    REAL(r_std), DIMENSION(nlitt,nelements)              :: fuel_10hr_pro
    REAL(r_std), DIMENSION(nlitt,nelements)              :: fuel_100hr_pro
    REAL(r_std), DIMENSION(nlitt,nelements)              :: fuel_1000hr_pro
    REAL(r_std), DIMENSION(nlevs)                        :: lignin_struc_pro !! ratio Lignine/Carbon in structural litter
                                                                             !! above and below ground
    REAL(r_std), DIMENSION(nleafages)                    :: leaf_frac_pro    !! fraction of leaves in leaf age class 
    REAL(r_std), DIMENSION(nleafages)                    :: leaf_age_pro     !! fraction of leaves in leaf age class 
    LOGICAL                :: PFTpresent_pro, senescence_pro                 !! Is pft there (unitless)
    REAL(r_std)            :: ind_pro, age_pro, lm_lastyearmax_pro, npp_longterm_pro
    REAL(r_std)            :: everywhere_pro
    REAL(r_std)            :: gpp_daily_pro, npp_daily_pro, co2_to_bm_pro
    REAL(r_std)            :: resp_maint_pro, resp_growth_pro
    REAL(r_std)            :: resp_hetero_pro, co2_fire_pro
  
    INTEGER                :: ipts,ivm,ivma,l,m,ipft_young_agec
    CHARACTER(LEN=10)      :: part_str                               !! string suffix indicating an index

    REAL(r_std), DIMENSION(npts,nvmap)       :: glcc_mtc             !! Increase in fraction of each MTC in its youngest age-class
    REAL(r_std), DIMENSION(npts,nvm)         :: glccReal_tmp         !! A temporary variable to hold glccReal
    REAL(r_std), DIMENSION(npts)             :: Deficit_pf2yf_final     !! 
    REAL(r_std), DIMENSION(npts)             :: Deficit_sf2yf_final     !! 
    REAL(r_std), DIMENSION(npts)             :: pf2yf_compen_sf2yf      !! 
    REAL(r_std), DIMENSION(npts)             :: sf2yf_compen_pf2yf      !!
    REAL(r_std), DIMENSION(npts,nvm)         :: glcc_harvest            !! Loss of fraction due to forestry harvest

    WRITE(numout,*) 'Entering gross_lcchange_SinAgeC_fh'
    glcc_harvest(:,:) = zero
    glccReal_tmp(:,:) = zero

    !! Some initialization
    convflux(:,:)=zero
    prod10(:,0,:)         = zero
    prod100(:,0,:)        = zero   
    cflux_prod10(:,:)     = zero
    cflux_prod100(:,:)    = zero

    CALL gross_glcc_firstday_SinAgeC_fh(npts,veget_max,harvest_matrix,   &
                          glccSecondShift,glccPrimaryShift,glccNetLCC,&
                          glccReal,glcc_pft,glcc_pftmtc,IncreDeficit,  &
                          Deficit_pf2yf_final, Deficit_sf2yf_final,   &
                          pf2yf_compen_sf2yf, sf2yf_compen_pf2yf)

    glcc_mtc(:,:) = SUM(glcc_pftmtc,DIM=2)
    DO ipts=1,npts
      ! Note that we assume people don't intentionally change baresoil to 
      ! vegetated land.
      DO ivma = 2,nvmap
        ! we assume only the youngest age class receives the incoming PFT
        ! [chaoyuejoy@gmail.com 2015-08-04] This line is commented to allow
        ! the case of only single age class being handled.
        IF ( glcc_mtc(ipts,ivma) .GT. min_stomate ) THEN
          ipft_young_agec = start_index(ivma)

          ! 1. we accumulate the scalar variables that will be inherited
          !    note we don't handle the case of harvesting forest because
          !    we assume glcc_pftmtc(forest->forest) would be zero and this
          !    case won't occur as it's filtered by the condition of
          !    (frac>min_stomate)
          CALL collect_legacy_pft(npts, ipts, ivma, glcc_pftmtc,    &
                  biomass, bm_to_litter, carbon, litter,            &
                  deepC_a, deepC_s, deepC_p,                        &
                  fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr,     &
                  lignin_struc, co2_to_bm, gpp_daily, npp_daily,    &
                  resp_maint, resp_growth, resp_hetero, co2_fire,   &
                  def_fuel_1hr_remain, def_fuel_10hr_remain,        &
                  def_fuel_100hr_remain, def_fuel_1000hr_remain,    &
                  deforest_litter_remain, deforest_biomass_remain,  &
                  veget_max_pro, carbon_pro, lignin_struc_pro, litter_pro, &
                  deepC_a_pro, deepC_s_pro, deepC_p_pro,            &
                  fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, fuel_1000hr_pro, &
                  bm_to_litter_pro, co2_to_bm_pro, gpp_daily_pro,   &
                  npp_daily_pro, resp_maint_pro, resp_growth_pro,   &
                  resp_hetero_pro, co2_fire_pro,                    &
                  convflux,prod10,prod100)

          !++TEMP++
          ! Here we substract the outgoing fraction from the source PFT.
          ! If a too small fraction remains in this source PFT, then it is
          ! exhausted, we empty it. The subroutine 'empty_pft' might be 
          ! combined with 'collect_legacy_pft', but now we just put it here.
          DO ivm = 1,nvm
            IF( glcc_pftmtc(ipts,ivm,ivma)>min_stomate ) THEN
              veget_max(ipts,ivm) = veget_max(ipts,ivm)-glcc_pftmtc(ipts,ivm,ivma)
              IF ( veget_max(ipts,ivm)<min_stomate ) THEN
                CALL empty_pft(ipts, ivm, veget_max, biomass, ind,       &
                       carbon, litter, lignin_struc, bm_to_litter,       &
                       deepC_a, deepC_s, deepC_p,                        &
                       fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr,     &
                       gpp_daily, npp_daily, gpp_week, npp_longterm,     &
                       co2_to_bm, resp_maint, resp_growth, resp_hetero,  &
                       lm_lastyearmax, leaf_frac, leaf_age, age,         &
                       everywhere, PFTpresent, when_growthinit,          &
                       senescence, gdd_from_growthinit, gdd_midwinter,   &
                       time_hum_min, gdd_m5_dormance, ncd_dormance,      &
                       moiavail_month, moiavail_week, ngd_minus5)
              ENDIF
            ENDIF
          ENDDO

          ! 2. we establish a proxy PFT with the fraction of veget_max_pro,
          !    which is going to be either merged with existing target 
          !    `ipft_young_agec` PFT, or fill the place if no existing target PFT
          !    exits.
          CALL initialize_proxy_pft(ipts,ipft_young_agec,veget_max_pro,       &
                 biomass_pro, co2_to_bm_pro, ind_pro, age_pro,                & 
                 senescence_pro, PFTpresent_pro,                              &
                 lm_lastyearmax_pro, everywhere_pro, npp_longterm_pro,        &
                 leaf_frac_pro,leaf_age_pro)

          CALL sap_take (ipts,ivma,veget_max,biomass_pro,biomass,co2_to_bm_pro)

          ! 3. we merge the newly initiazlized proxy PFT into existing one
          !    or use it to fill an empty PFT slot.
          CALL add_incoming_proxy_pft(npts, ipts, ipft_young_agec, veget_max_pro,&
                 carbon_pro, litter_pro, lignin_struc_pro, bm_to_litter_pro,    &
                 deepC_a_pro, deepC_s_pro, deepC_p_pro,                         &
                 fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, fuel_1000hr_pro,  &
                 biomass_pro, co2_to_bm_pro, npp_longterm_pro, ind_pro,         &
                 lm_lastyearmax_pro, age_pro, everywhere_pro,                   &  
                 leaf_frac_pro, leaf_age_pro, PFTpresent_pro, senescence_pro,   &
                 gpp_daily_pro, npp_daily_pro, resp_maint_pro, resp_growth_pro, &
                 resp_hetero_pro, co2_fire_pro,                                 &
                 veget_max, carbon, litter, lignin_struc, bm_to_litter,         &
                 deepC_a, deepC_s, deepC_p,                                     &
                 fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr,                  &
                 biomass, co2_to_bm, npp_longterm, ind,                         &
                 lm_lastyearmax, age, everywhere,                               &
                 leaf_frac, leaf_age, PFTpresent, senescence,                   &
                 gpp_daily, npp_daily, resp_maint, resp_growth,                 &
                 resp_hetero, co2_fire)
          
        ENDIF !IF ( glcc_mtc(ipts,ivma) .GT. min_stomate )

      ENDDO
    ENDDO

    !! Update 10 year-turnover pool content following flux emission
    !!     (linear decay (10%) of the initial carbon input)
    DO  l = 0, 8
      m = 10 - l
      cflux_prod10(:,:) =  cflux_prod10(:,:) + flux10(:,m,:)
      prod10(:,m,:)     =  prod10(:,m-1,:)   - flux10(:,m-1,:)
      flux10(:,m,:)     =  flux10(:,m-1,:)
      WHERE (prod10(:,m,:) .LT. 1.0) prod10(:,m,:) = zero
    ENDDO
    
    cflux_prod10(:,:) = cflux_prod10(:,:) + flux10(:,1,:) 
    flux10(:,1,:)     = 0.1 * prod10(:,0,:)
    prod10(:,1,:)     = prod10(:,0,:)
    
    !! 2.4.3 update 100 year-turnover pool content following flux emission\n
    DO l = 0, 98
       m = 100 - l
       cflux_prod100(:,:)  =  cflux_prod100(:,:) + flux100(:,m,:)
       prod100(:,m,:)      =  prod100(:,m-1,:)   - flux100(:,m-1,:)
       flux100(:,m,:)      =  flux100(:,m-1,:)
       
       WHERE (prod100(:,m,:).LT.1.0) prod100(:,m,:) = zero
    ENDDO
    
    cflux_prod100(:,:)  = cflux_prod100(:,:) + flux100(:,1,:) 
    flux100(:,1,:)      = 0.01 * prod100(:,0,:)
    prod100(:,1,:)      = prod100(:,0,:)
    prod10(:,0,:)        = zero
    prod100(:,0,:)       = zero 

    convflux        = convflux/one_year*dt_days
    cflux_prod10    = cflux_prod10/one_year*dt_days
    cflux_prod100   = cflux_prod100/one_year*dt_days

    ! Write out history files
    CALL histwrite_p (hist_id_stomate, 'glcc_pft', itime, &
         glcc_pft, npts*nvm, horipft_index)

    glccReal_tmp(:,1:12) = glccReal
    CALL histwrite_p (hist_id_stomate, 'glccReal', itime, &
         glccReal_tmp, npts*nvm, horipft_index)

    ! Write out forestry harvest variables
    DO ipts = 1,npts
      DO ivm = 1,nvm
        DO ivma = 1,nvmap
          IF (is_tree(ivm) .AND. is_tree(start_index(ivma))) THEN
            glcc_harvest(ipts,ivm) = glcc_harvest(ipts,ivm) + glcc_pftmtc(ipts,ivm,ivma)
          ENDIF
        ENDDO
      ENDDO
    ENDDO
    CALL histwrite_p (hist_id_stomate, 'glcc_harvest', itime, &
         glcc_harvest, npts*nvm, horipft_index)

    glccReal_tmp(:,:) = zero
    glccReal_tmp(:,1:4) = IncreDeficit
    CALL histwrite_p (hist_id_stomate, 'IncreDeficit', itime, &
         glccReal_tmp, npts*nvm, horipft_index)

    glccReal_tmp(:,:) = zero
    glccReal_tmp(:,1) = Deficit_pf2yf_final
    glccReal_tmp(:,2) = Deficit_sf2yf_final
    glccReal_tmp(:,3) = pf2yf_compen_sf2yf
    glccReal_tmp(:,4) = sf2yf_compen_pf2yf

    CALL histwrite_p (hist_id_stomate, 'DefiComForHarvest', itime, &
         glccReal_tmp, npts*nvm, horipft_index)

    DO ivma = 1, nvmap
      WRITE(part_str,'(I2)') ivma
      IF (ivma < 10) part_str(1:1) = '0'
      CALL histwrite_p (hist_id_stomate, 'glcc_pftmtc_'//part_str(1:LEN_TRIM(part_str)), &
           itime, glcc_pftmtc(:,:,ivma), npts*nvm, horipft_index)
    ENDDO
  END SUBROUTINE gross_glcchange_SinAgeC_fh


! ================================================================================================================================
!! SUBROUTINE   : add_incoming_proxy_pft
!!
!>\BRIEF        : Merge the newly incoming proxy PFT cohort with the exisiting
!!                cohort.
!! \n
!
!_ ================================================================================================================================
  SUBROUTINE add_incoming_proxy_pft(npts, ipts, ipft, veget_max_pro,  &
       carbon_pro, litter_pro, lignin_struc_pro, bm_to_litter_pro,    &
       deepC_a_pro, deepC_s_pro, deepC_p_pro,                         &
       fuel_1hr_pro, fuel_10hr_pro, fuel_100hr_pro, fuel_1000hr_pro,  &
       biomass_pro, co2_to_bm_pro, npp_longterm_pro, ind_pro,         &
       lm_lastyearmax_pro, age_pro, everywhere_pro,                   &  
       leaf_frac_pro, leaf_age_pro, PFTpresent_pro, senescence_pro,   &
       gpp_daily_pro, npp_daily_pro, resp_maint_pro, resp_growth_pro, &
       resp_hetero_pro, co2_fire_pro,                                 &
       veget_max, carbon, litter, lignin_struc, bm_to_litter,         &
       deepC_a, deepC_s, deepC_p,                                     &
       fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr,                  &
       biomass, co2_to_bm, npp_longterm, ind,                         &
       lm_lastyearmax, age, everywhere,                               &
       leaf_frac, leaf_age, PFTpresent, senescence,                   &
       gpp_daily, npp_daily, resp_maint, resp_growth,                 &
       resp_hetero, co2_fire)
    
    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                :: npts                !! Domain size - number of pixels (unitless)
    INTEGER, INTENT(in)                                :: ipts                !! Domain size - number of pixels (unitless)
    INTEGER, INTENT(in)                                :: ipft
    REAL(r_std), INTENT(in)                            :: veget_max_pro           !! The land fraction of incoming new PFTs that are 
                                                                              !! the sum of all its ancestor PFTs
    REAL(r_std), DIMENSION(:), INTENT(in)              :: carbon_pro
    REAL(r_std), DIMENSION(:), INTENT(in)              :: deepC_a_pro
    REAL(r_std), DIMENSION(:), INTENT(in)              :: deepC_s_pro
    REAL(r_std), DIMENSION(:), INTENT(in)              :: deepC_p_pro
    REAL(r_std), DIMENSION(:,:,:), INTENT(in)          :: litter_pro
    REAL(r_std), DIMENSION(:,:), INTENT(in)            :: fuel_1hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(in)            :: fuel_10hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(in)            :: fuel_100hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(in)            :: fuel_1000hr_pro
    REAL(r_std), DIMENSION(:,:), INTENT(in)            :: bm_to_litter_pro
    REAL(r_std), DIMENSION(:), INTENT(in)              :: lignin_struc_pro    !! ratio Lignine/Carbon in structural litter
                                                                              !! above and below ground
    REAL(r_std), DIMENSION(:,:), INTENT(in)            :: biomass_pro         !! biomass @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:), INTENT(in)              :: leaf_frac_pro       !! fraction of leaves in leaf age class 
    REAL(r_std), DIMENSION(:), INTENT(in)              :: leaf_age_pro        !! fraction of leaves in leaf age class 
    REAL(r_std), INTENT(in)     :: ind_pro, age_pro, lm_lastyearmax_pro
    REAL(r_std), INTENT(in)     :: npp_longterm_pro, co2_to_bm_pro 
    REAL(r_std), INTENT(in)                            :: everywhere_pro      !! is the PFT everywhere in the grid box or very 
    LOGICAL, INTENT(in)         :: PFTpresent_pro, senescence_pro             !! Is pft there (unitless)

    REAL(r_std), INTENT(in)     :: gpp_daily_pro, npp_daily_pro
    REAL(r_std), INTENT(in)     :: resp_maint_pro, resp_growth_pro
    REAL(r_std), INTENT(in)     :: resp_hetero_pro, co2_fire_pro

    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: veget_max           !! "maximal" coverage fraction of a PFT on the ground
                                                                              !! May sum to
                                                                              !! less than unity if the pixel has
                                                                              !! nobio area. (unitless, 0-1)
   
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: carbon              !! carbon pool: active, slow, or passive 
                                                                              !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: deepC_a             !! Permafrost soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: deepC_s             !! Permafrost soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: deepC_p             !! Permafrost soil carbon (g/m**3) passive
    REAL(r_std), DIMENSION(:,:,:,:,:), INTENT(inout)   :: litter              !! metabolic and structural litter, above and 
                                                                              !! below ground @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_1hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_10hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_100hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_1000hr
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: lignin_struc        !! ratio Lignine/Carbon in structural litter,
                                                                              !! above and below ground
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: bm_to_litter        !! Transfer of biomass to litter 
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: biomass             !! Stand level biomass @tex $(gC.m^{-2})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: co2_to_bm           !! CO2 taken from the atmosphere to get C to create  
                                                                              !! the seedlings @tex (gC.m^{-2}dt^{-1})$ @endtex

    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: npp_longterm        !! "Long term" mean yearly primary productivity 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: ind                 !! Number of individuals at the stand level
                                                                              !! @tex $(m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: age                 !! mean age (years)
    LOGICAL, DIMENSION(:,:), INTENT(inout)             :: PFTpresent          !! Tab indicating which PFTs are present in 
                                                                              !! each pixel
    LOGICAL, DIMENSION(:,:), INTENT(inout)             :: senescence          !! Flag for setting senescence stage (only 
                                                                              !! for deciduous trees)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: lm_lastyearmax      !! last year's maximum leaf mass for each PFT 
                                                                              !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: everywhere          !! is the PFT everywhere in the grid box or 
                                                                              !! very localized (after its introduction) (?)
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: leaf_frac           !! fraction of leaves in leaf age class (unitless;0-1)
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: leaf_age            !! Leaf age (days)

    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gpp_daily           !! Daily gross primary productivity  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: npp_daily           !! Net primary productivity 
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: resp_maint          !! Maintenance respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: resp_growth         !! Growth respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: resp_hetero         !! Heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: co2_fire            !! Heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 

    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)        :: moiavail_month       !! "Monthly" moisture availability (0 to 1, 
    !                                                                           !! unitless) 
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: moiavail_week       !! "Weekly" moisture availability 
    !                                                                           !! (0 to 1, unitless)
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gpp_week            !! Mean weekly gross primary productivity 
    !                                                                           !! @tex $(gC m^{-2} day^{-1})$ @endtex
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: ngd_minus5          !! Number of growing days (days), threshold 
    !                                                                           !! -5 deg C (for phenology)   
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: when_growthinit     !! How many days ago was the beginning of 
    !                                                                           !! the growing season (days)
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: time_hum_min        !! Time elapsed since strongest moisture 
    !                                                                           !! availability (days) 
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gdd_midwinter       !! Growing degree days (K), since midwinter 
    !                                                                           !! (for phenology) - this is written to the
    !                                                                           !!  history files 
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gdd_from_growthinit !! growing degree days, since growthinit 
    !                                                                           !! for crops
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gdd_m5_dormance     !! Growing degree days (K), threshold -5 deg 
    !                                                                           !! C (for phenology)
    ! REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: ncd_dormance        !! Number of chilling days (days), since 
    !                                                                           !! leaves were lost (for phenology) 

    !! 0.4 Local variables

    INTEGER(i_std)                                     :: iele                !! Indeces(unitless)
    INTEGER(i_std)                                     :: ilit,ilev,icarb     !! Indeces(unitless)
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs,nelements) :: litter_old      !! metabolic and structural litter, above and 
                                                                              !! below ground @tex ($gC m^{-2}$) @endtex
    REAL(r_std) :: veget_old,veget_total
  
    
    ! Back up some variables in case they're needed later
    litter_old(:,:,:,:,:) = litter(:,:,:,:,:)

    !! General idea
    ! The established proxy vegetation has a fraction of 'veget_max_pro'; the
    ! existing iPFT has a fraction of veget_max(ipts,ipft). 
    ! Suppose we want to merge a scalar variable B, the value of B after merging
    ! is (Bi*Vi+Bj*Vj)/(Vi+Vj), where Vi is the original veget_max, Vj is the
    ! incoming veget_max. Note that in case Vi=0, this equation remains solid,
    ! i.e. the veget_max after merging is Vj and B after merging is Bj. In other
    ! words, the proxy vegetation "fills" up the empty niche of iPFT.
    ! Also note that for many scalar variables our input value is Bj*Vj, which
    ! is accumulated from multiple ancestor PFTs.
    veget_old = veget_max(ipts,ipft)
    veget_total = veget_old+veget_max_pro

    !! Different ways of handling merging depending on nature of variables:

    !! 1. Area-based scalar variables, use the equation above
    !  biomass,carbon, litter, bm_to_litter, co2_to_bm, ind,
    !  lm_lastyearmax, npp_longterm, lm_lastyearmax,
    !  lignin_struc (ratio variable depending on area-based variable)
     
    !! 2. Variables are tentatively handled like area-based variables:
    !   leaf_frac, leaf_age,

    !! 3. Variables that are overwritten by the newly initialized PFT:
    !   PFTpresent, senescence

    !! 4. Variables whose operation is uncertain and are not handled currently:
    !  when_growthinit :: how many days ago was the beginning of the growing season (days)
    !  gdd_from_growthinit :: growing degree days, since growthinit 
    !  gdd_midwinter, time_hum_min, gdd_m5_dormance, ncd_dormance, 
    !  moiavail_month, moiavail_week, ngd_minus5

    !! 5. Variables that concern with short-term fluxes that do not apply in
    !  this case:
    !  gpp_daily, npp_daily etc.

    ! Add the coming veget_max_pro into existing veget_max
    veget_max(ipts,ipft) = veget_total

    ! Merge scalar variables which are defined on area basis
    carbon(ipts,:,ipft) =  (veget_old * carbon(ipts,:,ipft) + &
         carbon_pro(:))/veget_total
    deepC_a(ipts,:,ipft) =  (veget_old * deepC_a(ipts,:,ipft) + &
         deepC_a_pro(:))/veget_total
    deepC_s(ipts,:,ipft) =  (veget_old * deepC_s(ipts,:,ipft) + &
         deepC_s_pro(:))/veget_total
    deepC_p(ipts,:,ipft) =  (veget_old * deepC_p(ipts,:,ipft) + &
         deepC_p_pro(:))/veget_total
    litter(ipts,:,ipft,:,:) = (veget_old * litter(ipts,:,ipft,:,:) + &
         litter_pro(:,:,:))/veget_total
    fuel_1hr(ipts,ipft,:,:) = (veget_old * fuel_1hr(ipts,ipft,:,:) + &
         fuel_1hr_pro(:,:))/veget_total
    fuel_10hr(ipts,ipft,:,:) = (veget_old * fuel_10hr(ipts,ipft,:,:) + &
         fuel_10hr_pro(:,:))/veget_total
    fuel_100hr(ipts,ipft,:,:) = (veget_old * fuel_100hr(ipts,ipft,:,:) + &
         fuel_100hr_pro(:,:))/veget_total
    fuel_1000hr(ipts,ipft,:,:) = (veget_old * fuel_1000hr(ipts,ipft,:,:) + &
         fuel_1000hr_pro(:,:))/veget_total

    WHERE (litter(ipts,istructural,ipft,:,icarbon) .GT. min_stomate) 
      lignin_struc(ipts,ipft,:) = (veget_old*litter_old(ipts,istructural,ipft,:,icarbon)* &
          lignin_struc(ipts,ipft,:) + litter_pro(istructural,:,icarbon)* &
          lignin_struc_pro(:))/(veget_total*litter(ipts,istructural,ipft,:,icarbon))
    ENDWHERE
    bm_to_litter(ipts,ipft,:,:) = (veget_old * bm_to_litter(ipts,ipft,:,:) + & 
         bm_to_litter_pro(:,:))/veget_total

    biomass(ipts,ipft,:,:) = (biomass(ipts,ipft,:,:)*veget_old + &
         biomass_pro(:,:))/veget_total
    co2_to_bm(ipts,ipft) = (veget_old*co2_to_bm(ipts,ipft) + &
         co2_to_bm_pro)/veget_total
    ind(ipts,ipft) = (ind(ipts,ipft)*veget_old + ind_pro)/veget_total
    lm_lastyearmax(ipts,ipft) = (lm_lastyearmax(ipts,ipft)*veget_old + &
         lm_lastyearmax_pro)/veget_total
    npp_longterm(ipts,ipft) = (veget_old * npp_longterm(ipts,ipft) + &
         npp_longterm_pro)/veget_total

    !CHECK: Here follows the original idea in DOFOCO, more strictly,
    ! leas mass should be considered together. The same also applies on
    ! leaf age.
    leaf_frac(ipts,ipft,:) = (leaf_frac(ipts,ipft,:)*veget_old + &
         leaf_frac_pro(:))/veget_total
    leaf_age(ipts,ipft,:) = (leaf_age(ipts,ipft,:)*veget_old + &
         leaf_age_pro(:))/veget_total
    age(ipts,ipft) = (veget_old * age(ipts,ipft) + &
         age_pro)/veget_total

    ! Everywhere deals with the migration of vegetation. Copy the
    ! status of the most migrated vegetation for the whole PFT
    everywhere(ipts,ipft) = MAX(everywhere(ipts,ipft), everywhere_pro)

    ! Overwrite the original variables with that from newly initialized
    ! proxy PFT
    PFTpresent(ipts,ipft) = PFTpresent_pro
    senescence(ipts,ipft) = senescence_pro

    ! This is to close carbon loop when writing history variables.
    gpp_daily(ipts,ipft) = (veget_old * gpp_daily(ipts,ipft) + &
         gpp_daily_pro)/veget_total
    npp_daily(ipts,ipft) = (veget_old * npp_daily(ipts,ipft) + &
         npp_daily_pro)/veget_total
    resp_maint(ipts,ipft) = (veget_old * resp_maint(ipts,ipft) + &
         resp_maint_pro)/veget_total 
    resp_growth(ipts,ipft) = (veget_old * resp_growth(ipts,ipft) + &
         resp_growth_pro)/veget_total 
    resp_hetero(ipts,ipft) = (veget_old * resp_hetero(ipts,ipft) + &
         resp_hetero_pro)/veget_total 
    co2_fire(ipts,ipft) = (veget_old * co2_fire(ipts,ipft) + &
         co2_fire_pro)/veget_total 

    ! Phenology- or time-related variables will be copied from original values if 
    ! there is already youngest-age-class PFT there, otherwise they're left
    ! untouched, because 1. to initiliaze all new PFTs here is wrong and 
    ! phenology is not explicitly considered, so we cannot assign a value
    ! to these variables. 2. We assume they will be correctly filled if 
    ! other variables are in place (e.g., non-zero leaf mass will lead to
    ! onset of growing season). In this case, merging a newly initialized PFT
    ! to an existing one is not the same as merging PFTs when they grow 
    ! old enough to exceed thresholds.
    
    ! gpp_week(ipts,ipft) = (veget_old * gpp_week(ipts,ipft) + &
    !      gpp_week_pro)/veget_total
    ! when_growthinit(ipts,ipft) = (veget_old * when_growthinit(ipts,ipft) + &
    !      when_growthinit_pro)/veget_total
    ! gdd_from_growthinit(ipts,ipft) = (veget_old * gdd_from_growthinit(ipts,ipft) + &
    !      gdd_from_growthinit_pro)/veget_total
    ! gdd_midwinter(ipts,ipft) = (veget_old * gdd_midwinter(ipts,ipft) + &
    !      gdd_midwinter_pro)/veget_total
    ! time_hum_min(ipts,ipft) = (veget_old * time_hum_min(ipts,ipft) + &
    !      time_hum_min_pro)/veget_total
    ! gdd_m5_dormance(ipts,ipft) = (veget_old * gdd_m5_dormance(ipts,ipft) + &
    !      gdd_m5_dormance_pro)/veget_total
    ! ncd_dormance(ipts,ipft) = (veget_old * ncd_dormance(ipts,ipft) + &
    !      ncd_dormance_pro)/veget_total
    ! moiavail_month(ipts,ipft) = (veget_old * moiavail_month(ipts,ipft) + &
    !      moiavail_month_pro)/veget_total
    ! moiavail_week(ipts,ipft) = (veget_old * moiavail_week(ipts,ipft) + &
    !      moiavail_week_pro)/veget_total
    ! ngd_minus5(ipts,ipft) = (veget_old * ngd_minus5(ipts,ipft) + &
    !      ngd_minus5_pro)/veget_total
    
  
  END SUBROUTINE add_incoming_proxy_pft


! ================================================================================================================================
!! SUBROUTINE   : empty_pft
!!
!>\BRIEF        : Empty a PFT when,
!!                - it is exhausted because of land cover change.
!!                - it moves to the next age class
!! \n
!_ ================================================================================================================================
  SUBROUTINE empty_pft(ipts, ivm, veget_max, biomass, ind,       &
               carbon, litter, lignin_struc, bm_to_litter,       &
               deepC_a, deepC_s, deepC_p,                        &
               fuel_1hr, fuel_10hr, fuel_100hr, fuel_1000hr,     &
               gpp_daily, npp_daily, gpp_week, npp_longterm,     &
               co2_to_bm, resp_maint, resp_growth, resp_hetero,  &
               lm_lastyearmax, leaf_frac, leaf_age, age,         &
               everywhere, PFTpresent, when_growthinit,          &
               senescence, gdd_from_growthinit, gdd_midwinter,   &
               time_hum_min, gdd_m5_dormance, ncd_dormance,      &
               moiavail_month, moiavail_week, ngd_minus5)
    
    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                :: ipts               !! index for grid cell
    INTEGER, INTENT(in)                                :: ivm                !! index for pft

    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: veget_max           !! "maximal" coverage fraction of a PFT on the ground
                                                                              !! May sum to
                                                                              !! less than unity if the pixel has
                                                                              !! nobio area. (unitless, 0-1)
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: biomass             !! Stand level biomass @tex $(gC.m^{-2})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: ind                 !! Number of individuals at the stand level
                                                                              !! @tex $(m^{-2})$ @endtex 
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: carbon              !! carbon pool: active, slow, or passive 
                                                                              !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: deepC_a             !! Permafrost soil carbon (g/m**3) active
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: deepC_s             !! Permafrost soil carbon (g/m**3) slow
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: deepC_p             !! Permafrost soil carbon (g/m**3) passive
    REAL(r_std), DIMENSION(:,:,:,:,:), INTENT(inout)   :: litter              !! metabolic and structural litter, above and 
                                                                              !! below ground @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_1hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_10hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_100hr
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: fuel_1000hr
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: lignin_struc        !! ratio Lignine/Carbon in structural litter,
                                                                              !! above and below ground
    REAL(r_std), DIMENSION(:,:,:,:), INTENT(inout)     :: bm_to_litter        !! Transfer of biomass to litter 
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gpp_daily           !! Daily gross primary productivity  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: npp_daily           !! Net primary productivity 
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gpp_week            !! Mean weekly gross primary productivity 
                                                                              !! @tex $(gC m^{-2} day^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: npp_longterm        !! "Long term" mean yearly primary productivity 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: co2_to_bm           !! CO2 taken from the atmosphere to get C to create  
                                                                              !! the seedlings @tex (gC.m^{-2}dt^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: resp_maint          !! Maintenance respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: resp_growth         !! Growth respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: resp_hetero         !! Heterotrophic respiration  
                                                                              !! @tex $(gC m^{-2} dtslow^{-1})$ @endtex 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: lm_lastyearmax      !! last year's maximum leaf mass for each PFT 
                                                                              !! @tex ($gC m^{-2}$) @endtex
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: leaf_frac           !! fraction of leaves in leaf age class (unitless;0-1)
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)       :: leaf_age            !! Leaf age (days)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: age                 !! mean age (years)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: everywhere          !! is the PFT everywhere in the grid box or 
                                                                              !! very localized (after its introduction) (?)
    LOGICAL, DIMENSION(:,:), INTENT(inout)             :: PFTpresent          !! Tab indicating which PFTs are present in 
                                                                              !! each pixel
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: when_growthinit     !! How many days ago was the beginning of 
                                                                              !! the growing season (days)
    LOGICAL, DIMENSION(:,:), INTENT(inout)             :: senescence          !! Flag for setting senescence stage (only 
                                                                              !! for deciduous trees)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gdd_from_growthinit !! growing degree days, since growthinit 
                                                                              !! for crops
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gdd_midwinter       !! Growing degree days (K), since midwinter 
                                                                              !! (for phenology) - this is written to the
                                                                              !!  history files 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: time_hum_min        !! Time elapsed since strongest moisture 
                                                                              !! availability (days) 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: gdd_m5_dormance     !! Growing degree days (K), threshold -5 deg 
                                                                              !! C (for phenology)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: ncd_dormance        !! Number of chilling days (days), since 
                                                                              !! leaves were lost (for phenology) 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: moiavail_month      !! "Monthly" moisture availability (0 to 1, 
                                                                              !! unitless) 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: moiavail_week       !! "Weekly" moisture availability 
                                                                              !! (0 to 1, unitless)
    REAL(r_std), DIMENSION(:,:), INTENT(inout)         :: ngd_minus5          !! Number of growing days (days), threshold 
                                                                              !! -5 deg C (for phenology)   

    !! 0.4 Local variables
    INTEGER(i_std)                                     :: iele                !! Indeces(unitless)
    INTEGER(i_std)                                     :: ilit,ilev,icarb     !! Indeces(unitless)

    veget_max(ipts,ivm) = zero
    ind(ipts,ivm) = zero
    biomass(ipts,ivm,:,:) = zero
    litter(ipts,:,ivm,:,:) = zero
    fuel_1hr(ipts,ivm,:,:) = zero
    fuel_10hr(ipts,ivm,:,:) = zero
    fuel_100hr(ipts,ivm,:,:) = zero
    fuel_1000hr(ipts,ivm,:,:) = zero
    carbon(ipts,:,ivm) = zero 
    deepC_a(ipts,:,ivm) = zero 
    deepC_s(ipts,:,ivm) = zero 
    deepC_p(ipts,:,ivm) = zero 
    bm_to_litter(ipts,ivm,:,:) = zero
    DO ilev=1,nlevs
       lignin_struc(ipts,ivm,ilev) = zero
    ENDDO
    npp_longterm(ipts,ivm) = zero
    gpp_daily(ipts,ivm) = zero 
    gpp_week(ipts,ivm) = zero
    resp_maint(ipts,ivm) = zero
    resp_growth(ipts,ivm) = zero
    resp_hetero(ipts,ivm) = zero
    npp_daily(ipts,ivm) = zero
    co2_to_bm(ipts,ivm) = zero
    lm_lastyearmax(ipts,ivm) = zero
    age(ipts,ivm) = zero
    leaf_frac(ipts,ivm,:) = zero
    leaf_age(ipts,ivm,:) = zero
    everywhere(ipts,ivm) = zero
    when_growthinit(ipts,ivm) = zero
    gdd_from_growthinit(ipts,ivm) = zero
    gdd_midwinter(ipts,ivm) = zero
    time_hum_min(ipts,ivm) = zero
    gdd_m5_dormance(ipts,ivm) = zero
    ncd_dormance(ipts,ivm) = zero
    moiavail_month(ipts,ivm) = zero
    moiavail_week(ipts,ivm) = zero
    ngd_minus5(ipts,ivm) = zero
    PFTpresent(ipts,ivm) = .FALSE.
    senescence(ipts,ivm) = .FALSE.

  END SUBROUTINE empty_pft

! ================================================================================================================================
!! SUBROUTINE   : gross_lcc_firstday
!!
!>\BRIEF        : When necessary, adjust input glcc matrix, and allocate it
!!                into different contributing age classes and receiving 
!!                youngest age classes.
!! \n
!_ ================================================================================================================================

  ! Note: it has this name because this subroutine will also be called
  ! the first day of each year to precalculate the forest loss for the
  ! deforestation fire module.
  SUBROUTINE gross_glcc_firstday_SinAgeC_fh(npts,veget_max_org,harvest_matrix, &
                          glccSecondShift,glccPrimaryShift,glccNetLCC,&
                          glccReal,glcc_pft,glcc_pftmtc,IncreDeficit, &
                          Deficit_pf2yf_final, Deficit_sf2yf_final,   &
                          pf2yf_compen_sf2yf, sf2yf_compen_pf2yf)

    IMPLICIT NONE

    !! 0.1 Input variables

    INTEGER, INTENT(in)                                     :: npts           !! Domain size - number of pixels (unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: veget_max_org  !! "maximal" coverage fraction of a PFT on the ground
                                                                              !! May sum to
                                                                              !! less than unity if the pixel has
                                                                              !! nobio area. (unitless, 0-1)
    REAL(r_std), DIMENSION(npts,12),INTENT(in)              :: harvest_matrix !! 
                                                                              !! 
    REAL(r_std), DIMENSION (npts,12),INTENT(in)        :: glccSecondShift     !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                              !! used.
    REAL(r_std), DIMENSION (npts,12),INTENT(in)        :: glccPrimaryShift    !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                              !! used.
    REAL(r_std), DIMENSION (npts,12),INTENT(in)        :: glccNetLCC          !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                              !! used.

    !! 0.2 Output variables
    REAL(r_std), DIMENSION(npts,nvm,nvmap), INTENT(inout)   :: glcc_pftmtc    !! a temporary variable to hold the fractions each PFT is going to lose
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)         :: glcc_pft       !! Loss of fraction in each PFT
    REAL(r_std), DIMENSION(npts,12), INTENT(inout)          :: glccReal       !! The "real" glcc matrix that we apply in the model
                                                                              !! after considering the consistency between presribed
                                                                              !! glcc matrix and existing vegetation fractions.
    REAL(r_std), DIMENSION(npts,4), INTENT(inout)           :: IncreDeficit   !! "Increment" deficits, negative values mean that 
                                                                              !! there are not enough fractions in the source PFTs
                                                                              !! /vegetations to target PFTs/vegetations. I.e., these
                                                                              !! fraction transfers are presribed in LCC matrix but
                                                                              !! not realized.
    REAL(r_std), DIMENSION(npts), INTENT(inout)    :: Deficit_pf2yf_final     !! 
    REAL(r_std), DIMENSION(npts), INTENT(inout)    :: Deficit_sf2yf_final     !! 
    REAL(r_std), DIMENSION(npts), INTENT(inout)    :: pf2yf_compen_sf2yf      !! 
    REAL(r_std), DIMENSION(npts), INTENT(inout)    :: sf2yf_compen_pf2yf      !!
     

    !! 0.3 Modified variables
    
    !! 0.4 Local variables
    REAL(r_std), DIMENSION (npts,12)                :: glcc                !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                           !! used.
    REAL(r_std), DIMENSION(npts,nvmap)              :: veget_mtc           !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts,nagec_tree)         :: vegagec_tree        !! fraction of tree age-class groups, in sequence of old->young
    REAL(r_std), DIMENSION(npts,nagec_herb)         :: vegagec_grass       !! fraction of grass age-class groups, in sequence of old->young 
    REAL(r_std), DIMENSION(npts,nagec_herb)         :: vegagec_pasture     !! fraction of pasture age-class groups, in sequence of old->young
    REAL(r_std), DIMENSION(npts,nagec_herb)         :: vegagec_crop        !! fraction of crop age-class groups, in sequence of old->young

    
    REAL(r_std), DIMENSION(npts,4)                  :: veget_4veg      !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts)                    :: veget_tree      !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts)                    :: veget_grass     !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts)                    :: veget_pasture   !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts)                    :: veget_crop      !! "maximal" coverage fraction of a PFT on the ground

    REAL(r_std), DIMENSION(npts,nvm)         :: veget_max         !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts,nvm)         :: veget_max_tmp     !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts,nvm)         :: veget_max_old     !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts,nvm)         :: glcc_pft_tmp      !! Loss of fraction in each PFT

    ! Different indexes for convenient local uses
    ! We define the rules for gross land cover change matrix:
    ! 1 forest->grass
    ! 2 forest->pasture 
    ! 3 forest->crop
    ! 4 grass->forest
    ! 5 grass->pasture
    ! 6 grass->crop
    ! 7 pasture->forest
    ! 8 pasture->grass
    ! 9 pasture->crop
    ! 10 crop->forest
    ! 11 crop->grass
    ! 12 crop->pasture
    INTEGER :: f2g=1, f2p=2, f2c=3
    INTEGER :: g2f=4, g2p=5, g2c=6, p2f=7, p2g=8, p2c=9, c2f=10, c2g=11, c2p=12

    INTEGER, ALLOCATABLE                  :: indall_tree(:)       !! Indices for all tree PFTs
    INTEGER, ALLOCATABLE                  :: indold_tree(:)       !! Indices for old tree cohort only
    INTEGER, ALLOCATABLE                  :: indagec_tree(:,:)    !! Indices for secondary tree cohorts, 
                                                                  !! note the sequence is old->young.
    INTEGER, ALLOCATABLE                  :: indall_grass(:)      !! Indices for all grass PFTs
    INTEGER, ALLOCATABLE                  :: indold_grass(:)      !! Indices for old grasses only
    INTEGER, ALLOCATABLE                  :: indagec_grass(:,:)   !! Indices for secondary grass cohorts
                                                                  !! note the sequence is old->young.
    INTEGER, ALLOCATABLE                  :: indall_pasture(:)    !! Indices for all pasture PFTs
    INTEGER, ALLOCATABLE                  :: indold_pasture(:)    !! Indices for old pasture only
    INTEGER, ALLOCATABLE                  :: indagec_pasture(:,:) !! Indices for secondary pasture cohorts
                                                                  !! note the sequence is old->young.
    INTEGER, ALLOCATABLE                  :: indall_crop(:)       !! Indices for all crop PFTs
    INTEGER, ALLOCATABLE                  :: indold_crop(:)       !! Indices for old crops only
    INTEGER, ALLOCATABLE                  :: indagec_crop(:,:)    !! Indices for secondary crop cohorts
                                                                  !! note the sequence is old->young.
    INTEGER :: num_tree_sinagec,num_tree_mulagec,num_grass_sinagec,num_grass_mulagec,     &
               num_pasture_sinagec,num_pasture_mulagec,num_crop_sinagec,num_crop_mulagec, &
               itree,itree2,igrass,igrass2,ipasture,ipasture2,icrop,icrop2,pf2yf,sf2yf
    INTEGER :: i,j,ivma,staind,endind,ivm


    REAL(r_std), DIMENSION(npts,12)         :: glccDef            !! Gross LCC deficit, negative values mean that there
                                                                  !! are not enough fractions in the source vegetations
                                                                  !! to the target ones as presribed by the LCC matrix.
    REAL(r_std), DIMENSION(npts)            :: Deficit_pf2yf      !! 
    REAL(r_std), DIMENSION(npts)            :: Deficit_sf2yf      !! 
    REAL(r_std), DIMENSION(npts)            :: Surplus_pf2yf      !! 
    REAL(r_std), DIMENSION(npts)            :: Surplus_sf2yf      !! 
    REAL(r_std), DIMENSION(npts,12)         :: HmatrixReal        !! 
    INTEGER :: ipts
    

    !! 1. We first build all different indices that we are going to use
    !!    in handling the PFT exchanges, three types of indices are built:
    !!     - for all age classes
    !!     - include only oldest age classes
    !!     - include all age classes excpet the oldest ones
    ! We have to build these indices because we would like to extract from
    ! donating PFTs in the sequnce of old->young age classes, and add in the
    ! receving PFTs only in the youngest-age-class PFTs. These indicies allow
    ! us to know where the different age classes are.

    num_tree_sinagec=0          ! number of tree PFTs with only one single age class 
                                ! considered as the oldest age class
    num_tree_mulagec=0          ! number of tree PFTs having multiple age classes
    num_grass_sinagec=0
    num_grass_mulagec=0
    num_pasture_sinagec=0
    num_pasture_mulagec=0
    num_crop_sinagec=0
    num_crop_mulagec=0
    
    !! 1.1 Calculate the number of PFTs for different MTCs and allocate
    !! the old and all indices arrays.

    ! [Note here the sequence to identify tree,pasture,grass,crop] is
    ! critical. The similar sequence is used in the subroutine "calc_cover".
    ! Do not forget to change the sequence there if you modify here.
    DO ivma =2,nvmap
      staind=start_index(ivma)
      IF (nagec_pft(ivma)==1) THEN
        IF (is_tree(staind)) THEN
          num_tree_sinagec = num_tree_sinagec+1
        ELSE IF (is_grassland_manag(staind)) THEN
          num_pasture_sinagec = num_pasture_sinagec+1
        ELSE IF (natural(staind)) THEN
          num_grass_sinagec = num_grass_sinagec+1
        ELSE
          num_crop_sinagec = num_crop_sinagec+1
        ENDIF

      ELSE
        IF (is_tree(staind)) THEN
          num_tree_mulagec = num_tree_mulagec+1
        ELSE IF (is_grassland_manag(staind)) THEN
          num_pasture_mulagec = num_pasture_mulagec+1
        ELSE IF (natural(staind)) THEN
          num_grass_mulagec = num_grass_mulagec+1
        ELSE
          num_crop_mulagec = num_crop_mulagec+1
        ENDIF
      ENDIF
    ENDDO
    
    !! Allocate index array
    ! allocate all index
    ALLOCATE(indall_tree(num_tree_sinagec+num_tree_mulagec*nagec_tree))     
    ALLOCATE(indall_grass(num_grass_sinagec+num_grass_mulagec*nagec_herb))     
    ALLOCATE(indall_pasture(num_pasture_sinagec+num_pasture_mulagec*nagec_herb))     
    ALLOCATE(indall_crop(num_crop_sinagec+num_crop_mulagec*nagec_herb))     

    ! allocate old-ageclass index
    ALLOCATE(indold_tree(num_tree_sinagec+num_tree_mulagec))     
    ALLOCATE(indold_grass(num_grass_sinagec+num_grass_mulagec))     
    ALLOCATE(indold_pasture(num_pasture_sinagec+num_pasture_mulagec))     
    ALLOCATE(indold_crop(num_crop_sinagec+num_crop_mulagec))     

    !! 1.2 Fill the oldest-age-class and all index arrays
    itree=0
    igrass=0
    ipasture=0
    icrop=0
    itree2=1
    igrass2=1
    ipasture2=1
    icrop2=1
    DO ivma =2,nvmap
      staind=start_index(ivma)
      IF (is_tree(staind)) THEN
        itree=itree+1
        indold_tree(itree) = staind+nagec_pft(ivma)-1
        DO j = 0,nagec_pft(ivma)-1
          indall_tree(itree2+j) = staind+j
        ENDDO
        itree2=itree2+nagec_pft(ivma)
      ELSE IF (natural(staind) .AND. .NOT. is_grassland_manag(staind)) THEN
        igrass=igrass+1
        indold_grass(igrass) = staind+nagec_pft(ivma)-1
        DO j = 0,nagec_pft(ivma)-1
          indall_grass(igrass2+j) = staind+j
        ENDDO
        igrass2=igrass2+nagec_pft(ivma)
      ELSE IF (is_grassland_manag(staind)) THEN
        ipasture = ipasture+1
        indold_pasture(ipasture) = staind+nagec_pft(ivma)-1
        DO j = 0,nagec_pft(ivma)-1
          indall_pasture(ipasture2+j) = staind+j
        ENDDO
        ipasture2=ipasture2+nagec_pft(ivma)
      ELSE
        icrop = icrop+1
        indold_crop(icrop) = staind+nagec_pft(ivma)-1
        DO j = 0,nagec_pft(ivma)-1
          indall_crop(icrop2+j) = staind+j
        ENDDO
        icrop2=icrop2+nagec_pft(ivma)
      ENDIF
    ENDDO
    
    !! 1.3 Allocate and fill other age class index

    ! [chaoyuejoy@gmail.com 2015-08-05]
    ! note that we treat the case of (num_tree_mulagec==0) differently. In this
    ! case there is no distinction of age groups among tree PFTs. But we still
    ! we want to use the "gross_lcchange" subroutine. In this case we consider 
    ! them as having a single age group. In the subroutines
    ! of "type_conversion" and "cross_give_receive", only the youngest-age-group
    ! PFTs of a given MTC or vegetation type could receive the incoming fractions.
    ! To be able to handle this case with least amount of code change, we assign the index 
    ! of PFT between youngest and second-oldes (i.e., indagec_tree etc) the same as
    ! those of oldest tree PFTs (or all tree PFTs because in this cases these two indices
    ! are identical) . So that this case could be correctly handled in the subrountines
    ! of "type_conversion" and "cross_give_receive". This treatment allows use
    ! of gross land cover change subroutine with only one single age class. This single
    ! age class is "simultanously the oldest and youngest age class". At the same
    ! time, we also change the num_tree_mulagec as the same of num_crop_sinagec.
    ! The similar case also applies in grass,pasture and crop.

    IF (num_tree_mulagec .EQ. 0) THEN
      ALLOCATE(indagec_tree(num_tree_sinagec,1))
      indagec_tree(:,1) = indall_tree(:)
      num_tree_mulagec = num_tree_sinagec
    ELSE
      ALLOCATE(indagec_tree(num_tree_mulagec,nagec_tree-1))     
    END IF

    IF (num_grass_mulagec .EQ. 0) THEN
      ALLOCATE(indagec_grass(num_grass_sinagec,1))
      indagec_grass(:,1) = indall_grass(:)
      num_grass_mulagec = num_grass_sinagec
    ELSE
      ALLOCATE(indagec_grass(num_grass_mulagec,nagec_herb-1))     
    END IF

    IF (num_pasture_mulagec .EQ. 0) THEN
      ALLOCATE(indagec_pasture(num_pasture_sinagec,1))
      indagec_pasture(:,1) = indall_pasture(:)
      num_pasture_mulagec = num_pasture_sinagec
    ELSE
      ALLOCATE(indagec_pasture(num_pasture_mulagec,nagec_herb-1))
    END IF

    IF (num_crop_mulagec .EQ. 0) THEN
      ALLOCATE(indagec_crop(num_crop_sinagec,1))
      indagec_crop(:,1) = indall_crop(:)
      num_crop_mulagec = num_crop_sinagec
    ELSE
      ALLOCATE(indagec_crop(num_crop_mulagec,nagec_herb-1))
    END IF

    ! fill the non-oldest age class index arrays when number of age classes
    ! is more than 1.
    ! [chaoyuejoy@gmail.com, 2015-08-05]
    ! Note the corresponding part of code  will be automatically skipped
    ! when nagec_tree ==1 and/or nagec_herb ==1, i.e., the assginment
    ! in above codes when original num_*_mulagec variables are zero will be retained.
    itree=0
    igrass=0
    ipasture=0
    icrop=0
    DO ivma = 2,nvmap
      staind=start_index(ivma)
      IF (nagec_pft(ivma) > 1) THEN
        IF (is_tree(staind)) THEN
          itree=itree+1
          DO j = 1,nagec_tree-1
            indagec_tree(itree,j) = staind+nagec_tree-j-1
          ENDDO
        ELSE IF (natural(staind) .AND. .NOT. is_grassland_manag(staind)) THEN
          igrass=igrass+1
          DO j = 1,nagec_herb-1
            indagec_grass(igrass,j) = staind+nagec_herb-j-1
          ENDDO
        ELSE IF (is_grassland_manag(staind)) THEN
          ipasture=ipasture+1
          DO j = 1,nagec_herb-1
            indagec_pasture(ipasture,j) = staind+nagec_herb-j-1
          ENDDO
        ELSE
          icrop=icrop+1
          DO j = 1,nagec_herb-1
            indagec_crop(icrop,j) = staind+nagec_herb-j-1
          ENDDO
        ENDIF
      ENDIF
    ENDDO


    ! we make copies of original input veget_max
    ! veget_max will be modified through different operations in order to 
    ! check various purposes, e.g., whether input glcc is compatible with
    ! existing veget_max and how to allocate it etc.
    ! veget_max_old will not be modified
    veget_max(:,:) = veget_max_org(:,:)
    veget_max_old(:,:) = veget_max_org(:,:)

    !! 2. Calcuate the fractions covered by tree, grass, pasture and crops
    !!    for each age class

    !************************************************************************!
    !****block to calculate fractions for basic veg types and age classes ***!
    ! Note:
    ! 1. "calc_cover" subroutine does not depend on how many age classes
    ! there are in each MTC.
    ! 2. Fraction of baresoil is excluded here. This means transformation
    ! of baresoil to a vegetated PFT is excluded in gross land cover change.
    veget_mtc(:,:) = 0.
    vegagec_tree(:,:) = 0.
    vegagec_grass(:,:) = 0.
    vegagec_pasture(:,:) = 0.
    vegagec_crop(:,:) = 0.


    CALL calc_cover(npts,veget_max,veget_mtc,vegagec_tree,vegagec_grass, &
           vegagec_pasture,vegagec_crop)
 
    veget_tree(:) = SUM(vegagec_tree(:,:),DIM=2)
    veget_grass(:) = SUM(vegagec_grass(:,:),DIM=2)
    veget_pasture(:) = SUM(vegagec_pasture(:,:),DIM=2)
    veget_crop(:) = SUM(vegagec_crop(:,:),DIM=2)
    itree=1
    igrass=2
    ipasture=3
    icrop=4
    veget_4veg(:,itree) = veget_tree(:)
    veget_4veg(:,igrass) = veget_grass(:)
    veget_4veg(:,ipasture) = veget_pasture(:)
    veget_4veg(:,icrop) = veget_crop(:)
    !****end block to calculate fractions for basic veg types and age classes ***!
    !****************************************************************************!

    !********************** block to handle forestry harvest ****************
    !! 2B. Here we handle the forestry wood harvest
    ! Rules:
    ! 1. We take first from second oldest forest, then oldest forest
    
    pf2yf=1   !primary to young forest conversion because of harvest
    sf2yf=2   !old secondary to young forest conversion because of harvest
    
    !! Note that Deficit_pf2yf and Deficit_sf2yf are temporary, intermediate
    !! variables. The final deficits after mutual compensation are stored in 
    !! Deficit_pf2yf_final and Deficit_sf2yf_final.
    Deficit_pf2yf(:) = zero 
    Deficit_sf2yf(:) = zero
    Deficit_pf2yf_final(:) = zero 
    Deficit_sf2yf_final(:) = zero

    !! Note that both Surplus_pf2yf and Surplus_sf2yf and temporary intermediate
    !! variables, the final surplus after mutual compensation are not outputed.
    Surplus_pf2yf(:) = zero
    Surplus_sf2yf(:) = zero

    !! Note in the naming of pf2yf_compen_sf2yf and sf2yf_compen_pf2yf, active
    !! tense is used.
    pf2yf_compen_sf2yf(:) = zero  !primary->young conversion that compensates 
                               !the secondary->young conversion because of deficit
                               !in the latter
    sf2yf_compen_pf2yf(:) = zero  !seondary->young conversion that compensates
                               !the primary->young conversion because of the deficit
                               !in the latter
    

    !! Define the "real" harvest matrix after considering the mutual compenstation
    !! between primary->young and secondary->young transitions.
    HmatrixReal(:,:) = zero  !Harvest matrix real, used to hold the 
                                       !harvest matrix after considering the mutual
                                       !compensation between primary and old secondary
                                       !forest

    ! we sum together harvest from primary and secondary forest and consider 
    ! as all happening on parimary forest.
    HmatrixReal(:,1) = harvest_matrix(:,pf2yf) + harvest_matrix(:,sf2yf)

    ! Check the availability of forest fractions for harvest
    WHERE (veget_tree(:) .LE. HmatrixReal(:,1)) 
      Deficit_pf2yf_final(:) = veget_tree(:)-HmatrixReal(:,1)
      HmatrixReal(:,1) = veget_tree(:)
    ENDWHERE


    glcc_pft(:,:) = 0.
    glcc_pft_tmp(:,:) = 0.
    glcc_pftmtc(:,:,:) = 0.

    !! Allocate harvest-caused out-going primary and secondary forest fraction
    !! into different primary and secondary forest PFTs. 
    ! [Note: here we need only glcc_pft, but not glcc_pft_tmp and glcc_pftmtc.
    ! The latter two variables will be set to zero again when handling LCC in 
    ! later sections.]
    DO ipts=1,npts
      !pf2yf
      CALL type_conversion(ipts,pf2yf,HmatrixReal,veget_mtc,  &
                       indold_tree,indagec_tree,indagec_crop,num_crop_mulagec, &
                       1,nagec_herb,               &
                       vegagec_tree,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
    ENDDO

    ! Because we use the container of type_conversion, now the glcc_pft_tmp
    ! and glcc_pftmtc have wrong information (because harvest loss is assigned
    ! on the newly created youngest-age-class pasture/crop MTCs). So they have 
    ! to be re-initialized to zero. Only the information in glcc_pft is what
    ! we need.
    glcc_pft_tmp(:,:) = 0.
    glcc_pftmtc(:,:,:) = 0.
    !Here we need to put glcc_pft into glcc_pftmtc for forestry harvest.
    !The same MTC will be maintained when forest is harvested.
    DO ivm =1,nvm
      IF (is_tree(ivm)) THEN
        glcc_pftmtc(:,ivm,pft_to_mtc(ivm)) = glcc_pft(:,ivm)
      ENDIF 
    ENDDO
    !****************** end block to handle forestry harvest ****************
    veget_max_tmp(:,:) = veget_max(:,:)


    !************************************************************************!
    !****block to calculate fractions for basic veg types and age classes ***!
    ! Note:
    ! 1. "calc_cover" subroutine does not depend on how many age classes
    ! there are in each MTC.
    ! 2. Fraction of baresoil is excluded here. This means transformation
    ! of baresoil to a vegetated PFT is excluded in gross land cover change.
    veget_mtc(:,:) = 0.
    vegagec_tree(:,:) = 0.
    vegagec_grass(:,:) = 0.
    vegagec_pasture(:,:) = 0.
    vegagec_crop(:,:) = 0.


    CALL calc_cover(npts,veget_max,veget_mtc,vegagec_tree,vegagec_grass, &
           vegagec_pasture,vegagec_crop)
 
    veget_tree(:) = SUM(vegagec_tree(:,:),DIM=2)
    veget_grass(:) = SUM(vegagec_grass(:,:),DIM=2)
    veget_pasture(:) = SUM(vegagec_pasture(:,:),DIM=2)
    veget_crop(:) = SUM(vegagec_crop(:,:),DIM=2)
    itree=1
    igrass=2
    ipasture=3
    icrop=4
    veget_4veg(:,itree) = veget_tree(:)
    veget_4veg(:,igrass) = veget_grass(:)
    veget_4veg(:,ipasture) = veget_pasture(:)
    veget_4veg(:,icrop) = veget_crop(:)
    !****end block to calculate fractions for basic veg types and age classes ***!
    !****************************************************************************!

    !! 3. Decompose the LCC matrix to different PFTs
    !! We do this through several steps:
    !  3.1 Check whether input LCC matrix is feasible with current PFT fractions
    !      (i.e., the fractions of forest,grass,pasture and crops)
    !      and if not, adjust the transfer matrix by compensating the deficits
    !      using the surpluses.
    !  3.2 Allocate the decreasing fractions of tree/grass/pasture/crop to their
    !      respective age classes, in the sequences of old->young.
    !  3.3 Allocate the incoming fractions of tree/grass/pasture/crop to their
    !      respective youngest age classes. The incoming fractions are distributed
    !      according to the existing fractions of youngest-age-class PFTs of the
    !      same receiving vegetation type. If none of them exists, the incoming
    !      fraction is distributed equally.

    !!  3.1 Adjust LCC matrix if it's not feasible with current PFT fractions

    glcc(:,:) = glccSecondShift+glccPrimaryShift+glccNetLCC
    IncreDeficit(:,:) = 0.
    glccReal(:,:) = 0.
    glccDef(:,:) = 0.

    !to crop - sequence: p2c,g2c,f2c
    CALL glcc_compensation_full(npts,veget_4veg,glcc,glccReal,glccDef, &
                           p2c,ipasture,g2c,igrass,f2c,itree,icrop, &
                           IncreDeficit)

    !to pasture - sequence: g2p,c2p,f2p
    CALL glcc_compensation_full(npts,veget_4veg,glcc,glccReal,glccDef, &
                           g2p,igrass,c2p,icrop,f2p,itree,ipasture, &
                           IncreDeficit)

    !to grass - sequence: p2g,c2g,f2g
    CALL glcc_compensation_full(npts,veget_4veg,glcc,glccReal,glccDef, &
                           p2g,ipasture,c2g,icrop,f2g,itree,igrass, &
                           IncreDeficit)

    !to forest - sequence: c2f,p2f,g2f
    CALL glcc_compensation_full(npts,veget_4veg,glcc,glccReal,glccDef, &
                           c2f,icrop,p2f,ipasture,g2f,igrass,itree, &
                           IncreDeficit)

    !!  3.2 & 3.3 Allocate LCC matrix to different PFTs/age-classes

    ! because we use veget_max as a proxy variable and it has been changed
    ! when we derive the glccReal, so here we have to recover its original
    ! values, which is veget_max_tmp after the forestry harvest.
    veget_max(:,:) = veget_max_tmp(:,:)

    ! Calculate again fractions for different age-classes.
    veget_mtc(:,:) = 0.
    vegagec_tree(:,:) = 0.
    vegagec_grass(:,:) = 0.
    vegagec_pasture(:,:) = 0.
    vegagec_crop(:,:) = 0.

    CALL calc_cover(npts,veget_max,veget_mtc,vegagec_tree,vegagec_grass, &
           vegagec_pasture,vegagec_crop)

    !  We allocate in the sequences of old->young. Within the same age-class
    !  group, we allocate in proportion with existing PFT fractions.
    DO ipts=1,npts
      !f2c
      CALL type_conversion(ipts,f2c,glccReal,veget_mtc,       &
                       indold_tree,indagec_tree,indagec_crop,num_crop_mulagec,     &
                       nagec_tree,nagec_herb,                    &
                       vegagec_tree,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !f2p
      CALL type_conversion(ipts,f2p,glccReal,veget_mtc,       &
                       indold_tree,indagec_tree,indagec_pasture,num_pasture_mulagec,     &
                       nagec_tree,nagec_herb,                    &
                       vegagec_tree,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !f2g
      CALL type_conversion(ipts,f2g,glccReal,veget_mtc,       &
                       indold_tree,indagec_tree,indagec_grass,num_grass_mulagec,     &
                       nagec_tree,nagec_herb,                    &
                       vegagec_tree,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !g2c
      CALL type_conversion(ipts,g2c,glccReal,veget_mtc,       &
                       indold_grass,indagec_grass,indagec_crop,num_crop_mulagec,     &
                       nagec_herb,nagec_herb,                    &
                       vegagec_grass,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !g2p
      CALL type_conversion(ipts,g2p,glccReal,veget_mtc,       &
                       indold_grass,indagec_grass,indagec_pasture,num_pasture_mulagec,     &
                       nagec_herb,nagec_herb,                    &
                       vegagec_grass,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !g2f
      CALL type_conversion(ipts,g2f,glccReal,veget_mtc,       &
                       indold_grass,indagec_grass,indagec_tree,num_tree_mulagec,     &
                       nagec_herb,nagec_tree,                    &
                       vegagec_grass,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !p2c
      CALL type_conversion(ipts,p2c,glccReal,veget_mtc,       &
                       indold_pasture,indagec_pasture,indagec_crop,num_crop_mulagec,     &
                       nagec_herb,nagec_herb,                    &
                       vegagec_pasture,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !p2g
      CALL type_conversion(ipts,p2g,glccReal,veget_mtc,       &
                       indold_pasture,indagec_pasture,indagec_grass,num_grass_mulagec,     &
                       nagec_herb,nagec_herb,                    &
                       vegagec_pasture,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !p2f
      CALL type_conversion(ipts,p2f,glccReal,veget_mtc,       &
                       indold_pasture,indagec_pasture,indagec_tree,num_tree_mulagec,     &
                       nagec_herb,nagec_tree,                    &
                       vegagec_pasture,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !c2p
      CALL type_conversion(ipts,c2p,glccReal,veget_mtc,       &
                       indold_crop,indagec_crop,indagec_pasture,num_pasture_mulagec,     &
                       nagec_herb,nagec_herb,                    &
                       vegagec_crop,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !c2g
      CALL type_conversion(ipts,c2g,glccReal,veget_mtc,       &
                       indold_crop,indagec_crop,indagec_grass,num_grass_mulagec,     &
                       nagec_herb,nagec_herb,                    &
                       vegagec_crop,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
      !c2f
      CALL type_conversion(ipts,c2f,glccReal,veget_mtc,       &
                       indold_crop,indagec_crop,indagec_tree,num_tree_mulagec,     &
                       nagec_herb,nagec_tree,                    &
                       vegagec_crop,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
    ENDDO

  END SUBROUTINE gross_glcc_firstday_SinAgeC_fh


  SUBROUTINE cross_give_receive(ipts,frac_used,veget_mtc,                     &
                     indold_tree,indagec_crop,nagec_receive,num_crop_mulagec, &
                     veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)


    IMPLICIT NONE

    !! 0. Input variables
    INTEGER, INTENT(in)                             :: ipts
    REAL(r_std), INTENT(in)                         :: frac_used                 !! fraction that the giving PFTs are going to collectively give
    REAL(r_std), DIMENSION(:,:), INTENT(in)         :: veget_mtc            !! "maximal" coverage fraction of a PFT on the ground
    INTEGER, DIMENSION(:), INTENT(in)               :: indold_tree          !! Indices for PFTs giving out fractions; 
                                                                            !! here use old tree cohort as an example
    INTEGER, DIMENSION(:,:), INTENT(in)             :: indagec_crop         !! Indices for secondary basic-vegetation cohorts; The youngest age classes
                                                                            !! of these vegetations are going to receive fractions. 
                                                                            !! here we use crop cohorts as an example
    INTEGER, INTENT(in)                             :: num_crop_mulagec     !! number of crop MTCs with more than one age classes
    INTEGER, INTENT(in)                             :: nagec_receive        !! number of age classes in the receiving basic types
                                                                            !! (i.e., tree, grass, pasture, crop), here we can use crop
                                                                            !! as an example, nagec_receive=nagec_herb

    !! 1. Modified variables
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: veget_max            !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: glcc_pft             !! a temporary variable to hold the fractions each PFT is going to lose
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)    :: glcc_pftmtc          !! a temporary variable to hold the fraction of ipft->ivma, i.e., from 
                                                                            !! PFT_{ipft} to the youngest age class of MTC_{ivma}
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: glcc_pft_tmp         !! a temporary variable to hold the fractions each PFT is going to lose

    !! Local vriables
    INTEGER  :: j,ipft, iyoung
    REAL(r_std) :: totalveg


    ! Out final objective is to know glcc_pftmtc, i.e., the fraction from each PFT
    ! to the youngest age group of each MTC. We separate this task into two steps:
    ! 1. we allocate the total outgoing fraction into the same age-class PFTs of 
    ! the a basic-vegetation (for example, the same age-calss PFTs of forest);
    ! 2. we further allocate the outgoing fraction of each age-class PFT to 
    ! the different receiving youngest age-class PFTs of the same basic-vegetation
    ! type, for example, the youngest age-calss PFTs of cropland.
    
    ! glcc_pft_tmp used only as a temporary variable to store the value
    glcc_pft_tmp(ipts,indold_tree) = veget_max(ipts,indold_tree)/SUM(veget_max(ipts,indold_tree))*frac_used
    glcc_pft(ipts,indold_tree) = glcc_pft(ipts,indold_tree) + glcc_pft_tmp(ipts,indold_tree)
    !we have to remove the outgoing fraction from veget_max in order to use this information for next loop
    veget_max(ipts,indold_tree) = veget_max(ipts,indold_tree) - glcc_pft_tmp(ipts,indold_tree)

    ! when receiving basic-vegetation type has a single age group, it will be considered as
    ! both old and young age group (thus recevie the fraction donation), otherwise the youngest
    ! age group is always the final element of indagec_crop.
    IF (nagec_receive == 1) THEN
      iyoung = 1
    ELSE
      iyoung = nagec_receive - 1
    ENDIF

    totalveg = 0.
    DO j=1,num_crop_mulagec
      totalveg = totalveg + veget_mtc(ipts,agec_group(indagec_crop(j,iyoung))) 
    ENDDO
  
    IF (totalveg>min_stomate) THEN
      DO j=1,num_crop_mulagec
        ipft = indagec_crop(j,iyoung)
        glcc_pftmtc(ipts,indold_tree,agec_group(ipft)) = glcc_pft_tmp(ipts,indold_tree) &
                               *veget_mtc(ipts,agec_group(ipft))/totalveg
      ENDDO
    ELSE
      DO j=1,num_crop_mulagec
        ipft = indagec_crop(j,iyoung)
        glcc_pftmtc(ipts,indold_tree,agec_group(ipft)) = glcc_pft_tmp(ipts,indold_tree)/num_crop_mulagec
      ENDDO
    ENDIF

  END SUBROUTINE cross_give_receive

! ================================================================================================================================
!! SUBROUTINE   : type_conversion
!>\BRIEF        : Allocate outgoing into different age classes and incoming into
!!                yongest age-class of receiving MTCs.
!!
!! REMARK       : The current dummy variables give an example of converting forests
!!                to crops.
!! \n
!_ ================================================================================================================================
  SUBROUTINE type_conversion(ipts,f2c,glccReal,veget_mtc,       &
                     indold_tree,indagec_tree,indagec_crop,num_crop_mulagec,     &
                     nagec_giving,nagec_receive,                    &
                     vegagec_tree,veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp, &
                     iagec_start)

    IMPLICIT NONE

    !! Input variables
    INTEGER, INTENT(in)                             :: ipts,f2c
    REAL(r_std), DIMENSION(:,:), INTENT(in)         :: glccReal             !! The "real" glcc matrix that we apply in the model
                                                                            !! after considering the consistency between presribed
                                                                            !! glcc matrix and existing vegetation fractions.
    REAL(r_std), DIMENSION(:,:), INTENT(in)         :: veget_mtc            !! "maximal" coverage fraction of a PFT on the ground
    INTEGER, DIMENSION(:), INTENT(in)               :: indold_tree          !! Indices for PFTs giving out fractions; 
                                                                            !! here use old tree cohort as an example
    INTEGER, DIMENSION(:,:), INTENT(in)             :: indagec_tree         !! Indices for PFTs giving out fractions; 
                                                                            !! here use old tree cohort as an example
    INTEGER, DIMENSION(:,:), INTENT(in)             :: indagec_crop         !! Indices for secondary basic-vegetation cohorts; The youngest age classes
                                                                            !! of these vegetations are going to receive fractions. 
                                                                            !! here we use crop cohorts as an example
    INTEGER, INTENT(in)                             :: num_crop_mulagec     !! number of crop MTCs with more than one age classes
    INTEGER, INTENT(in)                             :: nagec_giving         !! number of age classes in the giving basic types
                                                                            !! (i.e., tree, grass, pasture, crop), here we can use tree
                                                                            !! as an example, nagec=nagec_tree
    INTEGER, INTENT(in)                             :: nagec_receive        !! number of age classes in the receiving basic types
                                                                            !! (i.e., tree, grass, pasture, crop), here we can use crop
                                                                            !! as an example, nagec=nagec_herb
    INTEGER, OPTIONAL, INTENT(in)                   :: iagec_start          !! starting index for iagec, this is added in order to handle
                                                                            !! the case of secondary forest harvest.

    !! 1. Modified variables
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: vegagec_tree         !! fraction of tree age-class groups, in sequence of old->young
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: veget_max            !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: glcc_pft             !! a temporary variable to hold the fractions each PFT is going to lose
    REAL(r_std), DIMENSION(:,:,:), INTENT(inout)    :: glcc_pftmtc          !! a temporary variable to hold the fraction of ipft->ivma, i.e., from 
    REAL(r_std), DIMENSION(:,:), INTENT(inout)      :: glcc_pft_tmp         !! Loss of fraction in each PFT

    !! Local vriables
    INTEGER  :: j,iagec,iagec_start_proxy
    REAL(r_std) :: frac_begin,frac_used
                                                                            !! PFT_{ipft} to the youngest age class of MTC_{ivma}
    IF (.NOT. PRESENT(iagec_start)) THEN
      iagec_start_proxy=1
    ELSE
      iagec_start_proxy=iagec_start
    ENDIF
    
    ! This subroutine handles the conversion from one basic-vegetation type
    ! to another, by calling the subroutine cross_give_receive, which handles
    ! allocation of giving-receiving fraction among the giving age classes
    ! and receiving basic-vegetation young age classes.
    ! We allocate in the sequences of old->young. Within the same age-class
    ! group, we allocate in proportion with existing PFT fractions. The same
    ! also applies in the receiving youngest-age-class PFTs, i.e., the receiving
    ! total fraction is allocated according to existing fractions of 
    ! MTCs of the same basic vegetation type, otherwise it will be equally
    ! distributed.

    frac_begin = glccReal(ipts,f2c)
    DO WHILE (frac_begin>min_stomate)
      DO iagec=iagec_start_proxy,nagec_giving
        IF (vegagec_tree(ipts,iagec)>frac_begin) THEN
          frac_used = frac_begin
        ELSE IF (vegagec_tree(ipts,iagec)>min_stomate) THEN
          frac_used = vegagec_tree(ipts,iagec)
        ELSE
          frac_used = 0.
        ENDIF
        
        IF (frac_used>min_stomate) THEN
          IF (iagec==1) THEN
            ! Note that vegagec_tree is fractions of tree age-class groups in the 
            ! the sequence of old->young, so iagec==1 means that we're handling 
            ! first the oldest-age-group tree PFTs.
            CALL cross_give_receive(ipts,frac_used,veget_mtc,              &
                     indold_tree,indagec_crop,nagec_receive,num_crop_mulagec, &
                      veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
          ELSE
            ! Note also the sequence of indagec_tree is from old->young, so by
            ! increasing iagec, we're handling progressively the old to young
            ! tree age-class PFTs.
            CALL cross_give_receive(ipts,frac_used,veget_mtc,              &
                     indagec_tree(:,iagec-1),indagec_crop,nagec_receive,num_crop_mulagec, &
                      veget_max,glcc_pft,glcc_pftmtc,glcc_pft_tmp)
          ENDIF
          frac_begin = frac_begin-frac_used
          vegagec_tree(ipts,iagec)=vegagec_tree(ipts,iagec)-frac_used
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE type_conversion

! ================================================================================================================================
!! SUBROUTINE   : calc_cover
!!
!>\BRIEF        Calculate coverage fraction for different age classes of forest,
!!              grass, pasture and crops and also for each metaclass. Note baresoil is excluded. 
!!              
!! DESCRIPTION :
!!  
!!
!! MAIN OUTPUT VARIABLE(S) :  
!!
!! \n
!_ ================================================================================================================================
  SUBROUTINE calc_cover(npts,veget_max,veget_mtc,vegagec_tree,vegagec_grass, &
                 vegagec_pasture,vegagec_crop)

   
    IMPLICIT NONE

    !! Input variables
    INTEGER, INTENT(in)                                       :: npts             !! Domain size - number of pixels (unitless)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)         :: veget_max           !! "maximal" coverage fraction of a PFT on the ground

    !! Output variables
    REAL(r_std), DIMENSION(npts,nvmap), INTENT(inout)         :: veget_mtc        !! "maximal" coverage fraction of a PFT on the ground
    REAL(r_std), DIMENSION(npts,nagec_tree), INTENT(inout)    :: vegagec_tree     !! fraction of tree age-class groups, in sequence of old->young
    REAL(r_std), DIMENSION(npts,nagec_herb), INTENT(inout)    :: vegagec_grass    !! fraction of grass age-class groups, in sequence of old->young
    REAL(r_std), DIMENSION(npts,nagec_herb), INTENT(inout)    :: vegagec_pasture  !! fraction of pasture age-class groups, in sequence of old->young
    REAL(r_std), DIMENSION(npts,nagec_herb), INTENT(inout)    :: vegagec_crop     !! fraction of crop age-class groups, in sequence of old->young

    !! Local variables
    INTEGER(i_std)                                          :: ivma,staind,endind,j    !! indices (unitless)

    ! Calculate veget_max for MTCs
    DO ivma = 1,nvmap
      staind = start_index(ivma)
      IF (nagec_pft(ivma) == 1) THEN
        veget_mtc(:,ivma) = veget_max(:,staind)
      ELSE
        veget_mtc(:,ivma) = \
          SUM(veget_max(:,staind:staind+nagec_pft(ivma)-1),DIM=2)
      ENDIF
    ENDDO

    ! Calculate veget_max for each age class
    DO ivma = 2,nvmap  !here we start with 2 to exclude baresoil (always PFT1)
      staind = start_index(ivma)
      endind = staind+nagec_pft(ivma)-1

      ! Single-age-class MTC goest to oldest age class.
      IF (nagec_pft(ivma) == 1) THEN
        IF (is_tree(staind)) THEN
          vegagec_tree(:,1) = vegagec_tree(:,1)+veget_max(:,staind)
        ELSE IF (is_grassland_manag(staind)) THEN
          vegagec_pasture(:,1) = vegagec_pasture(:,1)+veget_max(:,staind)
        ELSE IF (natural(staind)) THEN
          vegagec_grass(:,1) = vegagec_grass(:,1)+veget_max(:,staind)
        ELSE
          vegagec_crop(:,1) = vegagec_crop(:,1)+veget_max(:,staind)
        ENDIF

      ELSE
        IF (is_tree(staind)) THEN
          DO j=1,nagec_tree
            vegagec_tree(:,j) = vegagec_tree(:,j)+veget_max(:,endind-j+1)
          ENDDO
        ELSE IF (is_grassland_manag(staind)) THEN
          DO j=1,nagec_herb
            vegagec_pasture(:,j) = vegagec_pasture(:,j)+veget_max(:,endind-j+1)
          ENDDO
        ELSE IF (natural(staind)) THEN
          DO j=1,nagec_herb
            vegagec_grass(:,j) = vegagec_grass(:,j)+veget_max(:,endind-j+1)
          ENDDO
        ELSE
          DO j=1,nagec_herb
            vegagec_crop(:,j) = vegagec_crop(:,j)+veget_max(:,endind-j+1)
          ENDDO
        ENDIF
      ENDIF
    ENDDO

  END SUBROUTINE calc_cover

  ! Note this subroutine does not depend on how many age classes there are
  ! in different MTCs.
  SUBROUTINE glcc_compensation_full(npts,veget_4veg,glcc,glccReal,glccDef, &
                               p2c,ipasture,g2c,igrass,f2c,itree,icrop,    &
                               IncreDeficit)

    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                         :: npts        !! Domain size - number of pixels (unitless)
    INTEGER, INTENT(in)    :: p2c,ipasture,g2c,igrass,f2c,itree,icrop
    REAL(r_std), DIMENSION (npts,12),INTENT(in)                 :: glcc        !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                               !! used.

    !! 0.2 Output variables


    !! 0.3 Modified variables
    REAL(r_std), DIMENSION(npts,4), INTENT(inout)         :: veget_4veg        !! "maximal" coverage of tree/grass/pasture/crop
    REAL(r_std), DIMENSION(npts,12), INTENT(inout)        :: glccDef           !! Gross LCC deficit, negative values mean that there
                                                                               !! are not enough fractions in the source vegetations
                                                                               !! to the target ones as presribed by the LCC matrix.
    REAL(r_std), DIMENSION(npts,12), INTENT(inout)        :: glccReal          !! The "real" glcc matrix that we apply in the model
                                                                               !! after considering the consistency between presribed
                                                                               !! glcc matrix and existing vegetation fractions.
    REAL(r_std), DIMENSION(npts,4), INTENT(inout)         :: IncreDeficit      !! "Increment" deficits, negative values mean that 
                                                                               !! there are not enough fractions in the source PFTs
                                                                               !! /vegetations to target PFTs/vegetations. I.e., these
                                                                               !! fraction transfers are presribed in LCC matrix but
                                                                               !! not realized.
    
    !! 0.4 Local variables
    REAL(r_std), DIMENSION(npts)                          :: tmpdef            !! LCC deficits by summing up all the deficits to the
                                                                               !! the same target vegetation.


    !! 0. We first handle the cases where veget_4veg might be very small
    !tree
    WHERE(veget_4veg(:,itree) > min_stomate)
      glccDef(:,f2c) = veget_4veg(:,itree)-glcc(:,f2c)
      WHERE(veget_4veg(:,itree)>glcc(:,f2c))
        glccReal(:,f2c) = glcc(:,f2c)
      ELSEWHERE
        glccReal(:,f2c) = veget_4veg(:,itree)
      ENDWHERE
    ELSEWHERE
      glccReal(:,f2c) = 0.
      glccDef(:,f2c) = -1*glcc(:,f2c)
    ENDWHERE

    !pasture
    WHERE(veget_4veg(:,ipasture) > min_stomate)
      glccDef(:,p2c) = veget_4veg(:,ipasture)-glcc(:,p2c)
      WHERE(veget_4veg(:,ipasture)>glcc(:,p2c))
        glccReal(:,p2c) = glcc(:,p2c)
      ELSEWHERE
        glccReal(:,p2c) = veget_4veg(:,ipasture)
      ENDWHERE
    ELSEWHERE
      glccReal(:,p2c) = 0.
      glccDef(:,p2c) = -1*glcc(:,p2c)
    ENDWHERE

    !grass
    WHERE(veget_4veg(:,igrass) > min_stomate)
      glccDef(:,g2c) = veget_4veg(:,igrass)-glcc(:,g2c)
      WHERE(veget_4veg(:,igrass)>glcc(:,g2c))
        glccReal(:,g2c) = glcc(:,g2c)
      ELSEWHERE
        glccReal(:,g2c) = veget_4veg(:,igrass)
      ENDWHERE
    ELSEWHERE
      glccReal(:,g2c) = 0.
      glccDef(:,g2c) = -1*glcc(:,g2c)
    ENDWHERE

    !! 1. Compensation sequence: pasture,grass,forest 
    tmpdef(:) = glccDef(:,f2c)+glccDef(:,g2c)+glccDef(:,p2c)
    WHERE(glccDef(:,p2c)<0)
      WHERE(glccDef(:,g2c)<0)
        WHERE(glccDef(:,f2c)<0) ! 1 (-,-,-)
          IncreDeficit(:,icrop) = tmpdef(:)
        ELSEWHERE ! 2 (-,-,+)
          WHERE(tmpdef(:)>=min_stomate)
            glccReal(:,f2c) = glccReal(:,f2c)-glccDef(:,g2c)-glccDef(:,p2c)
          ELSEWHERE
            glccReal(:,f2c) = veget_4veg(:,itree)
            IncreDeficit(:,icrop) = tmpdef(:)
          ENDWHERE
        ENDWHERE
      ELSEWHERE
        WHERE(glccDef(:,f2c)<0) ! 3 (-,+,-)
          WHERE(tmpdef(:)>=min_stomate)
            glccReal(:,g2c) = glccReal(:,g2c)-glccDef(:,p2c)-glccDef(:,f2c)
          ELSEWHERE
            glccReal(:,g2c) = veget_4veg(:,igrass)
            IncreDeficit(:,icrop) = tmpdef(:)
          ENDWHERE
        ELSEWHERE ! 4 (-,+,+)
          WHERE(tmpdef(:)>=min_stomate)
            WHERE((glccDef(:,g2c)+glccDef(:,p2c))>=min_stomate)
              glccReal(:,g2c) = glccReal(:,g2c)-glccDef(:,p2c)
            ELSEWHERE
              glccReal(:,g2c) = veget_4veg(:,igrass)
              glccReal(:,f2c) = glccReal(:,f2c)-(glccDef(:,p2c)+glccDef(:,g2c))
            ENDWHERE
          ELSEWHERE
            glccReal(:,g2c) = veget_4veg(:,igrass)
            glccReal(:,f2c) = veget_4veg(:,itree)
            IncreDeficit(:,icrop) = tmpdef(:)
          ENDWHERE
        ENDWHERE
      ENDWHERE
    ELSEWHERE
      WHERE(glccDef(:,g2c)<0)
        WHERE(glccDef(:,f2c)<0) ! 5 (+,-,-)
          WHERE(tmpdef(:)>=min_stomate)
            glccReal(:,p2c) = glccReal(:,p2c)-glccDef(:,g2c)-glccDef(:,f2c)
          ELSEWHERE
            IncreDeficit(:,icrop) = tmpdef(:)
            glccReal(:,p2c) = veget_4veg(:,ipasture)
          ENDWHERE
        ELSEWHERE ! 6 (+,-,+)
          WHERE(tmpdef(:)>=min_stomate)
            WHERE((glccDef(:,p2c)+glccDef(:,g2c))>=min_stomate)
              glccReal(:,p2c) = glccReal(:,p2c)-glccDef(:,g2c)
            ELSEWHERE
              glccReal(:,p2c) = veget_4veg(:,ipasture)
              glccReal(:,f2c) = glccReal(:,f2c)-(glccDef(:,g2c)+glccDef(:,p2c))
            ENDWHERE
          ELSEWHERE
            IncreDeficit(:,icrop) = tmpdef(:)
            glccReal(:,p2c) = veget_4veg(:,ipasture)
            glccReal(:,f2c) = veget_4veg(:,itree)
          ENDWHERE
        ENDWHERE
      ELSEWHERE
        WHERE(glccDef(:,f2c)<0) ! 7 (+,+,-)
          WHERE(tmpdef(:)>=min_stomate)
            WHERE((glccDef(:,p2c)+glccDef(:,f2c))>=min_stomate)
              glccReal(:,p2c) = glccReal(:,p2c)-glccDef(:,f2c)
            ELSEWHERE
              glccReal(:,p2c) = veget_4veg(:,ipasture)
              glccReal(:,g2c) = glccReal(:,g2c)-(glccDef(:,f2c)+glccDef(:,p2c))
            ENDWHERE
          ELSEWHERE
            IncreDeficit(:,icrop) = tmpdef(:)
            glccReal(:,g2c) = veget_4veg(:,igrass)
            glccReal(:,p2c) = veget_4veg(:,ipasture)
          ENDWHERE
        ELSEWHERE ! 8 (+,+,+)
          !do nothing
        ENDWHERE
      ENDWHERE
    ENDWHERE
    veget_4veg(:,itree) = veget_4veg(:,itree) - glccReal(:,f2c)
    veget_4veg(:,igrass) = veget_4veg(:,igrass) - glccReal(:,g2c)
    veget_4veg(:,ipasture) = veget_4veg(:,ipasture) - glccReal(:,p2c)

  END SUBROUTINE glcc_compensation_full



  !! This subroutine implements non-full compensation, is currently
  !! abandoned.
  SUBROUTINE glcc_compensation(npts,veget_4veg,glcc,glccDef, &
                               p2c,ipasture,g2c,igrass,f2c,itree,icrop, &
                               IncreDeficit)

    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER, INTENT(in)                                         :: npts        !! Domain size - number of pixels (unitless)
    REAL(r_std), DIMENSION(npts,4), INTENT(in)                  :: veget_4veg  !! "maximal" coverage fraction of a PFT on the ground
    INTEGER, INTENT(in)    :: p2c,ipasture,g2c,igrass,f2c,itree,icrop

    !! 0.2 Output variables


    !! 0.3 Modified variables
    REAL(r_std), DIMENSION (npts,12),INTENT(inout)        :: glcc              !! the land-cover-change (LCC) matrix in case a gross LCC is 
                                                                               !! used.
    REAL(r_std), DIMENSION(npts,12), INTENT(inout)        :: glccDef           !! Gross LCC deficit, negative values mean that there
                                                                               !! are not enough fractions in the source vegetations
                                                                               !! to the target ones as presribed by the LCC matrix.
    REAL(r_std), DIMENSION(npts,4), INTENT(inout)         :: IncreDeficit      !! "Increment" deficits, negative values mean that 
                                                                               !! there are not enough fractions in the source PFTs
                                                                               !! /vegetations to target PFTs/vegetations. I.e., these
                                                                               !! fraction transfers are presribed in LCC matrix but
                                                                               !! not realized.
    
    !! 0.4 Local variables
    REAL(r_std), DIMENSION(npts)                          :: glccDef_all       !! LCC deficits by summing up all the deficits to the
                                                                               !! the same target vegetation.


    WHERE(veget_4veg(:,itree) > min_stomate)
      glccDef(:,f2c) = veget_4veg(:,itree)-glcc(:,f2c)
    ELSEWHERE
      glccDef(:,f2c) = -1*glcc(:,f2c)
      glcc(:,f2c) = 0.
    ENDWHERE

    WHERE(veget_4veg(:,ipasture) > min_stomate)
      glccDef(:,p2c) = veget_4veg(:,ipasture)-glcc(:,p2c)
    ELSEWHERE
      glccDef(:,p2c) = -1*glcc(:,p2c)
      glcc(:,p2c) = 0.
    ENDWHERE

    WHERE(veget_4veg(:,igrass) > min_stomate)
      glccDef(:,g2c) = veget_4veg(:,igrass)-glcc(:,g2c)
    ELSEWHERE
      glccDef(:,g2c) = -1*glcc(:,g2c)
      glcc(:,g2c) = 0.
    ENDWHERE

    glccDef_all(:) = glccDef(:,f2c)+glccDef(:,p2c)+glccDef(:,g2c)

    ! We allow the surpluses/deficits in p2c and g2c mutually compensating 
    ! for each other. If there are still deficits after this compensation,
    ! they will be further compensated for by the surpluses from f2c (if there are any
    ! surpluses). The ultimate deficits that cannot be compensated for 
    ! will be recorded and dropped. 

    ! Because we assume the "pasture rule" is used, i.e., the crops 
    ! are supposed to come primarily from pastures and grasses, normally
    ! we expect the deficits to occur in p2c or g2c rather than in f2c. But
    ! if it happens that f2c has deficits while p2c or g2c has surpluse,
    ! the surpluses will not be used to compensate for the f2c-deficits, 
    ! instead, we will just record and drop the f2c-deficits.

    ! In following codes for convenience we're not going to check
    ! whether surpluses in f2c are enough to compensate for deficits 
    ! in p2c or g2c or both. Instead, we just add their deficits on top
    ! of f2c. The issues of not-enough surpluses in f2c will be left for
    ! the codes after this section to handle.
    WHERE (glccDef(:,p2c) < 0.)
      glcc(:,p2c) = veget_4veg(:,ipasture)
      WHERE (glccDef(:,g2c) < 0.)
        glcc(:,g2c) = veget_4veg(:,igrass)
      ELSEWHERE
        WHERE (glccDef(:,g2c)+glccDef(:,p2c) > min_stomate)
          glcc(:,g2c) = glcc(:,g2c)-glccDef(:,p2c)
        ELSEWHERE
          glcc(:,g2c) = veget_4veg(:,igrass)
          ! whatever the case, we simply add the dificts to f2c
          glcc(:,f2c) = glcc(:,f2c)-glccDef(:,p2c)-glccDef(:,g2c)
        ENDWHERE
      ENDWHERE

    ELSEWHERE
      WHERE(glccDef(:,g2c) < 0.)
        glcc(:,g2c) = veget_4veg(:,igrass)
        WHERE(glccDef(:,p2c)+glccDef(:,g2c) > min_stomate)
          glcc(:,p2c) = glcc(:,p2c)-glccDef(:,g2c)
        ELSEWHERE
          glcc(:,p2c) = veget_4veg(:,ipasture)
          ! whatever the case, we simply add the dificts to f2c
          glcc(:,f2c) = glcc(:,f2c)-glccDef(:,p2c)-glccDef(:,g2c)
        ENDWHERE
      ELSEWHERE
        !Here p2c and g2c both show surplus, we're not going to check whether
        !glccDef(:,f2c) has negative values because we assume a "pasture rule"
        !is applied when constructing the gross LCC matrix, so deficits in 
        !f2c will just be dropped but not be compensated for by the surpluses in
        !p2c or g2c.
      ENDWHERE
    ENDWHERE

    ! 1. We calculate again the f2c-deficit because f2c-glcc is adjusted in the
    ! codes above as we allocated the deficits of p2c and g2c into f2c. 
    ! In cases where glccDef_all is less than zero, f2c-glcc will be larger
    ! than available forest veget_max and we therefore limit the f2c-glcc to
    ! available forest cover.
    ! 2. There is (probably) a second case where glccDef_all is larger then zero, 
    ! but f2c-glcc is higher than veget_tree, i.e., Originally f2c is given a 
    ! high value that there is deficit in f2c but surpluses exist for p2c and g2c.
    ! Normally we 
    ! assume this won't happen as explained above, given that a "pasture rule" was 
    ! used in constructing the gross LCC matrix. Nevertheless if this deos 
    ! happen, we will just drop the f2c deficit without being compensated 
    ! for by the surplus in p2c or g2c.
   
    ! we handle the 2nd case first
    WHERE(veget_4veg(:,itree) > min_stomate )
      WHERE(glccDef(:,f2c) < 0.)
        glcc(:,f2c) = veget_4veg(:,itree)
        WHERE (glccDef(:,p2c)+glccDef(:,g2c) > min_stomate)
          IncreDeficit(:,icrop) = glccDef(:,f2c)
        ELSEWHERE
          IncreDeficit(:,icrop) = glccDef_all(:)
        ENDWHERE
      ELSEWHERE
        WHERE(glccDef_all(:) < 0.) !handle the 1st case
          glcc(:,f2c) = veget_4veg(:,itree)
          IncreDeficit(:,icrop) = glccDef_all(:)
        ENDWHERE
      ENDWHERE
    ELSEWHERE
      WHERE(glccDef(:,p2c)+glccDef(:,g2c)>min_stomate)
        IncreDeficit(:,icrop) = glccDef(:,f2c)
      ELSEWHERE
        IncreDeficit(:,icrop) = glccDef_all(:)
      ENDWHERE
    ENDWHERE

  END SUBROUTINE glcc_compensation



END MODULE stomate_glcchange_SinAgeC_fh
