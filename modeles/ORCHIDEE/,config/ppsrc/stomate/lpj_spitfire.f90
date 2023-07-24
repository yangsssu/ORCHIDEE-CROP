










!  ==============================================================================================================================
!  MODULE		 			: lpj_spitfire
!
!  CONTACT		 			: orchidee-help _at_ ipsl.jussieu.fr
!
!  LICENCE	 			        : IPSL (2006)
!  This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF				       This module simulates fires in
!! natural vegetations in a prognostic way. 
!! 
!!
!!\n DESCRIPTION                                : Use daily meterological
!! information combined with information for fuel availability to simulate
!! burned area and gas emissions in fire. Only fires in natural PFTs have been
!! simulated.
!!
!! REFERENCE(S)
!! - K.Thonicke, A.Spessa, I.C. Prentice, S.P.Harrison, L.Dong, and
!! C.Carmona-Moreno, 2010: The influence of vegetation, fire spread and fire
!! behaviour on biomass burning and trace gas emissions: results from a
!! process-based model. Biogeosciences, 7, 1991-2011.
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/branches/DOC/ORCHIDEE/src_sechiba/enerbil.f90 $
!! $Date: 2012-00-00 12:22:59 +0100  $
!! $Revision: ??? $
!! \n
!_ ================================================================================================================================

!! Below are some remarks that further document model behavior or existing minor
!! issues in the model, they're minor in nature and do not affect large-scale
!! model performance.
!! ----------------------- Remarks ------------------------------------------------
!! Date 2015/04/13
! 1. There are few places where clarifications are needed, mainmy minor, they're
! marked as ??, or [UNCLEAR]
! 2. some varialbes are not actually used in some SUBROUTINEs or the whole module. 
! and they're designated as "NOT USED"
! 3. some sources of parameters need to be check. they're marked as "SOURCE"
! 4. it's been tested that observed BA will be read as they are (the monthly 
! ba will be repeated by day number of concerning month) if the external observed
! burned area is forced into the model. The only strange behavior is that the 
! last day of the year seems to repeat the first month value (it's also the case 
! for lightning), (cf./home/orchidee03/ychao/OUTPUT_A5F_STD_WITHFIRE_OLDmap_NA5070
! /FIRETEST/TEST_BA_LIGHTN/BATEST/observed_ba.txt)
! When foring BA, if we assume that BA is distributed equally across the month, 
! we should divide the monthly BA in GFED3 by number of days in that month 
! before feed the data into model.
! 4. it's been test (2012/06/03) for 0.5dX0.5d lightning data, it behaves the same
! way exactly as burned area. this means lightning data must be provided as daily
! value. 
! 5. as popdensity is given one value for one year. this value is read directly into
! model (model use constant popdens value throughout the whole simulation year)
!! --------------------------------------------------------------------------------

!!------------ Explanations for forcing external Burned area ----------------------
!! Date 2015/04/13
! 1. External burned area could be forced into model, currently only monthly external
! BA input is configuered, this monthly BA will be treated as to occur at the 
! beginning day of each month. The flag to activate the external BA forcing is
! the read_observed_ba and ratio_flag variable, for regions where ratio_flag is bigger than 0,
! BA will be externally forced.
! 2. When external BA is not forced, baadjust_ratio could be used to adjust the 
! simulated BA.
! 3. The consumption fraction in fires for fuels could also be imposed from 
! external input, read_cf_coarse control the input of coarse fuel (corresponding
! to 1000hr fuel in the model); read_cf_fine control the input for the fine
! fuel (corresponding to live grass, 1hr/10hr/100hr fuel in the model).
!
! This behaviour is very flexible, one has to read the code to fully understand it.
!!--------------------------------------------------------------------------------

MODULE lpj_spitfire

  ! modules used:
  USE xios_orchidee
  USE stomate_data
  USE pft_parameters
  USE constantes
  USE ioipsl_para

  IMPLICIT NONE

  ! private & public routines
  PRIVATE
  PUBLIC spitfire,spitfire_clear

  ! first call
  LOGICAL, SAVE                                                   :: firstcall = .TRUE.

CONTAINS


  SUBROUTINE spitfire_clear
    firstcall = .TRUE.
  END SUBROUTINE spitfire_clear

!! ================================================================================================================================
!! SUBROUTINE     : SPITFIRE 
!!
!>\BRIEF          A prognostic fire module. It uses daily temperature and
!! precipitation to calculate fire danger index, combined with ignition sources to
!! derive number of fires. Then Burned area, fuel consumption and tree crown
!! scorching, tree mortality were calculated. 
!!
!! DESCRIPTION	: 
!!
!! The module completes the following tasks:
!! 1. Calculate fire danger index from daily temperature and precipitation using
!! Nesterov Index (NI). The fuel mositure and moisture of extinction are compared
!! to derive a daily Fire Danger Index (FDI). Fuel moiture is calculated using
!! Nesterov Index by weighting among differnet types of fuels. The fuel type with
!! a bigger surface-area-to-volume ratio receives less weight in determining the
!! fuel moisture state.
!! 2. Use daily FDI and ignition source data to derive daily fire numbers.
!! 3. Apply Rothermel's equation to calculate fire Rate of Spread (ROS) for both
!! forward and backward. Rate of spread is determined in a way related with fire
!! reaction intensity(+), propagating flux ratio(+), wind speed(+), fuel bulk
!! density(-),effective heating number(-), and heat of pre-ignition(-).
!! 4. From daily FDI derive fire duration time, combined with rate of spread to
!! derive burned area.
!! 5. Fuel mositure calculated in the step of daily FDI can be again used to
!! determin consumption fraction of different types of fuels in fire.
!! 6. Calculate fire surface intensity using ROSf, fuel consumption, and burned
!! area, thus further to determin flame height and fire damage to tree crown. 
!! 7. Fire caused tree mortality are determined based on 2 considerations, the
!! damage of flame to tree crown, and damage of fire residence time to tree barks.
!! crowning scorching biomass release as emissions, while unburned dead tree
!! biomass part transfer to litter pool.
!! 
!! In summary, fire weather (temperature, preciptation, wind speed) plays a central 
!! role in this module, it determines fuel moisture, which futher determines
!! daily fire danger index (FDI) --> nubmer of fires --> fire duration --> fire
!! size --> burned area --> fire intensity, residence time --> tree demage
!! 
!! Important Note:
!! 1. over one day and within one pixel, fire has unified size and BA=Size X
!! Number. 
!! 2. Fuel consumption and burned area are calculated independently.
!! 
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): fire_frac, co2_fire, and trace gas emissions. 
!!
!! REFERENCES    :
!! - K.Thonicke, A.Spessa, I.C. Prentice, S.P.Harrison, L.Dong, and
!! C.Carmona-Moreno, 2010: The influence of vegetation, fire spread and fire
!! behaviour on biomass burning and trace gas emissions: results from a
!! process-based model. Biogeosciences, 7, 1991-2011.
!! - Rothermel, R. C.: A Mathematical Model for Predicting Fire
!! Spread in Wildland Fuels, Intermountain Forest and Range Experiment 
!! Station, Forest Service, US Dept. of Agriculture, Ogden, UtahUSDA 
!! Forest Service General Technical Report INT-115, 1972.
!! - Pyne, S. J., Andrews, P. L., and Laven, R. D.: Introduction to wildland
!! fire, 2 edition, Wiley, New York, 769 pp., 1996
!!
!! FLOWCHART     : None
!! \n 
!_ ================================================================================================================================


   SUBROUTINE spitfire(npts, dt_days, veget_max_org,resolution,contfrac,   &
              PFTpresent,t2m_min_daily,t2m_max_daily,                  &
              precip_daily,wspeed_daily,soilhum_daily,                 &
              lightn,litter,ni_acc,fire_numday,                        &
              fuel_1hr,fuel_10hr,fuel_100hr,                           &
              fuel_1000hr,ind,biomass,popd,a_nd,height,                &
              read_observed_ba, observed_ba,read_cf_fine,cf_fine,      &
              read_cf_coarse,cf_coarse,read_ratio_flag,                &
              ratio_flag,read_ratio,baadjust_ratio,date,               &
              bm_to_litter,co2_fire,                                   &
              lcc,bafrac_deforest_accu,emideforest_litter_accu,emideforest_biomass_accu,&
              deforest_litter_remain,deforest_biomass_remain,&
              def_fuel_1hr_remain,def_fuel_10hr_remain,&
              def_fuel_100hr_remain,def_fuel_1000hr_remain)


    
    ! PARAMETERS
    INTEGER, parameter     :: ntrace=6              !!number of different trace gas types included
    REAL(r_std), parameter :: MINER_TOT=0.055       !!total mineral content, source: Thonicke et al., 2010. p1010 Table A1 Row 5
    REAL(r_std), parameter :: H=18000.0             !!calorific heat content (kJ kg^{-1}), source: Thonicke et al. (2010) Appendix A
    REAL(r_std), parameter :: sigma_1hr=66.0        !!surface-area-to-volume ratio (cm^{2} cm^{-3}), source:  (USDA 1978, Harvey et al. 1997);
                                                    !! These two sources are given in a working doc by K. Thonicke, the exact sources unknown.
    REAL(r_std), parameter :: sigma_10hr=3.58       !!same as above
    REAL(r_std), parameter :: sigma_100hr=0.98      !!same as above
    REAL(r_std), parameter :: sigma_livegrass=80.0  !! NOT USED
    REAL(r_std), parameter :: me_livegrass=0.2      !!moisture of extinction for live grass fuel (unitless) 

    ! SOURCE this two parameters are said to follow Brown et al. 1981 but I don't
    !have the paper.
    REAL(r_std), parameter :: fbd_a = 1.2           !!scalar used to transform FBD of 10hr fuels to 1hr equivs, FBD: fuel bulk density (kg m^{-3})
    REAL(r_std), parameter :: fbd_b = 1.4           !!scalar used to transform FBD of 100hr fuels to 1hr equivs
    !SOURCE
    REAL(r_std), parameter :: fbd_C3_livegrass=4.0  !!kg/m3
    REAL(r_std), parameter :: fbd_C4_livegrass=4.0  !!kg/m3

    !INPUT VARIABLES
    INTEGER, INTENT(in)                             :: npts             !! Domain size
    REAL(r_std), INTENT(in)                         :: dt_days          !! time step of Stomate in days
    INTEGER(i_std),INTENT(in)                            :: date                !! Date (days) 
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)    :: veget_max_org        !! veget_max
    REAL(r_std), DIMENSION(npts,2), INTENT(in)      :: resolution       !! resolution at each grid point in m (1=E-W, 2=N-S)
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: contfrac         !! fraction of continent in the grid
    LOGICAL, DIMENSION(npts,nvm), INTENT(inout)     :: PFTpresent       !! logical variables to check whether a PFT present, defined in stomate.f90
    REAL(r_std),DIMENSION(npts), INTENT(in)         :: t2m_min_daily    !! Daily minimum 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts), INTENT(in)         :: t2m_max_daily    !! Daily maximum 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)        :: precip_daily     !! daily precip (mm/day)
    REAL(r_std), DIMENSION(npts), INTENT(in)        :: wspeed_daily     !! wind speed [m/s]
    REAL(r_std), DIMENSION(npts), INTENT(in)        :: lightn           !! number of daily lightning [flashes per km2 per day]] 
                                                                        !! lightn declared and allocated in slowproc.f90 
    LOGICAL, INTENT (in)                            :: read_observed_ba !! Flag for read in observed burned area, i.e., model is forced by external BA
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: observed_ba      !! Observed burned area, currently must be in monthly timestep


    LOGICAL, INTENT (in)                            :: read_cf_coarse   !! Flag to read external forced consumption fraction for coarse fuels, will be used
                                                                        !! on the 1000hr fuel in the model.
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: cf_coarse        !! Externally forced fire consumption fraction for coarse fuel, applied on 1000hr fuel.
    LOGICAL, INTENT (in)                            :: read_cf_fine     !! Flag to read external forced consumption fraction for fine fuel.
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: cf_fine          !! Externally forced fire consumption fraction for fine fuel, applied on live grass,
                                                                        !! and 1hr/10hr/100hr dead fuel.
    LOGICAL, INTENT (in)                            :: read_ratio       !! Flag for read in ratio to adjust simulated burned area.
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: baadjust_ratio   !! gridded input data used to adjust the simulated burned area, unitless.
    LOGICAL, INTENT (in)                            :: read_ratio_flag  !! Flag for read in observed burned area 
    REAL(r_std),DIMENSION (npts), INTENT (in)       :: ratio_flag       !! gridded flag value indicate where the burned area, conumption fractions
                                                                        !! will be forced in the model. When bigger than 0, the burned area and 
                                                                        !! consumption fractions will be forced in the model.


    REAL(r_std), DIMENSION(npts), INTENT(in)        :: popd             !! human population density [person per sqkm] 
                                                                        !! popd declared and allocated in slowproc.f90
    REAL(r_std), DIMENSION(npts), INTENT(in)        :: a_nd             !! parameter for potential human-caused ignitions;
                                                                        !! a_nd is parameterized in stomate_lpj before call of SPITFIRE
    REAL(r_std), DIMENSION(npts), INTENT(in)        :: soilhum_daily    !! daily top layer soil humidity, here we need only the top soil 
                                                                        !! layer humdity, and this is addressed by 
                                                                        !! passing soilhum_daily(:,1) as soilhum_daily when we 
                                                                        !! call SPITFIRE in stomate_lpj.f90
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)    :: height           !! tree height in m
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)    :: lcc              !! gross forest loss as fraction of griecell area


    ! MODIFIED VARIABLES
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs), INTENT(inout)  :: litter       !! metabolic and structural litter, above and belowground
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(inout)       :: biomass      !! biomass
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: fuel_1hr     !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: fuel_10hr    !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: fuel_100hr   !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: fuel_1000hr  !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: def_fuel_1hr_remain     !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: def_fuel_10hr_remain    !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: def_fuel_100hr_remain   !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: def_fuel_1000hr_remain  !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts),INTENT(INOUT)                   :: ni_acc       !! ni_acc declared and allocated in stomate.f90
    REAL(r_std), DIMENSION(npts),INTENT(INOUT)                   :: fire_numday  !! declared and allocated in stomate.f90
    REAL(r_std), DIMENSION(npts,nvm),INTENT(inout)               :: co2_fire     !! total C emission in fire, including live crown scorching and 
                                                                                 !! dead ground litter(tree/grass) + live grass leaf&fruit
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)              :: ind          !! Number of individuals / m2; defined in stomate.f90
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(inout)       :: bm_to_litter !! Biomass transfer to litter
    REAL(r_std), DIMENSION(npts,nvm), INTENT(inout)              :: bafrac_deforest_accu     !!accumulated deforestation fire fraction
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(inout)        :: emideforest_litter_accu  !!cumulative deforestation fire emissions from litter
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(inout)       :: emideforest_biomass_accu !!cumulative deforestation fire emissions from live biomass burning
    REAL(r_std), DIMENSION(npts,nlitt,nvm,nlevs), INTENT(inout) :: deforest_litter_remain 
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(inout)      :: deforest_biomass_remain 



    ! LOCAL SPITFIRE VARIABLES
    REAL(r_std), DIMENSION(npts)                    :: dfm_lg                  !! Daily fuel moisture for live grass, unitless, (0,1)
    REAL(r_std), DIMENSION(npts)                    :: dfm                     !! Daily fuel moisture of dead fuels on ground excluding 1000hr fuel,(0,1)
    REAL(r_std), DIMENSION(npts)                    :: ignition_efficiency     !! Ignition efficiency as limited by fuel load, (0,1), unitless.
    REAL(r_std), DIMENSION(npts)                    :: dfm_1hr                 !! daily fuel moisture for 1hr-fuel
    REAL(r_std), DIMENSION(npts)                    :: fuel_1hr_total          !! fuel_1hr_total is different from fuel_1hr(unit in C) and 
                                                                               !! it combines the fuel of all types and PFTs. unit: dry mass
    REAL(r_std), DIMENSION(npts)                    :: fuel_10hr_total         !! fuel_1hr/10hr/100hr/_total unit in dry mass for all natural PFTs
    REAL(r_std), DIMENSION(npts)                    :: fuel_100hr_total
    REAL(r_std), DIMENSION(npts)                    :: fuel_1000hr_total
    REAL(r_std), DIMENSION(npts)                    :: natural_litter_ag_total !! total above-ground litter of natural PFTs
    REAL(r_std), DIMENSION(npts)                    :: dead_fuel               !! sum of 1hr/10hr/100hr dead fuel (g dry mass/m2) for all natural PFTs
    REAL(r_std), DIMENSION(npts)                    :: dead_fuel_all           !! sum of 1hr/10hr/100hr/1000hr dead fuel (g dry mass/m2) for all natural PFTs
    REAL(r_std), DIMENSION(npts)                    :: livegrass               !! sum the leaf bimoass of natural grassland PFTs. unit is in dry mass but not carbon! 
    REAL(r_std), DIMENSION(npts)                    :: dead_fuel_all_livegrass !! sum of 1hr/10hr/100hr/1000hr dead fuel (g dry mass/m2) 
                                                                               !! for all natural PFTs plus livegrass biomass
    REAL(r_std), DIMENSION(npts)                    :: ratio_dead_fuel
    REAL(r_std), DIMENSION(npts)                    :: ratio_live_fuel
    REAL(r_std), DIMENSION(npts)                    :: me_litterfuel           !! the moisture of extinction weighted by the 
                                                                               !! dead groud litter fuel of different natural PFTs, unitless
    REAL(r_std), DIMENSION(npts)                    :: moist_extinction        !! the moisture of extinction weighted by ground dead fuel and livegrass,
                                                                               !! m_{e} in Eq.(8)
    REAL(r_std), DIMENSION(npts)                    :: d_fdi                   !! daily fire danger index,climatic fire risk, unitless between 0. and 1.
    REAL(r_std), DIMENSION(npts)                    :: net_fuel                !! mineral content adjusted dead_fuel (in kg biomass!)
    REAL(r_std), DIMENSION(npts)                    :: char_net_fuel           !! total net fuel(mineral content corrected) by summing the dead fuel and live grass,
                                                                               !! , in unit of kg dry mass per m2.
    REAL(r_std), DIMENSION(npts)                    :: wind_speed              !! wind_speed is Uforward in Thonicke et al. 2010; unit: m/min
    REAL(r_std), DIMENSION(npts)                    :: wind_forward            !! the name here is somewhat misleading; it's only for used in Eq(A5)
    REAL(r_std), DIMENSION(npts)                    :: lb                      !! length-to-breadth ratio of the fire ellipse, unitless, range[1,8]
    REAL(r_std), DIMENSION(npts)                    :: df
    REAL(r_std), DIMENSION(npts)                    :: db
    REAL(r_std), DIMENSION(npts)                    :: fire_durat              !! fire duration (min) 
    REAL(r_std), DIMENSION(npts)                    :: ros_f                   !! forward fire spred rate(m min^{-1})
    REAL(r_std), DIMENSION(npts)                    :: ros_b                   !! backward fire spread rate; unit: m min^{-1}
    REAL(r_std), DIMENSION(npts)                    :: ratio_fbd               !! proportion of dead fuel in class i PFT j as a proportion of total dead fuel
    REAL(r_std), DIMENSION(npts)                    :: ratio_C3_livegrass      !! ratio of C3 and C4 livegrass in total livegrass
    REAL(r_std), DIMENSION(npts)                    :: ratio_C4_livegrass
    REAL(r_std), DIMENSION(npts)                    :: dens_fuel_ave           !! weighted fuel bulk density by different fuel types except 1000h fuel,
                                                                               !! including only natural PFTs, in unit of kg dry mass per m3. 
    REAL(r_std), DIMENSION(npts)                    :: dens_livegrass_ave      !! C3/C4 weighted livegrass fuel bulk density, kg m^{-3}
    REAL(r_std), DIMENSION(npts)                    :: char_dens_fuel_ave      !! livegrass and dead fule weighted fuel bulk density 
                                                                               !! (excluding 1000hr dead fuel), in unit of kg dry mass per m3.
    REAL(r_std), DIMENSION(npts)                    :: sigma                   !! mass weighted mean fuel surface-area-to-volume ratio by different fuel types,
                                                                               !! in unit of cm^{-1}, given that area in unit of cm^{2} and volume in cm^{3}
    REAL(r_std), DIMENSION(npts)                    :: gamma                   !! tau_max*ita_m*ita_s; used in calculation of tau_l for postfire mortality.
    REAL(r_std), DIMENSION(npts)                    :: fpc_tree_total          !! total tree fraction
    REAL(r_std), DIMENSION(npts)                    :: fpc_grass_total         !! total grass fraction (including natural and agricultural grassland)
    REAL(r_std), DIMENSION(npts)                    :: area
    REAL(r_std), DIMENSION(npts)                    :: wetness                 !! wetness of fuel, calculated as dfm/moisture_of_extinction
    REAL(r_std), DIMENSION(npts)                    :: fuel_consum             !! total fuel consumption including 1hr/10hr/100hr (not 1000hr!) fuel. [g mass m^{-2}]
    REAL(r_std), DIMENSION(npts)                    :: d_i_surface             !! surface fire intensity [kW m^{-1}]
    REAL(r_std), DIMENSION(npts,nvm)                :: disturb_crown           !! a temporary variable, used to store the C emissions 
                                                                               !! from crown scorch of live tree crown.
    REAL(r_std), DIMENSION(npts)                    :: fc_crown                !! PFT weighted carbon emissions caused by crown scorching (gCm^{-2}).
                                                                               !! only for history file writing.
                                                                               !! this variable is PFT vcmax weighted sum of above one
    REAL(r_std), DIMENSION(npts)                    :: d_area_burnt            !! daily burned area (ha); local variable, only for history file writing.
    REAL(r_std), DIMENSION(npts)                    :: ba_escape               !! Escaped deforestation fire burned area (ha), it's included in d_area_burnt,
                                                                               !! However is separated for being put into history file.
    REAL(r_std), DIMENSION(npts)                    :: d_numfire               !! local variable, only for history file writing
    REAL(r_std), DIMENSION(npts)                    :: human_numfire           !! local variable, only for history file writing.
    REAL(r_std), DIMENSION(npts)                    :: lightn_numfire          !! local variable, only for history file writing.
    REAL(r_std), DIMENSION(npts)                    :: fire_frac               !! burned fraction, unitless
    REAL(r_std), DIMENSION(npts)                    :: area_land               !! Land area within gridcel excluding water body,[hectar=10000 m2]
    REAL(r_std), DIMENSION(npts)                    :: area_natural_veg         !! Land area covered with vegetation within gridcell [hectar]
    REAL(r_std), DIMENSION(npts)                    :: human_ign               !! human ignitions, [1 day^{-1} km^{-2}]
    REAL(r_std), DIMENSION(npts)                    :: lightn_efficiency       !! ligtning fractions that reach ground with sufficient energy to ignite,unitless
    REAL(r_std), DIMENSION(npts)                    :: human_suppression       !! huamn suppression effects on fires, unitless
    REAL(r_std), DIMENSION(npts)                    :: lightn_ign              !! lightning ignitions, [1 day^{-1} km^{-2}]
    REAL(r_std), DIMENSION(npts,ntrace)             :: dcflux_trace
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: fc_1hr                  !! fule consumption for 1hr fuel, [g biomass^m{-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: fc_10hr                 !! fule consumption for 10hr fuel, [g biomass^m{-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: fc_100hr                !! fule consumption for 100hr fuel, in the unit of g dry mass/m**2 (C/0.45)
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: fc_1000hr               !! fule consumption for 1000hr fuel, [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm)                :: fc_lg                   !! leaf biomass consumption for GRASS, [gC m^{-2}]
    REAL(r_std), DIMENSION(npts,nvm)                :: fc_lf                   !! fruit biomass consumption for GRASS, [gC m^{-2}]
    REAL(r_std), DIMENSION(npts)                    :: cf_ave                  !! the average fuel fire consumption fraction of 1hr/10hr/100hr fuel, unitless
    REAL(r_std), DIMENSION(npts)                    :: cf_1hr                  !! fire fuel consumption fraction for 1hr fuel, unitless
    REAL(r_std), DIMENSION(npts)                    :: cf_10hr                 !! fire fuel consumption fraction for 10hr fuel, unitless
    REAL(r_std), DIMENSION(npts)                    :: cf_100hr                !! fire fuel consumption fraction for 100hr fuel, unitless
    REAL(r_std), DIMENSION(npts)                    :: cf_1000hr               !! fire fuel consumption fraction for 1000hr fuel, unitless
    REAL(r_std), DIMENSION(npts)                    :: cf_lg                   !! fire fule consumption for livegrass leaf and fruit biomass, unitless
    REAL(r_std), DIMENSION(npts,nvm)                :: tau_l                   !! the residence time of fire; used in Eq(19)
    REAL(r_std), DIMENSION(npts,nvm)                :: ck                      !!the proportion of crown affected by fire ( Eq17: CK=(SH-H+CL)/CL )
    REAL(r_std), DIMENSION(npts,nvm)                :: postf_mort              !!
    REAL(r_std), DIMENSION(npts,nvm)                :: pm_tau
    REAL(r_std), DIMENSION(npts,nvm)                :: pm_ck
    REAL(r_std), DIMENSION(npts,nvm)                :: sh
    REAL(r_std), DIMENSION(npts,nvm)                :: nind_fa                 !! # trees affected by fire
    REAL(r_std), DIMENSION(npts,nvm)                :: nind_kill               !! # trees killed by fire
    REAL(r_std), DIMENSION(npts,nvm)                :: prop_fa_nk              !! propn of indivs fire affected but not killed
    REAL(r_std), DIMENSION(npts,nvm)                :: dcflux_fire_pft         !! fire carbon flux to atmosphere; including both crown emssions 
                                                                               !! and litter consumption emissions
    REAL(r_std), DIMENSION(npts,nvm,ntrace)         :: dcflux_trace_pft
    REAL(r_std), DIMENSION(npts,nvm)                :: litter_consump_pft      !! C emissions from ground litter burning (tree&grass) and grass leaf&fruit.
    REAL(r_std), DIMENSION(npts)                    :: litter_consump          !! Consumed litter during fire for each PFT,including ground dead litter for
                                                                               !! trees and grasses, and live grass leaf and fruit burning. (gCm^{-2})
    REAL(r_std), DIMENSION(npts,nvm)                :: tau_c                   !! critical time to cambial kill
    REAL(r_std), DIMENSION(npts)                    :: dia                     !! stem diameter (cm)
    REAL(r_std), DIMENSION(npts,nvm)                :: bt                      !! bark thickness (cm)
    REAL(r_std), DIMENSION(npts)                    :: tree_kill_frac          !! tree fraction that are killed in fire
    REAL(r_std), DIMENSION(npts)                    :: mean_fire_size_original !! mean fire size before the correction of surface fire intensity
    REAL(r_std), DIMENSION(npts)                    :: mean_fire_size          !! mean fire size
    REAL(r_std), DIMENSION(npts)                    :: natural_vegfrac         !! the fraction of the grid cell that are occupied by natural vegetation.
    REAL(r_std), DIMENSION(nvm,ntrace)              :: ef_trace                !! emission factors for trace gases
    REAL(r_std), DIMENSION(npts,nvm)                :: fc_1hr_carbon           !! [gCm^{-2}]
    REAL(r_std), DIMENSION(npts,nvm)                :: fc_10hr_carbon          !! [gCm^{-2}]  
    REAL(r_std), DIMENSION(npts,nvm)                :: fc_100hr_carbon         !! [gCm^{-2}]
    REAL(r_std), DIMENSION(npts,nvm)                :: fc_1000hr_carbon        !! [gCm^{-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: emideforest_fuel_1hr      !! fire emissions from deforestation fires for 1hr fuel [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: emideforest_fuel_10hr     !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: emideforest_fuel_100hr    !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: emideforest_fuel_1000hr   !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm)                :: emi_nondef                !! Fire carbon emissions not including deforestation fires, based on PFT area
    REAL(r_std), DIMENSION(npts,nvm)                :: emideforest_all           !! Fire carbon emissions from deforestation fires, based on PFT area
    REAL(r_std), DIMENSION(npts,nvm)                :: bafrac_deforest           !! Deforestation fire burned fraction as gridcell area
    REAL(r_std), DIMENSION(npts,nvm,nlitt)          :: emideforest_litter        !! Deforestation fire emissions from litter burning [gCm^{-2}], based on ground area
    REAL(r_std), DIMENSION(npts,nvm,nparts)         :: emideforest_biomass       !! live biomass emissions from deforestation fires [gCm^{-2}], based on ground area
    REAL(r_std), DIMENSION(npts,nvm)                :: veget_max           !! 
    INTEGER :: j,x,m,k,n,i,ivm,ipts

    REAL(r_std), DIMENSION(npts)                    :: proc_ba                 !! a proxy variable used to put the monthly observed BA at the first day
                                                                               !! of each month.
    REAL(r_std)                                     :: surface_threshold=50.0
    REAL(r_std)                                     :: fuel_low_bound=444.     !! below this ignition efficiency is 0 [dry matter/m2], 
                                                                               !! assuming a 0.45 carbon fraction.
    REAL(r_std)                                     :: fuel_high_bound=2222.   !! above this ignition efficiency is 1 [dry matter/m2], 
                                                                               !! assuming a 0.45 carbon fraction.
                                                                               !! the bounds sources are Arora & Boer (2005)
    INTEGER(r_std),DIMENSION(12)  :: monthday
    monthday = (/1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/)
    


    ! Initialise variables
    disturb_crown(:,:)=0.0
    sigma(:)=0.0
    lb(:)=0.0

    cf_lg(:)=0.0
    cf_1hr(:)=0.0
    cf_10hr(:)=0.0
    cf_100hr(:)=0.0
    cf_1000hr(:)=0.0
    fpc_tree_total(:)=0.0
    fpc_grass_total(:)=0.0
    net_fuel(:)=0.0
    dead_fuel(:)=0.0
    ratio_dead_fuel(:)=0.0
    ratio_live_fuel(:)=0.0
    livegrass(:)=0.0
    dead_fuel_all_livegrass(:)=0.0
    fuel_1hr_total(:)=0.0
    fuel_10hr_total(:)=0.0
    fuel_100hr_total(:)=0.0
    fuel_1000hr_total(:)=0
    dens_fuel_ave(:)=0.0
    d_area_burnt(:)=0.0
    ba_escape(:)=0.0
    fire_frac(:)=0.0
    d_i_surface(:)=0.0
    ck(:,:)=0.0
    tau_l(:,:)=0.0
    pm_tau(:,:)=0.0
    postf_mort(:,:)=0.0
    prop_fa_nk(:,:)=0.0
    nind_fa(:,:)=0.0
    nind_kill(:,:)=0.0
    dcflux_fire_pft(:,:)=0.0
    dcflux_trace(:,:)=0.0
    dcflux_trace_pft(:,:,:)=0.0
    fc_crown(:)=0.0
    litter_consump_pft(:,:)=0.0
    litter_consump(:)=0.0
    emi_nondef(:,:)=0.0
    co2_fire(:,:)=0.0
    human_ign(:)=0.0
    tree_kill_frac(:)=0.0
    fc_1hr_carbon(:,:)=0.0
    fc_10hr_carbon(:,:)=0.0   
    fc_100hr_carbon(:,:)=0.0
    fc_1000hr_carbon(:,:)=0.0
    human_suppression(:)=0.0
    natural_vegfrac(:) = zero
    veget_max(:,:) = veget_max_org(:,:) - lcc(:,:)
    WHERE(veget_max(:,:) <= zero) veget_max(:,:)=zero

    emideforest_fuel_1hr(:,:,:) = zero
    emideforest_fuel_10hr(:,:,:) = zero
    emideforest_fuel_100hr(:,:,:) = zero
    emideforest_fuel_1000hr(:,:,:) = zero
    emideforest_litter(:,:,:) = zero
    emideforest_biomass(:,:,:) = zero
    emideforest_all(:,:) = zero
    bafrac_deforest(:,:) = zero

    ratio_C3_livegrass(:) = 1.0
    ratio_C4_livegrass(:) = zero
    char_dens_fuel_ave(:) = zero

    !we assume 0.03 of lightnning flashes input has the power to ignite a fire
    lightn_efficiency(:)=0.03


    ! Assign emission factors for trace gas emissions resulting from acflux_fire_pft
    ! ef_CO2 defined in stomate_constants.f90; the unit of ef_trace(:) is in g/(g mass burned)
    DO j=1,nvm
       ef_trace(j,1)=ef_CO2(j)/1000.0   !CO2
       ef_trace(j,2)=ef_CO(j)/1000.0    !CO
       ef_trace(j,3)=ef_CH4(j)/1000.0   !CH4
       ef_trace(j,4)=ef_VOC(j)/1000.0   !VOC
       ef_trace(j,5)=ef_TPM(j)/1000.0   !TPM
       ef_trace(j,6)=ef_NOx(j)/1000.0   !NOx
    ENDDO

    ! this is done by assuming that in forced-BA simulations, the input BA data
    ! is at monthly time step.
    IF (ANY(monthday.eq.date)) THEN
      proc_ba(:)=observed_ba(:)
    ELSE
      proc_ba(:)=0.
    ENDIF

    !----------------------------------------------------------------
    !      Prepare multiple variables needed for fire simulation
    !----------------------------------------------------------------

    !! 0. Prepare some parameters for the simulation of fires.

    !0.1 calculate the fraction of area occupied by natural PFTs.
    DO j=2,nvm
      IF (natural(j)) THEN 
           natural_vegfrac(:)=natural_vegfrac(:)+veget_max(:,j)
      ENDIF
    ENDDO
    !calculate pixelarea(lat,area)
    area_land(:) = (resolution(:,1)*resolution(:,2)*contfrac(:))/10000.0  !unit:hectar (10000m2)
    area_natural_veg(:) = area_land(:)*natural_vegfrac(:)  !unit:hectar (10000m2)

    ! Calculate total above-ground litter
    natural_litter_ag_total(:)=0.0  !natural_litter_ag_total include litter from all natural PFTs.
    DO j=1,nvm
      IF (natural(j)) THEN 
           natural_litter_ag_total(:)=natural_litter_ag_total(:)+(litter(:,imetabolic,j,iabove)+ &
           litter(:,istructural,j,iabove))*veget_max(:,j)
      ENDIF
    ENDDO

    !0.2 Calculate the moisture of extinction weighted by the fuel presence
    ! of different natural PFTs (only dead fuel expressed by sum of litter)
    ! me: moisture of extinction; me is defined in constantes_mtc.f90 
    ! as 'flammability threshold' and is PFT-specific
    me_litterfuel(:)=0.0  !PFT weighted moisture of extinction.
    DO j=1,nvm
      IF (natural(j)) THEN
        WHERE (natural_litter_ag_total(:).gt.min_stomate)
          me_litterfuel(:)=me_litterfuel(:)+((litter(:,imetabolic,j,iabove)+litter(:,istructural,j,iabove)) * &
            veget_max(:,j)/natural_litter_ag_total(:))*me(j)
        ELSEWHERE
          me_litterfuel(:)=0.0
        ENDWHERE
      ENDIF
    ENDDO

    ! calculate tree and grass coverage respectively, here the grass should
    ! not include pasture, so if the pasture PFT is included, the `natural` 
    ! variable for the pasture PFTs should be FALSE
    fpc_tree_total(:)=0.0; fpc_grass_total(:)=0.0  
    DO j=1,nvm
      IF (natural(j)) THEN
        IF (is_tree(j)) THEN
          fpc_tree_total(:)=fpc_tree_total(:)+veget_max(:,j)
        ELSE
          fpc_grass_total(:)=fpc_grass_total(:)+veget_max(:,j) 
        ENDIF
      ENDIF
    ENDDO

    !0.3 Introduce reduction factor for wind speed with respect to forest or grass ground coverage.
    ! wind_speed is @tex $U_{forward}$ @endtex in Thonicke 2010.
    ! Note that the unit of input wind speed (wspeed_daily) should be @tex $m s^{-1}$ @endtex, 
    ! so that ROS is in unit of @tex $m min^{-1}$ @endtex.
    wind_speed(:)=(fpc_tree_total(:)*wspeed_daily(:)*60.0*0.4)+ &
                (fpc_grass_total(:)*wspeed_daily(:)*60.0*0.6)  
    wind_forward(:)=3.281*wind_speed(:) !wind_forward is used in Eq(A5) as 3.281*Uforward      

    !0.4 Calculate the length-to-breadth ratio of the elliptical fire scar.
    ! the minimum value of lb is 1 and maximum value 8
    ! [UNCLEAR] Notice that for Eq.(12) and Eq.(13) in Thonicke 2010, 
    ! 0.06*wind_speed is used here as U_{forward}. Compared with original Eq.(12),
    ! an extra 0.06 was multiplied. Not yet know why this 0.06 is used, it's
    ! in the original code given to me by Patricia.
    WHERE (wind_speed(:).lt.16.67)
      lb(:) = 1    
    ELSEWHERE (wind_speed(:).ge.16.67)
      lb(:)=min(8.,fpc_tree_total(:)* &
            (1.0+(8.729*((1.0-(exp(-0.03*0.06*wind_speed(:))))**2.155)))+ &
            (fpc_grass_total(:)*(1.1+((0.06*wind_speed(:))**0.0464))))
    ENDWHERE

    !0.5 Calculate fuel characteristics, fuel amount, bulk density.
    DO k=1,nlitt
      DO j=1,nvm
        IF (natural(j)) THEN 
          WHERE (fuel_1hr(:,j,k).gt.0.0)
            fuel_1hr_total(:)=fuel_1hr_total(:) + (fuel_1hr(:,j,k)/0.45)*veget_max(:,j) !fuel_1hr devided by 0.45 to change into the unit of dry mass(ranther than C)
          ENDWHERE

          WHERE (fuel_10hr(:,j,k).gt.0.0)
            fuel_10hr_total(:)=fuel_10hr_total(:) + (fuel_10hr(:,j,k)/0.45)*veget_max(:,j)
          ENDWHERE    

          WHERE (fuel_100hr(:,j,k).gt.0.0)
            fuel_100hr_total(:)=fuel_100hr_total(:) + (fuel_100hr(:,j,k)/0.45)*veget_max(:,j)
          ENDWHERE    

          WHERE (fuel_1000hr(:,j,k).gt.0.0)
            fuel_1000hr_total(:)=fuel_1000hr_total(:) + (fuel_1000hr(:,j,k)/0.45)*veget_max(:,j) 
          ENDWHERE

          ! k.eq.1 so that the grass leaf biomass would not be counted twice in the livegrass
          ! the PFT should be natural but grass(i.e., not tree or pasture),
          ! note livegrass is in unit of dry mass but not carbon!
          IF (k.eq.1 .and. natural(j) .and. (.not. is_tree(j))) THEN   
             livegrass(:)=livegrass(:) + (biomass(:,j,ileaf)/0.45)*veget_max(:,j)
          ENDIF !k
        ENDIF!natural
      ENDDO
    ENDDO

    ! calculate dead fuel load on the groud, note that dead fuel load include 
    ! 1_hr/10_hr/100_hr fuel class of natural PFTs, and 1000_hr fuel is not
    ! included in deaf_fuel and net_fuel
    dead_fuel(:) = fuel_1hr_total(:) + fuel_10hr_total(:) + &
       fuel_100hr_total(:)  !total dead fuel g dry mass per m2
    dead_fuel_all(:) = fuel_1hr_total(:) + fuel_10hr_total(:) + &
       fuel_100hr_total(:)+fuel_1000hr_total(:) 
    dead_fuel_all_livegrass(:)=dead_fuel_all(:)+livegrass(:)
    net_fuel(:)=(1.0-MINER_TOT)*(dead_fuel(:)/1000.0)  ! in unit of kg dry matter per m2
    ! total net fuel accounting for mineral content, in kg dry mass per m2. 
    char_net_fuel(:) = net_fuel(:) +(1.0-MINER_TOT)*livegrass(:)/1000.0 ! in kg dry mass

    !fuel bulk density, weighted per fuel class and fuel load   
    !dens_fuel is PFT-specific defined and initiated in constantes_mtc.f90, unit: kg/m3 
    dens_fuel_ave(:)=0.0
    DO j=1,nvm
      IF (natural(j)) THEN
        WHERE (dead_fuel(:).gt.min_stomate)
          ratio_fbd(:) = ( ( (fuel_1hr(:,j,istructural) + fuel_1hr(:,j,imetabolic)) &
                + fbd_a * (fuel_10hr(:,j,istructural)+ fuel_10hr(:,j,imetabolic)) &
                + fbd_b * (fuel_100hr(:,j,istructural) + fuel_100hr(:,j,imetabolic)) ) *veget_max(:,j) / 0.45) &
                / dead_fuel(:) 
          dens_fuel_ave(:) = dens_fuel_ave(:) + dens_fuel(j) * ratio_fbd(:)  !kg/m3
        ELSEWHERE
          dens_fuel_ave(:)=0.0
        ENDWHERE
      ENDIF
    ENDDO

    !calculate livegrass fuel moisture
    WHERE (livegrass(:).GT.min_stomate)
      !Eq(B2); soilhum_daily is \omiga_{s} in Eq.(B2); dfm_lg is live grass moisture
      dfm_lg(:) = max(0.0,((10.0/9.0)*soilhum_daily(:)-(1.0/9.0)))  
      ratio_C3_livegrass(:) = biomass(:,8,ileaf)/0.45*veget_max(:,8) / livegrass(:)  
      ratio_C4_livegrass(:) = biomass(:,9,ileaf)/0.45*veget_max(:,9) / livegrass(:)
    ELSEWHERE
      dfm_lg(:)=0.0
    ENDWHERE

    ! influence of livegrass on FBD(fuel bulk density)
    dens_livegrass_ave(:) = & 
       fbd_C3_livegrass *  ratio_C3_livegrass(:) + &
       fbd_C4_livegrass *  ratio_C4_livegrass(:)

    !calculate average fuel density weighted by different PFT and deal fuel & live grass 
    WHERE (dead_fuel(:).gt.min_stomate.or. livegrass(:).gt.min_stomate)
      ratio_dead_fuel(:) = dead_fuel(:)  / (dead_fuel(:) + livegrass(:))
      ratio_live_fuel(:) = livegrass(:) / (dead_fuel(:) + livegrass(:))
      char_dens_fuel_ave(:) = dens_fuel_ave(:)* ratio_dead_fuel(:) + &
                              dens_livegrass_ave(:) * ratio_live_fuel(:)
    ENDWHERE

    ! calculate deal fuel and livegrass weighted average moisture of extinction.
    ! moist_extinction is me in Eq(8); moisture of extinction. 
    moist_extinction(:) = me_litterfuel(:) *  ratio_dead_fuel(:) + &
         me_livegrass * ratio_live_fuel(:)

    !calculate mass weighted surface-area-to-volume ratio by fuel types.
    WHERE (dead_fuel(:).gt.min_stomate)
      sigma(:)=(fuel_1hr_total(:) * sigma_1hr + &
           fuel_10hr_total(:) * sigma_10hr + &
           fuel_100hr_total(:) * sigma_100hr) / dead_fuel(:)
    ELSEWHERE
      sigma(:)=0.00001
    ENDWHERE

    !-----------------------------------------------------------
    !      end preparing daily variables - start of simulation
    !-----------------------------------------------------------


    !! 1 Calculate daily fire burned area
    !! 1.1 Calculate fire danger index
    ! output variables: dfm(\omiga_{0} in Eq.6); dfm_1hr(moisture content for 1hr fuel);
    !                   d_fdi(fire danger index);
    ! modified variables: ni_acc(accumulative Nesterov Index)  
    CALL fire_danger_index(npts,d_fdi,dfm,dfm_1hr,t2m_min_daily,     &
              t2m_max_daily, precip_daily,me_litterfuel, ni_acc,     &
              fuel_1hr_total, fuel_10hr_total,fuel_100hr_total,      &
              dead_fuel,moist_extinction, ratio_dead_fuel,ratio_live_fuel)

    !calculate human ignitions
    CALL human_ign_fct(npts,popd,a_nd,human_ign)
    !CALL human_suppression_func(npts,popd,human_suppression)

    !apply the ignition efficiency as adjusted by fuel load
    WHERE (dead_fuel_all_livegrass(:) .LE. fuel_low_bound) 
      ignition_efficiency(:)=0.
    ELSEWHERE (dead_fuel_all_livegrass(:) .GT. fuel_low_bound .AND. dead_fuel_all_livegrass(:) .LT. fuel_high_bound)
      ignition_efficiency(:)=(dead_fuel_all_livegrass(:) - fuel_low_bound)/(fuel_high_bound - fuel_low_bound)
    ELSEWHERE
      ignition_efficiency(:)=1.
    ENDWHERE

    !! 1.2 Calculate number of fires from lightning and human ignitions

    !dervie potential fire number by lightnings
    lightn_ign(:)=lightn(:)*lightn_efficiency(:)*(1-human_suppression(:))

    !Here we apply Eq.(2), d_fdi: unitless; lightning ignition: 1/day/km**2 (as in the original nc file); 
    !human_ign: unit as 1/day/km**2
    !0.01*area_land: grid cell total land area in km**2; 
    !final unit: 1/day in the concered grid cell 
    WHERE (net_fuel(:).gt.0.001)  
      lightn_numfire(:)=d_fdi(:)*ignition_efficiency(:)* lightn_ign(:) * (0.01 * area_natural_veg(:))  
      human_numfire(:)=d_fdi(:)*ignition_efficiency(:)* human_ign(:) * (0.01 * area_natural_veg(:))
      d_numfire(:)=lightn_numfire(:) + human_numfire(:)
    ELSEWHERE  !not enough fuel
      d_numfire(:)=0.0
    ENDWHERE

    !! 1.3 Calculate daily area burnt

    ! calculate fire spread rate
    !output variables: ros_f(ROS_{f,surface} in Eq.9); gamma(used in calculation of tau_l); 
    !                  wetness (dfm/me).
    ! note when call rate_of_spread, ros_f has replaced U_front in the definition of rate_of_spread SUBROUTINE
    CALL rate_of_spread(npts,ros_f,wind_forward,sigma,  &
             dfm,me_litterfuel,H,char_dens_fuel_ave,    &
             char_net_fuel,moist_extinction,gamma,wetness)

    ! Calculate ROS_{b,surface}; Eq(10); wind_speed is U_{forward} in Eq.(10) 
    ros_b(:) = ros_f(:) * exp(-0.012 * wind_speed(:)) !Can FBP System; Eq.(10) in Thonicke et al. (2010)
    WHERE (ros_b(:).lt. 0.05) 
      ros_b(:) = 0.0 
    ENDWHERE 

    ! fire duration as a function of d_fdi; Eq(14)
    WHERE (d_fdi(:).ge.min_stomate)
      fire_durat(:)=241.0/(1.0+(240.0*exp(-11.06*d_fdi(:))))    !unit:min, Eq.(14) in Thonicke et al. (2010)
    ELSEWHERE
      fire_durat(:)=0.0
    ENDWHERE

    db(:)=ros_b(:)*fire_durat(:) !unit: m/min * min = m
    df(:)=ros_f(:)*fire_durat(:)

    ! Here we call deforestation fire process
    IF (allow_deforest_fire) THEN
      ! Calculate deforestation fire burned area fraction and emissions
      CALL deforestation_fire_proxy(npts,d_fdi,lcc,                     &
         deforest_biomass_remain,                     &
         def_fuel_1hr_remain,def_fuel_10hr_remain,                           &
         def_fuel_100hr_remain,def_fuel_1000hr_remain,                       &
         bafrac_deforest,emideforest_fuel_1hr,emideforest_fuel_10hr,        &
         emideforest_fuel_100hr, emideforest_fuel_1000hr,emideforest_biomass)

      ! If the accumulated deforestation fire burned area is higher than two times 
      ! of deforested area, we treat the surpassing part as natural fires.
      DO ivm = 1,nvm
        IF (is_tree(ivm)) THEN
          DO ipts = 1,npts
            IF (bafrac_deforest(ipts,ivm) > min_stomate) THEN
              IF (bafrac_deforest_accu(ipts,ivm)+bafrac_deforest(ipts,ivm) > 3*lcc(ipts,ivm)) THEN
                 d_area_burnt(ipts) = d_area_burnt(ipts) + bafrac_deforest(ipts,ivm) * area_land(ipts)
                 ba_escape(ipts) = ba_escape(ipts) + bafrac_deforest(ipts,ivm) * area_land(ipts)
                 emideforest_fuel_1hr(ipts,ivm,:) = zero
                 emideforest_fuel_10hr(ipts,ivm,:) = zero
                 emideforest_fuel_100hr(ipts,ivm,:) = zero
                 emideforest_fuel_1000hr(ipts,ivm,:) = zero
                 emideforest_biomass(ipts,ivm,:) = zero 
                 bafrac_deforest(ipts,ivm) = zero
              ELSE
                 bafrac_deforest_accu(ipts,ivm) = bafrac_deforest_accu(ipts,ivm) + bafrac_deforest(ipts,ivm)
                 emideforest_litter(ipts,ivm,:) = emideforest_fuel_1hr(ipts,ivm,:) + emideforest_fuel_10hr(ipts,ivm,:) &
                                             + emideforest_fuel_100hr(ipts,ivm,:) + emideforest_fuel_1000hr(ipts,ivm,:)
                 
                 emideforest_litter_accu(ipts,ivm,:) = emideforest_litter_accu(ipts,ivm,:) + emideforest_litter(ipts,ivm,:)  
                 emideforest_biomass_accu(ipts,ivm,:) = emideforest_biomass_accu(ipts,ivm,:) + emideforest_biomass(ipts,ivm,:)  
                 !update the remaining fuel
                 def_fuel_1hr_remain(ipts,ivm,:) = def_fuel_1hr_remain(ipts,ivm,:)-emideforest_fuel_1hr(ipts,ivm,:)
                 def_fuel_10hr_remain(ipts,ivm,:) = def_fuel_10hr_remain(ipts,ivm,:)-emideforest_fuel_10hr(ipts,ivm,:)
                 def_fuel_100hr_remain(ipts,ivm,:) = def_fuel_100hr_remain(ipts,ivm,:)-emideforest_fuel_100hr(ipts,ivm,:)
                 def_fuel_1000hr_remain(ipts,ivm,:) = def_fuel_1000hr_remain(ipts,ivm,:)-emideforest_fuel_1000hr(ipts,ivm,:)
                 deforest_biomass_remain(ipts,ivm,:) = deforest_biomass_remain(ipts,ivm,:)-emideforest_biomass(ipts,ivm,:)
                 deforest_litter_remain(ipts,:,ivm,iabove) = deforest_litter_remain(ipts,:,ivm,iabove)-emideforest_litter(ipts,ivm,:)
              END IF
            END IF
          END DO
        END IF
      END DO ! loop of nvm

      WHERE(veget_max(:,:)>min_stomate)
        emideforest_all(:,:) = (SUM(emideforest_litter,DIM=3) + SUM(emideforest_biomass,DIM=3))/veget_max(:,:)/dt_days
      ENDWHERE
    END IF

    ! area burnt from caused by ignitions on "this (simulation)" day [sptifire on daily timestep]
    WHERE (lb(:).gt.min_stomate)
      mean_fire_size_original(:)=(pi/(4.0*lb(:)))*((df(:)+db(:))**2.0)/10000.0 !mean_fire_size with unit as ha
      d_area_burnt(:)=d_numfire(:)* mean_fire_size_original(:)  ! d_area_burnt unit in ha
      mean_fire_size(:)=mean_fire_size_original(:)
      
      !set the flag to 1 when forcing BA, otherwise set the flag as -1 or 0
      WHERE (ratio_flag(:).gt.0.0)
       d_area_burnt(:)=proc_ba(:)
      ELSEWHERE
       d_area_burnt(:)=d_area_burnt(:)*baadjust_ratio(:)   !set the ratio alwasy to 1 when not forcing BA.
      ENDWHERE

      ! Prevent burned area within the limit of vegetated area
      WHERE (d_area_burnt(:).gt.area_natural_veg(:)) 
            d_area_burnt(:)=area_natural_veg(:)
      ENDWHERE

      fire_frac(:) = d_area_burnt(:)/area_land(:)

    ENDWHERE !  lb.gt.0

    !! 1.4 Calculate daily fire emissions

    fuel_consum(:)=0.0
    CALL fuel_consumption(npts,fuel_consum,fire_frac,           &
            fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr,livegrass,biomass,  &
            fuel_1hr_total,dfm,dfm_1hr,wetness,MINER_TOT,fc_1hr,fc_lg,    &
            fc_lf,fc_10hr,fc_100hr,fc_1000hr,cf_ave,                      &
            moist_extinction,dfm_lg,veget_max,                            &
            ratio_flag,cf_coarse,cf_fine,cf_lg,cf_1hr,cf_10hr,cf_100hr,cf_1000hr)

    ! calculate surface fire intensity
    ! Notice the fuel_consum doesn't include the 1000hr fuel
    WHERE ((fire_frac(:).gt.min_stomate).and.(fuel_consum(:).gt.min_stomate))
      d_i_surface(:)=H*(fuel_consum(:)/1000.0/fire_frac(:))*(ros_f(:)/60.0)  !Eq(15); unit of d_i_surface: kW/m
    ENDWHERE

    ! we suppress the simulated fires if their intensity is too small
    WHERE (d_i_surface(:) .lt. surface_threshold)
      lightn_numfire(:)=0.0
      human_numfire(:)=0.0
      d_numfire(:)=0.0
      d_area_burnt(:)=0.0
      fire_frac(:)=0.0
      d_i_surface(:)=0.0
      mean_fire_size(:)=0.0
      cf_lg(:)=0.0
      cf_1hr(:)=0.0
      cf_ave(:)=0.0
      cf_10hr(:)=0.0
      cf_100hr(:)=0.0
      cf_1000hr(:)=0.0
    ELSEWHERE
      fire_numday(:)=fire_numday(:)+1.
      d_numfire(:)=d_numfire(:)/area_land(:)  !change the unit of d_numfire to 1ha^{-1}day^{-1}
      lightn_numfire(:)=lightn_numfire(:)/area_land(:)  !change the unit of d_numfire to 1ha^{-1}day^{-1}
      human_numfire(:)=human_numfire(:)/area_land(:)  !change the unit of d_numfire to 1ha^{-1}day^{-1}
    ENDWHERE

    ! Implement fire consequences, remove biomass being burned, mortality etc.
    DO j=1,nvm
      IF (natural(j)) THEN
        DO k = 1, nlitt
          ! surface fire: update fuel load per dead fuel class and ground litter
          WHERE (d_i_surface(:) .ge. surface_threshold)
            fuel_1hr(:,j,k)=fuel_1hr(:,j,k) - fc_1hr(:,j,k)*0.45 ! convert back to gC/m2; fuel_1h(npts,nvm,nlitt) unit shoule be gC/m2
            fuel_10hr(:,j,k)=fuel_10hr(:,j,k) - fc_10hr(:,j,k)*0.45
            fuel_100hr(:,j,k)=fuel_100hr(:,j,k) - fc_100hr(:,j,k)*0.45
            fuel_1000hr(:,j,k)=fuel_1000hr(:,j,k) - fc_1000hr(:,j,k)  !always in gC/m2
            litter(:,k,j,iabove)=litter(:,k,j,iabove)-(fc_1hr(:,j,k)+fc_10hr(:,j,k)+fc_100hr(:,j,k))*0.45-fc_1000hr(:,j,k) 
          ELSEWHERE
            fc_1hr(:,j,k)=0.
            fc_10hr(:,j,k)=0.
            fc_100hr(:,j,k)=0.
            fc_1000hr(:,j,k)=0.
          ENDWHERE
        ENDDO !nlitt

        !include ground litter emission to litter_consump_pft
        litter_consump_pft(:,j)=litter_consump_pft(:,j)  &
        + (fc_1hr(:,j,imetabolic)+fc_10hr(:,j,imetabolic)+fc_100hr(:,j,imetabolic))*0.45 & 
           + fc_1000hr(:,j,imetabolic)              &
        + (fc_1hr(:,j,istructural)+fc_10hr(:,j,istructural)+fc_100hr(:,j,istructural))*0.45 &
           + fc_1000hr(:,j,istructural)

        !! Apply fire effects for trees, when surface fire intensity is greater than 50 W/m

        !! start of crown scorching
        ! crown_length defined in constantes_mtc.f90 indicating the fraction of crown length to tree height.
        IF (is_tree(j)) THEN

          !when I_{surface} < 50kWm-1, ignitions are extinguished
          WHERE (d_i_surface(:).ge. surface_threshold .and. PFTpresent(:,j))  
            sh(:,j)=f_sh(j)*(d_i_surface(:)**0.667)  !scorch height per PFT Eq.(16); 
                                                     !f_sh(j) defined in constantes_mtc.f90
          ELSEWHERE
            sh(:,j)=0.0       
          ENDWHERE

          WHERE (gamma(:).gt.min_stomate)
            tau_l(:,j)=2.0*(cf_ave(:)/gamma(:)) ! Equation(46) in SPITFIRE technical document.
          ELSEWHERE
            tau_l(:,j)=0.0
          ENDWHERE

          ck(:,j)=0.0
          pm_ck(:,j)=0.0

          WHERE (height(:,j).gt.min_stomate .and. crown_length(j).gt.min_stomate & 
                 .and. sh(:,j).ge.(height(:,j) - (height(:,j) * crown_length(j)))) 
            ck(:,j) = (sh(:,j) - height(:,j) + (height(:,j) * crown_length(j))) &
                        /(height(:,j) * crown_length(j))
          ENDWHERE

          WHERE (sh(:,j).GE.height(:,j))
            ck(:,j) = 1.0 
          ENDWHERE

          ! post-fire mortality from crown scorching
          WHERE (ck(:,j).gt.min_stomate)
            pm_ck(:,j)=r_ck(j)*(ck(:,j)**p_ck(j)) !Eq.(22); r_ck & p_ck defined in constantes_mtc.f90
          ENDWHERE

          ! post-fire mortality from cambial damage
          ! calculate Bark Thickness (bt) and tau_c
          dia(:) = (height(:,j)/pipe_tune2)**(1./pipe_tune3)*100 !height is defined as height_presc in src_parameters/constantes_veg.f90; unit:m
          ! because in lpj_crown.f90: height(:,j) = pipe_tune2*(dia(:)**pipe_tune3)
          bt(:,j)=BTpar1(j) * dia(:)+BTpar2(j)   !Eq(21)
          tau_c(:,j)=2.9 * (bt(:,j)**2.0)

          ! tau_l/tau_c is the ratio of the residence time of the fire to the critical time for cambial damage.
          WHERE (tau_c(:,j).ge.min_stomate)
            WHERE ( (tau_l(:,j)/tau_c(:,j)).ge.2.0) 
              pm_tau(:,j)=1.0
            ELSEWHERE ((tau_l(:,j)/tau_c(:,j)).gt.0.22)
              pm_tau(:,j)= (0.563*(tau_l(:,j)/tau_c(:,j)))-0.125
            ELSEWHERE ((tau_l(:,j)/tau_c(:,j)).le.0.22) 
              pm_tau(:,j)=0.0
            ENDWHERE
          ELSEWHERE
            pm_tau(:,j)=0.0    
          ENDWHERE
           
          ! Calculate total post-fire mortality from crown scorching AND cambial kill; Eq(18)
          postf_mort(:,j)=pm_tau(:,j)+pm_ck(:,j)-(pm_tau(:,j)*pm_ck(:,j))

          ! number of individuals affected by fire in grid cell
          nind_fa(:,j)=fire_frac(:)*ind(:,j)
          ! the number of individuals killed by crown scorching is 
          ! derived by multiplying mortality rate with number of individuals affected by fire. 
          nind_kill(:,j) = postf_mort(:,j) * nind_fa(:,j)


          ! Here explains how consequence of fire-killed trees are handled.
          ! the fraction of trees that are kill in fire is `postf_mort*fire_frac`,
          ! Of the killed trees, `ck` of them have been crown-scroached by fire
          ! and the 1-,10- and 100-h live fuel and 5% of the 1000-h fuel are combusted. 
          ! the biomass of trees that are crown-scoarched but not consumed in fire 
          ! would go to bm_to_litter. Then, the killed but not crown-scroached tree
          ! biomass (1-ck) go directly to bm_to_litter.
          
          ! In summary, all the biomass of killed tress should be removed from 
          ! live biomass pool as they are either combusted or transferred to bm_to_litter. 
          ! Of the fraction (postf_mort * fire_frac) that are killed in fire, of which
          !    --- ck (fraction) have been crown scorached, of which
          !        -- 1-,10- and 100-h live fule and 5% of the 1000-h fuel are combusted;
          !        -- 95% 1000-h fuel go to bm_to_litter;  
          !    --- (1-ck) (fraction) go to bm_to_litter.

          tree_kill_frac(:)=postf_mort(:,j)*fire_frac(:)
          !disturb_crown is carbon emssions from crown scorching.
          disturb_crown(:,j) = tree_kill_frac(:) * ck(:,j) *                    & 
           (biomass(:,j,ileaf) + biomass(:,j,ifruit) +  &
            0.0450 * biomass(:,j,isapabove) + 0.0450 * biomass(:,j,iheartabove) +     &
            0.0450 * biomass(:,j,icarbres) +                                         &
            0.0750 * biomass(:,j,isapabove) + 0.0750 * biomass(:,j,iheartabove) +     &
            0.0750 * biomass(:,j,icarbres) +                                         &
            0.2100 * biomass(:,j,isapabove) + 0.2100 * biomass(:,j,icarbres) +      &
            0.2100 * biomass(:,j,iheartabove) +                                       & 
            0.6700 * 0.050 * biomass(:,j,icarbres) + 0.6700 * 0.050 * biomass(:,j,iheartabove) + &
            0.6700 * 0.050 * biomass(:,j,isapabove)                               & 
           ) 

          !fire flux to atmosphere, now biomass emissions from trees are included.
          dcflux_fire_pft(:,j)=dcflux_fire_pft(:,j)+disturb_crown(:,j)
          fc_crown(:)=fc_crown(:)+disturb_crown(:,j)*veget_max(:,j)  

          ! biomass to litter transfer; (the killed but not crown scorching) AND (crown scorching but not combusted)
          ! 1-ck(:,j)---killed but not crown scorching;
          ! ck(:,j)*(1-0.045-0.075-0.21-0.67*0.05)---crown scorching but not combusted. 
          bm_to_litter(:,j,ileaf) = bm_to_litter(:,j,ileaf) +                           & 
                                    (tree_kill_frac(:) * (1-ck(:,j)) * biomass(:,j,ileaf)) 
          bm_to_litter(:,j,ifruit) = bm_to_litter(:,j,ifruit) +                         & 
                                    (tree_kill_frac(:) * (1-ck(:,j)) * biomass(:,j,ifruit)) 
          bm_to_litter(:,j,isapabove) = bm_to_litter(:,j,isapabove) +                   & 
                           (tree_kill_frac(:) * (1-ck(:,j)+ck(:,j)*  &
                           (1-0.045-0.075-0.21-0.67*0.05))* biomass(:,j,isapabove)) 
          bm_to_litter(:,j,icarbres) = bm_to_litter(:,j,icarbres) +                     &
                           (tree_kill_frac(:) * (1-ck(:,j)+ck(:,j)*  &
                           (1-0.045-0.075-0.21-0.67*0.05))* biomass(:,j,icarbres)) 
          bm_to_litter(:,j,iheartabove) = bm_to_litter(:,j,iheartabove) +               &
                           (tree_kill_frac(:) * (1-ck(:,j)+ck(:,j)*  &
                           (1-0.045-0.075-0.21-0.67*0.05))* biomass(:,j,iheartabove)) 
          ! move root/sapbelow/heartbelow biomass to belowground litter pool.
          bm_to_litter(:,j,iroot) = bm_to_litter(:,j,iroot) +                           & 
                                    (tree_kill_frac(:) * biomass(:,j,iroot)) 
          bm_to_litter(:,j,isapbelow) = bm_to_litter(:,j,isapbelow) +           & 
                                    (tree_kill_frac(:) * biomass(:,j,isapbelow)) 
          bm_to_litter(:,j,iheartbelow) = bm_to_litter(:,j,iheartbelow) +           & 
                                    (tree_kill_frac(:) * biomass(:,j,iheartbelow)) 

          ! biomass update after combustion and litter transfer.
          biomass(:,j,1) = biomass(:,j,1) - (tree_kill_frac(:) * biomass(:,j,1)) 
          biomass(:,j,2) = biomass(:,j,2) - (tree_kill_frac(:) * biomass(:,j,2)) 
          biomass(:,j,3) = biomass(:,j,3) - (tree_kill_frac(:) * biomass(:,j,3)) 
          biomass(:,j,4) = biomass(:,j,4) - (tree_kill_frac(:) * biomass(:,j,4)) 
          biomass(:,j,5) = biomass(:,j,5) - (tree_kill_frac(:) * biomass(:,j,5)) 
          biomass(:,j,6) = biomass(:,j,6) - (tree_kill_frac(:) * biomass(:,j,6)) 
          biomass(:,j,7) = biomass(:,j,7) - (tree_kill_frac(:) * biomass(:,j,7)) 
          biomass(:,j,8) = biomass(:,j,8) - (tree_kill_frac(:) * biomass(:,j,8)) 


          ! Update number of individuals surviving fire, ready for the next day. 
          ind(:,j)= ind(:,j) - nind_kill(:,j)

        ELSE !grass

          WHERE (d_i_surface(:).ge.surface_threshold .and. PFTpresent(:,j)) 
            !grass leaf & fruit consumption are also included in litter consumption
            litter_consump_pft(:,j)=litter_consump_pft(:,j)+fc_lg(:,j)+fc_lf(:,j) !leaf & fruit consumption in fire for GRASS also included in litter consumption
            biomass(:,j,ileaf)=biomass(:,j,ileaf)-fc_lg(:,j)
            biomass(:,j,ifruit)=biomass(:,j,ifruit)-fc_lf(:,j)
          ENDWHERE
        ENDIF   !IF (tree(j))

        ! add ground fuel combustion to the output variable of fire carbon flux per PFT
        ! dcflux_fire_pft:total fire emissions, including crown fuels and ground litter consumption
        dcflux_fire_pft(:,j)=dcflux_fire_pft(:,j) + litter_consump_pft(:,j)
        litter_consump(:)=litter_consump(:)+(litter_consump_pft(:,j) *veget_max(:,j)/dt_days)
      ENDIF   !IF natural
    ENDDO  !fire effects per PFTj


    ! total carbon emission this day; co2_fire should be equal to litter_consump+fc_crown

    emi_nondef(:,:)= dcflux_fire_pft(:,:)/dt_days
    co2_fire(:,:)= emi_nondef(:,:) + emideforest_all(:,:)
    fc_crown(:)=fc_crown(:)/dt_days 

    !!1.6 Calculate trace gas emissions
    DO x=1,6 !trace_species
      DO j=1,nvm
        dcflux_trace_pft(:,j,x)=dcflux_fire_pft(:,j) * ef_trace(j,x)/0.45 !
        dcflux_trace(:,x)=dcflux_trace(:,x)+ (dcflux_trace_pft(:,j,x) / dt_days)*veget_max(:,j) ! dcflux_trace has the same unit with dcflux_fire_pft, in g/m**2/day
      ENDDO
    ENDDO

    DO k=1,nlitt
      fc_1hr_carbon(:,:)=fc_1hr_carbon(:,:)+fc_1hr(:,:,k)*0.45
      fc_10hr_carbon(:,:)=fc_10hr_carbon(:,:)+fc_10hr(:,:,k)*0.45
      fc_100hr_carbon(:,:)=fc_100hr_carbon(:,:)+fc_100hr(:,:,k)*0.45
      fc_1000hr_carbon(:,:)=fc_1000hr_carbon(:,:)+fc_1000hr(:,:,k)
    ENDDO
     
    CALL histwrite_p (hist_id_stomate, 'CO2_FIRE_NonDef', itime, &
                    emi_nondef(:,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'CO2_FIRE_Def', itime, &
                    emideforest_all(:,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'D_FDI', itime, &
                    d_fdi(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'ROS_F', itime, &
                    ros_f(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'LIGHTN_NUMFIRE', itime, &
                    lightn_numfire(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'HUMAN_NUMFIRE', itime, &
                    human_numfire(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'D_NUMFIRE', itime, &
                    d_numfire(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'OBSERVED_BA', itime, &
                    observed_ba(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'D_AREA_BURNT', itime, &
                    d_area_burnt(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'BA_ESCAPE', itime, &
                    ba_escape(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'FIREFRAC_SPITFIRE', itime, &
                    fire_frac(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'CROWN_CONSUMP', itime, &
                    fc_crown(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'LITTER_CONSUMP', itime, &
                    litter_consump(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'LIGHTN_IGN_TOTAL', itime, &
                    lightn(:), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TRACE_GAS_CO2', itime, &
                    dcflux_trace(:,1), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TRACE_GAS_CO', itime, &
                    dcflux_trace(:,2), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TRACE_GAS_CH4', itime, &
                    dcflux_trace(:,3), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TRACE_GAS_VOC', itime, &
                    dcflux_trace(:,4), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TRACE_GAS_TPM', itime, &
                    dcflux_trace(:,5), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'TRACE_GAS_NOx', itime, &
                    dcflux_trace(:,6), npts, hori_index)
    CALL histwrite_p (hist_id_stomate, 'FIRE_NUMDAY', itime, &
                    fire_numday(:), npts, hori_index)

    CALL histwrite_p (hist_id_stomate, 'bafrac_deforest', itime, &
         bafrac_deforest(:,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'bafrac_deforest_accu', itime, &
         bafrac_deforest_accu(:,:), npts*nvm, horipft_index)

    CALL histwrite_p (hist_id_stomate, 'EDlitMET', itime, &
         emideforest_litter(:,:,imetabolic), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDlitSTR', itime, &
         emideforest_litter(:,:,istructural), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDlitMET', itime, &
         emideforest_litter_accu(:,:,imetabolic), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDlitSTR', itime, &
         emideforest_litter_accu(:,:,istructural), npts*nvm, horipft_index)



    CALL histwrite_p (hist_id_stomate, 'EDbioLEAF', itime, &
         emideforest_biomass(:,:,ileaf), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioRESERVE', itime, &
         emideforest_biomass(:,:,icarbres), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioFRUIT', itime, &
         emideforest_biomass(:,:,ifruit), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioSapABOVE', itime, &
         emideforest_biomass(:,:,isapabove), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioHeartABOVE', itime, &
         emideforest_biomass(:,:,iheartabove), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioSapBELOW', itime, &
         emideforest_biomass(:,:,isapbelow), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioHeartBELOW', itime, &
         emideforest_biomass(:,:,iheartbelow), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'EDbioROOT', itime, &
         emideforest_biomass(:,:,iroot), npts*nvm, horipft_index)

    CALL histwrite_p (hist_id_stomate, 'AccEDbioLEAF', itime, &
         emideforest_biomass_accu(:,:,ileaf), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioRESERVE', itime, &
         emideforest_biomass_accu(:,:,icarbres), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioFRUIT', itime, &
         emideforest_biomass_accu(:,:,ifruit), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioSapABOVE', itime, &
         emideforest_biomass_accu(:,:,isapabove), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioHeartABOVE', itime, &
         emideforest_biomass_accu(:,:,iheartabove), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioSapBELOW', itime, &
         emideforest_biomass_accu(:,:,isapbelow), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioHeartBELOW', itime, &
         emideforest_biomass_accu(:,:,iheartbelow), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'AccEDbioROOT', itime, &
         emideforest_biomass_accu(:,:,iroot), npts*nvm, horipft_index)


!debuging only
    !CALL histwrite_p (hist_id_stomate, 'cf_lg', itime, &
    !                cf_lg(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'cf_1hr', itime, &
    !                cf_1hr(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'cf_10hr', itime, &
    !                cf_10hr(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'cf_100hr', itime, &
    !                cf_100hr(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'cf_ave', itime, &
    !                cf_ave(:), npts, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'cf_1000hr', itime, &
    !                cf_1000hr(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'fc_1hr_carbon', itime, &
    !     fc_1hr_carbon(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'fc_10hr_carbon', itime, &
    !     fc_10hr_carbon(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'fc_100hr_carbon', itime, &
    !     fc_100hr_carbon(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'fc_1000hr_carbon', itime, &
    !     fc_1000hr_carbon(:,:), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_1hr_met', itime, &
         fuel_1hr(:,:,imetabolic), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_1hr_str', itime, &
         fuel_1hr(:,:,istructural), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_10hr_met', itime, &
         fuel_10hr(:,:,imetabolic), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_10hr_str', itime, &
         fuel_10hr(:,:,istructural), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_100hr_met', itime, &
         fuel_100hr(:,:,imetabolic), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_100hr_str', itime, &
         fuel_100hr(:,:,istructural), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_1000hr_met', itime, &
         fuel_1000hr(:,:,imetabolic), npts*nvm, horipft_index)
    CALL histwrite_p (hist_id_stomate, 'fuel_1000hr_str', itime, &
         fuel_1000hr(:,:,istructural), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'dead_fuel', itime, &
    !                dead_fuel(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'dead_fuel_all', itime, &
    !                dead_fuel_all(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'ck', itime, &
    !     ck(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'sh', itime, &
    !     sh(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'postf_mort', itime, &
    !     postf_mort(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'pm_tau', itime, &
    !     pm_tau(:,:), npts*nvm, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'pm_ck', itime, &
    !     pm_ck(:,:), npts*nvm, horipft_index)

    !CALL histwrite_p (hist_id_stomate, 'topsoilhum_daily', itime, &
    !     soilhum_daily(:), npts, horipft_index)

    !CALL histwrite_p (hist_id_stomate, 'fire_durat', itime, &
    !                fire_durat(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'ros_b', itime, &
    !                ros_b(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'ros_f', itime, &
    !                ros_f(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'wind_speed', itime, &
    !                wind_speed(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'mean_fire_size_or', itime, &
    !                mean_fire_size_original(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'mean_fire_size', itime, &
    !                mean_fire_size(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'd_i_surface', itime, &
    !                d_i_surface(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'sigma', itime, &
    !                sigma(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'char_dens_fuel_ave', itime, &
    !                char_dens_fuel_ave(:), npts, hori_index)

    END SUBROUTINE SPITFIRE


!! ================================================================================================================================
!! SUBROUTINE     : fire_danger_index
!!                                                                              
!>\BRIEF        Calculation of the fire danger index using the Nesterov Index      
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  dfm, dfm_1hr, d_fdi                                               
!! 
!! MODIFIED VALUE: ni_acc
!!                                                                              
!! REFERENCE(S)   : 
!! - K.Thonicke, A.Spessa, I.C. Prentice, S.P.Harrison, L.Dong, and
!! C.Carmona-Moreno, 2010: The influence of vegetation, fire spread and fire
!! behaviour on biomass burning and trace gas emissions: results from a
!! process-based model. Biogeosciences, 7, 1991-2011.
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    SUBROUTINE  fire_danger_index(npts,d_fdi,dfm,dfm_1hr,t2m_min_daily,  &
                  t2m_max_daily, precip_daily,me_litterfuel,ni_acc,      &
                  fuel_1hr_total,fuel_10hr_total,fuel_100hr_total,       &
                  dead_fuel,moist_extinction,ratio_dead_fuel, ratio_live_fuel)


    implicit none

    ! Domain size
    INTEGER, INTENT(in)                                 :: npts

    ! INPUT VARIABLES
    REAL(r_std),DIMENSION(npts), INTENT(in)              :: t2m_min_daily     !! Daily minimum 2 meter temperatures (K)
    REAL(r_std),DIMENSION(npts), INTENT(in)              :: t2m_max_daily     !! Daily maximum 2 meter temperatures (K)
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: precip_daily      !! daily precip (mm/day)
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: me_litterfuel     !! the moisture of extinction weighted by the
                                                                              !! dead groud litter fuel of different natural PFTs, unitless
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: fuel_1hr_total    !! in the unit of dry mass rather than C
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: fuel_10hr_total
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: fuel_100hr_total
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: dead_fuel
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: moist_extinction
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ratio_dead_fuel
    REAL(r_std), DIMENSION(npts), INTENT(in)             :: ratio_live_fuel

    ! OUTPUT VARIABLES 
    REAL(r_std), DIMENSION(npts), INTENT(out)            :: dfm               !! Daily fuel moisture (0,1), unitless.
    REAL(r_std), DIMENSION(npts), INTENT(out)            :: dfm_1hr           !! Fuel moisture for the 1hr fuel, used only in calculating the dfm_lg_d1hr
    REAL(r_std), DIMENSION(npts), INTENT(out)            :: d_fdi

    ! MODIFIED VARIABLES
    REAL(r_std), DIMENSION(npts),INTENT(INOUT)           :: ni_acc !!

    ! LOCAL VARIABLES
    ! alpha values are in proportion to their surface-to-volume ratios and reflects their impact on the overall dryness of fuel.
    ! lower suface-area-to-volume ratio leads to low alpha value, meaning that fuels needs a long time to be dry has less importance
    ! in determinning overall fuel mositure.
    REAL(r_std), parameter                           :: alpha=0.0015               !! this variables is NOT USED.
    REAL(r_std), parameter                           :: alpha_1hr=0.001            !! from alpha_1hr --> alpha_1000hr: source SupInfo Table 4 and Thonicke 2010 P1996
    REAL(r_std), parameter                           :: alpha_10hr=0.00005424
    REAL(r_std), parameter                           :: alpha_100hr=0.00001485 
    REAL(r_std), parameter                           :: alpha_1000hr = 0.000001    !! NOT USED
    REAL(r_std), parameter                           :: alpha_livegrass = 0.0005   !! SOURCE
    REAL(r_std), DIMENSION(npts)                     :: d_NI  !Nesterov Index (.C^2) 
    REAL(r_std), DIMENSION(npts)                     :: alpha_fuel
    REAL(r_std), DIMENSION(npts)                     :: char_alpha_fuel



    ! Initialise
    d_NI(:)=0.
    alpha_fuel(:)=0.

    ! calculate Nesterov Index Eq.(5); ZeroCelsius=273.15 in stomate_constant.f90
    WHERE  ( (precip_daily(:) .LE. 3.) .AND. ((t2m_min_daily(:) -4.) .GE. ZeroCelsius) ) 
      d_NI(:) = (t2m_max_daily(:)-273.16)*( t2m_max_daily(:) - (t2m_min_daily(:)-4.) )
    ENDWHERE 

    WHERE (d_NI(:) .GT. 0.)
      ni_acc(:) = ni_acc(:) + d_NI(:)
    ELSEWHERE
      ni_acc(:)=0.
    ENDWHERE

    ! litter moisture index, weighted per dead fuel class and

    WHERE (dead_fuel(:).gt.min_stomate) 
       alpha_fuel(:)=(alpha_1hr*fuel_1hr_total(:)+ &
                     alpha_10hr*fuel_10hr_total(:)+ &
                     alpha_100hr*fuel_100hr_total(:))/dead_fuel(:)
    ELSEWHERE
       alpha_fuel(:)=0.00001
    ENDWHERE

    dfm(:)=exp(-alpha_fuel(:)*ni_acc(:))  ! daily fuel moisture;  \omega_{o} in Eq(6); 
                                          ! used for fire spread rate calculation. 1000hr fuel ignored.
    dfm_1hr(:) = exp(-alpha_1hr * ni_acc(:))  !moisture for the fuel_1hr.

    ! calculate Fire Danger Index d_fdi, Eq.(8)
    ! moist_extinction acts as the 'me' in the FDI equation (Equation 8 in Thonicke 2010 Biogeoscience)
    WHERE (moist_extinction(:).le.min_stomate)
       d_fdi(:)=0.0
    ELSEWHERE
       d_fdi(:)=max(0.0,(1.0 - 1.0/moist_extinction(:)*dfm(:) ))
    ENDWHERE

    !Fire danger index becomes zero when Nesterov index is lower than 0.
    WHERE (d_NI(:).le.0.) 
       d_fdi(:)=0.0
    ENDWHERE


    ! output for testing only
    !CALL histwrite_p (hist_id_stomate, 'moist_extinction', itime, &
    !                moist_extinction(:), npts, horipft_index)
    !CALL histwrite_p (hist_id_stomate, 'alpha_fuel', itime, &
    !                alpha_fuel(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'ni_acc', itime, &
    !                ni_acc(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 't2m_min_daily', itime, &
    !                t2m_min_daily(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 't2m_max_daily', itime, &
    !                t2m_max_daily(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'precip_daily', itime, &
    !                precip_daily(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'dfm_1hr', itime, &
    !                dfm_1hr(:), npts, hori_index)

    END SUBROUTINE fire_danger_index

!! ================================================================================================================================
!! SUBROUTINE     : human_ign_fct
!!                                                                              
!>\BRIEF        Calculation human ignitions
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  Nonw
!! 
!! REFERENCE(S)   : 
!! - K.Thonicke, A.Spessa, I.C. Prentice, S.P.Harrison, L.Dong, and
!! C.Carmona-Moreno, 2010: The influence of vegetation, fire spread and fire
!! behaviour on biomass burning and trace gas emissions: results from a
!! process-based model. Biogeosciences, 7, 1991-2011.
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    ! Algorithm in this function is very carefully checked (2012/05/31)
    SUBROUTINE human_ign_fct(npts,popd,a_nd,human_ign) 

    implicit none

    ! Domain size
    INTEGER, INTENT(in)                              :: npts
    ! INPUT Variables
    REAL(r_std), DIMENSION(npts), INTENT(in)         ::  popd  !individuals km^-2
    REAL(r_std), DIMENSION(npts), INTENT(in)         ::  a_nd  !ignitions individual^-1 d^-1
    ! OUTPUT
    REAL(r_std), DIMENSION(npts), INTENT(out)        ::  human_ign  

    WHERE(popd(:).ge.min_stomate)
      !compared with Eq(3) in Thonicke et al. (2010), we have to again divide
      !by 100 to change the unit from ignitions d^{-1}ha^{-1} to ignitions d^{-1}km^{-1}
      human_ign(:)=30.0*(exp(-0.5*(popd(:)**0.5)))*a_nd(:)*popd(:)/10000.0 !unit: ignitions d^{-1}km^{-1}
    ELSEWHERE
      human_ign(:)=0.0
    ENDWHERE

    END SUBROUTINE human_ign_fct


!! ================================================================================================================================
!! SUBROUTINE     : human_ign_fct
!!                                                                              
!>\BRIEF        Calculation human ignitions
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  Nonw
!! 
!! REFERENCE(S)   : 
!! Li et al. (2011) A process-based fire parameterization of intermediate 
!! complexity in a Dynamic Global Vegetation Model.
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    SUBROUTINE human_suppression_func(npts,popd,human_suppression)

     implicit none

     !Domain size
     INTEGER, INTENT(in)                              :: npts

     !INPUT Variables
     REAL(r_std), DIMENSION(npts), INTENT(in)         ::  popd  !individuals km^-2
     !OUTPUT
     REAL(r_std), DIMENSION(npts), INTENT(out)        ::  human_suppression

     WHERE(popd(:).ge.min_stomate)
       ! source: Li et al. (2011) A process-based fire parameterization of intermediate complexity in a Dynamic Global
       ! Vegetation Model. Page 2766
       human_suppression(:) = 0.99-0.98*exp(-0.025*popd(:)) 
     ELSEWHERE
       human_suppression(:)=0.0
     ENDWHERE

    END SUBROUTINE human_suppression_func       


!! ================================================================================================================================
!! SUBROUTINE     : rate_of_spread
!!                                                                              
!>\BRIEF        Calculation fire spread rate, This is the application of Eq.(9) in
!!     Thonicke et al. (2010)
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  Nonw
!! 
!! REFERENCE(S)   : 
!! - K.Thonicke, A.Spessa, I.C. Prentice, S.P.Harrison, L.Dong, and
!! C.Carmona-Moreno, 2010: The influence of vegetation, fire spread and fire
!! behaviour on biomass burning and trace gas emissions: results from a
!! process-based model. Biogeosciences, 7, 1991-2011.
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    SUBROUTINE rate_of_spread(npts,U_front,wind_forward,sigma,  &
                  dfm,me_litterfuel,H, char_dens_fuel_ave,      &
                  char_net_fuel, moist_extinction,gamma,wetness)

    implicit none
    
    ! Domain size
    INTEGER, INTENT(in)                             :: npts

    ! INPUT Variables
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: wind_forward 
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: char_dens_fuel_ave
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: sigma
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: char_net_fuel
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: dfm 
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: me_litterfuel
    REAL(r_std), DIMENSION(npts), INTENT(in)         :: moist_extinction
    REAL(r_std), INTENT(in)                          :: H           !! heat content of the fuel (18000kJ/kg)

    ! OUTPUT Variables       
    REAL(r_std), DIMENSION(npts), INTENT(out)        :: U_front     !! Forward fire spread rate (m min^{-1}), ROS_{f,surface} in Thonicke et al. 2010 
    REAL(r_std), DIMENSION(npts), INTENT(out)        :: gamma
    REAL(r_std), DIMENSION(npts), INTENT(out)        :: wetness     !! wetness, unitless, the ratio of dead fuel moisture to the moisture of extinction

    ! LOCAL Variables
    REAL(r_std), DIMENSION(npts)                     :: dummy
    REAL(r_std), DIMENSION(npts)                     :: dummy2
    REAL(r_std), DIMENSION(npts)                     :: beta        !! Packing ratio, unitless, the ratio of fuel_bulk_density(kg m^{-3}) against 
                                                                    !! oven-dry particle_density (513kg m^{-3}).
    REAL(r_std), DIMENSION(npts)                     :: beta_op
    REAL(r_std), DIMENSION(npts)                     :: q_ig
    REAL(r_std), DIMENSION(npts)                     :: eps
    REAL(r_std), DIMENSION(npts)                     :: b
    REAL(r_std), DIMENSION(npts)                     :: c
    REAL(r_std), DIMENSION(npts)                     :: e
    REAL(r_std), DIMENSION(npts)                     :: phi_wind
    REAL(r_std), DIMENSION(npts)                     :: xi          !!propagating flux ratio used in the calculation of ROS_{f,surface}
    REAL(r_std), DIMENSION(npts)                     :: a
    REAL(r_std), DIMENSION(npts)                     :: gamma_max
    REAL(r_std), DIMENSION(npts)                     :: gamma_aptr  !!gamma'
    REAL(r_std), DIMENSION(npts)                     :: ir
    REAL(r_std), DIMENSION(npts)                     :: bet
    REAL(r_std), DIMENSION(npts)                     :: moist_damp


    ! mineral dampening coefficient
    REAL(r_std), parameter :: MINER_DAMP=0.41739 !Thonicke 2010 Table A1 Row 7
    REAL(r_std), parameter :: part_dens=513.0 !Thonicke 2010 Table A1 Row 5
    INTEGER :: m



    ! Initialise
    xi(:)=0.0
    gamma_aptr=0.0
    dummy2(:)=0.0

    beta(:) = char_dens_fuel_ave(:) / part_dens !packing ratio
    beta_op(:) = 0.200395*(sigma(:)**(-0.8189)) !Eq.(A6)
    bet(:) = beta(:) / beta_op(:)  !prepare for Eq.(A5)

    ! heat of pre-ignition
    q_ig(:)=581.0+2594.0*dfm(:)
    ! effective heating number
    eps(:)=exp(-4.528/sigma(:))
    ! influence of wind speed
    b(:)=0.15988*(sigma(:)**0.54)
    c(:)=7.47*(exp(-0.8711*(sigma(:)**0.55)))
    e(:)=0.7515*(exp(-0.01094*sigma(:)))

    WHERE(bet(:).GT.0.00001.AND.wind_forward(:).GT.min_stomate)
      phi_wind(:)=c(:)*(wind_forward(:)**b(:))*(bet(:)**(-e(:)))  !Eq.(A5)
    ENDWHERE

    WHERE(bet(:).LE.0.00001)
      phi_wind(:)=c(:)*(wind_forward(:)**b(:))*0.00001  !??UNIMPORTANT not sure this will pose a problem 
                                                        !but when bet --> 0+, bet**(-e) can be very big, which 
                                                        !is on the contrary with the simplied form.
    ENDWHERE

    WHERE(wind_forward(:).LE.min_stomate)
      phi_wind(:)=zero
    ENDWHERE

    ! propagating flux
    WHERE (sigma(:).le.0.00001) 
      xi(:)=0.0
    ELSEWHERE
      xi(:) = (exp((0.792 + 3.7597 * (sigma(:)**0.5)) * &  !Eq.(A2)
         (beta(:) + 0.1))) / (192 + 7.9095 * sigma(:))
    ENDWHERE

    ! reaction intensity
    a(:)=8.9033*(sigma(:)**(-0.7913))
    WHERE (a(:).le.0.00001 .OR. bet(:).le.0.00001)
       dummy(:)=0.0
    ELSEWHERE
       dummy(:)=exp(a(:)*(1.0-bet(:)))
    ENDWHERE
      gamma_max(:)=1.0/(0.0591+2.926*(sigma(:)**(-1.5)))  !Thonicke 2010 Table A1 Row 2

    WHERE(bet(:).GT.0.0001)
      gamma_aptr(:)=gamma_max(:)*(bet(:)**a(:))*dummy(:)
    ELSEWHERE
      gamma_aptr(:)=gamma_max(:)*(0.0001)*dummy(:)
    ENDWHERE
    
    ! wn/me in Eq. row 6 in Table A1 in Thonicke 2010.
    WHERE (moist_extinction(:).gt.min_stomate)
      wetness(:)=dfm(:)/moist_extinction(:)
    ELSEWHERE
      wetness(:)=0.0
    ENDWHERE

    WHERE (wetness(:).gt.min_stomate)
      moist_damp(:)=max(0.0,(1.0-(2.59*wetness(:))+ &
        (5.11*(wetness(:)**2.0))-(3.52*(wetness(:)**3.0))))
    ELSEWHERE
      moist_damp(:)=0.0
    ENDWHERE

    ir(:)=gamma_aptr(:) * char_net_fuel(:) * H * moist_damp(:) * MINER_DAMP !Eq.(A1)

    !for use in calculating tau_l for postfire mortality.
    gamma(:)=gamma_aptr(:)*moist_damp(:)*MINER_DAMP
      
    ! reaction intensity end

    WHERE ((char_dens_fuel_ave(:).le.min_stomate).or. &
           (eps(:).le.0.0).or.(q_ig(:).le.min_stomate)) 
      U_front(:)=0.0
    ELSEWHERE
      U_front(:)=(ir(:) * xi(:) * (1.0 + phi_wind(:))) / &
          (char_dens_fuel_ave(:) * eps(:) * q_ig(:))
    ENDWHERE

    END SUBROUTINE rate_of_spread

!! ================================================================================================================================
!! SUBROUTINE     : fuel_consumption
!!                                                                              
!>\BRIEF        Calculation fire fuel consumption
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  Nonw
!! 
!! REFERENCE(S)   : 
!! - K.Thonicke, A.Spessa, I.C. Prentice, S.P.Harrison, L.Dong, and
!! C.Carmona-Moreno, 2010: The influence of vegetation, fire spread and fire
!! behaviour on biomass burning and trace gas emissions: results from a
!! process-based model. Biogeosciences, 7, 1991-2011.
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    !fuel_consum--total fuel consumed excluding the 1000hr fuel; fc_1hr/10hr/100hr--fuel consumed; in the unit of g biomass/m**2 (C/0.45); fc_1000hr (unit gC/m**2) 
    !fuel_consum: unit g mass/m**2 weighted by veget_max among PFTs, includes only 1hr/10hr/100hr dead fuel, used for calculation of d_i_surface 
    SUBROUTINE fuel_consumption(npts,fuel_consum,fire_frac,           &
        fuel_1hr,fuel_10hr,fuel_100hr,fuel_1000hr,livegrass,biomass,  &
        fuel_1hr_total,dfm,dfm_1hr,wetness,MINER_TOT,fc_1hr,fc_lg,    &
        fc_lf,fc_10hr,fc_100hr,fc_1000hr,cf_ave,                      &
        moist_extinction,dfm_lg,veget_max,                            &
        ratio_flag,cf_coarse,cf_fine,cf_lg,cf_1hr,cf_10hr,cf_100hr,cf_1000hr)


    implicit none

    integer :: d,j,k

    ! Domain size
    INTEGER, INTENT(in)                             :: npts

    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: livegrass
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: fire_frac
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: dfm
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: dfm_1hr          !! Daily fuel moisture for 1hr fuel, unitless, (0,1)
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: dfm_lg           !! Daily live grass fuel moisture, unitless, (0,1)
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: wetness
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: moist_extinction
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: fuel_1hr_total
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: cf_coarse        !! Externally forced consumption fraction for coarse fuel, used
                                                                                    !! on 1000hr fuel.
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: cf_fine          !! Externally forced consumption fraction for fine fuel, applied
                                                                                    !! on live grass fuel, 1hr/10hr/100hr fuel.
    REAL(r_std), DIMENSION(npts), INTENT(in)                    :: ratio_flag       !! gridded value used to indicate where the external consumption
                                                                                    !! fractions will be applied in the model, when ratio_flag is larger
                                                                                    !! than 0, the external consumption fractions will be used.

    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)          :: fuel_1hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)          :: fuel_10hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)          :: fuel_100hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)          :: fuel_1000hr
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(in)         :: biomass
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)                :: veget_max
    REAL(r_std), INTENT(in)                                     :: MINER_TOT

    REAL(r_std), DIMENSION(npts), INTENT(inout)                 :: fuel_consum      !! total dead ground fuel consumed excluding the 1000hr fuel, as 
                                                                                    !! weighted by presence of PFTs (g dry mass per m2)
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(out)         :: fc_1hr           !! fc_*hr:fuel consumed, in the unit of biomass (C/0.45).
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(out)         :: fc_10hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(out)         :: fc_100hr
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(out)         :: fc_1000hr
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)               :: fc_lg            !! livegrass leaf fuel cosumed (g C m^{-2})
    REAL(r_std), DIMENSION(npts,nvm), INTENT(out)               :: fc_lf            !! livegrass fruit fuel consumed (gC m^{-2})
    REAL(r_std), DIMENSION(npts), INTENT(out)                   :: cf_ave           !! the average consumption fraction of 1hr/10hr/100hr fuel, unitless, (0,1)

    !LOCAL VARIABLES
    REAL(r_std), DIMENSION(npts)                                :: dfm_lg_d1hr      !! daily fuel moisture for 1hr dead fuel and livegrass moisture combined
    REAL(r_std), DIMENSION(npts), INTENT(out)                   :: cf_1hr           !! cf_*hr:consumption fraction, unitless, (0,1)
    REAL(r_std), DIMENSION(npts), INTENT(out)                   :: cf_lg
    REAL(r_std), DIMENSION(npts), INTENT(out)                   :: cf_10hr
    REAL(r_std), DIMENSION(npts), INTENT(out)                   :: cf_100hr
    REAL(r_std), DIMENSION(npts), INTENT(out)                   :: cf_1000hr
    REAL(r_std), DIMENSION(npts)                                :: wetness_lg
    REAL(r_std), DIMENSION(npts)                                :: wetness_1hr      !! the wetness of 1hr-fuel in comparison with moisture of extinction 
                                                                                    !! is influenced by live grass fuel moisture
    REAL(r_std), parameter :: me_livegrass=0.2                                      !! moisture of extinction for live grass (unitless) 


    ! Initialise
    fuel_consum(:)=0.0
    dfm_lg_d1hr(:)=0.0
    wetness_lg(:)=0.0
    wetness_1hr(:)=0.0

    !dfm_lg is the wlg in Eq.(B2) and Eq.(B3)
    WHERE (fuel_1hr_total(:).gt.min_stomate)
      dfm_lg_d1hr(:)=dfm_1hr(:)+(dfm_lg(:)*(livegrass(:)/fuel_1hr_total(:))) !Eq.(B3)
    ENDWHERE
 
    WHERE (moist_extinction(:).gt.min_stomate)
      wetness_lg(:)=dfm_lg(:)/me_livegrass
      wetness_1hr(:)=dfm_lg_d1hr(:)/moist_extinction(:)
    ELSEWHERE
      wetness_lg(:)=1.0
      wetness_1hr(:)=1.0
    ENDWHERE

    !Note that the fuel consumption fractions depend on only the moisture state and
    !thus are PFT independent.
    DO j=1,nvm
      IF (natural(j)) THEN
        ! livegrass consumption
        IF (.not.is_tree(j)) THEN
          fc_lg(:,j)=0.0
          fc_lf(:,j)=0.0
          WHERE (wetness_lg(:).le.0.18)
             cf_lg(:)=1.0
          ELSEWHERE (wetness_lg(:).gt.0.18.and.wetness_lg(:).le.0.73)
             cf_lg(:)=1.11-0.62*wetness_lg(:)
          ELSEWHERE (wetness_lg(:).gt.0.73.and.wetness_lg(:).le.1.0)
             cf_lg(:)=2.45-2.45*wetness_lg(:)
          ELSEWHERE (wetness_lg(:).gt.1.0)
             cf_lg(:)=0.0
          ENDWHERE
    
          WHERE (ratio_flag(:).gt.0.0)
            cf_lg(:)=cf_fine(:)
          ENDWHERE
    
          fc_lg(:,j)=cf_lg(:) *(1.0-MINER_TOT)* biomass(:,j,ileaf)*fire_frac(:)
          fc_lf(:,j)=cf_lg(:) *(1.0-MINER_TOT)* biomass(:,j,ifruit)*fire_frac(:)
        ENDIF !not a tree
    
        !dead fuel consumption
        DO k=1,nlitt
          !1hr fuel consumption
          fc_1hr(:,j,k)=0.0
          WHERE (fuel_1hr(:,j,k).gt.0.0)

            WHERE (wetness_1hr(:).le.0.18)
               cf_1hr(:)=1.0  
            ELSEWHERE (wetness_1hr(:).gt.0.18.and.wetness_1hr(:).le.0.73)
               cf_1hr(:)=1.11-0.62*wetness_1hr(:)
            ELSEWHERE (wetness_1hr(:).gt.0.73.and.wetness_1hr(:).le.1.0)
               cf_1hr(:)=2.45-2.45*wetness_1hr(:)
            ELSEWHERE (wetness_1hr(:).gt.1.0)
               cf_1hr(:)=0.0
            ENDWHERE

            WHERE (cf_1hr(:) .gt. 0.9)
              cf_1hr(:) = 0.9
            ENDWHERE
    
            WHERE (ratio_flag(:).gt.0.0)
              cf_1hr(:)=cf_fine(:)
            ENDWHERE
    
               fc_1hr(:,j,k)=cf_1hr(:)*(1.0-MINER_TOT)*(fuel_1hr(:,j,k)/0.45)*fire_frac(:)
          ENDWHERE
            
    
          !10hr fuel consumption
          fc_10hr(:,j,k)=0.0
          WHERE (fuel_10hr(:,j,k).gt.0.0)
            WHERE (wetness(:).le.0.12)
              cf_10hr(:)=1.0
            ELSEWHERE (wetness(:).gt.0.12.and.wetness(:).le.0.51)
              cf_10hr(:)=1.09-0.72*wetness(:)
            ELSEWHERE (wetness(:).gt.0.51.and.wetness(:).le.1.0)
              cf_10hr(:)=1.47-1.47*wetness(:)
            ELSEWHERE (wetness(:).gt.1.0)
              cf_10hr(:)=0.0
            ENDWHERE
    
            WHERE (cf_10hr(:) .gt. 0.9)
              cf_10hr(:) = 0.9
            ENDWHERE

            WHERE (ratio_flag(:).gt.0.0)
              cf_10hr(:)=cf_fine(:)
            ENDWHERE
    
            fc_10hr(:,j,k)=cf_10hr(:)* (1.0-MINER_TOT)*(fuel_10hr(:,j,k)/0.45)*fire_frac(:)
          ENDWHERE 
     
    
          !100hr fuel consumption
          fc_100hr(:,j,k)=0.0
          WHERE (fuel_100hr(:,j,k).gt.0.0)
            WHERE (wetness(:).le.0.38)
              cf_100hr(:)=0.98-0.85*wetness(:)
            ELSEWHERE (wetness(:).gt.0.38 .and. wetness(:).le.1.0)
              cf_100hr(:)=1.06-1.06*wetness(:)
            ELSEWHERE (wetness(:).gt.1.0)
              cf_100hr(:)=0.0
            ENDWHERE

            WHERE (cf_100hr(:) .gt. fire_max_cf_100hr(j))
              cf_100hr(:) = fire_max_cf_100hr(j)
            ENDWHERE
    
            WHERE (ratio_flag(:).gt.0.0)
              cf_100hr(:)=cf_fine(:)
            ENDWHERE
    
            fc_100hr(:,j,k)=cf_100hr(:)*(1.0-MINER_TOT)* (fuel_100hr(:,j,k)/0.45)*fire_frac(:)
          ENDWHERE
    
          !calculating average fuel consumption fraction
          !the 1000hr fuel consumption fraction is not included in the average consumption fraction.
          cf_ave(:)=(cf_1hr(:)+cf_10hr(:)+cf_100hr(:))/3.0
    
          !1000hr fuel consumption, not influencing rate of spread or surface fire intensity (Rothermel 1972)
          fc_1000hr(:,j,k)=0.0
          WHERE (fuel_1000hr(:,j,k).gt.0.0 .and. wetness(:).le.1.0)
            cf_1000hr(:) = -0.8*wetness(:)+0.8

            WHERE (cf_1000hr(:) .gt. fire_max_cf_1000hr(j))
              cf_1000hr(:) = fire_max_cf_1000hr(j)
            ENDWHERE

            WHERE (ratio_flag(:).gt.0.0)
              cf_1000hr(:)=cf_coarse(:)
            ENDWHERE
            fc_1000hr(:,j,k)=cf_1000hr(:) * (1.0-MINER_TOT)* fuel_1000hr(:,j,k)*fire_frac(:)
          ENDWHERE
    
          !total fuel consumption (without 1000hr fuel) in g biomass per m**2
          !Used to calculate fire frontline intensity
          fuel_consum(:)=fuel_consum(:)+(fc_1hr(:,j,k)+fc_10hr(:,j,k)+fc_100hr(:,j,k))*veget_max(:,j)
        ENDDO !nlitt
      ENDIF !natural 
    ENDDO !pft

    ! Only for debug purposes
    !CALL histwrite_p (hist_id_stomate, 'cf_fine', itime, &
    !                cf_fine(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'cf_coarse', itime, &
    !                cf_coarse(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'dfm_lg_d1hr', itime, &
    !                dfm_lg_d1hr(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'dfm_lg', itime, &
    !                dfm_lg(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'dfm', itime, &
    !                dfm(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'wetness', itime, &
    !                wetness(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'wetness_lg', itime, &
    !                wetness_lg(:), npts, hori_index)
    !CALL histwrite_p (hist_id_stomate, 'wetness_1hr', itime, &
    !                wetness_1hr(:), npts, hori_index)

    END SUBROUTINE fuel_consumption


!! ================================================================================================================================
!! SUBROUTINE     : deforestation_fire
!!                                                                              
!>\BRIEF       Simulate deforestation processes from land cover change.
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  Nonw
!! 
!! REFERENCE(S)   : 
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    SUBROUTINE deforestation_fire(npts,d_fdi,lcc,biomass,fuel_1hr,&
        fuel_10hr,fuel_100hr,fuel_1000hr,bafrac_deforest,&
        emideforest_fuel_1hr,emideforest_fuel_10hr,emideforest_fuel_100hr,&
        emideforest_fuel_1000hr,emideforest_biomass)

    IMPLICIT NONE

    !! Input variables
    ! Domain size
    INTEGER, INTENT(in)                                     :: npts
    REAL(r_std), DIMENSION(npts)                            :: d_fdi             !! daily fire danger index,climatic fire risk, unitless (0,1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: lcc               !! gross forest cover loss
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(in)     :: biomass
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)      :: fuel_1hr          !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)      :: fuel_10hr         !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)      :: fuel_100hr        !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)      :: fuel_1000hr       !! [gC^{m-2}]
     
    !! Output variables
    REAL(r_std), DIMENSION(npts,nvm),INTENT(inout)          :: bafrac_deforest         !! Deforestation fire burned fraction,unitless
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_1hr    !! fuel consumption for 1hr fuel (gCm^{-2})
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_10hr   !! 
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_100hr  !! 
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_1000hr !! 
    REAL(r_std), DIMENSION(npts,nvm,nparts),INTENT(inout)   :: emideforest_biomass     !! live biomass emissions from deforestation fires [gCm^{-2}]
    
    !! Local variables
    REAL(r_std), DIMENSION(npts)                            :: deforest_factor         !!Factor to scale the deforestation fire fraction, unitless
    REAL(r_std), DIMENSION(npts)                            :: vartemp
    REAL(r_std), DIMENSION(npts)                            :: cc_litter               !! Fire consumption fraction for litter, leaf,fruit and reserve biomass
    REAL(r_std), DIMENSION(npts)                            :: cc_wood                 !! Fire consumption fraction for heart and sapwood
    INTEGER(i_std)                                          :: ilit,j                  !! Indices (unitless)


    deforest_factor(:) = 0.017
    cc_litter(:) = 0.7 + 0.3*d_fdi(:)
    cc_wood(:) = 0.3 + d_fdi(:)*0.3  

    DO j=2,nvm
      IF (is_tree(j)) THEN
        WHERE (lcc(:,j) > min_stomate)
          !The equation of y=0.58*x-0.006 is derived by regressing GFED3 reported
          !deforestation fire burned fraction to the gross forest loss by Hansen 2010
          !for the closed canopy Amazonian forest
          vartemp(:) = MAX(0.0001,lcc(:,j)*0.58-0.006)
          bafrac_deforest(:,j) = deforest_factor(:) * vartemp(:) * d_fdi(:) 

          !We calculate fire emissions from litter and biomass
          emideforest_fuel_1hr(:,j,imetabolic) = cc_litter(:) * bafrac_deforest(:,j) * fuel_1hr(:,j,imetabolic)
          emideforest_fuel_1hr(:,j,istructural) = cc_litter(:) * bafrac_deforest(:,j) * fuel_1hr(:,j,istructural)
          emideforest_fuel_10hr(:,j,imetabolic) = cc_litter(:) * bafrac_deforest(:,j) * fuel_10hr(:,j,imetabolic)
          emideforest_fuel_10hr(:,j,istructural) = cc_litter(:) * bafrac_deforest(:,j) * fuel_10hr(:,j,istructural)
          emideforest_fuel_100hr(:,j,imetabolic) = cc_litter(:) * bafrac_deforest(:,j) * fuel_100hr(:,j,imetabolic)
          emideforest_fuel_100hr(:,j,istructural) = cc_litter(:) * bafrac_deforest(:,j) * fuel_100hr(:,j,istructural)
          emideforest_fuel_1000hr(:,j,imetabolic) = cc_litter(:) * bafrac_deforest(:,j) * fuel_1000hr(:,j,imetabolic)
          emideforest_fuel_1000hr(:,j,istructural) = cc_litter(:) * bafrac_deforest(:,j) * fuel_1000hr(:,j,istructural)

          emideforest_biomass(:,j,icarbres) = cc_litter(:) * bafrac_deforest(:,j) * biomass(:,j,icarbres)
          emideforest_biomass(:,j,ifruit) = cc_litter(:) * bafrac_deforest(:,j) * biomass(:,j,ifruit)
          emideforest_biomass(:,j,ileaf) = cc_litter(:) * bafrac_deforest(:,j) * biomass(:,j,ileaf)
          emideforest_biomass(:,j,iheartabove) = cc_wood(:) * bafrac_deforest(:,j) * biomass(:,j,iheartabove)
          emideforest_biomass(:,j,isapabove) = cc_wood(:) * bafrac_deforest(:,j) * biomass(:,j,isapabove)
        ENDWHERE 
      END IF
    END DO
   
   END SUBROUTINE deforestation_fire
   
!! ================================================================================================================================
!! SUBROUTINE     : deforestation_fire_proxy
!!                                                                              
!>\BRIEF       Simulate deforestation processes from land cover change.
!!                                                                              
!! DESCRIPTION   : None                            
!!                                                                              
!! RECENT CHANGE(S): None                                                       
!!                                                                              
!! RETURN VALUE :  Nonw
!! 
!! REFERENCE(S)   : 
!!                                                                              
!! FLOWCHART   : None                                                           
!! \n                                                                           
!_ ================================================================================================================================

    SUBROUTINE deforestation_fire_proxy(npts,d_fdi,lcc,                     &
        deforest_biomass_remain,                     &
        def_fuel_1hr_remain,def_fuel_10hr_remain,                           &
        def_fuel_100hr_remain,def_fuel_1000hr_remain,                       &
        bafrac_deforest, emideforest_fuel_1hr,emideforest_fuel_10hr,        &
        emideforest_fuel_100hr, emideforest_fuel_1000hr,emideforest_biomass)


    IMPLICIT NONE

    !! Input variables
    ! Domain size
    INTEGER, INTENT(in)                                     :: npts
    REAL(r_std), DIMENSION(npts)                            :: d_fdi             !! daily fire danger index,climatic fire risk, unitless (0,1)
    REAL(r_std), DIMENSION(npts,nvm), INTENT(in)            :: lcc               !! gross forest cover loss
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)        :: def_fuel_1hr_remain     !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)        :: def_fuel_10hr_remain    !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)        :: def_fuel_100hr_remain   !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nlitt), INTENT(in)        :: def_fuel_1000hr_remain  !! [gC^{m-2}]
    REAL(r_std), DIMENSION(npts,nvm,nparts), INTENT(in)      :: deforest_biomass_remain 
     
    !! Output variables
    REAL(r_std), DIMENSION(npts,nvm),INTENT(inout)          :: bafrac_deforest         !! Deforestation fire burned fraction,unitless
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_1hr    !! fuel consumption for 1hr fuel (gCm^{-2})
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_10hr   !! 
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_100hr  !! 
    REAL(r_std), DIMENSION(npts,nvm,nlitt),INTENT(inout)    :: emideforest_fuel_1000hr !! 
    REAL(r_std), DIMENSION(npts,nvm,nparts),INTENT(inout)   :: emideforest_biomass     !! live biomass emissions from deforestation fires [gCm^{-2}]
    
    !! Local variables
    REAL(r_std), DIMENSION(npts,nvm)                        :: bafrac_net              !! Deforestation fire burned fraction,unitless
    REAL(r_std), DIMENSION(npts)                            :: deforest_factor         !!Factor to scale the deforestation fire fraction, unitless
    REAL(r_std), DIMENSION(npts)                            :: vartemp
    REAL(r_std), DIMENSION(npts)                            :: cc_litter               !! Fire consumption fraction for litter, leaf,fruit and reserve biomass
    REAL(r_std), DIMENSION(npts)                            :: cc_wood                 !! Fire consumption fraction for heart and sapwood
    INTEGER(i_std)                                          :: ilit,j                  !! Indices (unitless)


    ! the 7.2 is tested. The test result could be found at 
    ! /home/cyue/Documents/ORCHIDEE/TEST_MICTv6bis/Deforestation_test/Deforestation_fire_annual_2001_2010_55e0e3e_T2.nc
    ! with the notebook be found at: /home/cyue/Documents/ORCHIDEE/TEST_MICTv6bis/Test_tag_MICTv6bisT2
    ! the readme file is at: /home/orchidee01/ychao/MICTv6/bin/readme.txt
    deforest_factor(:) = 0.017*7.2
    cc_litter(:) = 0.7 + 0.3*d_fdi(:)
    cc_wood(:) = 0.3 + d_fdi(:)*0.3  

    DO j=2,nvm
      IF (is_tree(j)) THEN
        WHERE (lcc(:,j) > min_stomate)
          !The equation of y=0.58*x-0.006 is derived by regressing GFED3 reported
          !deforestation fire burned fraction to the gross forest loss by Hansen 2010
          !for the closed canopy Amazonian forest
          vartemp(:) = MAX(0.0001,lcc(:,j)*0.58-0.006)
          bafrac_deforest(:,j) = deforest_factor(:) * vartemp(:) * d_fdi(:) 
          bafrac_net(:,j)=bafrac_deforest(:,j)/lcc(:,j)

          !We calculate fire emissions from litter and biomass
          !note here because cc_litter*bafrac_net < 1, the fuel will never be 
          !exhausted.
          emideforest_fuel_1hr(:,j,imetabolic) = cc_litter(:) * bafrac_net(:,j) * def_fuel_1hr_remain(:,j,imetabolic)
          emideforest_fuel_1hr(:,j,istructural) = cc_litter(:) * bafrac_net(:,j) * def_fuel_1hr_remain(:,j,istructural)
          emideforest_fuel_10hr(:,j,imetabolic) = cc_litter(:) * bafrac_net(:,j) * def_fuel_10hr_remain(:,j,imetabolic)
          emideforest_fuel_10hr(:,j,istructural) = cc_litter(:) * bafrac_net(:,j) * def_fuel_10hr_remain(:,j,istructural)
          emideforest_fuel_100hr(:,j,imetabolic) = cc_litter(:) * bafrac_net(:,j) * def_fuel_100hr_remain(:,j,imetabolic)
          emideforest_fuel_100hr(:,j,istructural) = cc_litter(:) * bafrac_net(:,j) * def_fuel_100hr_remain(:,j,istructural)
          emideforest_fuel_1000hr(:,j,imetabolic) = cc_litter(:) * bafrac_net(:,j) * def_fuel_1000hr_remain(:,j,imetabolic)
          emideforest_fuel_1000hr(:,j,istructural) = cc_litter(:) * bafrac_net(:,j) * def_fuel_1000hr_remain(:,j,istructural)

          emideforest_biomass(:,j,icarbres) = cc_litter(:) * bafrac_net(:,j) * deforest_biomass_remain(:,j,icarbres)
          emideforest_biomass(:,j,ifruit) = cc_litter(:) * bafrac_net(:,j) * deforest_biomass_remain(:,j,ifruit)
          emideforest_biomass(:,j,ileaf) = cc_litter(:) * bafrac_net(:,j) * deforest_biomass_remain(:,j,ileaf)
          emideforest_biomass(:,j,iheartabove) = cc_wood(:) * bafrac_net(:,j) * deforest_biomass_remain(:,j,iheartabove)
          emideforest_biomass(:,j,isapabove) = cc_wood(:) * bafrac_net(:,j) * deforest_biomass_remain(:,j,isapabove)
        ENDWHERE 
      END IF
    END DO
   
   END SUBROUTINE deforestation_fire_proxy
END MODULE lpj_spitfire
