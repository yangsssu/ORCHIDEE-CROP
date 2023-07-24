










! =================================================================================================================================
! MODULE       : routing
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       This module routes the water over the continents into the oceans and computes the water
!!             stored in floodplains or taken for irrigation.
!!
!!\n DESCRIPTION: None
!!
!! RECENT CHANGE(S): None
!!
!! REFERENCE(S)	:
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_sechiba/routing.f90 $
!! $Date: 2016-04-26 13:28:48 +0200 (Tue, 26 Apr 2016) $
!! $Revision: 3386 $
!! \n
!_ ================================================================================================================================

! 
!
! Histoire Salee
!---------------
! La douce riviere
! Sortant de son lit
! S'est jetee ma chere
! dans les bras mais oui
! du beau fleuve
!
! L'eau coule sous les ponts
! Et puis les flots s'emeuvent
! - N'etes vous pas au courant ?
! Il parait que la riviere 
! Va devenir mer
!                       Roland Bacri
!


MODULE routing

  USE ioipsl   
  USE xios_orchidee
  USE ioipsl_para 
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE sechiba_io
  USE interpol_help
  USE grid
  USE mod_orchidee_para


  IMPLICIT NONE
  PRIVATE
  PUBLIC :: routing_main, routing_initialize, routing_finalize, routing_clear

  LOGICAL, SAVE                                              :: l_first_routing=.TRUE.      !! Logical to initialize the routine.
!$OMP THREADPRIVATE(l_first_routing)
!! PARAMETERS
  INTEGER(i_std), PARAMETER                                  :: nbasmax=5                   !! The maximum number of basins we wish to have per grid box (truncation of the model) (unitless)
  INTEGER(i_std), SAVE                                       :: nbvmax                      !! The maximum number of basins we can handle at any time during the generation of the maps (unitless)
!$OMP THREADPRIVATE(nbvmax)
  REAL(r_std), PARAMETER                                     :: slow_tcst_cwrr = 25.0        !! Property of the slow reservoir, when CWRR hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: fast_tcst_cwrr = 3.0        !! Property of the fast reservoir, when CWRR hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: stream_tcst_cwrr = 0.24     !! Property of the stream reservoir, when CWRR hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: flood_tcst_cwrr = 4.0       !! Property of the floodplains reservoir, when CWRR hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: swamp_cst_cwrr = 0.2        !! Fraction of the river transport that flows to the swamps, when CWRR hydrology is activated (unitless;0-1)
  !
  REAL(r_std), PARAMETER                                     :: slow_tcst_chois = 25.0      !! Property of the slow reservoir, when Choisnel hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: fast_tcst_chois = 3.0       !! Property of the fast reservoir, when Choisnel hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: stream_tcst_chois = 0.24    !! Property of the stream reservoir, when Choisnel hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: flood_tcst_chois = 4.0      !! Property of the floodplains reservoir, when Choisnel hydrology is activated (day/m)
  REAL(r_std), PARAMETER                                     :: swamp_cst_chois = 0.2       !! Fraction of the river transport that flows to the swamps, when Choisnel hydrology is activated (unitless;0-1)
  !
  REAL(r_std), SAVE                                          :: fast_tcst                   !! Property of the fast reservoir, (day/m)
!$OMP THREADPRIVATE(fast_tcst)
  REAL(r_std), SAVE                                          :: slow_tcst                   !! Property of the slow reservoir, (day/m)
!$OMP THREADPRIVATE(slow_tcst)
  REAL(r_std), SAVE                                          :: stream_tcst                 !! Property of the stream reservoir, (day/m)
!$OMP THREADPRIVATE(stream_tcst)
  REAL(r_std), SAVE                                          :: flood_tcst                  !! Property of the floodplains reservoir, (day/m)
!$OMP THREADPRIVATE(flood_tcst)
  REAL(r_std), SAVE                                          :: swamp_cst                   !! Fraction of the river transport that flows to the swamps (unitless;0-1)
!$OMP THREADPRIVATE(swamp_cst)
  !
  !  Relation between volume and fraction of floodplains
  !
  REAL(r_std), SAVE                                          :: beta = 2.0                  !! Parameter to fix the shape of the floodplain (>1 for convex edges, <1 for concave edges) (unitless)
!$OMP THREADPRIVATE(beta)
  REAL(r_std), SAVE                                          :: betap = 0.5                 !! Ratio of the basin surface intercepted by ponds and the maximum surface of ponds (unitless;0-1)
!$OMP THREADPRIVATE(betap)
  REAL(r_std), SAVE                                          :: floodcri = 2000.0           !! Potential height for which all the basin is flooded (mm)
!$OMP THREADPRIVATE(floodcri)
  !
  !  Relation between maximum surface of ponds and basin surface, and drainage (mm/j) to the slow_res
  !
  REAL(r_std), PARAMETER                                     :: pond_bas = 50.0             !! [DISPENSABLE] - not used
  REAL(r_std), SAVE                                          :: pondcri = 2000.0            !! Potential height for which all the basin is a pond (mm)
!$OMP THREADPRIVATE(pondcri)
  !
  REAL(r_std), PARAMETER                                     :: maxevap_lake = 7.5/86400.   !! Maximum evaporation rate from lakes (kg/m^2/s)
  !
  REAL(r_std),SAVE                                           :: dt_routing                  !! Routing time step (s)
!$OMP THREADPRIVATE(dt_routing)
  !
  INTEGER(i_std), SAVE                                       :: diagunit = 87               !! Diagnostic file unit (unitless)
!$OMP THREADPRIVATE(diagunit)
  !
  ! Logicals to control model configuration
  !
  LOGICAL, SAVE                                              :: dofloodinfilt = .FALSE.     !! Logical to choose if floodplains infiltration is activated or not (true/false)
!$OMP THREADPRIVATE(dofloodinfilt)
  LOGICAL, SAVE                                              :: doswamps = .FALSE.          !! Logical to choose if swamps are activated or not (true/false)
!$OMP THREADPRIVATE(doswamps)
  LOGICAL, SAVE                                              :: doponds = .FALSE.           !! Logical to choose if ponds are activated or not (true/false)
!$OMP THREADPRIVATE(doponds)
  !
  ! The variables describing the basins and their routing, need to be in the restart file.
  !
  INTEGER(i_std), SAVE                                       :: num_largest                 !! Number of largest river basins which should be treated as independently as rivers
                                                                                            !! (not flow into ocean as diffusion coastal flow) (unitless)
!$OMP THREADPRIVATE(num_largest)
  REAL(r_std), SAVE                                          :: time_counter                !! Time counter (s)
!$OMP THREADPRIVATE(time_counter)
  REAL(r_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: routing_area_loc            !! Surface of basin (m^2)
!$OMP THREADPRIVATE(routing_area_loc)
  REAL(r_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: topo_resid_loc              !! Topographic index of the retention time (m)
!$OMP THREADPRIVATE(topo_resid_loc)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: route_togrid_loc            !! Grid into which the basin flows (unitless)
!$OMP THREADPRIVATE(route_togrid_loc)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: route_tobasin_loc           !! Basin in to which the water goes (unitless)
!$OMP THREADPRIVATE(route_tobasin_loc)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: route_nbintobas_loc         !! Number of basin into current one (unitless)
!$OMP THREADPRIVATE(route_nbintobas_loc)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: global_basinid_loc          !! ID of basin (unitless)
!$OMP THREADPRIVATE(global_basinid_loc)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: hydrodiag_loc               !! Variable to diagnose the hydrographs
!$OMP THREADPRIVATE(hydrodiag_loc)
  REAL(r_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:)       :: hydroupbasin_loc            !! The area upstream of the gauging station (m^2)
!$OMP THREADPRIVATE(hydroupbasin_loc)
  !
  ! parallelism
  REAL(r_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: routing_area_glo            !! Surface of basin (m^2)
!$OMP THREADPRIVATE(routing_area_glo)
  REAL(r_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)     :: topo_resid_glo              !! Topographic index of the retention time (m)
!$OMP THREADPRIVATE(topo_resid_glo)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: route_togrid_glo            !! Grid into which the basin flows (unitless)
!$OMP THREADPRIVATE(route_togrid_glo)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: route_tobasin_glo           !! Basin in to which the water goes (unitless)
!$OMP THREADPRIVATE(route_tobasin_glo)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: route_nbintobas_glo         !! Number of basin into current one (unitless)
!$OMP THREADPRIVATE(route_nbintobas_glo)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: global_basinid_glo          !! ID of basin (unitless)
!$OMP THREADPRIVATE(global_basinid_glo)
  INTEGER(i_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:,:)  :: hydrodiag_glo               !! Variable to diagnose the hydrographs
!$OMP THREADPRIVATE(hydrodiag_glo)
  REAL(r_std), SAVE, ALLOCATABLE, TARGET, DIMENSION(:)       :: hydroupbasin_glo            !! The area upstream of the gauging station (m^2)
!$OMP THREADPRIVATE(hydroupbasin_glo)
  !
  REAL(r_std), SAVE, POINTER, DIMENSION(:,:)                 :: routing_area                !! Surface of basin (m^2)
!$OMP THREADPRIVATE(routing_area)
  REAL(r_std), SAVE, POINTER, DIMENSION(:,:)                 :: topo_resid                  !! Topographic index of the retention time (m)
!$OMP THREADPRIVATE(topo_resid)
  INTEGER(i_std), SAVE, POINTER, DIMENSION(:,:)              :: route_togrid                !! Grid into which the basin flows (unitless)
!$OMP THREADPRIVATE(route_togrid)
  INTEGER(i_std), SAVE, POINTER, DIMENSION(:,:)              :: route_tobasin               !! Basin in to which the water goes (unitless)
!$OMP THREADPRIVATE(route_tobasin)
  INTEGER(i_std), SAVE, POINTER, DIMENSION(:,:)              :: route_nbintobas             !! Number of basin into current one (unitless)
!$OMP THREADPRIVATE(route_nbintobas)
  INTEGER(i_std), SAVE, POINTER, DIMENSION(:,:)              :: global_basinid              !! ID of basin (unitless)
!$OMP THREADPRIVATE(global_basinid)
  INTEGER(i_std), SAVE, POINTER, DIMENSION(:,:)              :: hydrodiag                   !! Variable to diagnose the hydrographs
!$OMP THREADPRIVATE(hydrodiag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: slowflow_diag               !! Diagnostic slow flow hydrographs (kg/dt)
!$OMP THREADPRIVATE(slowflow_diag)  
  REAL(r_std), SAVE, POINTER, DIMENSION(:)                   :: hydroupbasin                !! The area upstream of the gauging station (m^2)
!$OMP THREADPRIVATE(hydroupbasin)
  !
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: irrigated                   !! Area equipped for irrigation in each grid box (m^2)
!$OMP THREADPRIVATE(irrigated)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: floodplains                 !! Maximal surface which can be inundated in each grid box (m^2)
!$OMP THREADPRIVATE(floodplains)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: swamp                       !! Maximal surface of swamps in each grid box (m^2)
!$OMP THREADPRIVATE(swamp)
  !
  ! The reservoirs, also to be put into the restart file.
  !
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)             :: fast_reservoir              !! Water amount in the fast reservoir (kg)
!$OMP THREADPRIVATE(fast_reservoir)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)             :: slow_reservoir              !! Water amount in the slow reservoir (kg)
!$OMP THREADPRIVATE(slow_reservoir)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)             :: stream_reservoir            !! Water amount in the stream reservoir (kg)
!$OMP THREADPRIVATE(stream_reservoir)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)             :: flood_reservoir             !! Water amount in the floodplains reservoir (kg)
!$OMP THREADPRIVATE(flood_reservoir)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: lake_reservoir              !! Water amount in the lake reservoir (kg)
!$OMP THREADPRIVATE(lake_reservoir)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: pond_reservoir              !! Water amount in the pond reservoir (kg)
!$OMP THREADPRIVATE(pond_reservoir)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:,:)             :: flood_frac_bas              !! Flooded fraction per basin (unitless;0-1)
!$OMP THREADPRIVATE(flood_frac_bas)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: pond_frac                   !! Pond fraction per grid box (unitless;0-1)
!$OMP THREADPRIVATE(pond_frac)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: flood_height                !! Floodplain height (mm)
!$OMP THREADPRIVATE(flood_height)
  !
  ! The accumulated fluxes.
  !
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: floodout_mean               !! Accumulated flow out of floodplains (kg/m^2/dt)
!$OMP THREADPRIVATE(floodout_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: runoff_mean                 !! Accumulated runoff (kg/m^2/dt)
!$OMP THREADPRIVATE(runoff_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: drainage_mean               !! Accumulated drainage (kg/m^2/dt)
!$OMP THREADPRIVATE(drainage_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: transpot_mean               !! Mean potential transpiration from the plants (kg/m^2/dt)
!$OMP THREADPRIVATE(transpot_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: precip_mean                 !! Accumulated precipitation (kg/m^2/dt)
!$OMP THREADPRIVATE(precip_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: humrel_mean                 !! Mean soil moisture stress, mean root extraction potential (unitless)
!$OMP THREADPRIVATE(humrel_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: totnobio_mean               !! Mean last total fraction of no bio (unitless;0-1)
!$OMP THREADPRIVATE(totnobio_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: vegtot_mean                 !! Mean potentially vegetated fraction (unitless;0-1)
!$OMP THREADPRIVATE(vegtot_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: k_litt_mean                 !! Mean averaged conductivity for saturated infiltration in the 'litter' layer (kg/m^2/dt)
!$OMP THREADPRIVATE(k_litt_mean)
  !
  ! The averaged outflow fluxes.
  !
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: lakeinflow_mean              !! Mean lake inflow (kg/m^2/dt)
!$OMP THREADPRIVATE(lakeinflow_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: returnflow_mean              !! Mean water flow from lakes and swamps which returns to the grid box.
                                                                                             !! This water will go back into the hydrol or hydrolc module to allow re-evaporation (kg/m^2/dt)
!$OMP THREADPRIVATE(returnflow_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: reinfiltration_mean          !! Mean water flow which returns to the grid box (kg/m^2/dt)
!$OMP THREADPRIVATE(reinfiltration_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: irrigation_mean              !! Mean irrigation flux.
                                                                                             !! This is the water taken from the reservoirs and beeing put into the upper layers of the soil (kg/m^2/dt)
!$OMP THREADPRIVATE(irrigation_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: riverflow_mean               !! Mean Outflow of the major rivers.
                                                                                             !! The flux will be located on the continental grid but this should be a coastal point (kg/dt)
!$OMP THREADPRIVATE(riverflow_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: coastalflow_mean             !! Mean outflow on coastal points by small basins.
                                                                                             !! This is the water which flows in a disperse way into the ocean (kg/dt)
!$OMP THREADPRIVATE(coastalflow_mean)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: floodtemp                    !! Temperature to decide if floodplains work (K)
!$OMP THREADPRIVATE(floodtemp)
  INTEGER(i_std), SAVE                                       :: floodtemp_lev                !! Temperature level to decide if floodplains work (K)
!$OMP THREADPRIVATE(floodtemp_lev)
  !
  ! Diagnostic variables ... well sort of !
  !
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: irrig_netereq                !! Irrigation requirement (water requirements by the crop for its optimal growth (kg/m^2/dt)
!$OMP THREADPRIVATE(irrig_netereq)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: hydrographs                  !! Hydrographs at the outflow of the grid box for major basins (kg/dt)
!$OMP THREADPRIVATE(hydrographs)
  !
  ! Diagnostics for the various reservoirs we use (Kg/m^2)
  !
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: fast_diag                    !! Diagnostic for the fast reservoir (kg/m^2)
!$OMP THREADPRIVATE(fast_diag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: slow_diag                    !! Diagnostic for the slow reservoir (kg/m^2)
!$OMP THREADPRIVATE(slow_diag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: stream_diag                  !! Diagnostic for the stream reservoir (kg/m^2)
!$OMP THREADPRIVATE(stream_diag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: flood_diag                   !! Diagnostic for the floodplain reservoir (kg/m^2)
!$OMP THREADPRIVATE(flood_diag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: pond_diag                    !! Diagnostic for the pond reservoir (kg/m^2)
!$OMP THREADPRIVATE(pond_diag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: lake_diag                    !! Diagnostic for the lake reservoir (kg/m^2)
!$OMP THREADPRIVATE(lake_diag)
  REAL(r_std), SAVE, ALLOCATABLE, DIMENSION(:)               :: delsurfstor                  !! Diagnostic of the change in surface water storage (flood, pond and lake reservoirs) (kg/m^2)
!$OMP THREADPRIVATE(delsurfstor)
  !
CONTAINS
  !!  =============================================================================================================================
  !! SUBROUTINE:         routing_initialize
  !!
  !>\BRIEF	         Initialize the routing module
  !!
  !! DESCRIPTION:        Initialize the routing module. Read from restart file or read the routing.nc file to initialize the
  !!                     routing scheme. 
  !!
  !! RECENT CHANGE(S)
  !!
  !! REFERENCE(S)
  !! 
  !! FLOWCHART   
  !! \n
  !_ ==============================================================================================================================

  SUBROUTINE routing_initialize( kjit,       nbpt,           index,                 &
                                rest_id,     hist_id,        hist2_id,   lalo,      &
                                neighbours,  resolution,     contfrac,   stempdiag, &
                                returnflow,  reinfiltration, irrigation, riverflow, &
                                coastalflow, flood_frac,     flood_res )
       
    IMPLICIT NONE
    
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)     :: kjit                 !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)     :: nbpt                 !! Domain size (unitless)
    INTEGER(i_std), INTENT(in)     :: index(nbpt)          !! Indices of the points on the map (unitless)
    INTEGER(i_std),INTENT(in)      :: rest_id              !! Restart file identifier (unitless)
    INTEGER(i_std),INTENT(in)      :: hist_id              !! Access to history file (unitless)
    INTEGER(i_std),INTENT(in)      :: hist2_id             !! Access to history file 2 (unitless)
    REAL(r_std), INTENT(in)        :: lalo(nbpt,2)         !! Vector of latitude and longitudes (beware of the order !)

    INTEGER(i_std), INTENT(in)     :: neighbours(nbpt,8)   !! Vector of neighbours for each grid point 
                                                           !! (1=N, 2=NE, 3=E, 4=SE, 5=S, 6=SW, 7=W, 8=NW) (unitless)
    REAL(r_std), INTENT(in)        :: resolution(nbpt,2)   !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)        :: contfrac(nbpt)       !! Fraction of land in each grid box (unitless;0-1)
    REAL(r_std), INTENT(in)        :: stempdiag(nbpt,nbdl) !! Diagnostic soil temperature profile

    !! 0.2 Output variables
    REAL(r_std), INTENT(out)       :: returnflow(nbpt)     !! The water flow from lakes and swamps which returns to the grid box.
                                                           !! This water will go back into the hydrol or hydrolc module to allow re-evaporation (kg/m^2/dt)
    REAL(r_std), INTENT(out)       :: reinfiltration(nbpt) !! Water flow from ponds and floodplains which returns to the grid box (kg/m^2/dt)
    REAL(r_std), INTENT(out)       :: irrigation(nbpt)     !! Irrigation flux. This is the water taken from the reservoirs and beeing put into the upper layers of the soil (kg/m^2/dt)
    REAL(r_std), INTENT(out)       :: riverflow(nbpt)      !! Outflow of the major rivers. The flux will be located on the continental grid but this should be a coastal point (kg/dt)

    REAL(r_std), INTENT(out)       :: coastalflow(nbpt)    !! Outflow on coastal points by small basins. This is the water which flows in a disperse way into the ocean (kg/dt)
    REAL(r_std), INTENT(out)       :: flood_frac(nbpt)     !! Flooded fraction of the grid box (unitless;0-1)
    REAL(r_std), INTENT(out)       :: flood_res(nbpt)      !! Diagnostic of water amount in the floodplains reservoir (kg)
    
    !! 0.3 Local variables
    LOGICAL                        :: init_irrig           !! Logical to initialize the irrigation (true/false)
    LOGICAL                        :: init_flood           !! Logical to initialize the floodplains (true/false)
    LOGICAL                        :: init_swamp           !! Logical to initialize the swamps (true/false)

!_ ================================================================================================================================

    !
    ! do initialisation
    !
    nbvmax = 440
    ! Here we will allocate the memory and get the fixed fields from the restart file.
    ! If the info is not found then we will compute the routing map.
    !
    CALL routing_init (kjit, nbpt, index, returnflow, reinfiltration, irrigation, &
         riverflow, coastalflow, flood_frac, flood_res, stempdiag, rest_id)

    routing_area => routing_area_loc  
    topo_resid => topo_resid_loc
    route_togrid => route_togrid_loc
    route_tobasin => route_tobasin_loc
    global_basinid => global_basinid_loc
    hydrodiag => hydrodiag_loc
    
    ! This routine computes the routing map if the route_togrid_glo is undefined. This means that the
    ! map has not been initialized during the restart process..
    !
    !! Reads in the map of the basins and flow directions to construct the catchments of each grid box
    !
    IF ( COUNT(route_togrid_glo .GE. undef_int) .GT. 0 ) THEN
       CALL routing_basins_p(nbpt, lalo, neighbours, resolution, contfrac)
    ENDIF
    !
    ! Do we have what we need if we want to do irrigation
    !! Initialisation of flags for irrigated land, flood plains and swamps
    !
    init_irrig = .FALSE.
    IF ( do_irrigation ) THEN 
       IF (COUNT(irrigated .GE. undef_sechiba-1) > 0) init_irrig = .TRUE.
    END IF
    
    init_flood = .FALSE.
    IF ( do_floodplains ) THEN
       IF (COUNT(floodplains .GE. undef_sechiba-1) > 0) init_flood = .TRUE.
    END IF
    
    init_swamp = .FALSE.
    IF ( doswamps ) THEN
       IF (COUNT(swamp .GE. undef_sechiba-1) > 0 ) init_swamp = .TRUE.
    END IF
       
    !! If we have irrigated land, flood plains or swamps then we need to interpolate the 0.5 degree
    !! base data set to the resolution of the model.
    
    IF ( init_irrig .OR. init_flood .OR. init_swamp ) THEN
       CALL routing_irrigmap(nbpt, index, lalo, neighbours, resolution, &
            contfrac, init_irrig, irrigated, init_flood, floodplains, init_swamp, swamp, hist_id, hist2_id)
    ENDIF
    
    IF ( do_irrigation ) THEN 
       CALL xios_orchidee_send_field("irrigmap",irrigated)
       
       WRITE(numout,*) 'Verification : range of irrigated : ', MINVAL(irrigated), MAXVAL(irrigated) 
       IF ( .NOT. almaoutput ) THEN
          CALL histwrite_p(hist_id, 'irrigmap', 1, irrigated, nbpt, index)
       ELSE
          CALL histwrite_p(hist_id, 'IrrigationMap', 1, irrigated, nbpt, index)
       ENDIF
       IF ( hist2_id > 0 ) THEN
          IF ( .NOT. almaoutput ) THEN
             CALL histwrite_p(hist2_id, 'irrigmap', 1, irrigated, nbpt, index)
          ELSE
             CALL histwrite_p(hist2_id, 'IrrigationMap', 1, irrigated, nbpt, index)
          ENDIF
       ENDIF
    ENDIF
    
    IF ( do_floodplains ) THEN
       CALL xios_orchidee_send_field("floodmap",floodplains)
       
       WRITE(numout,*) 'Verification : range of floodplains : ', MINVAL(floodplains), MAXVAL(floodplains) 
       IF ( .NOT. almaoutput ) THEN
          CALL histwrite_p(hist_id, 'floodmap', 1, floodplains, nbpt, index)
       ELSE
          CALL histwrite_p(hist_id, 'FloodplainsMap', 1, floodplains, nbpt, index)
       ENDIF
       IF ( hist2_id > 0 ) THEN
          IF ( .NOT. almaoutput ) THEN
             CALL histwrite_p(hist2_id, 'floodmap', 1, floodplains, nbpt, index)
          ELSE
             CALL histwrite_p(hist2_id, 'FloodplainsMap', 1, floodplains, nbpt, index)
          ENDIF
       ENDIF
    ENDIF
    
    IF ( doswamps ) THEN
       CALL xios_orchidee_send_field("swampmap",swamp)
       
       WRITE(numout,*) 'Verification : range of swamp : ', MINVAL(swamp), MAXVAL(swamp) 
       IF ( .NOT. almaoutput ) THEN
          CALL histwrite_p(hist_id, 'swampmap', 1, swamp, nbpt, index)
       ELSE
          CALL histwrite_p(hist_id, 'SwampMap', 1, swamp, nbpt, index)
       ENDIF
       IF ( hist2_id > 0 ) THEN
          IF ( .NOT. almaoutput ) THEN
             CALL histwrite_p(hist2_id, 'swampmap', 1, swamp, nbpt, index)
          ELSE
             CALL histwrite_p(hist2_id, 'SwampMap', 1, swamp, nbpt, index)
          ENDIF
       ENDIF
    ENDIF
    
    !! This routine gives a diagnostic of the basins used.
    CALL routing_diagnostic_p(nbpt, index, lalo, resolution, contfrac, hist_id, hist2_id)
    
    l_first_routing = .FALSE.
    
  END SUBROUTINE routing_initialize

!! ================================================================================================================================
!! SUBROUTINE   : routing_main 
!!
!>\BRIEF          This module routes the water over the continents (runoff and
!!                drainage produced by the hydrolc or hydrol module) into the oceans. 
!!
!! DESCRIPTION (definitions, functional, design, flags):
!! The routing scheme (Polcher, 2003) carries the water from the runoff and drainage simulated by SECHIBA
!! to the ocean through reservoirs, with some delay. The routing scheme is based on
!! a parametrization of the water flow on a global scale (Miller et al., 1994; Hagemann
!! and Dumenil, 1998). Given the global map of the main watersheds (Oki et al., 1999;
!! Fekete et al., 1999; Vorosmarty et al., 2000) which delineates the boundaries of subbasins
!! and gives the eight possible directions of water flow within the pixel, the surface
!! runoff and the deep drainage are routed to the ocean. The time-step of the routing is one day.
!! The scheme also diagnoses how much water is retained in the foodplains and thus return to soil
!! moisture or is taken out of the rivers for irrigation. \n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): 
!! The result of the routing are 3 fluxes :
!! - riverflow   : The water which flows out from the major rivers. The flux will be located
!!                 on the continental grid but this should be a coastal point.
!! - coastalflow : This is the water which flows in a disperse way into the ocean. Essentially these
!!                 are the outflows from all of the small rivers.
!! - returnflow  : This is the water which flows into a land-point - typically rivers which end in
!!                 the desert. This water will go back into the hydrol module to allow re-evaporation.
!! - irrigation  : This is water taken from the reservoir and is being put into the upper 
!!                 layers of the soil.
!! The two first fluxes are in kg/dt and the last two fluxes are in kg/(m^2dt).\n
!!
!! REFERENCE(S) :
!! - Miller JR, Russell GL, Caliri G (1994)
!!   Continental-scale river flow in climate models.
!!   J. Clim., 7:914-928
!! - Hagemann S and Dumenil L. (1998)
!!   A parametrization of the lateral waterflow for the global scale.
!!   Clim. Dyn., 14:17-31
!! - Oki, T., T. Nishimura, and P. Dirmeyer (1999)
!!   Assessment of annual runoff from land surface models using total runoff integrating pathways (TRIP)
!!   J. Meteorol. Soc. Jpn., 77, 235-255
!! - Fekete BM, Charles V, Grabs W (2000)
!!   Global, composite runoff fields based on observed river discharge and simulated water balances.
!!   Technical report, UNH/GRDC, Global Runoff Data Centre, Koblenz
!! - Vorosmarty, C., B. Fekete, B. Meybeck, and R. Lammers (2000)
!!   Global system of rivers: Its role in organizing continental land mass and defining land-to-ocean linkages
!!   Global Biogeochem. Cycles, 14, 599-621
!! - Vivant, A-C. (?? 2002)
!!   Développement du schéma de routage et des plaines d'inondation, MSc Thesis, Paris VI University
!! - J. Polcher (2003)
!!   Les processus de surface a l'echelle globale et leurs interactions avec l'atmosphere
!!   Habilitation a diriger les recherches, Paris VI University, 67pp.
!!
!! FLOWCHART    :
!! \latexonly 
!! \includegraphics[scale=0.75]{routing_main_flowchart.png}
!! \endlatexonly
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_main(kjit, nbpt, index, &
       & lalo, neighbours, resolution, contfrac, totfrac_nobio, veget, veget_max, soil_deficit, floodout, runoff, &
       & drainage, transpot, evapot_corr, vegstress, precip_rain, humrel, k_litt, flood_frac, flood_res, &
       & stempdiag, reinf_slope, returnflow, reinfiltration, irrigation, riverflow, coastalflow, rest_id, hist_id, hist2_id, &
       & irrigr, do_irrig_reservoir, irrig_dep_threshold, reservoir_dep_max, do_awd, awd_dep, lai)

    IMPLICIT NONE

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)     :: kjit                 !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)     :: nbpt                 !! Domain size (unitless)
    INTEGER(i_std),INTENT(in)      :: rest_id              !! Restart file identifier (unitless)
    INTEGER(i_std),INTENT(in)      :: hist_id              !! Access to history file (unitless)
    INTEGER(i_std),INTENT(in)      :: hist2_id             !! Access to history file 2 (unitless)
    INTEGER(i_std), INTENT(in)     :: index(nbpt)          !! Indices of the points on the map (unitless)
    REAL(r_std), INTENT(in)        :: lalo(nbpt,2)         !! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)     :: neighbours(nbpt,8)   !! Vector of neighbours for each grid point (1=N, 2=NE, 3=E, 4=SE, 5=S, 6=SW, 7=W, 8=NW) (unitless)
    REAL(r_std), INTENT(in)        :: resolution(nbpt,2)   !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)        :: contfrac(nbpt)       !! Fraction of land in each grid box (unitless;0-1)
    REAL(r_std), INTENT(in)        :: totfrac_nobio(nbpt)  !! Total fraction of no-vegetation (continental ice, lakes ...) (unitless;0-1)
    REAL(r_std), INTENT(in)        :: veget(nbpt,nvm)      !! fraction of vegetation (unitless;0-1)
    REAL(r_std), INTENT(in)        :: veget_max(nbpt,nvm)  !! Maximal fraction of vegetation (unitless;0-1)
    REAL(r_std), INTENT(in)        :: lai(nbpt,nvm)        !! leaf area index
    REAL(r_std), INTENT(in)        :: soil_deficit(nbpt,nvm)  !! soil water deficit
    REAL(r_std), INTENT(in)        :: floodout(nbpt)       !! Grid-point flow out of floodplains (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: runoff(nbpt)         !! Grid-point runoff (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: drainage(nbpt)       !! Grid-point drainage (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: transpot(nbpt,nvm)   !! Potential transpiration of the vegetation (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: evapot_corr(nbpt)    !! Potential soil evaporation (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: vegstress(nbpt,nvm)  !! stress for vegetation growth (unitless; 0-1)
    REAL(r_std), INTENT(in)        :: precip_rain(nbpt)    !! Rainfall (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: k_litt(nbpt)         !! Averaged conductivity for saturated infiltration in the 'litter' layer (kg/m^2/dt)
    REAL(r_std), INTENT(in)        :: humrel(nbpt,nvm)     !! Soil moisture stress, root extraction potential (unitless)
    REAL(r_std), INTENT(in)        :: stempdiag(nbpt,nbdl) !! Diagnostic soil temperature profile
    REAL(r_std), INTENT(in)        :: reinf_slope(nbpt)    !! Coefficient which determines the reinfiltration ratio in the grid box due to flat areas (unitless;0-1)
    REAL(r_std), INTENT(in)        :: irrigr(nbpt,nstm)    !! irrigation reservoir [kg.m-2]
    LOGICAL, INTENT (in)           :: do_irrig_reservoir(nstm)  !! flag of irrigation reservoir for specific soil column
    REAL(r_std), INTENT (in)       :: irrig_dep_threshold(nstm) !! threshold of depth when irrigation occurs
    REAL(r_std), INTENT (in)       :: reservoir_dep_max(nstm)   !! maximum depth of irrigation reservoir
    LOGICAL, INTENT (in)           :: do_awd(nstm)         !! if do awd irrigation
    REAL(r_std), INTENT (in)       :: awd_dep(nstm)        !! water amount (in depth) of one irrigation event

    !! 0.2 Output variables
    REAL(r_std), INTENT(out)       :: returnflow(nbpt)     !! The water flow from lakes and swamps which returns to the grid box.
                                                           !! This water will go back into the hydrol or hydrolc module to allow re-evaporation (kg/m^2/dt)
    REAL(r_std), INTENT(out)       :: reinfiltration(nbpt) !! Water flow from ponds and floodplains which returns to the grid box (kg/m^2/dt)
    REAL(r_std), INTENT(out)       :: irrigation(nbpt)     !! Irrigation flux. This is the water taken from the reservoirs and beeing put into the upper layers of the soil (kg/m^2/dt)
    REAL(r_std), INTENT(out)       :: riverflow(nbpt)      !! Outflow of the major rivers. The flux will be located on the continental grid but this should be a coastal point (kg/dt)
    REAL(r_std), INTENT(out)       :: coastalflow(nbpt)    !! Outflow on coastal points by small basins. This is the water which flows in a disperse way into the ocean (kg/dt)
    REAL(r_std), INTENT(out)       :: flood_frac(nbpt)     !! Flooded fraction of the grid box (unitless;0-1)
    REAL(r_std), INTENT(out)       :: flood_res(nbpt)      !! Diagnostic of water amount in the floodplains reservoir (kg)

    !! 0.3 Local variables
    CHARACTER(LEN=30)              :: var_name             !! To store variables names for I/O (unitless)
    REAL(r_std), DIMENSION(1)      :: tmp_day              !! 
    REAL(r_std), DIMENSION(nbpt)   :: return_lakes         !! Water from lakes flowing back into soil moisture (kg/m^2/dt)
    INTEGER(i_std)                 :: ig, jv               !! Indices (unitless)
    REAL(r_std), DIMENSION(nbpt)   :: tot_vegfrac_nowoody  !! Total fraction occupied by grass (0-1,unitless)
    REAL(r_std), DIMENSION(nbpt)   :: tot_vegfrac_crop  !! Total fraction occupied by croplands (0-1,unitless)

!_ ================================================================================================================================

    !
    !! Computes the variables averaged between routing time steps and which will be used in subsequent calculations
    !
    floodout_mean(:) = floodout_mean(:) + floodout(:)
    runoff_mean(:) = runoff_mean(:) + runoff(:)
    drainage_mean(:) = drainage_mean(:) + drainage(:)
    floodtemp(:) = stempdiag(:,floodtemp_lev)
    precip_mean(:) =  precip_mean(:) + precip_rain(:)
    !
    !! Computes the total fraction occupied by the grasses and the crops for each grid cell
    tot_vegfrac_nowoody(:) = zero
    tot_vegfrac_crop(:) = zero
    DO jv  = 1, nvm
       IF ( (jv /= ibare_sechiba) .AND. .NOT.(is_tree(jv)) ) THEN
          tot_vegfrac_nowoody(:) = tot_vegfrac_nowoody(:) + veget_max(:,jv) 
       END IF
       IF ( (jv /= ibare_sechiba) .AND. ok_LAIdev(jv)  ) THEN ! cropland judgement using ok_LAIdev, xuhui
           tot_vegfrac_crop(:) = tot_vegfrac_crop(:) + veget_max(:,jv)
       ENDIF
    END DO

    DO ig = 1, nbpt
       IF ( tot_vegfrac_nowoody(ig) .GT. min_sechiba ) THEN
          DO jv = 1,nvm
             IF ( (jv /= ibare_sechiba) .AND. .NOT.(is_tree(jv)) ) THEN
                transpot_mean(ig) = transpot_mean(ig) + transpot(ig,jv) * veget_max(ig,jv)/tot_vegfrac_nowoody(ig)  
             END IF
          END DO
       ELSE
          IF (MAXVAL(veget_max(ig,2:nvm)) .GT. min_sechiba) THEN
             DO jv = 2, nvm
                transpot_mean(ig) = transpot_mean(ig) + transpot(ig,jv) * veget_max(ig,jv)/ SUM(veget_max(ig,2:nvm))
             ENDDO
          ENDIF
       ENDIF
    ENDDO

    !
    ! Averaged variables (i.e. *dt_sechiba/dt_routing). This accounts for the difference between the shorter
    ! timestep dt_sechiba of other parts of the model and the long dt_routing timestep (set to one day at present)
    !
    totnobio_mean(:) = totnobio_mean(:) + totfrac_nobio(:)*dt_sechiba/dt_routing
    k_litt_mean(:) = k_litt_mean(:) + k_litt(:)*dt_sechiba/dt_routing
    !
    ! Only potentially vegetated surfaces are taken into account. At the start of
    ! the growing seasons we will give more weight to these areas.
    !
    DO jv=2,nvm
       DO ig=1,nbpt
          humrel_mean(ig) = humrel_mean(ig) + humrel(ig,jv)*veget_max(ig,jv)*dt_sechiba/dt_routing
          vegtot_mean(ig) = vegtot_mean(ig) + veget_max(ig,jv)*dt_sechiba/dt_routing
       ENDDO
    ENDDO
    !
    time_counter = time_counter + dt_sechiba 
    !
    ! If the time has come we do the routing.
    !
    IF ( NINT(time_counter) .GE. NINT(dt_routing) ) THEN 
       !
       ! Check the water balance if needed
       !
       IF ( check_waterbal ) THEN
          CALL routing_waterbal(nbpt, .TRUE., floodout_mean, runoff_mean, drainage_mean, returnflow_mean, &
               & reinfiltration_mean, irrigation_mean, riverflow_mean, coastalflow_mean)
       ENDIF
       !
       ! Make sure we do not flood north of 49N as there freezing processes start to play a role and they
       ! are not yet well treated in ORCHIDEE.
       !
       DO ig=1,nbpt
          IF ( lalo(ig,1) > 49.0 ) THEN
             floodtemp(ig) = tp_00 - un
          ENDIF
       ENDDO
       !
       !! Computes the transport of water in the various reservoirs
       !
       CALL routing_flow(nbpt, dt_routing, lalo, floodout_mean, runoff_mean, drainage_mean, &
            & vegtot_mean, totnobio_mean, transpot_mean, transpot, evapot_corr, veget, veget_max, soil_deficit, &
            & precip_mean, humrel_mean, k_litt_mean, floodtemp, reinf_slope, &
            & lakeinflow_mean, returnflow_mean, reinfiltration_mean, irrigation_mean, riverflow_mean, &
            & coastalflow_mean, hydrographs, slowflow_diag, flood_frac, flood_res, vegstress, &
            & irrigr, do_irrig_reservoir, irrig_dep_threshold, reservoir_dep_max, do_awd, awd_dep, lai)
       !
       !! Responsible for storing the water in lakes
       !
       CALL routing_lake(nbpt, dt_routing, lakeinflow_mean, humrel_mean, return_lakes)
       !
       returnflow_mean(:) = returnflow_mean(:) + return_lakes(:)
       !
       !! Check the water balance in the routing scheme
       !
       IF ( check_waterbal ) THEN
          CALL routing_waterbal(nbpt, .FALSE., floodout_mean, runoff_mean, drainage_mean, returnflow_mean, &
               & reinfiltration_mean, irrigation_mean, riverflow_mean, coastalflow_mean)
       ENDIF
       !
       time_counter = zero
       !
       floodout_mean(:) = zero
       runoff_mean(:) = zero
       drainage_mean(:) = zero
       transpot_mean(:) = zero
       precip_mean(:) = zero
       !
       humrel_mean(:) = zero
       totnobio_mean(:) = zero
       k_litt_mean(:) = zero
       vegtot_mean(:) = zero
       !
       ! Change the units of the routing fluxes from kg/dt_routing into kg/dt_sechiba
       !                                    and from m^3/dt_routing into m^3/dt_sechiba
       !
       returnflow_mean(:) = returnflow_mean(:)/dt_routing*dt_sechiba
       reinfiltration_mean(:) = reinfiltration_mean(:)/dt_routing*dt_sechiba
       irrigation_mean(:) = irrigation_mean(:)/dt_routing*dt_sechiba
       irrig_netereq(:) = irrig_netereq(:)/dt_routing*dt_sechiba
       !
       ! Change units as above but at the same time transform the kg/dt_sechiba to m^3/dt_sechiba
       !
       riverflow_mean(:) = riverflow_mean(:)/dt_routing*dt_sechiba/mille
       coastalflow_mean(:) = coastalflow_mean(:)/dt_routing*dt_sechiba/mille
       hydrographs(:) = hydrographs(:)/dt_routing*dt_sechiba/mille
       slowflow_diag(:) = slowflow_diag(:)/dt_routing*dt_sechiba/mille
    ENDIF

    !
    ! Return the fraction of routed water for this time step.
    !
    returnflow(:) = returnflow_mean(:)
    reinfiltration(:) = reinfiltration_mean(:)
    irrigation(:) = irrigation_mean(:)
    riverflow(:) = riverflow_mean(:)
    coastalflow(:) = coastalflow_mean(:)  
    !
    ! Write diagnostics
    !
    CALL xios_orchidee_send_field("riversret",returnflow*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("hydrographs",hydrographs/dt_sechiba)
    CALL xios_orchidee_send_field("Qb",slowflow_diag/dt_sechiba) ! Qb in m3/s
    CALL xios_orchidee_send_field("reinfiltration",reinfiltration)
    CALL xios_orchidee_send_field("fastr",fast_diag)
    CALL xios_orchidee_send_field("slowr",slow_diag)
    CALL xios_orchidee_send_field("streamr",stream_diag)
    CALL xios_orchidee_send_field("lakevol",lake_diag)
    CALL xios_orchidee_send_field("pondr",pond_diag)
    CALL xios_orchidee_send_field("irrigation",irrigation*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("irrigation_alma",irrigation/dt_sechiba)
    CALL xios_orchidee_send_field("netirrig",irrig_netereq*one_day/dt_sechiba)
    CALL xios_orchidee_send_field("netirrig_alma",irrig_netereq/dt_sechiba)
    CALL xios_orchidee_send_field("floodh",flood_height)
    CALL xios_orchidee_send_field("floodr",flood_diag)
    delsurfstor = delsurfstor + flood_diag + pond_diag + lake_diag
    CALL xios_orchidee_send_field("DelSurfStor",delsurfstor)
    CALL xios_orchidee_send_field("SurfStor",flood_diag+pond_diag+lake_diag)

    IF ( .NOT. almaoutput ) THEN
       !
       CALL histwrite_p(hist_id, 'riversret', kjit, returnflow, nbpt, index)
       IF (do_floodplains .OR. doponds) THEN
          CALL histwrite_p(hist_id, 'reinfiltration', kjit, reinfiltration, nbpt, index)
       ENDIF
       CALL histwrite_p(hist_id, 'hydrographs', kjit, hydrographs, nbpt, index)
       !
       CALL histwrite_p(hist_id, 'fastr', kjit, fast_diag, nbpt, index)
       CALL histwrite_p(hist_id, 'slowr', kjit, slow_diag, nbpt, index)
       CALL histwrite_p(hist_id, 'streamr', kjit, stream_diag, nbpt, index)
       IF ( do_floodplains ) THEN
          CALL histwrite_p(hist_id, 'floodr', kjit, flood_diag, nbpt, index)
          CALL histwrite_p(hist_id, 'floodh', kjit, flood_height, nbpt, index)
       ENDIF
       CALL histwrite_p(hist_id, 'pondr', kjit, pond_diag, nbpt, index)
       CALL histwrite_p(hist_id, 'lakevol', kjit, lake_diag, nbpt, index)
       !
       IF ( do_irrigation ) THEN
          CALL histwrite_p(hist_id, 'irrigation', kjit, irrigation, nbpt, index)
          CALL histwrite_p(hist_id, 'returnflow', kjit, returnflow, nbpt, index)
          CALL histwrite_p(hist_id, 'netirrig', kjit, irrig_netereq, nbpt, index)
       ENDIF
       !
    ELSE
       !
       delsurfstor = delsurfstor + flood_diag + pond_diag + lake_diag
       CALL histwrite_p(hist_id, 'DelSurfStor', kjit, delsurfstor, nbpt, index)
       CALL histwrite_p(hist_id, 'SurfStor', kjit, flood_diag+pond_diag+lake_diag, nbpt, index)
       CALL histwrite_p(hist_id, 'Dis', kjit, hydrographs, nbpt, index)
       IF ( do_irrigation ) THEN
          CALL histwrite_p(hist_id, 'Qirrig', kjit, irrigation, nbpt, index)
          CALL histwrite_p(hist_id, 'Qirrig_req', kjit, irrig_netereq, nbpt, index)
       ENDIF
       !
    ENDIF
    IF ( hist2_id > 0 ) THEN
       IF ( .NOT. almaoutput ) THEN
          !
          CALL histwrite_p(hist2_id, 'riversret', kjit, returnflow, nbpt, index)
          IF (do_floodplains .OR. doponds) THEN
             CALL histwrite_p(hist2_id, 'reinfiltration', kjit, reinfiltration, nbpt, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'hydrographs', kjit, hydrographs, nbpt, index)
          !
          CALL histwrite_p(hist2_id, 'fastr', kjit, fast_diag, nbpt, index)
          CALL histwrite_p(hist2_id, 'slowr', kjit, slow_diag, nbpt, index)
          IF ( do_floodplains ) THEN
             CALL histwrite_p(hist2_id, 'floodr', kjit, flood_diag, nbpt, index)
             CALL histwrite_p(hist2_id, 'floodh', kjit, flood_height, nbpt, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'pondr', kjit, pond_diag, nbpt, index)
          CALL histwrite_p(hist2_id, 'streamr', kjit, stream_diag, nbpt, index)
          CALL histwrite_p(hist2_id, 'lakevol', kjit, lake_diag, nbpt, index)
          !
          IF ( do_irrigation ) THEN
             CALL histwrite_p(hist2_id, 'irrigation', kjit, irrigation, nbpt, index)
             CALL histwrite_p(hist2_id, 'returnflow', kjit, returnflow, nbpt, index)
             CALL histwrite_p(hist2_id, 'netirrig', kjit, irrig_netereq, nbpt, index)
          ENDIF
          !
       ELSE
          !
          delsurfstor=delsurfstor + flood_diag + pond_diag + lake_diag
          CALL histwrite_p(hist2_id, 'DelSurfStor', kjit, delsurfstor, nbpt, index)
          CALL histwrite_p(hist2_id, 'SurfStor', kjit, flood_diag+pond_diag+lake_diag, nbpt, index)
          CALL histwrite_p(hist2_id, 'Dis', kjit, hydrographs, nbpt, index)
          !
       ENDIF
    ENDIF
    !
    !
  END SUBROUTINE routing_main
  
  !!  =============================================================================================================================
  !! SUBROUTINE:         routing_finalize
  !!
  !>\BRIEF	         Write to restart file
  !!
  !! DESCRIPTION:        Write module variables to restart file
  !!
  !! RECENT CHANGE(S)
  !!
  !! REFERENCE(S)
  !! 
  !! FLOWCHART   
  !! \n
  !_ ==============================================================================================================================

  SUBROUTINE routing_finalize( kjit, nbpt, rest_id, flood_frac, flood_res )
    
    IMPLICIT NONE
    
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)     :: kjit                 !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)     :: nbpt                 !! Domain size (unitless)
    INTEGER(i_std),INTENT(in)      :: rest_id              !! Restart file identifier (unitless)
    REAL(r_std), INTENT(in)        :: flood_frac(nbpt)     !! Flooded fraction of the grid box (unitless;0-1)
    REAL(r_std), INTENT(in)        :: flood_res(nbpt)      !! Diagnostic of water amount in the floodplains reservoir (kg)
    
    !! 0.2 Local variables
    REAL(r_std), DIMENSION(1)      :: tmp_day              

!_ ================================================================================================================================
    
    !
    ! Write restart variables
    !
    tmp_day(1) = time_counter
    IF (is_root_prc) CALL restput (rest_id, 'routingcounter', 1, 1, 1, kjit, tmp_day)

    CALL restput_p (rest_id, 'routingarea', nbp_glo, nbasmax, 1, kjit, routing_area, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'routetogrid', nbp_glo, nbasmax, 1, kjit, REAL(route_togrid,r_std), 'scatter', &
         nbp_glo, index_g)
    CALL restput_p (rest_id, 'routetobasin', nbp_glo, nbasmax, 1, kjit, REAL(route_tobasin,r_std), 'scatter', &
         nbp_glo, index_g)
    CALL restput_p (rest_id, 'basinid', nbp_glo, nbasmax, 1, kjit, REAL(global_basinid,r_std), 'scatter', &
         nbp_glo, index_g)
    CALL restput_p (rest_id, 'topoindex', nbp_glo, nbasmax, 1, kjit, topo_resid, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'fastres', nbp_glo, nbasmax, 1, kjit, fast_reservoir, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'slowres', nbp_glo, nbasmax, 1, kjit, slow_reservoir, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'streamres', nbp_glo, nbasmax, 1, kjit, stream_reservoir, 'scatter',nbp_glo,index_g)
    CALL restput_p (rest_id, 'floodres', nbp_glo, nbasmax, 1, kjit, flood_reservoir, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'floodh', nbp_glo, 1, 1, kjit, flood_height, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'flood_frac_bas', nbp_glo, nbasmax, 1, kjit, flood_frac_bas, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'pond_frac', nbp_glo, 1, 1, kjit, pond_frac, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'flood_frac', nbp_glo, 1, 1, kjit, flood_frac, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'flood_res', nbp_glo, 1, 1, kjit, flood_res, 'scatter', nbp_glo, index_g)

    CALL restput_p (rest_id, 'lakeres', nbp_glo, 1, 1, kjit, lake_reservoir, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'pondres', nbp_glo, 1, 1, kjit, pond_reservoir, 'scatter',  nbp_glo, index_g)

    CALL restput_p (rest_id, 'lakeinflow', nbp_glo, 1, 1, kjit, lakeinflow_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'returnflow', nbp_glo, 1, 1, kjit, returnflow_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'reinfiltration', nbp_glo, 1, 1, kjit, reinfiltration_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'riverflow', nbp_glo, 1, 1, kjit, riverflow_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'coastalflow', nbp_glo, 1, 1, kjit, coastalflow_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'hydrographs', nbp_glo, 1, 1, kjit, hydrographs, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'slowflow_diag', nbp_glo, 1, 1, kjit, slowflow_diag, 'scatter',  nbp_glo, index_g)
    !
    ! Keep track of the accumulated variables
    !
    CALL restput_p (rest_id, 'floodout_route', nbp_glo, 1, 1, kjit, floodout_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'runoff_route', nbp_glo, 1, 1, kjit, runoff_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'drainage_route', nbp_glo, 1, 1, kjit, drainage_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'transpot_route', nbp_glo, 1, 1, kjit, transpot_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'precip_route', nbp_glo, 1, 1, kjit, precip_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'humrel_route', nbp_glo, 1, 1, kjit, humrel_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'totnobio_route', nbp_glo, 1, 1, kjit, totnobio_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'k_litt_route', nbp_glo, 1, 1, kjit, k_litt_mean, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'vegtot_route', nbp_glo, 1, 1, kjit, vegtot_mean, 'scatter',  nbp_glo, index_g)

    IF ( do_irrigation ) THEN
       CALL restput_p (rest_id, 'irrigated', nbp_glo, 1, 1, kjit, irrigated, 'scatter',  nbp_glo, index_g)
       CALL restput_p (rest_id, 'irrigation', nbp_glo, 1, 1, kjit, irrigation_mean, 'scatter',  nbp_glo, index_g)
    ENDIF

    IF ( do_floodplains ) THEN
       CALL restput_p (rest_id, 'floodplains', nbp_glo, 1, 1, kjit, floodplains, 'scatter',  nbp_glo, index_g)
    ENDIF
    IF ( doswamps ) THEN
       CALL restput_p (rest_id, 'swamp', nbp_glo, 1, 1, kjit, swamp, 'scatter',  nbp_glo, index_g)
    ENDIF
  
  END SUBROUTINE routing_finalize

!! ================================================================================================================================
!! SUBROUTINE 	: routing_init
!!
!>\BRIEF         This subroutine allocates the memory and get the fixed fields from the restart file.
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    :None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_init(kjit, nbpt, index, returnflow, reinfiltration, irrigation, &
       &                  riverflow, coastalflow, flood_frac, flood_res, stempdiag, rest_id)
    !
    IMPLICIT NONE
    !
    ! interface description
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                   :: kjit           !! Time step number (unitless)
    INTEGER(i_std), INTENT(in)                   :: nbpt           !! Domain size (unitless)
    INTEGER(i_std), DIMENSION (nbpt), INTENT(in) :: index          !! Indices of the points on the map (unitless)
    REAL(r_std), DIMENSION(nbpt,nbdl),INTENT(in) :: stempdiag      !! Temperature profile in soil
    INTEGER(i_std), INTENT(in)                   :: rest_id        !! Restart file identifier (unitless)
    !
!! OUTPUT VARIABLES
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: returnflow     !! The water flow from lakes and swamps which returns into the grid box.
                                                                   !! This water will go back into the hydrol or hydrolc module to allow re-evaporation (kg/m^2/dt)
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: reinfiltration !! Water flow from ponds and floodplains which returns to the grid box (kg/m^2/dt)
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: irrigation     !! Irrigation flux. This is the water taken from the reservoirs and beeing put into the upper layers of the soil.(kg/m^2/dt)
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: riverflow      !! Outflow of the major rivers. The flux will be located on the continental grid but this should be a coastal point (kg/dt)
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: coastalflow    !! Outflow on coastal points by small basins. This is the water which flows in a disperse way into the ocean (kg/dt)
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: flood_frac     !! Flooded fraction of the grid box (unitless;0-1)
    REAL(r_std), DIMENSION (nbpt),INTENT(out)    :: flood_res      !! Diagnostic of water amount in the floodplains reservoir (kg)
    !
!! LOCAL VARIABLES
    CHARACTER(LEN=80)                            :: var_name       !! To store variables names for I/O (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: tmp_real_g     !! A temporary real array for the integers
    REAL(r_std), DIMENSION(1)                    :: tmp_day        !!
    REAL(r_std)                                  :: ratio          !! Diagnostic ratio to check that dt_routing is a multiple of dt_sechiba (unitless)
    REAL(r_std)                                  :: totarea        !! Total area of basin (m^2)
    INTEGER(i_std)                               :: ier, ig, ib, ipn(1) !! Indices (unitless)

!_ ================================================================================================================================
    !
    !
    ! These variables will require the configuration infrastructure
    !
    !Config Key   = ROUTING_TIMESTEP
    !Config If    = RIVER_ROUTING
    !Config Desc  = Time step of the routing scheme
    !Config Def   = one_day
    !Config Help  = This values gives the time step in seconds of the routing scheme. 
    !Config         It should be multiple of the main time step of ORCHIDEE. One day
    !Config         is a good value.
    !Config Units = [seconds]
    !
    dt_routing = one_day
    CALL getin_p('DT_ROUTING', dt_routing)
    !
    !Config Key   = ROUTING_RIVERS
    !Config If    = RIVER_ROUTING
    !Config Desc  = Number of rivers 
    !Config Def   = 50
    !Config Help  = This parameter chooses the number of largest river basins
    !Config         which should be treated as independently as rivers and not
    !Config         flow into the oceans as diffusion coastal flow.
    !Config Units = [-]
    num_largest = 50
    CALL getin_p('ROUTING_RIVERS', num_largest)
    !
    !Config Key   = DO_FLOODINFILT
    !Config Desc  = Should floodplains reinfiltrate into the soil 
    !Config If    = RIVER_ROUTING
    !Config Def   = n
    !Config Help  = This parameters allows the user to ask the model
    !Config         to take into account the flood plains reinfiltration 
    !Config         into the soil moisture. It then can go 
    !Config         back to the slow and fast reservoirs
    !Config Units = [FLAG]
    !
    dofloodinfilt = .FALSE.
    CALL getin_p('DO_FLOODINFILT', dofloodinfilt)
    !
    !Config Key   = DO_SWAMPS
    !Config Desc  = Should we include swamp parameterization 
    !Config If    = RIVER_ROUTING
    !Config Def   = n
    !Config Help  = This parameters allows the user to ask the model
    !Config         to take into account the swamps and return 
    !Config         the water into the bottom of the soil. It then can go 
    !Config         back to the atmopshere. This tried to simulate 
    !Config         internal deltas of rivers.
    !Config Units = [FLAG]
    !
    doswamps = .FALSE.
    CALL getin_p('DO_SWAMPS', doswamps)
    !
    !Config Key   = DO_PONDS
    !Config Desc  = Should we include ponds 
    !Config If    = RIVER_ROUTING
    !Config Def   = n
    !Config Help  = This parameters allows the user to ask the model
    !Config         to take into account the ponds and return 
    !Config         the water into the soil moisture. It then can go 
    !Config         back to the atmopshere. This tried to simulate 
    !Config         little ponds especially in West Africa.
    !Config Units = [FLAG]
    !
    doponds = .FALSE.
    CALL getin_p('DO_PONDS', doponds)
    !
    ! Fix the time constants according to hydrol_cwrr flag
    !
    !
    !Config Key   = SLOW_TCST
    !Config Desc  = Time constant for the slow reservoir 
    !Config If    = RIVER_ROUTING 
    !Config Def   = n
    !Config Help  = This parameters allows the user to fix the 
    !Config         time constant (in days) of the slow reservoir
    !Config         in order to get better river flows for 
    !Config         particular regions.
    !Config Units = [days]
    !
!> A value for property of each reservoir (in day/m) is given to compute a time constant (in day)
!> for each reservoir (product of tcst and topo_resid).
!> The value of tcst has been calibrated for the three reservoirs over the Senegal river basin only,
!> during the 1 degree NCEP Corrected by Cru (NCC) resolution simulations (Ngo-Duc et al., 2005, Ngo-Duc et al., 2006) and
!> generalized for all the basins of the world. The "slow reservoir" and the "fast reservoir"
!> have the highest value in order to simulate the groundwater. 
!> The "stream reservoir", which represents all the water of the stream, has the lowest value.
!> Those figures are the same for all the basins of the world.
!> The value of slow_tcst is equal to fast_tcst when CWRR is activated.
!> This assumption should be re-discussed.
    !
    IF ( hydrol_cwrr ) THEN
       slow_tcst = slow_tcst_cwrr
    ELSE
       slow_tcst = slow_tcst_chois
    ENDIF
    CALL getin_p('SLOW_TCST', slow_tcst)
    !
    !Config Key   = FAST_TCST
    !Config Desc  = Time constant for the fast reservoir 
    !Config If    = RIVER_ROUTING 
    !Config Def   = fast_tcst_cwrr or fast_tcst_chois depending on flag HYDROL_CWRR
    !Config Help  = This parameters allows the user to fix the 
    !Config         time constant (in days) of the fast reservoir
    !Config         in order to get better river flows for 
    !Config         particular regions.
    !Config Units = [days]
    !
    IF ( hydrol_cwrr ) THEN
       fast_tcst = fast_tcst_cwrr
    ELSE
       fast_tcst = fast_tcst_chois
    ENDIF
    CALL getin_p('FAST_TCST', fast_tcst)
    !
    !Config Key   = STREAM_TCST
    !Config Desc  = Time constant for the stream reservoir 
    !Config If    = RIVER_ROUTING
    !Config Def   = stream_tcst_cwrr or stream_tcst_chois depending on flag HYDROL_CWRR
    !Config Help  = This parameters allows the user to fix the 
    !Config         time constant (in days) of the stream reservoir
    !Config         in order to get better river flows for 
    !Config         particular regions.
    !Config Units = [days]
    !
    IF ( hydrol_cwrr ) THEN
       stream_tcst = stream_tcst_cwrr
    ELSE
       stream_tcst = stream_tcst_chois
    ENDIF
    CALL getin_p('STREAM_TCST', stream_tcst)
    !
    !Config Key   = FLOOD_TCST
    !Config Desc  = Time constant for the flood reservoir 
    !Config If    = RIVER_ROUTING
    !Config Def   = 4.0
    !Config Help  = This parameters allows the user to fix the 
    !Config         time constant (in days) of the flood reservoir
    !Config         in order to get better river flows for 
    !Config         particular regions.
    !Config Units = [days]
    !
    IF ( hydrol_cwrr ) THEN
       flood_tcst = flood_tcst_cwrr
    ELSE
       flood_tcst = flood_tcst_chois
    ENDIF
    CALL getin_p('FLOOD_TCST', flood_tcst)
    !
    !Config Key   = SWAMP_CST
    !Config Desc  = Fraction of the river that flows back to swamps 
    !Config If    = RIVER_ROUTING
    !Config Def   = 0.2
    !Config Help  = This parameters allows the user to fix the 
    !Config         fraction of the river transport
    !Config         that flows to swamps
    !Config Units = [-]
    !
    IF ( hydrol_cwrr ) THEN
       swamp_cst = swamp_cst_cwrr
    ELSE
       swamp_cst = swamp_cst_chois
    ENDIF
    CALL getin_p('SWAMP_CST', swamp_cst)
    !
    !Config Key   = FLOOD_BETA
    !Config Desc  = Parameter to fix the shape of the floodplain  
    !Config If    = RIVER_ROUTING
    !Config Def   = 2.0
    !Config Help  = Parameter to fix the shape of the floodplain
    !Config         (>1 for convex edges, <1 for concave edges)
    !Config Units = [-] 
    CALL getin_p("FLOOD_BETA", beta)
    !
    !Config Key   = POND_BETAP
    !Config Desc  = Ratio of the basin surface intercepted by ponds and the maximum surface of ponds
    !Config If    = RIVER_ROUTING
    !Config Def   = 0.5
    !Config Help  = 
    !Config Units = [-] 
    CALL getin_p("POND_BETAP", betap)    
    !
    !Config Key   = FLOOD_CRI
    !Config Desc  = Potential height for which all the basin is flooded
    !Config If    = DO_FLOODPLAINS or DO_PONDS
    !Config Def   = 2000.
    !Config Help  = 
    !Config Units = [mm] 
    CALL getin_p("FLOOD_CRI", floodcri)
    !
    !Config Key   = POND_CRI
    !Config Desc  = Potential height for which all the basin is a pond
    !Config If    = DO_FLOODPLAINS or DO_PONDS
    !Config Def   = 2000.
    !Config Help  = 
    !Config Units = [mm] 
    CALL getin_p("POND_CRI", pondcri)

    !
    !
    ! In order to simplify the time cascade check that dt_routing
    ! is a multiple of dt_sechiba
    !
    ratio = dt_routing/dt_sechiba
    IF ( ABS(NINT(ratio) - ratio) .GT. 10*EPSILON(ratio)) THEN
       WRITE(numout,*) 'WARNING -- WARNING -- WARNING -- WARNING'
       WRITE(numout,*) "The chosen time step for the routing is not a multiple of the"
       WRITE(numout,*) "main time step of the model. We will change dt_routing so that"
       WRITE(numout,*) "this condition os fulfilled"
       dt_routing = NINT(ratio) * dt_sechiba
       WRITE(numout,*) 'THE NEW DT_ROUTING IS : ', dt_routing
    ENDIF
    !
    IF ( dt_routing .LT. dt_sechiba) THEN
       WRITE(numout,*) 'WARNING -- WARNING -- WARNING -- WARNING'
       WRITE(numout,*) 'The routing timestep can not be smaller than the one'
       WRITE(numout,*) 'of the model. We reset its value to the model''s timestep.'
       WRITE(numout,*) 'The old DT_ROUTING is : ', dt_routing
       dt_routing = dt_sechiba
       WRITE(numout,*) 'THE NEW DT_ROUTING IS : ', dt_routing
    ENDIF
    !
    var_name ="routingcounter"
    IF (is_root_prc) THEN
       CALL ioconf_setatt('UNITS', 's')
       CALL ioconf_setatt('LONG_NAME','Time counter for the routing scheme')
       CALL restget (rest_id, var_name, 1, 1, 1, kjit, .TRUE., tmp_day)
       IF (tmp_day(1) == val_exp) THEN
          time_counter = zero
       ELSE
          time_counter = tmp_day(1) 
       ENDIF
       CALL setvar (time_counter, val_exp, 'NO_KEYWORD', zero)
    ENDIF
    CALL bcast(time_counter)
!!$    CALL setvar_p (time_counter, val_exp, 'NO_KEYWORD', zero)

    
    ALLOCATE (routing_area_loc(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for routing_area_loc','','')

    ALLOCATE (routing_area_glo(nbp_glo,nbasmax))
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for routing_area_glo','','')
    var_name = 'routingarea'
    IF (is_root_prc) THEN
       CALL ioconf_setatt('UNITS', 'm^2')
       CALL ioconf_setatt('LONG_NAME','Area of basin')
       CALL restget (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., routing_area_glo, "gather", nbp_glo, index_g)
    ENDIF
    CALL scatter(routing_area_glo,routing_area_loc)
    routing_area=>routing_area_loc

    ALLOCATE (tmp_real_g(nbp_glo,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for tmp_real_g','','')

    ALLOCATE (route_togrid_loc(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for route_togrid_loc','','')
    ALLOCATE (route_togrid_glo(nbp_glo,nbasmax), stat=ier)      ! used in global in routing_flow
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for route_togrid_glo','','')

    IF (is_root_prc) THEN
       var_name = 'routetogrid'
       CALL ioconf_setatt('UNITS', '-')
       CALL ioconf_setatt('LONG_NAME','Grid into which the basin flows')
       CALL restget (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., tmp_real_g, "gather", nbp_glo, index_g)
       route_togrid_glo(:,:) = undef_int
       WHERE ( tmp_real_g .LT. val_exp )
          route_togrid_glo = NINT(tmp_real_g)
    ENDWHERE
    ENDIF
    CALL bcast(route_togrid_glo)                      ! used in global in routing_flow
    CALL scatter(route_togrid_glo,route_togrid_loc)
    route_togrid=>route_togrid_loc
    !
    ALLOCATE (route_tobasin_loc(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for route_tobasin_loc','','')

    ALLOCATE (route_tobasin_glo(nbp_glo,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for route_tobasin_glo','','')

    IF (is_root_prc) THEN
       var_name = 'routetobasin'
       CALL ioconf_setatt('UNITS', '-')
       CALL ioconf_setatt('LONG_NAME','Basin in to which the water goes')
       CALL restget (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., tmp_real_g, "gather", nbp_glo, index_g)
       route_tobasin_glo = undef_int
       WHERE ( tmp_real_g .LT. val_exp )
         route_tobasin_glo = NINT(tmp_real_g)
      ENDWHERE
    ENDIF
    CALL scatter(route_tobasin_glo,route_tobasin_loc)
    route_tobasin=>route_tobasin_loc
    !
    ! nbintobasin
    !
    ALLOCATE (route_nbintobas_loc(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for route_nbintobas_loc','','')
    ALLOCATE (route_nbintobas_glo(nbp_glo,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for route_nbintobas_glo','','')

    IF (is_root_prc) THEN
       var_name = 'routenbintobas'
       CALL ioconf_setatt('UNITS', '-')
       CALL ioconf_setatt('LONG_NAME','Number of basin into current one')
       CALL restget (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., tmp_real_g, "gather", nbp_glo, index_g)
       route_nbintobas_glo = undef_int
       WHERE ( tmp_real_g .LT. val_exp )
         route_nbintobas_glo = NINT(tmp_real_g)
      ENDWHERE
    ENDIF
    CALL scatter(route_nbintobas_glo,route_nbintobas_loc)
    route_nbintobas=>route_nbintobas_loc
    !
    ALLOCATE (global_basinid_loc(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for global_basinid_loc','','')
    ALLOCATE (global_basinid_glo(nbp_glo,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for global_basinid_glo','','')

    IF (is_root_prc) THEN
       var_name = 'basinid'
       CALL ioconf_setatt('UNITS', '-')
       CALL ioconf_setatt('LONG_NAME','ID of basin')
       CALL restget (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., tmp_real_g, "gather", nbp_glo, index_g)
       global_basinid_glo = undef_int
       WHERE ( tmp_real_g .LT. val_exp )
          global_basinid_glo = NINT(tmp_real_g)
       ENDWHERE
    ENDIF
    CALL scatter(global_basinid_glo,global_basinid_loc)
    global_basinid=>global_basinid_loc
    !
    ALLOCATE (topo_resid_loc(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for topo_resid_loc','','')
    ALLOCATE (topo_resid_glo(nbp_glo,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for topo_resid_glo','','')

    IF (is_root_prc) THEN
       var_name = 'topoindex'
       CALL ioconf_setatt('UNITS', 'm')
       CALL ioconf_setatt('LONG_NAME','Topographic index of the residence time')
       CALL restget (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., topo_resid_glo, "gather", nbp_glo, index_g)
    ENDIF
    CALL scatter(topo_resid_glo,topo_resid_loc)
    topo_resid=>topo_resid_loc

    ALLOCATE (fast_reservoir(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for fast_reservoir','','')
    var_name = 'fastres'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Water in the fast reservoir')
    CALL restget_p (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., fast_reservoir, "gather", nbp_glo, index_g)
    CALL setvar_p (fast_reservoir, val_exp, 'NO_KEYWORD', zero)

    ALLOCATE (slow_reservoir(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for slow_reservoir','','')
    var_name = 'slowres'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Water in the slow reservoir')
    CALL restget_p (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., slow_reservoir, "gather", nbp_glo, index_g)
    CALL setvar_p (slow_reservoir, val_exp, 'NO_KEYWORD', zero)

    ALLOCATE (stream_reservoir(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for stream_reservoir','','')
    var_name = 'streamres'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Water in the stream reservoir')
    CALL restget_p (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., stream_reservoir, "gather", nbp_glo, index_g)
    CALL setvar_p (stream_reservoir, val_exp, 'NO_KEYWORD', zero)

    ALLOCATE (flood_reservoir(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for flood_reservoir','','')
    var_name = 'floodres'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Water in the flood reservoir')
    CALL restget_p (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., flood_reservoir, "gather", nbp_glo, index_g)
    CALL setvar_p (flood_reservoir, val_exp, 'NO_KEYWORD', zero)

    ALLOCATE (flood_frac_bas(nbpt,nbasmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for flood_frac_bas','','')
    var_name = 'flood_frac_bas'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Flooded fraction per basin')
    CALL restget_p (rest_id, var_name, nbp_glo, nbasmax, 1, kjit, .TRUE., flood_frac_bas, "gather", nbp_glo, index_g)
    CALL setvar_p (flood_frac_bas, val_exp, 'NO_KEYWORD', zero)

    ALLOCATE (flood_height(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for flood_height','','')
    var_name = 'floodh'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., flood_height, "gather", nbp_glo, index_g)
    CALL setvar_p (flood_height, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE (pond_frac(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for pond_frac','','')
    var_name = 'pond_frac'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Pond fraction per grid box')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., pond_frac, "gather", nbp_glo, index_g)
    CALL setvar_p (pond_frac, val_exp, 'NO_KEYWORD', zero)
    
    var_name = 'flood_frac'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Flooded fraction per grid box')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., flood_frac, "gather", nbp_glo, index_g)
    CALL setvar_p (flood_frac, val_exp, 'NO_KEYWORD', zero)
    
    var_name = 'flood_res'
    CALL ioconf_setatt_p('UNITS','mm')
    CALL ioconf_setatt_p('LONG_NAME','Flooded quantity (estimation)')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., flood_res, "gather", nbp_glo, index_g)
    CALL setvar_p (flood_res, val_exp, 'NO_KEYWORD', zero)
!    flood_res = zero
    
    ALLOCATE (lake_reservoir(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for lake_reservoir','','')
    var_name = 'lakeres'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Water in the lake reservoir')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., lake_reservoir, "gather", nbp_glo, index_g)
    CALL setvar_p (lake_reservoir, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE (pond_reservoir(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for pond_reservoir','','')
    var_name = 'pondres'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Water in the pond reservoir')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., pond_reservoir, "gather", nbp_glo, index_g)
    CALL setvar_p (pond_reservoir, val_exp, 'NO_KEYWORD', zero)
    !
    ! Map of irrigated areas
    !
    IF ( do_irrigation ) THEN
       ALLOCATE (irrigated(nbpt), stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for irrigated','','')
       var_name = 'irrigated'
       CALL ioconf_setatt_p('UNITS', 'm^2')
       CALL ioconf_setatt_p('LONG_NAME','Surface of irrigated area')
       CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., irrigated, "gather", nbp_glo, index_g)
       CALL setvar_p (irrigated, val_exp, 'NO_KEYWORD', undef_sechiba)
    ENDIF
    
    IF ( do_floodplains ) THEN
       ALLOCATE (floodplains(nbpt), stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for floodplains','','')
       var_name = 'floodplains'
       CALL ioconf_setatt_p('UNITS', 'm^2')
       CALL ioconf_setatt_p('LONG_NAME','Surface which can be flooded')
       CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., floodplains, "gather", nbp_glo, index_g)
       CALL setvar_p (floodplains, val_exp, 'NO_KEYWORD', undef_sechiba)
    ENDIF
    IF ( doswamps ) THEN
       ALLOCATE (swamp(nbpt), stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for swamp','','')
       var_name = 'swamp'
       CALL ioconf_setatt_p('UNITS', 'm^2')
       CALL ioconf_setatt_p('LONG_NAME','Surface which can become swamp')
       CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., swamp, "gather", nbp_glo, index_g)
       CALL setvar_p (swamp, val_exp, 'NO_KEYWORD', undef_sechiba)
    ENDIF
    !
    ! Put into the restart file the fluxes so that they can be regenerated at restart.
    !
    ALLOCATE (lakeinflow_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for lakeinflow_mean','','')
    var_name = 'lakeinflow'
    CALL ioconf_setatt_p('UNITS', 'Kg/dt')
    CALL ioconf_setatt_p('LONG_NAME','Lake inflow')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., lakeinflow_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (lakeinflow_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE (returnflow_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for returnflow_mean','','')
    var_name = 'returnflow'
    CALL ioconf_setatt_p('UNITS', 'Kg/m^2/dt')
    CALL ioconf_setatt_p('LONG_NAME','Deep return flux')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., returnflow_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (returnflow_mean, val_exp, 'NO_KEYWORD', zero)
    returnflow(:) = returnflow_mean(:)
    
    ALLOCATE (reinfiltration_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for reinfiltration_mean','','')
    var_name = 'reinfiltration'
    CALL ioconf_setatt_p('UNITS', 'Kg/m^2/dt')
    CALL ioconf_setatt_p('LONG_NAME','Top return flux')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., reinfiltration_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (reinfiltration_mean, val_exp, 'NO_KEYWORD', zero)
    reinfiltration(:) = reinfiltration_mean(:)
    
    ALLOCATE (irrigation_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for irrigation_mean','','')
    ALLOCATE (irrig_netereq(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for irrig_netereq','','')
    irrig_netereq(:) = zero
    
    IF ( do_irrigation ) THEN
       var_name = 'irrigation'
       CALL ioconf_setatt_p('UNITS', 'Kg/dt')
       CALL ioconf_setatt_p('LONG_NAME','Artificial irrigation flux')
       CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., irrigation_mean, "gather", nbp_glo, index_g)
       CALL setvar_p (irrigation_mean, val_exp, 'NO_KEYWORD', zero)
    ELSE
       irrigation_mean(:) = zero
    ENDIF
    irrigation(:) = irrigation_mean(:) 
    
    ALLOCATE (riverflow_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for riverflow_mean','','')
    var_name = 'riverflow'
    CALL ioconf_setatt_p('UNITS', 'Kg/dt')
    CALL ioconf_setatt_p('LONG_NAME','River flux into the sea')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., riverflow_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (riverflow_mean, val_exp, 'NO_KEYWORD', zero)
    riverflow(:) = riverflow_mean(:)
    
    ALLOCATE (coastalflow_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for coastalflow_mean','','')
    var_name = 'coastalflow'
    CALL ioconf_setatt_p('UNITS', 'Kg/dt')
    CALL ioconf_setatt_p('LONG_NAME','Diffuse flux into the sea')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., coastalflow_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (coastalflow_mean, val_exp, 'NO_KEYWORD', zero)
    coastalflow(:) = coastalflow_mean(:)
    
    ! Locate it at the 2m level
    ipn = MINLOC(ABS(diaglev-2))
    floodtemp_lev = ipn(1)
    ALLOCATE (floodtemp(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for floodtemp','','')
    floodtemp(:) = stempdiag(:,floodtemp_lev)
    
    ALLOCATE(hydrographs(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for hydrographs','','')
    var_name = 'hydrographs'
    CALL ioconf_setatt_p('UNITS', 'm^3/dt')
    CALL ioconf_setatt_p('LONG_NAME','Hydrograph at outlow of grid')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., hydrographs, "gather", nbp_glo, index_g)
    CALL setvar_p (hydrographs, val_exp, 'NO_KEYWORD', zero)
 
    ALLOCATE(slowflow_diag(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for slowflow_diag','','')
    var_name = 'slowflow_diag'
    CALL ioconf_setatt_p('UNITS', 'm^3/dt')
    CALL ioconf_setatt_p('LONG_NAME','Slowflow hydrograph at outlow of grid')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE.,slowflow_diag, "gather", nbp_glo, index_g)
    CALL setvar_p (slowflow_diag, val_exp, 'NO_KEYWORD', zero)

    !
    ! The diagnostic variables, they are initialized from the above restart variables.
    !
    ALLOCATE(fast_diag(nbpt), slow_diag(nbpt), stream_diag(nbpt), flood_diag(nbpt), &
         & pond_diag(nbpt), lake_diag(nbpt), delsurfstor(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for fast_diag,..','','')
    
    fast_diag(:) = zero
    slow_diag(:) = zero
    stream_diag(:) = zero
    flood_diag(:) = zero
    pond_diag(:) = zero
    lake_diag(:) = zero
    delsurfstor(:) = zero
    
    DO ig=1,nbpt
       totarea = zero
       DO ib=1,nbasmax
          totarea = totarea + routing_area(ig,ib)
          fast_diag(ig) = fast_diag(ig) + fast_reservoir(ig,ib)
          slow_diag(ig) = slow_diag(ig) + slow_reservoir(ig,ib)
          stream_diag(ig) = stream_diag(ig) + stream_reservoir(ig,ib)
          flood_diag(ig) = flood_diag(ig) + flood_reservoir(ig,ib)
       ENDDO
       !
       fast_diag(ig) = fast_diag(ig)/totarea
       slow_diag(ig) = slow_diag(ig)/totarea
       stream_diag(ig) = stream_diag(ig)/totarea
       flood_diag(ig) = flood_diag(ig)/totarea
       !
       ! This is the volume of the lake scaled to the entire grid.
       ! It would be batter to scale it to the size of the lake
       ! but this information is not yet available.
       !
       lake_diag(ig) = lake_reservoir(ig)/totarea
       !
    ENDDO
    !
    ! Get from the restart the fluxes we accumulated.
    !
    ALLOCATE (floodout_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for floodout_mean','','')
    var_name = 'floodout_route'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Accumulated flow out of floodplains for routing')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., floodout_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (floodout_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE (runoff_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for runoff_mean','','')
    var_name = 'runoff_route'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Accumulated runoff for routing')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., runoff_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (runoff_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE(drainage_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for drainage_mean','','')
    var_name = 'drainage_route'
    CALL ioconf_setatt_p('UNITS', 'Kg')
    CALL ioconf_setatt_p('LONG_NAME','Accumulated drainage for routing')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., drainage_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (drainage_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE(transpot_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for transpot_mean','','')
    var_name = 'transpot_route'
    CALL ioconf_setatt_p('UNITS', 'Kg/m^2')
    CALL ioconf_setatt_p('LONG_NAME','Accumulated potential transpiration for routing/irrigation')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., transpot_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (transpot_mean, val_exp, 'NO_KEYWORD', zero)

    ALLOCATE(precip_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for precip_mean','','')
    var_name = 'precip_route'
    CALL ioconf_setatt_p('UNITS', 'Kg/m^2')
    CALL ioconf_setatt_p('LONG_NAME','Accumulated rain precipitation for irrigation')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., precip_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (precip_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE(humrel_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for humrel_mean','','')
    var_name = 'humrel_route'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Mean humrel for irrigation')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., humrel_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (humrel_mean, val_exp, 'NO_KEYWORD', un)
    
    ALLOCATE(k_litt_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for k_litt_mean','','')
    var_name = 'k_litt_route'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Mean cond. for litter')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., k_litt_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (k_litt_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE(totnobio_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for totnobio_mean','','')
    var_name = 'totnobio_route'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Last Total fraction of no bio for irrigation')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., totnobio_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (totnobio_mean, val_exp, 'NO_KEYWORD', zero)
    
    ALLOCATE(vegtot_mean(nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for vegtot_mean','','')
    var_name = 'vegtot_route'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Last Total fraction of vegetation')
    CALL restget_p (rest_id, var_name, nbp_glo, 1, 1, kjit, .TRUE., vegtot_mean, "gather", nbp_glo, index_g)
    CALL setvar_p (vegtot_mean, val_exp, 'NO_KEYWORD', un)
    !
    !
    DEALLOCATE(tmp_real_g)
    !
    ! Allocate diagnostic variables
    !
    ALLOCATE(hydrodiag_loc(nbpt,nbasmax),hydrodiag_glo(nbp_glo,nbasmax),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for hydrodiag_glo','','')
    hydrodiag=>hydrodiag_loc

    ALLOCATE(hydroupbasin_loc(nbpt),hydroupbasin_glo(nbp_glo), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_init','Pb in allocate for hydroupbasin_glo','','')
    hydroupbasin=>hydroupbasin_loc

  END SUBROUTINE routing_init
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_clear
!!
!>\BRIEF        : This subroutine deallocates the block memory previously allocated.
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_clear()
    !
    l_first_routing=.TRUE.
    !
    IF (ALLOCATED(routing_area_loc)) DEALLOCATE(routing_area_loc)
    IF (ALLOCATED(route_togrid_loc)) DEALLOCATE(route_togrid_loc)
    IF (ALLOCATED(route_tobasin_loc)) DEALLOCATE(route_tobasin_loc)
    IF (ALLOCATED(route_nbintobas_loc)) DEALLOCATE(route_nbintobas_loc)
    IF (ALLOCATED(global_basinid_loc)) DEALLOCATE(global_basinid_loc)
    IF (ALLOCATED(topo_resid_loc)) DEALLOCATE(topo_resid_loc)
    IF (ALLOCATED(routing_area_glo)) DEALLOCATE(routing_area_glo)
    IF (ALLOCATED(route_togrid_glo)) DEALLOCATE(route_togrid_glo)
    IF (ALLOCATED(route_tobasin_glo)) DEALLOCATE(route_tobasin_glo)
    IF (ALLOCATED(route_nbintobas_glo)) DEALLOCATE(route_nbintobas_glo)
    IF (ALLOCATED(global_basinid_glo)) DEALLOCATE(global_basinid_glo)
    IF (ALLOCATED(topo_resid_glo)) DEALLOCATE(topo_resid_glo)
    IF (ALLOCATED(fast_reservoir)) DEALLOCATE(fast_reservoir)
    IF (ALLOCATED(slow_reservoir)) DEALLOCATE(slow_reservoir)
    IF (ALLOCATED(stream_reservoir)) DEALLOCATE(stream_reservoir)
    IF (ALLOCATED(flood_reservoir)) DEALLOCATE(flood_reservoir)
    IF (ALLOCATED(flood_frac_bas)) DEALLOCATE(flood_frac_bas)
    IF (ALLOCATED(flood_height)) DEALLOCATE(flood_height)
    IF (ALLOCATED(pond_frac)) DEALLOCATE(pond_frac)
    IF (ALLOCATED(lake_reservoir)) DEALLOCATE(lake_reservoir)
    IF (ALLOCATED(pond_reservoir)) DEALLOCATE(pond_reservoir)
    IF (ALLOCATED(returnflow_mean)) DEALLOCATE(returnflow_mean)
    IF (ALLOCATED(reinfiltration_mean)) DEALLOCATE(reinfiltration_mean)
    IF (ALLOCATED(riverflow_mean)) DEALLOCATE(riverflow_mean)
    IF (ALLOCATED(coastalflow_mean)) DEALLOCATE(coastalflow_mean)
    IF (ALLOCATED(lakeinflow_mean)) DEALLOCATE(lakeinflow_mean)
    IF (ALLOCATED(runoff_mean)) DEALLOCATE(runoff_mean)
    IF (ALLOCATED(floodout_mean)) DEALLOCATE(floodout_mean)
    IF (ALLOCATED(drainage_mean)) DEALLOCATE(drainage_mean)
    IF (ALLOCATED(transpot_mean)) DEALLOCATE(transpot_mean)
    IF (ALLOCATED(precip_mean)) DEALLOCATE(precip_mean)
    IF (ALLOCATED(humrel_mean)) DEALLOCATE(humrel_mean)
    IF (ALLOCATED(k_litt_mean)) DEALLOCATE(k_litt_mean)
    IF (ALLOCATED(totnobio_mean)) DEALLOCATE(totnobio_mean)
    IF (ALLOCATED(vegtot_mean)) DEALLOCATE(vegtot_mean)
    IF (ALLOCATED(floodtemp)) DEALLOCATE(floodtemp)
    IF (ALLOCATED(hydrodiag_loc)) DEALLOCATE(hydrodiag_loc)
    IF (ALLOCATED(hydrodiag_glo)) DEALLOCATE(hydrodiag_glo)
    IF (ALLOCATED(hydroupbasin_loc)) DEALLOCATE(hydroupbasin_loc)    
    IF (ALLOCATED(hydroupbasin_glo)) DEALLOCATE(hydroupbasin_glo)
    IF (ALLOCATED(hydrographs)) DEALLOCATE(hydrographs)
    IF (ALLOCATED(slowflow_diag)) DEALLOCATE(slowflow_diag)
    IF (ALLOCATED(irrigation_mean)) DEALLOCATE(irrigation_mean)
    IF (ALLOCATED(irrigated)) DEALLOCATE(irrigated)
    IF (ALLOCATED(floodplains)) DEALLOCATE(floodplains)
    IF (ALLOCATED(swamp)) DEALLOCATE(swamp)
    IF (ALLOCATED(fast_diag)) DEALLOCATE(fast_diag)
    IF (ALLOCATED(slow_diag)) DEALLOCATE(slow_diag)
    IF (ALLOCATED(stream_diag)) DEALLOCATE(stream_diag)
    IF (ALLOCATED(flood_diag)) DEALLOCATE(flood_diag)
    IF (ALLOCATED(pond_diag)) DEALLOCATE(pond_diag)
    IF (ALLOCATED(lake_diag)) DEALLOCATE(lake_diag)
    IF (ALLOCATED(delsurfstor)) DEALLOCATE(delsurfstor)
    !
  END SUBROUTINE routing_clear
  !

!! ================================================================================================================================
!! SUBROUTINE 	: routing_flow
!!
!>\BRIEF         This subroutine computes the transport of water in the various reservoirs
!!                (including ponds and floodplains) and the water withdrawals from the reservoirs for irrigation.
!!
!! DESCRIPTION (definitions, functional, design, flags) :
!! This will first compute the amount of water which flows out of each of the 3 reservoirs using the assumption of an 
!! exponential decrease of water in the reservoir (see Hagemann S and Dumenil L. (1998)). Then we compute the fluxes 
!! for floodplains and ponds. All this will then be used in order to update each of the basins : taking water out of 
!! the up-stream basin and adding it to the down-stream one.
!! As this step happens globaly we have to stop the parallel processing in order to exchange the information. Once 
!! all reservoirs are updated we deal with irrigation. The final step is to compute diagnostic fluxes. Among them
!! the hydrographs of the largest rivers we have chosen to monitor.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): lakeinflow, returnflow, reinfiltration, irrigation, riverflow, coastalflow, hydrographs, flood_frac, flood_res
!!
!! REFERENCES   :
!! - Ngo-Duc, T., K. Laval, G. Ramillien, J. Polcher, and A. Cazenave (2007)
!!   Validation of the land water storage simulated by Organising Carbon and Hydrology in Dynamic Ecosystems (ORCHIDEE) with Gravity Recovery and Climate Experiment (GRACE) data.
!!   Water Resour. Res., 43, W04427, doi:10.1029/2006WR004941.
!! * Irrigation:
!! - de Rosnay, P., J. Polcher, K. Laval, and M. Sabre (2003)
!!   Integrated parameterization of irrigation in the land surface model ORCHIDEE. Validation over Indian Peninsula.
!!   Geophys. Res. Lett., 30(19), 1986, doi:10.1029/2003GL018024.
!! - A.C. Vivant (2003)
!!   Les plaines d'inondations et l'irrigation dans ORCHIDEE, impacts de leur prise en compte.
!!   , , 51pp.
!! - N. Culson (2004)
!!   Impact de l'irrigation sur le cycle de l'eau
!!   Master thesis, Paris VI University, 55pp.
!! - X.-T. Nguyen-Vinh (2005)
!!   Analyse de l'impact de l'irrigation en Amerique du Nord - plaine du Mississippi - sur la climatologie regionale
!!   Master thesis, Paris VI University, 33pp.
!! - M. Guimberteau (2006)
!!   Analyse et modifications proposees de la modelisation de l'irrigation dans un modele de surface.
!!   Master thesis, Paris VI University, 46pp.
!! - Guimberteau M. (2010)
!!   Modelisation de l'hydrologie continentale et influences de l'irrigation sur le cycle de l'eau.
!!   Ph.D. thesis, Paris VI University, 195pp.
!! - Guimberteau M., Laval K., Perrier A. and Polcher J. (2011).
!!   Global effect of irrigation and its impact on the onset of the Indian summer monsoon.
!!   In press, Climate Dynamics, doi: 10.1007/s00382-011-1252-5.
!! * Floodplains:
!! - A.C. Vivant (2002)
!!   L'ecoulement lateral de l'eau sur les surfaces continentales. Prise en compte des plaines d'inondations dans ORCHIDEE.
!!   Master thesis, Paris VI University, 46pp.
!! - A.C. Vivant (2003)
!!   Les plaines d'inondations et l'irrigation dans ORCHIDEE, impacts de leur prise en compte.
!!   , , 51pp.
!! - T. d'Orgeval (2006)
!!   Impact du changement climatique sur le cycle de l'eau en Afrique de l'Ouest: modelisation et incertitudes.
!!   Ph.D. thesis, Paris VI University, 188pp.
!! - T. d'Orgeval, J. Polcher, and P. de Rosnay (2008)
!!   Sensitivity of the West African hydrological cycle in ORCHIDEE to infiltration processes.
!!   Hydrol. Earth Syst. Sci., 12, 1387-1401
!! - M. Guimberteau, G. Drapeau, J. Ronchail, B. Sultan, J. Polcher, J.-M. Martinez, C. Prigent, J.-L. Guyot, G. Cochonneau,
!!   J. C. Espinoza, N. Filizola, P. Fraizy, W. Lavado, E. De Oliveira, R. Pombosa, L. Noriega, and P. Vauchel (2011)
!!   Discharge simulation in the sub-basins of the Amazon using ORCHIDEE forced by new datasets.
!!   Hydrol. Earth Syst. Sci. Discuss., 8, 11171-11232, doi:10.5194/hessd-8-11171-2011
!!
!! FLOWCHART    :None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_flow(nbpt, dt_routing, lalo, floodout, runoff, drainage, &
       &                  vegtot, totnobio, transpot_mean, transpot, evapot_corr, veget, veget_max, soil_deficit, &
       &                  precip, humrel, k_litt, floodtemp, reinf_slope, &
       &                  lakeinflow, returnflow, reinfiltration, irrigation, riverflow, &
       &                  coastalflow, hydrographs, slowflow_diag, flood_frac, flood_res, vegstress, &
       &                  irrigr, do_irrig_reservoir, irrig_dep_threshold, reservoir_dep_max, do_awd, awd_dep, lai)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                   :: nbpt                      !! Domain size (unitless)
    REAL(r_std), INTENT (in)                     :: dt_routing                !! Routing time step (s)
    REAL(r_std), INTENT(in)                      :: lalo(nbpt,2)              !! Vector of latitude and longitudes
    REAL(r_std), INTENT(in)                      :: runoff(nbpt)              !! Grid-point runoff (kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: floodout(nbpt)            !! Grid-point flow out of floodplains (kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: drainage(nbpt)            !! Grid-point drainage (kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: vegtot(nbpt)              !! Potentially vegetated fraction (unitless;0-1)
    REAL(r_std), INTENT(in)                      :: totnobio(nbpt)            !! Other areas which can not have vegetation
    REAL(r_std), INTENT(in)                      :: transpot_mean(nbpt)       !! Mean potential transpiration of the vegetation (kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: transpot(nbpt,nvm)        !! potential transpiration of each pft(kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: evapot_corr(nbpt)        !! potential soil evaporation(kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: veget(nbpt,nvm)       !! vegetation fraction of each pft (unitless;0-1)
    REAL(r_std), INTENT(in)                      :: veget_max(nbpt,nvm)       !! maximum vegetation fraction of each pft (unitless;0-1)
    REAL(r_std), INTENT(in)                      :: lai(nbpt,nvm)             !! leaf area index of each pft (unitless;0-1)
    REAL(r_std), INTENT(in)                      :: soil_deficit(nbpt,nvm)    !!
    REAL(r_std), INTENT(in)                      :: precip(nbpt)              !! Rainfall (kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: humrel(nbpt)              !! Soil moisture stress, root extraction potential (unitless)
    REAL(r_std), INTENT(in)                      :: k_litt(nbpt)              !! Averaged conductivity for saturated infiltration in the 'litter' layer (kg/m^2/dt)
    REAL(r_std), INTENT(in)                      :: floodtemp(nbpt)           !! Temperature to decide if floodplains work (K)
    REAL(r_std), INTENT(in)                      :: reinf_slope(nbpt)         !! Coefficient which determines the reinfiltration ratio in the grid box due to flat areas (unitless;0-1)
    REAL(r_std), INTENT(out)                     :: lakeinflow(nbpt)          !! Water inflow to the lakes (kg/dt)
    REAL(r_std), INTENT(in)                      :: irrigr(nbpt,nstm)         !! irrigation reservoir [kg.m-2]
    LOGICAL, INTENT (in)                         :: do_irrig_reservoir(nstm)  !! flag of irrigation reservoir for specific soil column
    REAL(r_std), INTENT (in)                     :: irrig_dep_threshold(nstm) !! threshold of depth when irrigation occurs
    REAL(r_std), INTENT (in)                     :: reservoir_dep_max(nstm)   !! maximum depth of irrigation reservoir
    LOGICAL, INTENT (in)                         :: do_awd(nstm)              !! if do awd irrigation
    REAL(r_std), INTENT (in)                     :: awd_dep(nstm)             !! water amount (in depth) of one irrigation event
    !
!! OUTPUT VARIABLES
    REAL(r_std), INTENT(out)                     :: returnflow(nbpt)          !! The water flow from lakes and swamps which returns into the grid box.
                                                                              !! This water will go back into the hydrol or hydrolc module to allow re-evaporation (kg/m^2/dt)
    REAL(r_std), INTENT(out)                     :: reinfiltration(nbpt)      !! Water flow from ponds and floodplains which returns to the grid box (kg/m^2/dt)
    REAL(r_std), INTENT(out)                     :: irrigation(nbpt)          !! Irrigation flux. This is the water taken from the reservoirs and beeing put into the upper layers of the soil (kg/m^2/dt)
    REAL(r_std), INTENT(out)                     :: riverflow(nbpt)           !! Outflow of the major rivers. The flux will be located on the continental grid but this should be a coastal point (kg/dt)
    REAL(r_std), INTENT(out)                     :: coastalflow(nbpt)         !! Outflow on coastal points by small basins. This is the water which flows in a disperse way into the ocean (kg/dt)
    REAL(r_std), INTENT(out)                     :: hydrographs(nbpt)         !! Hydrographs at the outflow of the grid box for major basins (kg/dt)
    REAL(r_std), INTENT(out)                     :: slowflow_diag(nbpt)       !! Hydrographs of slow_flow = routed slow_flow for major basins (kg/dt)
    REAL(r_std), INTENT(out)                     :: flood_frac(nbpt)          !! Flooded fraction of the grid box (unitless;0-1)
    REAL(r_std), INTENT(out)                     :: flood_res(nbpt)           !! Diagnostic of water amount in the floodplains reservoir (kg)
    REAL(r_std), INTENT(in)                      :: vegstress(nbpt,nvm)       !! vegetation growth stress
    !
!! LOCAL VARIABLES
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: fast_flow                 !! Outflow from the fast reservoir (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: slow_flow                 !! Outflow from the slow reservoir (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: stream_flow               !! Outflow from the stream reservoir (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: flood_flow                !! Outflow from the floodplain reservoir (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: pond_inflow               !! Inflow to the pond reservoir (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: pond_drainage             !! Drainage from pond (kg/m^2/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: flood_drainage            !! Drainage from floodplains (kg/m^2/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: return_swamp              !! Inflow to the swamp (kg/dt)
    !
    ! Irrigation per basin
    !
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: irrig_needs               !! Total irrigation requirement (water requirements by the crop for its optimal growth) (kg)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: irrig_actual              !! Possible irrigation according to the water availability in the reservoirs (kg)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: irrig_deficit             !! Amount of water missing for irrigation (kg)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: irrig_adduct              !! Amount of water carried over from other basins for irrigation (kg)
    !
    REAL(r_std), DIMENSION(nbpt, 0:nbasmax+3)    :: transport                 !! Water transport between basins (kg/dt)
    REAL(r_std), DIMENSION(nbp_glo, 0:nbasmax+3) :: transport_glo             !! Water transport between basins (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: floods                    !! Water flow in to the floodplains (kg/dt)
    REAL(r_std), DIMENSION(nbpt, nbasmax)        :: potflood                  !! Potential inflow to the swamps (kg/dt)
    REAL(r_std), DIMENSION(nbpt)                 :: tobeflooded               !! Maximal surface which can be inundated in each grid box (m^2)
    REAL(r_std), DIMENSION(nbpt)                 :: totarea                   !! Total area of basin (m^2)
    REAL(r_std), DIMENSION(nbpt)                 :: totflood                  !! Total amount of water in the floodplains reservoir (kg)
    REAL(r_std), DIMENSION(nbasmax)              :: pond_excessflow           !! 
    REAL(r_std)                                  :: flow                      !! Outflow computation for the reservoirs (kg/dt)
    REAL(r_std)                                  :: floodindex                !! Fraction of grid box area inundated (unitless;0-1)
    REAL(r_std)                                  :: pondex                    !! 
    REAL(r_std)                                  :: flood_frac_pot            !! Total fraction of the grid box which is flooded at optimum repartition (unitless;0-1)
    REAL(r_std)                                  :: stream_tot                !! Total water amount in the stream reservoirs (kg)
    REAL(r_std)                                  :: adduction                 !! Importation of water from a stream reservoir of a neighboring grid box (kg)
    REAL(r_std), DIMENSION(8,nbasmax)            :: streams_around            !! Stream reservoirs of the neighboring grid boxes (kg)
    INTEGER(i_std), DIMENSION(8)                 :: igrd                      !! 
    INTEGER(i_std), DIMENSION(2)                 :: ff                        !! 
    INTEGER(i_std), DIMENSION(1)                 :: fi                        !! 
    INTEGER(i_std)                               :: ig, ib, ib2, ig2, jv      !! Indices (unitless)
    INTEGER(i_std)                               :: rtg, rtb, in              !! Indices (unitless)
    INTEGER(i_std)                               :: ier                       !! Error handling 
    !
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: fast_flow_g               !! Outflow from the fast reservoir (kg/dt)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: slow_flow_g               !! Outflow from the slow reservoir (kg/dt)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: stream_flow_g             !! Outflow from the stream reservoir (kg/dt)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: irrig_deficit_glo         !! Amount of water missing for irrigation (kg)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: stream_reservoir_glo      !! Water amount in the stream reservoir (kg)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: irrig_adduct_glo          !! Amount of water carried over from other basins for irrigation (kg)
    !

    !! PARAMETERS
    LOGICAL, PARAMETER                           :: check_reservoir = .FALSE. !! Logical to choose if we write informations when a negative amount of water is occurring in a reservoir (true/false)

!_ ================================================================================================================================
    !
    transport(:,:) = zero
    transport_glo(:,:) = zero
    irrig_netereq(:) = zero
    irrig_needs(:,:) = zero
    irrig_actual(:,:) = zero
    irrig_deficit(:,:) = zero
    irrig_adduct(:,:) = zero
    totarea(:) = zero
    totflood(:) = zero
    !
    ! Compute all the fluxes
    !
    DO ib=1,nbasmax
       DO ig=1,nbpt
          !
          totarea(ig) = totarea(ig) + routing_area(ig,ib)
          totflood(ig) = totflood(ig) + flood_reservoir(ig,ib)
       ENDDO
    ENDDO
          !
!> The outflow fluxes from the three reservoirs are computed. 
!> The outflow of volume of water Vi into the reservoir i is assumed to be linearly related to its volume.
!> The water travel simulated by the routing scheme is dependent on the water retention index topo_resid
!> given by a 0.5 degree resolution map for each pixel performed from a simplification of Manning's formula
!> (Dingman, 1994; Ducharne et al., 2003).
!> The resulting product of tcst (in day/m) and topo_resid (in m) represents the time constant (day)
!> which is an e-folding time, the time necessary for the water amount
!> in the stream reservoir to decrease by a factor e. Hence, it gives an order of
!> magnitude of the travel time through this reservoir between
!> the sub-basin considered and its downstream neighbor.

    DO ib=1,nbasmax
       DO ig=1,nbpt
          IF ( route_tobasin(ig,ib) .GT. 0 ) THEN
             !
             ! Each of the fluxes is limited by the water in the reservoir and a small margin 
             ! (min_reservoir) to avoid rounding errors.
             !
             flow = MIN(fast_reservoir(ig,ib)/((topo_resid(ig,ib)/1000.)*fast_tcst*one_day/dt_routing),&
                  & fast_reservoir(ig,ib)-min_sechiba)
             fast_flow(ig,ib) = MAX(flow, zero)
             !
             flow = MIN(slow_reservoir(ig,ib)/((topo_resid(ig,ib)/1000.)*slow_tcst*one_day/dt_routing),&
                  & slow_reservoir(ig,ib)-min_sechiba)
             slow_flow(ig,ib) = MAX(flow, zero)
             !
             flow = MIN(stream_reservoir(ig,ib)/((topo_resid(ig,ib)/1000.)*stream_tcst* & 
                  & MAX(un-SQRT(flood_frac_bas(ig,ib)),min_sechiba)*one_day/dt_routing),&
                  & stream_reservoir(ig,ib)-min_sechiba)
             stream_flow(ig,ib) = MAX(flow, zero)
             ! 
             !
          ELSE
             fast_flow(ig,ib) = zero
             slow_flow(ig,ib) = zero
             stream_flow(ig,ib) = zero
          ENDIF
       ENDDO
    ENDDO
    !-
    !- Compute the fluxes out of the floodplains and ponds if they exist.
    !-
    IF (do_floodplains .OR. doponds) THEN
       DO ig=1,nbpt
          IF (flood_frac(ig) .GT. min_sechiba) THEN
             !
             flow = MIN(floodout(ig)*totarea(ig)*pond_frac(ig)/flood_frac(ig), pond_reservoir(ig)+totflood(ig))
             pondex = MAX(flow - pond_reservoir(ig), zero)
             pond_reservoir(ig) = pond_reservoir(ig) - (flow - pondex) 
             !
             ! If demand was over reservoir size, we will take it out from floodplains
             !
             pond_excessflow(:) = zero
             DO ib=1,nbasmax
                pond_excessflow(ib) = MIN(pondex*flood_frac_bas(ig,ib)/(flood_frac(ig)-pond_frac(ig)),&
                     &                    flood_reservoir(ig,ib))
                pondex = pondex - pond_excessflow(ib)
             ENDDO
             !
             IF ( pondex .GT. min_sechiba) THEN
                WRITE(numout,*) "Unable to redistribute the excess pond outflow over the water available in the floodplain."
                WRITE(numout,*) "Pondex = ", pondex
                WRITE(numout,*) "pond_excessflow(:) = ", pond_excessflow(:)
             ENDIF
             !
             DO ib=1,nbasmax
                !
                flow = floodout(ig)*routing_area(ig,ib)*flood_frac_bas(ig,ib)/flood_frac(ig) + pond_excessflow(ib)
                !
                flood_reservoir(ig,ib) = flood_reservoir(ig,ib) - flow
                !
                !
                IF (flood_reservoir(ig,ib) .LT. min_sechiba) THEN
                   flood_reservoir(ig,ib) = zero
                ENDIF
                IF (pond_reservoir(ig) .LT. min_sechiba) THEN
                   pond_reservoir(ig) = zero
                ENDIF
             ENDDO
          ENDIF
       ENDDO
    ENDIF

    !-
    !- Computing the drainage and outflow from floodplains
!> Drainage from floodplains is depending on a averaged conductivity (k_litt) 
!> for saturated infiltration in the 'litter' layer. Flood_drainage will be
!> a component of the total reinfiltration that leaves the routing scheme.
    !-
    IF (do_floodplains) THEN
       IF (dofloodinfilt) THEN
          DO ib=1,nbasmax
             DO ig=1,nbpt
                flood_drainage(ig,ib) = MAX(zero, MIN(flood_reservoir(ig,ib), &
                     & flood_frac(ig)*routing_area(ig,ib)*k_litt(ig)*dt_routing/one_day))
                flood_reservoir(ig,ib) = flood_reservoir(ig,ib) - flood_drainage(ig,ib)
             ENDDO
          ENDDO
       ELSE
          DO ib=1,nbasmax
             DO ig=1,nbpt
                flood_drainage(ig,ib) = zero 
             ENDDO
          ENDDO
       ENDIF
!> Outflow from floodplains is computed depending a delay. This delay is characterized by a time constant
!> function of the surface of the floodplains and the product of topo_resid and flood_tcst. flood_tcst
!> has been calibrated through observations in the Niger Inner Delta (D'Orgeval, 2006).
!
       DO ib=1,nbasmax
          DO ig=1,nbpt
             IF ( route_tobasin(ig,ib) .GT. 0 ) THEN
                IF (flood_frac_bas(ig,ib) .GT. min_sechiba) THEN
                   flow = MIN(flood_reservoir(ig,ib)  &
                        & /((topo_resid(ig,ib)/1000.)*flood_tcst* &
                        & flood_frac_bas(ig,ib)*one_day/dt_routing),&
                        & flood_reservoir(ig,ib))
                ELSE
                   flow = zero
                ENDIF
                flood_flow(ig,ib) = flow
             ELSE
                flood_flow(ig,ib) = zero
             ENDIF
          ENDDO
       ENDDO
    ELSE
       DO ib=1,nbasmax
          DO ig=1,nbpt
             flood_drainage(ig,ib) = zero
             flood_flow(ig,ib) = zero
             flood_reservoir(ig,ib) = zero
          ENDDO
       ENDDO
    ENDIF

    !-
    !- Computing drainage and inflow for ponds
!> Drainage from ponds is computed in the same way than for floodplains.
!> Reinfiltrated fraction from the runoff (i.e. the outflow from the fast reservoir)
!> is the inflow of the pond reservoir.
    !-
    IF (doponds) THEN
       ! If used, the slope coef is not used in hydrol for water2infilt
       DO ib=1,nbasmax
          DO ig=1,nbpt
             pond_inflow(ig,ib) = fast_flow(ig,ib) * reinf_slope(ig)
             pond_drainage(ig,ib) = MIN(pond_reservoir(ig)*routing_area(ig,ib)/totarea(ig), &
                  & pond_frac(ig)*routing_area(ig,ib)*k_litt(ig)*dt_routing/one_day)
             fast_flow(ig,ib) = fast_flow(ig,ib) - pond_inflow(ig,ib) 
          ENDDO
       ENDDO
    ELSE
       DO ib=1,nbasmax
          DO ig=1,nbpt
             pond_inflow(ig,ib) = zero
             pond_drainage(ig,ib) = zero
             pond_reservoir(ig) = zero
          ENDDO
       ENDDO
    ENDIF

!ym cette methode conserve les erreurs d'arrondie
!ym mais n'est pas la plus efficace

    !-
    !- Compute the transport from one basin to another
    !-

    IF (is_root_prc)  THEN
       ALLOCATE( fast_flow_g(nbp_glo, nbasmax), slow_flow_g(nbp_glo, nbasmax), &
            stream_flow_g(nbp_glo, nbasmax), stat=ier)
    ELSE
       ALLOCATE( fast_flow_g(1,1), slow_flow_g(1,1), &
            stream_flow_g(1, 1), stat=ier)
    ENDIF
    IF (ier /= 0) CALL ipslerr_p(3,'routing_flow','Pb in allocate for fast_flow_g','','')
       
    CALL gather(fast_flow,fast_flow_g)
    CALL gather(slow_flow,slow_flow_g)
    CALL gather(stream_flow,stream_flow_g)

    IF (is_root_prc) THEN
       DO ib=1,nbasmax
          DO ig=1,nbp_glo
             !
             rtg = route_togrid_glo(ig,ib)
             rtb = route_tobasin_glo(ig,ib)
             transport_glo(rtg,rtb) = transport_glo(rtg,rtb) + fast_flow_g(ig,ib) + slow_flow_g(ig,ib) + &
                  & stream_flow_g(ig,ib)
             !
          ENDDO
       ENDDO
    ENDIF

    DEALLOCATE( fast_flow_g, slow_flow_g, stream_flow_g )
   
    CALL scatter(transport_glo,transport)

    !-
    !- Do the floodings - First initialize
    !-
    return_swamp(:,:)=zero
    floods(:,:)=zero
    !-
!> Over swamp areas, a fraction of water (return_swamp) is withdrawn from the river depending on the
!> parameter swamp_cst.
!> It will be transferred into soil moisture and thus does not return directly to the river.
    !
    !- 1. Swamps: Take out water from the river to put it to the swamps
    !-
    !
    IF ( doswamps ) THEN
       tobeflooded(:) = swamp(:)
       DO ib=1,nbasmax
          DO ig=1,nbpt
             potflood(ig,ib) = transport(ig,ib) 
             !
             IF ( tobeflooded(ig) > 0. .AND. potflood(ig,ib) > 0. .AND. floodtemp(ig) > tp_00 ) THEN
                !
                IF (routing_area(ig,ib) > tobeflooded(ig)) THEN
                   floodindex = tobeflooded(ig) / routing_area(ig,ib)
                ELSE
                   floodindex = 1.0
                ENDIF
                return_swamp(ig,ib) = swamp_cst * potflood(ig,ib) * floodindex
                !
                tobeflooded(ig) = tobeflooded(ig) - routing_area(ig,ib) 
                !
             ENDIF
          ENDDO
       ENDDO
    ENDIF
    !-
    !- 2. Floodplains: Update the reservoir with the flux computed above.
    !-
    IF ( do_floodplains ) THEN
       DO ig=1,nbpt
          IF (floodplains(ig) .GT. min_sechiba .AND. floodtemp(ig) .GT. tp_00) THEN
             DO ib=1,nbasmax
                floods(ig,ib) = transport(ig,ib) - return_swamp(ig,ib) 
             ENDDO
          ENDIF
       ENDDO
    ENDIF
    !
    ! Update all reservoirs
!> The slow and deep reservoir (slow_reservoir) collect the deep drainage whereas the
!> fast_reservoir collects the computed surface runoff. Both discharge into a third reservoir
!> (stream_reservoir) of the next sub-basin downstream.
!> Water from the floodplains reservoir (flood_reservoir) flows also into the stream_reservoir of the next sub-basin downstream.
!> Water that flows into the pond_reservoir is withdrawn from the fast_reservoir.
    !
    DO ig=1,nbpt
       DO ib=1,nbasmax
          !
          fast_reservoir(ig,ib) =  fast_reservoir(ig,ib) + runoff(ig)*routing_area(ig,ib) - &
               & fast_flow(ig,ib) - pond_inflow(ig,ib)
          !
          slow_reservoir(ig,ib) = slow_reservoir(ig,ib) + drainage(ig)*routing_area(ig,ib) - &
               & slow_flow(ig,ib)
          !
          stream_reservoir(ig,ib) = stream_reservoir(ig,ib) + flood_flow(ig,ib) + transport(ig,ib) - &
               & stream_flow(ig,ib) - return_swamp(ig,ib) - floods(ig,ib)
          !
          flood_reservoir(ig,ib) = flood_reservoir(ig,ib) + floods(ig,ib) - &
               & flood_flow(ig,ib) 
          !
          pond_reservoir(ig) = pond_reservoir(ig) + pond_inflow(ig,ib) - pond_drainage(ig,ib)
          !
          IF ( flood_reservoir(ig,ib) .LT. zero ) THEN
             IF ( check_reservoir ) THEN
                WRITE(numout,*) "WARNING : negative flood reservoir at :", ig, ib, ". Problem is being corrected."
                WRITE(numout,*) "flood_reservoir, floods, flood_flow : ", flood_reservoir(ig,ib), floods(ig,ib), &
                     & flood_flow(ig,ib) 
             ENDIF
             stream_reservoir(ig,ib) = stream_reservoir(ig,ib) + flood_reservoir(ig,ib)
             flood_reservoir(ig,ib) = zero
          ENDIF
          !
          IF ( stream_reservoir(ig,ib) .LT. zero ) THEN
             IF ( check_reservoir ) THEN
                WRITE(numout,*) "WARNING : negative stream reservoir at :", ig, ib, ". Problem is being corrected."
                WRITE(numout,*) "stream_reservoir, flood_flow, transport : ", stream_reservoir(ig,ib), flood_flow(ig,ib), &
                     &  transport(ig,ib)
                WRITE(numout,*) "stream_flow, return_swamp, floods :", stream_flow(ig,ib), return_swamp(ig,ib), floods(ig,ib)
             ENDIF
             fast_reservoir(ig,ib) =  fast_reservoir(ig,ib) + stream_reservoir(ig,ib)
             stream_reservoir(ig,ib) = zero
          ENDIF
          !
          IF ( fast_reservoir(ig,ib) .LT. zero ) THEN
             IF ( check_reservoir ) THEN
                WRITE(numout,*) "WARNING : negative fast reservoir at :", ig, ib, ". Problem is being corrected."
                WRITE(numout,*) "fast_reservoir, runoff, fast_flow, ponf_inflow  : ", fast_reservoir(ig,ib), &
                     &runoff(ig), fast_flow(ig,ib), pond_inflow(ig,ib)
             ENDIF
             slow_reservoir(ig,ib) =  slow_reservoir(ig,ib) + fast_reservoir(ig,ib)
             fast_reservoir(ig,ib) = zero
          ENDIF

          IF ( slow_reservoir(ig,ib) .LT. - min_sechiba ) THEN
             IF ( check_reservoir ) THEN
                WRITE(numout,*) 'WARNING : There is a negative reservoir at :', ig, ib,lalo(ig,:)
                WRITE(numout,*) 'WARNING : slowr, slow_flow, drainage', &
                     & slow_reservoir(ig,ib), slow_flow(ig,ib), drainage(ig)
                WRITE(numout,*) 'WARNING : pondr, pond_inflow, pond_drainage', &
                     & pond_reservoir(ig), pond_inflow(ig,ib), pond_drainage(ig,ib)
             ENDIF
             CALL ipslerr_p(2, 'routing_flow', 'WARNING negative slow_reservoir.','','')
          ENDIF

       ENDDO
    ENDDO


    totflood(:) = zero
    DO ig=1,nbpt
       DO ib=1,nbasmax
          totflood(ig) = totflood(ig) + flood_reservoir(ig,ib)
       ENDDO
    ENDDO

    !-
    !- Computes the fraction of floodplains and ponds according to their volume
    !-
    IF (do_floodplains .OR. doponds) THEN
       flood_frac(:) = zero
       flood_height(:) = zero
       flood_frac_bas(:,:) = zero
       DO ig=1, nbpt
          IF (totflood(ig) .GT. min_sechiba) THEN
             ! We first compute the total fraction of the grid box which is flooded at optimum repartition
             flood_frac_pot = (totflood(ig) / (totarea(ig)*floodcri/(beta+un)))**(beta/(beta+un))
             flood_frac(ig) = MIN(floodplains(ig) / totarea(ig), flood_frac_pot)
             ! Then we diagnose the fraction for each basin with the size of its flood_reservoir 
             ! (flood_frac_bas may be > 1)
             DO ib=1,nbasmax
                IF (routing_area(ig,ib) .GT. min_sechiba) THEN
                   flood_frac_bas(ig,ib) = flood_frac(ig) * &
                        & (flood_reservoir(ig,ib) / totflood(ig)) / (routing_area(ig,ib) / totarea(ig))
                ENDIF
             ENDDO
             ! We diagnose the maximum height of floodplain
             flood_height(ig) = (beta/(beta+1))*floodcri*(flood_frac(ig))**(un/beta) + totflood(ig)/(totarea(ig)*flood_frac(ig)) 
             ! And finally add the pond surface
             pond_frac(ig) = MIN(un-flood_frac(ig), ((betap+1)*pond_reservoir(ig) / (pondcri*totarea(ig)))**(betap/(betap+1)) ) 
             flood_frac(ig) = flood_frac(ig) + pond_frac(ig)
             !
          ENDIF
       ENDDO
    ELSE
       flood_frac(:) = zero
       flood_height(:) = zero
       flood_frac_bas(:,:) = zero
    ENDIF

    !-
    !- Compute the total reinfiltration and returnflow to the grid box
!> A term of returnflow is computed including the water from the swamps that does not return directly to the river
!> but will be put into soil moisture (see hydrol module).
!> A term of reinfiltration is computed including the water that reinfiltrated from the ponds and floodplains areas.
!> It will be put into soil moisture (see hydrol module).
    !-
    IF (do_floodplains .OR. doswamps .OR. doponds) THEN
       returnflow(:) = zero
       reinfiltration(:) = zero
       !
       DO ib=1,nbasmax
          DO ig=1,nbpt
             returnflow(ig) =  returnflow(ig) + return_swamp(ig,ib)
             reinfiltration(ig) =  reinfiltration(ig) + pond_drainage(ig,ib) + flood_drainage(ig,ib) 
          ENDDO
       ENDDO
       !
       DO ig=1,nbpt
          returnflow(ig) = returnflow(ig)/totarea(ig)
          reinfiltration(ig) = reinfiltration(ig)/totarea(ig)
       ENDDO
    ELSE
       returnflow(:) = zero
       reinfiltration(:) = zero
    ENDIF

    !
    ! Compute the net irrigation requirement from Univ of Kassel
    !
    ! This is a very low priority process and thus only applies if
    ! there is some water left in the reservoirs after all other things.
    !
!> The computation of the irrigation is performed here.
!> * First step
!> In a first time, the water requirements (irrig_netereq) by the crops for their optimal growth are calculated
!> over each irrigated fraction (irrigated(ig)/totarea(ig)). It is the difference
!> between the maximal water loss by the crops (transpot_mean) and the net water amount kept by the soil
!> (precipitation and reinfiltration). Transpot_mean is computed in the routines enerbil and diffuco. It
!> is derived from the effective transpiration parametrization under stress-free conditions, called potential transpiration.
!> Crop_coef was used by a previous parametrization of irrigation in the code. Here, its value is equal to one.
!> The crop coefficient was constant in space and time to represent a mean resistance of the vegetation to the potential evaporation.
!> Now, the term crop_coef*Epot is substituted by transpot_mean (see Guimberteau et al., 2011).
!> * Second step
!> We compute irrigation needs in order to supply Irrig_netereq. Water for irrigation (irrig_actual) is withdrawn
!> from the reservoirs. The amount of water is withdrawn in priority from the stream reservoir.
!> If the irrigation requirement is higher than the water availability of the reservoir, water is withdrawn
!> from the fast reservoir or, in the extreme case, from the slow reservoir.
!> * Third step
!> We compute a deficit in water for irrigation. If it is positive, irrigation (depending on water availibility in the reservoirs)
!> has not supplied the crops requirements.
!
    IF ( do_irrigation ) THEN
       DO ig=1,nbpt
          !
          !IF ((vegtot(ig) .GT. min_sechiba) .AND. (humrel(ig) .LT. un-min_sechiba) .AND. &
          !     & (runoff(ig) .LT. min_sechiba) ) THEN
          IF (vegtot(ig) .GT. min_sechiba) THEN
             
!             irrig_netereq(ig) = (irrigated(ig) / totarea(ig) ) * MAX(zero, transpot_mean(ig) - &
!                  & (precip(ig)+reinfiltration(ig)) )
!             irrig_netereq(ig) = (irrigated(ig) / tot_vegfrac_crop(ig) ) * MAX(zero, transpot_agr(ig) - &
!                  & (precip(ig)+reinfiltration(ig)) )
             irrig_netereq(ig) = zero
             DO jv=2,nvm
                !IF ((veget_max(ig,jv) .GT. 0) .AND. ok_LAIdev(jv)) THEN
                !ZunYin change the condition when veget > 0
                IF ((lai(ig,jv) .GT. 0) .AND. ok_LAIdev(jv)) THEN
                    IF (do_irrig_reservoir(pref_soil_veg(jv))) THEN
                        !flooding irrigation with reservoir
                        IF (do_awd(pref_soil_veg(jv))) THEN
                            ! do AWD irrigation
                            IF (vegstress(ig,jv) .LT. irrig_threshold(jv)) THEN
                                IF (awd_dep(pref_soil_veg(jv)) .LT. zero) THEN
                                    irrig_netereq(ig) = irrig_netereq(ig) + MIN( irrig_dosmax*48, &
                                        & MAX(zero, soil_deficit(ig,jv)) ) * veget_max(ig,jv) 
                                ELSE
                                    irrig_netereq(ig) = irrig_netereq(ig) + &
                                    & awd_dep(pref_soil_veg(jv)) * veget_max(ig,jv) 
                                ENDIF
                            ENDIF
                        ELSEIF ((irrigr(ig,pref_soil_veg(jv)) .LT. irrig_dep_threshold(pref_soil_veg(jv)))) THEN
                            irrig_netereq(ig) = irrig_netereq(ig) + &
                                                & (irrig_dep_threshold(pref_soil_veg(jv)) - irrigr(ig,pref_soil_veg(jv))) * veget_max(ig,jv) 
                                            
                        ENDIF
                    ELSEIF ( (vegstress(ig,jv) .LT. irrig_threshold(jv)) ) THEN
                        IF (irrig_drip) THEN
                            irrig_netereq(ig) = irrig_netereq(ig) + MIN( irrig_dosmax*48, &
                                                & MAX(zero, transpot(ig,jv) * (veget(ig,jv)/veget_max(ig,jv)) - &
                                                !& evapot_corr(ig) * (1-veget(ig,jv)/veget_max(ig,jv)) - &
                                                & (precip(ig)+reinfiltration(ig)))) * veget_max(ig,jv)   
                        ELSE !flooding
                            irrig_netereq(ig) = irrig_netereq(ig) + MIN( irrig_dosmax*48, &
                                                & MAX(zero, soil_deficit(ig,jv)) ) * veget_max(ig,jv) 
                        ENDIF
                       ! irrigated must be the percentage of croplands irrigated
                    ENDIF
                ENDIF
             ENDDO
             ! irrig_netereq is the needs (mm) over the entire grid
             
          ENDIF
          !
          DO ib=1,nbasmax
             IF ( routing_area(ig,ib) .GT. 0 ) THEN
             
                irrig_needs(ig,ib) = irrig_netereq(ig) * routing_area(ig,ib)
!                irrig_needs(ig,ib) = irrig_netereq(ig) * tot_vegfrac_crop(ig) * routing_area(ig,ib)

                irrig_actual(ig,ib) = MIN(irrig_needs(ig,ib),&
                     &   stream_reservoir(ig,ib) + fast_reservoir(ig,ib) + slow_reservoir(ig,ib) )
                
                slow_reservoir(ig,ib) = MAX(zero, slow_reservoir(ig,ib) + &
                     & MIN(zero, fast_reservoir(ig,ib) + MIN(zero, stream_reservoir(ig,ib)-irrig_actual(ig,ib))))

                fast_reservoir(ig,ib) = MAX( zero, &
                     &  fast_reservoir(ig,ib) + MIN(zero, stream_reservoir(ig,ib)-irrig_actual(ig,ib)))

                stream_reservoir(ig,ib) = MAX(zero, stream_reservoir(ig,ib)-irrig_actual(ig,ib) )

                irrig_deficit(ig,ib) = irrig_needs(ig,ib)-irrig_actual(ig,ib)

             ENDIF
          ENDDO
          !
          ! Check if we cannot find the missing water in another basin of the same grid (stream reservoir only).
          ! If we find that then we create some adduction from that subbasin to the one where we need it for
          ! irrigation.
          !
!> If crops water requirements have not been supplied (irrig_deficit>0), we check if we cannot find the missing water
!> in another basin of the same grid. If there is water in the stream reservoir of this subbasin, we create some adduction
!> from that subbasin to the one where we need it for irrigation.
!> 
          DO ib=1,nbasmax

             stream_tot = SUM(stream_reservoir(ig,:))

             DO WHILE ( irrig_deficit(ig,ib) > min_sechiba .AND. stream_tot > min_sechiba)
                
                fi = MAXLOC(stream_reservoir(ig,:))
                ib2 = fi(1)

                irrig_adduct(ig,ib) = MIN(irrig_deficit(ig,ib), stream_reservoir(ig,ib2))
                stream_reservoir(ig,ib2) = stream_reservoir(ig,ib2)-irrig_adduct(ig,ib)
                irrig_deficit(ig,ib) = irrig_deficit(ig,ib)-irrig_adduct(ig,ib)
             
                stream_tot = SUM(stream_reservoir(ig,:))
                
             ENDDO
             
          ENDDO
          !
       ENDDO
       !
       ! If we are at higher resolution we might need to look at neighboring grid boxes to find the streams
       ! which can feed irrigation
!
!> At higher resolution (grid box smaller than 100x100km), we can import water from neighboring grid boxes
!> to the one where we need it for irrigation.
       !
       IF (is_root_prc) THEN
          ALLOCATE(irrig_deficit_glo(nbp_glo, nbasmax), stream_reservoir_glo(nbp_glo, nbasmax), &
               &        irrig_adduct_glo(nbp_glo, nbasmax), stat=ier)
       ELSE
          ALLOCATE(irrig_deficit_glo(0, 0), stream_reservoir_glo(0, 0), &
               &        irrig_adduct_glo(0, 0), stat=ier)
       ENDIF
       IF (ier /= 0) CALL ipslerr_p(3,'routing_flow','Pb in allocate for irrig_deficit_glo, stream_reservoir_glo,...','','')

       CALL gather(irrig_deficit, irrig_deficit_glo)
       CALL gather(stream_reservoir,  stream_reservoir_glo)
       CALL gather(irrig_adduct, irrig_adduct_glo)

       IF (is_root_prc) THEN
          !
          DO ig=1,nbp_glo
             ! Only work if the grid box is smaller than 100x100km. Else the piplines we build
             ! here would be too long to be reasonable.
             IF ( resolution_g(ig,1) < 100000. .AND. resolution_g(ig,2) < 100000. ) THEN
                DO ib=1,nbasmax
                   !
                   IF ( irrig_deficit_glo(ig,ib)  > min_sechiba ) THEN
                      !
                      streams_around(:,:) = zero
                      !
                      DO in=1,8
                         ig2 = neighbours_g(ig,in)
                         IF (ig2 .GT. 0 ) THEN
                            streams_around(in,:) = stream_reservoir_glo(ig2,:)
                            igrd(in) = ig2
                         ENDIF
                      ENDDO
                      !
                      IF ( MAXVAL(streams_around) .GT. zero ) THEN
                         !
                         ff=MAXLOC(streams_around)
                         ig2=igrd(ff(1))
                         ib2=ff(2)
                         !
                         IF ( routing_area_glo(ig2,ib2) .GT. 0 .AND. stream_reservoir_glo(ig2,ib2) > zero ) THEN
                            adduction = MIN(irrig_deficit_glo(ig,ib), stream_reservoir_glo(ig2,ib2))
                            stream_reservoir_glo(ig2,ib2) = stream_reservoir_glo(ig2,ib2) - adduction
                            irrig_deficit_glo(ig,ib) = irrig_deficit_glo(ig,ib) - adduction
                            irrig_adduct_glo(ig,ib) = irrig_adduct_glo(ig,ib) + adduction
                         ENDIF
                         !
                      ENDIF
                      !
                   ENDIF
                   !
                ENDDO
             ENDIF
          ENDDO
          !
       ENDIF
       !

       CALL scatter(irrig_deficit_glo, irrig_deficit)
       CALL scatter(stream_reservoir_glo,  stream_reservoir)
       CALL scatter(irrig_adduct_glo, irrig_adduct)

       DEALLOCATE(irrig_deficit_glo, stream_reservoir_glo, irrig_adduct_glo)

    ENDIF
    !
    !
    ! Compute the fluxes which leave the routing scheme
    !
    ! Lakeinflow is in Kg/dt
    ! returnflow is in Kg/m^2/dt
    !
    delsurfstor(:) = -flood_diag(:)-pond_diag(:)-lake_diag(:)
    hydrographs(:) = zero
    slowflow_diag(:) = zero
    fast_diag(:) = zero
    slow_diag(:) = zero
    stream_diag(:) = zero
    flood_diag(:) =  zero
    pond_diag(:) =  zero
    irrigation(:) = zero
    !
    !
    DO ib=1,nbasmax
       !
       DO ig=1,nbpt
          IF (hydrodiag(ig,ib) > 0 ) THEN
             hydrographs(ig) = hydrographs(ig) + fast_flow(ig,ib) + slow_flow(ig,ib) + & 
                  &  stream_flow(ig,ib) 
             slowflow_diag(ig) = slowflow_diag(ig) + slow_flow(ig,ib)
          ENDIF
          fast_diag(ig) = fast_diag(ig) + fast_reservoir(ig,ib)
          slow_diag(ig) = slow_diag(ig) + slow_reservoir(ig,ib)
          stream_diag(ig) = stream_diag(ig) + stream_reservoir(ig,ib)
          flood_diag(ig) = flood_diag(ig) + flood_reservoir(ig,ib)
          IF (do_fullirr) THEN
              irrigation(ig) = irrigation(ig) + irrig_needs(ig,ib) 
              ! when fully irrigated, we interrupt the water balance, and bring
              ! magic water
          ELSE
              irrigation (ig) = irrigation (ig) + irrig_actual(ig,ib) + irrig_adduct(ig,ib)
          ENDIF
       ENDDO
    ENDDO
    !
    DO ig=1,nbpt
       fast_diag(ig) = fast_diag(ig)/totarea(ig)
       slow_diag(ig) = slow_diag(ig)/totarea(ig)
       stream_diag(ig) = stream_diag(ig)/totarea(ig)
       flood_diag(ig) = flood_diag(ig)/totarea(ig)
       pond_diag(ig) = pond_reservoir(ig)/totarea(ig)
       !
       irrigation(ig) = irrigation(ig)/totarea(ig)
       !
       ! The three output types for the routing : endoheric basins,, rivers and 
       ! diffuse coastal flow.
       !
       lakeinflow(ig) = transport(ig,nbasmax+1)
       coastalflow(ig) = transport(ig,nbasmax+2)
       riverflow(ig) = transport(ig,nbasmax+3)
       !
    ENDDO
    !
    flood_res = flood_diag + pond_diag
    !
  END SUBROUTINE routing_flow
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_lake
!!
!>\BRIEF        : This subroutine stores water in lakes so that it does not cycle through the runoff.
!!                For the moment it only works for endoheric lakes but I can be extended in the future.
!!
!! DESCRIPTION (definitions, functional, design, flags): The return flow to the soil moisture reservoir
!! is based on a maximum lake evaporation rate (maxevap_lake). \n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    :None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_lake(nbpt, dt_routing, lakeinflow, humrel, return_lakes)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in) :: nbpt               !! Domain size (unitless)
    REAL(r_std), INTENT (in)   :: dt_routing         !! Routing time step (s)
    REAL(r_std), INTENT(in)    :: lakeinflow(nbpt)   !! Water inflow to the lakes (kg/dt)
    REAL(r_std), INTENT(in)    :: humrel(nbpt)       !! Soil moisture stress, root extraction potential (unitless)
    !
!! OUTPUT VARIABLES
    REAL(r_std), INTENT(out)   :: return_lakes(nbpt) !! Water from lakes flowing back into soil moisture (kg/m^2/dt)
    !
!! LOCAL VARIABLES
    INTEGER(i_std)             :: ig                 !! Indices (unitless)
    REAL(r_std)                :: refill             !!
    REAL(r_std)                :: total_area         !! Sum of all the surfaces of the basins (m^2)

!_ ================================================================================================================================
    !
    !
    DO ig=1,nbpt
       !
       total_area = SUM(routing_area(ig,:))
       !
       lake_reservoir(ig) = lake_reservoir(ig) + lakeinflow(ig)
       !uptake in Kg/dt
       refill = MAX(zero, maxevap_lake * (un - humrel(ig)) * dt_routing * total_area)
       return_lakes(ig) = MIN(refill, lake_reservoir(ig))
       lake_reservoir(ig) = lake_reservoir(ig) - return_lakes(ig)
       !Return in Kg/m^2/dt
       IF ( doswamps ) THEN
          return_lakes(ig) = return_lakes(ig)/total_area
       ELSE
          return_lakes(ig) = zero
       ENDIF
       !
       ! This is the volume of the lake scaled to the entire grid.
       ! It would be batter to scale it to the size of the lake
       ! but this information is not yet available.
       lake_diag(ig) = lake_reservoir(ig)/total_area
       !
    ENDDO
    !
  END SUBROUTINE routing_lake
  !

!! ================================================================================================================================
!! SUBROUTINE 	: routing_diagnostic_p
!!
!>\BRIEF         This parallelized subroutine gives a diagnostic of the basins used
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_diagnostic_p(nbpt, index, lalo, resolution, contfrac, hist_id, hist2_id)
    !
    IMPLICIT NONE
    
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)      :: nbpt               !! Domain size (unitless)
    INTEGER(i_std), INTENT(in)      :: index(nbpt)        !! Indices of the points on the map (unitless)
    REAL(r_std), INTENT(in)         :: lalo(nbpt,2)       !! Vector of latitude and longitudes (beware of the order !)
    REAL(r_std), INTENT(in)         :: resolution(nbpt,2) !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)         :: contfrac(nbpt)     !! Fraction of land in each grid box (unitless;0-1)
    INTEGER(i_std),INTENT (in)      :: hist_id            !! Access to history file (unitless)
    INTEGER(i_std),INTENT (in)      :: hist2_id           !! Access to history file 2 (unitless)
    !
!! LOCAL VARIABLES
    REAL(r_std), DIMENSION(nbpt)    :: nbrivers           !! Number of rivers in the grid (unitless)
    REAL(r_std), DIMENSION(nbpt)    :: basinmap           !! Map of basins (unitless)
    REAL(r_std), DIMENSION(nbp_glo) :: nbrivers_g         !! Number of rivers in the grid (unitless)
    REAL(r_std), DIMENSION(nbp_glo) :: basinmap_g         !! Map of basins (unitless)

!_ ================================================================================================================================
    routing_area => routing_area_glo  
    topo_resid => topo_resid_glo
    route_togrid => route_togrid_glo
    route_tobasin => route_tobasin_glo
    route_nbintobas => route_nbintobas_glo
    global_basinid => global_basinid_glo
    hydrodiag=>hydrodiag_glo
    hydroupbasin=>hydroupbasin_glo
    
    IF (is_root_prc) CALL routing_diagnostic(nbp_glo, index_g, lalo_g, resolution_g, contfrac_g, nbrivers_g,basinmap_g)

    routing_area => routing_area_loc  
    topo_resid => topo_resid_loc
    route_togrid => route_togrid_loc
    route_tobasin => route_tobasin_loc
    route_nbintobas => route_nbintobas_loc
    global_basinid => global_basinid_loc
    hydrodiag=>hydrodiag_loc
    hydroupbasin=>hydroupbasin_loc
    
    CALL scatter(nbrivers_g,nbrivers)
    CALL scatter(basinmap_g,basinmap)
    CALL scatter(hydrodiag_glo,hydrodiag_loc)
    CALL scatter(hydroupbasin_glo,hydroupbasin_loc)
       
    CALL xios_orchidee_send_field("basinmap",basinmap)
    CALL xios_orchidee_send_field("nbrivers",nbrivers)

    IF ( .NOT. almaoutput ) THEN
       CALL histwrite_p(hist_id, 'basinmap', 1, basinmap, nbpt, index)
       CALL histwrite_p(hist_id, 'nbrivers', 1, nbrivers, nbpt, index)
    ELSE
    ENDIF
    IF ( hist2_id > 0 ) THEN
       IF ( .NOT. almaoutput ) THEN
          CALL histwrite_p(hist2_id, 'basinmap', 1, basinmap, nbpt, index)
          CALL histwrite_p(hist2_id, 'nbrivers', 1, nbrivers, nbpt, index)
       ELSE
       ENDIF
    ENDIF
    
        
  END SUBROUTINE routing_diagnostic_p

!! ================================================================================================================================
!! SUBROUTINE 	: routing_diagnostic
!!
!>\BRIEF         This non-parallelized subroutine gives a diagnostic of the basins used. This produces some information 
!!               on the rivers which are being diagnosed.
!!
!! DESCRIPTION (definitions, functional, design, flags) : As not all rivers can be monitored in the model, we will only
!! archive num_largest rivers. In this routine we will diagnose the num_largest largest rivers and print to the standard
!! output the names of these basins and their area. The list of names of these largest rivers are taken from a list coded in the 
!! routine routing_names. As this standard output is not sufficient, we will also write it to a netCDF file with the routine 
!! routing_diagncfile. It is important to keep for diagnostic the fraction of the largest basins in each grid box and keep information
!! how they are linked one to the other.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): No output variables.
!!
!! REFERENCES   : None
!!
!! FLOWCHART    :None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_diagnostic(nbpt, l_index, lalo, resolution, contfrac, nbrivers, basinmap)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                   :: nbpt                !! Domain size  (unitless)
    INTEGER(i_std), INTENT(in)                   :: l_index(nbpt)       !! Indices of the points on the map (unitless)
    REAL(r_std), INTENT(in)                      :: lalo(nbpt,2)        !! Vector of latitude and longitudes (beware of the order !) 
    REAL(r_std), INTENT(in)                      :: resolution(nbpt,2)  !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)                      :: contfrac(nbpt)      !! Fraction of land in each grid box (unitless;0-1)
    !
!! OUTPUT VARIABLES
    REAL(r_std), DIMENSION(nbpt), INTENT(out)    :: nbrivers            !! Number of rivers in the grid (unitless)
    REAL(r_std), DIMENSION(nbpt), INTENT(out)    :: basinmap            !! Map of basins (unitless)
    !
!! LOCAL VARIABLES
    INTEGER(i_std), DIMENSION(nbpt,nbasmax)      :: outids              !! IDs of river to which this basin contributes (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)  :: pts                 !! List the points belonging to the basin (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)  :: ptbas               !! List the basin number for this point (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)  :: outpt               !! Outflow point for each basin (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)    :: nb_pts              !! Number of points in the basin (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:)       :: totarea             !! Total area of basin (m^2)
    REAL(r_std), ALLOCATABLE, DIMENSION(:)       :: tmparea             !! 
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)    :: topids              !! The IDs of the first num_largest basins (unitless)
    CHARACTER(LEN=25), ALLOCATABLE, DIMENSION(:) :: basin_names         !! Names of the rivers (unitless)
    CHARACTER(LEN=25)                            :: name_str            !!
    !
    LOGICAL                                      :: river_file          !! Choose to write a description of the rivers (true/false)
    CHARACTER(LEN=80)                            :: river_file_name     !! Filename in which we write the description of the rivers (unitless)
    !
    CHARACTER(LEN=25), ALLOCATABLE, DIMENSION(:)  :: sorted_names       !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: streams_nb         !! Number of streams in basin (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: streams_avehops    !! Average number of hops in streams (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: streams_minhops    !! Minimum number of hops in streams (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: streams_maxhops    !! Minimum number of hops in streams (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:)        :: streams_resid      !! Average residence time
    !
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lbasin_area        !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lbasin_uparea      !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: lrivercode         !!
    !
    INTEGER(i_std)                                :: ig, ib, og, ob, ign, ibn, ff(1), ic, icc, nb_small, idbas, slen, ii !! Indices (unitless)
    INTEGER(i_std)                                :: ier                !! Error handling
    CHARACTER(LEN=1)                              :: nn                 !!
    INTEGER(i_std)                                :: name_found         !!
    !
    REAL(r_std)                                   :: averesid           !!
    REAL(r_std), DIMENSION(nbasmax)               :: tmpbas             !!
    REAL(r_std), DIMENSION(nbpt,nbasmax)          :: areaupbasin        !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: sortedrivs         !!
    !
    ! Variables for the river coding
    !
    INTEGER(i_std)                               :: longest_river       !!
    INTEGER(i_std)                               :: nbmax               !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)  :: allstreams          !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)     :: upstreamchange      !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)    :: tstreams, tslen, tpts, tptbas, tcode !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:)       :: tuparea             !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:)       :: tupstreamchange     !!
    !
    LOGICAL                                      :: err_nbpt_grid_basin !! (true/false)
    LOGICAL                                      :: err_basin_number    !! (true/false)

!_ ================================================================================================================================
    !
    !
    ALLOCATE(pts(num_largest, nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for pts','','')

    ALLOCATE(ptbas(num_largest, nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for ptbas','','')

    ALLOCATE(outpt(num_largest, 2), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for outpt','','')

    ALLOCATE(nb_pts(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for nb_pts','','')

    ALLOCATE(totarea(num_largest), tmparea(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for totarea','','')

    ALLOCATE(topids(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for topids','','')

    ALLOCATE(sortedrivs(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for sortedrivs','','')

    ALLOCATE(sorted_names(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for sorted_names','','')

    ALLOCATE(streams_nb(num_largest), streams_avehops(num_largest), streams_minhops(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for streams_nb','','')

    ALLOCATE(streams_maxhops(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for streams_maxhops','','')

    ALLOCATE(streams_resid(num_largest), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for streams_resid','','')
    
    ALLOCATE(lbasin_area(num_largest,nbpt), lbasin_uparea(num_largest,nbpt), lrivercode(num_largest,nbpt), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for lbasin_area','','')
    
    IF ( .NOT. is_root_prc) THEN
       WRITE(numout,*) "routing_diagnostic is not suitable for running in parallel"
       WRITE(numout,*) "We are here on a non root processor. is_root_prc = ", is_root_prc
       WRITE(numout,*) "STOP from routing_diagnostic"
       CALL ipslerr_p(3,'routing_diagnostic','This routine is not suitable for running in parallel','','')
    ENDIF
    
    
    !Config Key   = RIVER_DESC
    !Config Desc  = Writes out a description of the rivers
    !Config If    = RIVER_ROUTING
    !Config Def   = n
    !Config Help  = This flag allows to write out a file containing the list of 
    !Config         rivers which are beeing simulated. It provides location of outflow
    !Config         drainage area, name and ID.
    !Config Units = [FLAG]
    !
    river_file=.FALSE.
    CALL getin('RIVER_DESC', river_file)
    !
    !Config Key   = RIVER_DESC_FILE
    !Config Desc  = Filename in which we write the description of the rivers. If suffix is ".nc" a netCDF file is created
    !Config If    = RIVER_DESC
    !Config Def   = river_desc.nc
    !Config Help  = File name where we will write the information. If the suffix is ".nc" a netCDF file is generated. Else
    !Config         a simple text file will contain some information. The netCDF file is valuable for post-processing the
    !               data as it will contain the fraction of the large basins in each grid box.
    !Config Units = [FILE]
    !
    river_file_name="river_desc.nc"
    CALL getin('RIVER_DESC_FILE', river_file_name)
    !
    !
    ! First we get the list of all river outflow points
    ! We work under the assumption that we only have num_largest basins finishing with
    ! nbasmax+3. This is checked in routing_truncate.
    !
    nb_small = 1
    outpt(:,:) = -1
    ic = 0
    DO ig=1,nbpt
       DO ib=1,nbasmax
          ign = route_togrid(ig, ib)
          ibn = route_tobasin(ig, ib)
          IF ( ibn .EQ. nbasmax+3) THEN
             ic = ic + 1
             outpt(ic,1) = ig
             outpt(ic,2) = ib
             !
             ! Get the largest id of the basins we call a river. This is
             ! to extract the names of all rivers.
             !
             IF ( global_basinid(ig,ib) > nb_small ) THEN
                nb_small = global_basinid(ig,ib)
             ENDIF
          ENDIF
       ENDDO
    ENDDO
    
    nb_small = MIN(nb_small, 349)
    
    ALLOCATE(basin_names(nb_small), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for basins_names','','')

    CALL routing_names(nb_small, basin_names)
    !
    ! Go through all points and basins to see if they outflow as a river and store the
    ! information needed in the various arrays.
    !
    nb_pts(:) = 0
    totarea(:) = zero
    hydrodiag(:,:) = 0
    areaupbasin(:,:) = zero
    outids(:,:) = -1
    ob = -1
    og = -1
    lbasin_area(:,:) = zero
    lbasin_uparea(:,:) = zero
    longest_river = 0
    !
    err_nbpt_grid_basin = .FALSE.
    loopgridbasin : DO ig=1,nbpt
       !
       DO ib=1,nbasmax
          IF ( routing_area(ig,ib) .GT. zero ) THEN
             ic = 0
             ign = ig
             ibn = ib
             ! Locate outflow point
             DO WHILE (ibn .GT. 0 .AND. ibn .LE. nbasmax .AND. ic .LT. nbasmax*nbpt)
                ic = ic + 1
                og = ign
                ob = ibn
                ign = route_togrid(og, ob)
                ibn = route_tobasin(og, ob)
                areaupbasin(og, ob) = areaupbasin(og, ob) + routing_area(ig,ib)
             ENDDO
             !
             longest_river = MAX(longest_river, ic)
             !
             ! Now that we have an outflow check if it is one of the num_largest rivers. 
             ! In this case we keeps the location so we diagnose it.
             !
             IF ( ibn .EQ. nbasmax + 3) THEN
                DO icc = 1,num_largest
                   IF ( outpt(icc,1) .EQ. og .AND. outpt(icc,2) .EQ. ob ) THEN
                      !
                      ! We only keep this point for our map if it is large enough.
                      !
                      nb_pts(icc) = nb_pts(icc) + 1
                      !
                      !
                      IF ( nb_pts(icc) > nbpt ) THEN
                         err_nbpt_grid_basin = .TRUE.
                         EXIT loopgridbasin
                      ENDIF
                      !
                      pts(icc, nb_pts(icc)) = ig
                      ptbas(icc, nb_pts(icc)) = ib
                      totarea(icc) = totarea(icc) + routing_area(ig,ib)
                      !
                      lbasin_area(icc,nb_pts(icc)) = routing_area(ig,ib)
                      !
                      ! ID of the river is taken from the last point before the outflow.
                      topids(icc) = global_basinid(og,ob)
                      outids(ig,ib) = global_basinid(og,ob)
                      !
                      ! On this gridbox and basin we will diagnose the hydrograph
                      !
                      hydrodiag(ig, ib) = 1
                      !
                   ENDIF
                ENDDO
             ENDIF
          ENDIF
          !
       ENDDO
       !
    ENDDO loopgridbasin
    !
    IF ( err_nbpt_grid_basin ) THEN
       WRITE(numout, *) "routing_diagnostic : The number of grid points in basin ", icc
       WRITE(numout, *) "routing_diagnostic : is larger than anticiped. "
       CALL ipslerr_p(3, 'routing_diagnostic', 'We are heading for a out of bounds in arrays pts, ptsbas and lbasin_area.',&
                     & 'Increase the last dimension of these arrays.','')
    ENDIF
    !
    ! Now we decide which points we will keep from the largest basins
    !
    ! Temporary fix
    route_nbintobas(:,:) = 0
    !
    basinmap(:) = zero
    DO ig=1,nbpt
       !
       ! Look for the dominant basin in this grid. This information only affects some
       ! diagnostics : hydrographs and saved area upstream.
       !
       icc = 0
       idbas = -1
       !
       DO ib=1,nbasmax
          IF ( outids(ig,ib) > 0 ) THEN
             IF ( COUNT(outids(ig,:) == outids(ig,ib)) > icc ) THEN
                icc = COUNT(outids(ig,:) == outids(ig,ib))
                idbas = outids(ig,ib)
             ENDIF
          ENDIF
       ENDDO
       !
       ! If we have found a point from the large basins and decided which one
       ! takes over this grid then we note it on the map.
       ! Clean-up a little the hydrodiag array
       !
       IF ( idbas > 0 ) THEN
          basinmap(ig) = REAL(idbas, r_std)
       ENDIF
       !
       ! Now place the hydrograph diagnostic on the point closest to the
       ! ocean.
       !
       tmpbas(:) = zero
       DO ib=1,nbasmax
          IF ( outids(ig,ib) .EQ. idbas) THEN
             tmpbas(ib) = areaupbasin(ig,ib)
          ENDIF
       ENDDO
       hydrodiag(ig,:) = 0
       ff=MAXLOC(tmpbas)
       hydrodiag(ig,ff(1)) = 1
       hydroupbasin(ig) = areaupbasin(ig,ff(1))
       !
    ENDDO
    !
    !
    !
    tmparea(:) = totarea(:)
    DO icc = 1, num_largest
       ff = MAXLOC(tmparea)
       sortedrivs(icc) = ff(1)
       tmparea(ff(1)) = 0.0
    ENDDO
    !
    ! Diagnose the complexity of the basins obtained and determine their code in the Pfafstetter system
    !
    nbmax=MAXVAL(nb_pts)
    ALLOCATE(allstreams(nbmax, longest_river), upstreamchange(nbmax, longest_river), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for allstreams','','')

    ALLOCATE(tstreams(longest_river), tupstreamchange(longest_river), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for tstreams','','')

    ALLOCATE(tslen(nbmax), tpts(nbmax), tptbas(nbmax), tuparea(nbmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for tslen','','')

    ALLOCATE(tcode(nbmax), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'routing_diagnostic','Pb in allocate for tcode','','')

    DO icc = 1, num_largest
       !
       ! Work through the largest basins
       !
       idbas = sortedrivs(icc)
       !
       streams_nb(idbas) = 0
       streams_avehops(idbas) = 0
       streams_minhops(idbas) = undef_int
       streams_maxhops(idbas) = 0
       streams_resid(idbas) = zero
       tslen(:) = 0
       !
       allstreams(:,:) = 0
       upstreamchange(:,:) = zero
       !
       DO ii=1,nb_pts(idbas)
          !
          ig = pts(idbas, ii)
          ib = ptbas(idbas, ii)
          !
          lbasin_uparea(idbas,ii) = areaupbasin(ig,ib)
          !
          slen = 0
          ign = ig
          ibn = ib
          og = ig
          ob = ib
          !
          averesid = zero
          tupstreamchange(:) = zero
          ! go to outflow point to count the number of hops
          DO WHILE (ibn .GT. 0 .AND. ibn .LE. nbasmax)
             ! Store data
             slen = slen + 1
             tstreams(slen) = ign
             tupstreamchange(slen) = areaupbasin(ign,ibn)-areaupbasin(og,ob)
             ! Move to next point
             og = ign
             ob = ibn
             ign = route_togrid(og, ob)
             ibn = route_tobasin(og, ob)
             averesid = averesid + topo_resid(og, ob)**2
          ENDDO
          !
          allstreams(ii,1:slen) = tstreams(slen:1:-1)
          upstreamchange(ii,1:slen) = tupstreamchange(slen:1:-1)
          tslen(ii) = slen
          !
          ! Save diagnostics
          !
          streams_nb(idbas) = streams_nb(idbas) + 1
          streams_avehops(idbas) = streams_avehops(idbas) + slen
          streams_resid(idbas) = streams_resid(idbas) + SQRT(averesid)
          IF ( slen < streams_minhops(idbas) ) THEN
             streams_minhops(idbas) = slen
          ENDIF
          IF ( slen > streams_maxhops(idbas) ) THEN
             streams_maxhops(idbas) = slen
          ENDIF
          !
       ENDDO
       ! build the average
       IF ( streams_nb(idbas) > 0 ) THEN
          streams_avehops(idbas) = streams_avehops(idbas)/streams_nb(idbas)
          streams_resid(idbas) = streams_resid(idbas)/REAL(streams_nb(idbas), r_std)
       ELSE
          ! River without streams ... very rare but happens
          streams_avehops(idbas) = zero
          streams_resid(idbas) = zero
          streams_maxhops(idbas) = zero
          streams_minhops(idbas) = zero
       ENDIF
       !
       !
       ii=nb_pts(idbas)
       tpts(:) = 0
       tpts(1:ii) = pts(idbas,1:ii)
       tptbas(:) = 0
       tptbas(1:ii) = ptbas(idbas,1:ii)
       tuparea(:) = 0
       tuparea(1:ii) = lbasin_uparea(idbas,1:ii)
       !
       CALL routing_diagcode(ii, tpts, tptbas, tuparea, tslen, MAXVAL(tslen), allstreams, upstreamchange, tcode)  
       !
       lrivercode(idbas,:) = 0
       lrivercode(idbas,1:ii) = tcode(1:ii)
       !
    ENDDO
    !
    ! Create the sorted list of names
    !
    err_basin_number = .FALSE.
    DO icc = 1, num_largest
       !
       ib=sortedrivs(icc)
       !
       IF ( topids(ib) .GT. nb_small ) THEN
          IF (topids(ib) <= 99 ) THEN
             WRITE(sorted_names(icc), '("Nb_",I2.2)') topids(ib)
          ELSE IF (topids(ib) <= 999 ) THEN
             WRITE(sorted_names(icc), '("Nb_",I3.3)') topids(ib)
          ELSE IF (topids(ib) <= 9999 ) THEN
             WRITE(sorted_names(icc), '("Nb_",I4.4)') topids(ib)
          ELSE IF (topids(ib) <= 99999 ) THEN
             WRITE(sorted_names(icc), '("Nb_",I5.5)') topids(ib)
          ELSE IF (topids(ib) <= 999999 ) THEN
             WRITE(sorted_names(icc), '("Nb_",I6.6)') topids(ib)
          ELSE
             err_basin_number = .TRUE.
             EXIT
          ENDIF

       ELSE
          sorted_names(icc) = basin_names(topids(ib))
       ENDIF
       !
    ENDDO
    !
    IF ( err_basin_number ) THEN
       CALL ipslerr_p(3, 'routing_diagnostic', 'We found a basin number larger than 999999.',&
            & 'This is impossible. Please verify your configuration.','')
    ENDIF
    !
    ! Check for doubles and rename if needed
    !
    DO icc = 1, num_largest
       name_found=0
       DO ic=1, num_largest
          IF ( TRIM(sorted_names(icc)) == TRIM(sorted_names(ic)) ) THEN
             name_found = name_found + 1
          ENDIF
       ENDDO
       !
       IF ( name_found > 1 ) THEN
          DO ic=num_largest,1,-1
             IF ( TRIM(sorted_names(icc)) == TRIM(sorted_names(ic)) ) THEN
                IF ( name_found > 1 ) THEN
                   WRITE(nn,'(I1)')  name_found
                   sorted_names(ic) = TRIM(sorted_names(ic))//nn
                   name_found = name_found - 1
                ENDIF
             ENDIF
          ENDDO
       ENDIF
       !
    ENDDO
    !
    ! Print to stdout on ROOT_PROC the diagnostics for the largest basins we have found.
    !
    DO icc = 1, num_largest
       !
       IF ( nb_pts(sortedrivs(icc)) .GT. 2 ) THEN
          name_str = sorted_names(icc)
          WRITE(numout,'("Basin ID ", I5," ", A15, " Area [km^2] : ", F13.4, " Nb points : ", I4)')&
               & topids(sortedrivs(icc)), name_str(1:15), totarea(sortedrivs(icc))/1.e6,  nb_pts(sortedrivs(icc))
          ENDIF
          !
    ENDDO
    !
    ! Save some of the basin information into files.
    !
    IF ( river_file ) THEN

       IF ( INDEX(river_file_name,".nc") > 1 ) THEN

          CALL routing_diagncfile(river_file_name, nbpt, lalo, nb_pts, topids, sorted_names, sortedrivs, &
               &                  pts, lbasin_area, lbasin_uparea, lrivercode, outpt, streams_nb, streams_avehops, &
               &                  streams_minhops, streams_maxhops, streams_resid)

       ELSE

          OPEN(diagunit, FILE=river_file_name)
          WRITE(diagunit,'(A)') "Basin ID, Area [km^2], Nb points, Lon and Lat of outflow"
          WRITE(diagunit,'(A)') "Nb streams, total number of hops, min, ave and max number of hops per stream"
          !
          DO icc = 1, num_largest
             !
             IF ( nb_pts(sortedrivs(icc)) .GT. 2 ) THEN
                !
                name_str = sorted_names(icc)
                !
                WRITE(diagunit,'(I5,A25,F14.5,I5,2F9.2)') topids(sortedrivs(icc)), name_str(1:15), totarea(sortedrivs(icc))/1.e6, &
                     &    nb_pts(sortedrivs(icc)), lalo(outpt(sortedrivs(icc),1),2), lalo(outpt(sortedrivs(icc),1),1)
                WRITE(diagunit,'(5I9,F16.4)') streams_nb(sortedrivs(icc)), &
                     & streams_avehops(sortedrivs(icc))*streams_nb(sortedrivs(icc)), &
                     & streams_minhops(sortedrivs(icc)), &
                     & streams_avehops(sortedrivs(icc)), &
                     & streams_maxhops(sortedrivs(icc)), streams_resid(sortedrivs(icc))
                !
             ENDIF
             !
          ENDDO
          !
          CLOSE(diagunit)
          !
       ENDIF
       !
    ENDIF
    !
    !
    nbrivers(:) = zero
    DO ig=1,nbpt
       nbrivers(ig) = COUNT(route_tobasin(ig,1:nbasmax) == nbasmax+3)
    ENDDO
    DO ig=1,nbpt
       IF ( nbrivers(ig) > 1 ) THEN
          WRITE(numout,*) 'Grid box ', ig, ' has ', NINT(nbrivers(ig)), ' outflow points.'
          WRITE(numout,*) 'The rivers which flow into the ocean at this point are :'
          DO icc=1,nbasmax
             IF ( route_tobasin(ig,icc) == nbasmax+3) THEN
                IF ( global_basinid(ig,icc) <= nb_small ) THEN
                   WRITE(numout,*) 'ID = ',global_basinid(ig,icc), ' Name = ', basin_names(global_basinid(ig,icc))
                ELSE
                   WRITE(numout,*) 'ID = ',global_basinid(ig,icc), ' Problem ===== ID is larger than possible'
                ENDIF
             ENDIF
          ENDDO
       ENDIF
    ENDDO
    !
    WRITE(numout,*) 'Maximum topographic index :', MAXVAL(topo_resid)
    ic = COUNT(topo_resid .GT. 0.)
    WRITE(numout,*) 'Mean topographic index :', SUM(topo_resid, MASK=topo_resid .GT. zero)/ic
    WRITE(numout,*) 'Minimum topographic index :', MINVAL(topo_resid, MASK=topo_resid .GT. zero)
    !
    DEALLOCATE(pts)
    DEALLOCATE(outpt)
    DEALLOCATE(nb_pts)
    DEALLOCATE(totarea, tmparea)
    DEALLOCATE(streams_nb, streams_avehops, streams_minhops, streams_maxhops)
    !
    DEALLOCATE(lbasin_area, lbasin_uparea, lrivercode)
    !
    DEALLOCATE(allstreams)
    DEALLOCATE(tstreams)
    DEALLOCATE(tslen, tpts, tptbas, tuparea)
    DEALLOCATE(tcode)
    !
    WRITE(numout,*) 'Maximum topographic index :', MAXVAL(topo_resid)
    ic = COUNT(topo_resid .GT. 0.)
    WRITE(numout,*) 'Mean topographic index :', SUM(topo_resid, MASK=topo_resid .GT. 0.)/ic
    WRITE(numout,*) 'Minimum topographic index :', MINVAL(topo_resid, MASK=topo_resid .GT. 0.)
    !
    !
  END SUBROUTINE routing_diagnostic
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_diagcode
!!
!>\BRIEF       This subroutine determines the code in the Pfafstetter system for all points
!!              within the given catchment.  
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): streamcode
!!
!! REFERENCES   : None
!!
!! FLOWCHART    :None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_diagcode(ip, tpts, tpbas, tuparea, tslen, ls, allstreams, upstreamchange, streamcode)  
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                   :: ip             !!
    INTEGER(i_std), INTENT(in)                   :: ls             !!
    INTEGER(i_std), DIMENSION(ip), INTENT(in)    :: tpts           !!
    INTEGER(i_std), DIMENSION(ip), INTENT(in)    :: tpbas          !!
    REAL(r_std), DIMENSION(ip), INTENT(in)       :: tuparea        !!
    INTEGER(i_std), DIMENSION(ip), INTENT(in)    :: tslen          !!
    INTEGER(i_std), DIMENSION(ip,ls), INTENT(in) :: allstreams     !!
    REAL(r_std), DIMENSION(ip,ls), INTENT(in)    :: upstreamchange !!
    !
!! OUTPUT VARIABLES
    INTEGER(i_std), DIMENSION(ip), INTENT(out)   :: streamcode     !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                               :: ilev, cntsubbas, ib, ic, i, it, ilevmax, imaxlen, nbzero !!
    INTEGER(i_std)                               :: tstreamcode(ip)!!
    INTEGER(i_std)                               :: indsubbas(ip)  !!
    INTEGER(i_std)                               :: iw(ip)         !!
    INTEGER(i_std)                               :: tdiff(ip)      !!
    INTEGER(i_std)                               :: tmpjunc(4)     !!
    INTEGER(i_std)                               :: junction(4)    !!
    INTEGER(i_std)                               :: ff(1)          !!
    INTEGER(i_std)                               :: ll             !!
    REAL(r_std)                                  :: chguparea(ip)  !!
    REAL(r_std)                                  :: largest        !!

!_ ================================================================================================================================
    !
    streamcode(:) = 0
    !
    ! If we accept 4 grid boxes per coded basin then per level we need at least
    ! 4*9=36 boxes.
    !
    ilevmax = 0
    it = ip
    DO WHILE (it >= 36)
       ilevmax = ilevmax+1
       it = it/9
    ENDDO
    !
    DO ilev=1,ilevmax
       !
       ! Count number of sub-basins we already have
       !
       cntsubbas=0
       tstreamcode(:) = streamcode(:)
       DO WHILE ( COUNT(tstreamcode(:) >= 0) > 0 )
         cntsubbas=cntsubbas+1
         indsubbas(cntsubbas) = MAXVAL(tstreamcode(:))
         WHERE ( tstreamcode(:) == indsubbas(cntsubbas) ) tstreamcode = -1
       ENDDO
       !
       ! Go through all these basins in order to find the next Pfafstetter numbers
       !
       DO ib=1,cntsubbas
          !
          ! Get all the streams which have the current Pfadstetter number
          !
          it=0
          DO ic=1,ip
             IF ( streamcode(ic) == indsubbas(ib) ) THEN
                it =it+1
                iw(it)=ic 
             ENDIF
          ENDDO
          !
          ! Which is the longest stream in this basin ?
          !
          ff=MAXLOC(tslen(iw(1:it)))
          imaxlen=iw(ff(1))
          chguparea(:) = zero
          chguparea(1:tslen(imaxlen)) = upstreamchange(imaxlen, 1:tslen(imaxlen))
          !
          IF ( COUNT(chguparea(1:tslen(imaxlen)) > 0) < 4 ) THEN
             !
             ! If this subbasin is too small we just set all points to zero
             !
             DO i=1,it
                streamcode(iw(i)) = streamcode(iw(i))*10
             ENDDO
          ELSE
             ! 
             ! Else do the Pfafstetter numbering
             ! 
             !
             ! Where do we have the 4 largest change in upstream area on this stream.
             ! This must be the confluence of 2 rivers and thus a junction point.
             !
             largest=pi*R_Earth*R_Earth
             DO i=1,4
                ff = MAXLOC(chguparea(1:tslen(imaxlen)), MASK = chguparea(1:tslen(imaxlen)) < largest)
                tmpjunc(i) = ff(1)
                largest=chguparea(tmpjunc(i))
             ENDDO
             ! sort junctions to go from the outflow up-stream
             ff(1)=0
             DO i=1,4
                junction(i) = MINVAL(tmpjunc, MASK=tmpjunc > ff(1))
                ff(1) = junction(i)
             ENDDO
             !
             ! Find all streams which are identical up to that junction and increase their code accordingly 
             !
             DO i=1,it
                ll=MIN(tslen(imaxlen),tslen(iw(i)))
                tdiff(1:ll) = allstreams(imaxlen,1:ll)-allstreams(iw(i),1:ll)
                nbzero = COUNT(tdiff(1:ll) == 0)
                IF (nbzero < junction(1) ) THEN
                   ! Before first of the 4 largest basins
                   streamcode(iw(i)) = streamcode(iw(i))*10+1
                ELSE IF (nbzero == junction(1) ) THEN
                   ! Stream part of the first largest basin
                   streamcode(iw(i)) = streamcode(iw(i))*10+2
                ELSE IF (nbzero < junction(2) ) THEN
                   ! Between first and second stream
                   streamcode(iw(i)) = streamcode(iw(i))*10+3
                ELSE IF (nbzero == junction(2) ) THEN
                   ! Stream part of the second basin
                   streamcode(iw(i)) = streamcode(iw(i))*10+4
                ELSE IF (nbzero < junction(3) ) THEN
                   ! In between stream 2 and 3
                   streamcode(iw(i)) = streamcode(iw(i))*10+5
                ELSE IF (nbzero == junction(3) ) THEN
                   ! Part of 3rd basin
                   streamcode(iw(i)) = streamcode(iw(i))*10+6
                ELSE IF (nbzero < junction(4) ) THEN
                   ! In between 3 and 4th basins
                   streamcode(iw(i)) = streamcode(iw(i))*10+7
                ELSE IF (nbzero == junction(4) ) THEN
                   ! Final of the 4 largest basins
                   streamcode(iw(i)) = streamcode(iw(i))*10+8
                ELSE
                   ! The rest of the points and also the basin of the longest stream
                   streamcode(iw(i)) = streamcode(iw(i))*10+9
                ENDIF
             ENDDO
          ENDIF
       ENDDO
       !
    ENDDO
    !
    !
  END SUBROUTINE routing_diagcode
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_diagncfile
!!
!>\BRIEF         This subroutine creates a netCDF file containing all the informations
!!                on the largest rivers which can be used for a refined analysis. 
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_diagncfile(river_file_name, nbpt, lalo, nb_pts, topids, sorted_names, sortedrivs, &
       &       lbasin_index, lbasin_area, lbasin_uparea, lrivercode, outpt, streams_nb, streams_avehops, &
       &       streams_minhops, streams_maxhops, streams_resid)
    !
    USE netcdf
    !
    IMPLICIT NONE
    !
    !
!! INPUT VARIABLES
    REAL(r_std), INTENT(in)                     :: lalo(nbpt,2)             !! Vector of latitude and longitudes (beware of the order !)

!! LOCAL VARIABLES
    CHARACTER(LEN=80)                           :: river_file_name          !! Filename in which we write the description of the rivers (1)
    INTEGER(i_std)                              :: nbpt                     !! Domain size  (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: nb_pts                   !! Number of points in the basin (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: topids                   !! The IDs of the first num_largest basins (unitless)
    CHARACTER(LEN=25), DIMENSION(num_largest)   :: sorted_names             !! Names of the basins to be put into the file (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: sortedrivs               !!
    INTEGER(i_std), DIMENSION(num_largest,nbpt) :: lbasin_index             !!
    REAL(r_std), DIMENSION(num_largest,nbpt)    :: lbasin_area              !!
    REAL(r_std), DIMENSION(num_largest,nbpt)    :: lbasin_uparea            !!
    INTEGER(i_std), DIMENSION(num_largest,nbpt) :: lrivercode               !!
    !
    INTEGER(i_std), DIMENSION(num_largest,2)    :: outpt                    !! Outflow point for each basin (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: streams_nb               !! Number of streams in basin (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: streams_avehops          !! Average number of hops in streams (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: streams_minhops          !! Minimum number of hops in streams (unitless)
    INTEGER(i_std), DIMENSION(num_largest)      :: streams_maxhops          !! Minimum number of hops in streams (unitless)
    REAL(r_std), DIMENSION(num_largest)         :: streams_resid            !! Average residence time
    !
    INTEGER(i_std)                              :: icc, fid, iret, ierr_tot, ib, ij, ik, i, j, lcc !! Indices (unitless)
    INTEGER(i_std)                              :: nlonid, nlatid, varid, varid2, varid3
    INTEGER(i_std)                              :: dims(2)                  !!
    REAL(r_std)                                 :: lon_min, lon_max, lat_min, lat_max
    CHARACTER(LEN=80)                           :: lon_name, lat_name, var_name, long_name, nc_name, att_str
    CHARACTER(LEN=15)                           :: gridtype                 !!
    !
    REAL(r_std)                                 :: basinfrac(iim_g,jjm_g)   !!
    REAL(r_std)                                 :: basinuparea(iim_g,jjm_g) !!
    INTEGER(i_std)                              :: basincode(iim_g,jjm_g)   !!
    !
    LOGICAL                                     :: check=.FALSE.            !! (true/false)
    !
!! PARAMETERS
    INTEGER(i_std),PARAMETER                    :: kind_r_diag=NF90_REAL8   !!
    INTEGER(i_std),PARAMETER                    :: kind_i_diag=NF90_INT     !!

!_ ================================================================================================================================
    !
    !
    ! 1.0 Create the NETCDF file and store the coordinates.
    !
    ! This variable should be defined and computed in the module grid.f90.
    ! Jan
    gridtype="regular"
    !
    iret = NF90_CREATE(TRIM(river_file_name), NF90_CLOBBER, fid)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not create file :', &
            & TRIM(river_file_name), '(Problem with disk place or filename ?)')
    ENDIF
    !
    ! 1.1 Define dimensions
    !
    IF ( INDEX(gridtype, "regular") == 1 ) THEN
       !
       ! 1.1.1 regular grid
       !
       iret = NF90_DEF_DIM(fid, 'lon', iim_g, dims(1))
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Dimension "x" can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
       iret = NF90_DEF_DIM(fid, 'lat', jjm_g, dims(2))
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Dimension "y" can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
    ELSE
       !
       ! 1.1.2 irregular grid
       !
       iret = NF90_DEF_DIM(fid, 'x', iim_g, dims(1))
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Dimension "x" can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
       
       iret = NF90_DEF_DIM(fid, 'y', jjm_g, dims(2))
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Dimension "y" can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
    ENDIF
    !
    !
    ! 1.2 Define variables and attributes
    !
    IF ( INDEX(gridtype, "regular") == 1 ) THEN
       !
       ! 1.2.1 regular grid
       !
       lon_name = 'lon'
       !
       iret = NF90_DEF_VAR(fid, lon_name, kind_r_diag, dims(1), nlonid)
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//lon_name//' can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
       !
       lat_name = 'lat'
       iret = NF90_DEF_VAR(fid, lat_name, kind_r_diag, dims(2), nlatid)
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//lat_name//' can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
       !
    ELSE
       !
       ! 1.2.2 irregular grid
       !
       lon_name = 'nav_lon'
       !
       iret = NF90_DEF_VAR(fid, lon_name, kind_r_diag, dims, nlonid)
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//lon_name//' can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
       !
       lat_name = 'nav_lat'
       iret = NF90_DEF_VAR(fid, lat_name, kind_r_diag, dims, nlatid)
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//lat_name//' can not be defined for the file : ', &
               &         TRIM(river_file_name),'(Solution ?)')
       ENDIF
       !
    ENDIF
    !
    ! 1.3 Add attributes to the coordinate variables
    !
    iret = NF90_PUT_ATT(fid, nlonid, 'units', "degrees_east") 
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lon_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    !
    lon_min = -180.
    lon_max = 180.
    !
    iret = NF90_PUT_ATT(fid, nlonid, 'valid_min', lon_min)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lon_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    iret = NF90_PUT_ATT(fid, nlonid, 'valid_max', lon_max)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lon_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    !
    iret = NF90_PUT_ATT(fid, nlonid, 'long_name', "Longitude")
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lon_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    iret = NF90_PUT_ATT(fid, nlatid, 'units', "degrees_north")
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lat_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    !
    lat_max = 90.
    lat_min = -90.
    !
    iret = NF90_PUT_ATT(fid, nlatid, 'valid_min', lat_min)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lat_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    iret = NF90_PUT_ATT(fid, nlatid, 'valid_max', lat_max)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lat_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    iret = NF90_PUT_ATT(fid, nlatid, 'long_name', "Latitude")
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not add attribut to variable '//lat_name//' for the file :', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    !
    iret = NF90_ENDDEF(fid)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', 'Could not end definitions in the file : ', &
 &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    !
    !  1.4 Write coordinates
    !
    IF ( INDEX(gridtype, "regular") == 1 ) THEN
       !
       ! 1.4.1 regular grid
       !
       iret = NF90_PUT_VAR(fid, nlonid, lon_g(1:iim_g,1))
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Could not put variable nav_lon  in the file : ', &
               &          TRIM(river_file_name),'(Solution ?)')
       ENDIF
       !
       iret = NF90_PUT_VAR(fid, nlatid, lat_g(1,1:jjm_g))
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Could not put variable nav_lat  in the file : ', &
               &          TRIM(river_file_name),'(Solution ?)')
       ENDIF
    ELSE
       !
       ! 1.4.2 irregular grid
       !
       iret = NF90_PUT_VAR(fid, nlonid, lon_g)
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Could not put variable nav_lon  in the file : ', &
               &          TRIM(river_file_name),'(Solution ?)')
       ENDIF
       !
       iret = NF90_PUT_VAR(fid, nlatid, lat_g)
       IF (iret /= NF90_NOERR) THEN
          CALL ipslerr_p (3,'routing_diagncfile', 'Could not put variable nav_lat  in the file : ', &
               &          TRIM(river_file_name),'(Solution ?)')
       ENDIF
    ENDIF
    !
    ! 2.0 Go through all basins and wirte the information into the netCDF file.
    !
    DO icc = 1, num_largest
       !
       ! 2.1 Compute the fields to be saved in the file
       !
       ib=sortedrivs(icc)
       !
       !
       IF ( nb_pts(ib) > 2 ) THEN
          !
          basinfrac(:,:) = zero
          basinuparea(:,:) = zero
          basincode(:,:) = zero
          !
          DO ij=1, nb_pts(ib)

             ik=lbasin_index(ib,ij)

             j = ((index_g(ik)-1)/iim_g) + 1
             i = (index_g(ik)-(j-1)*iim_g)

             basinfrac(i,j) = basinfrac(i,j) + lbasin_area(ib,ij)/(resolution_g(ik,1)*resolution_g(ik,2))
             basinuparea(i,j) = MAX(basinuparea(i,j), lbasin_uparea(ib,ij))
             basincode(i,j) = lrivercode(ib,ij)

          ENDDO
          !
          DO i=1,iim_g
             DO j=1,jjm_g
                IF ( basinfrac(i,j) <= EPSILON(zero) ) THEN
                   basinfrac(i,j) = undef_sechiba
                   basinuparea(i,j)  = undef_sechiba
                   basincode(i,j)  = undef_int
                ELSE
                   basinfrac(i,j) = MAX(basinfrac(i,j), un)
                ENDIF 
             ENDDO
          ENDDO
          !
          ! 
          ! 2.2 Define the variables in the netCDF file
          !
          iret = NF90_REDEF(fid)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', &
                  &          'Could not restart definitions in the file : ', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ! Create a name more suitable for a variable in a netCDF file
          !
          nc_name =  TRIM(sorted_names(icc))
          ! Take out all character which could cause problems
          lcc=LEN_TRIM(nc_name)
          DO ij=1,lcc
             IF ( nc_name(ij:ij) == " " ) nc_name(ij:ij) = "_"
             IF ( nc_name(ij:ij) == "(" ) nc_name(ij:ij) = "_"
             IF ( nc_name(ij:ij) == ")" ) nc_name(ij:ij) = "_"
          ENDDO
          ! reduce redundant "__"
          DO ij=1,lcc
             IF ( nc_name(ij:ij+1) == "__" ) nc_name(ij+1:)=nc_name(ij+2:lcc)
          ENDDO
          lcc=LEN_TRIM(nc_name)
          IF ( nc_name(lcc:lcc) == "_" ) nc_name(lcc:lcc) = " "
          !
          !
          ! 2.3 Fraction variable
          !
          IF (check) WRITE(*,*) "Define Fraction variable and add attributes"
          !
          var_name =  TRIM(nc_name)//"_frac"
          !
          iret = NF90_DEF_VAR(fid, TRIM(var_name), kind_r_diag, dims, varid)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//TRIM(var_name)//' can not be defined for the file : ', &
                  &         TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ierr_tot = 0
          ! Units
          iret = NF90_PUT_ATT(fid, varid, 'units', "-")
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Units',  iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          ! Long name
          long_name = "Fraction of basin "//TRIM(sorted_names(icc))//" per grid box"
          iret = NF90_PUT_ATT(fid, varid, 'long_name', long_name)
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Long_Name', long_name, iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          ! Missing value
          iret = NF90_PUT_ATT(fid, varid, 'missing_value', undef_sechiba)
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Missing value', undef_sechiba, iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ib=sortedrivs(icc)
          IF ( check ) WRITE(*,*) "Doing basin ", icc," corrsdponding to index = ", ib, "num_largest : ", num_largest
          !
          ! Nb of grid points in basin
          att_str='Nb_of_grid_points_in_basin'
          iret = NF90_PUT_ATT(fid, varid, att_str, nb_pts(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Nb of grid points in basin', nb_pts(ib), iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Longitude of outflow point 
          att_str='Longitude_of_outflow_point'
          iret = NF90_PUT_ATT(fid, varid, att_str, lalo(outpt(ib,1),2))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Longitude of outflow point', lalo(outpt(ib,1),2), iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Latitide of outflow point
          att_str='Latitude_of_outflow_point'
          iret = NF90_PUT_ATT(fid, varid, att_str, lalo(outpt(ib,1),1))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Latitude of outflow point',  lalo(outpt(ib,1),1), iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Number of streams
          att_str= 'Number_of_streams'
          iret = NF90_PUT_ATT(fid, varid, att_str, streams_nb(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Number of streams', streams_nb(ib), iret
             WRITE(*,*) TRIM(NF90_STRERROR(iret))
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Total number of hops to go to the oceans
          att_str='Total_number_of_hops_to_ocean'
          iret = NF90_PUT_ATT(fid, varid, att_str, streams_avehops(ib)*streams_nb(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Total number of hops to go to the oceans ', streams_avehops(ib)*streams_nb(ib), iret
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Minimum number of hops to go to the ocean for any stream
          att_str='Minimum_number_of_hops_to_ocean_for_any_stream'
          iret = NF90_PUT_ATT(fid, varid, att_str, streams_minhops(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Minimum number of hops to go tp the ocean for any stream', streams_minhops(ib), iret
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Average number of hops to go to the ocean for any stream 
          att_str='Average_number_of_hops_to_ocean_for_any_stream'
          iret = NF90_PUT_ATT(fid, varid, att_str, streams_avehops(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Average number of hops to go tp the ocean for any stream', streams_avehops(ib), iret
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Maximum number of hops to go to the ocean for any stream 
          att_str='Maximum_number_of_hops_to_ocean_for_any_stream'
          iret = NF90_PUT_ATT(fid, varid, att_str, streams_maxhops(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Maximum number of hops to go tp the ocean for any stream', streams_maxhops(ib), iret
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          ! Average residence time in the basin
          att_str='Average_residence_time_in_basin'
          iret = NF90_PUT_ATT(fid, varid, att_str, streams_resid(ib))
          IF (iret /= NF90_NOERR) THEN
             WRITE(*,*) 'Average residence time in the basin', streams_resid(ib), iret
             ierr_tot = ierr_tot + 1
          ENDIF
          !
          IF (ierr_tot > 0 ) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Could not add some attributes to variable '//var_name//' for the file :', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ! 2.4 Upstream area variable variable
          !
          IF (check) WRITE(*,*) "Define Upstream variable and add attributes"
          !
          ! Create a name more suitable for a variable in a netCDF file
          !
          var_name =  TRIM(nc_name)//"_upstream"
          DO ij=1,LEN_TRIM(var_name)
             IF ( var_name(ij:ij) == " " ) var_name(ij:ij) = "_"
          ENDDO
          !
          iret = NF90_DEF_VAR(fid, TRIM(var_name), kind_r_diag, dims, varid2)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//TRIM(var_name)//' can not be defined for the file : ', &
                  &         TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ierr_tot = 0
          ! Units
          iret = NF90_PUT_ATT(fid, varid2, 'units', "m^2")
          IF (iret /= NF90_NOERR) ierr_tot = ierr_tot + 1
          ! Long name
          long_name = "Upstream area of basin "//TRIM(sorted_names(icc))//" in the grid box"
          iret = NF90_PUT_ATT(fid, varid2, 'long_name', long_name)
          IF (iret /= NF90_NOERR) ierr_tot = ierr_tot + 1
          ! Missing value
          iret = NF90_PUT_ATT(fid, varid2, 'missing_value', undef_sechiba)
          IF (iret /= NF90_NOERR) ierr_tot = ierr_tot + 1
          !
          IF (ierr_tot > 0 ) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Could not add some attributes to variable '//var_name//' for the file :', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ! 2.5 Pfafstetter codes for basins
          !
          IF (check) WRITE(*,*) "Define Pfafstetter codes variable and add attributes"
          !
          var_name =  TRIM(nc_name)//"_coding"
          DO ij=1,LEN_TRIM(var_name)
             IF ( var_name(ij:ij) == " " ) var_name(ij:ij) = "_"
          ENDDO
          !
          iret = NF90_DEF_VAR(fid, TRIM(var_name), kind_i_diag, dims, varid3)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Variable '//TRIM(var_name)//' can not be defined for the file : ', &
                  &         TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ierr_tot = 0
          ! Units
          iret = NF90_PUT_ATT(fid, varid3, 'units', "-")
          IF (iret /= NF90_NOERR) ierr_tot = ierr_tot + 1
          ! Long name
          long_name = "Pfafstetter codes of grid boxes in basin "//TRIM(sorted_names(icc))
          iret = NF90_PUT_ATT(fid, varid3, 'long_name', long_name)
          IF (iret /= NF90_NOERR) ierr_tot = ierr_tot + 1
          ! Missing value
          iret = NF90_PUT_ATT(fid, varid3, 'missing_value', undef_int)
          IF (iret /= NF90_NOERR) ierr_tot = ierr_tot + 1
          !
          IF (ierr_tot > 0 ) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Could not add some attributes to variable '//var_name//' for the file :', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ! 2.6 ENDDEF of netCDF file
          !
          IF (check) WRITE(*,*) "END define"
          !
          iret = NF90_ENDDEF(fid)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', &
                  &          'Could not end definitions in the file : ', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
          ! 2.7 Write the data to the file
          !
          IF (check) WRITE(*,*) "Put basinfrac"
          iret = NF90_PUT_VAR(fid, varid, basinfrac)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Could not put variable basinfrac in the file : ', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF

          IF (check) WRITE(*,*) "Put basinuparea"
          iret = NF90_PUT_VAR(fid, varid2, basinuparea)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagncfile', 'Could not put variable basinuparea in the file : ', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF

          IF (check) WRITE(*,*) "Put basincode"
          iret = NF90_PUT_VAR(fid, varid3, basincode)
          IF (iret /= NF90_NOERR) THEN
             CALL ipslerr_p (3,'routing_diagfile', 'Could not put variable basincode in the file : ', &
                  &          TRIM(river_file_name),'(Solution ?)')
          ENDIF
          !
       ENDIF
       !
    ENDDO
    !
    IF (check) WRITE(*,*) "Close file"
    !
    ! Close netCDF file and do some memory management.
    !
    iret = NF90_CLOSE(fid)
    IF (iret /= NF90_NOERR) THEN
       CALL ipslerr_p (3,'routing_diagncfile', &
            &          'Could not end definitions in the file : ', &
            &          TRIM(river_file_name),'(Solution ?)')
    ENDIF
    !
    !
  END SUBROUTINE routing_diagncfile
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_basins_p
!!
!>\BRIEF        This parallelized subroutine computes the routing map if needed. 
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_basins_p(nbpt, lalo, neighbours, resolution, contfrac)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in) :: nbpt               !! Domain size (unitless)
    REAL(r_std), INTENT(in)    :: lalo(nbpt,2)       !! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in) :: neighbours(nbpt,8) !! Vector of neighbours for each grid point (1=N, 2=NE, 3=E, 4=SE, 5=S, 6=SW, 7=W, 8=NW) (unitless)
    REAL(r_std), INTENT(in)    :: resolution(nbpt,2) !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)    :: contfrac(nbpt)     !! Fraction of land in each grid box (unitless;0-1)

!_ ================================================================================================================================

!    INTEGER(i_std)    :: neighbours_tmp(nbpt,8)
!    INTEGER(i_std) :: i,j
    
!    DO i=1,nbp_loc
!      DO j=1,8
!	IF (neighbours(i,j)==-1) THEN
!	  neighbours_tmp(i,j)=neighbours(i,j)
!	ELSE
!	  neighbours_tmp(i,j)=neighbours(i,j)+nbp_para_begin(mpi_rank)-1
!	ENDIF  
!      ENDDO
!    ENDDO

    routing_area => routing_area_glo  
    topo_resid => topo_resid_glo
    route_togrid => route_togrid_glo
    route_tobasin => route_tobasin_glo
    route_nbintobas => route_nbintobas_glo
    global_basinid => global_basinid_glo
 
    IF (is_root_prc) CALL routing_basins(nbp_glo,lalo_g, neighbours_g, resolution_g, contfrac_g)

    routing_area => routing_area_loc  
    topo_resid => topo_resid_loc
    route_togrid => route_togrid_loc
    route_tobasin => route_tobasin_loc
    route_nbintobas => route_nbintobas_loc
    global_basinid => global_basinid_loc

    CALL scatter(routing_area_glo,routing_area_loc)
    CALL scatter(topo_resid_glo,topo_resid_loc)
    CALL scatter(route_togrid_glo,route_togrid_loc)
    CALL scatter(route_tobasin_glo,route_tobasin_loc)
    CALL scatter(route_nbintobas_glo,route_nbintobas_loc)
    CALL scatter(global_basinid_glo,global_basinid_loc)
    
  END SUBROUTINE routing_basins_p
  ! 
 
!! ================================================================================================================================
!! SUBROUTINE 	: routing_basins
!!
!>\BRIEF        This non-parallelized subroutine reads in the map of basins and flow direction to construct
!!              the catchments of each grid box. 
!!
!! DESCRIPTION (definitions, functional, design, flags) :
!! The work is done in a number of steps which are performed locally on the 
!! GCM grid:
!!  1) First we find the grid-points of the high resolution routing grid which are
!!     within the coarser grid of the GCM.
!!  2) When we have these grid points we decompose them into basins in the routine
!!     routing_findbasins. A number of simplifications are done if needed.
!!  3) In the routine routing_globalize we put the basin information of this grid
!!     into the global fields.
!! Then we work on the global grid to perform the following tasks :
!!  1) We link up the basins of the various grid points and check the global consistency.
!!  2) The area of each outflow point is computed.
!!  3) The final step is to reduce the number of basins in order to fit into the truncation.\n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): None, as the routine puts information into the global variables of the module.
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_basins(nbpt, lalo, neighbours, resolution, contfrac)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                    :: nbpt                  !! Domain size (unitless)
    REAL(r_std), INTENT(in)                       :: lalo(nbpt,2)          !! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)                    :: neighbours(nbpt,8)    !! Vector of neighbours for each grid point (1=N, 2=NE, 3=E, 4=SE, 5=S, 6=SW, 7=W, 8=NW) (unitless)
    REAL(r_std), INTENT(in)                       :: resolution(nbpt,2)    !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)                       :: contfrac(nbpt)        !! Fraction of land in each grid box (unitless;0-1)
    !
!! LOCAL VARIABLES
    CHARACTER(LEN=80)                             :: filename              !! Name of the netcdf file (unitless)
    INTEGER(i_std)                                :: iml, jml, lml, tml, fid, ib, ip, jp, fopt !! Indices (unitless)
    REAL(r_std)                                   :: lev(1), date, dt, coslat
    INTEGER(i_std)                                :: itau(1)               !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: trip                  !! The trip field (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: basins                !! The basin field (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: topoindex             !! Topographic index of the residence time (m)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: hierarchy             !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lat_rel               !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lon_rel               !!
    !
    INTEGER(i_std)                                :: nbi, nbj              !! Number of point in x and y within the grid (unitless)
    REAL(r_std)                                   :: min_topoind           !! The current minimum of topographic index (m)
    REAL(r_std)                                   :: max_basins            !!
    REAL(r_std)                                   :: invented_basins       !!
    !
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: area_bx               !! Area of each small box in the grid box (m^2)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: hierarchy_bx          !! Level in the basin of the point
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lon_bx                !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lat_bx                !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: topoind_bx            !! Topographic index of the residence time for each of the smaller boxes (m)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: trip_bx               !! The trip field for each of the smaller boxes (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: basin_bx              !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: coast_pts             !! The coastal flow points (unitless)
    !
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: basin_count           !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: basin_id              !!
    REAL(r_std),  ALLOCATABLE, DIMENSION(:,:)     :: basin_area            !!
    REAL(r_std),  ALLOCATABLE, DIMENSION(:,:)     :: basin_hierarchy       !!
    REAL(r_std),  ALLOCATABLE, DIMENSION(:,:)     :: basin_topoind         !! Topographic index of the residence time for a basin (m)
    REAL(r_std),  ALLOCATABLE, DIMENSION(:,:)     :: fetch_basin           !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: basin_flowdir         !! Water flow directions in the basin (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: outflow_grid          !! Type of outflow on the grid box (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: outflow_basin         !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: inflow_number         !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:) :: inflow_basin          !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:) :: inflow_grid           !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: nbcoastal             !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: coastal_basin         !!
    !
    ! Interpolation help variables
    !
    INTEGER(i_std)                                :: nix, njx              !!
    CHARACTER(LEN=30)                             :: callsign              !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:)    :: resol_lu              !! Resolution
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: mask                  !! Mask to exclude some points (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: sub_area              !! Area on the fine grid (m^2)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:) :: sub_index             !! Indices of the points we need on the fine grid (unitless)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: sub_pts               !! Number of high resolution points on this grid (unitless)
    INTEGER                                       :: ALLOC_ERR             !!
    LOGICAL                                       :: ok_interpol = .FALSE. !! Flag for interpolation (true/false)
    !
    INTEGER(i_std)                                :: nb_basin              !! Number of sub-basins (unitless)
    INTEGER(i_std)                                :: nwbas                 !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: basin_inbxid          !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: basin_sz              !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:)     :: basin_bxout           !!
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:) :: basin_pts             !!
    CHARACTER(LEN=7)                              :: fmt                   !!
    LOGICAL                                       :: debug = .FALSE.       !! (true/false)
    !
    INTEGER(i_std), DIMENSION(2)                  :: diagbox = (/ 1, 2 /)  !!

!_ ================================================================================================================================
    !
    !
    IF ( .NOT. is_root_prc) THEN
       WRITE(numout,*) "is_root_prc = ", is_root_prc
       CALL ipslerr_p (3,'routing_basins', &
            &          'routing_basins is not suitable for running in parallel', &
            &          'We are here on a non root processor. ','(STOP from routing_basins)')
    ENDIF
    !
    ! Test on diagbox and nbpt
    !
    IF (debug) THEN
       IF (ANY(diagbox .GT. nbpt)) THEN 
          WRITE(numout,*) "Debug diganostics : nbpt, diagbox", nbpt, diagbox
          call ipslerr_p(3,'routing_basin', &
               &      'Problem with diagbox in debug mode.', & 
               &      'diagbox values can''t be greater than land points number.', &
               &      '(decrease diagbox wrong value)')
       ENDIF
    ENDIF
    !
    !
    !  Needs to be a configurable variable
    !
    !
    !Config Key   = ROUTING_FILE
    !Config Desc  = Name of file which contains the routing information
    !Config If    = RIVER_ROUTING
    !Config Def   = routing.nc
    !Config Help  = The file provided here should alow the routing module to
    !Config         read the high resolution grid of basins and the flow direction 
    !Config         from one mesh to the other.
    !Config Units = [FILE]
    !
    filename = 'routing.nc'
    CALL getin('ROUTING_FILE',filename)
    !
    CALL flininfo(filename,iml, jml, lml, tml, fid)
    CALL flinclo(fid)
    !
    ! soils_param.nc file is 1° soit texture file.
    !
    ALLOCATE(lat_rel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for lat_rel','','')

    ALLOCATE(lon_rel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for lon_rel','','')

    ALLOCATE (trip(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for trip','','')

    ALLOCATE (basins(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basins','','')

    ALLOCATE (topoindex(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for topoindex','','')

    ALLOCATE (hierarchy(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for hierarchy','','')

    !
    CALL flinopen(filename, .FALSE., iml, jml, lml, lon_rel, lat_rel, lev, tml, itau, date, dt, fid)
    !!
    !! From the basin description data we will read the following variables :
    !!
    !! Trip : Provides the flow direction following the convention :
    !! trip = 1 : flow = N
    !! trip = 2 : flow = NE
    !! trip = 3 : flow = E
    !! trip = 4 : flow = SE
    !! trip = 5 : flow = S
    !! trip = 6 : flow = SW
    !! trip = 7 : flow = W
    !! trip = 8 : flow = NW
    !! trip = 97 : return flow into the ground
    !! trip = 98 : coastal flow (diffuse flow into the oceans)
    !! trip = 99 : river flow into the oceans
    !!
    !! Basins : Provides a uniqe ID for each basin. These IDs are also used to get
    !! the name of the basin from the table in routine routing_names.
    !! 
    !! Topoind :  is the topographic index for the retention time of the water in the
    !! grid box. It has been computed with the following formula : 1000 x sqrt(d^3/Dz) 
    !! where d is the distance of the river from the current grid box to the next one
    !! as indicated by the variable trip.
    !! Dz the hight difference between between the two grid boxes.
    !! All these variables are in meters.
    !! Furthermore  we have to limit the height difference to 5m in order to avoid any unpleasant
    !! surprises. If dz < 5m then dz=5.
    !!
    !
    CALL flinget(fid, 'trip', iml, jml, lml, tml, 1, 1, trip)
    !
    CALL flinget(fid, 'basins', iml, jml, lml, tml, 1, 1, basins)
    !
    CALL flinget(fid, 'topoind', iml, jml, lml, tml, 1, 1, topoindex)
    !
    CALL flinclo(fid)
    !
    min_topoind = MINVAL(topoindex, MASK=topoindex .LT. undef_sechiba-un)
    !
    DO ip=1,iml
       DO jp=1,jml
          IF ( trip(ip,jp) < 1.e10 .AND. topoindex(ip,jp) > 1.e10) THEN
             WRITE(numout,*) 'trip exists but not topoind :'
             WRITE(numout,*) 'ip, jp :', ip, jp
             WRITE(numout,*) 'trip, topoind : ', trip(ip,jp), topoindex(ip,jp)
             CALL ipslerr_p(3,'routing_basins','trip exists but not topoind','','')
          ENDIF
       ENDDO
    ENDDO

    ALLOCATE(resol_lu(iml,jml,2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for resol_lu','','')

    ALLOCATE(mask(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for mask','','')
    !
    ! Consider all points a priori
    !
    mask(:,:) = 0
    !
    DO ip=1,iml
       DO jp=1,jml
          !
          ! Determine the land mask of the basin map read from the file ROUTING_FILE
          !
          IF ( trip(ip,jp) < 1.e10 ) THEN
             mask(ip,jp) = 1
          ENDIF
          !
          ! Resolution in longitude
          !
          coslat = MAX( COS( lat_rel(ip,jp) * pi/180. ), mincos )     
          IF ( ip .EQ. 1 ) THEN
             resol_lu(ip,jp,1) = ABS( lon_rel(ip+1,jp) - lon_rel(ip,jp) ) * pi/180. * R_Earth * coslat
          ELSEIF ( ip .EQ. iml ) THEN
             resol_lu(ip,jp,1) = ABS( lon_rel(ip,jp) - lon_rel(ip-1,jp) ) * pi/180. * R_Earth * coslat
          ELSE
             resol_lu(ip,jp,1) = ABS( lon_rel(ip+1,jp) - lon_rel(ip-1,jp) )/2. * pi/180. * R_Earth * coslat
          ENDIF
          !
          ! Resolution in latitude
          !
          IF ( jp .EQ. 1 ) THEN
             resol_lu(ip,jp,2) = ABS( lat_rel(ip,jp) - lat_rel(ip,jp+1) ) * pi/180. * R_Earth
          ELSEIF ( jp .EQ. jml ) THEN
             resol_lu(ip,jp,2) = ABS( lat_rel(ip,jp-1) - lat_rel(ip,jp) ) * pi/180. * R_Earth
          ELSE
             resol_lu(ip,jp,2) =  ABS( lat_rel(ip,jp-1) - lat_rel(ip,jp+1) )/2. * pi/180. * R_Earth
          ENDIF
          !
       ENDDO
    ENDDO
    !
    ! The maximum number of points of the source map (basin description here) which can fit into
    ! any grid point of the ORCHIDEE grid is stimated here.
    ! Some margin is taken.
    !
    callsign = "routing_basins"
    ok_interpol = .FALSE.
    !  
    nix=INT(MAXVAL(resolution_g(:,1))/MAXVAL(resol_lu(:,:,1)))+2
    njx=INT(MAXVAL(resolution_g(:,2))/MAXVAL(resol_lu(:,:,2)))+2
    nbvmax = nix*njx*2
    !
    ! We are on the root processor here as this routine is not in parallel. So no need to broadcast.
    !
    WRITE(numout,*) "Projection arrays for ",callsign," : "
    WRITE(numout,*) "Routing : nbvmax = ", nbvmax


    ALLOCATE (sub_area(nbpt,nbvmax), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for sub_area','','')
    sub_area(:,:)=zero

    ALLOCATE (sub_index(nbpt,nbvmax,2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for sub_index','','')
    sub_index(:,:,:)=0

    ALLOCATE (sub_pts(nbpt), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for sub_pts','','')
    sub_pts(:)=0
    !
    ! routine aggregate will for each point of the ORCHIDEE grid determine which points
    ! of the source grid (basin definitions here) fit in there and which fraction of
    ! of the ORCHIDEE grid it represents.
    !
    CALL aggregate(nbpt, lalo, neighbours, resolution, contfrac, &
         &                iml, jml, lon_rel, lat_rel, mask, callsign, &
         &                nbvmax, sub_index, sub_area, ok_interpol)
    !
    WHERE (sub_area < 0) sub_area=zero
    !
    ! Some verifications
    !
    DO ib=1,nbpt
       sub_pts(ib) = COUNT(sub_area(ib,:) > zero)
       DO fopt=1,sub_pts(ib)
          IF (sub_area(ib, fopt) == 0 ) THEN
             WRITE(numout,*) "Zero Area - Sub_area > 0 : ", ib, fopt
             WRITE(numout,*) "Zero Area - lon : ",lalo(ib,2)
             WRITE(numout,*) "Zero Area - lon_rel : ", lon_rel(sub_index(ib, fopt, 1),sub_index(ib, fopt, 2))
             WRITE(numout,*) "Zero Area - lat : ",lalo(ib,1)
             WRITE(numout,*) "Zero Area - lat_rel : ", lat_rel(sub_index(ib, fopt, 1),sub_index(ib, fopt, 2))
          ENDIF
       ENDDO
    ENDDO
    !
    ! Do some memory management.
    !
    nwbas = MAXVAL(sub_pts)
    !
    ALLOCATE (area_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for area_bx','','')
    ALLOCATE (hierarchy_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for hierarchy_bx','','')
    ALLOCATE (lon_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for lon_bx','','')
    ALLOCATE (lat_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for lat_bx','','')
    ALLOCATE (topoind_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for topoind_bx','','')
    ALLOCATE (trip_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for trip_bx','','')
    ALLOCATE (basin_bx(nbvmax,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_bx','','')
    ALLOCATE (coast_pts(nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for coast_pts','','')
    ALLOCATE (basin_inbxid(nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_inbxid','','')
    ALLOCATE (basin_sz(nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_sz','','')
    ALLOCATE (basin_pts(nbvmax,nbvmax,2), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_pts','','')
    ALLOCATE (basin_bxout(nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_bxout','','')
    ALLOCATE (basin_count(nbpt), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_count','','')
    ALLOCATE (basin_area(nbpt,nwbas), basin_hierarchy(nbpt,nwbas), basin_topoind(nbpt,nwbas), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_area','','')
    ALLOCATE (fetch_basin(nbpt,nwbas), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for fetch_basin','','')
    ALLOCATE (basin_id(nbpt,nwbas),  basin_flowdir(nbpt,nwbas), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for basin_id','','')
    ALLOCATE (outflow_grid(nbpt,nwbas),outflow_basin(nbpt,nwbas), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for outflow_grid','','')
    ALLOCATE (inflow_number(nbpt,nwbas), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for inflow_number','','')
    ALLOCATE (inflow_basin(nbpt,nwbas,nbvmax), inflow_grid(nbpt,nwbas,nbvmax), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for inflow_basin','','')
    ALLOCATE (nbcoastal(nbpt), coastal_basin(nbpt,nwbas), stat=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_basins','Pb in allocate for nbcoastal','','')
    
    !    Order all sub points in each grid_box and find the sub basins
    !
    !    before we start we set the maps to empty
    !
    basin_id(:,:) = undef_int
    basin_count(:) = 0
    hierarchy(:,:) = undef_sechiba
    max_basins = MAXVAL(basins, MASK=basins .LT. 1.e10)
    invented_basins = max_basins
    nbcoastal(:) = 0
    !
    !! Finds,in each grid box, the distance to the outflow point ... this defines the order in which
    !! the water will go through the sub-basins and grid boxes.
    !
    CALL routing_hierarchy(iml, jml, trip, topoindex, hierarchy)
    !
    !
    DO ib =1, nbpt
       !
       !
       !  extract the information for this grid box
       !
       !! Extracts from the global high resolution fields the data for the current grid box.
       !
       CALL routing_getgrid(nbpt, iml, jml, ib, sub_pts, sub_index, sub_area, max_basins, min_topoind, &
            & lon_rel, lat_rel, lalo, resolution, contfrac, trip, basins, topoindex, hierarchy, &
            & nbi, nbj, area_bx, trip_bx, basin_bx, topoind_bx, hierarchy_bx, lon_bx, lat_bx)
       !
       !! Finds the basins: returns the list of all points which are within the same basin of the grid box.
       !
       CALL routing_findbasins(nbi, nbj, trip_bx, basin_bx, hierarchy_bx, topoind_bx,&
            & nb_basin, basin_inbxid, basin_sz, basin_bxout, basin_pts, coast_pts)


       !
       !  Deal with the case where nb_basin=0 for this grid box. In this case all goes into coastal flow.
       !
       IF ( debug .AND. (COUNT(diagbox .EQ. ib) .GT. 0) ) THEN
          WRITE(numout,*) '===================== IB = :', ib
          WRITE(numout,*) "sub_pts(ib) :", sub_pts(ib), "sub_area(ib,:) :",sub_area(ib,1:2)
          WRITE(numout,*) 'LON LAT of GCM :', lalo(ib,2), lalo(ib,1)
          WRITE(numout,*) 'Neighbor options :',  neighbours(ib,1:8)
          WRITE(numout,*) 'Resolution :', resolution(ib,1:2)
          WRITE(fmt,"('(',I3,'I6)')") nbi
          WRITE(numout,*) '-------------> trip ', trip_bx(1,1)
          DO jp=1,nbj
             WRITE(numout,fmt) trip_bx(1:nbi,jp)
          ENDDO
          WRITE(numout,*) '-------------> basin ',basin_bx(1,1)
          DO jp=1,nbj
             WRITE(numout,fmt) basin_bx(1:nbi,jp)
          ENDDO
          WRITE(numout,*) '-------------> hierarchy ',hierarchy_bx(1,1)
          DO jp=1,nbj
             WRITE(numout,fmt) INT(hierarchy_bx(1:nbi,jp)/1000.)
          ENDDO
          WRITE(numout,*) '-------------> topoindex ',topoind_bx(1,1)
          DO jp=1,nbj
             WRITE(numout,fmt) INT(topoind_bx(1:nbi,jp)/1000.)
          ENDDO
          !
          WRITE(numout,*) '------------> The basins we retain'
          DO jp=1,nb_basin
             WRITE(numout,*) 'index, size, bxout, coast :', basin_inbxid(jp), basin_sz(jp),&
                  & basin_bxout(jp), coast_pts(jp)
          ENDDO
          !
       ENDIF
       !
       !! Puts the basins found for the current grid box in the context of the global map.
       !
       CALL routing_globalize(nbpt, ib, neighbours, area_bx, trip_bx, hierarchy_bx, topoind_bx, min_topoind,&
            & nb_basin, basin_inbxid, basin_sz, basin_pts, basin_bxout, coast_pts, nwbas, basin_count,&
            & basin_area, basin_hierarchy, basin_topoind, basin_id, basin_flowdir, outflow_grid,&
            & nbcoastal, coastal_basin) 
       !
       !
       IF ( debug .AND. (COUNT(diagbox .EQ. ib) .GT. 0) ) THEN
          WRITE(numout,*) 'GLOBAL information after routing_globalize for grid ', ib
          DO jp=1,basin_count(ib)
             WRITE(numout,*) 'Basin ID : ', basin_id(ib, jp)
             WRITE(numout,*) 'Basin flowdir :', basin_flowdir(ib, jp)
             WRITE(numout,*) 'Basin hierarchy :', basin_hierarchy(ib, jp)
             WRITE(numout,*) 'Basin topoindex :', basin_topoind(ib, jp)
             WRITE(numout,*) 'Basin outflow grid :', outflow_grid(ib,jp)
          ENDDO
       ENDIF
       !
    ENDDO
    !
    !! Makes the connections between the bains and ensures global coherence.
    !
    CALL routing_linkup(nbpt, neighbours, nwbas, basin_count, basin_area, basin_id, basin_flowdir, &
         & basin_hierarchy, outflow_grid, outflow_basin, inflow_number, inflow_grid, inflow_basin, &
         & nbcoastal, coastal_basin, invented_basins)
    ! 
    !
    WRITE(numout,*) 'The maximum number of basins in any grid :', MAXVAL(basin_count)
    !
    IF ( debug ) THEN
       DO ib=1,SIZE(diagbox)
          IF ( diagbox(ib) .GT. 0 ) THEN
             WRITE(numout,*) 'After routing_linkup information for grid ', diagbox(ib)
             DO jp=1,basin_count(diagbox(ib))
                WRITE(numout,*) 'Basin ID : ', basin_id(diagbox(ib), jp)
                WRITE(numout,*) 'Basin outflow_grid :', outflow_grid(diagbox(ib), jp)
                WRITE(numout,*) 'Basin outflow_basin:', outflow_basin(diagbox(ib), jp)
                WRITE(numout,*) 'Basin hierarchy :', basin_hierarchy(diagbox(ib), jp)
             ENDDO
          ENDIF
       ENDDO
    ENDIF
    !
    !! Computes the fetch of each basin, upstream area in known.
    !
    CALL routing_fetch(nbpt, resolution, contfrac, nwbas, basin_count, basin_area, basin_id, outflow_grid, &
         & outflow_basin, fetch_basin)
    !
    !
    WRITE(numout,*) "Start reducing the number of basins per grid to meet the required truncation."
    !
    !! Reduces the number of basins per grid to the value chosen by the user.
    !
    CALL routing_truncate(nbpt, resolution, contfrac, nwbas, basin_count, basin_area, basin_topoind,&
         & fetch_basin, basin_id, basin_flowdir, outflow_grid, outflow_basin, inflow_number,&
         & inflow_grid, inflow_basin)
    !
    DEALLOCATE (lat_rel)
    DEALLOCATE (lon_rel)
    !
    DEALLOCATE (trip)
    DEALLOCATE (basins)
    DEALLOCATE (topoindex)
    DEALLOCATE (hierarchy)
    !
    DEALLOCATE (sub_area)
    DEALLOCATE (sub_index)
    DEALLOCATE (sub_pts)
    !
    DEALLOCATE (mask)
    DEALLOCATE (resol_lu)
    !
    DEALLOCATE (basin_count)
    DEALLOCATE (basin_area, basin_hierarchy, basin_topoind, fetch_basin)
    DEALLOCATE (basin_id,  basin_flowdir)
    DEALLOCATE (outflow_grid,outflow_basin)
    DEALLOCATE (inflow_number)
    DEALLOCATE (inflow_basin, inflow_grid)
    DEALLOCATE (nbcoastal, coastal_basin)

  END SUBROUTINE routing_basins


!! ================================================================================================================================
!! SUBROUTINE 	: routing_getgrid
!!
!>\BRIEF         This subroutine extracts from the global high resolution fields
!!               the data for the current grid box we are dealing with.
!!
!! DESCRIPTION (definitions, functional, design, flags) :
!! Convention for trip on the input :
!! The trip field follows the following convention for the flow of the water :
!! trip = 1 : flow = N
!! trip = 2 : flow = NE
!! trip = 3 : flow = E
!! trip = 4 : flow = SE
!! trip = 5 : flow = S
!! trip = 6 : flow = SW
!! trip = 7 : flow = W
!! trip = 8 : flow = NW
!! trip = 97 : return flow into the ground
!! trip = 98 : coastal flow (diffuse flow into the oceans) These values are created here
!! trip = 99 : river flow into the oceans
!!
!! On output, the grid boxes of the basin map which flow out of the GCM grid are identified
!! by numbers larger than 100 :
!! trip = 101 : flow = N out of the coarse grid
!! trip = 102 : flow = NE out of the coarse grid
!! trip = 103 : flow = E out of the coarse grid
!! trip = 104 : flow = SE out of the coarse grid
!! trip = 105 : flow = S out of the coarse grid
!! trip = 106 : flow = SW out of the coarse grid
!! trip = 107 : flow = W out of the coarse grid
!! trip = 108 : flow = NW out of the coarse grid
!! Inside the grid the convention remains the same as above (ie between 1 and 99).:\n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_getgrid(nbpt, iml, jml, ib, sub_pts, sub_index, sub_area, max_basins, min_topoind, &
       & lon_rel, lat_rel, lalo, resolution, contfrac, trip, basins, topoindex, hierarchy, &
       & nbi, nbj, area_bx, trip_bx, basin_bx, topoind_bx, hierarchy_bx, lon_bx, lat_bx)
    !
    IMPLICIT NONE
    !
!!  INPUT VARIABLES
    INTEGER(i_std), INTENT(in)  :: nbpt                        !! Domain size (unitless)
    INTEGER(i_std), INTENT(in)  :: iml                         !! X resolution of the high resolution grid
    INTEGER(i_std), INTENT(in)  :: jml                         !! Y resolution of the high resolution grid
    INTEGER(i_std), INTENT(in)  :: ib                          !! Current basin (unitless)
    INTEGER(i_std), INTENT(in)  :: sub_pts(nbpt)               !! Number of high resolution points on this grid (unitless)
    INTEGER(i_std), INTENT(in)  :: sub_index(nbpt,nbvmax,2)    !! Indices of the points we need on the fine grid (unitless)
    REAL(r_std), INTENT(inout)  :: max_basins                  !! The current maximum of basins
    REAL(r_std), INTENT(in)     :: min_topoind                 !! The current minimum of topographic index (m)
    REAL(r_std), INTENT(in)     :: sub_area(nbpt,nbvmax)       !! Area on the fine grid (m^2)
    REAL(r_std), INTENT(in)     :: lon_rel(iml,jml)            !!
    REAL(r_std), INTENT(in)     :: lat_rel(iml,jml)            !! coordinates of the fine grid
    REAL(r_std), INTENT(in)     :: lalo(nbpt,2)                !! Vector of latitude and longitudes (beware of the order !)
    REAL(r_std), INTENT(in)     :: resolution(nbpt,2)          !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)     :: contfrac(nbpt)              !! Fraction of land in each grid box (unitless;0-1)
    REAL(r_std), INTENT(inout)  :: trip(iml,jml)               !! The trip field (unitless)
    REAL(r_std), INTENT(inout)  :: basins(iml,jml)             !! data on the fine grid
    REAL(r_std), INTENT(inout)  :: topoindex(iml,jml)          !! Topographic index of the residence time (m)
    REAL(r_std), INTENT(inout)  :: hierarchy(iml, jml)         !! data on the fine grid
    !
!!  OUTPUT VARIABLES
    INTEGER(i_std), INTENT(out) :: nbi, nbj                    !! Number of point in x and y within the grid (unitless)
    REAL(r_std), INTENT(out)    :: area_bx(nbvmax,nbvmax)      !! Area of each small box in the grid box (m^2)
    REAL(r_std), INTENT(out)    :: hierarchy_bx(nbvmax,nbvmax) !! Level in the basin of the point
    REAL(r_std), INTENT(out)    :: lon_bx(nbvmax,nbvmax)       !!
    REAL(r_std), INTENT(out)    :: lat_bx(nbvmax,nbvmax)       !!
    REAL(r_std), INTENT(out)    :: topoind_bx(nbvmax,nbvmax)   !! Topographic index of the residence time for each of the smaller boxes (m)
    INTEGER(i_std), INTENT(out) :: trip_bx(nbvmax,nbvmax)      !! The trip field for each of the smaller boxes (unitless)
    INTEGER(i_std), INTENT(out) :: basin_bx(nbvmax,nbvmax)     !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)              :: ip, jp, ll(1), iloc, jloc   !! Indices (unitless)
    REAL(r_std)                 :: lonstr(nbvmax*nbvmax)       !!
    REAL(r_std)                 :: latstr(nbvmax*nbvmax)       !!

!_ ================================================================================================================================

    !
    ! Set everything to undef to locate easily empty points
    !
    trip_bx(:,:) = undef_int
    basin_bx(:,:) = undef_int
    topoind_bx(:,:) = undef_sechiba
    area_bx(:,:) = undef_sechiba
    hierarchy_bx(:,:) = undef_sechiba
    !
    IF ( sub_pts(ib) > 0 ) THEN
       !
       DO ip=1,sub_pts(ib)
          lonstr(ip) = lon_rel(sub_index(ib, ip, 1), sub_index(ib, ip, 2))
          latstr(ip) = lat_rel(sub_index(ib, ip, 1), sub_index(ib, ip, 2))
       ENDDO
       !
       !  Get the size of the area and order the coordinates to go from North to South and West to East
       !
       CALL routing_sortcoord(sub_pts(ib), lonstr, 'WE', nbi)
       CALL routing_sortcoord(sub_pts(ib), latstr, 'NS', nbj)
       !
       ! Transfer the data in such a way that (1,1) is the North Western corner and
       ! (nbi, nbj) the South Eastern.
       !
       DO ip=1,sub_pts(ib)
          ll = MINLOC(ABS(lonstr(1:nbi) - lon_rel(sub_index(ib, ip, 1), sub_index(ib, ip, 2))))
          iloc = ll(1)
          ll = MINLOC(ABS(latstr(1:nbj) - lat_rel(sub_index(ib, ip, 1), sub_index(ib, ip, 2))))
          jloc = ll(1)
          trip_bx(iloc, jloc) = NINT(trip(sub_index(ib, ip, 1), sub_index(ib, ip, 2)))
          basin_bx(iloc, jloc) = NINT(basins(sub_index(ib, ip, 1), sub_index(ib, ip, 2)))
          area_bx(iloc, jloc) = sub_area(ib, ip)
          topoind_bx(iloc, jloc) = topoindex(sub_index(ib, ip, 1), sub_index(ib, ip, 2))
          hierarchy_bx(iloc, jloc) = hierarchy(sub_index(ib, ip, 1), sub_index(ib, ip, 2))
          lon_bx(iloc, jloc) = lon_rel(sub_index(ib, ip, 1), sub_index(ib, ip, 2))
          lat_bx(iloc, jloc) = lat_rel(sub_index(ib, ip, 1), sub_index(ib, ip, 2))
       ENDDO
    ELSE
       !
       ! This is the case where the model invented a continental point
       !
       nbi = 1
       nbj = 1
       iloc = 1
       jloc = 1
       trip_bx(iloc, jloc) = 98
       basin_bx(iloc, jloc) = NINT(max_basins + 1)
       max_basins = max_basins + 1
       area_bx(iloc, jloc) = resolution(ib,1)*resolution(ib,2)*contfrac(ib)
       topoind_bx(iloc, jloc) = min_topoind
       hierarchy_bx(iloc, jloc) =  min_topoind
       lon_bx(iloc, jloc) = lalo(ib,2)
       lat_bx(iloc, jloc) = lalo(ib,1)
       !
    ENDIF
    !
    ! Tag in trip all the outflow conditions. The table is thus :
    ! trip = 100+n : Outflow into another grid box
    ! trip = 99    : River outflow into the ocean
    ! trip = 98    : This will be coastal flow (not organized as a basin)
    ! trip = 97    : return flow into the soil (local)
    !
    DO jp=1,nbj
       IF ( trip_bx(1,jp) .EQ. 8 .OR. trip_bx(1,jp) .EQ. 7 .OR. trip_bx(1,jp) .EQ. 6) THEN
          trip_bx(1,jp) = trip_bx(1,jp) + 100
       ENDIF
       IF ( trip_bx(nbi,jp) .EQ. 2 .OR. trip_bx(nbi,jp) .EQ. 3 .OR. trip_bx(nbi,jp) .EQ. 4) THEN
          trip_bx(nbi,jp) = trip_bx(nbi,jp) + 100
       ENDIF
    ENDDO
    DO ip=1,nbi
       IF ( trip_bx(ip,1) .EQ. 8 .OR. trip_bx(ip,1) .EQ. 1 .OR. trip_bx(ip,1) .EQ. 2) THEN
          trip_bx(ip,1) = trip_bx(ip,1) + 100
       ENDIF
       IF ( trip_bx(ip,nbj) .EQ. 6 .OR. trip_bx(ip,nbj) .EQ. 5 .OR. trip_bx(ip,nbj) .EQ. 4) THEN
          trip_bx(ip,nbj) = trip_bx(ip,nbj) + 100
       ENDIF
    ENDDO
    !
    !
    !  We simplify the outflow. We only need the direction normal to the
    !     box boundary and the 4 corners.
    ! 
    ! Northern border
    IF ( trip_bx(1,1) .EQ. 102 ) trip_bx(1,1) = 101
    IF ( trip_bx(nbi,1) .EQ. 108 ) trip_bx(nbi,1) = 101
    DO ip=2,nbi-1
       IF ( trip_bx(ip,1) .EQ. 108 .OR. trip_bx(ip,1) .EQ. 102 ) trip_bx(ip,1) = 101
    ENDDO
    ! Southern border
    IF ( trip_bx(1,nbj) .EQ. 104 ) trip_bx(1,nbj) = 105
    IF ( trip_bx(nbi,nbj) .EQ. 106 ) trip_bx(nbi,nbj) = 105
    DO ip=2,nbi-1
       IF ( trip_bx(ip,nbj) .EQ. 104 .OR. trip_bx(ip,nbj) .EQ. 106 ) trip_bx(ip,nbj) = 105
    ENDDO
    ! Eastern border
    IF ( trip_bx(nbi,1) .EQ. 104) trip_bx(nbi,1) = 103
    IF ( trip_bx(nbi,nbj) .EQ. 102) trip_bx(nbi,nbj) = 103
    DO jp=2,nbj-1
       IF ( trip_bx(nbi,jp) .EQ. 104 .OR. trip_bx(nbi,jp) .EQ. 102 ) trip_bx(nbi,jp) = 103
    ENDDO
    ! Western border
    IF ( trip_bx(1,1) .EQ. 106) trip_bx(1,1) = 107
    IF ( trip_bx(1,nbj) .EQ. 108) trip_bx(1,nbj) = 107
    DO jp=2,nbj-1
       IF ( trip_bx(1,jp) .EQ. 106 .OR. trip_bx(1,jp) .EQ. 108 ) trip_bx(1,jp) = 107
    ENDDO       
    !
    !
  END SUBROUTINE routing_getgrid
!
!! ================================================================================================================================
!! SUBROUTINE 	: routing_sortcoord
!!
!>\BRIEF         This subroutines orders the coordinates to go from North to South and West to East.
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_sortcoord(nb_in, coords, direction, nb_out)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)   :: nb_in             !!
    REAL(r_std), INTENT(inout)   :: coords(nb_in)     !!
    !
!! OUTPUT VARIABLES
    INTEGER(i_std), INTENT(out)  :: nb_out            !!
    !
!! LOCAL VARIABLES
    CHARACTER(LEN=2)             :: direction         !!
    INTEGER(i_std)               :: ipos              !!
    REAL(r_std)                  :: coords_tmp(nb_in) !!
    INTEGER(i_std), DIMENSION(1) :: ll                !!
    INTEGER(i_std)               :: ind(nb_in)        !!

!_ ================================================================================================================================
    !
    ipos = 1
    nb_out = nb_in
    !
    ! Compress the coordinates array
    !
    DO WHILE ( ipos < nb_in )
       IF ( coords(ipos+1) /= undef_sechiba) THEN
         IF ( COUNT(coords(ipos:nb_out) == coords(ipos)) > 1 ) THEN
            coords(ipos:nb_out-1) = coords(ipos+1:nb_out) 
            coords(nb_out:nb_in) = undef_sechiba
            nb_out = nb_out - 1
         ELSE
            ipos = ipos + 1
         ENDIF
      ELSE
         EXIT
      ENDIF
    ENDDO
    !
    ! Sort it now
    !
    ! First we get ready and adjust for the periodicity in longitude
    !
    coords_tmp(:) = undef_sechiba
    IF ( INDEX(direction, 'WE') == 1 .OR.  INDEX(direction, 'EW') == 1) THEN
       IF ( MAXVAL(ABS(coords(1:nb_out))) .GT. 160 ) THEN
          coords_tmp(1:nb_out) = MOD(coords(1:nb_out) + 360.0, 360.0)
       ELSE
          coords_tmp(1:nb_out) = coords(1:nb_out)
       ENDIF
    ELSE IF ( INDEX(direction, 'NS') == 1 .OR.  INDEX(direction, 'SN') == 1) THEN
       coords_tmp(1:nb_out) = coords(1:nb_out)
    ELSE
       WRITE(numout,*) 'The chosen direction (', direction,') is not recognized'
       CALL ipslerr_p(3,'routing_sortcoord','The chosen direction is not recognized','First section','')
    ENDIF
    !
    ! Get it sorted out now
    !
    ipos = 1
    !
    IF ( INDEX(direction, 'WE') == 1 .OR. INDEX(direction, 'SN') == 1) THEN
       DO WHILE (COUNT(ABS(coords_tmp(:)-undef_sechiba) > EPSILON(undef_sechiba)*10.) >= 1)
          ll = MINLOC(coords_tmp(:), coords_tmp /= undef_sechiba)
          ind(ipos) = ll(1) 
          coords_tmp(ll(1)) = undef_sechiba
          ipos = ipos + 1
       ENDDO
    ELSE IF ( INDEX(direction, 'EW') == 1 .OR. INDEX(direction, 'NS') == 1) THEN
       DO WHILE (COUNT(ABS(coords_tmp(:)-undef_sechiba) > EPSILON(undef_sechiba)*10.) >= 1)
          ll = MAXLOC(coords_tmp(:), coords_tmp /= undef_sechiba)
          ind(ipos) = ll(1) 
          coords_tmp(ll(1)) = undef_sechiba
          ipos = ipos + 1
       ENDDO
    ELSE
       WRITE(numout,*) 'The chosen direction (', direction,') is not recognized (second)'
       CALL ipslerr_p(3,'routing_sortcoord','The chosen direction is not recognized','Second section','')
    ENDIF
    !
    coords(1:nb_out) = coords(ind(1:nb_out))
    IF (nb_out < nb_in) THEN
       coords(nb_out+1:nb_in) = zero
    ENDIF
    !
  END SUBROUTINE routing_sortcoord
  !

!! ================================================================================================================================
!! SUBROUTINE 	: routing_findbasins
!!
!>\BRIEF         This subroutine finds the basins and does some clean up.
!!               The aim is to return the list off all points which are within the
!!               same basin of the grid box.
!!
!! DESCRIPTION (definitions, functional, design, flags) :
!!  We will also collect all points which directly flow into the ocean in one basin
!!  Make sure that we do not have a basin with two outflows and other exceptions.
!!  At this stage no effort is made to come down to the truncation of the model.
!!
!! Convention for trip    \n
!! -------------------    \n
!! Inside of the box :    \n
!! trip = 1 : flow = N    \n
!! trip = 2 : flow = NE    \n
!! trip = 3 : flow = E    \n
!! trip = 4 : flow = SE    \n
!! trip = 5 : flow = S    \n
!! trip = 6 : flow = SW    \n
!! trip = 7 : flow = W    \n
!! trip = 8 : flow = NW    \n
!! trip = 97 : return flow into the ground    \n
!! trip = 98 : coastal flow (diffuse flow into the oceans) These values are created here    \n
!! trip = 99 : river flow into the oceans    \n
!!
!! Out flow from the grid :    \n
!! trip = 101 : flow = N out of the coarse grid    \n
!! trip = 102 : flow = NE out of the coarse grid    \n
!! trip = 103 : flow = E out of the coarse grid    \n
!! trip = 104 : flow = SE out of the coarse grid    \n
!! trip = 105 : flow = S out of the coarse grid    \n
!! trip = 106 : flow = SW out of the coarse grid    \n
!! trip = 107 : flow = W out of the coarse grid    \n
!! trip = 108 : flow = NW out of the coarse grid!    \n
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE routing_findbasins(nbi, nbj, trip, basin, hierarchy, topoind, nb_basin, basin_inbxid, basin_sz,&
       & basin_bxout, basin_pts, coast_pts)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)    :: nbi                          !! Number of point in x within the grid (unitless)
    INTEGER(i_std), INTENT(in)    :: nbj                          !! Number of point in y within the grid (unitless)
    REAL(r_std), INTENT(in)       :: hierarchy(:,:)               !!
    REAL(r_std), INTENT(in)       :: topoind(:,:)                 !! Topographic index of the residence time (m)
    !
    !  Modified
    INTEGER(i_std), INTENT(inout) :: trip(:,:)                    !! The trip field (unitless)
    INTEGER(i_std), INTENT(inout) :: basin(:,:)                   !!
    !
!! OUTPUT VARIABLES
    INTEGER(i_std), INTENT(out)   :: nb_basin                     !! Number of sub-basins (unitless)
    INTEGER(i_std), INTENT(out)   :: basin_inbxid(nbvmax)         !!
    INTEGER(i_std), INTENT(out)   :: basin_sz(nbvmax)             !!
    INTEGER(i_std), INTENT(out)   :: basin_bxout(nbvmax)          !!
    INTEGER(i_std), INTENT(out)   :: basin_pts(nbvmax, nbvmax, 2) !!
    INTEGER(i_std), INTENT(out)   :: coast_pts(nbvmax)            !! The coastal flow points (unitless)
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                :: ibas, ilf, nbb, nb_in        !!
    INTEGER(i_std)                :: bname(nbvmax)                !!
    INTEGER(i_std)                :: sz(nbvmax)                   !!
    INTEGER(i_std)                :: pts(nbvmax,nbvmax,2)         !!
    INTEGER(i_std)                :: nbout(nbvmax)                !!
    INTEGER(i_std)                :: new_nb                       !!
    INTEGER(i_std)                :: new_bname(nbvmax)            !!
    INTEGER(i_std)                :: new_sz(nbvmax)               !!
    INTEGER(i_std)                :: new_pts(nbvmax,nbvmax,2)     !!
    INTEGER(i_std)                :: itrans                       !!
    INTEGER(i_std)                :: trans(nbvmax)                !!
    INTEGER(i_std)                :: outdir(nbvmax)               !!
    INTEGER(i_std)                :: tmpsz(nbvmax)                !!
    INTEGER(i_std)                :: ip, jp, jpp(1), ipb          !!
    INTEGER(i_std)                :: sortind(nbvmax)              !!
    CHARACTER(LEN=7)              :: fmt                          !!

!_ ================================================================================================================================
    !
    nbb = 0
    ibas = -1
    bname(:) = undef_int
    sz(:) = 0
    nbout(:) = 0
    new_pts(:,:,:) = 0
    !
    ! 1.0 Find all basins within this grid box
    !     Sort the variables per basin so that we can more easily
    !     access data from the same basin (The variables are :
    !     bname, sz, pts, nbout)
    !
    DO ip=1,nbi
       DO jp=1,nbj
          IF ( basin(ip,jp) .LT. undef_int) THEN
             IF ( COUNT(basin(ip,jp) .EQ. bname(:)) .EQ. 0 ) THEN
                nbb = nbb + 1
                IF ( nbb .GT. nbvmax ) CALL ipslerr_p(3,'routing_findbasins','nbvmax too small','first section','')
                bname(nbb) = basin(ip,jp)
                sz(nbb) = 0
             ENDIF
             !
             DO ilf=1,nbb
                IF ( basin(ip,jp) .EQ. bname(ilf) ) THEN
                   ibas = ilf
                ENDIF
             ENDDO
             !
             sz(ibas) = sz(ibas) + 1
             IF ( sz(ibas) .GT. nbvmax ) CALL ipslerr_p(3,'routing_findbasins','nbvmax too small','second section','')
             pts(ibas, sz(ibas), 1) = ip
             pts(ibas, sz(ibas), 2) = jp
             ! We deal only with outflow and leave flow back into the grid box for later.
             IF ( trip(ip,jp) .GE. 97 ) THEN
                nbout(ibas) = nbout(ibas) + 1
             ENDIF
             !
          ENDIF
          !
       ENDDO
    ENDDO
    !
    ! 2.0 All basins which have size 1 and flow to the ocean are put together.
    !
    itrans = 0
    coast_pts(:) = undef_int
    ! Get all the points we can collect
    DO ip=1,nbb
       IF ( sz(ip) .EQ. 1 .AND. trip(pts(ip,1,1),pts(ip,1,2)) .EQ. 99) THEN
          itrans = itrans + 1
          trans(itrans) = ip
          trip(pts(ip,1,1),pts(ip,1,2)) = 98
       ENDIF
    ENDDO
    ! put everything in the first basin
    IF ( itrans .GT. 1) THEN
       ipb = trans(1)
       coast_pts(sz(ipb)) = bname(ipb)
       bname(ipb) = -1
       DO ip=2,itrans
          sz(ipb) = sz(ipb) + 1
          coast_pts(sz(ipb)) = bname(trans(ip))
          sz(trans(ip)) = 0
          pts(ipb, sz(ipb), 1) = pts(trans(ip), 1, 1) 
          pts(ipb, sz(ipb), 2) = pts(trans(ip), 1, 2) 
       ENDDO
    ENDIF
    !
    ! 3.0 Make sure that we have only one outflow point in each basin
    !
    ! nbb is the number of basins on this grid box.
    new_nb = 0
    DO ip=1,nbb
       ! We only do this for grid-points which have more than one outflow
       IF ( sz(ip) .GT. 1 .AND. nbout(ip) .GT. 1) THEN
          !
          ! Pick up all points needed and store them in trans
          !
          itrans = 0
          DO jp=1,sz(ip)
             IF ( trip(pts(ip,jp,1),pts(ip,jp,2)) .GE. 97) THEN
                itrans = itrans + 1
                trans(itrans) = trip(pts(ip,jp,1),pts(ip,jp,2))
             ENDIF
          ENDDO
          !
          ! First issue : We have more than one point of the basin which flows into
          ! the ocean. In this case we put everything into coastal flow. It will go into
          ! a separate basin in the routing_globalize routine.
          !
          IF ( (COUNT(trans(1:itrans) .EQ. 99) + COUNT(trans(1:itrans) .EQ. 98)) .GT. 1) THEN
             DO jp=1,sz(ip)
                IF ( trip(pts(ip,jp,1),pts(ip,jp,2)) .EQ. 99 ) THEN
                   trip(pts(ip,jp,1),pts(ip,jp,2)) = 98
                   trans(itrans) = trip(pts(ip,jp,1),pts(ip,jp,2))
                ENDIF
             ENDDO
          ENDIF
          !
          ! Second issue : We have redundant outflows at the boundaries. That is two small grid
          ! boxes flowing into the same GCM grid box.
          !
          IF ( COUNT(trans(1:itrans) .GT. 100) .GE. 1) THEN
             CALL routing_simplify(nbi, nbj, trip, basin, hierarchy, bname(ip))
             itrans = 0
             DO jp=1,sz(ip)
                IF ( trip(pts(ip,jp,1),pts(ip,jp,2)) .GE. 9) THEN
                   itrans = itrans + 1
                   trans(itrans) = trip(pts(ip,jp,1),pts(ip,jp,2))
                ENDIF
             ENDDO
          ENDIF
          !
          ! Third issue : we have more than one outflow from the boxes. This could be 
          !             - flow into 2 or more neighboring GCM grids
          !             - flow into a neighboring GCM grids and into the ocean or be a return flow (=97. =98, =99)
          !             - flow into a neighboring GCM grids or ocean and back into the same GCM grid box
          ! The only solution is to cut the basin up in as many parts.
          !
          IF ( COUNT(trans(1:itrans) .GE. 97) .GT. 1) THEN
             !
             nb_in =  new_nb
             CALL routing_cutbasin(nbi, nbj, nbb, trip, basin, bname(ip), new_nb, new_bname, new_sz, new_pts)
             !
             ! If we have split the basin then we need to cancel the old one
             !
             IF ( nb_in .NE. new_nb) THEN
                sz(ip) = 0
             ENDIF
             !
          ENDIF
          !
       ENDIF
    ENDDO
    !
    !  Add the new basins to the end of the list
    !
    If ( nbb+new_nb .LE. nbvmax) THEN
       DO ip=1,new_nb
          bname(nbb+ip) = new_bname(ip)
          sz(nbb+ip) = new_sz(ip)
          pts(nbb+ip,:,:) = new_pts(ip,:,:)
       ENDDO
       nbb = nbb+new_nb
    ELSE
       WRITE(numout,*) 'Increase nbvmax. It is too small to contain all the basins (routing_findbasins)'
       CALL ipslerr_p(3,'routing_findbasins','Increase nbvmax.','It is too small to contain all the basins','')
    ENDIF
    !
    ! Keep the output direction
    !
    DO ip=1,nbb
       IF ( sz(ip) .GT. 0 ) THEN
          trans(:) = 0
          DO jp=1,sz(ip)
             trans(jp) = trip(pts(ip,jp,1),pts(ip,jp,2))
          ENDDO
          outdir(ip) = MAXVAL(trans(1:sz(ip)))
          IF ( outdir(ip) .GE. 97 ) THEN
             outdir(ip) = outdir(ip) - 100
          ELSE
             WRITE(numout,*) 'Why are we here and can not find a trip larger than 96'
             WRITE(numout,*) 'Does this mean that the basin does not have any outflow ', ip, bname(ip)
             WRITE(fmt,"('(',I3,'I9)')") nbi
             WRITE(numout,*) '-----------------------> trip'
             DO jp=1,nbj
                WRITE(numout,fmt) trip(1:nbi,jp)
             ENDDO
             WRITE(numout,*) '-----------------------> basin'
             DO jp=1,nbj
                WRITE(numout,fmt) basin(1:nbi,jp)
             ENDDO
             CALL ipslerr_p(3,'routing_findbasins','Probleme finding trip','','')
          ENDIF
       ENDIF
    ENDDO
    !
    !
    ! Sort the output by size of the various basins. 
    !
    nb_basin = COUNT(sz(1:nbb) .GT. 0)
    tmpsz(:) = -1
    tmpsz(1:nbb) = sz(1:nbb)
    DO ip=1,nbb
       jpp = MAXLOC(tmpsz(:))
       IF ( sz(jpp(1)) .GT. 0) THEN
          sortind(ip) = jpp(1)
          tmpsz(jpp(1)) = -1
       ENDIF
    ENDDO
    basin_inbxid(1:nb_basin) = bname(sortind(1:nb_basin))
    basin_sz(1:nb_basin) = sz(sortind(1:nb_basin))
    basin_pts(1:nb_basin,:,:) = pts(sortind(1:nb_basin),:,:)
    basin_bxout(1:nb_basin) = outdir(sortind(1:nb_basin))
    !
    ! We can only check if we have at least as many outflows as basins
    !
    ip = COUNT(trip(1:nbi,1:nbj) .GE. 97 .AND. trip(1:nbi,1:nbj) .LT. undef_int)
!!    ip = ip + COUNT(trip(1:nbi,1:nbj) .EQ. 97)
!!    IF ( COUNT(trip(1:nbi,1:nbj) .EQ. 98) .GT. 0) ip = ip + 1
    IF ( ip .LT. nb_basin ) THEN
       WRITE(numout,*) 'We have less outflow points than basins :', ip
       WRITE(fmt,"('(',I3,'I9)')") nbi
       WRITE(numout,*) '-----------------------> trip'
       DO jp=1,nbj
          WRITE(numout,fmt) trip(1:nbi,jp)
       ENDDO
       WRITE(numout,*) '-----------------------> basin'
       DO jp=1,nbj
          WRITE(numout,fmt) basin(1:nbi,jp)
       ENDDO
       WRITE(numout,*) 'nb_basin :', nb_basin
       WRITE(numout,*) 'Basin sized :', basin_sz(1:nb_basin)
       CALL ipslerr_p(3,'routing_findbasins','Probleme less outflow points than basins','','')
    ENDIF
    
  END SUBROUTINE routing_findbasins
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_simplify
!!
!>\BRIEF         This subroutine symplifies the routing out of each basin by taking 
!!               out redundancies at the borders of the GCM box.
!!               The aim is to have only one outflow point per basin and grid box.
!!               But here we will not change the direction of the outflow.  
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_simplify(nbi, nbj, trip, basin, hierarchy, basin_inbxid)
    !
    IMPLICIT NONE
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                             :: nbi                        !! Number of point in x within the grid (unitless)
    INTEGER(i_std)                             :: nbj                        !! Number of point in y within the grid (unitless)
    INTEGER(i_std)                             :: trip(:,:)                  !! The trip field (unitless)
    INTEGER(i_std)                             :: basin(:,:)                 !!
    REAL(r_std)                                :: hierarchy(:,:)             !!
    INTEGER(i_std)                             :: basin_inbxid               !!
    !
    INTEGER(i_std)                             :: ip, jp, nbout, basin_sz, iborder !!
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax)   :: trip_tmp                   !! Temporary trip field which only contains the values for the basin on which we currently work (1)
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax,2) :: trip_flow                  !!
    INTEGER(i_std), DIMENSION(nbvmax,2)        :: outflow                    !!
    INTEGER(i_std), DIMENSION(nbvmax)          :: outsz                      !!
    CHARACTER(LEN=7)                           :: fmt                        !!
    !
    INTEGER(i_std), DIMENSION(8,2)             :: inc                        !!
    INTEGER(i_std)                             :: itodo, ill(1), icc, ismall, ibas, iip, jjp, ib, id !! Indices (unitless)
    INTEGER(i_std), DIMENSION(nbvmax)          :: todopt                     !!
!!$, todosz
    REAL(r_std), DIMENSION(nbvmax)             :: todohi                     !!
    LOGICAL                                    :: not_found, debug = .FALSE. !! (true/false)

!_ ================================================================================================================================
    !
    !
    !  The routing code (i=1, j=2)
    !
    inc(1,1) = 0
    inc(1,2) = -1
    inc(2,1) = 1
    inc(2,2) = -1
    inc(3,1) = 1
    inc(3,2) = 0
    inc(4,1) = 1
    inc(4,2) = 1
    inc(5,1) = 0
    inc(5,2) = 1
    inc(6,1) = -1
    inc(6,2) = 1
    inc(7,1) = -1
    inc(7,2) = 0
    inc(8,1) = -1
    inc(8,2) = -1
    !
    !
    !  Symplify the outflow conditions first. We are only interested in the
    !  outflows which go to different GCM grid boxes.
    !
    IF ( debug ) THEN
       WRITE(numout,*) '+++++++++++++++++++ BEFORE ANYTHING ++++++++++++++++++++'
       WRITE(fmt,"('(',I3,'I6)')") nbi
       DO jp=1,nbj
          WRITE(numout,fmt) trip_tmp(1:nbi,jp)
       ENDDO
    ENDIF
    !
    !  transfer the trips into an array which only contains the basin we are interested in
    !
    trip_tmp(:,:) = -1
    basin_sz = 0
    DO ip=1,nbi
       DO jp=1,nbj
          IF ( basin(ip,jp) .EQ. basin_inbxid) THEN
             trip_tmp(ip,jp) = trip(ip,jp)
             basin_sz = basin_sz + 1
          ENDIF
       ENDDO
    ENDDO
    !
    ! Determine for each point where it flows to
    !
    CALL routing_findrout(nbi, nbj, trip_tmp, basin_sz, basin_inbxid, nbout, outflow, trip_flow, outsz)
    !
    !
    !
    !
    ! Over the width of a GCM grid box we can have many outflows but we are interested
    ! in only one for each basin. Thus we wish to collect them all to form only one outflow
    ! to the neighboring grid box.
    !
    DO iborder = 101,107,2
       !
       ! If we have more than one of these outflows then we need to merge the sub-basins
       !
       icc = COUNT(trip_tmp .EQ. iborder)-1
       DO WHILE ( icc .GT. 0)
          ! Pick out all the points we will have to do
          itodo = 0
          DO ip=1,nbout
             IF (trip_tmp(outflow(ip,1),outflow(ip,2)) .EQ. iborder) THEN
                itodo = itodo + 1
                todopt(itodo) = ip
!!$                todosz(itodo) = outsz(ip)
                ! We take the hierarchy of the outflow point as we will try to
                ! minimize if for the outflow of the entire basin.
                todohi(itodo) = hierarchy(outflow(ip,1),outflow(ip,2))
             ENDIF
          ENDDO
          !
          ! We change the direction of the smallest basin.
          !
          ill=MAXLOC(todohi(1:itodo))
          ismall = todopt(ill(1))
          !
          DO ip=1,nbi
             DO jp=1,nbj
                IF ( trip_flow(ip,jp,1) .EQ. outflow(ismall,1) .AND.&
                     & trip_flow(ip,jp,2) .EQ. outflow(ismall,2) ) THEN
                   ! Now that we have found a point of the smallest sub-basin we 
                   ! look around for another sub-basin
                   ib = 1
                   not_found = .TRUE.
                   DO WHILE ( not_found .AND. ib .LE. itodo ) 
                      IF ( ib .NE. ill(1) ) THEN
                         ibas = todopt(ib)
                         DO id=1,8
                            iip = ip + inc(id,1)
                            jjp = jp + inc(id,2)
                            ! Can we look at this points or is there any need to ?
                            IF ( iip .GE. 1 .AND. iip .LE. nbi .AND. &
                                 & jjp .GE. 1 .AND. jjp .LE. nbj .AND. not_found) THEN
                               ! Is this point the one we look for ?
                               IF ( trip_flow(iip,jjp,1) .EQ. outflow(ibas,1) .AND. &
                                    & trip_flow(iip,jjp,2) .EQ. outflow(ibas,2)) THEN
                                  trip_flow(ip,jp,1) = outflow(ibas,1)
                                  trip_flow(ip,jp,2) = outflow(ibas,2)
                                  trip_tmp(ip,jp) = id
                                  ! This last line ensures that we do not come back to this point
                                  ! and that in the end the outer while will stop
                                  not_found = .FALSE.
                               ENDIF
                            ENDIF
                         ENDDO
                      ENDIF
                      ib = ib + 1
                   ENDDO
                ENDIF
             ENDDO
          ENDDO
          !
          icc = icc - 1
       ENDDO
       !
       !
    ENDDO
    !
    IF ( debug ) THEN
       WRITE(numout,*) '+++++++++++++++++++ AFTER +++++++++++++++++++++++++++++'
       WRITE(fmt,"('(',I3,'I6)')") nbi
       DO jp=1,nbj
          WRITE(numout,fmt) trip_tmp(1:nbi,jp)
       ENDDO
    ENDIF
    !
    !  Put trip_tmp back into trip
    !
    DO ip=1,nbi
       DO jp=1,nbj
          IF ( trip_tmp(ip,jp) .GT. 0) THEN
             trip(ip,jp) = trip_tmp(ip,jp)
          ENDIF
       ENDDO
    ENDDO
    !
  END SUBROUTINE routing_simplify
!
!! ================================================================================================================================
!! SUBROUTINE 	: routing_cutbasin
!!
!>\BRIEF        This subroutine cuts the original basin which has more than one outflow
!!              into as many subbasins as outflow directions.  
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_cutbasin (nbi, nbj, nbbasins, trip, basin, basin_inbxid, nb, bname, sz, pts)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                 :: nbi, nbj             !! Number of point in x and y within the grid (unitless)
    INTEGER(i_std), INTENT(in)                 :: nbbasins             !!
    INTEGER(i_std), INTENT(in)                 :: basin_inbxid         !!
    !
    !  Modified
    INTEGER(i_std), INTENT(inout)              :: trip(:,:)            !! The trip field (unitless)
    INTEGER(i_std), INTENT(inout)              :: basin(:,:)           !!
    !
!! OUTPUT VARIABLES
    INTEGER(i_std), INTENT(out)                :: nb                   !!
    INTEGER(i_std), INTENT(out)                :: bname(nbvmax)        !!
    INTEGER(i_std), INTENT(out)                :: sz(nbvmax)           !!
    INTEGER(i_std), INTENT(out)                :: pts(nbvmax,nbvmax,2) !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                             :: ip, jp, iip, jjp, ib, ibb, id, nbout !! Indices (unitless)
    INTEGER(i_std)                             :: basin_sz             !!
    INTEGER(i_std)                             :: nb_in                !!
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax)   :: trip_tmp             !! Temporary trip field which only contains the values for the basin on which we currently work (unitless)
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax,2) :: trip_flow            !!
    INTEGER(i_std), DIMENSION(nbvmax,2)        :: outflow              !!
    INTEGER(i_std), DIMENSION(nbvmax)          :: outsz                !!
    CHARACTER(LEN=7)                           :: fmt                  !!
    LOGICAL                                    :: not_found            !! (true/false)
    LOGICAL                                    :: debug=.FALSE.        !! (true/false)
    !
    INTEGER(i_std), DIMENSION(8,2)             :: inc                  !!

!_ ================================================================================================================================
    !
    !
    !  The routing code (i=1, j=2)
    !
    inc(1,1) = 0
    inc(1,2) = -1
    inc(2,1) = 1
    inc(2,2) = -1
    inc(3,1) = 1
    inc(3,2) = 0
    inc(4,1) = 1
    inc(4,2) = 1
    inc(5,1) = 0
    inc(5,2) = 1
    inc(6,1) = -1
    inc(6,2) = 1
    inc(7,1) = -1
    inc(7,2) = 0
    inc(8,1) = -1
    inc(8,2) = -1
    !
    ! Set up a temporary trip field which only contains the values
    ! for the basin on which we currently work.
    !
    trip_tmp(:,:) = -1
    basin_sz = 0
    DO ip=1,nbi
       DO jp=1,nbj
          IF ( basin(ip,jp) .EQ. basin_inbxid) THEN
             trip_tmp(ip,jp) = trip(ip,jp)
             basin_sz = basin_sz + 1
          ENDIF
       ENDDO
    ENDDO
    !
    CALL routing_findrout(nbi, nbj, trip_tmp, basin_sz, basin_inbxid, nbout, outflow, trip_flow, outsz)
    ! 
!    IF ( debug ) THEN
!       DO ib = nb_in+1,nb
!          DO ip=1,sz(ib)
!             trip_tmp(pts(ib, ip, 1),pts(ib, ip, 2)) = ib*(-1)-900
!          ENDDO
!       ENDDO
!       WRITE(fmt,"('(',I3,'I6)')") nbi
!       WRITE(numout,*)  'BEFORE ------------> New basins '
!       WRITE(numout,*) nb, ' sz :', sz(1:nb)
!       DO jp=1,nbj
!          WRITE(numout,fmt) trip_tmp(1:nbi,jp)
!       ENDDO
!    ENDIF
    !
    !  Take out the small sub-basins. That is those which have only one grid box
    !  This is only done if we need to save space in the number of basins. Else we
    !  can take it easy and keep diverging sub-basins for the moment. 
    !
    IF ( nbbasins .GE. nbasmax ) THEN
       DO ib=1,nbout
          ! If the sub-basin is of size one and its larger neighbor is flowing into another
          ! direction then we put them together.
          IF ( outsz(ib) .EQ. 1 .AND. trip(outflow(ib,1), outflow(ib,2)) .GT. 99 ) THEN
             !
             not_found = .TRUE.
             DO id=1,8
                ip = outflow(ib,1)
                jp = outflow(ib,2)
                iip = ip + inc(id,1)
                jjp = jp + inc(id,2)
                ! Can we look at this points ?
                IF ( iip .GE. 1 .AND. iip .LE. nbi .AND. &
                     & jjp .GE. 1 .AND. jjp .LE. nbj .AND. not_found) THEN
                   ! Did we find a direct neighbor which is an outflow point ?
                   IF ( trip_tmp(iip,jjp) .GT. 100 ) THEN
                      ! IF so direct the flow towards it and update the tables.
                      not_found = .FALSE.
                      trip(ip,jp) = id
                      trip_tmp(ip,jp) = id
                      outsz(ib) = 0
                      ! update the table of this basin
                      DO ibb=1,nbout
                         IF ( iip .EQ. outflow(ibb,1) .AND. jjp .EQ. outflow(ibb,2) ) THEN
                            outsz(ibb) = outsz(ibb)+1 
                            trip_flow(ip,jp,1) = outflow(ibb,1)
                            trip_flow(ip,jp,2) = outflow(ibb,2)
                         ENDIF
                      ENDDO
                   ENDIF
                ENDIF
             ENDDO
          ENDIF
       ENDDO
    ENDIF
    !
    !
    !  Cut the basin if we have more than 1 left.
    !
    !
    IF ( COUNT(outsz(1:nbout) .GT. 0) .GT. 1 ) THEN
       !
       nb_in = nb
       !
       DO ib = 1,nbout
          IF ( outsz(ib) .GT. 0) THEN
             nb = nb+1
             IF ( nb .GT. nbvmax) THEN
                WRITE(numout,*) 'nbvmax too small, increase it (routing_cutbasin)'
             ENDIF
             bname(nb) = basin_inbxid
             sz(nb) = 0
             DO ip=1,nbi
                DO jp=1,nbj
                   IF ( (trip_flow(ip,jp,1) + trip_flow(ip,jp,1)) .GT. 0 .AND. &
                      & trip_flow(ip,jp,1) .EQ. outflow(ib,1) .AND. &
                      & trip_flow(ip,jp,2) .EQ. outflow(ib,2) ) THEN
                      sz(nb) = sz(nb) + 1
                      pts(nb, sz(nb), 1) = ip
                      pts(nb, sz(nb), 2) = jp
                   ENDIF
                ENDDO
             ENDDO
          ENDIF
       ENDDO
       ! A short verification 
       IF ( SUM(sz(nb_in+1:nb)) .NE. basin_sz) THEN
          WRITE(numout,*) 'Lost some points while spliting the basin'
          WRITE(numout,*) 'nbout :', nbout
          DO ib = nb_in+1,nb
             WRITE(numout,*) 'ib, SZ :', ib, sz(ib)
          ENDDO
          WRITE(fmt,"('(',I3,'I6)')") nbi
          WRITE(numout,*)  '-------------> trip '
          DO jp=1,nbj
             WRITE(numout,fmt) trip_tmp(1:nbi,jp)
          ENDDO
          CALL ipslerr_p(3,'routing_cutbasin','Lost some points while spliting the basin','','')
       ENDIF
       
       IF ( debug ) THEN
          DO ib = nb_in+1,nb
             DO ip=1,sz(ib)
                trip_tmp(pts(ib, ip, 1),pts(ib, ip, 2)) = ib*(-1)-900
             ENDDO
          ENDDO
          WRITE(fmt,"('(',I3,'I6)')") nbi
          WRITE(numout,*)  'AFTER-------------> New basins '
          WRITE(numout,*) nb, ' sz :', sz(1:nb)
          DO jp=1,nbj
             WRITE(numout,fmt) trip_tmp(1:nbi,jp)
          ENDDO
          IF ( MAXVAl(trip_tmp(1:nbi,1:nbj)) .GT. 0) THEN
             CALL ipslerr_p(3,'routing_cutbasin','Error in debug checking','','')
          ENDIF
       ENDIF
    ENDIF
    !
  END SUBROUTINE routing_cutbasin
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_hierarchy
!!
!>\BRIEF        This subroutine finds, for each point, the distance to the outflow
!!               point along the flowlines of the basin. 
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_hierarchy(iml, jml, trip, topoindex, hierarchy)
    !
    IMPLICIT NONE
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                  :: iml          !! X resolution of the high resolution grid
    INTEGER(i_std)                  :: jml          !! Y resolution of the high resolution grid
    REAL(r_std), DIMENSION(iml,jml) :: trip         !! The trip field (unitless)
    REAL(r_std), DIMENSION(iml,jml) :: hierarchy    !!
    REAL(r_std), DIMENSION(iml,jml) :: topoindex    !! Topographic index of the residence time (m)
    !
    INTEGER(i_std), DIMENSION(8,2)  :: inc          !!
    INTEGER(i_std)                  :: ip, jp, ib, ntripi, ntripj, cnt, trp !!
    REAL(r_std)                     :: topohier     !! The new value of topographically weighted hierarchy (m)
    REAL(r_std)                     :: topohier_old !! The old value of topographically weighted hierarchy (m)
    CHARACTER(LEN=7)                :: fmt          !!

!_ ================================================================================================================================
    !
    !  The routing code (i=1, j=2)
    !
    inc(1,1) = 0
    inc(1,2) = -1
    inc(2,1) = 1
    inc(2,2) = -1
    inc(3,1) = 1
    inc(3,2) = 0
    inc(4,1) = 1
    inc(4,2) = 1
    inc(5,1) = 0
    inc(5,2) = 1
    inc(6,1) = -1
    inc(6,2) = 1
    inc(7,1) = -1
    inc(7,2) = 0
    inc(8,1) = -1
    inc(8,2) = -1
    !
    DO ip=1,iml
       DO jp=1,jml
          IF ( trip(ip,jp) .LT. undef_sechiba ) THEN
             ntripi = ip
             ntripj = jp
             trp = NINT(trip(ip,jp))
             cnt = 1
             ! Warn for extreme numbers
             IF (  topoindex(ip,jp) .GT. 1.e10 ) THEN
                WRITE(numout,*) 'We have a very large topographic index for point ', ip, jp
                WRITE(numout,*) 'This can not be right :', topoindex(ip,jp)
                CALL ipslerr_p(3,'routing_hierarchy','Too large topographic index','','')
             ELSE
                topohier = topoindex(ip,jp)
             ENDIF
             !
             DO WHILE ( trp .GT. 0 .AND. trp .LT. 9 .AND. cnt .LT. iml*jml) 
                cnt = cnt + 1
                ntripi = ntripi + inc(trp,1)
                IF ( ntripi .LT. 1) ntripi = iml
                IF ( ntripi .GT. iml) ntripi = 1
                ntripj = ntripj + inc(trp,2)
                topohier_old = topohier
                topohier = topohier + topoindex(ntripi, ntripj)
                IF ( topohier_old .GT. topohier) THEN
                   WRITE(numout,*) 'Big Problem, how comes we climb up a hill ?'
                   WRITE(numout,*) 'The old value of topographicaly weighted hierarchy was : ', topohier_old
                   WRITE(numout,*) 'The new one is :', topohier
                   CALL ipslerr_p(3,'routing_hierarchy','Big Problem, how comes we climb up a hill ?','','')
                ENDIF
                trp = NINT(trip(ntripi, ntripj))
             ENDDO
             
             IF ( cnt .EQ. iml*jml) THEN
                WRITE(numout,*) 'We could not route point (routing_findrout) :', ip, jp
                WRITE(numout,*) '-------------> trip '
                WRITE(fmt,"('(',I3,'I6)')") iml
                DO ib=1,jml
                   WRITE(numout,fmt) trip(1:iml,ib)
                ENDDO
                CALL ipslerr_p(3,'routing_hierarchy','We could not route point','','')
             ENDIF
             
             hierarchy(ip,jp) = topohier
             
          ENDIF
       ENDDO
    ENDDO
    !
    !
  END SUBROUTINE routing_hierarchy
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_findrout
!!
!>\BRIEF        This subroutine simply computes the route to each outflow point
!!              and returns the outflow point for each point in the basin.  
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_findrout(nbi, nbj, trip, basin_sz, basinid, nbout, outflow, trip_flow, outsz)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std)                                          :: nbi       !! Number of point in x within the grid (unitless)
    INTEGER(i_std)                                          :: nbj       !! Number of point in y within the grid (unitless)
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax)                :: trip      !! The trip field (unitless)
    INTEGER(i_std)                                          :: basin_sz  !!
    INTEGER(i_std)                                          :: basinid   !!
    !
!! OUTPUT VARIABLES
    INTEGER(i_std), DIMENSION(nbvmax,2), INTENT(out)        :: outflow   !!
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax,2), INTENT(out) :: trip_flow !!
    INTEGER(i_std), INTENT(out)                             :: nbout     !! 
    INTEGER(i_std), DIMENSION(nbvmax), INTENT(out)          :: outsz     !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std), DIMENSION(8,2)                          :: inc       !!
    INTEGER(i_std)                                          :: ip, jp, ib, cnt, trp, totsz !! Indices (unitless)
    CHARACTER(LEN=7)                                        :: fmt       !!

!_ ================================================================================================================================
    !
    !
    !  The routing code (i=1, j=2)
    !
    inc(1,1) = 0
    inc(1,2) = -1
    inc(2,1) = 1
    inc(2,2) = -1
    inc(3,1) = 1
    inc(3,2) = 0
    inc(4,1) = 1
    inc(4,2) = 1
    inc(5,1) = 0
    inc(5,2) = 1
    inc(6,1) = -1
    inc(6,2) = 1
    inc(7,1) = -1
    inc(7,2) = 0
    inc(8,1) = -1
    inc(8,2) = -1
    !
    !
    !  Get the outflows and determine for each point to which outflow point it belong 
    !
    nbout = 0
    trip_flow(:,:,:) = 0
    DO ip=1,nbi
       DO jp=1,nbj
          IF ( trip(ip,jp) .GT. 9) THEN
             nbout = nbout + 1
             outflow(nbout,1) = ip
             outflow(nbout,2) = jp
          ENDIF 
          IF ( trip(ip,jp) .GT. 0) THEN
             trip_flow(ip,jp,1) = ip
             trip_flow(ip,jp,2) = jp
          ENDIF
       ENDDO
    ENDDO
    !
    ! Follow the flow of the water
    !
    DO ip=1,nbi
       DO jp=1,nbj
          IF ( (trip_flow(ip,jp,1) + trip_flow(ip,jp,2)) .GT. 0) THEN
             trp = trip(trip_flow(ip,jp,1), trip_flow(ip,jp,2))
             cnt = 0
             DO WHILE ( trp .GT. 0 .AND. trp .LT. 9 .AND. cnt .LT. nbi*nbj) 
                cnt = cnt + 1
                trip_flow(ip,jp,1) = trip_flow(ip,jp,1) + inc(trp,1)
                trip_flow(ip,jp,2) = trip_flow(ip,jp,2) + inc(trp,2)
                trp = trip(trip_flow(ip,jp,1), trip_flow(ip,jp,2))
             ENDDO
             IF ( cnt .EQ. nbi*nbj) THEN
                WRITE(numout,*) 'We could not route point (routing_findrout) :', ip, jp
                WRITE(numout,*) '-------------> trip '
                WRITE(fmt,"('(',I3,'I6)')") nbi
                DO ib=1,nbj
                   WRITE(numout,fmt) trip(1:nbi,ib)
                ENDDO
                CALL ipslerr_p(3,'routing_findrout','We could not route point','','')
             ENDIF
          ENDIF
       ENDDO
    ENDDO
    !
    !  What is the size of the region behind each outflow point ?
    !
    totsz = 0
    DO ip=1,nbout
       outsz(ip) = COUNT(trip_flow(:,:,1) .EQ. outflow(ip,1) .AND. trip_flow(:,:,2) .EQ. outflow(ip,2))
       totsz = totsz + outsz(ip)
    ENDDO
    IF ( basin_sz .NE. totsz) THEN
       WRITE(numout,*) 'Water got lost while I tried to follow it '
       WRITE(numout,*) basin_sz, totsz
       WRITE(numout,*) 'Basin id :', basinid
       DO ip=1,nbout
          WRITE(numout,*) 'ip :', ip, ' outsz :', outsz(ip), ' outflow :', outflow(ip,1), outflow(ip,2)
       ENDDO
       WRITE(numout,*) '-------------> trip '
       WRITE(fmt,"('(',I3,'I6)')") nbi
       DO jp=1,nbj
          WRITE(numout,fmt) trip(1:nbi,jp)
       ENDDO
       CALL ipslerr_p(3,'routing_findrout','Water got lost while I tried to follow it','','')
    ENDIF
    !
  END SUBROUTINE routing_findrout
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_globalize
!!
!>\BRIEF        This subroutine puts the basins found for grid box in the global map.
!!               Connection can only be made later when all information is together. 
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!! One of the outputs is basin_flowdir. Its convention is 1-8 for the directions from North to North
!! West going through South. The negative values will be -3 for return flow, -2 for coastal flow
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_globalize(nbpt, ib, neighbours, area_bx, trip_bx, hierarchy_bx, topoind_bx, min_topoind,&
       & nb_basin, basin_inbxid, basin_sz, basin_pts, basin_bxout, coast_pts, nwbas, basin_count,&
       & basin_area, basin_hierarchy, basin_topoind, basin_id, basin_flowdir, outflow_grid,&
       & nbcoastal, coastal_basin)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT (in)                :: nbpt                   !! Domain size (unitless)
    INTEGER(i_std), INTENT (in)                :: ib                     !! Current basin (unitless)
    INTEGER(i_std), INTENT(in)                 :: neighbours(nbpt,8)     !! Vector of neighbours for each grid point (1=N, 2=E, 3=S, 4=W)
    !
!! LOCAL VARIABLES 
    REAL(r_std), DIMENSION(nbvmax,nbvmax)      :: area_bx                !! Area of each small box in the grid box (m^2)
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax)   :: trip_bx                !! The trip field for each of the smaller boxes (unitless)
    REAL(r_std), DIMENSION(nbvmax,nbvmax)      :: hierarchy_bx           !! Level in the basin of the point
    REAL(r_std), DIMENSION(nbvmax,nbvmax)      :: topoind_bx             !! Topographic index of the residence time for each of the smaller boxes (m)
    REAL(r_std)                                :: min_topoind            !! The current minimum of topographic index (m)
    INTEGER(i_std)                             :: nb_basin               !! Number of sub-basins (unitless)
    INTEGER(i_std), DIMENSION(nbvmax)          :: basin_inbxid, basin_sz !! ID of basin, number of points in the basin
    INTEGER(i_std), DIMENSION(nbvmax,nbvmax,2) :: basin_pts              !! Points in each basin
    INTEGER(i_std), DIMENSION(nbvmax)          :: basin_bxout            !! outflow direction
    INTEGER(i_std)                             :: coast_pts(nbvmax)      !! The coastal flow points (unitless)
    ! global maps
    INTEGER(i_std)                             :: nwbas                  !!
    INTEGER(i_std), DIMENSION(nbpt)            :: basin_count            !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)      :: basin_id               !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)      :: basin_flowdir          !! Water flow directions in the basin (unitless)
    REAL(r_std), DIMENSION(nbpt,nwbas)         :: basin_area             !!
    REAL(r_std), DIMENSION(nbpt,nwbas)         :: basin_hierarchy        !!
    REAL(r_std), DIMENSION(nbpt,nwbas)         :: basin_topoind          !! Topographic index of the residence time for a basin (m)
    INTEGER(i_std), DIMENSION(nbpt,nwbas)      :: outflow_grid           !! Type of outflow on the grid box (unitless)
    INTEGER(i_std), DIMENSION(nbpt)            :: nbcoastal              !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)      :: coastal_basin          !!
    !
    INTEGER(i_std)                             :: ij, iz                 !! Indices (unitless)
    CHARACTER(LEN=4)                           :: hierar_method = 'OUTP' !!

!_ ================================================================================================================================
    !
    !
    DO ij=1, nb_basin
       !
       ! Count the basins and keep their ID
       !
       basin_count(ib) = basin_count(ib)+1
       if (basin_count(ib) > nwbas) then 
          WRITE(numout,*) 'ib=',ib
          call ipslerr_p(3,'routing_globalize', &
               &      'Problem with basin_count : ', & 
               &      'It is greater than number of allocated basin nwbas.', &
               &      '(stop to count basins)')
       endif 
       basin_id(ib,basin_count(ib)) = basin_inbxid(ij)
       !
       ! Transfer the list of basins which flow into the ocean as coastal flow.
       !
       IF ( basin_id(ib,basin_count(ib)) .LT. 0) THEN
          nbcoastal(ib) = basin_sz(ij)
          coastal_basin(ib,1:nbcoastal(ib)) = coast_pts(1:nbcoastal(ib))
       ENDIF
       !
       !
       ! Compute the area of the basin
       !
       basin_area(ib,ij) = zero
       basin_hierarchy(ib,ij) = zero
       !
       SELECT CASE (hierar_method)
          !
          CASE("MINI")
             basin_hierarchy(ib,ij) = undef_sechiba
          !
       END SELECT
       basin_topoind(ib,ij) = zero
       !
       DO iz=1,basin_sz(ij)
          !
          basin_area(ib,ij) = basin_area(ib,ij) + area_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2))
          basin_topoind(ib,ij) = basin_topoind(ib,ij) + topoind_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2))
          !
          ! There are a number of ways to determine the hierarchy of the entire basin.
          ! We allow for three here :
          !     - Take the mean value
          !     - Take the minimum value within the basin
          !     - Take the value at the outflow point
          ! Probably taking the value of the outflow point is the best solution.
          !
          SELECT CASE (hierar_method)
             !
             CASE("MEAN")
                ! Mean hierarchy of the basin
                basin_hierarchy(ib,ij) = basin_hierarchy(ib,ij) + &
                     & hierarchy_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2))
             CASE("MINI")
                ! The smallest value of the basin
                IF ( hierarchy_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2)) .LT. basin_hierarchy(ib,ij)) THEN
                   basin_hierarchy(ib,ij) = hierarchy_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2))
                ENDIF
             CASE("OUTP")
                ! Value at the outflow point
                IF ( trip_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2)) .GT. 100 ) THEN
                   basin_hierarchy(ib,ij) = hierarchy_bx(basin_pts(ij,iz,1),basin_pts(ij,iz,2))
                ENDIF
             CASE DEFAULT
                WRITE(numout,*) 'Unknown method for computing the hierarchy of the basin'
                CALL ipslerr_p(3,'routing_globalize','Unknown method for computing the hierarchy of the basin','','')
          END SELECT
          !
       ENDDO
       !
       basin_topoind(ib,ij) = basin_topoind(ib,ij)/REAL(basin_sz(ij),r_std)
       !
       SELECT CASE (hierar_method)
          !
          CASE("MEAN")
             basin_hierarchy(ib,ij) = basin_hierarchy(ib,ij)/REAL(basin_sz(ij),r_std)
          !
       END SELECT
       !
       ! To make sure that it has the lowest number if this is an outflow point we reset  basin_hierarchy
       !
       IF (basin_bxout(ij) .LT. 0) THEN
          basin_hierarchy(ib,ij) = min_topoind
          basin_topoind(ib,ij) = min_topoind
       ENDIF
       !
       !
       ! Keep the outflow boxes and basin
       !
       basin_flowdir(ib,ij) = basin_bxout(ij)
       IF (basin_bxout(ij) .GT. 0) THEN
          outflow_grid(ib,ij) = neighbours(ib,basin_bxout(ij))
       ELSE
          outflow_grid(ib,ij) = basin_bxout(ij)
       ENDIF
       !
       !
    ENDDO
    !

    !
  END SUBROUTINE routing_globalize
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_linkup
!!
!>\BRIEF         This subroutine makes the connections between the basins and ensure global coherence. 
!!
!! DESCRIPTION (definitions, functional, design, flags) :
!! The convention for outflow_grid is :
!! outflow_grid = -1 : River flow
!! outflow_grid = -2 : Coastal flow
!! outflow_grid = -3 : Return flow\n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_linkup(nbpt, neighbours, nwbas, basin_count, basin_area, basin_id, basin_flowdir, &
       & basin_hierarchy, outflow_grid, outflow_basin, inflow_number, inflow_grid, inflow_basin, nbcoastal,&
       & coastal_basin, invented_basins)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT (in)                    :: nbpt                  !! Domain size  (unitless)
    INTEGER(i_std), DIMENSION(nbpt,8), INTENT (in) :: neighbours            !!
    REAL(r_std), INTENT(in)                        :: invented_basins       !!
    !
    INTEGER(i_std)                                 :: nwbas                 !!
    INTEGER(i_std), DIMENSION(nbpt)                :: basin_count           !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)          :: basin_id              !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)          :: basin_flowdir         !! Water flow directions in the basin (unitless)
    REAL(r_std), DIMENSION(nbpt,nwbas)             :: basin_area            !!
    REAL(r_std), DIMENSION(nbpt,nwbas)             :: basin_hierarchy       !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)          :: outflow_grid          !! Type of outflow on the grid box (unitless)
    INTEGER(i_std), DIMENSION(nbpt,nwbas)          :: outflow_basin         !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)          :: inflow_number         !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas,nbvmax)   :: inflow_basin          !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas,nbvmax)   :: inflow_grid           !!
    INTEGER(i_std), DIMENSION(nbpt)                :: nbcoastal             !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)          :: coastal_basin         !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                                 :: sp, sb, sbl, inp, bid, outdm1, outdp1 !! Indices (unitless)
    INTEGER(i_std)                                 :: dp1, dm1, dm1i, dp1i, bp1, bm1 !! Indices (unitless)
    INTEGER(i_std)                                 :: dop, bop              !!
    INTEGER(i_std)                                 :: fbas(nwbas), nbfbas   !!
    REAL(r_std)                                    :: fbas_hierarchy(nwbas) !!
    INTEGER(i_std)                                 :: ff(1)                 !!
    !
    ! ERRORS 
    LOGICAL                                        :: error1, error2, error3, error4, error5 !! (true/false)
    !
!! PARAMETERS
    LOGICAL, PARAMETER                             :: check = .FALSE.       !! (true/false)

!_ ================================================================================================================================    
    error1=.FALSE.
    error2=.FALSE.
    error3=.FALSE.
    error4=.FALSE.
    error5=.FALSE.

    outflow_basin(:,:) = undef_int
    inflow_number(:,:) = 0
    !
    DO sp=1,nbpt
       DO sb=1,basin_count(sp)
          !
          inp = outflow_grid(sp,sb)
          bid = basin_id(sp,sb)
          !
          ! We only work on this point if it does not flow into the ocean
          ! At this point any of the outflows is designated by a negative values in
          ! outflow_grid
          !
          IF ( inp .GT. 0 ) THEN
             !
             ! Now find the basin in the onflow point (inp)
             !
             nbfbas = 0
             !
             !
             DO sbl=1,basin_count(inp)
                !
                ! Either it is a standard basin or one aggregated from ocean flow points.
                ! If we flow into a another grid box we have to make sure that its hierarchy in the
                ! basin is lower.
                !
                !
                IF ( basin_id(inp,sbl) .GT. 0 ) THEN
                   IF ( basin_id(inp,sbl) .EQ. bid .OR. basin_id(inp,sbl) .GT. invented_basins) THEN
                      nbfbas =nbfbas + 1
                      fbas(nbfbas) = sbl
                      fbas_hierarchy(nbfbas) = basin_hierarchy(inp,sbl)
                   ENDIF
                ELSE
                   IF ( COUNT(coastal_basin(inp,1:nbcoastal(inp)) .EQ. bid) .GT. 0 ) THEN
                      nbfbas =nbfbas + 1
                      fbas(nbfbas) = sbl
                      fbas_hierarchy(nbfbas) = basin_hierarchy(inp,sbl)
                   ENDIF
                ENDIF
                !
             ENDDO
             ! 
             !  If we have more than one basin we will take the one which is lowest
             !  in the hierarchy.
             !
             IF (nbfbas .GE. 1) THEN
                ff = MINLOC(fbas_hierarchy(1:nbfbas))
                sbl = fbas(ff(1))
                !
                bop = undef_int
                IF ( basin_hierarchy(inp,sbl) .LE. basin_hierarchy(sp,sb) ) THEN
                   IF ( basin_hierarchy(inp,sbl) .LE. basin_hierarchy(sp,sb) ) THEN
                      bop = sbl
                   ELSE
                      ! The same hierarchy is allowed if both grids flow in about 
                      ! the same direction :
                      IF ( ( MOD(basin_flowdir(inp,sbl)+1-1, 8)+1 .EQ. basin_flowdir(sp,sb)).OR. &
                           & ( basin_flowdir(inp,sbl) .EQ. basin_flowdir(sp,sb)).OR. &
                           & ( MOD(basin_flowdir(inp,sbl)+7-1, 8)+1 .EQ. basin_flowdir(sp,sb)) ) THEN
                         bop = sbl
                      ENDIF
                   ENDIF
                ENDIF
                !
                ! If the basin is suitable (bop < undef_int) then take it
                !
                IF ( bop .LT. undef_int ) THEN
                   outflow_basin(sp,sb) = bop
                   inflow_number(inp,bop) =  inflow_number(inp,bop) + 1
                   IF ( inflow_number(inp,bop) .LE. nbvmax ) THEN
                      inflow_grid(inp, bop, inflow_number(inp,bop)) = sp
                      inflow_basin(inp, bop, inflow_number(inp,bop)) = sb
                   ELSE
                      error1=.TRUE.
                      EXIT
                   ENDIF
                ENDIF
             ENDIF
             !
             !
          ENDIF
          !
          !
          !
          ! Did we find it ?
          !
          ! In case the outflow point was ocean or we did not find the correct basin we start to look
          ! around. We find two options for the outflow direction (dp1 & dm1) and the corresponding 
          ! basin index (bp1 & bm1).
          !
          !
          IF ( outflow_basin(sp,sb) .EQ. undef_int &
               & .AND. basin_flowdir(sp,sb) .GT. 0) THEN
             !
             dp1i = MOD(basin_flowdir(sp,sb)+1-1, 8)+1
             dp1 = neighbours(sp,dp1i)
             dm1i = MOD(basin_flowdir(sp,sb)+7-1, 8)+1
             IF ( dm1i .LT. 1 ) dm1i = 8
             dm1 = neighbours(sp,dm1i)
             !
             !
             bp1 = -1
             IF ( dp1 .GT. 0 ) THEN
                DO sbl=1,basin_count(dp1)
                   IF (basin_id(dp1,sbl) .EQ. bid .AND.&
                        & basin_hierarchy(sp,sb) .GE. basin_hierarchy(dp1,sbl) .AND. &
                        & bp1 .LT. 0) THEN
                      IF ( basin_hierarchy(sp,sb) .GT. basin_hierarchy(dp1,sbl) ) THEN
                         bp1 = sbl
                      ELSE
                         ! The same hierarchy is allowed if both grids flow in about 
                         ! the same direction :
                         IF ( ( MOD(basin_flowdir(dp1,sbl)+1-1, 8)+1 .EQ. basin_flowdir(sp,sb)).OR. &
                            & ( basin_flowdir(dp1,sbl) .EQ. basin_flowdir(sp,sb)).OR. &
                            & ( MOD(basin_flowdir(dp1,sbl)+7-1, 8)+1 .EQ. basin_flowdir(sp,sb)) ) THEN
                            bp1 = sbl
                         ENDIF
                      ENDIF
                   ENDIF
                ENDDO
             ENDIF
             !
             bm1 = -1
             IF ( dm1 .GT. 0 ) THEN
                DO sbl=1,basin_count(dm1)
                   IF (basin_id(dm1,sbl) .EQ. bid .AND.&
                        & basin_hierarchy(sp,sb) .GE. basin_hierarchy(dm1,sbl) .AND. &
                        & bm1 .LT. 0) THEN
                      IF ( basin_hierarchy(sp,sb) .GT. basin_hierarchy(dm1,sbl) ) THEN
                         bm1 = sbl
                      ELSE                         
                         ! The same hierarchy is allowed if both grids flow in about 
                         ! the same direction :
                         IF ( ( MOD(basin_flowdir(dm1,sbl)+1-1, 8)+1 .EQ. basin_flowdir(sp,sb)) .OR. &
                            & ( basin_flowdir(dm1,sbl) .EQ. basin_flowdir(sp,sb)) .OR. &
                            & ( MOD(basin_flowdir(dm1,sbl)+7-1, 8)+1 .EQ. basin_flowdir(sp,sb)) ) THEN
                            bm1 = sbl
                         ENDIF
                      ENDIF
                   ENDIF
                ENDDO
             ENDIF
             !
             !
             ! First deal with the case on land.
             !
             ! For that we need to check if the water will be able to flow out of the grid dp1 or dm1 
             ! and not return to our current grid. If it is the current grid
             ! then we can not do anything with that neighbour. Thus we set the
             ! value of outdm1 and outdp1 back to -1
             !
             outdp1 = undef_int
             IF ( dp1 .GT. 0 .AND. bp1 .GT. 0 ) THEN
                ! if the outflow is into the ocean then we put something less than undef_int in outdp1!
                IF (basin_flowdir(dp1,bp1) .GT. 0) THEN
                   outdp1 = neighbours(dp1,basin_flowdir(dp1,bp1))
                   IF ( outdp1 .EQ. sp ) outdp1 = undef_int 
                ELSE
                   outdp1 = nbpt + 1
                ENDIF
             ENDIF
             outdm1 = undef_int
             IF ( dm1 .GT. 0 .AND. bm1 .GT. 0 ) THEN
                IF (basin_flowdir(dm1,bm1) .GT. 0) THEN
                   outdm1 = neighbours(dm1,basin_flowdir(dm1,bm1))
                   IF ( outdm1 .EQ. sp )  outdm1 = undef_int
                ELSE
                   outdm1 = nbpt + 1
                ENDIF
             ENDIF
             !
             ! Now that we know our options we need go through them.
             !
             dop = undef_int
             bop = undef_int
             IF ( outdp1 .LT. undef_int .AND. outdm1 .LT. undef_int) THEN
                ! 
                ! In this case we let the current basin flow into the smaller one
                !
                IF ( basin_area(dp1,bp1) .LT.  basin_area(dm1,bm1) ) THEN
                   dop = dp1
                   bop = bp1
                ELSE
                   dop = dm1
                   bop = bm1
                ENDIF
                !
                !
             ELSE IF (  outdp1 .LT. undef_int ) THEN
                ! If only the first one is possible
                dop = dp1
                bop = bp1
             ELSE IF ( outdm1 .LT. undef_int ) THEN
                ! If only the second one is possible
                dop = dm1
                bop = bm1
             ELSE
                !
                ! Now we are at the point where none of the neighboring points is suitable 
                ! or we have a coastal point.
                !
                ! If there is an option to put the water into the ocean go for it.
                !
                IF ( outflow_grid(sp,sb) .LT. 0 .OR. dm1 .LT. 0 .OR. dp1 .LT. 0 ) THEN
                   dop = -1
                ELSE
                   !
                   ! If we are on a land point with only land neighbors but no one suitable to let the 
                   ! water flow into we have to look for a solution in the current grid box.
                   ! 
                   !
                   IF ( bp1 .LT. 0 .AND. bm1 .LT. 0 ) THEN
                      !
                      ! Do we have more than one basin with the same ID ?
                      !
                      IF ( COUNT(basin_id(sp,1:basin_count(sp)) .EQ. bid) .GE. 2) THEN
                         !
                         ! Now we can try the option of flowing into the basin of the same grid box.
                         !
                         DO sbl=1,basin_count(sp)
                            IF (sbl .NE. sb .AND. basin_id(sp,sbl) .EQ. bid) THEN
                               ! In case this basin has a lower hierarchy or flows into a totaly
                               ! different direction we go for it.
                               IF ( (basin_hierarchy(sp,sb) .GE. basin_hierarchy(sp,sbl)) .OR. &
                                    & (basin_flowdir(sp,sbl) .LT. dm1i .AND.&
                                    & basin_flowdir(sp,sbl) .GT. dp1i) ) THEN
                                  dop = sp
                                  bop = sbl
                                  IF (check) THEN
                                     IF (basin_hierarchy(sp,sb) .LT. basin_hierarchy(sp,sbl)) THEN
                                        WRITE(numout,*) '>>>>>>> POINT CORRECTED against hierarchy :',&
                                             & sp, sb, 'into', sbl
                                     ENDIF
                                  ENDIF
                               ENDIF
                               !
                            ENDIF
                         ENDDO
                         !
                      ENDIF
                   ENDIF
                ENDIF
                !
                IF ( dop .EQ. undef_int .AND. bop .EQ. undef_int ) THEN
                   IF (check) THEN
                      WRITE(numout,*) 'Why are we here with point ', sp, sb
                      WRITE(numout,*) 'Coordinates : (lon,lat) = ', lalo(sp,2), lalo(sp,1)
                      WRITE(numout,*) 'Contfrac : = ', contfrac(sp)
                      WRITE(numout,*) 'Local Basin ID :', basin_id(sp,1:basin_count(sp))
                      WRITE(numout,*) 'Local hierarchies :', basin_hierarchy(sp,1:basin_count(sp))
                      WRITE(numout,*) 'Local basin_flowdir :', basin_flowdir(sp,1:basin_count(sp))
                      WRITE(numout,*) 'Local outflowgrid :', outflow_grid(sp,1:basin_count(sp))
                      WRITE(numout,*) 'outflow_grid :', inp
                      WRITE(numout,*) 'Coodinates outflow : (lon,lat) = ', lalo(inp,2), lalo(inp,1)
                      WRITE(numout,*) 'Contfrac : = ', contfrac(inp)
                      WRITE(numout,*) 'Outflow Basin ID :', basin_id(inp,1:basin_count(inp))
                      WRITE(numout,*) 'Outflow hierarchies :', basin_hierarchy(inp,1:basin_count(inp))
                      WRITE(numout,*) 'Outflow basin_flowdir :', basin_flowdir(inp,1:basin_count(inp))
                      WRITE(numout,*) 'Explored options +1 :', dp1, bp1, outdp1
                      WRITE(numout,*) 'Explored +1 Basin ID :', basin_id(dp1,1:basin_count(dp1))
                      WRITE(numout,*) 'Explored +1 hierarchies :', basin_hierarchy(dp1,1:basin_count(dp1))
                      WRITE(numout,*) 'Explored +1 basin_flowdir :', basin_flowdir(dp1,1:basin_count(dp1))
                      WRITE(numout,*) 'Explored options -1 :', dm1, bm1, outdm1
                      WRITE(numout,*) 'Explored -1 Basin ID :', basin_id(dm1,1:basin_count(dm1))
                      WRITE(numout,*) 'Explored -1 hierarchies :', basin_hierarchy(dm1,1:basin_count(dm1))
                      WRITE(numout,*) 'Explored -1 basin_flowdir :', basin_flowdir(dm1,1:basin_count(dm1))
                      WRITE(numout,*) '****************************'
                   ENDIF
                   IF ( contfrac(sp) > 0.01 ) THEN
                      error2=.TRUE.
                      EXIT
                   ENDIF
                ENDIF
                !
             ENDIF
             !
             ! Now that we know where we want the water to flow to we write the
             ! the information in the right fields.
             !
             IF ( dop .GT. 0 .AND. dop .NE. undef_int ) THEN
                outflow_grid(sp,sb) = dop
                outflow_basin(sp,sb) = bop
                inflow_number(dop,bop) =  inflow_number(dop,bop) + 1
                IF ( inflow_number(dop,bop) .LE. nbvmax ) THEN
                   inflow_grid(dop, bop, inflow_number(dop,bop)) = sp
                   inflow_basin(dop, bop, inflow_number(dop,bop)) = sb
                ELSE
                   error3=.TRUE.
                   EXIT
                ENDIF
                !
             ELSE
                outflow_grid(sp,sb) = -2
                outflow_basin(sp,sb) = undef_int
             ENDIF
             !
          ENDIF
          !
          !
          ! If we still have not found anything then we have to check that there is not a basin
          ! within the same grid box which has a lower hierarchy.
          !
          !
          IF ( outflow_grid(sp,sb) .GT. 0 .AND. outflow_basin(sp,sb) .EQ. undef_int &
               & .AND. basin_flowdir(sp,sb) .GT. 0) THEN
             !
             
             IF (check) &
                  WRITE(numout,*) 'There is no reason to here, this part of the code should be dead :', sp,sb
             !
             DO sbl=1,basin_count(sp)
                !
                ! Three conditions are needed to let the water flow into another basin of the
                ! same grid :
                ! - another basin than the current one
                ! - same ID
                ! - of lower hierarchy.
                !
                IF ( (sbl .NE. sb) .AND. (basin_id(sp,sbl) .EQ. bid)&
                     & .AND. (basin_hierarchy(sp,sb) .GT. basin_hierarchy(sp,sbl)) ) THEN
                   outflow_basin(sp,sb) = sbl
                   inflow_number(sp,sbl) =  inflow_number(sp,sbl) + 1
                   IF ( inflow_number(sp,sbl) .LE. nbvmax ) THEN
                      IF ( sp .EQ. 42 .AND. sbl .EQ. 1) THEN
                         IF (check) &
                              WRITE(numout,*) 'ADD INFLOW (3):', sp, sb
                      ENDIF
                      inflow_grid(sp, sbl, inflow_number(sp,sbl)) = sp
                      inflow_basin(sp, sbl, inflow_number(sp,sbl)) = sb
                   ELSE
                      error4=.TRUE.
                      EXIT
                   ENDIF
                ENDIF
             ENDDO
          ENDIF
          !
          ! Ok that is it, we give up :-)
          !
          IF ( outflow_grid(sp,sb) .GT. 0 .AND. outflow_basin(sp,sb) .EQ. undef_int &
               & .AND. basin_flowdir(sp,sb) .GT. 0) THEN
             !
             error5=.TRUE.
             EXIT
          ENDIF
       ENDDO
       !
    ENDDO
    IF (error1) THEN
       WRITE(numout,*) " routing_linkup : bop .LT. undef_int",bop
       CALL ipslerr_p(3,'routing_linkup', &
            "bop .LT. undef_int",'Increase nbvmax','stop routing_linkup')
    ENDIF
    IF (error2) THEN
       CALL ipslerr_p(3,'routing_linkup', &
            &      'In the routine which make connections between the basins and ensure global coherence,', & 
            &      'there is a problem with outflow linkup without any valid direction. Try with check=.TRUE.', &
            &      '(Perhaps there is a problem with the grid.)')
    ENDIF
    IF (error3) THEN
       WRITE(numout,*) " routing_linkup : dop .GT. 0 .AND. dop .NE. undef_int",dop
       CALL ipslerr_p(3,'routing_linkup', &
            "dop .GT. 0 .AND. dop .NE. undef_int",'Increase nbvmax. Try with check=.TRUE.','stop routing_linkup')
    ENDIF
    IF (error4) THEN
       WRITE(numout,*) " routing_linkup : (sbl .NE. sb) .AND. (basin_id(sp,sbl) .EQ. bid) ", & 
            &  " .AND. (basin_hierarchy(sp,sb) .GT. basin_hierarchy(sp,sbl))",sbl,sb,basin_id(sp,sbl),bid, & 
            &  basin_hierarchy(sp,sb),basin_hierarchy(sp,sbl)
       CALL ipslerr_p(3,'routing_linkup', &
            "(sbl .NE. sb) .AND. (basin_id(sp,sbl) .EQ. bid) .AND. (basin_hierarchy(sp,sb) .GT. basin_hierarchy(sp,sbl))" &
            ,'Increase nbvmax. Try with check=.TRUE.','stop routing_linkup')
    ENDIF
    IF (error5) THEN
       WRITE(numout,*) 'We could not find the basin into which we need to flow'
       WRITE(numout,*) 'Grid point ', sp, ' and basin ', sb
       WRITE(numout,*) 'Explored neighbours :', dm1, dp1 
       WRITE(numout,*) 'Outflow direction :', basin_flowdir(sp,sb)
       WRITE(numout,*) 'Outlfow grid :', outflow_grid(sp,sb)
       WRITE(numout,*) 'Outlfow basin :',outflow_basin(sp,sb)
       WRITE(numout,*) 'basin ID:',basin_id(sp,sb)
       WRITE(numout,*) 'Hierarchy :', basin_hierarchy(sp,sb)
       CALL ipslerr_p(3,'routing_linkup', &
            "We could not find the basin into which we need to flow",'Try with check=.TRUE.','stop routing_linkup')
    ENDIF
    !
    ! Check for each outflow basin that it exists
    !
    DO sp=1,nbpt
       DO sb=1,basin_count(sp)
          !
          inp = outflow_grid(sp,sb)
          sbl = outflow_basin(sp,sb)
          IF ( inp .GE. 0 ) THEN
             IF ( basin_count(inp) .LT. sbl ) THEN
                WRITE(numout,*) 'point :', sp, ' basin :', sb
                WRITE(numout,*) 'Flows into point :', inp, ' basin :', sbl
                WRITE(numout,*) 'But it flows into now here as basin count = ', basin_count(inp)
                CALL ipslerr_p(3,'routing_linkup','Problem with outflow','','')
             ENDIF
          ENDIF
       ENDDO
    ENDDO
    !
  END SUBROUTINE routing_linkup
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_fetch
!!
!>\BRIEF        This subroutine computes the fetch of each basin. This means that for each basin we
!!               will know how much area is upstream. It will help decide how to procede with the
!!               the truncation later and allow to set correctly in outflow_grid the distinction
!!               between coastal and river flow. 
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_fetch(nbpt, resolution, contfrac, nwbas, basin_count, basin_area, basin_id,&
       & outflow_grid, outflow_basin, fetch_basin)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                           :: nbpt          !! Domain size  (unitless)
    !
    REAL(r_std), DIMENSION(nbpt,2), INTENT(in)           :: resolution    !! The size of each grid box in X and Y (m)
    REAL(r_std), DIMENSION(nbpt), INTENT(in)             :: contfrac      !! Fraction of land in each grid box (unitless;0-1)
    !
    INTEGER(i_std)                                       :: nwbas         !!
    INTEGER(i_std), DIMENSION(nbpt), INTENT(in)          :: basin_count   !!
    REAL(r_std), DIMENSION(nbpt,nwbas), INTENT(inout)    :: basin_area    !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas), INTENT(in)    :: basin_id      !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas), INTENT(inout) :: outflow_grid  !! Type of outflow on the grid box (unitless)
    INTEGER(i_std), DIMENSION(nbpt,nwbas), INTENT(in)    :: outflow_basin !!
!
!! OUTPUT VARIABLES
    REAL(r_std), DIMENSION(nbpt,nwbas), INTENT(out)      :: fetch_basin   !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                                        :: ib, ij, ff(1), it, itt, igrif, ibasf, nboutflow !! Indices (unitless)
    REAL(r_std)                                           :: contarea     !!
    REAL(r_std)                                           :: totbasins    !!
    REAL(r_std), DIMENSION(nbpt*nbvmax)                   :: tmp_area     !!
    INTEGER(i_std), DIMENSION(nbpt*nbvmax,2)              :: tmpindex     !!

!_ ================================================================================================================================
    !
    !
    ! Normalize the area of all basins
    !
    DO ib=1,nbpt
       !
       totbasins = SUM(basin_area(ib,1:basin_count(ib)))
       contarea = resolution(ib,1)*resolution(ib,2)*contfrac(ib)
       !
       DO ij=1,basin_count(ib)
          basin_area(ib,ij) = basin_area(ib,ij)/totbasins*contarea
       ENDDO
       !
    ENDDO
    WRITE(numout,*) 'Normalization done'
    !
    ! Compute the area upstream of each basin
    !
    fetch_basin(:,:) = zero
    !
    !
    DO ib=1,nbpt
       !
       DO ij=1,basin_count(ib)
          !
          fetch_basin(ib, ij) = fetch_basin(ib, ij) + basin_area(ib,ij)
          !
          igrif = outflow_grid(ib,ij)
          ibasf = outflow_basin(ib,ij)
          !
          itt = 0
          DO WHILE (igrif .GT. 0)
             fetch_basin(igrif,ibasf) =  fetch_basin(igrif,ibasf) + basin_area(ib, ij)
             it = outflow_grid(igrif, ibasf)
             ibasf = outflow_basin(igrif, ibasf)
             igrif = it
             itt = itt + 1
             IF ( itt .GT. 500) THEN
                WRITE(numout,&
                     "('Grid ',I5, ' and basin ',I5, 'did not converge after iteration ',I5)") ib, ij, itt
                WRITE(numout,*) 'Basin ID :', basin_id(igrif,ibasf)
                WRITE(numout,&
                     "('We are stuck with the flow into grid ',I5,' and basin ',I5)") igrif, ibasf
                IF ( itt .GT. 510) THEN
                   CALL ipslerr_p(3,'routing_fetch','Problem...','','')
                ENDIF
             ENDIF
          ENDDO
          !
       ENDDO
       !
    ENDDO
    !
    WRITE(numout,*) 'The smallest FETCH :', MINVAL(fetch_basin)
    WRITE(numout,*) 'The largest FETCH :', MAXVAL(fetch_basin)
    !
    ! Now we set for the 'num_largest' largest basins the outflow condition as stream flow 
    ! (i.e. outflow_grid = -1) and all other outflow basins are set to coastal flow 
    ! (i.e. outflow_grid = -2). The return flow is not touched.
    !
    nboutflow = 0
    !
    DO ib=1,nbpt
       !
       DO ij=1,basin_count(ib)
          !
          ! We do not need any more the river flow flag as we are going to reset it.
          !
          IF ( outflow_grid(ib,ij) .EQ. -1) THEN
             outflow_grid(ib,ij) = -2
          ENDIF
          !
          IF ( outflow_grid(ib,ij) .EQ. -2) THEN
             !
             nboutflow = nboutflow + 1
             tmp_area(nboutflow) = fetch_basin(ib,ij)
             tmpindex(nboutflow,1) = ib
             tmpindex(nboutflow,2) = ij
             !
          ENDIF
          !
       ENDDO
    ENDDO
    !
    DO ib=1, num_largest
       ff = MAXLOC(tmp_area(1:nboutflow))
       outflow_grid(tmpindex(ff(1),1), tmpindex(ff(1),2)) = -1
       tmp_area(ff(1)) = zero
    ENDDO
    !
  END SUBROUTINE routing_fetch
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_truncate
!!
!>\BRIEF         This subroutine reduces the number of basins per grid to the value chosen by the user.
!!               It also computes the final field which will be used to route the water at the
!!               requested truncation.  
!!
!! DESCRIPTION (definitions, functional, design, flags) : 
!! Truncate if needed and find the path closest to the high resolution data.
!!
!! The algorithm :
!! 
!! We only go through this procedure only as many times as there are basins to take out at most.
!! This is important as it allows the simplifications to spread from one grid to the other.
!! The for each step of the iteration and at each grid point we check the following options for 
!! simplifying the pathways of water :
!! 1) If the basin of a grid flows into another basin of the same grid. Kill the one which only
!!    served as a transition
!! 2) If in one grid box we have a number of basins which flow into the ocean as coastal flow. 
!!    We kill the smallest one and put into the largest basin. There is no need to manage many 
!!    basins going into the ocean as coastal flows. 
!! 3) If we have streams run in parallel from one gird box to the others (that is these are 
!!    different basins) we will put the smaller one in the larger one. This may hapen at any
!!    level of the flow but in theory it should propagate downstream.
!! 4) If we have two basins with the same ID but flow into different grid boxes we sacrifice
!!    the smallest one and route it through the largest. 
!!
!! Obviously if any of the options find something then we skip the rest and take out the basin.:\n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_truncate(nbpt, resolution, contfrac, nwbas, basin_count, basin_area, basin_topoind,&
       & fetch_basin, basin_id, basin_flowdir, outflow_grid, outflow_basin, inflow_number,&
       & inflow_grid, inflow_basin)
    !
    IMPLICIT NONE
    !
!! PARAMETERS
    INTEGER(i_std), PARAMETER                       :: pickmax = 200  !!

!! INPUT VARIABLES
    INTEGER(i_std)                                  :: nbpt           !! Domain size  (unitless)
    !
    REAL(r_std), DIMENSION(nbpt,2)                  :: resolution     !! The size of each grid box in X and Y (m)
    REAL(r_std), DIMENSION(nbpt), INTENT(in)        :: contfrac       !! Fraction of land in each grid box (unitless;0-1)
    !
    INTEGER(i_std)                                  :: nwbas          !!
    INTEGER(i_std), DIMENSION(nbpt)                 :: basin_count    !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)           :: basin_id       !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)           :: basin_flowdir  !! Water flow directions in the basin (unitless)
    REAL(r_std), DIMENSION(nbpt,nwbas)              :: basin_area     !!
    REAL(r_std), DIMENSION(nbpt,nwbas)              :: basin_topoind  !! Topographic index of the residence time for a basin (m)
    REAL(r_std), DIMENSION(nbpt,nwbas)              :: fetch_basin    !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)           :: outflow_grid   !! Type of outflow on the grid box (unitless)
    INTEGER(i_std), DIMENSION(nbpt,nwbas)           :: outflow_basin  !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)           :: inflow_number  !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas,nwbas)     :: inflow_basin   !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas,nwbas)     :: inflow_grid    !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                                  :: ib, ij, ibf, ijf, igrif, ibasf, cnt, pold, bold, ff(2) !! Indices (unitless)
    INTEGER(i_std)                                  :: ii, kbas, sbas, ik, iter, ibt, obj !! Indices (unitless)
    REAL(r_std), DIMENSION(nbpt,nbasmax)            :: floflo         !!
    REAL(r_std), DIMENSION(nbpt)                    :: gridarea       !!
    REAL(r_std), DIMENSION(nbpt)                    :: gridbasinarea  !!
    REAL(r_std)                                     :: ratio          !!
    INTEGER(i_std), DIMENSION(pickmax,2)            :: largest_basins !!
    INTEGER(i_std), DIMENSION(pickmax)              :: tmp_ids        !!
    INTEGER(i_std)                                  :: multbas        !!
    INTEGER(i_std)                                  :: iml(1)         !! X resolution of the high resolution grid
    INTEGER(i_std), DIMENSION(pickmax)              :: multbas_sz     !!
    REAL(r_std), DIMENSION(pickmax)                 :: tmp_area       !!
    INTEGER(i_std), DIMENSION(pickmax,pickmax)      :: multbas_list   !!
    !
    INTEGER(i_std)                                  :: nbtruncate     !!
    INTEGER(i_std), SAVE, ALLOCATABLE, DIMENSION(:) :: indextrunc     !!
!$OMP THREADPRIVATE(indextrunc)

!_ ================================================================================================================================
    !
    !
    IF ( .NOT. ALLOCATED(indextrunc)) THEN
       ALLOCATE(indextrunc(nbpt))
    ENDIF
    !
    ! We have to go through the grid as least as often as we have to reduce the number of basins
    ! For good measure we add 3 more passages.
    !
    !
    DO iter = 1, MAXVAL(basin_count) - nbasmax +3
       !
       ! Get the points over which we wish to truncate
       !
       nbtruncate = 0
       DO ib = 1, nbpt
          IF ( basin_count(ib) .GT. nbasmax ) THEN
             nbtruncate = nbtruncate + 1
             indextrunc(nbtruncate) = ib
          ENDIF
       ENDDO
       !
       ! Go through the basins which need to be truncated.       
       !
       DO ibt=1,nbtruncate
          !
          ib = indextrunc(ibt)
          !
          ! Check if we have basin which flows into a basin in the same grid
          ! kbas = basin we will have to kill
          ! sbas = basin which takes over kbas
          !
          kbas = 0
          sbas = 0
          !
          ! 1) Can we find a basin which flows into a basin of the same grid ?
          !
          DO ij=1,basin_count(ib)
             DO ii=1,basin_count(ib)
                IF ( outflow_grid(ib,ii) .EQ. ib .AND. outflow_basin(ib, ii) .EQ. ij .AND. kbas*sbas .NE. 0) THEN
                   kbas = ii
                   sbas = ij
                ENDIF
             ENDDO
          ENDDO
          !
          ! 2) Merge two basins which flow into the ocean as coastal or return flow 
          ! (outflow_grid = -2 or -3). Well obviously only if we have more than 1 and 
          ! have not found anything yet!
          !
          IF ( (COUNT(outflow_grid(ib,1:basin_count(ib)) .EQ. -2) .GT. 1 .OR.&
               & COUNT(outflow_grid(ib,1:basin_count(ib)) .EQ. -3) .GT. 1) .AND.&
               & kbas*sbas .EQ. 0) THEN
             !
             multbas = 0
             multbas_sz(:) = 0
             !
             IF ( COUNT(outflow_grid(ib,1:basin_count(ib)) .EQ. -2) .GT. 1 ) THEN
                obj = -2
             ELSE
                obj = -3
             ENDIF
             !
             ! First we get the list of all basins which go out as coastal or return flow (obj)
             !
             DO ij=1,basin_count(ib)
                IF ( outflow_grid(ib,ij) .EQ. obj ) THEN
                   multbas = multbas + 1
                   multbas_sz(multbas) = ij
                   tmp_area(multbas) = fetch_basin(ib,ij)
                ENDIF
             ENDDO
             !
             ! Now the take the smallest to be transfered to the largest
             !
             iml = MAXLOC(tmp_area(1:multbas), MASK = tmp_area(1:multbas) .GT. zero)
             sbas = multbas_sz(iml(1))
             iml = MINLOC(tmp_area(1:multbas), MASK = tmp_area(1:multbas) .GT. zero)
             kbas = multbas_sz(iml(1))
             !
          ENDIF
          !
          !   3) If we have basins flowing into the same grid but different basins then we put them 
          !   together. Obviously we first work with the grid which has most streams running into it
          !   and putting the smallest in the largests catchments.
          !
          IF ( kbas*sbas .EQ. 0) THEN
             !
             tmp_ids(1:basin_count(ib)) = outflow_grid(ib,1:basin_count(ib))
             multbas = 0
             multbas_sz(:) = 0
             !
             ! First obtain the list of basins which flow into the same basin
             !
             DO ij=1,basin_count(ib)
                IF ( outflow_grid(ib,ij) .GT. 0 .AND.&
                     & COUNT(tmp_ids(1:basin_count(ib)) .EQ. outflow_grid(ib,ij)) .GT. 1) THEN
                   multbas = multbas + 1
                   DO ii=1,basin_count(ib)
                      IF ( tmp_ids(ii) .EQ. outflow_grid(ib,ij)) THEN
                         multbas_sz(multbas) = multbas_sz(multbas) + 1
                         multbas_list(multbas,multbas_sz(multbas)) = ii
                         tmp_ids(ii) = -99
                      ENDIF
                   ENDDO
                ELSE
                   tmp_ids(ij) = -99
                ENDIF
             ENDDO
             !
             ! Did we come up with any basins to deal with this way ?
             !
             IF ( multbas .GT. 0 ) THEN
                !
                iml = MAXLOC(multbas_sz(1:multbas))
                ik = iml(1)
                !
                ! Take the smallest and largest of these basins !
                !
                DO ii=1,multbas_sz(ik)
                   tmp_area(ii) = fetch_basin(ib, multbas_list(ik,ii))
                ENDDO
                !
                iml = MAXLOC(tmp_area(1:multbas_sz(ik)), MASK = tmp_area(1:multbas_sz(ik)) .GT. zero)
                sbas = multbas_list(ik,iml(1))
                iml = MINLOC(tmp_area(1:multbas_sz(ik)), MASK = tmp_area(1:multbas_sz(ik)) .GT. zero)
                kbas = multbas_list(ik,iml(1))
                !
             ENDIF
             !
          ENDIF
          !
          !   4) If we have twice the same basin we put them together even if they flow into different
          !   directions. If one of them goes to the ocean it takes the advantage.
          !
          IF ( kbas*sbas .EQ. 0) THEN
             !
             tmp_ids(1:basin_count(ib)) = basin_id(ib,1:basin_count(ib))
             multbas = 0
             multbas_sz(:) = 0
             !
             ! First obtain the list of basins which have sub-basins in this grid box.
             ! (these are identified by their IDs)
             !
             DO ij=1,basin_count(ib)
                IF ( COUNT(tmp_ids(1:basin_count(ib)) .EQ. basin_id(ib,ij)) .GT. 1) THEN
                   multbas = multbas + 1
                   DO ii=1,basin_count(ib)
                      IF ( tmp_ids(ii) .EQ. basin_id(ib,ij)) THEN
                         multbas_sz(multbas) = multbas_sz(multbas) + 1
                         multbas_list(multbas,multbas_sz(multbas)) = ii
                         tmp_ids(ii) = -99
                      ENDIF
                   ENDDO
                ELSE
                   tmp_ids(ij) = -99
                ENDIF
             ENDDO
             !
             ! We are going to work on the basin with the largest number of sub-basins.
             ! (IF we have a basin which has subbasins !)
             !
             IF ( multbas .GT. 0 ) THEN
                !
                iml = MAXLOC(multbas_sz(1:multbas))
                ik = iml(1)
                !
                ! If one of the basins goes to the ocean then it is going to have the priority
                !
                tmp_area(:) = zero
                IF ( COUNT(outflow_grid(ib,multbas_list(ik,1:multbas_sz(ik))) .LT. 0) .GT. 0) THEN
                   DO ii=1,multbas_sz(ik)
                      IF ( outflow_grid(ib,multbas_list(ik,ii)) .LT. 0 .AND. sbas .EQ. 0 ) THEN
                         sbas = multbas_list(ik,ii)
                      ELSE
                         tmp_area(ii) = fetch_basin(ib, multbas_list(ik,ii))
                      ENDIF
                   ENDDO
                   ! take the smallest of the subbasins
                   iml = MINLOC(tmp_area(1:multbas_sz(ik)), MASK = tmp_area(1:multbas_sz(ik)) .GT. zero)
                   kbas = multbas_list(ik,iml(1))
                ELSE
                   !
                   ! Else we take simply the largest and smallest
                   !
                   DO ii=1,multbas_sz(ik)
                      tmp_area(ii) = fetch_basin(ib, multbas_list(ik,ii))
                   ENDDO
                   iml = MAXLOC(tmp_area(1:multbas_sz(ik)), MASK = tmp_area(1:multbas_sz(ik)) .GT. zero)
                   sbas = multbas_list(ik,iml(1))
                   iml = MINLOC(tmp_area(1:multbas_sz(ik)), MASK = tmp_area(1:multbas_sz(ik)) .GT. zero)
                   kbas = multbas_list(ik,iml(1))
                   !
                ENDIF
                !
                !
             ENDIF
          ENDIF
          !
          !
          !
          ! Then we call routing_killbas to clean up the basins in this grid
          !
          IF ( kbas .GT. 0 .AND. sbas .GT. 0 ) THEN
             CALL routing_killbas(nbpt, ib, kbas, sbas, nwbas, basin_count, basin_area, basin_topoind,&
                  & fetch_basin, basin_id, basin_flowdir, outflow_grid, outflow_basin, inflow_number,&
                  & inflow_grid, inflow_basin)
          ENDIF
          !
       ENDDO
       !
       !      
    ENDDO
    !
    ! If there are any grids left with too many basins we need to take out the big hammer !
    ! We will only do it if this represents less than 5% of all points.
    !
    IF ( COUNT(basin_count .GT. nbasmax) .GT. 0 ) THEN
       !
       !
       IF ( COUNT(basin_count .GT. nbasmax)/nbpt*100 .GT. 5 ) THEN
          WRITE(numout,*) 'We have ', COUNT(basin_count .GT. nbasmax)/nbpt*100, '% of all points which do not yet'
          WRITE(numout,*) 'have the right trunctaction. That is too much to apply a brutal method'
          DO ib = 1, nbpt
             IF ( basin_count(ib) .GT. nbasmax ) THEN
                !
                WRITE(numout,*) 'We did not find a basin which could be supressed. We will'
                WRITE(numout,*) 'not be able to reduce the truncation in grid ', ib
                DO ij=1,basin_count(ib)
                   WRITE(numout,*) 'grid, basin nb and id :', ib, ij, basin_id(ib,ij)
                   WRITE(numout,*) 'Outflow grid and basin ->', outflow_grid(ib,ij), outflow_basin(ib, ij)
                ENDDO
             ENDIF
          ENDDO
          CALL ipslerr_p(3,'routing_truncate','No basin found which could be supressed.','','')
       ELSE
          !
          !
          DO ib = 1,nbpt
             DO WHILE ( basin_count(ib) .GT. nbasmax )
                !
                WRITE(numout,*) 'HAMMER, ib, basin_count :', ib, basin_count(ib)
                !
                ! Here we simply put the smallest basins into the largest ones. It is really a brute force
                ! method but it will only be applied if everything has failed.
                !
                DO ii = 1,basin_count(ib)
                   tmp_area(ii) = fetch_basin(ib, ii)
                ENDDO
                !
                iml = MAXLOC(tmp_area(1:basin_count(ib)), MASK = tmp_area(1:basin_count(ib)) .GT. 0.)
                sbas =iml(1)
                iml = MINLOC(tmp_area(1:basin_count(ib)), MASK = tmp_area(1:basin_count(ib)) .GT. 0.)
                kbas = iml(1)
                !
                IF ( kbas .GT. 0 .AND. sbas .GT. 0 ) THEN
                   CALL routing_killbas(nbpt, ib, kbas, sbas, nwbas, basin_count, basin_area, basin_topoind,&
                        & fetch_basin, basin_id, basin_flowdir, outflow_grid, outflow_basin, inflow_number,&
                        & inflow_grid, inflow_basin)
                ENDIF
             ENDDO
          ENDDO
          !
       ENDIF
       !
       !
    ENDIF
    !
    ! Now that we have reached the right truncation (resolution) we will start
    ! to produce the variables we will use to route the water.
    !
    DO ib=1,nbpt
       !
       ! For non existing basins the route_tobasin variable is put to zero. This will allow us
       ! to pick up the number of basin afterwards.
       !
       route_togrid(ib,:) = ib
       route_tobasin(ib,:) = 0
       routing_area(ib,:) = zero
       !
    ENDDO
    !
    ! Transfer the info into the definitive variables
    !
    DO ib=1,nbpt
       DO ij=1,basin_count(ib)
          routing_area(ib,ij) = basin_area(ib,ij)
          topo_resid(ib,ij) = basin_topoind(ib,ij)
          global_basinid(ib,ij) = basin_id(ib,ij)
          route_togrid(ib,ij) = outflow_grid(ib,ij)
          route_tobasin(ib,ij) = outflow_basin(ib,ij)
       ENDDO
    ENDDO
    !
    !
    ! Set the new convention for the outflow conditions
    ! Now it is based in the outflow basin and the outflow grid will
    ! be the same as the current.
    ! returnflow to the grid : nbasmax + 1
    ! coastal flow           : nbasmax + 2
    ! river outflow          : nbasmax + 3
    !
    ! Here we put everything here in coastal flow. It is later where we will
    ! put the largest basins into river outflow.
    !
    DO ib=1,nbpt
       DO ij=1,basin_count(ib)
          ! River flows
          IF ( route_togrid(ib,ij) .EQ. -1 ) THEN
             route_tobasin(ib,ij) = nbasmax + 2
             route_togrid(ib,ij) = ib
          ! Coastal flows
          ELSE IF ( route_togrid(ib,ij) .EQ. -2 ) THEN
             route_tobasin(ib,ij) = nbasmax + 2
             route_togrid(ib,ij) = ib
          ! Return flow
          ELSE IF ( route_togrid(ib,ij) .EQ. -3 ) THEN
             route_tobasin(ib,ij) = nbasmax + 1
             route_togrid(ib,ij) = ib
          ENDIF
       ENDDO
    ENDDO
    !
    ! A second check on the data. Just make sure that each basin flows somewhere.
    !
    DO ib=1,nbpt
       DO ij=1,basin_count(ib)
          ibf = route_togrid(ib,ij)
          ijf = route_tobasin(ib,ij)
          IF ( ijf .GT. basin_count(ibf) .AND.  ijf .LE. nbasmax) THEN
             WRITE(numout,*) 'Second check'
             WRITE(numout,*) 'point :', ib, ' basin :', ij
             WRITE(numout,*) 'Flows into point :', ibf, ' basin :', ijf
             WRITE(numout,*) 'But it flows into now here as basin count = ', basin_count(ibf)
             CALL ipslerr_p(3,'routing_truncate','Problem with routing..','','')
          ENDIF
       ENDDO
    ENDDO
    !
    ! Verify areas of the continents
    !
    floflo(:,:) = zero
    gridarea(:) = contfrac(:)*resolution(:,1)*resolution(:,2)
    DO ib=1,nbpt
       gridbasinarea(ib) = SUM(routing_area(ib,:))
    ENDDO
    !
    DO ib=1,nbpt
       DO ij=1,basin_count(ib)
          cnt = 0
          igrif = ib
          ibasf = ij
          DO WHILE (ibasf .LE. nbasmax .AND. cnt .LT. nbasmax*nbpt)
             cnt = cnt + 1
             pold = igrif
             bold = ibasf
             igrif = route_togrid(pold, bold)
             ibasf = route_tobasin(pold, bold)
             IF ( ibasf .GT. basin_count(igrif)  .AND.  ibasf .LE. nbasmax) THEN
                WRITE(numout,*) 'We should not be here as the basin flows into the pampa'
                WRITE(numout,*) 'Last correct point :', pold, bold
                WRITE(numout,*) 'It pointed to in the new variables :', route_togrid(pold, bold),route_tobasin(pold, bold) 
                WRITE(numout,*) 'The old variables gave :', outflow_grid(pold, bold), outflow_basin(pold, bold) 
                WRITE(numout,*) 'Where we ended up :', igrif,ibasf
                CALL ipslerr_p(3,'routing_truncate','Problem with routing..','','')
             ENDIF
          ENDDO
          !
          IF ( ibasf .GT. nbasmax ) THEN
             floflo(igrif,bold) = floflo(igrif,bold) + routing_area(ib,ij)
          ELSE
             WRITE(numout,*) 'The flow did not end up in the ocean or in the grid cell.'
             WRITE(numout,*) 'For grid ', ib, ' and basin ', ij
             WRITE(numout,*) 'The last grid was ', igrif, ' and basin ', ibasf
             CALL ipslerr_p(3,'routing_truncate','Problem with routing..','','')
          ENDIF
       ENDDO
    ENDDO
    !
    DO ib=1,nbpt
       IF ( gridbasinarea(ib) > zero ) THEN
          ratio = gridarea(ib)/gridbasinarea(ib)
          routing_area(ib,:) = routing_area(ib,:)*ratio
       ELSE
          WRITE(numout,*) 'gridbasinarea(ib) <= zero. We should stop here :', ib
       ENDIF
    ENDDO
    !
    WRITE(numout,*) 'Sum of area of all outflow areas :',SUM(routing_area)
    WRITE(numout,*) 'Surface of all continents :', SUM(gridarea)
    !
    ! Redo the the distinction between river outflow and coastal flow. We can not
    ! take into account the return flow points.
    !
    ibf = 0
    DO ib=1, pickmax
       ff = MAXLOC(floflo)
       ! tdo - To take into account rivers that do not flow to the oceans 
       IF ( route_tobasin(ff(1), ff(2)) .GT. nbasmax ) THEN
!       IF ( route_tobasin(ff(1), ff(2)) .EQ. nbasmax + 2) THEN
          ibf = ibf + 1
          largest_basins(ibf,:) = ff(:)
       ENDIF
       floflo(ff(1), ff(2)) = zero
    ENDDO
    !
    ! Put the largest basins into river flows.
    !
    IF ( ibf .LT.  num_largest) THEN
       WRITE(numout,*) 'Not enough basins to choose the ',  num_largest, 'largest'
       CALL ipslerr_p(3,'routing_truncate','Not enough basins','','')
    ENDIF
    !
    !
    !
    DO ib=1, num_largest
       route_tobasin(largest_basins(ib,1),largest_basins(ib,2)) = nbasmax + 3
    ENDDO
    !
    WRITE(numout,*) 'NUMBER OF RIVERS :', COUNT(route_tobasin .GE. nbasmax + 3)
    !
  END SUBROUTINE  routing_truncate
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_killbas
!!
!>\BRIEF        The aim of this subroutine is to kill a basin (that is put into another larger one).
!!              When we do this we need to be careful and change all associated variables.  
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_killbas(nbpt, ib, tokill, totakeover, nwbas, basin_count, basin_area, basin_topoind,&
       & fetch_basin, basin_id, basin_flowdir, outflow_grid, outflow_basin, inflow_number,&
       & inflow_grid, inflow_basin)
    !
    !
    IMPLICIT NONE
    !
    INTEGER(i_std)                              :: tokill        !!
    INTEGER(i_std)                              :: totakeover    !!
    INTEGER(i_std)                              :: nbpt          !! Domain size  (unitless)
    INTEGER(i_std)                              :: ib            !! Current basin (unitless)
    !
    INTEGER(i_std)                              :: nwbas         !!
    INTEGER(i_std), DIMENSION(nbpt)             :: basin_count   !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)       :: basin_id      !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)       :: basin_flowdir !! Water flow directions in the basin (unitless)
    REAL(r_std), DIMENSION(nbpt,nwbas)          :: basin_area    !!
    REAL(r_std), DIMENSION(nbpt,nwbas)          :: basin_topoind !! Topographic index of the residence time for a basin (m)
    REAL(r_std), DIMENSION(nbpt,nwbas)          :: fetch_basin   !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)       :: outflow_grid  !! Type of outflow on the grid box (unitless)
    INTEGER(i_std), DIMENSION(nbpt,nwbas)       :: outflow_basin !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas)       :: inflow_number !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas,nwbas) :: inflow_basin  !!
    INTEGER(i_std), DIMENSION(nbpt,nwbas,nwbas) :: inflow_grid   !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                              :: inf, ibs, ing, inb, ibasf, igrif, it !! Indices (unitless)
    LOGICAL                                     :: doshift       !! (true/false)

!_ ================================================================================================================================
    !
    ! Update the information needed in the basin "totakeover"
    ! For the moment only area
    !
    WRITE(numout,*) 'KILL BASIN :', ib, tokill, totakeover, basin_id(ib,tokill), basin_id(ib,totakeover)
    !
    !
    basin_area(ib, totakeover) = basin_area(ib, totakeover) +  basin_area(ib, tokill)
    basin_topoind(ib, totakeover) = (basin_topoind(ib, totakeover) + basin_topoind(ib, tokill))/2.0
    !
    ! Add the fetch of the basin will kill to the one which gets the water
    !
    fetch_basin(ib, totakeover) = fetch_basin(ib, totakeover) + fetch_basin(ib, tokill)
    igrif = outflow_grid(ib,totakeover)
    ibasf = outflow_basin(ib,totakeover)
    !
    inf = 0
    DO WHILE (igrif .GT. 0)
       fetch_basin(igrif,ibasf) =  fetch_basin(igrif,ibasf) + fetch_basin(ib, tokill) 
       it = outflow_grid(igrif, ibasf)
       ibasf = outflow_basin(igrif, ibasf)
       igrif = it
       inf = inf + 1
    ENDDO
    !
    ! Take out the basin we have just rerouted from the fetch of the basins in which it used to flow.
    !
    igrif = outflow_grid(ib,tokill)
    ibasf = outflow_basin(ib,tokill)
    !
    DO WHILE (igrif .GT. 0)
       fetch_basin(igrif,ibasf) =  fetch_basin(igrif,ibasf) - fetch_basin(ib, tokill)
       it = outflow_grid(igrif, ibasf)
       ibasf = outflow_basin(igrif, ibasf)
       igrif = it
    ENDDO   
    !
    !  Redirect the flows which went into the basin to be killed before we change everything
    !
    DO inf = 1, inflow_number(ib, tokill)
       outflow_basin(inflow_grid(ib, tokill, inf), inflow_basin(ib, tokill, inf)) = totakeover
       inflow_number(ib, totakeover) = inflow_number(ib, totakeover) + 1
       inflow_grid(ib, totakeover,  inflow_number(ib, totakeover)) = inflow_grid(ib, tokill, inf)
       inflow_basin(ib, totakeover,  inflow_number(ib, totakeover)) = inflow_basin(ib, tokill, inf)
    ENDDO
    !
    ! Take out the basin to be killed from the list of inflow basins of the downstream basin
    ! (In case the basin does not flow into an ocean or lake)
    !
    IF ( outflow_grid(ib,tokill) .GT. 0) THEN
       !
       ing = outflow_grid(ib, tokill)
       inb = outflow_basin(ib, tokill)
       doshift = .FALSE.
       !
       DO inf = 1, inflow_number(ing, inb)
          IF ( doshift ) THEN
             inflow_grid(ing, inb, inf-1) = inflow_grid(ing, inb, inf)
             inflow_basin(ing, inb, inf-1) = inflow_basin(ing, inb, inf)
          ENDIF
          IF ( inflow_grid(ing, inb, inf) .EQ. ib .AND. inflow_basin(ing, inb, inf) .EQ. tokill) THEN
             doshift = .TRUE.
          ENDIF
       ENDDO
       !
       ! This is only to allow for the last check
       !
       inf = inflow_number(ing, inb)
       IF ( inflow_grid(ing, inb, inf) .EQ. ib .AND. inflow_basin(ing, inb, inf) .EQ. tokill) THEN
          doshift = .TRUE.
       ENDIF
       !
       IF ( .NOT. doshift ) THEN
          WRITE(numout,*) 'Strange we did not find the basin to kill in the downstream basin'
          CALL ipslerr_p(3,'routing_killbas','Basin not found','','')
       ENDIF
       inflow_number(ing, inb) = inflow_number(ing, inb) - 1
       
    ENDIF
    !
    ! Now remove from the arrays the information of basin "tokill"
    !
    basin_id(ib, tokill:basin_count(ib)-1) = basin_id(ib, tokill+1:basin_count(ib))
    basin_flowdir(ib, tokill:basin_count(ib)-1) = basin_flowdir(ib, tokill+1:basin_count(ib))
    basin_area(ib, tokill:basin_count(ib)-1) = basin_area(ib, tokill+1:basin_count(ib))
    basin_area(ib, basin_count(ib):nwbas) = zero
    basin_topoind(ib, tokill:basin_count(ib)-1) = basin_topoind(ib, tokill+1:basin_count(ib))
    basin_topoind(ib, basin_count(ib):nwbas) = zero
    fetch_basin(ib, tokill:basin_count(ib)-1) = fetch_basin(ib, tokill+1:basin_count(ib))
    fetch_basin(ib, basin_count(ib):nwbas) = zero
    !
    ! Before we remove the information from the outflow fields we have to correct the corresponding inflow fields
    ! of the grids into which the flow goes
    !
    DO ibs = tokill+1,basin_count(ib)
       ing = outflow_grid(ib, ibs)
       inb = outflow_basin(ib, ibs)
       IF ( ing .GT. 0 ) THEN
          DO inf = 1, inflow_number(ing, inb)
             IF ( inflow_grid(ing,inb,inf) .EQ. ib .AND. inflow_basin(ing,inb,inf) .EQ. ibs) THEN
                inflow_basin(ing,inb,inf) = ibs - 1
             ENDIF
          ENDDO
       ENDIF
    ENDDO
    outflow_grid(ib, tokill:basin_count(ib)-1) = outflow_grid(ib, tokill+1:basin_count(ib))
    outflow_basin(ib, tokill:basin_count(ib)-1) = outflow_basin(ib, tokill+1:basin_count(ib))
    !
    ! Basins which moved down also need to redirect their incoming flows.
    !
    DO ibs=tokill+1, basin_count(ib)
       DO inf = 1, inflow_number(ib, ibs)
          outflow_basin(inflow_grid(ib, ibs, inf), inflow_basin(ib, ibs, inf)) = ibs-1
       ENDDO
    ENDDO
    !
    ! Shift the inflow basins
    !
    DO it = tokill+1,basin_count(ib)
       inflow_grid(ib, it-1, 1:inflow_number(ib,it)) =  inflow_grid(ib, it, 1:inflow_number(ib,it))
       inflow_basin(ib, it-1, 1:inflow_number(ib,it)) =  inflow_basin(ib, it, 1:inflow_number(ib,it))
       inflow_number(ib,it-1) = inflow_number(ib,it)
    ENDDO
    !
    basin_count(ib) = basin_count(ib) - 1
    !
  END SUBROUTINE routing_killbas 
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_names
!!
!>\BRIEF         This subroutine lists the name of the largest basins which are explicitly listed in the basin
!!               description file used by ORCHIDEE.
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_names(numlar, basin_names)
    !
    IMPLICIT NONE
    !
    ! Arguments
    !
    INTEGER(i_std), INTENT(in)             :: numlar              !!
    CHARACTER(LEN=*), INTENT(inout)        :: basin_names(numlar) !! Name of the basins (unitless)
!! PARAMETERS
    INTEGER(i_std), PARAMETER              :: listleng=349        !!
    !
!! LOCAL VARIABLES
    INTEGER(i_std)                         :: lenstr, i           !!
    CHARACTER(LEN=60), DIMENSION(listleng) :: list_names          !!
    CHARACTER(LEN=60)                      :: tmp_str             !!

!_ ================================================================================================================================
    !

    lenstr = LEN(basin_names(1))
    !
    list_names(1) = "Amazon"
    list_names(2) = "Nile"
    list_names(3) = "Zaire"
    list_names(4) = "Mississippi"
    list_names(5) = "Amur"
    list_names(6) = "Parana"
    list_names(7) = "Yenisei"
    list_names(8) = "Ob"
    list_names(9) = "Lena"
    list_names(10) = "Niger"
    list_names(11) = "Zambezi"
    list_names(12) = "Erg Iguidi (Sahara)"
    list_names(13) = "Chang Jiang (Yangtze)"
    list_names(14) = "Mackenzie"
    list_names(15) = "Ganges"
    list_names(16) = "Chari"
    list_names(17) = "Volga"
    list_names(18) = "St. Lawrence"
    list_names(19) = "Indus"
    list_names(20) = "Syr-Darya"
    list_names(21) = "Nelson"
    list_names(22) = "Orinoco"
    list_names(23) = "Murray"
    list_names(24) = "Great Artesian Basin"
    list_names(25) = "Shatt el Arab"
    list_names(26) = "Orange"
    list_names(27) = "Huang He"
    list_names(28) = "Yukon"
    list_names(29) = "Senegal"
    list_names(30) = "Chott Jerid"
    list_names(31) = "Jubba"
    list_names(32) = "Colorado (Ari)"
    list_names(33) = "Rio Grande (US)"
    list_names(34) = "Danube"
    list_names(35) = "Mekong"
    list_names(36) = "Tocantins"
    list_names(37) = "Wadi al Farigh"
    list_names(38) = "Tarim"
    list_names(39) = "Columbia"
    list_names(40) = "Komadugu Yobe (Tchad)"
    list_names(41) = "Kolyma"
    list_names(42) = "Sao Francisco"
    list_names(43) = "Amu-Darya"
    list_names(44) = "GHAASBasin51"
    list_names(45) = "Dnepr"
    list_names(46) = "GHAASBasin61"
    list_names(47) = "Don"
    list_names(48) = "Colorado (Arg)"
    list_names(49) = "Limpopo"
    list_names(50) = "GHAASBasin50"
    list_names(51) = "Zhujiang"
    list_names(52) = "Irrawaddy"
    list_names(53) = "Volta"
    list_names(54) = "GHAASBasin54"
    list_names(55) = "Farah"
    list_names(56) = "Khatanga"
    list_names(57) = "Dvina"
    list_names(58) = "Urugay"
    list_names(59) = "Qarqan"
    list_names(60) = "GHAASBasin75"
    list_names(61) = "Parnaiba"
    list_names(62) = "GHAASBasin73"
    list_names(63) = "Indigirka"
    list_names(64) = "Churchill (Hud)"
    list_names(65) = "Godavari"
    list_names(66) = "Pur - Taz"
    list_names(67) = "Pechora"
    list_names(68) = "Baker"
    list_names(69) = "Ural"
    list_names(70) = "Neva"
    list_names(71) = "Liao"
    list_names(72) = "Salween"
    list_names(73) = "GHAASBasin73"
    list_names(74) = "Jordan"
    list_names(75) = "GHAASBasin78"
    list_names(76) = "Magdalena"
    list_names(77) = "Krishna"
    list_names(78) = "Salado"
    list_names(79) = "Fraser"
    list_names(80) = "Hai Ho"
    list_names(81) = "Huai"
    list_names(82) = "Yana"
    list_names(83) = "GHAASBasin95"
    list_names(84) = "GHAASBasin105"
    list_names(85) = "Kura"
    list_names(86) = "Olenek"
    list_names(87) = "Ogooue"
    list_names(88) = "Taymyr"
    list_names(89) = "Negro Arg"
    list_names(90) = "Chubut"
    list_names(91) = "GHAASBasin91"
    list_names(92) = "GHAASBasin122"
    list_names(93) = "GHAASBasin120"
    list_names(94) = "Sacramento"
    list_names(95) = "Fitzroy West"
    list_names(96) = "Grande de Santiago"
    list_names(97) = "Rufiji"
    list_names(98) = "Wisla"
    list_names(99) = "GHAASBasin47"
    list_names(100) = "GHAASBasin127"
    list_names(101) = "Hong"
    list_names(102) = "GHAASBasin97"
    list_names(103) = "Swan-Avon"
    list_names(104) = "Rhine"
    list_names(105) = "Cuanza"
    list_names(106) = "GHAASBasin106"
    list_names(107) = "GHAASBasin142"
    list_names(108) = "Roviuna"
    list_names(109) = "Essequibo"
    list_names(110) = "Elbe"
    list_names(111) = "Koksoak"
    list_names(112) = "Chao Phraya"
    list_names(113) = "Brahmani"
    list_names(114) = "GHAASBasin165"
    list_names(115) = "Pyasina"
    list_names(116) = "Fitzroy East"
    list_names(117) = "GHAASBasin173"
    list_names(118) = "Albany"
    list_names(119) = "Sanaga"
    list_names(120) = "GHAASBasin120"
    list_names(121) = "GHAASBasin178"
    list_names(122) = "GHAASBasin148"
    list_names(123) = "Brazos (Tex)"
    list_names(124) = "GHAASBasin124"
    list_names(125) = "Alabama"
    list_names(126) = "GHAASBasin174"
    list_names(127) = "GHAASBasin179"
    list_names(128) = "Balsas"
    list_names(129) = "GHAASBasin172"
    list_names(130) = "Burdekin"
    list_names(131) = "Colorado (Texas)"
    list_names(132) = "GHAASBasin150"
    list_names(133) = "Odra"
    list_names(134) = "Loire"
    list_names(135) = "GHAASBasin98"
    list_names(136) = "Galana"
    list_names(137) = "Kuskowin"
    list_names(138) = "Moose"
    list_names(139) = "Narmada"
    list_names(140) = "GHAASBasin140"
    list_names(141) = "GHAASBasin141"
    list_names(142) = "Flinders"
    list_names(143) = "Kizil Irmak"
    list_names(144) = "GHAASBasin144"
    list_names(145) = "Save"
    list_names(146) = "Roper"
    list_names(147) = "Churchill (Atlantic)"
    list_names(148) = "GHAASBasin148"
    list_names(149) = "Victoria"
    list_names(150) = "Back"
    list_names(151) = "Bandama"
    list_names(152) = "Severn (Can)"
    list_names(153) = "Po"
    list_names(154) = "GHAASBasin154"
    list_names(155) = "GHAASBasin155"
    list_names(156) = "GHAASBasin156"
    list_names(157) = "Rhone"
    list_names(158) = "Tana (Ken)"
    list_names(159) = "La Grande"
    list_names(160) = "GHAASBasin160"
    list_names(161) = "Cunene"
    list_names(162) = "Douro"
    list_names(163) = "GHAASBasin163"
    list_names(164) = "Nemanus"
    list_names(165) = "GHAASBasin165"
    list_names(166) = "Anabar"
    list_names(167) = "Hayes"
    list_names(168) = "Mearim"
    list_names(169) = "GHAASBasin169"
    list_names(170) = "Panuco"
    list_names(171) = "GHAASBasin171"
    list_names(172) = "Doce"
    list_names(173) = "Gasgoyne"
    list_names(174) = "GHAASBasin174"
    list_names(175) = "GHAASBasin175"
    list_names(176) = "Ashburton"
    list_names(177) = "GHAASBasin177"
    list_names(178) = "Peel"
    list_names(179) = "Daugava"
    list_names(180) = "GHAASBasin180"
    list_names(181) = "Ebro"
    list_names(182) = "Comoe"
    list_names(183) = "Jacui"
    list_names(184) = "GHAASBasin184"
    list_names(185) = "Kapuas"
    list_names(186) = "GHAASBasin186"
    list_names(187) = "Penzhina"
    list_names(188) = "Cauweri"
    list_names(189) = "GHAASBasin189"
    list_names(190) = "Mamberamo"
    list_names(191) = "Sepik"
    list_names(192) = "GHAASBasin192"
    list_names(193) = "Sassandra"
    list_names(194) = "GHAASBasin194"
    list_names(195) = "GHAASBasin195"
    list_names(196) = "Nottaway"
    list_names(197) = "Barito"
    list_names(198) = "GHAASBasin198"
    list_names(199) = "Seine"
    list_names(200) = "Tejo"
    list_names(201) = "GHAASBasin201"
    list_names(202) = "Gambia"
    list_names(203) = "Susquehanna"
    list_names(204) = "Dnestr"
    list_names(205) = "Murchinson"
    list_names(206) = "Deseado"
    list_names(207) = "Mitchell"
    list_names(208) = "Mahakam"
    list_names(209) = "GHAASBasin209"
    list_names(210) = "Pangani"
    list_names(211) = "GHAASBasin211"
    list_names(212) = "GHAASBasin212"
    list_names(213) = "GHAASBasin213"
    list_names(214) = "GHAASBasin214"
    list_names(215) = "GHAASBasin215"
    list_names(216) = "Bug"
    list_names(217) = "GHAASBasin217"
    list_names(218) = "Usumacinta"
    list_names(219) = "Jequitinhonha"
    list_names(220) = "GHAASBasin220"
    list_names(221) = "Corantijn"
    list_names(222) = "Fuchun Jiang"
    list_names(223) = "Copper"
    list_names(224) = "Tapti"
    list_names(225) = "Menjiang"
    list_names(226) = "Karun"
    list_names(227) = "Mezen"
    list_names(228) = "Guadiana"
    list_names(229) = "Maroni"
    list_names(230) = "GHAASBasin230"
    list_names(231) = "Uda"
    list_names(232) = "GHAASBasin232"
    list_names(233) = "Kuban"
    list_names(234) = "Colville"
    list_names(235) = "Thaane"
    list_names(236) = "Alazeya"
    list_names(237) = "Paraiba do Sul"
    list_names(238) = "GHAASBasin238"
    list_names(239) = "Fortesque"
    list_names(240) = "GHAASBasin240"
    list_names(241) = "GHAASBasin241"
    list_names(242) = "Winisk"
    list_names(243) = "GHAASBasin243"
    list_names(244) = "GHAASBasin244"
    list_names(245) = "Ikopa"
    list_names(246) = "Gilbert"
    list_names(247) = "Kouilou"
    list_names(248) = "Fly"
    list_names(249) = "GHAASBasin249"
    list_names(250) = "GHAASBasin250"
    list_names(251) = "GHAASBasin251"
    list_names(252) = "Mangoky"
    list_names(253) = "Damodar"
    list_names(254) = "Onega"
    list_names(255) = "Moulouya"
    list_names(256) = "GHAASBasin256"
    list_names(257) = "Ord"
    list_names(258) = "GHAASBasin258"
    list_names(259) = "GHAASBasin259"
    list_names(260) = "GHAASBasin260"
    list_names(261) = "GHAASBasin261"
    list_names(262) = "Narva"
    list_names(263) = "GHAASBasin263"
    list_names(264) = "Seal"
    list_names(265) = "Cheliff"
    list_names(266) = "Garonne"
    list_names(267) = "Rupert"
    list_names(268) = "GHAASBasin268"
    list_names(269) = "Brahmani"
    list_names(270) = "Sakarya"
    list_names(271) = "Gourits"
    list_names(272) = "Sittang"
    list_names(273) = "Rajang"
    list_names(274) = "Evros"
    list_names(275) = "Appalachicola"
    list_names(276) = "Attawapiskat"
    list_names(277) = "Lurio"
    list_names(278) = "Daly"
    list_names(279) = "Penner"
    list_names(280) = "GHAASBasin280"
    list_names(281) = "GHAASBasin281"
    list_names(282) = "Guadalquivir"
    list_names(283) = "Nadym"
    list_names(284) = "GHAASBasin284"
    list_names(285) = "Saint John"
    list_names(286) = "GHAASBasin286"
    list_names(287) = "Cross"
    list_names(288) = "Omoloy"
    list_names(289) = "Oueme"
    list_names(290) = "GHAASBasin290"
    list_names(291) = "Gota"
    list_names(292) = "Nueces"
    list_names(293) = "Stikine"
    list_names(294) = "Yalu"
    list_names(295) = "Arnaud"
    list_names(296) = "GHAASBasin296"
    list_names(297) = "Jequitinhonha"
    list_names(298) = "Kamchatka"
    list_names(299) = "GHAASBasin299"
    list_names(300) = "Grijalva"
    list_names(301) = "GHAASBasin301"
    list_names(302) = "Kemijoki"
    list_names(303) = "Olifants"
    list_names(304) = "GHAASBasin304"
    list_names(305) = "Tsiribihina"
    list_names(306) = "Coppermine"
    list_names(307) = "GHAASBasin307"
    list_names(308) = "GHAASBasin308"
    list_names(309) = "Kovda"
    list_names(310) = "Trinity"
    list_names(311) = "Glama"
    list_names(312) = "GHAASBasin312"
    list_names(313) = "Luan"
    list_names(314) = "Leichhardt"
    list_names(315) = "GHAASBasin315"
    list_names(316) = "Gurupi"
    list_names(317) = "GR Baleine"
    list_names(318) = "Aux Feuilles"
    list_names(319) = "GHAASBasin319"
    list_names(320) = "Weser"
    list_names(321) = "GHAASBasin321"
    list_names(322) = "GHAASBasin322"
    list_names(323) = "Yesil"
    list_names(324) = "Incomati"
    list_names(325) = "GHAASBasin325"
    list_names(326) = "GHAASBasin326"
    list_names(327) = "Pungoe"
    list_names(328) = "GHAASBasin328"
    list_names(329) = "Meuse"
    list_names(330) = "Eastmain"
    list_names(331) = "Araguari"
    list_names(332) = "Hudson"
    list_names(333) = "GHAASBasin333"
    list_names(334) = "GHAASBasin334"
    list_names(335) = "GHAASBasin335"
    list_names(336) = "GHAASBasin336"
    list_names(337) = "Kobuk"
    list_names(338) = "Altamaha"
    list_names(339) = "GHAASBasin339"
    list_names(340) = "Mand"
    list_names(341) = "Santee"
    list_names(342) = "GHAASBasin342"
    list_names(343) = "GHAASBasin343"
    list_names(344) = "GHAASBasin344"
    list_names(345) = "Hari"
    list_names(346) = "GHAASBasin346"
    list_names(347) = "Wami"
    list_names(348) = "GHAASBasin348"
    list_names(349) = "GHAASBasin349"
    !
    basin_names(:) = '    '
    !
    DO i=1,numlar
       tmp_str = list_names(i)
       basin_names(i) = tmp_str(1:MIN(lenstr,LEN_TRIM(tmp_str)))
    ENDDO
    !
  END SUBROUTINE routing_names
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_irrigmap
!!
!>\BRIEF         This  subroutine interpolates the 0.5x0.5 degree based map of irrigated areas to the resolution of the model.
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_irrigmap (nbpt, index, lalo, neighbours, resolution, contfrac, &
       &                       init_irrig, irrigated, init_flood, floodplains, init_swamp, swamp, hist_id, hist2_id)
    !
    IMPLICIT NONE
    !
!! PARAMETERS
    INTEGER(i_std), PARAMETER                      :: ilake = 1             !! Number of type of lakes area (unitless)
    INTEGER(i_std), PARAMETER                      :: idam = 2              !! Number of type of dams area (unitless)
    INTEGER(i_std), PARAMETER                      :: iflood = 3            !! Number of type of floodplains area (unitless)
    INTEGER(i_std), PARAMETER                      :: iswamp = 4            !! Number of type of swamps area (unitless)
    INTEGER(i_std), PARAMETER                      :: isal = 5              !! Number of type of salines area (unitless)
    INTEGER(i_std), PARAMETER                      :: ipond = 6             !! Number of type of ponds area (unitless)
    INTEGER(i_std), PARAMETER                      :: ntype = 6             !! Number of types of flooded surfaces (unitless)

!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in)                     :: nbpt                  !! Domain size  (unitless)
    INTEGER(i_std), INTENT(in)                     :: index(nbpt)           !! Index on the global map.
    REAL(r_std), INTENT(in)                        :: lalo(nbpt,2)          !! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)                     :: neighbours(nbpt,8)    !! Vector of neighbours for each grid point (1=N, 2=E, 3=S, 4=W)
    REAL(r_std), INTENT(in)                        :: resolution(nbpt,2)    !! The size of each grid box in X and Y (m)
    REAL(r_std), INTENT(in)                        :: contfrac(nbpt)        !! Fraction of land in each grid box (unitless;0-1)
    INTEGER(i_std), INTENT(in)                     :: hist_id               !! Access to history file (unitless)
    INTEGER(i_std), INTENT(in)                     :: hist2_id              !! Access to history file 2 (unitless)
    LOGICAL, INTENT(in)                            :: init_irrig            !! Logical to initialize the irrigation (true/false)
    LOGICAL, INTENT(in)                            :: init_flood            !! Logical to initialize the floodplains (true/false)
    LOGICAL, INTENT(in)                            :: init_swamp            !! Logical to initialize the swamps (true/false)
    !
!! OUTPUT VARIABLES
    REAL(r_std), INTENT(out)                       :: irrigated(:)          !! Irrigated surface in each grid box (m^2)
    REAL(r_std), INTENT(out)                       :: floodplains(:)        !! Surface which can be inundated in each grid box (m^2)
    REAL(r_std), INTENT(out)                       :: swamp(:)              !! Surface which can be swamp in each grid box (m^2)
    !
!! LOCAL VARIABLES
    ! Interpolation variables
    ! 
    INTEGER(i_std)                                 :: nbpmax, nix, njx, fopt !!
    CHARACTER(LEN=30)                              :: callsign              !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:)     :: resol_lu              !! Resolution read on the map
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)    :: mask                  !! Mask to exclude some points (unitless)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)       :: irrsub_area           !! Area on the fine grid (m^2)
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:)  :: irrsub_index          !! Indices of the points we need on the fine grid (unitless)
    INTEGER                                        :: ALLOC_ERR             !!
    LOGICAL                                        :: ok_interpol = .FALSE. !! Flag for interpolation (true/false)
    !
    CHARACTER(LEN=80)                              :: filename              !! Name of the netcdf file (unitless)
    INTEGER(i_std)                                 :: iml, jml, lml, tml, fid, ib, ip, jp, itype !! Indices (unitless)
    REAL(r_std)                                    :: lev(1), date, dt, coslat !!
    INTEGER(i_std)                                 :: itau(1)               !!
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)       :: latrel                !! Latitude
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)       :: lonrel                !! Longitude
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)       :: irrigated_frac        !! Irrigated fraction of the grid box (unitless;0-1)
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:)     :: flood_fracmax         !! Maximal flooded fraction of the grid box (unitless;0-1)
    REAL(r_std)                                    :: area_irrig            !! Irrigated surface in the grid box (m^2)
    REAL(r_std)                                    :: area_flood(ntype)     !! Flooded surface in the grid box (m^2)
!!$    REAL(r_std)                                :: irrigmap(nbpt)
!!$    REAL(r_std)                                :: floodmap(nbpt)
!!$    REAL(r_std)                                :: swampmap(nbpt)

!_ ================================================================================================================================

    nix = 0
    njx = 0
    !
    !Config Key   = IRRIGATION_FILE
    !Config Desc  = Name of file which contains the map of irrigated areas
    !Config Def   = floodplains.nc
    !Config If    = DO_IRRIGATION OR DO_FLOODPLAINS
    !Config Help  = The name of the file to be opened to read the field
    !Config         with the area in m^2 of the area irrigated within each
    !Config         0.5 0.5 deg grid box. The map currently used is the one
    !Config         developed by the Center for Environmental Systems Research 
    !Config         in Kassel (1995).
    !Config Units = [FILE]
    !
    filename = 'floodplains.nc'
    CALL getin_p('IRRIGATION_FILE',filename)
    !
    IF (is_root_prc) THEN
       CALL flininfo(filename,iml, jml, lml, tml, fid)
       CALL flinclo(fid)
    ENDIF
    CALL bcast(iml)
    CALL bcast(jml)
    CALL bcast(lml)
    CALL bcast(tml)
    !
    !
    ALLOCATE (latrel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for latrel','','')

    ALLOCATE (lonrel(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for lonrel','','')

    ALLOCATE (irrigated_frac(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for irrigated_frac','','')

    ALLOCATE (flood_fracmax(iml,jml,ntype), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for flood_fracmax','','')

    IF (is_root_prc) CALL flinopen(filename, .FALSE., iml, jml, lml, lonrel, latrel, lev, tml, itau, date, dt, fid)
    CALL bcast(lonrel)
    CALL bcast(latrel)
    
    !
    IF (is_root_prc) CALL flinget(fid, 'irrig', iml, jml, lml, tml, 1, 1, irrigated_frac)
    CALL bcast(irrigated_frac)
    IF (is_root_prc) CALL flinget(fid, 'lake', iml, jml, lml, tml, 1, 1, flood_fracmax(:,:,ilake))
    IF (is_root_prc) CALL flinget(fid, 'dam', iml, jml, lml, tml, 1, 1, flood_fracmax(:,:,idam))
    IF (is_root_prc) CALL flinget(fid, 'flood', iml, jml, lml, tml, 1, 1, flood_fracmax(:,:,iflood))
    IF (is_root_prc) CALL flinget(fid, 'swamp', iml, jml, lml, tml, 1, 1, flood_fracmax(:,:,iswamp))
    IF (is_root_prc) CALL flinget(fid, 'saline', iml, jml, lml, tml, 1, 1, flood_fracmax(:,:,isal))
    CALL bcast(flood_fracmax)
    !
    IF (is_root_prc) CALL flinclo(fid)
    !
    ! Set to zero all fraction which are less than 0.5%
    !
    DO ip=1,iml
       DO jp=1,jml
          !
          IF ( irrigated_frac(ip,jp) .LT. undef_sechiba-un) THEN
             irrigated_frac(ip,jp) = irrigated_frac(ip,jp)/100.
             IF ( irrigated_frac(ip,jp) < 0.005 ) irrigated_frac(ip,jp) = zero
          ENDIF
          !
          DO itype=1,ntype
             IF ( flood_fracmax(ip,jp,itype) .LT. undef_sechiba-1.) THEN
                flood_fracmax(ip,jp,itype) = flood_fracmax(ip,jp,itype)/100
                IF ( flood_fracmax(ip,jp,itype) < 0.005 )  flood_fracmax(ip,jp,itype) = zero
             ENDIF
          ENDDO
          !
       ENDDO
    ENDDO
    !
    WRITE(numout,*) 'lonrel : ', MAXVAL(lonrel), MINVAL(lonrel)
    WRITE(numout,*) 'latrel : ', MAXVAL(latrel), MINVAL(latrel)
    WRITE(numout,*) 'irrigated_frac : ', MINVAL(irrigated_frac, MASK=irrigated_frac .GT. 0), &
         &                          MAXVAL(irrigated_frac, MASK=irrigated_frac .LT. undef_sechiba)
    WRITE(numout,*) 'flood_fracmax : ', MINVAL(flood_fracmax, MASK=flood_fracmax .GT. 0), &
         &                      MAXVAL(flood_fracmax, MASK=flood_fracmax .LT. undef_sechiba)
    !
    ! Consider all points a priori
    !
    ALLOCATE(resol_lu(iml,jml,2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for resol_lu','','')

    ALLOCATE(mask(iml,jml), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for mask','','')
    mask(:,:) = 0

    DO ip=1,iml
       DO jp=1,jml
          !
          ! Exclude the points where we are close to the missing value.
          !
!MG This condition cannot be applied in floodplains/swamps configuration because
!   the same mask would be used for the interpolation of irrigation, floodplains and swamps maps.
!          IF ( irrigated_frac(ip,jp) < undef_sechiba ) THEN
             mask(ip,jp) = 1
!          ENDIF
          !
          ! Resolution in longitude
          !
          coslat = MAX( COS( latrel(ip,jp) * pi/180. ), mincos )     
          IF ( ip .EQ. 1 ) THEN
             resol_lu(ip,jp,1) = ABS( lonrel(ip+1,jp) - lonrel(ip,jp) ) * pi/180. * R_Earth * coslat
          ELSEIF ( ip .EQ. iml ) THEN
             resol_lu(ip,jp,1) = ABS( lonrel(ip,jp) - lonrel(ip-1,jp) ) * pi/180. * R_Earth * coslat
          ELSE
             resol_lu(ip,jp,1) = ABS( lonrel(ip+1,jp) - lonrel(ip-1,jp) )/2. * pi/180. * R_Earth * coslat
          ENDIF
          !
          ! Resolution in latitude
          !
          IF ( jp .EQ. 1 ) THEN
             resol_lu(ip,jp,2) = ABS( latrel(ip,jp) - latrel(ip,jp+1) ) * pi/180. * R_Earth
          ELSEIF ( jp .EQ. jml ) THEN
             resol_lu(ip,jp,2) = ABS( latrel(ip,jp-1) - latrel(ip,jp) ) * pi/180. * R_Earth
          ELSE
             resol_lu(ip,jp,2) =  ABS( latrel(ip,jp-1) - latrel(ip,jp+1) )/2. * pi/180. * R_Earth
          ENDIF
          !
       ENDDO
    ENDDO
    !
    ! The number of maximum vegetation map points in the GCM grid is estimated.
    ! Some lmargin is taken.
    !
    callsign = 'Irrigation map'
    ok_interpol = .FALSE.
    IF (is_root_prc) THEN
       nix=INT(MAXVAL(resolution_g(:,1))/MAXVAL(resol_lu(:,:,1)))+2
       njx=INT(MAXVAL(resolution_g(:,2))/MAXVAL(resol_lu(:,:,2)))+2
       nbpmax = nix*njx*2
       WRITE(numout,*) "Projection arrays for ",callsign," : "
       WRITE(numout,*) "nbpmax = ",nbpmax, nix, njx
    ENDIF
    CALL bcast(nbpmax)

    ALLOCATE(irrsub_index(nbpt, nbpmax, 2), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for irrsub_index','','')
    irrsub_index(:,:,:)=0

    ALLOCATE(irrsub_area(nbpt, nbpmax), STAT=ALLOC_ERR)
    IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'routing_irrigmap','Pb in allocate for irrsub_area','','')
    irrsub_area(:,:)=zero

    CALL aggregate_p(nbpt, lalo, neighbours, resolution, contfrac, &
         &                iml, jml, lonrel, latrel, mask, callsign, &
         &                nbpmax, irrsub_index, irrsub_area, ok_interpol)
    !
    !
    WHERE (irrsub_area < 0) irrsub_area=zero
    !  
    ! Test here if not all sub_area are larger than 0 if so, then we need to increase nbpmax
    !
    DO ib=1,nbpt
       !
       area_irrig = 0.0
       area_flood = 0.0
       !
       DO fopt=1,COUNT(irrsub_area(ib,:) > zero)
          !
          ip = irrsub_index(ib, fopt, 1)
          jp = irrsub_index(ib, fopt, 2)
          !
          IF (irrigated_frac(ip,jp) .LT. undef_sechiba-1.) THEN
             area_irrig = area_irrig + irrsub_area(ib,fopt)*irrigated_frac(ip,jp)
          ENDIF
          !
          DO itype=1,ntype
             IF (flood_fracmax(ip,jp,itype) .LT. undef_sechiba-1.) THEN
                area_flood(itype) = area_flood(itype) + irrsub_area(ib,fopt)*flood_fracmax(ip,jp,itype)
             ENDIF
          ENDDO
       ENDDO
       !
       ! Put the total irrigated and flooded areas in the output variables
       !
       IF ( init_irrig ) THEN
          irrigated(ib) = MIN(area_irrig, resolution(ib,1)*resolution(ib,2)*contfrac(ib))
          IF ( irrigated(ib) < 0 ) THEN
             WRITE(numout,*) 'We have a problem here : ', irrigated(ib) 
             WRITE(numout,*) 'resolution :', resolution(ib,1), resolution(ib,2)
             WRITE(numout,*) area_irrig
             CALL ipslerr_p(3,'routing_irrigmap','Problem with irrigated...','','')
          ENDIF
!!$          ! Compute a diagnostic of the map.
!!$          IF(contfrac(ib).GT.zero) THEN
!!$             irrigmap (ib) = irrigated(ib) / ( resolution(ib,1)*resolution(ib,2)*contfrac(ib) )
!!$          ELSE
!!$             irrigmap (ib) = zero
!!$          ENDIF
          !
       ENDIF
       !
       IF ( init_flood ) THEN
          floodplains(ib) = MIN(area_flood(iflood)+area_flood(idam)+area_flood(isal), &
               & resolution(ib,1)*resolution(ib,2)*contfrac(ib))
          IF ( floodplains(ib) < 0 ) THEN
             WRITE(numout,*) 'We have a problem here : ', floodplains(ib) 
             WRITE(numout,*) 'resolution :', resolution(ib,1), resolution(ib,2)
             WRITE(numout,*) area_flood
             CALL ipslerr_p(3,'routing_irrigmap','Problem with floodplains..','','')
          ENDIF
!!$          ! Compute a diagnostic of the map.
!!$          IF(contfrac(ib).GT.zero) THEN
!!$             floodmap(ib) = floodplains(ib) / ( resolution(ib,1)*resolution(ib,2)*contfrac(ib) )
!!$          ELSE
!!$             floodmap(ib) = 0.0
!!$          ENDIF
       ENDIF
       !
       IF ( init_swamp ) THEN
          swamp(ib) = MIN(area_flood(iswamp), resolution(ib,1)*resolution(ib,2)*contfrac(ib))
          IF ( swamp(ib) < 0 ) THEN
             WRITE(numout,*) 'We have a problem here : ', swamp(ib) 
             WRITE(numout,*) 'resolution :', resolution(ib,1), resolution(ib,2)
             WRITE(numout,*) area_flood
             CALL ipslerr_p(3,'routing_irrigmap','Problem with swamp...','','')
          ENDIF
!!$          ! Compute a diagnostic of the map.
!!$          IF(contfrac(ib).GT.zero) THEN
!!$             swampmap(ib) = swamp(ib) / ( resolution(ib,1)*resolution(ib,2)*contfrac(ib) )
!!$          ELSE
!!$             swampmap(ib) = zero
!!$          ENDIF
       ENDIF
       !
       !
    ENDDO
    !
    !
    
    IF ( init_irrig ) WRITE(numout,*) "Diagnostics irrigated :", MINVAL(irrigated), MAXVAL(irrigated)
    IF ( init_flood ) WRITE(numout,*) "Diagnostics floodplains :", MINVAL(floodplains), MAXVAL(floodplains)
    IF ( init_swamp ) WRITE(numout,*) "Diagnostics swamp :", MINVAL(swamp), MAXVAL(swamp)
!
! No compensation is done for overlapping floodplains, swamp and irrig. At least overlapping will not
! happen between floodplains and swamp alone
!    IF ( init_irrig .AND. init_flood ) THEN
!       DO ib = 1, nbpt
!          surp = (floodplains(ib)+swamp(ib)+irrigated(ib)) / (resolution(ib,1)*resolution(ib,2)*contfrac(ib))
!          IF ( surp .GT. un ) THEN
!             floodplains(ib) = floodplains(ib) / surp
!             swamp(ib) = swamp(ib) / surp
!             irrigated(ib) = irrigated(ib) / surp
!          ENDIF
!       ENDDO
!    ENDIF
    !
    DEALLOCATE (irrsub_area)
    DEALLOCATE (irrsub_index)
    !
    DEALLOCATE (mask)
    DEALLOCATE (resol_lu)
    !
    DEALLOCATE (lonrel)
    DEALLOCATE (latrel)
    !
  END SUBROUTINE routing_irrigmap
  !
!! ================================================================================================================================
!! SUBROUTINE 	: routing_waterbal
!!
!>\BRIEF         This subroutine checks the water balance in the routing module.
!!
!! DESCRIPTION (definitions, functional, design, flags) : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):
!!
!! REFERENCES   : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

SUBROUTINE routing_waterbal(nbpt, reinit, floodout, runoff, drainage, returnflow, &
               & reinfiltration, irrigation, riverflow, coastalflow)
    !
    IMPLICIT NONE
    !
!! INPUT VARIABLES
    INTEGER(i_std), INTENT(in) :: nbpt                 !! Domain size  (unitless)
    LOGICAL, INTENT(in)        :: reinit               !! Controls behaviour (true/false)
    REAL(r_std), INTENT(in)    :: floodout(nbpt)       !! Grid-point flow out of floodplains (kg/m^2/dt)
    REAL(r_std), INTENT(in)    :: runoff(nbpt)         !! Grid-point runoff (kg/m^2/dt)
    REAL(r_std), INTENT(in)    :: drainage(nbpt)       !! Grid-point drainage (kg/m^2/dt)
    REAL(r_std), INTENT(in)    :: returnflow(nbpt)     !! The water flow from lakes and swamps which returns to the grid box.
                                                       !! This water will go back into the hydrol or hydrolc module to allow re-evaporation (kg/m^2/dt)
    REAL(r_std), INTENT(in)    :: reinfiltration(nbpt) !! Water flow from ponds and floodplains which returns to the grid box (kg/m^2/dt)
    REAL(r_std), INTENT(in)    :: irrigation(nbpt)     !! Irrigation flux. This is the water taken from the reservoirs and beeing put into the upper layers of the soil (kg/m^2/dt)
    REAL(r_std), INTENT(in)    :: riverflow(nbpt)      !! Outflow of the major rivers. The flux will be located on the continental grid but this should be a coastal point (kg/dt)
    REAL(r_std), INTENT(in)    :: coastalflow(nbpt)    !! Outflow on coastal points by small basins. This is the water which flows in a disperse way into the ocean (kg/dt)
    !
    ! We sum-up all the water we have in the warious reservoirs
    !
    REAL(r_std), SAVE          :: totw_flood           !! Sum of all the water amount in the floodplains reservoirs (kg)
!$OMP THREADPRIVATE(totw_flood)
    REAL(r_std), SAVE          :: totw_stream          !! Sum of all the water amount in the stream reservoirs (kg)
!$OMP THREADPRIVATE(totw_stream)
    REAL(r_std), SAVE          :: totw_fast            !! Sum of all the water amount in the fast reservoirs (kg)
!$OMP THREADPRIVATE(totw_fast)
    REAL(r_std), SAVE          :: totw_slow            !! Sum of all the water amount in the slow reservoirs (kg)
!$OMP THREADPRIVATE(totw_slow)
    REAL(r_std), SAVE          :: totw_lake            !! Sum of all the water amount in the lake reservoirs (kg)
!$OMP THREADPRIVATE(totw_lake)
    REAL(r_std), SAVE          :: totw_pond            !! Sum of all the water amount in the pond reservoirs (kg)
!$OMP THREADPRIVATE(totw_pond)
    REAL(r_std), SAVE          :: totw_in              !! Sum of the water flow in to the routing scheme
!$OMP THREADPRIVATE(totw_in)
    REAL(r_std), SAVE          :: totw_out             !! Sum of the water flow out to the routing scheme
!$OMP THREADPRIVATE(totw_out)
    REAL(r_std), SAVE          :: totw_return          !! 
!$OMP THREADPRIVATE(totw_return)
    REAL(r_std), SAVE          :: totw_irrig           !! 
!$OMP THREADPRIVATE(totw_irrig)
    REAL(r_std), SAVE          :: totw_river           !! 
!$OMP THREADPRIVATE(totw_river)
    REAL(r_std), SAVE          :: totw_coastal         !! 
!$OMP THREADPRIVATE(totw_coastal)
    REAL(r_std)                :: totarea              !! Total area of basin (m^2)
    REAL(r_std)                :: area                 !! Total area of routing (m^2)
    INTEGER(i_std)             :: ig                   !! 
    !
    ! Just to make sure we do not get too large numbers !
    !
!! PARAMETERS
    REAL(r_std), PARAMETER     :: scaling = 1.0E+6     !!
    REAL(r_std), PARAMETER     :: allowed_err = 50.    !!

!_ ================================================================================================================================
    !
    IF ( reinit ) THEN
       !
       totw_flood = zero
       totw_stream = zero
       totw_fast = zero
       totw_slow = zero
       totw_lake = zero
       totw_pond = zero 
       totw_in = zero
       !
       DO ig=1,nbpt
          !
          totarea = SUM(routing_area(ig,:))
          !
          totw_flood = totw_flood + SUM(flood_reservoir(ig,:)/scaling)
          totw_stream = totw_stream + SUM(stream_reservoir(ig,:)/scaling)
          totw_fast = totw_fast + SUM(fast_reservoir(ig,:)/scaling)
          totw_slow = totw_slow + SUM(slow_reservoir(ig,:)/scaling)
          totw_lake = totw_lake + lake_reservoir(ig)/scaling
          totw_pond = totw_pond + pond_reservoir(ig)/scaling
          !
          totw_in = totw_in + (runoff(ig)*totarea + drainage(ig)*totarea - floodout(ig)*totarea)/scaling
          !
       ENDDO
       !
    ELSE
       !
       totw_out = zero
       totw_return = zero
       totw_irrig = zero
       totw_river = zero
       totw_coastal = zero
       area = zero
       !
       DO ig=1,nbpt
          !
          totarea = SUM(routing_area(ig,:))
          !
          totw_flood = totw_flood - SUM(flood_reservoir(ig,:)/scaling)
          totw_stream = totw_stream - SUM(stream_reservoir(ig,:)/scaling)
          totw_fast = totw_fast - SUM(fast_reservoir(ig,:)/scaling)
          totw_slow = totw_slow - SUM(slow_reservoir(ig,:)/scaling)
          totw_lake = totw_lake - lake_reservoir(ig)/scaling
          totw_pond = totw_pond - pond_reservoir(ig)/scaling
          !
          totw_return = totw_return + (reinfiltration(ig)+returnflow(ig))*totarea/scaling
          totw_irrig = totw_irrig + irrigation(ig)*totarea/scaling
          totw_river = totw_river + riverflow(ig)/scaling
          totw_coastal = totw_coastal + coastalflow(ig)/scaling
          !
          area = area + totarea
          !
       ENDDO
       totw_out = totw_return + totw_irrig + totw_river + totw_coastal
       !
       ! Now we have all the information to balance our water
       !
       IF ( ABS((totw_flood + totw_stream + totw_fast + totw_slow + totw_lake + totw_pond) - &
            & (totw_out - totw_in)) > allowed_err ) THEN
          WRITE(numout,*) 'WARNING : Water not conserved in routing. Limit at ', allowed_err, ' 10^6 kg'
          WRITE(numout,*) '--Water-- change : flood stream fast ', totw_flood, totw_stream, totw_fast
          WRITE(numout,*) '--Water-- change : slow, lake ', totw_slow, totw_lake
          WRITE(numout,*) '--Water>>> change in the routing res. : ', totw_flood + totw_stream + totw_fast + totw_slow + totw_lake
          WRITE(numout,*) '--Water input : ', totw_in
          WRITE(numout,*) '--Water output : ', totw_out
          WRITE(numout,*) '--Water output : return, irrig ', totw_return, totw_irrig
          WRITE(numout,*) '--Water output : river, coastal ',totw_river, totw_coastal
          WRITE(numout,*) '--Water>>> change by fluxes : ', totw_out - totw_in, ' Diff [mm/dt]: ',   &
               & ((totw_flood + totw_stream + totw_fast + totw_slow + totw_lake) - (totw_out - totw_in))/area

          ! Stop the model
          CALL ipslerr_p(3, 'routing_waterbal', 'Water is not conserved in routing.','','')
       ENDIF
       !
    ENDIF
    !
  END SUBROUTINE routing_waterbal
  !
  !
END MODULE routing
