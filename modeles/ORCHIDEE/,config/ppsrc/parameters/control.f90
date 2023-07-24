










! =================================================================================================================================
! MODULE       : control
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        "control" module contains subroutines to initialize run time control parameters. 
!!
!!\n DESCRIPTION: 
!!
!! SVN          :
!! $HeadURL: 
!! $Date:  
!! $Revision: 
!! \n
!_ ================================================================================================================================

MODULE control
  
  USE constantes_soil
  USE constantes_var
  USE pft_parameters
  USE vertical_soil

  IMPLICIT NONE

CONTAINS
!! ================================================================================================================================
!! SUBROUTINE   : control_initialize 
!!
!>\BRIEF        This subroutine reads the configuration flags which control the behaviour of the model
!!              This subroutine was previsouly named intsurf_config and located in intersurf module. 
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE control_initialize(dt)

    IMPLICIT NONE
    
    REAL(r_std), INTENT(in)                    :: dt            !! Time step in seconds
    INTEGER(i_std)                             :: jv            !! Local index variable
    INTEGER(i_std)                             :: ier           !! Error handeling

    ! Archive the sechiba time step into module constantes_var 
    dt_sechiba=dt

    ! Start reading options from parameter file 

    !Config Key   = CHECKTIME
    !Config Desc  = ORCHIDEE will print messages on time
    !Config If    = OK_SECHIBA
    !Config Def   = n
    !Config Help  = This flag permits to print debug messages on the time.
    !Config Units = [FLAG]
    !
    check_time = .FALSE.
    CALL getin_p('CHECKTIME',check_time)
    !
    !Config Key   = SOILTYPE_CLASSIF
    !Config Desc  = Type of classification used for the map of soil types 
    !Config Def   = zobler
    !Config If    = !IMPOSE_VEG
    !Config Help  = The classification used in the file that we use here 
    !Config         There are three classification supported:  
    !Config         FAO (3 soil types), Zobler (7 converted to 3) and USDA (12) 
    !Config Units = [-]
    !
    !-tdo- Suivant le type de classification utilisee pour le sol, on adapte nscm 
    soil_classif = 'zobler'
    CALL getin_p('SOILTYPE_CLASSIF',soil_classif)
    SELECTCASE (soil_classif)
    CASE ('zobler', 'fao','none')
       nscm = nscm_fao
    CASE ('usda')
       nscm = nscm_usda
    CASE DEFAULT
       WRITE(numout,*) "Unsupported soil type classification. Choose between zobler, fao and usda according to the map"
       STOP 'intsurf_config'
    ENDSELECT


    !Config Key   = RIVER_ROUTING
    !Config Desc  = Decides if we route the water or not
    !Config If    = OK_SECHIBA
    !Config Def   = n
    !Config Help  = This flag allows the user to decide if the runoff
    !Config         and drainage should be routed to the ocean
    !Config         and to downstream grid boxes.
    !Config Units = [FLAG]
    !
    river_routing = .FALSE.
    CALL getin_p('RIVER_ROUTING', river_routing)
    WRITE(numout,*) "RIVER routing is activated : ",river_routing
    !
    !Config key   = HYDROL_CWRR
    !Config Desc  = Allows to switch on the multilayer hydrology of CWRR
    !Config If    = OK_SECHIBA
    !Config Def   = n
    !Config Help  = This flag allows the user to decide if the vertical
    !Config         hydrology should be treated using the multi-layer 
    !Config         diffusion scheme adapted from CWRR by Patricia de Rosnay.
    !Config         by default the Choisnel hydrology is used.
    !Config Units = [FLAG]
    !
    hydrol_cwrr = .FALSE.
    CALL getin_p('HYDROL_CWRR', hydrol_cwrr)
    WRITE(numout,*) "CWRR hydrology is activated : ",hydrol_cwrr

    !Config Key   = DO_IRRIGATION
    !Config Desc  = Should we compute an irrigation flux 
    !Config If    = RIVER_ROUTING 
    !Config Def   = n
    !Config Help  = This parameters allows the user to ask the model
    !Config         to compute an irigation flux. This performed for the
    !Config         on very simple hypothesis. The idea is to have a good
    !Config         map of irrigated areas and a simple function which estimates
    !Config         the need to irrigate.
    !Config Units = [FLAG]
    !
    do_irrigation = .FALSE.
    CALL getin_p('DO_IRRIGATION', do_irrigation)

!!!!! crop irrigation
    ! whether we disrupt water balance to fulfill crop needs
    do_fullirr = .FALSE.
    CALL getin_p('DO_FULLIRR', do_fullirr)
    IF (do_fullirr) THEN
        WRITE(numout,*) "do full irrigation regardless of water balance"
    ENDIF 
!!!!! end crops, xuhui

!!!! crop rotation parameters
    ok_rotate = .FALSE.
    CALL getin_p('OK_ROTATE',ok_rotate)
    dyn_plntdt = .FALSE.
    CALL getin_p('DYN_PLNTDT',dyn_plntdt)
!    dyn_cropfert = .FALSE.
!    CALL getin_p('DYN_CROPFERT',dyn_cropfert)
    nvm_plnt = .FALSE.
    CALL getin_p('NVM_PLNT',nvm_plnt)
    nvm_rot = .FALSE.
    CALL getin_p('NVM_ROT',nvm_rot)
    nvm_nfert = .FALSE.
    CALL getin_p('NVM_NFERT',nvm_nfert)
    cyc_rot_max = 1
    CALL getin_p('CYC_ROT_MAX',cyc_rot_max)
    CALL getin_p('ROT_CMD_MAX',rot_cmd_max) !by default it is 5
    ! if reading maps, it will be automatically updated by map dimension
    !!! resolving conflict
    IF (.NOT. ok_rotate) THEN
        cyc_rot_max = 1
        WRITE(numout,*) 'xuhui: cyc_rot_max is forced to be 1, when ok_rotate is false'
    ELSE !ok_rotate
        !dyn_plntdt = .FALSE.
        !WRITE(numout,*) 'xuhui: deactivate plantdate dynamics so that it follows rotation command'
    ENDIF
!!!! end rotation, xuhui

    !
    !Config Key   = DO_FLOODPLAINS
    !Config Desc  = Should we include floodplains 
    !Config If    = RIVER_ROUTING 
    !Config Def   = n
    !Config Help  = This parameters allows the user to ask the model
    !Config         to take into account the flood plains and return 
    !Config         the water into the soil moisture. It then can go 
    !Config         back to the atmopshere. This tried to simulate 
    !Config         internal deltas of rivers.
    !Config Units = [FLAG]  
    !
    do_floodplains = .FALSE.
    CALL getin_p('DO_FLOODPLAINS', do_floodplains)
    !
    !Config Key   = CHECK_WATERBAL
    !Config Desc  = Should we check the global water balance 
    !Config If    = OK_SECHIBA
    !Config Def   = n
    !Config Help  = This parameters allows the user to check
    !Config         the integrated water balance at the end
    !Config         of each time step
    !Config Units = [FLAG]  
    !
    check_waterbal = .FALSE.
    CALL getin_p('CHECK_WATERBAL', check_waterbal)

    IF (check_waterbal .AND. do_fullirr) THEN
        WRITE(numout,*) "setting conflicts: "
        WRITE(numout,*) "DO_FULLIRR and CHECK_WATERBAL cannot coexist"
        WRITE(numout,*) "change CHECK_WATERBAL to false"
        check_waterbal = .FALSE.
    ENDIF

    !Config Key   = OK_EXPLICITSNOW
    !Config Desc  = Activate explict snow scheme
    !Config If    = OK_SECHIBA
    !Config Def   = FALSE
    !Config Help  = Activate explicit snow scheme instead of default snow scheme
    !Config Units = [FLAG]
    ok_explicitsnow = .FALSE.
    CALL getin_p('OK_EXPLICITSNOW', ok_explicitsnow)

    !Config Key   = OK_PC
    !Config Desc  = Activate explict snow scheme
    !Config If    = OK_SECHIBA
    !Config Def   = FALSE
    !Config Help  = Activate explicit snow scheme instead of default snow scheme
    !Config Units = [FLAG]
    ok_pc = .FALSE.
    CALL getin_p('OK_PC', ok_pc)

    !
    !Config Key   = STOMATE_OK_CO2
    !Config Desc  = Activate CO2?
    !Config If    = OK_SECHIBA 
    !Config Def   = n
    !Config Help  = set to TRUE if photosynthesis is to be activated
    !Config Units = [FLAG]
    !
    ok_co2 = .FALSE.
    CALL getin_p('STOMATE_OK_CO2', ok_co2)
    WRITE(numout,*) 'photosynthesis: ', ok_co2
    !
    !Config Key   = STOMATE_OK_STOMATE
    !Config Desc  = Activate STOMATE?
    !Config If    = OK_SECHIBA and OK_CO2
    !Config Def   = n
    !Config Help  = set to TRUE if STOMATE is to be activated
    !Config Units = [FLAG]
    !
    ok_stomate = .FALSE.
    CALL getin_p('STOMATE_OK_STOMATE',ok_stomate)
    WRITE(numout,*) 'STOMATE is activated: ',ok_stomate
    !
    !Config Key   = STOMATE_OK_DGVM
    !Config Desc  = Activate DGVM?
    !Config If    = OK_STOMATE
    !Config Def   = n
    !Config Help  = set to TRUE if DGVM is to be activated
    !Config Units = [FLAG]
    !
    ok_dgvm = .FALSE.
    CALL getin_p('STOMATE_OK_DGVM',ok_dgvm)
    !
    !Config Key   = CHEMISTRY_BVOC
    !Config Desc  = Activate calculations for BVOC
    !Config If    = OK_SECHIBA
    !Config Def   = n
    !Config Help  = set to TRUE if biogenic emissions calculation is to be activated
    !Config Units = [FLAG]
    !
    ok_bvoc = .FALSE.
    CALL getin_p('CHEMISTRY_BVOC', ok_bvoc)
    WRITE(numout,*) 'Biogenic emissions: ', ok_bvoc

    IF ( ok_bvoc ) THEN 
       ok_leafage         = .TRUE. 
       ok_radcanopy       = .TRUE. 
       ok_multilayer      = .TRUE.
       ok_pulse_NOx       = .TRUE.
       ok_bbgfertil_NOx   = .TRUE.
       ok_cropsfertil_NOx = .TRUE.
    ELSE
       ok_leafage         = .FALSE. 
       ok_radcanopy       = .FALSE. 
       ok_multilayer      = .FALSE.
       ok_pulse_NOx       = .FALSE.
       ok_bbgfertil_NOx   = .FALSE.
       ok_cropsfertil_NOx = .FALSE.
    ENDIF
    !
    !Config Key   = CHEMISTRY_LEAFAGE
    !Config Desc  = Activate LEAFAGE?
    !Config If    = CHEMISTRY_BVOC
    !Config Def   = n
    !Config Help  = set to TRUE if biogenic emissions calculation takes leaf age into account
    !Config Units = [FLAG]
    !
    CALL getin_p('CHEMISTRY_LEAFAGE', ok_leafage)
    WRITE(numout,*) 'Leaf Age: ', ok_leafage
    !
    !Config Key   = CANOPY_EXTINCTION 
    !Config Desc  = Use canopy radiative transfer model?
    !Config If    = CHEMISTRY_BVOC 
    !Config Def   = n
    !Config Help  = set to TRUE if canopy radiative transfer model is used for biogenic emissions 
    !Config Units = [FLAG]
    !
    CALL getin_p('CANOPY_EXTINCTION', ok_radcanopy)
    WRITE(numout,*) 'Canopy radiative transfer model: ', ok_radcanopy
    !
    !Config Key   = CANOPY_MULTILAYER
    !Config Desc  = Use canopy radiative transfer model with multi-layers
    !Config If    = CANOPY_EXTINCTION 
    !Config Def   = n
    !Config Help  = set to TRUE if canopy radiative transfer model is with multiple layers 
    !Config Units = [FLAG]
    !
    CALL getin_p('CANOPY_MULTILAYER', ok_multilayer)
    WRITE(numout,*) 'Multi-layer Canopy model: ', ok_multilayer
    !
    !Config Key   = NOx_RAIN_PULSE
    !Config Desc  = Calculate NOx emissions with pulse?
    !Config If    = CHEMISTRY_BVOC 
    !Config Def   = n
    !Config Help  = set to TRUE if NOx rain pulse is taken into account
    !Config Units = [FLAG]
    !
    CALL getin_p('NOx_RAIN_PULSE', ok_pulse_NOx)
    WRITE(numout,*) 'Rain NOx pulsing: ', ok_pulse_NOx
    !
    !Config Key   = NOx_BBG_FERTIL
    !Config Desc  = Calculate NOx emissions with bbg fertilizing effect?
    !Config If    = CHEMISTRY_BVOC 
    !Config Def   = n
    !Config Help  = set to TRUE if NOx emissions are calculated with bbg effect 
    !Config         Fertil effect of bbg on NOx soil emissions 
    !Config Units = [FLAG]
    !
    CALL getin_p('NOx_BBG_FERTIL', ok_bbgfertil_NOx)
    WRITE(numout,*) 'NOx bbg fertil effect: ', ok_bbgfertil_NOx
    !
    !Config Key   = NOx_FERTILIZERS_USE
    !Config Desc  = Calculate NOx emissions with fertilizers use?
    !Config If    = CHEMISTRY_BVOC 
    !Config Def   = n
    !Config Help  = set to TRUE if NOx emissions are calculated with fertilizers use
    !Config         Fertilizers use effect on NOx soil emissions  
    !Config Units = [FLAG] 
    !
    CALL getin_p('NOx_FERTILIZERS_USE', ok_cropsfertil_NOx)
    WRITE(numout,*) 'NOx Fertilizers use: ', ok_cropsfertil_NOx
    !Config Key  = Is CO2 impact on BVOC accounted for using Possell 2005 ?
    !Config Desc = In this case we use Possell 2005 parameterisation 
    !Config Desc = to take into account the impact of CO2 on biogenic emissions for 
    !Config Desc = isoprene 
    !Config Def  = n 
    !Config Help = set to TRUE if Possell parameterisation has to be considered for the CO2 impact
    !
    ok_co2bvoc_poss = .FALSE.
    CALL getin_p('CO2_FOR_BVOC_POSSELL', ok_co2bvoc_poss)
    WRITE(*,*) 'CO2 impact on BVOC - Possell parameterisation: ', ok_co2bvoc_poss
    !
    !Config Key  = Is CO2 impact on BVOC accounted for using Wilkinson 2009 ? 
    !Config Desc = In this case we use Wilkinson 2009 parameterisation 
    !Config Desc = to take into account the impact of CO2 on biogenic emissions for 
    !Config Desc = isoprene 
    !Config Def  = n 
    !Config Help = set to TRUE if Wilkinson parameterisation has to be considered for the CO2 impact
    !
    ok_co2bvoc_wilk = .FALSE.
    CALL getin_p('CO2_FOR_BVOC_WILKINSON', ok_co2bvoc_wilk)
    WRITE(*,*) 'CO2 impact on BVOC - Wilkinson parameterisation: ', ok_co2bvoc_wilk
    !

    !
    ! control initialisation with sechiba
    !
    ok_sechiba = .TRUE.
    !
    !
    ! Ensure consistency
    !
    IF ( ok_dgvm ) ok_stomate = .TRUE.
    IF ( ok_stomate ) ok_co2 = .TRUE.
    IF ( ok_multilayer .AND. .NOT.(ok_radcanopy) ) THEN
       ok_radcanopy  = .TRUE.
       WRITE(numout,*) 'You want to use the multilayer model without activating the flag CANOPY_EXTINCTION'
       WRITE(numout,*) 'We set CANOPY_EXTINCTION to TRUE to ensure consistency'
    ENDIF
    IF ( ok_dgvm .AND. ok_rotate) THEN
        STOP 'ok_dgvm & ok_rotate cannot be true at the same time'
    ENDIF



    !
    ! Here we need the same initialisation as above
    !
    ok_pheno = .TRUE.

    !
    ! Configuration : number of PFTs and parameters
    !

    ! 1. Number of PFTs defined by the user

    !Config Key   = NVM
    !Config Desc  = number of PFTs  
    !Config If    = OK_SECHIBA or OK_STOMATE
    !Config Def   = 13
    !Config Help  = The number of vegetation types define by the user
    !Config Units = [-]
    !
    CALL getin_p('NVM',nvm)
    WRITE(numout,*)'the number of pfts used by the model is : ', nvm

    ! 2. Should we read the parameters in the run.def file ?

    !Config Key   = IMPOSE_PARAM
    !Config Desc  = Do you impose the values of the parameters?
    !Config if    = OK_SECHIBA or OK_STOMATE
    !Config Def   = y
    !Config Help  = This flag can deactivate the reading of some parameters.
    !               Useful if you want to use the standard values without commenting the run.def
    !Config Units = [FLAG]
    !
    CALL getin_p('IMPOSE_PARAM',impose_param)

    !! Initialize vertical discretization
    IF (hydrol_cwrr) THEN
       !! Case CWRR : All initialization is done in the vertical module
       !! Calculate ngrnd and nslm
       CALL vertical_soil_init
    ELSE
       !! Case Choisnel : get depth of soil and number of soil levels
       ! Remove Config Key description because this was already done in vertical_soil_init.
       !Config Def   = 2.0 or 4.0 depending on hydrol_cwrr flag
       !Config Help  = Maximum depth of soil for soil moisture
       !Config Units = m
       zmaxh=4.0
       CALL getin_p("DEPTH_MAX_H",zmaxh)

       !Config Key   = THERMOSOIL_NBLEV
       !Config Desc  = Number of soil level
       !Config If    = HDYROL_CWRR=FALSE
       !Config Def   = 7
       !Config Help  = Use at least 11 for long term simulation where soil thermal inertia matters
       !Config Units = (-)
       ngrnd=7
       CALL getin_p("THERMOSOIL_NBLEV",ngrnd)

       ! Define nslm, number of levels in CWRR. This variable will not be used for Choisnel but needs to be initialized.
       nslm=11
    END IF

    ! 3. Allocate and intialize the pft parameters

    CALL pft_parameters_main()

    ! 4. Activation sub-models of ORCHIDEE

    CALL activate_sub_models()

    ! 5. Vegetation configuration

    CALL veget_config

    ! 6. Read the parameters in the run.def file  according the flags

    IF (impose_param ) THEN 
       CALL config_pft_parameters
    ENDIF

    IF ( ok_sechiba ) THEN
       IF (impose_param ) THEN
          CALL config_sechiba_parameters
          CALL config_sechiba_pft_parameters()
          WRITE(numout,*)'    some sechiba parameters have been imposed '
       ENDIF
    ENDIF


    !! Initialize variables in constantes_soil
    CALL config_soil_parameters()


    !! Coherence check for depth of thermosoil for long term simulation where soil thermal inertia matters
    !! ok_freeze_thermix is defined in config_soil_parameters
    IF (hydrol_cwrr) THEN
       ! Case CWRR
       IF (ok_freeze_thermix .AND. zmaxt < 11) THEN
          WRITE(numout,*) 'ERROR : Incoherence between ok_freeze_thermix activated and soil depth too small. '
          WRITE(numout,*) 'Here a soil depth of ', zmaxt, 'm is used for the soil thermodynamics'
          WRITE(numout,*) 'Set DEPTH_MAX_T=11 or higher in run.def parameter file or deactivate soil freezing'
          CALL ipslerr_p(3,'control_initialize','Too shallow soil chosen for the thermodynamic for soil freezing', &
               'Adapt run.def with at least DEPTH_MAX=11','')
       END IF
    ELSE
       ! Case Choisnel
       IF (ok_freeze_thermix .AND. ngrnd < 11) THEN
          WRITE(numout,*) 'ERROR : Incoherence between ok_freeze_thermix activated and ngrnd to small. Here used ngrnd=',ngrnd
          WRITE(numout,*) 'Set THERMOSOIL_NBLEV=11 or higher in run.def parameter file or deactivate soil freezing'
          CALL ipslerr_p(3,'control_initialize','Not enough thermodynamic soil levels for soil freezing', &
               'Adapt run.def with at least THERMOSOIL_NBLEV=11','')
       END IF
    END IF
        
    ! Define diaglev
    ! We take the top nslm (number of layer in CWRR) layer of the thermodynamics 
    ! for the diagnostics. The layers in the hydrology and the thermodynamics are
    ! placed a the same depth (the top nslm layers) but the upper boundary condition
    ! is simpler in the thermodynamics. 
    nbdl = nslm
    ALLOCATE(diaglev(nbdl), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'control_initialize','Pb in allocation of diaglev','','')

    IF ( hydrol_cwrr ) THEN
       ! Get diaglev from module vertical for CWRR
       diaglev=znt(1:nbdl)
    ELSE
       ! Calculate diaglev for Choisnel
       DO jv = 1, nbdl-1
           diaglev(jv) = zmaxh/(2**(nbdl-1) -1) * ( ( 2**(jv-1) -1) + ( 2**(jv)-1) ) / deux
      ENDDO
      diaglev(nbdl) = zmaxh
    END IF
    WRITE(numout,*) 'In control_initialize, diaglev = ',diaglev

    IF ( ok_co2 ) THEN
       IF ( impose_param ) THEN
          CALL config_co2_parameters
          WRITE(numout,*)'    some co2 parameters have been imposed '         
       ENDIF
    ENDIF
    
    IF ( ok_stomate ) THEN
       IF ( impose_param ) THEN
          CALL config_stomate_parameters
          CALL config_stomate_pft_parameters
          WRITE(numout,*)'    some stomate parameters have been imposed '
       ENDIF
    ENDIF
    
    IF ( ok_dgvm ) THEN
       IF ( impose_param ) THEN
          CALL config_dgvm_parameters
          WRITE(numout,*)'    some dgvm parameters have been imposed '         
       ENDIF
    ENDIF 

  END SUBROUTINE control_initialize
  
END MODULE control
