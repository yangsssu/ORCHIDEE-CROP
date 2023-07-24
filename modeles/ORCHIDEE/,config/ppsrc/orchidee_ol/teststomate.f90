










! =================================================================================================================================
! PROGRAM       : teststomate
!
! CONTACT	: orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      	: IPSL (2006). This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This program runs the STOMATE submodel using specific initial conditions 
!! and driving variables in order to obtain the state variables of STOMATE closed to the steady-state values  
!! quicker than when using the 'full' ORCHIDEE. 
!!                                        
!! \n DESCRIPTION (functional, design, flags): 
!! It integrates STOMATE for a given number of years using a specific forcing file.       
!! The initial GPP from this forcing is scaled in agreement with the updated calculated LAI in STOMATE 
!! Nothing is done for soil moisture which should actually evolve with the vegetation.            
!! - 1. It first reads a set of parameters, allocates variables and initializes them.
!! - 2. Then, a section on the set up of the STOMATE history file
!! - 3. A first call to slowproc subroutine in order to initialize SECHIBA variables
!! - 4. A loop over time in which slowproc is called at each time step
!! - 5. A restart file is written
!! 
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S)     : None
!!
!! SVN              :   
!! $HeadURL$
!! $Date$
!! $Revision$
!! \n
!_ ================================================================================================================================

PROGRAM teststomate

  ! modules used;

  USE netcdf
  USE defprec
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE stomate_data
  USE ioipsl_para
  USE mod_orchidee_para
  use tools_para
  USE grid
  USE slowproc
  USE stomate
  USE intersurf, ONLY:  intsurf_time, l_first_intersurf
  USE ioipslctrl, ONLY : ioipslctrl_histstom, ioipslctrl_histstomipcc

  IMPLICIT NONE

  !! 0. Variable and parameter declaration

  INTEGER(i_std)                            :: kjpij,kjpindex                       !! # of total/land points
  REAL(r_std)                               :: dtradia,dt_force                     !! time step of sechiba and stomate components 
                                                                                    !! read into the forcing file [second] 
 
  INTEGER(i_std),DIMENSION(:),ALLOCATABLE   :: indices                              !! index over land points
  INTEGER(i_std),DIMENSION(:),ALLOCATABLE   :: indexveg
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: soiltile, veget_x                    !! texture and fraction of vegetation type 
                                                                                    !! (accounts for LAI dynamic) 
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: totfrac_nobio                        !! Total fraction of ice+lakes+cities etc. in 
                                                                                    !! the mesh 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: frac_nobio                           !! Fraction of ice, lakes, cities etc. in the 
                                                                                    !! mesh 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: veget_max_x                          !! Fraction of vegetation type
  INTEGER(i_std), DIMENSION(:),ALLOCATABLE  :: njsc
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: lai_x                                !! Leaf Area Index as calculated in STOMATE 
                                                                                    !! [m2/m2] 
  REAL(r_std),DIMENSION (:,:,:), ALLOCATABLE:: frac_age_x                           !! Age efficacity from STOMATE for isoprene 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: veget_force_x                        !! Fraction of vegetation of PFTs in each grid 
                                                                                    !! cell (read in the forcinng file) [-] 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: veget_max_force_x                    !! fraction of maximum vegetation of PFTs in 
                                                                                    !! each grid cell (read in the forcinng file) 
                                                                                    !! [-] 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: lai_force_x                          !! Leaf Area Index as read in the forcing file 
                                                                                    !! [m2/m2] 
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: reinf_slope
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: t2m,t2m_min,temp_sol                 !! 2 m air temperature, min. 2 m air temp. 
                                                                                    !! during forcing time step and Surface 
                                                                                    !! temperature [K] 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: soiltemp,soilhum                     !!  Soil temperature and Relative soil moisture
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: humrel_x
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: litterhum                            !! Litter humidity
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: precip_rain,precip_snow              !! Rainfall, Snowfall
  !spitfire
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: wspeed                               !! 
  !endspit
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: gpp_x                                !! GPP (gC/(m**2 of total ground)/time step)
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: deadleaf_cover                       !! fraction of soil covered by dead leaves
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE  :: assim_param_x                        !! min/max/opt temperature for photosynthesis 
                                                                                    !! and vcmax/vjmax parameters 
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: height_x                             !! height (m)
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: qsintmax_x                           !! Maximum water on vegetation for interception
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: co2_flux                             !! CO2 flux in gC/m**2 of ground/second
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: fco2_lu                              !! Land Cover Change CO2 flux (gC/m**2 of 
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: temp_growth                          !! Growth temperature - Is equal to t2m_month (°C)
                                                                                    !! average ground/s) 
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: tot_bare_soil                        !! Total evaporating bare soil fraction 

  INTEGER(i_std),DIMENSION(:),ALLOCATABLE   :: indices_g                            !! Vector of indeces of land points, only known by root proc
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: x_indices_g                          !! Temporary vector of indeces of land points
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: x_neighbours_g                       !! Indeces of neighbours for each land point

  ! including permafrost
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE  :: tdeep
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE  :: hsdeep_long
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE  :: heat_Zimov
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: sfluxCH4_deep
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: sfluxCO2_deep
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: thawed_humidity
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: depth_organic_soil
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: snow
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: pb
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: zz_deep
  REAL(r_std),DIMENSION(:),ALLOCATABLE      :: zz_coef_deep
  REAL(r_std),DIMENSION(:,:,:),ALLOCATABLE      :: soilc_total
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE      :: snowdz
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE      :: snowrho
  INTEGER(i_std) :: hist_stomate_deepsoil
  INTEGER(i_std)     :: hist_stomate_snow
  REAL(r_std),DIMENSION(ndeep)                :: sol
  REAL(r_std),DIMENSION(nsnow)  :: snowlev            !! snow axis

  INTEGER                                   :: ier,iret                             !! Return value of functions used to catch 
                                                                                    !! errors 
  INTEGER                                   :: ncfid                                !! Id of the restart file of SECHIBA used in 
                                                                                    !! input 
  CHARACTER(LEN=20),SAVE                    :: thecalendar='noleap'                 !! Type of calendar used
  LOGICAL                                   :: a_er                                 !! Flag used to check errors when allocating
  CHARACTER(LEN=80) :: &                                                            !! Name of the restart files used for
       dri_restname_in,dri_restname_out, &                                          !! the driver in/out
       sec_restname_in,sec_restname_out, &                                          !! the sechiba component in/out
       sto_restname_in,sto_restname_out, &                                          !! the stomate component in/out
       stom_histname, stom_ipcc_histname                                            !! Name of the history files for stomate 
                                                                                    !! (standard and ippc format) 
  INTEGER(i_std)                            :: iim,jjm,llm                          !! # of lon,lat and time-step in the restart 
                                                                                    !! files 
  REAL, ALLOCATABLE, DIMENSION(:,:)         :: lon, lat                             !! Arrays of Longitude and latitude (iim,jjm) 
                                                                                    !! dimension for each processor 
  REAL, ALLOCATABLE, DIMENSION(:)           :: lev                                  !! height level (required by the restini 
                                                                                    !! interface) 
  LOGICAL                                   :: rectilinear                          !! Is the grid rectilinear
  REAL, ALLOCATABLE, DIMENSION(:)           :: lon_rect, lat_rect                   !! Vectors of Longitude and latitude (iim or 
                                                                                    !! jjm) dimension used in case of a 
                                                                                    !! rectilinear grid 
  REAL(r_std)                               :: dt                                   !! Time step of sechiba component read in the 
                                                                                    !! restart file [seconds] 
  INTEGER(i_std)                            :: itau_dep,itau_fin,itau               !! First, last and current time step of SECHIBA 
                                                                                    !! component 
  INTEGER(i_std)                            :: itau_len,itau_step                   !! Total length of the simulation and length 
                                                                                    !! between 2 calls of slowproc (expreseed in 
                                                                                    !! time steps of SECHIBA component) 
  REAL(r_std)                               :: date0                                !! Initial date
  INTEGER(i_std)                            :: rest_id_sec,rest_id_sto              !! ID's of the restart files for the SECHIBA 
                                                                                    !! and STOMATE components 
  INTEGER(i_std)                            :: hist_id_sec,hist_id_sec2             !! ID's of the history files of SECHIBA 
                                                                                    !! component (required by the interface of 
                                                                                    !! slowproc but not used) 
  INTEGER(i_std)                            :: hist_id_stom,hist_id_stom_IPCC       !! ID's of the history files of STOMATE 
                                                                                    !! component 
  CHARACTER(LEN=30)                         :: time_str                             !! String used for reading the length of 
                                                                                    !! simulation in the .def file 
  REAL(r_std)                               :: dt_stomate_loc                             !! 
  REAL                                      :: hist_days_stom,hist_days_stom_ipcc   !! Time frequency at which variables are 
                                                                                    !! written in the history file for the STOMATE 
                                                                                    !! component (standard and ipcc format) [day] 
  REAL                                      :: hist_dt_stom,hist_dt_stom_ipcc       !! Time frequency at which variables are 
                                                                                    !! written in the history file for the STOMATE 
                                                                                    !! component (standard and ipcc format) 
                                                                                    !! [second] 
  REAL(r_std), ALLOCATABLE, DIMENSION(:)    :: hist_PFTaxis                         !! Vector with PFT indeces used as axis in the 
                                                                                    !! history file 
  REAL(r_std),DIMENSION(10)                 :: hist_pool_10axis                     !! Vector with 10-year indeces used as axis in 
                                                                                    !! the history file (for land-use change) 
  REAL(r_std),DIMENSION(100)                :: hist_pool_100axis                    !! Vector with 100-year indeces used as axis in 
                                                                                    !! the history file (for land-use change) 
  REAL(r_std),DIMENSION(11)                 :: hist_pool_11axis                     !! Vector with 11-year indeces used as axis in 
                                                                                    !! the history file (for land-use change) 
  REAL(r_std),DIMENSION(101)                :: hist_pool_101axis                    !! Vector with 101-year indeces used as axis in 
                                                                                    !! the history file (for land-use change) 
  INTEGER                                   :: hist_PFTaxis_id,hist_IPCC_PFTaxis_id !! Id of the axis for PFT in the standard/IPCC 
                                                                                    !! format 
  INTEGER                                   :: hori_id 
  INTEGER                                   :: hist_pool_10axis_id                  !! Id of the axis for 10-year vector
  INTEGER                                   :: hist_pool_100axis_id                 !! Id of the axis for the 100-year vector
  INTEGER                                   :: hist_pool_11axis_id                  !! Id of the axis for the 11-year vector
  INTEGER                                   :: hist_pool_101axis_id                 !! Id of the axis for the 101-year vector
  INTEGER(i_std)                            :: i,j,iv                               !! used as counters
  LOGICAL                                   :: ldrestart_read,ldrestart_write       !! Flags to activate reading/writing of the 
                                                                                    !! restart file 
  LOGICAL                                   :: l1d                                  !! Are vector elements 1-dimension size ?
  INTEGER(i_std),PARAMETER                  :: nbvarmax=400                         !! Maximum number of variables assumed in the 
                                                                                    !! restart file of SECHIBA component used as 
                                                                                    !! input 
  INTEGER(i_std)                            :: nbvar                                !! Number of variables present in the restart 
                                                                                    !! file of SECHIBA component used as input 
  CHARACTER(LEN=200),DIMENSION(nbvarmax)     :: varnames                             !! Name of the variables present in the restart 
                                                                                    !! file of SECHIBA component used as input 
  INTEGER(i_std)                            :: varnbdim                             !! Number of dimensions of a variable 
                                                                                    !! varnames(i) 
  INTEGER(i_std),PARAMETER                  :: varnbdim_max=20                      !! Maximum number of dimensions of a variable 
                                                                                    !! varnames(i) 
  INTEGER,DIMENSION(varnbdim_max)           :: vardims
  CHARACTER(LEN=200)                        :: taboo_vars
  REAL(r_std),DIMENSION(1)                  :: xtmp
  INTEGER                                   :: nsfm,nsft
  INTEGER                                   :: iisf,iiisf
  INTEGER(i_std)                            :: max_totsize,totsize_1step
  INTEGER(i_std)                            :: totsize_tmp  
  INTEGER                                   :: vid,v_id
  CHARACTER(LEN=100)                        :: forcing_name
  REAL                                      :: x
  REAL(r_std),DIMENSION(:,:),ALLOCATABLE    :: var_3d
  REAL(r_std)                               :: var_1d(1)
  REAL(r_std)                               :: time_sec,time_step_sec
  REAL(r_std)                               :: time_dri,time_step_dri
  REAL(r_std),DIMENSION(1)                  :: r1d
  REAL(r_std)                               :: julian,djulian

  INTEGER(i_std)                            :: ji,jv,l
  INTEGER(i_std)                            :: printlev_loc

!---------------------------------------------------------------------

  !-
  ! 1. Reading parameters, Allocating variables and Initializations
  !-

  CALL Init_orchidee_para
  CALL init_timer

! Set specific write level to forcesoil using PRINTLEV_teststomate=[0-4] in run.def. 
! The global printlev is used as default value. 
  printlev_loc=get_printlev('teststomate')

  IF (is_root_prc) THEN
     !-
     ! open STOMATE's forcing file to read some basic info
     !-
     forcing_name = 'stomate_forcing.nc'
     CALL getin ('STOMATE_FORCING_NAME',forcing_name)
     iret = NF90_OPEN (TRIM(forcing_name),NF90_NOWRITE,forcing_id)
     IF (iret /= NF90_NOERR) THEN
        CALL ipslerr (3,'teststomate', &
             &        'Could not open file : ', &
             &          forcing_name,'(Did you forget it ?)')
     ENDIF
     ier = NF90_GET_ATT (forcing_id,NF90_GLOBAL,'dt_sechiba',dtradia)
     ier = NF90_GET_ATT (forcing_id,NF90_GLOBAL,'dt_stomate',dt_force)
     ier = NF90_GET_ATT (forcing_id,NF90_GLOBAL,'nsft',x)
     nsft = NINT(x)
     ier = NF90_GET_ATT (forcing_id,NF90_GLOBAL,'kjpij',x)
     kjpij = NINT(x)
     ier = NF90_GET_ATT (forcing_id,NF90_GLOBAL,'kjpindex',x)
     nbp_glo = NINT(x)
  ENDIF

  CALL bcast(dtradia)
  CALL bcast(dt_force)
  CALL bcast(nsft)
  CALL bcast(nbp_glo)
  !-
  WRITE(numout,*) 'ATTENTION dtradia=',dtradia,' dt_force=',dt_force
  ! Coherence test : stop if dtradia not equal dt_force
  IF (dtradia /= dt_force) CALL ipslerr (3, 'teststomate','dtradia must be equal to dt_force','','')

  ! Archive the sechiba time step in constantes_var module
  dt_sechiba=dtradia

  !-
  ! read info about land points
  !-
  IF (is_root_prc) THEN
     ALLOCATE (indices_g(nbp_glo),stat=ier)
     IF (ier /= 0) THEN
        CALL ipslerr (3,'teststomate', &
             'PROBLEM WITH ALLOCATION', &
             'for local variable indices_g','')
     ENDIF
     !
     ALLOCATE (x_indices_g(nbp_glo),stat=ier)
     IF (ier /= 0) THEN
        CALL ipslerr (3,'teststomate', &
             'PROBLEM WITH ALLOCATION', &
             'for global variable x_indices_g','')
     ENDIF

     ier = NF90_INQ_VARID (forcing_id,'index',vid)
     IF (ier /= NF90_NOERR) THEN
        CALL ipslerr (3,'teststomate', &
             'PROBLEM WITH READING VARIABLE ID', &
             'for global variable index','')
     ENDIF
     ier = NF90_GET_VAR   (forcing_id,vid,x_indices_g)
     IF (iret /= NF90_NOERR) THEN
        CALL ipslerr (3,'teststomate', &
             'PROBLEM WITH variable "index" in file ', &
             forcing_name,'(check this file)')
     ENDIF
     indices_g(:) = NINT(x_indices_g(:))
     DEALLOCATE (x_indices_g)
  ENDIF

!---------------------------------------------------------------------
!-
  !-
  ! activate CO2, STOMATE, but not sechiba
  !-
  river_routing = .FALSE.
  hydrol_cwrr = .FALSE.
  ok_sechiba = .FALSE.
  ok_co2 = .TRUE.
  ok_stomate = .TRUE.

  ! Deactivate writing of stomate_forcing.nc file
  allow_forcing_write=.FALSE.

  !-
  ! is DGVM activated?
  !-
  ok_dgvm = .FALSE.
  CALL getin_p('STOMATE_OK_DGVM',ok_dgvm)
  WRITE(numout,*) 'LPJ is activated: ',ok_dgvm

  ! Initialize parameter for off-line use : no coupling to atmospheric model
  OFF_LINE_MODE=.TRUE.
!-
! Configuration
!-
  ! 1. Number of PFTs defined by the user

  !Config Key   = NVM
  !Config Desc  = number of PFTs  
  !Config If    = OK_SECHIBA or OK_STOMATE
  !Config Def   = 13
  !Config Help  = The number of vegetation types define by the user
  !Config Units = [-]
  !
  CALL getin_p('NVM',nvm)
  WRITE(numout,*)'the number of pfts is : ', nvm

  ! 2. Should we read the parameters in the run.def file ?
  
  !Config Key   = IMPOSE_PARAM
  !Config Desc  = Do you impose the values of the parameters?
  !Config if    = OK_SECHIBA or OK_STOMATE
  !Config Def   = y
  !Config Help  = This flag can deactivate the reading of some parameters.
  !Config         Useful if you want to use the standard values without commenting the run.def
  !Config Units = [FLAG]
  !
  CALL getin_p('IMPOSE_PARAM',impose_param)

  ! 3. Allocate and intialize the pft parameters

  CALL pft_parameters_main()

  ! 4. Activation sub-models of ORCHIDEE

  CALL activate_sub_models()

  ! 5. Vegetation configuration (impose_veg, land_use, lcchange...previously in slowproc)

  CALL veget_config

  ! 6. Read the parameters in the run.def file

  IF (impose_param) THEN
     CALL config_pft_parameters
     CALL config_stomate_pft_parameters
     CALL config_co2_parameters
     CALL config_stomate_parameters
  ENDIF
  !-
  IF (ok_dgvm) THEN
     IF ( impose_param ) THEN
        CALL config_dgvm_parameters
     ENDIF
  ENDIF
!-
  !-
  ! restart files
  !-
  IF (is_root_prc) THEN
     ! Sechiba's restart files
     sec_restname_in = 'sechiba_start.nc'
     CALL getin('SECHIBA_restart_in',sec_restname_in)
     WRITE(numout,*) 'SECHIBA INPUT RESTART_FILE: ',TRIM(sec_restname_in)
     IF ( TRIM(sec_restname_in) .EQ. 'NONE' ) THEN
        STOP 'Need a restart file for Sechiba'
     ENDIF
     sec_restname_out = 'sechiba_rest_out.nc'
     CALL getin('SECHIBA_rest_out',sec_restname_out)
     WRITE(numout,*) 'SECHIBA OUTPUT RESTART_FILE: ',TRIM(sec_restname_out)
     ! Stomate's restart files
     sto_restname_in = 'stomate_start.nc'
     CALL getin('STOMATE_RESTART_FILEIN',sto_restname_in)
     WRITE(numout,*) 'STOMATE INPUT RESTART_FILE: ',TRIM(sto_restname_in)
     sto_restname_out = 'stomate_rest_out.nc'
     CALL getin('STOMATE_RESTART_FILEOUT',sto_restname_out)
     WRITE(numout,*) 'STOMATE OUTPUT RESTART_FILE: ',TRIM(sto_restname_out)

     !-
     ! We need to know iim, jjm.
     ! Get them from the restart files themselves.
     !-
     iret = NF90_OPEN (sec_restname_in,NF90_NOWRITE,ncfid)
     IF (iret /= NF90_NOERR) THEN
        CALL ipslerr (3,'teststomate', &
             &        'Could not open file : ', &
             &          sec_restname_in,'(Do you have forget it ?)')
     ENDIF
     iret = NF90_INQUIRE_DIMENSION (ncfid,1,len=iim_g)
     iret = NF90_INQUIRE_DIMENSION (ncfid,2,len=jjm_g)
     iret = NF90_INQ_VARID (ncfid, "time", iv)
     iret = NF90_GET_ATT (ncfid, iv, 'calendar',thecalendar)
     iret = NF90_CLOSE (ncfid)
     i=INDEX(thecalendar,ACHAR(0))
     IF ( i > 0 ) THEN
        thecalendar(i:20)=' '
     ENDIF
  ENDIF
  CALL bcast(iim_g)
  CALL bcast(jjm_g)
  CALL bcast(thecalendar)
  !-
  ! calendar
  !-
  CALL ioconf_calendar (thecalendar)
  CALL ioget_calendar  (one_year,one_day)
  !
  ! Parallelization :
  !
!  CALL init_data_para(iim_g,jjm_g,nbp_glo,indices_g)
  CALL set_grid_glo(iim_g,jjm_g,nbp_glo)
  CALL allocate_grid_glo

  ! Initialize index_g needed for module grid
  ! index_g is declared in mod_orchidee_para and allocated by allocate_grid_glo
  ! index_g and indices_g are the same but indeces_g only declared on root proc. 
  IF (is_root_prc) THEN
     index_g(:)=indices_g(:)
     ! Only use index_g from now and on => Deallocate indices_g.
     DEALLOCATE(indices_g)
  END IF
  CALL bcast(index_g)

  CALL init_orchidee_data_para_driver(nbp_glo, index_g)
  kjpindex=nbp_loc
  jjm=jj_nb
  iim=iim_g
  kjpij=iim*jjm
  IF (printlev_loc>=3) WRITE(numout,*) "Local grid : ",kjpindex,iim,jjm
  !-
  !-
  ! read info about grids
  !-
  !-
  llm=1
  ALLOCATE(lev(llm))
  IF (is_root_prc) THEN
     !-
     ier = NF90_INQ_VARID (forcing_id,'lalo',vid)
     ier = NF90_GET_VAR   (forcing_id,vid,lalo_g)
     !-
     ALLOCATE (x_neighbours_g(nbp_glo,8),stat=ier)
     ier = NF90_INQ_VARID (forcing_id,'neighbours',vid)
     ier = NF90_GET_VAR   (forcing_id,vid,x_neighbours_g)
     neighbours_g(:,:) = NINT(x_neighbours_g(:,:))
     DEALLOCATE (x_neighbours_g)
     !-
     ier = NF90_INQ_VARID (forcing_id,'resolution',vid)
     ier = NF90_GET_VAR   (forcing_id,vid,resolution_g)
     !-
     ier = NF90_INQ_VARID (forcing_id,'contfrac',vid)
     ier = NF90_GET_VAR   (forcing_id,vid,contfrac_g)

     lon_g(:,:) = zero
     lat_g(:,:) = zero
     lev(1)   = zero
     !-
     CALL restini &
          & (sec_restname_in, iim_g, jjm_g, lon_g, lat_g, llm, lev, &
          &  sec_restname_out, itau_dep, date0, dt, rest_id_sec)
     !-
     IF ( dt .NE. dtradia ) THEN
        WRITE(numout,*) 'dt',dt
        WRITE(numout,*) 'dtradia',dtradia
        CALL ipslerr (3,'teststomate', &
             &        'PROBLEM with time steps.', &
             &          sec_restname_in,'(dt .NE. dtradia)')
     ENDIF
     !-
     CALL restini &
          & (sto_restname_in, iim_g, jjm_g, lon_g, lat_g, llm, lev, &
          &  sto_restname_out, itau_dep, date0, dt, rest_id_sto)
     !-
     IF ( dt .NE. dtradia ) THEN
        WRITE(numout,*) 'dt',dt
        WRITE(numout,*) 'dtradia',dtradia
        CALL ipslerr (3,'teststomate', &
             &        'PROBLEM with time steps.', &
             &          sto_restname_in,'(dt .NE. dtradia)')
     ENDIF
  ENDIF
  CALL bcast(rest_id_sec)
  CALL bcast(rest_id_sto)
  CALL bcast(itau_dep)
  CALL bcast(date0)
  CALL bcast(dt)
  CALL bcast(lev)
  !---
  !--- Create the index table
  !---
  !--- This job return a LOCAL kindex
  !---
  ALLOCATE (indices(kjpindex),stat=ier)
  IF (printlev_loc>=3 .AND. is_root_prc) WRITE(numout,*) "index_g = ",index_g(1:nbp_glo)
  CALL scatter(index_g,indices)
  indices(1:kjpindex)=indices(1:kjpindex)-(jj_begin-1)*iim_g
  IF (printlev_loc>=3) WRITE(numout,*) "indices = ",indices(1:kjpindex)

  !---
  !--- initialize global grid
  !---
  CALL init_grid( kjpindex ) 
  CALL grid_stuff (nbp_glo, iim_g, jjm_g, lon_g, lat_g, index_g)

  !---
  !--- initialize local grid
  !---
  jlandindex = (((indices(1:kjpindex)-1)/iim) + 1)
  if (printlev_loc>=3) WRITE(numout,*) "jlandindex = ",jlandindex(1:kjpindex)
  ilandindex = (indices(1:kjpindex) - (jlandindex(1:kjpindex)-1)*iim)
  IF (printlev_loc>=3) WRITE(numout,*) "ilandindex = ",ilandindex(1:kjpindex)
  ALLOCATE(lon(iim,jjm))
  ALLOCATE(lat(iim,jjm))
  lon=zero
  lat=zero
  CALL scatter2D_mpi(lon_g,lon)
  CALL scatter2D_mpi(lat_g,lat)

  DO ji=1,kjpindex

     j = jlandindex(ji)
     i = ilandindex(ji)

     !- Create the internal coordinate table
!-
     lalo(ji,1) = lat(i,j)
     lalo(ji,2) = lon(i,j)
  ENDDO
  CALL scatter(neighbours_g,neighbours)
  CALL scatter(resolution_g,resolution)
  CALL scatter(contfrac_g,contfrac)
  CALL scatter(area_g,area)
  !-
  !- Check if we have by any chance a rectilinear grid. This would allow us to 
  !- simplify the output files.
  !
  rectilinear = .FALSE.
  IF (is_root_prc) THEN
     IF ( ALL(lon_g(:,:) == SPREAD(lon_g(:,1), 2, SIZE(lon_g,2))) .AND. &
       & ALL(lat_g(:,:) == SPREAD(lat_g(1,:), 1, SIZE(lat_g,1))) ) THEN
        rectilinear = .TRUE.
     ENDIF
  ENDIF
  CALL bcast(rectilinear)
  IF (rectilinear) THEN
     ALLOCATE(lon_rect(iim),stat=ier)
     IF (ier .NE. 0) THEN
        WRITE (numout,*) ' error in lon_rect allocation. We stop. We need iim words = ',iim
        STOP 'intersurf_history'
     ENDIF
     ALLOCATE(lat_rect(jjm),stat=ier)
     IF (ier .NE. 0) THEN
        WRITE (numout,*) ' error in lat_rect allocation. We stop. We need jjm words = ',jjm
        STOP 'intersurf_history'
     ENDIF
     lon_rect(:) = lon(:,1)
     lat_rect(:) = lat(1,:)
  ENDIF
  !-
  ! allocate arrays
  !-
  !
  a_er = .FALSE.
  ALLOCATE (hist_PFTaxis(nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (indexveg(kjpindex*nvm), stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (soiltile(kjpindex,nstm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (veget_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (totfrac_nobio(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (frac_nobio(kjpindex,nnobio),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (veget_max_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (lai_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (veget_force_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (veget_max_force_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (njsc(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (lai_force_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (reinf_slope(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (t2m(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (t2m_min(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (temp_sol(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (soiltemp(kjpindex,nbdl),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (soilhum(kjpindex,nbdl),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (humrel_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (litterhum(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (precip_rain(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (precip_snow(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (wspeed(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (gpp_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (deadleaf_cover(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (assim_param_x(kjpindex,nvm,npco2),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (height_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (qsintmax_x(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (co2_flux(kjpindex,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (fco2_lu(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (temp_growth(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (tot_bare_soil(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (frac_age_x(kjpindex,nvm,nleafages),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ! including permafrost variables
  ALLOCATE (tdeep(kjpindex,ndeep,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (hsdeep_long(kjpindex,ndeep,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (heat_Zimov(kjpindex,ndeep,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (sfluxCH4_deep(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (sfluxCO2_deep(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (thawed_humidity(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (depth_organic_soil(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (snow(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (pb(kjpindex),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (zz_deep(ndeep),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (zz_coef_deep(ndeep),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (soilc_total(kjpindex,ndeep,nvm),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (snowdz(kjpindex,nsnow),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  ALLOCATE (snowrho(kjpindex,nsnow),stat=ier)
  a_er = a_er .OR. (ier.NE.0)
  IF (a_er) THEN
     CALL ipslerr (3,'teststomate', &
          &        'PROBLEM WITH ALLOCATION', &
          &        'for local variables 1','')
  ENDIF
  !
  ! prepare forcing
  ! forcing file of STOMATE is read by block 
  ! in order to minimize the reading frequency
  ! Here is done the calculation of the number 
  ! of time steps we load in memory 
  !
  max_totsize = 50
  CALL getin_p ('STOMATE_FORCING_MEMSIZE',max_totsize)
  max_totsize = max_totsize * 1000000

  totsize_1step = SIZE(soiltile(:,3))*KIND(soiltile(:,3)) + &
       SIZE(humrel_x)*KIND(humrel_x) + &
       SIZE(litterhum)*KIND(litterhum) + &
       SIZE(t2m)*KIND(t2m) + &
       SIZE(t2m_min)*KIND(t2m_min) + &
       SIZE(temp_sol)*KIND(temp_sol) + &
       SIZE(soiltemp)*KIND(soiltemp) + &
       SIZE(soilhum)*KIND(soilhum) + &
       SIZE(precip_rain)*KIND(precip_rain) + &
       SIZE(precip_snow)*KIND(precip_snow) + &
       SIZE(wspeed)*KIND(wspeed) + &
       SIZE(gpp_x)*KIND(gpp_x) + &
       SIZE(veget_force_x)*KIND(veget_force_x) + &
       SIZE(veget_max_force_x)*KIND(veget_max_force_x) + &
       SIZE(lai_force_x)*KIND(lai_force_x)
  CALL reduce_sum(totsize_1step,totsize_tmp)
  CALL bcast(totsize_tmp)
  totsize_1step=totsize_tmp 

  ! check for consistency of the total number of forcing steps
  IF ( nsft .NE. INT(one_year/(dt_force/one_day)) ) THEN
     CALL ipslerr_p (3,'teststomate', &
          &        'stomate: error with total number of forcing steps', &
          &        'nsft','teststomate computation different with forcing file value.')
  ENDIF
  ! number of forcing steps in memory
  nsfm = MIN(nsft, &
       &       MAX(1,NINT( REAL(max_totsize,r_std) &
       &                  /REAL(totsize_1step,r_std))))
  !-
  WRITE(numout,*) 'Offline forcing of Stomate:'
  WRITE(numout,*) '  Total number of forcing steps:',nsft
  WRITE(numout,*) '  Number of forcing steps in memory:',nsfm
  !-
  ! init_forcing defined into stomate.f90
  ! allocate and set to zero driving variables of STOMATE
  ! ie variables that are read in the forcing file
  !-
  CALL init_forcing(kjpindex,nsfm,nsft)
  !-
  ! ensure that we read all new forcing states
  iisf = nsfm
  ! initialize the table that contains the indices
  ! of the forcing states that will be in memory
  isf(:) = (/ (i,i=1,nsfm) /)

  nf_written(:) = .FALSE.
  nf_written(isf(:)) = .TRUE.

  !-
  ! a time step for STOMATE corresponds to itau_step timesteps in SECHIBA
  !-
  itau_step = NINT(dt_force/dtradia)
  IF (printlev_loc>=3) WRITE(numout,*) "dtradia, dt_rest, dt_force, itau_step",dtradia, dt, dt_force, itau_step
  !
  CALL ioconf_startdate(date0)
  CALL intsurf_time( itau_dep, date0 )
  !-
  ! Length of integration
  !-
  WRITE(time_str,'(a)') '1Y'
  CALL getin_p ('TIME_LENGTH', time_str)
  ! transform into itau
  CALL tlen2itau(time_str, dt, date0, itau_len)
  ! itau_len*dtradia must be a multiple of dt_force
  itau_len = NINT( MAX(1.,FLOAT(NINT(itau_len*dtradia/dt_force))) &
       &                *dt_force/dtradia)
  !-
  itau_fin = itau_dep+itau_len
  !-
  ! 2. set up STOMATE history file
  !
  ! Initialize ioipsl_para module
  CALL  init_ioipsl_para

  !-
  !Config Key   = STOMATE_OUTPUT_FILE
  !Config Desc  = Name of file in which STOMATE's output is going to be written
  !Config If    = 
  !Config Def   = stomate_history.nc
  !Config Help  = This file is going to be created by the model
  !Config         and will contain the output from the model.
  !Config         This file is a truly COADS compliant netCDF file.
  !Config         It will be generated by the hist software from
  !Config         the IOIPSL package.
  !Config Units = FILE
  !-
  stom_histname='stomate_history.nc'
  CALL getin_p ('STOMATE_OUTPUT_FILE', stom_histname)
  WRITE(numout,*) 'STOMATE_OUTPUT_FILE', TRIM(stom_histname)
  !-
  !Config Key   = STOMATE_HIST_DT
  !Config Desc  = STOMATE history time step 
  !Config If    = 
  !Config Def   = 10.
  !Config Help  = Time step of the STOMATE history file
  !Config Units = days [d]
  !-
  hist_days_stom = 10.
  CALL getin_p ('STOMATE_HIST_DT', hist_days_stom)
  IF ( hist_days_stom == -1. ) THEN
     hist_dt_stom = -1.
     WRITE(numout,*) 'output frequency for STOMATE history file (d): one month.'
  ELSE
     hist_dt_stom = NINT( hist_days_stom ) * one_day
     WRITE(numout,*) 'output frequency for STOMATE history file (d): ', &
             hist_dt_stom/one_day
  ENDIF
  !-
  !- initialize
  WRITE(numout,*) "before histbeg : ",date0,dt
  IF ( rectilinear ) THEN
     CALL histbeg(stom_histname, iim, lon_rect, jjm, lat_rect,  1, iim, 1, jjm, &
          &     itau_dep, date0, dt, hori_id, hist_id_stom, domain_id=orch_domain_id)
  ELSE
     CALL histbeg(stom_histname, iim, lon, jjm, lat,  1, iim, 1, jjm, &
          &     itau_dep, date0, dt, hori_id, hist_id_stom, domain_id=orch_domain_id)
  ENDIF
  !- define PFT axis
  hist_PFTaxis = (/ ( REAL(i,r_std), i=1,nvm ) /)
  !- declare this axis
  CALL histvert (hist_id_stom, 'PFT', 'Plant functional type', &
       & '1', nvm, hist_PFTaxis, hist_PFTaxis_id)
!- define Pool_10 axis
   hist_pool_10axis = (/ ( REAL(i,r_std), i=1,10 ) /)
!- declare this axis
  CALL histvert (hist_id_stom, 'P10', 'Pool 10 years', &
       & '1', 10, hist_pool_10axis, hist_pool_10axis_id)

!- define Pool_100 axis
   hist_pool_100axis = (/ ( REAL(i,r_std), i=1,100 ) /)
!- declare this axis
  CALL histvert (hist_id_stom, 'P100', 'Pool 100 years', &
       & '1', 100, hist_pool_100axis, hist_pool_100axis_id)

!- define Pool_11 axis
   hist_pool_11axis = (/ ( REAL(i,r_std), i=1,11 ) /)
!- declare this axis
  CALL histvert (hist_id_stom, 'P11', 'Pool 10 years + 1', &
       & '1', 11, hist_pool_11axis, hist_pool_11axis_id)

!- define Pool_101 axis
   hist_pool_101axis = (/ ( REAL(i,r_std), i=1,101 ) /)
!- declare this axis
  CALL histvert (hist_id_stom, 'P101', 'Pool 100 years + 1', &
       & '1', 101, hist_pool_101axis, hist_pool_101axis_id)
! define deep permafrost axis for stomate variables
   sol(1:ndeep) = (/ (REAL(i,r_std),i=1,ndeep) /)
   CALL histvert(hist_id_stom, 'solth', 'deep soil levels',      'm', &
        &    ndeep, sol, hist_stomate_deepsoil)

   snowlev = (/ ( REAL(i,r_std), i=1,nsnow ) /)
   CALL histvert(hist_id_stom, 'snowlev', 'snow levels',      'index', &
        &    nsnow, snowlev, hist_stomate_snow)


  !- define STOMATE history file
  CALL ioipslctrl_histstom (hist_id_stom, nvm, iim, jjm, &
       & dt, hist_dt_stom, hori_id, hist_PFTaxis_id, &
 & hist_pool_10axis_id, hist_pool_100axis_id, &
 & hist_pool_11axis_id, hist_pool_101axis_id,&
 & hist_stomate_deepsoil, hist_stomate_snow)
  !- end definition
  CALL histend(hist_id_stom)
  !-
  !-
  ! STOMATE IPCC OUTPUTS IS ACTIVATED
  !-
  !Config Key   = STOMATE_IPCC_OUTPUT_FILE
  !Config Desc  = Name of file in which STOMATE's output is going to be written
  !Config If    = 
  !Config Def   = stomate_ipcc_history.nc
  !Config Help  = This file is going to be created by the model
  !Config         and will contain the output from the model.
  !Config         This file is a truly COADS compliant netCDF file.
  !Config         It will be generated by the hist software from
  !Config         the IOIPSL package.
  !Config Units = FILE
  !-
  stom_ipcc_histname='stomate_ipcc_history.nc'
  CALL getin_p('STOMATE_IPCC_OUTPUT_FILE', stom_ipcc_histname)       
  WRITE(numout,*) 'STOMATE_IPCC_OUTPUT_FILE', TRIM(stom_ipcc_histname)
  !-
  !Config Key   = STOMATE_IPCC_HIST_DT
  !Config Desc  = STOMATE IPCC history time step 
  !Config If    = 
  !Config Def   = 0.
  !Config Help  = Time step of the STOMATE IPCC history file
  !Config Units = days [d]
  !-
  hist_days_stom_ipcc = zero
  CALL getin_p('STOMATE_IPCC_HIST_DT', hist_days_stom_ipcc)       
  IF ( hist_days_stom_ipcc == moins_un ) THEN
     hist_dt_stom_ipcc = moins_un
     WRITE(numout,*) 'output frequency for STOMATE IPCC history file (d): one month.'
  ELSE
     hist_dt_stom_ipcc = NINT( hist_days_stom_ipcc ) * one_day
     WRITE(numout,*) 'output frequency for STOMATE IPCC history file (d): ', &
          hist_dt_stom_ipcc/one_day
  ENDIF

  ! test consistency between STOMATE_IPCC_HIST_DT and DT_STOMATE parameters
  dt_stomate_loc = one_day
  CALL getin_p('DT_STOMATE', dt_stomate_loc)
  IF ( hist_days_stom_ipcc > zero ) THEN
     IF (dt_stomate_loc > hist_dt_stom_ipcc) THEN
        WRITE(numout,*) "DT_STOMATE = ",dt_stomate_loc,"  , STOMATE_IPCC_HIST_DT = ",hist_dt_stom_ipcc
        CALL ipslerr_p (3,'intsurf_history', &
             &          'Problem with DT_STOMATE > STOMATE_IPCC_HIST_DT','', &
             &          '(must be less or equal)')
     ENDIF
  ENDIF

  IF ( hist_dt_stom_ipcc == 0 ) THEN
     hist_id_stom_ipcc = -1
  ELSE
     !-
     !- initialize
     IF ( rectilinear ) THEN
        CALL histbeg(stom_ipcc_histname, iim, lon_rect, jjm, lat_rect,  1, iim, 1, jjm, &
             &     itau_dep, date0, dt, hori_id, hist_id_stom_ipcc,domain_id=orch_domain_id)
     ELSE
        CALL histbeg(stom_ipcc_histname, iim, lon, jjm, lat,  1, iim, 1, jjm, &
             &     itau_dep, date0, dt, hori_id, hist_id_stom_ipcc,domain_id=orch_domain_id)
     ENDIF
     !- declare this axis
     CALL histvert (hist_id_stom_IPCC, 'PFT', 'Plant functional type', &
          & '1', nvm, hist_PFTaxis, hist_IPCC_PFTaxis_id)

     !- define STOMATE history file
     CALL ioipslctrl_histstomipcc(hist_id_stom_IPCC, nvm, iim, jjm, &
          & dt, hist_dt_stom_ipcc, hori_id, hist_IPCC_PFTaxis_id)

     !- end definition
     CALL histend(hist_id_stom_IPCC)

  ENDIF
  !
  CALL histwrite_p(hist_id_stom, 'Areas',  itau_dep+itau_step, area, kjpindex, indices)
  IF ( hist_id_stom_IPCC > 0 ) THEN
     CALL histwrite_p(hist_id_stom_IPCC, 'Areas',  itau_dep+itau_step, area, kjpindex, indices)
  ENDIF
  !
  hist_id_sec = -1
  hist_id_sec2 = -1
  !-
  ! 3. first call of slowproc to initialize variables
  !-
  itau = itau_dep
  !  
  DO ji=1,kjpindex
     DO jv=1,nvm
        indexveg((jv-1)*kjpindex + ji) = indices(ji) + (jv-1)*kjpij
     ENDDO
  ENDDO
  !-
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
  CALL getin_p ("DEPTH_MAX_H", zmaxh)
  !
  !MM Problem here with dpu which depends on soil type           
  DO l = 1, nbdl-1
     ! first 2.0 is dpu 
     ! second 2.0 is average
     diaglev(l) = zmaxh/(2**(nbdl-1) -1) * ( ( 2**(l-1) -1) + ( 2**(l) -1) ) / 2.0
  ENDDO
  diaglev(nbdl) = zmaxh
  !
!-
! Read the parameters in the "*.def" files
!-
  !
  !Config Key   = CLAYFRACTION_DEFAULT
  !Config Desc  = 
  !Config If    = OK_SECHIBA 
  !Config Def   = 0.2 
  !Config Help  = 
  !Config Units = [-]
  CALL getin_p('CLAYFRACTION_DEFAULT',clayfraction_default)
  !
  !Config Key   = MIN_VEGFRAC 
  !Config Desc  = Minimal fraction of mesh a vegetation type can occupy 
  !Config If    = OK_SECHIBA 
  !Config Def   = 0.001 
  !Config Help  = 
  !Config Units = [-]
  CALL getin_p('MIN_VEGFRAC',min_vegfrac)
  !
  !Config Key   = STEMPDIAG_BID 
  !Config Desc  = only needed for an initial LAI if there is no restart file
  !Config If    = OK_SECHIBA 
  !Config Def   = 280.
  !Config Help  = 
  !Config Units = [K]
  CALL getin_p('STEMPDIAG_BID',stempdiag_bid)
  !
!-
  CALL ioget_expval(val_exp)
  ldrestart_read = .TRUE.
  ldrestart_write = .FALSE.
  !-
  ! read some variables we need from SECHIBA's restart file
  !-
  CALL slowproc_initialize (itau,           kjpij,         kjpindex,            date0,          &
                            indices,        indexveg,      lalo,                neighbours,     &
                            resolution,     contfrac,      soiltile,            reinf_slope,    &
                            t2m,                                                                &
                            deadleaf_cover, assim_param_x, lai_x,               frac_age_x,     &
                            height_x,       veget_x,       frac_nobio,          njsc,           &
                            veget_max_x,    totfrac_nobio, qsintmax_x,          rest_id_sec,    &
                            rest_id_sto,    hist_id_stom,  tot_bare_soil,                       &
                            hist_id_stom_IPCC,   co2_flux, fco2_lu,             temp_growth,    &
                            soilc_total,    thawed_humidity, depth_organic_soil, heat_Zimov)


!  Old interface to slowproc_main, before revision 2581
!  CALL slowproc_main &
! &  (itau, kjpij, kjpindex, dt_force, date0, &
! &   ldrestart_read, ldrestart_write, &
! &   indices, indexveg, lalo, neighbours, resolution, contfrac, soiltile, reinf_slope, &
! &   t2m, t2m_min, temp_sol, soiltemp, &
! &   humrel_x, soilhum, litterhum, precip_rain, precip_snow, gpp_x, &
! &   deadleaf_cover, assim_param_x, lai_x, frac_age_x, height_x, veget_x, &
! &   frac_nobio, njsc, veget_max_x, totfrac_nobio, qsintmax_x, &
! &   rest_id_sec, hist_id_sec, hist_id_sec2, rest_id_sto, hist_id_stom, hist_id_stom_IPCC, co2_flux, fco2_lu, temp_growth)
  ! correct date


  date=1

  CALL intsurf_time( itau_dep+itau_step, date0 )
  l_first_intersurf=.FALSE.
  !-
  ! 4. Time loop
  !⁻
  DO itau = itau_dep+itau_step,itau_fin,itau_step
    !
    CALL intsurf_time( itau, date0 )
    !-
    ! next forcing state
    iisf = iisf+1
    IF (printlev_loc>=3) WRITE(numout,*) "itau,iisf : ",itau,iisf
    !---
    IF (iisf .GT. nsfm) THEN
!-----
!---- we have to read new forcing states from the file
!-----
!---- determine blocks of forcing states that are contiguous in memory
!-----
        CALL stomate_forcing_read(forcing_id,nsfm)

!--------------------------

!-----
!---- determine which forcing states must be read next time
!-----
      isf(1) = isf(nsfm)+1
      IF ( isf(1) .GT. nsft ) isf(1) = 1
        DO iiisf = 2, nsfm
           isf(iiisf) = isf(iiisf-1)+1
           IF ( isf(iiisf) .GT. nsft ) isf(iiisf) = 1
        ENDDO
        nf_written(isf(:)) = .TRUE.
!---- start again at first forcing state
        iisf = 1
     ENDIF
     humrel_x(:,:) = humrel_daily_fm(:,:,iisf)
     litterhum(:) = litterhum_daily_fm(:,iisf)
     t2m(:) = t2m_daily_fm(:,iisf)
     t2m_min(:) = t2m_min_daily_fm(:,iisf)
     temp_sol(:) = tsurf_daily_fm(:,iisf)
     soiltemp(:,:) = tsoil_daily_fm(:,:,iisf)
     soilhum(:,:) = soilhum_daily_fm(:,:,iisf)
     precip_rain(:) = precip_fm(:,iisf)
     gpp_x(:,:) = gpp_daily_fm(:,:,iisf)
     veget_force_x(:,:) = veget_fm(:,:,iisf)
     veget_max_force_x(:,:) = veget_max_fm(:,:,iisf)
     lai_force_x(:,:) = lai_fm(:,:,iisf)
     WHERE ( t2m(:) .LT. ZeroCelsius )
        precip_snow(:) = precip_rain(:)
        precip_rain(:) = zero
     ELSEWHERE
        precip_snow(:) = zero
     ENDWHERE
!---
!-- scale GPP to new lai and veget_max
!---
     WHERE ( lai_x(:,:) .EQ. zero ) gpp_x(:,:) = zero
!-- scale GPP to new LAI
     WHERE (lai_force_x(:,:) .GT. zero )
        gpp_x(:,:) = gpp_x(:,:)*ATAN(2.*lai_x(:,:)) &
 &                           /ATAN( 2.*MAX(lai_force_x(:,:),0.01))
    ENDWHERE
    !- scale GPP to new veget_max
    WHERE (veget_max_force_x(:,:) .GT. zero )
        gpp_x(:,:) = gpp_x(:,:)*veget_max_x(:,:)/veget_max_force_x(:,:)
    ENDWHERE
    !-
    !- number crunching
    !-
     ldrestart_read = .FALSE.
     ldrestart_write = .FALSE.
     CALL slowproc_main &
 &    (itau, kjpij, kjpindex, date0, &
 &     indices, indexveg, lalo, neighbours, resolution, contfrac, soiltile, &
 &     t2m, t2m_min, temp_sol, soiltemp, &
 &     humrel_x, soilhum, litterhum, precip_rain, precip_snow, & 
       !spitfire
 &     wspeed, &
       !endspit
 &     gpp_x, &
 &     deadleaf_cover, assim_param_x, lai_x, frac_age_x, height_x, veget_x, &
 &     frac_nobio, njsc, veget_max_x, totfrac_nobio, qsintmax_x, &
 &     rest_id_sec, hist_id_sec, hist_id_sec2, rest_id_sto, hist_id_stom, hist_id_stom_IPCC, co2_flux, fco2_lu, temp_growth,&
 &   tdeep, hsdeep_long, snow, heat_Zimov, pb, &
 &   sfluxCH4_deep, sfluxCO2_deep, tot_bare_soil, &
 &   thawed_humidity, depth_organic_soil, zz_deep, zz_coef_deep, &
 &   soilc_total,snowdz,snowrho)

  ENDDO ! end of the time loop


!-
! 5. write restart files
!-
  IF (is_root_prc) THEN
! first, read and write variables that are not managed otherwise
     taboo_vars = &
          &  '$lat$ $lon$ $lev$ $veget_year$ '// &
          &  '$height$ $veget$ $veget_max$ $frac_nobio$ '// &
          &  '$lai$ $soiltile_frac$ $clay_frac$ '// &
          &  '$nav_lon$ $nav_lat$ $nav_lev$ $time$ $time_steps$'
!-
     CALL ioget_vname(rest_id_sec, nbvar, varnames)
!-
     DO iv = 1, nbvar
!-- check if the variable is to be written here
        IF (INDEX( taboo_vars,'$'//TRIM(varnames(iv))//'$') .EQ. 0 ) THEN
           IF (printlev_loc>=3) WRITE(numout,*) "restart var : ",TRIM(varnames(iv)),itau_dep,itau_fin

!---- get variable dimensions, especially 3rd dimension
           CALL ioget_vdim &
                &      (rest_id_sec, varnames(iv), varnbdim_max, varnbdim, vardims)
           l1d = ALL(vardims(1:varnbdim) .EQ. 1)
!---- read it
           IF (l1d) THEN
              CALL restget (rest_id_sec,TRIM(varnames(iv)), &
                   1,1,1,itau_dep,.TRUE.,var_1d)
           ELSE
              ALLOCATE(var_3d(nbp_glo,vardims(3)),stat=ier)
              IF (ier .NE. 0) STOP 'ALLOCATION PROBLEM'
              CALL restget (rest_id_sec,TRIM(varnames(iv)), &
                   nbp_glo,vardims(3),1,itau_dep,.TRUE.,var_3d, &
                   "gather",nbp_glo,index_g)
           ENDIF
!---- write it
           IF (l1d) THEN
              CALL restput (rest_id_sec,TRIM(varnames(iv)), &
                   1,1,1,itau_fin,var_1d)
           ELSE
              CALL restput (rest_id_sec,TRIM(varnames(iv)), &
                   nbp_glo,vardims(3),1,itau_fin,var_3d, &
                   'scatter',nbp_glo,index_g)
              DEALLOCATE (var_3d)
           ENDIF
        ENDIF
     ENDDO
  ENDIF
  CALL barrier_para()

! call slowproc to write restart files
  ldrestart_read = .FALSE.
  ldrestart_write = .TRUE.
!-
  IF (printlev_loc>=3) WRITE(numout,*) "Call slowproc for restart."
       CALL slowproc_finalize (itau_fin,   kjpindex,    rest_id_sec, indices,    &
                               njsc,       lai_x,       height_x,    veget_x,    &
                               frac_nobio, veget_max_x, reinf_slope,             &
                               zz_deep, zz_coef_deep, thawed_humidity, depth_organic_soil, &
                               assim_param_x, frac_age_x )
!!$  CALL slowproc_main &
!!$ &  (itau_fin, kjpij, kjpindex, dt_force, date0, &
!!$ &   ldrestart_read, ldrestart_write, &
!!$ &   indices, indexveg, lalo, neighbours, resolution, contfrac, soiltile, reinf_slope, &
!!$ &   t2m, t2m_min, temp_sol, soiltemp, &
!!$ &   humrel_x, soilhum, litterhum, precip_rain, precip_snow, gpp_x, &
!!$ &   deadleaf_cover, assim_param_x, lai_x, frac_age_x, height_x, veget_x, &
!!$ &   frac_nobio, njsc, veget_max_x, totfrac_nobio, qsintmax_x, &
!!$ &   rest_id_sec, hist_id_sec, hist_id_sec2, rest_id_sto, hist_id_stom, hist_id_stom_IPCC, co2_flux, fco2_lu, temp_growth)
!-
! close files
!-
  IF (is_root_prc) THEN
     CALL restclo
     IF ( printlev_loc>=3 )  WRITE(numout,*) 'REST CLOSED'
  ENDIF
  CALL histclo

  IF (is_root_prc) &
       ier = NF90_CLOSE (forcing_id)
  IF (is_root_prc) THEN

     write(*,*) 'teststomate: writing driver restart file with correct time step.'
     dri_restname_in = 'driver_start.nc'
     CALL getin ('RESTART_FILEIN',dri_restname_in)
     dri_restname_out = 'driver_restart.nc'
     CALL getin ('RESTART_FILEOUT',dri_restname_out)
     CALL SYSTEM &
    &  ('cp '//TRIM(dri_restname_in)//' '//TRIM(dri_restname_out))
!-
     iret = NF90_OPEN (TRIM(sec_restname_out),NF90_NOWRITE,ncfid)
     iret = NF90_INQ_VARID (ncfid,'time',v_id)
     iret = NF90_GET_VAR   (ncfid,v_id,r1d)
     time_sec = r1d(1)
     iret = NF90_INQ_VARID (ncfid,'time_steps',v_id)
     iret = NF90_GET_VAR   (ncfid,v_id,time_step_sec)
     iret = NF90_CLOSE (ncfid)
!-
     iret = NF90_OPEN (TRIM(dri_restname_out),NF90_WRITE,ncfid)
     iret = NF90_INQ_VARID (ncfid,'time',v_id)
     iret = NF90_GET_VAR   (ncfid,v_id,r1d)
     time_dri = r1d(1)
     r1d(1) = time_sec
     iret = NF90_PUT_VAR   (ncfid,v_id,r1d)
     iret = NF90_INQ_VARID (ncfid,'time_steps',v_id)
     iret = NF90_GET_VAR   (ncfid,v_id,time_step_dri)
     iret = NF90_PUT_VAR   (ncfid,v_id,time_step_sec)
     iret = NF90_INQ_VARID (ncfid,'julian',v_id)
     iret = NF90_GET_VAR   (ncfid,v_id,r1d)
     julian  = r1d(1)
     djulian = (time_step_sec-time_step_dri)*dtradia/one_day
     julian  = julian &
    &         +djulian-FLOAT(INT((julian+djulian)/one_year))*one_year
     r1d(1) = julian
     iret = NF90_PUT_VAR   (ncfid,v_id,r1d)
     iret = NF90_CLOSE (ncfid)

  ENDIF
  IF (is_root_prc) THEN
     CALL getin_dump()
  ENDIF
  CALL MPI_FINALIZE(ier)
  WRITE(numout,*) "End of teststomate."

!---------------
END PROGRAM teststomate
!
!===
!
