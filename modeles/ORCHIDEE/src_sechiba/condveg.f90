! ===============================================================================================================================
! MODULE       : condveg
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        Initialise, compute and update the surface parameters emissivity,
!! roughness and albedo. 
!!
!! \n DESCRIPTION : The module uses 3 settings to control its flow:\n
!! 1. :: z0cdrag_ave to choose between two methods to calculate the grid average of 
!!    the roughness height. If set to true: the grid average is calculated by the drag coefficients 
!!    per PFT. If set to false: the grid average is calculated by the logarithm of the 
!!    roughness height per PFT.\n
!! 2. :: impaze for choosing surface parameters. If set to false, the values for the 
!!    soil albedo, emissivity and roughness height are set to default values which are read from 
!!    the run.def. If set to true, the user imposes its own values, fixed for the grid point. This is useful if 
!!    one performs site simulations, however, 
!!    it is not recommended to do so for spatialized simulations.
!!     roughheight_scal imposes the roughness height in (m) , 
!!	same for emis_scal (in %), albedo_scal (in %), zo_scal (in m)                       
!!     Note that these values are only used if 'impaze' is true.\n
!! 3. :: alb_bare_model for choosing values of bare soil albedo. If set to TRUE bare 
!!    soil albedo depends on soil wetness. If set to FALSE bare soil albedo is the mean 
!!    values of wet and dry soil albedos.\n
!!   The surface fluxes are calculated between two levels: the atmospheric level reference and the effective roughness height 
!! defined as the difference between the mean height of the vegetation and the displacement height (zero wind 
!!    level). Over bare soils, the zero wind level is equal to the soil roughness. Over vegetation, the zero wind level
!!    is increased by the displacement height
!!    which depends on the height of the vegetation. For a grid point composed of different types of vegetation, 
!! an effective surface roughness has to be calculated

!! RECENT CHANGE(S): None
!!
!! REFERENCES(S)    : None
!!
!! SVN              :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_sechiba/condveg.f90 $
!! $Date: 2016-04-26 13:28:48 +0200 (Tue, 26 Apr 2016) $
!! $Revision: 3386 $
!! \n
!_ ================================================================================================================================

MODULE condveg
  
  USE ioipsl
  USE xios_orchidee
  !
  ! modules used :
  USE constantes
  USE constantes_soil
  USE pft_parameters
  USE qsat_moisture
  USE interpol_help
  USE mod_orchidee_para
  USE ioipsl_para
  USE sechiba_io
  USE grid

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: condveg_main, condveg_initialize, condveg_finalize, condveg_clear 

  !
  ! variables used inside condveg module
  !
  LOGICAL, SAVE                     :: l_first_condveg=.TRUE.           !! To keep first call's trace
!$OMP THREADPRIVATE(l_first_condveg)
  REAL(r_std), ALLOCATABLE, SAVE    :: soilalb_dry(:,:)                 !! Albedo values for the dry bare soil (unitless)
!$OMP THREADPRIVATE(soilalb_dry)
  REAL(r_std), ALLOCATABLE, SAVE    :: soilalb_wet(:,:)                 !! Albedo values for the wet bare soil (unitless)
!$OMP THREADPRIVATE(soilalb_wet)
  REAL(r_std), ALLOCATABLE, SAVE    :: soilalb_moy(:,:)                 !! Albedo values for the mean bare soil (unitless)
!$OMP THREADPRIVATE(soilalb_moy)
  REAL(r_std), ALLOCATABLE, SAVE    :: soilalb_bg(:,:,:)                !! Albedo values for the background bare soil (unitless)
!$OMP THREADPRIVATE(soilalb_bg)
  REAL(r_std), ALLOCATABLE, SAVE    :: alb_bare(:,:)                    !! Mean bare soil albedo for visible and near-infrared 
                                                                        !! range (unitless) 
!$OMP THREADPRIVATE(alb_bare)
  REAL(r_std), ALLOCATABLE, SAVE    :: alb_veget(:,:)                   !! Mean vegetation albedo for visible and 
                                                                        !! near-infrared range (unitless) 
!$OMP THREADPRIVATE(alb_veget)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)  :: albedo_snow         !! Mean snow albedo over visible and near-infrared 
                                                                        !! range (unitless) 
!$OMP THREADPRIVATE(albedo_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)  :: albedo_glob         !! Mean surface albedo over visible and 
                                                                        !! near-infrared range (unitless)
!$OMP THREADPRIVATE(albedo_glob)
CONTAINS

!!
!============================================================================================================================= 
!! SUBROUTINE                             : condveg_initialize 
!! 
!>\BRIEF                                  Allocate module variables, read from
!restart file or initialize with default values 
!! 
!! DESCRIPTION                            : Allocate module variables, read from
!restart file or initialize with default values. 
!!                                          condveg_snow is called to initialize
!corresponding variables. 
!! 
!! RECENT CHANGE(S)                       : None 
!! 
!! MAIN OUTPUT VARIABLE(S) 
!! 
!! REFERENCE(S)                           : None 
!!  
!! FLOWCHART                              : None 
!! \n 
!_
!==============================================================================================================================
  SUBROUTINE condveg_initialize (kjit, kjpindex, index, veget, &
               veget_max, frac_nobio, totfrac_nobio, &
               lalo, neighbours, resolution, contfrac, rest_id, &
               zlev, drysoil_frac, height, snowdz, snowrho, tot_bare_soil, &
               snow, snow_age, snow_nobio, snow_nobio_age, &
               emis, albedo,   z0,         roughheight, roughheight_pft, &
              frac_snow_veg, frac_snow_nobio)

    !! 0. Variable and parameter declaration

    !! 0.1. Input variables  

    INTEGER(i_std), INTENT(in)                       :: kjit             !! Time step number (unitless)          
    INTEGER(i_std), INTENT(in)                       :: kjpindex         !! Domain size - Number of land pixels  (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in) :: index            !! Index for the points on the map (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in):: veget            !! PFT coverage fraction of a PFT (= ind*cn_ind) 
                                                                         !! (m^2 m^{-2}) 
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (in)  :: lalo             !! Geographical coordinates (degree)
    INTEGER(i_std),DIMENSION (kjpindex,8), INTENT(in):: neighbours       !! Neighbouring land grid cell
    REAL(r_std), DIMENSION (kjpindex,2), INTENT(in)  :: resolution       !! Size of grid in x and y direction (m)
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: contfrac         !! Fraction of land in each grid box 
    INTEGER(i_std), INTENT(in)                       :: rest_id          !! Restart file identifier 

    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in) :: veget_max        !! Fraction of vegetation type
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: frac_nobio    !! Fraction of continental ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: totfrac_nobio    !! total fraction of continental ice+lakes+...
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)    :: zlev             !! Height of first layer
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: drysoil_frac     !! Fraction of visibly Dry soil(between 0 and 1)
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in):: snowdz           !! Snow depth at each snow layer
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in):: snowrho          !! Snow density at each snow layer
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in) :: height           !! Vegetation Height (m)

    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: snow             !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: snow_age         !! Snow age
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: snow_nobio    !! Snow mass [Kg/m^2] on ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: snow_nobio_age   !! Snow age on ice, lakes, ...
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)    :: tot_bare_soil    !! Total evaporating bare soil fraction 

    !! 0.2. Output variables
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)   :: emis             !! Emissivity
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (out) :: albedo           !! Albedo, vis(1) and nir(2)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)   :: z0               !! Soil roughness height (m)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)   :: roughheight      !! Effective height for roughness
    REAL(r_std),DIMENSION (kjpindex, nvm), INTENT (out)   :: roughheight_pft 
    REAL(r_std),DIMENSION (kjpindex), INTENT(out)    :: frac_snow_veg    !! Snow cover fraction on vegeted area
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(out):: frac_snow_nobio  !! Snow cover fraction on non-vegeted area
  
    !! 0.3 Modified variables          
  
    !! 0.4 Local variables
    INTEGER(i_std)                                  :: ji, jv           !! Index
    INTEGER(i_std)                                  :: ier              !! Check errors in memory allocation
    CHARACTER(LEN=80)                               :: var_name         !! To store variables names for I/O

!_ ================================================================================================================================
  
    !! 1. Choice of calculation of snow albedo and soil albedo
    IF (.NOT. l_first_condveg) CALL ipslerr_p(3,'condveg_initialize','Error: initialization already done','','')
    l_first_condveg=.FALSE.

    !! 2. Allocate all albedo variables
       
    ! Dry soil albedo
    ALLOCATE (soilalb_dry(kjpindex,2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for soilalb_dry','','')
    soilalb_dry(:,:) = val_exp

    ! Wet soil albedo
    ALLOCATE (soilalb_wet(kjpindex,2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for soilalb_wet','','')
    soilalb_wet(:,:) = val_exp

    ! Mean soil albedo
    ALLOCATE (soilalb_moy(kjpindex,2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for soilalb_moy','','')
    soilalb_moy(:,:) = val_exp

    ! Background soil albedo
    ALLOCATE (soilalb_bg(kjpindex,2,12),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_initialize','Pb in allocation for soilalb_bg','','')

    ! Snow albedo of vegetative PFTs
    ALLOCATE (albedo_snow(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for albedo_snow','','')
       
    ! Mean vegetation albedo over visible and near-infrared range 
    ALLOCATE (albedo_glob(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for albedo_glob','','')
       
    ! Mean bare soil albedo
    ALLOCATE (alb_bare(kjpindex,2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for alb_bare','','')
    alb_bare(:,:) = val_exp

    ! Mean vegetation albedo
    ALLOCATE (alb_veget(kjpindex,2),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'condveg_init','Pb in allocation for alb_veget','','')
    alb_veget(:,:) = val_exp

       
    !! 3. Initialise bare soil albedo
    ! dry soil albedo
    var_name= 'soilalbedo_dry'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Dry bare soil albedo')
    CALL restget_p (rest_id, var_name, nbp_glo, 2, 1, kjit, .TRUE., soilalb_dry, "gather", nbp_glo, index_g)

    ! wet soil albedo
    var_name= 'soilalbedo_wet'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Wet bare soil albedo')
    CALL restget_p (rest_id, var_name, nbp_glo, 2, 1, kjit, .TRUE., soilalb_wet, "gather", nbp_glo, index_g)

    ! mean soil aledo
    var_name= 'soilalbedo_moy'
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Mean bare soil albedo')
    CALL restget_p (rest_id, var_name, nbp_glo, 2, 1, kjit, .TRUE., soilalb_moy, "gather", nbp_glo, index_g)

    ! background albedo map
    IF ( alb_bg_modis ) THEN
       ! Read background albedo from restart file
       CALL ioconf_setatt_p('UNITS', '-')
       CALL ioconf_setatt_p('LONG_NAME','Background soil albedo in visible interval')
       CALL restget_p (rest_id, 'soilalbedo_bg_1', nbp_glo, 12, 1, kjit, .TRUE., soilalb_bg(:,1,:), "gather", nbp_glo, index_g)

       CALL ioconf_setatt_p('UNITS', '-')
       CALL ioconf_setatt_p('LONG_NAME','Background soil albedo in infra-red interval')
       CALL restget_p (rest_id, 'soilalbedo_bg_2', nbp_glo, 12, 1, kjit, .TRUE., soilalb_bg(:,2,:), "gather", nbp_glo, index_g)

       IF ( ALL(soilalb_bg(:,:,:) == val_exp) ) THEN
          ! Variable not found in restart file. Read and interpolate it from file.
          CALL condveg_background_soilalb(kjpindex, lalo, neighbours, resolution, contfrac)
       END IF
    ENDIF

    ! z0
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Roughness')
    CALL restget_p (rest_id, 'z0', nbp_glo, 1, 1, kjit, .TRUE., z0, "gather", nbp_glo, index_g)

    ! roughness height
    CALL ioconf_setatt_p('UNITS', '-')
    CALL ioconf_setatt_p('LONG_NAME','Roughness height')
    CALL restget_p (rest_id, 'roughheight', nbp_glo, 1, 1, kjit, .TRUE., roughheight, "gather", nbp_glo, index_g)
    CALL restget_p (rest_id, 'roughheight_pft', nbp_glo, nvm, 1, kjit, .TRUE., roughheight_pft, "gather", nbp_glo, index_g)
       
    ! check if we have real values and not only missing values
    IF ( MINVAL(soilalb_wet) .EQ. MAXVAL(soilalb_wet) .AND. MAXVAL(soilalb_wet) .EQ. val_exp .OR.&
         & MINVAL(soilalb_dry) .EQ. MAXVAL(soilalb_dry) .AND. MAXVAL(soilalb_dry) .EQ. val_exp .OR.&
         & MINVAL(soilalb_moy) .EQ. MAXVAL(soilalb_moy) .AND. MAXVAL(soilalb_moy) .EQ. val_exp) THEN
       CALL condveg_soilalb(kjpindex, lalo, neighbours, resolution, contfrac)
       WRITE(numout,*) '---> val_exp ', val_exp
       WRITE(numout,*) '---> ALBEDO_wet VIS:', MINVAL(soilalb_wet(:,ivis)), MAXVAL(soilalb_wet(:,ivis))
       WRITE(numout,*) '---> ALBEDO_wet NIR:', MINVAL(soilalb_wet(:,inir)), MAXVAL(soilalb_wet(:,inir))
       WRITE(numout,*) '---> ALBEDO_dry VIS:', MINVAL(soilalb_dry(:,ivis)), MAXVAL(soilalb_dry(:,ivis))
       WRITE(numout,*) '---> ALBEDO_dry NIR:', MINVAL(soilalb_dry(:,inir)), MAXVAL(soilalb_dry(:,inir))
       WRITE(numout,*) '---> ALBEDO_moy VIS:', MINVAL(soilalb_moy(:,ivis)), MAXVAL(soilalb_moy(:,ivis))
       WRITE(numout,*) '---> ALBEDO_moy NIR:', MINVAL(soilalb_moy(:,inir)), MAXVAL(soilalb_moy(:,inir))
    ENDIF

   !! 2. Calculate emissivity

    ! If TRUE read in default values for emissivity
    IF ( impaze ) THEN
       !
       emis(:) = emis_scal
       !
    ! If FALSE set emissivity to 1.
    ELSE
       emis_scal = un
       emis(:) = emis_scal
    ENDIF

    !! 3. Calculate albedo

    ! If TRUE read in default values for albedo
    IF ( impaze ) THEN
       !
       albedo(:,ivis) = albedo_scal(ivis)
       albedo(:,inir) = albedo_scal(inir)
       !
    ELSE

       CALL condveg_albcalc (kjpindex,veget,drysoil_frac,tot_bare_soil,albedo)
 
    ENDIF

    !! 5. Calculate roughness height if it was not found in the restart file
    IF ( ALL(z0(:) == val_exp) .OR. ALL(roughheight(:) == val_exp)) THEN
       !! Calculate roughness height
       ! Chooses between two methods to calculate the grid average of the roughness.
       ! If impaze set to true:  The grid average is calculated by averaging the drag coefficients over PFT.
       ! If impaze set to false: The grid average is calculated by averaging the logarithm of the roughness length per PFT.
       IF ( impaze ) THEN
          ! Use parameter CONDVEG_Z0 and ROUGHHEIGHT from run.def
          z0(:) = z0_scal
          roughheight(:) = roughheight_scal
          roughheight_pft(:,:) = roughheight_scal
       ELSE
          ! Caluculate roughness height
          IF ( z0cdrag_ave ) THEN
             CALL condveg_z0cdrag(kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, zlev, &
                  height, tot_bare_soil, z0, roughheight, roughheight_pft)
          ELSE
             CALL condveg_z0logz(kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, height, tot_bare_soil, &
                  z0, roughheight, roughheight_pft)
          ENDIF
       ENDIF
    END IF

    ! Call subroutine 'condveg snow'
    CALL condveg_snow (kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, &
        snow, snow_age, snow_nobio, snow_nobio_age, snowdz, snowrho, tot_bare_soil, &
        albedo, albedo_snow, albedo_glob, frac_snow_veg,  frac_snow_nobio)

    IF (printlev>=3) WRITE (numout,*) 'condveg_initialize done ' 

  END SUBROUTINE condveg_initialize

!! ==============================================================================================================================
!! SUBROUTINE   : condveg_main
!!
!>\BRIEF        Calls the subroutines to initialise the variables, update the variables
!! and write out data and restart files. 
!!
!!
!! MAIN OUTPUT VARIABLE(S):  emis (emissivity), albedo (albedo of 
!! vegetative PFTs in visible and near-infrared range), z0 (surface roughness height),
!! roughheight (grid effective roughness height), soil type (fraction of soil types) 
!! 
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!!
!! REVISION(S)  : None
!!
!_ ================================================================================================================================

  SUBROUTINE condveg_main (kjit, kjpindex, index,&
       & lalo, neighbours, resolution, contfrac, veget, veget_max, frac_nobio, totfrac_nobio, &
       & zlev, snow, snow_age, snow_nobio, snow_nobio_age, tot_bare_soil, &
       & drysoil_frac, height,  snowdz, snowrho, emis, albedo, &
       & frac_snow_veg, frac_snow_nobio, &
       & z0, roughheight, roughheight_pft, rest_id, hist_id, hist2_id) 

     !! 0. Variable and parameter declaration

    !! 0.1 Input variables  

    INTEGER(i_std), INTENT(in)                       :: kjit             !! Time step number 
    INTEGER(i_std), INTENT(in)                       :: kjpindex         !! Domain size
    INTEGER(i_std),INTENT (in)                       :: rest_id          !! _Restart_ file identifier
    INTEGER(i_std),INTENT (in)                       :: hist_id          !! _History_ file identifier
    INTEGER(i_std), OPTIONAL, INTENT (in)            :: hist2_id          !! _History_ file 2 identifier
    ! input fields
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in) :: index            !! Indeces of the points on the map
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (in)  :: lalo             !! Geographical coordinates
    INTEGER(i_std),DIMENSION (kjpindex,8), INTENT(in):: neighbours       !! neighoring grid points if land
    REAL(r_std), DIMENSION (kjpindex,2), INTENT(in)  :: resolution       !! size in x an y of the grid (m)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)    :: contfrac         ! Fraction of land in each grid box.
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in) :: veget            !! Fraction of vegetation types
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in) :: veget_max        !! Fraction of vegetation type
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: frac_nobio    !! Fraction of continental ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: totfrac_nobio    !! total fraction of continental ice+lakes+...
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)    :: zlev             !! Height of first layer
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: snow             !! Snow mass [Kg/m^2]
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: snow_age         !! Snow age
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: snow_nobio    !! Snow mass [Kg/m^2] on ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: snow_nobio_age   !! Snow age on ice, lakes, ...
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)     :: drysoil_frac     !! Fraction of visibly Dry soil(between 0 and 1)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in) :: height           !! Vegetation Height (m)
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in):: snowdz           !! Snow depth at each snow layer
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in):: snowrho          !! Snow density at each snow layer
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)    :: tot_bare_soil    !! Total evaporating bare soil fraction 
    REAL(r_std),DIMENSION (kjpindex), INTENT(out)    :: frac_snow_veg    !! Snow cover fraction on vegeted area
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(out):: frac_snow_nobio  !! Snow cover fraction on non-vegeted area

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)   :: emis             !! Emissivity
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (out) :: albedo           !! Albedo, vis(1) and nir(2)
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)   :: z0               !! Roughness
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)   :: roughheight      !! Effective height for roughness
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)   :: roughheight_pft    

    !! 0.3 Modified variables

    !! 0.4 Local variables   

    CHARACTER(LEN=80)                               :: var_name         !! To store variables names for I/O
!_ ================================================================================================================================
  
    !! 3. Call subroutines to update fields

    ! Call the routine 'condveg_var_update' to update the fields of albedo, emissivity and roughness
    CALL condveg_var_update (kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, &
          drysoil_frac, zlev, height, tot_bare_soil, emis, albedo, z0, roughheight, roughheight_pft)

    ! Update snow parameters by calling subroutine 'condveg_snow'
    CALL condveg_snow (kjpindex,        veget,      veget_max,      frac_nobio, &
                       totfrac_nobio,   snow,       snow_age,       snow_nobio, & 
                       snow_nobio_age,  snowdz,     snowrho,        tot_bare_soil, albedo, albedo_snow, & 
                       albedo_glob,     frac_snow_veg, frac_snow_nobio)

    ! If this logical flag is set to true, the model
    ! will output all its data according to the ALMA 
    ! convention. 
    ! To facilitate the exchange of forcing data for land-surface schemes and the results produced by these schemes, 
    ! ALMA (Assistance for Land-surface Modelling activities) proposes to establish standards. 
    ! http://www.lmd.jussieu.fr/~polcher/ALMA/
    CALL xios_orchidee_send_field("soilalb_vis",alb_bare(:,1))
    CALL xios_orchidee_send_field("soilalb_nir",alb_bare(:,2))
    CALL xios_orchidee_send_field("vegalb_vis",alb_veget(:,1))
    CALL xios_orchidee_send_field("vegalb_nir",alb_veget(:,2))
    CALL xios_orchidee_send_field("albedo_alma",albedo_glob)
    CALL xios_orchidee_send_field("SAlbedo",albedo_snow)
    
    IF ( almaoutput ) THEN
       CALL histwrite_p(hist_id, 'Albedo', kjit, albedo_glob, kjpindex, index)
       CALL histwrite_p(hist_id, 'SAlbedo', kjit, albedo_snow, kjpindex, index)
       IF ( hist2_id > 0 ) THEN
          CALL histwrite_p(hist2_id, 'Albedo', kjit, albedo_glob, kjpindex, index)
          CALL histwrite_p(hist2_id, 'SAlbedo', kjit, albedo_snow, kjpindex, index)
       ENDIF
    ELSE
       CALL histwrite_p(hist_id, 'soilalb_vis', kjit, alb_bare(:,1), kjpindex, index)
       CALL histwrite_p(hist_id, 'soilalb_nir', kjit, alb_bare(:,2), kjpindex, index)
       CALL histwrite_p(hist_id, 'vegalb_vis',  kjit, alb_veget(:,1), kjpindex, index)
       CALL histwrite_p(hist_id, 'vegalb_nir',  kjit, alb_veget(:,2), kjpindex, index)
       IF ( hist2_id > 0 ) THEN
          CALL histwrite_p(hist2_id, 'soilalb_vis', kjit, alb_bare(:,1), kjpindex, index)
          CALL histwrite_p(hist2_id, 'soilalb_nir', kjit, alb_bare(:,2), kjpindex, index)
          CALL histwrite_p(hist2_id, 'vegalb_vis',  kjit, alb_veget(:,1), kjpindex, index)
          CALL histwrite_p(hist2_id, 'vegalb_nir',  kjit, alb_veget(:,2), kjpindex, index)
       ENDIF
    ENDIF

    IF (printlev>=3) WRITE (numout,*)' condveg_main done '

  END SUBROUTINE condveg_main

!!
!=============================================================================================================================
!! SUBROUTINE                             : condveg_finalize
!!
!>\BRIEF                                    Write to restart file
!!
!! DESCRIPTION                            : This subroutine writes the module
!variables and variables calculated in condveg
!!                                          to restart file
!!
!! RECENT CHANGE(S)                       : None
!!
!! REFERENCE(S)                           : None
!!
!! FLOWCHART                              : None
!! \n
!_
!==============================================================================================================================
  SUBROUTINE condveg_finalize (kjit, kjpindex, rest_id, z0, roughheight, roughheight_pft)

    !! 0. Variable and parameter declaration
    !! 0.1 Input variables 
    INTEGER(i_std), INTENT(in)                  :: kjit             !! Time step number
    INTEGER(i_std), INTENT(in)                  :: kjpindex         !! Domain size
    INTEGER(i_std),INTENT (in)                  :: rest_id          !! Restart file identifier
    REAL(r_std),DIMENSION(kjpindex), INTENT(in) :: z0               !! Roughness
    REAL(r_std),DIMENSION(kjpindex), INTENT(in) :: roughheight      !! Grid effective roughness height (m)     
    REAL(r_std),DIMENSION(kjpindex,nvm), INTENT(in) :: roughheight_pft    
    
    !_ ================================================================================================================================
    
    CALL restput_p (rest_id, 'soilalbedo_dry', nbp_glo, 2, 1, kjit, soilalb_dry, 'scatter',  nbp_glo, index_g)
    !
    CALL restput_p (rest_id, 'soilalbedo_wet', nbp_glo, 2, 1, kjit, soilalb_wet, 'scatter',  nbp_glo, index_g)
    !
    CALL restput_p (rest_id, 'soilalbedo_moy', nbp_glo, 2, 1, kjit, soilalb_moy, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'z0', nbp_glo, 1, 1, kjit, z0, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'roughheight', nbp_glo, 1, 1, kjit, roughheight, 'scatter',  nbp_glo, index_g)
    CALL restput_p (rest_id, 'roughheight_pft', nbp_glo, nvm, 1, kjit, roughheight_pft, 'scatter', nbp_glo, index_g)
    
    IF ( alb_bg_modis ) THEN
       CALL restput_p (rest_id, 'soilalbedo_bg_1', nbp_glo, 12, 1, kjit, soilalb_bg(:,1,:), 'scatter',  nbp_glo, index_g)
       CALL restput_p (rest_id, 'soilalbedo_bg_2', nbp_glo, 12, 1, kjit, soilalb_bg(:,2,:), 'scatter',  nbp_glo, index_g)
    END IF
  END SUBROUTINE condveg_finalize

!! ==============================================================================================================================
!! SUBROUTINE 	: condveg_clear
!!
!>\BRIEF        Deallocate albedo variables
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): None 
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE condveg_clear  ()

      l_first_condveg=.TRUE.
       
      ! Dry soil albedo
       IF (ALLOCATED (soilalb_dry)) DEALLOCATE (soilalb_dry)
       ! Wet soil albedo
       IF (ALLOCATED(soilalb_wet))  DEALLOCATE (soilalb_wet)
       ! Mean soil albedo
       IF (ALLOCATED(soilalb_moy))  DEALLOCATE (soilalb_moy)
       ! BG soil albedo
       IF (ALLOCATED(soilalb_bg))  DEALLOCATE (soilalb_bg)
       ! Mean snow albedo
       IF (ALLOCATED(albedo_snow))  DEALLOCATE (albedo_snow)
       ! Mean albedo
       IF (ALLOCATED(albedo_glob))  DEALLOCATE (albedo_glob)
       ! Mean albedo of bare soil
       IF (ALLOCATED(alb_bare))  DEALLOCATE (alb_bare)
       ! Mean vegetation albedo
       IF (ALLOCATED(alb_veget))  DEALLOCATE (alb_veget)

  END SUBROUTINE condveg_clear

  

!! ==============================================================================================================================
!! SUBROUTINE   : condveg_var_update
!!
!>\BRIEF        Update emissivity, albedo and roughness height.
!!
!! DESCRIPTION  : None
!!
!! MAIN OUTPUT VARIABLE(S): \n
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE condveg_var_update  (kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, &
       & drysoil_frac, zlev, height, tot_bare_soil, emis, albedo, z0, roughheight, roughheight_pft)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                          :: kjpindex         !! Domain size - Number of land pixels  (unitless) 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in)    :: veget            !! PFT coverage fraction of a PFT (= ind*cn_ind)
                                                                            !! (m^2 m^{-2})  
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in)    :: veget_max        !! PFT "Maximal" coverage fraction of a PFT 
                                                                            !! (= ind*cn_ind) (m^2 m^{-2})  
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: frac_nobio       !! Fraction of non-vegetative surfaces, i.e. 
                                                                            !! continental ice, lakes, etc. (unitless)  
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)        :: totfrac_nobio    !! Total fraction of non-vegetative surfaces, i.e. 
                                                                            !! continental ice, lakes, etc. (unitless)   
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)        :: drysoil_frac     !! Fraction of dry soil in visible range (unitless)   
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: zlev             !! Height of first layer (m)       
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in)    :: height           !! Vegetation height (m)         
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)        :: tot_bare_soil    !! Total evaporating bare soil fraction 

    !! 0.2 Output variables   
       
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: emis             !! Emissivity (unitless)            
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (out)    :: albedo           !! Albedo of vegetative PFTs in visible and 
                                                                            !! near-infrared range  
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: z0               !! Roughness height (m)              
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: roughheight      !! Grid effective roughness height (m)     
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (out)  :: roughheight_pft 
    
    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                      :: ji, jv           !! Indeces
!_ ================================================================================================================================

    !! 1. Calculate emissivity
    
    emis(:) = emis_scal
    
    !! 2. Calculate albedo
    
    ! If TRUE read in prescribed values for albedo
    IF ( impaze ) THEN

       albedo(:,ivis) = albedo_scal(ivis)
       albedo(:,inir) = albedo_scal(inir)

    ! If FALSE calculate albedo from ORCHIDEE default values
    ELSE

       CALL condveg_albcalc (kjpindex, veget, drysoil_frac, tot_bare_soil, albedo)

    ENDIF
    
    !! 3. Calculate roughness height
    
    ! If TRUE read in prescribed values for roughness height
    IF ( impaze ) THEN

       DO ji = 1, kjpindex
         z0(ji) = z0_scal
         roughheight(ji) = roughheight_scal
         roughheight_pft(ji,:) = roughheight_scal
      ENDDO

    ! Calculate roughness height
    ELSE
     
       IF ( z0cdrag_ave ) THEN
          CALL condveg_z0cdrag (kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, zlev, height, tot_bare_soil, &
               &                z0, roughheight, roughheight_pft)
       ELSE
          CALL condveg_z0logz (kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, height, tot_bare_soil, &
               &               z0, roughheight,  roughheight_pft)
       ENDIF
     
    ENDIF

    IF (printlev>=3) WRITE (numout,*) ' condveg_var_update done '

  END SUBROUTINE condveg_var_update


!! ==============================================================================================================================\n
!! SUBROUTINE   : condveg_snow
!!
!>\BRIEF        Calcuating snow albedo and snow cover fraction, which are then used to 
!! update the gridbox surface albedo following Chalita and Treut (1994).
!!
!! DESCRIPTION  : The snow albedo scheme presented below belongs to prognostic albedo 
!! category, i.e. the snow albedo value at a time step depends on the snow albedo value 
!! at the previous time step.
!!
!! First, the following formula (described in Chalita and Treut 1994) is used to describe 
!! the change in snow albedo with snow age on each PFT and each non-vegetative surfaces, 
!! i.e. continental ice, lakes, etc.: \n 
!! \latexonly 
!! \input{SnowAlbedo.tex}
!! \endlatexonly
!! \n
!! Where snowAge is snow age, tcstSnowa is a critical aging time (tcstSnowa=5 days)
!! snowaIni and snowaIni+snowaDec corresponds to albedos measured for aged and
!! fresh snow respectively, and their values for each PFT and each non-vegetative surfaces 
!! is precribed in in constantes_veg.f90.\n
!! In order to estimate gridbox snow albedo, snow albedo values for each PFT and 
!! each  non-vegetative surfaces with a grid box are weightedly summed up by their 
!! respective fractions.\n
!! Secondly, the snow cover fraction is computed as:
!! \latexonly 
!! \input{SnowFraction.tex}
!! \endlatexonly
!! \n
!! Where fracSnow is the fraction of snow on total vegetative or total non-vegetative
!! surfaces, snow is snow mass (kg/m^2) on total vegetated or total nobio surfaces.\n 
!! Finally, the surface albedo is then updated as the weighted sum of fracSnow, total
!! vegetated fraction, total nobio fraction, gridbox snow albedo, and previous
!! time step surface albedo.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): :: albedo; surface albedo. :: albedo_snow; snow
!! albedo
!!
!! REFERENCE(S) :  
!! Chalita, S. and H Le Treut (1994), The albedo of temperate and boreal forest and 
!!  the Northern Hemisphere climate: a sensitivity experiment using the LMD GCM,
!!  Climate Dynamics, 10 231-240.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE condveg_snow  (kjpindex,       veget,      veget_max,      frac_nobio,  &
                            totfrac_nobio,  snow,       snow_age,       snow_nobio,  &
                            snow_nobio_age, snowdz,     snowrho,     &
                            tot_bare_soil,  albedo,     albedo_snow, &
                            albedo_glob,    frac_snow_veg, frac_snow_nobio)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                          :: kjpindex        !! Domain size - Number of land pixels  (unitless)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)   :: veget           !! PFT coverage fraction of a PFT (= ind*cn_ind) 
                                                                           !! (m^2 m^{-2})   
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT(in)    :: veget_max
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: frac_nobio      !! Fraction of non-vegetative surfaces, i.e. 
                                                                           !! continental ice, lakes, etc. (unitless)     
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)        :: totfrac_nobio   !! Total fraction of non-vegetative surfaces, i.e. 
                                                                           !! continental ice, lakes, etc. (unitless)   
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)        :: snow            !! Snow mass in vegetation (kg m^{-2})           
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: snow_nobio      !! Snow mass on continental ice, lakes, etc. (kg m^{-2})      
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)        :: snow_age        !! Snow age (days)        
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in) :: snow_nobio_age  !! Snow age on continental ice, lakes, etc. (days)    
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in)   :: snowdz          !! Snow depth at each snow layer
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in)   :: snowrho         !! Snow density at each snow layer
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: tot_bare_soil   !! Total evaporating bare soil fraction 

    !! 0.2 Output variables
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: albedo_snow     !! Snow albedo (unitless ratio)     
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)      :: albedo_glob     !! Mean albedo (unitless ratio)     
    REAL(r_std), DIMENSION(kjpindex), INTENT(out)       :: frac_snow_veg   !! Fraction of snow on vegetation (unitless ratio)
    REAL(r_std), DIMENSION(kjpindex,nnobio), INTENT(out):: frac_snow_nobio !! Fraction of snow on continental ice, lakes, etc.

    !! 0.3 Modified variables
    REAL(r_std),DIMENSION (kjpindex,2), INTENT (inout)  :: albedo          !! Albedo (unitless ratio)          

    !! 0.4 Local variables
    INTEGER(i_std)                                      :: ji, jv, jb      !! indices (unitless)
    REAL(r_std), DIMENSION(kjpindex)                    :: snowa_veg       !! Albedo of snow covered area on vegetation
                                                                           !! (unitless ratio)
    REAL(r_std), DIMENSION(kjpindex,nnobio)             :: snowa_nobio     !! Albedo of snow covered area on continental ice, 
                                                                           !! lakes, etc. (unitless ratio)     
    REAL(r_std), DIMENSION(kjpindex)                    :: fraction_veg    !! Total vegetation fraction (unitless ratio)
    REAL(r_std), DIMENSION(kjpindex)                    :: agefunc_veg     !! Age dependency of snow albedo on vegetation 
                                                                           !! (unitless)
    REAL(r_std), DIMENSION(kjpindex,nnobio)             :: agefunc_nobio   !! Age dependency of snow albedo on ice, 
                                                                           !! lakes, .. (unitless)
    REAL(r_std)                                         :: alb_nobio       !! Albedo of continental ice, lakes, etc. 
                                                                           !!(unitless ratio)
    REAL(r_std), DIMENSION(kjpindex)                    :: snowrho_ave     !! Average snow density
    REAL(r_std), DIMENSION(kjpindex)                    :: snowdepth       !! Snow depth
!_ ================================================================================================================================

    !! 2. Calculate snow albedos on both total vegetated and total nobio surfaces
 
    ! The snow albedo could be either prescribed (in condveg_init.f90) or 
    !  calculated following Chalita and Treut (1994).
    ! Check if the precribed value fixed_snow_albedo exists
    IF (ABS(fixed_snow_albedo - undef_sechiba) .GT. EPSILON(undef_sechiba)) THEN
      snowa_veg(:) = fixed_snow_albedo
      snowa_nobio(:,:) = fixed_snow_albedo
    ELSE ! calculated following Chalita and Treut (1994)
      
      !! 2.1 Calculate age dependence

      ! On vegetated surfaces
      DO ji = 1, kjpindex
        agefunc_veg(ji) = EXP(-snow_age(ji)/tcst_snowa)
      ENDDO

      ! On non-vegtative surfaces
      DO jv = 1, nnobio ! Loop over # nobio types
        DO ji = 1, kjpindex
          agefunc_nobio(ji,jv) = EXP(-snow_nobio_age(ji,jv)/tcst_snowa)
        ENDDO
      ENDDO

      !! 2.1 Calculate snow albedo 

      ! For  vegetated surfaces
      fraction_veg(:) = un - totfrac_nobio(:)
      snowa_veg(:) = zero
      IF (ok_dgvm) THEN
         DO ji = 1, kjpindex
            IF ( fraction_veg(ji) .GT. min_sechiba ) THEN
!MM veget(:,1) BUG ??!!!!!!!!!!!
               snowa_veg(ji) = snowa_veg(ji) + &
                    tot_bare_soil(ji)/fraction_veg(ji) * ( snowa_aged(1)+snowa_dec(1)*agefunc_veg(ji) )
            ENDIF
         ENDDO

         DO jv = 2, nvm
            DO ji = 1, kjpindex
               IF ( fraction_veg(ji) .GT. min_sechiba ) THEN
                  snowa_veg(ji) = snowa_veg(ji) + &
                       veget(ji,jv)/fraction_veg(ji) * ( snowa_aged(jv)+snowa_dec(jv)*agefunc_veg(ji) )
               ENDIF
            ENDDO
         ENDDO
      ELSE
         DO jv = 1, nvm
            DO ji = 1, kjpindex
               IF ( fraction_veg(ji) .GT. min_sechiba ) THEN
                  snowa_veg(ji) = snowa_veg(ji) + &
                       veget(ji,jv)/fraction_veg(ji) * (snowa_aged(jv)+snowa_dec(jv)*agefunc_veg(ji) )
               ENDIF
            ENDDO
         ENDDO
      ENDIF
      !
      ! snow albedo on other surfaces
      !
      DO jv = 1, nnobio
        DO ji = 1, kjpindex
          snowa_nobio(ji,jv) = ( snowa_aged(1) + snowa_dec(1) * agefunc_nobio(ji,jv) ) 
        ENDDO
      ENDDO
    ENDIF
 
    !! 3. Calculate snow cover fraction for both total vegetated and total non-vegtative surfaces.\n
    IF (ok_explicitsnow) THEN
        snowrho_ave=sum(snowrho,2)/nsnow
	snowdepth=sum(snowdz,2)
	frac_snow_veg(:) = tanh(snowdepth(:)/(0.025*(snowrho_ave(:)/50.)))
    ELSE
	frac_snow_veg(:) = MIN(MAX(snow(:),zero)/(MAX(snow(:),zero)+snowcri_alb*sn_dens/100.0),un)
    END IF    

    DO jv = 1, nnobio
      frac_snow_nobio(:,jv) = MIN(MAX(snow_nobio(:,jv),zero)/(MAX(snow_nobio(:,jv),zero)+snowcri_alb),un)
    ENDDO


    !! 4. Update surface albedo
    
    ! Update surface albedo using the weighted sum of previous time step surface albedo,
    ! total vegetated fraction, total nobio fraction, snow cover fraction (both vegetated and 
    ! non-vegetative surfaces), and snow albedo (both vegetated and non-vegetative surfaces). 
    ! Although both visible and near-infrared surface albedo are presented, their calculations 
    ! are the same.
    DO jb = 1, 2

      albedo(:,jb) = ( fraction_veg(:) ) * &
                         ( (un-frac_snow_veg(:)) * albedo(:,jb) + &
                           ( frac_snow_veg(:)  ) * snowa_veg(:)    )
      DO jv = 1, nnobio ! Loop over # nobio surfaces
        ! 
        IF ( jv .EQ. iice ) THEN
          alb_nobio = alb_ice(jb)
        ELSE
          WRITE(numout,*) 'jv=',jv
          WRITE(numout,*) 'DO NOT KNOW ALBEDO OF THIS SURFACE TYPE'
          CALL ipslerr_p(3,'condveg_snow','DO NOT KNOW ALBEDO OF THIS SURFACE TYPE','','')
        ENDIF
        !
        albedo(:,jb) = albedo(:,jb) + &
                       ( frac_nobio(:,jv) ) * &
                         ( (un-frac_snow_nobio(:,jv)) * alb_nobio + &
                           ( frac_snow_nobio(:,jv)  ) * snowa_nobio(:,jv)   )
      ENDDO
    
    END DO
    
    ! Calculate the mean albedo
    albedo_glob(:) = (albedo(:,1) + albedo(:,2))/2

    ! Calculate snow albedo
    albedo_snow(:) =  fraction_veg(:) * frac_snow_veg(:) * snowa_veg(:)
    DO jv = 1, nnobio 
       albedo_snow(:) = albedo_snow(:) + &
            frac_nobio(:,jv) * frac_snow_nobio(:,jv) * snowa_nobio(:,jv)
    ENDDO
   
    IF (printlev>=3) WRITE (numout,*) ' condveg_snow done '

  END SUBROUTINE condveg_snow
  
!! ==============================================================================================================================
!! SUBROUTINE   : condveg_soilalb
!!
!>\BRIEF        This subroutine calculates the albedo of soil (without snow).
!!
!! DESCRIPTION  This subroutine reads the soil colour maps in 1 x 1 deg resolution 
!! from the Henderson-Sellers & Wilson database. These values are interpolated to 
!! the model's resolution and transformed into 
!! dry and wet albedos.\n
!!
!! If the soil albedo is calculated without the dependence of soil moisture, the
!! soil colour values are transformed into mean soil albedo values.\n 
!!
!! The calculations follow the assumption that the grid of the data is regular and 
!! it covers the globe. The calculation for the model grid are based on the borders 
!! of the grid of the resolution.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S):    soilalb_dry for visible and near-infrared range,
!!                             soilalb_wet for visible and near-infrared range, 
!!                             soilalb_moy for visible and near-infrared range 
!!
!! REFERENCE(S) : 
!! -Wilson, M.F., and A. Henderson-Sellers, 1985: A global archive of land cover and
!!  soils data for use in general circulation climate models. J. Clim., 5, 119-143.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE condveg_soilalb(nbpt, lalo, neighbours, resolution, contfrac)
  
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                    :: nbpt                  !! Number of points for which the data needs to be 
                                                                           !! interpolated (unitless)             
    REAL(r_std), INTENT(in)                       :: lalo(nbpt,2)          !! Vector of latitude and longitudes (degree)        
    INTEGER(i_std), INTENT(in)                    :: neighbours(nbpt,8)    !! Vector of neighbours for each grid point 
                                                                           !! (1=N, 2=E, 3=S, 4=W)  
    REAL(r_std), INTENT(in)                       :: resolution(nbpt,2)    !! The size of each grid cell in X and Y (km)
    REAL(r_std), INTENT(in)                       :: contfrac(nbpt)        !! Fraction of land in each grid cell (unitless)   

    !! 0.4 Local variables

    INTEGER(i_std)                                :: nbvmax                !! nbvmax for interpolation (unitless). It is the 
                                                                           !! dimension of the variables in which we store the list
                                                                           !! of points of the source grid which fit into one grid 
                                                                           !! box of the target. 
    CHARACTER(LEN=80)                             :: filename              !! Filename of soil colour map
    INTEGER(i_std)                                :: iml, jml, lml, &
                      &tml, fid, ib, ip, jp, fopt, ilf, lastjp, nbexp      !! Indices
    REAL(r_std)                                   :: lev(1), date, dt      !! Help variables to read in file data
    INTEGER(i_std)                                :: itau(1)               !! Help variables to read in file data
    REAL(r_std)                                   :: sgn                   !! Help variable to compute average bare soil albedo 
    REAL(r_std)                                   :: coslat                !! [DISPENSABLE]
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lat_rel               !! Help variable to read file data and allocate memory
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lon_rel               !! Help variable to read file data and allocate memory
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: soilcol               !! Help variable to read file data and allocate memory
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: sub_area              !! Help variable to read file data and allocate memory
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:) :: sub_index             !! Help variable to read file data and allocate memory
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: mask                  !! Help variable to read file data and allocate memory
    CHARACTER(LEN=30)                             :: callsign              !! Help variable to read file data and allocate memory
    LOGICAL                                       :: ok_interpol           !! Optional return of aggregate_2d
    INTEGER                                       :: ALLOC_ERR             !! Help varialbe to count allocation error
!_ ================================================================================================================================
  
  !! 1. Open file and allocate memory

  ! Open file with soil colours 

  !Config Key   = SOILALB_FILE
  !Config Desc  = Name of file from which the bare soil albedo
  !Config Def   = soils_param.nc
  !Config If    = NOT(IMPOSE_AZE)
  !Config Help  = The name of the file to be opened to read the soil types from 
  !Config         which we derive then the bare soil albedos. This file is 1x1 
  !Config         deg and based on the soil colors defined by Wilson and Henderson-Seller.
  !Config Units = [FILE]
  !
  filename = 'soils_param.nc'
  CALL getin_p('SOILALB_FILE',filename)
  
  ! Read data from file
  IF (is_root_prc) CALL flininfo(filename,iml, jml, lml, tml, fid)
  CALL bcast(iml)
  CALL bcast(jml)
  CALL bcast(lml)
  CALL bcast(tml)

  ! Allocate memory for latitudes
  ALLOCATE(lat_rel(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_soilalb','Pb in allocation for lat_rel','','')

  ! Allcoate memory for longitude
  ALLOCATE(lon_rel(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_soilalb','Pb in allocation for lon_rel','','')

  ! Allocate memory for mask
  ALLOCATE(mask(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_soilalb','Pb in allocation for mask','','')

  ! Allocate memory for soil data
  ALLOCATE(soilcol(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_soilalb','Pb in allocation for soilcol','','')
  
  ! Set values
  IF (is_root_prc) CALL flinopen(filename, .FALSE., iml, jml, lml, lon_rel, lat_rel, lev, tml, itau, date, dt, fid)
  CALL bcast(lon_rel)
  CALL bcast(lat_rel)
  
  IF (is_root_prc) CALL flinget(fid, 'soilcolor', iml, jml, lml, tml, 1, 1, soilcol)
  CALL bcast(soilcol)
  
  IF (is_root_prc) CALL flinclo(fid)
  
  ! Create mask with values of soil colour
  mask(:,:) = zero
  DO ip=1,iml
     DO jp=1,jml
        IF (soilcol(ip,jp) > min_sechiba) THEN
           mask(ip,jp) = un
        ENDIF
     ENDDO
  ENDDO
  
  ! Set nbvmax to 200 for interpolation
  ! This number is the dimension of the variables in which we store 
  ! the list of points of the source grid which fit into one grid box of the target. 
  nbvmax = 200
  
  callsign = 'Soil color map'
  
  ! Start with interpolation
  ok_interpol=.FALSE.
  DO WHILE ( .NOT. ok_interpol )
     WRITE(numout,*) "Projection arrays for ",callsign," : "
     WRITE(numout,*) "nbvmax = ",nbvmax
     
     ALLOCATE(sub_area(nbpt,nbvmax), STAT=ALLOC_ERR)
     IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_soilalb','Pb in allocation for sub_area','','')
     sub_area(:,:)=zero

     ALLOCATE(sub_index(nbpt,nbvmax,2), STAT=ALLOC_ERR)
     IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_soilalb','Pb in allocation for sub_index','','')
     sub_index(:,:,:)=0
     
     CALL aggregate_p(nbpt, lalo, neighbours, resolution, contfrac, &
          &                iml, jml, lon_rel, lat_rel, mask, callsign, &
          &                nbvmax, sub_index, sub_area, ok_interpol)
     
     IF ( .NOT. ok_interpol ) THEN
        DEALLOCATE(sub_area)
        DEALLOCATE(sub_index)
     ENDIF
     
     nbvmax = nbvmax * 2
  ENDDO
  
  ! Check how many points with soil information are found
  nbexp = 0

  soilalb_dry(:,:) = zero
  soilalb_wet(:,:) = zero
  soilalb_moy(:,:) = zero

  DO ib=1,nbpt ! Loop over domain size

     fopt =  COUNT(sub_area(ib,:) > zero)

     !! 3. Compute the average bare soil albedo parameters
     
     IF ( fopt .EQ. 0) THEN      ! If no points were interpolated
        nbexp = nbexp + 1
        soilalb_dry(ib,ivis) = (SUM(vis_dry)/classnb + SUM(vis_wet)/classnb)/deux
        soilalb_dry(ib,inir) = (SUM(nir_dry)/classnb + SUM(nir_wet)/classnb)/deux
        soilalb_wet(ib,ivis) = (SUM(vis_dry)/classnb + SUM(vis_wet)/classnb)/deux
        soilalb_wet(ib,inir) = (SUM(nir_dry)/classnb + SUM(nir_wet)/classnb)/deux
        soilalb_moy(ib,ivis) = SUM(albsoil_vis)/classnb
        soilalb_moy(ib,inir) = SUM(albsoil_nir)/classnb
     ELSE
        sgn = zero

        DO ilf = 1,fopt         ! If points were interpolated
           
           ip = sub_index(ib,ilf,1)
           jp = sub_index(ib,ilf,2)
           
           ! Weighted albedo values by interpolation area
           IF ( NINT(soilcol(ip,jp)) .LE. classnb) THEN
              soilalb_dry(ib,ivis) = soilalb_dry(ib,ivis) + vis_dry(NINT(soilcol(ip,jp))) * sub_area(ib,ilf)
              soilalb_dry(ib,inir) = soilalb_dry(ib,inir) + nir_dry(NINT(soilcol(ip,jp))) * sub_area(ib,ilf)
              soilalb_wet(ib,ivis) = soilalb_wet(ib,ivis) + vis_wet(NINT(soilcol(ip,jp))) * sub_area(ib,ilf)
              soilalb_wet(ib,inir) = soilalb_wet(ib,inir) + nir_wet(NINT(soilcol(ip,jp))) * sub_area(ib,ilf)
              soilalb_moy(ib,ivis) = soilalb_moy(ib,ivis) + albsoil_vis(NINT(soilcol(ip,jp))) * sub_area(ib,ilf)
              soilalb_moy(ib,inir) = soilalb_moy(ib,inir) + albsoil_nir(NINT(soilcol(ip,jp))) * sub_area(ib,ilf)
              sgn = sgn + sub_area(ib,ilf)
           ELSE
              CALL ipslerr_p(3,'condveg_soilalb','The file contains a soil color class which is incompatible with this program', &
                   '','')
           ENDIF
           
        ENDDO

        ! Normalize the surface
        IF ( sgn .LT. min_sechiba) THEN
           nbexp = nbexp + 1
           soilalb_dry(ib,ivis) = (SUM(vis_dry)/classnb + SUM(vis_wet)/classnb)/deux
           soilalb_dry(ib,inir) = (SUM(nir_dry)/classnb + SUM(nir_wet)/classnb)/deux
           soilalb_wet(ib,ivis) = (SUM(vis_dry)/classnb + SUM(vis_wet)/classnb)/deux
           soilalb_wet(ib,inir) = (SUM(nir_dry)/classnb + SUM(nir_wet)/classnb)/deux
           soilalb_moy(ib,ivis) = SUM(albsoil_vis)/classnb
           soilalb_moy(ib,inir) = SUM(albsoil_nir)/classnb
        ELSE
           soilalb_dry(ib,ivis) = soilalb_dry(ib,ivis)/sgn
           soilalb_dry(ib,inir) = soilalb_dry(ib,inir)/sgn
           soilalb_wet(ib,ivis) = soilalb_wet(ib,ivis)/sgn
           soilalb_wet(ib,inir) = soilalb_wet(ib,inir)/sgn
           soilalb_moy(ib,ivis) = soilalb_moy(ib,ivis)/sgn
           soilalb_moy(ib,inir) = soilalb_moy(ib,inir)/sgn           
        ENDIF

     ENDIF

  ENDDO

  IF ( nbexp .GT. 0 ) THEN
     WRITE(numout,*) 'CONDVEG_soilalb : The interpolation of the bare soil albedo had ', nbexp
     WRITE(numout,*) 'CONDVEG_soilalb : points without data. This are either coastal points or'
     WRITE(numout,*) 'CONDVEG_soilalb : ice covered land.'
     WRITE(numout,*) 'CONDVEG_soilalb : The problem was solved by using the average of all soils'
     WRITE(numout,*) 'CONDVEG_soilalb : in dry and wet conditions'
  ENDIF

  DEALLOCATE (lat_rel)
  DEALLOCATE (lon_rel)
  DEALLOCATE (mask)
  DEALLOCATE (sub_index)
  DEALLOCATE (sub_area)
  DEALLOCATE (soilcol)

  END SUBROUTINE condveg_soilalb


!! ==============================================================================================================================
!! SUBROUTINE 	: condveg_background_soilalb
!!
!>\BRIEF        This subroutine reads the albedo of bare soil
!!
!! DESCRIPTION  This subroutine reads the background albedo map in 0.5 x 0.5 deg resolution 
!! derived from JRCTIP product to be used as bare soil albedo. These values are then interpolated
!! to the model's resolution.\n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): soilalb_bg for visible and near-infrared range 
!!
!! REFERENCES	: None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================
  
  SUBROUTINE condveg_background_soilalb(nbpt, lalo, neighbours, resolution, contfrac)
  
    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                    :: nbpt                  !! Number of points for which the data needs to be 
                                                                           !! interpolated (unitless)             
    REAL(r_std), INTENT(in)                       :: lalo(nbpt,2)          !! Vector of latitude and longitudes (degree)        
    INTEGER(i_std), INTENT(in)                    :: neighbours(nbpt,8)    !! Vector of neighbours for each grid point 
                                                                           !! (1=N, 2=E, 3=S, 4=W)  
    REAL(r_std), INTENT(in)                       :: resolution(nbpt,2)    !! The size of each grid cell in X and Y (km)
    REAL(r_std), INTENT(in)                       :: contfrac(nbpt)        !! Fraction of land in each grid cell (unitless)   

    !! 0.4 Local variables

    INTEGER(i_std)                                :: nbvmax                !! nbvmax for interpolation (unitless). It is the 
                                                                           !! dimension of the variables in which we store the list
                                                                           !! of points of the source grid which fit into one grid 
                                                                           !! box of the target. 
    CHARACTER(LEN=80)                             :: filename              !! Filename of background albedo
    INTEGER(i_std)                                :: iml, jml, lml, tml    !! Indices
    INTEGER(i_std)                                :: fid, ib, ip, jp, fopt !! Indices
    INTEGER(i_std)                                :: ilf, ks, it           !! Indices
    REAL(r_std)                                   :: totarea               !! Help variable to compute average bare soil albedo 
    REAL(r_std), ALLOCATABLE, DIMENSION(:)        :: lat_lu, lon_lu        !! Latitudes and longitudes read from input file
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: lat_rel, lon_rel      !! Help variable to read file data and allocate memory
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: mask_lu               !! Help variable to read file data and allocate memory
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:)   :: mask
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:,:,:)  :: bg_albedo             !! Help variable to read file data and allocate memory
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:)      :: sub_area              !! Help variable to read file data and allocate memory
    INTEGER(i_std), ALLOCATABLE, DIMENSION(:,:,:) :: sub_index             !! Help variable to read file data and allocate memory
    CHARACTER(LEN=30)                             :: callsign              !! Help variable to read file data and allocate memory
    LOGICAL                                       :: ok_interpol           !! Optional return of aggregate_2d
    INTEGER                                       :: ALLOC_ERR             !! Help varialbe to count allocation error
!_ ================================================================================================================================
  
  !! 1. Open file and allocate memory

  ! Open file with background albedo

  !Config Key   = ALB_BG_FILE
  !Config Desc  = Name of file from which the background albedo is read 
  !Config Def   = alb_bg_jrctip.nc
  !Config If    = 
  !Config Help  = The name of the file to be opened to read background albedo 
  !Config Units = [FILE]
  !
  filename = 'alb_bg_jrctip.nc'
  CALL getin_p('ALB_BG_FILE',filename)
  
  ! Read data from file
  IF (is_root_prc) CALL flininfo(filename, iml, jml, lml, tml, fid)
  CALL bcast(iml)
  CALL bcast(jml)
  CALL bcast(lml)
  CALL bcast(tml)

  ALLOCATE(lon_lu(iml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Problem in allocation of variable lon_lu','','')

  ALLOCATE(lat_lu(jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Problem in allocation of variable lat_lu','','')

  ALLOCATE(mask_lu(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Pb in allocation for mask_lu','','')

  ALLOCATE(bg_albedo(iml,jml,2,tml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Pb in allocation for bg_albedo','','')

  IF (is_root_prc) THEN
     CALL flinget(fid, 'longitude', iml, 0, 0, 0, 1, 1, lon_lu)
     CALL flinget(fid, 'latitude', jml, 0, 0, 0, 1, 1, lat_lu)
     CALL flinget(fid, 'mask', iml, jml, 0, 0, 1, 1, mask_lu)
     CALL flinget(fid, 'bg_albedo', iml, jml, 2, tml, 1, 12, bg_albedo)
     CALL flinclo(fid)
  ENDIF

  CALL bcast(lon_lu)
  CALL bcast(lat_lu)
  CALL bcast(mask_lu)
  CALL bcast(bg_albedo)
  
  ALLOCATE(lon_rel(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Pb in allocation for lon_rel','','')

  ALLOCATE(lat_rel(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Pb in allocation for lat_rel','','')

  ALLOCATE(mask(iml,jml), STAT=ALLOC_ERR)
  IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Problem in allocation of variable mask','','')

  DO jp=1,jml
     lon_rel(:,jp) = lon_lu(:)
  ENDDO
  DO ip=1,iml
     lat_rel(ip,:) = lat_lu(:)
  ENDDO

  mask(:,:) = zero
  WHERE (mask_lu(:,:) > zero )
     mask(:,:) = un
  ENDWHERE

  ! Set nbvmax to 200 for interpolation
  ! This number is the dimension of the variables in which we store 
  ! the list of points of the source grid which fit into one grid box of the target. 
  nbvmax = 200
  callsign = 'Background soil albedo'
  
  ! Start interpolation
  ok_interpol=.FALSE.
  DO WHILE ( .NOT. ok_interpol )
     WRITE(numout,*) "Projection arrays for ",callsign," : "
     WRITE(numout,*) "nbvmax = ",nbvmax
     
     ALLOCATE(sub_area(nbpt,nbvmax), STAT=ALLOC_ERR)
     IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Pb in allocation for sub_area','','')
     sub_area(:,:)=zero

     ALLOCATE(sub_index(nbpt,nbvmax,2), STAT=ALLOC_ERR)
     IF (ALLOC_ERR /= 0) CALL ipslerr_p(3,'condveg_background_soilalb','Pb in allocation for sub_index','','')
     sub_index(:,:,:)=0
     
     CALL aggregate_p(nbpt, lalo, neighbours, resolution, contfrac, &
          iml, jml, lon_rel, lat_rel, mask, callsign, &
          nbvmax, sub_index, sub_area, ok_interpol)
     
     IF ( .NOT. ok_interpol ) THEN
        DEALLOCATE(sub_area)
        DEALLOCATE(sub_index)
        nbvmax = nbvmax * 2
     ENDIF
  ENDDO

  ! Compute the average
  soilalb_bg(:,:,:) = zero
  DO ib = 1, nbpt
     fopt = COUNT(sub_area(ib,:) > zero)
     IF ( fopt > 0 ) THEN
        totarea = zero
        DO ilf = 1, fopt
           ip = sub_index(ib,ilf,1)
           jp = sub_index(ib,ilf,2)
           DO ks = 1,2
              DO it = 1,12
                 soilalb_bg(ib,ks,it) = soilalb_bg(ib,ks,it) + bg_albedo(ip,jp,ks,it)*sub_area(ib,ilf)
              ENDDO
           ENDDO
           totarea = totarea + sub_area(ib,ilf)
        ENDDO
        ! Normalize
        soilalb_bg(ib,:,:) = soilalb_bg(ib,:,:)/totarea
     ELSE
        ! Set defalut value for points where the interpolation fail
        WRITE(numout,*) 'On point ', ib, ' no points were found for interpolation data. Mean value is used.'
        WRITE(numout,*) 'Location : ', lalo(ib,2), lalo(ib,1)
        soilalb_bg(ib,ivis,:) = 0.129
        soilalb_bg(ib,inir,:) = 0.247
     ENDIF
  ENDDO

  DEALLOCATE (lat_lu)
  DEALLOCATE (lat_rel)
  DEALLOCATE (lon_lu)
  DEALLOCATE (lon_rel)
  DEALLOCATE (mask_lu)
  DEALLOCATE (mask)
  DEALLOCATE (bg_albedo)
  DEALLOCATE (sub_area)
  DEALLOCATE (sub_index)

  END SUBROUTINE condveg_background_soilalb


!! ==============================================================================================================================
!! SUBROUTINE   : condveg_z0logz
!!
!>\BRIEF        Computation of grid average of roughness height by averaging  the 
!! logarithm of the roughness height of each grid box components fracbio and fracnobio.
!!
!! DESCRIPTION  : Calculates mean roughness height
!!  over the grid cell. The mean roughness height is derived from the vegetation 
!! height which is scaled by the roughness parameter. The sum of the logarithm of the 
!! roughness times the fraction per grid cell gives the average roughness height per 
!! grid cell for the vegetative PFTs. The roughness height for the non-vegetative PFTs 
!! is calculated in a second step. \n
!!
!! To compute the fluxes,  
!! the difference between the height of the vegetation and the zero plane displacement height
!! is needed and called roughheight .\n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): roughness height (z0), grid effective roughness height (roughheight)
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE condveg_z0logz (kjpindex, veget, veget_max, frac_nobio, totfrac_nobio, height, tot_bare_soil, &
       &                     z0, roughheight, roughheight_pft)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables
    
    INTEGER(i_std), INTENT(in)                          :: kjpindex      !! Domain size - Number of land pixels  (unitless) 
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)    :: veget         !! PFT coverage fraction of a PFT (= ind*cn_ind) 
                                                                         !! (m^2 m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)    :: veget_max     !! PFT "Maximal" coverage fraction of a PFT 
                                                                         !! (= ind*cn_ind) (m^2 m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nnobio), INTENT(in) :: frac_nobio    !! Fraction of non-vegetative surfaces, 
                                                                         !! i.e. continental ice, lakes, etc. (unitless)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)        :: totfrac_nobio !! Total fraction of non-vegetative surfaces, 
                                                                         !! i.e. continental ice, lakes, etc. (unitless)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)    :: height        !! Vegetation height (m)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: tot_bare_soil !! Total evaporating bare soil fraction 
    
    !! 0.2 Output variables

    REAL(r_std), DIMENSION(kjpindex), INTENT(out)       :: z0            !! Soil roughness height (m) 
    REAL(r_std), DIMENSION(kjpindex), INTENT(out)       :: roughheight   !! Grid effective roughness height (m) 
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(out)   :: roughheight_pft 
    
    !! 0.3 Modified variables

    !! 0.4 Local variables
    
    INTEGER(i_std)                                      :: jv            !! Loop index over PFTs (unitless)
    REAL(r_std), DIMENSION(kjpindex)                    :: sumveg        !! Fraction of bare soil (unitless) 
    REAL(r_std), DIMENSION(kjpindex)                    :: ave_height    !! Average vegetation height (m)
    REAL(r_std), DIMENSION(kjpindex)                    :: d_veg         !! PFT coverage of vegetative PFTs 
                                                                         !! (= ind*cn_ind) (m^2 m^{-2})
    REAL(r_std), DIMENSION(kjpindex)                    :: zhdispl       !! Zero plane displacement height (m)
    REAL(r_std), DIMENSION(kjpindex,nvm)                :: zhdispl_pft 
    REAL(r_std)                                         :: z0_nobio      !! Roughness of non-vegetative fraction (m),  
                                                                         !! i.e. continental ice, lakes, etc. 
!_ ================================================================================================================================
    
    !! 1. Preliminary calculation

    ! Calculate the roughness (m) of bare soil, z0_bare
    ! taken from constantes_veg.f90    
    z0(:) = tot_bare_soil(:) * LOG(z0_bare)

    ! Define fraction of bare soil
    sumveg(:) = tot_bare_soil(:)

    ! Set average vegetation height to zero
    ave_height(:) = zero

    !! 2. Calculate the mean roughness length 
    
    ! Calculate the mean roughness height of
    ! vegetative PFTs over the grid cell
    DO jv = 2, nvm !Loop over # vegetative PFTs

       ! In the case of forest, use parameter veget_max because 
       ! tree trunks influence the roughness even when there are no leaves
       IF ( is_tree(jv) ) THEN
          d_veg(:) = veget_max(:,jv)
       ELSE

          ! In the case of grass, use parameter veget because grasses 
          ! only influence the roughness during the growing season
          d_veg(:) = veget(:,jv)
       ENDIF
       
       ! Calculate the average roughness over the grid cell:
       ! The roughness for vegetative PFTs is calculated by
       ! the vegetation height per PFT multiplied by the roughness 
       ! parameter 'z0_over_height= 1/16'. If this scaled value is 
       ! lower than 0.01 than the value for the roughness length 
       ! of bare soil (0.01) is used. The sum of the logarithm of 
       ! the roughness times the fraction per grid cell gives the 
       ! logarithm of roughness length per grid cell for the vegetative
       ! PFTs.
       z0(:) = z0(:) + d_veg(:) * &
            LOG( MAX(height(:,jv)*z0_over_height,z0_bare) )
       ! Sum of bare soil and fraction vegetated fraction 
       sumveg(:) = sumveg(:) + d_veg(:)

       ! Weighted height of vegetation with maximal cover fraction
       ave_height(:) = ave_height(:) + veget_max(:,jv)*height(:,jv)
       
    ENDDO !Loop over # vegetative PFTs
    
    !! 3. Calculate the mean roughness length of non-vegetative surfaces \n

    ! Search for pixels with vegetated part to normalise 
    ! roughness length
    WHERE ( sumveg(:) > zero ) z0(:) = z0(:) / sumveg(:)
    
    ! Calculate fraction of roughness for vegetated part 
    z0(:) = (un - totfrac_nobio(:)) * z0(:)
    
    DO jv = 1, nnobio ! Loop over # of non-vegative surfaces
    
       ! Set rougness for ice 
       IF ( jv .EQ. iice ) THEN
          z0_nobio = z0_ice
       ELSE
          WRITE(numout,*) 'jv=',jv
          WRITE(numout,*) 'DO NOT KNOW ROUGHNESS OF THIS SURFACE TYPE'
          CALL ipslerr_p(3,'condveg_Z0logz','DO NOT KNOW ROUGHNESS OF THIS SURFACE TYPE','','')
       ENDIF
       
       ! Sum of vegetative roughness length and non-vegetative
       ! roughness length
       z0(:) = z0(:) + frac_nobio(:,jv) * LOG(z0_nobio)
    
    ENDDO ! loop over # of non-vegative surfaces
    
    !! 4. Calculate the zero plane displacement height and effective roughness length

    !  Take the exponential of the roughness length 
    z0(:) = EXP( z0(:) )

    ! Compute the zero plane displacement height which
    ! is an equivalent height of the vegetation for the absorption of momentum
    zhdispl(:) = ave_height(:) * height_displacement
    DO jv = 2,nvm
        zhdispl_pft(:,jv) = height(:,jv) * height_displacement
    ENDDO

    ! Then we compute what we call the grid effective roughness height.
    ! This is the height over which the roughness acts. It combines the
    ! zero plane displacement height and the vegetation height.  This 
    ! effective value is the difference between the height of the 
    ! vegetation and the zero plane displacement height.
    roughheight(:) = ave_height(:) - zhdispl(:)
    DO jv = 2,nvm
        roughheight_pft(:,jv) = height(:,jv) - zhdispl_pft(:,jv)
        !!! note that we only use roughheight_pft for croplands
    ENDDO

  END SUBROUTINE condveg_z0logz


!! ==============================================================================================================================
!! SUBROUTINE   : condveg_z0cdrag
!!
!>\BRIEF        Computation of grid average of roughness length by calculating 
!! the drag coefficient.
!!
!! DESCRIPTION  : This routine calculates the mean roughness height and mean 
!! effective roughness height over the grid cell. The mean roughness height (z0) 
!! is computed by averaging the drag coefficients  \n
!!
!! \latexonly 
!! \input{z0cdrag1.tex}
!! \endlatexonly
!! \n 
!!
!! where C is the drag coefficient at the height of the vegetation, kappa is the 
!! von Karman constant, z (Ztmp) is the height at which the fluxes are estimated and z0 the roughness height. 
!! The reference level for z needs to be high enough above the canopy to avoid 
!! singularities of the LOG. This height is set to  minimum 10m above ground. 
!! The drag coefficient increases with roughness height to represent the greater 
!! turbulence generated by rougher surfaces. 
!! The roughenss height is obtained by the inversion of the drag coefficient equation.\n
!!
!! The roughness height for the non-vegetative surfaces is calculated in a second step. 
!! In order to calculate the transfer coefficients the 
!! effective roughness height is calculated. This effective value is the difference
!! between the height of the vegetation and the zero plane displacement height.\nn
!!
!! RECENT CHANGE(S): None
!! 
!! MAIN OUTPUT VARIABLE(S):  :: roughness height(z0) and grid effective roughness height(roughheight)
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE condveg_z0cdrag (kjpindex,veget,veget_max,frac_nobio,totfrac_nobio,zlev, height, tot_bare_soil, &
       &                      z0, roughheight, roughheight_pft)

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables
   
    INTEGER(i_std), INTENT(in)                          :: kjpindex      !! Domain size - Number of land pixels  (unitless)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)    :: veget         !! PFT coverage fraction of a PFT (= ind*cn_ind) 
                                                                         !! (m^2 m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)    :: veget_max     !! PFT "Maximal" coverage fraction of a PFT 
                                                                         !! (= ind*cn_ind) (m^2 m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nnobio), INTENT(in) :: frac_nobio    !! Fraction of non-vegetative surfaces, 
                                                                         !! i.e. continental ice, lakes, etc. (unitless)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)        :: totfrac_nobio !! Total fraction of non-vegetative surfaces, 
                                                                         !! i.e. continental ice, lakes, etc. (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: zlev          !! Height of first layer (m)           
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)    :: height        !! Vegetation height (m)
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)       :: tot_bare_soil !! Total evaporating bare soil fraction 
    
    !! 0.2 Output variables

    REAL(r_std), DIMENSION(kjpindex), INTENT(out)       :: z0            !! Roughness height (m)
    REAL(r_std), DIMENSION(kjpindex), INTENT(out)       :: roughheight   !! Grid effective roughness height (m) 
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(out)   :: roughheight_pft  
    
    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                      :: jv            !! Loop index over PFTs (unitless)
    REAL(r_std), DIMENSION(kjpindex)                    :: sumveg        !! Fraction of bare soil (unitless)
    REAL(r_std), DIMENSION(kjpindex)                    :: ztmp          !! Max height of the atmospheric level (m)
    REAL(r_std), DIMENSION(kjpindex)                    :: ave_height    !! Average vegetation height (m)
    REAL(r_std), DIMENSION(kjpindex)                    :: d_veg         !! PFT coverage of vegetative PFTs 
                                                                         !! (= ind*cn_ind) (m^2 m^{-2})
    REAL(r_std), DIMENSION(kjpindex)                    :: zhdispl       !! Zero plane displacement height (m)
    REAL(r_std), DIMENSION(kjpindex,nvm)                :: zhdispl_pft    
    REAL(r_std)                                         :: z0_nobio      !! Roughness height of non-vegetative fraction (m),  
                                                                         !! i.e. continental ice, lakes, etc. 
!_ ================================================================================================================================
    
    !! 1. Preliminary calculation

    ! Set maximal height of first layer
    ztmp(:) = MAX(10., zlev(:))

    ! Calculate roughness for non-vegetative surfaces
    ! with the von Karman constant 
    z0(:) = tot_bare_soil(:) * (ct_karman/LOG(ztmp(:)/z0_bare))**2

    ! Fraction of bare soil
    sumveg(:) = tot_bare_soil(:)

    ! Set average vegetation height to zero
    ave_height(:) = zero
    
    !! 2. Calculate the mean roughness height 
    
    ! Calculate the mean roughness height of
    ! vegetative PFTs over the grid cell
    DO jv = 2, nvm

       ! In the case of forest, use parameter veget_max because 
       ! tree trunks influence the roughness even when there are no leaves
       IF ( is_tree(jv) ) THEN
          ! In the case of grass, use parameter veget because grasses 
          ! only influence the roughness during the growing season
          d_veg(:) = veget_max(:,jv)
       ELSE
          ! grasses only have an influence if they are really there!
          d_veg(:) = veget(:,jv)
       ENDIF
       
       ! Calculate the average roughness over the grid cell:
       ! The unitless drag coefficient is per vegetative PFT
       ! calculated by use of the von Karman constant, the height 
       ! of the first layer and the roughness. The roughness
       ! is calculated as the vegetation height  per PFT 
       ! multiplied by the roughness  parameter 'z0_over_height= 1/16'. 
       ! If this scaled value is lower than 0.01 then the value for 
       ! the roughness of bare soil (0.01) is used. 
       ! The sum over all PFTs gives the average roughness 
       ! per grid cell for the vegetative PFTs.
       z0(:) = z0(:) + d_veg(:) * (ct_karman/LOG(ztmp(:)/MAX(height(:,jv)*z0_over_height,z0_bare)))**2

       ! Sum of bare soil and fraction vegetated fraction
       sumveg(:) = sumveg(:) + d_veg(:)
       
       ! Weigh height of vegetation with maximal cover fraction
       ave_height(:) = ave_height(:) + veget_max(:,jv)*height(:,jv)
       
    ENDDO
    
    !! 3. Calculate the mean roughness height of vegetative PFTs over the grid cell
    
    !  Search for pixels with vegetated part to normalise 
    !  roughness height
    WHERE ( sumveg(:) .GT. zero ) z0(:) = z0(:) / sumveg(:)

    ! Calculate fraction of roughness for vegetated part 
    z0(:) = (un - totfrac_nobio(:)) * z0(:)

    DO jv = 1, nnobio ! Loop over # of non-vegative surfaces

       ! Set rougness for ice
       IF ( jv .EQ. iice ) THEN
          z0_nobio = z0_ice
       ELSE
          WRITE(numout,*) 'jv=',jv
          WRITE(numout,*) 'DO NOT KNOW ROUGHNESS OF THIS SURFACE TYPE'
          CALL ipslerr_p(3,'condveg_z0cdrag','DO NOT KNOW ROUGHNESS OF THIS SURFACE TYPE','','')
       ENDIF
       
       ! Sum of vegetative roughness length and non-vegetative
       ! roughness length
       z0(:) = z0(:) + frac_nobio(:,jv) * (ct_karman/LOG(ztmp(:)/z0_nobio))**2
       
    ENDDO ! Loop over # of non-vegative surfaces
    
    !! 4. Calculate the zero plane displacement height and effective roughness length

    !  Take the exponential of the roughness 
    z0(:) = ztmp(:) / EXP(ct_karman/SQRT(z0(:)))

    ! Compute the zero plane displacement height which
    ! is an equivalent height for the absorption of momentum
    zhdispl(:) = ave_height(:) * height_displacement
    DO jv = 2,nvm
        zhdispl_pft(:,jv) = height(:,jv) * height_displacement
    ENDDO

    ! In order to calculate the fluxes we compute what we call the grid effective roughness height.
    ! This is the height over which the roughness acts. It combines the
    ! zero plane displacement height and the vegetation height.
    roughheight(:) = ave_height(:) - zhdispl(:)
    DO jv = 2,nvm
        roughheight_pft(:,jv) = height(:,jv) - zhdispl_pft(:,jv)
        !!!! roughheight_pft only used for croplands, xuhui
    ENDDO

  END SUBROUTINE condveg_z0cdrag


!! ==============================================================================================================================
!! SUBROUTINE   : condveg_albcalc
!!
!>\BRIEF        This subroutine calculates the albedo without snow.
!!
!! DESCRIPTION  : The albedo is calculated for both the visible and near-infrared 
!! domain. First the mean albedo of the bare soil is calculated. Two options exist: 
!! either the soil albedo depends on soil wetness (drysoil_frac variable), or the soil albedo 
!! is set to a mean soil albedo value.
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): albedo 
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE condveg_albcalc (kjpindex, veget, drysoil_frac, tot_bare_soil, albedo)

 !! 0. Variable and parameter declaration

    !! 0.1 Input variables
 
    INTEGER(i_std), INTENT(in)                       :: kjpindex       !! Domain size - Number of land pixels  (unitless)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in) :: veget          !! PFT coverage fraction of a PFT (= ind*cn_ind) 
                                                                       !! (m^2 m^{-2})        
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)     :: drysoil_frac   !! Fraction of  dry soil (unitless) 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)    :: tot_bare_soil  !! Total evaporating bare soil fraction 
   
    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex,2), INTENT (out) :: albedo         !! Albedo for visible and near-infrared range 
                                                                       !! (unitless)   
 
    !! 0.3 Modified variables

    !! 0.4 Local variables
 
    REAL(r_std),DIMENSION (nvm,2)                    :: alb_leaf_tmp   !! Variable for albedo values for all PFTs and 
                                                                       !! spectral domains (unitless) 
    INTEGER(i_std)                                   :: ks             !! Index for visible and near-infraread range
    INTEGER(i_std)                                   :: jv             !! Index for vegetative PFTs
!_ ================================================================================================================================
    
 !! 1. Preliminary calculation

    ! Assign values of leaf albedo for visible and near-infrared range
    ! to local variable (constantes_veg.f90)
    alb_leaf_tmp(:,ivis) = alb_leaf_vis(:)
    alb_leaf_tmp(:,inir) = alb_leaf_nir(:)

 !! 2. Calculation and assignment of soil albedo

    DO ks = 1, 2! Loop over # of spectra

       ! If alb_bg_modis=TRUE, the background soil albedo map for the current simulated month is used
       ! If alb_bg_modis=FALSE and alb_bare_model=TRUE, the soil albedo calculation depends on soil moisture
       ! If alb_bg_modis=FALSE and alb_bare_model=FALSE, the mean soil albedo is used without the dependance on soil moisture
       ! see subroutines 'condveg_soilalb' and 'condveg_background_soilalb'
       IF ( alb_bg_modis ) THEN
          alb_bare(:,ks) = soilalb_bg(:,ks,month)
       ELSE
          IF ( alb_bare_model ) THEN
             alb_bare(:,ks) = soilalb_wet(:,ks) + drysoil_frac(:) * (soilalb_dry(:,ks) -  soilalb_wet(:,ks))
          ELSE
             alb_bare(:,ks) = soilalb_moy(:,ks)
          ENDIF
       ENDIF

       ! Soil albedo is weighed by fraction of bare soil          
       albedo(:,ks) = tot_bare_soil(:) * alb_bare(:,ks)

!! 3. Calculation of mean albedo of over the grid cell 
  
       ! Calculation of mean albedo of over the grid cell and
       !    mean albedo of only vegetative PFTs over the grid cell
       alb_veget(:,ks) = zero

       DO jv = 2, nvm  ! Loop over # of PFTs

          ! Mean albedo of grid cell for visible and near-infrared range
          albedo(:,ks) = albedo(:,ks) + veget(:,jv)*alb_leaf_tmp(jv,ks)

          ! Mean albedo of vegetation for visible and near-infrared range
          alb_veget(:,ks) = alb_veget(:,ks) + veget(:,jv)*alb_leaf_tmp(jv,ks)
       ENDDO ! Loop over # of PFTs

    ENDDO

  END SUBROUTINE condveg_albcalc

END MODULE condveg
