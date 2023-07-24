! =================================================================================================================================
! MODULE       : thermosoil
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        Calculates the soil temperatures by solving the heat
!! diffusion equation within the soil. This module is only used with CWRR hydrology.
!!
!!\n DESCRIPTION : General important informations about the numerical scheme and
!!                 the soil vertical discretization:\n
!!               - the soil is zmaxt deep (by default 10m) and divided into "ngrnd" layers. 
!!                 From 0-zmaxh(default 2m), the discretization is the same as for hydrology. 
!!                 From zmaxh(2m) and below, the depth increase linearly (by default) or geometrically. \n
!!               - "jg" is usually used as the index going from 1 to ngrnd to describe the
!!                  layers, from top (jg=1) to bottom (jg=ngrnd)\n
!!               - the thermal numerical scheme is implicit finite differences.\n
!!                 -- When it is resolved in thermosoil_profile at the present timestep t, the
!!                 dependancy from the previous timestep (t-1) is hidden in the
!!                 integration coefficients cgrnd and dgrnd, which are therefore
!!                 calculated at the very end of thermosoil_main (call to
!!                 thermosoil_coef) for use in the next timestep.\n
!!                 -- At timestep t, the system becomes :\n 
!!
!!                              T(k+1)=cgrnd(k)+dgrnd(k)*T(k) \n
!!                                      -- EQ1 -- \n
!!
!!                 (the bottom boundary condition has been used to obtained this equation).\n
!!                 To solve it, the uppermost soil temperature T(1) is required.
!!                 It is obtained from the surface temperature Ts, which is
!!                 considered a linear extrapolation of T(1) and T(2)\n
!!
!!                           Ts=(1+lambda)*T(1) -lambda*T(2) \n 
!!                                      -- EQ2--\n
!!
!!                 -- caveat 1 : Ts is called 'temp_soil_new' in this routine,
!!                 don' t act.\n
!!                 -- caveat 2 : actually, the surface temperature at time t Ts
!!                 depends on the soil temperature at time t through the
!!                 ground heat flux. This is again implicitly solved, with Ts(t)
!!                 expressed as :\n
!!
!!                 soilcap*(Ts(t)-Ts(t-1))/dt=soilflx+otherfluxes(Ts(t))\n 
!!                                      -- EQ3 --\n
!!
!!                 and the dependency from the previous timestep is hidden in
!!                 soilcap and soilflx (apparent surface heat capacity and heat
!!                 flux respectively). Soilcap and soilflx are therefore
!!                 calculated at the previous timestep, at the very end of thermosoil
!!                 (final call to thermosoil_coef) and stored to be used at the next time step.
!!                 At timestep t, EQ3 is solved for Ts in enerbil, and Ts
!!                 is used in thermosoil to get T(1) and solve EQ1.\n
!!
!! - lambda is the @tex $\mu$ @endtex of F. Hourdin' s PhD thesis, equation (A28); ie the
!! coefficient of the linear extrapolation of Ts (surface temperature) from T1 and T2 (ptn(jg=1) and ptn(jg=2)), so that:\n
!! Ts= (1+lambda)*T(1)-lambda*T(2) --EQ2-- \n
!! lambda = (zlt(1))/((zlt(2)-zlt(1))) \n
!!
!! RECENT CHANGE(S) : - Change soil thermal properties to consider also soil texture, rev 2922.
!!                    - Change vertical discretization, rev 2917. Note: In the revised thermosoil, 
!!                    cstgrnd and lskin are not needed any more. The depth znt, zlt and dlt
!!                    are computed in vertical_soil and are in meter
!!
!! REFERENCE(S) : None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_sechiba/thermosoil.f90 $
!! $Date: 2016-06-21 10:19:43 +0200 (Tue, 21 Jun 2016) $
!! $Revision: 3569 $
!! \n
!_ ================================================================================================================================

MODULE thermosoil

  USE ioipsl_para
  USE xios_orchidee
  USE constantes
  USE constantes_soil
  USE sechiba_io
  USE grid
  USE pft_parameters_var
  USE vertical_soil

  IMPLICIT NONE

  !private and public routines :
  PRIVATE
  PUBLIC :: thermosoil_main, thermosoil_clear,  thermosoil_initialize, thermosoil_finalize, thermosoil_rotation_update

  REAL(r_std), SAVE                               :: lambda                   !! See Module description
!$OMP THREADPRIVATE(lambda)
  REAL(r_std), SAVE                                  :: fz1                   !! usefull constants for diverse use
!$OMP THREADPRIVATE(fz1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: ptn                   !! vertically discretized 
!$OMP THREADPRIVATE(ptn)
                                                                              !! soil temperatures @tex ($K$) @endtex. 
!$OMP THREADPRIVATE(ptn)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:)    :: ptn_pftmean           !! Different levels soil temperature, mean across all pfts
!$OMP THREADPRIVATE(ptn_pftmean)
  REAL(r_std),  ALLOCATABLE,SAVE, DIMENSION (:)      :: dz1                   !! numerical constant used in the thermal numerical
                                                                              !! scheme  @tex ($m^{-1}$) @endtex. ; it corresponds
                                                                              !! to the coefficient  @tex $d_k$ @endtex of equation
                                                                              !! (A.12) in F. Hourdin PhD thesis.
!$OMP THREADPRIVATE(dz1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: z1                    !! constant of the numerical scheme; it is an 
                                                                              !! intermediate buffer for the calculation of the 
                                                                              !! integration coefficients cgrnd and dgrnd.
!$OMP THREADPRIVATE(z1)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: cgrnd                 !! integration coefficient for the numerical scheme,
                                                                              !! see eq.1
!$OMP THREADPRIVATE(cgrnd)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: dgrnd                 !! integration coefficient for the numerical scheme,
                                                                              !! see eq.1
!$OMP THREADPRIVATE(dgrnd)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: pcapa                 !! volumetric vertically discretized soil heat 
!$OMP THREADPRIVATE(pcapa)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: pkappa                !! vertically discretized soil thermal conductivity 
                                                                              !!  @tex ($W K^{-1} m^{-1}$) @endtex. Same as pcapa.
!$OMP THREADPRIVATE(pkappa)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: pcapa_en              !! heat capacity used for surfheat_incr and 
!$OMP THREADPRIVATE(pcapa_en)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: pcapa_snow               !! volumetric vertically discretized snow heat 
                                                                              !! capacity @tex ($J K^{-1} m^{-3}$) @endtex. 
!$OMP THREADPRIVATE(pcapa_snow)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: pkappa_snow              !! vertically discretized snow thermal conductivity 
                                                                              !! @tex ($W K^{-1} m^{-1}$) @endtex.
!$OMP THREADPRIVATE(pkappa_snow)

  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: ptn_beg               !! vertically discretized temperature at the 
                                                                              !! beginning of the time step  @tex ($K$) @endtex; 
                                                                              !! is used in 
                                                                              !! thermosoil_energy for energy-related diagnostic of
                                                                              !! the routine.
!$OMP THREADPRIVATE(ptn_beg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: temp_sol_beg          !! Surface temperature at the beginning of the 
                                                                              !! timestep  @tex ($K$) @endtex
!$OMP THREADPRIVATE(temp_sol_beg)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: surfheat_incr         !! Change in soil heat content during the timestep 
                                                                              !!  @tex ($J$) @endtex.
!$OMP THREADPRIVATE(surfheat_incr)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)      :: coldcont_incr         !! Change in snow heat content  @tex ($J$) @endtex.
!$OMP THREADPRIVATE(coldcont_incr)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: shum_ngrnd_perma      !! Saturation degree on the thermal axes (0-1, dimensionless) 
!$OMP THREADPRIVATE(shum_ngrnd_perma)

  REAL(r_std), SAVE                                  :: so_cond = 1.5396      !! Thermix soil layer discretization constant
!$OMP THREADPRIVATE(so_cond)
  REAL(r_std), SAVE                                  :: so_capa = 2.0514e+6   !! Thermix soil layer discretization constant
!$OMP THREADPRIVATE(so_capa)

!  Variables related to soil freezing
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: profil_froz           !! Frozen fraction of the soil on hydrological levels (-)
!$OMP THREADPRIVATE(profil_froz)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:)  :: shum_ngrnd_permalong  !! Long-term soil humidity (for permafrost) if ok_freeze_thermix ; shum_ngrnd_perma sinon.
!$OMP THREADPRIVATE(shum_ngrnd_permalong)
        LOGICAL, SAVE    :: ok_shum_ngrnd_permalong
!$OMP THREADPRIVATE(ok_shum_ngrnd_permalong)

    REAL(r_std),ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: pcappa_supp           !! Additional heat capacity due to soil freezing for each soil layer (J/K)
!$OMP THREADPRIVATE(pcappa_supp)
    REAL(r_std),ALLOCATABLE, SAVE, DIMENSION (:,:)   :: e_soil_lat            !! Accumulated latent heat for the whole soil (J)
!$OMP THREADPRIVATE(e_soil_lat)
    REAL(r_std), ALLOCATABLE, SAVE,DIMENSION(:)      :: overburden            !! Information read from IPA map for option read_permafrost_mapn
!$OMP THREADPRIVATE(overburden)
    REAL(r_std), ALLOCATABLE, SAVE,DIMENSION(:)      :: excess_ice            !! Information read from IPA map for option read_permafrost_map
!$OMP THREADPRIVATE(excess_ice)
    REAL(r_std), ALLOCATABLE, SAVE,DIMENSION(:)      :: permafrost            !! Information read from IPA map for option read_permafrost_map
!$OMP THREADPRIVATE(permafrost)
    REAL(r_std), ALLOCATABLE, SAVE,DIMENSION(:,:)    :: reftemp               !! Flag to initialize soil temperature using climatological temperature
!$OMP THREADPRIVATE(reftemp)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: dz5                      !! Used for numerical calculation [-]
!$OMP THREADPRIVATE(dz5)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: mcs                      !! Saturation humidity [m3/m3]
!$OMP THREADPRIVATE(mcs)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: SMCMAX                   !! Soil porosity [m3/m3]
!$OMP THREADPRIVATE(SMCMAX)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: QZ                       !! quartz content [-]
!$OMP THREADPRIVATE(QZ)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:)   :: so_capa_dry_ns           !! Dry soil Heat capacity of soils,J.m^{-3}.K^{-1} 
!$OMP THREADPRIVATE(so_capa_dry_ns)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: mc_layt                  !! Volumetric soil moisture (liquid+ice) (m3/m3) on the thermodynamical levels at interface
!$OMP THREADPRIVATE(mc_layt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: mcl_layt                 !! Volumetric soil moisture (liquid) (m3/m3) on the thermodynamical levels at interface
!$OMP THREADPRIVATE(mcl_layt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:) :: tmc_layt                 !! Total soil moisture content for each layer (liquid+ice) (mm) on the thermodynamical levels
!$OMP THREADPRIVATE(tmc_layt)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: mc_layt_pft              !! Volumetric soil moisture (liquid+ice) (m3/m3) on the thermodynamical levels at interface
!$OMP THREADPRIVATE(mc_layt_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: mcl_layt_pft             !! Volumetric soil moisture (liquid) (m3/m3) on the thermodynamical levels at interface
!$OMP THREADPRIVATE(mcl_layt_pft)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION (:,:,:) :: tmc_layt_pft             !! Total soil moisture content for each layer (liquid+ice) (mm) on the thermodynamical levels
!$OMP THREADPRIVATE(tmc_layt_pft)
  INTEGER(i_std), SAVE                            :: brk_flag = 0             !! Flag to consider bedrock: 0.no; 1.yes
!$OMP THREADPRIVATE(brk_flag)

!Vertical Permafrost Carbon
  LOGICAL, SAVE                                      :: use_toporganiclayer_tempdiff = .FALSE. 
  LOGICAL, SAVE                                      :: use_soilc_tempdiff = .TRUE. 
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:,:)         :: veget_mask_2d
  LOGICAL, SAVE                                      :: satsoil = .FALSE.


CONTAINS
  !!  =============================================================================================================================
  !! SUBROUTINE                             : thermosoil_initialize
  !! 
  !>\BRIEF                                  Allocate module variables, read from restart file or initialize with default values
  !! 
  !! DESCRIPTION                            : Allocate module variables, read from restart file or initialize with default values.
  !!                                          Call thermosoil_var_init to calculate physical constants.  
  !!                                          Call thermosoil_coef to calculate thermal soil properties. 
  !! 
  !! RECENT CHANGE(S)                       : None 
  !! 
  !! REFERENCE(S)                           : None 
  !!  
  !! FLOWCHART                              : None 
  !! \n 
  !_ ============================================================================================================================== 
  SUBROUTINE thermosoil_initialize(kjit, kjpindex, lalo, rest_id, veget_max, &
                      shumdiag_perma, snow, thawed_humidity, soilc_total, &
                      temp_sol_new, temp_sol_new_pft, & 
                      organic_layer_thick, stempdiag, soilcap, soilcap_pft, soilflx, soilflx_pft, &
                      gtemp, &
                      mc_layh,       mcl_layh,   tmc_layh, mc_layh_pft, mcl_layh_pft,   tmc_layh_pft, njsc, &
                      frac_snow_veg,frac_snow_nobio,totfrac_nobio, &
                      snowdz, snowrho, snowtemp,  lambda_snow, cgrnd_snow, dgrnd_snow, pb)

!!<<<<<<< .mine
!!                      snowdz, shumdiag_perma, snow, thawed_humidity, soilc_total, &
!!                      temp_sol_new, temp_sol_new_pft, & 
!!                      organic_layer_thick, stempdiag, soilcap, soilcap_pft, soilflx, soilflx_pft, &
!!                      cgrnd_soil, dgrnd_soil, zdz1_soil, zdz2_soil, &
!!                      gthick, gtemp, gpkappa, &
!!                      mc_layh,       mcl_layh,   tmc_layh, mc_layh_pft, mcl_layh_pft,   tmc_layh_pft,         njsc)
!!=======
!!                      shumdiag_perma, snow, thawed_humidity, soilc_total, &
!!                      temp_sol_new,                                               & 
!!                      organic_layer_thick, stempdiag, soilcap, soilflx,           &
!!                      gtemp,                                                      &
!!                      mc_layh,       mcl_layh,   tmc_layh,        njsc,     &
!!                      frac_snow_veg,frac_snow_nobio,totfrac_nobio, &
!!                      snowdz, snowrho, snowtemp,  lambda_snow, cgrnd_snow, dgrnd_snow, pb)
!!>>>>>>> .r3384done

    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: mc_layh          !! Volumetric soil moisture content (liquid+ice) for hydrological layers, at node (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: mcl_layh         !! Volumetric soil moisture content (liquid) for hydrological layers, at node (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: tmc_layh         !! Total soil moisture content(liquid+ice) for hydrological layers (mm)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)      :: njsc             !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)  :: mc_layh_pft      !! Volumetric soil moisture content (liquid+ice) for hydrological layers, at node (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)  :: mcl_layh_pft     !! Volumetric soil moisture content (liquid) for hydrological layers, at node (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)  :: tmc_layh_pft     !! Total soil moisture content(liquid+ice) for hydrological layers (mm)
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)          :: frac_snow_veg    !! Snow cover fraction on vegeted area
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in)   :: frac_snow_nobio  !! Snow cover fraction on non-vegeted area
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)           :: totfrac_nobio    !! Total fraction of continental ice+lakes+cities+...
                                                                              !! (unitless,0-1)
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT (in)    :: snowdz           !! Snow depth
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)   :: snowrho          !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)   :: snowtemp         !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)        :: pb               !! Surface presure (hPa)

    INTEGER(i_std), INTENT (in)                         :: kjit               !! Time step number (unitless) 
    INTEGER(i_std), INTENT (in)                         :: kjpindex           !! Domain size (unitless)
    REAL(r_std), DIMENSION (kjpindex,2), INTENT(in)     :: lalo               !! coordinates
    INTEGER(i_std), INTENT (in)                         :: rest_id            !! Restart file identifier (unitless)
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max          !! Fraction of vegetation type
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (in) :: shumdiag_perma     !! Soil saturation degree on the diagnostic axis (0-1, unitless)  
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)      :: snow               !! Snow mass @tex ($kg$) @endtex.
    REAL(r_std), DIMENSION(kjpindex),   INTENT (in)     :: thawed_humidity    !! specified humidity of thawed soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (in) :: soilc_total  !! total soil carbon for use in thermal calcs
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)      :: temp_sol_new       !! Surface temperature at the present time-step,
    REAL(r_std), DIMENSION (kjpindex, nvm), INTENT (in)  :: temp_sol_new_pft  !! Surface temperature at the present time-step,

    !! 0.2 Output variables


    !! 0.3 Modified variables
    REAL(r_std), DIMENSION(kjpindex),   INTENT (inout)  :: organic_layer_thick!! how deep is the organic soil?
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (out)   :: stempdiag        !! temperature profile on the levels in hydrol(K)
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)    :: soilflx            !! apparent soil heat flux @tex ($W m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex, nvm), INTENT (inout) :: soilflx_pft      !! apparent soil heat flux @tex ($W m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (out)        :: soilcap          !! apparent surface heat capacity considering snow and soil surface (J m-2 K-1)
    REAL(r_std),DIMENSION (kjpindex, nvm), INTENT (inout) :: soilcap_pft      !! apparent soil heat flux @tex ($W m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex),INTENT(out)          :: gtemp            !! First soil layer temperature

    !! 0.3 Modified variables
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)       :: lambda_snow     !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (inout):: cgrnd_snow      !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (inout):: dgrnd_snow      !! Integration coefficient for snow numerical scheme

    !! 0.4 Local variables
    REAL(r_std),DIMENSION (kjpindex,ngrnd,nvm)          :: reftemp_3d
    INTEGER(i_std)                                      :: ier, i, m 
    INTEGER(i_std)  :: jv, jg

    CHARACTER(LEN=80)                                   :: var_name           !! To store variables names for I/O
    REAL(r_std), DIMENSION (kjpindex,nvm)               :: veget_max_bg
    REAL(r_std), DIMENSION (kjpindex,nvm)               :: veget_mask_real

    LOGICAL, SAVE                                       :: ok_zimov
    REAL(r_std),DIMENSION (kjpindex,ngrnd)              :: temp               !! buffer
    REAL(r_std),DIMENSION (kjpindex,ngrnd-1)            :: temp1              !! buffer
    REAL(r_std),DIMENSION (kjpindex)                    :: temp2              !! buffer
    REAL(r_std),DIMENSION (kjpindex,ngrnd)                :: reftemp          !! Reference temperature read from file if read_reftemp activated
    LOGICAL                                               :: calculate_coef   !! Local flag to initialize variables by call to thermosoil_coef   
!_ ================================================================================================================================

    IF (printlev >= 3) WRITE (numout,*) 'Start thermosoil_initialize '

  !! 1. Initialisation

    !
    !  !! Flag to consider bedrock at deeper layers
    !  !! It affects heat capacity and thermal conductivity (energy balance). 
    !
    !Config Key  = BEDROCK_FLAG
    !Config Desc = Flag to consider bedrock at deeper layers.
    !Config If   = 
    !Config Def  = 0
    !Config Help = 0, no, 1, yes. 
    !Config Units = [FLAG]
    brk_flag = 0
    CALL getin_p('BEDROCK_FLAG', brk_flag)

    ok_shum_ngrnd_permalong = .FALSE.
    CALL getin_p ('OK_WETDIAGLONG',ok_shum_ngrnd_permalong)

    IF (ok_freeze_thermix .AND. ok_pc) THEN
        ok_shum_ngrnd_permalong = .TRUE.
    ENDIF

    CALL getin_p('satsoil', satsoil)
    IF (ok_freeze_thermix .AND. ok_pc) THEN
        use_toporganiclayer_tempdiff = .false.
        CALL getin_p('USE_TOPORGANICLAYER_TEMPDIFF',use_toporganiclayer_tempdiff)

        use_soilc_tempdiff = .false.
        CALL getin_p('USE_SOILC_TEMPDIFF', use_soilc_tempdiff)
        IF (use_toporganiclayer_tempdiff .AND. use_soilc_tempdiff) THEN
           WRITE(*,*) 'warning: thermosoil_getdiff: cant have both use_toporganiclayer_tempdiff and'
           WRITE(*,*) 'use_soilc_tempdiff set to .true.. using only use_soilc_tempdiff.'
           use_toporganiclayer_tempdiff = .FALSE.
        ENDIF
    ENDIF

  !! 2. Arrays allocations

    ALLOCATE (ptn(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of ptn','','')
    ptn(:,:,:) = 0

    ALLOCATE (ptn_pftmean(kjpindex,ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of ptn_pftmean','','')
    ptn_pftmean(:,:) = 0

    ALLOCATE (dz1(ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of dz1','','')
    dz1(:) = 0

    ALLOCATE (z1(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of z1','','')
    z1(:) = 0

    ALLOCATE (cgrnd(kjpindex,ngrnd-1,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of cgrnd','','')
    cgrnd(:,:,:) = 0

    ALLOCATE (dgrnd(kjpindex,ngrnd-1,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of dgrnd','','')
    dgrnd(:,:,:) = 0

    ALLOCATE (pcapa(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of pcapa','','')
    pcapa(:,:,:) = 0

    ALLOCATE (pkappa(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of pkappa','','')
    pkappa(:,:,:) = 0

    ALLOCATE (pcapa_snow(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of pcapa_snow','','')
    pkappa_snow(:,:) = 0

    ALLOCATE (pkappa_snow(kjpindex,nsnow),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of pkappa_snow','','')
    pkappa_snow(:,:) = 0

    ALLOCATE (surfheat_incr(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of surfheat_incr','','')

    ALLOCATE (coldcont_incr(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of coldcont_incr','','')

    ALLOCATE (pcapa_en(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of pcapa_en','','')

    ALLOCATE (ptn_beg(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of ptn_beg','','')

    ALLOCATE (temp_sol_beg(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of temp_sol_beg','','')

    ALLOCATE (shum_ngrnd_perma(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of shum_ngrnd_perma','','')

    shum_ngrnd_perma(:,:,:)=val_exp
    ALLOCATE (shum_ngrnd_permalong(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of shum_ngrnd_permalong','','')

    ALLOCATE (profil_froz(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of profil_froz','','')

    IF (ok_freeze_thermix) THEN
        ALLOCATE (pcappa_supp(kjpindex,ngrnd,nvm),stat=ier)
        IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of pcapa_supp','','')
    END IF
   
    IF (ok_Ecorr) THEN 
        ALLOCATE (e_soil_lat(kjpindex,nvm),stat=ier)
        IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of e_soil_lat','','')
    END IF

    IF (read_permafrost_map) THEN
        ALLOCATE (permafrost(kjpindex),stat=ier)
        IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of permafrost','','')

        ALLOCATE (excess_ice(kjpindex),stat=ier)
        IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of excess_ice','','')

        ALLOCATE (overburden(kjpindex),stat=ier)
        IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of overburden','','')
    END IF

    ALLOCATE (veget_mask_2d(kjpindex,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of veget_mask_2d','','')

    ALLOCATE (dz5(ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of dz5','','')

    ALLOCATE (mc_layt(kjpindex,ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of mc_layt','','')

    ALLOCATE (mcl_layt(kjpindex,ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of mcl_layt','','')

    ALLOCATE (tmc_layt(kjpindex,ngrnd),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of tmc_layt','','')

    ALLOCATE (mc_layt_pft(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of mc_layt_pft','','')

    ALLOCATE (mcl_layt_pft(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of mcl_layt_pft','','')

    ALLOCATE (tmc_layt_pft(kjpindex,ngrnd,nvm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of tmc_layt_pft','','')

    ALLOCATE (mcs(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of mcs','','')

    ALLOCATE (SMCMAX(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of SMCMAX','','')

    ALLOCATE (QZ(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of QZ','','')

    ALLOCATE (so_capa_dry_ns(nscm),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'thermosoil_initialize', 'Error in allocation of so_capa_dry_ns','','')


!! Soil texture choose
    SELECTCASE (nscm)
    CASE (3)
       SMCMAX(:) = SMCMAX_fao(:)
       QZ(:) = QZ_fao(:)
       so_capa_dry_ns(:) = so_capa_dry_ns_fao(:)
       mcs(:) = mcs_fao(:)
    CASE (12)
       SMCMAX(:) = SMCMAX_usda(:)
      QZ(:) = QZ_usda(:)
       so_capa_dry_ns(:) = so_capa_dry_ns_usda(:)
       mcs(:) = mcs_usda(:)
    CASE DEFAULT
       WRITE (numout,*) 'Unsupported soil type classification. Choose between zobler, fao and usda according to the map'
       STOP 'thermosoil_initialize'
    ENDSELECT

    !! 2. Initialize variable from restart file or with default values 

    !! Reads restart files for soil temperatures only. If no restart file is
    !! found,  the initial soil temperature is by default set to 280K at all depths. The user
    !! can decide to initialize soil temperatures at an other value, in which case he should set the flag THERMOSOIL_TPRO
    !! to this specific value in the run.def.

       IF (printlev>=3) WRITE (numout,*) ' we have to READ a restart file for THERMOSOIL variables'

        ptn(:,:,:) = val_exp
        CALL ioconf_setatt_p('UNITS', 'K')
        CALL ioconf_setatt_p('LONG_NAME','Soil Temperature profile')
        CALL restget_p (rest_id, 'ptn', nbp_glo, ngrnd, nvm, kjit, .TRUE., ptn, "gather", nbp_glo, index_g) !need to add veg dim

        !Config Key   = THERMOSOIL_TPRO
        !Config Desc  = Initial soil temperature profile if not found in restart
        !Config Def   = 280.
        !Config If    = OK_SECHIBA
        !Config Help  = The initial value of the temperature profile in the soil if 
        !Config         its value is not found in the restart file. This should only 
        !Config         be used if the model is started without a restart file. Here
        !Config         we only require one value as we will assume a constant 
        !Config         throughout the column.
        !Config Units = Kelvin [K]
        !
        IF (read_permafrost_map) THEN
            CALL read_permafrostmap(kjpindex,lalo,overburden,excess_ice,permafrost)
            ptn(:,:,:)=280._r_std
            DO jv=1,nvm
               DO i=1, ngrnd
                  WHERE (veget_mask_2d(:,jv) .AND. (ptn(:,i,jv).eq.val_exp.AND.permafrost(:).eq.1))
                     ptn(:,i,jv)=272._r_std
                  ENDWHERE
               ENDDO ! i=1, ngrnd
            ENDDO ! jv=1,nvm
        ELSE IF (read_reftemp) THEN
            IF ( ALL( ptn(:,:,:) == val_exp ) ) THEN
               CALL read_reftempfile(kjpindex,lalo,reftemp)
               DO jv = 1,nvm
                  ptn(:,:,jv)=reftemp(:,:)
               ENDDO ! jv = 1,nvm
            ENDIF
        ELSE
            CALL setvar_p (ptn, val_exp,'THERMOSOIL_TPRO',272._r_std)
        ENDIF
 
        ! Initialize ptn_beg (variable needed in thermosoil_coef before calucation in thermosoil_energy)
        ptn_beg(:,:,:) = ptn(:,:,:)

        ! Initialize temp_sol_beg with values from previous time-step
        temp_sol_beg(:) = temp_sol_new(:) 

        shum_ngrnd_permalong(:,:,:) = val_exp
        CALL ioconf_setatt_p('UNITS', '-')
        CALL ioconf_setatt_p('LONG_NAME','Long-term soil humidity')
        CALL restget_p (rest_id, 'shum_ngrnd_prmlng', nbp_glo, ngrnd, nvm, kjit, .TRUE.,shum_ngrnd_permalong, "gather", nbp_glo, index_g) 

        shum_ngrnd_perma(:,:,:) = val_exp
        CALL ioconf_setatt_p('UNITS', '-')
        CALL ioconf_setatt_p('LONG_NAME','soil humidity')
        CALL restget_p (rest_id, 'shum_ngrnd_perma', nbp_glo, ngrnd, nvm, kjit, .TRUE.,shum_ngrnd_perma, "gather", nbp_glo, index_g) 

        IF ( ALL(ABS(shum_ngrnd_perma(:,:,:)-val_exp).LT.EPSILON(val_exp)) ) THEN
           shum_ngrnd_perma = 1.
        ENDIF
        IF ( ALL(ABS(shum_ngrnd_permalong(:,:,:)-val_exp).LT.EPSILON(val_exp)) ) THEN
           shum_ngrnd_permalong = 1.
        ENDIF

        IF (ok_Ecorr) THEN
            CALL restget_p (rest_id, 'e_soil_lat', nbp_glo, nvm, 1, kjit,.TRUE.,e_soil_lat, "gather", nbp_glo, index_g)
            CALL setvar_p (e_soil_lat, val_exp,'NO_KEYWORD',zero)
        ENDIF

        IF (printlev>=3) WRITE (numout,*) ' thermosoil_init done '

        veget_max_bg(:,2:nvm) = veget_max(:,2:nvm)
        veget_max_bg(:,1) = MAX((un - SUM(veget_max(:,2:nvm), 2)), zero)

        ! Initialize ptn_pftmean 
        DO m=1,nvm
            DO jg = 1, ngrnd
                ptn_pftmean(:,jg) = ptn_pftmean(:,jg) + ptn(:,jg,m) * veget_max_bg(:,m)
            ENDDO ! jg = 1, ngrnd
        ENDDO ! m=1,nvm

        CALL getin_p('OK_ZIMOV',ok_zimov)
        
        !! 1.1. Allocate and initialize soil temperatures variables
        !! by reading restart files or using default values.
        ! CALL thermosoil_init (kjit, ldrestart_read, kjpindex, index, lalo, rest_id, &
        !                      & snowdz)

        !! 1.2.Computes physical constants and arrays; initializes soil thermal properties; produces the first stempdiag
        !!  Computes some physical constants and arrays depending on the soil vertical discretization 
        !! (lskin, cstgrnd, zz, zlt, dz1, dz2); get the vertical humidity onto the thermal levels, and 
        !! initializes soil thermal properties (pkappa, pcapa); produces the first temperature diagnostic stempdiag.
        
        CALL thermosoil_var_init (kjpindex, &
        &        shumdiag_perma, stempdiag, profil_froz, snowdz, &
        & thawed_humidity, organic_layer_thick, soilc_total, veget_max_bg, njsc, &
        & mc_layh, mcl_layh, tmc_layh, mc_layh_pft, mcl_layh_pft, tmc_layh_pft, &
        & snowrho, snowtemp, pb)
        !
        !! 1.3. Computes cgrd, dgrd, soilflx and soilcap coefficients from restart values or initialisation values.
        ! computes cgrd and dgrd coefficient from previous time step (restart)
        !
        CALL thermosoil_coef (&
            kjpindex,    temp_sol_new, temp_sol_new_pft, snow,   &
            soilcap,  soilcap_pft,   soilflx,   soilflx_pft,   njsc, &
            cgrnd,       dgrnd,        profil_froz, pcappa_supp, &
            organic_layer_thick, soilc_total, veget_max, snowdz, &
            snowrho,     snowtemp,     pb,  &
            frac_snow_veg, frac_snow_nobio, totfrac_nobio, &
            lambda_snow,   cgrnd_snow,      dgrnd_snow)
!!<<<<<<< .mine
!!            kjpindex,    temp_sol_new, temp_sol_new_pft,  snow,    &
!!            ptn,         soilcap,   soilcap_pft,   soilflx,  soilflx_pft,   zz,      &
!!            dz1,         dz2,          dz5,         njsc,    &
!!            cgrnd,       dgrnd,        zdz1_soil,   zdz2_soil, &
!!            cgrnd_soil,  dgrnd_soil,   profil_froz, pcappa_supp, &
!!            organic_layer_thick, soilc_total, veget_max, snowdz )
!!        
!!=======
!!            kjpindex,    temp_sol_new, snow,                 &
!!            soilcap,     soilflx,      njsc,                 &
!!            cgrnd,       dgrnd,        profil_froz, pcappa_supp, &
!!            organic_layer_thick, soilc_total, veget_max, snowdz, &
!!            snowrho,     snowtemp,     pb,  &
!!            frac_snow_veg, frac_snow_nobio, totfrac_nobio, &
!!            lambda_snow,   cgrnd_snow,      dgrnd_snow)
!!>>>>>>> .r3384done

    !     make vegetation masks so that we don't bother to calculated pfts on
    !     gridcells where they don's exist
    veget_max_bg(:,2:nvm) = veget_max(:,2:nvm)
    veget_max_bg(:,1) = MAX((un - SUM(veget_max(:,2:nvm), 2)), zero)
    veget_mask_2d(:,:) = .TRUE.
    !veget_mask_2d(:,:) = ( veget_max_bg(:,:) .GT. EPSILON(0.))
    WHERE ( veget_mask_2d(:,:) )
       veget_mask_real(:,:) = un
    ELSEWHERE
       veget_mask_real(:,:) = zero
    END WHERE

    ! Read gtemp from restart file
    CALL restget_p (rest_id, 'gtemp', nbp_glo, 1, 1, kjit, .TRUE., &
         gtemp, "gather", nbp_glo, index_g)
    CALL setvar_p (gtemp, val_exp,'NO_KEYWORD',zero)
    

    ! Read variables calculated in thermosoil_coef from restart file
    ! If the variables were not found in the restart file, the logical 
    ! calculate_coef will be true and thermosoil_coef will be called further below.
    ! These variables need to be in the restart file to avoid a time shift that
    ! would be done using thermosoil_coef at this stage.
    calculate_coef=.FALSE.
    CALL ioconf_setatt_p('UNITS', 'J m-2 K-1')
    CALL ioconf_setatt_p('LONG_NAME','Apparent surface heat capacity')
    CALL restget_p (rest_id, 'soilcap', nbp_glo, 1, 1, kjit, .TRUE., &
         soilcap, "gather", nbp_glo, index_g)
    IF (ALL(soilcap(:)==val_exp)) calculate_coef=.TRUE.

    CALL restget_p (rest_id, 'soilcap_pft', nbp_glo, nvm, 1, kjit, .TRUE., &
        soilcap_pft, "gather", nbp_glo, index_g)
    IF (ALL(soilcap_pft(:,:) == val_exp)) calculate_coef=.TRUE.

    CALL ioconf_setatt_p('UNITS', 'W m-2')
    CALL ioconf_setatt_p('LONG_NAME','Apparent soil heat flux')
    CALL restget_p (rest_id, 'soilflx', nbp_glo, 1, 1, kjit, .TRUE., &
         soilflx, "gather", nbp_glo, index_g)
    IF (ALL(soilflx(:)==val_exp)) calculate_coef=.TRUE.

    CALL restget_p (rest_id, 'soilflx_pft', nbp_glo, nvm, 1, kjit, .TRUE., &
        soilflx_pft, "gather", nbp_glo, index_g)
    IF (ALL(soilflx_pft(:,:) == val_exp)) calculate_coef=.TRUE.

    CALL ioconf_setatt_p('UNITS', 'J m-2 K-1') 
    CALL ioconf_setatt_p('LONG_NAME','Integration coefficient for the numerical scheme') 
    CALL restget_p (rest_id, 'cgrnd', nbp_glo, ngrnd-1, 1, kjit, .TRUE., & 
          cgrnd, "gather", nbp_glo, index_g) 
    IF (ALL(cgrnd(:,:,:)==val_exp)) calculate_coef=.TRUE. 

    CALL ioconf_setatt_p('UNITS', '') 
    CALL ioconf_setatt_p('LONG_NAME','Integration coefficient for the numerical scheme') 
    CALL restget_p (rest_id, 'dgrnd', nbp_glo, ngrnd-1, 1, kjit, .TRUE., & 
          dgrnd, "gather", nbp_glo, index_g) 
    IF (ALL(dgrnd(:,:,:)==val_exp)) calculate_coef=.TRUE. 

    CALL ioconf_setatt_p('UNITS', '')
    CALL ioconf_setatt_p('LONG_NAME','Integration coefficient for the numerical scheme')
    CALL restget_p (rest_id, 'cgrnd_snow', nbp_glo, nsnow, 1, kjit, .TRUE., &
         cgrnd_snow, "gather", nbp_glo, index_g)
    IF (ALL(cgrnd_snow(:,:)==val_exp)) calculate_coef=.TRUE.

    CALL ioconf_setatt_p('UNITS', '')
    CALL ioconf_setatt_p('LONG_NAME','Integration coefficient for the numerical scheme')
    CALL restget_p (rest_id, 'dgrnd_snow', nbp_glo, nsnow, 1, kjit, .TRUE., &
         dgrnd_snow, "gather", nbp_glo, index_g)
    IF (ALL(dgrnd_snow(:,:)==val_exp)) calculate_coef=.TRUE.

    CALL ioconf_setatt_p('UNITS', '')
    CALL ioconf_setatt_p('LONG_NAME','Coefficient of the linear extrapolation of surface temperature')
    CALL restget_p (rest_id, 'lambda_snow', nbp_glo, 1, 1, kjit, .TRUE., &
         lambda_snow, "gather", nbp_glo, index_g)
    IF (ALL(lambda_snow(:)==val_exp)) calculate_coef=.TRUE.

    !! 2.2.Computes physical constants and arrays; initializes soil thermal properties; produces the first stempdiag
    !!  Computes some physical constants and arrays depending on the soil vertical discretization 
    !! (lskin, cstgrnd, zz, zlt, dz1, dz2); get the vertical humidity onto the thermal levels
    CALL thermosoil_var_init (kjpindex, &
                        shumdiag_perma, stempdiag, profil_froz, snowdz, &
                        thawed_humidity, organic_layer_thick, soilc_total, veget_max, njsc, &
                        mc_layh, mcl_layh, tmc_layh,  mc_layh_pft, mcl_layh_pft, tmc_layh_pft, &
                        snowrho, snowtemp, pb)

    !! 2.3. Computes cgrnd, dgrnd, soilflx and soilcap coefficients only if they were not found in restart file.
    IF (calculate_coef) THEN
       ! Interpolate variables needed by thermosoil_coef to the thermal levels
       CALL thermosoil_humlev(kjpindex, shumdiag_perma, thawed_humidity, mc_layh, mcl_layh, tmc_layh, &
                                mc_layh_pft, mcl_layh_pft, tmc_layh_pft)

       IF (printlev>=3) WRITE (numout,*) 'thermosoil_coef will be called in the intialization phase'

       CALL thermosoil_coef (&
            kjpindex,    temp_sol_new, temp_sol_new_pft, snow,   &
            soilcap,  soilcap_pft,  soilflx,  soilflx_pft,  njsc,  &
            cgrnd,       dgrnd,        profil_froz, pcappa_supp, &
            organic_layer_thick, soilc_total, veget_max, snowdz, &
            snowrho, snowtemp, pb, &
            frac_snow_veg,frac_snow_nobio,totfrac_nobio,         &
            lambda_snow,   cgrnd_snow,      dgrnd_snow)

    END IF

  END SUBROUTINE thermosoil_initialize


!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_main
!!
!>\BRIEF        Thermosoil_main computes the soil thermal properties and dynamics, ie solves
!! the heat diffusion equation within the soil. 
!!
!! DESCRIPTION : The resolution of the soil heat diffusion equation 
!! relies on a numerical finite-difference implicit scheme
!! fully described in the reference and in the header of the thermosoil module.
!! - The dependency of the previous timestep hidden in the 
!! integration coefficients cgrnd and dgrnd (EQ1), calculated in thermosoil_coef, and 
!! called at the end of the routine to prepare for the next timestep.
!! - The effective computation of the new soil temperatures is performed in thermosoil_profile. 
!!
!! - thermosoil_coef calculates the coefficients for the numerical scheme for the very first iteration of thermosoil;
!! after that, thermosoil_coef is called only at the end of the module to calculate the coefficients for the next timestep.
!! - thermosoil_profile solves the numerical scheme.\n
!!
!! - Flags : one unique flag : THERMOSOIL_TPRO (to be set to the desired initial soil in-depth temperature in K; by default 280K)
!!
!! RECENT CHANGE(S) : Change vertical discretization (consistent with hydrology layers) and soil thermal properties (taking into account soil texture effects).
!!
!! MAIN OUTPUT VARIABLE(S): vertically discretized soil temperatures ptn, soil
!! thermal properties (pcapa, pkappa), apparent surface heat capacity (soilcap)
!! and heat flux (soilflx) to be used in enerbil at the next timestep to solve
!! the surface energy balance.
!!
!! REFERENCE(S) : 
!! - Hourdin, F. (1992). Study and numerical simulation of the general circulation of planetary atmospheres,
!!  Ph.D. thesis, Paris VII University. Remark: the part of F. Hourdin' s PhD thesis relative to the thermal
!!  integration scheme has been scanned and is provided along with the documentation, with name : 
!!  Hourdin_1992_PhD_thermal_scheme.pdf
!!
!! FLOWCHART    : 
!! \latexonly
!! \includegraphics[scale = 1]{thermosoil_flowchart.png}
!! \endlatexonly
!! 
!! \n
!_ ================================================================================================================================

  SUBROUTINE thermosoil_main (kjit, kjpindex, &
       index, indexgrnd, &
       temp_sol_new, temp_sol_new_pft, snow, soilcap, soilcap_pft, soilflx, soilflx_pft, &
       shumdiag_perma, stempdiag, ptnlev1, hist_id, hist2_id, &
       snowdz,snowrho, snowtemp,gtemp, pb, &
       mc_layh, mcl_layh, tmc_layh, mc_layh_pft, mcl_layh_pft, tmc_layh_pft, njsc, & 
       thawed_humidity, organic_layer_thick, heat_Zimov, deeptemp_prof, deephum_prof,&
       soilc_total, veget_max, &
       frac_snow_veg,frac_snow_nobio,totfrac_nobio, temp_sol_add, &
       lambda_snow, cgrnd_snow, dgrnd_snow)


    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                            :: kjit             !! Time step number (unitless) 
    INTEGER(i_std), INTENT(in)                            :: kjpindex         !! Domain size (unitless)
    INTEGER(i_std),INTENT (in)                            :: hist_id          !! Restart_ history file identifier 
                                                                              !! (unitless)
    INTEGER(i_std),INTENT (in)                            :: hist2_id         !! history file 2 identifier (unitless)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)      :: index            !! Indeces of the points on the map (unitless)
    INTEGER(i_std),DIMENSION (kjpindex*ngrnd), INTENT (in):: indexgrnd        !! Indeces of the points on the 3D map (vertical 
                                                                              !! dimension towards the ground) (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)      :: temp_sol_new     !! Surface temperature at the present time-step,
                                                                              !! temp_sol_new is only modified for the case ok_explicitsnow
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)     :: temp_sol_new_pft !! Surface temperature at the present time-step,
                                                                              !! Ts @tex ($K$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)         :: snow             !! Snow mass @tex ($kg$) @endtex.
                                                                              !! Caveat: when there is snow on the
                                                                              !! ground, the snow is integrated into the soil for
                                                                              !! the calculation of the thermal dynamics. It means
                                                                              !! that the uppermost soil layers can completely or 
                                                                              !! partially consist in snow. In the second case, zx1
                                                                              !! and zx2 are the fraction of the soil layer 
                                                                              !! consisting in snow and 'normal' soil, respectively
                                                                              !! This is calculated in thermosoil_coef.
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: shumdiag_perma   !! Soil saturation degree (0-1, unitless)
    REAL(r_std), DIMENSION(kjpindex),   INTENT (in)       :: thawed_humidity  !! specified humidity of thawed soil
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT (in)   :: heat_Zimov   !! heating associated with decomposition
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (in) :: soilc_total  !! total soil carbon for use in thermal calcs
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)    :: veget_max        !! Fraction of vegetation type 
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT (in)    :: snowdz           !! Snow  depth
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT (inout) :: snowtemp         !! Snow temperature
    REAL(r_std), DIMENSION(kjpindex),INTENT (in)          :: organic_layer_thick !! how deep is the organic soil?
    REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT (in)    :: snowrho          !! Snow density
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)         :: pb              !! Surface presure (hPa)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: mc_layh          !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid + ice) (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: mcl_layh         !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid) (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: tmc_layh         !! Total soil moisture content for each layer in hydrol(liquid + ice) (mm)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)  :: mc_layh_pft      !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid + ice) (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)  :: mcl_layh_pft     !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid) (m3/m3)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)  :: tmc_layh_pft     !! Total soil moisture content for each layer in hydrol(liquid + ice) (mm)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)      :: njsc             !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)          :: frac_snow_veg    !! Snow cover fraction on vegeted area
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in)   :: frac_snow_nobio  !! Snow cover fraction on non-vegeted area
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)           :: totfrac_nobio    !! Total fraction of continental ice+lakes+cities+...
                                                                              !!(unitless,0-1)
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)      :: temp_sol_add     !! additional surface temperature due to the melt of first layer
                                                                              !! at the present time-step @tex ($K$) @endtex

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (out)        :: ptnlev1          !! 1st level soil temperature   
    REAL(r_std), DIMENSION (kjpindex,ndeep,nvm), INTENT (out) :: deephum_prof !! moisture on a deep thermodynamic profile for permafrost calcs
    REAL(r_std), DIMENSION (kjpindex,ndeep,nvm), INTENT (out) :: deeptemp_prof!! temp on a deep thermodynamic profile for permafrost calcs
    REAL(r_std),DIMENSION (kjpindex),INTENT(out)          :: gtemp            !! First soil layer temperature

    !! 0.3 Modified variables

    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)      :: soilcap          !! apparent surface heat capacity considering snow and soil surface
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: soilcap_pft      !! apparent surface heat capacity
                                                                              !! @tex ($J m^{-2} K^{-1}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)      :: soilflx          !! apparent soil heat flux  considering snow and soil surface
                                                                              !! @tex ($W m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: soilflx_pft      !! apparent soil heat flux @tex ($W m^{-2}$) @endtex
                                                                              !! , positive 
                                                                              !! towards the soil, writen as Qg (ground heat flux) 
                                                                              !! in the history files, and computed at the end of 
                                                                              !! thermosoil for the calculation of Ts in enerbil, 
                                                                              !! see EQ3.
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (out)   :: stempdiag        !! temperature profile @tex ($K$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT(inout)       :: lambda_snow      !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (inout):: cgrnd_snow       !! Integration coefficient for snow numerical scheme
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (inout):: dgrnd_snow       !! Integration coefficient for snow numerical scheme

    !! 0.4 Local variables

    REAL(r_std), DIMENSION (kjpindex,nvm)                 :: veget_max_bg     !! Fraction of vegetation type 
    LOGICAL, SAVE                                         :: ok_zimov
    REAL(r_std),DIMENSION (kjpindex,ngrnd)                :: pkappa_pftmean           
    INTEGER(i_std)                                        :: jv,ji,m,jg
    CHARACTER(LEN=10)                                     :: part_str        !! string suffix indicating an index
    
    
!_ ================================================================================================================================

    veget_max_bg(:,2:nvm) = veget_max(:,2:nvm)
    veget_max_bg(:,1) = MAX((un - SUM(veget_max(:,2:nvm), 2)), zero)

  !! 3. Put the soil wetness diagnostic on the levels of the soil temperature

    !!?? this could logically be put just before the last call to
    !!thermosoil_coef, as the results are used there...
    CALL thermosoil_humlev(kjpindex, shumdiag_perma, thawed_humidity, mc_layh, mcl_layh, tmc_layh, &
                            mc_layh_pft, mcl_layh_pft, tmc_layh_pft)
    
    ! Compute long-term soil humidity (for permafrost)
    !    
    IF (ok_shum_ngrnd_permalong) THEN
        CALL thermosoil_wlupdate( kjpindex, ptn, shum_ngrnd_perma, shum_ngrnd_permalong )
    ELSE
        shum_ngrnd_permalong(:,:,:)=shum_ngrnd_perma(:,:,:)
    ENDIF
  !! 4. Effective computation of the soil temperatures profile, using the cgrnd and dgrnd coefficients from previous tstep.
    CALL thermosoil_profile (kjpindex, temp_sol_new, temp_sol_new_pft, ptn, stempdiag, &
         snowtemp, veget_max,                                        & 
         frac_snow_veg, frac_snow_nobio, totfrac_nobio,              &
         cgrnd_snow,    dgrnd_snow )

  !! 5. Call to thermosoil_energy, still to be clarified..

    CALL thermosoil_energy (kjpindex, temp_sol_new,  soilcap,  veget_max_bg)
    ptn_pftmean(:,:) = zero
    DO m=1,nvm
       DO jg = 1, ngrnd
          ptn_pftmean(:,jg) = ptn_pftmean(:,jg) + ptn(:,jg,m) * veget_max_bg(:,m)
       ENDDO ! jg = 1, ngrnd
    ENDDO ! m=1,nvm
   
 
    IF ( .NOT. almaoutput ) THEN
       !!need to write with PFT dimension
       DO jv = 1, nvm
          WRITE(part_str,'(I2)') jv
          IF (jv < 10) part_str(1:1) = '0'
          CALL histwrite_p(hist_id, 'ptn_'//part_str(1:LEN_TRIM(part_str)), &
               kjit, ptn(:,:,jv), kjpindex*ngrnd, indexgrnd)
       END DO
       CALL histwrite_p(hist_id, 'ptn_pftmean', kjit, ptn_pftmean, kjpindex*ngrnd, indexgrnd)
       IF (hydrol_cwrr) THEN
         DO jv = 1, nvm
             WRITE(part_str,'(I2)') jv
             IF (jv < 10) part_str(1:1) = '0'

             IF (ok_freeze_thermix .AND. permafrost_veg_exists(jv)) THEN
                 CALL histwrite_p(hist_id, 'pcapa_'//part_str(1:LEN_TRIM(part_str)), &
                      kjit, pcapa(:,:,jv), kjpindex*ngrnd, indexgrnd)
                 CALL histwrite_p(hist_id, 'pcappa_supp_'//part_str(1:LEN_TRIM(part_str)), &
                      kjit, pcappa_supp(:,:,jv), kjpindex*ngrnd, indexgrnd)
                 CALL histwrite_p(hist_id, 'pkappa_'//part_str(1:LEN_TRIM(part_str)), &
                      kjit, pkappa(:,:,jv), kjpindex*ngrnd, indexgrnd)
             ENDIF

             CALL histwrite_p(hist_id, 'shum_ngrnd_perma_'//part_str(1:LEN_TRIM(part_str)), &
                  kjit, shum_ngrnd_perma(:,:,jv), kjpindex*ngrnd, indexgrnd)
             CALL histwrite_p(hist_id,'shum_ngrnd_prmlng_'//part_str(1:LEN_TRIM(part_str)), &
                  kjit, shum_ngrnd_permalong(:,:,jv), kjpindex*ngrnd, indexgrnd)
             IF (ok_freeze_thermix .AND. permafrost_veg_exists(jv)) THEN
                  CALL histwrite_p(hist_id,'ptn_beg_'//part_str(1:LEN_TRIM(part_str)), &
                      kjit, ptn_beg(:,:,jv), kjpindex*ngrnd, indexgrnd)
             ENDIF
             !CALL histwrite_p(hist_id,'profil_froz_'//part_str(1:LEN_TRIM(part_str)), &
             !     kjit, profil_froz(:,:,jv), kjpindex*ngrnd, indexgrnd)
         END DO
         !CALL histwrite_p(hist_id, 'shumdiag_perma', kjit, shumdiag_perma, kjpindex*nbdl, indexnbdl)
       END IF
      CALL histwrite_p(hist_id, 'Qg', kjit, soilflx, kjpindex, index)

    ELSE !IF ( .NOT. almaoutput ) THEN
      CALL histwrite_p(hist_id, 'SoilTemp', kjit, ptn, kjpindex*ngrnd, indexgrnd)
      CALL histwrite_p(hist_id, 'Qg', kjit, soilflx, kjpindex, index)
      CALL histwrite_p(hist_id, 'DelSurfHeat', kjit, surfheat_incr, kjpindex, index)
      CALL histwrite_p(hist_id, 'DelColdCont', kjit, coldcont_incr, kjpindex, index)
    ENDIF  !IF ( .NOT. almaoutput ) THEN
    IF ( hist2_id > 0 ) THEN
       IF ( .NOT. almaoutput ) THEN
          CALL histwrite_p(hist2_id, 'ptn_pftmean', kjit, ptn_pftmean, kjpindex*ngrnd, indexgrnd)
       ELSE
          CALL histwrite_p(hist2_id, 'SoilTemp', kjit, ptn, kjpindex*ngrnd, indexgrnd)
          CALL histwrite_p(hist2_id, 'Qg', kjit, soilflx, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelSurfHeat', kjit, surfheat_incr, kjpindex, index)
          CALL histwrite_p(hist2_id, 'DelColdCont', kjit, coldcont_incr, kjpindex, index)
       ENDIF
    ENDIF
   
  !! 7. Considering the heat released by microbial respiration
    IF (ok_zimov) THEN
       CALL add_heat_Zimov(kjpindex, veget_max_bg, ptn, heat_zimov)
    END IF

  !! 8. A last final call to thermosoil_coef
 
    !! A last final call to thermosoil_coef, which calculates the different
    !!coefficients (cgrnd, dgrnd, soilcap, soilflx) from this time step to be
    !!used at the next time step, either in the surface temperature calculation
    !!(soilcap, soilflx) or in the soil thermal numerical scheme.
    !
    CALL thermosoil_coef (&
         kjpindex,    temp_sol_new, temp_sol_new_pft,  snow, &
         soilcap,  soilcap_pft,   soilflx,   soilflx_pft,   njsc, &
         cgrnd,       dgrnd,        profil_froz, pcappa_supp, &
         organic_layer_thick, soilc_total, veget_max_bg, snowdz, &
         snowrho,         snowtemp,       pb,           &
         frac_snow_veg, frac_snow_nobio, totfrac_nobio, &
         lambda_snow,   cgrnd_snow,      dgrnd_snow)

    !save some useful variables for new snow model
    ptn_pftmean(:,:) = zero
    pkappa_pftmean(:,:) = zero
    DO m=1,nvm
       DO jg = 1, ngrnd
          ptn_pftmean(:,jg) = ptn_pftmean(:,jg) + ptn(:,jg,m) * veget_max_bg(:,m)
          pkappa_pftmean(:,jg) = pkappa_pftmean(:,jg) + pkappa(:,jg,m) * veget_max_bg(:,m)
       END DO
    END DO

    ! Save variables for explicit snow model

    DO ji=1,kjpindex
       gtemp(ji) = ptn_pftmean(ji,1)
    ENDDO

    ptnlev1(:) = ptn_pftmean(:,1)

    !++cdk prep updated temp and moisture fields so they can be sent to stomate
    !permafrost calcs
    deephum_prof = shum_ngrnd_permalong
    deeptemp_prof = ptn
    !--cdk

#ifdef STRICT_CHECK    
    IF (ANY(deeptemp_prof > 500)) THEN
        CALL ipslerr_p(3, 'thermosoil_main', 'deeptemp_prof is bigger than 500 degrees kelvin', '', '')
    ENDIF
#endif

    !! Surface temperature is forced to zero celcius if its value is larger than melting point, only for explicit snow scheme
    IF  (ok_explicitsnow) THEN
       DO ji=1,kjpindex
          IF  (SUM(snowdz(ji,:)) .GT. 0.0) THEN
             WRITE(numout,*) 'xuhui: force soil temperature 0 when covered by snow'
             IF (temp_sol_new(ji) .GE. tp_00) THEN
                temp_sol_new(ji) = tp_00
             ENDIF
          END IF
       END DO
    ENDIF

    IF (printlev>=3) WRITE (numout,*) ' thermosoil_main done '

  END SUBROUTINE thermosoil_main

  !!  =============================================================================================================================
  !! SUBROUTINE                             : thermosoil_finalize
  !!
  !>\BRIEF                                    Write to restart file
  !!
  !! DESCRIPTION                            : This subroutine writes the module variables and variables calculated in thermosoil 
  !!                                          to restart file
  !! \n
  !_ ==============================================================================================================================
  SUBROUTINE thermosoil_finalize (kjit,    kjpindex, rest_id,   gtemp, &
                                  soilcap, soilcap_pft, soilflx, soilflx_pft, lambda_snow, cgrnd_snow, dgrnd_snow)

    !! 0. Variable and parameter declaration
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                            :: kjit             !! Time step number (unitless)  
    INTEGER(i_std), INTENT(in)                            :: kjpindex         !! Domain size (unitless) 
    INTEGER(i_std),INTENT (in)                            :: rest_id          !! Restart file identifier(unitless) 
    !! 0.2 Modified variables 
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)      :: soilcap          !! apparent surface heat capacity
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: soilcap_pft      !! apparent surface heat capacity
                                                                              !! @tex ($J m^{-2} K^{-1}$) @endtex
    REAL(r_std),DIMENSION (kjpindex), INTENT (inout)      :: soilflx          !! apparent soil heat flux @tex ($W m^{-2}$) @endtex
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (inout)  :: soilflx_pft      !! apparent soil heat flux @tex ($W m^{-2}$) @endtex
                                                                              !! , positive 
                                                                              !! towards the soil, writen as Qg (ground heat flux) 
                                                                              !! in the history files, and computed at the end of 
                                                                              !! thermosoil for the calculation of Ts in enerbil, 
                                                                              !! see EQ3.
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)         :: lambda_snow      !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (in)  :: cgrnd_snow       !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (in)  :: dgrnd_snow       !! Integration coefficient for snow numerical scheme

    REAL(r_std),DIMENSION (kjpindex),INTENT(in)           :: gtemp            !! the first soil layer temperature
    !! 0.3 Local variables
    INTEGER(i_std)                                        :: m
    CHARACTER(LEN=80)                                     :: var_name         !! To store variables names for I/O


!_ ================================================================================================================================
    
    !! 1. Write variables to restart file to be used for the next simulation
    IF (printlev>=3) WRITE (numout,*) 'Write restart file with THERMOSOIL variables'
    
    !! 2. Prepares the restart files for the next simulation

        IF (printlev>=3) WRITE (numout,*) ' we have to complete restart file with THERMOSOIL variables'

        CALL restput_p (rest_id, 'ptn', nbp_glo, ngrnd, nvm, kjit, ptn, 'scatter', nbp_glo, index_g)

        IF (ok_shum_ngrnd_permalong) THEN
           CALL restput_p (rest_id, 'shum_ngrnd_prmlng', nbp_glo, ngrnd, nvm, kjit,shum_ngrnd_permalong, 'scatter', nbp_glo, index_g) !need to add veg dim    
        END IF

        CALL restput_p (rest_id, 'shum_ngrnd_perma', nbp_glo, ngrnd, nvm, kjit, shum_ngrnd_perma, 'scatter', nbp_glo, index_g)      !need to add veg dim

        IF (ok_Ecorr) THEN
           var_name = 'e_soil_lat' 
           CALL restput_p (rest_id, var_name, nbp_glo, nvm, 1, kjit, e_soil_lat, 'scatter', nbp_glo, index_g)
        END IF

        CALL restput_p (rest_id, 'cgrnd', nbp_glo, ngrnd-1, nvm, kjit, cgrnd, 'scatter', nbp_glo, index_g)
        CALL restput_p (rest_id, 'dgrnd', nbp_glo, ngrnd-1, nvm, kjit, dgrnd, 'scatter', nbp_glo, index_g)
        CALL restput_p (rest_id, 'z1', nbp_glo, 1, 1, kjit, z1, 'scatter', nbp_glo, index_g)
        CALL restput_p (rest_id, 'pcapa', nbp_glo, ngrnd, nvm, kjit, pcapa, 'scatter', nbp_glo, index_g)
        CALL restput_p (rest_id, 'pcapa_en', nbp_glo, ngrnd, nvm, kjit, pcapa_en, 'scatter', nbp_glo, index_g)
        CALL restput_p (rest_id, 'pkappa', nbp_glo, ngrnd, nvm, kjit, pkappa, 'scatter', nbp_glo, index_g)

        var_name= 'temp_sol_beg'
        CALL restput_p(rest_id, var_name, nbp_glo, 1, 1, kjit, temp_sol_beg, 'scatter', nbp_glo, index_g)
 
        CALL restput_p(rest_id, 'gtemp', nbp_glo, 1, 1, kjit, gtemp, 'scatter', nbp_glo, index_g)

        var_name= 'soilcap'  
        CALL restput_p(rest_id, var_name, nbp_glo,   1, 1, kjit,  soilcap, 'scatter',  nbp_glo, index_g)
        CALL restput_p(rest_id, 'soilcap_pft', nbp_glo, nvm, 1, kjit, soilcap_pft, 'scatter', nbp_glo, index_g)
        
        var_name= 'soilflx'  
        CALL restput_p(rest_id, var_name, nbp_glo,   1, 1, kjit,  soilflx, 'scatter',  nbp_glo, index_g)
        CALL restput_p(rest_id, 'soilflx_pft', nbp_glo, nvm, 1, kjit,  soilflx_pft, 'scatter',  nbp_glo, index_g)

        CALL restput_p(rest_id, 'cgrnd_snow', nbp_glo, nsnow, 1, kjit, cgrnd_snow, 'scatter', nbp_glo, index_g)
        CALL restput_p(rest_id, 'dgrnd_snow', nbp_glo, nsnow, 1, kjit, dgrnd_snow, 'scatter', nbp_glo, index_g)
        CALL restput_p(rest_id, 'lambda_snow', nbp_glo, 1, 1, kjit, lambda_snow, 'scatter', nbp_glo, index_g)

END SUBROUTINE thermosoil_finalize
 
!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_clear
!!
!>\BRIEF        Deallocates the allocated arrays.
!! The call of thermosoil_clear originates from sechiba_clear but the calling sequence and 
!! its purpose require further investigation.
!!
!! DESCRIPTION  : None
!!
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : None 
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

 SUBROUTINE thermosoil_clear()

        IF ( ALLOCATED (ptn)) DEALLOCATE (ptn)
        IF ( ALLOCATED (ptn_pftmean)) DEALLOCATE (ptn_pftmean)
        IF ( ALLOCATED (z1)) DEALLOCATE (z1) 
        IF ( ALLOCATED (cgrnd)) DEALLOCATE (cgrnd) 
        IF ( ALLOCATED (dgrnd)) DEALLOCATE (dgrnd) 
        IF ( ALLOCATED (pcapa)) DEALLOCATE (pcapa)
        IF ( ALLOCATED (pkappa))  DEALLOCATE (pkappa)
        IF ( ALLOCATED (pcapa_snow)) DEALLOCATE (pcapa_snow)
        IF ( ALLOCATED (pkappa_snow))  DEALLOCATE (pkappa_snow)
        IF ( ALLOCATED (pcapa_en)) DEALLOCATE (pcapa_en)
        IF ( ALLOCATED (ptn_beg)) DEALLOCATE (ptn_beg)
        IF ( ALLOCATED (temp_sol_beg)) DEALLOCATE (temp_sol_beg)
        IF ( ALLOCATED (surfheat_incr)) DEALLOCATE (surfheat_incr)
        IF ( ALLOCATED (coldcont_incr)) DEALLOCATE (coldcont_incr)
        IF ( ALLOCATED (shum_ngrnd_perma)) DEALLOCATE (shum_ngrnd_perma)
        IF ( ALLOCATED (profil_froz)) DEALLOCATE (profil_froz)
        IF ( ALLOCATED (shum_ngrnd_permalong)) DEALLOCATE (shum_ngrnd_permalong)
        IF ( ALLOCATED (mc_layt)) DEALLOCATE (mc_layt)
        IF ( ALLOCATED (mcl_layt)) DEALLOCATE (mcl_layt)
        IF ( ALLOCATED (tmc_layt)) DEALLOCATE (tmc_layt)
        IF ( ALLOCATED (mc_layt_pft)) DEALLOCATE (mc_layt_pft)
        IF ( ALLOCATED (mcl_layt_pft)) DEALLOCATE (mcl_layt_pft)
        IF ( ALLOCATED (tmc_layt_pft)) DEALLOCATE (tmc_layt_pft)

 END SUBROUTINE thermosoil_clear


!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_var_init
!!
!>\BRIEF        Define and initializes the soil thermal parameters
!!		  
!! DESCRIPTION	: This routine\n
!! 1. Defines the parameters ruling the vertical grid of the thermal scheme (fz1, zalpha).\n
!! 2. Defines the scaling coefficients for adimensional depths (lskin, cstgrnd, see explanation in the 
!!    variables description of thermosoil_main). \n
!! 3. Calculates the vertical discretization of the soil (zz, zlt, dz2) and the constants used
!!    in the numerical scheme and which depend only on the discretization (dz1, lambda).\n
!! 4. Initializes the soil thermal parameters (capacity, conductivity) based on initial soil moisture content.\n
!! 5. Produces a first temperature diagnostic based on temperature initialization.\n
!!
!! The scheme comprizes ngrnd=7 layers by default.
!! The layer' s boundaries depths (zlt) follow a geometric series of ratio zalph=2 and first term fz1.\n
!! zlt(jg)=fz1.(1-zalph^jg)/(1-zalph) \n
!! The layers' boudaries depths are first calculated 'adimensionally', ie with a
!! discretization adapted to EQ5. This discretization is chosen for its ability at
!! reproducing a thermal signal with periods ranging from days to centuries. (see
!! Hourdin, 1992). Typically, fz1 is chosen as : fz1=0.3*cstgrnd (with cstgrnd the
!! adimensional attenuation depth). \n
!! The factor lskin/cstgrnd is then used to go from adimensional depths to
!! depths in m.\n
!! zz(real)=lskin/cstgrnd*zz(adimensional)\n
!! Similarly, the depths of the numerical nodes are first calculated
!! adimensionally, then the conversion factor is applied.\n
!! the numerical nodes (zz) are not exactly the layers' centers : their depths are calculated as follows:\n
!! zz(jg)=fz1.(1-zalph^(jg-1/2))/(1-zalph)\n
!! The values of zz and zlt used in the default thermal discretization are in the following table.
!! \latexonly
!! \includegraphics{thermosoil_var_init1.jpg}
!! \endlatexonly\n
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : None
!!
!! REFERENCE(S)	:
!! - Hourdin, F. (1992). Study and numerical simulation of the general circulation of 
!! planetary atmospheres, Ph.D. thesis, Paris VII University.
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE thermosoil_var_init(kjpindex, &
  & shumdiag_perma, stempdiag, profil_froz,snowdz, &
  & thawed_humidity,organic_layer_thick, soilc_total, veget_max, njsc, &
  & mc_layh, mcl_layh, tmc_layh, mc_layh_pft, mcl_layh_pft, tmc_layh_pft, &
  & snowrho, snowtemp, pb )

  !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                               :: kjpindex          !! Domain size (unitless)
    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (in)      :: shumdiag_perma    !! Relative soil humidity on the diagnostic axis 
                                                                                  !! (unitless), [0,1]. (see description of the 
                                                                                  !! variables of thermosoil_main for more 
                                                                                  !! explanations) 
    
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: veget_max         !! Fraction of vegetation type 
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)           :: thawed_humidity   !! specified humidity of thawed soil

    REAL(r_std), DIMENSION (kjpindex,nsnow),INTENT(in)       :: snowdz
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)           :: organic_layer_thick      !! how deep is the organic soil?    
    REAL(r_std), DIMENSION (kjpindex,ndeep,nvm), INTENT (in) :: soilc_total       !! total soil carbon for use in thermal calcs
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: njsc              !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)       :: mc_layh           !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid+ice) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)       :: mcl_layh          !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)       :: tmc_layh          !! Total soil moisture content for each layer in hydrol(liquid+ice) [mm]
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)      :: snowrho           !!Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)      :: snowtemp          !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)           :: pb                !! Surface presure (hPa)
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)   :: mc_layh_pft        !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid+ice) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)   :: mcl_layh_pft      !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in)   :: tmc_layh_pft      !! Total soil moisture content for each layer in hydrol(liquid+ice) [mm]

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex,nslm), INTENT (out)     :: stempdiag         !! Diagnostic temperature profile @tex ($K$)
                                                                                  !! @endtex
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm), INTENT(out) :: profil_froz

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                           :: jg       !! Index (unitless)


    !! 1. Initialization of the parameters of the vertical discretization and of the attenuation depths
    
    dz5(:) = 0.0
    !! Calculate so_capa_ice
    so_capa_ice = so_capa_dry + poros*capa_ice*rho_ice
    WRITE(numout,*) 'Calculation of so_capa_ice=', so_capa_ice,' using poros=',poros,' and capa_ice=',capa_ice
    
    !! 2.  Get the depth of the thermal levels (numerical nodes) and the layers boundaries from vertical module

    !! 2.2 Computing some usefull constants for the numerical scheme
    DO jg=1,ngrnd-1
      dz1(jg)  = un / (znt(jg+1) - znt(jg))
      dz5(jg) = (zlt(jg) - znt(jg)) * dz1(jg)
    ENDDO
    lambda = znt(1) * dz1(1)

    !! 2.3 Making vegetation masks so that we don't bother to calculated pfts on
    !!     gridcells where they don't exist
    veget_mask_2d(:,:) = .TRUE.
    !veget_mask_2d(:,:) = ( veget_max(:,:) .GT. EPSILON(0.))
    WHERE( ALL((.NOT. veget_mask_2d(:,:)), dim=2) )
       veget_mask_2d(:,1) = .TRUE.
    END WHERE

    !! 2.4 Get the wetness profile on the thermal vertical grid from the diagnostic axis
    CALL thermosoil_humlev(kjpindex, shumdiag_perma, thawed_humidity, mc_layh, mcl_layh, tmc_layh, &
                            mc_layh_pft, mcl_layh_pft, tmc_layh_pft)
    !

    !! 2.4 Thermal conductivity at all levels
    if (ok_explicitsnow) then
       CALL thermosoil_getdiff( kjpindex, ptn, njsc, shum_ngrnd_permalong, & 
                & profil_froz, pcappa_supp, organic_layer_thick, soilc_total, &
                  snowrho, snowtemp, pb)
       ! this is for the thin snow in order to prevent the warm surface
       CALL thermosoil_getdiff_thinsnow (kjpindex, ptn, shum_ngrnd_permalong, snowdz, profil_froz)
    else
       !if (ok_thermix_trunc) then
       !    ! pour convergence avec le trunc
       !    CALL thermosoil_getdiff_old_thermix_trunc2( kjpindex, pkappa, pcapa, pcapa_en )
       !else
       !    CALL thermosoil_getdiff_old_thermix_with_snow( kjpindex, ptn, wetdiaglong, snow, pkappa, pcapa, pcapa_en,profil_froz )
       !endif 
    endif

  !! 3. Compute a first diagnostic temperature profile

    CALL thermosoil_diaglev(kjpindex, stempdiag, veget_max)

    IF (printlev>=3) WRITE (numout,*) ' thermosoil_var_init done '

  END SUBROUTINE thermosoil_var_init
  

!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_coef
!!
!>\BRIEF        Calculate soil thermal properties, integration coefficients, apparent heat flux,
!! surface heat capacity,  
!!
!! DESCRIPTION	: This routine computes : \n
!!		1. the soil thermal properties. \n 
!!		2. the integration coefficients of the thermal numerical scheme, cgrnd and dgrnd,
!!              which depend on the vertical grid and on soil properties, and are used at the next 
!!              timestep.\n
!!              3. the soil apparent heat flux and surface heat capacity (soilflx
!!              and soilcap), used by enerbil to compute the surface temperature at the next
!!              timestep.\n
!!             -  The soil thermal properties depend on water content (shum_ngrnd_perma, shumdiag_perma, 
!!              mc_layt, mcl_layt, tmc_layt), dominant soil texture(njsc), and on the presence 
!!              of snow : snow is integrated into the soil for the thermal calculations, ie if there 
!!              is snow on the ground, the first thermal layer(s) consist in snow, depending on the 
!!              snow-depth. If a layer consists out of snow and soil, wheighed soil properties are 
!!              calculated\n
!!             - The coefficients cgrnd and dgrnd are the integration
!!              coefficients for the thermal scheme \n
!!                              T(k+1)=cgrnd(k)+dgrnd(k)*T(k) \n
!!                                      -- EQ1 -- \n
!!              They correspond respectively to $\beta$ and $\alpha$ from F. Hourdin\'s thesis and 
!!              their expression can be found in this document (eq A19 and A20)
!!             - soilcap and soilflx are the apparent surface heat capacity and flux
!!               used in enerbil at the next timestep to solve the surface
!!               balance for Ts (EQ3); they correspond to $C_s$ and $F_s$ in F.
!!               Hourdin\'s PhD thesis and are expressed in eq. A30 and A31. \n
!!                 soilcap*(Ts(t)-Ts(t-1))/dt=soilflx+otherfluxes(Ts(t)) \n
!!                                      -- EQ3 --\n
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): cgrnd, dgrnd, pcapa, pkappa, soilcap, soilflx
!!
!! REFERENCE(S) :
!! - Hourdin, F. (1992). Study and numerical simulation of the general circulation of planetary atmospheres,
!! Ph.D. thesis, Paris VII University. Remark: the part of F. Hourdin's PhD thesis relative to the thermal
!! integration scheme has been scanned and is provided along with the documentation, with name : 
!! Hourdin_1992_PhD_thermal_scheme.pdf
!!
!! FLOWCHART    : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE thermosoil_coef (kjpindex, temp_sol_new, temp_sol_new_pft, snow,              &
                              soilcap, soilcap_pft,   soilflx,  soilflx_pft,  njsc,             &
                              cgrnd,    dgrnd,        profil_froz, pcappa_supp, &
                              organic_layer_thick,    soilc_total, veget_max, snowdz, &
                              snowrho,         snowtemp,       pb,  &
                              frac_snow_veg, frac_snow_nobio, totfrac_nobio, &
                              lambda_snow,   cgrnd_snow,      dgrnd_snow)

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                                :: kjpindex     !! Domain size (unitless)
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)            :: temp_sol_new !! soil surface temperature @tex ($K$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)        :: temp_sol_new_pft !! soil surface temperature @tex ($K$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)            :: snow         !! snow mass @tex ($Kg$) @endtex
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)          :: njsc         !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)        :: veget_max    !!Fraction of vegetation type
    REAL(r_std), DIMENSION(kjpindex),   INTENT (in)           :: organic_layer_thick !! how deep is the organic soil?
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm),   INTENT (in) :: soilc_total  !! total soil carbon for use in thermal calcs
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)              :: frac_snow_veg   !! Snow cover fraction on vegeted area
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in)       :: frac_snow_nobio !! Snow cover fraction on non-vegeted area
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)               :: totfrac_nobio   !! Total fraction of continental ice+lakes+cities+...
                                                                                 !!(unitless,0-1)
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)    :: snowdz        !!Snow depth
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)    :: snowrho        !!Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)    :: snowtemp       !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)         :: pb         !! Surface presure (hPa)

    !! 0.2 Output variables

    REAL(r_std), DIMENSION (kjpindex,ngrnd-1,nvm), INTENT(out):: cgrnd        !! matrix coefficient for the computation of soil 
                                                                              !! temperatures (beta in F. Hourdin thesis)
    REAL(r_std), DIMENSION (kjpindex,ngrnd-1,nvm), INTENT(out):: dgrnd        !! matrix coefficient for the computation of soil 
                                                                              !! temperatures (alpha in F. Hourdin thesis)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm), INTENT(out)  :: profil_froz
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(out)     :: pcappa_supp
    REAL(r_std), DIMENSION (kjpindex), INTENT (out)           :: soilcap      !! surface heat capacity considering snow and soil surface
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (out)       :: soilcap_pft  !! surface heat capacity
                                                                              !! @tex ($J m^{-2} K^{-1}$) @endtex
    REAL(r_std), DIMENSION (kjpindex), INTENT (out)           :: soilflx      !! surface heat flux considering snow and soil surface @tex ($W m^{-2}$) @endtex,
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (out)       :: soilflx_pft  !! surface heat flux @tex ($W m^{-2}$) @endtex,
                                                                              !! positive towards the 
                                                                              !! soil, writen as Qg (ground heat flux) in the history 
                                                                              !! files.

    !! 0.3 Modified variable

    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)       :: lambda_snow  !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (inout):: cgrnd_snow   !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (inout):: dgrnd_snow   !! Integration coefficient for snow numerical scheme

    !! 0.4 Local variables

    REAL(r_std), DIMENSION (kjpindex,nvm)                     :: soilcap_pft_nosnow 
    REAL(r_std), DIMENSION (kjpindex,nvm)                     :: soilflx_pft_nosnow 
    REAL(r_std), DIMENSION (kjpindex)                      :: snowcap             !! apparent snow heat capacity @tex ($J m^{-2} K^{-1}$)
    REAL(r_std), DIMENSION (kjpindex)                      :: snowflx             !! apparent snow-atmosphere heat flux @tex ($W m^{-2}$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nsnow)                :: dz1_snow
    REAL(r_std), DIMENSION (kjpindex,nsnow)                :: ZSNOWDZM
    REAL(r_std), DIMENSION (kjpindex,nsnow)                :: dz2_snow
    REAL(r_std), DIMENSION (kjpindex,nsnow)                :: zdz1_snow
    REAL(r_std), DIMENSION (kjpindex,nsnow)                :: zdz2_snow
    REAL(r_std), DIMENSION (kjpindex)                      :: z1_snow

    INTEGER(i_std)                                            :: ji, jg,jv
    REAL(r_std), DIMENSION (kjpindex,ngrnd-1,nvm)             :: zdz1

    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)               :: zdz2
    REAL(r_std), DIMENSION (kjpindex)                         :: z1           !! numerical constant @tex ($W m^{-1} K^{-1}$) @endtex


    REAL(r_std), DIMENSION (kjpindex)                         :: soilcap_nosnow!! surface heat capacity
                                                                               !! @tex ($J m^{-2} K^{-1}$)
                                                                               !! @endtex
    REAL(r_std), DIMENSION (kjpindex)                         :: soilflx_nosnow!! surface heat flux @tex ($W m^{-2}$) @endtex,
                                                                               !! positive towards the soil, written as Qg
                                                                               !!(ground heat flux in the history files).

    REAL(r_std), DIMENSION (kjpindex)                      :: cgrnd_soil   !! surface soil layer
    REAL(r_std), DIMENSION (kjpindex)                      :: dgrnd_soil   !! surface soil layer
    REAL(r_std), DIMENSION (kjpindex)                      :: zdz1_soil    !! surface soil layer
    REAL(r_std), DIMENSION (kjpindex)                      :: zdz2_soil    !! surface soil layer
!_ ================================================================================================================================

    !! Initialisation of local variables
    z1(:) = zero
    zdz2(:,:,:) = zero
    zdz1(:,:,:) = zero
    soilcap_pft_nosnow(:,:) = zero
    soilflx_pft_nosnow(:,:) = zero
    snowcap(:) = zero
    snowflx(:) = zero
    dz1_snow(:,:) = zero
    ZSNOWDZM(:,:) = zero
    dz2_snow(:,:) = zero
    zdz1_snow(:,:) = zero
    zdz2_snow(:,:) = zero
    z1_snow(:) = zero
    soilcap_nosnow(:) = zero
    soilflx_nosnow(:) = zero
    cgrnd_soil(:) = zero
    dgrnd_soil(:) = zero
    zdz1_soil(:) = zero
    zdz2_soil(:) = zero
    soilcap(:) = zero
    soilflx(:) = zero
    soilcap_pft(:,:) = zero
    soilflx_pft(:,:) = zero

  !! 1. Computation of the soil thermal properties
   
    ! Computation of the soil thermal properties; snow properties are also accounted for
    IF (ok_explicitsnow) THEN
       CALL thermosoil_getdiff( kjpindex, ptn, njsc, shum_ngrnd_permalong,&
                profil_froz, pcappa_supp, organic_layer_thick, soilc_total, &
                snowrho, snowtemp, pb)
       ! this is for the thin snow in order to prevent the warm surface
       ! CALL thermosoil_getdiff_thinsnow (kjpindex, ptn, shum_ngrnd_permalong, snowdz,profil_froz)
    ELSE
           CALL thermosoil_getdiff_old_thermix_with_snow( kjpindex, snow, njsc )
    ENDIF

    ! ok_freeze_thermix must be true
    IF (ok_Ecorr) THEN
        CALL thermosoil_readjust(kjpindex, ptn)
    ENDIF

    !! 2. Computation of the coefficients of the numerical integration scheme for the soil layers

    !! 2.1 Calculate numerical coefficients zdz1 and zdz2

     DO jv=1,nvm
       DO jg=1,ngrnd
          WHERE (veget_mask_2d(:,jv))
           zdz2(:,jg,jv)=pcapa(:,jg,jv) * dlt(jg)/dt_sechiba
          ENDWHERE
       ENDDO ! DO jg=1,ngrnd
       
       DO jg=1,ngrnd-1
          WHERE (veget_mask_2d(:,jv))
           zdz1(:,jg,jv) = dz1(jg) * pkappa(:,jg,jv)
          ENDWHERE
       ENDDO !DO jg=1,ngrnd-1

       !IF (jv == 14) THEN
       !    WRITE(numout,*)'Zun 0 zdz1:',zdz1(1,:,14)
       !    WRITE(numout,*)'Zun 0 dz1:',dz1(:)
       !    WRITE(numout,*)'Zun 0 pkappa:',pkappa(1,:,14)
       !ENDIF

    !! 2.2 Calculate coefficients cgrnd and dgrnd for soil
       WHERE (veget_mask_2d(:,jv))
        z1(:) = zdz2(:,ngrnd,jv) + zdz1(:,ngrnd-1,jv)
        cgrnd(:,ngrnd-1,jv) = (phigeoth + zdz2(:,ngrnd,jv) * ptn(:,ngrnd,jv)) / z1(:)
        dgrnd(:,ngrnd-1,jv) = zdz1(:,ngrnd-1,jv) / z1(:)
       ENDWHERE

       !IF (jv == 14) THEN
       !    WRITE(numout,*)'Zun 1 z1:',z1(:)
       !    WRITE(numout,*)'Zun 1 dgrnd:',dgrnd(1,:,14)
       !    WRITE(numout,*)'Zun 1 cgrnd:',cgrnd(1,:,14)
       !ENDIF

       DO jg = ngrnd-1,2,-1
         WHERE (veget_mask_2d(:,jv))
             !ZunYin here is the problem, Z1 = NA when zdz2 and zdz1 = 0
          z1(:) = un / (zdz2(:,jg,jv) + zdz1(:,jg-1,jv) + zdz1(:,jg,jv) * (un - dgrnd(:,jg,jv)))
          cgrnd(:,jg-1,jv) = (ptn(:,jg,jv) * zdz2(:,jg,jv) + zdz1(:,jg,jv) * cgrnd(:,jg,jv)) * z1(:)
          dgrnd(:,jg-1,jv) = zdz1(:,jg-1,jv) * z1(:)
         ENDWHERE
       ENDDO ! jg = ngrnd-1,2,-1

       !IF (jv == 14) THEN
       !    WRITE(numout,*)'ZUn 2 z1:',z1(:)
       !    WRITE(numout,*)'ZUn zdz1:',zdz1(1,:,14)
       !    WRITE(numout,*)'ZUn zdz2:',zdz2(1,:,14)
       !    WRITE(numout,*)'Zun 2 dgrnd:',dgrnd(1,:,14)
       !    WRITE(numout,*)'Zun 2 cgrnd:',cgrnd(1,:,14)
       !ENDIF

       !IF (jv == 14) THEN
       !    !WRITE(numout,*)'ZUn zdz2:',zdz2(1,:,14) OK
       !    WRITE(numout,*)'ZUn 3 z1:',z1(:)
       !    WRITE(numout,*)'Zun 3 dgrnd:',dgrnd(1,:,14)
       !    WRITE(numout,*)'Zun 3 cgrnd:',cgrnd(1,:,14)
       !ENDIF

     !! 3. Computation of the apparent ground heat flux 
       
       !! Computation of the apparent ground heat flux (> towards the soil) and
       !! apparent surface heat capacity, used at the next timestep by enerbil to
       !! compute the surface temperature.

       IF (ok_LAIdev(jv)) THEN
           WHERE (veget_mask_2d(:,jv)) !soil
             soilflx_pft(:,jv) = zdz1(:,1,jv) * (cgrnd(:,1,jv) + (dgrnd(:,1,jv)-1.) * ptn(:,1,jv))
             soilcap_pft(:,jv) = (zdz2(:,1,jv) * dt_sechiba + dt_sechiba * (un - dgrnd(:,1,jv)) * zdz1(:,1,jv))
             z1(:) = lambda * (un - dgrnd(:,1,jv)) + un
             soilcap_pft(:,jv) = soilcap_pft(:,jv) / z1(:)
             soilflx_pft(:,jv) = soilflx_pft(:,jv) + &
                & soilcap_pft(:,jv) * (ptn(:,1,jv) * z1(:) - lambda * cgrnd(:,1,jv) - temp_sol_new_pft(:,jv)) / dt_sechiba 
           ENDWHERE
       ELSE   
           WHERE (veget_mask_2d(:,jv)) !soil
             soilflx_pft(:,jv) = zdz1(:,1,jv) * (cgrnd(:,1,jv) + (dgrnd(:,1,jv)-1.) * ptn(:,1,jv))
             soilcap_pft(:,jv) = (zdz2(:,1,jv) * dt_sechiba + dt_sechiba * (un - dgrnd(:,1,jv)) * zdz1(:,1,jv))
             z1(:) = lambda * (un - dgrnd(:,1,jv)) + un
             soilcap_pft(:,jv) = soilcap_pft(:,jv) / z1(:)
             soilflx_pft(:,jv) = soilflx_pft(:,jv) + &
                & soilcap_pft(:,jv) * (ptn(:,1,jv) * z1(:) - lambda * cgrnd(:,1,jv) - temp_sol_new(:)) / dt_sechiba 
           ENDWHERE
       ENDIF

       WHERE (veget_mask_2d(:,jv)) !soil
         soilflx_pft_nosnow(:,jv) = zdz1(:,1,jv) * (cgrnd(:,1,jv) + (dgrnd(:,1,jv)-1.) * ptn(:,1,jv))
         soilcap_pft_nosnow(:,jv) = (zdz2(:,1,jv) * dt_sechiba + dt_sechiba * (un - dgrnd(:,1,jv)) * zdz1(:,1,jv))
         z1(:) = lambda * (un - dgrnd(:,1,jv)) + un
         soilcap_pft_nosnow(:,jv) = soilcap_pft_nosnow(:,jv) / z1(:)
         soilflx_pft_nosnow(:,jv) = soilflx_pft_nosnow(:,jv) + &
            & soilcap_pft_nosnow(:,jv) * (ptn(:,1,jv) * z1(:) - lambda * cgrnd(:,1,jv) - temp_sol_new(:)) / dt_sechiba 
       ENDWHERE
    ENDDO ! jv=1,nvm

    ! 4 here is where I normalize to take the weighted means of each of the
    ! PFTs for surface energy fluxes

    IF (ok_explicitsnow) THEN
        DO ji = 1,kjpindex
              DO jv=1,nvm !pft
                 !IF ( veget_mask_2d(ji,jv) .AND. SUM(snowdz(ji,:)) .LE. 0.01) THEN
                 IF ( veget_mask_2d(ji,jv) ) THEN
                    soilflx_nosnow(ji) = soilflx_nosnow(ji) + (soilflx_pft_nosnow(ji,jv)*veget_max(ji,jv))
                    soilcap_nosnow(ji) = soilcap_nosnow(ji) + (soilcap_pft_nosnow(ji,jv)*veget_max(ji,jv))
                    cgrnd_soil(ji) = cgrnd_soil(ji) + (cgrnd(ji,1,jv)*veget_max(ji,jv))
                    dgrnd_soil(ji) = dgrnd_soil(ji) + (dgrnd(ji,1,jv)*veget_max(ji,jv))
                    zdz1_soil(ji)  = zdz1_soil(ji)  + (zdz1(ji,1,jv)*veget_max(ji,jv))
                    zdz2_soil(ji)  = zdz2_soil(ji)  + (zdz2(ji,1,jv)*veget_max(ji,jv))

                 END IF
              END DO
        END DO
    ELSE
        DO ji = 1,kjpindex
              DO jv=1,nvm !pft
                 IF ( veget_mask_2d(ji,jv) ) THEN
                    soilflx_nosnow(ji) = soilflx_nosnow(ji) + (soilflx_pft_nosnow(ji,jv)*veget_max(ji,jv))
                    soilcap_nosnow(ji) = soilcap_nosnow(ji) + (soilcap_pft_nosnow(ji,jv)*veget_max(ji,jv))

                    cgrnd_soil(ji) = cgrnd_soil(ji) + (cgrnd(ji,1,jv)*veget_max(ji,jv))
                    dgrnd_soil(ji) = dgrnd_soil(ji) + (dgrnd(ji,1,jv)*veget_max(ji,jv))
                    zdz1_soil(ji)  = zdz1_soil(ji)  + (zdz1(ji,1,jv)*veget_max(ji,jv))
                    zdz2_soil(ji)  = zdz2_soil(ji)  + (zdz2(ji,1,jv)*veget_max(ji,jv))

                 END IF
              END DO
        END DO
    ENDIF

    !! 3. Computation of the coefficients of the numerical integration scheme for the snow layers

    !! 3.1 Calculate numerical coefficients zdz1_snow, zdz2_snow and lambda_snow
    DO ji = 1, kjpindex

       IF ( ok_explicitsnow ) THEN

          ! Calculate internal values
          DO jg = 1, nsnow
             ZSNOWDZM(ji,jg) = MAX(snowdz(ji,jg),psnowdzmin)
          ENDDO
          dz2_snow(ji,:)=ZSNOWDZM(ji,:)

          DO jg = 1, nsnow-1
             dz1_snow(ji,jg)  = 2.0 / (dz2_snow(ji,jg+1)+dz2_snow(ji,jg))
          ENDDO

          lambda_snow(ji) = dz2_snow(ji,1)/2.0 * dz1_snow(ji,1)

          DO jg=1,nsnow
             zdz2_snow(ji,jg)=pcapa_snow(ji,jg) * dz2_snow(ji,jg)/dt_sechiba
          ENDDO

          DO jg=1,nsnow-1
             zdz1_snow(ji,jg) = dz1_snow(ji,jg) * pkappa_snow(ji,jg)
          ENDDO

          ! the bottom snow
          zdz1_snow(ji,nsnow) = pkappa_snow(ji,nsnow) / ( zlt(1) + dz2_snow(ji,nsnow)/2 )

       ELSE
          ! Without explict snow
          lambda_snow(ji) = lambda
       ENDIF

    ENDDO



    !! 3.2 Calculate coefficients cgrnd_snow and dgrnd_snow for snow
    DO jv = 1, nvm
      DO ji = 1,kjpindex
       IF ( ok_explicitsnow ) THEN
          ! bottom level
          z1_snow(ji) = zdz2(ji,1,jv)+(un-dgrnd_soil(ji))*zdz1_soil(ji)+zdz1_snow(ji,nsnow)
          cgrnd_snow(ji,nsnow) = (zdz2_soil(ji) * ptn_pftmean(ji,1) + zdz1_soil(ji) * cgrnd_soil(ji) ) / z1_snow(ji)
          dgrnd_snow(ji,nsnow) = zdz1_snow(ji,nsnow) / z1_snow(ji)

          ! next-to-bottom level
          z1_snow(ji) = zdz2_snow(ji,nsnow)+(un-dgrnd_snow(ji,nsnow))*zdz1_snow(ji,nsnow)+zdz1_snow(ji,nsnow-1)
          cgrnd_snow(ji,nsnow-1) = (zdz2_snow(ji,nsnow)*snowtemp(ji,nsnow)+&
               zdz1_snow(ji,nsnow)*cgrnd_snow(ji,nsnow))/z1_snow(ji)
          dgrnd_snow(ji,nsnow-1) = zdz1_snow(ji,nsnow-1) / z1_snow(ji)

          DO jg = nsnow-1,2,-1
             z1_snow(ji) = un / (zdz2_snow(ji,jg) + zdz1_snow(ji,jg-1) + zdz1_snow(ji,jg) * (un - dgrnd_snow(ji,jg)))
             cgrnd_snow(ji,jg-1) = (snowtemp(ji,jg) * zdz2_snow(ji,jg) + zdz1_snow(ji,jg) * cgrnd_snow(ji,jg)) * z1_snow(ji)
             dgrnd_snow(ji,jg-1) = zdz1_snow(ji,jg-1) * z1_snow(ji)
          ENDDO
       ELSE
          ! Without explict snow 
          cgrnd_snow(ji,:) = zero
          dgrnd_snow(ji,:) = zero
       ENDIF
      ENDDO
    ENDDO

  !! 4. Computation of the apparent ground heat flux 
    !! Computation of apparent snow-atmosphere flux  
    DO ji = 1,kjpindex
       IF ( ok_explicitsnow ) THEN
          snowflx(ji) = zdz1_snow(ji,1) * (cgrnd_snow(ji,1) + (dgrnd_snow(ji,1)-1.) * snowtemp(ji,1))
          snowcap(ji) = (zdz2_snow(ji,1) * dt_sechiba + dt_sechiba * (un - dgrnd_snow(ji,1)) * zdz1_snow(ji,1))
          z1_snow(ji) = lambda_snow(ji) * (un - dgrnd_snow(ji,1)) + un 
          snowcap(ji) = snowcap(ji) / z1_snow(ji)
          snowflx(ji) = snowflx(ji) + &
               & snowcap(ji) * (snowtemp(ji,1) * z1_snow(ji) - lambda_snow(ji) * cgrnd_snow(ji,1) - temp_sol_new(ji)) / dt_sechiba
       ELSE
          snowflx(ji) = zero
          snowcap(ji) = zero
       ENDIF
    ENDDO

    !! Add snow fraction
    IF ( ok_explicitsnow ) THEN
       ! Using an effective heat capacity and heat flux by a simple pondering of snow and soil fraction
       DO ji = 1, kjpindex
          soilcap(ji) = snowcap(ji)*frac_snow_veg(ji)*(1-totfrac_nobio(ji))+    & ! weights related to snow cover fraction on vegetation  
               soilcap_nosnow(ji)*SUM(frac_snow_nobio(ji,:))*totfrac_nobio(ji)+ & ! weights related to SCF on nobio
               soilcap_nosnow(ji)*(1-(frac_snow_veg(ji)*(1-totfrac_nobio(ji))-SUM(frac_snow_nobio(ji,:))*totfrac_nobio(ji))) ! weights related to non snow fraction
          soilflx(ji) = snowflx(ji)*frac_snow_veg(ji)*(1-totfrac_nobio(ji))+    & ! weights related to snow cover fraction on vegetation  
               soilflx_nosnow(ji)*SUM(frac_snow_nobio(ji,:))*totfrac_nobio(ji)+ & ! weights related to SCF on nobio
               soilflx_nosnow(ji)*(1-(frac_snow_veg(ji)*(1-totfrac_nobio(ji))-SUM(frac_snow_nobio(ji,:))*totfrac_nobio(ji))) ! weights related to non snow fraction
       ENDDO
    ELSE
       ! Do not consider snow fraction
       soilcap(:)=soilcap_nosnow(:)
       soilflx(:)=soilflx_nosnow(:)
    END IF

    IF (printlev>=3) WRITE (numout,*) ' thermosoil_coef done '

  END SUBROUTINE thermosoil_coef
 
 
!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_profile
!!
!>\BRIEF        In this routine solves the numerical soil thermal scheme, ie calculates the new soil temperature profile; 
!! 
!!
!! DESCRIPTION	: The calculation of the new soil temperature profile is based on
!! the cgrnd and dgrnd values from the previous timestep and the surface temperature Ts aka temp_sol_new. (see detailed
!! explanation in the header of the thermosoil module or in the reference).\n
!!                              T(k+1)=cgrnd(k)+dgrnd(k)*T(k)\n
!!                                      -- EQ1 --\n
!!                           Ts=(1+lambda)*T(1) -lambda*T(2)\n 
!!                                      -- EQ2--\n
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S): ptn (soil temperature profile on the thermal axis), 
!!                          stempdiag (soil temperature profile on the diagnostic axis)
!!
!! REFERENCE(S) :
!! - Hourdin, F. (1992). Study and numerical simulation of the general circulation of planetary atmospheres,
!! Ph.D. thesis, Paris VII University. Remark: the part of F. Hourdin's PhD thesis relative to the thermal
!! integration scheme has been scanned and is provided along with the documentation, with name : 
!! Hourdin_1992_PhD_thermal_scheme.pdf
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================

 SUBROUTINE thermosoil_profile (kjpindex, temp_sol_new, temp_sol_new_pft, ptn, stempdiag,       &
                                snowtemp, veget_max,                          &
                                frac_snow_veg, frac_snow_nobio,totfrac_nobio, &
                                cgrnd_snow,    dgrnd_snow)

  !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                               :: kjpindex       !! Domain size (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: temp_sol_new   !! Surface temperature at the present time-step 
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)        :: temp_sol_new_pft   !! Surface temperature at the present time-step 
                                                                               !! @tex ($K$) @endtex
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)       :: veget_max      !! Fraction of vegetation type 
    REAL(r_std),DIMENSION (kjpindex), INTENT(in)             :: frac_snow_veg  !! Snow cover fraction on vegeted area
    REAL(r_std),DIMENSION (kjpindex,nnobio), INTENT(in)      :: frac_snow_nobio!! Snow cover fraction on non-vegeted area
    REAL(r_std),DIMENSION (kjpindex),INTENT(in)              :: totfrac_nobio  !! Total fraction of continental ice+lakes+cities+...
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(in)       :: snowtemp       !! Snow temperature
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(in)       :: cgrnd_snow     !! Integration coefficient for snow numerical scheme
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(in)       :: dgrnd_snow     !! Integration coefficient for snow numerical scheme
 
    !! 0.3 Modified variables

    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (out)      :: stempdiag      !! diagnostic temperature profile 
                                                                               !! @tex ($K$) @endtex

    !! 0.3 Modified variables

    REAL(r_std),DIMENSION (kjpindex,ngrnd,nvm), INTENT (inout)   :: ptn        !! vertically discretized soil temperatures 
                                                                               !! @tex ($K$) @endtex

    !! 0.4 Local variables

    INTEGER(i_std)                                           :: ji, jg, jv
    REAL(r_std)                                              :: temp_sol_eff   !! effective surface temperature including snow and soil
     
!_ ================================================================================================================================

  !! 1. Computes the snow temperatures   
         
  !! 1.1. ptn(jg=1) using EQ1 and EQ2       
    DO jv = 1,nvm 
      DO ji = 1,kjpindex
         IF ( veget_mask_2d(ji,jv) ) THEN
            IF (ok_explicitsnow ) THEN !!! again, explicit snow not compatible with crop column temperature, xuhui
               ! Using an effective surface temperature by a simple pondering 
               temp_sol_eff=snowtemp(ji,nsnow)*frac_snow_veg(ji)*(1-totfrac_nobio(ji))+ &     ! weights related to snow cover fraction on vegetation  
                   temp_sol_new(ji)*SUM(frac_snow_nobio(ji,:))*totfrac_nobio(ji)+ &           ! weights related to SCF on nobio
                   temp_sol_new(ji)*(1-(frac_snow_veg(ji)*(1-totfrac_nobio(ji))-SUM(frac_snow_nobio(ji,:))*totfrac_nobio(ji))) ! weights related to non snow fraction
               ! Soil temperature calculation with explicit snow if there is snow on the ground
               ptn(ji,1,jv) = cgrnd_snow(ji,nsnow) + dgrnd_snow(ji,nsnow) * temp_sol_eff
            ELSE
               ! Standard soil temperature calculation
               IF (ok_LAIdev(jv) .AND. veget_max(ji,jv)>0) THEN
                   ptn(ji,1,jv) = (lambda * cgrnd(ji,1,jv) + temp_sol_new_pft(ji,jv)) / (lambda *(un - dgrnd(ji,1,jv)) + un)
               ELSE
                   ptn(ji,1,jv) = (lambda * cgrnd(ji,1,jv) + temp_sol_new(ji)) / (lambda *(un - dgrnd(ji,1,jv)) + un)
               ENDIF
            ENDIF
         ENDIF
      ENDDO

      !! 1.2. ptn(jg=2:ngrnd) using EQ1.
      DO jg = 1,ngrnd-1
        DO ji = 1,kjpindex
         IF ( veget_mask_2d(ji,jv) ) THEN 
          ptn(ji,jg+1,jv) = cgrnd(ji,jg,jv) + dgrnd(ji,jg,jv) * ptn(ji,jg,jv)
         ENDIF
        ENDDO
      ENDDO
    ENDDO

    !! 2. Assigne the soil temperature to the output variable. It is already on the right axis. 
    CALL thermosoil_diaglev(kjpindex, stempdiag, veget_max)

    IF (printlev>=3) WRITE (numout,*) ' thermosoil_profile done '

  END SUBROUTINE thermosoil_profile


!================================================================================================================================
!! SUBROUTINE   : thermosoil_cond
!!
!>\BRIEF          Calculate soil thermal conductivity.  
!!
!! DESCRIPTION  : This routine computes soil thermal conductivity
!!                Code introduced from NOAH LSM. 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): cnd
!!
!! REFERENCE(S) :
!!    Farouki, O.T.,1986: Thermal Properties of Soils. Series on Rock
!!            and Soil Mechanics, Vol. 11, Trans Tech, 136 PP.
!!    Johansen, O., 1975: Thermal Conductivity of Soils. Ph.D. Thesis,
!!            University of Trondheim,
!!    Peters-Lidard, C. D., Blackburn, E., Liang, X., & Wood, E. F.,
!!            1998: The effect of soil thermal conductivity 
!!            Parameterization on Surface Energy fluxes
!!            and Temperatures. J. of The Atmospheric Sciences,
!!            Vol. 55, pp. 1209-1224.
!! Modify histroy:
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================

  SUBROUTINE thermosoil_cond (kjpindex, njsc, smc, qz, smcmax, sh2o, cnd)

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                                 :: kjpindex      !! Domain size (unitless)
    INTEGER(i_std), DIMENSION (kjpindex), INTENT (in)          :: njsc          !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std), DIMENSION (kjpindex,ngrnd), INTENT(IN)        :: smc           !! Volumetric Soil Moisture Content (m3/m3)
    REAL(r_std), DIMENSION (nscm), INTENT(IN)                  :: qz            !! Quartz Content (Soil Type Dependent) (0-1)               
    REAL(r_std), DIMENSION (nscm), INTENT(IN)                  :: smcmax        !! Soil Porosity (0-1)
    REAL(r_std), DIMENSION (kjpindex,ngrnd), INTENT(IN)        :: sh2o          !! Unfrozen Soil Moisture Content; Frozen Soil Moisture = smc - sh2o
    
    !! 0.2 Output variables
    REAL(r_std), DIMENSION (kjpindex,ngrnd), INTENT(OUT)       :: cnd           !! Soil Thermal Conductivity (W/m/k)
    
    !! 0.3 Modified variables
    
    !! 0.4 Local variables
    REAL(r_std), DIMENSION (kjpindex,ngrnd)                    :: ake           !! Kerston Number (unitless)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)                    :: thksat        !! Saturated Thermal Conductivity (W/m/k)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)                    :: satratio      !! Degree of Saturation (0-1)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)                    :: xu            !! Unfrozen Volume For Saturation (0-1)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)                    :: xunfroz       !! Unfrozon Volume Fraction (0-1)
    REAL(r_std)                                                :: thko          !! Thermal Conductivity for Other Ssoil Components (W/m/k)
    REAL(r_std)                                                :: gammd         !! Dry Dendity (kg/m3)
    REAL(r_std)                                                :: thkdry        !! Dry Thermal Conductivity (W/m/k)
    REAL(r_std)                                                :: thks          !! Thermal Conductivity for the Solids Combined (Quartz + Other) (W/m/k)
    INTEGER(i_std)                                             :: ji, jg, jst
    
!_================================================================================================================================
    
    !! 1. Dry and Saturated Thermal Conductivity.
   
    DO ji = 1,kjpindex
      jst = njsc(ji)

      !! 1.1. Dry density (Kg/m3) and Dry thermal conductivity (W.M-1.K-1)
      gammd = (1. - smcmax(jst))*2700.
      thkdry = (0.135* gammd+ 64.7)/ (2700. - 0.947* gammd)

      !! 1.2. thermal conductivity of "other" soil components
      IF (qz(jst) > 0.2) THEN
         thko = 2.0
      ELSEIF (qz(jst) <= 0.2) THEN
         thko = 3.0
      ENDIF

      !! 1.3. Thermal conductivity of solids
      thks = (THKQTZ ** qz(jst))* (thko ** (1. - qz(jst)))

      DO jg = 1,ngrnd      
        !! 1.4. saturation ratio
        satratio(ji,jg) = smc(ji,jg) / smcmax(jst)
    
        !! 1.5. Saturated Thermal Conductivity (thksat)
        xunfroz(ji,jg) = sh2o(ji,jg) / smc(ji,jg)   ! Unfrozen Fraction (From i.e., 100%Liquid, to 0. (100% Frozen))
        xu(ji,jg) = xunfroz(ji,jg) * smcmax(jst)  ! Unfrozen volume for saturation (porosity*xunfroz)
        thksat(ji,jg) = thks ** (1. - smcmax(jst))* THKICE ** (smcmax(jst) - xu(ji,jg))* THKW ** (xu(ji,jg))
      END DO ! DO jg = 1,ngrnd

      !! 2. Kerston Number (ake)
      DO jg = 1,ngrnd
        IF ( (sh2o(ji,jg) + 0.0005) <  smc(ji,jg) ) THEN
          ! Frozen
          ake(ji,jg) = satratio(ji,jg)
        ELSE
          ! Unfrozen
          IF ( satratio(ji,jg) >  0.1 ) THEN
            ake(ji,jg) = LOG10 (satratio(ji,jg)) + 1.0
          ELSEIF ( satratio(ji,jg) >  0.05 .AND. satratio(ji,jg) <=  0.1 ) THEN
            ake(ji,jg) = 0.7 * LOG10 (satratio(ji,jg)) + 1.0
          ELSE
            ake(ji,jg) = 0.0  ! use k = kdry
          END IF
        END IF
      END DO ! DO jg = 1,ngrnd

      !! 3. Thermal conductivity (cnd)
      DO jg = 1,ngrnd
        cnd(ji,jg) = ake(ji,jg) * (thksat(ji,jg) - thkdry) + thkdry
      END DO ! DO jg = 1,ngrnd

    END DO !DO ji = 1,kjpindex
    
  END SUBROUTINE thermosoil_cond


!================================================================================================================================
!! SUBROUTINE   : thermosoil_cond_pft
!!
!>\BRIEF          Calculate soil thermal conductivity.  
!!
!! DESCRIPTION  : This routine computes soil thermal conductivity
!!                Code introduced from NOAH LSM. 
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): cnd
!!
!! REFERENCE(S) :
!!    Farouki, O.T.,1986: Thermal Properties of Soils. Series on Rock
!!            and Soil Mechanics, Vol. 11, Trans Tech, 136 PP.
!!    Johansen, O., 1975: Thermal Conductivity of Soils. Ph.D. Thesis,
!!            University of Trondheim,
!!    Peters-Lidard, C. D., Blackburn, E., Liang, X., & Wood, E. F.,
!!            1998: The effect of soil thermal conductivity 
!!            Parameterization on Surface Energy fluxes
!!            and Temperatures. J. of The Atmospheric Sciences,
!!            Vol. 55, pp. 1209-1224.
!! Modify histroy:
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================

  SUBROUTINE thermosoil_cond_pft (kjpindex, njsc, smc, qz, porosnet, sh2o, cnd)

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                                 :: kjpindex      !! Domain size (unitless)
    INTEGER(i_std), DIMENSION (kjpindex), INTENT (in)          :: njsc          !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
!    REAL(r_std), DIMENSION (kjpindex,ngrnd), INTENT(IN)        :: smc           !! Volumetric Soil Moisture Content (m3/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm), INTENT(IN)        :: smc           !! Volumetric Soil Moisture Content (m3/m3)
    REAL(r_std), DIMENSION (nscm), INTENT(IN)                  :: qz            !! Quartz Content (Soil Type Dependent) (0-1)               
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm), INTENT(IN)    :: porosnet      !! Soil Porosity (0-1)
!    REAL(r_std), DIMENSION (kjpindex,ngrnd), INTENT(IN)        :: sh2o          !! Unfrozen Soil Moisture Content; Frozen Soil Moisture = smc - sh2o
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm), INTENT(IN)        :: sh2o          !! Unfrozen Soil Moisture Content; Frozen Soil Moisture = smc - sh2o
    
    !! 0.2 Output variables
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm), INTENT(OUT)   :: cnd           !! Soil Thermal Conductivity (W/m/k)
    
    !! 0.3 Modified variables
    
    !! 0.4 Local variables
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)                :: ake           !! Kerston Number (unitless)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)                :: thksat        !! Saturated Thermal Conductivity (W/m/k)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)                :: satratio      !! Degree of Saturation (0-1)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)                :: xu            !! Unfrozen Volume For Saturation (0-1)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)                    :: xunfroz       !! Unfrozon Volume Fraction (0-1)
    REAL(r_std)                                                :: thko          !! Thermal Conductivity for Other Ssoil Components (W/m/k)
    REAL(r_std)                                                :: gammd         !! Dry Dendity (kg/m3)
    REAL(r_std)                                                :: thkdry        !! Dry Thermal Conductivity (W/m/k)
    REAL(r_std)                                                :: thks          !! Thermal Conductivity for the Solids Combined (Quartz + Other) (W/m/k)
    INTEGER(i_std)                                             :: ji, jg, jst, jv
    
!_================================================================================================================================
    
    !! 1. Dry and Saturated Thermal Conductivity.
   
    DO ji = 1,kjpindex
      jst = njsc(ji)

      !! 1.1. thermal conductivity of "other" soil components
      IF (qz(jst) > 0.2) THEN
         thko = 2.0
      ELSEIF (qz(jst) <= 0.2) THEN
         thko = 3.0
      ENDIF

      !! 1.2. Thermal conductivity of solids
      thks = (THKQTZ ** qz(jst))* (thko ** (1. - qz(jst)))

      DO jv = 1,nvm
         IF (veget_mask_2d(ji, jv)) THEN
            DO jg = 1,ngrnd     
 
               !! 1.3. Dry density (Kg/m3) and Dry thermal conductivity (W.M-1.K-1)
               gammd = (1. - porosnet(ji, jg, jv))*2700.
               thkdry = (0.135* gammd+ 64.7)/ (2700. - 0.947* gammd)
               !! 1.4. saturation ratio
               satratio(ji,jg,jv) = smc(ji,jg,jv) / porosnet(ji, jg, jv)
    
               !! 1.5. Saturated Thermal Conductivity (thksat)
               xunfroz(ji,jg) = sh2o(ji,jg,jv) / smc(ji,jg,jv)   ! Unfrozen Fraction (From i.e., 100%Liquid, to 0. (100% Frozen))
               xu(ji,jg,jv) = xunfroz(ji,jg) * porosnet(ji, jg, jv)  ! Unfrozen volume for saturation (porosity*xunfroz)
               thksat(ji,jg,jv) = thks ** (1. - porosnet(ji, jg, jv))* THKICE ** (porosnet(ji, jg, jv) - xu(ji,jg,jv))* THKW ** (xu(ji,jg,jv))
            END DO ! DO jg = 1,ngrnd

            !! 2. Kerston Number (ake)
            DO jg = 1,ngrnd
              IF ( (sh2o(ji,jg,jv) + 0.0005) <  smc(ji,jg,jv) ) THEN
                ! Frozen
                ake(ji,jg,jv) = satratio(ji,jg,jv)
              ELSE
                ! Unfrozen
                IF ( satratio(ji,jg,jv) >  0.1 ) THEN
                   ake(ji,jg,jv) = LOG10 (satratio(ji,jg,jv)) + 1.0
                ELSEIF ( satratio(ji,jg,jv) >  0.05 .AND. satratio(ji,jg,jv) <=  0.1 ) THEN
                   ake(ji,jg,jv) = 0.7 * LOG10 (satratio(ji,jg,jv)) + 1.0
                ELSE
                   ake(ji,jg,jv) = 0.0  ! use k = kdry
                END IF
              END IF
           END DO ! DO jg = 1,ngrnd

          !! 3. Thermal conductivity (cnd)
          DO jg = 1,ngrnd
            cnd(ji,jg,jv) = ake(ji,jg,jv) * (thksat(ji,jg,jv) - thkdry) + thkdry
          END DO ! DO jg = 1,ngrnd

        END IF !IF (veget_mask_2d(ji, jv))
      END DO !DO jv = 1,nvm

    END DO !DO ji = 1,kjpindex
    
  END SUBROUTINE thermosoil_cond_pft


!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_humlev
!!
!>\BRIEF        Interpolates the diagnostic soil humidity profile shumdiag_perma(nbdl, diagnostic axis) onto 
!!              the thermal axis, which gives shum_ngrnd_perma(ngrnd, thermal axis).
!!
!! DESCRIPTION  :  Interpolate the volumetric soil moisture content from the node to the interface of the layer. 
!!                 The values for the deep layers in thermosoil where hydrology is not existing are constant. 
!!                 No interpolation is needed for the total soil moisture content and for the soil saturation degree.
!! The depths of the diagnostic levels are diaglev(1:nbdl), computed in slowproc.f90.
!! Recall that when the 11-layer hydrology is used,
!! shum_ngrnd_perma and shumdiag_perma are with reference to the moisture content (mc)
!! at the wilting point mcw : shum_ngrnd_perma=(mc-mcw)/(mcs-mcw).
!! with mcs the saturated soil moisture content.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): mc_layt, mcl_layt, tmc_layt, shum_ngrnd_perma (soil humidity profile on the thermal axis)
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================
  SUBROUTINE thermosoil_humlev(kjpindex, shumdiag_perma, thawed_humidity, mc_layh, mcl_layh, tmc_layh, &
                                mc_layh_pft, mcl_layh_pft, tmc_layh_pft)
  
  !! 0. Variables and parameter declaration

    !! 0.1 Input variables
 
    INTEGER(i_std), INTENT(in)                            :: kjpindex    !! Domain size (unitless)
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: shumdiag_perma    !! Relative soil humidity on the diagnostic axis. 
                                                                         !! (0-1, unitless). Caveats : when "hydrol" (the 11-layers
                                                                         !! hydrology) is used, this humidity is calculated with 
                                                                         !! respect to the wilting point : 
                                                                         !! shumdiag_perma= (mc-mcw)/(mcs-mcw), with mc : moisture 
                                                                         !! content; mcs : saturated soil moisture content; mcw: 
                                                                         !! soil moisture content at the wilting point. when the 2-layers
                                                                         !! hydrology "hydrolc" is used, shumdiag_perma is just
                                                                         !! a diagnostic humidity index, with no real physical 
                                                                         !! meaning.
    REAL(r_std), DIMENSION(kjpindex), INTENT (in)  :: thawed_humidity    !! specified humidity of thawed soil
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: mc_layh     !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid+ice) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: mcl_layh    !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm), INTENT (in)    :: tmc_layh    !! Total soil moisture content for each layer in hydrol(liquid+ice) [mm]
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in) :: mc_layh_pft     !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid+ice) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in) :: mcl_layh_pft    !! Volumetric soil moisture content for each layer in hydrol at nodes(liquid) [m/s]
    REAL(r_std),DIMENSION (kjpindex,nslm,nvm), INTENT (in) :: tmc_layh_pft    !! Total soil moisture content for each layer in hydrol(liquid+ice) [mm]

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    INTEGER(i_std)                                        :: ji, jd, jv

!_ ================================================================================================================================

    shum_ngrnd_perma(:,:,:) = zero
    IF (printlev >= 4) WRITE(numout,*) 'Start thermosoil_humlev' 

    
    !!!! *layt should be PFT specified, xuhui
    ! The values for the deep layers in thermosoil where hydrology is not existing are constant. 
    ! For exemple if thermosoil uses 8m, and hydrol uses 2m vertical discretization,
    ! the values between 2m and 8m are constant.
    DO ji=1,kjpindex
       DO jd = 1, nslm
          IF(jd == 1) THEN
             mc_layt(ji,jd) = mc_layh(ji,jd)
             mcl_layt(ji,jd) = mcl_layh(ji,jd)
          ELSEIF(jd == 2) THEN
             mc_layt(ji, jd) = mc_layh(ji,jd-1)*(znt(jd)-zlt(jd-1))/(znt(jd)-0.0) + &
                  mc_layh(ji, jd)*(zlt(jd-1)-0.0)/(znt(jd)-0.0)
             mcl_layt(ji, jd) = mcl_layh(ji,jd-1)*(znt(jd)-zlt(jd-1))/(znt(jd)-0.0) + &
                  mcl_layh(ji, jd)*(zlt(jd-1)-0.0)/(znt(jd)-0.0)
          ELSEIF(jd == nslm) THEN
             mc_layt(ji, jd) = mc_layh(ji,jd-1)*(zlt(jd)-zlt(jd-1))/(zlt(jd)-znt(jd-1))  + &
                  mc_layh(ji,jd)*(zlt(jd-1)-znt(jd-1))/(zlt(jd)-znt(jd-1))
             mcl_layt(ji, jd) = mcl_layh(ji,jd-1)*(zlt(jd)-zlt(jd-1))/(zlt(jd)-znt(jd-1))  + &
                  mcl_layh(ji,jd)*(zlt(jd-1)-znt(jd-1))/(zlt(jd)-znt(jd-1))
          ELSE
             mc_layt(ji, jd) = mc_layh(ji, jd-1)*(1-dz5(jd-1)) + mc_layh(ji,jd)*dz5(jd-1)
             mcl_layt(ji, jd) = mcl_layh(ji, jd-1)*(1-dz5(jd-1)) + mcl_layh(ji,jd)*dz5(jd-1)
          ENDIF
          DO jv = 1,nvm
              IF(jd == 1) THEN
                 mc_layt_pft(ji,jd,jv) = MAX(mc_layh_pft(ji,jd,jv), min_sechiba)
                 mcl_layt_pft(ji,jd,jv) = MAX(mcl_layh_pft(ji,jd,jv), min_sechiba)
              ELSEIF(jd == 2) THEN
                 mc_layt_pft(ji,jd,jv) = MAX(mc_layh_pft(ji,jd-1,jv)*(znt(jd)-zlt(jd-1))/(znt(jd)-0.0) + &
                      mc_layh_pft(ji,jd,jv)*(zlt(jd-1)-0.0)/(znt(jd)-0.0), min_sechiba)
                 mcl_layt_pft(ji,jd,jv) = MAX(mcl_layh_pft(ji,jd-1,jv)*(znt(jd)-zlt(jd-1))/(znt(jd)-0.0) + &
                      mcl_layh_pft(ji,jd,jv)*(zlt(jd-1)-0.0)/(znt(jd)-0.0), min_sechiba)
              ELSEIF(jd == nslm) THEN
                 mc_layt_pft(ji,jd,jv) = MAX(mc_layh_pft(ji,jd-1,jv)*(zlt(jd)-zlt(jd-1))/(zlt(jd)-znt(jd-1))  + &
                      mc_layh_pft(ji,jd,jv)*(zlt(jd-1)-znt(jd-1))/(zlt(jd)-znt(jd-1)),min_sechiba)
                 mcl_layt_pft(ji,jd,jv) = MAX(mcl_layh_pft(ji,jd-1,jv)*(zlt(jd)-zlt(jd-1))/(zlt(jd)-znt(jd-1))  + &
                      mcl_layh_pft(ji,jd,jv)*(zlt(jd-1)-znt(jd-1))/(zlt(jd)-znt(jd-1)),min_sechiba)
              ELSE
                 mc_layt_pft(ji,jd,jv) = MAX(mc_layh_pft(ji,jd-1,jv)*(1-dz5(jd-1)) + mc_layh_pft(ji,jd,jv)*dz5(jd-1), min_sechiba)
                 mcl_layt_pft(ji,jd,jv) = MAX(mcl_layh_pft(ji,jd-1,jv)*(1-dz5(jd-1)) + mcl_layh_pft(ji,jd,jv)*dz5(jd-1), min_sechiba)
              ENDIF
          ENDDO ! jv
          tmc_layt(ji,jd) = tmc_layh(ji,jd)
          tmc_layt_pft(ji,jd,:) = tmc_layh_pft(ji,jd,:)
       ENDDO !jd
       
       ! The deep layers in thermosoil where hydro is not existing
       DO jd = nslm+1, ngrnd
          mc_layt(ji,jd) = mc_layh(ji,nslm)
          mcl_layt(ji,jd) = mcl_layh(ji,nslm)
          tmc_layt(ji,jd) = tmc_layh(ji,nslm)
          mc_layt_pft(ji,jd,:) = mc_layh_pft(ji,nslm,:)
          mcl_layt_pft(ji,jd,:) = mcl_layh_pft(ji,nslm,:)
          tmc_layt_pft(ji,jd,:) = tmc_layh_pft(ji,nslm,:)
       ENDDO
    ENDDO


    IF (.NOT. satsoil ) THEN

       DO jv = 1, nvm

          ! The values for the deep layers in thermosoil where hydro is not existing are constant.
          ! For exemple if thermosoil uses 8m, and hydrol uses 2m vertical discretization,
          ! the values between 2m and 8m are constant.

          DO jd = 1, nbdl
             WHERE ( veget_mask_2d(:,jv) )
                shum_ngrnd_perma(:,jd,jv) = shumdiag_perma(:,jd)
             ENDWHERE
          END DO
          DO jd = nbdl+1,ngrnd
             WHERE ( veget_mask_2d(:,jv) )
                shum_ngrnd_perma(:,jd,jv) = shumdiag_perma(:,nbdl)
!Former version of MICT before the new soil vertical discretization
!see update_deep_soil_moisture
!               IF ( (ptn(ji,jd,jv) .GT. (ZeroCelsius+fr_dT/2.)) THEN
!                   shum_ngrnd_perma(ji,jd,jv) = thawed_humidity(ji)
!               ENDIF
!No else defined ??
!-> Right now we stay with the TRUNK version. Possibility to add a flag later to reactivate this part as an option.
             ENDWHERE
          END DO
       END DO

!now update the deep permafrost soil moisture separately
!CALL update_deep_soil_moisture(kjpindex, shumdiag_perma,proglevel_bottomdiaglev, proglevel_zdeep, &
!	thawed_humidity)

    ELSE
!This is a weird option, what about the coherence with shumdiag_perma ans the hydrology in general?
       shum_ngrnd_perma(:,:,:) = 1.
    ENDIF
       
    IF (printlev >= 4) WRITE(numout,*) 'thermosoil_humlev done' 

  END SUBROUTINE thermosoil_humlev


!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_energy
!!
!>\BRIEF         Energy check-up.
!!
!! DESCRIPTION  : I didn\'t comment this routine since at do not understand its
!! ask initial designers (Jan ? Nathalie ?).
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : ??
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================

  SUBROUTINE thermosoil_energy(kjpindex, temp_sol_new, soilcap, veget_max)
  
   !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                           :: kjpindex    !! Domain size
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: temp_sol_new!! New soil temperature
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)        :: soilcap     !! Soil capacity
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)   :: veget_max   !! Fraction of vegetation type 

    !! 0.2 Local variables
    INTEGER(i_std)  :: ji, jg

    IF (printlev>=3) WRITE (numout,*) 'entering thermosoil_energy'
    !

     DO ji = 1, kjpindex
      surfheat_incr(ji) = zero
      coldcont_incr(ji) = zero
     ENDDO
    !
    !  Sum up the energy content of all layers in the soil.
    !
    DO ji = 1, kjpindex
    !
       IF (SUM(pcapa_en(ji,1,:)*veget_max(ji,:)) .LE. sn_capa) THEN
          !
          ! Verify the energy conservation in the surface layer
          !
          coldcont_incr(ji) = soilcap(ji) * (temp_sol_new(ji) - temp_sol_beg(ji))
          surfheat_incr(ji) = zero
       ELSE
          !
          ! Verify the energy conservation in the surface layer
          !
          surfheat_incr(ji) = soilcap(ji) * (temp_sol_new(ji) - temp_sol_beg(ji))
          coldcont_incr(ji) = zero
       ENDIF
    ENDDO
    
    ptn_beg(:,:,:)      = ptn(:,:,:)
    temp_sol_beg(:)   = temp_sol_new(:)

  END SUBROUTINE thermosoil_energy



!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_readjust
!!
!>\BRIEF        
!!
!! DESCRIPTION  : Energy conservation : Correction to make sure that the same latent heat is released and 
!!                consumed during freezing and thawing  
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S): ptn (soil temperature profile on the thermal axis), 
!!                          
!! REFERENCE(S) :
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================

  SUBROUTINE thermosoil_readjust(kjpindex, ptn)

   !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                                :: kjpindex
    
    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(inout)   :: ptn

    !! 0.3 Local variables
    INTEGER(i_std)  :: ji, jg, jv
    INTEGER(i_std)  :: lev3m  !! Closest interface level to 3m
    REAL(r_std) :: ptn_tmp

    ! The energy is spread over the layers down to approximatly 3m
    ! Find the closest level to 3m. It can be below or above 3m.
    lev3m=MINLOC(ABS(zlt(:)-3.0),dim=1)
    IF (printlev >= 3) WRITE(numout,*) 'In thermosoil_adjust: lev3m=',lev3m, ' zlt(lev3m)=', zlt(lev3m)
    
    DO jv = 1,nvm
      DO jg=1, ngrnd
          DO ji=1, kjpindex
            IF (veget_mask_2d(ji,jv)) THEN
               ! All soil latent energy is put into e_soil_lat(ji, 1)
               ! because the variable soil layers make it difficult to keep track of all
               ! layers in this version
               ! NOTE : pcapa has unit J/K/m3 and pcappa_supp has J/K
               e_soil_lat(ji, jv)=e_soil_lat(ji, jv)+pcappa_supp(ji,jg,jv)*(ptn(ji,jg,jv)-ptn_beg(ji,jg,jv))
            ENDIF
          ENDDO ! ji=1, kjpindex
      ENDDO ! jg=1, ngrnd
    ENDDO ! jv = 1,nvm

    DO jv = 1,nvm
      DO ji=1, kjpindex
        IF (veget_mask_2d(ji,jv)) THEN
          IF (e_soil_lat(ji,jv).GT.min_sechiba.AND.MINVAL(ptn(ji,:,jv)).GT.ZeroCelsius+fr_dT/2.) THEN
                ! The soil is thawed: we spread the excess of energy over the uppermost 6 levels e.g. 2.7m
                ! Here we increase the temperatures
                DO jg=1,lev3m
                  ptn_tmp=ptn(ji,jg,jv)

                  ptn(ji,jg,jv)=ptn(ji,jg,jv)+MIN(e_soil_lat(ji,jv)/pcapa(ji,jg,jv)/zlt(lev3m), 0.5)
                  e_soil_lat(ji,jv)=e_soil_lat(ji,jv)-(ptn(ji,jg,jv)-ptn_tmp)*pcapa(ji,jg,jv)*dlt(jg)
                ENDDO ! jg=1,lev3m
          ELSE IF (e_soil_lat(ji,jv).LT.-min_sechiba.AND.MINVAL(ptn(ji,:,jv)).GT.ZeroCelsius+fr_dT/2.) THEN
                ! The soil is thawed
                ! Here we decrease the temperatures
                DO jg=1,lev3m
                  ptn_tmp=ptn(ji,jg,jv)
                  ptn(ji,jg,jv)=MAX(ZeroCelsius+fr_dT/2., ptn_tmp+e_soil_lat(ji,jv)/pcapa(ji,jg,jv)/zlt(6))
                  e_soil_lat(ji,jv)=e_soil_lat(ji,jv)+(ptn_tmp-ptn(ji,jg,jv))*pcapa(ji,jg,jv)*dlt(jg)
                ENDDO ! jg=1,6
          ENDIF 
        ENDIF 
      ENDDO ! ji=1, kjpindex
    ENDDO ! jv = 1,nvm

  END SUBROUTINE thermosoil_readjust
   
!-------------------------------------------------------------------



!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_getdiff
!!
!>\BRIEF          Computes soil and snow heat capacity and conductivity    
!!
!! DESCRIPTION	: Computation of the soil thermal properties; snow properties are also accounted for
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S): 
!!                          
!! REFERENCE(S) :
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================

  SUBROUTINE thermosoil_getdiff( kjpindex, ptn, njsc, shum_ngrnd_permalong, &
    profil_froz, pcappa_supp, organic_layer_thick, soilc_total, snowrho,    &
    snowtemp, pb)

   !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                                :: kjpindex
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(in)     :: shum_ngrnd_permalong
    REAL(r_std), DIMENSION(kjpindex), INTENT (in)            :: organic_layer_thick    !! how deep is the organic soil?
    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT (in)  :: soilc_total            !! total soil carbon for use in thermal calcs
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)         :: njsc       !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in) :: snowrho        !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in) :: snowtemp       !! Snow temperature
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: pb         !! Surface presure (hPa)

    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(inout)  :: ptn                    !! Soil temperature profile 

    !! 0.3 Output variables
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(out)    :: pcappa_supp
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(out)    :: profil_froz

    !! 0.3 Local variables
    REAL(r_std)                                              :: xx                     !! Unfrozen fraction of the soil
    REAL(r_std)                                              :: p
    REAL(r_std), DIMENSION(kjpindex,ngrnd,nvm)               :: zx1, zx2    
    REAL(r_std)                                              :: cap_iw                 !! Heat capacity of ice/water mixture
    REAL(r_std)                                              :: so_capa_dry_net
    REAL(r_std)                                              :: so_capa_dry_net_old
    REAL(r_std), DIMENSION(kjpindex,ngrnd,nvm)               :: poros_net
    REAL(r_std), DIMENSION(kjpindex,ngrnd,nvm)               :: poros_net_old
    REAL(r_std)                                              :: cond_solid_net
    REAL(r_std)                                              :: so_cond_dry_net
    INTEGER(i_std)                                           :: ji,jg,jv
    INTEGER                                             :: jst
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pkappa_tmp !! soil thermal conductivity (W/m/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: pkappa_pft_tmp !! soil thermal conductivity (W/m/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pcapa_tmp  !! soil heat capacity (J/m3/K) 
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: pcapa_pft_tmp  !! soil heat capacity (J/m3/K) 
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: pkappa_old !! soil thermal conductivity (W/m/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: pcapa_old  !! soil heat capacity (J/m3/K) 
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)      :: mc_layt_pft_tmp  !! Volumetric soil moisture for each layer(liquid+ice) (m/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)      :: mcl_layt_pft_tmp !! Volumetric soil moisture for each layer(liquid) (m/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)      :: tmc_layt_pft_tmp !! Total soil moisture content for each layer(liquid+ice) (mm)

    pkappa_tmp(:,:) = 0.0
    pcapa_tmp(:,:) = 0.0
    pkappa_pft_tmp(:,:,:) = 0.0
    pcapa_pft_tmp(:,:,:) = 0.0 
    pkappa_old(:,:,:) = 0.0
    pcapa_old(:,:,:) = 0.0

     ! Organic and anorgaic layer fraction
     !
     ! Default: organic layer not taken into account
     zx1(:,:,:) = 0.
     !
     IF ( use_toporganiclayer_tempdiff ) THEN
       !
       ! level 1
       !
       DO jv = 1,nvm
         DO ji = 1,kjpindex
           IF ( organic_layer_thick(ji) .GT. zlt(1) ) THEN
             !! the 1st level is in the organic => the 1st layer is entirely organic
             zx1(ji,1,jv) = 1. !!zx1 being the fraction of each level that is organic, zx2 is the remainder
           ELSE IF ( organic_layer_thick(ji) .GT. zero ) THEN
             !! the 1st level is beyond the organic and the organic is present
             zx1(ji,1,jv) = organic_layer_thick(ji) / zlt(1)
           ELSE
             ! there is no organic at all
             zx1(ji,1,jv) = 0.
           ENDIF
         ENDDO
       ENDDO
       !
       ! other levels
       !
       DO jg = 2, ngrnd !- 2
         DO ji = 1,kjpindex
           IF ( organic_layer_thick(ji) .GT. zlt(jg) ) THEN
             ! the current level is in the organic => the current layer is
             ! entirely organic
             zx1(ji,jg,1) = 1.
           ELSE IF ( organic_layer_thick(ji) .GT. zlt(jg-1) ) THEN
             ! the current layer is partially organic
             zx1(ji,jg,1) = (organic_layer_thick(ji) - zlt(jg-1)) / (zlt(jg) - zlt(jg-1))
           ELSE
             ! both levels are out of organic => the current layer is entirely
             ! mineral soil       
             zx1(ji,jg,1) = 0.
           ENDIF
         ENDDO
       ENDDO
       DO jv = 2, nvm
         zx1(ji,jg,jv) = zx1(ji,jg,1)
       ENDDO
       ! IF ( use_toporganiclayer_tempdiff ) THE
     ELSEIF ( use_soilc_tempdiff ) THEN
       !
       DO jv = 1,nvm
         DO jg = 1, ngrnd
           DO ji = 1,kjpindex
             zx1(ji,jg,jv) = MIN((soilc_total(ji,jg,jv)/soilc_max),1.)   !after lawrence and slater
           ENDDO
         ENDDO
       ENDDO
       ! 
     ENDIF ! ( use_soilc_tempdiff ) THEN
     !
     zx2(:,:,:) = 1.-zx1(:,:,:)

     DO jv = 1,nvm
       DO jg = 1, ngrnd
         DO ji = 1,kjpindex
           jst = njsc(ji)
           mc_layt_pft_tmp(ji,jg,jv) = MAX(mc_layt_pft(ji,jg,jv), min_sechiba)
           mcl_layt_pft_tmp(ji,jg,jv) = MAX(mcl_layt_pft(ji,jg,jv), min_sechiba)
           tmc_layt_pft_tmp(ji,jg,jv) = MAX(tmc_layt_pft(ji,jg,jv), min_sechiba)
           IF (veget_mask_2d(ji,jv)) THEN
             !
             ! 1. Calculate dry heat capacity and conductivity, taking
             ! into account the organic and mineral fractions in the layer
             !
             ! Former MICT version
             so_capa_dry_net_old = zx1(ji,jg,jv) * so_capa_dry_org + zx2(ji,jg,jv) * so_capa_dry
             !Here we take into account the new dependance of the soil heat capacity from the soil type.
             so_capa_dry_net = zx1(ji,jg,jv) * so_capa_dry_org + zx2(ji,jg,jv) * so_capa_dry_ns(jst)

             cond_solid_net  = un / ( zx1(ji,jg,jv) / cond_solid_org  + zx2(ji,jg,jv) / cond_solid  )
             poros_net_old(ji,jg,jv) = zx1(ji,jg,jv) * poros_org + zx2(ji,jg,jv) * poros
             !Here we take into account the new dependance of the porosity from the soil type.
             poros_net(ji,jg,jv) = zx1(ji,jg,jv) * poros_org + zx2(ji,jg,jv) * SMCMAX(jst)
             !
             so_cond_dry_net = un / ( zx1(ji,jg,jv) / so_cond_dry_org + zx2(ji,jg,jv) / so_cond_dry )
             pcapa_tmp(ji, jg) = so_capa_dry_net + water_capa * tmc_layt(ji,jg)/mille/dlt(jg)
             pcapa_pft_tmp(ji,jg,jv) = so_capa_dry_net + water_capa * tmc_layt_pft_tmp(ji,jg,jv)/mille/dlt(jg)
             !
             ! 2. Calculate heat capacity with allowance for permafrost

             IF (ok_freeze_thermix) THEN
                ! 2.1. soil heat capacity depending on temperature and humidity
                IF (ptn(ji,jg,jv) .LT. ZeroCelsius-fr_dT/2.) THEN
                  ! frozen soil
                  profil_froz(ji,jg,jv) = 1.
                  pcappa_supp(ji,jg, jv)= 0.
                 !! this is from Koven's version: pcapa(ji,jg,jv) = so_capa_dry_net + shum_ngrnd_permalong(ji,jg,jv)*poros_net(ji,jg,jv)*capa_ice*rho_ice
                  pcapa_old(ji,jg,jv) = so_capa_dry_net_old + shum_ngrnd_permalong(ji,jg,jv)*(so_capa_ice - so_capa_dry_net_old)!Isa : old version, proved to be correct
                  IF (ok_LAIdev(jv)) THEN
                      pcapa(ji,jg,jv) = so_capa_dry_net + so_capa_ice * tmc_layt_pft_tmp(ji,jg,jv) / mille / dlt(jg)
                  ELSE
                      pcapa(ji,jg,jv) = so_capa_dry_net + so_capa_ice * tmc_layt(ji,jg) / mille / dlt(jg) 
                  ENDIF
                ELSEIF (ptn(ji,jg,jv) .GT. ZeroCelsius+fr_dT/2.) THEN
                  ! unfrozen soil         
                  profil_froz(ji,jg,jv) = 0.
                  pcappa_supp(ji,jg,jv)= 0.
                  !! this is from Koven's version: pcapa(ji,jg,jv) =  so_capa_dry_net + shum_ngrnd_permalong(ji,jg,jv)*poros_net(ji,jg,jv)*capa_water*rho_water
                  pcapa_old(ji,jg,jv) = so_capa_dry_net_old + shum_ngrnd_permalong(ji,jg,jv)*(so_capa_wet - so_capa_dry_net_old) 
                  IF (ok_LAIdev(jv)) THEN
                      pcapa(ji,jg,jv) = pcapa_pft_tmp(ji,jg,jv)
                  ELSE
                      pcapa(ji,jg,jv) = pcapa_tmp(ji,jg)
                  ENDIF
                ELSE
   
                ! x is the unfrozen fraction of soil water              
                xx = (ptn(ji,jg,jv)-(ZeroCelsius-fr_dT/2.)) / fr_dT
                profil_froz(ji,jg,jv) = (1. - xx)
                ! net heat capacity of the ice/water mixture
                cap_iw = xx * so_capa_wet + (1.-xx) * so_capa_ice
                ! cap_iw = x * 4.E6 + (1.-x) * 2.E6 !DKtest - compar. w/ theor. sol. 
                pcapa_old(ji,jg,jv) = so_capa_dry_net_old + shum_ngrnd_permalong(ji,jg,jv)*(cap_iw-so_capa_dry_net_old) + &
                                  shum_ngrnd_permalong(ji,jg,jv)*poros_net_old(ji,jg,jv)*lhf*rho_water/fr_dT

                IF (ok_LAIdev(jv)) THEN
                    pcapa(ji,jg,jv) = so_capa_dry_net + &
                         & water_capa * tmc_layt_pft_tmp(ji,jg,jv)/ mille / dlt(jg) * xx + so_capa_ice * tmc_layt_pft_tmp(ji,jg,jv) / mille/dlt(jg) * (1.-xx)
!                         & water_capa * tmc_layt(ji,jg)/ mille / dlt(jg) * xx + so_capa_ice * tmc_layt(ji,jg) / mille/dlt(jg) * (1.-xx)
                ELSE
                    pcapa(ji,jg,jv) = so_capa_dry_net + &
                         & water_capa * tmc_layt(ji,jg)/ mille / dlt(jg) * xx + so_capa_ice * tmc_layt(ji,jg) / mille/dlt(jg) * (1.-xx)
                ENDIF
                pcappa_supp(ji,jg,jv)= shum_ngrnd_permalong(ji,jg,jv)*poros_net(ji,jg,jv)*lhf*rho_water/fr_dT*dlt(jg)

                ENDIF
             ELSE !++cdk this is physically wrong and only to be used to test the influence of latent heat
                profil_froz(ji,jg,jv) = 0.
                pcapa_old(ji,jg,jv) = so_capa_dry_net_old + shum_ngrnd_perma(ji,jg,jv)*(so_capa_wet - so_capa_dry_net_old)

                IF (ok_LAIdev(jv)) THEN
                    !pcapa(ji,jg,jv) = pcapa_tmp(ji, jg)
                    pcapa(ji,jg,jv) = pcapa_pft_tmp(ji, jg, jv) !! added xuhui
                ELSE
                    pcapa(ji,jg,jv) = pcapa_tmp(ji, jg)
                ENDIF

                IF (brk_flag == 1) THEN
                   ! Bedrock flag is activated
                   pcapa(ji,ngrnd-1:ngrnd,jv) = brk_capa
                   !pcapa_en(ji,ngrnd-1:ngrnd,jv) = brk_capa 
                   pkappa(ji,ngrnd-1:ngrnd,jv) = brk_cond
                ENDIF
             ENDIF
           ENDIF ! IF (veget_mask_2d(ji,jv)) THEN
          ENDDO
         ENDDO
        ENDDO

        pcapa_en(:,:,:) = pcapa(:,:,:)


    !
    ! 3. Calculate the heat conductivity with allowance for permafrost
    !
    CALL thermosoil_cond_pft (kjpindex, njsc, mc_layt_pft_tmp, QZ, poros_net, mcl_layt_pft_tmp, pkappa)
!    DO jv = 1,nvm
!        CALL thermosoil_cond_pft(kjpindex, njst, mc_layt_pft_tmp, QZ, poros_net, mcl_layt_pft_tmp, pkappa_pft(:,:,jv))
!    ENDDO

    !! Computes snow heat capacity and conductivity    
    DO ji = 1,kjpindex
       pcapa_snow(ji,:) = snowrho(ji,:) * xci
       pkappa_snow(ji,:) = (ZSNOWTHRMCOND1 + ZSNOWTHRMCOND2*snowrho(ji,:)*snowrho(ji,:)) +      &
            MAX(0.0,(ZSNOWTHRMCOND_AVAP+(ZSNOWTHRMCOND_BVAP/(snowtemp(ji,:)+ &
            ZSNOWTHRMCOND_CVAP)))*(XP00/(pb(ji)*100.)))
    END DO
   
   END SUBROUTINE thermosoil_getdiff

!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_getdiff_old_thermix_with_snow
!!
!>\BRIEF          Computes soil heat capacity and conductivity    
!!
!! DESCRIPTION	: Computes soil heat capacity and conductivity
!!                Special case with old snow without soil freezing
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S):
!!                          
!! REFERENCE(S) :
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================


   SUBROUTINE thermosoil_getdiff_old_thermix_with_snow( kjpindex, snow, njsc )


   !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in) :: kjpindex
    REAL(r_std),DIMENSION(kjpindex),INTENT (in)      :: snow
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in) :: njsc !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)


    !! 0.2 Local variables
    INTEGER                                          :: ji,jg,jv
    REAL(r_std)                                      :: snow_h       !! snow_h is the snow height @tex ($m$) @endtex 
    REAL(r_std)                                      :: zx1, zx2     !! zx1 and zx2 are the layer fraction consisting in snow and soil respectively.
    INTEGER                                             :: jst
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pkappa_tmp   !! soil thermal conductivity (W/m/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pcapa_tmp    !! soil heat capacity (J/m3/K) 
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: pkappa_pft_tmp   !! soil thermal conductivity (W/m/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: pcapa_pft_tmp    !! soil heat capacity (J/m3/K) 
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pkappa_wet   !! wet soil thermal conductivity (W/m/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pcapa_wet    !! wet soil heat capacity (J/m3/K)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: mc_layt_tmp      !! volumetric soil moisture (liquid+ice) (m/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: mcl_layt_tmp     !! volumetric soil moisture (liquid) (m/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: tmc_layt_tmp     !! total soil moisture content for each layer, mm
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: mc_layt_pft_tmp      !! volumetric soil moisture (liquid+ice) (m/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: mcl_layt_pft_tmp     !! volumetric soil moisture (liquid) (m/m3)
    REAL(r_std), DIMENSION (kjpindex,ngrnd,nvm)         :: tmc_layt_pft_tmp     !! total soil moisture content for each layer, mm

    REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: mcs_tmp          !! Saturated soil moisture (liquid+ice) (m3/m3)
      
    ! Computation of the soil thermal properties; snow properties are also accounted for

   pkappa_tmp(:,:) = 0.0
   pcapa_tmp(:,:) = 0.0
   pkappa_pft_tmp(:,:,:) = 0.0
   pcapa_pft_tmp(:,:,:) = 0.0
   pkappa_wet(:,:) = 0.0
   pcapa_wet(:,:) = 0.0
   DO ji = 1, kjpindex
      jst = njsc(ji)
      mcs_tmp(ji,:) = mcs(jst)
      DO jg = 1, ngrnd
          pcapa_tmp(ji, jg) = so_capa_dry_ns(jst) + water_capa * tmc_layt(ji,jg)/mille/dlt(jg)
          pcapa_wet(ji, jg) = so_capa_dry_ns(jst) + water_capa * mcs(jst)
          DO jv = 1,nvm
              mc_layt_pft_tmp(ji, jg, jv) = MAX(mc_layt_pft(ji,jg,jv), min_sechiba)
              mcl_layt_pft_tmp(ji, jg, jv) = MAX(mcl_layt_pft(ji,jg,jv), min_sechiba)
!              CALL thermosoil_cond(kjpindex, njsc, mc_layt_pft_tmp(:,:, jv), QZ, &
!                   SMCMAX, mcl_layt_pft_tmp(:, :, jv), pkappa_pft_tmp(:, :, jv))
              tmc_layt_pft_tmp(ji, jg, jv) = MAX(tmc_layt_pft(ji,jg,jv), min_sechiba)
              pcapa_pft_tmp(ji, jg, jv) = so_capa_dry_ns(jst) + water_capa * tmc_layt_pft_tmp(ji,jg,jv)/mille/dlt(jg)
          ENDDO
      ENDDO
    ENDDO

    DO jv = 1,nvm
        CALL thermosoil_cond(kjpindex, njsc, mc_layt_pft_tmp(:,:, jv), QZ, &
                   SMCMAX, mcl_layt_pft_tmp(:, :, jv), pkappa_pft_tmp(:, :, jv))
    ENDDO
    CALL thermosoil_cond(kjpindex, njsc, mc_layt, QZ, SMCMAX, mcl_layt, pkappa_tmp)
    CALL thermosoil_cond(kjpindex, njsc, mcs_tmp, QZ, SMCMAX, mcs_tmp, pkappa_wet)

    DO ji = 1,kjpindex
      snow_h = snow(ji) / sn_dens

      ! First layer
      IF ( snow_h .GT. zlt(1) ) THEN
          pcapa(ji,1,:) = sn_capa
          pcapa_en(ji,1,:) = sn_capa
          pkappa(ji,1,:) = sn_cond
      ELSE IF ( snow_h .GT. zero ) THEN
          pcapa_en(ji,1,:) = sn_capa
          zx1 = snow_h / zlt(1)
          zx2 = ( zlt(1) - snow_h) / zlt(1)
          pcapa(ji,1,:) = zx1 * sn_capa + zx2 * pcapa_wet(ji,1)
          pkappa(ji,1,:) = un / ( zx1 / sn_cond + zx2 / (pkappa_wet(ji,1)) )
      ELSE
        DO jv = 1,nvm
            IF (ok_LAIdev(jv)) THEN
    !          pkappa(ji,1,:) = pkappa_tmp(ji,1)
    !          pcapa(ji,1,:) = pcapa_tmp(ji,1)
    !          pcapa_en(ji,1,:) = pcapa_tmp(ji,1)
              pkappa(ji,1,jv) = pkappa_pft_tmp(ji,1,jv)
              pcapa(ji,1,jv) = pcapa_pft_tmp(ji,1,jv)
              pcapa_en(ji,1,jv) = pcapa_pft_tmp(ji,1,jv)
            ELSE
              pkappa(ji,1,jv) = pkappa_tmp(ji,1)
              pcapa(ji,1,jv) = pcapa_tmp(ji,1)
              pcapa_en(ji,1,jv) = pcapa_tmp(ji,1)
            ENDIF
        ENDDO
      ENDIF

      ! Mid layers
      DO jg = 2, ngrnd - 2
        IF ( snow_h .GT. zlt(jg) ) THEN
            pcapa(ji,jg,:) = sn_capa
            pkappa(ji,jg,:) = sn_cond
            pcapa_en(ji,jg,:) = sn_capa
        ELSE IF ( snow_h .GT. zlt(jg-1) ) THEN
            zx1 = (snow_h - zlt(jg-1)) / (zlt(jg) - zlt(jg-1))
            zx2 = ( zlt(jg) - snow_h) / (zlt(jg) - zlt(jg-1))
            pcapa_en(ji,jg,:) = sn_capa
            pcapa(ji, jg,:) = zx1 * sn_capa + zx2 * pcapa_wet(ji,jg)
            pkappa(ji,jg,:) = un / ( zx1 / sn_cond + zx2 / (pkappa_wet(ji,jg)))
        ELSE
          DO jv = 1,nvm
            IF (ok_LAIdev(jv)) THEN
    !            pcapa(ji,jg,:) = pcapa_tmp(ji, jg)
    !            pkappa(ji,jg,:) = pkappa_tmp(ji,jg)
    !            pcapa_en(ji,jg,:) = pcapa_tmp(ji, jg)
                pcapa(ji,jg,jv) = pcapa_pft_tmp(ji, jg, jv)
                pkappa(ji,jg,jv) = pkappa_pft_tmp(ji,jg, jv)
                pcapa_en(ji,jg,jv) = pcapa_pft_tmp(ji, jg, jv)
            ELSE
                pcapa(ji,jg,jv) = pcapa_tmp(ji, jg)
                pkappa(ji,jg,jv) = pkappa_tmp(ji,jg)
                pcapa_en(ji,jg,jv) = pcapa_tmp(ji, jg)
            ENDIF
          ENDDO
        ENDIF
      ENDDO

      ! Last two layers: These layers can not be filled with snow
      DO jg = ngrnd - 1, ngrnd
         pcapa(ji,jg,:) = so_capa_dry
         pkappa(ji,jg,:) = so_cond_dry
         pcapa_en(ji,jg,:) = so_capa_dry
      END DO
      
      IF (brk_flag == 1) THEN
        ! Bedrock flag is activated
        DO jg = ngrnd-1,ngrnd
           pcapa(ji,jg,:) = brk_capa
           pcapa_en(ji,jg,:) = brk_capa
           pkappa(ji,jg,:) = brk_cond
        ENDDO
      ENDIF

    ENDDO ! DO ji = 1,kjpindex


    END SUBROUTINE thermosoil_getdiff_old_thermix_with_snow


!! ================================================================================================================================ 
!! SUBROUTINE   : thermosoil_getdiff_old_thermix_without_snow
!!
!>\BRIEF          Computes soil and snow heat capacity and conductivity    
!!
!! DESCRIPTION	: Calculations of soil and snow thermal properties without effect of freezing. This subroutine is only
!!                call for the case with explictsnow activated.
!!                
!!
!! RECENT CHANGE(S) : None
!! \n 
!_ ================================================================================================================================

    SUBROUTINE thermosoil_getdiff_old_thermix_without_snow( kjpindex, njsc, snowrho, snowtemp, pb )

   !! 0. Variables and parameter declaration

    !! 0.1 Input variables
      INTEGER(i_std), INTENT(in) :: kjpindex
      INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)    :: njsc     !! Index of the dominant soil textural class in the grid cell (1-nscm, unitless)
      REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in) :: snowrho  !! Snow density
      REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in) :: snowtemp !! Snow temperature
      REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: pb       !! Surface presure (hPa)


    !! 0.1 Local variables
      INTEGER(i_std)    				  :: ji,jg, jst     !! Index
      REAL(r_std), DIMENSION (kjpindex,ngrnd)             :: pcapa_tmp      !! Soil heat capacity (J/m3/K) 

      !! Computes soil heat capacity and conductivity
      DO jg = 1,ngrnd
         DO ji = 1,kjpindex
            jst = njsc(ji)
            pcapa_tmp(ji, jg) = so_capa_dry_ns(jst) + water_capa * tmc_layt(ji,jg)/mille/dlt(jg)
            pcapa(ji,jg,:) = pcapa_tmp(ji, jg)
            pcapa_en(ji,jg,:) = pcapa_tmp(ji, jg)
         ENDDO
      ENDDO

      CALL thermosoil_cond (kjpindex, njsc, mc_layt, QZ, SMCMAX, mcl_layt, pkappa)

      IF (brk_flag == 1) THEN
        ! Bedrock flag is activated
        DO jg = ngrnd-1,ngrnd
          DO ji = 1,kjpindex
             pcapa(ji,jg,:) = brk_capa
             pcapa_en(ji,jg,:) = brk_capa 
             pkappa(ji,jg,:) = brk_cond
          ENDDO
        ENDDO
      ENDIF

    !! Computes snow heat capacity and conductivity
    DO ji = 1,kjpindex
        pcapa_snow(ji,:) = snowrho(ji,:) * xci
        pkappa_snow(ji,:) = (ZSNOWTHRMCOND1 + ZSNOWTHRMCOND2*snowrho(ji,:)*snowrho(ji,:)) +      &
              MAX(0.0,(ZSNOWTHRMCOND_AVAP+(ZSNOWTHRMCOND_BVAP/(snowtemp(ji,:)+ &
              ZSNOWTHRMCOND_CVAP)))*(XP00/(pb(ji)*100.)))
    END DO


    END SUBROUTINE thermosoil_getdiff_old_thermix_without_snow


!! ================================================================================================================================ 
!! SUBROUTINE   : read_permafrostmap 
!! 
!>\BRIEF           
!! 
!! DESCRIPTION  : Read permafrost map 
!!                 
!! 
!! RECENT CHANGE(S) : None 
!!  
!! MAIN OUTPUT VARIABLE(S): 
!!                           
!! REFERENCE(S) : 
!! 
!! FLOWCHART    : None  
!! \n  
!_ ================================================================================================================================ 

  SUBROUTINE read_permafrostmap(kjpindex,lalo,overburden,excess_ice,permafrost)
    
    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in) :: kjpindex
    REAL(r_std), DIMENSION(kjpindex,2), INTENT(in) :: lalo

    !! 0.2 Modified variables
    REAL(r_std), DIMENSION(kjpindex), INTENT(inout) :: overburden
    REAL(r_std), DIMENSION(kjpindex), INTENT(inout) :: excess_ice
    REAL(r_std), DIMENSION(kjpindex), INTENT(inout) :: permafrost
    
    !! 0.3 Local variables
    INTEGER(i_std) :: ip, ix, iy, imin, jmin, ier
    REAL(r_std) :: dlon, dlonmin, dlat, dlatmin
    CHARACTER(LEN=80) :: filename
    INTEGER(i_std) :: iml, jml, lml, tml, fid
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:) :: xx,yy, permafrost_file, continuous_file, discontinuous_file
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:) :: sporadic_file, isolated_file, overburden_file, excess_ice_file
    REAL(r_std),ALLOCATABLE,DIMENSION(:) :: x,y
    REAL(r_std) :: lev(1), date, dt
    INTEGER(i_std) :: itau(1)
    
    !
    !Config Key   = PERMAFROST_MAP_FILE
    !Config Desc  = Permafrost input data file path
    !Config If    = READ_PERMAFROST_MAP
    !Config Def   = permafrost_map.nc
    !Config Help  = Path to input data file for permafrost soil freezing
    !Config Units = [FILE]
    !
    filename = 'permafrost_map.nc'
    CALL getin_p('PERMAFROST_MAP_FILE',filename)
    IF ( filename .NE. "NONE" ) THEN
       CALL flininfo(filename,iml, jml, lml, tml, fid)
       ALLOCATE (yy(iml,jml), stat=ier)
       ALLOCATE (xx(iml,jml), stat=ier)
       ALLOCATE (x(iml),y(jml), stat=ier)
       ALLOCATE (continuous_file(iml,jml), stat=ier)
       ALLOCATE (discontinuous_file(iml,jml), stat=ier)
       ALLOCATE (sporadic_file(iml,jml), stat=ier)
       ALLOCATE (isolated_file(iml,jml), stat=ier)
       ALLOCATE (overburden_file(iml,jml), stat=ier)
       ALLOCATE (excess_ice_file(iml,jml), stat=ier)
       ALLOCATE (permafrost_file(iml,jml), stat=ier)
       CALL flinopen (filename, .FALSE., iml, jml, lml, &
            xx, yy, lev, tml, itau, date, dt, fid)
       CALL flinget (fid, 'continuous_permafrost', iml, jml, lml, tml, &
            1, 1, continuous_file)
       CALL flinget (fid, 'discontinuous_permafrost', iml, jml, lml, tml, &
            1, 1, discontinuous_file)
       CALL flinget (fid, 'sporadic_permafrost', iml, jml, lml, tml, &
            1, 1, sporadic_file)
       CALL flinget (fid, 'isolated_permafrost', iml, jml, lml, tml, &
            1, 1, isolated_file)
       CALL flinget (fid, 'thick_overburden', iml, jml, lml, tml, &
            1, 1, overburden_file)
       CALL flinget (fid, 'high_ground_ice_content', iml, jml, lml, tml, &
            1, 1, excess_ice_file)
       CALL flinclo (fid)
       ! On suppose que le fichier est regulier.
       ! Si ce n'est pas le cas, tant pis. Les temperatures seront mal
       ! initialisees et puis voila. De toute maniere, il faut avoir
       ! l'esprit mal tourne pour avoir l'idee de faire un fichier de
       ! climatologie avec une grille non reguliere.
       permafrost_file(:,:) = continuous_file + discontinuous_file + sporadic_file + isolated_file
       x(:) = xx(:,1)
       y(:) = yy(1,:)
       ! prendre la valeur la plus proche
       DO ip = 1, kjpindex
          dlonmin = HUGE(1.)
          DO ix = 1,iml
             dlon = MIN( ABS(lalo(ip,2)-x(ix)), ABS(lalo(ip,2)+360.-x(ix)), ABS(lalo(ip,2)-360.-x(ix)) )
             IF ( dlon .LT. dlonmin ) THEN
                imin = ix
                dlonmin = dlon
             ENDIF
          ENDDO
          dlatmin = HUGE(1.)
          DO iy = 1,jml
             dlat = ABS(lalo(ip,1)-y(iy))
             IF ( dlat .LT. dlatmin ) THEN
                jmin = iy
                dlatmin = dlat
             ENDIF
          ENDDO
          permafrost(ip) = permafrost_file(imin,jmin)
          overburden(ip) = overburden_file(imin,jmin)
          excess_ice(ip) = excess_ice_file(imin,jmin)
       ENDDO
       DEALLOCATE (yy)
       DEALLOCATE (xx)
       DEALLOCATE (x)
       DEALLOCATE (continuous_file)
       DEALLOCATE (discontinuous_file)
       DEALLOCATE (sporadic_file)
       DEALLOCATE (isolated_file)
       DEALLOCATE (overburden_file)
       DEALLOCATE (excess_ice_file)
       DEALLOCATE (permafrost_file)
    ENDIF
    WRITE(*,*) 'cdk: #points permafrost', SUM(permafrost)
    WRITE(*,*) 'cdk: #points overburden', SUM(overburden)
    WRITE(*,*) 'cdk: #points excess_ice', SUM(excess_ice)
    
  END SUBROUTINE read_permafrostmap


!! ================================================================================================================================
!! SUBROUTINE   : read_reftempfile
!!
!>\BRIEF          
!!
!! DESCRIPTION	: Read file with longterm temperature
!!                
!!
!! RECENT CHANGE(S) : None
!! 
!! MAIN OUTPUT VARIABLE(S): reftemp : Reference temerature
!!                          
!! REFERENCE(S) :
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================

 SUBROUTINE read_reftempfile(kjpindex,lalo,reftemp)
    

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                        :: kjpindex
    REAL(r_std), DIMENSION(kjpindex,2), INTENT(in)    :: lalo

    !! 0.2 Modified variables
    REAL(r_std), DIMENSION(kjpindex, ngrnd), INTENT(inout) :: reftemp

    !! 0.3 Local variables    
    INTEGER(i_std)                                    :: ip, ix, iy, imin, jmin, ier
    REAL(r_std)                                       :: dlon, dlonmin, dlat, dlatmin
    CHARACTER(LEN=80)                                 :: filename
    INTEGER(i_std)                                    :: iml, jml, lml, tml, fid
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:)            :: xx,yy
    REAL(r_std),ALLOCATABLE,DIMENSION(:,:)            :: reftemp_file
    REAL(r_std),ALLOCATABLE,DIMENSION(:)              :: x,y
    REAL(r_std)                                       :: lev(1), date, dt
    INTEGER(i_std)                                    :: itau(1)
    

    filename = 'reftemp.nc'
    CALL getin_p('SOIL_REFTEMP_FILE',filename)

       CALL flininfo(filename,iml, jml, lml, tml, fid)
       ALLOCATE (yy(iml,jml), stat=ier)
       ALLOCATE (xx(iml,jml), stat=ier)
       ALLOCATE (x(iml),y(jml), stat=ier)
       ALLOCATE (reftemp_file(iml,jml), stat=ier)

       CALL flinopen (filename, .FALSE., iml, jml, lml, &
            xx, yy, lev, tml, itau, date, dt, fid)
       CALL flinget (fid, 'temperature', iml, jml, lml, tml, &
            1, 1, reftemp_file)
       CALL flinclo (fid)
       ! On suppose que le fichier est regulier.
       ! Si ce n'est pas le cas, tant pis. Les temperatures seront mal
       ! initialisees et puis voila. De toute maniere, il faut avoir
       ! l'esprit mal tourne pour avoir l'idee de faire un fichier de
       ! climatologie avec une grille non reguliere.
       x(:) = xx(:,1)
       y(:) = yy(1,:)
       ! prendre la valeur la plus proche
       DO ip = 1, kjpindex
          dlonmin = HUGE(1.)
          DO ix = 1,iml
             dlon = MIN( ABS(lalo(ip,2)-x(ix)), ABS(lalo(ip,2)+360.-x(ix)), ABS(lalo(ip,2)-360.-x(ix)) )
             IF ( dlon .LT. dlonmin ) THEN
                imin = ix
                dlonmin = dlon
             ENDIF
          ENDDO
          dlatmin = HUGE(1.)
          DO iy = 1,jml
             dlat = ABS(lalo(ip,1)-y(iy))
             IF ( dlat .LT. dlatmin ) THEN
                jmin = iy
                dlatmin = dlat
             ENDIF
          ENDDO
          reftemp(ip, :) = reftemp_file(imin,jmin)+273.15
       ENDDO
       DEALLOCATE (yy)
       DEALLOCATE (xx)
       DEALLOCATE (x)
       DEALLOCATE (reftemp_file)


    
  END SUBROUTINE read_reftempfile

!! 
!================================================================================================================================ 
!! SUBROUTINE   : add_heat_Zimov 
!! 
!>\BRIEF          heat 
!! 
!! DESCRIPTION  :  
!! 
!! RECENT CHANGE(S) : None 
!! 
!! MAIN OUTPUT VARIABLE(S): 
!! 
!! REFERENCE(S) : 
!! 
!! FLOWCHART    : None 
!! \n 
!_ 
!================================================================================================================================ 
   SUBROUTINE add_heat_Zimov(kjpindex, veget_max_bg, ptn, heat_Zimov)
    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                                 :: kjpindex
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT(in)         :: veget_max_bg !! Fraction of vegetation type 

    REAL(r_std), DIMENSION(kjpindex,ndeep,nvm), INTENT (in)   :: heat_Zimov   !! heating associated with decomposition

    !! 0.2 Modified variables
     REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(inout)  :: ptn

    !! 0.3 Local variables
    INTEGER(r_std) :: ji, jg, jv 

    IF (printlev>=3) WRITE (numout,*) 'entering add_heat_Zimov'

    DO ji = 1, kjpindex
       DO jv = 1,nvm
          IF ( veget_mask_2d(ji,jv) ) THEN
             DO jg = 1, ngrnd
                ptn(ji,jg,jv) = ptn(ji,jg,jv) + heat_zimov(ji,jg,jv) * dt_sechiba / ( pcapa(ji,jg,jv) * dlt(jg) )
             END DO
          END IF
       END DO
    END DO

    ! ptn_pftmean needs to be updated to ensure consistency
    ptn_pftmean(:,:) = zero
    DO jv=1,nvm
       DO jg = 1, ngrnd
          ptn_pftmean(:,jg) = ptn_pftmean(:,jg) + ptn(:,jg,jv) * veget_max_bg(:,jv)
       ENDDO ! jg = 1, ngrnd
    ENDDO ! m=1,nvm

    IF (printlev>=3) WRITE (numout,*) ' add_heat_Zimov done'

  END SUBROUTINE add_heat_Zimov

!!
!! ================================================================================================================================
!! SUBROUTINE   : thermosoil_diaglev
!!
!>\BRIEF        Interpolation of the soil in-depth temperatures onto the diagnostic profile.
!!
!! DESCRIPTION  : This is a very easy linear interpolation, with intfact(jd, jg) the fraction
!! the thermal layer jg comprised within the diagnostic layer jd. The depths of
!! the diagnostic levels are diaglev(1:nbdl), computed in slowproc.f90.
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S): stempdiag (soil temperature profile on the diagnostic axis)
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None 
!! \n 
!_ ================================================================================================================================
  SUBROUTINE thermosoil_diaglev(kjpindex, stempdiag, veget_max)

  !! 0. Variables and parameter declaration

    !! 0.1 Input variables
 
    INTEGER(i_std), INTENT(in)                          :: kjpindex       !! Domain size (unitless)
    REAL(r_std), DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max      !! Fraction of vegetation type 
    !! 0.2 Output variables

    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (out) :: stempdiag      !! Diagnostoc soil temperature profile @tex ($K$) @endtex
    
    !! 0.3 Modified variables

    !! 0.4 Local variables
    INTEGER(i_std)                                      :: jg,jv
    REAL(r_std), DIMENSION (kjpindex,nvm)               :: veget_max_bg     !! Fraction of vegetation type 

!_ ================================================================================================================================
    veget_max_bg(:,2:nvm) = veget_max(:,2:nvm)
    veget_max_bg(:,1) = MAX((un - SUM(veget_max(:,2:nvm), 2)), zero)
    
    stempdiag(:,:) = 0.
    DO jg = 1, nbdl
      DO jv = 1, nvm
        stempdiag(:,jg) = stempdiag(:,jg) + ptn(:,jg,jv)*veget_max_bg(:,jv)
      ENDDO
    ENDDO

  END SUBROUTINE thermosoil_diaglev
 
!================================================================================================================================ 
!! SUBROUTINE   : update_deep_soil_moisture 
!! 
!>\BRIEF        updating deep soil moisture 
!!   
!! DESCRIPTION  :   
!! 
!! RECENT CHANGE(S) : None 
!! 
!! MAIN OUTPUT VARIABLE(S):  
!! 
!! REFERENCE(S) : None 
!! 
!! FLOWCHART    : None  
!! \n  
!_ 
!================================================================================================================================ 
    SUBROUTINE update_deep_soil_moisture (kjpindex, shumdiag_perma, proglevel_bottomdiaglev, &
         proglevel_zdeep, thawed_humidity)

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                            :: kjpindex            !! Domain size
    REAL(r_std),DIMENSION (kjpindex,nbdl), INTENT (in)    :: shumdiag_perma      !! Diagnostoc profile
    INTEGER(i_std), INTENT (in)                           :: proglevel_bottomdiaglev !! for keeping track of where the base of the diagnostic level meets the prognostic level
    INTEGER(i_std), INTENT (in)                           :: proglevel_zdeep     !! for keeping track of where the prognostic levels meet zdeep
    REAL(r_std), DIMENSION(kjpindex),   INTENT (in)       :: thawed_humidity     !! specified humidity of thawed soil

    !! 0.2 Modified variables

    !! 0.3 Output variables

    !! 0.4 Local variables
    INTEGER(i_std) :: ji, jd, jv

    IF (printlev>=3) WRITE (numout,*) 'entering update_deep_soil_misture'


    DO ji = 1, kjpindex
       DO jv = 1,nvm
          IF ( veget_mask_2d(ji,jv) ) THEN
             DO jd = proglevel_zdeep, ngrnd
                IF ( (ptn(ji,jd,jv) .GT. (ZeroCelsius+fr_dT/2.)) ) THEN
                   shum_ngrnd_perma(ji,jd,jv) = thawed_humidity(ji)
                END IF
             END DO
          END IF
       END DO
    END DO

    DO jd =  proglevel_bottomdiaglev, proglevel_zdeep-1
       DO ji = 1, kjpindex
          DO jv = 1,nvm
             IF (veget_mask_2d(ji,jv)) THEN
                CALL lint (diaglev(nslm), shumdiag_perma(ji,nslm), z_deepsoil,shum_ngrnd_perma(ji,proglevel_zdeep,jv), &
                     znt(jd), shum_ngrnd_perma(ji,jd,jv), 1)
             END IF
          END DO
       END DO
    END DO

    IF (printlev>=3) WRITE (numout,*) ' update_deep_soil_misture done'
    
    END SUBROUTINE update_deep_soil_moisture

!! 
!================================================================================================================================ 
!! SUBROUTINE   : lint 
!! 
!>\BRIEF        Simple interpolation 
!! 
!! DESCRIPTION  :     ! Interpolation linÃ©aire entre des points (x1,y1) et(x2,y2)) 
!! Ces commentaires en mauvais franÃ§ais permettent savoir qui a 
!! ecrit la subroutine :-) - DK           
!! 
!! RECENT CHANGE(S) : None 
!! 
!! MAIN OUTPUT VARIABLE(S):  
!! 
!! REFERENCE(S) : None 
!! 
!! FLOWCHART    : None  
!! \n  
!_ 
!================================================================================================================================
  SUBROUTINE lint(x1,y1,x2,y2,x,y,NY)
    !! 0. Variables and parameter declaration

    !! 0.1 Input variables    

    REAL, INTENT(in)                   ::  x1,x2,y1,y2,x
    INTEGER, INTENT(in)                ::  NY

    !! 0.2 Modified variables
    REAL, DIMENSION(NY), INTENT(inout) :: y

    !! 0.3 Local variables
    REAL, PARAMETER                    :: EPSILON = 1.E-10
    
    IF (ABS(x1 - x2) .LT. EPSILON) THEN
       PRINT *, 'ERROR IN lint(x1,y1,x2,y2,y,NY) : x1==x2!'
       PRINT *, 'x1=',x1,'  x2=',x2
       PRINT *, 'y1=',y1,'  y2=',y2
       STOP
    END IF
    
    IF (x1 .LE. x .AND. x .LE. x2) THEN
       y = x*(y2-y1)/(x2-x1) + (y1*x2 - y2*x1)/(x2-x1)
       !      ELSE
       !        y = UNDEF
    END IF
    
  END SUBROUTINE lint

!!
!================================================================================================================================ 
!! SUBROUTINE   : thermosoil_wlupdate 
!! 
!>\BRIEF          Updates the long-term soil humidity     
!! 
!! DESCRIPTION  :  
!! 
!! RECENT CHANGE(S) : None 
!!  
!! MAIN OUTPUT VARIABLE(S):  
!!                           
!! REFERENCE(S) : 
!! 
!! FLOWCHART    : None  
!! \n  
!_
!================================================================================================================================
  SUBROUTINE thermosoil_wlupdate( kjpindex, ptn, hsd, hsdlong )
    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                           :: kjpindex
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(in)        :: ptn
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(in)        :: hsd

    !! 0.2 Modified variables
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(inout)     :: hsdlong

    !! 0.3 Local variables
    INTEGER(i_std) ::  il
    REAL(r_std), PARAMETER               :: tau_freezesoil = 30.*86400. 

    !
    DO il = 1, ndeep
       WHERE ( ( ptn(:,il,:) .GT. ZeroCelsius + fr_dT/2. ) .AND. veget_mask_2d(:,:))
          hsdlong(:,il,:) = ( hsd(:,il,:) * dt_sechiba + hsdlong(:,il,:) *(tau_freezesoil-dt_sechiba) ) / tau_freezesoil
       ENDWHERE
    END DO

    IF (printlev>=3) WRITE (numout,*) 'entering thermosoil_wlupdate'

   END SUBROUTINE thermosoil_wlupdate

!!
!================================================================================================================================ 
!! SUBROUTINE   : thermosoil_getdiff_thinsnow 
!! 
!>\BRIEF          Computes soil heat capacity and conductivity     
!! 
!! DESCRIPTION  : Computation of the soil thermal properties; snow properties are also accounted for 
!! 
!! RECENT CHANGE(S) : None 
!!  
!! MAIN OUTPUT VARIABLE(S): 
!!                           
!! REFERENCE(S) : 
!! 
!! FLOWCHART    : None  
!! \n  
!_
!================================================================================================================================ 
     SUBROUTINE thermosoil_getdiff_thinsnow (kjpindex, ptn, shum_ngrnd_permalong, snowdz, profil_froz)

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables
    INTEGER(i_std),INTENT(in)                           :: kjpindex
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(in)        :: ptn
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(in)        :: shum_ngrnd_permalong
    REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT (in)           :: snowdz

    !! 0.2 Output variables
    REAL(r_std),DIMENSION(kjpindex,ngrnd,nvm),INTENT(out)    :: profil_froz

    !! 0.3 Local variables
    REAL(r_std)                                         :: x
    REAL(r_std), DIMENSION(kjpindex)                    :: snow_h
    REAL(r_std), DIMENSION(kjpindex,ngrnd)              :: zx1, zx2
    INTEGER(i_std)                                      :: ji,jg,jv


    DO ji = 1,kjpindex

      ! 1. Determine the fractions of snow and soil

      snow_h(ji) = SUM(snowdz(ji,:))

      IF (snow_h(ji) .LE. 0.01) THEN

         !
         !  1.1. The first level
         !
         IF ( snow_h(ji) .GT. zlt(1) ) THEN

             ! the 1st level is in the snow => the 1st layer is entirely snow
             zx1(ji,1) = 1.
             zx2(ji,1) = 0.
                
         ELSE IF ( snow_h(ji) .GT. zero ) THEN

             ! the 1st level is beyond the snow and the snow is present
             zx1(ji,1) = snow_h(ji) / zlt(1)
             zx2(ji,1) = ( zlt(1) - snow_h(ji)) / zlt(1)        
         ENDIF

         !
         DO jv = 1,nvm
          DO jg = 1, 1
           IF (veget_mask_2d(ji,jv)) THEN
            !
            ! 2. Calculate frozen profile for hydrolc.f90
        !
            IF (ptn(ji,jg,jv) .LT. ZeroCelsius-fr_dT/2.) THEN
                profil_froz(ji,jg,jv) = 1.

                 ELSEIF (ptn(ji,jg,jv) .GT. ZeroCelsius+fr_dT/2.) THEN
                profil_froz(ji,jg,jv) = 0.
                 ELSE

                   ! x is the unfrozen fraction of soil water              
                   x = (ptn(ji,jg,jv)-(ZeroCelsius-fr_dT/2.)) / fr_dT   
              profil_froz(ji,jg,jv) = (1. - x)

            ENDIF

            ! 3. heat capacity calculation
        !
            ! 3.0 old heat capacity calculation
            pcapa(ji,jg,jv) = so_capa_dry + shum_ngrnd_permalong(ji,jg,jv)*(so_capa_wet - so_capa_dry)

        ! 3.1. Still some improvement from the old_version : Take into account the snow and soil fractions in the layer

            pcapa(ji,jg,jv) = zx1(ji,jg) * sn_capa + zx2(ji,jg) * pcapa(ji,jg,jv)

        ! 3.2. Calculate the heat capacity for energy conservation check 
        IF ( zx1(ji,jg).GT.0. ) THEN
               pcapa_en(ji,jg,jv) = sn_capa
        ELSE
               pcapa_en(ji,jg,jv) = pcapa(ji,jg,jv)
        ENDIF
            !
            !4. heat conductivity calculation
        !
            !4.0 old heat conductivity calculation
            pkappa(ji,jg,jv) = so_cond_dry + shum_ngrnd_permalong(ji,jg,jv)*(so_cond_wet - so_cond_dry)

            !4.0 Still some improvement from the old_version : Take into account the snow and soil fractions in the layer

            pkappa(ji,jg,jv) = un / ( zx1(ji,jg) / sn_cond + zx2(ji,jg) / pkappa(ji,jg,jv) )

          ENDIF
         END DO
        END DO
      ENDIF
    ENDDO


   END SUBROUTINE thermosoil_getdiff_thinsnow

  SUBROUTINE thermosoil_rotation_update(ji, kjpindex, matrix_rot, old_veget_max)
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                          :: ji, kjpindex      !! domain size
    REAL(r_std),DIMENSION (nvm), INTENT (in)            :: old_veget_max     !! max fraction of vegetation type
    REAL(r_std), DIMENSION (nvm, nvm), INTENT(in)       :: matrix_rot    !! rotation matrix

    !! 0.4 Local variables
    INTEGER(i_std)                           :: jv, jsrc, jtar, ii

    REAL(r_std),DIMENSION(ngrnd,nvm)         :: ptn_old, dilu_ptn
    REAL(r_std),DIMENSION(ngrnd-1,nvm)       :: cgrnd_old, dgrnd_old, dilu_cgrnd, dilu_dgrnd
    REAL(r_std), DIMENSION(nvm)     :: maxfrac, maxfrac_new

!!!!----------------------------------------------------------------------------------------------

    maxfrac = old_veget_max
    maxfrac_new = old_veget_max(:)
    DO jsrc = 1,nvm
        DO jtar = 1,nvm
            IF (matrix_rot(jsrc,jtar) .GT. 0.0) THEN
                maxfrac_new(jtar) = maxfrac_new(jtar) + maxfrac(jsrc) * matrix_rot(jsrc,jtar)
                maxfrac_new(jsrc) = maxfrac_new(jsrc) - maxfrac(jsrc) * matrix_rot(jsrc,jtar)
            ENDIF
        ENDDO
    ENDDO


    ptn_old = ptn(ji,:,:)
    cgrnd_old = cgrnd(ji,:,:)
    dgrnd_old = dgrnd(ji,:,:)
    DO jtar = 1,nvm
        dilu_ptn(:,:) = zero
        dilu_cgrnd(:,:) = zero
        dilu_dgrnd(:,:) = zero
        IF ( SUM(matrix_rot(:,jtar)) .GT. min_sechiba ) THEN
            DO jsrc = 1,nvm
                IF ( matrix_rot(jsrc,jtar) .GT. min_sechiba ) THEN
                    dilu_ptn(:,jsrc) = ptn_old(:,jsrc)
                    dilu_cgrnd(:,jsrc) = cgrnd_old(:,jsrc)
                    dilu_dgrnd(:,jsrc) = dgrnd_old(:,jsrc)
                ENDIF
            ENDDO
            ptn(ji,:,jtar) = ptn_old(:,jtar) * maxfrac(jtar) * (1.0 - SUM(matrix_rot(jtar,:)))
            cgrnd(ji,:,jtar) = cgrnd_old(:,jtar) * maxfrac(jtar) * (1.0 - SUM(matrix_rot(jtar,:)))
            dgrnd(ji,:,jtar) = dgrnd_old(:,jtar) * maxfrac(jtar) * (1.0 - SUM(matrix_rot(jtar,:)))
            DO jsrc = 1,nvm
                ptn(ji,:,jtar) = ptn(ji,:,jtar) + maxfrac(jsrc) * matrix_rot(jsrc,jtar) * dilu_ptn(:,jsrc)  
                cgrnd(ji,:,jtar) = cgrnd(ji,:,jtar) + maxfrac(jsrc) * matrix_rot(jsrc,jtar) * dilu_cgrnd(:,jsrc)
                dgrnd(ji,:,jtar) = dgrnd(ji,:,jtar) + maxfrac(jsrc) * matrix_rot(jsrc,jtar) * dilu_dgrnd(:,jsrc)
            ENDDO
            ptn(ji,:,jtar) = ptn(ji,:,jtar) / maxfrac_new(jtar)
            cgrnd(ji,:,jtar) = cgrnd(ji,:,jtar) / maxfrac_new(jtar)
            dgrnd(ji,:,jtar) = dgrnd(ji,:,jtar) / maxfrac_new(jtar)
        ENDIF
    ENDDO

    IF (printlev>=4) THEN
        WRITE(numout,*) 'xuhui: debug for thermosoil rotation, ji:',ji
        DO ii=1,2
            WRITE(numout,*) 'checking first 2 layers:'
            WRITE(numout,*) 'ii, ptn_old(ii,:)', ii, ptn_old(ii,:)
            WRITE(numout,*) 'ii, ptn(ji,ii,:)', ii, ptn(ji,ii,:)
            WRITE(numout,*) 'ii, cgrnd_old(ii,:)', ii, cgrnd_old(ii,:)
            WRITE(numout,*) 'ii, cgrnd(ji,ii,:)', ii, cgrnd(ji,ii,:)
            WRITE(numout,*) 'ii, dgrnd_old(ii,:)', ii, dgrnd_old(ii,:)
            WRITE(numout,*) 'ii, dgrnd(ji,ii,:)', ii, dgrnd(ji,ii,:)
        ENDDO
    ENDIF
  END SUBROUTINE thermosoil_rotation_update

END MODULE thermosoil
