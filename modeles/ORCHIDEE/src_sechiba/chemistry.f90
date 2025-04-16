! ================================================================================================================================
!  MODULE       : chemistry
!
!  CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
!  LICENCE      : IPSL (2006)
!  This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF   
!!
!!\n DESCRIPTION: 
!!
!! RECENT CHANGE(S): The content of this module were previously part of the diffuco module.
!!                   ok_inca changed name into ok_bvoc and DIFFUCO_OK_INCA changed into CHEMISTRY_BVOC
!!                   LEAFAGE_OK_INCA changed name into CHEMISTRY_LEAFAGE
!!
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_sechiba/chemistry.f90 $
!! $Date: 2016-03-25 17:44:27 +0100 (Fri, 25 Mar 2016) $
!! $Revision: 3320 $
!! \n
!_ ================================================================================================================================

MODULE chemistry

  USE constantes
  USE qsat_moisture
  USE sechiba_io
  USE ioipsl
  USE pft_parameters
  USE grid
  USE ioipsl_para 
  USE xios_orchidee

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: chemistry_initialize, chemistry_bvoc, chemistry_interface_orchidee_inca


  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_iso            !! Biogenic isoprene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_iso)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_mono           !! Biogenic monoterpene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_mono)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_ORVOC          !! Biogenic ORVOC emission - (kgC.m^{-2}.s^{-1}) 
!$OMP THREADPRIVATE(flx_ORVOC)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_MBO            !! Biogenic MBO emission -
                                                                       !! MethylButanOl (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_MBO)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_methanol       !! Biogenic methanol emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_methanol)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_acetone        !! Biogenic acetone emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_acetone)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_acetal         !! Biogenic Acetaldehyde emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_acetal)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_formal         !! Biogenic Formaldehyde emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_formal)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_acetic         !! Biogenic Acetic Acid emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_acetic)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_formic         !! Biogenic Formic Acid emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_formic)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_no_soil        !! Biogenic NO emission by soil (kgC.m^{-2}.s^{-1})               
!$OMP THREADPRIVATE(flx_no_soil)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_no             !! Biogenic net NO emission (kgC.m^{-2}.s^{-1})                 
!$OMP THREADPRIVATE(flx_no)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: flx_fertil_no      !! Biogenic NO emission due to N-fertilisation 
!$OMP THREADPRIVATE(flx_fertil_no)

  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_apinen         !! Biogenic alfa pinene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_apinen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_bpinen         !! Biogenic beta pinene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_bpinen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_limonen        !! Biogenic limonene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_limonen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_myrcen         !! Biogenic myrcene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_myrcen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_sabinen        !! Biogenic sabinene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_sabinen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_camphen        !! Biogenic camphene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_camphen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_3caren         !! Biogenic 3-carene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_3caren)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_tbocimen       !! Biogenic t-beta-ocimene emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_tbocimen)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_othermono      !! Biogenic other monoterpenes emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_othermono)
  REAL(r_std),ALLOCATABLE, SAVE, DIMENSION(:,:)  :: flx_sesquiter      !! Biogenic sesquiterpenes emission (kgC.m^{-2}.s^{-1})
!$OMP THREADPRIVATE(flx_sesquiter)
  
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:,:) :: CRF                !! Canopy reduction factor for net NO flux calculation (kjpindex,nvm)    
!$OMP THREADPRIVATE(CRF)

  ! variables used inside diffuco_inca module 
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:)     :: ok_siesta        !! Flag for controlling post-pulse period (true/false)
!$OMP THREADPRIVATE(ok_siesta)
  LOGICAL, ALLOCATABLE, SAVE, DIMENSION(:)     :: allow_pulse      !! Flag for controlling pulse period (true/false)
!$OMP THREADPRIVATE(allow_pulse)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pulse            !! Pulse fonction 
!$OMP THREADPRIVATE(pulse)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pulseday         !! Counter for pulse period
!$OMP THREADPRIVATE(pulseday)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: siestaday        !! Counter for post-pulse period
!$OMP THREADPRIVATE(siestaday)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: pulselim         !! Pulse period length
!$OMP THREADPRIVATE(pulselim)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: siestalim        !! Post-pulse period length
!$OMP THREADPRIVATE(siestalim)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: area2            !! Grid cell area (m^2)
!$OMP THREADPRIVATE(area2)
  REAL(r_std), SAVE                            :: nbre_precip 
!$OMP THREADPRIVATE(nbre_precip)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: flx_co2_bbg_year !! CO2 emissions from bbg, 
                                                                   !! read in a file (kgC.m^{-2}.year^{-1})
!$OMP THREADPRIVATE(flx_co2_bbg_year)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: N_qt_WRICE_year  !! N fertilizers on wetland rice,
                                                                   !! read in a file expressed in kgN/year/grid cell
!$OMP THREADPRIVATE(N_qt_WRICE_year)
  REAL(r_std), ALLOCATABLE, SAVE, DIMENSION(:) :: N_qt_OTHER_year  !! N fertilizers on other crops and grasses,
                                                                   !! read in a file expressed in kgN/year/grid cell
!$OMP THREADPRIVATE(N_qt_OTHER_year)

  REAL(r_std), DIMENSION(:,:), SAVE, ALLOCATABLE     :: veget_max_chem        !! Max. vegetation fraction (0-1, unitless)
!$OMP THREADPRIVATE(veget_max_chem) 
  REAL(r_std),DIMENSION (:), SAVE, ALLOCATABLE     :: snow_chem         !! Snow mass (kg)
!$OMP THREADPRIVATE(snow_chem) 
  REAL(r_std),DIMENSION (:,:), SAVE, ALLOCATABLE     :: veget_chem            !! Fraction of vegetation type (-)
!$OMP THREADPRIVATE(veget_chem) 
  REAL(r_std), DIMENSION(:,:), SAVE, ALLOCATABLE     :: lai_chem              !! Leaf area index (m^2.m^{-2})
!$OMP THREADPRIVATE(lai_chem) 

CONTAINS

!! ================================================================================================================================
!! SUBROUTINE   : chemistry_initialize
!!
!>\BRIEF         This subroutine initializes the chemistry module
!!
!! DESCRIPTION  : Some of the variables and flags used chemistry_bvoc are allocated and initialised here.
!!
!! RECENT CHANGE(S): Changed name from diffuco_inca_init to chemistry_initialize
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!_ ================================================================================================================================

  SUBROUTINE chemistry_initialize(kjpindex, lalo, neighbours, resolution)
    
    !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                         :: kjpindex         !! Domain size (unitless) 
    REAL(r_std), DIMENSION(kjpindex,2), INTENT (in)    :: lalo             !! Geographical coordinates
    INTEGER(i_std), DIMENSION(kjpindex,8), INTENT (in) :: neighbours       !! Vector of neighbours for each 
                                                                           !! grid point (1=N, 2=E, 3=S, 4=W)
    REAL(r_std),DIMENSION (kjpindex,2), INTENT(in)     :: resolution       !! The size in km of each grid-box in X and Y

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables

    LOGICAL                             :: allow_weathergen, density
    CHARACTER(LEN=80)                   :: filename
    INTEGER(i_std)                      :: iml, jml, lml, tml, force_id
    INTEGER(i_std)                      :: ier

!_ ================================================================================================================================

    ALLOCATE (pulse(kjpindex),stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable pulse','','')
    pulse(:) = un

    ! If we acount for NOx pulse emissions
    IF (ok_pulse_NOx) THEN

       ALLOCATE (ok_siesta(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable ok_siesta','','')
       ok_siesta(:) = .FALSE.

       ALLOCATE (allow_pulse(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable allow_pulse','','')
       allow_pulse(:) = .FALSE.

       ALLOCATE (pulseday(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable pulseday','','')
       pulseday(:) = zero

       ALLOCATE (siestaday(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable siestaday','','')
       siestaday(:) = zero

       ALLOCATE (pulselim(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable pulselim','','')
       pulselim(:) = zero

       ALLOCATE (siestalim(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable siestalim','','')
       siestalim(:) = zero

    END IF ! (ok_pulse_NOx) 

    ! If we acount for NOx emissions by N-fertilizers
    IF (ok_cropsfertil_NOx) THEN

       ALLOCATE (area2(kjpindex),stat=ier)
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable area2','','')
       area2(:) = resolution(:,1)*resolution(:,2) 

       ALLOCATE (N_qt_WRICE_year(kjpindex),stat=ier)  !! N fertilizers on wetland rice, read in file 
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable N_qt_WRICE_year','','')
       N_qt_WRICE_year(:) = zero
    
       ALLOCATE (N_qt_OTHER_year(kjpindex),stat=ier)  !! N fertilizers on other crops and grasses, read in file 
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable N_qt_OTHER_year','','')
       N_qt_OTHER_year(:) = zero

       WRITE (numout,*) ' *********************** Interpolating N fertilizers files for NOx emissions... '
       density  = .FALSE. 

       !Config Key   = N_FERTIL_FILE
       !Config Desc  = File name
       !Config If    = CHEMISTRY_BVOC and NOx_FERTILIZERS_USE
       !Config Def   = orchidee_fertilizer_1995.nc
       !Config Help  = 
       !Config Units = - 
       filename = 'orchidee_fertilizer_1995.nc'
       CALL getin_p('N_FERTIL_FILE',filename)
       CALL chemistry_read_p (kjpindex, lalo, neighbours, resolution, filename, 'N_qt_WRICE_year', density, &
            N_qt_WRICE_year)
       CALL chemistry_read_p (kjpindex, lalo, neighbours, resolution, filename, 'N_qt_OTHER_year', density, &
            N_qt_OTHER_year)    

    END IF

    ALLOCATE (flx_iso(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_iso','','')
    flx_iso(:,:) = 0. 

    ALLOCATE (flx_mono(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_mono','','')
    flx_mono(:,:) = 0. 

    ALLOCATE (flx_ORVOC(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_ORVOC ','','')
    flx_ORVOC(:,:) = 0. 

    ALLOCATE (flx_MBO(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_MBO','','')
    flx_MBO(:,:) = 0. 

    ALLOCATE (flx_methanol(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_methanol','','')
    flx_methanol(:,:) = 0. 

    ALLOCATE (flx_acetone(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_acetone','','')
    flx_acetone(:,:) = 0. 

    ALLOCATE (flx_acetal(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_acetal','','')
    flx_acetal(:,:) = 0. 

    ALLOCATE (flx_formal(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_formal','','')
    flx_formal(:,:) = 0. 

    ALLOCATE (flx_acetic(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_acetic','','')
    flx_acetic(:,:) = 0. 

    ALLOCATE (flx_formic(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_formic','','')
    flx_formic(:,:) = 0. 

    ALLOCATE (flx_no_soil(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_no_soil','','')
    flx_no_soil(:,:) = 0. 

    ALLOCATE (flx_no(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_no','','')
    flx_no(:,:) = 0. 
       
    ALLOCATE (flx_fertil_no(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_fertil_no','','')
    flx_fertil_no(:,:) = 0. 

    ALLOCATE (flx_apinen(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_apinen','','')
    flx_apinen(:,:) = 0.       

    ALLOCATE (flx_bpinen (kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_bpinen','','')
    flx_bpinen(:,:) = 0.      

    ALLOCATE (flx_limonen  (kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_limonen','','')
    flx_limonen(:,:) = 0.    

    ALLOCATE (flx_myrcen(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_myrcen','','')
    flx_myrcen(:,:) = 0.       

    ALLOCATE (flx_sabinen(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_sabinen','','')
    flx_sabinen(:,:) = 0.      

    ALLOCATE (flx_camphen(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_camphen','','')
    flx_camphen(:,:) = 0.      

    ALLOCATE (flx_3caren(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_3caren','','')
    flx_3caren(:,:) = 0.       

    ALLOCATE (flx_tbocimen(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_tbocimen','','')
    flx_tbocimen(:,:) = 0.     

    ALLOCATE (flx_othermono(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_othermono','','')
    flx_othermono(:,:) = 0.    

    ALLOCATE (flx_sesquiter(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable flx_sesquiter','','')
    flx_sesquiter(:,:) = 0.    

    ALLOCATE(CRF(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable CRF ','','')
    CRF(:,:) = 0. 


    ALLOCATE(veget_max_chem(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable veget_max_chem ','','')
    veget_max_chem(:,:) = 0. 
  
    ALLOCATE(snow_chem(kjpindex), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable snow_chem ','','')
    snow_chem(:) = 0. 

    ALLOCATE(veget_chem(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable veget_chem ','','')
    veget_chem(:,:) = 0. 

    ALLOCATE(lai_chem(kjpindex,nvm), stat=ier) 
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_init','Problem in allocate of variable lai_chem ','','')
    lai_chem(:,:) = 0. 

    ! If we acount for NOx emissions due to Biomass Burning
    IF (ok_bbgfertil_NOx) THEN

       ALLOCATE (flx_co2_bbg_year(kjpindex),stat=ier) !! CO2 emissions from bbg, read in file 
       IF (ier /= 0) CALL ipslerr_p(3,'chemistry_initialize','Problem in allocate of variable flx_co2_bbg_year','','')
       flx_co2_bbg_year(:) = zero    

       WRITE (numout,*) ' *********************** Interpolating CO2 bbg files for NOx emissions... '
       !Config Key   = N_FERTIL_FILE
       !Config Desc  = File name
       !Config If    = CHEMISTRY_BVOC and NOx_FERTILIZERS_USE
       !Config Def   = orchidee_fertilizer_1995.nc
       !Config Help  = ...
       !Config Units = -
       filename = 'orchidee_bbg_clim.nc'
       CALL getin_p('CO2_BBG_FILE',filename)
       density  = .TRUE.
       CALL chemistry_read_p (kjpindex, lalo, neighbours, resolution, filename,'flx_co2_bbg_year',density, &
            flx_co2_bbg_year)

    END IF

    IF ( OFF_LINE_MODE ) THEN

       !-
       !- What are the alowed options for the temporal interpolation
       !-
       ! ALLOW_WEATHERGEN : Allow weather generator to create data
       ! This parameter is already read in the driver
       allow_weathergen = .FALSE.
       CALL getin_p('ALLOW_WEATHERGEN',allow_weathergen)
       
       ! FORCING_FILE : Name of file containing the forcing data
       ! This parameter is already read in the driver
       filename='forcing_file.nc'
       CALL getin_p('FORCING_FILE',filename)
       CALL flininfo(filename,iml, jml, lml, tml, force_id)   
       WRITE(numout,*) 'Number of data per year in forcing file :', tml 
       CALL flinclo(force_id)
       WRITE(numout,*) 'Forcing file closed in chemistry_initialize'
       
       
       IF ( allow_weathergen ) THEN
          WRITE(numout,*) '**chemistry_initialize: Using weather generator, careful to precip division for NOx '
          nbre_precip = un
          WRITE(numout,*) 'Division pour les precip, NOx:', nbre_precip
       ELSE
          WRITE(numout,*) 'DT_SECHIBA :', dt_sechiba
          nbre_precip = (one_day/dt_sechiba)/(tml/one_year)
          WRITE(numout,*) 'Division pour les precip, NOx:', nbre_precip
       END IF

    ELSE ! (in coupled mode)

       nbre_precip = un
       
    END IF  ! (OFF_LINE_MODE)

       
  END SUBROUTINE chemistry_initialize


!! ================================================================================================================================
!! SUBROUTINE   : chemistry_bvoc
!!
!>\BRIEF         This subroutine computes biogenic emissions of reactive compounds, that is of
!!               VOCs (volatile organic compounds) from vegetation and NOx (nitrogen oxides) from soils.
!!               Calculation are mostly based on the works by Guenther et al. (1995) and Yienger and Levy (1995).\n 
!!
!! DESCRIPTION  : Biogenic VOC emissions from vegetation are based on the parameterisations developped by
!!                Guenther et al. (1995). Biogenic VOCs considered here are: isoprene, monoterpenes, OVOC and ORVOC 
!!                as bulked emissions, methanol, acetone, acetaldehyde, formaldehyde, acetic acid, formic acid
!!                as single emissions.\n
!!                For every biogenic VOCs an emission factor (EF), depending on the PFT considered, is used.\n
!!                Isoprene emissions depend on temperature and radiation. A partition between sunlit and shaded
!!                leaves is taken into account and either one (if ok_multilayer = FALSE) or several layers
!!                (if ok_multilayer = TRUE) in the canopy can be used.\n
!!                When radiation extinction is considered, the canopy radiative transfer model takes into 
!!                account light extinction through canopy, calculating first need diffuse and direct radiation
!!                based on Andrew Friend 2001 radiative model and Spitters et al. 1986. The calculation of lai, 
!!                parscat, parsh and parsun, laisun and laishabsed based on Guenther et al.(JGR, 1995) and Norman (1982).\n
!!                Emissions for other BVOCs (monoterpenes, OVOC, ORVOC and other single compounds such as
!!                methanol, acetone...) depend only on temperature.\n   
!!                The impact of leaf age, using an emission activity prescribed for each of the 4 leaf age
!!                classes can also be considered for isoprene and methanol emissions when ok_leafage = TRUE.\n
!!                NOx emissions from soils are based on Yienger and Levy (1995) and depend on soil moisture
!!                and temperature and PFT. The pulse effect, related to significant rain occuring after severe
!!                drought can also be considered (ok_pulse_NOx = TRUE), as well as the increase in emissions related to
!!                biomass buring (ok_bbgfertil_NOx = TRUE) or use of fertilizers (ok_cropsfertil_NOx = TRUE). 
!!                A net NO flux is eventually calculated taking into account loss by deposition on the surface, using
!!                a Canopy Reduction Factor (CRF) based on stomatal and leaf area.\n 
!!                This subroutine is called by diffuco_main only if biogenic emissions are activated
!!                for sechiba (flag CHEMISTRY_BVOC=TRUE).\n
!!
!! RECENT CHANGE(S): Changed name from diffuco_inca to chemistry_bvoc
!!
!! MAIN OUTPUT VARIABLE(S): :: PAR, :: PARsun, :: PARsh, :: laisun, :: laish,
!!                          :: flx_iso, :: flx_mono, :: flx_ORVOC, :: flx_MBO,
!!                          :: flx_methanol, :: flx_acetone, :: flx_acetal, :: flx_formal,
!!                          :: flx_acetic, :: flx_formic, :: flx_no_soil, :: flx_no,
!!                          :: CRF, :: flx_fertil_no, :: Trans, :: Fdf,
!!                          :: PARdf, :: PARdr, :: PARsuntab, :: PARshtab
!!
!! REFERENCE(S) :
!! - Andrew Friend (2001), Modelling canopy CO2 fluxes: are 'big-leaf' simplifications justified? 
!! Global Ecology and Biogeography, 10, 6, 603-619, doi: 10.1046/j.1466-822x.2001.00268.x 
!! - Spitters, C.J.T, Toussaint, H.A.J.M, Groudriaan, J. (1986), Separating the diffuse and direct
!! component of global radiation and its implications for modeling canopy photosynthesis, Agricultural
!! and Forest Meteorology, 38, 1-3, 217-229, doi:10.1016/0168-1923(86)90060-2
!! - Norman JM (1982) Simulation of microclimates. In: Hatfield JL, Thomason IJ (eds)
!!  Biometeorology in integrated pest management. Academic, New York, pp 65–99
!! - Guenther, A., Hewitt, C. N., Erickson, D., Fall, R., Geron, C., Graedel, T., Harley, P.,
!! Klinger, L., Lerdau, M., McKay, W. A., Pierce, T., Scholes, B., Steinbrecher, R., Tallamraju,
!! R., Taylor, J. et Zimmerman, P. (1995), A global model of natural volatile organic compound
!! emissions, J. Geophys. Res., 100, 8873-8892.
!! - MacDonald, R. et Fall, R. (1993), Detection of substantial emissions of methanol from
!! plants to the atmosphere, Atmos. Environ., 27A, 1709-1713.
!! - Guenther, A., Geron, C., Pierce, T., Lamb, B., Harley, P. et Fall, R. (2000), Natural emissions
!! of non-methane volatile organic compounds, carbon monoxide, and oxides of nitrogen from
!! North America, Atmos. Environ., 34, 2205-2230.
!! - Yienger, J. J. et Levy II, H. (1995), Empirical model of global soil-biogenic NOx emissions,
!! J. Geophys. Res., 100, 11,447-11,464.
!! - Lathiere, J., D.A. Hauglustaine, A. Friend, N. De Noblet-Ducoudre, N. Viovy, and
!!  G. Folberth (2006), Impact of climate variability and land use changes on global biogenic volatile 
!! organic compound emissions, Atmospheric Chemistry and Physics, 6, 2129-2146. 
!! - Lathiere, J., D.A. Hauglustaine, N. De Noblet-Ducoudre, G. Krinner et G.A. Folberth (2005),
!! Past and future changes in biogenic volatile organic compound emissions simulated with a global
!! dynamic vegetation model, Geophysical Research Letters, 32, doi: 10.1029/2005GL024164.
!! - Lathiere, J. (2005), Evolution des emissions de composes organiques et azotes par la biosphere
!!  continentale dans le modele LMDz-INCA-ORCHIDEE, These de doctorat, Universite Paris VI.
!!
!! FLOWCHART    : None
!_ ================================================================================================================================

  SUBROUTINE chemistry_bvoc (kjpindex, swdown, coszang, temp_air, &
       temp_sol, ptnlev1, precip_rain, humrel, veget_max, lai, &
       frac_age, lalo, ccanopy, cim,  wind, snow, &
       veget, hist_id, hist2_id,kjit, index, &
       indexlai, indexveg)

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                                 :: kjpindex         !! Domain size - terrestrial pixels only (unitless)
    INTEGER(i_std), INTENT(in)                                 :: kjit             !! Time step number (-) 
    INTEGER(i_std),INTENT (in)                                 :: hist_id          !! History file identifier (-)
    INTEGER(i_std),INTENT (in)                                 :: hist2_id         !! History file 2 identifier (-)
    INTEGER(i_std),DIMENSION (kjpindex), INTENT (in)           :: index            !! Indeces of the points on the map (-)
    INTEGER(i_std),DIMENSION (kjpindex*(nlai+1)), INTENT (in)  :: indexlai         !! Indeces of the points on the 3D map
    INTEGER(i_std),DIMENSION (kjpindex*nvm), INTENT (in)       :: indexveg         !! Indeces of the points on the 3D map (-)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: swdown           !! Down-welling surface short-wave flux 
                                                                                   !! (W.m^{-2})
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: coszang          !! Cosine of the solar zenith angle (unitless) 
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: temp_air         !! Air temperature (K)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: temp_sol         !! Skin temperature (K)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: ptnlev1          !! 1st level of soil temperature (K)
    REAL(r_std), DIMENSION(kjpindex), INTENT(in)               :: precip_rain      !! Rain precipitation !!?? init
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)           :: humrel           !! Soil moisture stress (0-1, unitless)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)           :: veget_max        !! Max. vegetation fraction (0-1, unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)              :: snow             !! Snow mass (kg)
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)          :: veget            !! Fraction of vegetation type (-)
    REAL(r_std), DIMENSION(kjpindex,nvm), INTENT(in)           :: lai              !! Leaf area index (m^2.m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nvm,nleafages), INTENT(in) :: frac_age         !! Age efficacity from STOMATE for iso 
    REAL(r_std), DIMENSION(kjpindex,2), INTENT(in)             :: lalo             !! Geographical coordinates for pixels (degrees)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)              :: ccanopy          !! CO2 concentration inside the canopy
    REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)          :: cim              !! Intercellular CO2 over nlai 
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)              :: wind             !! Wind module (m s^{-1})

    !! 0.2 Output variables

    !! 0.3 Modified variables

    !! 0.4 Local variables 

    INTEGER(i_std)                             :: ji, jv, jf, jl    !! Indices (unitless)
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: fol_dens          !! foliar density (gDM.m^{-2})
    REAL(r_std), DIMENSION(kjpindex)           :: tleaf             !! Foliar temperature (K)
    REAL(r_std), DIMENSION(kjpindex)           :: t_no              !! Temperature used for soil NO emissions (C)
    REAL(r_std), DIMENSION(kjpindex)           :: exp_1             !! First exponential used in the calculation of 
                                                                    !! isoprene dependancy to Temperature 
    REAL(r_std), DIMENSION(kjpindex)           :: exp_2             !! Second exponential used in the calculation of 
                                                                    !! Isoprene dependancy to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_iso            !! Isoprene dependancy to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Cl_iso            !! Isoprene dependancy to Light 
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_mono           !! Monoterpene dependancy to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_sesq           !! Sesquiterpenes dependancy to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_meth           !! Methanol dependancy to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_acet           !! Acetone dependancy to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_oxyVOC         !! Other oxygenated BVOC dependancy to Temperature
    REAL(r_std)                                :: GAMMA_iso         !! Temperature and light dependancy for isoprene and fo each PFT
    REAL(r_std)                                :: GAMMA_iso_m       !! Temperature and light dependancy for isoprene and fo each PFT for multilayer
    REAL(r_std), DIMENSION(kjpindex)           :: Ylt_mono          !! Total Temperature and light dependancy for monoterpenes
    REAL(r_std), DIMENSION(kjpindex)           :: Ylt_sesq          !! Total Temperature and light dependancy for sesquiterpens
    REAL(r_std), DIMENSION(kjpindex)           :: Ylt_meth          !! Total Temperature and light dependancy for methanol
    REAL(r_std), DIMENSION(kjpindex)           :: Ylt_acet          !! Total Temperature and light dependancy for acetone
    REAL(r_std), DIMENSION(kjpindex)           :: Ct_MBO            !! MBO dependance to Temperature
    REAL(r_std), DIMENSION(kjpindex)           :: Cl_MBO            !! MBO dependance to Light
    REAL(r_std), DIMENSION(kjpindex)           :: Xvar              !! Parameter used in the calculation 
                                                                    !! of MBO dependance to Temperature
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: flx_OVOC          !! Biogenic OVOC emission - 
                                                                    !! Other Volatil Organic Components (kgC.m^{-2}.s^{-1})
    !!Canopy radiative transfer model
    REAL(r_std)                                :: day               !! Day of The Year
    REAL(r_std), DIMENSION(kjpindex)           :: So                !! Maximum radiation at the Earth surface (W.m^{-2})
    REAL(r_std), DIMENSION(kjpindex)           :: Rfrac             !! Parameter in the regression of diffuse 
                                                                    !! share on transmission
    REAL(r_std), DIMENSION(kjpindex)           :: Kfrac             !! Parameter in the regression of diffuse 
                                                                    !! share on transmission
    REAL(r_std), DIMENSION(kjpindex)           :: swdf              !! Sw diffuse radiation (W.m^{-2}) 
    REAL(r_std), DIMENSION(kjpindex)           :: swdr              !! Sw direct radiation (W.m^{-2}) 
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: PARscat           !! Scatter PAR @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: Clsun_iso         !! Isoprene dependance to light for sun leaves 
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: Clsh_iso          !! Isoprene dependance to light for shaded leaves
    !! for multilayer canopy model for iso flux
    REAL(r_std), DIMENSION(kjpindex,nlai+1)    :: PARscattab        !! Scatter PAR @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std), DIMENSION(nlai+1)             :: laitab            !! LAI per layer (m^2.m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nlai)      :: laisuntabdep      !! LAI of sun leaves in each layer (m^2.m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nlai)      :: laishtabdep       !! LAI of shaded leaves in each layer 
                                                                    !! (m^2.m^{-2})
    REAL(r_std)                                :: Clsun_iso_tab     !! Isoprene dependance to light 
                                                                    !! for sun leaves and per layer 
    REAL(r_std)                                :: Clsh_iso_tab      !! Isoprene dependance to light 
                                                                    !! for shaded leaves and per layer
    !for multilayer canopy model Spitter et al. 1986
    REAL(r_std), DIMENSION(kjpindex,nlai+1)    :: PARnotscat        !! Not-Scattered PAR 
    REAL(r_std), DIMENSION(kjpindex,nlai+1)    :: PARabsdir         !! Absorbed light of the PAR direct flux  
    REAL(r_std), DIMENSION(kjpindex,nlai+1)    :: PARabsdiff        !! Absorbed light of the PAR diffuse flux  
    REAL(r_std), PARAMETER                     :: sigma = 0.20      !! scattering coefficient of single leaves and for visible radiation
    REAL(r_std), PARAMETER                     :: cluster = 0.85    !! clustering coefficient for leaves, the same that is setting for default in MEGAN V2.10
    REAL(r_std)                                :: rho               !! reflection index of a green, closed vegetation
    REAL(r_std)                                :: kbl               !! extinction coefficient of black leaves
    REAL(r_std)                                :: kdf               !! extinction coefficient of diffuse flux
    !!Leaf age
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: Eff_age_iso       !! Isoprene emission dependance to Leaf Age 
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: Eff_age_meth      !! Methanol emission dependance to Leaf Age 
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: Eff_age_VOC       !! Other VOC emission dependance to Leaf Age 
    !!BBG and Fertilizers for NOx soil emission
    REAL(r_std), DIMENSION(kjpindex)           :: veget_max_nowoody !! sum of veget_max for non-woody PFT
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: N_qt_WRICE_pft    !! N fertiliser on RICE 
                                                                    !! (kgN per year per grid cell)
    REAL(r_std), DIMENSION(kjpindex,nvm)       :: N_qt_OTHER_pft    !! N fertiliser on other veg 
                                                                    !! (kgN per year per grid cell)
    !! CO2 inhibition effect on isoprene
    REAL(r_std),DIMENSION (kjpindex,nvm)       :: fco2_wshort       !! Wilkinson short term function for CO2 impact on BVOC (isoprene) 
    REAL(r_std),DIMENSION (kjpindex)           :: fco2_wlong        !! Wilkinson long term function for CO2 impact on BVOC (isoprene) 
    REAL(r_std)                                :: fco2_ctrl
    REAL(r_std),DIMENSION (kjpindex,nvm)       :: fco2              !! Function for CO2 impact on BVOC (isoprene)
    REAL(r_std), DIMENSION(kjpindex)           :: Ismax_short
    REAL(r_std), DIMENSION(kjpindex)           :: h_short
    REAL(r_std), DIMENSION(kjpindex)           :: Cstar_short
    REAL(r_std)                                :: Ismax_long
    REAL(r_std)                                :: h_long
    REAL(r_std)                                :: Cstar_long

    !! 0.5 Parameters values

    REAL(r_std), PARAMETER :: CT1 = 95000.0       !! Empirical coeffcient (see Guenther .et. al, 1995, eq(10)) (J.mol^{-1})
    REAL(r_std), PARAMETER :: CT2 = 230000.0      !! Empirical coefficient (see Guenther .et. al, 1995, eq(10)) (J.mol^{-1})
    REAL(r_std), PARAMETER :: TS = 303.0          !! Leaf temperature at standard condition
                                                  !! (see Guenther .et. al, 1995, eq(10)) (K)
    REAL(r_std), PARAMETER :: TM = 314.0          !! Leaf temperature (see Guenther .et. al, 1995, eq(10)) (K)

    REAL(r_std), PARAMETER :: alpha_ = 0.0027     !! Empirical coeffcient (see Guenther .et. al, 1995, eq(9)) (unitless)
    REAL(r_std), PARAMETER :: CL1 = 1.066         !! Empirical coeffcient (see Guenther .et. al, 1995, eq(9)) (unitless)
    REAL(r_std), PARAMETER :: beta = 0.09         !! Empirical coeffcient (see Guenther .et. al, 1995, eq(11)) (K^{-1})
    REAL(r_std), PARAMETER :: lai_threshold = 11. !! Lai threshold for the calculation of scattered radiation
                                                  !! based on Guenther .et. al (1995) (m^2.m^{-2})

                                              
    ! Biogenic emissions
    REAL(r_std),DIMENSION(kjpindex)          :: PAR                !! Photosynthetic active radiation, half of swdown
                                                                   !! @tex ($\mu mol photons. m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm)      :: PARsun             !! PAR received by sun leaves
                                                                   !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm)      :: PARsh              !! PAR received by shaded leaves 
                                                                   !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex,nvm)      :: laisun             !! Leaf area index of Sun leaves (m^2.m^{-2})
    REAL(r_std),DIMENSION(kjpindex,nvm)      :: laish              !! Leaf area index of Shaded leaves (m^2.m^{-2}) 

    CHARACTER(LEN=14)                        :: tleafsun_name      !! To store variables names for I/O
    CHARACTER(LEN=13)                        :: tleafsh_name       !! To store variables names for I/O
    REAL(r_std), DIMENSION(kjpindex,nlai+1)  :: Tleafsun_temp      !! temporary sunlit leaf temperature matrix for writing
    REAL(r_std), DIMENSION(kjpindex,nlai+1)  :: Tleafsh_temp       !! temporary shade leaf temperature matrix for writing
    REAL(r_std), DIMENSION(kjpindex,nlai+1)  :: laisuntab          !! LAI of sun leaves per layer (m^2.m^{-2})
    REAL(r_std), DIMENSION(kjpindex,nlai+1)  :: laishtab           !! LAI of shaded leaves per layer 
    REAL(r_std),DIMENSION(kjpindex)          :: Fdf                !! Diffuse Fraction of the radiation (0-1, unitless)
    REAL(r_std),DIMENSION(kjpindex,nlai+1)   :: PARsuntab          !! PAR received by sun leaves 
                                                                   !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex,nlai+1)   :: PARshtab           !! PAR received by shaded leaves 
                                                                   !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex)          :: PARdf              !! Diffuse PAR
                                                                   !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex)          :: PARdr              !! Direct PAR 
                                                                   !! @tex ($\mu mol m^{-2} s^{-1}$) @endtex
    REAL(r_std),DIMENSION(kjpindex)          :: Trans              !! Atmospheric Transmissivity (unitless)

!_ ================================================================================================================================
        fco2 = 0.
        fco2_wshort = 0.
        fco2_wlong = 0.
        Fdf(:) = 0.
        PAR(:) = 0. 
        PARsun(:,:) = 0. 
        PARsh(:,:) = 0. 
        laisun(:,:) = 0. 
        laish(:,:) = 0. 
        CRF(:,:) = 0.               
        Trans(:) = 0.            
        PARdf(:) = 0.           
        PARdr(:) = 0.            
        PARsuntab(:,:) = 0.        
        PARshtab(:,:) = 0.        
   

        !! 0. mise a jour des variables pour le couplage avec inca 
        veget_max_chem(:,:) = veget_max(:,:)
        veget_chem(:,:) = veget(:,:) 
        lai_chem(:,:) = lai(:,:) 
        snow_chem(:) = snow(:) 

    !! 1. Canopy radiative transfer model

    !! Canopy radiative transfer model: takes into account light extinction through canopy
    !! First need to calculate diffuse and direct radiation
    !! Based on Andrew Friend radiative model (Global Ecology & Biogeography, 2001)
    !! And Spitters et al. (Agricultural and Forest Meteorology, 1986)
       
    IF ( ok_radcanopy ) THEN

       DO ji = 1, kjpindex
          IF (coszang(ji) .GT. zero) THEN
             day = julian_diff
             !! 1.1 Extra-terrestrial solar irradiance at a plan parallel to Earh's surface
             So(ji) = Sct*( un + 0.033*COS(360.*pi/180.*day/365.))*coszang(ji)
             !! 1.2 Atmospheric transmissivity
             Trans(ji) = swdown(ji)/So(ji)
             !! 1.3 Empirical calculation of fraction diffuse from transmission based on Spitters et al. (1986)
             Rfrac(ji) = 0.847 - 1.61*coszang(ji) + 1.04*(coszang(ji)**2.)
             Kfrac(ji) = (1.47 - Rfrac(ji))/1.66      
             IF (Trans(ji) .LE. 0.22) THEN
                Fdf(ji) = un
             ELSE IF (Trans(ji) .LE. 0.35) THEN
                Fdf(ji) = un - 6.4*((Trans(ji) - 0.22)**2.) 
             ELSE IF (Trans(ji) .LE. Kfrac(ji)) THEN
                Fdf(ji) = 1.47 - 1.66*Trans(ji)
             ELSE
                Fdf(ji) = Rfrac(ji)
             END IF
             !! 1.4 Direct and diffuse sw radiation in W.m^{-2}
             swdf(ji) = swdown(ji)*Fdf(ji)
             swdr(ji) = swdown(ji)*(un-Fdf(ji))
          ELSE
             swdf(ji) = zero
             swdr(ji) = zero
          END IF

          !! 1.5 PAR diffuse and direct in umol/m^2/s
          PARdf(ji) = swdf(ji) * W_to_mmol * RG_to_PAR
          PARdr(ji) = swdr(ji) * W_to_mmol * RG_to_PAR 
       END DO

       !! 1.6 Calculation of lai, parscat, parsh and parsun, laisun and laish !!?? define the terms
       !! Based on Guenther et al. (JGR, 1995) and Norman (1982)
       ! One-layer canopy model or multi-layer canopy model
       IF (ok_multilayer) THEN 


          ! Calculation PER LAYER
          DO jl = 1, nlai+1
            laitab(jl) = laimax*(EXP(lai_level_depth*(jl-1) - un)/(EXP(lai_level_depth*nlai) - un))

         !introduction of the Spitter way to calculate radiation over the levels !!!!!!!
             DO ji = 1, kjpindex
                ! Coefficients calculation:
                IF (ABS(ACOS(coszang(ji))) .LT. pi/2. .AND. coszang(ji) .NE. zero) THEN
                   kbl = cluster*0.5/ABS(coszang(ji))
                   rho = ((1-SQRT((1 - sigma)))/(1+SQRT((1 - sigma))))*(2/(1+1.6*ABS(coszang(ji))))
                ELSE
                   kbl = 0.
                   rho = 0.
                END IF 
                kdf = cluster*0.8*SQRT((1 - sigma))
                IF (ABS(ACOS(coszang(ji))) .LT. pi/2. .AND. coszang(ji) .NE. zero) THEN
                   PARnotscat(ji,jl) = (1 - sigma)*PARdr(ji)*kbl*EXP(-SQRT((1 - sigma))*kbl*laitab(jl))
                   PARabsdir(ji,jl) = (1 - rho)*SQRT((1 - sigma))*PARdr(ji)*kbl*EXP(-SQRT((1 - sigma))*kbl*laitab(jl))
                   PARabsdiff(ji,jl) = (1 - rho)*PARdf(ji)*kdf*EXP(-kdf*laitab(jl))
                   PARshtab(ji,jl) = (PARabsdiff(ji,jl) + (PARabsdir(ji,jl) - PARnotscat(ji,jl)))/(1 - sigma)
                   PARsuntab(ji,jl) = PARshtab(ji,jl) + (1-sigma)*kbl*PARdr(ji)/(1 - sigma) 
                   !correction using the equation (4) in Bodin et al 2012 and (7) or (8) in Spitter et al 1986 
                   !using the extinction coefficient kbl = 0.5/coszang and not only 0.5
                   IF (jl .NE. nlai+1) THEN
                      laisuntabdep(ji,jl) =(laitab(jl+1)-laitab(jl))*EXP(-kbl*laitab(jl))
                      laishtabdep(ji,jl) =(laitab(jl+1)-laitab(jl))*(1.-EXP(-kbl*laitab(jl)))
                   ENDIF
                ELSE
                   PARshtab(ji,jl) = PARdf(ji)*kdf*EXP(-kdf*laitab(jl))
                   PARsuntab(ji,jl) = 0.
                   laisuntab(ji,jl) = 0. 
                   laishtab(ji,jl) = laitab(jl)
                   IF (jl .NE. nlai+1) THEN
                      laisuntabdep(ji,jl) = 0.
                      laishtabdep(ji,jl) = laitab(jl+1)-laitab(jl)
                   ENDIF  
                END IF
             END DO
          END DO



       ! introduction of the Spitter way to calculate radiation over the levels !!!!!!!
       ELSE
          ! Calculation FOR one layer
          DO jv = 1, nvm
             DO ji = 1, kjpindex
                IF (lai(ji,jv) .LE. lai_threshold) THEN
                   PARscat(ji,jv) = 0.07*PARdr(ji)*(1.1 - 0.1*lai(ji,jv))*exp(-coszang(ji))
                ELSE
                   PARscat(ji,jv) = zero
                END IF

                IF (coszang(ji) .NE. zero ) THEN
                   PARsh(ji,jv) = PARdf(ji)*exp(-0.5*((lai(ji,jv))**0.7)) + PARscat(ji,jv)
                   PARsun(ji,jv) = PARdr(ji)*COS(60.*pi/180.)/coszang(ji) + PARsh(ji,jv)
                ELSE
                   PARsh(ji,jv) = PARdf(ji)*exp(-0.5*(lai(ji,jv)**0.7)) + PARscat(ji,jv)
                   PARsun(ji,jv) = zero 
                END IF
                IF (ABS(ACOS(coszang(ji))) .LT. pi/2. .AND. coszang(ji) .NE. zero) THEN 
                   ! calculation as in Lathiere (2005) = with correction removing lai in Guenther (1995)
                   laisun(ji,jv) = (un - exp(-0.5*lai(ji,jv)/(coszang(ji))))*coszang(ji)/COS(60.*pi/180.)
                   laish(ji,jv) = lai(ji,jv) - laisun(ji,jv)
                ELSE
                   laisun(ji,jv) = zero
                   laish(ji,jv) = lai(ji,jv)
                END IF
             END DO
          END DO
       ENDIF
    END IF


    !! 2. Calculation of non-PFT dependant parameters used for VOC emissions
    DO ji = 1, kjpindex ! (loop over # pixels)
       !! 2.1 Calculation of Tleaf (based on Lathiere, 2005)


       tleaf(ji) = temp_air(ji)

       !! 2.2 Isoprene emission dependency - with no PARsun/PARshaded partitioning - Guenther et al. (1995) and Lathiere (2005)
       !> @codeinc $$?? ecrire les equation en latex ? 
       exp_1(ji) = exp( (CT1 * ( tleaf(ji) - TS )) / (RR*TS*tleaf(ji)) )
       exp_2(ji) = exp( (CT2 *( tleaf(ji) - TM )) / (RR*TS*tleaf(ji)) )
       PAR(ji)   = swdown(ji) * W_to_mmol * RG_to_PAR        ! from W/m^2 to umol photons/m^2/s and half of sw for PAR
       Ct_iso(ji)    = exp_1(ji)/(un + exp_2(ji))            ! temperature dependance  
       Cl_iso(ji)    = alpha_*CL1*PAR(ji)/sqrt(un + (alpha_**2) * (PAR(ji)**2) ) ! light dependance
       !> @endcodeinc
  
       !! 2.3 Monoterpene and oxy VOB emission dependency to Temperature 
       !!     light independant fraction
       !> @codeinc
       !Ct_mono(ji) = exp(beta*(tleaf(ji) - TS))  ! Old method
       Ct_mono(ji) = exp(beta_mono*(tleaf(ji) - TS))
       Ct_sesq(ji) = exp(beta_sesq*(tleaf(ji) - TS))
       Ct_meth(ji) = exp(beta_meth*(tleaf(ji) - TS))
       Ct_acet(ji) = exp(beta_acet*(tleaf(ji) - TS))
       Ct_oxyVOC(ji) = exp(beta_oxyVOC*(tleaf(ji) - TS))     
       !> @endcodeinc
       !! 2.4 MBO biogenic emissions dependency, only from PFT7 and PFT4 for location of vegetation emitter
       ! but in fact MBO fluxes only in America (ponderosa and lodgepole pines only found in these areas)
       !> @codeinc
       Xvar(ji) = ((un/312.3) - (un/tleaf(ji)))/RR
       !> @endcodeinc
       !! 2.4.1 temperature dependency
       !> @codeinc
       Ct_MBO(ji)    = (1.52*209000.0*exp(67000.0*Xvar(ji)))/(209000.0 - 67000.0*(un - exp(209000.0*Xvar(ji))))
       !> @endcodeinc
       !! 2.4.2 light dependency
       Cl_MBO(ji)    = (0.0011*1.44*PAR(ji))/(sqrt(un + (0.0011**2)*(PAR(ji)**2)))
       !! 2.5 NO biogenic emissions given in ngN/m^2/s, emission factor in ngN/m^2/s too
       !! calculation of temperature used for NO soil emissions
       t_no(ji) = ptnlev1(ji) - ZeroCelsius  !!temp must be in celsius to calculate no emissions
       !! 2.6 calculation of non-woody veget_max fraction
       IF (ok_cropsfertil_NOx) THEN
          veget_max_nowoody(ji) = zero
          DO jv = 1,nvm
             IF ( (jv /= ibare_sechiba) .AND. .NOT.(is_tree(jv)) ) THEN
                veget_max_nowoody(ji) = veget_max_nowoody(ji) + veget_max(ji,jv)
             ENDIF
          ENDDO
       END IF
    END DO ! (loop over # pixels)

    !! 2bis. Calculation of CO2 function for inhibition effect on isoprene 
    ! 2 approaches can be used: either Possell et al. (2005) or Wilkinson et al. (2006) 

!! 19/04/2010 and then implemented in version revised by Nicolas Vuichard, 08042014
!! Impact of atmospheric CO2 on isoprene emissions
!! Can be activated or not
!! If considered, can use either Possell 2005 or Wilkinson 2009 parameterisation 
!! This is used to rescale the emission factor, considered to be measured at 350 ppm of CO2
!! to the CO2 conditions of the run

IF ( ok_co2bvoc_poss ) THEN
   WRITE(numout,*) 'CO2 impact on isoprene: Possell calculation'

   !! Possell function needs to be normalized (experiments at 400 ppm and EF before 1995)
   !! Normalized at 350 ppm
   fco2_ctrl = (-0.0123+(441.4795/350.)+(-1282.65/(350.**2)))

   !! 2 tests: using the canopy (atmospheric) CO2 'ccanopy'
   !! or the intercellular CO2 over nlai 'cim' 
   !! with cim = ccanopy*0.667 
   !! in the end I go for ccanopy for the Possell function since too much differences
   !! when using cim and also the function has been derived based on atmospheric CO2
   DO ji = 1, kjpindex

      fco2(ji,:) = (-0.0123+(441.4795/ccanopy(ji))+(-1282.65/(ccanopy(ji)*ccanopy(ji))))/fco2_ctrl

   END DO
ELSE IF ( ok_co2bvoc_wilk ) THEN
   WRITE(numout,*) 'CO2 impact on isoprene: Wilkinson calculation'

   !! In the Wilkinson function, 2 impacts are considered:
   !! -short-term impact for CO2 variation during a single day (seconds/minutes)
   !! -long-term impact for CO2 variation during leaf-growth (weeks/month)


   !! Long-term parameters
   !! Constant
   Ismax_long = 1.344
   h_long = 1.4614
   Cstar_long = 585.
   !! Short-term parameters 
   !! They have to be calculated based on atmospheric CO2
   !! 10/05/2010
   !! For atmospheric CO2 lower than 400 ppm or higher than 1200 ppm
   !! (min and max CO2 level tested for short-term effect in Wilkinson et al. 2009)
   !! we use the parameters calculated at 400/1200 ppm. For intermediate CO2 concentration,
   !! parameters are calculated. 
   !! Linear interpolation

   DO ji = 1, kjpindex

      IF (ccanopy(ji) .LE. 400.) THEN

         Ismax_short(ji) = 1.072
         h_short(ji) = 1.7
         Cstar_short(ji) = 1218.

      ELSE IF (ccanopy(ji) .EQ. 600.) THEN

         Ismax_short(ji) = 1.036
         h_short(ji) = 2.0125
         Cstar_short(ji) = 1150.

      ELSE IF (ccanopy(ji) .EQ. 800.) THEN

         Ismax_short(ji) = 1.046
         h_short(ji) = 1.5380
         Cstar_short(ji) = 2025.

      ELSE IF (ccanopy(ji) .GE. 1200.) THEN

         Ismax_short(ji) = 1.014
         h_short(ji) = 2.8610
         Cstar_short(ji) = 1525.


      ELSE IF ((ccanopy(ji) .GT. 400.) .AND. (ccanopy(ji) .LT. 600.)) THEN

         Ismax_short(ji) = 1.036 + (ccanopy(ji)-600.)*(1.036-1.072)/(600.-400.)
         h_short(ji) = 2.0125 + (ccanopy(ji)-600.)*(2.0125-1.7)/(600.-400.)
         Cstar_short(ji) =  1150. + (ccanopy(ji)-600.)*(1150.-1218.)/(600.-400.)

      ELSE IF ((ccanopy(ji) .GT. 600.) .AND. (ccanopy(ji) .LT. 800.)) THEN

         Ismax_short(ji) = 1.046 + (ccanopy(ji)-800.)*(1.046-1.036)/(800.-600.)
         h_short(ji) = 1.5380 + (ccanopy(ji)-800.)*(1.5380-2.0125)/(800.-600.)
         Cstar_short(ji) = 2025. + (ccanopy(ji)-800.)*(2025.-1150.)/(800.-600.)

      ELSE IF ((ccanopy(ji) .GT. 800.) .AND. (ccanopy(ji) .LT. 1200.)) THEN

        Ismax_short(ji) = 1.014 + (ccanopy(ji)-1200.)*(1.014-1.046)/(1200.-800.)
        h_short(ji) = 2.8610 + (ccanopy(ji)-1200.)*(2.8610-1.5380)/(1200.-800.)
        Cstar_short(ji) = 1525. + (ccanopy(ji)-1200.)*(1525.-2025.)/(1200.-800.)


      END IF

   END DO

   WRITE(numout,*) '***Wilkinson BVOC-CO2 function: parameters***'
   WRITE(numout,*) 'Ismax_long: ', Ismax_long
   WRITE(numout,*) 'h_long: ', h_long
   WRITE(numout,*) 'Cstar_long: ', Cstar_long
   WRITE(numout,*) 'Ismax_short: ', MAXVAL(Ismax_short(:)) , MINVAL(Ismax_short(:))
   WRITE(numout,*) 'h_short: ', MAXVAL(h_short(:)) , MINVAL(h_short(:))
   WRITE(numout,*) 'Cstar_short: ', MAXVAL(Cstar_short(:)) , MINVAL(Cstar_short(:))
   WRITE(numout,*) '******'

   DO ji = 1, kjpindex
      fco2_wlong(ji) = (Ismax_long-((Ismax_long*((0.667*ccanopy(ji))**h_long))/&
                     & ((Cstar_long**h_long)+(0.667*ccanopy(ji))**h_long)))/1.06566
      DO jv = 1, nvm
         fco2_wshort(ji,jv) = (Ismax_short(ji)-((Ismax_short(ji)*((cim(ji,jv))**h_short(ji)))/&
                            & ((Cstar_short(ji)**h_short(ji))+(cim(ji,jv))**h_short(ji))))/1.010803
      END DO
   END DO

   DO ji = 1, kjpindex
      DO jv = 1, nvm
         fco2(ji,jv) = fco2_wshort(ji,jv)*fco2_wlong(ji)
      END DO
   END DO

ELSE
      WRITE(numout,*) 'CO2 impact on isoprene not considered'
      fco2(:,:) = 1.
END IF


    !! 3. Calculation of PFT dependant parameters and
    ! Calculation of VOC emissions flux

    Eff_age_iso(:,:) = zero
    Eff_age_meth(:,:) = zero


    DO jv = 1, nvm ! loop over the PDFs
       DO ji = 1, kjpindex ! loop over the grid cell
          ! 6-Calculation of Leaf Age Function (Lathiere 2005)
          IF ( ok_leafage ) THEN
             DO jf = 1, nleafages
                !> @codeinc
                Eff_age_iso(ji,jv) = Eff_age_iso(ji,jv) + frac_age(ji,jv,jf)*iso_activity(jf)
                Eff_age_meth(ji,jv) = Eff_age_meth(ji,jv) + frac_age(ji,jv,jf)*methanol_activity(jf)
                !> @endcodeinc 
             END DO
             !> @codeinc
             Eff_age_VOC(ji,jv) = un
             !> @endcodeinc
          ELSE
             Eff_age_iso(ji,jv) = un
             Eff_age_meth(ji,jv) = un
             Eff_age_VOC(ji,jv) = un
          END IF
          !! 5. Calculation of foliar density
          IF ( sla(jv) .eq. zero ) THEN
             fol_dens(ji,jv) = zero
          ELSE
             ! 2 factor for conversion from gC to gDM
             fol_dens(ji,jv) = 2 * lai(ji,jv)/sla(jv)
          ENDIF
          !! 6. Calculation of VOC emissions from vegetation
          IF ( ok_radcanopy ) THEN
             ! if multi-layer canopy model
             IF (ok_multilayer) THEN 

                laisun(ji,jv) = zero
                laish(ji,jv) = zero
                GAMMA_iso_m  = zero
                flx_iso(ji,jv) = zero
                flx_mono(ji,jv) = zero
                flx_apinen(ji,jv) = zero
                flx_bpinen(ji,jv) = zero
                flx_limonen(ji,jv) = zero
                flx_myrcen(ji,jv) =  zero
                flx_sabinen(ji,jv) =  zero
                flx_camphen(ji,jv) = zero
                flx_3caren(ji,jv) = zero
                flx_tbocimen(ji,jv) = zero
                flx_othermono(ji,jv) = zero
                flx_sesquiter(ji,jv) =  zero
                flx_methanol(ji,jv) = zero
                flx_acetone(ji,jv) =  zero
                flx_acetal(ji,jv) = zero
                flx_formal(ji,jv) = zero
                flx_acetic(ji,jv) = zero
                flx_formic(ji,jv) = zero
                ! loop over the NLAI canopy layers
                DO jl = 1, nlai
                   IF ((laitab(jl) .LE. lai(ji,jv)).AND.(lai(ji,jv).NE.zero)) THEN
                      !sunlit vegetation 
                      Clsun_iso_tab   = alpha_*CL1*PARsuntab(ji,jl)/sqrt(un + (alpha_**2) * (PARsuntab(ji,jl)**2) )
                      ! shaded vegetation
                      Clsh_iso_tab    = alpha_*CL1*PARshtab(ji,jl)/sqrt(un + (alpha_**2) * (PARshtab(ji,jl)**2) ) 
                      flx_iso(ji,jv) = flx_iso(ji,jv) + (laisuntabdep(ji,jl)*Clsun_iso_tab+ &
                           & laishtabdep(ji,jl)*Clsh_iso_tab)* &
                           & fol_dens(ji,jv)/lai(ji,jv)*Ct_iso(ji)*em_factor_isoprene(jv)* &
                           & Eff_age_iso(ji,jv)*fco2(ji,jv)*1e-9/one_hour

                      GAMMA_iso_m = GAMMA_iso_m + (laisuntabdep(ji,jl)*Clsun_iso_tab+ &
                           & laishtabdep(ji,jl)*Clsh_iso_tab)* &
                           & fol_dens(ji,jv)/lai(ji,jv)*Ct_iso(ji)*1e-9/one_hour

                      laisun(ji,jv) = laisun(ji,jv) + laisuntabdep(ji,jl)
                      laish(ji,jv)  = laish(ji,jv) + laishtabdep(ji,jl)
                   END IF
                END DO

                !! 6.1 Calculation of monoterpene biogenic emissions
                flx_mono(ji,jv) = ((1-LDF_mono)*Ct_mono(ji)*1e-9/one_hour*fol_dens(ji,jv) + LDF_mono*GAMMA_iso_m)* &
                     & em_factor_monoterpene(jv)*Eff_age_VOC(ji,jv) 
                !! 6.12 Calculation of sesquiterpenes biogenic emission 
                flx_sesquiter(ji,jv) = ((1-LDF_sesq)*Ct_sesq(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_sesq*GAMMA_iso_m)* &
                     & em_factor_sesquiterp(jv)*Eff_age_VOC(ji,jv)
                !! 6.13 Calculation of methanol biogenic emissions
                flx_methanol(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_meth*GAMMA_iso_m)* &
                     & em_factor_methanol(jv)*Eff_age_meth(ji,jv)
                !! 6.14 Calculation of acetone biogenic emissions
                flx_acetone(ji,jv) = ((1-LDF_acet)*Ct_acet(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_acet*GAMMA_iso_m)* &
                     & em_factor_acetone(jv)*Eff_age_VOC(ji,jv)
                !! 6.14 Calculation of acetaldehyde biogenic emissions
                flx_acetal(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_meth*GAMMA_iso_m)* &
                     & em_factor_acetal(jv)*Eff_age_VOC(ji,jv)
                !! 6.16 Calculation of formaldehyde biogenic emissions
                flx_formal(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_meth*GAMMA_iso_m)* &
                     & em_factor_formal(jv)*Eff_age_VOC(ji,jv)
                !! 6.17 Calculation of acetic acid biogenic emissions
                flx_acetic(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_meth*GAMMA_iso_m)* &
                     & em_factor_acetic(jv)*Eff_age_VOC(ji,jv)
                !! 6.18 Calculation of formic acid biogenic emissions
                flx_formic(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)*1e-9/one_hour*fol_dens(ji,jv) +LDF_meth*GAMMA_iso_m)* &
                     & em_factor_formic(jv)*Eff_age_VOC(ji,jv)


                !! 6.3 Calculation of alfa pinene biogenic emission 
                flx_apinen(ji,jv) = em_factor_apinene(jv)*flx_mono(ji,jv) 
                !! 6.4 Calculation of beta pinene biogenic emission 
                flx_bpinen(ji,jv) = em_factor_bpinene(jv)*flx_mono(ji,jv) 
                !! 6.5 Calculation of limonene biogenic emission 
                flx_limonen(ji,jv) = em_factor_limonene(jv)*flx_mono(ji,jv) 
                !! 6.6 Calculation of myrcene biogenic emission !! 
                flx_myrcen(ji,jv) = em_factor_myrcene(jv)*flx_mono(ji,jv) 
                !! 6.7 Calculation of sabinene biogenic emission
                flx_sabinen(ji,jv) = em_factor_sabinene(jv)*flx_mono(ji,jv) 
                !! 6.8 Calculation of camphene biogenic emission 
                flx_camphen(ji,jv) = em_factor_camphene(jv)*flx_mono(ji,jv) 
                !! 6.9 Calculation of 3-carene biogenic emission 
                flx_3caren(ji,jv) = em_factor_3carene(jv)*flx_mono(ji,jv) 
                !! 6.10 Calculation of t-beta-ocimene biogenic emission
                flx_tbocimen(ji,jv) = em_factor_tbocimene(jv)*flx_mono(ji,jv) 
                !! 6.11 Calculation of other monoterpenes biogenic emission
                flx_othermono(ji,jv) = em_factor_othermonot(jv)*flx_mono(ji,jv) 

                ! if mono-layer canopy model
             ELSE
                !sunlit vegetation 
                Clsun_iso(ji,jv)   = alpha_*CL1*PARsun(ji,jv)/sqrt(un + (alpha_**2) * (PARsun(ji,jv)**2) )
                ! shaded vegetation      
                Clsh_iso(ji,jv)    = alpha_*CL1*PARsh(ji,jv)/sqrt(un + (alpha_**2) * (PARsh(ji,jv)**2) )       
                IF (lai(ji,jv) .NE. zero) THEN
                   !! 6.1 Calculation of isoprene biogenic emissions
                   GAMMA_iso = (laisun(ji,jv)*Clsun_iso(ji,jv) + laish(ji,jv)*Clsh_iso(ji,jv))/lai(ji,jv)*Ct_iso(ji)
                   flx_iso(ji,jv) = GAMMA_iso*fol_dens(ji,jv)*em_factor_isoprene(jv)*Eff_age_iso(ji,jv)*fco2(ji,jv)*1e-9/one_hour
                   !! 6.2 Calculation of monoterpene biogenic emissions
                   flx_mono(ji,jv) = ((1-LDF_mono)*Ct_mono(ji)+LDF_mono*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_monoterpene(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
                   !! 6.3 Calculation of alfa pinene biogenic emission 
                   flx_apinen(ji,jv) = em_factor_apinene(jv)*flx_mono(ji,jv)
                   !! 6.4 Calculation of beta pinene biogenic emission 
                   flx_bpinen(ji,jv) = em_factor_bpinene(jv)*flx_mono(ji,jv)
                   !! 6.5 Calculation of limonene biogenic emission 
                   flx_limonen(ji,jv) = em_factor_limonene(jv)*flx_mono(ji,jv)
                   !! 6.6 Calculation of myrcene biogenic emission 
                   flx_myrcen(ji,jv) = em_factor_myrcene(jv)*flx_mono(ji,jv)
                   !! 6.7 Calculation of sabinene biogenic emission 
                   flx_sabinen(ji,jv) = em_factor_sabinene(jv)*flx_mono(ji,jv)
                   !! 6.8 Calculation of camphene biogenic emission 
                   flx_camphen(ji,jv) = em_factor_camphene(jv)*flx_mono(ji,jv)
                   !! 6.9 Calculation of 3-carene biogenic emission 
                   flx_3caren(ji,jv) = em_factor_3carene(jv)*flx_mono(ji,jv)
                   !! 6.10 Calculation of t-beta-ocimene biogenic emission 
                   flx_tbocimen(ji,jv) = em_factor_tbocimene(jv)*flx_mono(ji,jv)
                   !! 6.11 Calculation of other monoterpenes biogenic emission 
                   flx_othermono(ji,jv) = em_factor_othermonot(jv)*flx_mono(ji,jv)
                   !! 6.12 Calculation of sesquiterpenes biogenic emission 
                   flx_sesquiter(ji,jv) = ((1-LDF_sesq)*Ct_sesq(ji)+LDF_sesq*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_sesquiterp(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
                   !! 6.13 Calculation of methanol biogenic emissions
                   flx_methanol(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)+LDF_meth*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_methanol(jv)*Eff_age_meth(ji,jv)*1e-9/one_hour
                   !! 6.14 Calculation of acetone biogenic emissions
                   flx_acetone(ji,jv) = ((1-LDF_acet)*Ct_acet(ji)+LDF_acet*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_acetone(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
                   !! 6.15 Calculation of acetaldehyde biogenic emissions
                   flx_acetal(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)+LDF_meth*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_acetal(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
                   !! 6.16 Calculation of formaldehyde biogenic emissions
                   flx_formal(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)+LDF_meth*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_formal(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
                   !! 6.17 Calculation of acetic acid biogenic emissions
                   flx_acetic(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)+LDF_meth*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_acetic(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
                   !! 6.18 Calculation of formic acid biogenic emissions
                   flx_formic(ji,jv) = ((1-LDF_meth)*Ct_meth(ji)+LDF_meth*GAMMA_iso)*fol_dens(ji,jv)* &
                        & em_factor_formic(jv)*Eff_age_VOC(ji,jv)*1e-9/one_hour
 
                ELSE
                   ! 
                   flx_iso(ji,jv) = zero
                   flx_mono(ji,jv) = zero
                   flx_apinen(ji,jv) = zero 
                   flx_bpinen(ji,jv) = zero 
                   flx_limonen(ji,jv) = zero 
                   flx_myrcen(ji,jv) =  zero
                   flx_sabinen(ji,jv) =  zero 
                   flx_camphen(ji,jv) = zero 
                   flx_3caren(ji,jv) = zero 
                   flx_tbocimen(ji,jv) = zero
                   flx_othermono(ji,jv) = zero 
                   flx_sesquiter(ji,jv) =  zero 
                   flx_methanol(ji,jv) = zero
                   flx_acetone(ji,jv) =  zero 
                   flx_acetal(ji,jv) = zero
                   flx_formal(ji,jv) = zero 
                   flx_acetic(ji,jv) = zero 
                   flx_formic(ji,jv) = zero 
                END IF
             END IF
             ! if no light extinction due to vegetation  
          ELSE
             !! Isoprene emissions - general equation
             flx_iso(ji,jv) = fol_dens(ji,jv)*Ct_iso(ji)*Cl_iso(ji)*Eff_age_iso(ji,jv)*fco2(ji,jv)* &
                  em_factor_isoprene(jv)*1e-9/one_hour
             !! 6.2 Calculation of monoterpene biogenic emissions
             Ylt_mono(ji) = ((1-LDF_mono)*Ct_mono(ji)+LDF_mono*Ct_iso(ji)*Cl_iso(ji)) 
             flx_mono(ji,jv) = fol_dens(ji,jv)*em_factor_monoterpene(jv)*Ylt_mono(ji)*Eff_age_VOC(ji,jv)*&
                  1e-9/one_hour
             !! 6.3 Calculation of alfa pinene biogenic emission 
             flx_apinen(ji,jv) = em_factor_apinene(jv)*flx_mono(ji,jv)  
             !! 6.4 Calculation of beta pinene biogenic emission 
             flx_bpinen(ji,jv) = em_factor_bpinene(jv)*flx_mono(ji,jv)                       
             !! 6.5 Calculation of limonene biogenic emission 
             flx_limonen(ji,jv) = em_factor_limonene(jv)*flx_mono(ji,jv)                      
             !! 6.6 Calculation of myrcene biogenic emission 
             flx_myrcen(ji,jv) = em_factor_myrcene(jv)*flx_mono(ji,jv)                       
             !! 6.7 Calculation of sabinene biogenic emission 
             flx_sabinen(ji,jv) = em_factor_sabinene(jv)*flx_mono(ji,jv)           
             !! 6.8 Calculation of camphene biogenic emission 
             flx_camphen(ji,jv) = em_factor_camphene(jv)*flx_mono(ji,jv)
             !! 6.9 Calculation of 3-carene biogenic emission 
             flx_3caren(ji,jv) = em_factor_3carene(jv)*flx_mono(ji,jv)                       
             !! 6.10 Calculation of t-beta-ocimene biogenic emission 
             flx_tbocimen(ji,jv) = em_factor_tbocimene(jv)*flx_mono(ji,jv)                     
             !! 6.11 Calculation of other monoterpenes biogenic emission 
             flx_othermono(ji,jv) = em_factor_othermonot(jv)*flx_mono(ji,jv)                    
             !! 6.12 Calculation of sesquiterpenes biogenic emission 
             Ylt_sesq(ji) = ((1-LDF_sesq)*Ct_sesq(ji)+LDF_sesq*Ct_iso(ji)*Cl_iso(ji))
             flx_sesquiter(ji,jv) = fol_dens(ji,jv)*em_factor_sesquiterp(jv)*Ylt_sesq(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour    
             !! 6.16 Calculation of methanol biogenic emissions
             Ylt_meth(ji) = ((1-LDF_meth)*Ct_meth(ji)+LDF_meth*Ct_iso(ji)*Cl_iso(ji))
             flx_methanol(ji,jv) = fol_dens(ji,jv)*em_factor_methanol(jv)*Ylt_meth(ji)*Eff_age_meth(ji,jv)*1e-9/one_hour
             !! 6.17 Calculation of acetone biogenic emissions
             Ylt_acet(ji) = ((1-LDF_acet)*Ct_acet(ji)+LDF_acet*Ct_iso(ji)*Cl_iso(ji))
             flx_acetone(ji,jv) = fol_dens(ji,jv)*em_factor_acetone(jv)*Ylt_acet(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
             !! 6.18 Calculation of acetaldehyde biogenic emissions
             flx_acetal(ji,jv) = fol_dens(ji,jv)*em_factor_acetal(jv)*Ylt_meth(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
             !! 6.19 Calculation of formaldehyde biogenic emissions
             flx_formal(ji,jv) = fol_dens(ji,jv)*em_factor_formal(jv)*Ylt_meth(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
             !! 6.20 Calculation of acetic acid biogenic emissions
             flx_acetic(ji,jv) = fol_dens(ji,jv)*em_factor_acetic(jv)*Ylt_meth(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
             !! 6.21 Calculation of formic acid biogenic emissions
             flx_formic(ji,jv) = fol_dens(ji,jv)*em_factor_formic(jv)*Ylt_meth(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour

          END IF

          !! 6.22 Calculation of ORVOC biogenic emissions
          !! Other Reactive Volatile Organic Compounds
          !> @codeinc
          flx_ORVOC(ji,jv) = fol_dens(ji,jv)*em_factor_ORVOC(jv)*Ct_mono(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
          !> @endcodeinc
          !! 6.4 Calculation of OVOC biogenic emissions
          !! Other Volatile Organic Compounds
          flx_OVOC(ji,jv) = fol_dens(ji,jv)*em_factor_OVOC(jv)*Ct_mono(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
          !! 6.5 Calculation of MBO biogenic emissions
          !! 2-Methyl-3-Buten-2-ol 
          IF(lalo(ji,1) .GE. 20. .AND. lalo(ji,2) .LE. -100) THEN
             flx_MBO(ji,jv) = fol_dens(ji,jv)*em_factor_MBO(jv)*Ct_MBO(ji)*Cl_MBO(ji)*Eff_age_VOC(ji,jv)*1e-9/one_hour
          ELSE
             flx_MBO(ji,jv) = zero
          END IF
       END DO

    END DO


    !! 7. Calculation of NOx emissions from soils
    ! Based on Yienger & Levy (1995) and Lathiere (2005, chapter 3)
    DO ji = 1, kjpindex
       !! 7.1 Precipitation-related pulse function
       IF (ok_pulse_NOx) THEN
          ! if we are during a period where pulses are not allowed
          IF (ok_siesta(ji)) THEN
             ! if this period is not over 
             IF (FLOOR(siestaday(ji)) .LE. siestalim(ji)) THEN
                siestaday(ji) = siestaday(ji) + (dt_sechiba/one_day)
                ! if this period is over
             ELSE
                ok_siesta(ji) = .FALSE.
                siestaday(ji) = zero
             END IF
          END IF
          ! if we are during a period where pulses are allowed
          IF ((.NOT. ok_siesta(ji)) .AND. (.NOT. allow_pulse(ji))) THEN
             IF (humrel(ji,1) .LT. 0.15) THEN
                ! if precip exceeds 1 mm/day over one time step => a pulse occurs
                IF(precip_rain(ji)/nbre_precip .GE. un/(one_day/dt_sechiba)) THEN
                   ! if precip is up to 5 mm/day => pulse length is 3 days
                   IF (precip_rain(ji)/nbre_precip .LT. 5./(one_day/dt_sechiba)) THEN
                      pulselim(ji) = 3.
                      ! if precip is up to 15 mm/day => pulse length is 7 days
                   ELSE IF (precip_rain(ji)/nbre_precip .LT. 15./(one_day/dt_sechiba)) THEN
                      pulselim(ji) = 7.
                      ! if precip is upper than 15 mm/day => pulse length is 14 days
                   ELSE IF (precip_rain(ji)/nbre_precip .GE. 15./(one_day/dt_sechiba)) THEN
                      pulselim(ji) = 14.
                   END IF
                   allow_pulse(ji)=.TRUE.
                   pulseday(ji) = un
                END IF
             END IF
          END IF
          ! if we were during a pulse period
          IF (allow_pulse(ji)) THEN
             ! if we are still during the pulse period
             ! 16/06/2010 We assume a (pulselim-1) days for the pulse length (NVui+Jlath)
             IF(FLOOR(pulseday(ji)) .LT. pulselim(ji)) THEN
                ! calculation of the pulse function
                IF (pulselim(ji).EQ.3) THEN
                   pulse(ji) = 11.19*exp(-0.805*pulseday(ji))
                ELSE IF (pulselim(ji).EQ.7) THEN
                   pulse(ji) = 14.68*exp(-0.384*pulseday(ji))
                ELSE IF (pulselim(ji).EQ.14) THEN 
                   pulse(ji) = 18.46*exp(-0.208*pulseday(ji))
                END IF
                pulseday(ji) = pulseday(ji) + (dt_sechiba/one_day)
                ! if the pulse period is over
             ELSE
                ! pulse function is set to 1 
                pulse(ji) = un
                allow_pulse(ji) = .FALSE.
                siestaday(ji) = un
                siestalim(ji) = pulselim(ji)
                ok_siesta(ji) = .TRUE. 
             END IF
          END IF
          ! no precipitation-related pulse function
       ELSE
          pulse(ji) = un
       END IF
    END DO

    !! 7.2 Calculation of NO basal emissions including pulse effect
    DO jv = 1, nvm
       DO ji = 1, kjpindex
          !Tropical forests
          IF ( is_tropical(jv) .AND. is_evergreen(jv) ) THEN
             ! Wet soils
             IF (humrel(ji,1) .GT. 0.3) THEN
                flx_no_soil(ji,jv) = 2.6*pulse(ji)
                ! Dry soils
             ELSE
                flx_no_soil(ji,jv) = 8.6*pulse(ji)
             END IF
             !Else If agricultural lands OR Wet soils
          ELSE IF ( ( .NOT.(natural(jv)) ) .OR. ( humrel(ji,1) .GT. 0.3 ) ) THEN
             ! Calculation of NO emissions depending of Temperature
             IF (t_no(ji) .LT. zero) THEN
                flx_no_soil(ji,jv) = zero
             ELSE IF (t_no(ji) .LE. 10.) THEN
                flx_no_soil(ji,jv) = 0.28*em_factor_no_wet(jv)*t_no(ji)*pulse(ji)
             ELSE IF (t_no(ji) .LE. 30.) THEN
                flx_no_soil(ji,jv) = em_factor_no_wet(jv)*exp(0.103*t_no(ji))*pulse(ji)
             ELSE
                flx_no_soil(ji,jv) = 21.97*em_factor_no_wet(jv)*pulse(ji)
             END IF
             !Else if Temp negative
          ELSE IF (t_no(ji) .LT. zero) THEN
             flx_no_soil(ji,jv) = zero
             !Else if Temp <= 30
          ELSE IF (t_no(ji) .LE. 30.) THEN
             flx_no_soil(ji,jv) = (em_factor_no_dry(jv)*t_no(ji))/30.*pulse(ji)
          ELSE
             flx_no_soil(ji,jv) = em_factor_no_dry(jv)*pulse(ji)
          END IF

          !! 7.3 IF ACTIVATED (ok_bbgfertil_NOx = TRUE) calculation of NOx soil emission increase due to biomass burning
          ! Calculation of Biomass-Burning-induced NOx emissions (Lathiere, 2005)
          ! => NOx emissions 3-fold increase
          IF (ok_bbgfertil_NOx) THEN
             IF ( natural(jv) ) THEN
                ! North Tropical zone from May to June
                IF ((lalo(ji,1) .LE. 30. .AND. lalo(ji,1) .GE. zero).AND. &
                     (day .GE. 121. .AND. day .LE. 181).AND.(flx_co2_bbg_year(ji) .GT. 0.1)) THEN
                   flx_no_soil(ji,jv) = flx_no_soil(ji,jv)*3.
                   ! South Tropical zone from November to December
                ELSE IF ((lalo(ji,1) .GE. -30. .AND. lalo(ji,1) .LT. zero).AND.(day .GE. 305.).AND. & 
                        (flx_co2_bbg_year(ji) .GT. 0.1)) THEN
                   flx_no_soil(ji,jv) = flx_no_soil(ji,jv)*3.
                END IF
             END IF
          END IF

          !! 7.4 IF ACTIVATED (ok_cropsfertil_NOx = TRUE) calculation of NOx soil emission increase due to fertilizer use 
          ! Calculation of N-fertiliser-induced NOx emissions
          flx_fertil_no(ji,jv) = zero
          IF (ok_cropsfertil_NOx) THEN
             IF (veget_max_nowoody(ji) .NE. zero) THEN
                ! Non-agricultural lands
                IF ( (jv == ibare_sechiba) .OR. is_tree(jv) ) THEN
                   N_qt_WRICE_pft(ji,jv) = zero
                   N_qt_OTHER_pft(ji,jv) = zero
                ! Grasslands or Croplands
                ELSE
                   N_qt_WRICE_pft(ji,jv) = N_qt_WRICE_year(ji)*veget_max(ji,jv)/veget_max_nowoody(ji)
                   N_qt_OTHER_pft(ji,jv) = N_qt_OTHER_year(ji)*veget_max(ji,jv)/veget_max_nowoody(ji)
                END IF
             ELSE
                N_qt_WRICE_pft(ji,jv) = zero
                N_qt_OTHER_pft(ji,jv) = zero
             END IF

             ! North temperate regions from May to August
             ! OR South Temperate regions from November to February
             IF (((lalo(ji,1) .GT. 30.) .AND. (day .GE. 121. .AND. day .LE. 243.).AND.(veget_max(ji,jv) .NE. zero)) .OR. & 
             &  ((lalo(ji,1) .LT. -30.) .AND. (day .GE. 305. .OR. day .LE. 59.) .AND.(veget_max(ji,jv) .NE. zero))) THEN
                ! 1e12 for conversion from kg to Ng
                ! 1/(365/12*24*60*60*4) for conversion from year to seconds corrected for 4 months of emissions
                flx_fertil_no(ji,jv) = (N_qt_WRICE_pft(ji,jv)*(1/30.)+N_qt_OTHER_pft(ji,jv))*(2.5/100.) &
                     & *1e12/(365*24*60*60*4/12)/(area2(ji)*veget_max(ji,jv))
                ! OR Tropical regions all the year
             ELSE IF ((lalo(ji,1) .GE. -30.).AND.(lalo(ji,1) .LE. 30.).AND.(veget_max(ji,jv) .NE. zero)) THEN
                flx_fertil_no(ji,jv) = (N_qt_WRICE_pft(ji,jv)*(1/30.)+N_qt_OTHER_pft(ji,jv))*(2.5/100.) &
                     & *1e12/(365*24*60*60)/(area2(ji)*veget_max(ji,jv))
             END IF
             flx_no_soil(ji,jv) = flx_no_soil(ji,jv) + flx_fertil_no(ji,jv)
          END IF

          !! 7.5 Calculation of net NO flux above soil accounting for surface deposition, 
          !! based on the Canopy Reduction Factor (CRF), calculated using Leaf Area and Stomatal Area
          !kc=cuticle absorptivity = 0.24m^2/m^2
          !ks=stomatal absorptivity = 8.75m^2/m^2
          !Larch=Larcher SAI/LAI ratio given in Larcher 1991
          !> @codeinc
          CRF(ji,jv) = (exp(-8.75*Larch(jv)*lai(ji,jv)) + exp(-0.24*lai(ji,jv)))/2.
          flx_no(ji,jv) = flx_no_soil(ji,jv)*CRF(ji,jv)
          !> @endcodeinc
       END DO
    END DO


    ! Write output with XIOS
    CALL xios_orchidee_send_field("PAR",PAR)
    CALL xios_orchidee_send_field("ptnlev1",ptnlev1)
    CALL xios_orchidee_send_field("flx_fertil_no",flx_fertil_no)
    CALL xios_orchidee_send_field("flx_iso",flx_iso)
    CALL xios_orchidee_send_field("flx_mono",flx_mono)
    CALL xios_orchidee_send_field("flx_ORVOC",flx_ORVOC)
    CALL xios_orchidee_send_field("flx_MBO",flx_MBO)
    CALL xios_orchidee_send_field("flx_methanol",flx_methanol)
    CALL xios_orchidee_send_field("flx_acetone",flx_acetone)
    CALL xios_orchidee_send_field("flx_acetal",flx_acetal)
    CALL xios_orchidee_send_field("flx_formal",flx_formal)
    CALL xios_orchidee_send_field("flx_acetic",flx_acetic)
    CALL xios_orchidee_send_field("flx_formic",flx_formic)
    CALL xios_orchidee_send_field("flx_no_soil",flx_no_soil)
    CALL xios_orchidee_send_field("flx_no",flx_no)
    CALL xios_orchidee_send_field('flx_apinen'   , flx_apinen)
    CALL xios_orchidee_send_field('flx_bpinen'   , flx_bpinen)
    CALL xios_orchidee_send_field('flx_limonen'  ,flx_limonen)
    CALL xios_orchidee_send_field('flx_myrcen'   , flx_myrcen)
    CALL xios_orchidee_send_field('flx_sabinen'  ,flx_sabinen)
    CALL xios_orchidee_send_field('flx_camphen'  ,flx_camphen)
    CALL xios_orchidee_send_field('flx_3caren'   , flx_3caren)
    CALL xios_orchidee_send_field('flx_tbocimen' ,flx_tbocimen)
    CALL xios_orchidee_send_field('flx_othermono',flx_othermono)
    CALL xios_orchidee_send_field('flx_sesquiter',flx_sesquiter)
    CALL xios_orchidee_send_field("CRF",CRF)
    CALL xios_orchidee_send_field('fco2', fco2)

    IF ( ok_radcanopy ) THEN
       CALL xios_orchidee_send_field("Fdf",Fdf)
       CALL xios_orchidee_send_field("PARdf",PARdf)
       CALL xios_orchidee_send_field("PARdr",PARdr)
       CALL xios_orchidee_send_field("Trans",Trans)
       
       IF (ok_multilayer) THEN
          CALL xios_orchidee_send_field("PARsuntab",PARsuntab)
          CALL xios_orchidee_send_field("PARshtab",PARshtab)
       ELSE
          CALL xios_orchidee_send_field("PARsun",PARsun)
          CALL xios_orchidee_send_field("PARsh",PARsh)
          CALL xios_orchidee_send_field("laisun",laisun)
          CALL xios_orchidee_send_field("laish",laish)
       ENDIF
    ENDIF

    IF ( ok_bbgfertil_Nox ) THEN
       CALL xios_orchidee_send_field("flx_co2_bbg_year",flx_co2_bbg_year)
    END IF

    IF ( ok_cropsfertil_Nox ) THEN
       CALL xios_orchidee_send_field("N_qt_WRICE_year",N_qt_WRICE_year)
       CALL xios_orchidee_send_field("N_qt_OTHER_year",N_qt_OTHER_year)
    END IF
    

    ! Write output with IOIPSL
    IF ( .NOT. almaoutput ) THEN

       CALL histwrite_p(hist_id, 'PAR', kjit, PAR, kjpindex, index)
       IF ( ok_radcanopy ) THEN
          CALL histwrite_p(hist_id, 'laisun', kjit, laisun, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist_id, 'laish', kjit, laish, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist_id, 'Fdf', kjit, Fdf, kjpindex, index)
          IF (ok_multilayer) THEN 
             CALL histwrite_p(hist_id, 'PARsuntab', kjit, PARsuntab, kjpindex*(nlai+1), indexlai)
             CALL histwrite_p(hist_id, 'PARshtab', kjit, PARshtab, kjpindex*(nlai+1), indexlai)
          ELSE
             CALL histwrite_p(hist_id, 'PARsun', kjit, PARsun, kjpindex*nvm, indexveg)
             CALL histwrite_p(hist_id, 'PARsh', kjit, PARsh, kjpindex*nvm, indexveg)
          END IF
          CALL histwrite_p(hist_id, 'coszang', kjit, coszang, kjpindex, index)
          CALL histwrite_p(hist_id, 'PARdf', kjit, PARdf, kjpindex, index)
          CALL histwrite_p(hist_id, 'PARdr', kjit, PARdr, kjpindex, index)
          CALL histwrite_p(hist_id, 'Trans', kjit, Trans, kjpindex, index)
       END IF
       CALL histwrite_p(hist_id, 'flx_fertil_no', kjit, flx_fertil_no, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'CRF', kjit, CRF, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'fco2', kjit, fco2, kjpindex*nvm, indexveg)

       IF ( ok_bbgfertil_Nox ) THEN
          CALL histwrite_p(hist_id, 'flx_co2_bbg_year', 1, flx_co2_bbg_year, kjpindex, index)
       ENDIF
       IF ( ok_cropsfertil_Nox ) THEN
          CALL histwrite_p(hist_id, 'N_qt_WRICE_year', 1, N_qt_WRICE_year, kjpindex, index)
          CALL histwrite_p(hist_id, 'N_qt_OTHER_year', 1, N_qt_OTHER_year, kjpindex, index)
       ENDIF
       CALL histwrite_p(hist_id, 'ptnlev1', kjit, ptnlev1, kjpindex, index)
       CALL histwrite_p(hist_id, 'flx_iso', kjit, flx_iso, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_mono', kjit, flx_mono, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_apinen', kjit, flx_apinen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_bpinen', kjit, flx_bpinen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_limonen', kjit, flx_limonen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_myrcen', kjit, flx_myrcen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_sabinen', kjit, flx_sabinen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_camphen', kjit, flx_camphen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_3caren', kjit, flx_3caren, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_tbocimen', kjit, flx_tbocimen, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_othermono', kjit, flx_othermono, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_sesquiter', kjit, flx_sesquiter, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_ORVOC', kjit, flx_ORVOC, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_MBO', kjit, flx_MBO, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_methanol', kjit, flx_methanol, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_acetone', kjit, flx_acetone, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_acetal', kjit, flx_acetal, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_formal', kjit, flx_formal, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_acetic', kjit, flx_acetic, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_formic', kjit, flx_formic, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_no_soil', kjit, flx_no_soil, kjpindex*nvm, indexveg)
       CALL histwrite_p(hist_id, 'flx_no', kjit, flx_no, kjpindex*nvm, indexveg)
       
       IF ( hist2_id > 0 ) THEN
          CALL histwrite_p(hist2_id, 'PAR', kjit, PAR, kjpindex, index)
          IF ( ok_radcanopy ) THEN
             CALL histwrite_p(hist2_id, 'PARsun', kjit, PARsun, kjpindex*nvm, indexveg)
             CALL histwrite_p(hist2_id, 'PARsh', kjit, PARsh, kjpindex*nvm, indexveg)
             CALL histwrite_p(hist2_id, 'laisun', kjit, laisun, kjpindex*nvm, indexveg)
             CALL histwrite_p(hist2_id, 'laish', kjit, laish, kjpindex*nvm, indexveg)
          ENDIF
          CALL histwrite_p(hist2_id, 'flx_fertil_no', kjit, flx_fertil_no, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'CRF', kjit, CRF, kjpindex*nvm, indexveg)
          IF ( ok_bbgfertil_Nox ) THEN
             CALL histwrite_p(hist2_id, 'flx_co2_bbg_year', 1, flx_co2_bbg_year, kjpindex, index)
          ENDIF
          IF ( ok_cropsfertil_Nox ) THEN
             CALL histwrite_p(hist2_id, 'N_qt_WRICE_year', 1, N_qt_WRICE_year, kjpindex, index)
             CALL histwrite_p(hist2_id, 'N_qt_OTHER_year', 1, N_qt_OTHER_year, kjpindex, index)
          ENDIF
          CALL histwrite_p(hist2_id, 'ptnlev1', kjit, ptnlev1, kjpindex, index)
          CALL histwrite_p(hist2_id, 'flx_iso', kjit, flx_iso, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_mono', kjit, flx_mono, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_apinen', kjit, flx_apinen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_bpinen', kjit, flx_bpinen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_limonen', kjit, flx_limonen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_myrcen', kjit, flx_myrcen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_sabinen', kjit, flx_sabinen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_camphen', kjit, flx_camphen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_3caren', kjit, flx_3caren, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_tbocimen', kjit, flx_tbocimen, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_othermono', kjit, flx_othermono, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_sesquiter', kjit, flx_sesquiter, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_ORVOC', kjit, flx_ORVOC, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_MBO', kjit, flx_MBO, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_methanol', kjit, flx_methanol, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_acetone', kjit, flx_acetone, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_acetal', kjit, flx_acetal, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_formal', kjit, flx_formal, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_acetic', kjit, flx_acetic, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_formic', kjit, flx_formic, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_no_soil', kjit, flx_no_soil, kjpindex*nvm, indexveg)
          CALL histwrite_p(hist2_id, 'flx_no', kjit, flx_no, kjpindex*nvm, indexveg)
       ENDIF
    ENDIF

    IF (printlev>=3) WRITE(numout,*) 'OK chemistry_bvoc'


  END SUBROUTINE chemistry_bvoc

!! ================================================================================================================================
!! SUBROUTINE   : chemistry_read
!!
!>\BRIEF         This subroutine will read and interpolate the data set of CO2 emissions from biomass burning.\n
!!
!! DESCRIPTION  :  This subroutine will interpolate the 1 x 1 deg based data set of CO2 emissions from biomass burning
!!                 expresssed in kgC/m^2 per year (=> density = TRUE), N fertiliser amount expressed in kgN per year and
!!                 per grid cell (=> density = FALSE) to the resolution of the model.\ n
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): :: data_year
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!_ ================================================================================================================================
  SUBROUTINE chemistry_read_p (nbpt,     lalo,      neighbours, resolution, &
                               filename, fieldname, density,    data_year)

    USE mod_orchidee_para
    IMPLICIT NONE

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)       :: nbpt                !! Number of points for which the data needs to be interpolated
    REAL(r_std), INTENT(in)          :: lalo(nbpt,2)        !! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)       :: neighbours(nbpt,8)  !! Vector of neighbours for each grid point (1=N, 2=E, 3=S, 4=W)
    REAL(r_std), INTENT(in)          :: resolution(nbpt,2)  !! The size in m of each grid-box in X and Y 
    CHARACTER(LEN=*), INTENT(in)     :: filename
    CHARACTER(LEN=*), INTENT(in)     :: fieldname
    LOGICAL, INTENT(in)              :: density

    !! 0.2 Output variables

    REAL(r_std), INTENT(out), dimension(nbpt) :: data_year  !! data per year (climatology) per unit area 

    !! 0.3 Modified variables


    REAL(r_std), dimension(nbp_glo) :: data_year_g


    CALL gather(lalo,lalo_g)
    call gather(neighbours, neighbours_g) 
    call gather(resolution,resolution_g) 

    IF (is_root_prc) CALL chemistry_read (&
         nbp_glo, lalo_g, neighbours_g, resolution_g, &
         filename, fieldname, density, data_year_g)
    CALL scatter(data_year_g, data_year) 


  END SUBROUTINE chemistry_read_p


  SUBROUTINE chemistry_read (nbpt, lalo, neighbours, resolution, filename, fieldname, density, data_year)

    IMPLICIT NONE

    !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)       :: nbpt                !! Number of points for which the data needs to be interpolated
    REAL(r_std), INTENT(in)          :: lalo(nbpt,2)        !! Vector of latitude and longitudes (beware of the order !)
    INTEGER(i_std), INTENT(in)       :: neighbours(nbpt,8)  !! Vector of neighbours for each grid point (1=N, 2=E, 3=S, 4=W)
    REAL(r_std), INTENT(in)          :: resolution(nbpt,2)  !! The size in m of each grid-box in X and Y 
    CHARACTER(LEN=*), INTENT(in)     :: filename
    CHARACTER(LEN=*), INTENT(in)     :: fieldname
    LOGICAL, INTENT(in)              :: density

    !! 0.2 Output variables

    REAL(r_std), INTENT(out), DIMENSION(nbpt) :: data_year  !! data per year (climatology) per unit area 



    !! 0.4 Local variables 

    INTEGER(i_std) :: iml, jml, lml, tml, fid, ib, ip, jp, ilf, lastjp, nbexp
    REAL(r_std) :: lev(1), date, dt, coslat
    REAL(r_std) :: lon_up, lon_low, lat_up, lat_low
    INTEGER(i_std) :: itau(1)
    INTEGER(i_std) :: ier
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:) :: lat_rel, lon_rel, lat_ful, lon_ful
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:) :: data_year_file
    REAL(r_std), ALLOCATABLE, DIMENSION(:,:) :: loup_rel, lolow_rel, laup_rel, lalow_rel
    REAL(r_std) :: area2                                     !! total area of the final grid box 
    REAL(r_std) :: data_grid                                 !! emissions for the final grid point before being 
                                                             !! ponderated by area2
    REAL(r_std) :: data_global                               !! global emissions to check
    REAL(r_std) :: ax, ay, sgn, surp, ax_file, ay_file
    REAL(r_std) :: lonrel, louprel, lolowrel

!_ ================================================================================================================================

    !
    !
    CALL flininfo(filename,iml, jml, lml, tml, fid)
    !
    !
    ALLOCATE (lat_rel(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable lat_rel','','')
    ALLOCATE (lon_rel(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable lon_rel','','')
    ALLOCATE (laup_rel(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable laup_rel','','')
    ALLOCATE (loup_rel(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable loup_rel','','')
    ALLOCATE (lalow_rel(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable lalow_rel','','')
    ALLOCATE (lolow_rel(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable lolow_rel','','')
    ALLOCATE (lat_ful(iml+2,jml+2), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable lat_ful','','')
    ALLOCATE (lon_ful(iml+2,jml+2), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable lon_ful','','')
    ALLOCATE (data_year_file(iml,jml), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'chemistry_read','Problem in allocate of variable data_year_file','','')
    !
    CALL flinopen(filename, .FALSE., iml, jml, lml, lon_rel, lat_rel, lev, tml, itau, date, dt, fid)
    !
    CALL flinget(fid, trim(fieldname), iml, jml, lml, tml, 1, 1, data_year_file)
    !
    CALL flinclo(fid)
    !
    nbexp = 0
    !
    !    Duplicate the border assuming we have a global grid going from west to east
    !
    lon_ful(2:iml+1,2:jml+1) = lon_rel(1:iml,1:jml)
    lat_ful(2:iml+1,2:jml+1) = lat_rel(1:iml,1:jml)
    !
    IF ( lon_rel(iml,1) .LT. lon_ful(2,2)) THEN
       lon_ful(1,2:jml+1) = lon_rel(iml,1:jml)
       lat_ful(1,2:jml+1) = lat_rel(iml,1:jml)
    ELSE
       lon_ful(1,2:jml+1) = lon_rel(iml,1:jml)-360
       lat_ful(1,2:jml+1) = lat_rel(iml,1:jml)
    ENDIF

    IF ( lon_rel(1,1) .GT. lon_ful(iml+1,2)) THEN
       lon_ful(iml+2,2:jml+1) = lon_rel(1,1:jml)
       lat_ful(iml+2,2:jml+1) = lat_rel(1,1:jml)
    ELSE
       lon_ful(iml+2,2:jml+1) = lon_rel(1,1:jml)+360
       lat_ful(iml+2,2:jml+1) = lat_rel(1,1:jml)
    ENDIF
    !
    sgn = lat_rel(1,1)/ABS(lat_rel(1,1))
    lat_ful(2:iml+1,1) = sgn*180 - lat_rel(1:iml,1)
    sgn = lat_rel(1,jml)/ABS(lat_rel(1,jml))
    lat_ful(2:iml+1,jml+2) = sgn*180 - lat_rel(1:iml,jml)
    lat_ful(1,1) = lat_ful(iml+1,1)
    lat_ful(iml+2,1) = lat_ful(2,1)
    lat_ful(1,jml+2) = lat_ful(iml+1,jml+2)
    lat_ful(iml+2,jml+2) = lat_ful(2,jml+2)
    !
    ! Add the longitude lines to the top and bottom
    !
    lon_ful(:,1) = lon_ful(:,2)
    lon_ful(:,jml+2) = lon_ful(:,jml+1)
    !
    !  Get the upper and lower limits of each grid box
    !
    DO ip = 1,iml
       DO jp = 1,jml
          loup_rel(ip,jp) =MAX(0.5*(lon_ful(ip,jp+1)+lon_ful(ip+1,jp+1)), 0.5*(lon_ful(ip+1,jp+1)+lon_ful(ip+2,jp+1)))
          lolow_rel(ip,jp) =MIN(0.5*(lon_ful(ip,jp+1)+lon_ful(ip+1,jp+1)), 0.5*(lon_ful(ip+1,jp+1)+lon_ful(ip+2,jp+1)))
          laup_rel(ip,jp) =MAX(0.5*(lat_ful(ip+1,jp)+lat_ful(ip+1,jp+1)), 0.5*(lat_ful(ip+1,jp+1)+lat_ful(ip+1,jp+2)))
          lalow_rel(ip,jp) =MIN(0.5*(lat_ful(ip+1,jp)+lat_ful(ip+1,jp+1)), 0.5*(lat_ful(ip+1,jp+1)+lat_ful(ip+1,jp+2)))
       ENDDO
    ENDDO
    !
    !   Now we take each grid point and find out which values from the forcing we need to average
    !
    DO ib =1, nbpt
       !
       !  We find the 4 limits of the grid-box. As we transform the resolution of the model
       !  into longitudes and latitudes we do not have the problem of periodicity.
       ! coslat is a help variable here !
       !
       coslat = MAX(COS(lalo(ib,1) * pi/180. ), mincos )*pi/180. * R_Earth
       !
       lon_up = lalo(ib,2)+ resolution(ib,1)/(2.0*coslat)
       lon_low =lalo(ib,2) - resolution(ib,1)/(2.0*coslat)
       !
       coslat = pi/180. * R_Earth
       !
       lat_up =lalo(ib,1)+resolution(ib,2)/(2.0*coslat)
       lat_low =lalo(ib,1)-resolution(ib,2)/(2.0*coslat)
       !
       !
       !  Find the grid boxes from the data that go into the model's boxes
       !  We still work as if we had a regular grid ! Well it needs to be localy regular so
       !  so that the longitude at the latitude of the last found point is close to the one of the next point.
       !
       lastjp = 1
       data_grid = zero
       area2 = zero

       DO ip = 1,iml
          !
          !  Either the center of the data grid point is in the interval of the model grid or
          !  the East and West limits of the data grid point are on either sides of the border of
          !  the data grid.
          !
          !
          !  We find the 4 limits of the grid-box. As we transform the resolution of the model
          !  into longitudes and latitudes we do not have the problem of periodicity.
          ! coslat is a help variable here !
          !
          !
          !  To do that correctly we have to check if the grid box sits on the date-line.
          !
          IF ( lon_low < -180.0 ) THEN
             lonrel = MOD( lon_rel(ip,lastjp) - 360.0, 360.0)
             lolowrel = MOD( lolow_rel(ip,lastjp) - 360.0, 360.0)
             louprel = MOD( loup_rel(ip,lastjp) - 360.0, 360.0)
             !
          ELSE IF ( lon_up > 180.0 ) THEN
             lonrel = MOD( 360. - lon_rel(ip,lastjp), 360.0)
             lolowrel = MOD( 360. - lolow_rel(ip,lastjp), 360.0)
             louprel = MOD( 360. - loup_rel(ip,lastjp), 360.0)
          ELSE
             lonrel = lon_rel(ip,lastjp)
             lolowrel = lolow_rel(ip,lastjp)
             louprel = loup_rel(ip,lastjp)
          ENDIF
          !
          !
          !
          IF ( lonrel > lon_low .AND. lonrel < lon_up .OR. &
               & lolowrel < lon_low .AND.  louprel > lon_low .OR. &
               & lolowrel < lon_up  .AND.  louprel > lon_up ) THEN
             ! 
             DO jp = 1, jml
                !
                ! Now that we have the longitude let us find the latitude
                !
                IF ( lat_rel(ip,jp) > lat_low .AND. lat_rel(ip,jp) < lat_up .OR. &
                     & lalow_rel(ip,jp) < lat_low .AND. laup_rel(ip,jp) > lat_low .OR.&
                     & lalow_rel(ip,jp) < lat_up .AND. laup_rel(ip,jp) > lat_up) THEN
                   !
                   lastjp = jp
                   !
                   ! Mising values in the file are assumed to be 1e20
                   !
                   IF ( lon_low < -180.0 ) THEN
                      lolowrel = MOD( lolow_rel(ip,jp) - 360.0, 360.0)
                      louprel = MOD( loup_rel(ip,jp) - 360.0, 360.0)
                      !
                   ELSE IF ( lon_up > 180.0 ) THEN
                      lolowrel = MOD( 360. - lolow_rel(ip,jp), 360.0)
                      louprel = MOD( 360. - loup_rel(ip,jp), 360.0)
                   ELSE
                      lolowrel = lolow_rel(ip,jp)
                      louprel = loup_rel(ip,jp)
                   ENDIF
                   !
                   ! Get the area of the fine grid in the model grid
                   !
                   coslat = MAX( COS( lat_rel(ip,jp) * pi/180. ), mincos )
                   ax = (MIN(lon_up,louprel)-MAX(lon_low, lolowrel))*pi/180. * R_Earth * coslat
                   ay = (MIN(lat_up, laup_rel(ip,jp))-MAX(lat_low,lalow_rel(ip,jp)))*pi/180. * R_Earth
                   !!to calculate the surface of the initial grid box, ie data one
                   ax_file = (louprel-lolowrel)*pi/180. * R_Earth * coslat
                   ay_file = (laup_rel(ip,jp)-lalow_rel(ip,jp))*pi/180. * R_Earth
                   !
                   IF (data_year_file(ip,jp) .LT. undef_sechiba-un) THEN
                      IF(density) THEN
                         data_grid = data_grid + ax*ay*data_year_file(ip,jp)
                      ELSE
                         data_grid = data_grid + ax*ay*(data_year_file(ip,jp)/(ax_file*ay_file))
                      ENDIF
                      area2 = area2 + ax*ay
                   ENDIF
                   !
                ENDIF
                !
                !
             ENDDO
             !
          ENDIF
          !
       ENDDO
       !
       ! Put the total data_year areas in the output variables
       !
       IF(density) THEN
          data_year(ib) = data_grid/area2 
       ELSE
          data_year(ib) = data_grid
       ENDIF


       IF ( data_year(ib) < 0 ) THEN
          WRITE(numout,*) 'In chemistry_read', filename, fieldname 
          WRITE(numout,*) 'we have a problem here : ', data_year(ib)
          WRITE(numout,*) 'resolution :', resolution(ib,1), resolution(ib,2)
          WRITE(numout,*) area2
          CALL ipslerr_p(3,'dchemistry_read','Problem with data_year < 0','','')
       ENDIF

       !!Accumulates emissions (in Kg/year) per grid box to calculate global emissions
       data_global = data_global + data_grid
    ENDDO
    
    WRITE(numout,*) trim(filename), ' ', trim(fieldname), ' RESULT data_year : ', MINVAL(data_year), ' ', MAXVAL(data_year)
    WRITE(numout,*) trim(filename), ' ', trim(fieldname), ' RESULT Global data emissions : ', data_global 
    

  END SUBROUTINE chemistry_read

!! ================================================================================================================================
!! SUBROUTINE   : chemistry_interface_orchidee_inca
!!
!>\BRIEF         This subroutine will make the interface between inca model and orchidee model 
!!
!! DESCRIPTION  :  This subroutine allow the transfer of fluxes between surface and atmospheric chemistry. It is called from Inca
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): :: 
!!
!! REFERENCE(S) : None
!!
!! FLOWCHART    : None
!_ ================================================================================================================================

 SUBROUTINE chemistry_interface_orchidee_inca( &
     &  nvm_inca, veget_max_inca, veget_frac_inca, lai_inca, snow_inca, &
     & field_out_names, fields_out, field_in_names, fields_in)


     INTEGER, INTENT(out)                      :: nvm_inca            !! Number of vegetation types
     REAL(r_std), DIMENSION (:,:), INTENT(out) :: veget_max_inca      !! Max. fraction of vegetation type (LAI -> infty)
     REAL(r_std), DIMENSION (:,:), INTENT(out) :: veget_frac_inca     !! Fraction of vegetation type (unitless, 0-1)  
     REAL(r_std), DIMENSION (:,:), INTENT(out) :: lai_inca            !! Surface foliere
     REAL(r_std), DIMENSION (:)  , INTENT(out) :: snow_inca           !! Snow mass [Kg/m^2]

     !
     ! Optional arguments
     !
     ! Names and fields for emission variables : to be transport by Orchidee to Inca
     CHARACTER(LEN=*),DIMENSION(:), OPTIONAL, INTENT(IN) :: field_out_names
     REAL(r_std),DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: fields_out
     !
     ! Names and fields for deposit variables : to be transport from chemistry model by INCA to ORCHIDEE.
     CHARACTER(LEN=*),DIMENSION(:), OPTIONAL, INTENT(IN) :: field_in_names
     REAL(r_std),DIMENSION(:,:), OPTIONAL, INTENT(IN)    :: fields_in
     !
     ! Number of fields to give (nb_fields_out) or get from (nb_fields_in) INCA :
     INTEGER(i_std), SAVE   :: nb_fields_out, nb_fields_in
!$OMP THREADPRIVATE(nb_fields_out, nb_fields_in)

     ! Id of fields to give (nb_fields_out) or get from (nb_fields_in) INCA :
     INTEGER(i_std)  :: i_fields_out, i_fields_in
     LOGICAL, SAVE   :: l_first_chemistry_inca=.TRUE. !! Initialisation has to be done one time
!$OMP THREADPRIVATE(l_first_chemistry_inca)
     !
     IF (l_first_chemistry_inca) THEN

        ! il faut verifier que ok_bvoc (chemistry_ok_bvoc) est bien active sinon on arrete tout
        if (.not.ok_bvoc) then 
          CALL ipslerr_p (3,'chemistry_inca', &
            &          'you need to activate chemistry_ok_bvoc in orchidee.def',&
            &          'This model won''t be able to continue.', &
            &          '')
        endif

        ! Prepare fieds out/in for interface with INCA.
        IF (PRESENT(field_out_names)) THEN
           nb_fields_out=SIZE(field_out_names)
        ELSE
           nb_fields_out=0
        ENDIF

        IF (PRESENT(field_in_names)) THEN
           nb_fields_in=SIZE(field_in_names)
        ELSE
           nb_fields_in=0
        ENDIF
        l_first_chemistry_inca = .FALSE.

     ENDIF

     ! Give to Inca value save in Orchidee 
     nvm_inca=nvm
     veget_max_inca=veget_max_chem
     veget_frac_inca=veget_chem
     lai_inca = lai_chem
     snow_inca=snow_chem
     
    ! Fields for deposit variables : to be transport from chemistry model by INCA to ORCHIDEE.
    DO i_fields_in=1,nb_fields_in
       SELECT CASE(TRIM(field_in_names(i_fields_in)))
       CASE DEFAULT 
          CALL ipslerr_p (3,'chemistry_inca', &
            &          'You ask in INCA an unknown field '//TRIM(field_in_names(i_fields_in))//&
            &          ' to give to ORCHIDEE for this specific version.',&
            &          'This model won''t be able to continue.', &
            &          '(check your tracer parameters in INCA)')
       END SELECT
    ENDDO
    
    ! Fields for Biogenic emissions : to be transport by Orchidee to Inca
    DO i_fields_out=1,nb_fields_out
       SELECT CASE(TRIM(field_out_names(i_fields_out)))
       CASE("flx_iso") 
          fields_out(:,:,i_fields_out)=flx_iso(:,:)
       CASE("flx_mono") 
          fields_out(:,:,i_fields_out)=flx_mono(:,:)
       CASE("flx_ORVOC") 
          fields_out(:,:,i_fields_out)=flx_ORVOC(:,:)
       CASE("flx_MBO") 
          fields_out(:,:,i_fields_out)=flx_MBO(:,:)
       CASE("flx_methanol") 
          fields_out(:,:,i_fields_out)=flx_methanol(:,:)
       CASE("flx_acetone") 
          fields_out(:,:,i_fields_out)=flx_acetone(:,:)
       CASE("flx_acetal") 
          fields_out(:,:,i_fields_out)=flx_acetal(:,:)
       CASE("flx_formal") 
          fields_out(:,:,i_fields_out)=flx_formal(:,:)
       CASE("flx_acetic") 
          fields_out(:,:,i_fields_out)=flx_acetic(:,:)
       CASE("flx_formic") 
          fields_out(:,:,i_fields_out)=flx_formic(:,:)
       CASE("flx_no_soil") 
          fields_out(:,:,i_fields_out)=flx_no_soil(:,:)
       CASE("flx_nox") 
          fields_out(:,:,i_fields_out)=flx_no(:,:)
       CASE("flx_fertil_no") 
          fields_out(:,:,i_fields_out)=flx_fertil_no(:,:)
       CASE("flx_apinen")
          fields_out(:,:,i_fields_out)=flx_apinen(:,:)
       CASE("flx_bpinen")
          fields_out(:,:,i_fields_out)=flx_bpinen(:,:)
       CASE("flx_limonen")
          fields_out(:,:,i_fields_out)=flx_limonen(:,:)
       CASE("flx_myrcen")
          fields_out(:,:,i_fields_out)=flx_myrcen(:,:)
       CASE("flx_sabinen")
          fields_out(:,:,i_fields_out)=flx_sabinen(:,:)
       CASE("flx_camphen")
          fields_out(:,:,i_fields_out)=flx_camphen(:,:)
       CASE("flx_3caren")
          fields_out(:,:,i_fields_out)=flx_3caren(:,:)
       CASE("flx_tbocimen")
          fields_out(:,:,i_fields_out)=flx_tbocimen(:,:)
       CASE("flx_othermono")
          fields_out(:,:,i_fields_out)=flx_othermono(:,:)
       CASE("flx_sesquiter")
          fields_out(:,:,i_fields_out)=flx_sesquiter(:,:)

       CASE DEFAULT 
          CALL ipslerr_p (3,'chemistry_inca', &
            &          'You ask from INCA an unknown field '//TRIM(field_out_names(i_fields_out))//&
            &          ' to ORCHIDEE for this specific version.',&
            &          'This model won''t be able to continue.', &
            &          '(check your tracer parameters in INCA)')
       END SELECT
    ENDDO
    

   END SUBROUTINE chemistry_interface_orchidee_inca


END MODULE chemistry
