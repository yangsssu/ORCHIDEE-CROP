










! =================================================================================================================================
! MODULE        : explicitsnow
!
! CONTACT       : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE       : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module computes multilayer snow ... by Tao Wang 
!!
!!\n DESCRIPTION : contains.
!! 
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S) : None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/shushi.peng/ORCHIDEE/src_sechiba/explicitsnow.f90 $
!! $Date: 2015-03-10 10:16:04 +0100 (Tue, 10 Mar 2015) $
!! $Revision: 2535 $
!! \n
!_ ================================================================================================================================

!!
!! This module computes hydrologic snow processes on continental points.
!!
MODULE explicitsnow
  USE ioipsl_para
  ! modules used :
  USE constantes_var
  USE constantes_soil
  USE constantes
  USE pft_parameters 
  USE qsat_moisture 
  USE sechiba_io

  IMPLICIT NONE

  ! Public routines :
  PRIVATE
  PUBLIC :: explicitsnow_main, explicitsnow_initialize, explicitsnow_finalize

CONTAINS

!================================================================================================================================
!! SUBROUTINE   : explicitsnow_initialize
!!
!>\BRIEF        Read variables for explictsnow module from restart file
!!                
!! DESCRIPTION  : 
!!
!! \n
!_
!================================================================================================================================
  SUBROUTINE explicitsnow_initialize( kjit,     kjpindex, rest_id,    snowrho,   &
                                      snowtemp, snowdz,   snowheat,   snowgrain)

    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                           :: kjit           !! Time step number 
    INTEGER(i_std), INTENT(in)                           :: kjpindex       !! Domain size
    INTEGER(i_std),INTENT (in)                           :: rest_id        !! Restart file identifier
    
    !! 0.2 Output variables
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowrho        !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowtemp       !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowdz         !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowheat       !! Snow heat content
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(out)  :: snowgrain      !! Snow grainsize
    
    
    !! 1. Read from restart file
    CALL restget_p (rest_id, 'snowrho', nbp_glo, nsnow, 1, kjit,.TRUE.,snowrho, "gather", nbp_glo, index_g)
    CALL setvar_p (snowrho, val_exp, 'Snow Density profile', xrhosmin)
    
    CALL restget_p (rest_id, 'snowtemp', nbp_glo,  nsnow, 1, kjit,.TRUE.,snowtemp, "gather", nbp_glo,  index_g)
    CALL setvar_p (snowtemp, val_exp, 'Snow Temperature profile', tp_00)
    
    CALL restget_p (rest_id, 'snowdz', nbp_glo,  nsnow, 1, kjit,.TRUE.,snowdz, "gather", nbp_glo,  index_g)
    CALL setvar_p (snowdz, val_exp, 'Snow depth profile', 0.0)
    
    CALL restget_p (rest_id, 'snowheat', nbp_glo,  nsnow, 1, kjit,.TRUE.,snowheat, "gather", nbp_glo,  index_g)
    CALL setvar_p (snowheat, val_exp, 'Snow Heat profile', 0.0)
    
    CALL restget_p (rest_id, 'snowgrain', nbp_glo,  nsnow, 1, kjit,.TRUE.,snowgrain, "gather", nbp_glo,  index_g)
    CALL setvar_p (snowgrain, val_exp, 'Snow grain profile', 0.0)
    
  END SUBROUTINE explicitsnow_initialize
  

!================================================================================================================================
!! SUBROUTINE   : explicitsnow_main
!!
!>\BRIEF        
!!                
!! DESCRIPTION  : 
!!
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================
 SUBROUTINE explicitsnow_main(kjpindex,     precip_rain,   precip_snow,    temp_air,    pb,           &
                              u,            v,             temp_sol_new,   soilcap,     pgflux,       &
                              frac_nobio,   totfrac_nobio, gtemp,                                     &
                              lambda_snow,  cgrnd_snow,    dgrnd_snow,                                & ! in  
                              vevapsno,     snow_age,      snow_nobio_age, snow_nobio,  snowrho,      &
                              snowgrain,    snowdz,        snowtemp,       snowheat,    snow,         &
                              temp_sol_add,                                                           &
                              snowliq,      subsnownobio, grndflux,      snowmelt,       tot_melt,    &
                              soilflxresid, subsinksoil) 

    !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                               :: kjpindex         !! Domain size
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_rain      !! Rainfall
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: precip_snow      !! Snowfall
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: temp_air         !! Air temperature
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: pb               !! Surface pressure
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: u,v              !! Horizontal wind speed

    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: soilcap          !! Soil capacity
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: pgflux           !! Net energy into snowpack
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(in)     :: frac_nobio       !! Fraction of continental ice, lakes, ...
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: totfrac_nobio    !! Total fraction of continental ice+lakes+ ...
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: gtemp            !! First soil layer temperature 
    !TW
    REAL(r_std), DIMENSION (kjpindex),INTENT(inout)          :: soilflxresid
    REAL(r_std), DIMENSION (kjpindex), INTENT(in)            :: lambda_snow      !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (in)     :: cgrnd_snow       !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT (in)     :: dgrnd_snow       !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex), INTENT (in)            :: temp_sol_new     !! skin temperature


    !! 0.2 Output fields
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)           :: snowmelt         !! Snow melt
    REAL(r_std), DIMENSION (kjpindex), INTENT(out)           :: tot_melt         !! Total melt from ice and snow




    !! 0.3 Modified fields
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: subsnownobio     !! Sublimation of snow on other surface types (ice, lakes, ...)
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: vevapsno         !! Snow evaporation  @tex ($kg m^{-2}$) @endtex 
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: subsinksoil      !! Excess of sublimation as a sink for the soil
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: snow_age         !! Snow age
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: snow_nobio       !! Ice water balance
    REAL(r_std), DIMENSION (kjpindex,nnobio), INTENT(inout)  :: snow_nobio_age   !! Snow age on ice, lakes, ...
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowrho          !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowtemp         !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowgrain        !! Snow grainsize
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowdz           !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowheat         !! Snow heat content
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowliq          !! Snow liquid content (m)
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: snow             !! Snow mass [Kg/m^2]
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: grndflux         !! Net flux into soil [W/m2]
    REAL(r_std), DIMENSION (kjpindex), INTENT(inout)         :: temp_sol_add     !! Additional energy to melt snow for snow ablation case (K)

 
    !! 0.4 Local declaration
    INTEGER(i_std)                                           :: ji, iv, jj,m,jv
    REAL(r_std),DIMENSION  (kjpindex)                        :: snow_depth_tmp
    REAL(r_std),DIMENSION  (kjpindex)                        :: snowmelt_from_maxmass
    REAL(r_std)                                              :: snowdzm1
    REAL(r_std), DIMENSION (kjpindex)                        :: thrufal          !! Water leaving snow pack [kg/m2/s]
    REAL(r_std), DIMENSION (kjpindex)                        :: d_age            !! Snow age change
    REAL(r_std), DIMENSION (kjpindex)                        :: xx               !! Temporary
    REAL(r_std), DIMENSION (kjpindex)                        :: snowmelt_tmp,snowmelt_ice,icemelt,temp_sol_new_old
    REAL(r_std), DIMENSION (kjpindex,nsnow)                  :: snowdz_old
    REAL(r_std), DIMENSION (kjpindex)                        :: ZLIQHEATXS
    REAL(r_std), DIMENSION (kjpindex)                        :: ZSNOWEVAPS, ZSNOWDZ,subsnowveg
    REAL(r_std)                                              :: maxmass_snowdepth 
    REAL(r_std), DIMENSION (kjpindex,nsnow)                  :: WSNOWDZ,SMASS
    REAL(r_std), DIMENSION (kjpindex)                        :: SMASSC,snowacc
    INTEGER(i_std) :: locjj
    REAL(r_std)                                              :: grndflux_tmp
    REAL(r_std), DIMENSION (nsnow)                           :: snowtemp_tmp
    REAL(r_std)                                              :: s2flux_tmp,fromsoilflux
    REAL(r_std), DIMENSION (kjpindex,nsnow)                  :: pcapa_snow 
    REAL(r_std), DIMENSION (kjpindex)                        :: psnowhmass
    REAL(r_std), PARAMETER                                   :: XP00 = 1.E5


    !! 1. Initialization
    
    temp_sol_new_old = temp_sol_new
    DO ji=1,kjpindex
       snowmelt_ice(ji) = zero
       icemelt(ji) = zero
       tot_melt(ji) = zero
       snowmelt(ji) = zero
    ENDDO
    
    !! 2. on Vegetation
    ! 2.1 Snow fall
    CALL explicitsnow_fall(kjpindex,precip_snow,temp_air,u,v,totfrac_nobio,snowrho,snowdz,&
         & snowheat,snowgrain,snowtemp,psnowhmass)
    
    ! 2.2 calculate the new snow discretization
    snow_depth_tmp(:) = SUM(snowdz(:,:),2)
    
    snowdz_old = snowdz
    
    CALL explicitsnow_levels(kjpindex,snow_depth_tmp, snowdz)
    
    ! 2.3 Snow heat redistribution
    CALL explicitsnow_transf(kjpindex,snowdz_old,snowdz,snowrho,snowheat,snowgrain)
    
    ! 2.4 Diagonize water portion of the snow from snow heat content:
    DO ji=1, kjpindex
       IF (SUM(snowdz(ji,:)) .GT. 0.0) THEN
          snowtemp(ji,:) = snow3ltemp_1d(snowheat(ji,:),snowrho(ji,:),snowdz(ji,:))
          snowliq(ji,:) = snow3lliq_1d(snowheat(ji,:),snowrho(ji,:),snowdz(ji,:),snowtemp(ji,:))
       ELSE
          snowliq(ji,:) = zero
          snowtemp(ji,:) = tp_00
       ENDIF
    END DO
    
    ! 2.5 snow compaction
    CALL explicitsnow_compactn(kjpindex,snowtemp,snowrho,snowdz)
    ! Update snow heat 
    DO ji = 1, kjpindex
       snowheat(ji,:) = snow3lheat_1d(snowliq(ji,:),snowrho(ji,:),snowdz(ji,:),snowtemp(ji,:))
    ENDDO

    !2.6 Calculate the snow temperature profile based on heat diffusion
    CALL explicitsnow_profile (kjpindex,cgrnd_snow,dgrnd_snow,lambda_snow,temp_sol_new, snowtemp,snowdz,temp_sol_add)
    
    !2.7 Test whether snow is existed on the ground or not
    grndflux(:)=0.0
    CALL explicitsnow_gone(kjpindex,pgflux,&
         & snowheat,snowtemp,snowdz,snowrho,snowliq,grndflux,snowmelt,soilflxresid)

    !2.8 Calculate snow melt/refreezing processes
    CALL explicitsnow_melt_refrz(kjpindex,precip_rain,pgflux,soilcap,&
         & snowtemp,snowdz,snowrho,snowliq,snowmelt,grndflux,temp_air,soilflxresid)


    ! 2.9 Snow sublimation changing snow thickness
    snow(:) = 0.0
    DO ji=1,kjpindex !domain size
       snow(ji) = SUM(snowrho(ji,:) * snowdz(ji,:))
    ENDDO
    
    subsnownobio(:,:) = zero
    subsinksoil(:) = zero
    
    DO ji=1, kjpindex ! domain size
       IF ( snow(ji) > snowcri ) THEN
          subsnownobio(ji,iice) = frac_nobio(ji,iice)*vevapsno(ji)
          subsnowveg(ji) = vevapsno(ji) - subsnownobio(ji,iice)
       ELSE
          IF ( frac_nobio(ji,iice) .GT. min_sechiba) THEN
             subsnownobio(ji,iice) = vevapsno(ji)
             subsnowveg(ji) = zero
          ELSE
             subsnownobio(ji,iice) = zero
             subsnowveg(ji) = vevapsno(ji)
          ENDIF
       ENDIF
       
       !! 2.8.1 Check that sublimation on the vegetated fraction is possible.
       IF (subsnowveg(ji) .GT. snow(ji)) THEN
          IF( (un - totfrac_nobio(ji)).GT.min_sechiba) THEN
             subsinksoil (ji) = (subsnowveg(ji) - snow(ji))/ (un - totfrac_nobio(ji))
          END IF
          ! Sublimation is thus limited to what is available
          subsnowveg(ji) = snow(ji)
          snow(ji) = zero
          snowdz(ji,:)  =  0
          snowliq(ji,:)   =  0
          snowtemp(ji,:) = tp_00 
          vevapsno(ji) = subsnowveg(ji) + subsnownobio(ji,iice)
       ELSE
          ! Calculating the snow accumulation  
          WSNOWDZ(ji,:)= snowdz(ji,:)*snowrho(ji,:)
          SMASSC (ji)= 0.0
          DO jj=1,nsnow
             SMASS(ji,jj)   = SMASSC(ji) + WSNOWDZ(ji,jj)
             SMASSC(ji)      = SMASSC(ji) + WSNOWDZ(ji,jj)
          ENDDO
          ! Finding the layer
          locjj=0
          DO jj=1,nsnow-1
             IF ((SMASS(ji,jj) .LE. subsnowveg(ji)) .AND. (SMASS(ji,jj+1) .GE. subsnowveg(ji)) ) THEN
                locjj=jj+1
             ENDIF
          ENDDO
          
          ! Calculating the removal of snow depth
          IF (locjj .EQ. 1) THEN
             ZSNOWEVAPS(ji)  = subsnowveg(ji)/snowrho(ji,1)
             ZSNOWDZ(ji)     = snowdz(ji,1) - ZSNOWEVAPS(ji)
             snowdz(ji,1)     = MAX(0.0, ZSNOWDZ(ji))
          ELSE IF (locjj .GT. 1) THEN
             snowacc(ji)=0
             DO jj=1,locjj-1
                snowacc(ji)=snowacc(ji)+snowdz(ji,jj)*snowrho(ji,jj)
                snowdz(ji,jj)=0
             ENDDO
             ZSNOWEVAPS(ji)  = (subsnowveg(ji)-snowacc(ji))/snowrho(ji,locjj)
             ZSNOWDZ(ji)     = snowdz(ji,locjj) - ZSNOWEVAPS(ji)
             snowdz(ji,locjj)     = MAX(0.0, ZSNOWDZ(ji))
          ELSE
             ZSNOWEVAPS(ji)  = subsnowveg(ji)/snowrho(ji,1)
             ZSNOWDZ(ji)     = snowdz(ji,1) - ZSNOWEVAPS(ji)
             snowdz(ji,1)     = MAX(0.0, ZSNOWDZ(ji))
          ENDIF
          
       ENDIF
    ENDDO
    
    !2.10 Calculate snow grain size using the updated thermal gradient
    
    CALL explicitsnow_grain(kjpindex,snowliq,snowdz,gtemp,snowtemp,pb,snowgrain)
    
    !2.11  Update snow heat
    ! Update the heat content (variable stored each time step)
    ! using current snow temperature and liquid water content:
    !
    ! First, make check to make sure heat content not too large
    ! (this can result due to signifigant heating of thin snowpacks):
    ! add any excess heat to ground flux:
    !
    DO ji=1,kjpindex
       DO jj=1,nsnow
          ZLIQHEATXS(ji)  = MAX(0.0, snowliq(ji,jj)*ph2o - 0.10*snowdz(ji,jj)*snowrho(ji,jj))*chalfu0/dt_sechiba
          snowliq(ji,jj) = snowliq(ji,jj) - ZLIQHEATXS(ji)*dt_sechiba/(ph2o*chalfu0)
          snowliq(ji,jj) = MAX(0.0, snowliq(ji,jj))
          grndflux(ji)   = grndflux(ji)   + ZLIQHEATXS(ji)
       ENDDO
    ENDDO
    
    snow(:) = 0.0
    DO ji=1,kjpindex !domain size
       snow(ji) = SUM(snowrho(ji,:) * snowdz(ji,:))
    ENDDO
        
    DO ji = 1, kjpindex
       snowheat(ji,:) = snow3lheat_1d(snowliq(ji,:),snowrho(ji,:),snowdz(ji,:),snowtemp(ji,:))
    ENDDO
    
    !3. on land ice (using the default ORCHIDEE snow scheme)
    
    DO ji = 1,kjpindex
       
       !! 3.1. It is snowing
       
       snow_nobio(ji,iice) = snow_nobio(ji,iice) + frac_nobio(ji,iice)*precip_snow(ji) + &
            & frac_nobio(ji,iice)*precip_rain(ji)
       
       !! 3.2. Sublimation - was calculated before it can give us negative snow_nobio but that is OK
       !!      Once it goes below a certain values (-maxmass_snow for instance) we should kill
       !!      the frac_nobio(ji,iice) !
       
       snow_nobio(ji,iice) = snow_nobio(ji,iice) - subsnownobio(ji,iice)
       
       !! 3.3. ice melt only for continental ice fraction
       
       snowmelt_tmp(ji) = zero
       IF (temp_sol_new_old(ji) .GT. tp_00) THEN
          
          !! 3.3.1 If there is snow on the ice-fraction it can melt
          
          snowmelt_tmp(ji) = frac_nobio(ji,iice)*(temp_sol_new_old(ji) - tp_00) * soilcap(ji) / chalfu0
          
          IF ( snowmelt_tmp(ji) .GT. snow_nobio(ji,iice) ) THEN
             snowmelt_tmp(ji) = MAX( 0., snow_nobio(ji,iice))
          ENDIF
          snowmelt_ice(ji) = snowmelt_ice(ji) + snowmelt_tmp(ji)
          snow_nobio(ji,iice) = snow_nobio(ji,iice) - snowmelt_tmp(ji)
          
       ENDIF

       !! Ice melt only if there is more than a given mass : maxmass_snow, i.e. only weight melts glaciers !
       
       IF ( snow_nobio(ji,iice) .GE. maxmass_snow ) THEN
          icemelt(ji) = snow_nobio(ji,iice) - maxmass_snow
          snow_nobio(ji,iice) = maxmass_snow
       ENDIF
       
    END DO
    
    
    !! 4. On other surface types - not done yet
    
    IF ( nnobio .GT. 1 ) THEN
       WRITE(numout,*) 'WE HAVE',nnobio-1,' SURFACE TYPES I DO NOT KNOW'
       WRITE(numout,*) 'CANNOT TREAT SNOW ON THESE SURFACE TYPES'
       CALL ipslerr_p(3,'explicitsnow_main','Cannot treat snow on these surface types.','','')
    ENDIF
    
    !! 5. Computes snow age on land and land ice (for albedo)
    
    DO ji = 1, kjpindex
       
       !! 5.1. Snow age on land 
       
       IF (snow(ji) .LE. zero) THEN
          snow_age(ji) = zero
       ELSE
          snow_age(ji) =(snow_age(ji) + (un - snow_age(ji)/max_snow_age) * dt_sechiba/one_day) &
               & * EXP(-precip_snow(ji) / snow_trans)
       ENDIF
       
       !! 5.2. Snow age on land ice
       
       !! Age of snow on ice: a little bit different because in cold regions, we really
       !! cannot negect the effect of cold temperatures on snow metamorphism any more.
       
       IF (snow_nobio(ji,iice) .LE. zero) THEN
          snow_nobio_age(ji,iice) = zero
       ELSE
          
          d_age(ji) = ( snow_nobio_age(ji,iice) + &
               &  (un - snow_nobio_age(ji,iice)/max_snow_age) * dt_sechiba/one_day ) * &
               &  EXP(-precip_snow(ji) / snow_trans) - snow_nobio_age(ji,iice)
          IF (d_age(ji) .GT. 0. ) THEN
             xx(ji) = MAX( tp_00 - temp_sol_new(ji), zero )
             xx(ji) = ( xx(ji) / 7._r_std ) ** 4._r_std
             d_age(ji) = d_age(ji) / (un+xx(ji))
          ENDIF
          snow_nobio_age(ji,iice) = MAX( snow_nobio_age(ji,iice) + d_age(ji), zero )
          
       ENDIF
       
    ENDDO
    
    
    !! 6. Check the snow on land 
    DO ji=1,kjpindex
       IF (snow(ji) .EQ. 0) THEN
          snowrho(ji,:)=50.0
          snowgrain(ji,:)=0.0
          snowdz(ji,:)=0.0
          snowliq(ji,:)=0.0
       ENDIF
    ENDDO
    
    
    ! Snow melt only if there is more than a given mass : maxmass_snow
    ! Here I suggest to remove the snow based on a certain threshold of snow
    ! depth instead of snow mass because it is quite difficult for
    ! explictsnow module to calculate other snow properties following the
    ! removal of snow mass
    ! to define the threshold of snow depth based on old snow density (330
    ! kg/m3)
    !       maxmass_snowdepth=maxmass_snow/sn_dens 
    snowmelt_from_maxmass(:) = 0.0
    
    !! 7. compute total melt 
    
    DO ji=1,kjpindex 
       tot_melt(ji) = icemelt(ji) + snowmelt(ji) + snowmelt_ice(ji) + snowmelt_from_maxmass(ji)
    ENDDO
    IF (printlev>=3) WRITE(numout,*) 'explicitsnow_main done'
    
  END SUBROUTINE explicitsnow_main
  

!================================================================================================================================
!! SUBROUTINE   : explicitsnow_finalize
!!
!>\BRIEF        Write variables for explictsnow module to restart file
!!                
!! DESCRIPTION  : Write variables for explictsnow module to restart file
!!
!! \n
!_
!================================================================================================================================
  SUBROUTINE explicitsnow_finalize ( kjit,     kjpindex, rest_id,    snowrho,   &
       snowtemp, snowdz,   snowheat,   snowgrain)
    
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                           :: kjit           !! Time step number 
    INTEGER(i_std), INTENT(in)                           :: kjpindex       !! Domain size
    INTEGER(i_std),INTENT (in)                           :: rest_id        !! Restart file identifier
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowrho        !! Snow density
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowtemp       !! Snow temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowdz         !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowheat       !! Snow heat content
    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(in)  :: snowgrain      !! Snow grainsize
    
    
    !! 1. Write to restart file
    CALL restput_p(rest_id, 'snowrho', nbp_glo, nsnow, 1, kjit, snowrho, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'snowtemp', nbp_glo, nsnow, 1, kjit, snowtemp, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'snowdz', nbp_glo, nsnow, 1, kjit, snowdz, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'snowheat', nbp_glo, nsnow, 1, kjit, snowheat, 'scatter', nbp_glo, index_g)
    CALL restput_p(rest_id, 'snowgrain', nbp_glo, nsnow, 1, kjit, snowgrain, 'scatter', nbp_glo, index_g)
    
  END SUBROUTINE explicitsnow_finalize
  

!================================================================================================================================
!! SUBROUTINE   : explicitsnow_grain
!!
!>\BRIEF        Compute evolution of snow grain size
!!                
!! DESCRIPTION  : 
!!
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : R. Jordan (1991)
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================


 SUBROUTINE explicitsnow_grain(kjpindex,snowliq,snowdz,gtemp,snowtemp,pb,snowgrain)

      !! 0.1 Input variables
      INTEGER(i_std),INTENT(in)                                  :: kjpindex         !! Domain size
      REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(in)           :: snowliq          !! Liquid water content
      REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(in)           :: snowdz           !! Snow depth (m)
      REAL(r_std),DIMENSION(kjpindex),INTENT(in)                 :: gtemp            !! First soil layer temperature
      REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(in)           :: snowtemp         !! Snow temperature
      REAL(r_std),DIMENSION (kjpindex),INTENT(in)                :: pb               !! Surface pressure (hpa) 

      !! 0.2 Output variables

      !! 0.3 Modified variables

      REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)        :: snowgrain        !! Snow grain size

      !! 0.4 Local variables
      REAL(r_std),DIMENSION(kjpindex,nsnow)                      :: zsnowdz,zdz,ztheta
      REAL(r_std),DIMENSION(kjpindex,0:nsnow)                    :: ztemp,zdiff,ztgrad,zthetaa,zfrac,&
                                                                    zexpo,zckt_liq,zckt_ice,zckt
      REAL(r_std),DIMENSION(kjpindex,nsnow)                      :: zrhomin,zgrainmin
      INTEGER(i_std) :: ji,jj

      !! 0.5 Local parameters
      REAL(r_std), PARAMETER                                     :: ztheta_crit = 0.02     !! m3 m-3
      REAL(r_std), PARAMETER                                     :: zc1_ice     = 8.047E+9 !! kg m-3 K
      REAL(r_std), PARAMETER                                     :: zc1_liq     = 5.726E+8 !! kg m-3 K
      REAL(r_std), PARAMETER                                     :: zdeos       = 0.92E-4  !! effective diffusion
                                                                                           !! coef for water vapor in snow
                                                                                           !! at 0C and 1000 mb (m2 s-1)
      REAL(r_std), PARAMETER                                     :: zg1         = 5.0E-7   !! m4 kg-1
      REAL(r_std), PARAMETER                                     :: zg2         = 4.0E-12  !! m2 s-1
      REAL(r_std), PARAMETER                                     :: ztheta_w      = 0.05   !! m3 m-3
      REAL(r_std), PARAMETER                                     :: ztheta_crit_w = 0.14   !! m3 m-3
      REAL(r_std), PARAMETER                                     :: zdzmin        = 0.01   !! m : minimum thickness
                                                                                           !! for thermal gradient evaluation:
                                                                                           !! to prevent excessive gradients
                                                                                           !! for vanishingly thin snowpacks.
      REAL(r_std), PARAMETER                                     :: xp00=1.E5 
      !! 1. initialize
 
      DO ji=1,kjpindex 


         zsnowdz(ji,:)  = MAX(xsnowdmin/nsnow, snowdz(ji,:))

         DO jj=1,nsnow-1
            zdz(ji,jj)      = zsnowdz(ji,jj) + zsnowdz(ji,jj+1)
         ENDDO
            zdz(ji,nsnow)     = zsnowdz(ji,nsnow)

         ! compute interface average volumetric water content (m3 m-3):
         ! first, layer avg VWC:
         !
          ztheta(ji,:) = snowliq(ji,:)/MAX(xsnowdmin, zsnowdz(ji,:))

         ! at interfaces:
          zthetaa(ji,0)      = ztheta(ji,1)
          DO jj=1,nsnow-1
             zthetaa(ji,jj)  = (zsnowdz(ji,jj)  *ztheta(ji,jj)   +             &
                                   zsnowdz(ji,jj+1)*ztheta(ji,jj+1))/zdz(ji,jj)
          ENDDO
          zthetaa(ji,nsnow) = ztheta(ji,nsnow)
          ! compute interface average temperatures (K):
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          !
          ztemp(ji,0)      = snowtemp(ji,1)
          DO jj=1,nsnow-1
             ztemp(ji,jj)  = (zsnowdz(ji,jj)  *snowtemp(ji,jj)   +             &
                                 zsnowdz(ji,jj+1)*snowtemp(ji,jj+1))/zdz(ji,jj)
          ENDDO
          ztemp(ji,nsnow) = snowtemp(ji,nsnow)
!
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          ! compute variation of saturation vapor pressure with temperature
          ! for solid and liquid phases:
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          zexpo(ji,:)    = chalsu0/(xrv*ztemp(ji,:))
          zckt_ice(ji,:) = (zc1_ice/ztemp(ji,:)**2)*(zexpo(ji,:) - 1.0)*EXP(-zexpo(ji,:))
          !
          zexpo(ji,:)    = chalev0/(xrv*ztemp(ji,:))
          zckt_liq(ji,:) = (zc1_liq/ztemp(ji,:)**2)*(zexpo(ji,:) - 1.0)*EXP(-zexpo(ji,:))
          !
          ! compute the weighted ice/liquid total variation (N m-2 K):
          !
          zfrac(ji,:)    = MIN(1.0, zthetaa(ji,:)/ztheta_crit)
          zckt(ji,:)     = zfrac(ji,:)*zckt_liq(ji,:) + (1.0 - zfrac(ji,:))*zckt_ice(ji,:)

          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          ! Compute effective diffusion coefficient (m2 s-1):
          ! -diffudivity relative to a reference diffusion at 1000 mb and freezing point
          !  multiplied by phase energy coefficient
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          !
          DO jj=0,nsnow
             zdiff(ji,jj) = zdeos*(xp00/(pb(ji)*100.))*((ztemp(ji,jj)/tp_00)**6)*zckt(ji,jj)
          ENDDO 

          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          ! Temperature gradient (K m-1):

          ztgrad(ji,0)      = 0.0 ! uppermost layer-mean and surface T's are assumed to be equal
          DO jj=1,nsnow-1
             ztgrad(ji,jj)  = 2*(snowtemp(ji,jj)-snowtemp(ji,jj+1))/MAX(zdzmin, zdz(ji,jj))
          ENDDO
          !
          ! assume at base of snow, temperature is in equilibrium with soil
          ! (but obviously must be at or below freezing point!)
          !
          ztgrad(ji,nsnow) = 2*(snowtemp(ji,nsnow) - MIN(tp_00, gtemp(ji)))/MAX(zdzmin, zdz(ji,nsnow))
          ! prognostic grain size (m) equation:
          !-------------------------------------------------------------------
          ! first compute the minimum grain size (m):
          !
          zrhomin(ji,:)     = xrhosmin
          zgrainmin(ji,:)   = snow3lgrain_1d(zrhomin(ji,:))

          ! dry snow:
          ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
          !
          DO jj=1,nsnow

             IF(ztheta(ji,jj) == 0.0) THEN

              ! only allow growth due to vapor flux INTO layer: 
              ! aab add sublimation(only condensation) as upper BC...?

                snowgrain(ji,jj)  = snowgrain(ji,jj) +                                      &
                                       (dt_sechiba*zg1/MAX(zgrainmin(ji,jj),snowgrain(ji,jj)))*      &
                                       ( zdiff(ji,jj-1)*MAX(0.0,ztgrad(ji,jj-1)) -                &
                                       zdiff(ji,jj)  *MIN(0.0,ztgrad(ji,jj)) )
             ELSE
 
              ! wet snow
              ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
              !
                snowgrain(ji,jj)  = snowgrain(ji,jj) +                                      &
                                       (dt_sechiba*zg2/MAX(zgrainmin(ji,jj),snowgrain(ji,jj)))*      &
                                       MIN(ztheta_crit_w, ztheta(ji,jj) + ztheta_w)
             END IF

          ENDDO


      ENDDO


    END SUBROUTINE explicitsnow_grain

!!
!================================================================================================================================
!! SUBROUTINE   : explicitsnow_compactn
!!
!>\BRIEF        Compute Compaction/Settling
!!                
!! DESCRIPTION  : 
!!     Snow compaction due to overburden and settling.
!!     Mass is unchanged: layer thickness is reduced
!!     in proportion to density increases. Method
!!     of Anderson (1976): see Loth and Graf, 1993,
!!     J. of Geophys. Res., 98, 10,451-10,464.
!!
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : Loth and Graf (1993), Mellor (1964) and Anderson (1976)
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================


   SUBROUTINE explicitsnow_compactn(kjpindex,snowtemp,snowrho,snowdz)

         !! 0.1 Input variables

         INTEGER(i_std),INTENT(in)                                 :: kjpindex         !! Domain size
         REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(in)          :: snowtemp         !! Snow temperature

         !! 0.2 Output variables

         !! 0.3 Modified variables

         REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)       :: snowrho          !! Snow density
         REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)       :: snowdz           !! Snow depth

         !! 0.4 Local variables

         REAL(r_std),DIMENSION(kjpindex,nsnow)                     :: zwsnowdz,zsmass,zsnowrho2,zviscocity,zsettle 
         REAL(r_std),DIMENSION(kjpindex)                           :: zsmassc          !! cummulative snow mass (kg/m2)
         REAL(r_std),DIMENSION(kjpindex)                           :: snowdepth_crit
         INTEGER(i_std)                                            :: ji,jj

        !! 1. initialize

        zsnowrho2  = snowrho
        zsettle(:,:)    = ZSNOWCMPCT_ACM
        zviscocity(:,:) = ZSNOWCMPCT_V0

        !! 2. Calculating Cumulative snow mass (kg/m2):

        DO ji=1, kjpindex


            IF (SUM(snowdz(ji,:)) .GT. 0.0) THEN 

              zwsnowdz(ji,:)= snowdz(ji,:)*snowrho(ji,:)

              zsmassc (ji)= 0.0

              DO jj=1,nsnow
                 zsmass(ji,jj)   = zsmassc(ji) + zwsnowdz(ji,jj)
                 zsmassc(ji)      = zsmassc(ji) + zwsnowdz(ji,jj)
              ENDDO


              !! 3. Computing compaction/Settling
              ! ----------------------
              ! Compaction/settling if density below upper limit
              ! (compaction is generally quite small above ~ 500 kg m-3):
              !
              DO jj=1,nsnow
                 IF (snowrho(ji,jj) .LT. xrhosmax) THEN
            
              !
              ! First calculate settling due to freshly fallen snow: (NOTE:bug here for the snow temperature profile)
              !
             
              zsettle(ji,jj)     = ZSNOWCMPCT_ACM*EXP(                                      &
                                     -ZSNOWCMPCT_BCM*(tp_00-MIN(tp_00,snowtemp(ji,jj)))                      &
                                     -ZSNOWCMPCT_CCM*MAX(0.0,                                  &
                                      snowrho(ji,jj)-ZSNOWCMPCT_RHOD))
              !
              ! Snow viscocity:
              !
              zviscocity(ji,jj)   = ZSNOWCMPCT_V0*EXP( ZSNOWCMPCT_VT*(tp_00-MIN(tp_00,snowtemp(ji,jj))) +        &
                                  ZSNOWCMPCT_VR*snowrho(ji,jj) )

              ! Calculate snow density: compaction from weight/over-burden
              ! Anderson 1976 method:
              zsnowrho2(ji,jj)    = snowrho(ji,jj) + snowrho(ji,jj)*dt_sechiba*(          &
                                   (cte_grav*zsmass(ji,jj)/zviscocity(ji,jj))                 &
                                    + zsettle(ji,jj) )
              ! Conserve mass by decreasing grid thicknesses in response
              ! to density increases
              !
              snowdz(ji,jj)  = snowdz(ji,jj)*(snowrho(ji,jj)/zsnowrho2(ji,jj))

                 ENDIF
              ENDDO

              ! Update density (kg m-3):
              snowrho(ji,:) = zsnowrho2(ji,:)

            ENDIF

        ENDDO

      END SUBROUTINE explicitsnow_compactn

!!
!================================================================================================================================
!! SUBROUTINE   : explicitsnow_transf
!!
!>\BRIEF        Computing snow mass and heat redistribution due to grid thickness configuration resetting
!!                
!! DESCRIPTION  : Snow mass and heat redistibution due to grid thickness
!!                configuration resetting. Total mass and heat content
!!                of the overall snowpack unchanged/conserved within this routine. 
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================

   SUBROUTINE explicitsnow_transf(kjpindex,snowdz_old,snowdz,snowrho,snowheat,snowgrain)

     !! 0.1 Input variables

     INTEGER(i_std),INTENT(in)                                           :: kjpindex         !! Domain size
     REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(in)                    :: snowdz_old       !! Snow depth at the previous time step 

     !! 0.2 Output variables

     !! 0.3 Modified variables

     REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)                 :: snowrho          !! Snow density 
     REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)                 :: snowgrain        !! Snow grain size
     REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)                 :: snowdz           !! Snow depth (m)
     REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)                 :: snowheat         !! Snow heat content/enthalpy (J/m2)
                                                                        
     !! 0.4 Local varibles                                              
      
     REAL(r_std),DIMENSION(kjpindex,nsnow)                               :: zsnowzo
     REAL(r_std),DIMENSION(kjpindex,nsnow)                               :: zsnowzn 
     REAL(r_std),DIMENSION(kjpindex,nsnow)                               :: zsnowddz 
     REAL(r_std),DIMENSION(kjpindex,nsnow)                               :: zdelta
     REAL(r_std),DIMENSION(kjpindex,nsnow)                               :: zsnowrhon,zsnowheatn,zsnowgrainn
     REAL(r_std),DIMENSION(kjpindex)                                     :: zsnowmix_delta
     REAL(r_std),DIMENSION(kjpindex)                                     :: zsumheat, zsumswe, zsumgrain
     INTEGER(i_std),DIMENSION(nsnow,2)                                   :: locflag
     REAL(r_std)                                                         :: psnow
     INTEGER(i_std)                                                      :: ji,jj,jjj

     ! Initialization
     zsnowzo(:,:)      = 0.0
     zsnowzn(:,:)      = 0.0
     zsnowddz(:,:)     = 0.0
     zdelta(:,:)       = 0.0
     zsnowrhon(:,:)    = 0.0
     zsnowheatn(:,:)   = 0.0
     zsnowgrainn(:,:)  = 0.0
 
     zsumheat(:)       = 0.0
     zsumswe(:)        = 0.0
     zsumgrain(:)      = 0.0
     zsnowmix_delta(:) = 0.0
     locflag(:,:)      = 0

     DO ji=1, kjpindex 
 

        psnow = SUM(snowdz(ji,:))

     IF (psnow .GE. xsnowcritd .AND. snowdz_old(ji,1) .NE. 0 .AND. snowdz_old(ji,2) .NE. 0 .AND. snowdz_old(ji,3) .NE. 0) THEN
          !
          zsnowzo(ji,1)     = snowdz_old(ji,1)
          zsnowzn(ji,1)     = snowdz(ji,1)
          !
          DO jj=2,nsnow
             zsnowzo(ji,jj) = zsnowzo(ji,jj-1) + snowdz_old(ji,jj)
             zsnowzn(ji,jj) = zsnowzn(ji,jj-1) + snowdz(ji,jj)
          ENDDO

          !layer thickness change

          zsnowddz(ji,:)    = zsnowzn(ji,:) - zsnowzo(ji,:)
          !
          ! Calculate the delta function:
          !
          zdelta(ji,:)    = 0.0
          WHERE(zsnowddz(ji,:) > 0.0) zdelta(ji,:) = 1.0
          !

          ! Calculate mass and heat transfers due to grid adjustment/changes:
          ! Upper layer:
          !
          zsnowrhon(ji,1)  = ( snowdz_old(ji,1)*snowrho(ji,1) + zsnowddz(ji,1)*      &
                            (     zdelta(ji,1) *snowrho(ji,2) +                 &
                            (1.0-zdelta(ji,1))*snowrho(ji,1) ) )               &
                            /snowdz(ji,1)
          !
          IF (snowdz_old(ji,1) .GT. 0.0) THEN

                zsnowheatn(ji,1) = snowheat(ji,1) + zsnowddz(ji,1)*                    &
                                  ((    zdelta(ji,1) *snowheat(ji,2)/snowdz_old(ji,2)) +  &
                                  ((1.0-zdelta(ji,1))*snowheat(ji,1)/snowdz_old(ji,1)) )
          ELSE
                zsnowheatn(ji,1) = snowheat(ji,2)/snowdz_old(ji,2)*zsnowddz(ji,1)

          ENDIF
                !
          zsnowgrainn(ji,1)  = ( snowdz_old(ji,1)*snowgrain(ji,1) + zsnowddz(ji,1)*  &
                            (     zdelta(ji,1) *snowgrain(ji,2) +               &
                            (1.0-zdelta(ji,1))*snowgrain(ji,1) ) )             &
                            /snowdz(ji,1)




          ! Lowest layer:
          !
          zsnowrhon(ji,nsnow)  = ( snowdz_old(ji,nsnow)*snowrho(ji,nsnow) -      &
                                    zsnowddz(ji,nsnow-1)*                                &
                                    (     zdelta(ji,nsnow-1) *snowrho(ji,nsnow) +     &
                                    (1.0-zdelta(ji,nsnow-1))*snowrho(ji,nsnow-1) ) )  &
                                    /snowdz(ji,nsnow)
          !
          zsnowheatn(ji,nsnow) = snowheat(ji,nsnow) - zsnowddz(ji,nsnow-1)*   &
                                 ((    zdelta(ji,nsnow-1) *snowheat(ji,nsnow)/     &
                                 snowdz_old(ji,nsnow)) +                         &
                                 ((1.0-zdelta(ji,nsnow-1))*snowheat(ji,nsnow-1)    &
                                 /snowdz_old(ji,nsnow-1)) )
          !
          zsnowgrainn(ji,nsnow)  = ( snowdz_old(ji,nsnow)*snowgrain(ji,nsnow) -    &
                                   zsnowddz(ji,nsnow-1)*                         &
                                   (     zdelta(ji,nsnow-1) *snowgrain(ji,nsnow) +     &
                                   (1.0-zdelta(ji,nsnow-1))*snowgrain(ji,nsnow-1) ) ) &
                                  /snowdz(ji,nsnow)


          !
          zsnowzo(ji,1)     = snowdz_old(ji,1)
          zsnowzn(ji,1)     = snowdz(ji,1)
          !
          DO jj=2,nsnow
             zsnowzo(ji,jj) = zsnowzo(ji,jj-1) + snowdz_old(ji,jj)
             zsnowzn(ji,jj) = zsnowzn(ji,jj-1) + snowdz(ji,jj)
          ENDDO


          DO jj=2,nsnow-1

             !first diagonise where the new snow layer lies in the old snow discretization             
             DO jjj=nsnow,1,-1

                !upper bound of the snow layer
                IF (zsnowzn(ji,jj-1) .LE. zsnowzo(ji,jjj)) THEN
                   locflag(jj,1) = jjj
                ENDIF

                !lower bound of the snow layer
                IF (zsnowzn(ji,jj)   .LE. zsnowzo(ji,jjj)) THEN
                   locflag(jj,2) = jjj
                ENDIF

             ENDDO

             !to interpolate
             ! when heavy snow occurred
             IF (locflag(jj,1) .EQ. locflag(jj,2)) THEN 

                zsnowrhon(ji,jj) = snowrho(ji,locflag(jj,1))

                zsnowheatn(ji,jj) = snowheat(ji,locflag(jj,1))*snowdz(ji,jj)/snowdz_old(ji,locflag(jj,1)) 

                zsnowgrainn(ji,jj) = snowgrain(ji,locflag(jj,1)) 

             ELSE 

                !snow density

                zsnowrhon(ji,jj) = snowrho(ji,locflag(jj,1)) * &
                                      (zsnowzo(ji,locflag(jj,1))-zsnowzn(ji,jj-1)) + &
                                      snowrho(ji,locflag(jj,2)) * &
                                      (zsnowzn(ji,jj)-zsnowzo(ji,locflag(jj,2)-1))

                DO jjj=locflag(jj,1),locflag(jj,2)-1
                   zsnowrhon(ji,jj)=zsnowrhon(ji,jj) + &
                   (jjj-locflag(jj,1))*snowrho(ji,jjj)*snowdz_old(ji,jjj)  
                ENDDO

                zsnowrhon(ji,jj) = zsnowrhon(ji,jj) / snowdz(ji,jj)


                !snow heat

                zsnowheatn(ji,jj) = snowheat(ji,locflag(jj,1)) * &
                                 (zsnowzo(ji,locflag(jj,1))-zsnowzn(ji,jj-1))/snowdz_old(ji,locflag(jj,1)) + &
                                       snowheat(ji,locflag(jj,2)) * &
                                 (zsnowzn(ji,jj)-zsnowzo(ji,locflag(jj,2)-1))/snowdz_old(ji,locflag(jj,2))

                DO jjj=locflag(jj,1),locflag(jj,2)-1
                   zsnowheatn(ji,jj)=zsnowheatn(ji,jj) + &
                                        (jjj-locflag(jj,1))*snowheat(ji,jjj)
                ENDDO



                !snow grain
                zsnowgrainn(ji,jj) = snowgrain(ji,locflag(jj,1)) * &
                                      (zsnowzo(ji,locflag(jj,1))-zsnowzn(ji,jj-1)) + &
                                      snowgrain(ji,locflag(jj,2)) * &
                                      (zsnowzn(ji,jj)-zsnowzo(ji,locflag(jj,2)-1))

                DO jjj=locflag(jj,1),locflag(jj,2)-1
                   zsnowgrainn(ji,jj)=zsnowgrainn(ji,jj) + &
                   (jjj-locflag(jj,1))*snowgrain(ji,jjj)*snowdz_old(ji,jjj)
                ENDDO

                zsnowgrainn(ji,jj) = zsnowgrainn(ji,jj) / snowdz(ji,jj)


             ENDIF
                        
          ENDDO
          snowrho(ji,:)    = zsnowrhon(ji,:)
          snowheat(ji,:)   = zsnowheatn(ji,:)
          snowgrain(ji,:)  = zsnowgrainn(ji,:)

     ENDIF

          ! Vanishing or very thin snowpack check:
          ! -----------------------------------------
          !
          ! NOTE: ONLY for very shallow snowpacks, mix properties (homogeneous):
          ! this avoids problems related to heat and mass exchange for
          ! thin layers during heavy snowfall or signifigant melt: one
          ! new/old layer can exceed the thickness of several old/new layers.
          ! Therefore, mix (conservative):
          !
          ! modified by Tao Wang
            IF (psnow > 0 .AND. (psnow < xsnowcritd .OR. snowdz_old(ji,1) &
            & .eq. 0 .OR. snowdz_old(ji,2) .eq. 0 .OR. snowdz_old(ji,3) .eq. 0)) THEN
                zsumheat(ji) = SUM(snowheat(ji,:))
                zsumswe(ji)  = SUM(snowrho(ji,:)*snowdz_old(ji,:))
                zsumgrain(ji)= SUM(snowgrain(ji,:)*snowdz_old(ji,:))
                zsnowmix_delta(ji) = 1.0
                DO jj=1,nsnow
                   zsnowheatn(ji,jj)  = zsnowmix_delta(ji)*(zsumheat(ji)/nsnow)
                   snowdz(ji,jj)    = zsnowmix_delta(ji)*(psnow/nsnow) 
                   zsnowrhon(ji,jj)   = zsnowmix_delta(ji)*(zsumswe(ji)/psnow)
                   zsnowgrainn(ji,jj) = zsnowmix_delta(ji)*(zsumgrain(ji)/psnow)
                ENDDO
          ! Update mass (density and thickness), heat and grain size:
          ! ------------------------------------------------------------
          !
          snowrho(ji,:)    = zsnowrhon(ji,:)
          snowheat(ji,:)   = zsnowheatn(ji,:)
          snowgrain(ji,:)  = zsnowgrainn(ji,:)

        ENDIF

     ENDDO


   END SUBROUTINE explicitsnow_transf

  
!!
!================================================================================================================================
!! SUBROUTINE   : explicitsnow_fall
!!
!>\BRIEF    Computes snowfall    
!!                
!! DESCRIPTION  : 
!routine. 
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================
 
   SUBROUTINE explicitsnow_fall(kjpindex,precip_snow,temp_air,u,v,totfrac_nobio,snowrho,snowdz,&
                        & snowheat,snowgrain,snowtemp,psnowhmass)

       !! 0.1 Input variables

       INTEGER(i_std),INTENT(in)                              :: kjpindex            !! Domain size
       REAL(r_std),DIMENSION(kjpindex),INTENT(in)             :: precip_snow         !! Snow rate (SWE) (kg/m2 per dt_sechiba)
       REAL(r_std),DIMENSION(kjpindex),INTENT(in)             :: temp_air            !! Air temperature
       REAL(r_std),DIMENSION(kjpindex),INTENT(in)             :: u,v                 !! Horizontal wind speed
       REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(in)       :: snowtemp            !! Snow temperature
       REAL(r_std),DIMENSION(kjpindex),INTENT(in)             :: totfrac_nobio
       !! 0.2 Output variables

       REAL(r_std), DIMENSION(kjpindex),INTENT(out)           :: psnowhmass          !! Heat content of snowfall (J/m2)

       !! 0.3 Modified variables
 
       REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)    :: snowrho             !! Snow density profile (kg/m3)
       REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)    :: snowdz              !! Snow layer thickness profile (m)
       REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)    :: snowheat            !! Snow heat content/enthalpy (J/m2)
       REAL(r_std),DIMENSION(kjpindex,nsnow),INTENT(inout)    :: snowgrain           !! Snow grain size

       !! 0.4 Local variables

       REAL(r_std), DIMENSION(kjpindex)                       :: rhosnew             !! Snowfall density
       REAL(r_std), DIMENSION(kjpindex)                       :: dsnowfall           !! Snowfall thickness (m)
       REAL(r_std), DIMENSION(kjpindex,nsnow)                 :: snowdz_old
       REAL(r_std), DIMENSION(kjpindex)                       :: snow_depth,snow_depth_old,newgrain
       REAL(r_std)                                            :: snowfall_delta,speed
       INTEGER(i_std)                                         :: ji,jj

       !! 1. initialize the variables

       snowdz_old = snowdz
       DO ji=1,kjpindex
                 snow_depth(ji) = SUM(snowdz(ji,:))
       ENDDO

       snow_depth_old = snow_depth
 
       snowfall_delta = 0.0 

       !! 2. incorporate snowfall into snowpack 
       DO ji = 1, kjpindex
   
       speed = MAX(min_wind, SQRT (u(ji)*u(ji) + v(ji)*v(ji)))
        
          ! new snow fall on snowpack 
          ! NOTE: when the surface temperature is zero, it means that snowfall has no
          ! heat content but it can be used for increasing the thickness and changing the density (maybe it is a bug)
          psnowhmass(ji) = 0.0
          IF ( (precip_snow(ji) .GT. 0.0) ) THEN

             !calculate

             psnowhmass(ji) = precip_snow(ji)*(un-totfrac_nobio(ji))* &
                                (xci*(snowtemp(ji,1)-tp_00)-chalfu0)
            
             ! Snowfall density: Following CROCUS (Pahaut 1976)
             !
             rhosnew(ji)   = MAX(xrhosmin, snowfall_a_sn + snowfall_b_sn*(temp_air(ji)-tp_00)+         &
                   snowfall_c_sn*SQRT(speed))

             ! Augment total pack depth:
             ! Note that dsnowfall(ji) can be equal to zero if totfrac_nobio(ji)=1
             dsnowfall(ji) = (precip_snow(ji)*(un-totfrac_nobio(ji)))/rhosnew(ji) !snowfall thickness (m)

             snow_depth(ji) = snow_depth(ji) + dsnowfall(ji) 


             ! Fresh snowfall changes the snowpack density and liquid content in uppermost layer 

             IF (dsnowfall(ji) .GT. zero) THEN
               snowrho(ji,1) = (snowdz(ji,1)*snowrho(ji,1) + dsnowfall(ji)*rhosnew(ji))/     &
                                (snowdz(ji,1)+dsnowfall(ji))

               snowdz(ji,1) = snowdz(ji,1) + dsnowfall(ji)

               ! Add energy of snowfall to snowpack:
               ! Update heat content (J/m2) (therefore the snow temperature
               ! and liquid content):
               !
               snowheat(ji,1)  = snowheat(ji,1) + psnowhmass(ji)
               !
               ! Incorporate snowfall grain size:
               !
               newgrain(ji)    = MIN(dgrain_new_max, snow3lgrain_0d(rhosnew(ji)))
               
               snowgrain(ji,1) = (snowdz_old(ji,1)*snowgrain(ji,1) + dsnowfall(ji)*newgrain(ji))/ &
                    snowdz(ji,1)
            ENDIF
          ELSE
            dsnowfall(ji) = 0.
          ENDIF

          ! new snow fall on snow free surface. 
          ! we use the linearization for the new snow fall on snow-free ground 

          IF ( (dsnowfall(ji) .GT. zero) .AND. (snow_depth_old(ji) .EQ. zero) ) THEN

             snowfall_delta = 1.0
              
             DO jj=1,nsnow

                snowdz(ji,jj)   = snowfall_delta*(dsnowfall(ji)/nsnow) + &
                                  (1.0-snowfall_delta)*snowdz(ji,jj)

                snowheat(ji,jj)   = snowfall_delta*(psnowhmass(ji)/nsnow) + &
                                  (1.0-snowfall_delta)*snowheat(ji,jj)

                snowrho(ji,jj)    = snowfall_delta*rhosnew(ji)            + &
                                  (1.0-snowfall_delta)*snowrho(ji,jj)

                snowgrain(ji,jj)=   snowfall_delta*newgrain(ji)           + &
                                  (1.0-snowfall_delta)*snowgrain(ji,jj)

             ENDDO


          ENDIF


       ENDDO 

     END SUBROUTINE explicitsnow_fall

!!
!================================================================================================================================
!! SUBROUTINE   : explicitsnow_gone
!!
!>\BRIEF        Check whether snow is gone 
!!                
!! DESCRIPTION  : If so, set thickness (and therefore mass and heat) and liquid
!!                content to zero, and adjust fluxes of water, evaporation and
!!                heat into underlying surface. 
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================

   SUBROUTINE explicitsnow_gone(kjpindex,pgflux,&
                        snowheat,snowtemp,snowdz,snowrho,snowliq,grndflux,snowmelt,soilflxresid)

     !! 0.1 Input variables

     INTEGER(i_std), INTENT(in)                                 :: kjpindex     !! Domain size
     REAL(r_std),DIMENSION (kjpindex), INTENT (in)              :: pgflux       !! Net energy into snow pack(w/m2)
     REAL(r_std),DIMENSION (kjpindex,nsnow),INTENT(in)          :: snowheat     !! Snow heat content
     REAL(r_std),DIMENSION(kjpindex),INTENT(in)                 :: soilflxresid

     !! 0.2 Output variables

     !! 0.3 Modified variables

     REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)     :: snowtemp     !! Snow temperature
     REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)     :: snowdz       !! Snow depth
     REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(inout)      :: snowrho      !! Snow density
     REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(inout)      :: snowliq      !! Liquid water content
     REAL(r_std),DIMENSION(kjpindex), INTENT(inout)             :: grndflux     !! Soil/snow interface heat flux (W/m2)
     REAL(r_std),DIMENSION(kjpindex),INTENT(inout)              :: snowmelt     !! Snow melt
     REAL(r_std),DIMENSION(kjpindex)                            :: thrufal      !! Water leaving snowpack(kg/m2/s)

     !! 0.4 Local variables

     INTEGER(i_std)                                             :: ji,jj
     REAL(r_std),DIMENSION(kjpindex)                            :: snowgone_delta
     REAL(r_std),DIMENSION (kjpindex)                           :: totsnowheat  !!snow heat content at each layer 
     REAL(r_std),DIMENSION(kjpindex)                            :: snowdepth_crit

     ! first caculate total snowpack snow heat content
     snowgone_delta(:) = un
     thrufal(:)=0.0
     snowmelt(:)=0
     totsnowheat(:)  = SUM(snowheat(:,:),2) 
     
     DO ji = 1, kjpindex 

           IF ( (SUM(snowdz(ji,:)) .GT. zero)) THEN

              IF( pgflux(ji) + soilflxresid(ji) >= (-totsnowheat(ji)/dt_sechiba) ) THEN

                 grndflux(ji) = pgflux(ji) + (totsnowheat(ji)/dt_sechiba) + soilflxresid(ji) 

                 thrufal(ji)=SUM(snowrho(ji,:)*snowdz(ji,:))

                 snowgone_delta(ji) = 0.0       

                 snowmelt(ji) = snowmelt(ji)+thrufal(ji)

              ENDIF

              ! update of snow state (either still present or not)

              DO jj=1,nsnow
                 snowdz(ji,jj)  =   snowdz(ji,jj) *snowgone_delta(ji)
                 snowliq(ji,jj)   =   snowliq(ji,jj) *snowgone_delta(ji)
                 snowtemp(ji,jj) = (1.0-snowgone_delta(ji))*tp_00 + snowtemp(ji,jj)*snowgone_delta(ji)
              ENDDO

           ENDIF
     ENDDO 

   END SUBROUTINE explicitsnow_gone

!================================================================================================================================
!! SUBROUTINE   : explicitsnow_melt_refrz
!!
!>\BRIEF        Computes snow melt and refreezing processes within snowpack
!!                
!! DESCRIPTION  : 
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================

   SUBROUTINE explicitsnow_melt_refrz(kjpindex,precip_rain,pgflux,soilcap,&
                     snowtemp,snowdz,snowrho,snowliq,snowmelt,grndflux,temp_air,soilflxresid)

      !! 0.1 Input variables

      INTEGER(i_std), INTENT (in)                             :: kjpindex     !! Domain size
      REAL(r_std),DIMENSION (kjpindex,nsnow)                  :: pcapa_snow   !! Heat capacity for snow
      REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: precip_rain  !! Rainfall      
      REAL(r_std),DIMENSION (kjpindex), INTENT(in)            :: temp_air     !! Air temperature
      REAL(r_std),DIMENSION (kjpindex),INTENT(in)             :: pgflux       !! Net energy into snowpack(w/m2)
      REAL(r_std),DIMENSION (kjpindex),INTENT(in)             :: soilcap      !! Soil heat capacity
      REAL(r_std),DIMENSION (kjpindex),INTENT(in)             :: soilflxresid

      !! 0.2 Output variables

      !! 0.3 Modified variables

      REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowtemp     !! Snow temperature 
      REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)  :: snowdz       !! Snow depth
      REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowrho      !! Snow layer density
      REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT(inout)   :: snowliq      !! Liquid water content
      REAL(r_std),DIMENSION(kjpindex),INTENT(inout)           :: grndflux     !! Net energy input to soil 
      REAL(r_std),DIMENSION (kjpindex), INTENT(inout)         :: snowmelt     !! Snowmelt

      !! 0.4 Local variables

      REAL(r_std),DIMENSION (kjpindex)                        :: meltxs       !! Residual snowmelt energy applied to underlying soil
      REAL(r_std)                                             :: enerin,melttot,hrain 
      REAL(r_std),DIMENSION (nsnow)                           :: zsnowlwe
      REAL(r_std),DIMENSION (nsnow)                           :: flowliq
      REAL(r_std),DIMENSION (kjpindex)                        :: snowmass
      REAL(r_std),DIMENSION (nsnow)                           :: zphase       !! Phase change (from ice to water) (J/m2)
      REAL(r_std),DIMENSION (nsnow)                           :: zphase2      !! Phase change (from water to ice)
      REAL(r_std),DIMENSION (nsnow)                           :: zphase3      !! Phase change related with net energy input to snowpack
      REAL(r_std),DIMENSION (nsnow)                           :: zsnowdz      !! Snow layer depth
      REAL(r_std),DIMENSION (nsnow)                           :: zsnowmelt    !! Snow melt (liquid water) (m)
      REAL(r_std),DIMENSION (nsnow)                           :: zsnowtemp
      REAL(r_std),DIMENSION (nsnow)                           :: zmeltxs      !! Excess melt
      REAL(r_std),DIMENSION (nsnow)                           :: zwholdmax    !! Maximum liquid water holding (m)
      REAL(r_std),DIMENSION (nsnow)                           :: zcmprsfact   !! Compression factor due to densification from melting
      REAL(r_std),DIMENSION (nsnow)                           :: zscap        !! Snow heat capacity (J/m3 K)
      REAL(r_std),DIMENSION (nsnow)                           :: zsnowliq     !! (m)
      REAL(r_std),DIMENSION (nsnow)                           :: snowtemp_old
      REAL(r_std),DIMENSION (0:nsnow)                         :: zflowliqt    !!(m)
      REAL(r_std)                                             :: zrainfall,zpcpxs
      REAL(r_std)                                             :: ztotwcap
      REAL(r_std),DIMENSION(kjpindex,nsnow)                   :: snowdz_old,snowliq_old
      INTEGER(i_std)                                          :: jj,ji, iv
      REAL(r_std),DIMENSION(nsnow)                            :: snowdz_old2
      REAL(r_std),DIMENSION(nsnow)                            :: zsnowrho 
    
      !initialize
      snowdz_old = snowdz
      snowliq_old = snowliq

      DO ji = 1, kjpindex


         snowmass(ji) = SUM(snowrho(ji,:) * snowdz(ji,:))
         IF ((snowmass(ji) .GT. 0.)) THEN

           !! 1 snow melting due to positive snowpack snow temperature
 
             !! 1.0 total liquid equivalent water content of each snow layer

               zsnowlwe(:) = snowrho(ji,:) * snowdz(ji,:)/ ph2o 

             !! 1.1 phase change (J/m2)

               pcapa_snow(ji,:) = snowrho(ji,:)*xci

               !we should make a change in order to avoid the remaining energy
               !still be left without melting the underlying two snow layers

               zphase(:)  = MIN(pcapa_snow(ji,:)*MAX(0.0, snowtemp(ji,:)-tp_00)*      &
                                snowdz(ji,:),                                       &
                            MAX(0.0,zsnowlwe(:)-snowliq(ji,:))*chalfu0*ph2o)

             !! 1.2 update snow liq water and temperature if melting

               zsnowmelt(:) = zphase(:)/(chalfu0*ph2o)

             !! 1.3 cool off the snow layer temperature due to melt

               zsnowtemp(:) = snowtemp(ji,:) - zphase(:)/(pcapa_snow(ji,:)* snowdz(ji,:))

               snowtemp(ji,:) = MIN(tp_00, zsnowtemp(:))

               zmeltxs(:)   = (zsnowtemp(:)-snowtemp(ji,:))*pcapa_snow(ji,:)*snowdz(ji,:)

             !! 1.4 loss of snowpack depth and liquid equivalent water

               zwholdmax(:) = snow3lhold_1d(snowrho(ji,:),snowdz(ji,:)) ! 1 dimension

               zcmprsfact(:) = (zsnowlwe(:)-MIN(snowliq(ji,:)+zsnowmelt(:),zwholdmax(:)))/ &
                               (zsnowlwe(:)-MIN(snowliq(ji,:),zwholdmax(:)))

               snowdz(ji,:)    = snowdz(ji,:)*zcmprsfact(:)

               snowrho(ji,:)     = zsnowlwe(:)*ph2o/snowdz(ji,:)

               snowliq(ji,:)   = snowliq(ji,:) + zsnowmelt(:)


           !! 2 snow refreezing process
              !! 2.1 freeze liquid water in any layer
               zscap(:)     = snowrho(ji,:)*xci  !J K-1 m-3
               zphase2(:)    = MIN(zscap(:)*                                          &
                                MAX(0.0, tp_00 - snowtemp(ji,:))*snowdz(ji,:),             &
                                snowliq(ji,:)*chalfu0*ph2o)

               ! warm layer and reduce liquid if freezing occurs
               zsnowdz(:) = MAX(xsnowdmin/nsnow, snowdz(ji,:))
               snowtemp_old(:) = snowtemp(ji,:) 
               snowtemp(ji,:) = snowtemp(ji,:) + zphase2(:)/(zscap(:)*zsnowdz(:))
               
               ! Reduce liquid portion if freezing occurs:
               snowliq(ji,:) = snowliq(ji,:) - ( (snowtemp(ji,:)-snowtemp_old(:))*       &
               zscap(:)*zsnowdz(:)/(chalfu0*ph2o) )
               snowliq(ji,:) = MAX(snowliq(ji,:), 0.0)
           !! 3. thickness change due to snowmelt in excess of holding capacity
               zwholdmax(:) = snow3lhold_1d(snowrho(ji,:),snowdz(ji,:)) ! 1 dimension
               flowliq(:) = MAX(0.,(snowliq(ji,:)-zwholdmax(:)))
               snowliq(ji,:)  = snowliq(ji,:) - flowliq(:)
               snowdz(ji,:) = snowdz(ji,:) - flowliq(:)*ph2o/snowrho(ji,:)
               ! to prevent possible very small negative values (machine
               ! prescision as snow vanishes
               snowdz(ji,:) = MAX(0.0, snowdz(ji,:)) 

           !! 4. liquid water flow
               ztotwcap = SUM(zwholdmax(:)) 
               ! Rain entering snow (m):
               !ORCHIDEE has assumed that all precipitation entering snow has
               !left the snowpack and finally become runoff in hydrolc_soil. Here we just put zrainfall as 0.0
               !!zrainfall = precip_rain(ji)/ph2o ! rainfall (m)
               zrainfall = 0.0

               zflowliqt(0) = MIN(zrainfall,ztotwcap)
               ! Rain assumed to directly pass through the pack to runoff (m):
               zpcpxs = zrainfall - zflowliqt(0)
               !
               DO jj=1,nsnow
                  zflowliqt(jj) = flowliq(jj)
               ENDDO

               ! translated into a density increase:
               flowliq(:) = 0.0                ! clear this array for work
               zsnowliq(:) = snowliq(ji,:)      ! reset liquid water content
               !

               DO jj=1,nsnow
                  snowliq(ji,jj)  = snowliq(ji,jj) + zflowliqt(jj-1)
                  flowliq(jj)   = MAX(0.0, (snowliq(ji,jj)-zwholdmax(jj)))
                  snowliq(ji,jj)  = snowliq(ji,jj) - flowliq(jj)

                  !Modified by Tao Wang based on previous ISBA-ES scheme
                  snowrho(ji,jj)  = snowrho(ji,jj)  + (snowliq(ji,jj) - zsnowliq(jj))*       &
                                        & ph2o/MAX(xsnowdmin/nsnow,snowdz(ji,jj))

                  zflowliqt(jj) = zflowliqt(jj) + flowliq(jj)  
               ENDDO

               snowmelt(ji)  = snowmelt(ji) + (zflowliqt(nsnow) + zpcpxs) *  ph2o

             ! excess heat from melting, using it to warm underlying ground to conserve energy
               meltxs(ji) = SUM(zmeltxs(:))/dt_sechiba  ! (W/m2)

             ! energy flux into the soil 
               grndflux(ji) = grndflux(ji) + meltxs(ji)  

         ENDIF

      ENDDO

    END SUBROUTINE explicitsnow_melt_refrz

!================================================================================================================================
!! SUBROUTINE   : explicitsnow_levels
!!
!>\BRIEF        Computes snow discretization based on given total snow depth
!!                
!! DESCRIPTION  : 
!! RECENT CHANGE(S) : None 
!!
!! MAIN OUTPUT VARIABLE(S): None
!!
!! REFERENCE(S) : 
!!
!! FLOWCHART    : None
!! \n
!_
!================================================================================================================================ 
  SUBROUTINE explicitsnow_levels( kjpindex,snow_thick, snowdz)
    
    !! 0.1 Input variables
    INTEGER(i_std), INTENT(in)                                     :: kjpindex     !! Domain size
    REAL(r_std),DIMENSION (kjpindex),   INTENT (in)                :: snow_thick   !! Total snow depth

    !! 0.2 Output variables

    !! 0.3 Modified variables

    REAL(r_std), DIMENSION (kjpindex,nsnow), INTENT(inout)         :: snowdz                           !! Snow depth

    !! 0.4 Local variables

    INTEGER(i_std)                                                 :: il,it,ji
    INTEGER(i_std)                                                 :: i,j, ix
    REAL(r_std), DIMENSION(kjpindex)                               :: xi, xf
    REAL(r_std), PARAMETER, DIMENSION(3)                           :: ZSGCOEF1  = (/0.25, 0.50, 0.25/) !! Snow grid parameters 
    REAL(r_std), PARAMETER, DIMENSION(2)                           :: ZSGCOEF2  = (/0.05, 0.34/)       !! Snow grid parameters 
    REAL(r_std), PARAMETER                                         :: ZSNOWTRANS = 0.20                !! Minimum total snow depth at which surface layer thickness is constant (m)
    REAL(r_std), PARAMETER                                         :: XSNOWCRITD = 0.03                !! (m)

    DO ji=1,kjpindex
       IF ( snow_thick(ji) .LE. (XSNOWCRITD+0.01)) THEN

        snowdz(ji,1) = MIN(0.01, snow_thick(ji)/nsnow)
        snowdz(ji,3) = MIN(0.01, snow_thick(ji)/nsnow)
        snowdz(ji,2) = snow_thick(ji) - snowdz(ji,1) - snowdz(ji,3)
   
       ENDIF 
    ENDDO

    WHERE ( snow_thick(:) .LE. ZSNOWTRANS .AND. &
            snow_thick(:) .GT. (XSNOWCRITD+0.01) )
       !
        snowdz(:,1) = snow_thick(:)*ZSGCOEF1(1)
        snowdz(:,2) = snow_thick(:)*ZSGCOEF1(2) 
        snowdz(:,3) = snow_thick(:)*ZSGCOEF1(3)
       !
    END WHERE

     DO ji = 1,kjpindex
      IF (snow_thick(ji) .GT. ZSNOWTRANS) THEN
          snowdz(ji,1) = ZSGCOEF2(1)
          snowdz(ji,2) = (snow_thick(ji)-ZSGCOEF2(1))*ZSGCOEF2(2) + ZSGCOEF2(1)
        !When using simple finite differences, limit the thickness
        !factor between the top and 2nd layers to at most 10
          snowdz(ji,2)  = MIN(10*ZSGCOEF2(1),  snowdz(ji,2) )
          snowdz(ji,3)  = snow_thick(ji) - snowdz(ji,2) - snowdz(ji,1)

      ENDIF
    ENDDO
      

  END SUBROUTINE explicitsnow_levels

!!
!================================================================================================================================
!! SUBROUTINE   : explicitsnow_profile
!!
!>\BRIEF        
!!
!! DESCRIPTION  : In this routine solves the numerical soil thermal scheme, ie calculates the new soil temperature profile.
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
  SUBROUTINE explicitsnow_profile (kjpindex, cgrnd_snow,dgrnd_snow,lambda_snow,temp_sol_new, snowtemp,snowdz,temp_sol_add)

  !! 0. Variables and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std), INTENT(in)                               :: kjpindex     !! Domain size (unitless)
    REAL(r_std),DIMENSION (kjpindex), INTENT (in)            :: temp_sol_new !! skin temperature
    REAL(r_std), DIMENSION (kjpindex,nsnow),INTENT (in)      :: cgrnd_snow   !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex,nsnow),INTENT (in)      :: dgrnd_snow   !! Integration coefficient for snow numerical scheme
    REAL(r_std), DIMENSION (kjpindex),INTENT(in)             :: lambda_snow  !! Coefficient of the linear extrapolation of surface temperature 
    REAL(r_std), DIMENSION (kjpindex,nsnow),INTENT(in)       :: snowdz       !! Snow layer thickness
    REAL(r_std), DIMENSION (kjpindex),INTENT(inout)          :: temp_sol_add !! Additional energy to melt snow for snow ablation case (K)

    !! 0.3 Modified variable

    !! 0.2 Output variables

    !! 0.3 Modified variables
    REAL(r_std),DIMENSION (kjpindex,nsnow), INTENT (inout)   :: snowtemp

    !! 0.4 Local variables

    INTEGER(i_std)                                           :: ji, jg
!_
!================================================================================================================================
  !! 1. Computes the snow temperatures ptn.
    DO ji = 1,kjpindex
      IF (SUM(snowdz(ji,:)) .GT. 0) THEN
        snowtemp(ji,1) = (lambda_snow(ji) * cgrnd_snow(ji,1) + (temp_sol_new(ji)+temp_sol_add(ji))) / &
             (lambda_snow(ji) * (un - dgrnd_snow(ji,1)) + un)
        temp_sol_add(ji) = zero 
        DO jg = 1,nsnow-1
            snowtemp(ji,jg+1) = cgrnd_snow(ji,jg) + dgrnd_snow(ji,jg) * snowtemp(ji,jg)
        ENDDO
      ENDIF
    ENDDO
    IF (printlev>=3) WRITE (numout,*) ' explicitsnow_profile done '

  END SUBROUTINE explicitsnow_profile

END MODULE explicitsnow
