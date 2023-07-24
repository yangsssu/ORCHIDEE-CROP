MODULE vertical_soil
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_parameters/vertical_soil.f90 $
!! $Date: 2016-03-15 15:22:11 +0100 (Tue, 15 Mar 2016) $
!! $Revision: 3270 $

  USE defprec
  USE ioipsl_para
  USE vertical_soil_var

  IMPLICIT NONE


  PUBLIC :: vertical_soil_init

CONTAINS
!! ================================================================================================================================
!! SUBROUTINE   : vertical_soil_init
!!
!>\BRIEF        
!!
!! DESCRIPTION  : To define the total number of layers 
!!                for hydrology model (nslm),  
!!                and for thermodynamics model ngrnd.
!!
!! We have 2 type of levels in the hydrology and the thermodynamics :
!!  (1) the nodes where the scalar variables are computed.
!!  (2) the intermediate levels: these are the bounds of the layers and thus the
!!      contact point between layer i and i+1.
!!
!! Situation in hydrol.f90
!!   The CWRR model only uses two variables to describe the vertical discretization :
!!   (1) znh : the depth of the nodes where soil moisture is computed. The nodes are not
!!     supposed to be in the middle of the layer. For the first and last layers the
!!     nodes are put onto the intermediate level. At 0 for the first layer and
!!     zmaxh for the last layer.
!!   (2) dnh : the distance between two nodes
!!
!! Situation in thermosoil.f90 :
!!   For the thermodynamics other variables are needed to describe the vertical structure :
!!   (1) znt(znh) : the depth of the nodes are the same as in hydrol.f90 except for
!!     the first and last levels. In older versions than revision XXXX of thermosoil it is 
!!     computed with fz(i+1/2)
!!   (2) zlt (Zint) (zz_coef in thermosoil) : is the depth of the intermediate levels. 
!!     In revision before XXXX of thermosoil.f90 it's computed by fz(i). 
!!     Since revision XXXX, in the new formulation (vertical.f90) it is computed as 
!!     zint(i) = (znh(i)+znh(i+1))/2
!!   (3) dlt(dz_tmp) (dz2 in thermosoil): This is the thickness of the layer, i.e the
!!     distance between the top and bottom of the layer. Thus it is given by
!!     zint(i+1)-zint(i). It should not be confused with dnh in hydrology which is the
!!     distance between nodes.
!!
!! in standard hydrology model (de Rosnay Patricia, 2000; depth_topthickness = 9.77517107e-04, zmaxh=2.0, refinebottom = .FALSE.):
!!
!! Example for standard depth of the hydrology
!! Number of layers in hydrol =  11
!! znh = depth of nodes
!! [  0.00000000e+00   1.95503421e-03   5.86510264e-03 1.36852395e-02
!!    2.93255132e-02   6.06060606e-02   1.23167155e-01 2.48289345e-01
!!    4.98533725e-01   9.99022483e-01   2.00000000e+00 ]
!! dnh = internode distance
!! [ 0.          0.00195503  0.00391007  0.00782014  0.01564027 0.03128055
!!   0.06256109  0.12512219  0.25024438  0.50048876  1.00097752 ]
!! dlh = soil layer thickness
!! [ 0.00097752  0.00293255  0.0058651   0.01173021  0.02346041 0.04692082
!!   0.09384164  0.18768328  0.37536657  0.75073314  0.50048876 ]
!! hcum = depth of soil layer bottom
!! [  9.77517107e-04   3.91006843e-03   9.77517107e-03 2.15053764e-02
!!    4.49657869e-02   9.18866081e-02   1.85728250e-01 3.73411535e-01
!!    7.48778104e-01   1.49951124e+00   2.00000000e+00 ]
!!
!!
!! In thermal model (an example of: depth_topthickness = 9.77517107e-04, zmaxt=10, depth_geom=10, refinebottom = .FALSE.): 
!! Number of layers in thermosoil =  19
!! The approximate maximal depth for thermosoil =  10.0078201332
!! The actual maximal depth for thermosoil =  10.0
!! znt= 
!! [ 4.88758554e-04   1.95503421e-03   5.86510264e-03 1.36852395e-02
!!   2.93255132e-02   6.06060606e-02   1.23167155e-01 2.48289345e-01
!!   4.98533725e-01   9.99022483e-01   1.74975562e+00 2.50048876e+00
!!   3.50146627e+00   4.50244379e+00   5.50342131e+00 6.50439882e+00
!!   7.50537634e+00   8.50635386e+00   9.50733137e+00 ]
!! dlt= 
!! [ 9.77517107e-04   2.93255132e-03   5.86510264e-03 1.17302053e-02
!!   2.34604106e-02   4.69208211e-02   9.38416423e-02 1.87683285e-01
!!   3.75366569e-01   7.50733138e-01   5.00488758e-01 1.00097752e+00
!!   1.00097752e+00   1.00097752e+00   1.00097752e+00 1.00097752e+00
!!   1.00097752e+00   1.00097752e+00   9.93157383e-01 ]
!! zlt= 
!! [ 9.77517107e-04   3.91006843e-03   9.77517107e-03 2.15053764e-02
!!   4.49657869e-02   9.18866081e-02   1.85728250e-01 3.73411535e-01
!!   7.48778104e-01   1.49951124e+00   2.00000000e+00 3.00097752e+00
!!   4.00195503e+00   5.00293255e+00   6.00391007e+00 7.00488758e+00
!!   8.00586510e+00   9.00684262e+00   1.00000000e+01 ]
!!
!!
!! A simple figure below shows the discretization.                       ^
!!   '------' means interface; 'X' means node; '...' means many layers;  | means distance. 
!!                                                                       v
!! Keep in mind that the nodes are not necessarly in the middle of the layers. 
!! 
!!           Hydrology                     Thermodynamics
!!
!!    --------X------- znh(1) ^           ------------------- 1st   zlt(1)  ^
!!                           |                                                    |
!!                           |                   X        znt(1)               |depth_topthickness, dlt(1)  
!!                           |dnh(2)                                               |
!!    -----------------      |           ------------------- 2nd   zlt(2)  v   ^
!!                           |                                                        |
!!            X        znh(2) v  ^                X        znt(2)                   |dlt(2) 
!!                              |                                                     |
!!    -----------------         |dnh(3)   ------------------- 3rd   zlt(3)      v
!!                              | 
!!            X        znh(3)    v                X        znt(3)
!!
!!    ----------------- ...              ------------------- ...
!!
!!                      ...                      X           ...
!!
!!    --------X------- znh(nslm)        ------------------- nslm     zlt(nslm)
!!
!!                                               X        znt(nslm)
!!
!!                                       ------------------- nslm+1   zlt(nslm+1)
!!
!!                                               X        znt(nslm+1)
!! 
!!                                       ------------------- ...
!! 
!!                                               X        znt(ngrnd)
!!
!!                                       ------------------- ngrnd    zlt(ngrnd)
!!
!!
!! RECENT CHANGE(S): None
!!
!! MAIN OUTPUT VARIABLE(S): 
!!
!! REFERENCE(S) :
!!
!! FLOWCHART    :
!! \n
!_ ================================================================================================================================

  SUBROUTINE vertical_soil_init

    !! 0. Variable and parameter declaration
    !! 0.1 Local variables
    INTEGER(i_std), PARAMETER            :: nblayermax=500    !! Preset 500 for max. number of soil layers.
    INTEGER(i_std)                       :: i, irev, iref, ntmp, ier
    REAL(r_std)                          :: zgeoend           !! The node position where the geometrical increases of nodes stopped. (m)
    REAL(r_std)                          :: dgeoend           !! The distance of the node at zgeoend and the node above. (m)
    REAL(r_std)                          :: dcst              !! Related to refine at bottom. Work in progress...
    REAL(r_std)                          :: cstint            !! Related to refine at bottom. Work in progress...
    REAL(r_std)                          :: hh                !! Temporay variable, to calculate the layer thickness for temperature at zmaxh. 
    REAL(r_std)                          :: ratio             !! The ratio of geometric increas.
    INTEGER(i_std)                       :: nbrefine          !! Related to refine at bottom. Work in progress...
    INTEGER(i_std)                       :: nbcst             !! Related to refine at bottom. Work in progress...
    INTEGER(i_std)                       :: igeo              !! Related to refine at bottom. Work in progress...
    REAL(r_std), DIMENSION(nblayermax+1) :: ztmp              !! Depth at the node of each layer (m)
    REAL(r_std), DIMENSION(nblayermax+1) :: zint              !! Depth at the interface of each layer (m)
    REAL(r_std), DIMENSION(nblayermax+1) :: dtmp              !! Distance between the current node and the one above (m)
    REAL(r_std), DIMENSION(nblayermax+1) :: drefine           !! Related to refine at bottom. Work in progress...
    INTEGER(i_std), DIMENSION(1)         :: imin              !! Related to refine at bottom. Work in progress...

    !! Variables controling the vertical discretiztaion
    REAL(r_std)                          :: depth_topthickness   !! Thickness of the top Layer (m)
    REAL(r_std)                          :: depth_cstthickness   !! Depth at which constant layer thickness start (m)
    REAL(r_std)                          :: depth_geom           !! Depth at which we resume geometrical increases for temperature (m)
    REAL(r_std)                          :: ratio_geom_below     !! Ratio of the geometrical series defining the thickness below DEPTH_GEOM.
    LOGICAL                              :: refinebottom         !! Whether or not the hydrology layers will be refined towards the bottom.

    
    !! 1. Read parameters from run.def file
    
    !Config Key   = DEPTH_MAX_T
    !Config Desc  = Maximum depth of the soil thermodynamics
    !Config If    = 
    !Config Def   = 38 
    !Config Help  = Maximum depth of soil for temperature.
    !Config Units = m
    !For carbon permafrost, to get ndeep=32 layers, we set DEPTH_TMAX=38. and RATIO_GEOM_BELOW=1.05	
    zmaxt = 38.0
    CALL getin_p("DEPTH_MAX_T",zmaxt)
    
    !Config Key   = DEPTH_MAX_H
    !Config Desc  = Maximum depth of soil moisture
    !Config If    = 
    !Config Def   = 2.0 or 4.0 depending on hydrol_cwrr
    !Config Help  = Maximum depth of soil for soil moisture (CWRR).
    !Config Units = m
    ! Default value for CWRR (only CWRR enters this subroutineDEPTH_TMAX=38.)
    zmaxh=2.0
    CALL getin_p("DEPTH_MAX_H",zmaxh)
    
    ! Verification
    IF ( zmaxh > zmaxt) THEN
       CALL ipslerr_p(3,'vertical_soil_init',"ERROR : zmaxh needs to be smaller than zmaxt.", &
            "Correction : zmaxh set to zmaxt", " ")
    ENDIF
    
    !Config Key   = DEPTH_TOPTHICK
    !Config Desc  = Thickness of upper most Layer 
    !Config If    = 
    !Config Def   = 9.77517107e-04
    !Config Help  = Thickness of top hydrology layer for soil moisture (CWRR).
    !Config Units = m
    depth_topthickness = 9.77517107e-04
    CALL getin_p("DEPTH_TOPTHICK",depth_topthickness)
    
    !Config Key   = DEPTH_CSTTHICK
    !Config Desc  = Depth at which constant layer thickness start
    !Config If    = 
    !Config Def   = DEPTH_MAX_H 
    !Config Help  = Depth at which constant layer thickness start (smaller than zmaxh/2)
    !Config Units = m
    depth_cstthickness=zmaxh
    CALL getin_p("DEPTH_CSTTHICK",depth_cstthickness)
    
    IF ( (depth_cstthickness /= zmaxh) .AND. (depth_cstthickness > zmaxh/2.0) ) THEN
       CALL ipslerr_p(2,'vertical_soil_init',"ERROR : depth_cstthickness needs to be smaller than zmaxh/2.0", &
            "Correction : Constant thickness disabled", " ")
       depth_cstthickness = zmaxh
    ENDIF
    
    ! REFINEBOTTOM option is under work... This option can not be set from the parameter file for the moment. 
    ! All the related code for refinebottom except this getin are found in this module but needs to be tested.
!!$  !Config Key   = REFINEBOTTOM
!!$  !Config Desc  = Depth at which the hydrology layers will be refined towards the bottom.
!!$  !Config If    = 
!!$  !Config Def   = .FALSE.
!!$  !Config Help  = Depth at which the hydrology layers will be refined towards the bottom.
!!$  !               This is important when lower boundary conditions is different from a free drainage.
!!$  !Config Units = -
!!$  ! 
    refinebottom = .FALSE.
    CALL getin_p("REFINEBOTTOM",refinebottom)
    
    !Config Key   = DEPTH_GEOM
    !Config Desc  = Depth at which we resume geometrical increases for temperature
    !Config If    = 
    !Config Def   = DEPTH_MAX_H 
    !Config Help  = Depth at which the thickness increases again for temperature.
    !               This depth has to be deeper than the bottom of the hydrology.
    !Config Units = m
    depth_geom=zmaxh
    CALL getin_p("DEPTH_GEOM",depth_geom)
    
    IF ( depth_geom < zmaxh ) THEN
       CALL ipslerr_p(3,'vertical_soil_init',"ERROR : depth_geom needs to be larger than zmaxh", &
            "Correction : setting depth_geom to zmaxh", " ")
    ENDIF
    
    !Config Key   = RATIO_GEOM_BELOW
    !Config Desc  = Ratio of the geometrical series defining the thickness below DEPTH_GEOM
    !Config If    = 
    !Config Def   = 2
    !Config Help  = Ratio of the geometrical series defining the thickness below DEPTH_GEOM.
    !               This parameter allows to cover the depth needed for temperature with fewer layers.
    !Config Units = -
!    ratio_geom_below=2
!For carbon permafrost, to get ndeep=32 layers, we set DEPTH_TMAX=38. and RATIO_GEOM_BELOW=1.05	
    ratio_geom_below=1.05
    CALL getin_p("RATIO_GEOM_BELOW",ratio_geom_below)
    
    !
    ! Computing the layer depth for soil moisture. This defines the number of layers needed.
    !
    ztmp(:) = 0.0
    dtmp(:) = 0.0
    DO i=1,nblayermax
       IF ( ztmp(i) < depth_cstthickness ) THEN
          ztmp(i+1) = depth_topthickness*2.0*((2**i)-1)
          dtmp(i+1) = ztmp(i+1)-ztmp(i)
          igeo=i+1
          zgeoend=ztmp(i+1)
          dgeoend=dtmp(i+1)
       ELSE
          ztmp(i+1) = ztmp(i)+dtmp(i)
          dtmp(i+1) = dtmp(i)
       ENDIF
    ENDDO
    !
    ! refine at bottom. Work in progress...
    !
    nbrefine = 1
    drefine(:) = 0.0
    IF (refinebottom) THEN
       !
       ! Compute parameters for the constant increment interval before refining,
       ! If needed !
       cstint=zmaxh-(2.0*zgeoend)
       nbcst=MAX(INT(cstint/dgeoend), 0)
       IF ( nbcst > 0 ) THEN
          dcst=cstint/nbcst
       ELSE
          dcst=dgeoend
       ENDIF
       !
       ! If we have to add constant increments
       !
       IF ( nbcst > 0 ) THEN
          ! Add linear levels
          DO i=igeo,igeo+nbcst-1
             ztmp(i+1) = ztmp(i)+dcst
             dtmp(i+1) = dcst
          ENDDO
          ! Refine the levels toward the bottom
          DO i=igeo+nbcst,2*igeo+nbcst-2
             irev=(2*igeo+nbcst-1)-i
             ztmp(i+1) = ztmp(i)+dtmp(irev+1)
             dtmp(i+1) = ztmp(i+1)-ztmp(i)
             drefine(nbrefine)=dtmp(i+1)
             nbrefine=nbrefine+1
          ENDDO
          ! Without constant increments
       ELSE
          imin=MINLOC(ABS(ztmp(:)-(zmaxh/2.0)))
          igeo=imin(1)
          ztmp(igeo)=zmaxh/2.0
          dtmp(igeo) = ztmp(igeo)-ztmp(igeo-1)
          DO i=igeo,2*igeo
             irev=(2*igeo)-i
             ztmp(i+1) = ztmp(i)+dtmp(irev)
             dtmp(i+1) = ztmp(i-1)-ztmp(i)
             drefine(nbrefine)=dtmp(i+1)
             nbrefine=nbrefine+1
          ENDDO
       ENDIF
    ENDIF
    
    ! Find the index (nslm) of the node closest to the zmaxh
    imin=MINLOC(ABS(ztmp(:)-zmaxh))
    nslm=imin(1)
    !
    ! ALLOCATE the arrays we need to keep
    !
    ALLOCATE(znh(nslm), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable znh','','')
    ALLOCATE(dnh(nslm), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable dnh','','')
    ALLOCATE(dlh(nslm), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable dlh','','')
    ALLOCATE(zlh(nslm), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable zlh','','')
    !
    ! Extract now the values we need to reach zmaxh
    !
    znh(:)=ztmp(1:nslm)
    
    ! Force the last hydrological layer and node to be zmaxh
    znh(nslm)=zmaxh
    dnh(:)=dtmp(1:nslm)
    ! Recalculate the distance between the last 2 nodes
    dnh(nslm) = ztmp(nslm)-ztmp(nslm-1)
    
    ! Calculate the thickness of the layers
    DO i = 1, nslm-1
       dlh(i) = (dnh(i) + dnh(i+1)) / 2.0
    ENDDO
    dlh(nslm) = dnh(nslm)/2
    
    WRITE(numout,*) "=========================================================="
    WRITE(numout,*) "Number of layers in hydrol = ", nslm
    WRITE(numout,*) "znh = depth of nodes"
    WRITE(numout,*) znh(:)
    WRITE(numout,*) "dnh = internode distance"
    WRITE(numout,*) dnh(:)
    WRITE(numout,*) "dlh = layer thickness"
    WRITE(numout,*) dlh(:)
    WRITE(numout,*) "Total depth in hydrol has been calculated to ", znh(nslm)
    WRITE(numout,*) "======================================================================"
    
    !
    ! Extension for the thermodynamics below zmaxh
    !
    ntmp = nslm
    ztmp(:)=0.0
    ! Exception for the first layer. It is at the top in the hydrology and in
    ! the middle for temperature.
    ztmp(1)=depth_topthickness/2
    DO i=2,ntmp
       ztmp(i)=znh(i)
    ENDDO
    
    ! Exception for the last layer where again temperature needs to be in
    ! the middle of the layer. Also add a layer with the same thickness
    hh=dnh(ntmp)/2.0
    ztmp(ntmp)=ztmp(ntmp)-hh/2.0
    ztmp(ntmp+1)=ztmp(ntmp)+hh*1.5
    ztmp(ntmp+2)=ztmp(ntmp+1)+hh*2.0
    ntmp=ntmp+2
    
    ! If we have created a refined region at the bottom of the soil moisture. We need to
    ! unwinde it for temperature below zmaxh
    IF ( nbrefine > 1 ) THEN
       !ZunYin
       DO i=ntmp,ntmp+nbrefine-2
       !DO i=ntmp,ntmp+nbrefine
          iref=nbrefine-1-(i-ntmp)
          !iref=(nbrefine+1)-(i-ntmp)
          ztmp(i+1)=ztmp(i)+drefine(iref)
       ENDDO
       ntmp=ntmp+nbrefine-1
       !ntmp=ntmp+nbrefine
    ENDIF

    !WRITE(numout,*) "Zun ztmp_1:",ztmp(:)
    !WRITE(numout,*) "Zun drefine:",drefine(:)
    !WRITE(numout,*) "Zun nbrefine:",nbrefine
    !WRITE(numout,*) "Zun iref:",iref
    !WRITE(numout,*) "Zun ntmp:",ntmp
    !WRITE(numout,*) "Zun ztmp:",ztmp(:)
    !
    ! Resume the geometric increas of thickness
    DO i=ntmp,nblayermax
       IF ( ztmp(i) < depth_geom ) THEN
          ratio = 1.0
       ELSE
          ratio = ratio_geom_below
       ENDIF
       ztmp(i+1)=ztmp(i)+ratio*(ztmp(i)-ztmp(i-1))
    ENDDO
    
    ! Compute the depth of the lower interface of each layer.
    !
    zint(1) = depth_topthickness
    DO i=2,nblayermax-1
       zint(i) = (ztmp(i)+ztmp(i+1))/2.0
    ENDDO
    zint(nslm-1) = (znh(nslm-1) + znh(nslm))/2.0
    zint(nslm) = (znh(nslm))
    zint(nblayermax) = ztmp(nblayermax)+(ztmp(nblayermax)-ztmp(nblayermax-1))/2.0
    
    ! Determine the total number of layers for thermal (ngrnd).
    !
    imin=MINLOC(ABS(zint(:)-zmaxt))
    ngrnd=imin(1)
    ALLOCATE(znt(ngrnd), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable znt','','')
    ALLOCATE(dlt(ngrnd), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable dlt','','')
    ALLOCATE(zlt(ngrnd), stat=ier)
    IF (ier /= 0) CALL ipslerr_p(3,'vertical_soil_init','Problem in allocate of variable zlt','','')
    
    ! Assign values for znt, zlt, dlt
    !
    znt(:)=ztmp(1:ngrnd)
    zlt(:) = zint(1:ngrnd)
    dlt(1) = zint(1)
    DO i=2,ngrnd
       dlt(i) = zint(i)-zint(i-1)
    ENDDO
    ! Depth of layers for the hydrology are the same as for thermosoil but only the upper nslm layers are used
    zlh(:) = zlt(1:nslm)
    
    ! Force the last thermal layer and node to be zmaxt
    zlt(ngrnd) = zmaxt
    dlt(ngrnd) = zmaxt-zint(ngrnd-1)

    WRITE(numout,*) "Number of layers in thermosoil = ", ngrnd
    WRITE(numout,*) "The maximal depth for thermosoil (DEPTH_MAX_T) = ", zlt(ngrnd)
    WRITE(numout,*) "to be compared with calculated maximal depth for thermosoil (not used) = ", zint(ngrnd)
    WRITE(numout,*) "Depth of the nodes in thermosoil, znt=",znt
    WRITE(numout,*) "Thickness of the layers, dlt=", dlt
    WRITE(numout,*) "Depth of the interface between the layers, zlt=", zlt(1:ngrnd)
    WRITE(numout,*) "======================================================================"
    
  END SUBROUTINE vertical_soil_init
  
END MODULE vertical_soil
