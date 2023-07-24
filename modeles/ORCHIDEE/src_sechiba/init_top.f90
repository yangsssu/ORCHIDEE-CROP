! =================================================================================================================================
! MODULE        : init_top
!
! CONTACT       : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE       : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module computes the parameters for TOPMODEL. 
!!
!!\n DESCRIPTION : contains.
!! 
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S) : None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/shushi.peng/ORCHIDEE/src_sechiba/init_top.f90 $
!! $Date: 2015-03-10 10:16:04 +0100 (Tue, 10 Mar 2015) $
!! $Revision: 2535 $
!! \n
!_ ================================================================================================================================

!-----------------------------------------------------------------
!--------------- special set of characters for SCCS information
!-----------------------------------------------------------------
!      %Z% Lib:%F%, Version:%I%, Date:%D%, Last modified:%E%
!-----------------------------------------------------------------
!     ######################
MODULE init_top 
!     ######################
!
!
!
        USE ioipsl
        USE constantes
!        USE constantes_soil
        USE pft_parameters
!pss:!        USE constantes_veg
!!        USE reqdprec
        USE gammad_inc

        IMPLICIT NONE
        INTEGER, PARAMETER              :: IDOUBLE=KIND(1.D0)  ! Double precision
CONTAINS
!
!     ######spl
      SUBROUTINE init_top_main(kjpindex, lalo, veget_max, PWWILT, PWSAT, PD_TOP, PM, PTI_MIN, PTI_MAX, &
                           PTI_MOY, PTI_STD, PTI_SKEW, PTAB_FSAT, PTAB_WTOP, PTAB_FWET, PTAB_WTOP_WET, ZPAS)
!
!
!     #####################################################################
!
!!****  *INIT_TOP*  
!!
!!    PURPOSE
!!    =======
!
!     Calculates the new array of the Datin-Saulnier TOPMODEL framework fonction for xsat and compute each 
!     satured fraction for each xsat value of the grids cells but also the active TOPMODEL-layer array, 
!     the normalized mean deficit array.
!     For calculate new array, we use the incomplete gamma function. (see gamma_inc.f for more detail)
!         
!     
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!        ===================
!
!*      0.1    declarations of arguments
!
INTEGER(i_std), INTENT(in)                         :: kjpindex         !! Domain size
REAL(r_std),DIMENSION (kjpindex,2), INTENT (in)     :: lalo       !! Geogr. coordinates (latitude,longitude) (degrees)
REAL(r_std),DIMENSION (kjpindex,nvm), INTENT (in)  :: veget_max        !! Max. fraction of vegetation type (LAI -> infty)
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PWWILT
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PWSAT
!                                       PWWILT = the wilting point volumetric 
!                                                water content (m3 m-3)
!                                       PWSAT  = saturation volumetric water content
!                                                of the soil (m3 m-3)
!

REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PTI_MIN
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PTI_MAX
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PTI_STD
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PTI_SKEW
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PM
REAL(r_std), INTENT (in)       :: PD_TOP
REAL(r_std),DIMENSION (kjpindex), INTENT (in)       :: PTI_MOY
!                                       PTI_MOY  = ti mean
!                                       PTI_MIN  = ti min
!                                       PTI_MAX  = ti max
!                                       PTI_STD  = ti standard deviation
!                                       PTI_SKEW = ti skewness
!                                       PM       = exponential decay factor of the local deficit
!                                       PD_TOP   = Topmodel active soil depth
!
REAL(r_std), DIMENSION(kjpindex,1000), INTENT(OUT) :: PTAB_FSAT, PTAB_WTOP,PTAB_FWET, PTAB_WTOP_WET
REAL(r_std), DIMENSION(kjpindex), INTENT (OUT)       :: ZPAS
!                                       PTAB_FWET = Wetland fraction array
!                                       PTAB_FSAT = Satured fraction array
!                                       PTAB_WTOP = Active TOPMODEL-layer array
!
!*      0.2    declarations of local variables
!
REAL(KIND=IDOUBLE), DIMENSION(kjpindex)    :: ZXSAT
!                                             ZXSAT = Initial satured index for all index 



!
REAL(KIND=IDOUBLE)    :: ZXI, ZPHI, ZNU, ZTI_MOY, ZTI_MAX, ZTI_MIN
!                        ZTI_MOY = ti mean after regression
!                        ZXI     = ti pdf parameter
!                        ZPHI    = ti pdf parameter
!                        ZNU     = ti pdf parameter
!
REAL(KIND=IDOUBLE)    :: ZFTOT, ZYMAX, ZYMIN
REAL(KIND=IDOUBLE)    :: ZGYMAX, ZGYMIN
!                        ZFTOT   = total fraction of a grid cell
!                        ZYMAX   = yi maximum variable
!                        ZYMIN   = yi minimum variable
!                        ZGYMAX  = incomplete gamma function for ymax (GAMSTAR result)
!                        ZGYMIN  = incomplete gamma function for ymin (GAMSTAR result)
!
REAL(KIND=IDOUBLE)    :: ZXSAT_IND, ZYSAT, ZY0, ZDMOY, ZXMOY, ZFMED, ZF0
!                        ZXSAT_IND = Satured index for all index 
!                        ZYSAT     = changing variable of satured index
!                        ZY0       = changing variable of dry index
!                        ZDMOY     = grid cell average deficit (= Dbar/M)
!                        ZXMOY     = ti mean value on wet (1-fsat-f0) fraction
!                        ZF0       = dry fraction
!                        ZFMED     = wet (1-fsat-f0) fraction
!
REAL(KIND=IDOUBLE)    :: ZG, ZGYSAT, ZGY0
!                        ZG     = GAM result 
!                        ZGYSAT = the incomplete gamma function for ysat (GAMSTAR result)
!                        ZGY0   = the incomplete gamma function for y0 (GAMSTAR result)
!
REAL(KIND=IDOUBLE)    :: ZD0, ZGAM_PHI
!                        ZD0  = Normalized TOPMODEL maximum deficit D0/M coefficient
!                        ZPAS = pas for calculate the new xsat values array
!
!
INTEGER               :: IFLG, IFLGST
!                        IFLG   = incomplete gamma function error flag (GAM result)
!                        IFLGST = incomplete gamma function error flag (GAMSTAR result)
!                                 (see gamma_inc.f for more detail)
!
INTEGER               :: INI, I, IND, JSI_MIN, JSI_MAX, IPAS, qq, I_zti_moy
!    
REAL(r_std),DIMENSION(kjpindex)              :: sum_veget

!-------------------------------------------------------------------------------
!
!       1 TOPMODEL SURFACE RUNOFF SCHEME
!       ================================
!
!  1.1     initialisation of local variable
!  ----------------------------------------
!

sum_veget=SUM(veget_max(:,2:13),2)


! Grid cells number
!

INI = kjpindex 
IPAS = SIZE(PTAB_FSAT,2)
!
! GAM result (not use here !)
!
ZG  = 0.0
!       
! ti_sat values
!
ZXSAT(:) = PTI_MIN(:)-ZPAS(:)

!
!  1.2     Algorithme
!  ------------------
!
!grid cells loops 
!
DO I=1,INI

!la boucle suivante permet de calculer la fraction a wfc (1er pas de boucle) et la fraction wetland (2nd pas de temps de boucle)
DO I_zti_moy=1,2

!
!   IF(PTI_SKEW(I)/=-99.99)THEN
IF((PTI_SKEW(I)/=-99.99) .AND. (PTI_STD(I)/=-99.99) .AND. (PM(I) .GE. 0.01))THEN
!   write(*,*) I, PTI_SKEW(I)
!
!  1.2.0 first initialisation
!  --------------------------
!
   ZXI       = 0.0        
   ZPHI      = 0.0
   ZNU       = 0.0
!
!  Wolock and McCabe (2000) linear regression equation between the mean
!  topographic index computed with a 1000 meter DEM and a 100 meter DEM.
!
!
!   ZTI_MOY=0.961*PTI_MOY(I)-1.957


   IF (I_zti_moy .EQ. 1)THEN
      ZTI_MOY=PTI_MOY(I) 
      ZTI_MIN=PTI_MIN(I)      
      ZTI_MAX=PTI_MAX(I)
   ELSEIF (I_zti_moy .EQ. 2)THEN
      ZTI_MOY=PTI_MOY(I)+ SHIFT_fsat_fwet !shift de la distribution d indice topo
      ZTI_MIN=PTI_MIN(I)+ SHIFT_fsat_fwet
      ZTI_MAX=PTI_MAX(I)+ SHIFT_fsat_fwet
      !! non necessaire de modifier le skewness et la std: SKWENESS(X+a)=SKWNESS(X) et STD(X+a)=STD(X)
   ENDIF


!  Calculate topographic index pdf parameters 
!

   ZXI  = PTI_SKEW(I)*PTI_STD(I)/2. 
   if (ZXI .NE. 0.0) then
   ZPHI = (PTI_STD(I)/ZXI)**2
   ZNU  = PTI_MOY(I)-ZPHI*ZXI

   !vire deserts
   if ((ZPHI .LT. 100.0) .AND. ((PWSAT(I)-PWWILT(I)) .GT. min_sechiba) &
        & .AND. (PWSAT(I) .GT. min_sechiba) .AND. (sum_veget(I) .GT. 0.01)) then

   ZD0 = (PWSAT(I)-PWWILT(I))*PD_TOP/PM(I)

!  Initialise
!
   ZGYMAX = 0.0
   ZGYMIN = 0.0
   ZFTOT  = 0.0
   ZYMIN  = 0.0
   ZYMAX  = 0.0
!
!  variable changing yi ---> (ti-nu)/xi
!
   ZYMIN = (ZTI_MIN-ZNU)/ZXI
   ZYMAX = (ZTI_MAX-ZNU)/ZXI
!  
!  Supress numerical artifact
!
   ZYMIN = MAX(0.0,ZYMIN)
!
!  Errors flags indicating a number of error condition in G and GSTAR
!  (see gamma_inc.f for more detail)
!
   IFLG        =0
   IFLGST      =0
!
!  Computation of F(0 --> ymin)
!
   CALL DGAM(ZPHI,ZYMIN,2.,ZG,ZGYMIN,IFLG,IFLGST)
!      
!  if the incomplete gamma function don't work, print why
!
   IF (IFLGST/=0) PRINT*,'MAILLE =',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YMIN= ',ZYMIN      
!
!  Computation of F(0 --> ymax)
!
   CALL DGAM(ZPHI,ZYMAX,2.,ZG,ZGYMAX,IFLG,IFLGST)
!      
!  if the incomplete gamma function don't work, print why
!
   IF (IFLGST/=0) PRINT*,'MAILLE =',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YMAX= ',ZYMAX 
!
!  FTOT = F(0 --> ymax) - F(0 --> ymin)
!
   ZFTOT=ZGYMAX-ZGYMIN   
   if (ZFTOT .NE. 0.0) then
!
!  initialization of loop control variables
!
   JSI_MAX = 0
!
!  Define the new limits for the satured index loop
!
   JSI_MIN = 1
   JSI_MAX = IPAS
   ZPAS(I)    = (ZTI_MAX-ZTI_MIN)/(IPAS-1)
!
!  1.2.2 Calculate all topmodel arrays
!  -----------------------------------
!
!  Satured index loop
!
   DO IND=JSI_MIN,JSI_MAX
!
!     initialize of loops variables
!
      ZXSAT_IND = 0.0 
      ZYSAT     = 0.0
      ZY0       = 0.0
      ZDMOY     = 0.0
      ZXMOY     = 0.0
      ZFMED     = 0.0
!
!     Initialize of incomplete gamma function flags and variables
!
      IFLG   = 0
      IFLGST = 0
      ZGYSAT = 0.0
      ZGY0   = 0.0
!
!     calculate xsat for all new index
!
      ZXSAT_IND=ZTI_MIN+(IND-1)*ZPAS(I)
!
!     Changing variable to compute incomplete gamma function 
!
      ZYSAT=(ZXSAT_IND-ZNU)/ZXI
      ZY0  =((ZXSAT_IND-ZD0)-ZNU)/ZXI
!      
!     Calculate Y0 and ysat and assume ymin < y0 < ymax !

      ZYSAT=MAX(ZYMIN,MIN(ZYSAT,ZYMAX))
      ZY0  =MAX(ZYMIN,MIN(ZY0,ZYMAX))
!
!     call incomplete gamma function for xsat
!
      CALL DGAM(ZPHI,ZYSAT,2.,ZG,ZGYSAT,IFLG,IFLGST)
!
!     if the incomplete gamma function don't works, print why
!
      IF (IFLGST/=0) print*,'MAILLE= ',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'YSAT= ',ZYSAT
!
!     call incomplete gamma function for xsat
!
      CALL DGAM(ZPHI,ZY0,2.,ZG,ZGY0,IFLG,IFLGST)
!
!     if the incomplete gamma function don't works, print why
!
      IF (IFLGST/=0) print*,'MAILLE= ',I,'FLGST= ',IFLGST,'PHI= ',ZPHI,'Y0= ',ZY0
!
!     compute satured fraction as FSAT = F(0 --> ymax) - F(0 --> ysat)
!
      IF (I_zti_moy .EQ. 1)THEN
          PTAB_FSAT(I,IND)=(ZGYMAX-ZGYSAT)/ZFTOT
      ELSEIF (I_zti_moy .EQ. 2)THEN
          PTAB_FWET(I,IND)=(ZGYMAX-ZGYSAT)/ZFTOT
      ENDIF
!
!     Calculate FMED =  F(0 --> ysat) - F(0 --> y0)
!
      ZFMED=MAX(0.0,((ZGYSAT-ZGY0)/ZFTOT))
!
!     compute driest fraction as F0 = 1-Fsat-Fwet
!
      IF (I_zti_moy .EQ. 1)THEN
          ZF0=MAX(0.0,(1.0-PTAB_FSAT(I,IND)-ZFMED))  
      ELSEIF (I_zti_moy .EQ. 2)THEN
          ZF0=MAX(0.0,(1.0-PTAB_FWET(I,IND)-ZFMED))  
      ENDIF
   
!
      IF (ZFMED/=0.0) THEN
!
!        Compute the new x mean, xmoy', over the wet fraction Fwet
!
         CALL gamma(ZPHI, ZGAM_PHI)
         
         ZXMOY = ZNU+ZXI*(ZPHI+(EXP(-ZY0)*(ZY0**(ZPHI/2))*(ZY0**(ZPHI/2))     &
              -EXP(-ZYSAT)*(ZYSAT**(ZPHI/2))*(ZYSAT**(ZPHI/2)))/(ZFMED*ZGAM_PHI))
 
!        supress numerical artifacs
!
            ZXMOY =MAX((ZXSAT_IND-ZD0),MIN(ZXSAT_IND,ZXMOY))
!
!        Calculate the mean normalysed deficit as Dbar/M = (1-fsat-f0)*(xsat-xmoy')+f0*D0/M
!
            ZDMOY = ZFMED*(ZXSAT_IND-ZXMOY)+ZF0*ZD0
!
      ENDIF
!
!     supress numerical artifacs
! 
      ZDMOY = MAX(0.0,MIN(ZDMOY,ZD0))
!
!     Solves Dbar = (Wsat-WT)*d_top with Dbar/M (=ZDMOY) = (Wsat-WT)*d_top/M
!
      !modifie le calcul de PTAB_WTOP du au fait que le deficit est calcule par rapport a wfc et non par rapport a wsat

      IF (I_zti_moy .EQ. 1)THEN
          PTAB_WTOP(I,IND) = PWSAT(I)-(PM(I)*ZDMOY/PD_TOP)    
!pss: should not minus PWWILT, otherwise not consistent with hydro_subgrid.f90
!          PTAB_WTOP(I,IND) = PTAB_WTOP(I,IND) - PWWILT(I)
      ELSEIF (I_zti_moy .EQ. 2)THEN
         !ne modifie que une des deux fonctions (c.a.d PTAB_FSAT/PTAB_FWET et pas PTAB_WTOP)  
         !PTAB_WTOP_WET(I,IND) = PWSAT(I)-(PM(I)*ZDMOY/PD_TOP(I))   
         !PTAB_WTOP_WET(I,IND) = PTAB_WTOP_WET(I,IND) - PWWILT(I)
         PTAB_WTOP_WET(I,IND) = PTAB_WTOP(I,IND)
      ENDIF

      !suprime eventuels pbs 
      IF (I_zti_moy .EQ. 1)THEN     
          IF ( PTAB_WTOP(I,IND) <= 0.0 ) PTAB_FSAT(I,IND) = 0.0
!pss: normally not need to decide the over-saturation as zero saturated fraction
!          IF ( PTAB_WTOP(I,IND) >= PWSAT(I) ) PTAB_FSAT(I,IND) = 0.0
      ELSEIF (I_zti_moy .EQ. 2)THEN
          IF ( PTAB_WTOP_WET(I,IND) <= 0.0 ) PTAB_FWET(I,IND) = 0.0
!pss: normally not need to decide the over-saturation as zero saturated fraction
!          IF ( PTAB_WTOP_WET(I,IND) >= PWSAT(I) ) PTAB_FWET(I,IND) = 0.0
      ENDIF


!
    ENDDO

 else !if (ZFTOT .NE. 0.0) then 

     IF (I_zti_moy .EQ. 1)THEN
     
         PTAB_WTOP(I,:) = 0.0
         PTAB_FSAT(I,:) = 0.0    
     

      ELSEIF (I_zti_moy .EQ. 2)THEN
        
          PTAB_WTOP_WET(I,:) = 0.0
          PTAB_FWET(I,:) = 0.0    
    

      ENDIF
       
   endif !if (ZFTOT .NE. 0.0) then


else ! if ((ZPHI .LT. 100.0) .AND. ((PWSAT(I)-PWWILT(I)) .GT. min_sechiba) .AND. (PWSAT(I) .GT. min_sechiba) .AND. (sum_veget(I) .GT. 0.02))
   
    IF (I_zti_moy .EQ. 1)THEN
        
        PTAB_WTOP(I,:) = 0.0
        PTAB_FSAT(I,:) = 0.0    
        
        
    ELSEIF (I_zti_moy .EQ. 2)THEN
        
        PTAB_WTOP_WET(I,:) = 0.0
        PTAB_FWET(I,:) = 0.0    
        
        
    ENDIF

   !fin de la boucle if sur la condition ZPHI > 100.0
endif ! if ((ZPHI .LT. 100.0) .AND....

else

    IF (I_zti_moy .EQ. 1)THEN
        
        PTAB_WTOP(I,:) = 0.0
        PTAB_FSAT(I,:) = 0.0    
        
        
    ELSEIF (I_zti_moy .EQ. 2)THEN
        
        PTAB_WTOP_WET(I,:) = 0.0
        PTAB_FWET(I,:) = 0.0    
        
        
    ENDIF




 endif  !if (ZXI .NE. 0.0)

    ELSE


        IF (I_zti_moy .EQ. 1)THEN
            
            PTAB_WTOP(I,:) = 0.0
            PTAB_FSAT(I,:) = 0.0    
            
            
        ELSEIF (I_zti_moy .EQ. 2)THEN
        
            PTAB_WTOP_WET(I,:) = 0.0
            PTAB_FWET(I,:) = 0.0    
            
            
        ENDIF



     ENDIF !IF((PTI_SKEW(I)/=-99.99) .AND. (PTI_STD(I)/=-99.99) .AND. (PM(I).GE. 0.01))
!
  ENDDO


ENDDO
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE init_top_main 
!
!*******************************************
!*           FUNCTION  GAMMA(X)            *
!* --------------------------------------- *
!* Returns the value of Gamma(x) in double *
!* precision as EXP(LN(GAMMA(X))) for X>0. *
!*******************************************
!!!  real*8 Function gamma(xx)
SUBROUTINE gamma(xx,gamma_re)
  IMPLICIT NONE
  REAL(r_std), INTENT(IN)   :: xx
  REAL(r_std), INTENT(OUT)   :: gamma_re 
  REAL cof(6),stp,half,one,fpf,x,tmp,ser
  INTEGER j
  DATA cof,stp /76.18009173d0,-86.50532033d0,24.01409822d0,  &
       -1.231739516d0,0.120858003d-2,-0.536382d-5,2.50662827465d0/
  DATA half,one,fpf /0.5d0,1.0d0,5.5d0/
  x=xx-one
!  PRINT*, 'x',x
  tmp=x+fpf
!  PRINT*, 'tmp', tmp
  tmp=(x+half)*DLOG(tmp)-tmp
!  PRINT*, 'tmp', tmp
  ser=one
  do j=1,6
    x=x+one
    ser=ser+cof(j)/x
  end do
!  PRINT*, 'ser', ser
  gamma_re = DEXP(tmp+DLOG(stp*ser))
!  return
END SUBROUTINE gamma

END MODULE init_top
!
