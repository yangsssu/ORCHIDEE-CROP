! =================================================================================================================================
! MODULE        : hydro_subgrid
!
! CONTACT       : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE       : IPSL (2006)
! This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF        This module computes TOPMODEL saturated fraction. 
!!
!!\n DESCRIPTION : contains.
!! 
!! RECENT CHANGE(S) : None
!!
!! REFERENCE(S) : None
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/shushi.peng/ORCHIDEE/src_sechiba/hydro_subgrid.f90 $
!! $Date: 2015-03-10 10:16:04 +0100 (Tue, 10 Mar 2015) $
!! $Revision: 2535 $
!! \n
!_ ================================================================================================================================

!-----------------------------------------------------------------
!--------------- special set of characters for SCCS information
!-----------------------------------------------------------------
!      %Z% Lib:%F%, Version:%I%, Date:%D%, Last modified:%E%
!-----------------------------------------------------------------
!     #################
MODULE hydro_subgrid 
!     #################
!
      USE ioipsl
      USE constantes
      USE constantes_soil
!pss!      USE constantes_veg
!!      USE reqdprec
!
      IMPLICIT NONE
CONTAINS
!
      SUBROUTINE hydro_subgrid_main(kjpindex, PTAB_FSAT, PTAB_WTOP, phumtot, profil_froz_hydro, PFSAT,&
    & PTAB_FWET,PTAB_WTOP_WET,PFWET, PD_TOP, &
    & ruu_ch, PWT1,PWT2,PWT3,PWT4,PM,PTI_MIN,PTI_MAX,PAS, dz)
!
!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
!
INTEGER(i_std), INTENT(in)                         :: kjpindex         !! Domain size
REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: ruu_ch
REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: PM
REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: PTI_MIN
REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: PTI_MAX
REAL(r_std),DIMENSION (kjpindex), INTENT (in)      :: PAS
REAL(r_std),DIMENSION (nslm+1),   INTENT (in)      :: dz
!                                   PW_TOP  = total liquid volumetric water
!                                             content of the upper layer
!                                   PWI_TOP = total ice volumetric water
!                                             content of the below layer

REAL(r_std), INTENT (in)       :: PD_TOP

!
REAL(r_std), DIMENSION(kjpindex), INTENT(OUT)  :: PFSAT,PFWET,PWT1,PWT2,PWT3,PWT4
!                                   PFSAT   = satured fraction
!                                   PFWET   = wetland fraction
!
REAL(r_std), DIMENSION(kjpindex,1000), INTENT(IN) :: PTAB_FSAT, PTAB_WTOP, PTAB_FWET, PTAB_WTOP_WET 
!                                   PTAB_FSAT = Satured fraction array
!                                   PTAB_WTOP = Active TOPMODEL-layer array
!                                   PTAB_FWET = Wetland fraction array
!
REAL(r_std),DIMENSION (kjpindex), INTENT (in) :: phumtot
REAL(r_std),DIMENSION (kjpindex, nslm), INTENT (in) :: profil_froz_hydro 
!REAL(r_std),DIMENSION (kjpindex,nstm), INTENT (in) :: psoiltile

!*      0.2    declarations of local variables
!
REAL, DIMENSION(kjpindex)                     :: ZW_TOP,ZW_TOP_WET
!REAL, DIMENSION(kjpindex)                     :: ZFSAT, ZW_TOP, ZFWET
!                                        ZW_TOP  = ative TOPMODEL-soil moisture at 't' (m3 m-3)
!
REAL, DIMENSION(:,:), ALLOCATABLE     :: ZCOMP,ZCOMP_WET
!                                                      ZCOMP     = array for wt comparaison with wt_array
!
INTEGER, DIMENSION(kjpindex)                  :: I_IND,I_IND_WET,I_IND_WT1,I_IND_WT2,I_IND_WT3,I_IND_WT4
!                                        I_IND = change in xsat (or fsat) index
!
INTEGER                               :: INI, IND, I 
!                                        INI      = nombre de mailles        
!                                        IND      = Indice de boucle        
!REAL(r_std), DIMENSION(kjpindex)                     :: PW_TOP, PWI_TOP
REAL(r_std), DIMENSION (kjpindex)                    :: phumtot_ave
INTEGER(i_std)                                      :: ji,jv
REAL(r_std), DIMENSION (kjpindex)                    :: pfrac_froz_fsat
INTEGER(r_std), DIMENSION (kjpindex)                 :: nb_count
!-------------------------------------------------------------------------------
!
!valeur pr chaque grid-cell
    phumtot_ave(:) = 0.0
!
    DO ji = 1, kjpindex
       !!pmc_ave(ji) = SUM(pmc(ji,:,1)*psoiltile(ji,1)+pmc(ji,:,2)*psoiltile(ji,2)+pmc(ji,:,3)*psoiltile(ji,3))!SUM sur les 11 couches
       !variable _ave servait initialement a faire la somme sur les PFT ou sur les types de sol. inutile maintenant
       phumtot_ave(ji) = phumtot(ji)      
    ENDDO

    DO ji = 1, kjpindex
       pfrac_froz_fsat(ji)= (SUM(profil_froz_hydro(ji,1:9) * dz(1:9)) / SUM(dz(1:9)))
    ENDDO
!
INI=kjpindex
ALLOCATE(ZCOMP(INI,SIZE(PTAB_WTOP,2)))
ALLOCATE(ZCOMP_WET(INI,SIZE(PTAB_WTOP_WET,2)))
!
I_IND (:) =0 !indice seuil pr calcul de fraction a saturation
I_IND_WET (:) =0 !indice seuil pr fraction de wetland (avc WTD=0)
I_IND_WT1 (:) =0 !indice des fractions de wetland avec une WTD en-dessous du sol
I_IND_WT2 (:) =0
I_IND_WT3 (:) =0
I_IND_WT4 (:) =0

ZW_TOP(:) =0.0
ZW_TOP_WET(:) =0.0
PFSAT (:) =0.0
PFWET (:) =0.0
!
!        
IND=SIZE(PTAB_WTOP,2)
!  
! Here, we determinate the new fsat by comparaison between wt_array and WT
!
!ZW_TOP(:) = MIN(((PW_TOP(:) + PWI_TOP(:)+pdrainage_t(:)))/(PD_TOP(:)*1000.), PTAB_WTOP(:,1))
!ZW_TOP_WET(:) = MIN(((PW_TOP(:) + PWI_TOP(:)+pdrainage_t(:)))/(PD_TOP(:)*1000.), PTAB_WTOP_WET(:,1))
ZW_TOP(:) = MIN((phumtot_ave(:))/(PD_TOP*1000.), PTAB_WTOP(:,1))
ZW_TOP_WET(:) = MIN((phumtot_ave(:))/(PD_TOP*1000.), PTAB_WTOP_WET(:,1))

!
! compare wt_array and WT
!
ZCOMP(:,:) = 999.0
ZCOMP_WET(:,:) = 999.0
!
DO I=1,IND
  !prise en compte du gel pour le calcul de fsat:
  !soluce 1: diminue la quantité d eau utilise pour calculer le deficit
  !soluce 2: utilise la qte d eau totale (liq + solide) puis diminue le fsat obtenu avec ce calcul
  ! ZCOMP(:,I) = ABS(PTAB_WTOP(:,I) - ((pfrac_froz_gqsb(:)* dsg_ave(:) + &
  !    & (PD_TOP(:)-dsg_ave(:))*pfrac_froz_bqsb(:))/PD_TOP(:))*(ruu_ch(:)/1000.0) - ZW_TOP(:))
  ZCOMP(:,I) = ABS(PTAB_WTOP(:,I) - ZW_TOP(:))
  ZCOMP_WET(:,I) = ABS(PTAB_WTOP_WET(:,I) - ZW_TOP_WET(:))
ENDDO
!
! calculate array index where tab_wt = WT
!
!recherche de l indice seuil pour les fractions a saturation et fractions de wetland
I_IND(:)=INT(MINLOC(ZCOMP(:,:),2))
I_IND_WET(:)=INT(MINLOC(ZCOMP_WET(:,:),2))

!boucle de type ci-dessus peut etre utilise si la fonction MINLOC n est pas vectorise par le compilo
!DO ji = 1, kjpindex
!   I_IND(ji)=INT(MINLOC(ZCOMP(ji,:),1))
!ENDDO


WHERE ((PTI_MAX(:) /= -99.99 ) .AND. &
        & (ruu_ch(:) .GE. min_sechiba) .AND. (PD_TOP .GE. min_sechiba) .AND. (PAS(:) .GE. min_sechiba))
    I_IND_WT1(:)=INT(I_IND_WET(:) - ((4*WTD1_borne/PD_TOP)/(ruu_ch(:)*PD_TOP/1000.))/PAS(:))! indice pour deficit entre 0 et -6cm
    I_IND_WT2(:)=INT(I_IND_WET(:) - ((4*WTD2_borne/PD_TOP)/(ruu_ch(:)*PD_TOP/1000.))/PAS(:))! indice pour deficit entre 0 et -12 cm
    I_IND_WT3(:)=INT(I_IND_WET(:) - ((4*WTD3_borne/PD_TOP)/(ruu_ch(:)*PD_TOP/1000.))/PAS(:))! indice pour deficit entre 0 et -18cm
    I_IND_WT4(:)=INT(I_IND_WET(:) - ((4*WTD4_borne/PD_TOP)/(ruu_ch(:)*PD_TOP/1000.))/PAS(:))! indice pour deficit entre 0 et -24cm
    ! le pd_top au denominateur est un peu etrange 
    ! pourtant ce calcul donne les bonnes fractions:
    ! un test a ete realise en utilisant I_IND et en calculant la fraction pour un deficit entre 0 et 2 m => fraction ~ 1
elsewhere
    I_IND_WT1(:)=999
    I_IND_WT2(:)=999
    I_IND_WT3(:)=999
    I_IND_WT4(:)=999
endwhere

WHERE ( I_IND_WT1(:) .LE. 0.0 )  
    I_IND_WT1(:)=1
endwhere
WHERE ( I_IND_WT2(:) .LE. 0.0 )  
    I_IND_WT2(:)=1
endwhere
WHERE ( I_IND_WT3(:) .LE. 0.0 )  
    I_IND_WT3(:)=1
endwhere
WHERE ( I_IND_WT4(:) .LE. 0.0 )  
    I_IND_WT4(:)=1
endwhere

DO I=1,INI
  nb_count(I)=COUNT(PTAB_FSAT (I,:)>0.0)!!= COUNT(PTAB_FWET (I,:)>0.0) aussi
ENDDO

DO I=1,INI
!  calculate fsat
  IF(nb_count(I)>0.0)THEN
      PFSAT(I) = PTAB_FSAT (I,I_IND(I))
      PFWET(I) = PTAB_FWET (I,I_IND_WET(I))
      !PFWET(I) = PTAB_FWET (I,I_IND(I)) ! test pour voir si somme des fractions de deficit entre 0 et 2m ~ 1
      PWT1(I) = PTAB_FWET (I,I_IND_WT1(I))-PFWET(I) 
      PWT2(I) = PTAB_FWET (I,I_IND_WT2(I))-(PWT1(I)+PFWET(I))
      PWT3(I) = PTAB_FWET (I,I_IND_WT3(I))-(PWT2(I)+PWT1(I)+PFWET(I))
      PWT4(I) = PTAB_FWET (I,I_IND_WT4(I))-(PWT3(I)+PWT2(I)+PWT1(I)+PFWET(I))
   ELSE
       PFSAT(I) = 0.0 !Australia
       PFWET(I) = 0.0
       PWT1(I) = 0.0
       PWT2(I) = 0.0
       PWT3(I) = 0.0
       PWT4(I) = 0.0
   ENDIF
ENDDO
!

WHERE ( PTAB_WTOP(:,1) .EQ. PTAB_WTOP(:,900) )  
    PFSAT(:)=0.0
    PFWET(:)=0.0
    PWT1(:)=0.0
    PWT2(:)=0.0
    PWT3(:)=0.0
    PWT4(:)=0.0
endwhere

!prise en compte du gel pour diminuer les differentes fractions 
WHERE ( phumtot_ave(:).GT. 0.0 )
    PFSAT(:)=(1-pfrac_froz_fsat(:))*PFSAT(:)
    PFWET(:)=(1-pfrac_froz_fsat(:))*PFWET(:)  
    PWT1(:)= (1-pfrac_froz_fsat(:))*PWT1(:)
    PWT2(:)= (1-pfrac_froz_fsat(:))*PWT2(:)
    PWT3(:)= (1-pfrac_froz_fsat(:))*PWT3(:)
    PWT4(:)= (1-pfrac_froz_fsat(:))*PWT4(:)
ENDWHERE

!
!-------------------------------------------------------------------------------
!
END SUBROUTINE hydro_subgrid_main
END MODULE hydro_subgrid 
