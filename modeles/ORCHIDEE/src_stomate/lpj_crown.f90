! =================================================================================================================================
! MODULE       : lpj_crown
!
! CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
! LICENCE      : IPSL (2006)
!                This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF       Calculate individual crown area from stem mass
!!
!! \n DESCRIPTION : Calculating crown area of individual tree by diameter and tree height
!!
!! REFERENCE(S) :
!! - Smith, B., I. C. Prentice, et al. (2001), Representation of vegetation
!!  dynamics in the modelling of terrestrial ecosystems: comparing two
!!  contrasting approaches within European climate space,
!!  Global Ecology and Biogeography, 10, 621-637.
!!
!! SVN          :
!! $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_stomate/lpj_crown.f90 $
!! $Date: 2015-11-16 14:26:03 +0100 (Mon, 16 Nov 2015) $
!! $Revision: 3026 $
!! \n
!_ ================================================================================================================================

MODULE lpj_crown

  USE ioipsl_para
  USE stomate_data
  USE constantes
  USE pft_parameters
  
  IMPLICIT NONE
  
  ! private & public routines

  PRIVATE
  PUBLIC crown
  
CONTAINS
  
  
!! ================================================================================================================================
!! SUBROUTINE    : lpj_crown
!!
!>\BRIEF         Calculate individual crown area from stem mass
!!
!! DESCRIPTION   : Calculating crown area of individual tree by diameter and tree height
!! which are also calculated internally within this program from stem mass and allometory.
!! Calculations for diameter, height and crown area originate from eqns 1, 2, and 3 in 
!! Appendix B, Smith et al. (2001) following Huang et al. 1992 and Zeide 1993.
!! \latexonly
!!  \input{lpj_crown1.tex}
!!  \input{lpj_crown2.tex}
!!  \input{lpj_crown3.tex}
!! \endlatexonly
!! \n
!! where \f$k_{allom1}(=100.)\f$, \f$k_{allom2}(=40.)\f$, \f$k_{allom3}(=0.85)\f$ and \f$k_{rp}(=1.6)\f$ are 
!! constants, \f$WD\f$ is wood density (\f$=2 \times 10^5\f$ gC m\f$^3\f$) and \f$CA_{max}\f$ is maximum 
!! crown area (\f$=27.3\f$ m\f$^2\f$).
!!
!! RECENT CHANGE(S) : None
!!
!! MAIN OUTPUT VARIABLE(S) : ::cn_ind (crown area per individual, @tex $m^2 $ @endtex) and ::height (m)
!!
!! REFERENCE(S)   :
!! - Huang, S., Titus, S.J. and Wiens, D.P. (1992) Comparison of nonlinear height–diameter functions for major 
!! Alberta tree species. Canadian Journal of Forest Research, 22, 1297–1304.\n
!! - Zeide, B. (1993) Primary unit of the tree crown. Ecology, 74, 1598–1602.\n
!! - Smith, B., I. C. Prentice, et al. (2001), Representation of vegetation dynamics in the modelling of 
!! terrestrial ecosystems: comparing two contrasting approaches within European climate space,
!! Global Ecology and Biogeography, 10, 621-637.\n
!! 
!! FLOWCHART : None
!! \n
!_ ================================================================================================================================

  SUBROUTINE crown &
       &  (npts, PFTpresent, ind, biomass, woodmass_ind, veget_max, cn_ind, height)

  !! 0. Variable and parameter declaration

    !! 0.1 Input variables

    INTEGER(i_std),INTENT(in)                         :: npts              !! Domain size (unitless) 
    LOGICAL,DIMENSION(npts,nvm),INTENT(in)            :: PFTpresent        !! Is pft there (unitless)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in)        :: ind               !! [DISPENSABLE] Density of individuals 
                                                                           !! @tex $(m^{-2})$ @endtex
    REAL(r_std),DIMENSION(npts,nvm,nparts,nelements),INTENT(in) :: biomass !! [DISPENSABLE] Biomass @tex $(gC.m^{-2})$ @endtex
    REAL(r_std),DIMENSION(npts,nvm),INTENT(in)        :: woodmass_ind      !! Woodmass of the individual, needed to calculate 
                                                                           !! crownarea in lpj_crown (gC)

    !! 0.2 Output variables
  
    REAL(r_std),DIMENSION(npts,nvm),INTENT(out)       :: cn_ind            !! Crown area per individual @tex $(m^{2})$ @endtex    

    !! 0.3 Modified variables

    REAL(r_std),DIMENSION(npts,nvm),INTENT(inout)     :: veget_max        !! [DISPENSABLE] "Maximal" coverage fraction of a PFT 
                                                                          !! infinity) on ground (unitless)
    REAL(r_std),DIMENSION(npts,nvm),INTENT(inout)     :: height           !! Height of vegetation (m)           

    !! 0.4 Local variables
	
!   REAL(r_std),DIMENSION(npts)                       :: woodmass        !! Wood mass of an individual (gC)
    INTEGER(i_std)                                    :: j               !! Index
    REAL(r_std),DIMENSION(npts)                       :: dia             !! Stem diameter (m)
    REAL(r_std),DIMENSION(nvm)                        :: height_presc_12 !! [DISPENSABLE] Prescribed height of each pfts (m)

!_ ================================================================================================================================
    
  !! 1. Initializations
    
    !! 1.1 Check if DGVM is activated
    IF (.NOT.ok_dgvm .AND. lpj_gap_const_mort) THEN
       CALL ipslerr_p(3,'crown','Coherence error','crown cannot be called with static vegetation.','')
    ENDIF
    
    !! 1.2 Initialize output to zero
    cn_ind(:,:) = zero

    !! 1.3 Copy prescribed height to new variable**3 !![DISPENSABLE]
    height_presc_12(1:nvm) = height_presc(1:nvm)     !![DISPENSABLE]
    
  !! 2. Calculate (or prescribe) crown area
    
    DO j = 2,nvm ! loop over PFTs
       IF (is_tree(j)) THEN
          
          !! 2.1 Trees
          IF (natural(j)) THEN

             !! 2.1.1 Natural trees
             !WHERE (PFTpresent(:,j) .AND.ind(:,j).GT.min_stomate)
             WHERE (PFTpresent(:,j) .AND.woodmass_ind(:,j).GT.min_stomate)

                !! 2.1.1.1 Calculate individual wood mass**2

                !! S. Zaehle note that woodmass_ind needs to be defined on the individual, hence
                !! biomass*veget_max/ind, not as stated here, correction MERGE 
                !!         woodmass(:) = &
                !! &         (biomass(:,j,isapabove,icarbon) + biomass(:,j,isapbelow,icarbon) &
                !! &         +biomass(:,j,iheartabove,icarbon) + biomass(:,j,iheartbelow,icarbon))/ind(:,j)
          
                !! 2.1.1.2 Stem diameter from pipe model
                !          Stem diameter (pipe model) is calculated by allometory (eqn 1, Appdx B, Smith et al. (2001))
                !!!$          dia(:) = (woodmass(:)/(pipe_density*pi/4.*pipe_tune2)) &
                dia(:) = (woodmass_ind(:,j)/(pipe_density*pi/4.*pipe_tune2)) &
                 &       **(1./(2.+pipe_tune3))

                !! 2.1.1.3 Individual tree height from pipe model
                !          Individual tree height (eqn 2, Appdx B, Smith et al. (2001))
                height(:,j) = pipe_tune2*(dia(:)**pipe_tune3)

                !!!$S. Zaehle : The constraint on height has nothing to do with LPJ (for that purpose there's dia_max
                !!!$ cannot see why this is necessary - it also blurrs the output, hence I leave it commented
                !!!$ WHERE (height(:,j) > height_presc_12(j))
                !!!$    dia(:) = (height_presc_12(j)/pipe_tune2)**(1./pipe_tune3)
                !!!$    height(:,j) = height_presc_12(j)
                !!!$ ENDWHERE

                !! 2.1.1.4 Crown area of individual tree  
                !          Calculate crown area, truncate crown area for trunks with large diameters 
                ! crown area cannot exceed a certain value, prescribed through maxdia 
                ! (eqn 3, Appdx B, Smith et al. (2001))
                cn_ind(:,j) = pipe_tune1*MIN(dia(:),maxdia(j))**pipe_tune_exp_coeff

             ENDWHERE
          ELSE

             !! 2.1.2 Agricultural tree
             !        To be developped if needed
             CALL ipslerr_p(3,'crown','Cannot treat agricultural trees.','','')
          ENDIF
       ELSE
          
       !! 2.2 Grasses
          
          WHERE (PFTpresent(:,j))

             !! 2.2.1 Crown area of grass
             !        An "individual" is 1 m^2 of grass
             cn_ind(:,j) = un
          ENDWHERE
       ENDIF
       
       !! 2.3 Recalculate vegetation cover 
       
       !!!$S. Zaehle : since now all state variables are defined on veget_max it is very
       !!!$ dangerous to change this several times in stomate_lpj, as then GPP, turnover and allocated 
       !!!$ biomass are not defined on the same space! Hence, veget_max is now kept constant
       !!!$ and updated at the end of stomate_lpj in lpj_cover.f90
       !!!$ Eventually, this routine should only be called once at the beginning and the end of stomate_lpj
       !!!$ or prefereably cn_ind made a saved state variable!
       !!!$ IF (natural(j).AND.ok_dgvm) THEN
       !!!$   veget_max(:,j) = ind(:,j) * cn_ind(:,j)
       !!!$ ENDIF

    ENDDO ! loop over PFTs

  END SUBROUTINE crown

END MODULE lpj_crown
