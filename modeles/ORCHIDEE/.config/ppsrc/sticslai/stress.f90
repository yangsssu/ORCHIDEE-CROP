










! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This module calculates the water stress indices, swfac and turfac.
!> - Stics book paragraphe 7.3.1, 7.3.2, page 138-140
!>
!! Relative transpiration, i.e. the relationship between actual transpiration and maximal transpiration (ep/eop), is a bilinear function of the available
!! water content in the root zone, teta (i.e. the water content above the wilting point in cm3 of water/cm3 of dry soil).
!!
!! The water content threshold separating the maximal transpiration stage and the reduced transpiration stage (tetstomate) depends on root density,
!! the stomatal functioning of the plant, and the evaporative demand (Brisson, 1998c). It was shown that this threshold does not depend on the soil type,
!! for example via the maximal available water content, as is commonly assumed.
!!
!! In the calculations below, cumlracz is the summation over the whole rooting depth, zrac, of effective root length density lracz,
!! psisto is the critical potential of stomatal closure (positive in bars) and rayon is the mean root radius which is assumed to be equal to 0.02 cm.
!!
!! The ep/eop ratio is equal to the stomatal stress index, swfac.  The stress turgor index turfac which affects leaf growth comes into play earlier.
!! The method for calculating it is copied from the method used for swfac using the critical potential of cell expansion psiturg.
!! Since psiturg is lower than psisto, we obtain a higher teturg threshold.  In other words, leaf growth can be inhibited even when transpiration is still at
!! its maximum level.



!!
!! However, in our model, we just use the relative of available soil moisture to fielding capacity to represent the slowing effects on LAI. 
!! The humrel is harmonized. 
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine stress(n,                  &   ! IN
                  nrec,               &  
                  lai,                & 
                  eop,                &
                  shumdiag_cm_day,    &   ! IN
                  vswc,               &   ! IN
                  humrel,             &
                  shumrel,            &
                  swfac,              &   ! INOUT
                  turfac,             &
                  senfac)                 
  USE Stics
  USE constantes  


  
  IMPLICIT NONE

!: Arguments
!: IN

  integer, intent(IN)                  :: n  
  integer, intent(IN)                  :: nrec 
  real,    intent(IN)                  :: lai           ! leaf area index
  real,    intent(IN)                  :: eop      !> // OUTPUT // Maximum transpiration flux  // mm
  real,    intent(IN), dimension(3)    :: shumdiag_cm_day 
  real,    intent(IN)                  :: vswc      !> daily humrel data
  real,    intent(IN)                  :: humrel
  
!: INOUT
  real,    intent(INOUT) :: swfac      !> // OUTPUT // Index of stomatic water stress  // 0-1
  real,    intent(INOUT) :: turfac      !> // OUTPUT // Index of turgescence water stress  // 0-1
  real,    intent(INOUT) :: senfac      !> // OUTPUT // Water stress index on senescence // 0-1
! OUT
  real,    intent(OUT)    :: shumrel     ! average relative soil moisture to holding capacity at sowing depth

! temporal variables 

  real  ::    teta   ! volumetric soil water content m3/m3

!      print *, 'in stress, the vegstress and humrel is', vswc, humrel
      

      ! ** si pas de plante
      shumrel = sum(shumdiag_cm_day)/3.0 ! relative to soil holding capacity
!      print *, 'humrel in stress is', humrel
!      print *, 'eop and lai in stress is', eop, lai
      if ((P_codelaitr == 1 .and. lai <= 0.)          &
          .or. (nrec /= 0 .and. nrec > 0 .and. P_codcueille <= 1)     &
          .or. (eop <= 0.)                                 &
         ) then

        swfac  = 1.
        turfac = 1.
        senfac = 1.
      else
         ! we calculate the water stress based on the volumic soil water contents.
         
         teta = vswc  ! this is the volumetric soil water contents
         !teta = vswc        

         !: Calcul de swfac
              if (teta <= tetstomate) then
                swfac = teta / tetstomate
              else
                swfac = 1.
              endif
              !write(70,*) n,'swfac=',swfac,teta,tetstomate,P_swfacmin
              swfac = max(swfac, P_swfacmin)
    
    
              !: Calcul de turfac
              if (teta <= teturg) then
                turfac = teta / teturg
              else
                turfac = 1.
              endif
              turfac = max(turfac, P_swfacmin)
        
              !: Calcul de senfac
              if (teta <= tetsen) then
                senfac = teta / tetsen
              else
                senfac = 1.0
              endif
              senfac = max(senfac, P_swfacmin) 
 
!          print *, 'do we go into here? in stress'
!          print *, 'in stress, the swfac, senfac, and turfac is :', swfac, senfac, turfac
      endif
      !!! a temporary setting to remove water stress for rice
!      if (P_codeplante == 'ric') then
!          swfac = 1
!          senfac = 1
!          turfac = 1
!      endif

return
end subroutine stress
 
 
