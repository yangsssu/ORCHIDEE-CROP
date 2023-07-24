










! This subrutine is addressing the Carbon allocation for crops in combination with STICS
! Author: Xuhui Wang
! Date: 03/12/2014

module crop_alloc

! use modules

USE ioipsl
USE pft_parameters
USE constantes
USE netcdf

IMPLICIT NONE

CONTAINS

subroutine crop_bmalloc(in_cycle,         &
                      deltai,           &
                      dltaisen,         &
                      ssla,             &
                      pgrain,           &
                      deltgrain,          &
                      reprac,           &
                      nger,             &
                      nlev,             &
                      ndrp,             &
                      nlax,             &    ! input 
                      nmat,             &
                      nrec,             &
!                      is_recycle,       &
                      bm_alloc_tot,     &    ! input
                      biomass,          &
                      c_reserve,        &    ! out
                      c_leafb,          &    ! out
                      bm_alloc,         &    ! inout
                      P_densitesem,     &
                      P_pgrainmaxi,     &
                      P_tigefeuil,     &
                      P_slamax,        &
                      slai,            &
                      tday_counter)               ! parameter

   !USE ioipsl
   !USE pft_parameters
   !USE constantes
   
   ! Declaration part
   
   ! 0.0 INPUT PART
   LOGICAL, INTENT(IN)                 :: in_cycle            
   REAL(r_std),  INTENT(IN)            :: deltai         ! lai increment  // unit in m2 m-2
   REAL(r_std),  INTENT(IN)            :: dltaisen       ! lai senescence  // unit in m2 m-2
   REAL(r_std),  INTENT(IN)            :: ssla           ! sla from STICS // unit in g cm -2
   REAL(r_std),  INTENT(IN)            :: pgrain         ! weight per grain (dry matter, but not carbon)  // g
   REAL(r_std),  INTENT(IN)            :: deltgrain        ! grain yield increment (dry matter but not carbon)  // unit g c / m2
   REAL(r_std),  INTENT(IN)            :: reprac 
   INTEGER(i_std),  INTENT(IN)            :: nger
   INTEGER(i_std),  INTENT(IN)            :: nlev 
   INTEGER(i_std),  INTENT(IN)            :: ndrp
   INTEGER(i_std),  INTENT(IN)            :: nlax
   INTEGER(i_std),  INTENT(IN)            :: nmat
   INTEGER(i_std),  INTENT(IN)            :: nrec
!   LOGICAL,  INTENT(IN)                   :: is_recycle
   REAL(r_std), INTENT(IN)             :: bm_alloc_tot   ! unit in g m-2 
   REAL(r_std),  INTENT(IN)            :: P_densitesem
   REAL(r_std),  INTENT(IN)            :: P_tigefeuil
   REAL(r_std),  INTENT(IN)            :: P_pgrainmaxi
   REAL(r_std),  INTENT(IN)            :: P_slamax
   REAL(r_std), DIMENSION(nparts), INTENT(INOUT)             :: biomass   ! unit in g m-2 
   INTEGER(i_std), INTENT(IN)             :: tday_counter
   ! 1.0 INOUT PART

   REAL(r_std), INTENT(INOUT)            ::c_reserve  ! crop reserve 
   REAL(r_std), INTENT(INOUT)            ::c_leafb ! crop leaf biomass derived from STICS
   REAL(r_std), INTENT(INOUT)            ::slai ! stics simulated lai
   REAL(r_std), DIMENSION(nparts), INTENT(INOUT)            ::bm_alloc ! crop leaf biomass derived from STICS
   
   ! 2.0 local 
   REAL(r_std)      ::  grainrem   ! daily grain minus reservoir, the remaining carbon
   REAL(r_std)      ::  deltmagrain   ! daily grain/ (unit in carbon)
   INTEGER(i_std)   :: ipart
   REAL(r_std)      :: tempalloc
   REAL(r_std)      :: tempdlai
   REAL(r_std)      :: netdeltai
   REAL(r_std)      :: temprest
   REAL(r_std)      :: maxremobi,sla0,sla1,remobi
   LOGICAL, PARAMETER :: mydebug=.FALSE.


    ! Part one: conversion from biomass(dry matter) to carbon
    deltmagrain = deltgrain*0.48

    ! STRATEGY: 
    ! We keep the leaf biomass, grain and reprac from STICS 
    ! Total available biomass for allocation is dltams and cropreserv. 

    !IF (bavard .GE. 3) WRITE(numout,*) 'Entering crop alloc'
    
    ! 1. whether or not necessary to enter into this process
    
    if (.not. in_cycle) then
        return   ! if not yet into the crop cycle or finish the cycle
    endif

    ! 1. initialize the bm_alloc (biomass allocation)
    !

    bm_alloc(:) = 0.      ! 8 parts
       
    ! 2.  leaf biomass from STICS 
    ! in this subroutine, we USED the Leaf biomass and GRAIN yield, the leaf biomass and grain production is adjusted accoring to different stages (in detail see leaf and grain processes)
    c_leafb = 0.
    if (in_cycle) then
       if (deltai > 0.) then  ! just for leaf growth period
           c_leafb = deltai/ssla*10000.0*0.48
       elseif (deltai < 0.) then
           c_leafb = deltai/ssla*10000.0*0.48
       else
           c_leafb = 0.
       endif
    else
       c_leafb = 0.
    endif
    netdeltai = deltai - dltaisen
  
    
    ! 3. reinitialization of leaf and fruit biomass
    if (biomass(ileaf) .gt. 0.0) then
        sla0 = slai/biomass(ileaf)
    else
        sla0 = P_slamax
    endif
    if (sla0 .eq. 0.0) then
        sla0 = P_slamax
    endif
    if (sla0 .LT. 0) then
       write(*,*) 'allocation sla error: sla0,',sla0
       STOP
    endif
    bm_alloc(ileaf) = c_leafb - dltaisen/sla0
    if (bm_alloc(ileaf) .LT. 0 .and. netdeltai .GT. 0) then
        write(*,*) 'allocation leaf error: bm_alloc(ileaf)',bm_alloc(ileaf)
    endif
    if (bm_alloc(ileaf)<0 .and. biomass(ileaf)+bm_alloc(ileaf)<0) then
        bm_alloc(ileaf) = -biomass(ileaf)
    endif
    bm_alloc(ifruit)= deltmagrain
    bm_alloc(iroot) = bm_alloc_tot * reprac
    if (reprac .GE. 1.) then
       write(numout,*) 'reprac > 1: ',reprac
       stop
    endif
    bm_alloc(isapabove) = P_tigefeuil * c_leafb
    bm_alloc(icarbres) = 0.

    if (mydebug) then
        write(numout,*) 'xuhui, alloc initial:'
        write(numout,*) 'biomass(ileaf) ', biomass(ileaf)
        write(numout,*) 'slai ', slai
        write(numout,*) 'sla0 ', sla0
        write(numout,*) 'deltai ', deltai
        write(numout,*) 'dltaisen ', dltaisen
        write(numout,*) 'reprac ', reprac
        write(numout,*) 'deltmagrain ', deltmagrain
        write(numout,*) 'P_tigefeuil ', P_tigefeuil
        write(numout,*) 'bm_alloc_tot ', bm_alloc_tot
        write(numout,*) 'bm_alloc(ileaf,isapabove,iroot,ifruit) '
        write(numout,*)  bm_alloc(ileaf), bm_alloc(isapabove), bm_alloc(iroot), bm_alloc(ifruit)
        write(numout,*) 'nger nlev nlax ndrp nrec '
        write(numout,*)  nger, nlev, nlax, ndrp,  nrec
        write(numout,*) 'biomass(ileaf, isapabove, iroot, ifruit, icarbres) '
        write(numout,*)  biomass(ileaf), biomass(isapabove), biomass(iroot), biomass(ifruit), biomass(icarbres)
    endif
    
    ! it is possible that bm_alloc(ileaf) is negative
 
    ! 4.  real allocation for each grid and each pft


    ! STRATEGY: 
    ! 1. carbon allocation priority is different for different parts;
    ! 2. even for the same pool, the priority is changing along with time (stage revolution) 
    
    ! 3.1 FOR STAGE [nger, nlev]

    ! the c_reserve starts to decreasing because the root growth
    ! and we allocate all carbon into root

    if ((nger .gt. 0) .and. (nlev .eq. 0)) then ! germination occured but did not emerge, during this stage only root and reserve pools
       if ( biomass(icarbres) > 0.) then  ! adjust the reserve dynamics
          ! addressing the c_reserve dynamics
          bm_alloc(iroot) = biomass(icarbres)*reprac 
          bm_alloc(icarbres) = 0. - biomass(icarbres)*reprac 
          bm_alloc(ileaf) = 0.
          bm_alloc(isapabove) = 0.
          bm_alloc(ifruit) = 0.
       else
          !c_reserve = 0.
          bm_alloc(icarbres) = 0.
          bm_alloc(iroot) = 0.
          bm_alloc(ileaf) = 0.
          bm_alloc(isapabove) = 0.
          bm_alloc(ifruit) = 0.
       endif
    endif
    
    ! 3.2 FOR STAGE [NLEV, NDRP)
    if ((nlev .gt. 0) .and. (ndrp .eq. 0)) then 
    ! emergence and photosynthese, whereas grain is not filling
    ! in this stage, we keep the leaf and grain biomass
    ! root with the higher priority
        tempalloc =  bm_alloc(ileaf)+bm_alloc(iroot)+bm_alloc(isapabove)
        bm_alloc(ifruit) = 0.
        if (tempalloc > bm_alloc_tot) then
            if (tempalloc < bm_alloc_tot + biomass(icarbres)) then                
!                biomass(icarbres) = biomass(icarbres) - (tempalloc - bm_alloc_tot)
                bm_alloc(icarbres) = - (tempalloc - bm_alloc_tot)
            else ! new c + c reserve is insufficient to meet the demand
                if (biomass(icarbres)<0) then
                    biomass(icarbres) = 0.
                    bm_alloc(icarbres) = 0.
                else
                    bm_alloc(icarbres) = - biomass(icarbres)
                endif
                bm_alloc(iroot) = reprac * bm_alloc_tot
                if ( tday_counter >= nlev .and. tday_counter < nlev+8) then ! we create some biomass for leaf at the beginning
                    bm_alloc(ileaf) = deltai/P_slamax*10000.0*0.48
                    bm_alloc(icarbres) = -bm_alloc(ileaf)
                else
                    tempdlai = (bm_alloc_tot + biomass(icarbres) - bm_alloc(iroot))/(1+P_tigefeuil)*ssla/10000.0/0.48
                    bm_alloc(ileaf) = tempdlai/ssla*10000.0*0.48
                    bm_alloc(isapabove) = P_tigefeuil*bm_alloc(ileaf)
                    if (netdeltai > tempdlai) then
                        slai = slai - (netdeltai - tempdlai)
                    endif
                endif
            endif
        else
            bm_alloc(icarbres) = bm_alloc_tot - tempalloc
        endif
!        if (bm_alloc(ileaf)<0) then ! remobilize the leaf biomass for future use (grain mainly)
!           bm_alloc(icarbres) = bm_alloc(icarbres) - bm_alloc(ileaf)
!        endif
!    endif
  
    
    ! 3.3 STAGE [ndrp nrec) 
    ! in this stage, there is potentially competition between leaf and fruit,
    ! conserve root/shoot ratio, fulfil grain first, reduce deltai when necessary
    ! note that leaf growth stop at nlax, grain filling stop at nmat
    ! no fundamental differences exist for [nlax, nmat)
       
    else if ((ndrp .gt. 0) .and. ( (nmat .eq. 0) .or. (tday_counter .eq. nmat))) then ! from grain filling to maturity
       tempalloc =  bm_alloc(ileaf)+bm_alloc(ifruit)+bm_alloc(iroot)+bm_alloc(isapabove)

       if (tempalloc > bm_alloc_tot) then
           if (tempalloc < bm_alloc_tot + biomass(icarbres)) then ! use c reserval
!               biomass(icarbres) = biomass(icarbres) - (tempalloc - bm_alloc_tot)
               bm_alloc(icarbres) = - (tempalloc - bm_alloc_tot)
           else ! new c + c reserval is insufficient to meet the demand
               if (biomass(icarbres)<0) then
                   biomass(icarbres) = 0.
               else
                   bm_alloc(icarbres) = 0. - biomass(icarbres)
               endif
               bm_alloc(iroot) = reprac * bm_alloc_tot
               if (bm_alloc(ileaf)>=0) then
                   temprest = bm_alloc_tot + biomass(icarbres) - bm_alloc(iroot)
               else ! bm_alloc(ileaf) < 0 
                   temprest = bm_alloc_tot + biomass(icarbres) - bm_alloc(ileaf) - bm_alloc(iroot)
               endif
               if (temprest < bm_alloc(ifruit)) then ! if not sufficient for grain alone
                   if (nlax .gt. 0) then ! remobilize some carbon from leaf
                       sla0 = slai/biomass(ileaf)*10000.0*0.48 ! m2/gC --> cm2/g drymass
                       sla1 = min(sla0+5,P_slamax)
                       maxremobi = slai/(sla0/0.48/10000.0) - slai/(sla1/0.48/10000)   !cm2/g --> m2/gC
                       remobi = min(maxremobi, bm_alloc(ifruit)-temprest)
                       
                       bm_alloc(ifruit) = temprest + remobi
                       bm_alloc(ileaf) = min(bm_alloc(ileaf),0.) - remobi
                       ! when bm_alloc(ileaf)<0, further remove remobi
                       ! when bm_alloc(ileaf)>0, then stop the planned location to lai
                       bm_alloc(isapabove) = 0.
                   else
                       bm_alloc(ifruit) = temprest
                       bm_alloc(ileaf) = min(0., bm_alloc(ileaf))
                       bm_alloc(isapabove) = 0.
                       if (netdeltai>0) then
                           slai = slai - netdeltai
                       endif
                   endif
               else !temprest >= bm_alloc(ifruit)
                   if (bm_alloc(ileaf)<0) then ! the rest is left in reserval
                       bm_alloc(icarbres) = temprest - bm_alloc(ifruit)
                   else !  the rest goes to leaf
                       tempdlai = (temprest - bm_alloc(ifruit))/(1+P_tigefeuil)*ssla/10000.0/0.48
                       bm_alloc(ileaf) = tempdlai/ssla*10000.0*0.48
                       bm_alloc(isapabove) = P_tigefeuil*bm_alloc(ileaf)
                       if (netdeltai > tempdlai) then ! which is almost guarantee because bm_alloc(ileaf)>0
                           slai = slai - (netdeltai - tempdlai)
                       endif
                   endif
               endif
           endif
       else
           bm_alloc(icarbres) = bm_alloc_tot - tempalloc
       endif   
!    endif

    ! 3.5 stage [nmat nrec)
    else if ((nmat .gt. 0) .and. (nrec .eq. 0))  then
        ! xuhui noted:
        ! in STICS, nmat = physiology maturity, which means 
        ! harvested organs stop growing in dry matter (p20 STICS book)
        ! It is difficult to imagine vegetative part (leaf, root) are still growing,
        ! while the harvest organs are not. So my decision is that
        ! no allocation to any parts of the crop
        ! = no more npp
        if (bm_alloc(ileaf)<0) then
            bm_alloc(isapabove) = -bm_alloc(ileaf)
            bm_alloc(iroot) = 0.
            bm_alloc(ifruit) = 0.
            bm_alloc(icarbres) = 0.
        else
            bm_alloc(:) = 0.
        endif
!    endif    
!    ! 3.5 STAGE [When nrec occur]
!    ! when harvest, we alloc some carbon into reservoire pool
!    
!    else if ( nrec .gt. 0  .OR.  (nmat .GT. 0 .AND. is_recycle) ) then  ! harvest occurs
    else if ( nrec .gt. 0 ) then  ! harvest occurs
!    ! only allocate to root & grain
!       bm_alloc(isapabove) = 0.
!       if (bm_alloc(ileaf)<0)  then
!           tempalloc =  bm_alloc(ifruit) + bm_alloc(iroot) + bm_alloc(ileaf)
!       else !bm_alloc(ileaf)>0 ! this should be a buggy boundary condition
!           bm_alloc(ileaf) = 0.
!           tempalloc = bm_alloc(ifruit) + bm_alloc(iroot)
!       endif
!
!       if (tempalloc >= bm_alloc_tot) then !no enough c
!          bm_alloc(ifruit) = bm_alloc_tot - bm_alloc(iroot)
!          bm_alloc(icarbres) = 0.          
!       else 
!          bm_alloc(icarbres) =bm_alloc_tot - tempalloc
!       endif
       ! but we have to put some carbon into reserve (seeds for the next year) 
       c_reserve = P_densitesem*pgrain*0.48 ! seeds 
       bm_alloc(ileaf) = 0. !at harvest, the senescence will be treated as litter, not as re-allocation
       if (biomass(icarbres) > 0.) then
           bm_alloc(isapabove) = biomass(icarbres) !the rest of c reservoire will be return to soil as litter
           bm_alloc(icarbres) = -biomass(icarbres)
       else
           bm_alloc(isapabove) = 0.
           bm_alloc(icarbres) = 0.
       endif
!       biomass(ifruit) =  biomass(ifruit) - c_reserve   !max(grainrem, 0.); 
       bm_alloc(ifruit) = -c_reserve
       bm_alloc(icarbres) = bm_alloc(icarbres) + c_reserve
       DO ipart = 1,nparts
           IF (bm_alloc(ipart)<0) THEN
               WRITE(numout,*) 'ipart :',ipart
               WRITE(numout,*) 'bm_alloc < 0 :',bm_alloc(ipart)
               WRITE(numout,*) 'biomass :', biomass(ipart)
           ENDIF
       ENDDO
    else
!        write(numout,*) 'growth stage not recognized'
        write(numout,*) 'no allocation occurred in tday_counter: ', tday_counter
        write(numout,*) 'nger, nlev, ndrp, nmat, nrec'
        write(numout,*) nger, nlev, ndrp, nmat, nrec
!        STOP
    endif

    if (mydebug)  then
        write(numout,*) 'xuhui, leaving crop_alloc'
        write(numout,*) 'bm_alloc(ileaf,isapabove,iroot,ifruit,icarbres) '
        write(numout,*)  bm_alloc(ileaf), bm_alloc(isapabove), bm_alloc(iroot), bm_alloc(ifruit), bm_alloc(icarbres)
        write(numout,*) 'slai: ', slai
    endif
  
end subroutine crop_bmalloc

end module crop_alloc
