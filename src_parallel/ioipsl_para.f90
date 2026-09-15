! Overlap of IOIPSL functions for specific parallel use in ORCHIDEE.

!-
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_parallel/ioipsl_para.f90 $ 
!< $Date: 2016-03-25 17:33:03 +0100 (Fri, 25 Mar 2016) $
!< $Author: albert.jornet $
!< $Revision: 3318 $
!-

MODULE ioipsl_para
  USE ioipsl
  USE mod_orchidee_para_var
  USE mod_orchidee_transfert_para
!-
  IMPLICIT NONE

  INTEGER, SAVE :: orch_domain_id 
!-
   INTEGER :: orch_ipslout=6, orch_ilv_cur=0, orch_ilv_max=0
!$OMP THREADPRIVATE( orch_ipslout, orch_ilv_cur, orch_ilv_max )

!-
!-
#include "src_parallel.h"
!-
  INTERFACE getin_p
    MODULE PROCEDURE getin_p_c,getin_p_c1,   &
         getin_p_i,getin_p_i1,getin_p_i2,&
         getin_p_r,getin_p_r1,getin_p_r2,&
         getin_p_l,getin_p_l1,getin_p_l2
  END INTERFACE
!-
  INTERFACE restput_p
     MODULE PROCEDURE &
          restput_p_r3d, restput_p_r2d, restput_p_r1d, &
          restput_p_opp_r3d, restput_p_opp_r2d, restput_p_opp_r1d
  END INTERFACE
!-
  INTERFACE restget_p
     MODULE PROCEDURE &
          restget_p_r3d, restget_p_r2d, restget_p_r1d, &
          restget_p_opp_r3d, restget_p_opp_r2d, restget_p_opp_r1d
  END INTERFACE


  INTERFACE histwrite_p
     MODULE PROCEDURE &
     histwrite_r1d_p,histwrite_r2d_p,histwrite_r3d_p     
  END INTERFACE

CONTAINS



  SUBROUTINE Init_ioipsl_para

    IMPLICIT NONE
    
    INTEGER,DIMENSION(2) :: ddid
    INTEGER,DIMENSION(2) :: dsg
    INTEGER,DIMENSION(2) :: dsl
    INTEGER,DIMENSION(2) :: dpf
    INTEGER,DIMENSION(2) :: dpl
    INTEGER,DIMENSION(2) :: dhs
    INTEGER,DIMENSION(2) :: dhe 

    IF (is_omp_root) THEN
      ddid=(/ 1,2 /)
      dsg=(/ iim_g, jjm_g /)
      dsl=(/ iim_g, jj_nb /)
      dpf=(/ 1,jj_begin /)
      dpl=(/ iim_g, jj_end /)
      dhs=(/ ii_begin-1,0 /)
      if (mpi_rank==mpi_size-1) then
        dhe=(/0,0/)
      else
         dhe=(/ iim_g-ii_end,0 /)  
      endif
    
      call flio_dom_set(mpi_size,mpi_rank,ddid,dsg,dsl,dpf,dpl,dhs,dhe, &
                        'APPLE',orch_domain_id)
     ENDIF
     
  END SUBROUTINE Init_ioipsl_para

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   Definition de ioconf_setatt_p      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE ioconf_setatt_p (attname,attvalue)
!---------------------------------------------------------------------
  IMPLICIT NONE
!-
  CHARACTER(LEN=*), INTENT(in) :: attname,attvalue
!---------------------------------------------------------------------

  IF (is_root_prc) THEN 
     CALL ioconf_setatt(attname,attvalue)
  ENDIF

END SUBROUTINE ioconf_setatt_p

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   Definition de parallel ipslerr functions    !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!===
SUBROUTINE ipslnlf_p (new_number,old_number)
!!--------------------------------------------------------------------
!! The "ipslnlf" routine allows to know and modify
!! the current logical number for the messages.
!!
!! SUBROUTINE ipslnlf (new_number,old_number)
!!
!! Optional INPUT argument
!!
!! (I) new_number : new logical number of the file
!!
!! Optional OUTPUT argument
!!
!! (I) old_number : current logical number of the file
!!--------------------------------------------------------------------
  IMPLICIT NONE
!-
  INTEGER,OPTIONAL,INTENT(IN)  :: new_number
  INTEGER,OPTIONAL,INTENT(OUT) :: old_number
!---------------------------------------------------------------------
  IF (PRESENT(old_number)) THEN
#ifndef CPP_OMP
    CALL ipslnlf(old_number=orch_ipslout)
#endif
    old_number = orch_ipslout
  ENDIF
  IF (PRESENT(new_number)) THEN
    orch_ipslout = new_number
#ifndef CPP_OMP
    CALL ipslnlf(new_number=orch_ipslout)
#endif
  ENDIF
!---------------------
END SUBROUTINE ipslnlf_p
!===
SUBROUTINE ipslerr_p (plev,pcname,pstr1,pstr2,pstr3)
!---------------------------------------------------------------------
!! The "ipslerr_p" routine
!! allows to handle the messages to the user.
!!
!! parallel version of IOIPSL ipslerr
!!
!! INPUT
!!
!! plev   : Category of message to be reported to the user
!!          1 = Note to the user
!!          2 = Warning to the user
!!          3 = Fatal error
!! pcname : Name of subroutine which has called ipslerr
!! pstr1   
!! pstr2  : Strings containing the explanations to the user
!! pstr3
!---------------------------------------------------------------------
   IMPLICIT NONE
!-
   INTEGER :: plev
   CHARACTER(LEN=*) :: pcname,pstr1,pstr2,pstr3
!-
   CHARACTER(LEN=30),DIMENSION(3) :: pemsg = &
  &  (/ "NOTE TO THE USER FROM ROUTINE ", &
  &     "WARNING FROM ROUTINE          ", &
  &     "FATAL ERROR FROM ROUTINE      " /)
!---------------------------------------------------------------------
   IF ( (plev >= 1).AND.(plev <= 3) ) THEN
     orch_ilv_cur = plev
     orch_ilv_max = MAX(orch_ilv_max,plev)
     WRITE(orch_ipslout,'(/,A," ",A)') TRIM(pemsg(plev)),TRIM(pcname)
     WRITE(orch_ipslout,'(3(" --> ",A,/))') TRIM(pstr1),TRIM(pstr2),TRIM(pstr3)
   ENDIF
   IF (plev == 3) THEN
     WRITE(orch_ipslout,'("Fatal error from ORCHIDEE. STOP in ipslerr_p with code")')
#if defined (__INTEL_COMPILER) || defined(__GFORTRAN__)
     CALL FLUSH(orch_ipslout)
#endif
 
#ifdef CPP_PARA
    CALL MPI_ABORT(plev)
#endif     
    STOP 1
   ENDIF
!---------------------
END SUBROUTINE ipslerr_p

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   Definition des getin -> bcast      !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! -- Les chaines de caracteres -- !!
  
  SUBROUTINE getin_p_c(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    CHARACTER(LEN=*),INTENT(INOUT) :: VarOut    

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_c  


  SUBROUTINE getin_p_c1(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    CHARACTER(LEN=*),INTENT(INOUT) :: VarOut(:)    

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_c1 

!! -- Les entiers -- !!
  
  SUBROUTINE getin_p_i(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    INTEGER,INTENT(INOUT) :: VarOut    

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_i

  SUBROUTINE getin_p_i1(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    INTEGER,INTENT(INOUT) :: VarOut(:)

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_i1

  SUBROUTINE getin_p_i2(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    INTEGER,INTENT(INOUT) :: VarOut(:,:)

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_i2

!! -- Les flottants -- !!
  
  SUBROUTINE getin_p_r(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    REAL,INTENT(INOUT) :: VarOut

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_r

  SUBROUTINE getin_p_r1(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    REAL,INTENT(INOUT) :: VarOut(:)

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_r1

  SUBROUTINE getin_p_r2(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    REAL,INTENT(INOUT) :: VarOut(:,:)

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_r2

!! -- Les Booleens -- !!
  
  SUBROUTINE getin_p_l(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    LOGICAL,INTENT(INOUT) :: VarOut

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_l

  SUBROUTINE getin_p_l1(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    LOGICAL,INTENT(INOUT) :: VarOut(:)

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_l1

  SUBROUTINE getin_p_l2(VarIn,VarOut)
    IMPLICIT NONE    
    CHARACTER(LEN=*),INTENT(IN) :: VarIn
    LOGICAL,INTENT(INOUT) :: VarOut(:,:)

    IF (is_root_prc) CALL getin(VarIn,VarOut)
    CALL bcast(VarOut)
  END SUBROUTINE getin_p_l2
!-
!-----------------------------
!-----------------------------
!-----------------------------
!-
  SUBROUTINE restget_p_opp_r1d &
  (fid, vname_q, iim, jjm, llm, itau, def_beha, &
   var, MY_OPERATOR, nbindex, ijndex)
! DO NOT USE THIS FUNCTION WITH NON GRID VARIABLE !
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    LOGICAL def_beha
    REAL :: var(:)
    CHARACTER(LEN=*) :: MY_OPERATOR
    INTEGER :: nbindex, ijndex(nbindex)
    !-----------------------------
    REAL, ALLOCATABLE, DIMENSION(:) :: temp_g

    IF (is_root_prc) THEN 
       ALLOCATE( temp_g(iim*jjm*llm) )
    ELSE
       ALLOCATE( temp_g(1) )
    ENDIF
        
    IF (is_root_prc) THEN 
       CALL restget &
            (fid, vname_q, iim, jjm, llm, itau, def_beha, &
            temp_g, MY_OPERATOR, nbindex, ijndex)
    ENDIF
    CALL scatter(temp_g,var)
    DEALLOCATE(temp_g)
  END SUBROUTINE restget_p_opp_r1d
!-
!===
!-
  SUBROUTINE restget_p_opp_r2d &
  (fid, vname_q, iim, jjm, llm, itau, def_beha, &
   var, MY_OPERATOR, nbindex, ijndex)
    IMPLICIT NONE
    !-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    LOGICAL def_beha
    REAL :: var(:,:)
    CHARACTER(LEN=*) :: MY_OPERATOR
    INTEGER :: nbindex, ijndex(nbindex)
    !-----------------------------
    REAL, ALLOCATABLE, DIMENSION(:,:) :: temp_g

    IF (is_root_prc) THEN 
       ALLOCATE( temp_g(iim,jjm) )
    ELSE
      ALLOCATE( temp_g(1,1) )
    ENDIF

    IF (is_root_prc) THEN 
       CALL restget &
            (fid, vname_q, iim, jjm, llm, itau, def_beha, &
            temp_g, MY_OPERATOR, nbindex, ijndex)
    ENDIF
    CALL scatter(temp_g,var)
    DEALLOCATE(temp_g)
  END SUBROUTINE restget_p_opp_r2d
!-
!===
!-
  SUBROUTINE restget_p_opp_r3d &
  (fid, vname_q, iim, jjm, llm, itau, def_beha, &
   var, MY_OPERATOR, nbindex, ijndex)
    IMPLICIT NONE
    !-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    LOGICAL def_beha
    REAL :: var(:,:,:)
    CHARACTER(LEN=*) :: MY_OPERATOR
    INTEGER :: nbindex, ijndex(nbindex)
    !-----------------------------
    CHARACTER(LEN=5) :: part_str
    CHARACTER(LEN=LEN(vname_q) + LEN(part_str) + 1) :: var_name
    INTEGER :: m

    DO m=1, SIZE(var, 3)
        WRITE(part_str,'(I2)') m
        IF ( m < 10 ) part_str(1:1) = '0'
        var_name = TRIM(vname_q)//'_'//part_str(1:LEN_TRIM(part_str))
        CALL restget_p (fid, var_name, iim, jjm, 1, itau, def_beha, &
             var(:,:,m), MY_OPERATOR, nbindex, ijndex)
    END DO

END SUBROUTINE restget_p_opp_r3d
!-
!===
!-
  SUBROUTINE restget_p_r1d &
  (fid,vname_q,iim,jjm,llm,itau,def_beha,var)
! DO NOT USE THIS FUNCTION WITH NON GRID VARIABLE !
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    LOGICAL :: def_beha
    REAL :: var(:)
    !-------------------------
    REAL, ALLOCATABLE, DIMENSION(:) :: temp_g

    IF (is_root_prc) THEN 
       ALLOCATE( temp_g(iim*jjm*llm) )
    ELSE
       ALLOCATE( temp_g(1) )
    ENDIF

    IF (is_root_prc) THEN 
       CALL restget &
            (fid,vname_q,iim,jjm,llm,itau,def_beha,temp_g)
    ENDIF
    CALL scatter(temp_g,var)
    DEALLOCATE(temp_g)
  END SUBROUTINE restget_p_r1d
!-
!===
!-
  SUBROUTINE restget_p_r2d &
  (fid,vname_q,iim,jjm,llm,itau,def_beha,var)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    LOGICAL :: def_beha
    REAL :: var(:,:)
    !-------------------------
    REAL, ALLOCATABLE, DIMENSION(:,:) :: temp_g

    IF (is_root_prc) THEN 
       ALLOCATE( temp_g(iim,jjm) )
    ELSE
       ALLOCATE( temp_g(1,1) )
    ENDIF
    IF (is_root_prc) THEN 
       CALL restget &
            (fid,vname_q,iim,jjm,llm,itau,def_beha,temp_g)
    ENDIF
    CALL scatter(temp_g,var)
    DEALLOCATE(temp_g)
  END SUBROUTINE restget_p_r2d
!-
!===
!-
  SUBROUTINE restget_p_r3d &
  (fid,vname_q,iim,jjm,llm,itau,def_beha,var)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    LOGICAL def_beha
    REAL :: var(:,:,:)
    !-------------------------
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: temp_g

    IF (is_root_prc) THEN 
       ALLOCATE( temp_g(iim,jjm,llm) )
    ELSE 
       ALLOCATE( temp_g(1,1,1) )
    ENDIF
    
    IF (is_root_prc) THEN 
       CALL restget &
            (fid,vname_q,iim,jjm,llm,itau,def_beha,temp_g)
    ENDIF
    CALL scatter(temp_g,var)
    DEALLOCATE(temp_g)
  END SUBROUTINE restget_p_r3d
!-
!-----------------------------
!-----------------------------
!-
  SUBROUTINE restput_p_opp_r1d &
  (fid, vname_q, iim, jjm, llm, itau, var, MY_OPERATOR, nbindex, ijndex)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    REAL :: var(:)
    CHARACTER(LEN=*) :: MY_OPERATOR
    INTEGER :: nbindex, ijndex(nbindex)
    !-----------------------------
    REAL, ALLOCATABLE, DIMENSION(:) :: temp_g

    IF (is_root_prc) THEN 
      ALLOCATE( temp_g(iim*jjm*llm) )
    ELSE
      ALLOCATE ( temp_g(1) )
    ENDIF
    
    CALL gather(var,temp_g)
    IF (is_root_prc) THEN
       CALL restput &
            (fid, vname_q, iim, jjm, llm, itau, temp_g, MY_OPERATOR, nbindex, ijndex)
    ENDIF

    DEALLOCATE( temp_g )
          
  END SUBROUTINE restput_p_opp_r1d
!-
!===
!-
  SUBROUTINE restput_p_opp_r2d &
  (fid, vname_q, iim, jjm, llm, itau, var, MY_OPERATOR, nbindex, ijndex)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    REAL :: var(:,:)
    CHARACTER(LEN=*) :: MY_OPERATOR
    INTEGER :: nbindex, ijndex(nbindex)
    !-----------------------------
    REAL, ALLOCATABLE, DIMENSION(:,:) :: temp_g

    IF (is_root_prc) THEN
      ALLOCATE( temp_g(iim,jjm) )
    ELSE
      ALLOCATE( temp_g(1,1) )
    ENDIF
          
    CALL gather(var,temp_g)
    IF (is_root_prc) THEN
       CALL restput &
            (fid, vname_q, iim, jjm, llm, itau, temp_g, MY_OPERATOR, nbindex, ijndex)
    ENDIF
    DEALLOCATE( temp_g )
          
  END SUBROUTINE restput_p_opp_r2d
!-
!===
!-
  SUBROUTINE restput_p_opp_r3d &
  (fid, vname_q, iim, jjm, llm, itau, var, MY_OPERATOR, nbindex, ijndex)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    REAL :: var(:,:,:)
    CHARACTER(LEN=*) :: MY_OPERATOR
    INTEGER :: nbindex, ijndex(nbindex)
    !-----------------------------
    CHARACTER(LEN=5) :: part_str
    CHARACTER(LEN=LEN(part_str) + LEN(vname_q) + 1) :: var_name
    INTEGER :: m

    DO m=1,SIZE(var, 3)
        WRITE(part_str,'(I2)') m
        IF (m<10) part_str(1:1)='0'
        var_name = TRIM(vname_q)//'_'//part_str(1:LEN_TRIM(part_str))
        CALL restput_p &
             &    (fid, var_name, iim, jjm , 1, itau, &
             &     var(:,:,m), MY_OPERATOR, nbindex, ijndex)
    ENDDO

          
  END SUBROUTINE restput_p_opp_r3d
!-
!===
!-
  SUBROUTINE restput_p_r1d (fid,vname_q,iim,jjm,llm,itau,var)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    REAL :: var(:)
    !-----------------------------
    REAL, ALLOCATABLE, DIMENSION(:) :: temp_g

    IF (is_root_prc) THEN
      ALLOCATE( temp_g(iim*jjm*llm) )
    ELSE
      ALLOCATE( temp_g(1) )
    ENDIF
    
    CALL gather(var,temp_g)
    IF (is_root_prc) THEN
       CALL restput (fid,vname_q,iim,jjm,llm,itau,temp_g)
    ENDIF
    DEALLOCATE( temp_g )
          
  END SUBROUTINE restput_p_r1d
!-
!===
!-
  SUBROUTINE restput_p_r2d (fid,vname_q,iim,jjm,llm,itau,var)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    REAL :: var(:,:)
    !-------------------------
    REAL, ALLOCATABLE, DIMENSION(:,:) :: temp_g

    IF (is_root_prc) THEN
      ALLOCATE( temp_g(iim,jjm) )
    ELSE
      ALLOCATE( temp_g(1,1) )
    ENDIF
    
    CALL gather(var,temp_g)
    IF (is_root_prc) THEN
       CALL restput (fid,vname_q,iim,jjm,llm,itau,temp_g)
    ENDIF
    DEALLOCATE( temp_g )
          
  END SUBROUTINE restput_p_r2d
!-
!===
!-
  SUBROUTINE restput_p_r3d (fid,vname_q,iim,jjm,llm,itau,var)
    IMPLICIT NONE
!-
    INTEGER :: fid
    CHARACTER(LEN=*) :: vname_q
    INTEGER :: iim, jjm, llm, itau
    REAL :: var(:,:,:)
    !-------------------------
    REAL, ALLOCATABLE, DIMENSION(:,:,:) :: temp_g

    IF (is_root_prc) THEN
      ALLOCATE( temp_g(iim,jjm,llm) )
    ELSE
      ALLOCATE( temp_g(iim,jjm,llm) )
    ENDIF
    
    CALL gather(var,temp_g)
    IF (is_root_prc) THEN
       CALL restput (fid,vname_q,iim,jjm,llm,itau,temp_g)
    ENDIF
    DEALLOCATE( temp_g )
          
  END SUBROUTINE restput_p_r3d


  SUBROUTINE histwrite_r1d_p(pfileid,pvarname,pitau,pdata,nbindex,nindex)
    IMPLICIT NONE
!-
    INTEGER,INTENT(IN) :: pfileid, pitau, nbindex, nindex(nbindex)
    REAL,DIMENSION(:),INTENT(IN) :: pdata
    CHARACTER(LEN=*),INTENT(IN) :: pvarname
    
    REAL,DIMENSION(nbp_mpi)    :: pdata_mpi
    
    IF (pfileid > 0) THEN 
       ! Continue only if the file is initilalized
       CALL gather_omp(pdata,pdata_mpi)
       IF (is_omp_root) THEN
          CALL histwrite(pfileid,pvarname,pitau,pdata_mpi,nbp_mpi,kindex_mpi) 
       ENDIF
    END IF
      
  END SUBROUTINE histwrite_r1d_p
  

  SUBROUTINE histwrite_r2d_p(pfileid,pvarname,pitau,pdata,nbindex,nindex)
    IMPLICIT NONE
!-
    INTEGER,INTENT(IN) :: pfileid, pitau, nbindex, nindex(nbindex)
    REAL,DIMENSION(:,:),INTENT(IN) :: pdata
    CHARACTER(LEN=*),INTENT(IN) :: pvarname

    IF (pfileid > 0) THEN 
       ! Continue only if the file is initilalized
       CALL body(size(pdata,2),nindex)
    END IF

  CONTAINS 

    SUBROUTINE body(dim,nindex)
    INTEGER :: dim
    INTEGER :: nindex(nbp_omp,dim)
    
    INTEGER :: nindex_mpi(nbp_mpi,dim)
    REAL    :: pdata_mpi(nbp_mpi,dim)
    
      CALL gather_omp(pdata,pdata_mpi)
      CALL gather_omp(nindex,nindex_mpi)
    
      IF (is_omp_root) THEN
       CALL histwrite(pfileid,pvarname,pitau,pdata_mpi,nbp_mpi*dim,reshape(nindex_mpi,(/nbp_mpi*dim/)))
      ENDIF
    END SUBROUTINE body
       
  END SUBROUTINE histwrite_r2d_p

  
  SUBROUTINE histwrite_r3d_p(pfileid,pvarname,pitau,pdata,nbindex,nindex)
    IMPLICIT NONE
!-
    INTEGER,INTENT(IN) :: pfileid, pitau, nbindex, nindex(nbindex)
    REAL,DIMENSION(:,:,:),INTENT(IN) :: pdata
    CHARACTER(LEN=*),INTENT(IN) :: pvarname
  
    STOP 2 
    
  END SUBROUTINE histwrite_r3d_p


END MODULE ioipsl_para
