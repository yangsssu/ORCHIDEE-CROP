










! Obsolete parallel tools : Barrier

!-
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_parallel/tools_para.f90 $ 
!< $Date: 2014-03-26 10:19:44 +0100 (Wed, 26 Mar 2014) $
!< $Author: josefine.ghattas $
!< $Revision: 1993 $
!-

MODULE tools_para
!-
  USE mod_orchidee_para_var, ONLY : MPI_COMM_ORCH
!-
! Redefinition of MPI function if not second underscore in MPI library.
! One must use -DMPI_SECOND__ in precompilation to activate those definitions.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/src_parallel.h,v 1.3 2009/06/24 10:15:00 ssipsl Exp $
!-
















































































































































































































!-
CONTAINS

  SUBROUTINE barrier_para()
    CALL MPI_BARRIER(MPI_COMM_ORCH,ierr)
  END SUBROUTINE barrier_para

END MODULE tools_para
