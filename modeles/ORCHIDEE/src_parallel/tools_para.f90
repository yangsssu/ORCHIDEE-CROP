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
#include "src_parallel.h"
!-
CONTAINS

  SUBROUTINE barrier_para()
#ifdef CPP_PARA
    CALL MPI_BARRIER(MPI_COMM_ORCH,ierr)
#endif
  END SUBROUTINE barrier_para

END MODULE tools_para
