! Overlapp of MPI functions not present in some MPI implementations.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/mpi_dummy.h,v 1.2 2007/06/12 08:04:26 ssipsl Exp $
!-
#ifdef MPI_SECOND__

SUBROUTINE MPI_NULL_COPY_FN
END SUBROUTINE MPI_NULL_COPY_FN

SUBROUTINE MPI_NULL_DELETE_FN
END SUBROUTINE MPI_NULL_DELETE_FN

SUBROUTINE MPI_COMM_NULL_COPY_FN
END SUBROUTINE MPI_COMM_NULL_COPY_FN

SUBROUTINE MPI_COMM_NULL_DELETE_FN
END SUBROUTINE MPI_COMM_NULL_DELETE_FN

SUBROUTINE MPI_TYPE_NULL_COPY_FN
END SUBROUTINE MPI_TYPE_NULL_COPY_FN

SUBROUTINE MPI_TYPE_NULL_DELETE_FN
END SUBROUTINE MPI_TYPE_NULL_DELETE_FN

SUBROUTINE MPI_WIN_NULL_COPY_FN
END SUBROUTINE MPI_WIN_NULL_COPY_FN

SUBROUTINE MPI_WIN_NULL_DELETE_FN
END SUBROUTINE MPI_WIN_NULL_DELETE_FN

SUBROUTINE MPI_DUP_FN
END SUBROUTINE MPI_DUP_FN

SUBROUTINE MPI_COMM_DUP_FN
END SUBROUTINE MPI_COMM_DUP_FN

SUBROUTINE MPI_TYPE_DUP_FN
END SUBROUTINE MPI_TYPE_DUP_FN

SUBROUTINE MPI_WIN_DUP_FN
END SUBROUTINE MPI_WIN_DUP_FN

FUNCTION MPI_WTIME () RESULT (R)
  DOUBLE PRECISION R
END FUNCTION MPI_WTIME

FUNCTION MPI_WTICK () RESULT (R)
  DOUBLE PRECISION :: R
END FUNCTION MPI_WTICK

FUNCTION PMPI_WTIME () RESULT (R)
  DOUBLE PRECISION R
END FUNCTION PMPI_WTIME

FUNCTION PMPI_WTICK () RESULT (R)
  DOUBLE PRECISION R
END FUNCTION PMPI_WTICK

#endif