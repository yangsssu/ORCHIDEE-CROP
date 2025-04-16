! ================================================================================================================================
!  MODULE       : mod_orchidee_para_var
!
!  CONTACT      : orchidee-help _at_ ipsl.jussieu.fr
!
!  LICENCE      : IPSL (2006)
!  This software is governed by the CeCILL licence see ORCHIDEE/ORCHIDEE_CeCILL.LIC
!
!>\BRIEF   This module contains public variables for parallelization and domain decomposition 
!!
!!\n DESCRIPTION: \n
!!
!! REFERENCE(S)	: None
!!
!! SVN          :
!! $HeadURL: $
!! $Date: $
!! $Revision: $
!! \n
!_ ================================================================================================================================

MODULE mod_orchidee_para_var

  USE defprec

  !
  ! 1. Variables related to the MPI parallelization and the MPI horizontal domain decompostion. 
  !    These variables were previously declared in mod_orchidee_mpi_data
  !

  ! Unit for output messages
  INTEGER(i_std), SAVE :: numout = 6
  !$OMP THREADPRIVATE(numout)

  INTEGER,SAVE :: mpi_size                                            !! Number of parallel processes
  INTEGER,SAVE :: mpi_rank                                            !! my rank num
  INTEGER,SAVE :: mpi_rank_root                                       !! rank of MPI root
  LOGICAL,SAVE :: is_mpi_root                                         !! Only MPI root proc is true
  LOGICAL,SAVE :: is_ok_mpi                                           

  INTEGER(i_std),SAVE              :: nbp_mpi                         !! number of local continental points in each mpi group
  INTEGER(i_std),SAVE              :: nbp_glo                         !! number of global continental points
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: nbp_mpi_para
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: nbp_mpi_para_begin
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: nbp_mpi_para_end 

  INTEGER,SAVE :: nbp_mpi_begin
  INTEGER,SAVE :: nbp_mpi_end


  INTEGER(i_std),SAVE              :: iim_g                           !! Dimension of global fields for longitude
  INTEGER(i_std),SAVE              :: jjm_g                           !! Dimension of global fields for latitude
  ! i x j 2D points (not land points) index
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: ij_para_nb           ! Number of 2D points for each mpi_rank block
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: ij_para_begin        ! First 2D point for each mpi_rank block
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: ij_para_end          ! Last 2D point for each mpi_rank block
  ! i 2D index 
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: ii_para_begin        ! First i index of 2D point for each mpi_rank block
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: ii_para_end          ! Last i index of 2D point for each mpi_rank block
  ! j 2D index
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: jj_para_nb           ! Number of complete j lines for each mpi_rank block
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: jj_para_begin        ! First j index of 2D point for each mpi_rank block
  INTEGER,SAVE,ALLOCATABLE,DIMENSION(:) :: jj_para_end          ! Last j index of 2D point for each mpi_rank block

  INTEGER,SAVE :: ii_begin
  INTEGER,SAVE :: ii_end
  INTEGER,SAVE :: jj_begin
  INTEGER,SAVE :: jj_end
  INTEGER,SAVE :: jj_nb
  INTEGER,SAVE :: ij_begin
  INTEGER,SAVE :: ij_end
  INTEGER,SAVE :: ij_nb

  INTEGER,SAVE :: MPI_COMM_ORCH
  INTEGER,SAVE :: MPI_REAL_ORCH
  INTEGER,SAVE :: MPI_INT_ORCH
  LOGICAL, SAVE :: cpl_lmdz

  INTEGER,SAVE,ALLOCATABLE :: kindex_mpi (:)
  INTEGER,SAVE             :: offset_mpi


  !
  ! 2. Variables related to the OpenMP paralelization and OpenMP horizontal domain decomposition 
  !    These variables were previously declared in mod_orchidee_omp_data
  !

  ! Check all OpenMP transferts in ORCHIDEE : use this flag to debug synchronism with OpenMP 
  LOGICAL, PARAMETER :: check_all_transfert=.FALSE.


  INTEGER(i_std),SAVE :: omp_size
  INTEGER(i_std),SAVE :: omp_rank
  LOGICAL,SAVE :: is_omp_root
  LOGICAL,SAVE :: is_ok_omp
  !$OMP  THREADPRIVATE(omp_rank,is_omp_root)

  INTEGER(i_std),SAVE,DIMENSION(:),ALLOCATABLE :: nbp_omp_para_nb
  INTEGER(i_std),SAVE,DIMENSION(:),ALLOCATABLE :: nbp_omp_para_begin
  INTEGER(i_std),SAVE,DIMENSION(:),ALLOCATABLE :: nbp_omp_para_end    

  INTEGER(i_std),SAVE :: nbp_omp
  INTEGER(i_std),SAVE :: nbp_omp_begin
  INTEGER(i_std),SAVE :: nbp_omp_end
  INTEGER(i_std),SAVE :: offset_omp
  !$OMP  THREADPRIVATE(nbp_omp,nbp_omp_begin,nbp_omp_end,offset_omp)

  ! Flag for each OMP process for ORCHIDEE to verify synchronization if function Synchro_Omp is used
  LOGICAL,SAVE,ALLOCATABLE :: proc_synchro_omp(:)

  INTEGER, SAVE :: numout_omp = -1
  !$OMP  THREADPRIVATE(numout_omp)

  ! For debugging OpenMP processes : id of OMP function for each task.
  ! If one task is not in the same function, we can see it.
  INTEGER(i_std),SAVE,ALLOCATABLE,DIMENSION(:) :: omp_function
  ! It is not SHARED.
  ! List of values :
  CHARACTER(LEN=28), PARAMETER :: omp_fct_name(-1:72) = (/ &
       "Initialization              ", &
       "Synchro_Omp                 ", &
       "check_buffer_i              ", &
       "check_buffer_r              ", &
       "check_buffer_l              ", &
       "bcast_omp_c                 ", &
       "bcast_omp_i                 ", &
       "bcast_omp_i1                ", &
       "bcast_omp_i2                ", &
       "bcast_omp_i3                ", &
       "bcast_omp_i4                ", &
       "bcast_omp_r                 ", &
       "bcast_omp_r1                ", &
       "bcast_omp_r2                ", &
       "bcast_omp_r3                ", &
       "bcast_omp_r4                ", &
       "bcast_omp_l                 ", &
       "bcast_omp_l1                ", &
       "bcast_omp_l2                ", &
       "bcast_omp_l3                ", &
       "bcast_omp_l4                ", &
       "scatter_omp_i               ", &
       "scatter_omp_i1              ", &
       "scatter_omp_i2              ", &
       "scatter_omp_i3              ", &
       "scatter_omp_r               ", &
       "scatter_omp_r1              ", &
       "scatter_omp_r2              ", &
       "scatter_omp_r3              ", &
       "scatter_omp_l               ", &
       "scatter_omp_l1              ", &
       "scatter_omp_l2              ", &
       "scatter_omp_l3              ", &
       "gather_omp_i0               ", &
       "gather_omp_i                ", &
       "gather_omp_i1               ", &
       "gather_omp_i2               ", &
       "gather_omp_i3               ", &
       "gather_omp_r0               ", &
       "gather_omp_r                ", &
       "gather_omp_r1               ", &
       "gather_omp_r2               ", &
       "gather_omp_r3               ", &
       "gather_omp_l0               ", &
       "gather_omp_l                ", &
       "gather_omp_l1               ", &
       "gather_omp_l2               ", &
       "gather_omp_l3               ", &
       "reduce_sum_omp_i            ", &
       "reduce_sum_omp_i1           ", &
       "reduce_sum_omp_i2           ", &
       "reduce_sum_omp_i3           ", &
       "reduce_sum_omp_i4           ", &
       "reduce_sum_omp_r            ", &
       "reduce_sum_omp_r1           ", &
       "reduce_sum_omp_r2           ", &
       "reduce_sum_omp_r3           ", &
       "reduce_sum_omp_r4           ", &
       "orch_bcast_omp_cgen         ", &
       "orch_bcast_omp_igen         ", &
       "orch_bcast_omp_rgen         ", &
       "orch_bcast_omp_lgen         ", &
       "orch_scatter_omp_igen       ", &
       "orch_scatter_omp_rgen       ", &
       "orch_scatter_omp_lgen       ", &
       "orch_gather_omp_simple_igen ", &
       "orch_gather_omp_igen        ", &
       "orch_gather_omp_simple_rgen ", &
       "orch_gather_omp_rgen        ", &
       "orch_gather_omp_simple_lgen ", &
       "orch_gather_omp_lgen        ", &
       "orch_reduce_sum_omp_igen    ", &
       "orch_reduce_sum_omp_rgen    ", &
       "check_buffer_c              " /)

  ! Previous value for own omp_function
  INTEGER, SAVE :: omp_previous
  !$OMP  THREADPRIVATE(omp_previous)

  ! 
  !! 3. Variables previously declared in mod_orchide_para
  !

  INTEGER,SAVE :: nbp_loc                                             !! number of local continental points
  !$OMP THREADPRIVATE(nbp_loc)
  INTEGER,SAVE :: offset
  !$OMP THREADPRIVATE(offset)

  LOGICAL,SAVE :: is_root_prc = .FALSE.                               !! Only root proc for MPI and OpenMP is true
  !$OMP THREADPRIVATE(is_root_prc)

  !! Global array used by stomate and sechiba
  !-
  !! index of land points on the 2D map
  INTEGER(i_std),ALLOCATABLE,DIMENSION(:),SAVE   :: index_g
  !-
  !! indices of the 4 neighbours of each grid point (1=N, 2=E, 3=S, 4=W)
  INTEGER(i_std),ALLOCATABLE,DIMENSION(:,:),SAVE :: neighbours_g
  !-
  !! resolution at each grid point in m (1=E-W, 2=N-S)
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:),SAVE    :: resolution_g 
  REAL(r_std),ALLOCATABLE,DIMENSION(:),SAVE    :: area_g 
  !-
  !! Geographical coordinates
  REAL(r_std),ALLOCATABLE,DIMENSION(:,:),SAVE    :: lalo_g
  ! Global grid, for all process
  REAL(r_std), ALLOCATABLE, DIMENSION(:,:), SAVE     :: lon_g, lat_g, zlev_g
  !-
  !! Fraction of continents
  REAL(r_std),ALLOCATABLE,DIMENSION(:),SAVE      :: contfrac_g  


END MODULE mod_orchidee_para_var
