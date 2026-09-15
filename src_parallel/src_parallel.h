! Redefinition of MPI function if not second underscore in MPI library.
! One must use -DMPI_SECOND__ in precompilation to activate those definitions.

!-
!- $Header: /home/ssipsl/CVSREP/ORCHIDEE/src_parallel/src_parallel.h,v 1.3 2009/06/24 10:15:00 ssipsl Exp $
!-

#ifdef MPI_SECOND__

#define MPI_BARRIER MPI_BARRIER_
#define MPI_FINALIZE MPI_FINALIZE_
#define MPI_COMM_SIZE MPI_COMM_SIZE_
#define MPI_INIT MPI_INIT_
#define MPI_COMM_RANK MPI_COMM_RANK_
#define MPI_GATHER MPI_GATHER_
#define MPI_GATHERV MPI_GATHERV_
#define MPI_BCAST MPI_BCAST_
#define MPI_SCATTERV MPI_SCATTERV_
#define MPI_REDUCE MPI_REDUCE_

#endif

#ifdef NF_SECOND__


#define nf_get_varm_double nf_get_varm_double_
#define nf_get_vara_text nf_get_vara_text_
#define nf_get_vara_real nf_get_vara_real_
#define nf_get_varm_int1 nf_get_varm_int1_
#define nf_get_varm_int2 nf_get_varm_int2_
#define nf_get_varm_text nf_get_varm_text_
#define nf_get_varm_real nf_get_varm_real_
#define nf_get_vars_int1 nf_get_vars_int1_
#define nf_get_vars_int nf_get_vars_int_
#define nf_get_vars_double nf_get_vars_double_
#define nf_get_vars_text nf_get_vars_text_
#define nf_inq_att nf_inq_att_
#define nf_inq_attid nf_inq_attid_
#define nf_inq_atttype nf_inq_atttype_
#define nf_inq_attname nf_inq_attname_
#define nf_inq_attlen nf_inq_attlen_
#define nf_inq nf_inq_
#define nf_inq_dim nf_inq_dim_
#define nf_inq_dimlen nf_inq_dimlen_
#define nf_inq_dimid nf_inq_dimid_
#define nf_inq_libvers nf_inq_libvers_
#define nf_inq_format nf_inq_format_
#define nf_inq_natts nf_inq_natts_
#define nf_inq_dimname nf_inq_dimname_
#define nf_inq_base_pe nf_inq_base_pe_
#define nf_get_vars_real nf_get_vars_real_
#define nf_inq_nvars nf_inq_nvars_
#define nf_inq_ndims nf_inq_ndims_
#define nf_get_vars_int2 nf_get_vars_int2_
#define nf_inq_var nf_inq_var_
#define nf_inq_vardimid nf_inq_vardimid_
#define nf_inq_varid nf_inq_varid_
#define nf_inq_varnatts nf_inq_varnatts_
#define nf_inq_varndims nf_inq_varndims_
#define nf_inq_vartype nf_inq_vartype_
#define nf_inq_varname nf_inq_varname_
#define nf_issyserr nf_issyserr_
#define nf_put_att_double nf_put_att_double_
#define nf_open nf_open_
#define nf_put_att_int1 nf_put_att_int1_
#define nf_put_att_int nf_put_att_int_
#define nf_put_att_text nf_put_att_text_
#define nf_put_att_real nf_put_att_real_
#define nf_put_att_int2 nf_put_att_int2_
#define nf_inq_unlimdim nf_inq_unlimdim_
#define nf_get_varm_int nf_get_varm_int_
#define nf_put_var1_int nf_put_var1_int_
#define nf_put_var1_int2 nf_put_var1_int2_
#define nf_put_var1_int1 nf_put_var1_int1_
#define nf_put_var1_text nf_put_var1_text_
#define nf_put_var_int nf_put_var_int_
#define nf_put_var_double nf_put_var_double_
#define nf_put_var1_real nf_put_var1_real_
#define nf_put_var_real nf_put_var_real_
#define nf_put_vara_int2 nf_put_vara_int2_
#define nf_put_vara_int1 nf_put_vara_int1_
#define nf_put_vara_int nf_put_vara_int_
#define nf_put_vara_double nf_put_vara_double_
#define nf_put_vara_text nf_put_vara_text_
#define nf_put_varm_int1 nf_put_varm_int1_
#define nf_put_varm_int2 nf_put_varm_int2_
#define nf_put_varm_int nf_put_varm_int_
#define nf_put_varm_double nf_put_varm_double_
#define nf_put_varm_real nf_put_varm_real_
#define nf_put_vara_real nf_put_vara_real_
#define nf_put_var_text nf_put_var_text_
#define nf_put_var_int2 nf_put_var_int2_
#define nf_put_var_int1 nf_put_var_int1_
#define nf_put_vars_double nf_put_vars_double_
#define nf_put_varm_text nf_put_varm_text_
#define nf_put_var1_double nf_put_var1_double_
#define nf_put_vars_int1 nf_put_vars_int1_
#define nf_put_vars_int2 nf_put_vars_int2_
#define nf_put_vars_text nf_put_vars_text_
#define nf_put_vars_real nf_put_vars_real_
#define nf_rename_att nf_rename_att_
#define nf_set_base_pe nf_set_base_pe_
#define nf_rename_var nf_rename_var_
#define nf_rename_dim nf_rename_dim_
#define nf_redef nf_redef_
#define nf_sync nf_sync_
#define nf_strerror nf_strerror_
#define nf_set_fill nf_set_fill_
#define nf_set_default_format nf_set_default_format_
#define nf_put_vars_int nf_put_vars_int_
#define nf_get_var_int2 nf_get_var_int2_
#define nf__create_mp nf__create_mp_
#define nf__open nf__open_
#define nf__open_mp nf__open_mp_
#define nf__enddef nf__enddef_
#define nf_abort nf_abort_
#define nf__create nf__create_
#define nf_copy_att nf_copy_att_
#define nf_create nf_create_
#define nf_def_dim nf_def_dim_
#define nf_copy_var nf_copy_var_
#define nf_close nf_close_
#define nf_del_att nf_del_att_
#define nf_delete nf_delete_
#define nf_def_var nf_def_var_
#define nf_enddef nf_enddef_
#define nf_get_att_real nf_get_att_real_
#define nf_get_att_double nf_get_att_double_
#define nf_get_att_int nf_get_att_int_
#define nf_get_att_int2 nf_get_att_int2_
#define nf_get_att_int1 nf_get_att_int1_
#define nf_get_att_text nf_get_att_text_
#define nf_get_var1_int nf_get_var1_int_
#define nf_get_var1_int1 nf_get_var1_int1_
#define nf_get_var1_text nf_get_var1_text_
#define nf_get_var1_real nf_get_var1_real_
#define nf_get_var1_int2 nf_get_var1_int2_
#define nf_get_var1_double nf_get_var1_double_
#define nf_get_var_int nf_get_var_int_
#define nf_get_var_int2 nf_get_var_int2_
#define nf_get_var_real nf_get_var_real_
#define nf_get_var_int1 nf_get_var_int1_
#define nf_get_var_text nf_get_var_text_
#define nf_get_vara_int1 nf_get_vara_int1_
#define nf_get_vara_int nf_get_vara_int_
#define nf_get_vara_int2 nf_get_vara_int2_
#define nf_get_vara_double nf_get_vara_double_
#define nf_get_var_double nf_get_var_double_
#define nf_get_varm_int nf_get_varm_int_
#define nf_get_varm_double nf_get_varm_double_
#define nf_get_varm_int2 nf_get_varm_int2_
#define nf_get_vars_double nf_get_vars_double_
#define nf_get_varm_text nf_get_varm_text_
#define nf_get_varm_real nf_get_varm_real_
#define nf_get_varm_int1 nf_get_varm_int1_
#define nf_get_vara_text nf_get_vara_text_
#define nf_get_vars_text nf_get_vars_text_
#define nf_get_vars_real nf_get_vars_real_
#define nf_get_vars_int2 nf_get_vars_int2_
#define nf_get_vars_int1 nf_get_vars_int1_
#define nf_inq_att nf_inq_att_
#define nf_inq_attid nf_inq_attid_
#define nf_inq nf_inq_
#define nf_inq_atttype nf_inq_atttype_
#define nf_inq_attname nf_inq_attname_
#define nf_inq_dimlen nf_inq_dimlen_
#define nf_inq_dimid nf_inq_dimid_
#define nf_inq_libvers nf_inq_libvers_
#define nf_inq_format nf_inq_format_
#define nf_inq_dimname nf_inq_dimname_
#define nf_inq_dim nf_inq_dim_
#define nf_inq_base_pe nf_inq_base_pe_
#define nf_inq_ndims nf_inq_ndims_
#define nf_inq_unlimdim nf_inq_unlimdim_
#define nf_inq_nvars nf_inq_nvars_
#define nf_inq_var nf_inq_var_
#define nf_inq_natts nf_inq_natts_
#define nf_inq_varid nf_inq_varid_
#define nf_inq_varname nf_inq_varname_
#define nf_inq_varnatts nf_inq_varnatts_
#define nf_inq_vardimid nf_inq_vardimid_
#define nf_inq_attlen nf_inq_attlen_
#define nf_get_vars_int nf_get_vars_int_
#define nf_get_vara_real nf_get_vara_real_
#define nf_inq_vartype nf_inq_vartype_
#define nf_issyserr nf_issyserr_
#define nf_open nf_open_
#define nf_put_att_int1 nf_put_att_int1_
#define nf_put_att_int2 nf_put_att_int2_
#define nf_put_att_text nf_put_att_text_
#define nf_put_var1_int nf_put_var1_int_
#define nf_put_var1_int1 nf_put_var1_int1_
#define nf_put_var1_double nf_put_var1_double_
#define nf_put_var1_int2 nf_put_var1_int2_
#define nf_put_var1_real nf_put_var1_real_
#define nf_put_att_real nf_put_att_real_
#define nf_put_var1_text nf_put_var1_text_
#define nf_put_att_int nf_put_att_int_
#define nf_put_att_double nf_put_att_double_
#define nf_put_var_double nf_put_var_double_
#define nf_put_var_int2 nf_put_var_int2_
#define nf_put_var_int1 nf_put_var_int1_
#define nf_put_var_real nf_put_var_real_
#define nf_put_var_int nf_put_var_int_
#define nf_inq_varndims nf_inq_varndims_
#define nf_put_vara_int1 nf_put_vara_int1_
#define nf_put_vara_text nf_put_vara_text_
#define nf_put_vara_real nf_put_vara_real_
#define nf_put_varm_int nf_put_varm_int_
#define nf_put_varm_double nf_put_varm_double_
#define nf_put_vara_int2 nf_put_vara_int2_
#define nf_put_varm_int1 nf_put_varm_int1_
#define nf_put_varm_real nf_put_varm_real_
#define nf_put_varm_text nf_put_varm_text_
#define nf_put_vars_int1 nf_put_vars_int1_
#define nf_put_vars_int nf_put_vars_int_
#define nf_put_vars_real nf_put_vars_real_
#define nf_put_vars_int2 nf_put_vars_int2_
#define nf_put_vars_text nf_put_vars_text_
#define nf_put_vars_double nf_put_vars_double_
#define nf_put_varm_int2 nf_put_varm_int2_
#define nf_rename_dim nf_rename_dim_
#define nf_rename_att nf_rename_att_
#define nf_redef nf_redef_
#define nf_set_base_pe nf_set_base_pe_
#define nf_set_default_format nf_set_default_format_
#define nf_rename_var nf_rename_var_
#define nf_put_vara_int nf_put_vara_int_
#define nf_strerror nf_strerror_
#define nf_set_fill nf_set_fill_
#define nf_put_var_text nf_put_var_text_
        
#define NF_GET_VARM_DOUBLE NF_GET_VARM_DOUBLE_
#define NF_GET_VARA_TEXT NF_GET_VARA_TEXT_
#define NF_GET_VARA_REAL NF_GET_VARA_REAL_
#define NF_GET_VARM_INT1 NF_GET_VARM_INT1_
#define NF_GET_VARM_INT2 NF_GET_VARM_INT2_
#define NF_GET_VARM_TEXT NF_GET_VARM_TEXT_
#define NF_GET_VARM_REAL NF_GET_VARM_REAL_
#define NF_GET_VARS_INT1 NF_GET_VARS_INT1_
#define NF_GET_VARS_INT NF_GET_VARS_INT_
#define NF_GET_VARS_DOUBLE NF_GET_VARS_DOUBLE_
#define NF_GET_VARS_TEXT NF_GET_VARS_TEXT_
#define NF_INQ_ATT NF_INQ_ATT_
#define NF_INQ_ATTID NF_INQ_ATTID_
#define NF_INQ_ATTTYPE NF_INQ_ATTTYPE_
#define NF_INQ_ATTNAME NF_INQ_ATTNAME_
#define NF_INQ_ATTLEN NF_INQ_ATTLEN_
#define NF_INQ NF_INQ_
#define NF_INQ_DIM NF_INQ_DIM_
#define NF_INQ_DIMLEN NF_INQ_DIMLEN_
#define NF_INQ_DIMID NF_INQ_DIMID_
#define NF_INQ_LIBVERS NF_INQ_LIBVERS_
#define NF_INQ_FORMAT NF_INQ_FORMAT_
#define NF_INQ_NATTS NF_INQ_NATTS_
#define NF_INQ_DIMNAME NF_INQ_DIMNAME_
#define NF_INQ_BASE_PE NF_INQ_BASE_PE_
#define NF_GET_VARS_REAL NF_GET_VARS_REAL_
#define NF_INQ_NVARS NF_INQ_NVARS_
#define NF_INQ_NDIMS NF_INQ_NDIMS_
#define NF_GET_VARS_INT2 NF_GET_VARS_INT2_
#define NF_INQ_VAR NF_INQ_VAR_
#define NF_INQ_VARDIMID NF_INQ_VARDIMID_
#define NF_INQ_VARID NF_INQ_VARID_
#define NF_INQ_VARNATTS NF_INQ_VARNATTS_
#define NF_INQ_VARNDIMS NF_INQ_VARNDIMS_
#define NF_INQ_VARTYPE NF_INQ_VARTYPE_
#define NF_INQ_VARNAME NF_INQ_VARNAME_
#define NF_ISSYSERR NF_ISSYSERR_
#define NF_PUT_ATT_DOUBLE NF_PUT_ATT_DOUBLE_
#define NF_OPEN NF_OPEN_
#define NF_PUT_ATT_INT1 NF_PUT_ATT_INT1_
#define NF_PUT_ATT_INT NF_PUT_ATT_INT_
#define NF_PUT_ATT_TEXT NF_PUT_ATT_TEXT_
#define NF_PUT_ATT_REAL NF_PUT_ATT_REAL_
#define NF_PUT_ATT_INT2 NF_PUT_ATT_INT2_
#define NF_INQ_UNLIMDIM NF_INQ_UNLIMDIM_
#define NF_GET_VARM_INT NF_GET_VARM_INT_
#define NF_PUT_VAR1_INT NF_PUT_VAR1_INT_
#define NF_PUT_VAR1_INT2 NF_PUT_VAR1_INT2_
#define NF_PUT_VAR1_INT1 NF_PUT_VAR1_INT1_
#define NF_PUT_VAR1_TEXT NF_PUT_VAR1_TEXT_
#define NF_PUT_VAR_INT NF_PUT_VAR_INT_
#define NF_PUT_VAR_DOUBLE NF_PUT_VAR_DOUBLE_
#define NF_PUT_VAR1_REAL NF_PUT_VAR1_REAL_
#define NF_PUT_VAR_REAL NF_PUT_VAR_REAL_
#define NF_PUT_VARA_INT2 NF_PUT_VARA_INT2_
#define NF_PUT_VARA_INT1 NF_PUT_VARA_INT1_
#define NF_PUT_VARA_INT NF_PUT_VARA_INT_
#define NF_PUT_VARA_DOUBLE NF_PUT_VARA_DOUBLE_
#define NF_PUT_VARA_TEXT NF_PUT_VARA_TEXT_
#define NF_PUT_VARM_INT1 NF_PUT_VARM_INT1_
#define NF_PUT_VARM_INT2 NF_PUT_VARM_INT2_
#define NF_PUT_VARM_INT NF_PUT_VARM_INT_
#define NF_PUT_VARM_DOUBLE NF_PUT_VARM_DOUBLE_
#define NF_PUT_VARM_REAL NF_PUT_VARM_REAL_
#define NF_PUT_VARA_REAL NF_PUT_VARA_REAL_
#define NF_PUT_VAR_TEXT NF_PUT_VAR_TEXT_
#define NF_PUT_VAR_INT2 NF_PUT_VAR_INT2_
#define NF_PUT_VAR_INT1 NF_PUT_VAR_INT1_
#define NF_PUT_VARS_DOUBLE NF_PUT_VARS_DOUBLE_
#define NF_PUT_VARM_TEXT NF_PUT_VARM_TEXT_
#define NF_PUT_VAR1_DOUBLE NF_PUT_VAR1_DOUBLE_
#define NF_PUT_VARS_INT1 NF_PUT_VARS_INT1_
#define NF_PUT_VARS_INT2 NF_PUT_VARS_INT2_
#define NF_PUT_VARS_TEXT NF_PUT_VARS_TEXT_
#define NF_PUT_VARS_REAL NF_PUT_VARS_REAL_
#define NF_RENAME_ATT NF_RENAME_ATT_
#define NF_SET_BASE_PE NF_SET_BASE_PE_
#define NF_RENAME_VAR NF_RENAME_VAR_
#define NF_RENAME_DIM NF_RENAME_DIM_
#define NF_REDEF NF_REDEF_
#define NF_SYNC NF_SYNC_
#define NF_STRERROR NF_STRERROR_
#define NF_SET_FILL NF_SET_FILL_
#define NF_SET_DEFAULT_FORMAT NF_SET_DEFAULT_FORMAT_
#define NF_PUT_VARS_INT NF_PUT_VARS_INT_
#define NF_GET_VAR_INT2 NF_GET_VAR_INT2_
#define NF__CREATE_MP NF__CREATE_MP_
#define NF__OPEN NF__OPEN_
#define NF__OPEN_MP NF__OPEN_MP_
#define NF__ENDDEF NF__ENDDEF_
#define NF_ABORT NF_ABORT_
#define NF__CREATE NF__CREATE_
#define NF_COPY_ATT NF_COPY_ATT_
#define NF_CREATE NF_CREATE_
#define NF_DEF_DIM NF_DEF_DIM_
#define NF_COPY_VAR NF_COPY_VAR_
#define NF_CLOSE NF_CLOSE_
#define NF_DEL_ATT NF_DEL_ATT_
#define NF_DELETE NF_DELETE_
#define NF_DEF_VAR NF_DEF_VAR_
#define NF_ENDDEF NF_ENDDEF_
#define NF_GET_ATT_REAL NF_GET_ATT_REAL_
#define NF_GET_ATT_DOUBLE NF_GET_ATT_DOUBLE_
#define NF_GET_ATT_INT NF_GET_ATT_INT_
#define NF_GET_ATT_INT2 NF_GET_ATT_INT2_
#define NF_GET_ATT_INT1 NF_GET_ATT_INT1_
#define NF_GET_ATT_TEXT NF_GET_ATT_TEXT_
#define NF_GET_VAR1_INT NF_GET_VAR1_INT_
#define NF_GET_VAR1_INT1 NF_GET_VAR1_INT1_
#define NF_GET_VAR1_TEXT NF_GET_VAR1_TEXT_
#define NF_GET_VAR1_REAL NF_GET_VAR1_REAL_
#define NF_GET_VAR1_INT2 NF_GET_VAR1_INT2_
#define NF_GET_VAR1_DOUBLE NF_GET_VAR1_DOUBLE_
#define NF_GET_VAR_INT NF_GET_VAR_INT_
#define NF_GET_VAR_INT2 NF_GET_VAR_INT2_
#define NF_GET_VAR_REAL NF_GET_VAR_REAL_
#define NF_GET_VAR_INT1 NF_GET_VAR_INT1_
#define NF_GET_VAR_TEXT NF_GET_VAR_TEXT_
#define NF_GET_VARA_INT1 NF_GET_VARA_INT1_
#define NF_GET_VARA_INT NF_GET_VARA_INT_
#define NF_GET_VARA_INT2 NF_GET_VARA_INT2_
#define NF_GET_VARA_DOUBLE NF_GET_VARA_DOUBLE_
#define NF_GET_VAR_DOUBLE NF_GET_VAR_DOUBLE_
#define NF_GET_VARM_INT NF_GET_VARM_INT_
#define NF_GET_VARM_DOUBLE NF_GET_VARM_DOUBLE_
#define NF_GET_VARM_INT2 NF_GET_VARM_INT2_
#define NF_GET_VARS_DOUBLE NF_GET_VARS_DOUBLE_
#define NF_GET_VARM_TEXT NF_GET_VARM_TEXT_
#define NF_GET_VARM_REAL NF_GET_VARM_REAL_
#define NF_GET_VARM_INT1 NF_GET_VARM_INT1_
#define NF_GET_VARA_TEXT NF_GET_VARA_TEXT_
#define NF_GET_VARS_TEXT NF_GET_VARS_TEXT_
#define NF_GET_VARS_REAL NF_GET_VARS_REAL_
#define NF_GET_VARS_INT2 NF_GET_VARS_INT2_
#define NF_GET_VARS_INT1 NF_GET_VARS_INT1_
#define NF_INQ_ATT NF_INQ_ATT_
#define NF_INQ_ATTID NF_INQ_ATTID_
#define NF_INQ NF_INQ_
#define NF_INQ_ATTTYPE NF_INQ_ATTTYPE_
#define NF_INQ_ATTNAME NF_INQ_ATTNAME_
#define NF_INQ_DIMLEN NF_INQ_DIMLEN_
#define NF_INQ_DIMID NF_INQ_DIMID_
#define NF_INQ_LIBVERS NF_INQ_LIBVERS_
#define NF_INQ_FORMAT NF_INQ_FORMAT_
#define NF_INQ_DIMNAME NF_INQ_DIMNAME_
#define NF_INQ_DIM NF_INQ_DIM_
#define NF_INQ_BASE_PE NF_INQ_BASE_PE_
#define NF_INQ_NDIMS NF_INQ_NDIMS_
#define NF_INQ_UNLIMDIM NF_INQ_UNLIMDIM_
#define NF_INQ_NVARS NF_INQ_NVARS_
#define NF_INQ_VAR NF_INQ_VAR_
#define NF_INQ_NATTS NF_INQ_NATTS_
#define NF_INQ_VARID NF_INQ_VARID_
#define NF_INQ_VARNAME NF_INQ_VARNAME_
#define NF_INQ_VARNATTS NF_INQ_VARNATTS_
#define NF_INQ_VARDIMID NF_INQ_VARDIMID_
#define NF_INQ_ATTLEN NF_INQ_ATTLEN_
#define NF_GET_VARS_INT NF_GET_VARS_INT_
#define NF_GET_VARA_REAL NF_GET_VARA_REAL_
#define NF_INQ_VARTYPE NF_INQ_VARTYPE_
#define NF_ISSYSERR NF_ISSYSERR_
#define NF_OPEN NF_OPEN_
#define NF_PUT_ATT_INT1 NF_PUT_ATT_INT1_
#define NF_PUT_ATT_INT2 NF_PUT_ATT_INT2_
#define NF_PUT_ATT_TEXT NF_PUT_ATT_TEXT_
#define NF_PUT_VAR1_INT NF_PUT_VAR1_INT_
#define NF_PUT_VAR1_INT1 NF_PUT_VAR1_INT1_
#define NF_PUT_VAR1_DOUBLE NF_PUT_VAR1_DOUBLE_
#define NF_PUT_VAR1_INT2 NF_PUT_VAR1_INT2_
#define NF_PUT_VAR1_REAL NF_PUT_VAR1_REAL_
#define NF_PUT_ATT_REAL NF_PUT_ATT_REAL_
#define NF_PUT_VAR1_TEXT NF_PUT_VAR1_TEXT_
#define NF_PUT_ATT_INT NF_PUT_ATT_INT_
#define NF_PUT_ATT_DOUBLE NF_PUT_ATT_DOUBLE_
#define NF_PUT_VAR_DOUBLE NF_PUT_VAR_DOUBLE_
#define NF_PUT_VAR_INT2 NF_PUT_VAR_INT2_
#define NF_PUT_VAR_INT1 NF_PUT_VAR_INT1_
#define NF_PUT_VAR_REAL NF_PUT_VAR_REAL_
#define NF_PUT_VAR_INT NF_PUT_VAR_INT_
#define NF_INQ_VARNDIMS NF_INQ_VARNDIMS_
#define NF_PUT_VARA_INT1 NF_PUT_VARA_INT1_
#define NF_PUT_VARA_TEXT NF_PUT_VARA_TEXT_
#define NF_PUT_VARA_REAL NF_PUT_VARA_REAL_
#define NF_PUT_VARM_INT NF_PUT_VARM_INT_
#define NF_PUT_VARM_DOUBLE NF_PUT_VARM_DOUBLE_
#define NF_PUT_VARA_INT2 NF_PUT_VARA_INT2_
#define NF_PUT_VARM_INT1 NF_PUT_VARM_INT1_
#define NF_PUT_VARM_REAL NF_PUT_VARM_REAL_
#define NF_PUT_VARM_TEXT NF_PUT_VARM_TEXT_
#define NF_PUT_VARS_INT1 NF_PUT_VARS_INT1_
#define NF_PUT_VARS_INT NF_PUT_VARS_INT_
#define NF_PUT_VARS_REAL NF_PUT_VARS_REAL_
#define NF_PUT_VARS_INT2 NF_PUT_VARS_INT2_
#define NF_PUT_VARS_TEXT NF_PUT_VARS_TEXT_
#define NF_PUT_VARS_DOUBLE NF_PUT_VARS_DOUBLE_
#define NF_PUT_VARM_INT2 NF_PUT_VARM_INT2_
#define NF_RENAME_DIM NF_RENAME_DIM_
#define NF_RENAME_ATT NF_RENAME_ATT_
#define NF_REDEF NF_REDEF_
#define NF_SET_BASE_PE NF_SET_BASE_PE_
#define NF_SET_DEFAULT_FORMAT NF_SET_DEFAULT_FORMAT_
#define NF_RENAME_VAR NF_RENAME_VAR_
#define NF_PUT_VARA_INT NF_PUT_VARA_INT_
#define NF_STRERROR NF_STRERROR_
#define NF_SET_FILL NF_SET_FILL_
#define NF_PUT_VAR_TEXT NF_PUT_VAR_TEXT_

#endif














































































































































































































