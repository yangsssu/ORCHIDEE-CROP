PROGRAM test_complete

  USE xios
  USE mod_wait
  IMPLICIT NONE
  INCLUDE "mpif.h"
  INTEGER :: rank
  INTEGER :: size_loc
  INTEGER :: ierr
  
  CHARACTER(len=*),PARAMETER :: id="client"
  INTEGER :: comm
  TYPE(xios_time)      :: dtime
  TYPE(xios_context) :: ctx_hdl
  INTEGER,PARAMETER :: ni_glo=100
  INTEGER,PARAMETER :: nj_glo=100 
  INTEGER,PARAMETER :: llm=5 
  DOUBLE PRECISION  :: lval(llm)=1
  TYPE(xios_field) :: field_hdl
  TYPE(xios_fieldgroup) :: fieldgroup_hdl
  TYPE(xios_file) :: file_hdl
  TYPE(xios_variable) :: var_hdl
  LOGICAL :: ok
  CHARACTER(len=256) :: crname, str_temp
  DOUBLE PRECISION,DIMENSION(ni_glo,nj_glo) :: lon_glo,lat_glo
  DOUBLE PRECISION :: field_A_glo(ni_glo,nj_glo,llm)
  DOUBLE PRECISION,ALLOCATABLE :: lon(:,:),lat(:,:),field_A_atm(:,:,:), field_A_srf(:,:), lonvalue(:)
  INTEGER, ALLOCATABLE :: kindex(:)
  INTEGER :: ni,ibegin,iend,nj,jbegin,jend
  INTEGER :: i,j,l,ts,n, nb_pt

!!! MPI Initialization

  CALL MPI_INIT(ierr)
  
  CALL init_wait
 
!!! XIOS Initialization (get the local communicator)

  CALL xios_initialize(id,return_comm=comm)

  CALL MPI_COMM_RANK(comm,rank,ierr)
  CALL MPI_COMM_SIZE(comm,size_loc,ierr)  
  

!###########################################################################
! ATM Context
!###########################################################################

!!! Initialization of global and local coordinates for regular grid

  DO j=1,nj_glo
    DO i=1,ni_glo
      lon_glo(i,j)=(i-1)+(j-1)*ni_glo
      lat_glo(i,j)=1000+(i-1)+(j-1)*ni_glo
      DO l=1,llm
        field_A_glo(i,j,l)=(i-1)+(j-1)*ni_glo+10000*l
      ENDDO
    ENDDO
  ENDDO
  ni=ni_glo ; ibegin=1

  jbegin=1
  DO n=0,size_loc-1
    nj=nj_glo/size_loc
    IF (n<MOD(nj_glo,size_loc)) nj=nj+1
    IF (n==rank) exit 
    jbegin=jbegin+nj
  ENDDO
  
  iend=ibegin+ni-1 ; jend=jbegin+nj-1

  ALLOCATE(lon(ni,nj),lat(ni,nj),field_A_atm(0:ni+1,-1:nj+2,llm),lonvalue(ni*nj))
  lon(:,:)=lon_glo(ibegin:iend,jbegin:jend)
  lat(:,:)=lat_glo(ibegin:iend,jbegin:jend)
  field_A_atm(1:ni,1:nj,:)=field_A_glo(ibegin:iend,jbegin:jend,:)
 

!!! ATMOSPHERE context

  CALL xios_context_initialize("atmosphere",comm)
  CALL xios_get_handle("atmosphere",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)
  
  CALL xios_set_context_attr("atmosphere",calendar_type="Gregorian") 
  CALL xios_set_context_attr("atmosphere",start_date="2000-01-01 00:00:00")
  CALL xios_set_context_attr("atmosphere",time_origin="1999-01-01 15:00:00")

  CALL xios_set_axis_attr("axis_atm",size=llm ,value=lval) ;

  CALL xios_set_domain_attr("domain_atm",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj)
  CALL xios_set_domain_attr("domain_atm",data_dim=2, data_ibegin=-1, data_ni=ni+2, data_jbegin=-2, data_nj=nj+4)
  CALL xios_set_domain_attr("domain_atm",lonvalue=RESHAPE(lon,(/ni*nj/)),latvalue=RESHAPE(lat,(/ni*nj/)))

  CALL xios_set_domain_attr("domain_atm_zoom",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj)
  CALL xios_set_domain_attr("domain_atm_zoom",data_dim=2, data_ibegin=-1, data_ni=ni+2, data_jbegin=-2, data_nj=nj+4)
  CALL xios_set_domain_attr("domain_atm_zoom",lonvalue=RESHAPE(lon,(/ni*nj/)),latvalue=RESHAPE(lat,(/ni*nj/)))
  CALL xios_set_domain_attr("domain_atm_zoom",zoom_ibegin=40, zoom_ni=20, zoom_jbegin=40, zoom_nj=10)

!!! field_definition group activation

  CALL xios_set_fieldgroup_attr("field_definition",enabled=.TRUE.)

!!! Creation of new field

  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B_atm")

!!! Attribute inheritance from another field 

  CALL xios_set_attr(field_hdl,field_ref="field_A_atm",name="field_B_atm")
  
!!! Assign new field into a file (with a new name)

  CALL xios_get_handle("output_atmosphere",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl)
  CALL xios_set_attr(field_hdl,field_ref="field_B_atm",name="field_C_atm")
    
!!! Timestep definition 

  dtime%second=3600
  CALL xios_set_timestep(dtime) 
    
!!! Get longitude values and local domain sizes (to check functionality)

  ni=0 ; lonvalue(:)=0
  CALL xios_get_domain_attr("domain_atm",ni=ni,lonvalue=lonvalue)
    
  PRINT *,"ni",ni
  PRINT *,"lonvalue",lonvalue ;

!!! End of context definition

  CALL xios_close_context_definition()

!!! Test on fields/files values
  
  !!! Is an attribute defined ?

  CALL xios_is_defined_field_attr("field_A_atm",enabled=ok)
  PRINT *,"field_A_atm : attribute enabled is defined ? ",ok

  !!! Get an attibute value
  
  CALL xios_get_field_attr("field_A_atm",name=crname)
  PRINT *,"field_A_atm : attribute name is : ",TRIM(crname)

  !!! Is a field active (i.e need to give the value ) ?

    PRINT*,"field field_A_atm is active ? ",xios_field_is_active("field_A_atm")

  !!! Is a field defined ?

    PRINT*,"field field_A_atm is valid ?",xios_is_valid_field("field_A_atm")


!###########################################################################
! SRF Context
!###########################################################################

!!! Initialization of global and local coordinates for indexed grid (1 point every 2 points)

    nb_pt=ni*nj/2
    ALLOCATE(kindex(nb_pt),field_A_srf(nb_pt,llm))
    DO i=1,nb_pt
      kindex(i)=2*i-1
    ENDDO
    field_A_srf(1:nb_pt,:)=RESHAPE(field_A_glo(ibegin:iend:2,jbegin:jend,:),(/ nb_pt,llm /))

  CALL xios_context_initialize("surface",comm)
  CALL xios_get_handle("surface",ctx_hdl)
  CALL xios_set_current_context(ctx_hdl)
  
  CALL xios_set_context_attr("surface",calendar_type="Gregorian") 
  CALL xios_set_context_attr("surface",start_date="2000-01-01 00:00:00")
  CALL xios_set_context_attr("surface",time_origin="1999-01-01 15:00:00")

  CALL xios_set_axis_attr("axis_srf",size=llm ,value=lval) ;
  CALL xios_set_domain_attr("domain_srf",ni_glo=ni_glo, nj_glo=nj_glo, ibegin=ibegin, ni=ni,jbegin=jbegin,nj=nj)
  CALL xios_set_domain_attr("domain_srf",data_dim=1, data_ibegin=0, data_ni=nb_pt)
  CALL xios_set_domain_attr("domain_srf",data_n_index=nb_pt, data_i_index=kindex)
  CALL xios_set_domain_attr("domain_srf",lonvalue=RESHAPE(lon,(/ni*nj/)),latvalue=RESHAPE(lat,(/ni*nj/)))

!!! Creation of new field 

  CALL xios_get_handle("field_definition",fieldgroup_hdl)
  CALL xios_add_child(fieldgroup_hdl,field_hdl,"field_B_srf")

!!! Attribute inheritance from another field 

  CALL xios_set_attr(field_hdl,field_ref="field_A_srf",name="field_B_srf")

!!! Assign new field into a file (with a new name)

  CALL xios_get_handle("output_surface",file_hdl)
  CALL xios_add_child(file_hdl,field_hdl)
  CALL xios_set_attr(field_hdl,field_ref="field_B_srf",name="field_C_srf")

!!! Add a variable as field local attribute

  CALL xios_add_child(field_hdl,var_hdl,"my_local_attribute")
  CALL xios_set_attr(var_hdl,type="string")
  ok=xios_setVar("my_local_attribute","attribute_local")

!!! Add a variable as file global attribute

  CALL xios_add_child(file_hdl,var_hdl,"my_global_attribute")
  CALL xios_set_attr(var_hdl,type="string")
  ok=xios_setVar("my_global_attribute","attribute_global")

!!! Modify a variable used as attribute (defined in xml file)

  ok=xios_setVar("my_global_attribute_xml","6h_file")

!!! Get the value of a variable (defined in xml file)
  
  ok=xios_getVar("my_attribute1",str_temp)
  PRINT *, "my_attribute1 is :",TRIM(str_temp)
      
!!! Timestep definition

  dtime%second=1800
  CALL xios_set_timestep(dtime) 
    
!!! Get longitude values and local domain sizes (to check functionality)

  ni=0 ; lonvalue(:)=0
  CALL xios_get_domain_attr("domain_srf",ni=ni,lonvalue=lonvalue)
    
  PRINT *,"ni",ni
  PRINT *,"lonvalue",lonvalue ;

!!! End of SRF context definition

  CALL xios_close_context_definition()

!####################################################################################
!!! Loop on timesteps
!####################################################################################

    DO ts=1,24*10

      CALL xios_get_handle("atmosphere",ctx_hdl)
      CALL xios_set_current_context(ctx_hdl)    

!!! Update of calendar 

      CALL xios_update_calendar(ts)

!!! Put the value of atm field

      CALL xios_send_field("field_A_atm",field_A_atm)

!!! Change of context 

      CALL xios_get_handle("surface",ctx_hdl)
      CALL xios_set_current_context(ctx_hdl)    

!!! Update of calendar 

      CALL xios_update_calendar(ts)

!!! Put the value of srf field

      CALL xios_send_field("field_A_srf",field_A_srf)

      CALL wait_us(5000) ;
    ENDDO

!####################################################################################
!!! Finalization
!####################################################################################

!!! End of contextes

    CALL xios_context_finalize()
    CALL xios_get_handle("atmosphere",ctx_hdl)
    CALL xios_set_current_context(ctx_hdl)    
    CALL xios_context_finalize()
    
!!! End of XIOS

    CALL xios_finalize()
  
    CALL MPI_FINALIZE(ierr)
  
  END PROGRAM test_complete



  

  
