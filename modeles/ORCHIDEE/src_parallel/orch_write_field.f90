! Yann Meurdesoif functions for sequentiel tests.

!-
!< $HeadURL: svn://forge.ipsl.jussieu.fr/orchidee/perso/xuhui.wang/MICTcrop/ORCHIDEE/src_parallel/orch_write_field.f90 $ 
!< $Date: 2013-10-14 15:38:24 +0200 (Mon, 14 Oct 2013) $
!< $Author: josefine.ghattas $
!< $Revision: 1536 $
!-

module orch_Write_Field
  
  USE mod_orchidee_para

  IMPLICIT NONE

  integer, parameter :: MaxWriteField = 100
  integer, dimension(MaxWriteField),save :: FieldId
  integer, dimension(MaxWriteField),save :: FieldVarId
  integer, dimension(MaxWriteField),save :: FieldIndex
  character(len=255), dimension(MaxWriteField) ::  FieldName
  
  integer, save,dimension(:), allocatable :: Index_Write_Field
  integer,save :: iim
  integer,save :: jjm
  integer,save :: NbPoint
  real, parameter :: undef_var=0.
  
  integer,save :: NbField = 0
  
  interface WriteField
    module procedure WriteField_4d,WriteField_3d,WriteField_2d,WriteField_1d
  end interface WriteField
 
  interface WriteFieldI
    module procedure WriteFieldI_3d,WriteFieldI_2d,WriteFieldI_1d
  end interface WriteFieldI

  private :: iim,jjm,NbPoint 
  contains
  
    subroutine Init_WriteField(iim0,jjm0,NbPoint0,Index0)
    implicit none
      integer,intent(in) :: iim0
      integer,intent(in) :: jjm0
      integer,intent(in) :: NbPoint0
      integer,intent(in) :: Index0(NbPoint0)
    
      iim=iim0
      jjm=jjm0
      Nbpoint=Nbpoint0
      ALLOCATE(Index_Write_Field(NbPoint))
      Index_Write_Field(:)=Index0(:)
    end subroutine Init_WriteField
    
    function GetFieldIndex(name)
    implicit none
      integer          :: GetFieldindex
      character(len=*) :: name
    
      character(len=255) :: TrueName
      integer            :: i
       
      
      TrueName=TRIM(ADJUSTL(name))
    
      GetFieldIndex=-1
      do i=1,NbField
        if (TrueName==FieldName(i)) then
          GetFieldIndex=i
          exit
        endif
      enddo
    end function GetFieldIndex

    subroutine WriteFieldI_3d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:,:,:) :: Field 
      integer, dimension(3) :: Dim
      integer,dimension(4) :: Dim_tmp
      integer :: i
      
      real, allocatable, dimension(:,:,:) :: Field_tmp 
      
      Dim=shape(Field)
      allocate(Field_tmp(iim*jjm,Dim(2),dim(3)))
      field_tmp(:,:,:)=undef_var
      
      do i=1,NbPoint
        field_tmp(Index_Write_Field(i),:,:)=Field(i,:,:)
      enddo
      
      Dim_tmp(1)=iim
      Dim_tmp(2)=jjm
      Dim_tmp(3)=dim(2)
      Dim_tmp(4)=dim(3)
      call WriteField_gen(name,Field_tmp,4,Dim_tmp)  
  
      deallocate(Field_tmp)
    end subroutine WriteFieldI_3d

    subroutine WriteFieldI_2d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:,:) :: Field 
      integer, dimension(2) :: Dim
      integer,dimension(3) :: Dim_tmp
      integer :: i
      
      real, allocatable, dimension(:,:) :: Field_tmp 
      
      Dim=shape(Field)
      allocate(Field_tmp(iim*jjm,Dim(2)))
      field_tmp(:,:)=undef_var
      
      do i=1,NbPoint
        field_tmp(Index_Write_Field(i),:)=Field(i,:)
      enddo
      
      Dim_tmp(1)=iim
      Dim_tmp(2)=jjm
      Dim_tmp(3)=dim(2)

      call WriteField_gen(name,Field_tmp,3,Dim_tmp)  
  
      deallocate(Field_tmp)
    end subroutine WriteFieldI_2d

    subroutine WriteFieldI_1d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:) :: Field 
      integer, dimension(1) :: Dim
      integer,dimension(2) :: Dim_tmp
      integer :: i
      
      real, allocatable, dimension(:) :: Field_tmp 
      
      Dim=shape(Field)
      allocate(Field_tmp(iim*jjm))
      field_tmp(:)=undef_var
      
      do i=1,NbPoint
        field_tmp(Index_Write_Field(i))=Field(i)
      enddo
      
      Dim_tmp(1)=iim
      Dim_tmp(2)=jjm

      call WriteField_gen(name,Field_tmp,2,Dim_tmp)  
  
      deallocate(Field_tmp)
    end subroutine WriteFieldI_1d
        
    subroutine WriteField_4d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:,:,:,:) :: Field 
      integer, dimension(4) :: Dim
      
      Dim=shape(Field)
      call WriteField_gen(name,Field,4,Dim)  
  
    end subroutine WriteField_4d
     
    subroutine WriteField_3d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:,:,:) :: Field 
      integer, dimension(3) :: Dim
      
      Dim=shape(Field)
      call WriteField_gen(name,Field,3,Dim)  
  
    end subroutine WriteField_3d
    
    subroutine WriteField_2d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:,:) :: Field 
      integer, dimension(2) :: Dim
      
      Dim=shape(Field)
      call WriteField_gen(name,Field,2,Dim)  
  
    end subroutine WriteField_2d
    
    subroutine WriteField_1d(name,Field)
    implicit none
      character(len=*) :: name
      real, dimension(:) :: Field 
      integer, dimension(1) :: Dim
      
      Dim=shape(Field)
      call WriteField_gen(name,Field,1,Dim)  
  
    end subroutine WriteField_1d
        
    subroutine CreateNewField(name,NbDim,DimSize)
    USE ioipsl
    implicit none
    include 'netcdf.inc'  
      character(len=*) :: name
      integer :: NbDim
      integer :: DimSize(NbDim)
      integer :: TabDim(NbDim+1)
      integer :: status
      
      
      NbField=NbField+1
      FieldName(NbField)=TRIM(ADJUSTL(name))
      FieldIndex(NbField)=1
      
      WRITE(numout,*) 'CREATE_NEW_FIELD ',name,NbDim,DimSize
      CALL flush(6)
      status = NF_CREATE(TRIM(ADJUSTL(name))//'.nc', NF_CLOBBER, FieldId(NbField))
      if (NbDim>=1) status = NF_DEF_DIM(FieldId(NbField),'I',DimSize(1),TabDim(1))
      if (NbDim>=2) status = NF_DEF_DIM(FieldId(NbField),'J',DimSize(2),TabDim(2))
      if (NbDim>=3) status = NF_DEF_DIM(FieldId(NbField),'K',DimSize(3),TabDim(3))
      if (NbDim>=4) status = NF_DEF_DIM(FieldId(NbField),'L',DimSize(4),TabDim(4))
      status = NF_DEF_DIM(FieldId(NbField),'iter',NF_UNLIMITED,TabDim(NbDim+1))
      status = NF_DEF_VAR(FieldId(NbField),FieldName(NbField),NF_DOUBLE,NbDim+1,TabDim,FieldVarId(NbField))
      status = NF_ENDDEF(FieldId(NbField))

    end subroutine CreateNewField
    
  function int2str(int)
    implicit none
    integer, parameter :: MaxLen=10
    integer,intent(in) :: int
    character(len=MaxLen) :: int2str
    logical :: flag
    integer :: i
    flag=.true.
    
    i=int
    
    int2str=''
    do while (flag)
      int2str=CHAR(MOD(i,10)+48)//int2str
      i=i/10
      if (i==0) flag=.false.
    enddo
  end function int2str


end module Orch_Write_Field

    subroutine WriteField_gen(name,Field,NbDim,DimSize)
    use orch_write_field
    implicit none
    include 'netcdf.inc'
      character(len=*) :: name
      integer :: NbDim
      integer,dimension(NbDim) :: DimSize
      real,dimension(*) :: Field
      
      integer :: status
      integer :: ind
      integer :: start(NbDim+1)
      integer :: count(NbDim+1)
      integer :: i
           
      Ind=GetFieldIndex(name)
      if (Ind==-1) then
        call CreateNewField(name,NbDim,DimSize)
	Ind=GetFieldIndex(name)
      else
        FieldIndex(Ind)=FieldIndex(Ind)+1
      endif
      
      do i=1,NbDim
        start(i)=1
        count(i)=DimSize(i)
      enddo
      start(NbDim+1)=FieldIndex(Ind)
      count(NbDim+1)=1

      status = NF_PUT_VARA_DOUBLE(FieldId(Ind),FieldVarId(Ind),start,count,Field)
      status = NF_SYNC(FieldId(Ind))
      
    end subroutine WriteField_gen
