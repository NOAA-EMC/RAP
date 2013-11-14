subroutine nc2bin(dh1,DateStr1, &
           VarName,ifswap,iunit,nrecs,&
           varname_all,memoryorder_all,domainend_all,file_offset,      &
           if_integer)

  use mpi
  use kinds, only: r_single,i_llong,i_kind,r_kind

  IMPLICIT NONE
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  logical,intent(in) :: ifswap
  logical,intent(in)  :: if_integer
! for background file
  integer(i_kind),intent(in) :: iunit
  integer(i_kind),intent(in) :: nrecs
  integer(i_llong),intent(in):: file_offset(nrecs)
  character(132), intent(in) :: varname_all(nrecs),memoryorder_all(nrecs)
  integer(i_kind),intent(in) :: domainend_all(3,nrecs)
  integer :: index
!
! for background file
  integer(i_kind), intent(in)      :: dh1
  character(len=19),intent(in)  :: DateStr1
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
!
  CHARACTER (LEN=19),intent(in)  :: VarName
!
  integer(i_kind)  :: nlon_regional,nlat_regional,nsig_regional

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,ierr
  integer, ALLOCATABLE:: ibuf4(:)
  real(r_single),allocatable:: field3(:,:,:)
  integer,allocatable:: ifield3(:,:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!
  wrf_real=104_i_kind
!-------------  get grid info

  end_index=0
  call ext_ncd_get_var_info (dh1,trim(VarName),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1
  write(6,*)' rmse_var=',trim(VarName)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE

  if(ndim1 == 2) end_index(3)=1
  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional

!   cycle one field
!
  write(6,*) '=========================================='

  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  if( (domainend_all(1,index)/=end_index(1)) .or. &
      (domainend_all(2,index)/=end_index(2)) .or. &
      (domainend_all(3,index)/=end_index(3)) ) then
      write(6,*) 'Dimensions do not match between background and surface files'
      write(6,*) 'background=',domainend_all(:,index)
      write(6,*) 'surface=',end_index(:)
      stop 234
  endif

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
     allocate(field3(nlon_regional,nsig_regional,nlat_regional))
     if(if_integer) allocate(ifield3(nlon_regional,nsig_regional,nlat_regional))
  else if(trim(memoryorder_all(index))=='XYZ') then 
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
     if(if_integer) allocate(ifield3(nlon_regional,nlat_regional,nsig_regional))
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=1
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
     if(if_integer) allocate(ifield3(nlon_regional,nlat_regional,nsig_regional))
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  if(if_integer) then
    call ext_ncd_read_field(dh1,DateStr1,TRIM(VarName),              &
       ifield3,WrfType,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  else
    call ext_ncd_read_field(dh1,DateStr1,TRIM(VarName),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  endif

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    if(if_integer) then
      write(6,*)' max,min =',maxval(ifield3(:,:,k)),minval(ifield3(:,:,k))
    else
      write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
    endif
  enddo

  DO j=5,1,-1
     write(*,'(11f7.1)') (field3(i,j,1),i=692,702)
  enddo

  hor_size=nlon_regional*nlat_regional
  allocate(ibuf4(hor_size*nsig_regional))
  call to_i2r(ibuf4,field3,(hor_size*nsig_regional))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size*nsig_regional,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional
         if(if_integer) then
           write(6,*) 'k, max, min, mid ',k,maxval(ifield3(:,k,:)),minval(ifield3(:,k,:)), &
                    ifield3(nlon_regional/2,k,nlat_regional/2)
         else
           write(6,*) 'k, max, min, mid ',k,maxval(field3(:,k,:)),minval(field3(:,k,:)), &
                    field3(nlon_regional/2,k,nlat_regional/2)
         endif
       enddo
     else
       do k=1,nsig_regional
         if(if_integer) then
           write(6,*) 'k, max, min, mid ',k,maxval(ifield3(:,:,k)),minval(ifield3(:,:,k)), &
                    ifield3(nlon_regional/2,nlat_regional/2,k)
         else
           write(6,*) 'k, max, min, mid ',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
         endif
       enddo
     endif
  end if

  deallocate(field3)
  if(if_integer) deallocate(ifield3)

end subroutine nc2bin

