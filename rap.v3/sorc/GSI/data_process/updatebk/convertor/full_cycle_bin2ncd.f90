subroutine full_cycle_bin2ncd(VarName,dh,DateStr,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset)

  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! for binary file
  integer(i_kind),intent(in) :: iunit
  integer(i_kind),intent(in) :: nrecs
  integer(i_llong),intent(in):: file_offset(nrecs)
  character(132), intent(in) :: varname_all(nrecs),memoryorder_all(nrecs)
  integer(i_kind),intent(in) :: domainend_all(3,nrecs)
  integer :: index
!
  integer(i_kind), intent(in)      :: dh
  character(len=19),intent(in)  :: DateStr
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
  real(r_single),allocatable:: field3(:,:,:)
  real(r_single),allocatable:: field3ncd(:,:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  write(6,*)
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(VarName)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!
!   cycle one field
!
  write(6,*) '=========================================='

  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
     allocate(field3(nlon_regional,nsig_regional,nlat_regional))
  else if(trim(memoryorder_all(index))=='XYZ') then 
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  hor_size=nlon_regional*nlat_regional
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        field3,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
  end if

!-------------  get grid info

  end_index=0
  call ext_ncd_get_var_info (dh,trim(VarName),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )

  if(ndim1 == 2) end_index(3)=1
  if( (nlon_regional .ne. end_index(1)) .or.    &
      (nlat_regional .ne. end_index(2)) .or.    &
      (nsig_regional .ne. end_index(3)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
      write(6,*)' end_index=',end_index
      stop 123
  endif

  write(6,*)' >>>>>>>>>>>  write out data to dh2  = ',dh
  write(6,*)' rmse_var=',trim(VarName)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index

  allocate(field3ncd(nlon_regional,nlat_regional,nsig_regional))
  if(trim(memoryorder_all(index))=='XZY') then
     DO k=1,nsig_regional
        field3ncd(:,:,k)=field3(:,k,:)
     ENDDO
  else if(trim(memoryorder_all(index))=='XYZ') then
     field3ncd=field3
  else if(trim(memoryorder_all(index))=='XY') then
     field3ncd=field3
  endif
  do k=1,nsig_regional
      write(6,*) 'k,max, min, mid ',k,maxval(field3ncd(:,:,k)),minval(field3ncd(:,:,k)), &
                    field3ncd(nlon_regional/2,nlat_regional/2,k)
  enddo

  call ext_ncd_write_field(dh,DateStr,TRIM(VarName),              &
       field3ncd,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  deallocate(field3ncd)
  deallocate(field3)

end subroutine full_cycle_bin2ncd 

