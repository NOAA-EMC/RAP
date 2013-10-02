subroutine full_cycle_varaible_binary(VarName,ifswap,iunit,nrecs,iunit_sfc,nrecs_sfc, &
           varname_all,memoryorder_all,domainend_all,file_offset,      &
           varname_all_sfc,memoryorder_all_sfc,domainend_all_sfc,file_offset_sfc)

  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  logical,intent(in) :: ifswap
! for background file
  integer(i_kind),intent(in) :: iunit
  integer(i_kind),intent(in) :: nrecs
  integer(i_llong),intent(in):: file_offset(nrecs)
  character(132), intent(in) :: varname_all(nrecs),memoryorder_all(nrecs)
  integer(i_kind),intent(in) :: domainend_all(3,nrecs)
  integer :: index

! for background file
  integer(i_kind),intent(in) :: iunit_sfc
  integer(i_kind),intent(in) :: nrecs_sfc
  integer(i_llong),intent(in):: file_offset_sfc(nrecs_sfc)
  character(132),  intent(in):: varname_all_sfc(nrecs_sfc),memoryorder_all_sfc(nrecs_sfc)
  integer(i_kind), intent(in):: domainend_all_sfc(3,nrecs_sfc)
  integer :: index_sfc
!
  CHARACTER (LEN=19),intent(in)  :: VarName
!
  integer(i_kind)  :: nlon_regional,nlat_regional,nsig_regional

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,ierr
  integer, ALLOCATABLE:: ibuf4(:)
  real(r_single),allocatable:: field3(:,:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!
!   cycle one field
!
  write(6,*) '=========================================='

  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  call retrieve_index(index_sfc,VarName,varname_all_sfc,nrecs_sfc)
  if(index_sfc < 0) then
      print*,VarName," not found in surface file"
      stop 1234
  endif
  if( (domainend_all(1,index)/=domainend_all_sfc(1,index_sfc)) .or. &
      (domainend_all(2,index)/=domainend_all_sfc(2,index_sfc)) .or. &
      (domainend_all(3,index)/=domainend_all_sfc(3,index_sfc)) ) then
      write(6,*) 'Dimensions do not match between background and surface files'
      write(6,*) 'background=',domainend_all(:,index)
      write(6,*) 'surface=',domainend_all_sfc(:,index_sfc)
      stop 234
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
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)

  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(field3(:,k,:)),minval(field3(:,k,:)), &
                    field3(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
         write(6,*) 'k,max, min, mid ',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif 
  end if

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
         write(6,*) 'k, max, min, mid ',k,maxval(field3(:,k,:)),minval(field3(:,k,:)), &
                    field3(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif
  end if

  deallocate(field3)

end subroutine full_cycle_varaible_binary

