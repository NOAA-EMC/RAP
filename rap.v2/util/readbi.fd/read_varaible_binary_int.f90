subroutine read_varaible_binary_int(VarName,ifswap,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset)

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

!
  CHARACTER (LEN=19),intent(in)  :: VarName
!
  integer(i_kind)  :: nlon_regional,nlat_regional,nsig_regional

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,ierr
  integer, ALLOCATABLE:: ibuf4(:)
  integer(i_kind),allocatable:: ifield3(:,:,:)

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

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
     allocate(ifield3(nlon_regional,nsig_regional,nlat_regional))
  else if(trim(memoryorder_all(index))=='XYZ') then 
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(ifield3(nlon_regional,nlat_regional,nsig_regional))
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(ifield3(nlon_regional,nlat_regional,nsig_regional))
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  write(*,*) index+1, file_offset(index+1)
  write(*,*) 'dimension=',nlon_regional,nlat_regional,nsig_regional
  hor_size=nlon_regional*nlat_regional
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,ifield3,(hor_size*nsig_regional))
  deallocate(ibuf4)

  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(ifield3(:,k,:)),minval(ifield3(:,k,:)), &
                    ifield3(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
         write(6,*) 'k,max, min, mid ',k,maxval(ifield3(:,:,k)),minval(ifield3(:,:,k)), &
                    ifield3(nlon_regional/2,nlat_regional/2,k)
         do j=440-1,440+1
         do i=197-1,197+1
            write(*,*) i,j,ifield3(i,j,k)
         enddo
         enddo
       enddo
     endif 
  end if

  deallocate(ifield3)

end subroutine read_varaible_binary_int

