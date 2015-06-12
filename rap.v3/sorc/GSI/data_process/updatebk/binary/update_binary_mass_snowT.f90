subroutine update_binary_mass_snowT(iunit,nrecs,iunit_sfc,nrecs_sfc, &
           varname_all,memoryorder_all,domainend_all,file_offset,      &
           varname_all_sfc,memoryorder_all_sfc,domainend_all_sfc,file_offset_sfc)
!$$$  documentation block
!                .      .    .                                       .
!
!  ------ Read SNOWC, TSK, SOILT1 and update TSK, SOILT1 on snow point
!
!   prgmmr: Ming Hu                 date: 2011-03-1
!
! program history log:
!
! attributes:
!   language: f90
!
!$$$

  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

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
  CHARACTER (LEN=19)  :: VarName
!
  integer(i_kind)  :: nlon_regional,nlat_regional,nsig_regional

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,ierr
  real(r_single),allocatable:: snowc(:,:)
  real(r_single),allocatable:: tsk(:,:), tsk_sfc(:,:)
  real(r_single),allocatable:: soilt1(:,:), soilt1_sfc(:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!
!   cycle one field
!
  write(6,*) '=========================================='

  VarName='TSLB'
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
  else if(trim(memoryorder_all(index))=='XYZ') then 
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if
  allocate(snowc(nlon_regional,nlat_regional))
  allocate(tsk(nlon_regional,nlat_regional))
  allocate(tsk_sfc(nlon_regional,nlat_regional))
  allocate(soilt1(nlon_regional,nlat_regional))
  allocate(soilt1_sfc(nlon_regional,nlat_regional))

  hor_size=nlon_regional*nlat_regional
!

  VarName='TSK'
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
!  read tsk from surface file
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                        tsk_sfc,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     write(6,*) 'k,max, min, mid ',k,maxval(tsk_sfc(:,:)),minval(tsk_sfc(:,:)), &
                    tsk_sfc(nlon_regional/2,nlat_regional/2)
  end if

!  read tsk from background  file
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        tsk,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     write(6,*) 'k,max, min, mid ',k,maxval(tsk(:,:)),minval(tsk(:,:)), &
                   tsk(nlon_regional/2,nlat_regional/2)
  end if

  VarName='SOILT1'
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
!  read tsk from surface file
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                       soilt1_sfc,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     write(6,*) 'k,max, min, mid ',k,maxval(soilt1_sfc(:,:)),minval(soilt1_sfc(:,:)), &
                    soilt1_sfc(nlon_regional/2,nlat_regional/2)
  end if

!  read tsk from background  file
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        soilt1,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     write(6,*) 'k,max, min, mid ',k,maxval(soilt1(:,:)),minval(soilt1(:,:)), &
                   soilt1(nlon_regional/2,nlat_regional/2)
  end if

  VarName='SNOWC'
  call retrieve_index(index_sfc,VarName,varname_all_sfc,nrecs_sfc)
  if(index_sfc < 0) then
      print*,VarName," not found in surface file"
      stop 1234
  endif
!  read tsk from surface file
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                       snowc,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     write(6,*) 'k,max, min, mid ',k,maxval(snowc(:,:)),minval(snowc(:,:)), &
                    snowc(nlon_regional/2,nlat_regional/2)
  end if

!
! cycle snow grid point
!
  DO j=1,nlat_regional
  DO i=1,nlon_regional
     if(snowc(i,j) > 0.001 ) then
       tsk(i,j)    = tsk_sfc(i,j)
       soilt1(i,j) = soilt1_sfc(i,j)
     endif
  ENDDO
  ENDDO

  write(6,*) 'write out TSK, SOILT1'
!
!  write results back 
!
  VarName='TSK'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  call mpi_file_write_at(iunit,file_offset(index+1),tsk,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*) 'k, max, min, mid ',k,maxval(tsk(:,:)),minval(tsk(:,:)), &
                    tsk(nlon_regional/2,nlat_regional/2)
  end if

  VarName='SOILT1'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  call mpi_file_write_at(iunit,file_offset(index+1),soilt1,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*) 'k, max, min, mid ',k,maxval(soilt1(:,:)),minval(soilt1(:,:)), &
                    soilt1(nlon_regional/2,nlat_regional/2)
  end if

  deallocate(soilt1)
  deallocate(soilt1_sfc)
  deallocate(tsk)
  deallocate(tsk_sfc)
  deallocate(snowc)

end subroutine update_binary_mass_snowT 

