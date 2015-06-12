subroutine partial_cycle_varaible_binary(iunit,nrecs,iunit_sfc,nrecs_sfc, &
           varname_all,memoryorder_all,domainend_all,file_offset,      &
           varname_all_sfc,memoryorder_all_sfc,domainend_all_sfc,file_offset_sfc)

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
  real(r_single),allocatable:: tslb(:,:,:), tslb_sfc(:,:,:)

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
     allocate(tslb(nlon_regional,nsig_regional,nlat_regional))
     allocate(tslb_sfc(nlon_regional,nsig_regional,nlat_regional))
  else if(trim(memoryorder_all(index))=='XYZ') then 
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(tslb(nlon_regional,nlat_regional,nsig_regional))
     allocate(tslb_sfc(nlon_regional,nlat_regional,nsig_regional))
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(tslb(nlon_regional,nlat_regional,nsig_regional))
     allocate(tslb_sfc(nlon_regional,nlat_regional,nsig_regional))
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
!  read tslb from surface file
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                        tslb_sfc,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all_sfc(index_sfc))=='XZY') then
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(tslb_sfc(:,k,:)),minval(tslb_sfc(:,k,:)), &
                    tslb_sfc(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
         write(6,*) 'k,max, min, mid ',k,maxval(tslb_sfc(:,:,k)),minval(tslb_sfc(:,:,k)), &
                    tslb_sfc(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif 
  end if

!  read tslb from background  file
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        tslb,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(tslb(:,k,:)),minval(tslb(:,k,:)), &
                    tslb(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
         write(6,*) 'k,max, min, mid ',k,maxval(tslb(:,:,k)),minval(tslb(:,:,k)), &
                    tslb(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif
  end if

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
     if(snowc(i,j) > 0.9 ) then
       tsk(i,j)    = tsk_sfc(i,j)
       soilt1(i,j) = soilt1_sfc(i,j)
!       tslb(i,1,j) = tslb_sfc(i,1,j)
!       tslb(i,2,j) = tslb_sfc(i,2,j)
     endif
  ENDDO
  ENDDO
  VarName='TSLB'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  if(trim(memoryorder_all(index))=='XZY') then
    DO j=1,nlat_regional
    DO i=1,nlon_regional
     if(snowc(i,j) > 0.9 ) then
       tslb(i,1,j) = tslb_sfc(i,1,j)
       tslb(i,2,j) = tslb_sfc(i,2,j)
     endif
    ENDDO
    ENDDO
    DO k=3,nsig_regional
      tslb(:,k,:)=tslb_sfc(:,k,:)
    ENDDO
  else
    DO j=1,nlat_regional
    DO i=1,nlon_regional
     if(snowc(i,j) > 0.9 ) then
       tslb(i,j,1) = tslb_sfc(i,j,1)
       tslb(i,j,2) = tslb_sfc(i,j,2)
     endif
    ENDDO
    ENDDO
    DO k=3,nsig_regional
      tslb(:,:,k)=tslb_sfc(:,:,k)
    ENDDO
  endif

  write(6,*) 'write out TSK, SOILT1, TSLB'
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

  VarName='TSLB'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  call mpi_file_write_at(iunit,file_offset(index+1),tslb,  &
            hor_size*nsig_regional,mpi_real4,mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(tslb(:,k,:)),minval(tslb(:,k,:)), &
                    tslb(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
         write(6,*) 'k, max, min, mid ',k,maxval(tslb(:,:,k)),minval(tslb(:,:,k)), &
                    tslb(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif
  end if

  deallocate(tslb)
  deallocate(tslb_sfc)
  deallocate(soilt1)
  deallocate(soilt1_sfc)
  deallocate(tsk)
  deallocate(tsk_sfc)
  deallocate(snowc)

end subroutine partial_cycle_varaible_binary

