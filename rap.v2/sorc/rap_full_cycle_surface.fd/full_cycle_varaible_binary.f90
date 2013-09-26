subroutine full_cycle_varaible_binary(VarName,ifswap,iunit,nrecs,iunit_sfc,nrecs_sfc, &
           varname_all,memoryorder_all,domainend_all,file_offset,      &
           varname_all_sfc,memoryorder_all_sfc,domainend_all_sfc,file_offset_sfc,if_integer)

  use mpi
  use kinds, only: r_single,i_llong,i_kind

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
  integer,allocatable:: ifield3(:,:,:)

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

  hor_size=nlon_regional*nlat_regional
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)

  if(if_integer) ifield3=field3
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional
         if(if_integer) then
           write(6,*) 'k, max, min, mid ',k,maxval(ifield3(:,k,:)),minval(ifield3(:,k,:)),&
                    ifield3(nlon_regional/2,k,nlat_regional/2)
         else
           write(6,*) 'k, max, min, mid ',k,maxval(field3(:,k,:)),minval(field3(:,k,:)),&
                    field3(nlon_regional/2,k,nlat_regional/2)
         endif
       enddo
     else
       do k=1,nsig_regional
         if(if_integer) then
           write(6,*) 'k,max, min, mid ',k,maxval(ifield3(:,:,k)),minval(ifield3(:,:,k)), &
                    ifield3(nlon_regional/2,nlat_regional/2,k)
         else
           write(6,*) 'k,max, min, mid ',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
         endif
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

end subroutine full_cycle_varaible_binary

subroutine full_cycle_varaible_binary_sfc629(VarName,ifswap,iunit,nrecs,iunit_sfc,nrecs_sfc, &
           varname_all,memoryorder_all,domainend_all,file_offset,      &
           varname_all_sfc,memoryorder_all_sfc,domainend_all_sfc,file_offset_sfc,if_integer)

  use mpi
  use kinds, only: r_single,i_llong,i_kind

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
  integer(i_kind)  :: nlon_regional_sfc,nlat_regional_sfc,nsig_regional_sfc

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,ierr
  integer, ALLOCATABLE:: ibuf4(:)
  real(r_single),allocatable:: field3(:,:,:)
  real(r_single),allocatable:: field3_9(:,:,:)

  integer, parameter :: nsoilold=6,nsoilnew=9
  real(r_single) :: zsold(nsoilold),zsnew(nsoilnew)
  real(r_single) :: wk,wkp1

  integer(i_kind) :: k9,k6,nsig_regional9
  logical :: ifsamelevel
!
!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
!
!   cycle one field
!
  zsold = (/ 0.00 , 0.05 , 0.20 , 0.40 , 1.60 , 3.00 /)
  zsnew = (/ 0.00 , 0.01 , 0.04 , 0.10 , 0.30, 0.60, 1.00 , 1.60, 3.00 /)
!
  if(if_integer) then
     write(*,*) 'Cannot be integer field'
     stop 345
  endif

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

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
     nlon_regional_sfc=domainend_all_sfc(1,index)
     nlat_regional_sfc=domainend_all_sfc(3,index)
     nsig_regional_sfc=domainend_all_sfc(2,index)
  else if(trim(memoryorder_all(index))=='XYZ') then 
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     nlon_regional_sfc=domainend_all_sfc(1,index)
     nlat_regional_sfc=domainend_all_sfc(2,index)
     nsig_regional_sfc=domainend_all_sfc(3,index)
  else if(trim(memoryorder_all(index))=='XY') then
     write(6,*) ' wrong memory order ',trim(memoryorder_all(index))
     write(6,*) 'should be 3D field'
     stop 123
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  if( (nlon_regional/=nlon_regional_sfc) .or. &
      (nlat_regional/=nlat_regional_sfc) ) then
      write(6,*) 'Dimensions do not match between background and surface files'
      write(6,*) 'background=',nlon_regional,nlat_regional
      write(6,*) 'surface=',nlat_regional,nlat_regional_sfc
      stop 234
  endif
  if( nsig_regional/=nsig_regional_sfc) then
     if(nsig_regional==9 .and. nsig_regional_sfc==6) then
       write(*,*) 'soil model: covert 6 level to 9 level'
     else
       write(6,*) 'Dimensions do not match between background and surface files'
       write(6,*) 'background=',nsig_regional
       write(6,*) 'surface=',nsig_regional_sfc
       stop 234
     endif
  endif
  if(trim(memoryorder_all(index))=='XZY') then
    allocate(field3(nlon_regional,nsig_regional_sfc,nlat_regional))
    allocate(field3_9(nlon_regional,nsig_regional,nlat_regional))
  else
    allocate(field3(nlon_regional,nlat_regional,nsig_regional_sfc))
    allocate(field3_9(nlon_regional,nlat_regional,nsig_regional))
  endif

  hor_size=nlon_regional*nlat_regional
  allocate(ibuf4(hor_size*nsig_regional_sfc))
  CALL mpi_file_read_at(iunit_sfc,file_offset_sfc(index_sfc+1),     &
                        ibuf4,hor_size*nsig_regional_sfc,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional_sfc))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional_sfc))
  deallocate(ibuf4)

  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
       do k=1,nsig_regional_sfc
           write(6,*) 'k, max, min, mid ',k,maxval(field3(:,k,:)),minval(field3(:,k,:)),&
                    field3(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional_sfc
           write(6,*) 'k,max, min, mid ',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
                    field3(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif 
  end if

  if( (nsig_regional .ne. nsig_regional_sfc) ) then
      write(6,*) ' swith Dimensions: ',nsig_regional_sfc,nsig_regional
      do k9=1,nsoilnew
         ifsamelevel=.false.
         do k6=1,nsoilold
            if(abs(zsold(k6)-zsnew(k9)) < 1.0e-5) then
                if(trim(memoryorder_all(index))=='XZY') then
                   field3_9(:,k9,:)=field3(:,k6,:)
                else
                   field3_9(:,:,k9)=field3(:,:,k6)
                endif
                write(*,*) 'assign old ',k6,zsold(k6), 'to new ', k9,zsnew(k9)
                ifsamelevel=.true.
            endif
         enddo
         if(ifsamelevel) then
            cycle
         else
            do k6=1,nsoilold-1
               if((zsold(k6)<zsnew(k9)) .and. (zsold(k6+1) > zsnew(k9)) ) k=k6
            enddo
            wkp1=(zsnew(k9)-zsold(k))/(zsold(k+1)-zsold(k))
            wk=1.0-wkp1
            if(trim(memoryorder_all(index))=='XZY') then
               field3_9(:,k9,:)=field3(:,k,:)*wk + field3(:,k+1,:)*wkp1
            else
               field3_9(:,:,k9)=field3(:,:,k)*wk + field3(:,:,k+1)*wkp1
            endif
            write(*,*) k9, 'is interpolated between',k,' and ',k+1, 'with ', &
                     wk, wkp1
         endif
      enddo
  else
      write(6,*) ' Keep same vertical Dimensions: ',nsig_regional,nsig_regional_sfc
      field3_9=field3
  endif

  allocate(ibuf4(hor_size*nsig_regional))
  call to_i2r(ibuf4,field3_9,(hor_size*nsig_regional))
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
           write(6,*) 'k, max, min, mid ',k,maxval(field3_9(:,k,:)),minval(field3_9(:,k,:)), &
                    field3_9(nlon_regional/2,k,nlat_regional/2)
       enddo
     else
       do k=1,nsig_regional
           write(6,*) 'k, max, min, mid ',k,maxval(field3_9(:,:,k)),minval(field3_9(:,:,k)), &
                    field3_9(nlon_regional/2,nlat_regional/2,k)
       enddo
     endif
  end if

  deallocate(field3)
  deallocate(field3_9)

end subroutine full_cycle_varaible_binary_sfc629

