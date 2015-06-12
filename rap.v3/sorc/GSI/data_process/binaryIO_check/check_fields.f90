program  check_fields

  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE

!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  integer(i_kind),allocatable:: start_block(:),end_block(:)
  integer(i_kind),allocatable:: start_byte(:),end_byte(:)
  integer(i_llong),allocatable:: file_offset(:)
  integer(i_llong) n_position
  character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
  integer(i_kind),allocatable:: domainend_all(:,:)
  integer(i_kind) nrecs
  integer(i_kind) status_hdr
  integer(i_kind) hdrbuf(512)

  integer :: iunit

  CHARACTER (LEN=9)  :: filename
  CHARACTER (LEN=19) :: VarName

  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  REAL(r_single) :: garb

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,index,ierr
  real(r_single) :: pt_regional
  real(r_single),allocatable:: field2(:,:)
  real(r_single),allocatable:: field3(:,:,:),field3b(:,:,:),field3c(:,:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

!!! MPI IO

  iunit=33
!  fileName='wrf_inout'
  fileName='wrfbdy_d1'
!  fileName='geo_em.d1'
  write(*,*) ' check file: ',trim(fileName)
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'problem with wrfges = ',&
          trim(fileName),', Status = ',status_hdr
     call stop2(74)
  endif
  close(iunit)

  call count_recs_wrf_binary_file(iunit, trim(fileName), nrecs)
        write(*,*) 'nrecs: ', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, trim(filename), nrecs,  &
          datestr_all,varname_all,memoryorder_all,domainend_all,   &
          start_block,end_block,start_byte,end_byte,file_offset)

  do N=1,NRECS
     write(*,'(i4,2x,a20,a5,3i10)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)
  enddo

  call retrieve_index(index,'T',varname_all,nrecs)
  if(index<0) stop

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
  end if
  if(trim(memoryorder_all(index))=='XYZ') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
  end if
  allocate(field2(nlon_regional,nlat_regional))
!
!   Now test mpiio
!
  close(iunit)
  call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdonly,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if

  VarName='P_TOP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
      call mpi_file_read_at(iunit,file_offset(index+1),     &
                            garb,1,mpi_real4,               &
                            mpi_status_ignore, ierr)

      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
          print*,VarName, ' from MPIIO READ= ',garb
          write(6,*) 'cenlat= ', garb
      end if
  end if

  hor_size=nlon_regional*nlat_regional
  varName='XLAT'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
       write(6,*)' MPIIO: max,min XLAT(:,1)=',&
       maxval(field2(:,1)),minval(field2(:,1))
       write(6,*)' MPIIO: max,min XLAT(1,:)=',&
       maxval(field2(1,:)),minval(field2(1,:))
       write(6,*)' MPIIO: xlat(1,1),xlat(nlon,1)=',&
       field2(1,1),field2(nlon_regional,1)
       write(6,*)' MPIIO: xlat(1,nlat),xlat(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
      end if
  end if

  varName='XLONG'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                          field2,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
       write(6,*)' MPIIO: max,min XLONG(:,1)=',&
       maxval(field2(:,1)),minval(field2(:,1))
       write(6,*)' MPIIO: max,min XLONG(1,:)=',&
       maxval(field2(1,:)),minval(field2(1,:))
       write(6,*)' MPIIO: XLONG(1,1),XLONG(nlon,1)=',&
       field2(1,1),field2(nlon_regional,1)
       write(6,*)' MPIIO: XLONG(1,nlat),XLONG(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
      end if
  end if

  allocate(field3b(nlon_regional,nsig_regional,nlat_regional))
  allocate(field3c(nlon_regional,nsig_regional,nlat_regional))

  varName='T'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
       write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'T: max, min, mid ',k,maxval(field3b(:,k,:)),minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='QVAPOR'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QVAPOR: max, min, mid ',k,maxval(field3b(:,k,:)),minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='QCLOUD'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QCLOUD: max, min, mid ',k,maxval(field3b(:,k,:)), &
          minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='QRAIN'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QRAIN: max, min, mid ',k,maxval(field3b(:,k,:)), &
          minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='QICE'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QICE: max, min, mid ',k,maxval(field3b(:,k,:)), &
          minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='QSNOW'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QSNOW: max, min, mid ',k,maxval(field3b(:,k,:)), &
          minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='QGRAUP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QGRAUP: max, min, mid ',k,maxval(field3b(:,k,:)), &
          minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  varName='RAD_TTEN_DFI'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3b,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'RAD_TTEN_DFI: max, min, mid ',k,maxval(field3b(:,k,:)), &
          minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if
  
  call mpi_file_close(iunit,ierror)
  call MPI_FINALIZE(ierror)

end program check_fields

