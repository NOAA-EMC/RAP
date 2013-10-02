program full_cycle_surface_binary_mass

  use mpi
  use kinds, only: r_single,i_llong,i_kind,i_byte

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
  integer(i_byte),allocatable :: buf1(:)
  integer(i_byte) :: buf2(4)
  real(r_single) :: buf4
  integer(i_kind) :: ii,iii
  equivalence (buf2,buf4)

  integer :: iunit

      CHARACTER (LEN=9)  :: filename
!      CHARACTER (LEN=19) , INTENT(IN) :: file_date_string
  CHARACTER (LEN=19)              :: VarName
!      CHARACTER (LEN=150)             :: chartemp

!      INTEGER :: i , j , k , loop, IMAX, JMAX
!      INTEGER :: DATAHANDLE, num_metgrid_levels

!!
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  REAL(r_single) :: garb
!  REAL :: dummy,tmp,garb
!      REAL, ALLOCATABLE:: dumdata(:,:,:)
!
!      CHARACTER (LEN= 8) :: dummy_char

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
  fileName='wrf_inout'
!  fileName='wrfbdy_d1'
!  fileName='geo_em.d1'
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'CONVERT_BINARY_MASS:  problem with wrfges = ',&
          trim(fileName),', Status = ',status_hdr
     call stop2(74)
  endif
  close(iunit)

  write(*,*) 'hdrbuf=',hdrbuf(1:20)

  call count_recs_wrf_binary_file(iunit, trim(fileName), nrecs)
        write(*,*) 'nrecs: ', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs),start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, trim(filename), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

  do N=1,NRECS
     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)
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

!                  pt_regional
  call retrieve_index(index,'P_TOP',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,trim(filename),pt_regional,start_block(index+1), &
             end_block(index+1),start_byte(index+1),end_byte(index+1))
  write(*,*) 'P_TOP=',pt_regional

  allocate(field2(nlon_regional,nlat_regional))
!                  XLAT
  call retrieve_index(index,'XLAT',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))

  write(6,*)' MPIIO: max,min XLAT(:,1)=',&
       maxval(field2(:,1)),minval(field2(:,1))
  write(6,*)' MPIIO: max,min XLAT(1,:)=',&
       maxval(field2(1,:)),minval(field2(1,:))
  write(6,*)' MPIIO: xlat(1,1),xlat(nlon,1)=',&
       field2(1,1),field2(nlon_regional,1)
  write(6,*)' MPIIO: xlat(1,nlat),xlat(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)

!                  XLONG
  call retrieve_index(index,'XLONG',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))
  write(6,*)' MPIIO: max,min XLONG(:,1)=',&
       maxval(field2(:,1)),minval(field2(:,1))
  write(6,*)' MPIIO: max,min XLONG(1,:)=',&
       maxval(field2(1,:)),minval(field2(1,:))
  write(6,*)' MPIIO: xlong(1,1),xlong(nlon,1)=',&
       field2(1,1),field2(nlon_regional,1)
  write(6,*)' MPIIO: xlong(1,nlat),xlong(nlon,nlat)=', &
       field2(1,nlat_regional),field2(nlon_regional,nlat_regional)
!

   close(iunit)

!
!   Now test mpiio
!
  call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdonly,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if

  allocate(buf1(4))
  VarName='DX'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
      call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                            buf1,4,mpi_integer1,               &
                            mpi_status_ignore, ierr)

      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
          do i=1,4
            buf2(i)=buf1(i)
          enddo
          print*,VarName, ' from MPIIO READ= '
          write(6,*) 'DX= ', buf4
      end if
  end if

  VarName='DY'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
      call mpi_file_read_at(iunit,file_offset(index)+5*4,     &
                            buf1,4,mpi_integer1,              &
                            mpi_status_ignore, ierr)

      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
          print*,VarName, ' from MPIIO READ= ',garb
            do i=1,4
            buf2(i)=buf1(i)
          enddo
          write(6,*) 'DY= ', buf4
      end if
  end if

  VarName='P_TOP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
      call mpi_file_read_at(iunit,file_offset(index+1),     &
                            buf1,4,mpi_integer1,            &
                            mpi_status_ignore, ierr)

      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
          print*,VarName, ' from MPIIO READ= ',garb
            do i=1,4
            buf2(i)=buf1(i)
          enddo
          write(6,*) 'P_TOP= ', buf4
      end if
  end if

  hor_size=nlon_regional*nlat_regional
  deallocate(buf1)
  allocate(buf1(4*hor_size))
  varName='XLAT'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                          buf1,4*hor_size,mpi_integer1,       &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
       ii=0
       do j=1,nlat_regional
       do i=1,nlon_regional
         ii=ii+1
         do n=1,4
            buf2(n)=buf1(n)
         enddo
         field2(i,j)=buf4
       enddo
       enddo
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
                          buf1,4*hor_size,mpi_integer1,    &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
       ii=0
       do j=1,nlat_regional
       do i=1,nlon_regional
         ii=ii+1
         do n=1,4
            buf2(n)=buf1(n)
         enddo
         field2(i,j)=buf4
       enddo
       enddo
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

!  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  allocate(field3b(nlon_regional,nsig_regional,nlat_regional))
  allocate(field3c(nlon_regional,nsig_regional,nlat_regional))

  varName='PB'
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
          write(*,*) 'PB: max, min, mid ',maxval(field3b(:,k,:)),minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if
  varName='P'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3c,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'P: max, min, mid ',k,maxval(field3c(:,k,:)),minval(field3c(:,k,:)), field3c(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if
  
  field3b=field3b+field3c
  do k=1,nsig_regional
      write(*,*) 'P all: max, min, mid ',k,maxval(field3b(:,k,:)),minval(field3b(:,k,:)), field3b(nlon_regional/2,k,nlat_regional/2)
  enddo

  varName='QSNOW'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3c,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'QSNOW: max, min, mid ',k,maxval(field3c(:,k,:)), &
        minval(field3c(:,k,:)), field3c(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  call mpi_file_close(iunit,ierror)

  call mpi_file_open(mpi_comm_world, trim(filename),     &
                    mpi_mode_rdwr ,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if

  varName='RAD_TTEN_DFI'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     call mpi_file_write_at(iunit,file_offset(index+1),field3c,  &
            hor_size*nsig_regional,mpi_real4,mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error writing ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: write out ',VarName
      end if
  endif

  varName='RAD_TTEN_DFI'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),              &
                          field3c,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
        write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) 'RAD_TTEN_DFI: max, min, mid ',k,maxval(field3c(:,k,:)), &
        minval(field3c(:,k,:)), field3c(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  call mpi_file_close(iunit,ierror)

  call MPI_FINALIZE(ierror)

end program full_cycle_surface_binary_mass

