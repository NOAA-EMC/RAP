program  check_fields

  use mpi
  use kinds, only: r_single,i_llong,i_kind
  use native_endianness, only: byte_swap

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
  real(r_single),allocatable:: field3(:,:,:)

!=====================
  character(len=120) :: flnm1
  character(len=19)  :: DateStr1
  integer(i_kind)    :: dh1

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var

! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  real(r_single),allocatable:: field3_nc(:,:,:)

  character(132),allocatable:: varname_check(:)

!
  real :: rmax, rmin
  integer :: imax,jmax,imin,jmin
  integer :: num_swap

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  call ext_ncd_ioinit(sysdepinfo,status)

!
!           open netcdf file to read
!
  flnm1='wrfinput_d01_nc'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'diff_nc_bin:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!
!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond


  wrf_real=104_i_kind

!!! MPI IO

  iunit=33
  fileName='wrf_inout'
!  fileName='wrfbdy_d1'
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

  call initialize_byte_swap_wrf_binary_file(iunit,trim(fileName))
  call count_recs_wrf_binary_file(iunit, trim(fileName), nrecs)
        write(*,*) 'nrecs: ', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  allocate(varname_check(nrecs))

  call inventory_wrf_binary_file(iunit, trim(filename), nrecs,  &
          datestr_all,varname_all,memoryorder_all,domainend_all,   &
          start_block,end_block,start_byte,end_byte,file_offset)

  do N=1,NRECS
     write(*,'(i4,2x,a20,a5,3i10)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)
  enddo
  close(iunit)

!
!   Now test mpiio
!
  call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdonly,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if

varname_check(1)='U'
varname_check(2)='V'
varname_check(3)='T'
varname_check(4)='QVAPOR'
varname_check(5)='QCLOUD'
varname_check(6)='QRAIN'
varname_check(7)='QICE'
varname_check(8)='QSNOW'
varname_check(9)='QGRAUP'
varname_check(10)='QNRAIN'
varname_check(11)='MU'
varname_check(12)='SST'
varname_check(13)='SMOIS'
varname_check(14)='TSLB'
varname_check(15)='TSK'
varname_check(16)='SOILT1'
varname_check(17)='TH2'
do n=1,17

  varName=trim(varname_check(n))
  call retrieve_index(index,trim(varName),varname_all,nrecs)
  if(index<0) stop

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
  elseif(trim(memoryorder_all(index))=='XYZ') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
  else
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=1
  end if
!
  write(*,*) 'Dimension=',nlon_regional,nlat_regional,nsig_regional
  allocate(field3(nlon_regional,nsig_regional,nlat_regional))
  hor_size=nlon_regional*nlat_regional

  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in file"
  else
     CALL mpi_file_read_at(iunit,file_offset(index+1),             &
                          field3,hor_size*nsig_regional,mpi_real4, &
                          mpi_status_ignore, ierr)
     if(byte_swap) then
        num_swap=hor_size*nsig_regional
        call to_native_endianness_i4(field3,num_swap)
     end if
      if (ierr /= 0) then
          print*,"Error reading ", VarName," using MPIIO"
      else
       write(6,*)' MPIIO: read in ',VarName
        do k=1,nsig_regional
          write(*,*) trim(VarName),' max, min, mid ',k,maxval(field3(:,k,:)),minval(field3(:,k,:)), field3(nlon_regional/2,k,nlat_regional/2)
        enddo
      end if
  end if

  rmse_var=trim(varName)
  end_index=0
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )
  write(6,*)' <<<<<<<<<<<<<<   Read in data from dh1  = ',dh1
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  write(6,*)'ierr  = ',ierr   !DEDE

  nlon_regional=end_index(1)
  nlat_regional=end_index(2)
  nsig_regional=end_index(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  if( ndim1 == 2 ) then
      allocate(field3_nc(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
      allocate(field3_nc(nlon_regional,nlat_regional,nsig_regional))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3_nc,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    field3_nc(:,:,k)=field3_nc(:,:,k)-field3(:,k,:)
    write(6,*)'diff max,min =',k,maxval(field3_nc(:,:,k)),minval(field3_nc(:,:,k))
    rmax=-99999.0
    rmin=99999.0
    imax=0
    jmax=0
    imin=0
    jmin=0
    DO j=1,nlat_regional
    DO i=1,nlon_regional
      if(field3_nc(i,j,k) > rmax) then
        rmax=field3_nc(i,j,k) 
        imax=i
        jmax=j
      endif
      if(field3_nc(i,j,k) < rmin) then
        rmin=field3_nc(i,j,k)
        imin=i
        jmin=j
      endif
    ENDDO
    ENDDO
    write(*,*) 'max location=',rmax,imax,jmax
    write(*,*) 'min location=',rmin,imin,jmin
  enddo
  deallocate(field3_nc,field3)

enddo  !n variables

!=======================================
  call ext_ncd_ioclose(dh1, Status)
  call mpi_file_close(iunit,ierror)
  call MPI_FINALIZE(ierror)

end program check_fields

