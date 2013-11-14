program sync_nc2bin_geo 
!$$$  documentation block
!                .      .    .                                       .
!   full_cycle_surface_netcdf_mass: read surface variables from latest wrf forecast
!        in mass binary valid at the same time as background fields 
!        and update them in wrf mass background file
!
!
!   prgmmr: Ming Hu                 date: 2010-07-26
!
! program history log:
!   Ming Hu    date: 2013-06-04  : update to RAPv2 based on netcdf version
!


  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE

!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! for background file
  integer(i_kind),allocatable:: start_block(:),end_block(:)
  integer(i_kind),allocatable:: start_byte(:),end_byte(:)
  integer(i_llong),allocatable:: file_offset(:)
  character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
  integer(i_kind),allocatable:: domainend_all(:,:)
  integer(i_kind) nrecs
  CHARACTER(9)  :: filename
  integer(i_kind) :: iunit

! for background file
  character(len=180) :: flnm1
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

!
  integer(i_kind) status_hdr
  integer(i_kind) hdrbuf(512)

  CHARACTER (LEN=19)  :: VarName
  logical :: if_integer
!
!
  INTEGER :: istatus,iret,ierr
  INTEGER :: n
  INTEGER :: imn,ihrst,imin
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff
  logical :: ifswap

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)
  
  ifswap=.true.

!!! MPI IO

! open and check background file
  iunit=33
  fileName='geoem_int'
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'full_cycle_surface_binary_mass:  problem with = ',&
          trim(fileName),', Status = ',status_hdr
     call stop2(74)
  endif
  close(iunit)

!
!
!           open netcdf file to read
  call ext_ncd_ioinit(sysdepinfo,status)
!
  flnm1='geo_em.d01.nc'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'diff_nc_bin:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!
!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') &
         iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond


  wrf_real=104_i_kind
!
!  inventory both files
!
! first background file
  call count_recs_wrf_binary_file(iunit, ifswap,trim(fileName), nrecs)
  write(*,*) 'number of records in ',trim(fileName), '=', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, ifswap,trim(filename), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

!  do N=1,NRECS
!     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)  
!  enddo

  read(datestr_all(100),15)iyear,imn,iday,ihrst,imin
  write(*,*) 'Current date and time for background = ',iyear,imn,iday,ihrst,imin
  close(iunit)

15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)

!
!   cycle the surface fields
!
  call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdwr,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if

!  ------ 
  if_integer=.false.
  VarName='HGT_M'
  call mpi_barrier(mpi_comm_world,ierror)
  call nc2bin(dh1,DateStr1, VarName,ifswap,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset,if_integer)
!  ------ 
  if_integer=.false.
  VarName='HGT_U'
  call mpi_barrier(mpi_comm_world,ierror)
  call nc2bin(dh1,DateStr1, VarName,ifswap,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset,if_integer)
!  ------ 
  if_integer=.false.
  VarName='HGT_V'
  call mpi_barrier(mpi_comm_world,ierror)
  call nc2bin(dh1,DateStr1, VarName,ifswap,iunit,nrecs, &
           varname_all,memoryorder_all,domainend_all,file_offset,if_integer)

!
  call ext_ncd_ioclose(dh1, Status)

  call mpi_file_close(iunit,ierror)

  call MPI_FINALIZE(ierror)

end program sync_nc2bin_geo

