program full_cycle_surface_convertor
!$$$  documentation block
!                .      .    .                                       .
!   full_cycle_surface_convertor : read surface variables from latest wrf forecast
!        in mass valid at the same time as background fields 
!        and update them in wrf mass background file. The format can be from netcdf
!        to binary or from binary tp netcdf
!
!  List of variables updated:
!       SMOIS
!       SNOW
!       SNOWH
!       SNOWC
!       SST
!    if surface data are in the same time with background data
!       CANWAT
!       SOILT1
!       TSLB
!       TSK
!    if surface data are older than background data
!       TSLB(3,4,5,6)
!         for snow grid point
!           SOILT1
!           TSK
!           TSLB(1,2)
!
!   prgmmr: Ming Hu                 date: 2011-02-17
!
! program history log:
!


  use mpi
  use kinds, only: r_single,i_llong,i_kind

  IMPLICIT NONE

!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! for binary file
  integer(i_kind),allocatable:: start_block(:),end_block(:)
  integer(i_kind),allocatable:: start_byte(:),end_byte(:)
  integer(i_llong),allocatable:: file_offset(:)
  character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
  integer(i_kind),allocatable:: domainend_all(:,:)
  integer(i_kind) nrecs
  CHARACTER(9)  :: filename
  integer(i_kind) :: iunit
!
  integer(i_kind) status_hdr
  integer(i_kind) hdrbuf(512)

! for netcdf file
  character(len=120) :: flnm
  character(len=19)  :: DateStr
  integer(i_kind)    :: dh
  character (len=80) :: SysDepInfo

  CHARACTER (LEN=19)  :: VarName
  CHARACTER (LEN=19)  :: VarNamelist(20)
!
  logical :: ifncd2bin
!
  integer(i_kind) :: Status, Status_next_time
  INTEGER :: iret,ierr
  INTEGER :: n
  integer(i_kind) :: imonth,ihour,iminute,isecond
  INTEGER :: iyear,imn,iday,ihrst,imin
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

!!! MPI IO
  ifncd2bin=.true.     ! read surface from netcdf file and write to binary file
!  ifncd2bin=.false.   ! read surface from binary file and write to netcdf file

! open and check binary file
  iunit=33
  fileName='wrfbinary'
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'full_cycle_surface_convertor  :  problem with = ',&
          trim(fileName),', Status = ',status_hdr
     call stop2(74)
  endif
  close(iunit)
!
!  inventory binary file
!
  call count_recs_wrf_binary_file(iunit, trim(fileName), nrecs)
  write(*,*) 'number of records in ',trim(fileName), '=', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, trim(filename), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

  do N=1,NRECS
     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)  
  enddo

  read(datestr_all(100),15)iyear,imn,iday,ihrst,imin
  write(*,*) 'Current date and time for background = ',iyear,imn,iday,ihrst,imin
15   format(i4,1x,i2,1x,i2,1x,i2,1x,i2)

  IDATE=0
  IDATE(1)=iyear
  IDATE(2)=imn
  IDATE(3)=iday
  IDATE(5)=ihrst
  IDATE(6)=imin

  close(iunit)

!
!           open netcdf file to read or write
!
  flnm='wrfnetcdf'
  call ext_ncd_ioinit(sysdepinfo,status)
  if(ifncd2bin) then
      call ext_ncd_open_for_read( trim(flnm), 0, 0, "", dh, Status)
      if ( Status /= 0 )then
         write(6,*)'cycle surface convertor:  cannot open flnm = ',&
              trim(flnm),', Status = ', Status
         stop 74
      endif
  else
      call ext_ncd_open_for_update( trim(flnm), 0, 0, "", dh, Status)
      if ( Status /= 0 )then
         write(6,*)'cycle surface convertor:  cannot open flnm = ',&
              trim(flnm),', Status = ', Status
         stop 75
      endif
  endif
!-------------  get date info  from netcdf file 
  call ext_ncd_get_next_time(dh, DateStr, Status_next_time)
  read(DateStr,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond
  JDATE=0
  JDATE(1)=iyear
  JDATE(2)=imonth
  JDATE(3)=iday
  JDATE(5)=ihour
  JDATE(6)=iminute

! find time difference
  rinc(1)=iw3jdn(jdate(1),jdate(2),jdate(3))-  &
          iw3jdn(idate(1),idate(2),idate(3))
  rinc(2:5)=jdate(5:8)-idate(5:8)
  write(*,*) RINC   ! DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS
  timediff=rinc(1)*24+rinc(2)   ! hours
  if(abs(timediff) > 1 ) then
    write(*,*) 'binary and netcdf are not in the same time '
    stop
  endif
!
!   open binary file
!
  if(ifncd2bin) then
      call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdwr,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
          call wrf_error_fatal("Error opening file with mpi io")
      end if
  else
      call mpi_file_open(mpi_comm_world, trim(filename),     &
                    mpi_mode_rdonly,mpi_info_null, iunit, ierr)
      if (ierr /= 0) then
          call wrf_error_fatal("Error opening file with mpi io")
      end if
  end if

  VarNamelist(1)='SMOIS'
  VarNamelist(2)='SNOW'
  VarNamelist(3)='SNOWH'
  VarNamelist(4)='SNOWC'
  VarNamelist(5)='CANWAT'
  VarNamelist(6)='SST'
  VarNamelist(7)='TSLB'
  VarNamelist(8)='SOILT1'
  VarNamelist(9)='SEAICE'
  VarNamelist(10)='TSK'
 
  DO n=1,10
     VarName=VarNamelist(n)
     if(ifncd2bin) then
        call full_cycle_ncd2bin(VarName,dh,DateStr,                         &
             iunit,nrecs,varname_all,memoryorder_all,domainend_all,file_offset)
     else
        call full_cycle_bin2ncd(VarName,dh,DateStr,                         &
             iunit,nrecs,varname_all,memoryorder_all,domainend_all,file_offset)
     endif
  ENDDO

  call ext_ncd_ioclose(dh, Status)
  call mpi_file_close(iunit,ierror)

  call MPI_FINALIZE(ierror)

end program full_cycle_surface_convertor

