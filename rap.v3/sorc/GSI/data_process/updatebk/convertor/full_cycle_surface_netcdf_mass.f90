program full_cycle_surface_netcdf_mass
!$$$  documentation block
!                .      .    .                                       .
!   full_cycle_surface_netcdf_mass: read surface variables from latest wrf mass
!            netcdf 0 h forecast 
!           and update them in wrf mass background file
!  List of variables updated:
!	SMOIS
!	SNOW
!	SNOWH
!       SNOWC
!       SST
!    if surface data are in the same time with background data
!	CANWAT
!       SOILT1
!	TSLB
!	TSK
!    if surface data are older than background data
!       TSLB(3,4,5,6)
!         for snow grid point
!           SOILT1
!           TSK
!           TSLB(1,2)
!
!
!   prgmmr: Ming Hu                 date: 2008-11-21
!
! program history log:
!
!    2009-11-09 Ming Hu : add snowc
!
!   input argument list:
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind
  implicit none

! Declare local parameters

  character(len=120) :: flnm1
  character(len=120) :: flnm2
  character(len=19)  :: DateStr1
  character(len=19)  :: DateStr2
  integer(i_kind)    :: dh1
  integer(i_kind)    :: dh2

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var

  call ext_ncd_ioinit(sysdepinfo,status)

!
!           open netcdf file to read
!
  flnm1='wrfout_d01_save'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!
!          open netcdf file to write
!
  flnm2='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm2), 0, 0, "", dh2, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm2 = ',&
          trim(flnm2),', Status = ', Status
     stop 75
  endif

!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                        ,iyear,imonth,iday,ihour,iminute,isecond
  JDATE=0
  JDATE(1)=iyear
  JDATE(2)=imonth
  JDATE(3)=iday
  JDATE(5)=ihour
  JDATE(6)=iminute

!-------------  get date info from file write out

  call ext_ncd_get_next_time(dh2, DateStr2, Status_next_time)
  read(DateStr2,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)') ' write data to file at time  (y,m,d,h,m,s):'        &
              ,iyear,imonth,iday,ihour,iminute,isecond
  IDATE=0
  IDATE(1)=iyear
  IDATE(2)=imonth
  IDATE(3)=iday
  IDATE(5)=ihour
  IDATE(6)=iminute
!
! find time difference
  rinc(1)=iw3jdn(jdate(1),jdate(2),jdate(3))-  &
          iw3jdn(idate(1),idate(2),idate(3))
  rinc(2:5)=jdate(5:8)-idate(5:8)
  write(*,*) RINC   ! DAYS, HOURS, MINUTES, SECONDS, MILLISECONDS
  timediff=rinc(1)*24+rinc(2)   ! hours
  if(timediff < -49 .or. timediff > 0.1 ) then
    write(*,*) 'surface data file is too old, NO CYCLE'
    stop 
  endif
!
!  ------ update SMOIS
  rmse_var='SMOIS'
  call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update SNOW
  rmse_var='SNOW'
  call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update SNOWH
  rmse_var='SNOWH'
  call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update SNOWC
  rmse_var='SNOWC'
  call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

  if( abs(timediff) < 0.01 ) then
!  ------ update SEAICE
      rmse_var='SST'
      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update CANWAT
      rmse_var='CANWAT'
      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update TSLB
      rmse_var='TSLB'
      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update SOILT1
      rmse_var='SOILT1'
      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

!  ------ update SEAICE 
      rmse_var='SEAICE'
      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)


!  ------ update TSK
      rmse_var='TSK'
      call update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)

   else

!  ------ partial update TSK, SOILT1, TSLB
      call update_netcdf_mass_partial(dh1,dh2,DateStr1,DateStr2)

   endif


!-------------  close files ----------------
  call ext_ncd_ioclose(dh1, Status)
  call ext_ncd_ioclose(dh2, Status)

end program full_cycle_surface_netcdf_mass

subroutine update_netcdf_mass(dh1,dh2,DateStr1,DateStr2,rmse_var)
!$$$  documentation block
!                .      .    .                                       .
!   update_netcdf_mass: read one variable from netcdf file and 
!           and write it into another netcdf file
!
!   prgmmr: Ming Hu                 date: 2009-01-16
!
! program history log:
!
!   input argument list:
!      dh1 :    handle of file read in
!      dh2 :    handle of file write out
!      DateStr1 : time string of file read in 
!      DateStr2 : time string of file write out
!      rmse_var :  variable updated
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind, r_kind
  implicit none

!
  integer(i_kind), intent(in)      :: dh1
  integer(i_kind), intent(in)      :: dh2
  character (len=31), intent(in)   :: rmse_var
  character(len=19),intent(in)  :: DateStr1
  character(len=19),intent(in)  :: DateStr2
  
! rmse stuff
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index
  character (len= 4) :: staggering
  character (len= 3) :: ordering
  
  character (len=80), dimension(3)  ::  dimnames
  integer(i_kind) wrf_real
  
! Declare local parameters
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field3(:,:,:)
  
  integer(i_kind) :: k
  integer(i_kind) :: ierr
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Update variable ', trim(rmse_var)
  write(6,*) ' ================== '

  wrf_real=104_i_kind
!-------------  get grid info
  
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
      allocate(field3(nlon_regional,nlat_regional,1))
  elseif( ndim1 == 3 ) then
      allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
      write(6,*) 'update_netcdf_mass: Wrong dimension '
      stop 123
  endif

  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )

  if(ndim1 == 2) end_index(3)=1
  DO k=1,end_index(3)
    write(6,*)' max,min =',maxval(field3(:,:,k)),minval(field3(:,:,k))
  enddo
  
!-------------  get grid info
   
  end_index=0
  call ext_ncd_get_var_info (dh2,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index, WrfType, ierr    )

  if( (nlon_regional .ne. end_index(1)) .or.    &
      (nlat_regional .ne. end_index(2)) .or.    &
      (nsig_regional .ne. end_index(3)) ) then
      write(6,*) ' Dimensions do not match!!!'
      write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
      stop 123
  endif

  write(6,*)' >>>>>>>>>>>  write out data to dh2  = ',dh2       
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index=',end_index
  call ext_ncd_write_field(dh2,DateStr2,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index,               & !dom
       start_index,end_index,               & !mem
       start_index,end_index,               & !pat
       ierr                                 )
  
  deallocate(field3)
  
end subroutine update_netcdf_mass

SUBROUTINE wrf_debug( level , str )
  USE module_wrf_error
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  INTEGER               :: debug_level
  CHARACTER (LEN=256) :: time_str
  CHARACTER (LEN=256) :: grid_str
  CHARACTER (LEN=512) :: out_str
!  CALL get_wrf_debug_level( debug_level )
  IF ( level .LE. debug_level ) THEN
    ! old behavior
!      CALL wrf_message( str )
  ENDIF
  write(*,*) 'wrf_debug called !'
  RETURN
END SUBROUTINE wrf_debug

