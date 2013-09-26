subroutine update_SNOWICE_binary_mass(ifswap,snowiceRR, xland, nlon, nlat)
!$$$  documentation block
!                .      .    .                                       .
!   update_SNOWICE_netcdf_mass: read SNOW from wrf mass binary old background file
!           and update SST in wrf mass background file
!   prgmmr: Ming Hu                 date: 2010-07-25
!
! program history log:
!
! 2009-07-27: make consistent of all land surface parameters and using fraction seac ice
! if ( fractional_seaice == 0 ) then
!    xice_threshold = 0.5
! else if ( fractional_seaice == 1 ) then
!    xice_threshold = 0.02
! endif
!
!
! For sea-ice:
!                     vegcat(i,j)=24     not used
!                     ivgtyp(i,j)=24     int IVGTYP
!                     lu_index(i,j)=24   float LU_INDEX
!                     landmask(i,j)=1.   float LANDMASK
!                     xland(i,j)=1.      float XLAND
!                     isltyp(i,j)=16.    int ISLTYP
! 
! For water:
!                     vegcat(i,j)=16
!                     ivgtyp(i,j)=16
!                     lu_index(i,j)=16
!                     landmask(i,j)=0.
!                     xland(i,j)=2.
!                     isltyp(i,j)=14.
! 

!
!   input argument list:
!       snowRR: snow  cover
!       iceRR:  seaice cover
!       xland:    land and sea mask
!       nlon:  x dimension
!       nlat:  Y dimension
!
! attributes:
!   language: f90
!
!$$$

  
  use mpi
  use kinds, only: r_single,i_llong,i_kind

  implicit none

  logical,intent(in) :: ifswap
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

!
  integer :: nlon, nlat
  real  :: snowiceRR(nlon,nlat)
  real  :: xland(nlon,nlat)

! Declare local parameters

  character(len=19)  :: DateStr1
  
  integer(i_kind) :: i,j,k
  integer(i_kind) :: l, n
  integer(i_kind) :: ierr, ier, Status

! for background file IO
  integer(i_kind),allocatable:: start_block(:),end_block(:)
  integer(i_kind),allocatable:: start_byte(:),end_byte(:)
  integer(i_llong),allocatable:: file_offset(:)
  character(132),allocatable:: datestr_all(:),varname_all(:),memoryorder_all(:)
  integer(i_kind),allocatable:: domainend_all(:,:)
  integer(i_kind) nrecs
  CHARACTER (LEN=9)  :: filename
  integer(i_kind) :: iunit,index,hor_size
!
  integer(i_kind) status_hdr
  integer(i_kind) hdrbuf(512)

  CHARACTER (LEN=19)  :: VarName

! rmse stuff
  
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  integer,allocatable::ifield2(:,:)
  real(r_single),allocatable::field2(:,:)
  real(r_single),allocatable::field3(:,:,:)
  real(r_single),allocatable::precip(:,:)
  real(r_single),allocatable::surftemp(:,:)
  real(r_single),allocatable::landmask_soilmoisture1(:,:)

  real(r_single),allocatable::snow(:,:)
  real(r_single),allocatable::snowh(:,:)
  real(r_single),allocatable::snowc(:,:)
  real(r_single),allocatable::seaice(:,:)

!  surface parameters
!                     ivgtyp(i,j)=24     int IVGTYP
!                     isltyp(i,j)=16.    int ISLTYP
  real(r_single),allocatable::landmask(:,:)
  real(r_single),allocatable::xland_rr(:,:)
  real(r_single),allocatable::lu_index(:,:)
!  integer(i_kind),allocatable:: ivgtyp(:,:)
!  integer(i_kind),allocatable:: isltyp(:,:)
  integer,allocatable:: ivgtyp(:,:)
  integer,allocatable:: isltyp(:,:)
!
!
  real(r_single)    :: xice_threshold
  integer(i_kind)   :: fractional_seaice
!
  real(r_single)    :: time, time1, time2
  real(r_single)    :: a, b

  real(r_single)    :: R, Cp, RCP, P0
  integer(i_kind)   :: num_seaice2water, num_water2seaice
  integer   :: MSLANDID

  integer, ALLOCATABLE:: ibuf4(:)
!
!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  R = 287.06  ! gas constant JKg-1K-1
  Cp= 1003.5  ! Specific heat capacity  JKg-1K-1
  RCP = R/Cp
  P0=100000.0 ! Pa
!
  fractional_seaice=0
  if ( fractional_seaice == 0 ) then
    xice_threshold = 0.5
    write(*,*) ' do not use fraction sea ice'
  else if ( fractional_seaice == 1 ) then
    xice_threshold = 0.02
    write(*,*) ' use fraction sea ice'
  endif
!  

!   transfer code from binary arw inout file
!      to temporary binary format

! open and check background file
  iunit=33
  fileName='wrf_inout'
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'update_SNOWICE_binary_mass:  problem with = ',&
          trim(fileName),', Status = ',status_hdr
     stop 74
  endif
  close(iunit)
  write(*,*) 'hdrbuf', (hdrbuf(1:20))


!
!  inventory background file
!
  call count_recs_wrf_binary_file(iunit,ifswap, trim(fileName), nrecs)
  write(*,*) 'number of records in ',trim(fileName), '=', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit,ifswap, trim(filename), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)

!Mhu  do N=1,NRECS
!Mhu     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)
!Mhu  enddo

  close(iunit)
!
! open file 
!
  call mpi_file_open(mpi_comm_world, trim(filename),     &
                     mpi_mode_rdwr,mpi_info_null, iunit, ierr)
  if (ierr /= 0) then
      call wrf_error_fatal("Error opening file with mpi io")
  end if


!-------------  get date info
!-------------  get grid info

  VarName='T'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif

   write(*,*) datestr_all(index)
  read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' precipiation and snow data from background file at time:'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
     allocate(field3(nlon_regional,nsig_regional,nlat_regional))
  else if(trim(memoryorder_all(index))=='XYZ') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  hor_size=nlon_regional*nlat_regional

  if( (nlon_regional/=nlon) .or. (nlat_regional/=nlat) ) then
      write(6,*) 'Dimensions do not match between input and geo file'
      write(6,*) 'input=',nlon,nlat
      write(6,*) 'geo=',nlon_regional,nlat_regional
      stop 234
  endif

  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(snow(nlon_regional,nlat_regional))
  allocate(snowh(nlon_regional,nlat_regional))
  allocate(snowc(nlon_regional,nlat_regional))
  allocate(seaice(nlon_regional,nlat_regional))
  allocate(surftemp(nlon_regional,nlat_regional))
  allocate(landmask_soilmoisture1(nlon_regional,nlat_regional))

  allocate(landmask(nlon_regional,nlat_regional))
  allocate(xland_rr(nlon_regional,nlat_regional))
  allocate(lu_index(nlon_regional,nlat_regional))
  allocate(ivgtyp(nlon_regional,nlat_regional))
  allocate(isltyp(nlon_regional,nlat_regional))

  allocate(ifield2(nlon_regional,nlat_regional))
  allocate(field2(nlon_regional,nlat_regional))
  allocate(precip(nlon_regional,nlat_regional))
  precip=0.0
  field2=0.0
  ifield2=0
  field3=0.0
  
if(1==1) then   ! use 1st level atmosphere temperature
  write(6,*) '================================================='
  VarName='T'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)

  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
        surftemp(:,:)=field3(:,1,:) + 300.0
     else
        surftemp(:,:)=field3(:,:,1) + 300.0
     endif
  end if
  write(6,*)' max,min temp=',maxval(surftemp),minval(surftemp)

  write(6,*) '================================================='
  VarName='P'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
        precip(:,:)=field3(:,1,:)
     else
        precip(:,:)=field3(:,:,1)
     endif
  end if

  write(6,*) '================================================='
  VarName='PB'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
        precip(:,:)=precip(:,:)+field3(:,1,:)
     else
        precip(:,:)=precip(:,:)+field3(:,:,1) 
     endif
  end if
  surftemp=surftemp*(precip/P0)**RCP
  write(6,*)' max,min surface pressure =',maxval(precip),minval(precip)
  write(6,*)' max,min surface temp (K)=',maxval(surftemp),minval(surftemp)
endif
!
!  write(6,*) '================================================='
!  rmse_var='TSK'
!  surftemp=field2(:,:)
!  write(6,*)' max,min surface temp (K)=',maxval(surftemp),minval(surftemp)
!
  write(6,*) '================================================='
  precip=0
  VarName='QRAIN'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
        precip(:,:)=field3(:,1,:) 
     else
        precip(:,:)=field3(:,:,1)
     endif
  end if

  DO k=1,nsig_regional
    write(6,*)' max,min QRAIN=',k, maxval(field3(:,k,:)),minval(field3(:,k,:))
  ENDDO
  write(6,*) '================================================='
  VarName='QSNOW'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
        precip(:,:)=precip(:,:)+field3(:,1,:) 
     else
        precip(:,:)=precip(:,:)+field3(:,:,1) 
     endif
  end if
  DO k=1,nsig_regional
    write(6,*)' max,min QSNOW=',k, maxval(field3(:,k,:)),minval(field3(:,k,:))
  ENDDO

  write(6,*) '================================================='
  VarName='QGRAUP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     if(trim(memoryorder_all(index))=='XZY') then
        precip(:,:)=precip(:,:)+field3(:,1,:) 
     else
        precip(:,:)=precip(:,:)+field3(:,:,1) 
     endif
  end if
  DO k=1,nsig_regional
    write(6,*)' max,min QGRAUP=',k, maxval(field3(:,k,:)),minval(field3(:,k,:))
  ENDDO
  write(6,*)' max,min precip=',maxval(precip(:,:)),minval(precip(:,:))

  write(6,*) '================================================='
  VarName='SNOW'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     snow=field2                
  end if
  write(6,*)' max,min SNOW=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='SNOWH'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     snowh=field2                  
  end if
  write(6,*)' max,min SNOWH=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='SNOWC'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     snowc=field2                  
  end if
  write(6,*)' max,min SNOWC=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='SEAICE'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     seaice=field2                  
  end if
  write(6,*)' max,min SEAICE=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='SMOIS'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  deallocate(field3)
  if(trim(memoryorder_all(index))=='XZY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(3,index)
     nsig_regional=domainend_all(2,index)
     allocate(field3(nlon_regional,nsig_regional,nlat_regional))
  else if(trim(memoryorder_all(index))=='XYZ') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else if(trim(memoryorder_all(index))=='XY') then
     nlon_regional=domainend_all(1,index)
     nlat_regional=domainend_all(2,index)
     nsig_regional=domainend_all(3,index)
     allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  else
     write(6,*) ' No such memory order ',trim(memoryorder_all(index))
     stop 123
  end if

  allocate(ibuf4(hor_size*nsig_regional))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size*nsig_regional,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size*nsig_regional))
  call to_r2i(ibuf4,field3,(hor_size*nsig_regional))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName ! use soil mositure to find water =1 water
     if(trim(memoryorder_all(index))=='XZY') then
        landmask_soilmoisture1(:,:)=field3(:,1,:)
     else
        landmask_soilmoisture1(:,:)=field3(:,:,1)
     endif
  end if

  do k=1,nsig_regional
    write(6,*)' max,min SMOIS=',k, maxval(field3(:,k,:)),minval(field3(:,k,:))
  enddo

  write(6,*) '================================================='
  VarName='XLAND'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     xland_rr=field2                  
  end if
  write(6,*)' max,min XLAND=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='LANDMASK'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     landmask=field2                  
  end if
  write(6,*)' max,min LANDMASK=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='LU_INDEX'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ibuf4,hor_size,mpi_real4,       &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call to_r2i(ibuf4,field2,(hor_size))
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     lu_index=field2                  
  end if
  write(6,*)' max,min LU_INDEX=',maxval(field2),minval(field2)

  write(6,*) '================================================='
  VarName='IVGTYP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ifield2,hor_size,mpi_integer4,  &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ifield2,(hor_size))
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     ivgtyp=ifield2                  
  end if
  write(6,*)' max,min IVGTYP=',maxval(ifield2),minval(ifield2)

  write(6,*) '================================================='
  VarName='ISLTYP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  CALL mpi_file_read_at(iunit,file_offset(index+1),     &
                        ifield2,hor_size,mpi_integer4,  &
                          mpi_status_ignore, ierr)
  if(ifswap) call to_native_endianness_i4(ifield2,(hor_size))
  if (ierr /= 0) then
     print*,"Error reading ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: read in ',VarName
     isltyp=ifield2                  
  end if
  write(6,*)' max,min ISLTYP=',maxval(ifield2),minval(ifield2)
  write(6,*) '================================================='

!
!  trim snow
!
  DO J=1,nlat
  DO I=1,nlon
! xland is the RR land/water mask from the geo* file - no effect from sea ice, =1 for land, 0 - water.
    if(int(xland(i,j)+0.01) == 1  .and. int(seaice(i,j)+0.01) == 0 ) then  ! on land
      if(snowiceRR(i,j) < 1.0e-12 .and. snow(i,j) > 0.0 ) then   ! over forecast snow ?
!tgs may be increase 274iK to 276K? Sometimes 100-200mm of snow trimmed with 274.
      if(precip(i,j) < 1.0e-12 .and. surftemp(i,j) > 276.0 ) then   ! make sure 
        write(6,*) 'trim snow',i,j,snow(i,j),precip(i,j),surftemp(i,j),snowiceRR(i,j) 
        snow(i,j) = 0.0
        snowh(i,j) = 0.0
        snowc(i,j) = 0.0
      endif
      endif
    endif
  ENDDO
  ENDDO

!
!  replace seaice and xland
!
if(1==1) then  ! turn off , use GFS sea ice
  num_seaice2water=0
  num_water2seaice=0
  DO J=1,nlat
  DO I=1,nlon
    if( int(xland(i,j)+0.01) == 0 ) then    ! water
      if(seaice(i,j) >= xice_threshold .and. snowiceRR(i,j) < xice_threshold ) then  ! turn old seaice into water
! For water:
            ivgtyp(i,j)=16
            lu_index(i,j)=16
            landmask(i,j)=0.
            xland_rr(i,j)=2.
            isltyp(i,j)=14.
            seaice(i,j)=snowiceRR(i,j)
            num_seaice2water = num_seaice2water + 1
      elseif(seaice(i,j) < xice_threshold .and. snowiceRR(i,j) >= xice_threshold ) then  ! turn old water into seaice
! for sea ice
             ivgtyp(i,j)=24    
             lu_index(i,j)=24 
             landmask(i,j)=1.   
             xland_rr(i,j)=1.     
             isltyp(i,j)=16. 
             seaice(i,j)=snowiceRR(i,j)
             num_water2seaice=num_water2seaice+1
!       else
!             seaice(i,j)=snowiceRR(i,j)
      endif
    endif
  ENDDO
  ENDDO
  write(*,*) 'SUMMARY on seaice:'
  write(*,*) 'grid point from old seaice into water: ', num_seaice2water
  write(*,*) 'grid point from old water  into seaice: ', num_water2seaice 
endif
!
!  get rid of snow on water
!
  DO J=1,nlat
  DO I=1,nlon
    if( int(xland(i,j)+0.01) == 0 ) then    ! water
      if( seaice(i,j) < 0.001 .and. snow(i,j) > 0.0 ) then  ! snow on water
        snow(i,j) = 0.0
        snowh(i,j) = 0.0
        snowc(i,j) = 0.0
      endif
    endif
  ENDDO
  ENDDO

  call MPI_BARRIER(mpi_comm_world,ierror)
!
!           update mass core binary file with snow,snowh,snowc
!
  write(6,*) ' ================== '
  write(6,*) ' trim snow and replace ice '
  write(6,*) ' ================== '
     
   
  write(6,*) '================================================='
  field2=seaice
  VarName='SEAICE'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  field2=snowc
  VarName='SNOWC'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  field2=snowh
  VarName='SNOWH'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  field2=snow
  VarName='SNOW'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  field2=landmask
  VarName='LANDMASK'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  field2=xland_rr
  VarName='XLAND'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  field2=lu_index
  VarName='LU_INDEX'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  allocate(ibuf4(hor_size))
  call to_i2r(ibuf4,field2,(hor_size))
  if(ifswap) call to_native_endianness_i4(ibuf4,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ibuf4,  &
            hor_size,mpi_real4,mpi_status_ignore, ierr)
  deallocate(ibuf4)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  write(6,*) '================================================='
  ifield2=isltyp
  VarName='ISLTYP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  if(ifswap) call to_native_endianness_i4(ifield2,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ifield2,  &
            hor_size,mpi_integer4,mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(ifield2),minval(ifield2)
  end if

  write(6,*) '================================================='
  ifield2=ivgtyp
  VarName='IVGTYP'
  call retrieve_index(index,VarName,varname_all,nrecs)
  if(index < 0) then
      print*,VarName," not found in background file"
      stop 1234
  endif
  if(ifswap) call to_native_endianness_i4(ifield2,(hor_size))
  call mpi_file_write_at(iunit,file_offset(index+1),ifield2,  &
            hor_size,mpi_integer4,mpi_status_ignore, ierr)
  if (ierr /= 0) then
     print*,"Error writing ", VarName," using MPIIO"
  else
     write(6,*)' MPIIO: write out ',VarName
     write(6,*)' max,min=',maxval(ifield2),minval(ifield2)
  end if

  deallocate(field2)
  deallocate(ifield2)
  deallocate(field3)
  
  call mpi_file_close(iunit,ierror)
  call MPI_FINALIZE(ierror)

end subroutine update_SNOWICE_binary_mass
