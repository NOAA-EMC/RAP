SUBROUTINE GET_RR_GRID(ifswap,mype,xlon,ylat,nlon,nlat,xland,vegtyp)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT:
!     This routine read in Rapid Refresh grid and land mask
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT:
!
!   OUTPUT:
!      xlon:  longitude in each grid
!      ylat:  latitude in each grid
!      xland: land mask
!
!   INPUT FILES:  imssnow
!
!   OUTPUT FILES:  RRimssnow
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  wJET
!
!$$$
!
!_____________________________________________________________________


!
  use kinds, only: r_single,i_llong,i_kind
  implicit none
!

  logical,intent(in) :: ifswap
!  grid
  integer :: mype
  integer, intent(in) :: nlon,nlat
  real, intent(out):: xlon(nlon,nlat)    !
  real, intent(out):: ylat(nlon,nlat)    !
  real, intent(out):: xland(nlon,nlat)    !
  real, intent(out):: vegtyp(nlon,nlat)    !
!
  integer :: tnlon,tnlat
!
! MPI variables
!  integer :: npe, mype, mypeLocal,ierror

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
!!
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional

  integer :: i,j,k,n,hor_size
  INTEGER :: istatus,iret,index,ierr
  real(r_single),allocatable:: field2(:,:)

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup

!!! MPI IO

  iunit=33
  fileName='geo_em.d1'
  open(iunit,file=trim(fileName),form='unformatted')
! Check for valid input file
  read(iunit,iostat=status_hdr)hdrbuf
  if(status_hdr /= 0) then
     write(6,*)'CONVERT_BINARY_MASS:  problem with wrfges = ',&
          trim(fileName),', Status = ',status_hdr
     call stop2(74)
  endif
  close(iunit)

  call count_recs_wrf_binary_file(iunit, ifswap,trim(fileName), nrecs)
  if(mype==0)      write(*,*) 'nrecs: ', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, ifswap,trim(filename), nrecs,            &
                 datestr_all,varname_all,memoryorder_all,domainend_all,   &
                 start_block,end_block,start_byte,end_byte,file_offset)

!  do N=1,NRECS
!     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),      &
!           trim(memoryorder_all(n)),domainend_all(:,n)
!  enddo

  call retrieve_index(index,'LANDMASK',varname_all,nrecs)
  if(index<0) then
      print*," can not found LANDMASK in geo file"
      stop 1234
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

  if( (nlon_regional/=nlon) .or. (nlat_regional/=nlat) ) then
      write(6,*) 'Dimensions do not match between input and geo file'
      write(6,*) 'input=',nlon,nlat              
      write(6,*) 'geo=',nlon_regional,nlat_regional    
      stop 234
  endif

  allocate(field2(nlon_regional,nlat_regional))
!
  call retrieve_field(iunit,ifswap,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))
  xland=field2
  if(mype==0) write(6,*)' MPIIO: landmask, max, min=', maxval(xland),minval(xland)

!                  LU_INDEX
  call retrieve_index(index,'LU_INDEX',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,ifswap,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))

  vegtyp=field2
!                  XLAT
  call retrieve_index(index,'XLAT_M',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,ifswap,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))

  ylat=field2
  if(mype==0) write(6,*)' MPIIO: max,min XLAT(:,1)=',&
       maxval(ylat(:,1)),minval(ylat(:,1))
  if(mype==0) write(6,*)' MPIIO: max,min XLAT(1,:)=',&
       maxval(ylat(1,:)),minval(ylat(1,:))
  if(mype==0) write(6,*)' MPIIO: xlat(1,1),xlat(nlon,1)=',&
       ylat(1,1),ylat(nlon_regional,1)
  if(mype==0) write(6,*)' MPIIO: xlat(1,nlat),xlat(nlon,nlat)=', &
       ylat(1,nlat_regional),ylat(nlon_regional,nlat_regional)

!                  XLONG
  call retrieve_index(index,'XLONG_M',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,ifswap,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))
  xlon=field2
  if(mype==0) write(6,*)' MPIIO: max,min XLONG(:,1)=',&
       maxval(xlon(:,1)),minval(xlon(:,1))
  if(mype==0) write(6,*)' MPIIO: max,min XLONG(1,:)=',&
       maxval(xlon(1,:)),minval(xlon(1,:))
  if(mype==0) write(6,*)' MPIIO: xlong(1,1),xlong(nlon,1)=',&
       xlon(1,1),xlon(nlon_regional,1)
  if(mype==0) write(6,*)' MPIIO: xlong(1,nlat),xlong(nlon,nlat)=', &
       xlon(1,nlat_regional),xlon(nlon_regional,nlat_regional)
!
  close(iunit)
  deallocate(field2)

!
END SUBROUTINE GET_RR_GRID
