program initial_arw_ens 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    initial_arw_ens 
!   prgmmr: Hu         org: GSD                date: 2015-03-24
!
! abstract: read pertubations on ARW A grid and generate initial files for ARW
!            ensembles
!
!
! program history log:
!   2015-03-23  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  zeus
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_single
  use constants, only : max_varname_length,half
  implicit none
  INCLUDE 'netcdf.inc'

  integer :: n_ens
  integer :: nlon,nlat,nsig,num_fields
  integer(i_kind) :: nc2d,nc3d
  character(len=max_varname_length),allocatable,dimension(:) :: cvars2d 
  character(len=max_varname_length),allocatable,dimension(:) :: cvars3d  

  real(r_single),allocatable,dimension(:,:,:)::en_perts
  real(r_kind),dimension(:,:),allocatable:: workh

  real(r_single),allocatable,dimension(:,:,:):: field3

! Declare netcdf parameters

  character(len=120) :: flnm1
  character(len=120) :: flnm_new
  character(len=19)  :: DateStr1
  integer(i_kind)    :: dh1
  integer(i_kind)    :: dh2

  integer(i_kind) :: Status, Status_next_time
  integer(i_kind) :: iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) :: iw3jdn,JDATE(8),IDATE(8)
  real(r_single) :: rinc(5), timediff

  character (len=80) :: SysDepInfo
  character (len=31) :: rmse_var
!
!
!
  integer(i_kind) i,j,k,n
  integer(i_kind) ic2,ic3
  character(255) filename
  integer(i_kind),dimension(4):: idate4
  integer(i_kind) im,i0

  integer(i_kind) :: iunit
  character*100   :: BUF
!
!
!
  n_ens=0
  call getarg(1,BUF)
  read(BUF,'(I5)') n_ens
  if(n_ens > 0) then
     write(*,*) 'the ensemble member number==',n_ens
  else
     write(*,*) 'wrong ensemble member number==',n_ens
     stop
  endif
!
!           open netcdf file to read
  call ext_ncd_ioinit(sysdepinfo,status)
!
  flnm1='wrf_inout'
  call ext_ncd_open_for_read( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'save_soil_netcdf_mass:  cannot open flnm1 = ',&
          trim(flnm1),', Status = ', Status
     stop 74
  endif
!
!-------------  get date info  from file read in

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)')   &
                       iyear,imonth,iday,ihour,iminute,isecond
  write(6,'(a,6I5)')' read data from file at time (y,m,d,h,m,s):'    &
                       ,iyear,imonth,iday,ihour,iminute,isecond
!
!   get dimensions
  iunit=20
  write(filename,'(a,I4.4)') 'en_perts4arw.mem',1
  write(*,*) 'read dimension from ', trim(filename)
  open(iunit,file=trim(filename),form='unformatted')
     read(iunit) nc3d,nc2d
     allocate(cvars3d(nc3d),cvars2d(nc2d))
     rewind(iunit)
     read(iunit) nc3d,nc2d,cvars3d,cvars2d
     read(iunit) nlat,nlon,nsig
  close(iunit)
  write(*,*) 'nlat,nlon,nsig=',nlat,nlon,nsig
  write(*,'(I5,A10,10A6)') nc3d,'cvars3d=',(trim(cvars3d(ic3)),ic3=1,nc3d)
  write(*,'(I5,A10,10A6)') nc2d,'cvars2d=',(trim(cvars2d(ic2)),ic2=1,nc2d)

  num_fields=nc3d*nsig+nc2d
  allocate(workh(nlat,nlon))
  allocate(en_perts(nlat,nlon,num_fields))

!
!  read perturbations
!
  do n=1,n_ens

     write(filename,'(a,I4.4)') 'en_perts4arw.mem',n
     write(*,*) 
     write(*,*) 'read perturbations for ', trim(filename)
     open(iunit,file=trim(filename),form='unformatted')
        read(iunit) 
        read(iunit)

        do k=1,num_fields

            read(iunit) workh
!            write(*,*) k,maxval(workh),minval(workh)
            do j=1,nlon
               do i=1,nlat
                  en_perts(i,j,k)=workh(i,j)
               end do
            end do

        end do

     close(iunit)

     write(flnm_new,'(a,I4.4)') 'wrfinput_d01.mem',n
     call ext_ncd_open_for_update( trim(flnm_new), 0, 0, "", dh2, Status)
     if ( Status /= 0 )then
        write(6,*)'gen_initial_ensemble:  cannot open flnm = ',&
             trim(flnm_new),', Status = ', Status
        stop 74
     endif

     rmse_var='T' 
     allocate(field3(nlon,nlat,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,nsig)
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              field3(j,i,k)=field3(j,i,k)+en_perts(i,j,k+2*nsig)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat,nsig)
     deallocate(field3)

     rmse_var='U' 
     allocate(field3(nlon+1,nlat,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon+1,nlat,nsig)
     do k=1,nsig
        do j=1,nlon+1
           do i=1,nlat
              im=max(1,j-1)
              i0=min(nlon,j)
              field3(j,i,k)=field3(j,i,k)+half*(en_perts(i,im,k)+en_perts(i,i0,k))
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon+1,nlat,nsig)
     deallocate(field3)

     rmse_var='V' 
     allocate(field3(nlon,nlat+1,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat+1,nsig)
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat+1
              im=max(1,i-1)
              i0=min(nlon,i)
              field3(j,i,k)=field3(j,i,k)+half*(en_perts(im,j,k)+en_perts(i0,j,k))
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat+1,nsig)
     deallocate(field3)

     rmse_var='QVAPOR' 
     allocate(field3(nlon,nlat,nsig))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,nsig)
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              field3(j,i,k)=field3(j,i,k)+en_perts(i,j,k+3*nsig)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat,nsig)
     deallocate(field3)

     rmse_var='MU' 
     allocate(field3(nlon,nlat,1))
     call read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,1)
     do k=1,1
        do j=1,nlon
           do i=1,nlat
              field3(j,i,k)=field3(j,i,k)+en_perts(i,j,k+6*nsig)
           end do
        end do
     end do
     call update_netcdf_mass(dh2,DateStr1,rmse_var,field3,nlon,nlat,1)
     deallocate(field3)

     call ext_ncd_ioclose(dh2, Status)
  enddo ! n

  deallocate(workh)
!
!     do ic3=1,nc3d
!
!        select case (trim(cvars3d(ic3)))
!
!           case('sf','SF')
!
!                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
!
!           case('vp','VP')
!
!                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
!
!           case('t','T')
!
!                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
!           case('q','Q')
!
!!                       en_perts(n,i,j,(ic3-1)*grd_ens%nsig+k)=w3(i,j,k)
!        end select
!     end do
!     do ic2=1,nc2d
!
!        select case (trim(cvars2d(ic2)))
!
!!           case('ps','PS')
!
!                    en_perts(n,i,j,nc3d*grd_ens%nsig+ic2)=w2(i,j)
!        end select
!     end do
!  end do
  call ext_ncd_ioclose(dh1, Status)

end program initial_arw_ens

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

