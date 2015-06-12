subroutine read_netcdf_mass(dh1,DateStr1,rmse_var,field3,nlon,nlat,nsig)
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
!      DateStr1 : time string of file read in 
!      rmse_var :  variable updated
!
! attributes:
!   language: f90
!
!$$$

  use kinds, only: r_single,i_kind, r_kind
  implicit none

!
  integer(i_kind),   intent(in)  :: dh1
  character (len=31),intent(in)  :: rmse_var
  character(len=19), intent(in)  :: DateStr1
  integer(i_kind),   intent(in)  ::  nlon,nlat,nsig
  real(r_single),    intent(out) :: field3(nlon,nlat,nsig)
  
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
  
  integer(i_kind) :: k
  integer(i_kind) :: ierr
!
!  
!
  write(6,*) 
  write(6,*) ' ================== '
  write(6,*) ' Read variable ', trim(rmse_var)
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
  if(ndim1 == 2) nsig_regional=1
  if( nlon_regional /= nlon .or. &
      nlat_regional /= nlat .or. &
      nsig_regional /= nsig) then

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

!  do k=1,nsig_regional
!      write(6,*)' max,min =',k,maxval(field3(:,:,k)),minval(field3(:,:,k))
!  enddo
  
end subroutine read_netcdf_mass

