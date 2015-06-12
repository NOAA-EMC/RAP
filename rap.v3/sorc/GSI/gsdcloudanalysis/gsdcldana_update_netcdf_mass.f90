subroutine gsdcldana_update_netcdf_mass
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    update_netcdf_mass  create netcdf format wrf restart file from internal binary file.
!   prgmmr:
!
! abstract: create netcdf format wrf restart file from internal binary file
!
! program history log:
!   2004-11-05  treadon - add return code 75 for error stop
!   2004-12-15  treadon - remove get_lun, read guess from file "wrf_inout"
!   2005-12-09  middlecoff - initialize character variable staggering and removed staggering1,staggering2
!   2006-04-06  middlecoff - added read of SM and SICE to match the writes in wrwrfmass.F90  
!                            and read in the rest of the fields to match the writes in wrwrfmass.F90  
!   2006-06-09  liu - bug fix: replace SM and SICE with SMOIS and XICE
!   2009-08-14  lueken - update documentation
!   2010-03-29  Hu  - add code to update 5 cloud/hydrometeor variables for cloud analysis
!   2008-03-29  Hu  - bug fix: replace XICE with SEAICE and 
!                              comment out update for SMOIS (the actually 
!                              variable is Landmask there).
!   2012-01-09  Hu  - add code to update START_TIME to analysis time
!   2012-04-13  Whitaker - clip positive definite quantities to tiny_single
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single,i_kind,r_kind
  use constants, only: h300,tiny_single
  use rapidrefresh_cldsurf_mod, only: l_cloud_analysis,l_gsd_soilTQ_nudge
  use gsi_metguess_mod, only: gsi_metguess_get,GSI_MetGuess_Bundle
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use guess_grids, only: ntguessig
  use obsmod, only: iadate
!  use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type

  implicit none

  include 'netcdf.inc'

! Declare local parameters

  character(len=120) :: flnm1,flnm2
  character(len=19)  :: DateStr1
  integer(i_kind)            :: dh1,iw3jdn

  integer(i_kind) :: iunit

  integer(i_kind) :: i,j,k
  integer(i_kind) :: ndim1
  integer(i_kind) :: WrfType
  integer(i_kind), dimension(4)  :: start_index, end_index1
  character (len= 4) :: staggering=' N/A'
  character (len= 3) :: ordering

  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo


  integer(i_kind) :: it, nguess, ierr, istatus, Status, Status_next_time
  real(r_kind), pointer :: ges_qc(:,:,:)
  real(r_kind), pointer :: ges_qi(:,:,:)
  real(r_kind), pointer :: ges_qr(:,:,:)
  real(r_kind), pointer :: ges_qs(:,:,:)
  real(r_kind), pointer :: ges_qg(:,:,:)

! binary stuff

! rmse stuff

  character (len=31) :: rmse_var

  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional,nsig_soil_regional
  real(r_single) pt_regional,pdtop_regional,dy_nmm
  real(r_single),allocatable::field3(:,:,:),field2(:,:),field1(:),field2b(:,:)
  real(r_single),allocatable::field3u(:,:,:),field3v(:,:,:)
  integer(i_kind),allocatable::ifield2(:,:)
  integer(i_kind) wrf_real
  data iunit / 15 /
  wrf_real=104
  end_index1=0

! Inquire about guess fields
  call gsi_metguess_get('dim',nguess,ierr)
  if (nguess>0) then
!    get pointer to relevant instance of cloud-related backgroud
     it=ntguessig
     ierr=0
     call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'ql', ges_qc, istatus );ierr=ierr+istatus
     call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qi', ges_qi, istatus );ierr=ierr+istatus
     call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qr', ges_qr, istatus );ierr=ierr+istatus
     call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qs', ges_qs, istatus );ierr=ierr+istatus
     call GSI_BundleGetPointer ( GSI_MetGuess_Bundle(it), 'qg', ges_qg, istatus );ierr=ierr+istatus
     if (ierr/=0) nguess=0
  end if

! transfer code from diffwrf for converting netcdf wrf nmm restart file
! to temporary binary format

!  if( i_gsdcldanal_type==6) call ext_ncd_ioinit(sysdepinfo,status)
   call ext_ncd_ioinit(sysdepinfo,status)
!
!           update mass core netcdf file with analysis variables from 3dvar
!
  flnm1='wrf_inout'
  call ext_ncd_open_for_update( trim(flnm1), 0, 0, "", dh1, Status)
  if ( Status /= 0 )then
     write(6,*)'UPDATE_NETCDF_MASS:  problem with flnm1 = ',&
          trim(flnm1),', Status = ', Status
     call stop2(75)
  endif
        
  
  close(51)
  flnm2='siganl'
  open(iunit,file=flnm2,form='unformatted')

     
!-------------  get date info

  call ext_ncd_get_next_time(dh1, DateStr1, Status_next_time)
  read(DateStr1,'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  write(6,*)' gsdcldana_update_netcdf_mass'
  write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

!-------------  get grid info
  rmse_var='SMOIS'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
  nlon_regional=end_index1(1)
  nlat_regional=end_index1(2)
  nsig_soil_regional=end_index1(3)

  rmse_var='T'
  call ext_ncd_get_var_info (dh1,rmse_var,ndim1,ordering,staggering, &
                               start_index,end_index1, WrfType, ierr    )
  nlon_regional=end_index1(1)
  nlat_regional=end_index1(2)
  nsig_regional=end_index1(3)
  write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(field2(nlon_regional,nlat_regional))
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))
  allocate(field2b(nlon_regional,nlat_regional))
  allocate(ifield2(nlon_regional,nlat_regional))
  allocate(field1(max(nlon_regional,nlat_regional,nsig_regional)))
  
  rmse_var='P_TOP'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       pt_regional,WRF_REAL,0,0,0,ordering,          &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' p_top=',pt_regional
  read(iunit) ! iyear,imonth,iday,ihour,iminute,isecond, &
!        nlon_regional,nlat_regional,nsig_regional,pt_regional
  
  read(iunit) ! field1(1:nsig_regional)  ! AETA1  (ZNU)
  
  read(iunit) ! field1(1:nsig_regional+1)  !  ETA1 (ZNW)
  
  read(iunit) ! field2   !XLAT,DX_MC
  
  read(iunit) ! field2   !XLONG,DY_MC
  
  rmse_var='MUB'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_read_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  write(6,*)' max,min MUB=',maxval(field2),minval(field2)
  
  read(iunit)   field2b   !psfc
  write(6,*)' max,min psfc=',maxval(field2b),minval(field2b)
  field2b=field2b-field2-pt_regional
  write(6,*)' max,min MU=',maxval(field2b),minval(field2b)
  rmse_var='MU'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2b,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  read(iunit) ! field2   ! PHB (FIS)
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! T
     write(6,*)' k,max,min,mid T=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  field3=field3-h300
  rmse_var='T'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
  do k=1,nsig_regional
     read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! Q
     write(6,*)' k,max,min,mid Q=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
          field3(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='QVAPOR'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  where (field3 < tiny_single) field3 = tiny_single
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  deallocate(field3)

  allocate(field3u(nlon_regional+1,nlat_regional,nsig_regional))
  do k=1,nsig_regional
     read(iunit)((field3u(i,j,k),i=1,nlon_regional+1),j=1,nlat_regional)   ! U
     write(6,*)' k,max,min,mid U=',k,maxval(field3u(:,:,k)),minval(field3u(:,:,k)), &
          field3u(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='U'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3u,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  deallocate(field3u)
  
  allocate(field3v(nlon_regional,nlat_regional+1,nsig_regional))
  do k=1,nsig_regional
     read(iunit)((field3v(i,j,k),i=1,nlon_regional),j=1,nlat_regional+1)   ! V
     write(6,*)' k,max,min,mid V=',k,maxval(field3v(:,:,k)),minval(field3v(:,:,k)), &
          field3v(nlon_regional/2,nlat_regional/2,k)
  end do
  rmse_var='V'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field3v,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  deallocate(field3v)
  
  allocate(field3(nlon_regional,nlat_regional,nsig_regional))

  read(iunit)   field2   !  LANDMASK
  write(6,*)'max,min LANDMASK=',maxval(field2),minval(field2)

  read(iunit)   field2   ! SEAICE
  write(6,*)'max,min SEAICE=',maxval(field2),minval(field2)
  rmse_var='SEAICE'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  read(iunit)   field2   !SST
  write(6,*)' max,min SST=',maxval(field2),minval(field2)
  rmse_var='SST'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )
  
! Read in the rest of the fields
  if(l_gsd_soilTQ_nudge) then
     do k=4,9
        read(iunit) field2 !Rest of the fields
        write(6,*)'read max,min REST',k,maxval(field2),minval(field2)
     end do

     do k=1,nsig_soil_regional
        read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! smois
        write(6,*)' k,max,min,mid SMOIS=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
             field3(nlon_regional/2,nlat_regional/2,k)
     end do
     rmse_var='SMOIS'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
     write(6,*)' rmse_var=',trim(rmse_var)
     write(6,*)' ordering=',ordering
     write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
     write(6,*)' ndim1=',ndim1
     write(6,*)' staggering=',staggering
     write(6,*)' start_index=',start_index
     write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
          field3,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index1,               & !dom
          start_index,end_index1,               & !mem
          start_index,end_index1,               & !pat
          ierr                                 )

     do k=1,nsig_soil_regional
        read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! tslb
        write(6,*)' k,max,min,mid TSLB=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
             field3(nlon_regional/2,nlat_regional/2,k)
     end do
     rmse_var='TSLB'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
     write(6,*)' rmse_var=',trim(rmse_var)
     write(6,*)' ordering=',ordering
     write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
     write(6,*)' ndim1=',ndim1
     write(6,*)' staggering=',staggering
     write(6,*)' start_index=',start_index
     write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
          field3,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index1,               & !dom
          start_index,end_index1,               & !mem
          start_index,end_index1,               & !pat
          ierr                                 )
  else
     do k=4,11   ! corrected according to Ming Hu's finding

        read(iunit) field2 !Rest of the fields
        write(6,*)'read max,min REST',k,maxval(field2),minval(field2)
     end do
  endif

  read(iunit)   field2   !TSK
  write(6,*)' max,min TSK=',maxval(field2),minval(field2)
  rmse_var='TSK'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  read(iunit)   field2   !Q2
  write(6,*)' max,min Q2=',maxval(field2),minval(field2)
  rmse_var='Q2'
  call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
       start_index,end_index1, WrfType, ierr    )
  write(6,*)' rmse_var=',trim(rmse_var)
  write(6,*)' ordering=',ordering
  write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
  write(6,*)' ndim1=',ndim1
  write(6,*)' staggering=',staggering
  write(6,*)' start_index=',start_index
  write(6,*)' end_index1=',end_index1
  call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
       field2,WRF_REAL,0,0,0,ordering,           &
       staggering, dimnames ,               &
       start_index,end_index1,               & !dom
       start_index,end_index1,               & !mem
       start_index,end_index1,               & !pat
       ierr                                 )

  if(l_gsd_soilTQ_nudge) then
     read(iunit)   field2   !SOILT1
     write(6,*)' max,min SOILT1 d=',maxval(field2),minval(field2)
     rmse_var='SOILT1'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
     write(6,*)' rmse_var=',trim(rmse_var)
     write(6,*)' ordering=',ordering
     write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
     write(6,*)' ndim1=',ndim1
     write(6,*)' staggering=',staggering
     write(6,*)' start_index=',start_index
     write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
          field2,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index1,               & !dom
          start_index,end_index1,               & !mem
          start_index,end_index1,               & !pat
          ierr                                 )

     read(iunit)   field2   !TH2
     write(6,*)' max,min TH2 d=',maxval(field2),minval(field2)
     rmse_var='TH2'
     call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
          start_index,end_index1, WrfType, ierr    )
     write(6,*)' rmse_var=',trim(rmse_var)
     write(6,*)' ordering=',ordering
     write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
     write(6,*)' ndim1=',ndim1
     write(6,*)' staggering=',staggering
     write(6,*)' start_index=',start_index
     write(6,*)' end_index1=',end_index1
     call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
          field2,WRF_REAL,0,0,0,ordering,           &
          staggering, dimnames ,               &
          start_index,end_index1,               & !dom
          start_index,end_index1,               & !mem
          start_index,end_index1,               & !pat
          ierr                                 )
  endif

  if (l_cloud_analysis .or. nguess>0) then
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qc
       write(6,*)' k,max,min,mid Qc=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QCLOUD'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qr
       write(6,*)' k,max,min,mid Qr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QRAIN'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qs
       write(6,*)' k,max,min,mid Qs=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QSNOW'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qi
       write(6,*)' k,max,min,mid Qi=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QICE'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qg
       write(6,*)' k,max,min,mid Qg=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QGRAUP'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   !  Qnr
       write(6,*)' k,max,min,mid Qnr=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &                                                      
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='QNRAIN'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    where (field3 < tiny_single) field3 = tiny_single
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )

! skip TTEN
if(1==2) then
    do k=1,nsig_regional
       read(iunit)((field3(i,j,k),i=1,nlon_regional),j=1,nlat_regional)   ! TTEN 
       write(6,*)' k,max,min,mid TTEN=',k,maxval(field3(:,:,k)),minval(field3(:,:,k)), &
            field3(nlon_regional/2,nlat_regional/2,k)
    end do
    rmse_var='RAD_TTEN_DFI'
    call ext_ncd_get_var_info (dh1,trim(rmse_var),ndim1,ordering,staggering, &
         start_index,end_index1, WrfType, ierr    )
    write(6,*)' rmse_var=',trim(rmse_var)
    write(6,*)' ordering=',ordering
    write(6,*)' WrfType,WRF_REAL=',WrfType,WRF_REAL
    write(6,*)' ndim1=',ndim1
    write(6,*)' staggering=',staggering
    write(6,*)' start_index=',start_index
    write(6,*)' end_index1=',end_index1
    call ext_ncd_write_field(dh1,DateStr1,TRIM(rmse_var),              &
         field3,WRF_REAL,0,0,0,ordering,           &
         staggering, dimnames ,               &
         start_index,end_index1,               & !dom
         start_index,end_index1,               & !mem
         start_index,end_index1,               & !pat
         ierr                                 )
endif

  endif     ! l_cloud_analysis

  deallocate(field1,field2,field2b,ifield2,field3)
  call ext_ncd_ioclose(dh1, Status)
  close(iunit)
  !
  !  reopen, update global attributes.
  !
  ierr = NF_OPEN(trim(flnm1), NF_WRITE, dh1)
  IF (ierr .NE. NF_NOERR) print *, 'OPEN ',NF_STRERROR(ierr)
  ierr = NF_PUT_ATT_TEXT(dh1,NF_GLOBAL,'START_DATE',len_trim(DateStr1),DateStr1)
  IF (ierr .NE. NF_NOERR) print *,'PUT START_DATE', NF_STRERROR(ierr)
  ierr = NF_PUT_ATT_TEXT(dh1,NF_GLOBAL,'SIMULATION_START_DATE',len_trim(DateStr1),DateStr1)
  IF (ierr .NE. NF_NOERR) print *,'PUT SIMULATION_START_DATE', NF_STRERROR(ierr)
  ierr = NF_PUT_ATT_REAL(dh1,NF_GLOBAL,'GMT',NF_FLOAT,1,float(iadate(4)))
  IF (ierr .NE. NF_NOERR) print *,'PUT GMT', NF_STRERROR(ierr)
  ierr = NF_PUT_ATT_INT(dh1,NF_GLOBAL,'JULYR',NF_INT,1,iadate(1))
  IF (ierr .NE. NF_NOERR) print *,'PUT JULYR', NF_STRERROR(ierr)
  ierr=NF_PUT_ATT_INT(dh1,NF_GLOBAL,'JULDAY',NF_INT,1,iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1)
  IF (ierr .NE. NF_NOERR) print *,'PUT JULDAY', NF_STRERROR(ierr)
  ierr = NF_CLOSE(dh1)
  IF (ierr .NE. NF_NOERR) print *, 'CLOSE ',NF_STRERROR(ierr)
  
end subroutine gsdcldana_update_netcdf_mass

