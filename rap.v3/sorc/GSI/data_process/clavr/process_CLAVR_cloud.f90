module share

  use kinds, only: r_kind,i_kind,r_single

  real,  allocatable ::   lat_l(:,:)
  real,  allocatable ::   lon_l(:,:)
  real,  allocatable ::   ptop_l(:,:)
  real,  allocatable ::   lwp_l(:,:)
  real,  allocatable ::   teff_l(:,:)
  real,  allocatable ::   hgt_l(:,:)
  real,  allocatable ::   emiss_l(:,:)
  integer(i_kind), allocatable ::   phase_l(:,:)

contains

  subroutine read_CLAVR_cloud(satfile,missing_val,nx,ny,mask,ctp,ctt,cth,emiss,lwp,lat,lon)
    use netcdf
    
    implicit none
    
    character(len=*),                   intent(in)  :: satfile
    real,                               intent(in)  :: missing_val
    integer,                            intent(out) :: nx,ny
    real,   allocatable, dimension(:,:),intent(out) :: ctp, ctt, cth, lwp, emiss, lat, lon
    integer,allocatable, dimension(:,:),intent(out) :: mask
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    integer, dimension(50)              :: status
    real,allocatable, dimension(:,:)    :: tmp
    integer                             :: xDimID, yDimID
    integer                             :: ncid
    integer                             :: maskVarID, ctpVarID, cttVarID, cthVarID, &
                                           lwpVarID, latVarID, lonVarID, emissVarID
    real                                :: scale_fac, offset, fill_val
    
    ! Open file and read precip field
    status(:)  = nf90_NoErr
    status(1)  = nf90_open(trim(satfile),nf90_nowrite,ncid)

    ! Get dimensions
    status(2)  = nf90_inq_dimid(ncid,"pixel_elements_along_scan_direction",xDimID)
    status(3)  = nf90_inquire_dimension(ncid,xDimID,len=nx)
    status(4)  = nf90_inq_dimid(ncid,"scan_lines_along_track_direction",yDimID)
    status(5)  = nf90_inquire_dimension(ncid,yDimID,len=ny)
    print *, 'dim lens: ', nx,ny

    ! Allocate Arrays
    allocate(mask(nx,ny),tmp(nx,ny),ctp(nx,ny),ctt(nx,ny),cth(nx,ny),emiss(nx,ny),lwp(nx,ny),lat(nx,ny),lon(nx,ny))

    ! Get Cloud Mask
    status(6)  = nf90_inq_varid(ncid,'cloud_mask',maskVarID)
    status(7)  = nf90_get_var(ncid,maskVarID,mask)

    ! Get Latitudes
    status(8)  = nf90_inq_varid(ncid,'latitude',latVarID)
    status(9)  = nf90_get_var(ncid,latVarID,lat)
    status(10) = nf90_get_att(ncid,latVarID,"scale_factor",scale_fac)
    status(11) = nf90_get_att(ncid,latVarID,"add_offset",offset)
    status(12) = nf90_get_att(ncid,latVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(lat .ne. fill_val) tmp = (lat * scale_fac) + offset
    lat = tmp

    ! Get Longitudes
    status(13) = nf90_inq_varid(ncid,'longitude',lonVarID)
    status(14) = nf90_get_var(ncid,lonVarID,lon)
    status(15) = nf90_get_att(ncid,lonVarID,"scale_factor",scale_fac)
    status(16) = nf90_get_att(ncid,lonVarID,"add_offset",offset)
    status(17) = nf90_get_att(ncid,lonVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(lon .ne. fill_val) tmp = (lon * scale_fac) + offset
    lon = tmp
    
    ! Get Cloud Top Pressure
    status(18) = nf90_inq_varid(ncid,'cld_press_acha',ctpVarID)
    status(19) = nf90_get_var(ncid,ctpVarID,ctp)
    status(20) = nf90_get_att(ncid,ctpVarID,"scale_factor",scale_fac)
    status(21) = nf90_get_att(ncid,ctpVarID,"add_offset",offset)
    status(22) = nf90_get_att(ncid,ctpVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(ctp .eq. fill_val .and. mask .ge. 0) tmp = 1013
    where(ctp .ne. fill_val) tmp = (ctp * scale_fac) + offset
    ctp = tmp

    ! Get Cloud Top Temperature
    status(23) = nf90_inq_varid(ncid,'cld_temp_acha',cttVarID)
    status(24) = nf90_get_var(ncid,cttVarID,ctt)
    status(25) = nf90_get_att(ncid,cttVarID,"scale_factor",scale_fac)
    status(26) = nf90_get_att(ncid,cttVarID,"add_offset",offset)
    status(27) = nf90_get_att(ncid,cttVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(ctt .eq. fill_val .and. mask .ge. 0) tmp = 330 
    where(ctt .ne. fill_val) tmp = (ctt * scale_fac) + offset
    ctt = tmp
    
    ! Get Cloud Top Height
    status(28) = nf90_inq_varid(ncid,'cld_height_top_acha',cthVarID)
    status(29) = nf90_get_var(ncid,cthVarID,cth)
    status(30) = nf90_get_att(ncid,cthVarID,"scale_factor",scale_fac)
    status(31) = nf90_get_att(ncid,cthVarID,"add_offset",offset)
    status(32) = nf90_get_att(ncid,cthVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(cth .eq. fill_val .and. mask .ge. 0) tmp = 0
    where(cth .ne. fill_val) tmp = (cth * scale_fac) + offset
    cth = tmp
 
    ! Get Liquid Water Path
    status(33) = nf90_inq_varid(ncid,'cloud_water_path',lwpVarID)  !
    status(34) = nf90_get_var(ncid,lwpVarID,lwp)
    status(35) = nf90_get_att(ncid,lwpVarID,"scale_factor",scale_fac)
    status(36) = nf90_get_att(ncid,lwpVarID,"add_offset",offset)
    status(37) = nf90_get_att(ncid,lwpVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(lwp .eq. fill_val .and. mask .ge. 0) tmp = 0
    where(lwp .ne. fill_val) tmp = (lwp * scale_fac) + offset
    lwp = tmp
    
    ! Get Cloud Emissivity
    status(38) = nf90_inq_varid(ncid,'cld_emiss_acha',emissVarID)
    status(39) = nf90_get_var(ncid,emissVarID,emiss)
    status(40) = nf90_get_att(ncid,emissVarID,"scale_factor",scale_fac)
    status(41) = nf90_get_att(ncid,emissVarID,"add_offset",offset)
    status(42) = nf90_get_att(ncid,emissVarID,"_FillValue",fill_val)
    tmp = missing_val
    where(emiss .eq. fill_val .and. mask .ge. 0) tmp = 0
    where(emiss .ne. fill_val) tmp = (emiss * scale_fac) + offset
    emiss = tmp

    print*, status(:42)
    if(any(status(:42) /= nf90_NoErr)) stop "Error reading NetCDF file"
    
    status(1) = nf90_close(ncid)
    
    print *, 'Done reading CLAVR file'
    
  end subroutine read_CLAVR_cloud

end module share

program  process_CLAVR_cloud
!
!   PRGMMR: Patrick Hofmann             ORG: NOAA/ESRL/GSD/AMB
!     DATE: 10/17/2012
!
! ABSTRACT: 
!     This routine reads in CLAVR-NESDIS cloud products and 
!     interpolates them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 + EXTENSIONS
!   MACHINE:  nJET
!
!$$$
!
!_____________________________________________________________________
!
  use share
  use kinds, only: r_kind,i_kind,r_single
  use map_utils
  use misc_definitions_module , only : PROJ_LC, PROJ_ROTLL
  use constants_module ,only : EARTH_RADIUS_M
  use constants, only: init_constants_derived, deg2rad
  use gridmod_gsimap ,only : nlon,nlat,init_general_transform,tll2xy,txy2ll

  implicit none
!
  INCLUDE 'netcdf.inc'
!
  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file, file
!
!  grid
!  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  real(r_kind),allocatable:: rxlon(:,:)    !
  real(r_kind),allocatable:: rylat(:,:)    !

  real ::  userDX, userDY, CEN_LAT, CEN_LON
  real ::  userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON
  integer :: MAP_PROJ

  type (proj_info) :: proj_stack
  real :: truelat1, truelat2, stdlon, lat1, lon1, r_earth
  real :: knowni, knownj, dx
  real :: user_known_x,user_known_y

  CHARACTER*180   geofile
!
!  For NASA LaRC 
!
  CHARACTER*180   workPath
  CHARACTER*80   satfile
  INTEGER ::   nxp, nyp  ! dimension
  
!     ****VARIABLES FOR THIS NETCDF FILE****
!
  CHARACTER*24 :: cbase_time
  INTEGER(i_kind) ::  base_time
  INTEGER(i_kind) ::  ibase_year,ibase_month,ibase_day,ibase_hour,ihour
  INTEGER(i_kind) ::  icycle_year,icycle_month,icycle_day,icycle_hour
  real*8      time_offset
  !real(r_single), allocatable ::   lat_l(:,:)
  !real(r_single), allocatable ::   lon_l(:,:)
  !real(r_single), allocatable ::   ptop_l(:,:)
  !integer(i_kind), allocatable ::   phase_l(:,:)
!
!  array for RR
!
  real(r_single), allocatable ::   w_pcld(:,:)
  real(r_single), allocatable ::   w_eca(:,:)
  real(r_single), allocatable ::   w_tcld(:,:)
  real(r_single), allocatable ::   w_frac(:,:)
  real(r_single), allocatable ::   w_hgt(:,:)
  real(r_single), allocatable ::   w_lwp (:,:)
  integer(i_kind),allocatable ::   nlev_cld(:,:)

!
! Working
  integer  nfov
  parameter (nfov=160)
  real, allocatable ::     Pxx(:,:,:),Txx(:,:,:),Hxx(:,:,:),Exx(:,:,:),WPxx(:,:,:)
  real, pointer, dimension(:,:) :: test
  real,allocatable  ::     xdist(:,:,:), xxxdist(:)
  integer,allocatable  ::  PHxx(:,:,:),index(:,:), jndex(:)
  real     fr,sqrt, qc, type
  integer  ioption
  integer  ixx,ii,jj,med_pt,igrid,jgrid  &
               ,ncount,ncount1,ncount2,ii1,jj1,nobs,n

!
!
!  ** misc
      
  real(r_kind)        :: xc  ! x-grid coordinate (grid units)
  real(r_kind)        :: yc  ! y-grid coordinate (grid units)
  real(r_kind)        :: rlon  ! earth longitude (radians)
  real(r_kind)        :: rlat  ! earth latitude  (radians)

  real,parameter :: missing_val = -999

  logical     ::outside     ! .false., then point is inside x-y domain
                            ! .true.,  then point is outside x-y domain

  integer i,j,k,ipt,jpt,cfov
  Integer nf_status,nf_fid,nf_vid

  integer :: NCID
  integer :: isat

  integer :: status,mype
  character*10  atime

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
! set geogrid fle name
!
  call init_constants_derived

  workPath='./'
  write(geofile,'(a,a)') trim(workPath), 'geo_em.d01.nc'

  write(*,*) 'geofile', trim(geofile)
  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT

  call GET_MAP_ATT_geo(geofile, userDX, userDY, CEN_LAT, CEN_LON, &
                userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ)
  write(*,*) userDX, userDY, CEN_LAT, CEN_LON
  write(*,*) userTRUELAT1,userTRUELAT2,MOAD_CEN_LAT,STAND_LON,MAP_PROJ
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat),rxlon(nlon,nlat))
  allocate(ylat(nlon,nlat),rylat(nlon,nlat))

  call OPEN_geo(geofile, NCID)
  call GET_geo_sngl_geo(NCID,Nlon,Nlat,ylat,xlon)
  call CLOSE_geo(NCID)
!
!   setup  map
!
!  if (MAP_PROJ == PROJ_LC) then
!     user_known_x = (NLON+1)/2.0
!     user_known_y = (NLAT+1)/2.0
!     call map_init(proj_stack)
!
!     call map_set(MAP_PROJ, proj_stack, &
!                  truelat1=userTRUELAT1, &
!                  truelat2=userTRUELAT2, &
!                  stdlon=STAND_LON, &
!                  lat1=CEN_LAT, &
!                  lon1=CEN_LON, &
!                  knowni=user_known_x, &
!                  knownj=user_known_y, &
!                  dx=userDX, &
!                  r_earth=earth_radius_m)
!  else
  mype=0
  rylat=ylat*deg2rad
  rxlon=xlon*deg2rad
  call init_general_transform(rylat,rxlon,mype)
  !  endif
  !
  allocate (Pxx(nlon,nlat,nfov),Txx(nlon,nlat,nfov),Hxx(nlon,nlat,nfov),Exx(nlon,nlat,nfov))
  allocate (PHxx(nlon,nlat,nfov),WPxx(nlon,nlat,nfov))
  allocate (xdist(nlon,nlat,nfov), xxxdist(nfov))
  allocate (index(nlon,nlat), jndex(nfov))
  index=0

  Pxx(:,:,:) = missing_val
  Txx(:,:,:) = missing_val
  Exx(:,:,:) = missing_val
  Hxx(:,:,:) = missing_val
  PHxx(:,:,:) = missing_val
  WPxx(:,:,:) = missing_val

!
!  read in the CLAVR-NESDIS cloud data
do isat=1,2
  if (isat == 1) then
     file = 'clavr_east.nc'
  else
     file = 'clavr_west.nc'
  endif

  call read_CLAVR_cloud(file,missing_val,nxp,nyp,phase_l,ptop_l,teff_l,hgt_l,emiss_l,lwp_l,lat_l,lon_l)

  print*, 'in main. nx, ny = ', nxp, nyp
  print*, 'isat ', isat
! -----------------------------------------------------------
! -----------------------------------------------------------
!     Map each FOV onto RR grid points 
! -----------------------------------------------------------
! -----------------------------------------------------------
  do jpt=1,nyp
     do ipt=1,nxp
        if (phase_l(ipt,jpt).ge.0) then
           if (lon_l(ipt,jpt) < -100. .and. isat == 1) cycle
           if (lon_l(ipt,jpt) > -100. .and. lon_l(ipt,jpt) < 0. .and. isat == 2) cycle
           
           !  Indicates there is some data (not missing)
           !         if (MAP_PROJ == PROJ_LC) then
           !           call latlon_to_ij(proj_stack, 90.0-lat_l(ipt,jpt), lon_l(ipt,jpt), xc, yc)
           !         else
           rlon=lon_l(ipt,jpt)*deg2rad
           !rlat=(90.0-lat_l(ipt,jpt))*deg2rad
           rlat=lat_l(ipt,jpt)*deg2rad
           call tll2xy(rlon,rlat,xc,yc)
           
           ! * Compute RR grid x/y at lat/lon of cloud data
           ! -----------------------------------------------------------
           ! * XC,YC should be within RR boundary, i.e., XC,YC >0
           
           ii1 = int(xc+0.5)
           jj1 = int(yc+0.5)
           
           do jj = max(1,jj1-1), min(nlat,jj1+1)
              if (jj1-1.ge.1 .and. jj1+1.le.nlat) then
                 do ii = max(1,ii1-1), min(nlon,ii1+1)
                    if (ii1-1.ge.1 .and. ii1+1.le.nlon) then
                       !         if(XC .ge. 1. .and. XC .lt. nlon .and.        &
                       !            YC .ge. 1. .and. YC .lt. nlat) then
                       !             ii1 = int(xc+0.5)
                       !             jj1 = int(yc+0.5)
                       !             ii=ii1
                       !             jj=jj1
                       
                       ! * We check multiple data within gridbox
                       
                       if (index(ii,jj).lt.nfov) then
                          index(ii,jj) = index(ii,jj) + 1
                          !if(phase_l(ipt,jpt) .ne. missing_val) PHxx(ii,jj,index(ii,jj)) = phase_l(ipt,jpt)
                          !if(ptop_l(ipt,jpt)  .ne. missing_val) Pxx(ii,jj,index(ii,jj))  = ptop_l(ipt,jpt)
                          !if(teff_l(ipt,jpt)  .ne. missing_val) Txx(ii,jj,index(ii,jj))  = teff_l(ipt,jpt)
                          !if(emiss_l(ipt,jpt) .ne. missing_val) Exx(ii,jj,index(ii,jj))  = emiss_l(ipt,jpt)
                          !if(lwp_l(ipt,jpt)   .ne. missing_val) WPxx(ii,jj,index(ii,jj)) = lwp_l(ipt,jpt)
                          PHxx(ii,jj,index(ii,jj)) = phase_l(ipt,jpt)
                          Pxx(ii,jj,index(ii,jj))  = ptop_l(ipt,jpt)
                          Txx(ii,jj,index(ii,jj))  = teff_l(ipt,jpt)
                          Exx(ii,jj,index(ii,jj))  = emiss_l(ipt,jpt)
                          Hxx(ii,jj,index(ii,jj))  = hgt_l(ipt,jpt)
                          WPxx(ii,jj,index(ii,jj)) = lwp_l(ipt,jpt)
                          !print *, ii,jj,index(ii,jj),size(xdist,1),size(xdist,2),size(xdist,3)
                          xdist(ii,jj,index(ii,jj)) = sqrt((xc+1-ii)**2 + (yc+1-jj)**2)
                       else
                          write(6,*) ' too many data in one grid, increase nfov'
                          write(6,*) nfov, ii,jj
                          stop 1234
                       end if
                    endif
                 enddo ! ii
              endif
           enddo  ! jj
           !if(XC .ge. 1. .and. XC .lt. nlon .and. YC .ge. 1. .and. YC .lt. nlat) then
           !   ii = int(xc+0.5)
           !   jj = int(yc+0.5)
           !   Pxx(ii,jj) = ptop_l(ipt,jpt)
           !endif   ! observation is in the domain
           
        endif   ! phase_l >= 0
     enddo   ! ipt
  enddo   ! jpt

  deallocate(phase_l,ptop_l,teff_l,hgt_l,emiss_l,lwp_l,lat_l,lon_l)
enddo


  allocate(w_pcld(nlon,nlat))
  allocate(w_tcld(nlon,nlat))
  allocate(w_frac(nlon,nlat))
  allocate(w_eca(nlon,nlat))
  allocate(w_hgt(nlon,nlat))
  allocate(w_lwp(nlon,nlat))
  allocate(nlev_cld(nlon,nlat))

  w_pcld(:,:) = missing_val
  w_tcld(:,:) = missing_val
  w_frac(:,:) = missing_val
  w_hgt(:,:) = missing_val
  w_eca(:,:) = missing_val
  w_lwp(:,:) = missing_val
  nlev_cld(:,:) = missing_val

  ! * ioption = 1 is nearest neighrhood
  ! * ioption = 2 is median of cloudy fov
  ioption = 2
  
  do jj = 1,nlat
     do ii = 1,nlon
        if (index(ii,jj) .lt. 3) then
           !w_pcld(ii,jj) = Pxx(ii,jj,1)
        elseif(index(ii,jj) .ge. 3) then
           
           ! * We decided to use nearest neighborhood for ECA values,
           ! *     a kind of convective signal from GOES platform...
           !
           ! * Sort to find median value 
           if(ioption .eq. 2) then    !pick median 
              do i=1,index(ii,jj)
                 jndex(i) = i
                 xxxdist(i) = Pxx(ii,jj,i)
              enddo
              call sortmed(xxxdist,index(ii,jj),jndex,fr)
              med_pt = index(ii,jj)/2  + 1
              w_pcld(ii,jj) = Pxx(ii,jj,jndex(med_pt)) ! hPa
              w_tcld(ii,jj) = Txx(ii,jj,jndex(med_pt)) ! K
              w_lwp(ii,jj)  = WPxx(ii,jj,jndex(med_pt)) ! g/m^2
              w_eca(ii,jj)  = Exx(ii,jj,jndex(med_pt)) ! %
              w_hgt(ii,jj)  = Hxx(ii,jj,jndex(med_pt)) ! km
              if (w_pcld(ii,jj) .gt. 1012.99) w_frac(ii,jj) = 0.0
              !if (w_pcld(ii,jj).lt. 0 .and. PHxx(ii,jj,jndex(med_pt)) .ne. missing_val) then
              !   w_pcld(ii,jj)   = 1013. ! hPa - no cloud
              !   w_frac(ii,jj)   = 0.0
              !   w_tcld(ii,jj)   = 330
              !   w_lwp(ii,jj)    = 0
              !   nlev_cld(ii,jj) = 0
              !end if
           endif   ! pick median

           ! cloud fraction based on phase (0 are clear), what about -9 ????
           if( w_pcld(ii,jj) < 1012.99) then
              cfov = 0
              do i=1,index(ii,jj)
                 if(PHxx(ii,jj,i) .gt. 0.1) cfov = cfov + 1
              enddo
              w_frac(ii,jj) = float(cfov)/(max(1,index(ii,jj)))     !  fraction
              if( w_frac(ii,jj) > 0.01 ) nlev_cld(ii,jj) = 1
           endif
        endif   ! index > 3
     enddo  !ii
  enddo  !jj

  ! For now, just replace w_frac with w_eca
  !w_frac = w_eca

  ! Use w_frac below 2km, and w_eca above 2km
  where (w_hgt>=2.) w_frac = w_eca

  call write_bufr_CLAVR(nlon,nlat,index,w_pcld,w_tcld,w_frac,w_lwp,nlev_cld)
  
  call write_CLAVR_cloud('rap_cld.nc',missing_val,w_pcld,w_tcld,w_frac,w_lwp,nlat,nlon)
  
end program process_CLAVR_cloud

subroutine write_CLAVR_cloud(satfile,missing_val,ctp,ctt,cf,lwp,nlat,nlon)
  use netcdf
  
  implicit none

  character(len=*),              intent(in) :: satfile
  integer,                       intent(in) :: nlat, nlon
  real,                          intent(in) :: missing_val 
  real,    dimension(nlon,nlat), intent(in) :: ctp,ctt,cf,lwp
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  integer, dimension(20)             :: status
  integer                            :: ncid, xDimID, yDimID
  integer                            :: latVarID, lonVarID
  integer :: ctpVarID, cttVarID, cfVarID, lwpVarID

  !if(associated(ptop)) print*, 'ptop is associated'
  ! Open file and read precip field
  status(:)  = nf90_NoErr
  status(1)  = nf90_create(trim(satfile),nf90_clobber,ncid)
  
  ! Define dimension variables
  status(2) = nf90_def_dim(ncid,'lon',nlon,xDimID)
  status(3) = nf90_def_dim(ncid,'lat',nlat,yDimID)
  ! Define variables and missing values
  status(4) = nf90_def_var(ncid,'CTP',nf90_float,(/xDimID,yDimID/),ctpVarID)
  status(5) = nf90_put_att(ncid,ctpVarID,'_FillValue',missing_val)

  status(6) = nf90_def_var(ncid,'CTT',nf90_float,(/xDimID,yDimID/),cttVarID)
  status(7) = nf90_put_att(ncid,cttVarID,'_FillValue',missing_val)

  status(8) = nf90_def_var(ncid,'CF',nf90_float,(/xDimID,yDimID/),cfVarID)
  status(9) = nf90_put_att(ncid,cfVarID,'_FillValue',missing_val)

  status(10) = nf90_def_var(ncid,'CTH',nf90_float,(/xDimID,yDimID/),lwpVarID)
  status(11) = nf90_put_att(ncid,lwpVarID,'_FillValue',missing_val)

  ! End of definitions
  status(12) = nf90_enddef(ncid)
    
  ! Put dimension variables and state variables
  status(13) = nf90_put_var(ncid,ctpVarID,ctp)
  status(14) = nf90_put_var(ncid,cttVarID,ctt)
  status(15) = nf90_put_var(ncid,cfVarID,cf)
  status(16) = nf90_put_var(ncid,lwpVarID,lwp)
  
  print*, status(:)
  if(any(status(:16) /= nf90_NoErr)) stop "Error writing interpolated state to NetCDF file"
  
  status(1) = nf90_close(ncid)
     
  print *, 'Done writing CLAVR file'

end subroutine write_CLAVR_cloud

subroutine sortmed(p,n,is)
      real p(n)
      integer is(n)
! * count cloudy fov
      real    f
      integer cfov
      cfov = 0
      do i=1,n
! - changed for NASA LaRC, p set = -9 for clear FOVs
         if(p(i) .gt. 0.) cfov = cfov + 1
      enddo
      f = float(cfov)/(max(1,n))
! cloud-top pressure is sorted high cld to clear
      nm1 = n-1 
      do 10 i=1,nm1
      ip1 = i+1 
        do 10 j=ip1,n
        if(p(i).le.p(j)) goto 10
          temp = p(i) 
          p(i) = p(j)
          p(j) = temp
          iold  = is(i)
          is(i) = is(j)
          is(j) = iold
   10 continue
      return
end subroutine sortmed
                                                                                 
