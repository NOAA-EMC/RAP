program process_NSSL_mosaic
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2007-12-17
!
! ABSTRACT: 
!     This routine read in NSSL reflectiivty mosaic fiels and 
!     interpolate them into GSI mass grid
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT FILES:  mosaic_files
!
!   OUTPUT FILES:
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
!      use constants, only: zero,one_tenth,one,deg2rad,rad2deg
!      use gridmod, only: regional,nlon,nlat,nsig,         &
!                         tll2xy,txy2ll,                   &
!                         regional_time,nhr_assimilation,  &
!                         regional_fhr,    &
!                         ylat,xlon
  use mpi
  use kinds, only: r_kind,i_kind

  implicit none
!
!  INCLUDE 'netcdf.inc'
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

  real     :: rad2deg = 180.0/3.1415926
!
  character*256 output_file
!
!  grid
  integer(i_kind) :: nlon,nlat
  real,allocatable:: xlon(:,:)    !
  real,allocatable:: ylat(:,:)    !
  REAL, allocatable :: ref3d(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: ref0(:,:,:)   ! 3D reflectivity
  REAL, allocatable :: maxref(:,:)   ! composite reflectivity
  integer , allocatable :: imaxref(:,:)   ! composite reflectivity
  REAL(r_kind), allocatable :: ref3d_column(:,:)   ! 3D reflectivity in column
  CHARACTER*180   geofile
!
!  For reflectiivty mosaic
!
  INTEGER ::    ntiles
  CHARACTER*180   workPath
  CHARACTER*180   mosaicfile

  INTEGER ::   mscNlon   ! number of longitude of mosaic data
  INTEGER ::   mscNlat   ! number of latitude of mosaic data
  INTEGER ::   mscNlev   ! number of vertical levels of mosaic data
  REAL, allocatable :: mscValue(:,:)    ! reflectivity

  type mosaic_head
            integer(4):: nx
            integer(4):: ny
            integer(4):: nz
            real(4):: dx
            real(4):: dy
            real(4):: ctrl_lat, ctrl_lon
            real(4):: zp(31)
            real(4):: missing_value
            real(4):: no_radar_cover
  end type mosaic_head

  type(mosaic_head):: mhead
  real(4), allocatable :: ref(:,:,:)
  integer(4) nbytes_in
  integer(4) bufint
  real(4) bufreal

  integer(4) :: flsize(8), this_flsize

  real t_lat_s(8)
  real t_lat_n(8)
  real t_lon_w(8)
  real t_lon_e(8)
  data t_lat_s /40.0,40.0,40.0,40.0,  &
                20.0,20.0,20.0,20.0/
  data t_lon_w /-130.0,-110.0,-90.0,-80.0,  &
                -130.0,-110.0,-90.0,-80.0/
  data t_lat_n /55.0,55.0,55.0,55.0,  &
                40.0,40.0,40.0,40.0/
  data t_lon_e /-110.0,-90.0,-80.0,-60.0,  &
                -110.0,-90.0,-80.0,-60.0/

  REAL :: lonMin,latMin,lonMax,latMax,dlon,dlat
!
!  lightning staff
!
  REAL, allocatable :: lightning(:,:)    ! lightning
  REAL, allocatable :: dbz_lightning(:,:)    ! lightning in 2d dbz
!
!
!  ** misc
      
  real        ::rix  ! x-grid coordinate (grid units)
  real        ::riy  ! y-grid coordinate (grid units)
  logical     ::outside     ! .false., then point is inside x-y domain
                              ! .true.,  then point is outside x-y domain

  integer i,j,k,itype,iymdh,ier,jret,ifn
  integer iz,n,nlv,isao,nflag,np,ilen,iflag,iostat

  REAL ::  rlat,rlon
  INTEGER  :: ip,jp
  REAL ::  rip,rjp
  REAL ::  dip,djp
  REAL ::  w1,w2,w3,w4
  REAL ::  ref1,ref2,ref3,ref4,refl_ltng

  INTEGER(i_kind)  ::  maxlvl, tversion
  INTEGER(i_kind)  ::  numlvl,numref
  INTEGER(i_kind)  ::  idate
  REAL, allocatable ::   xland (:,:)
  integer :: status,ios,ierr
  logical :: ifswap

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror) 
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

write(*,*) mype, 'deal with mosaic'
  ifswap=.true.
!
! set geogrid fle name
!
  flsize(1)=522609334; flsize(2)=522609334
  flsize(3)=261435334; flsize(4)=522609334
  flsize(5)=696696334; flsize(6)=696696334
  flsize(7)=348522334; flsize(8)=696696334

  workPath='./'
  geofile='geo_em.d1'


  write(*,*) 'geofile', trim(geofile)
  call GEO_GET_GRID_DIM_bin(geofile,NLON,NLAT,ifswap,ierr)
  if(ierr /=0 ) then
    write(6,*) 'Cannot read geogrid file'
    goto 999
  endif
!  call GET_DIM_ATT_geo(geofile,NLON,NLAT)
  write(*,*) 'NLON,NLAT',NLON,NLAT
!
!  get GSI horizontal grid in latitude and longitude
!
  allocate(xlon(nlon,nlat))
  allocate(ylat(nlon,nlat))
  allocate(xland(nlon,nlat))

  call GEO_GET_GRID_bin(geofile,xlon,ylat,Nlon,Nlat,xland,ifswap,ierr)
  if(ierr /=0 ) then
    write(6,*) 'Cannot read geogrid file'
    goto 999
  endif
  deallocate(xland)

!
! set NCEP mosaic file name and read in the head
!
  mypeLocal=mype+1
  workPath='./'
  write(mosaicfile,'(a,a,I1)') trim(workPath), 'mosaic_t',mypeLocal
!
!   deal with certain tile
!
   tversion = 8
   maxlvl = 31
   write(*,*) 'process tile:',trim(mosaicfile)
   allocate(ref3d(nlon,nlat,maxlvl))
   ref3d=-999.0

   this_flsize=flsize(mype+1)
!  open(1,file=mosaicfile,access='direct', recl=this_flsize,&
!                action='read',status='old',err=800)
!      read(1,rec=1,num=nbytes_in,err=800) mhead
   open(1,file=mosaicfile,access='stream', status='old',err=800)
      read(1,err=800) mhead
   close(1)
   bufint=mhead%nx
!  call swapint(bufint)
   mscNlon=bufint
   bufint=mhead%ny
!  call swapint(bufint)
   mscNlat=bufint
   bufint=mhead%nz
!  call swapint(bufint)
   mscNlev=bufint

   latMin=t_lat_s(mype+1)
   latMax=t_lat_n(mype+1)
   lonMin=t_lon_w(mype+1)
   lonMax=t_lon_e(mype+1)

   bufreal=mhead%dx
!  call swapreal(bufreal)
   dlon=bufreal
   bufreal=mhead%dy
!  call swapreal(bufreal)
   dlat=bufreal
   write(*,*) mscNlon,mscNlat,mscNlev
   write(*,*) 'Area of tile=',lonMin,latMin,lonMax,latMax,dlon,dlat

   allocate(ref(mscNlev,mscNlat,mscNlon))
   allocate(mscValue(mscNlon,mscNlat))
!
   if( maxlvl /= mscNlev ) then
       write(*,*) 'Wrong vertical layers:', maxlvl, mscNlev
       call stop2(100)
   endif
!
!  ingest mosaic file and interpolation
!

!  open(21,file=mosaicfile,access='direct', recl=this_flsize ,&
!           action='read',status='old',err=800)
!      read(21,rec=1,num=nbytes_in,err=800) mhead,                 &
!     read(21,rec=1,err=800) mhead,                 &

   open(21,file=mosaicfile,access='stream', status='old')
      read(21) mhead,                 &
             (((ref(k,j,i),i=1,mscNlon),j=mscNlat,1,-1),k=1,mscNlev)
   close(21)

   DO k=1, mscNlev
     write(*,*) 'mype=',mype+1,k, maxval(ref(k,:,:)),minval(ref(k,:,:))

     Do j=1,mscNlat
     Do i=1,mscNlon
          mscValue(i,j)=ref(k,j,i)
     enddo
     enddo
          DO j=1,nlat
          DO i=1,nlon
             rlat=ylat(i,j)
             rlon=xlon(i,j)

             rip=(rlon-lonMin)/dlon+1
             rjp=(latMax-rlat)/dlat+1
             ip=int(rip)
             jp=int(rjp)
             dip=rip-ip
             djp=rjp-jp 

             if( ip >= 1 .and. ip < mscNlon ) then
             if( jp >= 1 .and. jp < mscNlat ) then
! inside mosaic domain
               w1=(1.0-dip)*(1.0-djp)
               w2=dip*(1.0-djp)
               w3=dip*djp
               w4=(1.0-dip)*djp
               ref1=mscValue(ip,jp)
               ref2=mscValue(ip+1,jp)
               ref3=mscValue(ip+1,jp+1)
               ref4=mscValue(ip,jp+1)
               if(ref1 > -500.0 .and. ref2 > -500.0 .and.  &
                  ref3 > -500.0 .and. ref4 > -500.0 ) then
                  ref3d(i,j,k)=(ref1*w1+ref2*w2+ref3*w3+ref4*w4)
               elseif(ref1 > -5000.0 .and. ref2 > -5000.0 .and.  &
                  ref3 > -5000.0 .and. ref4 > -5000.0 ) then
                  ref3d(i,j,k)=-99.0   ! clear
               else
                  ref3d(i,j,k)=-999.0  ! no observation
               endif
             endif
             endif
          ENDDO
          ENDDO
   ENDDO  ! mscNlev

   deallocate(ref)
   deallocate(mscValue)
   write(*,*) 'Successful processing tile:',trim(mosaicfile)
   goto 900
800 continue
    write(*,*) 'There is a problem in processing tile:',trim(mosaicfile)
900 continue

   call mpi_barrier(MPI_COMM_WORLD,ierror)
!
!  collect data from all processes to root (0)
!
   if(mype==0) then
     allocate( ref0(nlon,nlat,maxlvl) )
     allocate( maxref(nlon,nlat) )
     allocate( imaxref(nlon,nlat) )
   endif
   call MPI_REDUCE(ref3d, ref0, nlon*nlat*maxlvl, MPI_REAL, MPI_MAX, 0, &
                     MPI_COMM_WORLD, ierror)
   deallocate(ref3d)

!
   open(12,file='mosaic_cycle_date',action='read',status='old',iostat=ios)
   if(ios ==0 ) then
      read(12,*,iostat=ios) idate
      if(ios /= 0) then
         write(6,*) 'cannot read in cycle time '
         idate=2000010100
      endif
      close(12)
      write(6,*) 'cycle time is :', idate
   else
      write(6,*) 'cannot read in cycle time '
      idate=2000010100
   endif
!
  if(mype==0) then
!
    allocate(ref3d_column(maxlvl+2,nlon*nlat))
    ref3d_column=-999.0
    numref=0
    DO j=1,nlat
    DO i=1,nlon
!    DO j=2,nlat-1
!    DO i=2,nlon-1
      numlvl=0
      DO k=1,maxlvl
        if(abs(ref0(i,j,k)) < 888.0 ) numlvl=numlvl+1
      ENDDO
      if(numlvl > 0 ) then
        numref=numref+1
        ref3d_column(1,numref)=float(i)
        ref3d_column(2,numref)=float(j)
        DO k=1,maxlvl
           ref3d_column(2+k,numref)=ref0(i,j,k)
        ENDDO
      endif
    ENDDO
    ENDDO

    write(*,*) 'Dump out results', numref, 'out of', nlon*nlat
    OPEN(10,file=trim(workPath)//'RefInGSI.dat',form='unformatted')
      write(10) nlon,nlat,maxlvl
      write(10) ref0
!     write(10) maxlvl,nlon,nlat,numref,1,2
!     write(10) ((ref3d_column(k,i),k=1,maxlvl+2),i=1,numref)
    close(10)
  
    if(numref>0) then
       write(*,*) 'Start write_bufr_nsslref'
       call write_bufr_nsslref(maxlvl,nlon,nlat,numref,ref3d_column,idate)
    else
       write(*,*) '0 observation, do not write  mosaic bufr'
    endif
  endif

999 continue 
  call MPI_FINALIZE(ierror)
!
end program process_NSSL_mosaic

subroutine swapint(bufint_inout)

  integer(4),intent(inout) ::  bufint_inout
  integer(4) ::  bufint
  integer(1) buf4(4),buf1
  equivalence (buf4,bufint)

   bufint=bufint_inout
   buf1=buf4(4)
   buf4(4)=buf4(1)
   buf4(1)=buf1
   buf1=buf4(3)
   buf4(3)=buf4(2)
   buf4(2)=buf1

   bufint_inout=bufint

end subroutine swapint

subroutine swapreal(bufreal_inout)

  real(4),intent(inout) :: bufreal_inout
  real(4) :: bufreal
  integer(1) buf4(4),buf1
  equivalence (buf4,bufreal)

   bufreal=bufreal_inout

   buf1=buf4(4)
   buf4(4)=buf4(1)
   buf4(1)=buf1
   buf1=buf4(3)
   buf4(3)=buf4(2)
   buf4(2)=buf1

   bufreal_inout=bufreal

end subroutine swapreal

