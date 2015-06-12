PROGRAM process_SST
!
!   PRGMMR: Ming Hu  nd Tanya Smirnova   ORG: GSD        DATE: 2010-09-25
!
! ABSTRACT: 
!     This routine reads in SST
!
! 
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
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

  use mpi

  implicit none
!
!
! MPI variables
  integer :: npe, mype, mypeLocal,ierror

! RR grid
  integer :: nlon,nlat
! RR in LC
!  parameter (nlon=648,nlat=647)
! RR in RLL
  parameter (nlon=758,nlat=567)
  real :: xlon(nlon,nlat)    !
  real :: ylat(nlon,nlat)    !
  real :: xland(nlon,nlat)   !
  real :: vegtyp(nlon,nlat)   !

  real, allocatable :: sstRR(:,:)    ! sst in RR 
  real, allocatable :: sstGlobal(:,:)  ! sst from global dataset
  integer, allocatable :: imaskSST(:,:)

!
  character*80 input_file
  integer :: istatus
  integer :: i,j,iwater,ilake,iice
!
  INTEGER :: iyear, imonth, iday, ihr

  logical :: ifswap

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
! MPI setup
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(mpi_comm_world,npe,ierror)
  call MPI_COMM_RANK(mpi_comm_world,mype,ierror)

  ifswap=.true.
!
!  if(mype==0) then

  call GET_RR_GRID(ifswap,mype,xlon,ylat,nlon,nlat,xland,vegtyp)
!
  allocate(sstRR(nlon,nlat))
  allocate(sstGlobal(4320,2160))

  sstRR=0
! 0.083333 degre SST
  call read_sstGlobal(sstGlobal,iyear,imonth,iday,ihr)
  if(mype==0) write(6,*)' read in global sst data', iyear, imonth, iday, ihr

  allocate(imaskSST(4320,2160))
  if(ifswap) then
      OPEN (11,FILE='RTG_SST_landmask.dat',CONVERT='BIG_ENDIAN')
  else
      OPEN (11,FILE='RTG_SST_landmask.dat')
  endif
! 
! Read in land sea tags (0 for ocean; 3 for land) 
!
    READ (11,'(80I1)') imaskSST
    CLOSE (11)
    if(mype==0) print *,'imaskSST(600,210) ', imaskSST(600,210)
!
! 1 degree SST
!      call read_sst1deg(sst1deg,iyear,imonth,iday,ihr)

      call sstGlobal2RR (sstGlobal,imaskSST,xland,nlon,nlat,xlon,ylat,sstRR)
    if(mype==0)   write(6,*)'from global  data ylat/xlon/sstRR(516,258)',   &
                 ylat(516,258),xlon(516,258),sstRR(516,258)

! High-resolution SST plus Great Lakes data
     if(mype==0)  write(6,*)' read 14km sst data', imonth, iday
!       iwater=16
       iwater=17 ! MODIS
       ilake =21 ! MODIS
       iice = 15 ! MODIS

       write(6,*)' read 14km sst data'
      call sst14k (sstRR, ylat, xlon, vegtyp, iwater, ilake, nlat, nlon)

     if(mype==0) write(6,*)'after sst14k ylat/xlon/sstRR(516,258)',                 &
                 ylat(516,258),xlon(516,258),sstRR(516,258)
!
  call update_SST_netcdf_mass(mype,ifswap,sstRR, ylat, xlon, nlon, nlat,xland, &
               vegtyp,ilake,iice)
!
!  endif ! mype==0

  call MPI_FINALIZE(ierror)
!
END PROGRAM process_SST

SUBROUTINE read_sstGlobal(sst,iyear,imonth,iday,ihr)
!
!   PRGMMR: Ming Hu, Tanya Smirnova    ORG: GSD        DATE: 2010-09-27
!
! ABSTRACT:
!     This routine read in GLOBAL SST data from a grib file
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT 
!   OUTPUT 
!      sst :  sea surface temperature
!      iyear: Year
!      imonth: month
!      iday:   day
!      ihr:  hour
!   INPUT FILES:  sst_1degree
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


!  USE GRIB_MOD
  implicit none

  INTEGER JF, MBUF
!  PARAMETER (JF=360*180)
  PARAMETER (JF=4320*2160)
  INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
  LOGICAL*1 LB(JF)
  REAL, intent(out)::  SST(JF)
  PARAMETER(MBUF=256*2160)
  CHARACTER CBUF(MBUF)

  INTEGER :: LUGB,LUGI, J, IRET, K, KF
  character(200):: FNAME1

  INTEGER :: I, iyear, imonth, iday, ihr

!-----------------------------------------------------------------------
  LUGB = 11
  LUGI = 0
  J = 0

!  FNAME1='./sst_1degree'
  FNAME1='SSTRTG'
  call baopen(LUGB,trim(fname1),IRET)
!grib2  call baopenr(LUGB,trim(fname1),IRET)
  if ( IRET .ne. 0) then
      write(6,*) 'bad baopen!!! ', IRET
      STOP
  endif

  JPDS=-1
  JGDS=-1
  JPDS(5) = 11
  call GETGB(LUGB,LUGI,JF,J,JPDS,JGDS,    &
             KF,K,KPDS,KGDS,LB,SST,IRET)

  if (IRET .ne. 0) then
    write(6,*) 'bad getgb ', IRET
    STOP
  endif
  write(*,*)  'sst=', maxval(sst), minval(sst),k

!  iyear=2000 + KPDS(8)
!  imonth=KPDS(9)
!  iday=KPDS(10)
!  ihr=KPDS(11)
!       POLAR STEREOGRAPHIC GRIDS
!      KGDS(i)
!          (2)   - N(I) NR POINTS ALONG LAT CIRCLE
!          (3)   - N(J) NR POINTS ALONG LON CIRCLE
!          (4)   - LA(1) LATITUDE OF ORIGIN
!          (5)   - LO(1) LONGITUDE OF ORIGIN
!          (6)   - RESOLUTION FLAG  (RIGHT ADJ COPY OF OCTET 17)
!          (7)   - LOV GRID ORIENTATION
!          (8)   - DX - X DIRECTION INCREMENT
!          (9)   - DY - Y DIRECTION INCREMENT
!          (10)  - PROJECTION CENTER FLAG
!          (11)  - SCANNING MODE (RIGHT ADJ COPY OF OCTET 28)
!  do j=1,1024,5
!    write(*,'(1024i1)') (int(F((j-1)*1024 + i)+0.2),i=1,1024,4)
!  enddo
end subroutine 
