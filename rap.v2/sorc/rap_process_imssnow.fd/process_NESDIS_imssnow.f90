PROGRAM process_NESDIS_imssnow
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT: 
!     This routine read in NESDIS NESDIS SNOW/ICE data from a grib file and  
!     map them into RR mass grid
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


  implicit none
!
! RR grid
  integer :: nlonRR,nlatRR
  parameter (nlonRR=758,nlatRR=567)
  real :: xlonRR(nlonRR,nlatRR)    !
  real :: ylatRR(nlonRR,nlatRR)    !
  real :: xlandRR(nlonRR,nlatRR)   !
  real :: xlandIMS(nlonRR,nlatRR)   !

  real, allocatable :: snowiceRR(:,:)    ! snow/ice in RR 
!
  integer :: i,j
  INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
!
! sea ice and snow, and mask
  REAL, allocatable :: ICEC(:,:)
  REAL, allocatable :: SNOWICEC(:,:)
  Logical*1, allocatable :: lmask(:,:)
  REAL, allocatable :: maskims(:,:)
  integer, allocatable :: imaskims(:,:)
  INTEGER :: iyear, imonth, iday, ihr
  
  logical :: ifswap

!**********************************************************************
!
!            END OF DECLARATIONS....start of program

  ifswap=.true.

  allocate(ICEC(6144,6144))
  allocate(SNOWICEC(6144,6144))
  allocate(lmask(6144,6144))
  allocate(maskims(6144,6144))
  allocate(imaskims(6144,6144))
  ICEC=0
  SNOWICEC=0
  call read_issnow(icec,snowicec,lmask,kgds,kpds,iyear,imonth,iday,ihr)
  write(*,*) 'NESDIS SNOW and ICE in ', iyear,imonth,iday,ihr

  call GET_RR_GRID(ifswap,xlonRR,ylatRR,nlonRR,nlatRR,xlandRR)
!
!  Map NESDIS SNOW/ICE to RR grid: Based on the following RUC routine:
! /whome/rucdev/code/13km/hybpre_code/snohires.f
!
  DO J = 1, 6144
    DO I = 1, 6144
      IF (icec(I,J) > 0. .OR. snowicec(I,J) > 0.) THEN
        snowicec(I,J) = 1.
      ELSE
        snowicec(I,J) = 0.
      ENDIF

!tgs Logical lmask --> .true. for land, .false. for water
      IF (lmask(I,J)) THEN
        maskims(i,j) = 1.    ! land
      ELSE
        maskims(i,j) = 0.    ! water
      ENDIF
    ENDDO
  ENDDO
  deallocate(icec)
!
! get NESDIS-IMS LAND-WATER mask
!                                          (SEA=0,LAND=1)
!  open (43, file='nam_imsmask', form='formatted')
!  DO J = 1, 6144
!     READ(43,'(80i1)')  (imaskims(I,J),I=1,6144)
!  end do
!  close (43)
!  maskims=float(imaskims)
!  deallocate(imaskims)
!  write (6,*) ' read IMS snow land-water mask'
!
! map to RR grid
!
  allocate(snowiceRR(nlonRR,nlatRR))
  snowiceRR=0  ! pecentage
  call map2RR(snowicec,maskims,kgds,kpds,xlandRR,nlonRR,nlatRR,xlonRR,ylatRR,snowiceRR,xlandIMS)
  deallocate(snowicec)
  deallocate(maskims)
!
!
!  trim snow cover field based on NESDIS snow cover data
!
  call update_SNOWICE_binary_mass(ifswap,snowiceRR, xlandRR, nlonRR, nlatRR,xlandIMS)
!

END PROGRAM process_NESDIS_imssnow

SUBROUTINE map2RR(f,maskims,kgds_src,kpds_src,xlandRR,nlonRR,nlatRR,xlonRR,ylatRR,snowiceRR,xlandIMS)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT:
!     This routine map NESDIS SNOW/ICE data to RR grid
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


  implicit none

  INTEGER JPDS(200),JGDS(200),KPDS_SRC(200),KGDS_SRC(200)
  REAL, intent(in):: F(6144,6144)
  REAL, intent(in):: maskims(6144,6144)
!  grid
  integer, intent(in) :: nlonRR,nlatRR
  real, intent(in):: xlandRR(nlonRR,nlatRR)
  real, intent(in):: xlonRR(nlonRR,nlatRR)    !
  real, intent(in):: ylatRR(nlonRR,nlatRR)    !
!
  real, intent(out):: snowiceRR(nlonRR,nlatRR)    !

!
  real :: xlandIMS(nlonRR,nlatRR)
  REAL :: DTR
  REAL :: XPNMC8,YPNMC8,ENNMC8,ALNMC8,ORIENT8
  REAL :: XPNMCAF,YPNMCAF,ENNMCAF,ALNMCAF,ORIENTAF

  real :: YYLAT,XLONG, RM, RAD, X, Y
  integer :: IS,IP1,JS,JP1, IPOINT, JPOINT
!
  integer :: iland,KOUNT
  real    :: XRATIO,YRATIO,AREA11,AREA21,AREA12,AREA22,AREA
  integer :: i,j,k,ifound,LL,JPE,JPB,IPE,IPB,NK,MK

!
  integer                   :: nret
  real                      :: dum
  real, parameter           :: undefined_value = -1.0

!
  print *,'NESDIS grid info: kgds_src', kgds_src

    DO j=1,nlatRR
    DO i=1,nlonRR
         call gdswiz(kgds_src,-1,1,undefined_value,X,Y, &
                     xlonRR(i,j),ylatRR(i,j),nret,0,dum,dum)
         if (nret /= 1) then
           print*,"- ERROR!! MODEL POINT OUTSIDE NESDIS GRID."
!            snowiceRR(i,j) = 0.
         else
! check if X and Y are defined
	 if(X == undefined_value .or. Y == undefined_value) then
!          snowiceRR(i,j) = 0.0 
          goto 1111
         else
         endif
            IS  = NINT(X)
            IP1 = IS + 1
            JS  = NINT(Y)
            JP1 = JS + 1

           ipoint = nint(X)
           jpoint = nint(Y)

!           if (f(ipoint,jpoint) == 1) then
!             snowiceRR(i,j) = 1.0
!           else
!             snowiceRR(i,j) = 0.0
!           end if

         end if


!  AND ONLY SEA POINTS ARE INTERPOLATED TO SEA POINTS (FOR ICE)
!  (NESDIS/IMS LAND MASK: SEA=0,LAND=1, WHILE THE RR MASK in XLAND IS: SEA=2,
!  LAND=1).
!

      ILAND = 1
      IF( int(xlandRR(i,j)) == 0 ) ILAND=0

!tgs - buggy      IF( int(maskims(i,j)) == ILAND ) THEN
      IF( int(maskims(is,js)) == ILAND ) THEN
        snowiceRR(i,j) = F(IPOINT,JPOINT)
      ELSE
!
!  NEAREST NEIGHBOR NOT SAME SFC TYPE, SO USE ALL 4 SURROUNDING POINTS
!
        KOUNT = 0
!
        XRATIO = X - REAL(IS)
        YRATIO = Y - REAL(JS)
!
        AREA11 = (1.0E0 - XRATIO) * (1.0E0 - YRATIO)
        AREA21 = XRATIO * (1.0E0 - YRATIO)
        AREA12 = (1.0E0 - XRATIO) * YRATIO
        AREA22 = XRATIO * YRATIO
!
!
        IF( int(maskims(IS, JS)) .EQ. ILAND) THEN
           KOUNT  = KOUNT + 1
           AREA   = AREA11
           IPOINT = IS
           JPOINT = JS
        END IF
!
        IF( int(maskims(IS, JP1)) .EQ. ILAND ) THEN
           KOUNT = KOUNT +1
           IF (KOUNT .EQ. 1) THEN
              IPOINT = IS
              JPOINT = JP1
           ELSEIF (AREA12 .GT. AREA) THEN
              AREA   = AREA12
              IPOINT = IS
              JPOINT = JP1
           END IF
        END IF
!
        IF( int(maskims(IP1, JS)) .EQ. ILAND ) THEN
           KOUNT = KOUNT + 1
           IF (KOUNT .EQ. 1) THEN
              AREA   = AREA21
              IPOINT = IP1
              JPOINT = JS
           ELSEIF (AREA21 .GT. AREA) THEN
              AREA   = AREA21
              IPOINT = IP1
              JPOINT = JS
           END IF
        END IF
!
!
        IF( int(maskims(IP1, JP1)) .EQ. ILAND ) THEN
           KOUNT = KOUNT + 1
           IF (KOUNT .EQ. 1) THEN
              AREA   = AREA22
              IPOINT = IP1
              JPOINT = JP1
           ELSEIF (AREA22 .GT. AREA) THEN
              AREA   = AREA22
              IPOINT = IP1
              JPOINT = JP1
           END IF
        END IF
!
!     DETERMINE SNO/ICE USING NEAREST NEIGHBOR WITH SAME SFC TYPE 
!
        IF(KOUNT .GT. 0) THEN
            snowiceRR(i,j) = F(IPOINT,JPOINT)
        ELSE
!
!         NO IMMEDIATELY SURROUNDING POINTS IN THE 6144 X 6144 FIELD OF
!         SNOW/ICE HAVE THE SAME LAND-SEA TYPE AS THE model POINT.  THE

!         model POINT MAY BE SMALL ISLAND OR LAKE OR SMALL BAY OR PENNIN.
!         (INVARIABLY A SMALL LAKE IN ETA GRID)
!         SO EXPAND SEARCH RADIUS AND TAKE FIRST SFC TYPE MATCH
!
            IPOINT = NINT(X)
            JPOINT = NINT(Y)
!
!  Define the frame (no. of grid points) over which to search for
!    a matching land/water type from IMS data for the model gridpoint.
            ifound=0
            DO LL=1,16
              JPE = MIN (6144, JPOINT+LL)
              JPB = MAX (1 , JPOINT-LL)
              IPE = MIN (6144, IPOINT+LL)
              IPB = MAX (1 , IPOINT-LL)
!
              DO NK=IPB,IPE
              DO MK=JPB,JPE
                 IF ( int(maskims(nk,mk)) == ILAND .and. ifound ==0 ) THEN
                    snowiceRR(i,j) = F(NK,MK)
                    ifound=1
                 ENDIF
              ENDDO  ! MK
              ENDDO  ! NK
            ENDDO  ! LL
!
!  NO LAND/SEA MASK MATCHES FOUND, SO 
!     A) NORTH OF 55N, WE ASSIGN SNOW/ICE IRRESPECTIVE OF SFC TYPE
!     B) SOUTH OF 55N, WE KEEP A PRIORI ZERO DEFAULT
!   (THE "B" OPTION BEST FOR WARMER LATS OF U.S., WHERE THIS CONDITION 
!   IS VIRTUALLY ALWAYS A SMALL ETA LAKE WITH NO COUNTERPART WATER 
!   NEARBY IN THE NESDIS/IMS GRID, E.G., SALT LAKE, WHERE WE MUST
!   AVOID GETTING SEA-ICE OWING TO SURROUNDING SNOW COVER)
!
!            IF (YYLAT .GE. 55.0 .and. ifound==0) THEN
            IF ( ifound==0 ) THEN
               snowiceRR(i,j) = F(IPOINT,JPOINT)
            ENDIF
        ENDIF   !  KOUNT .GT. 0
!
      ENDIF
!
!tgs - save NESDIS 4-km land/water mask
           xlandIMS(i,j)=maskims(IPOINT,JPOINT)
!

1111 continue
    ENDDO  ! nlon
    ENDDO  ! nlat
                                                                                                                       
end subroutine map2RR

SUBROUTINE read_issnow(icec,snowc,lb,kgds,iyear,imonth,iday,ihr)
!
!   PRGMMR: Ming Hu          ORG: GSD        DATE: 2009-04-15
!
! ABSTRACT:
!     This routine read in NESDIS NESDIS SNOW/ICE data from a grib file
!
! PROGRAM HISTORY LOG:
!
!   variable list
!
! USAGE:
!   INPUT 
!   OUTPUT 
!      icec : ice cover
!      snowc: snow cover
!      iyear: Year
!      imonth: month
!      iday:   day
!      ihr:  hour
!   INPUT FILES:  imssnow
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


  implicit none

  INTEGER JF, MBUF
  PARAMETER (JF=6144*6144)
!  PARAMETER (JF=1024*1024)
  INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
  LOGICAL*1 LB(JF)   !water/land mask
  REAL, intent(out)::  ICEC(JF)
  REAL, intent(out)::  SNOWC(JF)
!  PARAMETER(MBUF=256*1024)
  PARAMETER(MBUF=256*6144)
  CHARACTER CBUF(MBUF)

  INTEGER :: LUGB,LUGI, J, IRET, K, KF
  character(200):: FNAME1

  INTEGER :: I, iyear, imonth, iday, ihr

!-----------------------------------------------------------------------
  LUGB = 11
  LUGI = 0
  J = 0

!  FNAME1='/public/data/grids/ncep/snow/grib/latest.SNOW_IMS'
  FNAME1='./imssnow'
  call baopen(LUGB,trim(fname1),IRET)
  if ( IRET .ne. 0) then
      write(6,*) 'bad baopen!!! ', IRET
      STOP
  endif

  JPDS=-1
  JGDS=-1
  JPDS(5) = 91  ! ice cover
  call GETGB(LUGB,LUGI,JF,J,JPDS,JGDS,    &
             KF,K,KPDS,KGDS,LB,ICEC,IRET)
  if (IRET .ne. 0) then
    write(6,*) 'bad getgb ', IRET
    STOP
  endif

  JPDS(5) = 238  ! ice cover
  call GETGB(LUGB,LUGI,JF,J,JPDS,JGDS,    &
             KF,K,KPDS,KGDS,LB,SNOWC,IRET)
  if (IRET .ne. 0) then
    write(6,*) 'bad getgb ', IRET
    STOP
  endif

  iyear=2000 + KPDS(8)
  imonth=KPDS(9)
  iday=KPDS(10)
  ihr=KPDS(11)
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
!  do j=1,6144,100
!    write(*,*) (lb(((j-1)*6144 + i)+0.2),i=1,6144,50)
!  enddo
end subroutine 

SUBROUTINE GET_RR_GRID(ifswap,xlon,ylat,nlon,nlat,xland)
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
  integer, intent(in) :: nlon,nlat
  real, intent(out):: xlon(nlon,nlat)    !
  real, intent(out):: ylat(nlon,nlat)    !
  real, intent(out):: xland(nlon,nlat)    !
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
        write(*,*) 'nrecs: ', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, ifswap,trim(filename), nrecs,            &
                 datestr_all,varname_all,memoryorder_all,domainend_all,   &
                 start_block,end_block,start_byte,end_byte,file_offset)

!mhu  do N=1,NRECS
!mhu     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),      &
!mhu           trim(memoryorder_all(n)),domainend_all(:,n)
!mhu  enddo

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
  write(6,*)' MPIIO: landmask, max, min=', maxval(xland),minval(xland)

!                  XLAT
  call retrieve_index(index,'XLAT_M',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,ifswap,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))

  ylat=field2
  write(6,*)' MPIIO: max,min XLAT(:,1)=',&
       maxval(ylat(:,1)),minval(ylat(:,1))
  write(6,*)' MPIIO: max,min XLAT(1,:)=',&
       maxval(ylat(1,:)),minval(ylat(1,:))
  write(6,*)' MPIIO: xlat(1,1),xlat(nlon,1)=',&
       ylat(1,1),ylat(nlon_regional,1)
  write(6,*)' MPIIO: xlat(1,nlat),xlat(nlon,nlat)=', &
       ylat(1,nlat_regional),ylat(nlon_regional,nlat_regional)

!                  XLONG
  call retrieve_index(index,'XLONG_M',varname_all,nrecs)
  if(index<0) stop
  call retrieve_field(iunit,ifswap,trim(filename),field2,start_block(index+1),end_block(index+1),&
                               start_byte(index+1),end_byte(index+1))
  xlon=field2
  write(6,*)' MPIIO: max,min XLONG(:,1)=',&
       maxval(xlon(:,1)),minval(xlon(:,1))
  write(6,*)' MPIIO: max,min XLONG(1,:)=',&
       maxval(xlon(1,:)),minval(xlon(1,:))
  write(6,*)' MPIIO: xlong(1,1),xlong(nlon,1)=',&
       xlon(1,1),xlon(nlon_regional,1)
  write(6,*)' MPIIO: xlong(1,nlat),xlong(nlon,nlat)=', &
       xlon(1,nlat_regional),xlon(nlon_regional,nlat_regional)
!
  close(iunit)
  deallocate(field2)

!
END SUBROUTINE GET_RR_GRID
