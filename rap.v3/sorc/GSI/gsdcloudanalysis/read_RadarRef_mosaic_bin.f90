subroutine read_RadarRef_mosaic_bin(nread,ndata,infile,obstype,lunout,twind,sis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_RadarRef_mosaic     Reading in reflectivity mosaic in RR grid
!
!   PRGMMR: Ming Hu          ORG: NP22        DATE: 2006-03-27
!
! ABSTRACT: 
!     This routine read in reflectivity mosaic data.  The data has already
!          been interpolated into analysis grid and in form of BUFR.
!
! PROGRAM HISTORY LOG:
!    2008-12-20  Hu  make it read in BUFR form reflectivity  data
!    2010-04-09  Hu  make changes based on current trunk style
!    2013-03-27  Hu  add code to map obs from WRF mass H grid to analysis grid
!
!   input argument list:
!     infile   - unit from which to read mosaic information file
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     twind    - input group time window (hours)
!     sis      - observation variable name
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!
! USAGE:
!   INPUT FILES:  refInGSI
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  Linux cluster(Wjet)
!
!$$$
!
!_____________________________________________________________________
!
  use kinds, only: r_kind,r_double,i_kind,r_single
  use constants, only: zero,one
  use convinfo, only: nconvtype,ctwind,cgross,cermax,cermin,cvar_b,cvar_pg, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,icsubtype,ioctype
  use gsi_4dvar, only: l4dvar,winlen
  use gridmod, only: nlon,nlat,nlon_regional,nlat_regional
  use mod_wrfmass_to_a, only: wrfmass_obs_to_a8

  implicit none
!

  character(10),    intent(in)    :: infile,obstype
  integer(i_kind),  intent(in)    :: lunout
  integer(i_kind),  intent(inout) :: nread,ndata
  real(r_kind),     intent(in   ) :: twind
  character(20),    intent(in)    :: sis
!
!  For reflectiivty mosaic
!
  integer(i_kind) nreal,nchanl

  integer(i_kind) ifn,i,j
 
  real(r_kind)  :: maxref
  integer(i_kind) :: ilon,ilat

  logical :: nsslrefobs
!
!  for read in bufr 
!
    character(8) subset,sid
    integer(i_kind)  :: lunin,idate
    integer(i_kind)  :: ireadmg,ireadsb

    INTEGER(i_kind)  ::  maxlvl
    INTEGER(i_kind)  ::  numlvl,numref,numobsa
    INTEGER(i_kind)  ::  n,k,iret
    INTEGER(i_kind)  ::  nmsg,ntb
    INTEGER(i_kind),PARAMETER  ::  maxobs=2000000

    REAL(r_kind),allocatable :: ref3d_column(:,:)   ! 3D reflectivity in column

    REAL(r_single), allocatable :: ref0(:,:,:)   ! 3D reflectivity
    INTEGER(i_kind) ::   ref_maxlvl,ref_nlon,ref_nlat

    integer(i_kind)  :: ikx
    real(r_kind)     :: timeo,t4dv

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
   nsslrefobs = .false.
   ikx=0
   do i=1,nconvtype
       if(trim(obstype) == trim(ioctype(i)) .and. abs(icuse(i))== 1) then
           nsslrefobs=.true.
           ikx=i
       endif
   end do

   nread=0
   ndata=0
   nchanl=0
   ifn = 15
   ntb=0

   if(nsslrefobs) then
      lunin = 10            
      maxlvl= 31
      allocate(ref3d_column(maxlvl+2,maxobs))
      ref3d_column=-999.0

      OPEN  ( UNIT = lunin, FILE = trim(infile),form='unformatted',err=200, &
              convert='LITTLE_ENDIAN')
      read(lunin)  ref_maxlvl,ref_nlon,ref_nlat
      write(*,*)  'read mosaic ==',ref_maxlvl,ref_nlon,ref_nlat
      allocate( ref0(nlon,nlat,maxlvl) )
      read(lunin) ref0
      close(lunin)
      
      DO j=2,ref_nlat-1
      DO i=2,ref_nlon-1
        numlvl=0
        DO k=1,maxlvl
          if(abs(ref0(i,j,k)) < 888.0 ) numlvl=numlvl+1
        ENDDO
        if(numlvl > 0 ) then
          ntb=ntb+1
          ref3d_column(1,ntb)=float(i)
          ref3d_column(2,ntb)=float(j)
          DO k=1,maxlvl
             ref3d_column(2+k,ntb)=ref0(i,j,k)
          ENDDO
        endif
      ENDDO
      ENDDO

      deallocate(ref0)

      nmsg=ntb
      write(6,*)'read_RadarRef_mosaic: messages/reports = ',nmsg,'/',ntb
      numref=ntb
!
!   write
!
      ilon=1
      ilat=2
      nread=numref
      ndata=numref
      nreal=maxlvl+2
      if(numref > 0 ) then
         if(nlon==nlon_regional .and. nlat==nlat_regional) then
            write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
            write(lunout) ((ref3d_column(k,i),k=1,maxlvl+2),i=1,numref)
         else
            call wrfmass_obs_to_a8(ref3d_column,nreal,numref,ilat,ilon,numobsa)
            nread=numobsa
            ndata=numobsa
            write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
            write(lunout) ((ref3d_column(k,i),k=1,maxlvl+2),i=1,numobsa)
         endif
         deallocate(ref3d_column)
      endif
    endif
 
    call closbf(lunin)
    return
200 continue
    write(6,*) 'read_RadarRef_mosaic, Warning : cannot find radar data file'

end subroutine read_RadarRef_mosaic_bin
!
!
