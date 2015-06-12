subroutine update_SST_netcdf_mass (mype,ifswap,sstRR,glat, glon, nlon, nlat, &
                                        xland, vegtyp,ilake,iice)
!$$$  documentation block
!                .      .    .                                       .
!   update_SST_binary_mass: read SST from wrf mass binary old background file
!           and update SST in wrf mass background file
!   prgmmr: Ming Hu                 date: 2008-02-25
!   updates: Tanya Smirnova         date: 2010-10-11
!
! program history log:
!
!
!   input argument list:
!       sstRR: sst
!       nlon:  x dimension
!       nlat:  Y dimension
!
! attributes:
!   language: f90
!
!$$$

  use mpi
  use kinds, only: r_single,i_kind,i_llong
  implicit none

  logical,intent(in) :: ifswap
  integer :: mype
  integer :: nlon, nlat,ilake, iice
  real  :: sstRR(nlon,nlat)
  real  :: glat(nlon,nlat)
  real  :: glon(nlon,nlat)
  real  :: xland(nlon,nlat)
  real  :: vegtyp(nlon,nlat)

! Declare local parameters

  integer(i_kind) :: i,j,k
  integer(i_kind) :: l, n
  integer(i_kind) :: ierr, ier

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
  
  character (len=31) :: rmse_var
  integer(i_kind) iyear,imonth,iday,ihour,iminute,isecond
  integer(i_kind) nlon_regional,nlat_regional,nsig_regional
  real(r_single),allocatable::field2(:,:)
  real(r_single),allocatable::field3(:,:,:)
  real(r_single),allocatable::surftemp(:,:)
  real(r_single),allocatable::temp2m(:,:)
  real(r_single),allocatable::sst(:,:)
  real(r_single),allocatable::lu_index(:,:)
  real(r_single),allocatable::landmask_soilmoisture1(:,:)

  integer(i_kind) wrf_real

  real(r_single)    :: time, time1, time2
  real(r_single)    :: a, b

  integer, ALLOCATABLE:: ibuf4(:)

! Lakes from RUC model
! -- Great Salt Lake lake surface temps
        REAL salt_lake_lst (13)
        data salt_lake_lst &
        /1.,3.,6.,13.,17.,20.,26.,25.,20.,14.,9.,3.,1./
! -- Salton Sea - California
        REAL salton_lst (13)
        data salton_lst &
        / 12.8, 12.8, 17.2, &
         21.1, 24.4, 26.7,  &
         30.0, 31.7, 29.4,  &
         25.0, 20.6, 15.0,  &
         12.8/
! -- Lake Champlain - Vermont
        REAL champ_lst (13)
        data champ_lst&
       /  1.3,  0.6,  1.0,  &
          3.0,  7.5, 15.5,  &
         20.5, 21.8, 18.2,  &
         13.0,  8.2,  4.5,  &
          1.3/

        real xc1,yc1, xc2,yc2
        integer isup,jsup, iwin,jwin, isalton,jsalton

      integer julm(13)
      data julm/0,31,59,90,120,151,181,212,243,273,304,334,365/

        INTEGER  mon1,mon2,day1,day2,juld
        real rday,wght1,wght2

!**********************************************************************
!
!            END OF DECLARATIONS....start of program
!
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
  if(mype==0) write(*,*) 'hdrbuf', (hdrbuf(1:20))

!
!  inventory background file
!
  call count_recs_wrf_binary_file(iunit, ifswap,trim(fileName), nrecs)
 if(mype==0)  write(*,*) 'number of records in ',trim(fileName), '=', nrecs

  allocate(datestr_all(nrecs),varname_all(nrecs),domainend_all(3,nrecs))
  allocate(memoryorder_all(nrecs))
  allocate(start_block(nrecs),end_block(nrecs))
  allocate(start_byte(nrecs),end_byte(nrecs),file_offset(nrecs))

  call inventory_wrf_binary_file(iunit, ifswap,trim(filename), nrecs,  &
                      datestr_all,varname_all,memoryorder_all,domainend_all,   &
                      start_block,end_block,start_byte,end_byte,file_offset)
  if(mype==0) then
  do N=1,NRECS
     write(*,'(i4,2x,a30,a5,3i5)') N, trim(varname_all(N)),trim(memoryorder_all(n)),domainend_all(:,n)
  enddo
  endif

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

   if(mype==0) write(*,*) datestr_all(index)
  read(datestr_all(index),'(i4,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)') iyear,imonth,iday,ihour,iminute,isecond
  if(mype==0) write(6,*)' Skin temp data from background file at time:'
  if(mype==0) write(6,*)' iy,m,d,h,m,s=',iyear,imonth,iday,ihour,iminute,isecond

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

  if(mype==0) write(6,*)' nlon,lat,sig_regional=',nlon_regional,nlat_regional,nsig_regional
  allocate(surftemp(nlon_regional,nlat_regional))
  allocate(temp2m(nlon_regional,nlat_regional))
  allocate(sst(nlon_regional,nlat_regional))
  allocate(lu_index(nlon_regional,nlat_regional))
  allocate(landmask_soilmoisture1(nlon_regional,nlat_regional))

  allocate(field2(nlon_regional,nlat_regional))
  
!
  if(mype==0) write(6,*) '================================================='
  VarName='TSK'
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
     if(mype==0) write(6,*)' MPIIO: read in ',VarName
     surftemp=field2(:,:)
  end if
  if(mype==0) write(6,*)' max,min temp=',maxval(surftemp),minval(surftemp)
  if(mype==0) write(6,*)' max,min bck skin temp (K)=',maxval(surftemp),minval(surftemp)
  if(mype==0)      write(6,*)'background skin temp(170,170)', surftemp(170,170)
  if(mype==0)      write(6,*)'new  sstRR(170,170)', sstRR(170,170)

!
  if(mype==0) write(6,*) '================================================='
  VarName='T2'
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
     if(mype==0) write(6,*)' MPIIO: read in ',VarName
     temp2m=field2(:,:)
  end if
  if(mype==0) write(6,*)' max,min 2m temp (K) =', &
                          maxval(temp2m),minval(temp2m)
!
  if(mype==0) write(6,*) '================================================='
  VarName='SST'
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
     if(mype==0) write(6,*)' MPIIO: read in ',VarName
     sst=field2
  end if
  if(mype==0) write(6,*)' max,min bck sst (K)=',maxval(sst),minval(sst)
  if(mype==0)      write(6,*)'background sst(170,170)', sst(170,170)
  if(mype==0)      write(6,*)'new  sstRR(170,170)', sstRR(170,170)
!
  if(mype==0) write(6,*) '================================================='
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
     if(mype==0) write(6,*)' MPIIO: read in ',VarName
     lu_index=field2
  end if
  if(mype==0) write(6,*)' max,min XLAND=',maxval(field2),minval(field2)
!
  if(mype==0) write(6,*) '================================================='
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
     if(mype==0) write(6,*)' MPIIO: read in ',VarName ! use soil mositure to find water =1 water
     if(trim(memoryorder_all(index))=='XZY') then
        landmask_soilmoisture1(:,:)=field3(:,1,:)
     else
        landmask_soilmoisture1(:,:)=field3(:,:,1)
     endif
  end if

! Compute weight for the current date
       juld = julm(imonth) + iday
       if(juld.le.15) juld=juld+365

       mon2 = imonth
       if(iday.gt.15) mon2 = mon2 + 1
       if(mon2.eq.1) mon2=13
       mon1=mon2-1
! **** Assume data valid at 15th of month
       day2=julm(mon2)+15
       day1=julm(mon1)+15
       rday=juld
       wght1=(day2-rday)/float(day2-day1)
       wght2=(rday-day1)/float(day2-day1)
       if(mype==0) write(6,*)'Date weights =',wght1,wght2
!
!  update skin temperature over water
!
if(1==1) then  ! turn off , use GFS SST
! find i,j for a point in northern Lake Superior
  DO J=1,nlat
  DO I=1,nlon
   if((glat(i,j)>48.4 .and. glat(i,j)<49.6) .and. (glon(i,j)<-87.9 &
      .and. glon(i,j)>-88.1)) then
     isup=i
     jsup=j
     if(mype==0) print *,' Lake Superior --> i,j,glat(i,j),glon(i,j)', &
                    i,j,glat(i,j),glon(i,j), &
     'vegtyp(i,j)=',vegtyp(i,j),'lu_index(i,j)',lu_index(i,j),xland(i,j)
     goto 99
   endif
  ENDDO
  ENDDO

99  continue

! find i,j for a point in northern Lake Winnipeg
  DO J=1,nlat
  DO I=1,nlon
   if((glat(i,j)>53.3 .and. glat(i,j)<53.7) .and. (glon(i,j)<-98.3 &
       .and. glon(i,j)>-98.7)) then
     iwin=i
     jwin=j
     if(mype==0) print *,' Lake Winnipeg --> i,j,glat(i,j),glon(i,j)', &
                    i,j,glat(i,j),glon(i,j), &
     'vegtyp(i,j)=',vegtyp(i,j),'lu_index(i,j)',lu_index(i,j),xland(i,j)
     goto 999
   endif
  ENDDO
  ENDDO

999  continue

  DO J=1,nlat
  DO I=1,nlon
!    if( abs(landmask_soilmoisture1(i,j) -1.0) < 0.00001 ) then    ! water
    if( xland(i,j) < 0.00001 ) then    ! water, could be sea ice
!        if(abs(lu_index(i,j)- 24) > 0.01) then  !USGS ice = 24
        if(abs(lu_index(i,j) - iice) > 0.01) then    ! MODIS ice = 15
! only unfrozen water points (sea or lakes)
!
!     if(vegtyp(i,j)==ilake .and. (sstRR(i,j)-temp2m(i,j)).lt.-5) then
! correct lake temperature when it is too cold, excluding the Great Lakes
!        IF (.not.(GLAT(i,j).LT.50..AND.GLAT(i,j).GT.40. .AND.   &
!        glon(i,j).LT.-74..AND.glon(i,j).GT.-94.)) then
!          print *,'corrected lake sstRR at i,j =,
!          sstRR,temp2m,vegtyp',i,j,sstRR(i,j),temp2m(i,j)
!              sstRR(i,j)=max(273.15,sstRR(i,j) + 0.75*(temp2m(i,j)-sstRR(i,j)))
!        ENDIF  ! no Great Lakes
!     endif  ! lakes correction

       if(vegtyp(i,j)==ilake ) then
! --- Great Salt Lake, Utah Lake -- Utah
            if (glat(i,j).gt.39.5 .and. glat(i,j).lt.42. .and.  &
               glon(i,j).gt.-114..and. glon(i,j).lt.-111.) then
           write(6,*)'Global data Salt Lake temp',i,j,sstRR(i,j)
            sstRR(i,j) = 273.15 + wght1*salt_lake_lst(mon1)  &
                       +wght2*salt_lake_lst(mon2)
            write(6,*)'Climatology Salt Lake temp',i,j,sstRR(i,j)  &
                ,glat(i,j),glon(i,j)
            end if

! --- Salton Sea -- California
            if (glat(i,j).gt.33. .and. glat(i,j).lt.33.7 .and.  &
                glon(i,j).gt.-116.3 .and. glon(i,j).lt.-115.3) then
            write(6,*)'Global data Salton Sea temp',i,j,sstRR(i,j)
            sstRR(i,j) = 273.15 + wght1*salton_lst(mon1)  &
                       +wght2*salton_lst(mon2)
            write(6,*)'Climatology Salton Sea temp',i,j,sstRR(i,j)  &
                ,glat(i,j),glon(i,j)
              isalton=i
              jsalton=j
            end if

! --- Lake Champlain -- Vermont
            if (glat(i,j).gt.44. .and. glat(i,j).lt.45.2 .and.  &
               glon(i,j).gt.-74. .and. glon(i,j).lt.-73.) then
            write(6,*)'Global data Lake Champlain temp',i,j,sstRR(i,j)
            sstRR(i,j) = 273.15 + wght1*champ_lst(mon1)  &
                       +wght2*champ_lst(mon2)
            write(6,*)'Climatology Lake Champlain temp',i,j,sstRR(i,j)  &
                ,glat(i,j),glon(i,j)
            end if
! --- For Lake Nipigon, use point for n. Lake Superior
!   -- Lake Nipigon is deep!
            if (glat(i,j).gt.49. .and. glat(i,j).lt.51. .and. &
               glon(i,j).gt.-90. .and. glon(i,j).lt.-87.) then
               write(6,*)'Global data Lake Nipigon temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(isup,jsup)
                write(6,*)'Lake Nipigon temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if

     if(1 == 2) then
! --- For Lake of the Woods and other
!      Minnesota lakes, use point for n. Lake Winnipeg
!    -- These lakes are NOT DEEP!
            if (glat(i,j).gt.46. .and. glat(i,j).lt.50. .and.  &
               glon(i,j).gt.-96. .and. glon(i,j).lt.-93.) then
                write(6,*)'Global data Minnesota lake temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(iwin,jwin)
                write(6,*)'Minnesota lake temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if

! --- For Canadian lakes, including Winnipeg, Manitoba, Winnipegosis,
!      use point for n. Lake Winnipeg
!    -- These lakes are NOT DEEP!
            if (glat(i,j).gt.50. .and. glat(i,j).lt.68. .and.  &
               glon(i,j).gt.-148. .and. glon(i,j).lt.-48.) then
                write(6,*)'Global data Canadian lake temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(iwin,jwin)
                write(6,*)'Canadian lake temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if
! --- For lakes in Washington, Oregon, Nevada
!      use point for n. Lake Winnipeg (?????)
!    -- These lakes are NOT DEEP!
            if (glat(i,j) > 33.8 .and. glat(i,j) < 50. .and.  &
               glon(i,j) < -114. ) then
                write(6,*)'Global data US west lake temp',i,j,sstRR(i,j)
                sstRR(i,j) = sstRR(iwin,jwin)
!                sstRR(i,j) = sstRR(isalton,jsalton)
                write(6,*)'US west lake temp',i,j,sstRR(i,j) &
                 ,glat(i,j),glon(i,j)
            end if

     endif ! 1 == 2
      endif   ! lakes

!update skin temp
              surftemp(i,j)=sstRR(i,j)
        else
! frozen water - MODIS type = 15
        if(lu_index(i,j) == iice .and. vegtyp(i,j)==ilake .and. & 
           sstRR(i,j) > 273.) then
!           print *,'Ice lake cannnot have SST > 273K'
!           print *,'i,j,sstRR,lu_index,vegtyp ='
!           ,i,j,sstRR(i,j),lu_index(i,j),vegtyp(i,j)
! set skin temp of frozen lakes to 2-m temperature
              sstRR(i,j)= min(273.15,temp2m(i,j))
        endif
        endif  ! MODIS ice = 15

    endif  ! water and ice
        sst(i,j)=sstRR(i,j)
  ENDDO
  ENDDO
  if(mype==0) write(*,*) 'Skin temperature updated with current SST'
  if(mype==0)      write(6,*)'updated skin temp(170,170)', surftemp(170,170)
  if(mype==0)      write(6,*)'updated sst(170,170)', sst(170,170)
endif

  call MPI_BARRIER(mpi_comm_world,ierr)
!
!
!           update mass core binary file with new SST
!
  if(mype==0) write(6,*) ' ============================= '
  if(mype==0) write(6,*) ' update SST in background file '
  if(mype==0) write(6,*) ' ============================= '
     
  if(mype==0) write(6,*) '================================================='
  field2=surftemp
  if(mype==0) write(6,*)' max,min skin temp =',maxval(field2),minval(field2)
  VarName='TSK'
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
     if(mype==0) write(6,*)' MPIIO: write out ',VarName
     if(mype==0) write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  if(mype==0) write(6,*) '================================================='
  field2=sst
  if(mype==0) write(6,*)' max,min sst =',maxval(field2),minval(field2)
  VarName='SST'
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
     if(mype==0) write(6,*)' MPIIO: write out ',VarName
     if(mype==0) write(6,*)' max,min=',maxval(field2),minval(field2)
  end if

  deallocate(field2)
  deallocate(field3)

  call mpi_file_close(iunit,ierr)
     
end subroutine update_SST_netcdf_mass
