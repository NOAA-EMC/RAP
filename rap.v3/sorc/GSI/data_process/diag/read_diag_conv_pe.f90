PROGRAM read_diag_conv
!
!  This is to read GSI conventional diagnositic data from subroutine set*
!
!      ps
!      t
!      q
!      pw
!      uv
!      sst
!      gps
!
!  For example in setupt.f90:
!     write(7)'  t',nchar,nreal,ii,mype
!     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
!        cdiagbuf(ii)       ! station id
!        rdiagbuf(1,ii)     ! observation type
!        rdiagbuf(2,ii)     ! observation subtype
!        rdiagbuf(3,ii)     ! observation latitude (degrees)
!        rdiagbuf(4,ii)     ! observation longitude (degrees)
!        rdiagbuf(5,ii)     ! station elevation (meters)
!        rdiagbuf(6,ii)     ! observation pressure (hPa)
!        rdiagbuf(7,ii)     ! observation height (meters)
!        rdiagbuf(8,ii)     ! obs time (hours relative to analysis time)
!        rdiagbuf(9,ii)     ! input prepbufr qc or event mark
!        rdiagbuf(10,ii)    ! setup qc or event mark (currently qtflg only)
!        rdiagbuf(11,ii)    ! read_prepbufr data usage flag
!        rdiagbuf(12,ii)    ! analysis usage flag (1=use, -1=not used)
!        rdiagbuf(13,ii)    ! nonlinear qc relative weight
!        rdiagbuf(14,ii)    ! prepbufr inverse obs error (K**-1)
!        rdiagbuf(15,ii)    ! read_prepbufr inverse obs error (K**-1)
!        rdiagbuf(16,ii)    ! final inverse observation error (K**-1)
!        rdiagbuf(17,ii)    ! temperature observation (K)
!        rdiagbuf(18,ii)    ! obs-ges used in analysis (K)
!        rdiagbuf(19,ii)    ! obs-ges w/o bias correction (K) (future slot)
!

  use kinds, only: r_kind,r_single,i_kind

  implicit none

  real(r_kind) tiny_r_kind
!
! read in variables
!
  character(8),allocatable,dimension(:):: cdiagbuf
  real(r_single),allocatable,dimension(:,:)::rdiagbuf
  integer(i_kind) nchar,nreal,ii,mype
  integer(i_kind) idate
!
!  namelist files
!
  character(180) :: dirname           ! GSI running directory
  character(80) ::  outfilename       ! file name saving results
  integer :: numpe                    ! processor numbers used to run GSI
                                      ! Note, we read file in each processor instead of combined one
  integer :: nloop(5)                 ! which outer loop want to read
  namelist/iosetup/ dirname, outfilename,numpe,nloop
!
! output variables
!
  character(len=3)  :: var
  real :: rlat,rlon,rprs,robs1,rdpt1,robs2,rdpt2,ruse,rerr
  real :: rdhr, ddiff
  character(8) :: tmp
  integer :: itype,iuse,iusev
!
!  misc.
!
  character(180) ::  infilename
  character(10) ::  cipe,cloop
  character ::  ch
  integer :: ipe,i,j,k,ios, iloop
  integer :: ic, iflg

  integer,dimension(300):: imap_ps,imap_t,imap_q,imap_pw,imap_sst,imap_uv
!
!  tiny_r_kind = tiny(0)
!
  call convinfo_read(imap_ps,imap_t,imap_q,imap_pw,imap_sst,imap_uv)
!
  dirname='./'
  outfilename='diag_results'
  numpe=64
  nloop=0
  open(11,file='namelist.conv')
   read(11,iosetup)
  close(11)
!
  DO iloop = 1, 5
  if(nloop(iloop) > 0 ) then
  write(cloop,'(I2.2)') nloop(iloop) 
  open(42, file=trim(outfilename)//".conv_"//trim(cloop),IOSTAT=ios)
  if(ios > 0 ) then
       write(*,*) 'LOOP:',iloop,' cannot open file ', trim(outfilename)//".conv_"//trim(cloop)
       cycle
  else
       write(*,*) 'LOOP:',iloop,' open file ', trim(outfilename)//".conv_"//trim(cloop)
  endif
!
  DO ipe=0, numpe-1
!  DO ipe=20,20
     write(cipe,'(I4.4)')  ipe
     infilename=trim(dirname)//"/pe"//trim(cipe)//".conv_"//trim(cloop)
     write(*,'(I4,a)') ipe, trim(infilename)
     OPEN (17,FILE=trim(infilename),STATUS='OLD',IOSTAT=ios,ACCESS='SEQUENTIAL',  &
             FORM='UNFORMATTED' )
     if(ios > 0 ) then
       write(*,*) ' no diag file availabe at', ipe
       cycle
     endif
     if(ipe==0) then
        read(17, ERR=999) idate
        write(*,*) 'process date: ',idate
     endif
100  continue
     read(17, ERR=999,end=110) var, nchar,nreal,ii,mype
     write(*,*) var, nchar,nreal,ii,mype
     if (ii > 0) then
          allocate(cdiagbuf(ii),rdiagbuf(nreal,ii))
          read(17,ERR=999,end=110) cdiagbuf, rdiagbuf
          do i=1,ii
             itype=rdiagbuf(1,i)    ! observation type
             rlat=rdiagbuf(3,i)     ! observation latitude (degrees)
             rlon=rdiagbuf(4,i)     ! observation longitude (degrees)
             rprs=rdiagbuf(6,i)     ! observation pressure (hPa)
             rdhr=rdiagbuf(8,i)     ! obs time (hours relative to analysis time)
             iuse=int(rdiagbuf(12,i))    ! analysis usage flag (1=use, -1=monitoring ) 
             iusev=int(rdiagbuf(11,i))    ! analysis usage flag ( value ) 
             ddiff=rdiagbuf(18,i)   ! obs-ges used in analysis (K)
             rerr = 0
             if (rdiagbuf(16,i) > 0) then   ! final inverse observation error (K**-1)
               rerr=1.0/rdiagbuf(16,i)
             end if 
             robs1=rdiagbuf(17,i)    !  observation (K)
             rdpt1=rdiagbuf(18,i)    !  obs-ges used in analysis 
! check use
             if(iuse == -1 ) then          
                if( iusev  == 0 ) then 
                  iuse = -2      ! use and rejection because of obs error 
                elseif( iusev  == 100 ) then 
                  iuse = -3      !  monitor  with good quality
                  if( rerr < 1.0e-12) iuse = -4   ! monitor but rejection because of obs error
                elseif( iusev  == 150 ) then 
                  iuse = -5          ! use  and bad flag 
                elseif( iusev  == 160 ) then 
                  iuse = -6          ! monitor  and bad flag 
                else
                  write(6,*) ' unknow iusev, check',iuse,iusev,rerr 
                endif
             endif
!
!   When the data is q, unit convert kg/kg -> g/kg **/
             if (var == "  q") then
                robs1 = robs1 * 1000.0
                rdpt1 = rdpt1 * 1000.0
                rerr = rerr * 1000.0
            end if
!   When the data is pw, replase the rprs to -999.0 **/
           if (var == " pw") then
                 rprs=-999.0;
           end if
           tmp = cdiagbuf(i)
           iflg = 0
           do ic=8,1,-1
            ch = tmp(ic:ic)
            if (ch > ' ' .and. ch <= 'z') then
              iflg = 1
            else
               tmp(ic:ic) = ' '
            end if
            if (ch == ' '  .and. iflg == 1) then
               tmp(ic:ic) = '_'
            endif 
           enddo
!
!  write out result for one variable on one pitch
           if (var .ne. " uv") then 
              write (42,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,   &
                    F8.2,I3,F8.2,F8.2,F15.1,I5)') var,tmp,itype, rdhr,   &
                    rlat,rlon,rprs,iuse,robs1,rdpt1 ! ,rerr,iusev
           else
              write (42,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,   &
                    F8.2,I3,F8.2,F8.2$)') var, tmp,itype, rdhr, &
                    rlat,rlon,rprs,iuse,robs1,rdpt1
           endif

!  ** When the data is uv, additional output is needed **/
           if (var == " uv") then 
             robs2=rdiagbuf(20,i)
             rdpt2=rdiagbuf(21,i);
             write (42,'(F8.2,F8.2,F15.1,I5)') robs2, rdpt2 !,rerr,iusev
           endif

          enddo   ! i  end for one station

          deallocate(cdiagbuf,rdiagbuf)
     else
        read(17)
     endif
     goto 100  ! goto another variable
110  continue

    close(17)

  ENDDO  ! ipe

  close(42)
  write(*,*) 'Close 42 now'

  ENDIF ! do this loop
  ENDDO ! iloop ; end of out loop

  STOP 9999

999    PRINT *,'error read in diag file'
      stop 1234

END PROGRAM read_diag_conv

  subroutine convinfo_read(imap_ps,imap_t,imap_q,imap_pw,imap_sst,imap_uv)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    convinfo_read      read conventional information file
!
    character(len=1)cflg
    character(len=16) cob
    character(len=7) iotype
    character(len=120) crecord
    integer lunin,i,n,nc,ier,istat
    integer nlines,maxlines

    character(len=16),allocatable, dimension(:)::ioctype
    integer,allocatable,dimension(:):: icuse,ictype,icsubtype
    integer,dimension(300):: imap_ps,imap_t,imap_q,imap_pw,imap_sst,imap_uv

    imap_ps=-10
    imap_t=-10
    imap_q=-10
    imap_pw=-10
    imap_sst=-10
    imap_uv=-10
    lunin = 47
    open(lunin,file='convinfo',form='formatted')
    rewind(lunin)
    nconvtype=0
    nlines=0
    read1: do
      read(lunin,1030,err=333, end=300)cflg,iotype
1030  format(a1,a7,2x,a120)
      nlines=nlines+1
      if(cflg == '!')cycle
      nconvtype=nconvtype+1
    enddo read1

300 continue

    if(nconvtype == 0) then
       write(6,*) 'CONVINFO_READ: NO CONVENTIONAL DATA USED'
       return
    endif

    allocate(icuse(nconvtype),ictype(nconvtype),icsubtype(nconvtype), &
             ioctype(nconvtype))

    rewind(lunin)
    nc=0
    do i=1,nlines
       read(lunin,1030)cflg,iotype,crecord
       if(cflg == '!')cycle
       nc=nc+1
       ioctype(nc)=iotype
           !otype   type isub iuse 
           !ps       120    0    1 
 !ioctype(nc),
           !  ictype(nc),
           !     icsubtype(nc),
           !              icuse(nc),

       read(crecord,*)ictype(nc),icsubtype(nc),icuse(nc)
!       write(6,1031)ioctype(nc),ictype(nc),icsubtype(nc),icuse(nc)
1031   format('READ_CONVINFO: ',a7,1x,i3,1x,i4,1x,i2,1x,g12.6)
       if(trim(ioctype(nc)) == 'ps') imap_ps(ictype(nc))=icuse(nc)
       if(trim(ioctype(nc)) == 't') imap_t(ictype(nc))=icuse(nc)
       if(trim(ioctype(nc)) == 'q') imap_q(ictype(nc))=icuse(nc)
       if(trim(ioctype(nc)) == 'pw') imap_pw(ictype(nc))=icuse(nc)
       if(trim(ioctype(nc)) == 'sst') imap_sst(ictype(nc))=icuse(nc)
       if(trim(ioctype(nc)) == 'uv') imap_uv(ictype(nc))=icuse(nc)

    enddo

    close(lunin)
!    DO i =1, 300
!    write(*,'(10I4)') i, imap_t(i),imap_q(i),imap_pw(i),imap_sst(i),imap_uv(i)
!    enddo

    return
333 continue
    write(*,*) ' error in read'
    stop 1234
  end subroutine convinfo_read
