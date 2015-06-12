program bufr_decode_l2rwbufr
!
! 10/17/2013             DTC
!
! Read all level 2 radar radial velocity obs out from NCEP Level II radial
! velocity bufr. Also, read and write bufr table for radar from the file.
!
! MNEMONIC used in this code
!    SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW
!    YEAR MNTH DAYS HOUR MINU SECO
!    SCID HNQV VOCP VOID
!    DIST125M DMVR DVSW
! Please refer BUFR table for explanations of MNEMONIC
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=1000
 character(80):: hdstr= 'SSTN CLAT CLON HSMSL HSALG ANEL ANAZ QCRW'
 character(80):: hdstr2='YEAR MNTH DAYS HOUR MINU SECO HDR' 
 character(80):: hdstr3='SCID HNQV VOCP VOID'
 character(80):: obstr='DIST125M DMVR DVSW'
 real(8) :: hdr(mxmn),hdr2(mxmn),hdr3(mxmn),obs(3,mxlv)

 INTEGER        :: ireadmg,ireadsb

 character(8)   :: subset
 integer        :: unit_in=10,unit_out=20
 integer        :: idate,nmsg,ntb
 integer        :: i,k,iret

 character(8)   :: c_sid
 real(8)        :: rstation_id
 equivalence(rstation_id,c_sid)

 integer        :: numrwbin
!
 integer,dimension(5):: idate5cyc 
 integer,dimension(5):: idate5obs 
 integer:: mincyc,minobs,diffmin
 integer,dimension(50):: sumbin, timebin
 integer :: startbin,binnum
!
!
!
 startbin=-90
 timebin(1)=startbin
 do i=1,49
   timebin(i+1)=timebin(i)+5
 enddo
 sumbin=0
!
 open(unit_out,file='l2rwbufr.bin',form='unformatted',status='unknown')

 open(24,file='bufr_radar.table')
 open(unit_in,file='l2rwbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)
 call datelen(10)
   nmsg=0
   ntb = 0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
!     write(*,*)
!     write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate

     if(nmsg==1) then
        write(*,'(3a,i10)') 'subset=',subset,' cycle time =',idate
        idate5cyc(1)=idate/1000000
        idate5cyc(2)=idate/10000 - idate5cyc(1)*100
        idate5cyc(3)=idate/100 - idate5cyc(1)*10000 - idate5cyc(2)*100
        idate5cyc(4)=idate - idate5cyc(1)*1000000 - idate5cyc(2)*10000 - &
                  idate5cyc(3)*100
        idate5cyc(5)=0
!        write(*,*) 'cycle time=',idate,idate5cyc,mincyc
        call w3fs21(idate5cyc,mincyc)
     endif

     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       call ufbint(unit_in,hdr ,mxmn,1  ,iret,hdstr)
       call ufbint(unit_in,hdr2,mxmn,1  ,iret,hdstr2)
       call ufbint(unit_in,hdr3,mxmn,1  ,iret,hdstr3)
       rstation_id=hdr(1)
!       write(*,*)
!       write(*,'(2I10,a14,10f8.1)') ntb,iret,c_sid,(hdr(i),i=2,8)
!       write(*,'(10f8.1)') (hdr2(i),i=1,6)
!       write(*,'(I10,a14,10f6.1)') ntb,c_sid,(hdr2(i),i=1,6)
       idate5obs(1)=hdr2(1)
       idate5obs(2)=hdr2(2)
       idate5obs(3)=hdr2(3)
       idate5obs(4)=hdr2(4)
       idate5obs(5)=hdr2(5)
!       write(*,*) idate5obs
       call w3fs21(idate5obs,minobs)
       diffmin=minobs-mincyc
       binnum=(diffmin-startbin)/5+1
       if(binnum < 0 .or. binnum > 50 ) then
         write(*,*) binnum,diffmin,idate5obs
         write(*,*) 'obs out of boundary'
         stop
       endif
!       write(*,*) binnum,diffmin,idate5obs
       sumbin(binnum)=sumbin(binnum)+1
if(1==2) then
       call ufbint(unit_in,obs ,3,  mxlv,iret,obstr)
       if(iret >= 1) then
          numrwbin=0
          do i=1,iret
             if(obs(2,i) < 10000.0) numrwbin=numrwbin+1
          enddo
          if(numrwbin>=1) then
             ntb = ntb+1
!             write(unit_out,'(2I10,a14,10f8.1)') ntb,numrwbin,c_sid,(hdr(i),i=2,8)
!             write(unit_out,'(34x,10f8.1)') (hdr2(i),i=1,6)
             write(unit_out) ntb,numrwbin,(hdr(i),i=1,8)
             write(unit_out) (hdr2(i),i=1,6)
             write(unit_out) (hdr3(i),i=1,4)
          endif
          do i=1,iret
             if(obs(2,i) < 10000.0) & 
!                write(*,'(a10,i10,5f16.2)') 'rw obs=',i,obs(:,i)
                write(unit_out) obs(1:3,i)
          enddo
       endif
endif
     enddo sb_report
   enddo msg_report
 call closbf(unit_in)

 close(unit_out)

 write(*,*) 'message=',nmsg,'  subset=',ntb
 write(*,*) 'Time range of the observations'
 write(*,'(50I10)') timebin(11:22)
 write(*,'(50I10)') sumbin(11:22)

end program
