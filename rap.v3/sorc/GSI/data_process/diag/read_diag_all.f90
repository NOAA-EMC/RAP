      PROGRAM read_gsi 
! Code based on original work by Susan Sahm
! with additions by Dezso Devenyi
! 27 May 2009
! Purpose: reading diagnostic conv_0* files produced by GSI
! Remarks:
! 1) in present version dirname is fixed (conv_02). In real time
!    application it can be given from script.
! 2) in present version big_endian commented out
! 3) number or processes is hardwired to 76 (see 75 in do loop)
!    In real time application it can be given from script.
! Present usage: read_gsi > output file
! It can be mofified to output results any text file readable by Bill
! in his graphical system.

      real,allocatable,dimension(:,:)::rdiagbuf
      character(8),allocatable,dimension(:):: cdiagbuf
      character(8) tmp
      integer i,nchar,nreal,k,j,ii,it,jj,l,nn,mype,nrec,iuse
      character(3) var
      character ch
      real rlat,rlon,rprs,robs1,rdpt1,robs2,rdpt2,ruse,rerr
      real rdhr
      real heig1,heig2,heig3,ddiff   !DEDE 19 Feb 2009
      integer itype, ic,iii
      character(6) string
      character(14) diag_file
      character(8) dirname

       dirname=".conv_03"
!       print*, dirname

       do 23 iii=0,75
!        print*,'value of iii = ',iii
           write(string,900) iii
900     format("pe00",i2.2)
!        print*,string
           diag_file=string//dirname 
!        print*,diag_file

      OPEN (7,FILE=diag_file,STATUS='OLD',ERR=998,
     1     ACCESS='SEQUENTIAL',
!     1     convert= 'big_endian',
     1     FORM='UNFORMATTED')
       nchar = 1
      do while (nchar > 0)
        read(7, ERR=999)var, nchar,nreal,nrec,mype
!        write (*,'(A4,I3,I5,I5,I5)') var ,nchar, nreal, nrec,mype
        if (nrec > 0) then
          allocate(cdiagbuf(nrec),rdiagbuf(nreal,nrec))
          read(7)cdiagbuf, rdiagbuf
          do i=1,nrec
             itype=rdiagbuf(1,i)
             rlat=rdiagbuf(3,i)
             rlon=rdiagbuf(4,i)
!             heig1=rdiagbuf(5,i)
             rprs=rdiagbuf(6,i)
!             heig2=rdiagbuf(7,i)
             rdhr=rdiagbuf(8,i)
             ruse=rdiagbuf(12,i)
             ddiff=rdiagbuf(18,i)
!             heig3=rdiagbuf(19,i)
             iuse = int(ruse)
!             if(iuse == -1) goto 78
             rerr = 0
             if (rdiagbuf(16,i) > 0) then
               rerr=1.0/rdiagbuf(16,i)
             end if 
           robs1=rdiagbuf(17,i)
           rdpt1=rdiagbuf(18,i)
C   When the data is q, unit convert kg/kg -> g/kg **/
          if (var == "  q") then
              robs1 = robs1 * 1000.0
              rdpt1 = rdpt1 * 1000.0
              rerr = rerr * 1000.0
          end if

C   When the data is pw, replase the rprs to -999.0 **/
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
        end do
         if (var .ne. " uv") then 
          write (*,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,
     +              F8.2,I3,F8.2,F8.2)') var,
     +              tmp,itype, rdhr,
     +              rlat,rlon,rprs,iuse,robs1,rdpt1
         else
          write (*,'(A3," @ ",A8," : ",I3,F6.2,F8.2,F8.2,
     +              F8.2,I3,F8.2,F8.2$)') var,
     +              tmp,itype, rdhr,
     +              rlat,rlon,rprs,iuse,robs1,rdpt1
         endif

C  ** When the data is uv, additional output is needed **/
         if (var == " uv") then 
           robs2=rdiagbuf(20,i)
           rdpt2=rdiagbuf(21,i);
           write (*,'( F8.2,F8.2)') robs2, rdpt2
        endif
 78     continue       
        end do ! number of records
          deallocate(cdiagbuf,rdiagbuf)
       end if  ! nrec > 0
      end do  ! nchar > 0

       call exit(1)
998    PRINT *,'error opening input file'
       call exit(1)
999    continue
 23     end do
          END 
