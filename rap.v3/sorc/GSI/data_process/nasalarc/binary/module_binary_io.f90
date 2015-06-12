subroutine count_recs_wrf_binary_file(in_unit,wrfges,nrecs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    count_recs_binary_file  count # recs on wrf binary file
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: count number of sequential records contained in wrf binary
!             file.  this is done by opening the file in direct access
!             mode with block length of 2**20, the size of the physical
!             blocks on ibm "blue" and "white" machines.  for optimal
!             performance, change block length to correspond to the
!             physical block length of host machine disk space. 
!             records are counted by looking for the 4 byte starting
!             and ending sequential record markers, which contain the
!             record size in bytes.  only blocks are read which are known
!             by simple calculation to contain these record markers.
!             even though this is done on one processor, it is still
!             very fast, and the time will always scale by the number of
!             sequential records, not their size.  this step and the
!             following inventory step consistently take less than 0.1 seconds
!             to complete.
!
! program history log:
!   2004-11-29  parrish
!   2005-02-17  todling, ifdef'ed wrf code out
!   2006-04-06  middlecoff - replace fortran open,close with openfileread,closefile
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrfges - filename of input wrf binary restart file
!
!   output argument list:
!     nrecs            - number of sequential records found on input wrf binary restart file.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

!   do an initial read through of a wrf binary file, and get total number of sequential file records

  use kinds, only: i_byte,i_long,i_llong,i_kind
  implicit none

  integer(i_kind),intent(in   ) :: in_unit
  character(9)   ,intent(in   ) :: wrfges
  integer(i_kind),intent(  out) :: nrecs

  character(10) cwrfges
  integer(i_llong) nextbyte,locbyte,thisblock
  integer(i_byte) lenrec4(4)
  integer(i_long) lenrec,lensave
  equivalence (lenrec4(1),lenrec)
  integer(i_byte) missing4(4)
  integer(i_long) missing
  equivalence (missing,missing4(1))
  integer(i_llong),parameter:: lrecl=2**12_i_llong
  integer(i_byte) buf(lrecl)
  integer(i_kind) i,loc_count,nreads
  logical lastbuf
  integer(i_kind) ierr

  cwrfges = wrfges
  cwrfges(10:10) = char(0)
  call openfileread (in_unit, ierr, cwrfges)
! open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  nrecs=0
  missing=-9999_i_long
  nextbyte=0_i_llong
  locbyte=lrecl
  nreads=0
  lastbuf=.false.
  do

!    get length of next record

     do i=1,4
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
        lenrec4(i)=buf(locbyte)
     end do
     if(lenrec <= 0_i_long .and. lastbuf) go to 900
     if(lenrec <= 0_i_long .and. .not.lastbuf) go to 885
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)

     nrecs=nrecs+1
    
     loc_count=1
     do i=2,4
        if(loc_count>=lenrec) exit
        loc_count=loc_count+1
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     end do
     do i=1,4
        if(loc_count>=lenrec) exit
        loc_count=loc_count+1
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     end do
     nextbyte=nextbyte-loc_count+lenrec
     locbyte=locbyte-loc_count+lenrec
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     lensave=lenrec
     do i=1,4
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
        lenrec4(i)=buf(locbyte)
     end do
     if(lenrec /= lensave) go to 890
    
  end do

880 continue
  write(6,*)' reached impossible place in count_recs_wrf_binary_file'
  call closefile(in_unit,ierr)
  return

885 continue
  write(6,*)' problem in count_recs_wrf_binary_file, lenrec has bad value before end of file'
  write(6,*)'     lenrec =',lenrec
  call closefile(in_unit,ierr)
  return

890 continue
  write(6,*)' problem in count_recs_wrf_binary_file, beginning and ending rec len words unequal'
  write(6,*)'     begining reclen =',lensave
  write(6,*)'       ending reclen =',lenrec
  write(6,*)'             in_unit =',in_unit
  call closefile(in_unit,ierr)
  return

900 continue
  write(6,*)' normal end of file reached in count_recs_wrf_binary_file'
  write(6,*)'        nblocks=',thisblock
  write(6,*)'          nrecs=',nrecs
  write(6,*)'         nreads=',nreads
  call closefile(in_unit,ierr)

end subroutine count_recs_wrf_binary_file

subroutine inventory_wrf_binary_file(in_unit,wrfges,nrecs, &
                                     datestr_all,varname_all,memoryorder_all,domainend_all, &
                                     start_block,end_block,start_byte,end_byte,file_offset)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inventory_wrf_binary_file  get contents of wrf binary file
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: generate list of contents and map of wrf binary file which can be
!             used for reading and writing with mpi io routines.
!             same basic routine as count_recs_wrf_binary_file, except 
!             now wrf unpacking routines are used to decode wrf header
!             records, and send back lists of variable mnemonics, dates,
!             grid dimensions, and byte addresses relative to start of 
!             file for each field (this is used by mpi io routines).
!
! program history log:
!   2004-11-29  parrish
!   2006-04-06  middlecoff - replace fortran open,close with openfileread,closefile
!   2007-04-12  parrish - add output variable memoryorder_all to be used with modifications
!                          which allow any combination of ikj/ijk grid ordering for 3D fields.
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrfges - filename of input wrf binary restart file
!     nrecs            - number of sequential records found on input wrf binary restart file.
!                          (obtained by a previous call to count_recs_wrf_binary_file)
!
!   output argument list:  (all following dimensioned nrecs)
!     datestr_all      - date character string for each field, where applicable (or else blanks)
!     varname_all      - wrf mnemonic for each variable, where applicable (or blank)
!     memoryorder_all
!     domainend_all    - dimensions of each field, where applicable (up to 3 dimensions)
!     start_block      - direct access block number containing 1st byte of record
!                            (after 4 byte record mark)
!     end_block        - direct access block number containing last byte of record
!                            (before 4 byte record mark)
!     start_byte       - relative byte address in direct access block of 1st byte of record
!     end_byte         - relative byte address in direct access block of last byte of record
!     file_offset      - absolute address of byte before 1st byte of record (used by mpi io)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_byte,i_long,i_llong,i_kind, r_single
! use module_internal_header_util, only: int_get_ti_header_char,int_get_write_field_header
  implicit none

  integer(i_kind) ,intent(in   ) :: in_unit,nrecs
  character(9)    ,intent(in   ) :: wrfges
  character(132)  ,intent(  out) :: datestr_all(nrecs),varname_all(nrecs),memoryorder_all(nrecs)
  integer(i_kind) ,intent(  out) :: domainend_all(3,nrecs)
  integer(i_kind) ,intent(  out) :: start_block(nrecs),end_block(nrecs)
  integer(i_kind) ,intent(  out) :: start_byte(nrecs),end_byte(nrecs)
  integer(i_llong),intent(  out) :: file_offset(nrecs)

  character(10) cwrfges
  integer(i_kind) irecs
  integer(i_llong) nextbyte,locbyte,thisblock
  integer(i_byte) lenrec4(4)
  integer(i_long) lenrec,lensave
  equivalence (lenrec4(1),lenrec)
  integer(i_byte) missing4(4)
  integer(i_long) missing
  equivalence (missing,missing4(1))
  integer(i_llong),parameter:: lrecl=2**12_i_llong
  integer(i_byte) buf(lrecl)
  integer(i_kind) i,loc_count,nreads
  logical lastbuf
  integer(i_byte) hdrbuf4(2048)
  integer(i_long) hdrbuf(512)
  equivalence(hdrbuf(1),hdrbuf4(1))
  integer(i_kind),parameter:: int_field       =       530
  integer(i_kind),parameter:: int_dom_ti_char =       220
  integer,parameter:: int_dom_ti_real =       140
  integer,parameter:: int_dom_ti_integer =       180
  integer(i_kind) hdrbufsize
  integer(i_kind) inttypesize
  integer(i_kind) datahandle
  character(128) element,dumstr,strdata
  integer(i_kind) loccode
  character(132) blanks
  integer(i_kind) typesize
  integer(i_kind) fieldtype
  integer(i_kind) domaindesc
  character(132) memoryorder,stagger,dimnames(3)
  integer(i_kind) domainstart(3),domainend(3)
  integer(i_kind) patchstart(3),patchend(3)
  character(132) datestr,varname
  integer(i_kind) itypesize
  integer(i_kind) ierr
  integer(i_kind) count, idata(1)
  real(r_single)  rdata(16)

  call wrf_sizeof_integer(itypesize)
  inttypesize=itypesize


  blanks=trim(' ')

  cwrfges = wrfges
  cwrfges(10:10) = char(0)
  call openfileread (in_unit, ierr, cwrfges)
! open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)
  irecs=0
  missing=-9999_i_long
  nextbyte=0_i_llong
  locbyte=lrecl
  nreads=0
  lastbuf=.false.
  do

!    get length of next record

     do i=1,4
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
        lenrec4(i)=buf(locbyte)
     end do
     if(lenrec <= 0_i_long .and. lastbuf) go to 900
     if(lenrec <= 0_i_long .and. .not. lastbuf) go to 885
     nextbyte=nextbyte+1_i_llong
     locbyte=locbyte+1_i_llong
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)

     irecs=irecs+1
     start_block(irecs)=thisblock
     start_byte(irecs)=locbyte
     file_offset(irecs)=nextbyte-1_i_llong
     hdrbuf4(1)=buf(locbyte)
     hdrbuf4(2:4)=missing4(2:4)
     hdrbuf4(5:8)=missing4(1:4)
     datestr_all(irecs)=blanks
     varname_all(irecs)=blanks
     memoryorder_all(irecs)=blanks
     domainend_all(1:3,irecs)=0

     loc_count=1
     do i=2,8
        if(loc_count>=lenrec) exit
        loc_count=loc_count+1
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
        hdrbuf4(i)=buf(locbyte)
     end do

     if(lenrec==2048_i_long) write(6,*)' irecs,hdrbuf(2),int_dom_ti_char,int_field=', &
                                       irecs,hdrbuf(2),int_dom_ti_char,int_field
     if(lenrec==2048_i_long.and.   &
        (hdrbuf(2) == int_dom_ti_char .or. hdrbuf(2) == int_field .or.  &
         hdrbuf(2) == int_dom_ti_real .or. hdrbuf(2) == int_dom_ti_integer)) then

!    bring in next full record, so we can unpack datestr, varname, and domainend
        do i=9,lenrec
           loc_count=loc_count+1
           nextbyte=nextbyte+1_i_llong
           locbyte=locbyte+1_i_llong
           if(locbyte > lrecl .and. lastbuf) go to 900
           if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
           hdrbuf4(i)=buf(locbyte)
        end do

        if(hdrbuf(2) == int_dom_ti_char) then

           call int_get_ti_header_char(hdrbuf,hdrbufsize,inttypesize, &
                    datahandle,element,dumstr,strdata,loccode)
           varname_all(irecs)=trim(element)
           datestr_all(irecs)=trim(strdata)
           write(6,*)' irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),trim(datestr_all(irecs))

        else if(hdrbuf(2) == int_dom_ti_real) then

          call int_get_ti_header_real(hdrbuf,hdrbufsize,inttypesize,typesize, &
                   datahandle,element,rdata,count,loccode)
          varname_all(irecs)=trim(element)
!          datestr_all(irecs)=trim(strdata)
          write(6,*)' irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),rdata(1:count)
        else if(hdrbuf(2) == int_dom_ti_integer) then

          call int_get_ti_header_integer(hdrbuf,hdrbufsize,inttypesize,typesize, &
                   datahandle,element,idata,count,loccode)
          varname_all(irecs)=trim(element)
!          datestr_all(irecs)=trim(strdata)
          write(6,*)' irecs,varname,datestr = ',irecs,trim(varname_all(irecs)),idata(1:count)
        else
           call int_get_write_field_header(hdrbuf,hdrbufsize,typesize, &
              datahandle,datestr,varname,fieldtype, &
              domaindesc,memoryorder,stagger,dimnames, &
              domainstart,domainend,patchstart,patchend)
           varname_all(irecs)=trim(varname)
           datestr_all(irecs)=trim(datestr)
           memoryorder_all(irecs)=trim(memoryorder)
           domainend_all(1:3,irecs)=domainend(1:3)
           write(6,*)' irecs,datestr,domend,varname = ', &
                 irecs,trim(datestr_all(irecs)),domainend_all(1:3,irecs),trim(varname_all(irecs))

        end if

     end if

     nextbyte=nextbyte-loc_count+lenrec
     locbyte=locbyte-loc_count+lenrec
     if(locbyte > lrecl .and. lastbuf) go to 900
     if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
     end_block(irecs)=thisblock
     end_byte(irecs)=locbyte
     lensave=lenrec
     do i=1,4
        nextbyte=nextbyte+1_i_llong
        locbyte=locbyte+1_i_llong
        if(locbyte > lrecl .and. lastbuf) go to 900
        if(locbyte > lrecl) call next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
        lenrec4(i)=buf(locbyte)
     end do
     if(lenrec /= lensave) go to 890
    
  end do

880 continue
  write(6,*)' reached impossible place in inventory_wrf_binary_file'
  call closefile(in_unit,ierr)
  return

885 continue
  write(6,*)' problem in inventory_wrf_binary_file, lenrec has bad value before end of file'
  write(6,*)'     lenrec =',lenrec
  call closefile(in_unit,ierr)
  return

890 continue
  write(6,*)' problem in inventory_wrf_binary_file, beginning and ending rec len words unequal'
  write(6,*)'     begining reclen =',lensave
  write(6,*)'       ending reclen =',lenrec
  write(6,*)'               irecs =',irecs
  write(6,*)'               nrecs =',nrecs
  call closefile(in_unit,ierr)
  return

900 continue
  write(6,*)' normal end of file reached in inventory_wrf_binary_file'
  write(6,*)'        nblocks=',thisblock
  write(6,*)'          irecs,nrecs=',irecs,nrecs
  write(6,*)'         nreads=',nreads
  call closefile(in_unit,ierr)

end subroutine inventory_wrf_binary_file
subroutine next_buf(in_unit,buf,nextbyte,locbyte,thisblock,lrecl,nreads,lastbuf)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    next_buf    bring in next direct access block
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: bring in next direct access block when needed, as the file is scanned
!             from beginning to end during counting and inventory of records.
!             (subroutines count_recs_wrf_binary_file and inventory_wrf_binary_file)
!
! program history log:
!   2004-11-29  parrish
!   2006-04-06  middlecoff - replace direct access read with getbytes
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     nextbyte         - byte number from beginning of file that is desired 
!     locbyte          - byte number from beginning of last block read for desired byte
!     lrecl            - direct access block length
!     nreads           - number of blocks read before now (for diagnostic information only)
!     lastbuf          - logical, if true, then no more blocks, so return 
!
!   output argument list:
!     buf              - output array containing contents of next block
!     locbyte          - byte number from beginning of new block read for desired byte
!     thisblock        - number of new block being read by this routine
!     nreads           - number of blocks read now (for diagnostic information only)
!     lastbuf          - logical, if true, then at end of file.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_byte,i_llong,i_kind
  implicit none

  integer(i_llong),intent(in   ) :: lrecl
  integer(i_kind) ,intent(in   ) :: in_unit
  integer(i_llong),intent(in   ) :: nextbyte
  integer(i_byte) ,intent(  out) :: buf(lrecl)
  integer(i_llong),intent(  out) :: thisblock
  integer(i_kind) ,intent(inout) :: nreads
  integer(i_llong),intent(inout) :: locbyte
  logical         ,intent(inout) :: lastbuf

  integer(i_kind) ierr

  if(lastbuf) return

  ierr=0
  nreads=nreads+1

!  compute thisblock:

  thisblock = 1_i_llong + (nextbyte-1_i_llong)/lrecl

  locbyte = 1_i_llong+mod(locbyte-1_i_llong,lrecl)

! The Fortran standard does not 
!  - specify what iostat should be for a DA read past the EOF
!  - provide a way to detect end-of-file for a DA file
!  - apply the concept end-of-file to direct-access files
! Consequently,the standard does not specify what the contents
! of the buffer will be for locations past the EOF.

! Hence the replacement of the DA read below with the call
! to getbytes

! read(in_unit,rec=thisblock,iostat=ierr)buf
! lastbuf = ierr /= 0

  call getbytes(in_unit, buf, thisblock, lrecl, ierr)
  lastbuf = ierr == 1

end subroutine next_buf

subroutine retrieve_index(index,string,varname_all,nrecs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    retrieve_index  get record number of desired variable
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: by examining previously generated inventory of wrf binary restart file,
!             find record number that contains the header record for variable
!             identified by input character variable "string".
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     string           - mnemonic for variable desired
!     varname_all      - list of all mnemonics obtained from inventory of file
!     nrecs            - total number of sequential records counted in wrf
!                        binary restart file
!
!   output argument list:
!     index            - desired record number
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  implicit none

  integer(i_kind),intent(in   ) :: nrecs
  integer(i_kind),intent(  out) :: index
  character(*)   ,intent(in   ) :: string
  character(132) ,intent(in   ) :: varname_all(nrecs)

  integer(i_kind) i

  do i=1,nrecs
     if(trim(string) == trim(varname_all(i))) then
        index=i
        return
     end if
  end do

  write(6,*)'RETRIEVE_INDEX:  ***PROBLEM*** reading wrf nmm binary file, ',&
       'rec id "',trim(string),'" not found'
  index=-1
  return

end subroutine retrieve_index

subroutine retrieve_field(in_unit,wrfges,out,start_block,end_block,start_byte,end_byte)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    retrieve_field  retrieve field from wrf binary file
!   prgmmr: parrish          org: np22                date: 2004-11-29
!
! abstract: still using direct access, retrieve a field from the wrf binary restart file.
!
! program history log:
!   2004-11-29  parrish
!
!   input argument list:
!     in_unit          - fortran unit number where input file is opened through.
!     wrfges - filename of input wrf binary restart file
!     start_block      - direct access block number containing 1st byte of record
!                            (after 4 byte record mark)
!     end_block        - direct access block number containing last byte of record
!                            (before 4 byte record mark)
!     start_byte       - relative byte address in direct access block of 1st byte of record
!     end_byte         - relative byte address in direct access block of last byte of record
!
!   output argument list:
!     out              - output buffer where desired field is deposited
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_byte,i_kind
  implicit none

  integer(i_kind),intent(in   ) :: in_unit
  character(9)   ,intent(in   ) :: wrfges
  integer(i_kind),intent(in   ) :: start_block,end_block,start_byte,end_byte
  integer(i_byte),intent(  out) :: out(*)

  integer(i_kind),parameter:: lrecl=2**12
  integer(i_byte) buf(lrecl)
  integer(i_kind) i,ii,k,ibegin,iend,ierr

  open(in_unit,file=trim(wrfges),access='direct',recl=lrecl)

  write(6,*)'RETRIEVE_FIELD:  start_block,end_block,s_,e_byte=',&
       start_block,end_block,start_byte,end_byte
  ii=0
  do k=start_block,end_block
     read(in_unit,rec=k,iostat=ierr)buf
     ibegin=1 ; iend=lrecl
     if(k == start_block) ibegin=start_byte
     if(k == end_block) iend=end_byte
     do i=ibegin,iend
        ii=ii+1
        out(ii)=buf(i)
     end do
  end do
  close(in_unit)
  
end subroutine retrieve_field


SUBROUTINE int_get_ti_header_integer( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_integer except that Data is read from
! the file.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
!  INCLUDE 'intio_tags.h'
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element
  INTEGER, INTENT(OUT)         ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = 1
  CALL int_unpack_string ( Element, hdrbuf( n/itypesize + 1 ), n ) ;
!write(0,*)'int_get_ti_header_integer ',TRIM(Element), Data(1)
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_ti_header_integer

SUBROUTINE int_get_ti_header_real( hdrbuf, hdrbufsize, itypesize, typesize, &
                              DataHandle, Element, Data, Count, code )
!<DESCRIPTION>
!<PRE>
! Same as int_gen_ti_header_real except that Data is read from
! the file.
!</PRE>
!</DESCRIPTION>
  IMPLICIT NONE
!  INCLUDE 'intio_tags.h'
  INTEGER, INTENT(INOUT)       ::  hdrbuf(*)
  INTEGER, INTENT(OUT)         ::  hdrbufsize
  INTEGER, INTENT(IN)          ::  itypesize, typesize
  CHARACTER*(*), INTENT(INOUT) ::  Element
  REAL, INTENT(OUT)            ::  Data(*)
  INTEGER, INTENT(OUT)         ::  DataHandle, Count, code
!Local
  INTEGER i, n
!

  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, Data, Count, code )
  i = 1
  CALL int_unpack_string ( Element, hdrbuf( n/itypesize + 1 ), n ) ;
!write(0,*)'int_get_ti_header_real ',TRIM(Element), Data(1)
  hdrbufsize = hdrbuf(1)
  RETURN
END SUBROUTINE int_get_ti_header_real

SUBROUTINE int_get_ti_header_char( hdrbuf, hdrbufsize, itypesize, &
                              DataHandle, Element, VarName, Data, code )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    int_get_ti_header_char
!   prgmmr: 
!
! abstract: Same as int_gen_ti_header_char except that Data is read from the file.
!
! program history log:
!     2008-03-31  safford - add subroutine doc block
!     2009-01-03  todling - wrapped unavailable routine int_get_ti_header_c within ifdef
!     2009-09-28  guo     - flagged uninitialized variable to signal possible conflict.
!
!   input argument list:
!     hdrbuf     - 
!     itypesize  - 
!     Element    - 
!     Data       - 
!     VarName    - 
!
!   output argument list:
!     hdrbuf     - 
!     hdrbufsize - 
!     Element    - 
!     Data       - 
!     VarName    - 
!     DataHandle - 
!     code       - 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  IMPLICIT NONE

! INCLUDE 'intio_tags.h'
  INTEGER(i_kind), INTENT(INOUT) ::  hdrbuf(*)
  INTEGER(i_kind), INTENT(  OUT) ::  hdrbufsize
  INTEGER(i_kind), INTENT(IN   ) ::  itypesize
  CHARACTER*(*)  , INTENT(INOUT) ::  Element, Data, VarName
  INTEGER(i_kind), INTENT(  OUT) ::  DataHandle, code
!Local
  INTEGER(i_kind) i, n, DummyCount, typesize
  CHARACTER * 132  dummyData
!  logical, external :: debug_foo
!
  CALL int_get_ti_header_c ( hdrbuf, hdrbufsize, n, itypesize, typesize, &
                           DataHandle, dummyData, DummyCount, code )
  i = n/itypesize+1 ;
  CALL int_unpack_string ( Element, hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( Data   , hdrbuf( i ), n ) ; i = i + n
  CALL int_unpack_string ( VarName  , hdrbuf( i ), n ) ; i = i + n
  hdrbufsize = hdrbuf(1)

  RETURN
END SUBROUTINE int_get_ti_header_char

SUBROUTINE int_get_write_field_header ( hdrbuf, hdrbufsize, ftypesize, &
                                        DataHandle , DateStr , VarName , FieldType ,                 &
                                        DomainDesc , MemoryOrder , Stagger , DimNames ,              &
                                        DomainStart , DomainEnd ,                                    &
                                        PatchStart , PatchEnd )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    int_get_write_field_header
!   prgmmr: 
!
! abstract:  See documentation block in int_gen_write_field_header() for 
!            a description of a "write field" header.  
!
! program history log:
!     2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     hdrbuf     - 
!     ftypesize  - 
!     DateStr    -
!     VarName    - 
!     MemoryOrder
!     Stagger
!     DimNames
!
!   output argument list:
!     hdrbuf     - 
!     hdrbufsize - 
!     ftypesize  - 
!     DataHandle - 
!     DateStr    -
!     VarName    - 
!     FieldType
!     DomainDesc
!     MemoryOrder
!     Stagger
!     DimNames
!     DomainStart,DomainEnd
!     PatchStart,PatchEnd
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  IMPLICIT NONE

! INCLUDE 'intio_tags.h'
  INTEGER(i_kind)              , INTENT(INOUT) ::  hdrbuf(*)
  INTEGER(i_kind)              , INTENT(  OUT) ::  hdrbufsize
  INTEGER(i_kind)              , INTENT(INOUT) ::  ftypesize
  INTEGER(i_kind)              , INTENT(  OUT) :: DataHandle
  CHARACTER*(*)                , INTENT(INOUT) :: DateStr
  CHARACTER*(*)                , INTENT(INOUT) :: VarName
  INTEGER(i_kind)              , INTENT(  OUT) :: FieldType
  INTEGER(i_kind)              , INTENT(  OUT) :: DomainDesc
  CHARACTER*(*)                , INTENT(INOUT) :: MemoryOrder
  CHARACTER*(*)                , INTENT(INOUT) :: Stagger
  CHARACTER*(*)   ,dimension(*), INTENT(INOUT) :: DimNames
  INTEGER(i_kind) ,dimension(*), INTENT(  OUT) :: DomainStart, DomainEnd
  INTEGER(i_kind) ,dimension(*), INTENT(  OUT) :: PatchStart,  PatchEnd
!Local
  integer(i_kind),parameter:: int_field       =       530
  CHARACTER*132 mess
  INTEGER(i_kind) i, n

  hdrbufsize = hdrbuf(1)
  IF ( hdrbuf(2) /= int_field ) THEN
     write(mess,*)'int_get_write_field_header: hdrbuf(2) ne int_field ',hdrbuf(2),int_field
     CALL wrf_error_fatal3 ( "module_internal_header_util.b" , 220 ,  mess )
  ENDIF
  ftypesize = hdrbuf(3)

  i = 4
  DataHandle = hdrbuf(i)     ; i = i+1
  call int_unpack_string( DateStr, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( VarName, hdrbuf(i), n )     ; i = i+n
  FieldType = hdrbuf(i)      ; i = i+1
  call int_unpack_string( MemoryOrder, hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( Stagger, hdrbuf(i), n )     ; i = i+n
  call int_unpack_string( DimNames(1), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(2), hdrbuf(i), n ) ; i = i+n
  call int_unpack_string( DimNames(3), hdrbuf(i), n ) ; i = i+n
  DomainStart(1) = hdrbuf(i)    ; i = i+1
  DomainStart(2) = hdrbuf(i)    ; i = i+1
  DomainStart(3) = hdrbuf(i)    ; i = i+1
  DomainEnd(1) = hdrbuf(i)       ; i = i+1
  DomainEnd(2) = hdrbuf(i)       ; i = i+1
  DomainEnd(3) = hdrbuf(i)       ; i = i+1
  PatchStart(1) = hdrbuf(i)     ; i = i+1
  PatchStart(2) = hdrbuf(i)     ; i = i+1
  PatchStart(3) = hdrbuf(i)     ; i = i+1
  PatchEnd(1) = hdrbuf(i)       ; i = i+1
  PatchEnd(2) = hdrbuf(i)       ; i = i+1
  PatchEnd(3) = hdrbuf(i)       ; i = i+1
  DomainDesc = hdrbuf(i)       ; i = i+1

  RETURN
END SUBROUTINE int_get_write_field_header

SUBROUTINE int_unpack_string ( str, buf, n )
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    int_unpack_string
!   prgmmr: 
!
! abstract:  This routine is used to extract a string from a sequence of integers.  
!            The first integer is the string length.  
!
! program history log:
!     2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     str        -
!     buf        -
!
!   output argument list:
!     str        -
!     n          -
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: i_kind
  IMPLICIT NONE

  CHARACTER*(*)                , INTENT(  OUT) :: str
  INTEGER(i_kind)              , INTENT(  OUT) :: n       ! on return, N is the number of ints copied from buf
  INTEGER(i_kind), DIMENSION(*), INTENT(IN   ) :: buf
!Local
  INTEGER(i_kind) i
  INTEGER(i_kind) strlen

  strlen = buf(1)
  str = ""
  DO i = 1, strlen
     str(i:i) = char(buf(i+1))
  ENDDO
  n = strlen + 1
END SUBROUTINE int_unpack_string

!

MODULE module_wrf_error
!$$$   module documentation block
!
! module:  module_wrf_error
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add module and subroutine doc blocks
!
! subroutines included:
!   wrf_at_debug_level           ---
!   init_module_wrf_error        ---
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: wrf_at_debug_level
  public :: init_module_wrf_error
! set passed variables to public
  public :: wrf_debug_level

  INTEGER(i_kind) :: wrf_debug_level = 0

CONTAINS

  LOGICAL FUNCTION wrf_at_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  wrf_at_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    use kinds, only: i_kind
    IMPLICIT NONE

    INTEGER(i_kind) , INTENT(IN   ) :: level

    wrf_at_debug_level = ( level <= wrf_debug_level )
    RETURN
  END FUNCTION wrf_at_debug_level

  SUBROUTINE init_module_wrf_error
!$$$   subprogram documentation block
!
! subprogram:  init_module_wrf_error
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    IMPLICIT NONE
  END SUBROUTINE init_module_wrf_error

END MODULE module_wrf_error

  SUBROUTINE set_wrf_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  set_wrf_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    USE module_wrf_error
    use kinds, only: i_kind
    IMPLICIT NONE

    INTEGER(i_kind) , INTENT(IN   ) :: level

    wrf_debug_level = level
    RETURN
  END SUBROUTINE set_wrf_debug_level

  SUBROUTINE get_wrf_debug_level ( level )
!$$$   subprogram documentation block
!
! subprogram:  get_wrf_debug_level
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!
!   output argument list:
!     level    - debug level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
    USE module_wrf_error
    use kinds, only: i_kind
    IMPLICIT NONE

    INTEGER(i_kind) , INTENT(  OUT) :: level

    level = wrf_debug_level
    RETURN
  END SUBROUTINE get_wrf_debug_level


SUBROUTINE wrf_debug( level , str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_debug
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     level    - debug level
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  use kinds, only: i_kind
  IMPLICIT NONE

  CHARACTER*(*) str
  INTEGER(i_kind) , INTENT (IN   ) :: level
  INTEGER(i_kind)                  :: debug_level

  CALL get_wrf_debug_level( debug_level )
  IF ( level <= debug_level ) THEN
    ! old behavior
     CALL wrf_message( str )
  ENDIF
  RETURN
END SUBROUTINE wrf_debug

SUBROUTINE wrf_message( str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_message
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!    str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  IMPLICIT NONE

  CHARACTER*(*), intent(in   ) :: str

  write(6,*) TRIM(str)
  print*, TRIM(str)
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( TRIM(str), WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message

! intentionally write to stderr only
SUBROUTINE wrf_message2( str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_message2
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!    str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  IMPLICIT NONE

  CHARACTER*(*), intent(in   ) :: str

  write(6,*) str
!TBH:  Calls to logwrite cut off str in ESMF 2.2.0.
!TBH:  Restore this call once this ESMF bug is fixed.
!TBH#ifdef USE_LOGERR
!TBH  IF ( WRFU_IsInitialized() ) THEN
!TBH    CALL WRFU_LogWrite( str, WRFU_LOG_INFO )
!TBH  ENDIF
!TBH#endif
END SUBROUTINE wrf_message2

SUBROUTINE wrf_error_fatal3( file_str, line, str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_error_fatal3
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     file_str -
!     line     -
!     str      -
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  use kinds, only: i_kind
  IMPLICIT NONE

  CHARACTER*(*)   , intent (in   ) :: file_str
  INTEGER(i_kind) , INTENT (IN   ) :: line  ! only print file and line if line > 0
  CHARACTER*(*)   , intent (in   ) :: str
  CHARACTER*256 :: line_str

  write(line_str,'(i6)') line
  CALL wrf_message( '-------------- FATAL CALLED ---------------' )
  ! only print file and line if line is positive
  IF ( line > 0 ) THEN
     CALL wrf_message( 'FATAL CALLED FROM FILE:  '//file_str//'  LINE:  '//TRIM(line_str) )
  ENDIF
  CALL wrf_message( str )
  CALL wrf_message( '-------------------------------------------' )
! CALL wrf_abort
  call stop2(199)
END SUBROUTINE wrf_error_fatal3

SUBROUTINE wrf_error_fatal( str )
!$$$   subprogram documentation block
!
! subprogram:  wrf_error_fatal
!
! abstract:  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     str      -
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  IMPLICIT NONE

  CHARACTER*(*),intent(in   ) :: str

  CALL wrf_error_fatal3 ( ' ', 0, str )
END SUBROUTINE wrf_error_fatal

SUBROUTINE wrf_check_error( expected, actual, str, file_str, line )
!$$$   subprogram documentation block
!
! subprogram:  wrf_check_error
!
! abstract:   Check to see if expected value == actual value
!             If not, print message and exit.  
!
! program history log:
!   2008-03-31  safford - add subroutine doc block
!
!   input argument list:
!     expected -
!     actual   -
!     line     -
!     str
!     file_str
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  USE module_wrf_error
  use kinds, only: i_kind
  IMPLICIT NONE

  INTEGER(i_kind) , INTENT (IN   ) :: expected
  INTEGER(i_kind) , INTENT (IN   ) :: actual
  CHARACTER*(*)   , intent (in   ) :: str
  CHARACTER*(*)   , intent (in   ) :: file_str
  INTEGER(i_kind) , INTENT (IN   ) :: line
  CHARACTER (LEN=512)   :: rc_str
  CHARACTER (LEN=512)   :: str_with_rc

  IF ( expected /= actual ) THEN
     WRITE (rc_str,*) '  Routine returned error code = ',actual
     str_with_rc = TRIM(str // rc_str)
     CALL wrf_error_fatal3 ( file_str, line, str_with_rc )
  ENDIF
END SUBROUTINE wrf_check_error
SUBROUTINE stop2(n)
integer :: n

stop 1234

END SUBROUTINE stop2
