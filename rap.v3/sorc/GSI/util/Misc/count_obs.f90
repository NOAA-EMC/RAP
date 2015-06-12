program count_obs
!
!  This tool reads in observation numbers from stdout and fort files
!  and give a short report on observatio numbers.
!
!  2012/05/04          Ming Hu  (GSD)
!
!
   implicit none
   
   integer :: npe
   integer :: nvar
   integer,allocatable :: nobs_s(:,:)
   character(10),allocatable :: dtype(:),dplat(:)
   integer,allocatable :: ntotal(:)
   integer :: icycle
   integer :: iunit
   integer :: iyear,imonth,iday,ihour

   character(10) :: filename
   integer, parameter:: maxobstype=100,maxcount=3
   integer :: nobs_t(0:maxobstype,maxcount)
   integer :: nobs_q(0:maxobstype,maxcount)
   integer :: nobs_uv(0:maxobstype,maxcount)
   integer :: nobs_ps(0:maxobstype,maxcount)
   integer :: nobs_pw(0:maxobstype,maxcount)
   integer :: nobs_sst(0:maxobstype,maxcount)

   character(10),allocatable :: satdplat(:),satdtype(:)
   integer,allocatable :: nsatdata(:,:)

   integer :: is,ii,ntype,k,nsat

!
   nobs_t=0
   nobs_q=0
   nobs_uv=0
   nobs_ps=0
   nobs_pw=0
   nobs_sst=0

   nvar=100
   allocate(dtype(nvar),dplat(nvar))
   allocate(ntotal(nvar))
   allocate(satdplat(nvar),satdtype(nvar))
   allocate(nsatdata(nvar,3))

   call timepe_read_stdout(iyear,imonth,iday,ihour,npe)
   icycle=iyear*1000000+imonth*10000+iday*100+ihour
   allocate(nobs_s(nvar,npe))

   call count_read_stdout(nvar,npe,dplat,dtype,nobs_s,ntype)
    
   ntotal=0
   if(ntype>0) then
      do is =1,ntype
         do ii=1,npe
            ntotal(is)=ntotal(is)+nobs_s(is,ii)
         enddo
      enddo

! count radaince
!
      write(*,*) 'count radiance'
      call count_fort_207(nvar,satdplat,satdtype,nsatdata,nsat)
!
!  count T
!
      do is =1 , ntype
        if(trim(dtype(is))=='ps') then
           write(*,*) 'count ',dtype(is)
           call count_fort_sinlgelvl(dtype(is),nobs_ps,maxobstype,maxcount)
        elseif(trim(dtype(is))=='t') then
           write(*,*) 'count ',dtype(is)
           call count_fort_multilvl(dtype(is),nobs_t,maxobstype,maxcount)
        elseif(trim(dtype(is))=='q') then
           write(*,*) 'count ',dtype(is)
           call count_fort_multilvl(dtype(is),nobs_q,maxobstype,maxcount)
        elseif(trim(dtype(is))=='uv') then
           write(*,*) 'count ',dtype(is)
           call count_fort_multilvl(dtype(is),nobs_uv,maxobstype,maxcount)
        elseif(trim(dtype(is))=='sst') then
           write(*,*) 'count ',dtype(is)
           call count_fort_sinlgelvl(dtype(is),nobs_sst,maxobstype,maxcount)
        elseif(trim(dtype(is))=='pw') then
           write(*,*) 'count ',dtype(is)
           call count_fort_sinlgelvl(dtype(is),nobs_pw,maxobstype,maxcount)
        endif
      enddo

! summary
        write(*,*) 
        write(*,'(a,I10,a,I4,a)') 'Cycle ',icycle,' reads in',ntype, ' data types: '
        do is =1,ntype
           ii=0
        if(nsat > 0 ) then
              do k=1,nsat
                if(satdplat(k) == dplat(is) .and. satdtype(k)==dtype(is)) ii=k
              enddo
           endif
           if(ii>0) then
              write(*,9000) is,dtype(is),dplat(is),ntotal(is),(nsatdata(ii,k),k=1,3)
           else
              write(*,9000) is,dtype(is),dplat(is),ntotal(is)
           endif
        enddo
 9000 format(I5,5x,2A10,I10,3I10)

    endif ! ntype > 0
          
! save
       open(13,file='obs_num_summary.txt')
       write(13,*) 
       write(13,'(A6,I10)') 'cycle=',icycle
       if(ntype>0) then
          write(13,'(100A9)') (trim(dplat(is)),is=1,ntype)
          write(13,'(100A9)') (trim(dtype(is)),is=1,ntype)
          write(13,'(100I9)') (ntotal(is),is=1,ntype)
       endif ! ntype > 0
       close(13)

     deallocate(dtype,dplat,ntotal,nobs_s,satdtype,satdplat,nsatdata)

end program

subroutine count_fort_multilvl(dtype,nobs,maxobstype,maxcount)
  implicit none
  character(10), intent(in) :: dtype 
  integer, intent(in) :: maxobstype,maxcount
  integer, intent(inout) :: nobs(0:maxobstype,maxcount)

  character(8) :: filename
  character*120 :: cline
  integer :: itype, icount,iindex,ibase
  integer :: i,j,k,ios

  nobs=0

  if(trim(dtype)=='t') then
     filename='fort.203'
     ibase=100
  elseif(trim(dtype)=='q') then
     filename='fort.204'
     ibase=100
  elseif(trim(dtype)=='uv') then
     filename='fort.202'
     ibase=200
  endif
  
  
  open(10, file=trim(filename),status='old',iostat=ios)
  if(ios==0) then

100 continue
      read(10,'(a120)',end=200)  cline
      if(cline(1:7)==' o-g 01' .and. cline(30:34)=='count') then
      if(cline(21:23) /= 'all') then
         read(cline(21:23),'(I3)') itype
         read(cline(113:120),'(I8)') icount
         iindex=itype-ibase
         if(iindex < 0 .or. iindex > maxobstype) then
            write(*,*) 'Error: index out of range',iindex,itype,maxobstype
            stop 123
         endif
!         write(*,*) itype, icount,iindex
         if(cline(17:19)=='   ') then
            nobs(iindex,1)=icount
         elseif(cline(17:19)=='rej') then
            nobs(iindex,2)=icount
         elseif(cline(17:19)=='mon') then
            nobs(iindex,3)=icount
         endif
      endif
      endif
    go to 100

200 continue
    close(10)
!  check
       write(*,'(4A10)') 'type','use','rej','mon'
    do i=0,maxobstype
       if(nobs(i,1)+nobs(i,2)+nobs(i,3) > 0) &
       write(*,'(4I10)') i+ibase,(nobs(i,k),k=1,3)
    enddo
  endif ! ios==0

end subroutine

subroutine count_fort_sinlgelvl(dtype,nobs,maxobstype,maxcount)
  implicit none
  character(10), intent(in) :: dtype 
  integer, intent(in) :: maxobstype,maxcount
  integer, intent(inout) :: nobs(0:maxobstype,maxcount)

  character(8) :: filename
  character*120 :: cline
  integer :: itype, icount,iindex,ibase
  integer :: i,j,k,ios

  nobs=0

  if(trim(dtype)=='ps') then
     filename='fort.201'
     ibase=100
  elseif(trim(dtype)=='sst') then
     filename='fort.213'
     ibase=100
  elseif(trim(dtype)=='pw') then
     filename='fort.205'
     ibase=100
  endif
  
  
  open(10, file=trim(filename),status='old',iostat=ios)
  if(ios==0) then
  
100 continue
      read(10,'(a120)',end=200)  cline
      if(cline(1:7)==' o-g 01') then
      if(cline(21:23) /= 'all') then
         read(cline(21:23),'(I3)') itype
         read(cline(31:38),'(I8)') icount
         iindex=itype-ibase
         if(iindex < 0 .or. iindex > maxobstype) then
            write(*,*) 'Error: index out of range',iindex,itype,maxobstype
            stop 123
         endif
!         write(*,*) itype, icount,iindex
         if(cline(17:19)=='   ') then
            nobs(iindex,1)=icount
         elseif(cline(17:19)=='rej') then
            nobs(iindex,2)=icount
         elseif(cline(17:19)=='mon') then
            nobs(iindex,3)=icount
         endif
      endif
      endif
    go to 100

200 continue
    close(10)
!  check
       write(*,'(4A10)') 'type','use','rej','mon'
    do i=0,maxobstype
       if(nobs(i,1)+nobs(i,2)+nobs(i,3) > 0) &
       write(*,'(4I10)') i+ibase,(nobs(i,k),k=1,3)
    enddo
  endif ! ios==0

end subroutine

subroutine count_fort_207(nvar,satdplat,satdtype,nsatdata,nsat)
  implicit none
  integer, intent(in) :: nvar
  character(10), intent(out) :: satdplat(nvar)
  character(10), intent(out) :: satdtype(nvar)
  integer, intent(out) :: nsatdata(nvar,3)
  integer, intent(out) :: nsat

  character(8) :: filename
  character*120 :: cline
  integer :: is,k,ios

  nsat=0
  nsatdata=0
  filename='fort.207'
  
  is=1
  open(10, file=trim(filename),status='old',iostat=ios)
  if(ios==0) then

100 continue
     if(is <= nvar) then
        read(10,'(a120)',end=200)  cline
        if(cline(1:10)=='o-g 01 rad') then
           satdplat(is)=cline(13:22)
           satdtype(is)=cline(23:32)    
           read(cline(36:43),'(I8)') nsatdata(is,1)
           read(cline(47:54),'(I8)') nsatdata(is,2)
           read(cline(58:65),'(I8)') nsatdata(is,3)
           if(nsatdata(is,1) > 0) is=is+1
        endif
     else
        write(*,*) 'nvar is too small, increase nvar !!!'
        stop 11
     endif
    go to 100

200 continue
    close(10)

    nsat=is-1

!  check

    write(*,'(5A15)') 'satellite', 'instrument','# read','# keep','# assim'
    do is=1,nsat
       write(*,'(2A15,4I15)') satdplat(is),satdtype(is),(nsatdata(is,k),k=1,3)
    enddo
  endif ! ios==0

end subroutine

subroutine timepe_read_stdout(iyear,imonth,iday,ihour,npe)
  implicit none
  integer, intent(out) :: npe
  integer, intent(out) :: iyear,imonth,iday,ihour

  character(8) :: filename
  character*120 :: cline

  integer :: ios

  filename='stdout'
  
  open(10, file=trim(filename),status='old',iostat=ios)
  if(ios==0) then

100 continue
        read(10,'(a120)',end=200)  cline
        if(cline(2:27)=='GESINFO:  Analysis date is') then
           read(cline(29:76),'(4I12)') iyear,imonth,iday,ihour
        endif
        if(cline(31:33)=='npe') then
           read(cline(49:51),'(I3)') npe
        endif
    go to 100

200 continue
    close(10)

!  check
     write(*,*) 'time=',iyear,imonth,iday,ihour
     write(*,*) 'npe=',npe
  else
     write(*,*) 'Cannot find stdout, stop'
     stop 123
  endif ! ios==0

end subroutine

subroutine count_read_stdout(nvar,npe,dplat,dtype,ndata,ntype)
  implicit none
  integer, intent(in) :: nvar,npe
  character(10), intent(out) :: dplat(nvar)
  character(10), intent(out) :: dtype(nvar)
  integer, intent(out) :: ndata(nvar,npe)
  integer, intent(out) :: ntype

  integer :: ndata10(10)
  character(8) :: filename
  character*120 :: cline
  integer :: is,k,numline,istart,iend,i,ii,ios

  ntype=0
  ndata=0
  filename='stdout'
  
  is=1
  open(10, file=trim(filename),status='old',iostat=ios)
  if(ios==0) then

100 continue
     if(is <= nvar) then
        read(10,'(a120)',end=200)  cline
        if(cline(1:8)=='OBS_PARA') then
           dplat(is)=cline(21:30)
           dtype(is)=cline(11:20)    
           read(cline(31:110),'(8I10)') ndata(is,1:8)
           numline= (npe-8)/10 + 1
           k=1
           do while (k <= numline)
              read(10,'(a120)',end=200)  cline
              istart=(k-1)*10+9
              iend=min(istart+10-1,npe)
              if(cline(1:10)=='          ') then
                 read(cline(16:110),'(10I10)') ndata10
                 ii=1
                 do i=istart,iend
                    ndata(is,i)=ndata10(ii)
                    ii=ii+1
                 enddo
                 k=k+1
              endif
           enddo
           is=is+1
        endif
     else
        write(*,*) 'nvar is too small, increase nvar !!!'
        stop 11
     endif
    go to 100

200 continue
    close(10)

    ntype=is-1

!  check
!    write(*,'(5A15)') 'type', 'number'
!    do is=1,ntype
!       write(*,1000) dtype(is),dplat(is),(ndata(is,k),k=1,npe)
!    enddo
 1000 format(10x,2A10,8I10,/,(10X,10I10))
  endif ! ios==0

end subroutine
