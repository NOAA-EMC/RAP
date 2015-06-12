PROGRAM RemapVertical_regional_BE
!
!  read in old regional BE and remap to new vertical coordinate
!
  use kinds,only : r_single,i_kind, r_kind
  use constants, only: zero,one
  implicit none
!
  integer(i_kind) :: msig,mlat,nsig
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),allocatable,dimension(:):: sigma
  real(r_kind),allocatable,dimension(:):: sigmar
!
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn

  real(r_single),allocatable,dimension(:,:):: wgvi ,bvi
  real(r_single),allocatable,dimension(:,:,:):: agvi
!
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn
  real(r_single),dimension(:,:),allocatable::  corqq_avn

  real(r_kind),dimension(:,:),allocatable:: corz_rrr,hwll_rrr
  real(r_kind),dimension(:,:),allocatable::  corqq_rrr

  real(r_single),allocatable,dimension(:,:):: corz, hwll, vz
  real(r_single),allocatable,dimension(:,:):: corqq
!
  real(r_kind),dimension(:),allocatable::  rlsigo,rlsig
  integer(i_kind),allocatable,dimension(:):: lsig
  real(r_kind),allocatable,dimension(:):: coef1,coef2
!
  integer(i_kind):: inerr=12,istat
  integer(i_kind):: outerr=22
  integer(i_kind):: isig,k,i,j
  character*5 var
  integer(i_kind) :: m, m1, l, l1
!
!  get new vertical coordinate
!
  read(47) nsig
  allocate ( sigma(1:nsig) )
  allocate ( sigmar(1:nsig) )
  read(47) sigmar
  sigma=sigmar
  write(*,*) nsig,sigma
!
  open(inerr,file='/mnt/lfs0/projects/wrfruc/mhu/vapor/fix_nam_2011/nam_nmmstat_na.gcv',form='unformatted',status='old',convert='big_endian')
  open(outerr,file='RR_nam11_nmmstat_na.gcv',form='unformatted',status='new')
  
! Read header.
  rewind inerr
  read(inerr) msig,mlat
  write(outerr) nsig,mlat
  write(*,*) 'msig,mlat',msig,mlat
! Read background error file to get balance variables
  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )
  allocate ( agvi(0:mlat+1,1:nsig,1:nsig) )
  allocate ( bvi(0:mlat+1,1:nsig),wgvi(0:mlat+1,1:nsig) )

  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  write(outerr)clat_avn,(sigma(k),k=1,nsig)

! compute vertical(pressure) interpolation index and weight
  allocate ( rlsig(nsig) )
  allocate ( rlsigo(msig) )
  do k=1,nsig
     rlsig(k)=log(sigmar(k))
  enddo
  do k=1,msig
     rlsigo(k)=log(sigma_avn(k))
  enddo

  allocate(lsig(nsig),coef1(nsig),coef2(nsig))
    do k=1,nsig
       if(rlsig(k)>=rlsigo(1))then
          m=1
          m1=2
          lsig(k)=1
          coef1(k)=one
          coef2(k)=zero
       else if(rlsig(k)<=rlsigo(msig))then
          m=msig-1
          m1=msig
          lsig(k)=msig-1
          coef1(k)=zero
          coef2(k)=one
       else
          m_loop: do m=1,msig-1
             m1=m+1
             if((rlsig(k)<=rlsigo(m))   .and.  &
                  (rlsig(k)>rlsigo(m1))     )then
                lsig(k)=m
                exit m_loop
             end if
          enddo m_loop
          coef1(k)=(rlsigo(m1)-rlsig(k))/(rlsigo(m1)-rlsigo(m))
          coef2(k)=one-coef1(k)
          if(lsig(k)==msig)then
             lsig(k)=msig-1
             coef2(k)=one
             coef1(k)=zero
          endif
       endif
    end do

  read(inerr)agv_avn,bv_avn,wgv_avn
!   Load agv wgv bv
    do k=1,nsig
       m=lsig(k)
       m1=m+1
       do i=1,mlat
          wgvi(i,k)=wgv_avn(i,m)*coef1(k)+wgv_avn(i,m1)*coef2(k)
          bvi (i,k)=bv_avn (i,m)*coef1(k)+bv_avn (i,m1)*coef2(k)
       enddo

       do j=1,nsig
          l=lsig(j)
          l1=l+1
          do i=1,mlat
             agvi(i,j,k)=(agv_avn(i,l,m)*coef1(j)+agv_avn(i,l1,m)*coef2(j))*coef1(k) &
                      +(agv_avn(i,l,m1)*coef1(j)+agv_avn(i,l1,m1)*coef2(j))*coef2(k)
          enddo
       enddo
    enddo
  write(outerr) agvi,bvi,wgvi

! Read amplitudes
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat /= 0) exit
     if(isig > 1) then
        write(outerr) var, nsig
     else
        write(outerr) var, isig
     endif
     write(*,*) 'var, isig= ',var, isig
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )
     allocate ( corz_rrr(1:mlat,1:isig) )
     allocate ( hwll_rrr(0:mlat+1,1:isig) )
     if(isig > 1) then
       allocate ( corz(1:mlat,1:nsig) )
       allocate ( hwll(0:mlat+1,1:nsig) )
       allocate ( vz(1:nsig,0:mlat+1) )
     else
       allocate ( corz(1:mlat,1:isig) )
       allocate ( hwll(0:mlat+1,1:isig) )
       allocate ( vz(1:isig,0:mlat+1) )
     endif

     if (var/='q') then
        read(inerr) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        allocate ( corqq_rrr(1:mlat,1:isig) )
        allocate ( corqq(1:mlat,1:nsig) )
        read(inerr) corz_avn,corqq_avn
     end if

     read(inerr) hwll_avn
     if (isig>1) then
        read(inerr) vztdq_avn
     end if

! vertical interpolation
     if(isig>1) then
       if(var=='q') corqq_rrr=corqq_avn
       corz_rrr=corz_avn
       hwll_rrr=hwll_avn
       do k=1,nsig
           m=lsig(k)
           m1=m+1
           do i=1,mlat
              corz(i,k)=corz_rrr(i,m)*coef1(k)+corz_rrr(i,m1)*coef2(k)
           enddo
           if (var=='q') then
             do i=1,mlat
               corqq(i,k)=corqq_rrr(i,m)*coef1(k)+corqq_rrr(i,m1)*coef2(k)
             enddo
           endif

           do i=0,mlat+1
              hwll(i,k)=hwll_rrr(i,m)*coef1(k)+hwll_rrr(i,m1)*coef2(k)
               vz(k,i)=vztdq_avn(m,i)*coef1(k)+vztdq_avn(m1,i)*coef2(k)
           enddo
       enddo
     else
       corz=corz_avn
       hwll=hwll_avn
     endif
     if (var/='q') then
       write(outerr) corz
     else
       write(outerr) corz,corqq
     endif
     write(outerr) hwll
     if (isig>1) then
        write(outerr) vz
     end if

     deallocate ( corz_avn,corz,corz_rrr )
     deallocate ( hwll_avn,hwll,hwll_rrr )
     deallocate ( vztdq_avn,vz )
     if (var=='q') deallocate ( corqq_avn,corqq,corqq_rrr )

  enddo read
  close(inerr)
  close(outerr)

END
