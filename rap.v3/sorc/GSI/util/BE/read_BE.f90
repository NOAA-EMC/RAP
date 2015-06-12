PROGRAM tune_be
!
!  tune GSI be
!
  use kinds,only : r_single,i_kind,r_kind
  implicit none
!
  integer(i_kind) :: msig,mlat
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn
!
  integer(i_kind):: inerr=12,istat
  integer(i_kind):: outerr=24
  integer(i_kind):: isig,k,i,j
  character*5 var
!
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn
  real(r_single),dimension(:,:),allocatable::  corqq_avn
!
  real(r_kind) fctr_agv,fctr_bv,fctr_wgv
!
  open(inerr,file='berror_stats',form='unformatted',status='old')
!  open(inerr,file='nam_glb_berror.f77.gcv',form='unformatted',status='old')
  open(outerr,file='berror_stats_Big',form='unformatted',status='new',convert='big_endian')
  
! Read header.
  rewind inerr
  read(inerr) msig,mlat
  write(outerr) msig,mlat

  write(*,*) msig,mlat
!
! Read background error file to get balance variables
  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  read(inerr)agv_avn,bv_avn,wgv_avn
  write(*,*) clat_avn
  write(*,*) sigma_avn
  
  write(outerr)clat_avn,(sigma_avn(k),k=1,msig)
  write(outerr)agv_avn,bv_avn,wgv_avn

! Read amplitudes
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat /= 0) exit
     write(outerr) var, isig
     write(*,*) 'var, isig= ',var, isig
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )

     if (var/='q') then
        read(inerr) corz_avn
        write(outerr) corz_avn
        write(*,*) maxval(corz_avn), minval(corz_avn)
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inerr) corz_avn,corqq_avn
        write(outerr) corz_avn,corqq_avn
     end if

     read(inerr) hwll_avn
     write(outerr) hwll_avn
     if (isig>1) then
        read(inerr) vztdq_avn
        write(outerr) vztdq_avn
     end if

     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q') deallocate ( corqq_avn )

  enddo read
  close(inerr)
  close(outerr)

END
