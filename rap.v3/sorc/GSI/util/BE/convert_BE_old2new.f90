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
!  old be
!
  real(r_single),dimension(:),allocatable::  clat_avn_old,sigma_avn_old,corp_avn_old, &
                                             hwllp_avn_old
  real(r_single),dimension(:,:),allocatable::  bv_avn_old,wgv_avn_old,corqq_avn_old
  real(r_single),dimension(:,:,:),allocatable:: corz_avn_old,hwll_avn_old,&
       vztdq_avn_old,agv_avn_old

!
  open(inerr,file='nam_glb_berror.f77_Little_Endian',form='unformatted',status='old')

  read(inerr) msig,mlat
!
  allocate ( corz_avn_old(1:mlat,1:msig,1:4) )
  allocate ( corqq_avn_old(1:mlat,1:msig) )
  allocate ( sigma_avn_old(1:msig) )
  allocate ( hwll_avn_old(0:mlat+1,1:msig,1:4) )
  allocate ( vztdq_avn_old(1:msig,0:mlat+1,1:4) )
  allocate ( agv_avn_old(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn_old(0:mlat+1,1:msig),wgv_avn_old(0:mlat+1,1:msig) )
  allocate ( clat_avn_old(mlat),corp_avn_old(mlat),hwllp_avn_old(0:mlat+1) )

! Read in berror 

  read(inerr)clat_avn_old,(sigma_avn_old(k),k=1,msig)
  read(inerr)corz_avn_old,corp_avn_old,corqq_avn_old
  read(inerr)hwll_avn_old,hwllp_avn_old
  read(inerr)vztdq_avn_old
  read(inerr)agv_avn_old,bv_avn_old,wgv_avn_old
  close(inerr)
!
  open(outerr,file='nam_glb_berror.f77_Little_Endian.gcv',form='unformatted',status='new')
!  
  write(outerr) msig,mlat

! Read background error file to get balance variables
  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

  clat_avn=clat_avn_old
  sigma_avn=sigma_avn_old
  agv_avn=agv_avn_old
  bv_avn=bv_avn_old
  wgv_avn=wgv_avn_old
  write(outerr)clat_avn,(sigma_avn(k),k=1,msig)
  write(outerr)agv_avn,bv_avn,wgv_avn

! Read amplitudes
     isig=msig
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )
     allocate ( corqq_avn(1:mlat,1:isig) )
     isig=msig

  DO i=1,4
     if(i==1) var='sf'
     if(i==2) var='vp'
     if(i==3) var='t'
     if(i==4) var='q'
     write(outerr,iostat=istat) var, isig
  
     write(*,*) 'var, isig= ',var, isig

     corz_avn=corz_avn_old(:,:,i)
     if(i==4) then
        corqq_avn=corqq_avn_old
        write(outerr) corz_avn,corqq_avn
     else
        write(outerr) corz_avn
     endif
     hwll_avn=hwll_avn_old(:,:,i)
     write(outerr) hwll_avn
     vztdq_avn=vztdq_avn_old(:,:,i)
     write(outerr) vztdq_avn
  enddo

  var='ps'
  isig=1
  write(outerr,iostat=istat) var, isig
  write(*,*) 'var, isig= ',var, isig
  write(outerr) corp_avn_old
  write(outerr) hwllp_avn_old

     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     deallocate ( corqq_avn )

  close(outerr)

END
