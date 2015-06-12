PROGRAM read_be
!
  use kinds,only : r_single,i_kind
  implicit none
!
  integer(i_kind) :: msig,mlat
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn
!
  integer(i_kind):: inerr=12,istat
  integer(i_kind):: isig,k,i,j
  character*5 var
!
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn
  real(r_single),dimension(:,:),allocatable::  corqq_avn
  real(r_single) :: factor_corz
  integer(i_kind):: istart
!
!
!  open(inerr,file='nam_nmmstat_na.gcv',form='unformatted',status='old',convert='big_endian')
!  open(inerr,file='nam_glb_berror.f77.gcv',form='unformatted',status='old',convert='big_endian')
!  open(inerr,file='nam_glb_berror.f77_Little_Endian.gcv',form='unformatted',status='old')
!  open(inerr,file='berror_stats_global_RR_tune_IBM',form='unformatted',status='old',convert='big_endian')
  open(inerr,file='RR_nam11_nmmstat_na.gcv',form='unformatted',status='old')
!  open(inerr,file='berror_stats_global_RR',form='unformatted',status='old')
!  open(inerr,file='berror_stats_NAM_RR',form='unformatted',status='old')
  istart=0
!  istart=94
  
! Read header.
  rewind inerr
  read(inerr) msig,mlat

  OPEN(33,file='gsi_be.bin',form='unformatted')
  write(33) msig
  write(33) mlat
  
  write(*,*) 'msig,mlat',msig,mlat
! Read background error file to get balance variables
  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  read(inerr)agv_avn,bv_avn,wgv_avn

  write(*,*) clat_avn(:)
!  write(*,*) clat_avn(132)
!  write(*,*) clat_avn(istart+37)
  DO i =1, msig
    write(*,*) i,sigma_avn(i),sigma_avn(i)*1013.0
  ENDDO
  write(33) agv_avn*1.0e8
  write(*,*) 'agv_avn= ',maxval(agv_avn(istart+37,:,:)*1.0e8),minval(agv_avn(istart+37,:,:)*1.0e8)
  write(33) bv_avn*100.0
  write(*,*) 'bv_avn= ',maxval(bv_avn(istart:mlat+1,:)*100),minval(bv_avn(istart:mlat+1,:)*100)
  write(33) wgv_avn*10.0e8
  write(*,*) 'wgv_avn= ',maxval(wgv_avn(istart:mlat+1,:)*1.e9),minval(wgv_avn(istart:mlat+1,:)*1.e9)

! Read amplitudes
  read: do
     read(inerr,iostat=istat) var, isig
     if (istat /= 0) exit
     write(*,*) 'var, isig= ',var, isig
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )

     if (var/='q') then
        read(inerr) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inerr) corz_avn,corqq_avn
     end if

     read(inerr) hwll_avn
     if (isig>1) then
        read(inerr) vztdq_avn
     end if

     factor_corz=1.0
     if(var=='sf') factor_corz=1.0e-5
     if(var=='vp') factor_corz=1.0e-5
     write(33) corz_avn*factor_corz
     write(*,*) 'corz_avn= ',maxval(corz_avn(istart+1:mlat,:)*factor_corz),& 
                             minval(corz_avn(istart+1:mlat,:)*factor_corz)
     write(33) hwll_avn/1000.0
     write(*,*) 'hwll_avn= ',maxval(hwll_avn(istart:mlat+1,:)/1000.0),& 
                             minval(hwll_avn(istart:mlat+1,:)/1000.0)
     write(33) ((vztdq_avn(i,j),j=0,mlat+1),i=1,isig)
     write(*,*) 'vztdq_avn= ',maxval(vztdq_avn(:,istart:mlat+1)),& 
                              minval(vztdq_avn(:,istart:mlat+1))

     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q') deallocate ( corqq_avn )

  enddo read
  close(inerr)
  close(33)

END
