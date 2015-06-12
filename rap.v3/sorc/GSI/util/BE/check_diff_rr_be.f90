PROGRAM check_diff_rr_be
!
  use kinds,only : r_single,r_kind,i_kind
  implicit none
!
  integer(i_kind) :: mlat,nsig,nc3d,nc2d,nvars
!
  integer(i_kind):: inerr=45
  integer(i_kind):: k,i,j,nn
!
  real(r_kind),allocatable,dimension(:,:):: corp, hwllp
  real(r_kind),allocatable,dimension(:,:,:):: corz, hwll, vz

  real(r_kind),allocatable,dimension(:,:):: wgvi ,bvi
  real(r_kind),allocatable,dimension(:,:,:):: agvi
!
  real(r_kind),allocatable,dimension(:,:):: corp2, hwllp2
  real(r_kind),allocatable,dimension(:,:,:):: corz2, hwll2, vz2

  real(r_kind),allocatable,dimension(:,:):: wgvi2 ,bvi2
  real(r_kind),allocatable,dimension(:,:,:):: agvi2
!
  character*250 :: path1,path2
  integer :: imax,jmax,kmax
  real :: maxz
!
  path1='/mnt/lfs0/projects/wrfruc/mhu/temp/check_gsi_be/2010101812/gsiprd_RR/'
  path2='/mnt/lfs0/projects/wrfruc/mhu/temp/check_gsi_be/2010101812/gsiprd/'

!   Allocate arrays in stats file

  open(inerr,file=trim(path1)//'fort.46',form='unformatted',status='old')
  read(inerr) mlat,nsig
  allocate ( agvi(0:mlat+1,1:nsig,1:nsig) )
  allocate ( bvi(0:mlat+1,1:nsig),wgvi(0:mlat+1,1:nsig) )
  read(inerr) agvi
  read(inerr) bvi
  read(inerr) wgvi
  close(inerr)

  open(inerr,file=trim(path1)//'fort.45',form='unformatted',status='old')
  read(inerr) mlat,nsig,nc3d,nc2d,nvars
  
  write(*,*) mlat,nsig,nc3d,nc2d,nvars
! Allocate arrays in stats file  allocate ( corz(1:mlat,1:nsig,1:nc3d) )
  allocate ( corz(1:mlat,1:nsig,1:nc3d) )
  allocate ( corp(1:mlat,nc2d) )
  allocate ( hwll(0:mlat+1,1:nsig,1:nc3d),hwllp(0:mlat+1,nvars-nc3d) )
  allocate ( vz(1:nsig,0:mlat+1,1:nc3d) )

  read(inerr) corz
  read(inerr) corp
  read(inerr) hwll
  read(inerr) hwllp
  read(inerr) vz
  close(inerr)

!   Allocate arrays in stats file

  open(inerr,file=trim(path2)//'fort.46',form='unformatted',status='old')
  read(inerr) mlat,nsig
  allocate ( agvi2(0:mlat+1,1:nsig,1:nsig) )
  allocate ( bvi2(0:mlat+1,1:nsig),wgvi2(0:mlat+1,1:nsig) )
  read(inerr) agvi2
  read(inerr) bvi2
  read(inerr) wgvi2
  close(inerr)

  open(inerr,file=trim(path2)//'fort.45',form='unformatted',status='old')
  read(inerr) mlat,nsig,nc3d,nc2d,nvars
  
  write(*,*) mlat,nsig,nc3d,nc2d,nvars
! Allocate arrays in stats file  allocate ( corz(1:mlat,1:nsig,1:nc3d) )
  allocate ( corz2(1:mlat,1:nsig,1:nc3d) )
  allocate ( corp2(1:mlat,nc2d) )
  allocate ( hwll2(0:mlat+1,1:nsig,1:nc3d),hwllp2(0:mlat+1,nvars-nc3d) )
  allocate ( vz2(1:nsig,0:mlat+1,1:nc3d) )

  read(inerr) corz2
  read(inerr) corp2
  read(inerr) hwll2
  read(inerr) hwllp2
  read(inerr) vz2
  close(inerr)

! check diff
  agvi2=agvi2-agvi
  bvi2=bvi2-bvi
  wgvi2=wgvi2-wgvi
  corz2=corz2-corz
  corp2=corp2-corp
  hwll2=hwll2-hwll
  hwllp2=hwllp2-hwllp
  vz2=vz2-vz
!
    write(*,*) 'agvi=',maxval(agvi2), minval(agvi2)
    write(*,*) 'bvi2=',maxval(bvi2), minval(bvi2)
    write(*,*) 'wgvi2=',maxval(wgvi2), minval(wgvi2)
    write(*,*) 'corz2=',maxval(corz2), minval(corz2)
    write(*,*) 'corp2=',maxval(corp2), minval(corp2)
    write(*,*) 'hwll2=',maxval(hwll2), minval(hwll2)
    write(*,*) 'hwllp2=',maxval(hwllp2), minval(hwllp2)
    write(*,*) 'vz2=',maxval(vz2), minval(vz2)
  DO j=1,nc3d
    write(*,*) 'corz2=',j,maxval(corz2(:,:,j)), minval(corz2(:,:,j))
    write(*,*) 'hwll2=',j,maxval(hwll2(:,:,j)), minval(hwll2(:,:,j))
  ENDDO
  maxz=-999.0
  DO k=1,nsig
  DO j=1,nc3d
  DO i=1,mlat
     if(corz2(i,k,j) > maxz) then
        maxz=corz2(i,k,j)
        imax=i
        jmax=j
        kmax=k
     endif
  ENDDO
  ENDDO
  ENDDO
  write(*,*) 'max=',maxz,imax,jmax,kmax
  write(*,*) corz2(imax,kmax,jmax),corz(imax,kmax,jmax)
END
