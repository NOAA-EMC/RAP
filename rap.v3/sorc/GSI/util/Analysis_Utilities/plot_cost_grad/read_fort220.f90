PROGRAM read_fort220
!
!  This is a sample program to read in content in fort.220
!
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: nobs_type

  implicit none

!
!  parameter and variables to read in
!
  integer(i_kind),parameter:: ipen = 5+nobs_type
  integer(i_kind),parameter:: istp_iter = 5
  integer(i_kind),parameter:: ioutpen = istp_iter*4

  real(r_quad),dimension(ipen):: pbc
  real(r_quad),dimension(ipen):: bpen,cpen
  real(r_kind),dimension(ipen):: stpx

  real(r_kind),dimension(0:istp_iter):: stp
  real(r_kind),dimension(ioutpen):: outpen,outstp

  integer(i_kind) istp_use

  real(r_kind) gnormx,penx,penalty,pennorm
  real(r_kind),dimension(2):: gnorm
  character(5) step
  real(r_kind) stp1,b

!
!  namelist files
!
  integer(i_kind) miter,niter(5)   ! number of outer loop and number of inner iteration 
  namelist/steps/ miter,niter

!
!
!
  real(r_kind) costgradient(4,200)
!
!  local
!
  integer(i_kind) iout_iter,jiter,iter,i, im,in
  character(100) ctemp

!
! code start
!
  istp_use=2
  iout_iter=210

  open(11,file='namelist.readfort220')
   read(11,steps)
  close(11)

  OPEN(iout_iter,file='fort.220',form='formatted')
  rewind(iout_iter)

  DO im=1,miter
    read(iout_iter,*)
    read(iout_iter,*)
    read(iout_iter,*)
    read(iout_iter,*)
    read(iout_iter,*)

    DO in=1,niter(im)+1
      read(iout_iter,100) ctemp,(pbc(i),i=1,ipen)
      read(iout_iter,101) ctemp,(stpx(i),i=1,ipen)
      read(iout_iter,105) ctemp,(bpen(i),i=1,ipen)
      read(iout_iter,110) ctemp,(cpen(i),i=1,ipen)
!      write(*,100) ctemp,(pbc(i),i=1,ipen)
!      write(*,101) ctemp,(stpx(i),i=1,ipen)
!      write(*,105) ctemp,(bpen(i),i=1,ipen)
!      write(*,110) ctemp,(cpen(i),i=1,ipen)
100   format(a3,3e25.18/,(3x,3e25.18))
101   format(a3,3e25.18/,(3x,3e25.18))
105   format(a3,3e25.18/,(3x,3e25.18))
110   format(a3,3e25.18/,(3x,3e25.18))

      read(iout_iter,200) ctemp,(stp(i),i=0,istp_use)
      read(iout_iter,201) ctemp,(outstp(i),i=1,istp_use*4)
      read(iout_iter,202) ctemp,(outpen(i),i=1,istp_use*4)
!      write(*,200) ctemp,(stp(i),i=0,istp_use)
!      write(*,201) ctemp,(outstp(i),i=1,istp_use*4)
!      write(*,202) ctemp,(outpen(i),i=1,istp_use*4)
200   format(a22,6(e24.18,1x))
201   format(a20,(8(e12.6,1x)))
202   format(a20,(8(e12.6,1x)))


      read(iout_iter,310) ctemp,jiter,iter,penalty,gnorm(1),stp1,b
      read(iout_iter,320) ctemp,jiter,iter,penx,gnormx,step
!      write(*,310) ctemp,jiter,iter,penalty,gnorm(1),stp1,b
!      write(*,320) ctemp,jiter,iter,penx,gnormx,step

310   format(a20,i3,i4,1x,4(e24.18,1x),2(g12.6,1x))
320   format(a20,i3,i4,1x,2(e24.18,1x),a5)

      costgradient(2*im-1,in)=penalty
      costgradient(2*im,in)=gnorm(1)

     ENDDO  !in
   ENDDO  !im

   close(iout_iter)

   OPEN(12,file='steps.txt')
     write(12,'(2I10)')  2*miter+1, niter(1)+1
   close(12)
   OPEN(12,file='cost_gradient.txt')
   DO in=1,niter(1)+1
     write(12,'(5f10.2)') float(in-1),(costgradient(im,in),im=1,4)
   ENDDO   !  in
   close(12)

END PROGRAM read_fort220
