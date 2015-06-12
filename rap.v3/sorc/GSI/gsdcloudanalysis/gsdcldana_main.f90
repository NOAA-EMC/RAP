!-------------------------------------------------------------------------
!  NASA/GSFC, Global Modeling and Assimilation Office, Code 610.3, GMAO  !
!-------------------------------------------------------------------------
!BOP

! ! IPROGRAM: gsimain -- runs NCEP gsi

! ! INTERFACE:

   program gsi

   use gsdcldana_gsimod, only: gsdcldana_gsimain_initialize,gsdcldana_gsimain_run,gsdcldana_gsimain_finalize
   use timermod, only: timer_pri
   use kinds, only: i_kind
   use mpeu_util, only: die
   implicit none

!==================================================================================================

   integer(i_kind):: ier
   character(len=*),parameter:: myname='gsimain'

   call gsdcldana_gsimain_initialize

   call gsdcldana_gsimain_run(init_pass=.true.,last_pass=.true.)

   call gsdcldana_gsimain_finalize

   call timer_pri(6)

   end program gsi

