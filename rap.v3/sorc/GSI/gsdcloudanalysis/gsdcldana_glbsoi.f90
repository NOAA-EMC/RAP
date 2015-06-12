subroutine gsdcldana_glbsoi(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    glbsoi               driver for gridpoint statistical 
!                                     interpolation
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: executes gridpoint spectral interpolation analysis.
!
! program history log:
!   1990-10-06  parrish
!   1994-02-11  parrish
!   1998-05-15  weiyu yang       mpp version
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   1993-12-22  kleist,d., treadon, r.,derber, j.  modules, updates and comments
!   2004-06-21  treadon - update documentation
!   2004-07-08  treadon - fix vertical indexing bug in call set_ozone_var
!   2004-07-24  treadon - add only to module use, add intent in/out
!   2004-11-22  parrish - add code to handle regional netcdf i/o
!   2004-12-13  treadon - limit runtime output from CRTM & IRSSE initilizations
!   2004-12-22  treadon - add optional code to compute/write out innovation
!                         information following all outer loops
!   2005-01-22  parrish - add balmod, compact_diffs
!   2005-02-23  wu      - setup norm rh
!   2005-03-01  parrish - add parts of regional anisotropic background option 
!                         (not complete yet)
!   2005-04-14  yanqiu zhu - support for observation sensitivity study
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-05-27  kleist  - add call to destroy grids from patch interpolation
!   2005-06-08  treadon - add switch_on_derivatives for create/destroy_ges_bias_grids
!   2005-06-13  li/treadon - move destroy_sfc_grids after write_all
!   2005-07-12  kleist  - add Jc term
!   2005-08-16  guo - add gmao surface interface
!   2005-09-28  parrish - add logic to only allow NCEP global or WRF_NMM to use jc routines
!   2005-09-29  derber  - simplify observation file handling
!   2005-11-21  kleist - use tendency module, add call to get moisture diagnostics
!   2005-11-28  derber - move read_obs to glbsoi
!   2005-11-29  derber - move read guess and background error calculations outside 
!                        external iteration
!   2006-01-09  derber - absorb set_nrh_var into compute_derived
!   2006-01-10  treadon - move read*files into gesinfo, consolidate read*guess calls
!   2006-01-12  treadon - replace pCRTM with CRTM
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-04-12  treadon - remove mpimod (not used)
!   2006-04-21  kleist  - remove call to setupjc, no longer exists
!   2006-07-28  derber  - remove creation of obs inner loop data file name
!   2006-08-15  parrish - add call to create_vtrans (get vertical modes if nvmodes_keep > 0)
!   2006-12-04  todling - split bias and guess init/final (rename ges_bias routines)
!   2006-12-15  todling - no need to protect destroy call to tendvars
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2007-03-15  su      - to delocate the converr arrays
!   2007-05-08  kleist  - add preliminary verions of mp_compact_diffs module
!   2007-06-08  kleist/treadon - add prefix (task id or path) to obs_setup
!   2007-06-21  rancic - add pbl code
!   2007-06-27  tremolet- observation sensitivity
!   2007-06-29  jung    - update CRTM interface
!   2007-07-05  todling - skip calculating o-a when running in 4d-var mode
!   2007-08-27  tremolet- changed outer loop control for 4dvar
!   2007-09-30  todling - add timer
!   2007-10-25  todling - obsdiag files now written by observer
!   2007-11-12  todling - write sat bias moved from write_all here (ESMF-interface support)
!   2008-01-04  tremolet- outer loop for sensitivity computations
!   2008-11-03  sato    - enable use of global anisotropic mode
!   2008-12-02  todling - remove references to pcgsoi_tl and old obs_sen
!   2009-01-28  todling - move write_all to pcgsoi (for consistency w/ 4dvar branch of code)
!                       - use observer to avoid redundant code
!   2009-08-19  guo     - changed setuprhsall() interface for multi-pass observer.
!   2009-08-31  parrish - add call to fmg_initialize_e when tlnmc_type=4.  Initializes
!                          alternative regional tangent linear normal mode constraint which
!                          allows for variation of coriolis parameter and map factor.
!   2009-09-12  parrish - add call to hybrid_ensemble_setup.  if l_hyb_ens=.true., then
!                          subroutine hybrid_ensemble_setup is called, which creates 
!                          everything needed for a hybrid ensemble 3dvar analysis.
!   2009-09-14  guo     - move compact_diff related statements to observer_init() in observer.F90
!   2010-02-20  parrish - move hybrid_ensemble_setup to beginning of code and read
!                          in ensemble perturbations where hybrid_ensemble_setup was previously located.
!   2010-04-27  zhu     - add call to pcinfo. when newpc4pred=.true., pcinfo is called
!                         to calculate additional preconditioner; add newpc4pred in
!                         radinfo_write's interface
!   2010-05-12  zhu     - add option passive_bc for radiance bias correction for monitored channels
!   2010-10-01  el akkraoui/todling - add Bi-CG as optional minimization scheme
!   2011-04-07  todling - newpc4pred now in radinfo
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2013-07-02  parrish - remove references to init_strongvars_1, init_strongvars_2
!
!   input argument list:
!     mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: rearth
  use mpimod, only: npe
!  use adjtest_obs, only: adtest_obs
!  use jfunc, only: miter,jiter,jiterstart,jiterend,iguess,&
!      write_guess_solution,&
!      tendsflag,xhatsave
!  use anberror, only: anisotropic, &
!      create_anberror_vars_reg,destroy_anberror_vars_reg,&
!      create_anberror_vars,destroy_anberror_vars
!  use anisofilter, only: anprewgt_reg
!  use anisofilter_glb, only: anprewgt
!  use berror, only: create_berror_vars_reg,create_berror_vars,&
!      set_predictors_var,destroy_berror_vars_reg,&
!      destroy_berror_vars,bkgv_flowdep,pcinfo
!  use balmod, only: create_balance_vars_reg,create_balance_vars, &
!      destroy_balance_vars_reg,destroy_balance_vars,prebal,prebal_reg
!  use compact_diffs, only: create_cdiff_coefs,inisph
  use gridmod, only: nlat,nlon,nsig,rlats,regional,&
      twodvar_regional,wgtlats
  use guess_grids, only: nfldsig
!  use obsmod, only: write_diag,perturb_obs,ditype
!  use turblmod, only: create_turblvars,destroy_turblvars
!  use obs_sensitivity, only: lobsensfc, iobsconv, lsensrecompute, &
!      init_fc_sens, save_fc_sens, lobsensincr, lobsensjb
!  use smooth_polcarf, only: norsp,destroy_smooth_polcas
!  use jcmod, only: ljcdfi
!  use gsi_4dvar, only: l4dvar, lsqrtb, lbicg, lanczosave, ladtest_obs
!  use pcgsoimod, only: pcgsoi
!  use control_vectors, only: dot_product,read_cv,write_cv
!  use radinfo, only: radinfo_write,passive_bc,newpc4pred
!  use pcpinfo, only: pcpinfo_write
!  use converr, only: converr_destroy
!  use zrnmi_mod, only: zrnmi_initialize
  use gsdcldana_observermod, only: gsdcldana_observer_init,gsdcldana_observer_set,gsdcldana_observer_finalize
  use timermod, only: timer_ini, timer_fnl
!  use hybrid_ensemble_parameters, only: l_hyb_ens,destroy_hybens_localization_parameters
!  use hybrid_ensemble_isotropic, only: create_ensemble,load_ensemble
!  use gfs_stratosphere, only: destroy_nmmb_vcoords,use_gfs_stratosphere
!  use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type
  use mpimod, only: ierror,mpi_comm_world

  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: mype

! Declare local variables
  logical slow_pole_in,laltmin

  integer(i_kind) nlev_mp,jiterlast
  real(r_kind) :: zgg
  character(len=12) :: clfile

!*******************************************************************************************
!
! Initialize timer for this procedure
!  call timer_ini('glbsoi')
!  if (mype==0) call w3tagb('glbsoi',1999,0232,0055,'NP23')


! If l_hyb_ens is true, then initialize machinery for hybrid ensemble 3dvar
!  if(l_hyb_ens) then
!     call hybens_grid_setup
!  end if

! Check for alternative minimizations
!  laltmin = lsqrtb.or.lbicg

! Initialize observer
  if (mype==0) call w3tagb('gsdcldana_observer_init',1999,0232,0055,'NP23')
  call gsdcldana_observer_init
  if (mype==0)  call w3tage('gsdcldana_observer_init')

! Check GSI options against available number of guess time levels
!  if (nfldsig == 1) then
!     if (bkgv_flowdep) then
!        bkgv_flowdep = .false.
!        if (mype==0) &
!           write(6,*)'GLBSOI: ***WARNING*** reset bkgv_flowdep=',bkgv_flowdep,&
!           ', because only ',nfldsig,' guess time level available'
!     endif
!  endif

  if (mype==0) call w3tagb('gsdcldana_observer_set',1999,0232,0055,'NP23')
! Read observations and scatter
  call gsdcldana_observer_set
  if (mype==0)  call w3tage('gsdcldana_observer_set')
!  call MPI_BARRIER(mpi_comm_world,ierror)

! cloud analysis
  if (mype==0) call w3tagb('gsdcloudanalysis_sa',1999,0232,0055,'NP23')
  call gsdcloudanalysis_sa(mype)
!  call MPI_BARRIER(mpi_comm_world,ierror)
  if (mype==0)  call w3tage('gsdcloudanalysis_sa')

  if (mype==0) call w3tagb('gsdcldana_write_all',1999,0232,0055,'NP23')
! Write output analysis files
  call gsdcldana_write_all(-1,mype)
!  call prt_guess('analysis')
  if (mype==0)  call w3tage('gsdcldana_write_all')

! Finalize observer
!  if (mype==0) call w3tagb('gsdcldana_observer_finalize',1999,0232,0055,'NP23')
  call gsdcldana_observer_finalize
!  if (mype==0)  call w3tage('gsdcldana_observer_finalize')

! Finalize timer for this procedure
!  call timer_fnl('glbsoi')
!  if (mype==0)  call w3tage('glbsoi')

  return

! End of routine
end subroutine gsdcldana_glbsoi
