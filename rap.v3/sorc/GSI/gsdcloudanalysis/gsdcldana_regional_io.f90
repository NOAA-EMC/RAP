module gsdcldana_regional_io
!$$$   module documentation block
!                .      .    .                                       .
! module:  regional_io
! prgmmr:  treadon           org: np23                date: 2004-12-29
!
! abstract: This module contains routines that handle the input/output
!           of regional gsi guess(analysis) grids
!
! program history log:
!   2004-12-29  treadon
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!   2005-07-06  parrish - add variable update_pint
!   2005-10-17  parrish - add ctph0,stph0,tlm0 
!   2010-09-15  pagowski - add cmaq
!   2012-02-16  parrish - if use_gfs_stratosphere true, then broadcast extra parameters to all pes from pe 0.
!   
! Subroutines Included:
!   sub convert_regional_guess  - convert regional guess to internal format
!   sub write_regional_analysis - write regional analysis
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use gridmod, only: wrf_mass_regional,wrf_nmm_regional,&
       nems_nmmb_regional,cmaq_regional,&
       twodvar_regional,netcdf
  use mpimod, only: mpi_comm_world,ierror
  implicit none

! set default to private
  private
! set subroutines to public
  public :: gsdcldana_write_regional_analysis
! set passed variables to public
  public :: update_pint,preserve_restart_date

  logical update_pint            !  if true, then this is nmm run with pint variable, so update pint
                                 !    (where pint is non-hydrostatic 3-d pressure variable)
  logical preserve_restart_date  !  if true, then do not update date information on restart file

contains


  subroutine gsdcldana_write_regional_analysis(mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    write_regional_analysis
!     prgmmr:    treadon     org:  np23               date: 2004-12-29
!
! abstract:  write regional analysis grid to output file
!
! program history log:
!   2004-12-29  treadon
!   2005-05-24  pondeca - add 2dvar only surface analysis option
!
!   input argument list:
!      mype - mpi task id
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

    use kinds, only: i_kind
    implicit none

!   Declare passed variables
    integer(i_kind),intent(in):: mype

!   Write nmm analysis file.  Consider two possible
!   output formats:  netcdf or binary
    if (wrf_nmm_regional) then
       if (netcdf) then
          call wrwrfnmma_netcdf(mype)
          if (mype==0) then
             call update_netcdf_nmm
          end if
          call mpi_barrier(mpi_comm_world,ierror)
       else
          call wrwrfnmma_binary(mype)
       end if
    end if

!   Write mass analysis file.  Consider two possible
!   output formats:  netcdf or binary
    if (wrf_mass_regional) then
       if(netcdf) then
          call wrwrfmassa_netcdf(mype)
          if (mype==0) then
             call gsdcldana_update_netcdf_mass
!             call gsdcldana_update_netcdf_mass_a
          endif
!          if (mype==12) then
!             call gsdcldana_update_netcdf_mass_b
!          endif
!          call gsdcldana_update_netcdf_mass_parall
          call mpi_barrier(mpi_comm_world,ierror)
       else
          call wrwrfmassa_binary(mype)
       end if
    end if

!write cmaq analysis

    if (cmaq_regional) call write_cmaq(mype)

!   Write nems nmmb analysis file.

    if (nems_nmmb_regional) call wrnemsnmma_binary(mype)

!   Write 2d analysis file
!   output format: binary
    if (twodvar_regional) call wr2d_binary(mype)

    return
  end subroutine gsdcldana_write_regional_analysis
  
end module gsdcldana_regional_io
