!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_RTlocal
!
! PURPOSE:
!       Module containing the raditive transfer solution routines.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_RTSolution
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_SpcCoeff:              Module containing the shared CRTM spectral
!                                   coefficients (SpcCoeff) and their
!                                   load/destruction routines. 
!                                   USEs TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        SPCCOEFF_DEFINE module
!                                        SPCCOEFF_BINARY_IO module
!                                        CRTM_PARAMETERS module
!
!       CRTM_Atmosphere_Define:     Module defining the CRTM Atmosphere
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_CLOUD_DEFINE module
!
!       CRTM_Surface_Define:        Module defining the CRTM Surface data
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_GeometryInfo_Define:   Module defining the CRTM GeometryInfo
!                                   data structure and containing routines
!                                   to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_PARAMETERS module
!
!       CRTM_AtmAbsorption_Define:  Module defining the CRTM AtmAbsorption
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_AtmScatter_Define:     Module defining the CRTM AtmScatter
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_SfcOptics_Define:      Module to compute the surface optical
!                                   properties required for determining
!                                   the surface contribution to the radiative
!                                   transfer.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_RTSolution_Define:     Module defining the CRTM RTSolution
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Compute_RTSolution:     Function to solve the radiative transfer 
!                                      problem.
!
!         CRTM_Compute_RTSolution_TL:  Function to solve the tangent-linear
!                                      radiative transfer problem.
!
!         CRTM_Compute_RTSolution_AD:  Function to solve the adjoint
!                                      radiative transfer problem.
!
!       PRIVATE subprograms
!       -------------------
!       
!         *** USERS ADD INFO HERE FOR ANY PRIVATE SUBPROGRAMS ***
!
!
!
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Clear_RTSolution:   Subroutine to clear the scalar members of a
!                                CRTM_RTSolution structure.
!                                SOURCE: CRTM_RTSOLUTION_DEFINE module
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS at JCSDA;    Quanhua.Liu@noaa.gov 
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2004
!
!  Copyright (C) 2004 Yong Han, Quanhua Liu, Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!--------------------------------------------------------------------------------

MODULE CRTM_RTlocal


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Error_Handler
  USE CRTM_Parameters


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE

  ! ------------------
  ! Default visibilities
  ! ------------------

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_RTSolution.f90,v 1.13.2.12 2005/10/21 18:56:08 paulv Exp $'

 
  ! -- The maximum number of doubling processes in the
  ! -- the doubling-adding scheme.
  INTEGER,         PRIVATE, PARAMETER :: MAX_NUMBER_DOUBLING = 25


  ! --------------------------------------
  ! Strucutre definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------

  TYPE, PUBLIC :: CRTM_RTVariables_type
  !  PRIVATE

    ! ---------------------
    ! Dimension information
    ! ---------------------

    INTEGER :: n_Layers  = 0  ! Number of atmospheric layers
    INTEGER :: n_Streams = 0  ! Number of *hemispheric* stream angles used in RT
    INTEGER :: n_Angles  = 0  ! n_Streams + sensor zenith angle

    REAL( fp_kind )                            :: Secant_Down_Angle = 0
    REAL( fp_kind ), DIMENSION( MAX_N_LAYERS ) :: Delta_Tau         = ZERO

    ! ----------------
    ! Planck radiances
    ! ----------------

    REAL( fp_kind )                               :: Planck_Surface    = ZERO
    REAL( fp_kind ), DIMENSION(  0:MAX_N_LAYERS ) :: Planck_Atmosphere = ZERO

                                                                                                                                                                                                           
    ! -- Quadrature information
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES ) :: COS_Angle  = ZERO  ! Gaussian quadrature abscissa
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES ) :: COS_Weight = ZERO  ! Gaussian quadrature weights
                                                                                                                           
    ! -- Logical switches
    LOGICAL :: Diffuse_Surface = .TRUE.
    LOGICAL :: Scattering_RT   = .FALSE.


    ! -- Emission model variables
    REAL( fp_kind ) :: Total_OD  = ZERO
    REAL( fp_kind ), DIMENSION(   MAX_N_LAYERS ) :: e_Layer_Trans_UP   = ZERO
    REAL( fp_kind ), DIMENSION(   MAX_N_LAYERS ) :: e_Layer_Trans_DOWN = ZERO
    REAL( fp_kind ), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Rad_UP     = ZERO
    REAL( fp_kind ), DIMENSION( 0:MAX_N_LAYERS ) :: e_Level_Rad_DOWN   = ZERO
    

    INTEGER,         DIMENSION( MAX_N_LAYERS ) :: Number_Doubling   = 0
    ! --------------
    ! Phase matrices
    ! --------------

    ! -- Forward and backward scattering phase matrices
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                MAX_N_LAYERS  ) :: Pff = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                MAX_N_LAYERS  ) :: Pbb = ZERO

    ! -- Positive and negative cosine angle Legendre phase functions
    REAL( fp_kind ), DIMENSION( 0:MAX_N_LEGENDRE_TERMS, &
                                MAX_N_ANGLES            ) :: Pplus  = ZERO
    REAL( fp_kind ), DIMENSION( 0:MAX_N_LEGENDRE_TERMS, &
                                MAX_N_ANGLES            ) :: Pminus = ZERO

    ! -- Original forward and backward scattering phase matrices.
    ! -- These may be slightly negative and, if so, need to be made
    ! -- positive and thus adjusted to ensure energy conservation
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                MAX_N_LAYERS  ) :: Off = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                MAX_N_LAYERS  ) :: Obb = ZERO

    ! -- Normalisation factor and intermediate sum used for original
    ! -- phase matrix energy conservation.
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_LAYERS  ) :: n_Factor = ZERO
    REAL( fp_kind ), DIMENSION( 0:MAX_N_ANGLES, &
                                MAX_N_LAYERS    ) :: sum_fac = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS) :: Inv_Gamma  = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS) :: Inv_GammaT = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS) :: Refl_Trans = ZERO
                                                                                                                           
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS) :: s_Layer_Trans = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_LAYERS) :: s_Layer_Refl  = ZERO
                                                                                                                           
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_N_LAYERS) :: s_Level_Refl_UP = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, 0:MAX_N_LAYERS )              :: s_Level_Rad_UP  = ZERO
                                                                                                                           
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_LAYERS) :: s_Layer_Source_UP   = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_LAYERS) :: s_Layer_Source_DOWN = ZERO
                                                                                                                           
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_LAYERS) :: C1 = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_LAYERS) :: C2 = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_LAYERS) :: D1 = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_LAYERS) :: D2 = ZERO        

    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_NUMBER_DOUBLING, MAX_N_LAYERS ) :: Trans   = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_NUMBER_DOUBLING, MAX_N_LAYERS ) :: Refl    = ZERO
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, MAX_N_ANGLES, 0:MAX_NUMBER_DOUBLING, MAX_N_LAYERS ) :: Inv_BeT = ZERO

  END TYPE CRTM_RTVariables_type



!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id:  Exp $
!
! $Date:  $
!
! $Revision:  $
!
! $Name:  $
!
! $State: Exp $
!
! $Log:  $

!
!
END MODULE CRTM_RTlocal
!
