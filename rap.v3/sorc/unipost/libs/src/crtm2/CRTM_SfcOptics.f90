!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_SfcOptics
!
! PURPOSE:
!       Module to compute the surface optical properties required for
!       determining the surface contribution to the radiative transfer.
!       
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_SfcOptics
!
! MODULES:
!       Type_Kinds:                Module containing definitions for kinds
!                                  of variable types.
!
!       Error_Handler:             Module to define simple error codes and
!                                  handle error conditions
!                                  USEs: FILE_UTILITY module
!
!       CRTM_Parameters:           Module of parameter definitions for the CRTM.
!                                  USEs: TYPE_KINDS module
!
!       CRTM_SpcCoeff:             Module containing the shared CRTM spectral
!                                  coefficients structure, SC. 
!                                  USEs TYPE_KINDS module
!                                       ERROR_HANDLER module
!                                       SPCCOEFF_DEFINE module
!                                       SPCCOEFF_BINARY_IO module
!                                       CRTM_PARAMETERS module
!
!       CRTM_Surface_Define:       Module defining the CRTM Surface
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       CRTM_GeometryInfo_Define:  Module defining the CRTM GeometryInfo
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       CRTM_SfcOptics_Define:     Module defining the CRTM SfcOptics
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       CRTM_MW_Land_SfcOptics:    Module to compute the surface optical
!                                  properties for LAND surfaces at microwave
!                                  frequencies required for determining the LAND
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_MW_Water_SfcOptics:   Module to compute the surface optical
!                                  properties for WATER surfaces at microwave
!                                  frequencies required for determining the WATER
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_MW_Snow_SfcOptics:    Module to compute the surface optical
!                                  properties for SNOW surfaces at microwave
!                                  frequencies required for determining the SNOW
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_MW_Ice_SfcOptics:     Module to compute the surface optical
!                                  properties for ICE surfaces at microwave
!                                  frequencies required for determining the ICE
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_IR_Land_SfcOptics:    Module to compute the surface optical
!                                  properties for LAND surfaces at infrared
!                                  frequencies required for determining the LAND
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_IR_Water_SfcOptics:   Module to compute the surface optical
!                                  properties for WATER surfaces at infrared
!                                  frequencies required for determining the WATER
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_IR_Snow_SfcOptics:    Module to compute the surface optical
!                                  properties for SNOW surfaces at infrared
!                                  frequencies required for determining the SNOW
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_IR_Ice_SfcOptics:     Module to compute the surface optical
!                                  properties for ICE surfaces at infrared
!                                  frequencies required for determining the ICE
!                                  surface contribution to the radiative transfer.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_SURFACE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_SFCOPTICS_DEFINE module
!
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!       CRTM_Compute_SurfaceT:        Subroutine to compute the average of
!                                     the various surface type temperatures
!                                     weighted by their coverage fraction.
!
!       CRTM_Compute_SurfaceT_TL:     Subroutine to compute the tangent-linear
!                                     average of the various surface type
!                                     temperatures weighted by their coverage
!                                     fraction.
!
!       CRTM_Compute_SurfaceT_AD:     Subroutine to compute the adjoint of the
!                                     average of the various surface type
!                                     temperatures weighted by their coverage
!                                     fraction.
!
!       CRTM_Compute_SfcOptics:       Function to compute the surface optical
!                                     properties.
!
!       CRTM_Compute_SfcOptics_TL:    Function to compute the tangent-linear
!                                     surface optical properties.
!
!       CRTM_Compute_SfcOptics_AD:    Function to compute the adjoint
!                                     surface optical properties.
!
!       PRIVATE subprograms
!       -------------------
!       
!         None.
!
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Associated_SfcOptics:    Function to test the association status
!                                     of the pointer members of a SfcOptics
!                                     structure.
!                                     SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_Destroy_SfcOptics:       Function to re-initialize an
!                                     CRTM_SfcOptics structure.
!                                     SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_Allocate_SfcOptics:      Function to allocate the pointer
!                                     members of an CRTM_SfcOptics
!                                     structure.
!                                     SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!       CRTM_Assign_SfcOptics:        Function to copy an CRTM_SfcOptics
!                                     structure.
!                                     SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!       MICROWAVE functions
!       -------------------
!       Compute_MW_Land_SfcOptics:    Function to compute the surface emissivity
!                                     and reflectivity at microwave frequencies
!                                     over a land surface.
!                                     SOURCE: CRTM_MW_LAND_SFCOPTICS module
!
!       Compute_MW_Water_SfcOptics:   Function to compute the surface emissivity
!                                     and reflectivity at microwave frequencies
!                                     over a water surface.
!                                     SOURCE: CRTM_MW_WATER_SFCOPTICS module
!
!       Compute_MW_Snow_SfcOptics:    Function to compute the surface emissivity
!                                     and reflectivity at microwave frequencies
!                                     over a snow surface.
!                                     SOURCE: CRTM_MW_SNOW_SFCOPTICS module
!
!       Compute_MW_Ice_SfcOptics:     Function to compute the surface emissivity
!                                     and reflectivity at microwave frequencies
!                                     over an ice surface.
!                                     SOURCE: CRTM_MW_ICE_SFCOPTICS module
!
!       INFRARED functions
!       ------------------
!       Compute_IR_Land_SfcOptics:    Function to compute the surface emissivity
!                                     and reflectivity at infrared frequencies
!                                     over a land surface.
!                                     SOURCE: CRTM_IR_LAND_SFCOPTICS module
!
!       Compute_IR_Water_SfcOptics:   Function to compute the surface emissivity
!                                     and reflectivity at infrared frequencies
!                                     over a water surface.
!                                     SOURCE: CRTM_IR_WATER_SFCOPTICS module
!
!       Compute_IR_Snow_SfcOptics:    Function to compute the surface emissivity
!                                     and reflectivity at infrared frequencies
!                                     over a snow surface.
!                                     SOURCE: CRTM_IR_SNOW_SFCOPTICS module
!
!       Compute_IR_Ice_SfcOptics:     Function to compute the surface emissivity
!                                     and reflectivity at infrared frequencies
!                                     over an ice surface.
!                                     SOURCE: CRTM_IR_ICE_SFCOPTICS module
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       02-Apr-2004
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

MODULE CRTM_SfcOptics


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler

  ! -- CRTM modules

  USE CRTM_Parameters
  USE CRTM_SpcCoeff
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  ! -- The SfcOptics structure definition module
  ! -- The PUBLIC entities in CRTM_SfcOptics_Define
  ! -- are also explicitly defined as PUBLIC here so 
  ! -- a user need only USE CRTM_SfcOptics.
  USE CRTM_SfcOptics_Define

  ! -- The SfcOptics Science modules
  USE CRTM_MW_Land_SfcOptics
  USE CRTM_MW_Water_SfcOptics
  USE CRTM_MW_Snow_SfcOptics
  USE CRTM_MW_Ice_SfcOptics

  USE CRTM_IR_Land_SfcOptics
  USE CRTM_IR_Water_SfcOptics
  USE CRTM_IR_Snow_SfcOptics
  USE CRTM_IR_Ice_SfcOptics


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_SfcOptics structure data type
  ! -- in the CRTM_SfcOptics_Define module
  PUBLIC :: CRTM_SfcOptics_type

  ! -- CRTM_SfcOptics structure routines inherited
  ! -- from the CRTM_SfcOptics_Define module
  PUBLIC :: CRTM_Associated_SfcOptics
  PUBLIC :: CRTM_Destroy_SfcOptics
  PUBLIC :: CRTM_Allocate_SfcOptics
  PUBLIC :: CRTM_Assign_SfcOptics

  ! -- Science routines in this modules
  PUBLIC :: CRTM_Compute_SfcOptics
  PUBLIC :: CRTM_Compute_SfcOptics_TL
  PUBLIC :: CRTM_Compute_SfcOptics_AD

  PUBLIC :: CRTM_Compute_SurfaceT
  PUBLIC :: CRTM_Compute_SurfaceT_TL
  PUBLIC :: CRTM_Compute_SurfaceT_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_SfcOptics.f90,v 1.13.4.3 2005/10/19 14:48:34 paulv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################



!  *** USERS INSERT PRIVATE SUBPROGRAMS HERE ***




!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_SurfaceT
!
! PURPOSE:
!       Subroutine to compute the average of the various surface type
!       temperatures weighted by their coverage fraction.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_SurfaceT( Surface,  &  ! Input
!                                   SfcOptics )  ! Output     
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_SurfaceT( Surface,  &  ! Input
                                    SfcOptics )  ! Output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),   INTENT( IN )     :: Surface

    ! -- Output 
    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE WEIGHTED AVERAGE SURFACE TEMPERATURE --       #
    !#--------------------------------------------------------------------------#

    SfcOptics%Surface_Temperature = &
      ( Surface%Land_Coverage  * Surface%Land_Temperature  ) + &
      ( Surface%Water_Coverage * Surface%Water_Temperature ) + &
      ( Surface%Snow_Coverage  * Surface%Snow_Temperature  ) + &
      ( Surface%Ice_Coverage   * Surface%Ice_Temperature   )

  END SUBROUTINE CRTM_Compute_SurfaceT 





!----------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_SurfaceT_TL
!
! PURPOSE:
!       Subroutine to compute the tangent-linear average of the various
!       surface type temperatures weighted by their coverage fraction.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_SurfaceT_TL( Surface,     &  ! Input
!                                      Surface_TL,  &  ! Input     
!                                      SfcOptics_TL )  ! In/Output     
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linerar
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_SurfaceT_TL( Surface,     &  ! Input
                                       Surface_TL,  &  ! Input
                                       SfcOptics_TL )  ! Output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),   INTENT( IN )     :: Surface
    TYPE( CRTM_Surface_type ),   INTENT( IN )     :: Surface_TL

    ! -- Output 
    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics_TL



    !#--------------------------------------------------------------------------#
    !#  -- COMPUTE THE TANGENT-LINEAR WEIGHTED AVERAGE SURFACE TEMPERATURE --   #
    !#--------------------------------------------------------------------------#


    SfcOptics_TL%Surface_Temperature = &
      ( Surface%Land_Coverage  * Surface_TL%Land_Temperature  ) + &
      ( Surface%Water_Coverage * Surface_TL%Water_Temperature ) + &
      ( Surface%Snow_Coverage  * Surface_TL%Snow_Temperature  ) + &
      ( Surface%Ice_Coverage   * Surface_TL%Ice_Temperature   )

  END SUBROUTINE CRTM_Compute_SurfaceT_TL





!----------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_SurfaceT_AD
!
! PURPOSE:
!       Subroutine to compute the adjoint of the average of the various
!       surface type temperatures weighted by their coverage fraction.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_SurfaceT_AD( Surface,      &  ! Input
!                                      SfcOptics_AD, &  ! Input
!                                      Surface_AD    )  ! Output
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface temperature required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       Even though the SfcOptics_AD argument is listed as an INPUT, its
!       INTENT is ( IN OUT ) as it is modified on output since the
!       Surface_Temperature component is set to zero after the adjoint
!       calculation.
!
!       Even though the Surface_AD argument is listed as an OUTPUT, its
!       INTENT is ( IN OUT ) as the components of the adjoint calculation
!       in this routine may already have a value from a previous adjoint
!       calculation performed on the structure.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       In addition to the input/output requirements described in the SIDE
!       EFFECTS section, the SfcOptics_AD and Surface_AD arguments require
!       an INTENT of IN OUT to prevent memory leaks.
!
!S-
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_SurfaceT_AD( Surface,      &  ! Input
                                       SfcOptics_AD, &  ! Input
                                       Surface_AD    )  ! Output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),   INTENT( IN )     :: Surface
    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics_AD

    ! -- Output 
    TYPE( CRTM_Surface_type ),   INTENT( IN OUT ) :: Surface_AD



    !#--------------------------------------------------------------------------#
    !#       -- COMPUTE THE ADJOINT WEIGHTED AVERAGE SURFACE TEMPERATURE --     #
    !#--------------------------------------------------------------------------#


    Surface_AD%Land_Temperature  = Surface_AD%Land_Temperature + &
                                   ( Surface%Land_Coverage  * SfcOptics_AD%Surface_Temperature )

    Surface_AD%Water_Temperature = Surface_AD%Water_Temperature + &
                                   ( Surface%Water_Coverage * SfcOptics_AD%Surface_Temperature )

    Surface_AD%Snow_Temperature  = Surface_AD%Snow_Temperature  + &
                                   ( Surface%Snow_Coverage  * SfcOptics_AD%Surface_Temperature )

    Surface_AD%Ice_Temperature   = Surface_AD%Ice_Temperature   + &
                                   ( Surface%Ice_Coverage   * SfcOptics_AD%Surface_Temperature )

    SfcOptics_AD%Surface_Temperature = ZERO


  END SUBROUTINE CRTM_Compute_SurfaceT_AD


!----------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_SfcOptics
!
! PURPOSE:
!       Function to compute the surface optical properties and populate
!       the output SfcOptics structure for a single channel.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics( Surface,                  &  ! Input
!                                              GeometryInfo,             &  ! Input
!                                              Channel_Index,            &  ! Input, scalar
!                                              SfcOptics,                &  ! Output     
!                                              Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_GeometryInfo_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        On Input:  The Secant_Angle component is assumed to
!                                   contain data.
!                        On Output: The Emissivity and Reflectivity components
!                                   will contain the required data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       None of the input or output structures are checked in this routine as
!       it will be called for every channel.
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT. This is necessary because the argument should be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics( Surface,       &  ! Input
                                   GeometryInfo,  &  ! Input
                                   Channel_Index, &  ! Input
                                   SfcOptics,     &  ! Output
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- In/Output 
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics'

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    REAL( fp_kind ) :: SIN2_Angle, Sensor_Scan_RADIANS
    REAL( fp_kind ), DIMENSION( SfcOptics%n_Angles, MAX_N_STOKES  ) :: Emissivity
    REAL( fp_kind ), DIMENSION( SfcOptics%n_Angles, MAX_N_STOKES, &
                                SfcOptics%n_Angles, MAX_N_STOKES  ) :: Reflectivity
    INTEGER         :: Sensor_Type, Polarization



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status  = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALISE LOCAL VARIABLES --                      #
    !#--------------------------------------------------------------------------#

    ! -- Assign a short name to the USED SfcOptics dimensions
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles

    ! -- Sensor type and polarization
    Sensor_Type  = SC%Sensor_Type( Channel_Index )  
    Polarization = SC%Polarization( Channel_Index )

    ! -- Initialise the local emissivity and reflectivities
    Emissivity = ZERO 
    Reflectivity = ZERO



    !#--------------------------------------------------------------------------#
    !#                        -- BRANCH ON SENSOR TYPE --                       #
    !#--------------------------------------------------------------------------#

    Sensor_Select: SELECT CASE ( Sensor_Type )



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( MICROWAVE_SENSOR )

        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------

        Microwave_Land: IF( Surface%Land_Coverage > ZERO) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Land_SfcOptics( Surface,       &  ! Input
                                                    GeometryInfo,  &  ! Input
                                                    Channel_Index, &  ! Input
                                                    SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW land SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index                              
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on land coverage fraction
          Emissivity(1:nZ,1:2)            = SfcOptics%Emissivity(1:nZ,1:2) * Surface%Land_Coverage
          Reflectivity(1:nZ,1:2,1:nZ,1:2) = SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Land_Coverage

         ENDIF Microwave_Land


        ! ---------------------------------------
        ! Microwave WATER emissivity/reflectivity
        ! ---------------------------------------

        Microwave_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Water_SfcOptics( Surface,       &  ! Input
                                                     GeometryInfo,  &  ! Input
                                                     Channel_Index, &  ! Input
                                                     SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW water SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on water coverage fraction
          Emissivity(1:nZ,1:2)            = Emissivity(1:nZ,1:2) + &
                                            ( SfcOptics%Emissivity(1:nZ,1:2) * Surface%Water_Coverage )

          Reflectivity(1:nZ,1:2,1:nZ,1:2) = Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
                                            ( SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Water_Coverage )

         ENDIF Microwave_Water


        ! --------------------------------------
        ! Microwave SNOW emissivity/reflectivity
        ! --------------------------------------

        Microwave_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Snow_SfcOptics( Surface,       &  ! Input
                                                    GeometryInfo,  &  ! Input
                                                    Channel_Index, &  ! Input
                                                    SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW snow SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on snow coverage fraction
          Emissivity(1:nZ,1:2)            = Emissivity(1:nZ,1:2) + &
                                            ( SfcOptics%Emissivity(1:nZ,1:2) * Surface%Snow_Coverage )

          Reflectivity(1:nZ,1:2,1:nZ,1:2) = Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
                                            ( SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Snow_Coverage )

        ENDIF Microwave_Snow


        ! -------------------------------------
        ! Microwave ICE emissivity/reflectivity
        ! -------------------------------------

        Microwave_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Ice_SfcOptics( Surface,       &  ! Input
                                                   GeometryInfo,  &  ! Input
                                                   Channel_Index, &  ! Input
                                                   SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW ice SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on snow coverage fraction
          Emissivity(1:nZ,1:2)            = Emissivity(1:nZ,1:2) + &
                                            ( SfcOptics%Emissivity(1:nZ,1:2) * Surface%Ice_Coverage )

          Reflectivity(1:nZ,1:2,1:nZ,1:2) = Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
                                            ( SfcOptics%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Ice_Coverage )

        ENDIF Microwave_Ice



        !#----------------------------------------------------------------------#
        !#                 -- HANDLE THE DECOUPLED POLARISATION --              #
        !#                                                                      #
        !# The SfcOptics n_Stokes dimension determines whether the surface      #
        !# optics takes into account the second order effect of cross           #
        !# polarisation, e.g. if the surface optics for a purely vertically     #
        !# polarised channel has a horizontal (or other) component due to       #
        !# scattering at the surface.                                           #
        !#                                                                      #
        !# If the SfcOptics n_Stokes dimension == 1, the polarisations are      #
        !# decoupled.                                                           #
        !#----------------------------------------------------------------------#

        Decoupled_Polarization: IF( SfcOptics%n_Stokes == 1 ) THEN


          ! ------------------------------------------------------
          ! Decoupled polarisation. Branch on channel polarisation
          ! ------------------------------------------------------


          Polarization_Type: SELECT CASE( Polarization )


            ! -- The unpolarised case, I
            ! -- e = (eV + eH)/2
            ! -- r = (rV + rH)/2
            ! -- Note: INTENSITY == UNPOLARIZED == FIRST_STOKES_COMPONENT
            CASE( INTENSITY )

              SfcOptics%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity(1:nZ,1) + Emissivity(1:nZ,2) )

              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity(1:nZ,1,1:nZ,1) + Reflectivity(1:nZ,2,1:nZ,2) )


            ! -- The second Stokes component, Q, the polarisation difference.
            ! -- e = (eV - eH)/2
            ! -- r = (rV - rH)/2
            CASE( SECOND_STOKES_COMPONENT ) 

              SfcOptics%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity(1:nZ,1) - Emissivity(1:nZ,2) )

              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity(1:nZ,1,1:nZ,1) - Reflectivity(1:nZ,2,1:nZ,2) )


            ! -- The third Stokes component, U.
            CASE ( THIRD_STOKES_COMPONENT ) 

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,3)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,3,1:nZ,3)


            ! -- The fourth Stokes component, V.
            CASE ( FOURTH_STOKES_COMPONENT )  

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,4)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,4,1:nZ,4)


            ! -- Vertical linear polarisation
            CASE ( VL_POLARIZATION ) 

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)


            ! -- Horizontal linear polarisation
            CASE ( HL_POLARIZATION ) 

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,2)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,2,1:nZ,2)

            ! -- +45deg. linear polarisation
            CASE ( plus45L_POLARIZATION ) 

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)


            ! -- -45deg. linear polarisation
            CASE ( minus45L_POLARIZATION ) 

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)


            ! -- Vertical, mixed polarisation. This category of polarisation is
            ! -- for those microwave channels where the nadir polarisation is
            ! -- vertical, but the instrument scans cross-track.
            ! -- e = eV * COS^2(z)  +  eH * (1-COS^2(z))
            ! -- r = rV * COS^2(z)  +  rH * (1-COS^2(z))
            CASE ( VL_MIXED_POLARIZATION )


              DO i = 1, nZ
                SIN2_Angle =  ( GeometryInfo%Distance_Ratio &
                 * SIN(DEGREES_TO_RADIANS * SfcOptics%Angle(i) ) )**2 

                SfcOptics%Emissivity(i,1) = ( Emissivity(i,1) * (ONE-SIN2_Angle) + &
                                            ( Emissivity(i,2) * SIN2_Angle) )

                SfcOptics%Reflectivity(i,1,i,1) = ( Reflectivity(i,1,i,1) * (ONE-SIN2_Angle) ) + &
                                                  ( Reflectivity(i,2,i,2) * SIN2_Angle )
              ENDDO


            ! -- Horizontal, mixed polarisation. This category of polarisation is
            ! -- for those microwave channels where the nadir polarisation is
            ! -- horizontal, but the instrument scans cross-track.
            ! -- e = eV * (1-COS^2(z))  +  eH * COS^2(z)
            ! -- r = rV * (1-COS^2(z))  +  rH * COS^2(z)
            CASE ( HL_MIXED_POLARIZATION )

              DO i = 1, nZ
                SIN2_Angle =  ( GeometryInfo%Distance_Ratio &
                 * SIN(DEGREES_TO_RADIANS * SfcOptics%Angle(i) ) )**2

                SfcOptics%Emissivity(i,1) = ( Emissivity(i,1) * SIN2_Angle ) + &
                                            ( Emissivity(i,2) * (ONE-SIN2_Angle) )

                SfcOptics%Reflectivity(i,1,i,1) = ( Reflectivity(i,1,i,1) * SIN2_Angle ) + &
                                                  ( Reflectivity(i,2,i,2) * (ONE-SIN2_Angle)  )
              ENDDO


            ! -- Right circular polarisation
            CASE ( RC_POLARIZATION )

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)


            ! -- Left circular polarisation
            CASE ( LC_POLARIZATION )

              SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
              SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)


            ! -- Serious problem if we got to this points
            CASE DEFAULT

               Error_Status = FAILURE
               WRITE( Message, '( "Unrecognised polarization flag for microwave ",&
                                 &"channel index ", i4 )' ) Channel_Index
               CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
               RETURN
 
           END SELECT Polarization_Type


        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------

          SfcOptics%Emissivity(1:nZ,1:nL)             = Emissivity(1:nZ,1:nL)
          SfcOptics%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = Reflectivity(1:nZ,1:nL,1:nZ,1:nL)

        ENDIF Decoupled_Polarization



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( INFRARED_SENSOR )


        ! -------------------------------------          
        ! Infrared LAND emissivity/reflectivity
        ! -------------------------------------

        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Land_SfcOptics( Surface,       &  ! Input
                                                    GeometryInfo,  &  ! Input
                                                    Channel_Index, &  ! Input
                                                    SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on land coverage fraction
          Emissivity(1:nZ,1)          = SfcOptics%Emissivity(1:nZ,1)          * Surface%Land_Coverage
          Reflectivity(1:nZ,1,1:nZ,1) = SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Land_Coverage

        ENDIF Infrared_Land


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------

        Infrared_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Water_SfcOptics( Surface,       &  ! Input
                                                     GeometryInfo,  &  ! Input
                                                     Channel_Index, &  ! Input
                                                     SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on water coverage fraction
          Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
            ( SfcOptics%Emissivity(1:nZ,1) * Surface%Water_Coverage )

          Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + & 
            ( SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Water_Coverage )

        END IF Infrared_Water

        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------

        Infrared_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Snow_SfcOptics( Surface,       &  ! Input
                                                    GeometryInfo,  &  ! Input
                                                    Channel_Index, &  ! Input
                                                    SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on snow coverage fraction
          Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
            ( SfcOptics%Emissivity(1:nZ,1) * Surface%Snow_Coverage )

          Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + & 
            ( SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Snow_Coverage )

        ENDIF Infrared_Snow


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------

        Infrared_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Ice_SfcOptics( Surface,       &  ! Input
                                                   GeometryInfo,  &  ! Input
                                                   Channel_Index, &  ! Input
                                                   SfcOptics      )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on Ice coverage fraction
          Emissivity(1:nZ,1) = Emissivity(1:nZ,1) + &
            ( SfcOptics%Emissivity(1:nZ,1) * Surface%Ice_Coverage )

          Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1) + & 
            ( SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Ice_Coverage )

        ENDIF Infrared_Ice


        ! -----------------------
        ! Assign the final result
        ! -----------------------

        SfcOptics%Emissivity(1:nZ,1)          = Emissivity(1:nZ,1)
        SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity(1:nZ,1,1:nZ,1)



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                       ## VISIBLE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( VISIBLE_SENSOR )


        ! -------------------
        ! Default values only
        ! -------------------

        SfcOptics%Emissivity(1:nZ,1)          = 0.95_fp_kind
        SfcOptics%Reflectivity(1:nZ,1,1:nZ,1) = ONE - SfcOptics%Emissivity(1,1)



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE DEFAULT

        Error_Status = FAILURE
        WRITE( Message, '( "Unrecognised sensor type for channel index ", i4 )' ) &
                        Channel_Index
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN

    END SELECT Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics





!----------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface optical properties
!       and populate the output SfcOptics_TL structure for a single channel.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics_TL( Surface,                  &  ! Input
!                                                 SfcOptics,                &  ! Input     
!                                                 Surface_TL,               &  ! Input
!                                                 GeometryInfo,             &  ! Input
!                                                 Channel_Index,            &  ! Input, scalar
!                                                 SfcOptics_TL,             &  ! In/Output     
!                                                 Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_GeometryInfo_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the radiative
!                        transfer calculation.
!                        On Input:  The Secant_Angle component is assumed to
!                                   contain data.
!                        On Output: The Emissivity and Reflectivity components
!                                   will contain the required data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       None of the input or output structures are checked in this routine as
!       it will be called for every channel.
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument should be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics_TL( Surface,       &  ! Input
                                      SfcOptics,     &  ! Input
                                      Surface_TL,    &  ! Input
                                      GeometryInfo,  &  ! Input
                                      Channel_Index, &  ! Input
                                      SfcOptics_TL,  &  ! Output
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN )     :: SfcOptics
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface_TL
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- In/Output 
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics_TL

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    INTEGER :: Sensor_Type, Polarization
    REAL( fp_kind ) :: SIN2_Angle, Sensor_Scan_RADIANS
    REAL( fp_kind ), DIMENSION( SfcOptics%n_Angles, MAX_N_STOKES  ) :: Emissivity_TL
    REAL( fp_kind ), DIMENSION( SfcOptics%n_Angles, MAX_N_STOKES, &
                                SfcOptics%n_Angles, MAX_N_STOKES  ) :: Reflectivity_TL


    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    !#--------------------------------------------------------------------------#
    !#                    -- INITIALISE LOCAL VARIABLES --                      #
    !#--------------------------------------------------------------------------#

    ! -- Assign a short name to the USED SfcOptics dimensions
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles

    ! -- Sensor type and polarization
    Sensor_Type  = SC%Sensor_Type( Channel_Index )  
    Polarization = SC%Polarization( Channel_Index )

    ! -- Initialise the local emissivity and reflectivities
    Emissivity_TL   = ZERO 
    Reflectivity_TL = ZERO



    !#--------------------------------------------------------------------------#
    !#                        -- BRANCH ON SENSOR TYPE --                       #
    !#--------------------------------------------------------------------------#

    Sensor_Select: SELECT CASE ( Sensor_Type )



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( MICROWAVE_SENSOR )

        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------

        Microwave_Land: IF( Surface%Land_Coverage > ZERO) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Land_SfcOptics_TL( Surface,       &  ! Input
                                                       SfcOptics,     &  ! Input
                                                       Surface_TL,    &  ! Input
                                                       GeometryInfo,  &  ! Input
                                                       Channel_Index, &  ! Input
                                                       SfcOptics_TL,  &  ! In/Output
                                                       Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW land SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on land coverage fraction
          Emissivity_TL(1:nZ,1:2) = &
            SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Land_Coverage

          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Land_Coverage

        ENDIF Microwave_Land


        ! ---------------------------------------
        ! Microwave WATER emissivity/reflectivity
        ! ---------------------------------------

        Microwave_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Water_SfcOptics_TL( Surface,       &  ! Input
                                                        SfcOptics,     &  ! Input
                                                        Surface_TL,    &  ! Input
                                                        GeometryInfo,  &  ! Input
                                                        Channel_Index, &  ! Input
                                                        SfcOptics_TL,  &  ! In/Output
                                                        Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW water SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on water coverage fraction
          Emissivity_TL(1:nZ,1:2) = Emissivity_TL(1:nZ,1:2) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Water_Coverage )

          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Water_Coverage )

        ENDIF Microwave_Water


        ! --------------------------------------
        ! Microwave SNOW emissivity/reflectivity
        ! --------------------------------------

        Microwave_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Snow_SfcOptics_TL( Surface,       &  ! Input
                                                       SfcOptics,     &  ! Input
                                                       Surface_TL,    &  ! Input
                                                       GeometryInfo,  &  ! Input
                                                       Channel_Index, &  ! Input
                                                       SfcOptics_TL,  &  ! In/Output
                                                       Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW snow SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on snow coverage fraction
          Emissivity_TL(1:nZ,1:2) = Emissivity_TL(1:nZ,1:2) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Snow_Coverage )

          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Snow_Coverage )

        ENDIF Microwave_Snow


        ! -------------------------------------
        ! Microwave ICE emissivity/reflectivity
        ! -------------------------------------

        Microwave_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_MW_Ice_SfcOptics_TL( Surface,       &  ! Input
                                                      SfcOptics,     &  ! Input
                                                      Surface_TL,    &  ! Input
                                                      GeometryInfo,  &  ! Input
                                                      Channel_Index, &  ! Input
                                                      SfcOptics_TL,  &  ! In/Output
                                                      Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW ice SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on snow coverage fraction
          Emissivity_TL(1:nZ,1:2) = Emissivity_TL(1:nZ,1:2) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1:2) * Surface%Ice_Coverage )

          Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) = Reflectivity_TL(1:nZ,1:2,1:nZ,1:2) + &
            ( SfcOptics_TL%Reflectivity(1:nZ,1:2,1:nZ,1:2) * Surface%Ice_Coverage )

        ENDIF Microwave_Ice




        !#----------------------------------------------------------------------#
        !#                 -- HANDLE THE DECOUPLED POLARISATION --              #
        !#                                                                      #
        !# The SfcOptics n_Stokes dimension determines whether the surface      #
        !# optics takes into account the second order effect of cross           #
        !# polarisation, e.g. if the surface optics for a purely vertically     #
        !# polarised channel has a horizontal (or other) component due to       #
        !# scattering at the surface.                                           #
        !#                                                                      #
        !# If the SfcOptics n_Stokes dimension == 1, the polarisations are      #
        !# decoupled.                                                           #
        !#----------------------------------------------------------------------#

        Decoupled_Polarization: IF( SfcOptics%n_Stokes == 1 ) THEN


          ! ------------------------------------------------------
          ! Decoupled polarisation. Branch on channel polarisation
          ! ------------------------------------------------------

          Polarization_Type: SELECT CASE( Polarization )


            ! -- The unpolarised case, I
            ! -- e = (eV + eH)/2
            ! -- r = (rV + rH)/2
            ! -- Note: INTENSITY == UNPOLARIZED == FIRST_STOKES_COMPONENT
            CASE( INTENSITY )

              SfcOptics_TL%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity_TL(1:nZ,1) + Emissivity_TL(1:nZ,2) )

              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity_TL(1:nZ,1,1:nZ,1) + Reflectivity_TL(1:nZ,2,1:nZ,2) )


            ! -- The second Stokes component, Q, the polarisation difference.
            ! -- e = (eV - eH)/2
            ! -- r = (rV - rH)/2
            CASE( SECOND_STOKES_COMPONENT ) 

              SfcOptics_TL%Emissivity(1:nZ,1) = &
                POINT_5 * ( Emissivity_TL(1:nZ,1) - Emissivity_TL(1:nZ,2) )

              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = &
                POINT_5 * ( Reflectivity_TL(1:nZ,1,1:nZ,1) - Reflectivity_TL(1:nZ,2,1:nZ,2) )


            ! -- The third Stokes component, U.
            CASE ( THIRD_STOKES_COMPONENT ) 

              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,3)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,3,1:nZ,3)


            ! -- The fourth Stokes component, V.
            CASE ( FOURTH_STOKES_COMPONENT )  

              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,4)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,4,1:nZ,4)


            ! -- Vertical linear polarisation
            CASE ( VL_POLARIZATION ) 

              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)


            ! -- Horizontal linear polarisation
            CASE ( HL_POLARIZATION ) 

              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,2)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(:,2,:,2)


            ! -- +45deg. linear polarisation
            CASE ( plus45L_POLARIZATION ) 

              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)


            ! -- -45deg. linear polarisation
            CASE ( minus45L_POLARIZATION ) 

              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)


            ! -- Vertical, mixed polarisation. This category of polarisation is
            ! -- for those microwave channels where the nadir polarisation is
            ! -- vertical, but the instrument scans cross-track.
            ! -- e = eV * COS^2(z)  +  eH * (1-COS^2(z))
            ! -- r = rV * COS^2(z)  +  rH * (1-COS^2(z))
            CASE ( VL_MIXED_POLARIZATION )

              DO i = 1, nZ
                SIN2_Angle =  ( GeometryInfo%Distance_Ratio &
                 * SIN(DEGREES_TO_RADIANS * SfcOptics%Angle(i) ) )**2

                SfcOptics_TL%Emissivity(i,1) = ( Emissivity_TL(i,1) * (ONE-SIN2_Angle) ) + &
                                               ( Emissivity_TL(i,2) * SIN2_Angle )

                SfcOptics_TL%Reflectivity(i,1,i,1) = ( Reflectivity_TL(i,1,i,1) * (ONE-SIN2_Angle) ) + &
                                                     ( Reflectivity_TL(i,2,i,2) * SIN2_Angle )
              ENDDO


            ! -- Horizontal, mixed polarisation. This category of polarisation is
            ! -- for those microwave channels where the nadir polarisation is
            ! -- horizontal, but the instrument scans cross-track.
            ! -- e = eV * (1-COS^2(z))  +  eH * COS^2(z)
            ! -- r = rV * (1-COS^2(z))  +  rH * COS^2(z)
            CASE ( HL_MIXED_POLARIZATION )

              DO i = 1, nZ
                SIN2_Angle =  ( GeometryInfo%Distance_Ratio &
                 * SIN(DEGREES_TO_RADIANS * SfcOptics%Angle(i) ) )**2

                SfcOptics_TL%Emissivity(i,1) = ( Emissivity_TL(i,1) * SIN2_Angle ) + &
                                               ( Emissivity_TL(i,2) * (ONE-SIN2_Angle)  )

                SfcOptics_TL%Reflectivity(i,1,i,1) = ( Reflectivity_TL(i,1,i,1) * SIN2_Angle ) + &
                                                     ( Reflectivity_TL(i,2,i,2) * (ONE-SIN2_Angle) )
              ENDDO


            ! -- Right circular polarisation
            CASE ( RC_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)


            ! -- Left circular polarisation
            CASE ( LC_POLARIZATION )
              SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
              SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)


            ! -- Serious problem if we got to this point
            CASE DEFAULT

               Error_Status = FAILURE
               WRITE( Message, '( "Unrecognised polarization flag for microwave ",&
                                 &"channel index ", i4 )' ) &
                               Channel_Index
               CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
               RETURN
 
           END SELECT Polarization_Type


        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------

          SfcOptics_TL%Emissivity   = Emissivity_TL(1:nZ,1:nL)
          SfcOptics_TL%Reflectivity = Reflectivity_TL(1:nZ,1:nL,1:nZ,1:nL)

        ENDIF Decoupled_Polarization





      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( INFRARED_SENSOR )


        ! -------------------------------------
        ! Infrared LAND emissivity/reflectivity
        ! -------------------------------------

        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Land_SfcOptics_TL( Surface,       &  ! Input
                                                       SfcOptics,     &  ! Input
                                                       Surface_TL,    &  ! Input
                                                       GeometryInfo,  &  ! Input
                                                       Channel_Index, &  ! Input
                                                       SfcOptics_TL,  &  ! In/Output
                                                       Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on land coverage fraction
          Emissivity_TL(1:nZ,1) = &
            SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Land_Coverage

          Reflectivity_TL(1:nZ,1,1:nZ,1) = &
            SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Land_Coverage

        ENDIF Infrared_Land


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------

        Infrared_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Water_SfcOptics_TL( Surface,       &  ! Input
                                                        SfcOptics,     &  ! Input
                                                        Surface_TL,    &  ! Input
                                                        GeometryInfo,  &  ! Input
                                                        Channel_Index, &  ! Input
                                                        SfcOptics_TL,  &  ! In/Output
                                                        Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF


          ! -- Accumulate the surface optics properties
          ! -- based on water coverage fraction
          Emissivity_TL(1:nZ,1) = Emissivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Water_Coverage )

          Reflectivity_TL(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1) + & 
            ( SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Water_Coverage )

        END IF Infrared_Water


        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------

        Infrared_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Snow_SfcOptics_TL( Surface,       &  ! Input
                                                       SfcOptics,     &  ! Input
                                                       Surface_TL,    &  ! Input
                                                       GeometryInfo,  &  ! Input
                                                       Channel_Index, &  ! Input
                                                       SfcOptics_TL,  &  ! In/Output
                                                       Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on snow coverage fraction
          Emissivity_TL(1:nZ,1) = Emissivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Snow_Coverage )

          Reflectivity_TL(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1) + & 
            ( SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Snow_Coverage )

        ENDIF Infrared_Snow


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------

        Infrared_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! -- Compute the surface optics
          Error_Status = Compute_IR_Ice_SfcOptics_TL( Surface,       &  ! Input
                                                      SfcOptics,     &  ! Input
                                                      Surface_TL,    &  ! Input
                                                      GeometryInfo,  &  ! Input
                                                      Channel_Index, &  ! Input
                                                      SfcOptics_TL,  &  ! In/Output
                                                      Message_Log = Message_Log ) ! Error_message

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics_TL at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

          ! -- Accumulate the surface optics properties
          ! -- based on Ice coverage fraction
          Emissivity_TL(1:nZ,1) = Emissivity_TL(1:nZ,1) + &
            ( SfcOptics_TL%Emissivity(1:nZ,1) * Surface%Ice_Coverage )

          Reflectivity_TL(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1) + & 
            ( SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) * Surface%Ice_Coverage )

        ENDIF Infrared_Ice


        ! -----------------------
        ! Assign the final result
        ! -----------------------

        SfcOptics_TL%Emissivity(1:nZ,1)          = Emissivity_TL(1:nZ,1)
        SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = Reflectivity_TL(1:nZ,1,1:nZ,1)



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                       ## VISIBLE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( VISIBLE_SENSOR )


        ! -------------------
        ! Default values only
        ! -------------------

        SfcOptics_TL%Emissivity(1:nZ,1)          = ZERO
        SfcOptics_TL%Reflectivity(1:nZ,1,1:nZ,1) = ZERO



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE DEFAULT

        Error_Status = FAILURE
        WRITE( Message, '( "Unrecognised sensor type for channel index ", i4 )' ) &
                        Channel_Index
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN

    END SELECT Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics_TL





!----------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface optical properties
!       for a single channel.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_SfcOptics_AD( Surface,                  &  ! Input
!                                                 SfcOptics,                &  ! Input
!                                                 SfcOptics_AD,             &  ! Input
!                                                 GeometryInfo,             &  ! Input
!                                                 Channel_Index,            &  ! Input
!                                                 Surface_AD,               &  ! Output
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties.
!                        **NOTE: On EXIT from this function, the contents of
!                                this structure may be modified (e.g. set to
!                                zero.)
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_GeometryInfo_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        **NOTE: On ENTRY to this function, the contents of
!                                this structure should be defined (e.g.
!                                initialized to some value based on the
!                                position of this function in the call chain.)
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!S-
!----------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_SfcOptics_AD( Surface,       &  ! Input
                                      SfcOptics,     &  ! Input
                                      SfcOptics_AD,  &  ! Input
                                      GeometryInfo,  &  ! Input
                                      Channel_Index, &  ! Input
                                      Surface_AD,    &  ! Output
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN )     :: SfcOptics
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics_AD
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- In/Output 
    TYPE( CRTM_Surface_type ),      INTENT( IN OUT ) :: Surface_AD

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_SfcOptics_AD'


    ! ---------------
    ! Local variables
    ! ---------------
    CHARACTER(256)  :: Message
    INTEGER :: i
    INTEGER :: nL, nZ
    INTEGER :: Sensor_Type, Polarization
    REAL( fp_kind ) :: SIN2_Angle, Sensor_Scan_RADIANS
    REAL( fp_kind ), DIMENSION( SfcOptics%n_Angles, MAX_N_STOKES  ) :: Emissivity_AD
    REAL( fp_kind ), DIMENSION( SfcOptics%n_Angles, MAX_N_STOKES, &
                                SfcOptics%n_Angles, MAX_N_STOKES  ) :: Reflectivity_AD



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALISE LOCAL VARIABLES --                      #
    !#--------------------------------------------------------------------------#

    ! -- Assign a short name to the USED SfcOptics dimensions
    nL = SfcOptics%n_Stokes
    nZ = SfcOptics%n_Angles

    ! -- Sensor type and polarization
    Sensor_Type  = SC%Sensor_Type( Channel_Index )  
    Polarization = SC%Polarization( Channel_Index )

    ! -- Initialise the local emissivity and reflectivity adjoints
    Emissivity_AD = ZERO 
    Reflectivity_AD = ZERO



    !#--------------------------------------------------------------------------#
    !#                        -- BRANCH ON SENSOR TYPE --                       #
    !#--------------------------------------------------------------------------#

    Sensor_Select: SELECT CASE ( Sensor_Type )



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                     ## MICROWAVE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( MICROWAVE_SENSOR )



        !#----------------------------------------------------------------------#
        !#                 -- HANDLE THE DECOUPLED POLARISATION --              #
        !#                                                                      #
        !# The SfcOptics n_Stokes dimension determines whether the surface      #
        !# optics takes into account the second order effect of cross           #
        !# polarisation, e.g. if the surface optics for a purely vertically     #
        !# polarised channel has a horizontal (or other) component due to       #
        !# scattering at the surface.                                           #
        !#                                                                      #
        !# If the SfcOptics n_Stokes dimension == 1, the polarisations are      #
        !# decoupled.                                                           #
        !#----------------------------------------------------------------------#

        Decoupled_Polarization: IF( SfcOptics%n_Stokes == 1 ) THEN


          ! ------------------------------------------------------
          ! Decoupled polarisation. Branch on channel polarisation
          ! ------------------------------------------------------

          Polarization_Type: SELECT CASE( Polarization )


            ! -- The unpolarised case, I
            ! -- e = (eV + eH)/2
            ! -- r = (rV + rH)/2
            ! -- Note: INTENSITY == UNPOLARIZED == FIRST_STOKES_COMPONENT
            CASE( INTENSITY )

              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              Emissivity_AD(1:nZ,2) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1) 
              Reflectivity_AD(1:nZ,2,1:nZ,2) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO
 

            ! -- The second Stokes component, Q, the polarisation difference.
            ! -- e = (eV - eH)/2
            ! -- r = (rV - rH)/2
            CASE( SECOND_STOKES_COMPONENT ) 

              Emissivity_AD(1:nZ,1) =  SfcOptics_AD%Emissivity(1:nZ,1)
              Emissivity_AD(1:nZ,2) = -SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) =  SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              Reflectivity_AD(1:nZ,2,1:nZ,2) = -SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO
 

            ! -- The third Stokes component, U.
            CASE ( THIRD_STOKES_COMPONENT ) 

              Emissivity_AD(1:nZ,3) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,3,1:nZ,3) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- The fourth Stokes component, V.
            CASE ( FOURTH_STOKES_COMPONENT )  

              Emissivity_AD(1:nZ,4) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,4,1:nZ,4) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- Vertical linear polarisation
            CASE ( VL_POLARIZATION ) 

              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- Horizontal linear polarisation
            CASE ( HL_POLARIZATION ) 

              Emissivity_AD(1:nZ,2) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,2,1:nZ,2) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- +45deg. linear polarisation
            CASE ( plus45L_POLARIZATION ) 

              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- -45deg. linear polarisation
            CASE ( minus45L_POLARIZATION ) 

              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- Vertical, mixed polarisation. This category of polarisation is
            ! -- for those microwave channels where the nadir polarisation is
            ! -- vertical, but the instrument scans cross-track.
            ! -- e = eV * COS^2(z)  +  eH * (1-COS^2(z))
            ! -- r = rV * COS^2(z)  +  rH * (1-COS^2(z))
            CASE ( VL_MIXED_POLARIZATION )

              DO i = 1, nZ
                SIN2_Angle =  ( GeometryInfo%Distance_Ratio &
                 * SIN(DEGREES_TO_RADIANS * SfcOptics%Angle(i) ) )**2

                Emissivity_AD(i,1) = SfcOptics_AD%Emissivity(i,1) * (ONE-SIN2_Angle)
                Emissivity_AD(i,2) = SfcOptics_AD%Emissivity(i,1) * SIN2_Angle

                Reflectivity_AD(i,1,i,1) = SfcOptics_AD%Reflectivity(i,1,i,1) * (ONE-SIN2_Angle)
                Reflectivity_AD(i,2,i,2) = SfcOptics_AD%Reflectivity(i,1,i,1) * SIN2_Angle

              ENDDO

                SfcOptics_AD%Emissivity = ZERO
                SfcOptics_AD%Reflectivity = ZERO

            ! -- Horizontal, mixed polarisation. This category of polarisation is
            ! -- for those microwave channels where the nadir polarisation is
            ! -- horizontal, but the instrument scans cross-track.
            ! -- e = eV * (1-COS^2(z))  +  eH * COS^2(z)
            ! -- r = rV * (1-COS^2(z))  +  rH * COS^2(z)
            CASE ( HL_MIXED_POLARIZATION )

              DO i = 1, nZ     
               SIN2_Angle =  ( GeometryInfo%Distance_Ratio &
                 * SIN(DEGREES_TO_RADIANS * SfcOptics%Angle(i) ) )**2

                Emissivity_AD(i,1) = SfcOptics_AD%Emissivity(i,1) * SIN2_Angle
                Emissivity_AD(i,2) = SfcOptics_AD%Emissivity(i,1) * (ONE-SIN2_Angle)  

                Reflectivity_AD(i,1,i,1) = SfcOptics_AD%Reflectivity(i,1,i,1) * SIN2_Angle
                Reflectivity_AD(i,2,i,2) = SfcOptics_AD%Reflectivity(i,1,i,1) * (ONE-SIN2_Angle)

              ENDDO

                SfcOptics_AD%Emissivity = ZERO
                SfcOptics_AD%Reflectivity = ZERO

            ! -- Right circular polarisation
            CASE ( RC_POLARIZATION )

              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- Left circular polarisation
            CASE ( LC_POLARIZATION )

              Emissivity_AD(1:nZ,1) = SfcOptics_AD%Emissivity(1:nZ,1)
              SfcOptics_AD%Emissivity = ZERO
 
              Reflectivity_AD(1:nZ,1,1:nZ,1) = SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1)
              SfcOptics_AD%Reflectivity = ZERO


            ! -- Serious problem if we got to this point
            CASE DEFAULT

              Error_Status = FAILURE
              WRITE( Message, '( "Unrecognised polarization flag for microwave ",&
                                &"channel index ", i4 )' ) &
                              Channel_Index
              CALL Display_Message( ROUTINE_NAME, &
                               TRIM( Message ), &
                               Error_Status, &
                               Message_Log = Message_Log )
              RETURN
 
          END SELECT Polarization_Type


        ELSE


          ! ------------------------------------
          ! Coupled polarization from atmosphere
          ! considered. Simply copy the data
          ! ------------------------------------

          Emissivity_AD(1:nZ,1:nL) = SfcOptics_AD%Emissivity(1:nZ,1:nL)
          SfcOptics_AD%Emissivity = ZERO
 
          Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL) = SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL)
          SfcOptics_AD%Reflectivity = ZERO

        ENDIF Decoupled_Polarization



        !#----------------------------------------------------------------------#
        !#                    -- PROCESS THE SURFACE TYPES --                   #
        !#----------------------------------------------------------------------#

        ! -------------------------------------
        ! Microwave ICE emissivity/reflectivity
        ! -------------------------------------

        Microwave_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on ice coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            ( Emissivity_AD(1:nZ,1:2) * Surface%Ice_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            ( Reflectivity_AD(1:nZ,1:2,1:nZ,1:2) * Surface%Ice_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_MW_Ice_SfcOptics_AD( Surface,                  &  ! Input
                                                      SfcOptics,                &  ! Input     
                                                      SfcOptics_AD,             &  ! Input     
                                                      GeometryInfo,             &  ! Input
                                                      Channel_Index,            &  ! Input, scalar
                                                      Surface_AD,               &  ! Output
                                                      Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW ice SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Microwave_Ice


        ! --------------------------------------
        ! Microwave SNOW emissivity/reflectivity
        ! --------------------------------------

        Microwave_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on snow coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            ( Emissivity_AD(1:nZ,1:2) * Surface%Snow_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            ( Reflectivity_AD(1:nZ,1:2,1:nZ,1:2) * Surface%Snow_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_MW_Snow_SfcOptics_AD( Surface,                  &  ! Input
                                                       SfcOptics,                &  ! Input     
                                                       SfcOptics_AD,             &  ! Input     
                                                       GeometryInfo,             &  ! Input
                                                       Channel_Index,            &  ! Input, scalar
                                                       Surface_AD,               &  ! Output
                                                       Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW snow SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Microwave_Snow


        ! ---------------------------------------
        ! Microwave WATER emissivity/reflectivity
        ! ---------------------------------------

        Microwave_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on water coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            ( Emissivity_AD(1:nZ,1:2) * Surface%Water_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            ( Reflectivity_AD(1:nZ,1:2,1:nZ,1:2) * Surface%Water_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_MW_Water_SfcOptics_AD( Surface,                  &  ! Input
                                                        SfcOptics,                &  ! Input     
                                                        SfcOptics_AD,             &  ! Input     
                                                        GeometryInfo,             &  ! Input
                                                        Channel_Index,            &  ! Input, scalar
                                                        Surface_AD,               &  ! Output
                                                        Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW water SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Microwave_Water


        ! --------------------------------------
        ! Microwave LAND emissivity/reflectivity
        ! --------------------------------------

        Microwave_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on land coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:2) = &
            SfcOptics_AD%Emissivity(1:nZ,1:2) + &
            ( Emissivity_AD(1:nZ,1:2) * Surface%Land_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:2,1:nZ,1:2) + &
            ( Reflectivity_AD(1:nZ,1:2,1:nZ,1:2) * Surface%Land_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_MW_Land_SfcOptics_AD( Surface,                  &  ! Input
                                                       SfcOptics,                &  ! Input     
                                                       SfcOptics_AD,             &  ! Input     
                                                       GeometryInfo,             &  ! Input
                                                       Channel_Index,            &  ! Input, scalar
                                                       Surface_AD,               &  ! Output
                                                       Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing MW land SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Microwave_Land



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                      ## INFRARED CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( INFRARED_SENSOR )


        ! ------------------------------------
        ! Infrared ICE emissivity/reflectivity
        ! ------------------------------------

        Infrared_Ice: IF( Surface%Ice_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on ice coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            ( Emissivity_AD(1:nZ,1:nL) * Surface%Ice_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            ( Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL) * Surface%Ice_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_IR_Ice_SfcOptics_AD( Surface,                  &  ! Input
                                                      SfcOptics,                &  ! Input     
                                                      SfcOptics_AD,             &  ! Input     
                                                      GeometryInfo,             &  ! Input
                                                      Channel_Index,            &  ! Input, scalar
                                                      Surface_AD,               &  ! Output
                                                      Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR ice SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Infrared_Ice


        ! -------------------------------------
        ! Infrared SNOW emissivity/reflectivity
        ! -------------------------------------

        Infrared_Snow: IF( Surface%Snow_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on snow coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            ( Emissivity_AD(1:nZ,1:nL) * Surface%Snow_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            ( Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL) * Surface%Snow_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_IR_Snow_SfcOptics_AD( Surface,                  &  ! Input
                                                       SfcOptics,                &  ! Input     
                                                       SfcOptics_AD,             &  ! Input     
                                                       GeometryInfo,             &  ! Input
                                                       Channel_Index,            &  ! Input, scalar
                                                       Surface_AD,               &  ! Output
                                                       Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR snow SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Infrared_Snow


        ! --------------------------------------
        ! Infrared WATER emissivity/reflectivity
        ! --------------------------------------

        Infrared_Water: IF( Surface%Water_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on water coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            ( Emissivity_AD(1:nZ,1:nL) * Surface%Water_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            ( Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL) * Surface%Water_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_IR_Water_SfcOptics_AD( Surface,                  &  ! Input
                                                        SfcOptics,                &  ! Input     
                                                        SfcOptics_AD,             &  ! Input     
                                                        GeometryInfo,             &  ! Input
                                                        Channel_Index,            &  ! Input, scalar
                                                        Surface_AD,               &  ! Output
                                                        Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR water SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Infrared_Water


        ! --------------------------------------
        ! Infrared LAND emissivity/reflectivity
        ! --------------------------------------

        Infrared_Land: IF( Surface%Land_Coverage > ZERO ) THEN

          ! -- The surface optics properties based on land coverage fraction
          ! -- Note that the Emissivity_AD and Reflectivity_AD local adjoints
          ! -- are NOT zeroed here.
          SfcOptics_AD%Emissivity(1:nZ,1:nL) = &
            SfcOptics_AD%Emissivity(1:nZ,1:nL) + &
            ( Emissivity_AD(1:nZ,1:nL) * Surface%Land_Coverage )

          SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) = &
            SfcOptics_AD%Reflectivity(1:nZ,1:nL,1:nZ,1:nL) + &
            ( Reflectivity_AD(1:nZ,1:nL,1:nZ,1:nL) * Surface%Land_Coverage )


          ! -- Compute the surface optics adjoints
          Error_Status = Compute_IR_Land_SfcOptics_AD( Surface,                  &  ! Input
                                                       SfcOptics,                &  ! Input     
                                                       SfcOptics_AD,             &  ! Input     
                                                       GeometryInfo,             &  ! Input
                                                       Channel_Index,            &  ! Input, scalar
                                                       Surface_AD,               &  ! Output
                                                       Message_Log = Message_Log )  ! Error messaging

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error computing IR land SfcOptics_AD at ", &
                              &"channel index ", i4 )' ) &
                            Channel_Index
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF

        ENDIF Infrared_Land



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                       ## VISIBLE CALCULATIONS ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE ( VISIBLE_SENSOR )


        ! -------------------
        ! Default values only
        ! -------------------

        SfcOptics_AD%Emissivity(1:nZ,1)       = ZERO
        SfcOptics_AD%Reflectivity(1:nZ,1,1:nZ,1) = ZERO



      !##########################################################################
      !##########################################################################
      !##                                                                      ##
      !##                        ## INVALID SENSOR TYPE ##                     ##
      !##                                                                      ##
      !##########################################################################
      !##########################################################################

      CASE DEFAULT

        Error_Status = FAILURE
        WRITE( Message, '( "Unrecognised sensor type for channel index ", i4 )' ) &
                        Channel_Index
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN

    END SELECT Sensor_Select

  END FUNCTION CRTM_Compute_SfcOptics_AD

END MODULE CRTM_SfcOptics


!----------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!----------------------------------------------------------------------------------
!
! $Id: CRTM_SfcOptics.f90,v 1.13.4.3 2005/10/19 14:48:34 paulv Exp $
!
! $Date: 2005/10/19 14:48:34 $
!
! $Revision: 1.13.4.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_SfcOptics.f90,v $
! Revision 1.13.4.3  2005/10/19 14:48:34  paulv
! - Corrected non-standard, too-long source lines.
! - Using short names for angle and Stokes dimensions of SfcOptics structures.
!
! Revision 1.13.4.2  2005/08/24 13:30:53  qliu
! -- Deleted unused variable j.
!
! Revision 1.13.4.1  2005/08/16 20:08:26  qliu
! - Removed references to node-based sensor type and polarization flags.
!
! Revision 1.13  2005/08/16 20:01:17  qliu
! - Now using MAX_N_STOKES parameter for local array dimensioning.
! - Added Sensor_Type and Polarization local variables for the SpcCoeff data
!   type values. This was done to facilitate changes for the OPSS based code.
!
! Revision 1.12  2005/06/29 01:06:15  paulv
! - Corrected misnamed functions in forward Compute_SfcOptics() function.
! - Added code to Compute_SfcOptics_TL() and Compute_SfcOptics_AD() functions.
!
! Revision 1.11  2005/06/27 13:18:16  paulv
! - Moved internal subprogram wrappers to emissivity routines to their own
!   modules. Ecept for IR/VIS routine which required some mor processing
!   (e.g. determination of stype etc.)
!
! Revision 1.10  2005/06/26 15:34:39  paulv
! - Checkin prior to removing internal wrapper routines to their own
!   modules.
! - Changed names of sensor routines for SEA surface to WATER.
! - Updated header documentation.
!
! Revision 1.9  2005/06/24 16:53:35  paulv
! - Standardised calls to surface optics routines.
! - Updated documentation and module USE for individual modules for each
!   sensor and land surface type.
!
! Revision 1.8  2005/06/21 22:00:51  paulv
! - Major revision. Incomplete. Provisional checkin.
!   o Added Compute_SurfaceT subroutines to compute the average
!     surface temperature weighted by surface type coverage fractions.
!   o Added code to main Compute_SfcOptics() function to handle the different
!     sensor and land surface type.
!   o Updated documentation.
!
! Revision 1.7  2005/02/01 16:03:43  paulv
! - Added adjoint shell function.
!
! Revision 1.6  2005/01/28 21:13:19  paulv
! - Added CRTM_Compute_SfcOptics_TL() function shell.
!
! Revision 1.5  2004/11/05 16:06:33  paulv
! - Replaced Init() with Associated() function in PUBLIC list.
! - Updated header documentation.
!
! Revision 1.4  2004/07/02 20:31:44  paulv
! - Like the AtmScatter modules, these also had CRLF at the end of each line.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/06/29 17:01:17  paulv
! - Corrected documentation error.
!
! Revision 1.1  2004/06/29 16:25:43  paulv
! Initial checkin.
!
!
!
!


