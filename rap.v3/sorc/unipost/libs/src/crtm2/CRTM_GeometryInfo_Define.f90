!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_GeometryInfo_Define
!
! PURPOSE:
!       Module defining the CRTM GeometryInfo data structure.
!       
! CATEGORY:
!       CRTM : GeometryInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_GeometryInfo_Define
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds
!                            of variable types.
!
!       Error_Handler:       Module to define simple error codes and
!                            handle error conditions
!                            USEs: FILE_UTILITY module
!
!       CRTM_Parameters:     Module containing parameters used throughout
!                            the CRTM. This module is used here to initialize
!                            the default diffusivity angle for approximating
!                            the flux transmittance.
!
! CONTAINS:
!       CRTM_Compute_GeometryInfo:  Subroutine to compute the GeometryInfo
!                                   structure derived components.
!
! DERIVED TYPES:
!       CRTM_GeometryInfo_type
!       ----------------------
!       Definition of the public CRTM_GeometryInfo data structure.
!       Fields are:
!
!         USER INPUTS: It is expected the user will fill these components
!         -----------  of the GeometryInfo structure
!
!
!         Earth_Radius:          Radius of the Earth at the current location.
!                                UNITS:      kilometres (km)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Satellite_Height:      Height of the satellite above the Earth's 
!                                surface at the current location.
!                                UNITS:      kilometres (km)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Longitude:             Earth longitude.
!                                UNITS:      degrees East (0->360)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Latitude:              Earth latitude.
!                                UNITS:      degrees North (-90->+90)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Surface_Altitude:      Altitude of the Earth's surface at the specified
!                                lon/lat location.
!                                UNITS:      metres (m)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Sensor_Zenith_Angle:   The sensor zenith angle, Z. If a flat Earth
!                                is assumed, then the Sensor_Scan_Angle and
!                                Sensor_Zenith_Angle are the same.
!                                              | 
!                                   Zenith   -0A0-  <--Satellite
!                                      |      /
!                                      |     /
!                                      |    /
!                                      |   /
!                                      |  /
!                                      |Z/
!                                      |/
!                                   -------------
!                                      ^
!                                      FOV
!                                UNITS:      Degrees.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Sensor_Azimuth_Angle:  The sensor azimuth angle, A, is the angle
!                                subtended by the horizontal projection of
!                                a direct line from the satellite to the FOV
!                                and the North-South axis measured clockwise
!                                from North.
!                                         North     O  <-- Sub-satellite point
!                                           |      /
!                                           |     /
!                                           |    /
!                                           |   /
!                                           |  /
!                                           |A/
!                                           |/
!                                  West-----O-----East
!                                           ^
!                                          FOV
!                                UNITS:      Degrees form North (0->360)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Source_Zenith_Angle:   The source zenith angle, ZS. The source is
!                                typically the Sun (IR/VIS) or Moon (MW/VIS).
!                                   Zenith     X  <--Source
!                                      |      /
!                                      |     /
!                                      |    /
!                                      |   /
!                                      |ZS/
!                                      | /
!                                      |/
!                                   -------------
!                                      ^
!                                      FOV
!                                UNITS:      Degrees (-180 -> +180)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Source_Azimuth_Angle:  The source azimuth angle, AS, is the angle
!                                subtended by the horizontal projection of
!                                a direct line from the source to the FOV
!                                and the North-South axis measured clockwise
!                                from North.
!                                         North     
!                                           |      / <-- horizontal projection
!                                           |     /      of direct line from 
!                                           |    /       source 
!                                           |   /
!                                           |AS/
!                                           | /
!                                           |/
!                                  West-----O-----East
!                                           ^
!                                          FOV
!                                UNITS:      Degrees from North (0->360).
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Flux_Zenith_Angle:     The zenith angle, F, used to approximate downwelling
!                                flux transmissivity. If not set, the default value
!                                is that of the diffusivity approximation, such that
!                                  sec(F) = 5/3
!                                Maximum allowed value of F is determined from
!                                  sec(F) = 9/4
!                                UNITS:      Degrees
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!
!         DERIVED COMPONENTS: These components will be derived from values from the
!         ------------------  user input components via CRTM_Compute_GeometryInfo.
!
!         Sensor_Scan_Angle:     The sensor scan angle, S, from nadir.
!                                      |         
!                                    -0A0-  <--Satellite     
!                                      |\        
!                                      |S\       
!                                      |  \      
!                                      |   \     
!                                      |    \    
!                                      |     \   
!                                      |      \  
!                                    ------------
!                                      ^       ^ 
!                                     Nadir   FOV
!                                UNITS:      Degrees
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Sensor_Scan_Radian:    The sensor scan angle, S, in radians. This
!                                value is derived from the Sensor_Scan_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Sensor_Zenith_Radian:  The sensor zenith angle, Z, in radians. This
!                                value is derived from the Sensor_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Sensor_Azimuth_Radian: The sensor azimuth angle, A, in radians. This
!                                value is derived from the Sensor_Azimuth_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Secant_Sensor_Zenith:  The secant of the sensor zenith
!                                angle, sec(Z). The value is derived
!                                from the Sensor_Zenith_Angle component.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Source_Zenith_Radian:  The Source zenith angle, ZS, in radians. This
!                                value is derived from the Source_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Source_Azimuth_Radian: The Source azimuth angle, AS, in radians. This
!                                value is derived from the Source_Azimuth_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Secant_Source_Zenith:  The secant of the source zenith
!                                angle, sec(ZS). The value is derived
!                                from the Source_Zenith_Angle component.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Flux_Zenith_Radian:    The flux zenith angle, F, in radians. This
!                                value is derived from the Flux_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
!         Secant_Flux_Zenith:    The secant of the flux zenith angle, sec(F).
!                                This value is derived from the Flux_Zenith_Angle
!                                component and is used to approximate the
!                                downwelling flux transmissivity (diffusivity
!                                approximation).
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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
!------------------------------------------------------------------------------

MODULE CRTM_GeometryInfo_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler

  USE CRTM_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE

  PUBLIC :: CRTM_Compute_GeometryInfo


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_GeometryInfo_Define.f90,v 1.10 2005/09/23 13:43:03 yhan Exp $'

  ! -- GeometryInfo scalar invalid value
  INTEGER,         PRIVATE, PARAMETER ::    INVALID = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FP_INVALID = ZERO 


  ! ---------------------------------
  ! GeometryInfo data type definition
  ! ---------------------------------

  TYPE, PUBLIC :: CRTM_GeometryInfo_type

    ! ----------
    ! User Input
    ! ----------

    ! -- Earth radius and satellite height
    REAL( fp_kind ) :: Earth_Radius     = 6370.0_fp_kind ! (km), mean earth radius 
    REAL( fp_kind ) :: Satellite_Height = 800.0_fp_kind  ! (km), default
    REAL( fp_kind ) :: Distance_Ratio = 0.888424 ! default,(Earth_Radius/(Earth_Radius+Satellite_Height))

    ! -- Earth location
    REAL( fp_kind ) :: Longitude        = ZERO
    REAL( fp_kind ) :: Latitude         = ZERO
    REAL( fp_kind ) :: Surface_Altitude = ZERO

    ! -- Sensor angle information
    REAL( fp_kind ) :: Sensor_Zenith_Angle  = ZERO
    REAL( fp_kind ) :: Sensor_Azimuth_Angle = ZERO 

    ! -- Source angle information
    REAL( fp_kind ) :: Source_Zenith_Angle  = FP_INVALID
    REAL( fp_kind ) :: Source_Azimuth_Angle = ZERO

    ! -- Flux angle information
    REAL( fp_kind ) :: Flux_Zenith_Angle = DIFFUSIVITY_ANGLE


    ! -----------------------
    ! Derived from User Input
    ! -----------------------

    ! -- Sensor angle information
    REAL( fp_kind ) :: Sensor_Scan_Angle     = ZERO
    REAL( fp_kind ) :: Sensor_Scan_Radian    = FP_INVALID
    REAL( fp_kind ) :: Sensor_Zenith_Radian  = FP_INVALID
    REAL( fp_kind ) :: Sensor_Azimuth_Radian = ZERO

    REAL( fp_kind ) :: Secant_Sensor_Zenith = FP_INVALID

    ! -- Source angle information
    REAL( fp_kind ) :: Source_Zenith_Radian  = FP_INVALID
    REAL( fp_kind ) :: Source_Azimuth_Radian = ZERO

    REAL( fp_kind ) :: Secant_Source_Zenith = FP_INVALID

    ! -- Flux angle information
    REAL( fp_kind ) :: Flux_Zenith_Radian = DIFFUSIVITY_RADIAN
    REAL( fp_kind ) :: Secant_Flux_Zenith = SECANT_DIFFUSIVITY

  END TYPE CRTM_GeometryInfo_type


CONTAINS


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
!       CRTM_Compute_GeometryInfo
! 
! PURPOSE:
!       Function to compute the derived geometry from the user specified
!       components of the CRTM_GeometryInfo structure.
!
! CATEGORY:
!       CRTM : GeometryInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo,             &  ! In/Output
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       GeometryInfo:  The CRTM_GeometryInfo structure containing the user
!                      defined inputs, in particular the angles.
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to the screen.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       GeometryInfo:  The CRTM_GeometryInfo structure with the derived
!                      angle components filled..
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE invalid data was found
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:                Subroutine to output messages
!                                      SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       This subroutines changes the values of the derived components of the
!       GeometryInfo structure argument.
!
! RESTRICTIONS:
!       None.
!       
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_GeometryInfo( gInfo,        &  ! In/Output
                                      Message_Log ) &  ! Optional input
                                    RESULT ( Error_Status )

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/output
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN OUT ) :: gInfo

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_GeometryInfo'



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! The sensor angles
    ! -----------------

    IF ( ABS( gInfo%Sensor_Zenith_Angle ) > MAX_SENSOR_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor zenith angle', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( gInfo%Sensor_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Sensor_Azimuth_Angle > MAX_SENSOR_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      gInfo%Sensor_Azimuth_Angle = ZERO
    END IF


    ! -----------------
    ! The Source angles
    ! -----------------

!    IF ( ABS( gInfo%Source_Zenith_Angle ) > MAX_SOURCE_ZENITH_ANGLE ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Invalid source zenith angle', &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!      RETURN
!    END IF

    IF ( gInfo%Source_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Source_Azimuth_Angle > MAX_SOURCE_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid source azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      gInfo%Source_Azimuth_Angle = ZERO
    END IF


    ! ---------------
    ! The Flux angles
    ! ---------------

    IF ( ABS( gInfo%Flux_Zenith_Angle ) > MAX_FLUX_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid flux zenith angle', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Sensor angles
    ! -------------

    gInfo%Sensor_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Sensor_Zenith_Angle
    gInfo%Sensor_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Sensor_Azimuth_Angle
    gInfo%Secant_Sensor_Zenith  = ONE / COS( gInfo%Sensor_Zenith_Radian )


    ! -------------
    ! Source angles
    ! -------------

    gInfo%Source_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Source_Zenith_Angle
    gInfo%Source_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Source_Azimuth_Angle
    gInfo%Secant_Source_Zenith  = ONE / COS( gInfo%Source_Zenith_Radian )


    ! -----------
    ! Flux angles
    ! -----------

    gInfo%Flux_Zenith_Radian = DEGREES_TO_RADIANS * gInfo%Flux_Zenith_Angle
    gInfo%Secant_Flux_Zenith = ONE / COS( gInfo%Flux_Zenith_Radian )

  END FUNCTION CRTM_Compute_GeometryInfo

END MODULE CRTM_GeometryInfo_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_GeometryInfo_Define.f90,v 1.10 2005/09/23 13:43:03 yhan Exp $
!
! $Date: 2005/09/23 13:43:03 $
!
! $Revision: 1.10 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_GeometryInfo_Define.f90,v $
! Revision 1.10  2005/09/23 13:43:03  yhan
! --- Changed Sensor_Scan_Angle into a derived input instead of a user input and
!     add an equation in CRTM_Compute_GeometryInfo() to derived it from
!     Sensor_Zenith_Angle.
!
! Revision 1.9  2005/08/15 19:00:49  paulv
! - Initialisation values are now ZERO instead of FP_INVALID.
! - Invalid source zenith angle check is commented out for the time being.
!   A source zenith angle > 85-90degrees means the source (e.g. Sun, moon)
!   is over the horizon, and not necessarily an error.
!
! Revision 1.8  2005/07/20 15:19:26  paulv
! - Corrected error in CRTM_Compute_GeometryInfo() interface.
!
! Revision 1.7  2005/07/20 14:28:44  paulv
! - Removed ONLY clause from CRTM_Parameters USE statement.
!
! Revision 1.6  2005/06/29 01:16:58  paulv
! - Major changes to angle components of structure.
! - Added CRTM_Compute_GeometryInfo() function to fill in derived structure
!   components.
!
! Revision 1.5  2004/11/03 16:06:57  paulv
! - Updated to Fortran-95.
! - Removed initialisation subroutines. This module now contains *only* the
!   structure definition.
! - Updated documentation.
!
! Revision 1.4  2004/08/06 18:40:30  paulv
! - Added radius/height and azimuth members to structure.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/05/20 19:03:32  paulv
! - Overloaded Init() subroutine to handle scalar and rank-1 array arguments.
!
! Revision 1.1  2004/05/19 19:55:19  paulv
! Initial checkin.
!
!
!
!
