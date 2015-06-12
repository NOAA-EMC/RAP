!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Surface_Binary_IO
!
! PURPOSE:
!       Module containing routines to inquire, read, and write Binary format
!       CRTM_Surface files.
!       
! CATEGORY:
!       CRTM : Surface : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Surface_Binary_IO
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds
!                                of variable types.
!
!       File_Utility:            Module containing generic file utility routines
!
!       Error_Handler:           Module to define simple error codes and
!                                handle error conditions
!                                USEs: FILE_UTILITY module
!
!       Binary_File_Utility:     Module for utility routines for "Binary"
!                                datafiles (unformatted, sequential).
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      ERROR_HANDLER module
!
!       CRTM_Surface_Define:     Module defining the CRTM_Surface data
!                                structure and containing routines to
!                                manipulate it.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!
! CONTAINS:
!       CRTM_Inquire_Surface_Binary:    Function to inquire Binary format
!                                       CRTM_Surface files.
!
!       CRTM_Read_Surface_Binary:       Function to read CRTM_Surface
!                                       structures from Binary format
!                                       CRTM_Surface files.
!
!       CRTM_Write_Surface_Binary:      Function to write CRTM_Surface
!                                       structures to Binary format
!                                       CRTM_Surface files.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jul-2004
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

MODULE CRTM_Surface_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE Binary_File_Utility

  USE CRTM_Surface_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Inquire_Surface_Binary
  PUBLIC :: CRTM_Read_Surface_Binary
  PUBLIC :: CRTM_Write_Surface_Binary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Read_Surface_Binary
    MODULE PROCEDURE Read_Surface_Scalar
    MODULE PROCEDURE Read_Surface_Rank1
  END INTERFACE CRTM_Read_Surface_Binary

  INTERFACE CRTM_Write_Surface_Binary
    MODULE PROCEDURE Write_Surface_Scalar
    MODULE PROCEDURE Write_Surface_Rank1
  END INTERFACE CRTM_Write_Surface_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTM_Surface_Binary_IO.f90,v 1.10 2005/08/18 15:20:23 paulv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE  = 1.0_fp_kind


CONTAINS




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Read_Surface_Record( FileID,       &  ! Input
                                Surface,      &  ! Output
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                    INTENT( IN )     :: FileID

    ! -- Outut
    TYPE( CRTM_Surface_type ),  INTENT( IN OUT ) :: Surface

    ! -- Error handler Message log
    CHARACTER( * ),   OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Record)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: Allocate_Status

    INTEGER :: Type_in_File
    INTEGER :: Type_by_Coverage
    REAL( fp_kind ) :: Total_Coverage
    INTEGER :: n_Channels



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE SURFACE DATA --                     #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The gross surface type
    ! ----------------------

    READ( FileID, IOSTAT = IO_Status ) Type_in_File, &
                                       Surface%Land_Coverage, &
                                       Surface%Water_Coverage, &
                                       Surface%Snow_Coverage, &
                                       Surface%Ice_Coverage

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading surface type and coverage. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -- Simple check of coverage
    Surface%Land_Coverage  = MAX( Surface%Land_Coverage,  ZERO )
    Surface%Water_Coverage = MAX( Surface%Water_Coverage, ZERO )
    Surface%Snow_Coverage  = MAX( Surface%Snow_Coverage,  ZERO )
    Surface%Ice_Coverage   = MAX( Surface%Ice_Coverage,   ZERO )


    ! -- Check the total coverage
    Total_Coverage = Surface%Land_Coverage  + &
                     Surface%Water_Coverage + &
                     Surface%Snow_Coverage  + &
                     Surface%Ice_Coverage  
    IF ( Total_Coverage > ONE ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Total coverage fraction sum > 1.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -- Compute the coverage type
    Type_by_Coverage = 0
    IF ( Surface%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( Surface%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( Surface%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( Surface%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE


    ! -- Check the file and coverge surfce types
    IF ( Type_in_File /= Type_by_Coverage ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Coverage surface type, '//TRIM( SURFACE_TYPE_NAME( Type_by_Coverage ) )//&
                            ', inconsistent with that specified in file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------
    ! Surface wind speed
    ! ------------------

    READ( FileID, IOSTAT = IO_Status ) Surface%Wind_Speed
                                         

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading surface wind speed data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ----------------------
    ! Land surface type data
    ! ----------------------

    READ( FileID, IOSTAT = IO_Status ) Surface%Land_Type, &
                                       Surface%Land_Temperature, &
                                       Surface%Soil_Moisture_Content, &
                                       Surface%Canopy_Water_Content , &
                                       Surface%Vegetation_Fraction, &
                                       Surface%Soil_Temperature

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading land surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! -- Check the type
    IF ( Surface%Land_Type < 0 .OR. Surface%Land_Type > N_VALID_LAND_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised land surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Land_Type = INVALID_LAND
    END IF


    ! -----------------------
    ! Water surface type data
    ! -----------------------

    READ( FileID, IOSTAT = IO_Status ) Surface%Water_Type, &
                                       Surface%Water_Temperature, &
                                       Surface%Wind_Direction, &
                                       Surface%Salinity
                                         
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading water surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! -- Check the type
    IF ( Surface%Water_Type < 0 .OR. Surface%Water_Type > N_VALID_WATER_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised water surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Water_Type = INVALID_WATER
    END IF


    ! ----------------------
    ! Snow surface type data
    ! ----------------------

    READ( FileID, IOSTAT = IO_Status ) Surface%Snow_Type, &
                                       Surface%Snow_Temperature, &
                                       Surface%Snow_Depth, &
                                       Surface%Snow_Density, &
                                       Surface%Snow_Grain_Size

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading snow surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! -- Check the type
    IF ( Surface%Snow_Type < 0 .OR. Surface%Snow_Type > N_VALID_SNOW_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised snow surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Snow_Type = INVALID_SNOW
    END IF


    ! ---------------------
    ! Ice surface type data
    ! ---------------------

    READ( FileID, IOSTAT = IO_Status ) Surface%Ice_Type, &
                                       Surface%Ice_Temperature, &
                                       Surface%Ice_Thickness, &
                                       Surface%Ice_Density, &
                                       Surface%Ice_Roughness

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ice surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! -- Check the type
    IF ( Surface%Ice_Type < 0 .OR. Surface%Ice_Type > N_VALID_ICE_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised ice surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Ice_Type = INVALID_ICE
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE SensorData DATA --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------------
    ! Destroy the SensorData structure. This step will be taken
    ! care of in the calling routine, Read_Surface_Binary(), so
    ! the following is a belt-and-braces thing. :o)
    ! ---------------------------------------------------------

    Destroy_Status = CRTM_Destroy_SensorData( Surface%SensorData, &
                                              Message_Log = Message_Log )

    IF ( Destroy_Status /= SUCCESS ) THEN
      Message = 'Error destroying SensorData structure.'
      GOTO 1000  ! Clean up
    END IF


    ! ------------------------
    ! Read the data dimensions
    ! ------------------------

    READ( FileID, IOSTAT = IO_Status ) n_Channels

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading SensorData dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------
    ! Read the data if required
    ! -------------------------

    IF ( n_Channels > 0 ) THEN

      ! -- Allocate the structure
      Allocate_Status = CRTM_Allocate_SensorData( n_Channels, &
                                                  Surface%SensorData, &
                                                  Message_Log = Message_Log )

      IF ( Allocate_Status /= SUCCESS ) THEN
        Message = 'Error allocating SensorData structure.'
        GOTO 1000  ! Clean up
      END IF


      ! -- Read the Sensor data
      READ( FileID, IOSTAT = IO_Status ) Surface%SensorData%NCEP_Sensor_ID, &  
                                         Surface%SensorData%WMO_Satellite_ID, &
                                         Surface%SensorData%WMO_Sensor_ID, &   
                                         Surface%SensorData%Sensor_Channel, &
                                         Surface%SensorData%Tb
  
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SensorData. IOSTAT = ", i5 )' ) IO_Status
        GOTO 1000  ! Clean up
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Surface( Surface )
    CLOSE( FileID )

  END FUNCTION Read_Surface_Record


  FUNCTION Write_Surface_Record( FileID,       &  ! Input
                                 Surface,      &  ! Input
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                    INTENT( IN )  :: FileID
    TYPE( CRTM_Surface_type ),  INTENT( IN )  :: Surface

    ! -- Error handler Message log
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Record)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: Type_by_Coverage
    INTEGER :: l 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE SURFACE DATA --                     #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The gross surface type
    ! ----------------------

    ! -- Compute the coverage type
    Type_by_Coverage = 0
    IF ( Surface%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( Surface%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( Surface%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( Surface%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE

    WRITE( FileID, IOSTAT = IO_Status ) Type_by_Coverage, &
                                        Surface%Land_Coverage, &
                                        Surface%Water_Coverage, &
                                        Surface%Snow_Coverage, &
                                        Surface%Ice_Coverage
                                         
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing surface type and coverage fractions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ------------------
    ! Surface wind speed
    ! ------------------

    WRITE( FileID, IOSTAT = IO_Status ) Surface%Wind_Speed

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing surface wind speed data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ----------------------
    ! Land surface type data
    ! ----------------------

    WRITE( FileID, IOSTAT = IO_Status ) Surface%Land_Type, &
                                        Surface%Land_Temperature, &
                                        Surface%Soil_Moisture_Content, &
                                        Surface%Canopy_Water_Content, &
                                        Surface%Vegetation_Fraction, &
                                        Surface%Soil_Temperature

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing land surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -----------------------
    ! Water surface type data
    ! -----------------------

    WRITE( FileID, IOSTAT = IO_Status ) Surface%Water_Type, &
                                        Surface%Water_Temperature, &
                                        Surface%Wind_Direction, &
                                        Surface%Salinity
                                         
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing water surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ----------------------
    ! Snow surface type data
    ! ----------------------

    WRITE( FileID, IOSTAT = IO_Status ) Surface%Snow_Type, &
                                        Surface%Snow_Temperature, &
                                        Surface%Snow_Depth, &
                                        Surface%Snow_Density, &
                                        Surface%Snow_Grain_Size

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing snow surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ---------------------
    ! Ice surface type data
    ! ---------------------

    WRITE( FileID, IOSTAT = IO_Status ) Surface%Ice_Type, &
                                        Surface%Ice_Temperature, &
                                        Surface%Ice_Thickness, &
                                        Surface%Ice_Density, &
                                        Surface%Ice_Roughness

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ice surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF




    !#--------------------------------------------------------------------------#
    !#                   -- WRITE THE SensorData STRUCTURE --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Write the data dimensions
    ! -------------------------

    WRITE( FileID, IOSTAT = IO_Status ) Surface%SensorData%n_Channels

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing SensorData dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -----------------------------
    ! Write the sensor ids and data
    ! -----------------------------

    IF ( Surface%SensorData%n_Channels > 0 ) THEN

      WRITE( FileID, IOSTAT = IO_Status ) Surface%SensorData%NCEP_Sensor_ID, &
                                          Surface%SensorData%WMO_Satellite_ID, &
                                          Surface%SensorData%WMO_Sensor_ID, &
                                          Surface%SensorData%Sensor_Channel, &
                                          Surface%SensorData%Tb
  
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SensorData. IOSTAT = ", i5 )' ) IO_Status
        GOTO 1000  ! Clean up
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )

  END FUNCTION Write_Surface_Record





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Inquire_Surface_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Surface structure files.
!
! CATEGORY:
!       CRTM : Surface : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Surface_Binary( Filename,                  &  ! Input
!                                                   n_Locations = n_Locations, &  ! Optional output
!                                                   RCS_Id      = RCS_Id,      &  ! Revision control
!                                                   Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Locations:  The number of surface data locations in the file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:  Function to open Binary format
!                          data files.
!                          SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:   Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
!       File_Exists:       Function to test for the existance
!                          of files.
!                          SOURCE: FILE_UTILITY module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Apr-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Surface_Binary( Filename,     &  ! Input
                                        n_Locations,  &  ! Optional output
                                        RCS_Id,       &  ! Revision control
                                        Message_Log ) &  ! Error messaging
                                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )     :: Filename

    ! -- Optional output
    INTEGER,         OPTIONAL, INTENT( OUT )    :: n_Locations

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Surface_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Locations_in_File



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OPEN THE Surface DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE NUMBER OF DATA LOCATIONS --               #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Locations_in_File

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Locations data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                    -- SAVE THE NUMBER OF LOCATIONS --                    #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Locations ) ) n_Locations = n_Locations_in_File



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Inquire_Surface_Binary


!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Read_Surface_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Surface structure files.
!
! CATEGORY:
!       CRTM : Surface : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Surface_Binary( Filename,                  &  ! Input
!                                                Surface,                   &  ! output
!                                                n_Locations = n_Locations, &  ! Optional output
!                                                RCS_Id      = RCS_Id,      &  ! Revision control
!                                                Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Surface:      Structure containing the Surface data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Locations:  The actual number of surface data locations read in.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Destroy_Surface:      Function to re-initialize a CRTM_Surface
!                                  structure.
!                                  SOURCE: CRTM_Surface_DEFINE module
!
!       Open_Binary_File:          Function to open Binary format
!                                  data files.
!                                  SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:           Subroutine to output messages
!                                  SOURCE: ERROR_HANDLER module
!
!       File_Exists:               Function to test for the existance
!                                  of files.
!                                  SOURCE: FILE_UTILITY module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Surface_Scalar( Filename,     &  ! Input
                                Surface,      &  ! Output
                                n_Locations,  &  ! Optional output
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )     :: Filename

    ! -- Output
    TYPE( CRTM_Surface_type ), INTENT( IN OUT ) :: Surface

    ! -- Optional output
    INTEGER,         OPTIONAL, INTENT( OUT )    :: n_Locations

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Scalar)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Destroy_Status

    INTEGER :: n_Input_Locations
    INTEGER :: n_Locations_Read

    TYPE( CRTM_Surface_type ) :: Dummy_Surface
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'File '//TRIM( Filename )//' not found.'
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OPEN THE Surface DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE NUMBER OF DATA LOCATIONS --               #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Locations

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Locations data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! ----------------------------------------
    ! Issue warning message if n_Locations > 1
    ! ----------------------------------------

    IF ( n_Input_Locations > 1 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of locations > 1 and output Surface structure '//&
                            'is scalar. Only the first Surface structure will be read.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE STRUCTURE DATA --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Initialize data locations read
    ! ------------------------------

    n_Locations_Read = 0


    ! ----------------------------------------------
    ! Read the structure data into a dummy structure
    ! ----------------------------------------------

    Error_Status = Read_Surface_Record( FileID, &
                                        Dummy_Surface, &
                                        Message_Log = Message_Log )

    IF ( Error_Status == FAILURE ) THEN
      Message = 'Error reading Surface record from '//TRIM( Filename )
      GOTO 1000
    END IF

    IF ( Error_Status == WARNING ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Warning flagged in surface record read.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------------
    ! Copy dummy structure to output
    ! ------------------------------

    ! -- Copy the data into the output array
    Error_Status = CRTM_Assign_Surface( Dummy_Surface, &
                                        Surface, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error copying Surface structure.'
      GOTO 1000
    END IF

    ! -- Set value for the number of locations read
    n_Locations_Read = 1


    ! ---------------------------
    ! Destroy the dummy structure
    ! ---------------------------

    Error_Status = CRTM_Destroy_Surface( Dummy_Surface )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying dummy Surface structure.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- SAVE THE NUMBER OF DATA LOCATIONS READ --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Locations ) ) THEN
      n_Locations = n_Locations_Read
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Surface( Surface, Dummy_Surface )
    CLOSE( FileID )

  END FUNCTION Read_Surface_Scalar


  FUNCTION Read_Surface_Rank1( Filename,     &  ! Input
                               Surface,      &  ! Output
                               n_Locations,  &  ! Optional output
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )     :: Filename

    ! -- Output
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: Surface

    ! -- Optional output
    INTEGER,                   OPTIONAL,       INTENT( OUT )    :: n_Locations

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Rank-1)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Destroy_Status

    INTEGER :: n_Input_Locations
    INTEGER :: m, n_Locations_Read

    TYPE( CRTM_Surface_type ) :: Dummy_Surface
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OPEN THE Surface DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE NUMBER OF DATA LOCATIONS --               #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Locations

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Locations data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF


    ! -----------------------------------------------------------
    ! Issue warning message if n_Locations > size of output array
    ! -----------------------------------------------------------

    IF ( n_Input_Locations > SIZE( Surface ) ) THEN
      WRITE( Message, '( "Number of data locations, ", i5, " > size of the output Surface ", &
                        &"structure array, ", i5, ". Only the first ", i5, &
                        &" Surface structures will be read." )' ) &
                      n_Input_Locations, SIZE( Surface ), SIZE( Surface )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      n_Input_Locations = SIZE( Surface )
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- LOOP OVER PROFILES --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Initialize profiles read
    ! ------------------------

    n_Locations_Read = 0


    ! --------------------------------------------------------------
    ! Loop over all the data locations (even potentially empty ones)
    ! --------------------------------------------------------------

    Location_Loop: DO m = 1, n_Input_Locations


      ! ----------------------------------------------
      ! Read the structure data into a dummy structure
      ! ----------------------------------------------

      Error_Status = Read_Surface_Record( FileID, &
                                          Dummy_Surface, &
                                          Message_Log = Message_Log )

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error reading Surface element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000
      END IF

      IF ( Error_Status == WARNING ) THEN
        WRITE( Message, '( "Warning flagged in reading surface record #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF


      ! ------------------------------------
      ! Copy dummy structure to output array
      ! ------------------------------------

      ! -- Increment profiles read
      n_Locations_Read = n_Locations_Read + 1

      ! -- Copy the data into the output array
      Error_Status = CRTM_Assign_Surface( Dummy_Surface, &
                                          Surface( n_Locations_Read ), &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error copying Surface element #", i5, "." )' ) m
        GOTO 1000
      END IF


      ! ---------------------------
      ! Destroy the dummy structure
      ! ---------------------------

      Error_Status = CRTM_Destroy_Surface( Dummy_Surface )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying dummy Surface structure at element #", i5, "." )' ) m
        GOTO 1000
      END IF

    END DO Location_Loop



    !#--------------------------------------------------------------------------#
    !#                -- SAVE THE NUMBER OF DATA LOCATIONS READ --              #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Output an info message
    ! ----------------------

    WRITE( Message, '( "Number of surface data locations read from ", a, ": ", i5 )' ) &
                    TRIM( Filename ), n_Locations_Read
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( MEssage ), &
                          INFORMATION, &
                          Message_Log = Message_Log )


    ! ---------------------------------------
    ! Assign a value to the optional argument
    ! ---------------------------------------

    IF ( PRESENT( n_Locations ) ) THEN
      n_Locations = n_Locations_Read
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Surface( Surface )
    Destroy_Status = CRTM_Destroy_Surface( Dummy_Surface )
    CLOSE( FileID )


  END FUNCTION Read_Surface_Rank1





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Write_Surface_Binary
!
! PURPOSE:
!       Function to write Binary format CRTM Surface files.
!
! CATEGORY:
!       CRTM : Surface : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Surface_Binary( Filename,                 &  ! Input
!                                                 Surface,                  &  ! Input
!                                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Surface format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       Surface:      Structure containing the Surface data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Surface_Scalar( Filename,     &  ! Input
                                 Surface,      &  ! Input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )  :: Filename
    TYPE( CRTM_Surface_type ), INTENT( IN )  :: Surface

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Scalar)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OPEN THE Surface DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- WRITE THE NUMBER OF DATA LOCATIONS --                #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) 1

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Locations data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_Surface_Record( FileID, &
                                         Surface, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Surface record to '//TRIM( Filename )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_Surface_Scalar


  FUNCTION Write_Surface_Rank1( Filename,     &  ! Input
                                Surface,      &  ! Input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: Filename
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN )  :: Surface

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Rank-1)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m, n_Output_Locations
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OPEN THE Surface DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- WRITE THE NUMBER OF VALID DATA LOCATIONS --             #
    !#--------------------------------------------------------------------------#

    n_Output_Locations = SIZE( Surface )

    WRITE( FileID, IOSTAT = IO_Status ) n_Output_Locations

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Locations data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- LOOP OVER DATA LOCATIONS --                      #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Loop over all the locations
    ! ---------------------------

    Location_Loop: DO m = 1, n_Output_Locations


      ! ------------------------
      ! Write the structure data
      ! ------------------------

      Error_Status = Write_Surface_Record( FileID, &
                                           Surface(m), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Surface element #", i5, " to ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000
      END IF

    END DO Location_Loop



    !#--------------------------------------------------------------------------#
    !#                       -- OUTPUT AND INFO MESSAGE --                      #
    !#--------------------------------------------------------------------------#

    WRITE( Message, '( "Number of surface data elements written to ", a, ": ", i5 )' ) &
                    TRIM( Filename ), n_Output_Locations
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( MEssage ), &
                          INFORMATION, &
                          Message_Log = Message_Log )



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_Surface_Rank1

END MODULE CRTM_Surface_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Surface_Binary_IO.f90,v 1.10 2005/08/18 15:20:23 paulv Exp $
!
! $Date: 2005/08/18 15:20:23 $
!
! $Revision: 1.10 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Surface_Binary_IO.f90,v $
! Revision 1.10  2005/08/18 15:20:23  paulv
! - Removed all references to Surface%Type component. The Type component is
!   no longer in the Surface structure.
!
! Revision 1.9  2005/06/29 01:14:05  paulv
! - Updated to reflect changes to Surface structure where the gross surface
!   type flag has been removed. File record read and write functions modified.
!
! Revision 1.8  2005/06/16 15:00:49  paulv
! - Added Inquire() functions.
!
! Revision 1.7  2005/01/28 21:23:56  paulv
! - Cosmetic changes only.
!
! Revision 1.6  2004/11/05 15:58:25  paulv
! - Upgraded to Fortran-95
! - Added structure association test to the Write() function.
! - Changed INTENT of Surface structure in Read() function from OUT to
!   IN OUT. Necessary to prevent memory leaks.
! - Simplified I/O to use structure components directly.
! - Altered SensorData I/O to reflect changes in structure declaration.
! - Removed Init() functions from routines with local structures.
! - Updated header documentation.
!
! Revision 1.5  2004/08/06 18:37:26  paulv
! - Moved the dummy structure initialization in the rank-1 read so it's the
!   first executable statement.
!
! Revision 1.4  2004/08/05 21:57:09  paulv
! - Updated header documentation.
!
! Revision 1.3  2004/08/05 17:38:40  paulv
! - Changed to use updated structure definition.
!
! Revision 1.2  2004/07/27 14:33:36  paulv
! - Integrated new CRTM Surface structure into the I/O routines.
!
! Revision 1.1  2004/07/22 19:48:17  paulv
! Initial checkin.
!
!
!
!
