!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Aerosol_Binary_IO
!
! PURPOSE:
!       Module containing routines to inquire, read, and write Binary format
!       CRTM_Aerosol files.
!       
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Aerosol_Binary_IO
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
!       CRTM_Aerosol_Define:     Module defining the CRTM_Aerosol data
!                                structure and containing routines to
!                                manipulate it.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!
! CONTAINS:
!       CRTM_Inquire_Aerosol_Binary: Function to inquire Binary
!                                    CRTM_Aerosol files.
!
!       CRTM_Read_Aerosol_Binary:    Function to read CRTM_Aerosol
!                                    structures from Binary
!                                    CRTM_Aerosol files.
!
!       CRTM_Write_Aerosol_Binary:   Function to write CRTM_Aerosol
!                                    structures to Binary CRTM_Aerosol
!                                    files.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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

MODULE CRTM_Aerosol_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler
  USE Binary_File_Utility

  USE CRTM_Aerosol_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Inquire_Aerosol_Binary
  PUBLIC :: CRTM_Read_Aerosol_Binary
  PUBLIC :: CRTM_Write_Aerosol_Binary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Read_Aerosol_Binary
    MODULE PROCEDURE Read_Aerosol_Scalar
    MODULE PROCEDURE Read_Aerosol_Rank1
  END INTERFACE CRTM_Read_Aerosol_Binary

  INTERFACE CRTM_Write_Aerosol_Binary
    MODULE PROCEDURE Write_Aerosol_Scalar
    MODULE PROCEDURE Write_Aerosol_Rank1
  END INTERFACE CRTM_Write_Aerosol_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTM_Aerosol_Binary_IO.f90,v 1.2 2005/06/15 17:46:36 paulv Exp $'

  ! -- Keyword set values
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1

  INTEGER, PRIVATE, PARAMETER :: NO    = UNSET
  INTEGER, PRIVATE, PARAMETER :: YES   = SET


CONTAINS




!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  FUNCTION Read_Aerosol_Record( FileID,       &  ! Input
                                Aerosol,      &  ! Output
                                No_Allocate,  &  ! Optional input
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN )     :: FileID

    ! -- Outut
    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: Aerosol

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Aerosol_Binary(Record)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_Allocate
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: n_Layers, n_Modes



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Process the optional No_Allocate argument
    ! -------------------------------------------

    ! -- Default action is to allocate the structure....
    Yes_Allocate = .TRUE.

    ! -- ...unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE Aerosol DIMENSIONS --                    #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Layers, n_Modes

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Aerosol data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- ALLOCATE THE Aerosol STRUCTURE IF REQUIRED --             #
    !#--------------------------------------------------------------------------#

    IF ( Yes_Allocate ) THEN


      ! ----------------------
      ! Perform the allocation
      ! ----------------------

      Error_Status = CRTM_Allocate_Aerosol( n_Layers, &
                                            n_Modes, &
                                            Aerosol, &
                                            Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error allocating Aerosol structure.'
        GOTO 1000
      END IF


    ELSE


      ! ---------------------------------------------------------
      ! Structure already allocated. Check the association status
      ! ---------------------------------------------------------

      IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) THEN
        Message = 'Aerosol structure components are not associated.'
        GOTO 1000  ! Clean up
      END IF


      ! --------------------------
      ! Check the dimension values
      ! --------------------------

      IF ( n_Layers /= Aerosol%n_Layers .OR. &
           n_Modes  >  Aerosol%Max_Modes     ) THEN
        Message = 'Aerosol data dimensions are inconsistent with structure definition'
        GOTO 1000  ! Clean up
      END IF


      ! ------------------
      ! Set the mode value
      ! ------------------

      Aerosol%n_Modes = n_Modes

    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- READ THE AEROSOL DATA --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) Aerosol%Type, &
                                       Aerosol%Effective_Radius(:,1:n_Modes), &
                                       Aerosol%Effective_Variance(:,1:n_Modes), &
                                       Aerosol%Concentration(:,1:n_Modes)

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Aerosol data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
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

    IF ( CRTM_Associated_Aerosol( Aerosol ) ) THEN
      Destroy_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                             Message_Log = Message_Log )
    END IF

    CLOSE( FileID, IOSTAT = IO_Status )

  END FUNCTION Read_Aerosol_Record


  FUNCTION Write_Aerosol_Record( FileID,       &  ! Input
                                 Aerosol,      &  ! Input
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN )  :: FileID
    TYPE( CRTM_Aerosol_type ), INTENT( IN )  :: Aerosol

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Aerosol_Binary(Record)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) THEN
      Message = 'Some or all INPUT Aerosol pointer members are NOT associated.'
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE Aerosol DIMENSIONS --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) Aerosol%n_Layers, Aerosol%n_Modes

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Aerosol data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE AEROSOL DATA --                     #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) Aerosol%Type, &
                                        Aerosol%Effective_Radius(:,1:Aerosol%n_Modes), &
                                        Aerosol%Effective_Variance(:,1:Aerosol%n_Modes), &
                                        Aerosol%Concentration(:,1:Aerosol%n_Modes)

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Aerosol data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
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
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR, IOSTAT = IO_Status )

  END FUNCTION Write_Aerosol_Record





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
!       CRTM_Inquire_Aerosol_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Aerosol structure files.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Aerosol_Binary( Filename,                 &  ! Input
!                                                   n_Aerosols  = n_Aerosols, &  ! Optional output
!                                                   RCS_Id      = RCS_Id,     &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Aerosol format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Aerosols:     The number of Aerosol profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file inquire was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
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

  FUNCTION CRTM_Inquire_Aerosol_Binary( Filename,     &  ! Input
                                        n_Aerosols,   &  ! Optional output
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
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Aerosols

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Aerosol_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Aerosols_in_File



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
    !#                     -- OPEN THE Aerosol DATA FILE --                     #
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
    !#                      -- READ THE NUMBER OF AerosolS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Aerosols_in_File

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Aerosols data dimension from ", a, &
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
    !#                     -- SAVE THE NUMBER OF AerosolS --                    #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Aerosols ) ) n_Aerosols = n_Aerosols_in_File



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

  END FUNCTION CRTM_Inquire_Aerosol_Binary


!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Read_Aerosol_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Aerosol structure files.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Aerosol_Binary( Filename,                      &  ! Input
!                                                Aerosol,                       &  ! Output
!                                                No_File_Close = No_File_Close, &  ! Optional input
!                                                No_Allocate   = No_Allocate,   &  ! Optional input
!                                                n_Aerosols    = n_Aerosols,    &  ! Optional output
!                                                RCS_Id        = RCS_Id,        &  ! Revision control
!                                                Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Aerosol format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:  Set this argument to NOT close the file upon exit.
!                       If == 0, the input file is closed upon exit [DEFAULT]
!                          == 1, the input file is NOT closed upon exit. 
!                       If not specified, the default action is to close the
!                       input file upon exit.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       No_Allocate:    Set this argument to NOT allocate the output Aerosol
!                       structure in this routine based on the data dimensions
!                       read from the input data file. This assumes that the
!                       structure has already been allocated prior to calling 
!                       this function.
!                       If == 0, the output Aerosol structure is allocated [DEFAULT]
!                          == 1, the output Aerosol structure is NOT allocated
!                       If not specified, the default action is to allocate 
!                       the output Aerosol structure to the dimensions specified
!                       in the input data file.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Aerosol:        Structure containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Scalar or Rank-1
!                       ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Aerosols:     The actual number of aerosol profiles read in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Destroy_Aerosol:      Function to re-initialize a CRTM_Aerosol
!                                  structure.
!                                  SOURCE: CRTM_AEROSOL_DEFINE module
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
!       If an error occurs:
!       - the input file is closed and,
!       - the output Aerosol structure is deallocated.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Aerosol argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Aerosol_Scalar( Filename,      &  ! Input
                                Aerosol,       &  ! Output
                                No_File_Close, &  ! Optional input
                                No_Allocate,   &  ! Optional input
                                n_Aerosols,    &  ! Optional output
                                RCS_Id,        &  ! Revision control
                                Message_Log )  &  ! Error messaging
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
    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: Aerosol

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_File_Close
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Optional output
    INTEGER,         OPTIONAL, INTENT( OUT )    :: n_Aerosols

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Aerosol_Binary(Scalar)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: n_Input_Aerosols



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

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get its file id
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      ! -- Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 

      ! -- Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF

    END IF


    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE NUMBER OF AEROSOLS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Aerosols

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Aerosols data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -----------------------
    ! Check if n_Aerosols > 1
    ! -----------------------

    IF ( n_Input_Aerosols > 1 ) THEN
      WRITE( Message, '( "Number of aerosols > 1 (",i5,") and output Aerosol structure ", &
                        &"is scalar." )' ) n_Input_Aerosols
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE STRUCTURE DATA --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_Aerosol_Record( FileID, &
                                        Aerosol, &
                                        No_Allocate = No_Allocate, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Aerosol record from '//TRIM( Filename )
      GOTO 2000  ! Clean up (file already closed)
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- SAVE THE NUMBER OF AEROSOLS READ --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Aerosols ) ) n_Aerosols = 1



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

    IF ( CRTM_Associated_Aerosol( Aerosol ) ) THEN
      Destroy_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                             Message_Log = Message_Log )
    END IF

  END FUNCTION Read_Aerosol_Scalar


  FUNCTION Read_Aerosol_Rank1( Filename,      &  ! Input
                               Aerosol,       &  ! Output
                               No_File_Close, &  ! Optional input
                               No_Allocate,   &  ! Optional input
                               n_Aerosols,    &  ! Optional output
                               RCS_Id,        &  ! Revision control
                               Message_Log )  &  ! Error messaging
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
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol

    ! -- Optional input
    INTEGER,                   OPTIONAL,       INTENT( IN )     :: No_File_Close
    INTEGER,                   OPTIONAL,       INTENT( IN )     :: No_Allocate

    ! -- Optional output
    INTEGER,                   OPTIONAL,       INTENT( OUT )    :: n_Aerosols

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Aerosol_Binary(Rank-1)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: m, n_Input_Aerosols



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

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get its FileID.
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      ! -- Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 

      ! -- Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF

    END IF


    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF




    !#--------------------------------------------------------------------------#
    !#                      -- READ THE NUMBER OF AEROSOLS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Aerosols

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Aerosols data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ------------------------------------------
    ! Check if n_Aerosols > size of output array
    ! ------------------------------------------

    IF ( n_Input_Aerosols > SIZE( Aerosol ) ) THEN
      WRITE( Message, '( "Number of aerosols, ", i5, " > size of the output Aerosol ", &
                        &"structure array, ", i5, "." )' ) &
                      n_Input_Aerosols, SIZE( Aerosol )
      GOTO 1000  ! Clean up
    END IF


    !#--------------------------------------------------------------------------#
    !#                           -- LOOP OVER AEROSOLS --                       #
    !#--------------------------------------------------------------------------#

    Aerosol_Loop: DO m = 1, n_Input_Aerosols

      Error_Status = Read_Aerosol_Record( FileID, &
                                          Aerosol(m), &
                                          No_Allocate = No_Allocate, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Aerosol element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000  ! Clean up
      END IF

    END DO Aerosol_Loop



    !#--------------------------------------------------------------------------#
    !#                  -- SAVE THE NUMBER OF AEROSOL READ --                   #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Aerosols ) ) n_Aerosols = n_Input_Aerosols



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                           Message_Log = Message_Log )

  END FUNCTION Read_Aerosol_Rank1





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Write_Aerosol_Binary
!
! PURPOSE:
!       Function to write Binary format Aerosol files.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Aerosol_Binary( Filename,                      &  ! Input
!                                                 Aerosol,                       &  ! Input
!                                                 No_File_Close = No_File_Close, &  ! Optional input
!                                                 RCS_Id        = RCS_Id,        &  ! Revision control
!                                                 Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of an output
!                       Aerosol format data file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Aerosol:        Structure containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Scalar or Rank-1
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:  Set this argument to NOT close the file upon exit.
!                       If == 0, the input file is closed upon exit [DEFAULT]
!                          == 1, the input file is NOT closed upon exit. 
!                       If not specified, the default action is to close the
!                       input file upon exit.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
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
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Aerosol_Scalar( Filename,      &  ! Input
                                 Aerosol,       &  ! Input
                                 No_File_Close, &  ! Optional input
                                 RCS_Id,        &  ! Revision control
                                 Message_Log )  &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )  :: Filename
    TYPE( CRTM_Aerosol_type ), INTENT( IN )  :: Aerosol

    ! -- Optional input
    INTEGER,        OPTIONAL,  INTENT( IN )  :: No_File_Close

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Aerosol_Binary(Scalar)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
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
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output  = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 1000  ! Clean up
      END IF

    END IF


    ! --------------------------------------
    ! Check the Aerosol structure dimensions
    ! --------------------------------------

    IF ( Aerosol%n_Layers < 1 .OR. &
         Aerosol%n_Modes  < 1      ) THEN
      Message = 'Dimensions of Aerosol structure are < or = 0.'
      GOTO 1000
    END IF


    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE NUMBER OF PROFILES --                   #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) 1

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Aerosols data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_Aerosol_Record( FileID, &
                                         Aerosol, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Aerosol record to '//TRIM( Filename )
      GOTO 2000
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- CLOSE THE OUTPUT FILE IF REQUIRED --                  #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_Aerosol_Scalar


  FUNCTION Write_Aerosol_Rank1( Filename,      &  ! Input
                                Aerosol,       &  ! Input
                                No_File_Close, &  ! Optional input
                                RCS_Id,        &  ! Revision control
                                Message_Log )  &  ! Error messaging
                              RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: Filename
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN )  :: Aerosol

    ! -- Optional input
    INTEGER,                   OPTIONAL,       INTENT( IN )  :: No_File_Close

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Aerosol_Binary(Rank-1)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m
 


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

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output  = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 1000  ! Clean up
      END IF

    END IF


    ! --------------------------------------
    ! Check the Aerosol structure dimensions
    ! --------------------------------------

    IF ( ANY( Aerosol%n_Layers < 1 ) .OR. &
         ANY( Aerosol%n_Modes  < 1 )      ) THEN
      Message = 'Dimensions of some Aerosol structures is < or = 0.'
      GOTO 1000
    END IF


    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE NUMBER OF AEROSOLS --                   #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) SIZE( Aerosol )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Aerosols data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 2000
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- LOOP OVER AEROSOLS --                         #
    !#--------------------------------------------------------------------------#

    Aerosol_Loop: DO m = 1, SIZE( Aerosol )

      Error_Status = Write_Aerosol_Record( FileID, &
                                           Aerosol(m), &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Aerosol element #", i5, " to ", a )' ) &
                      m, TRIM( Filename )
        GOTO 2000
      END IF

    END DO Aerosol_Loop



    !#--------------------------------------------------------------------------#
    !#                 -- CLOSE THE OUTPUT FILE IF REQUIRED --                  #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_Aerosol_Rank1

END MODULE CRTM_Aerosol_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Aerosol_Binary_IO.f90,v 1.2 2005/06/15 17:46:36 paulv Exp $
!
! $Date: 2005/06/15 17:46:36 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Aerosol_Binary_IO.f90,v $
! Revision 1.2  2005/06/15 17:46:36  paulv
! - Added Inquire() function.
!
! Revision 1.1  2005/03/24 15:12:41  paulv
! Initial checkin.
!
!
!
