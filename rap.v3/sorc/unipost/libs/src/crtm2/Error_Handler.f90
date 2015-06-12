!------------------------------------------------------------------------------
!M+
! NAME:
!       Error_Handler
!
! PURPOSE:
!       Module to define simple error codes and output messages.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Error_Handler
!
! OUTPUTS:
!       SUCCESS:     Code specifying successful completion.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: PARAMETER, PUBLIC
!
!       INFORMATION: Code specifying information output.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: PARAMETER, PUBLIC
!
!       WARNING:     Code specifying warning state. Execution can
!                    continue but results may be incorrect.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: PARAMETER, PUBLIC
!
!       FAILURE:     Code specifying severe error. Execution cannot
!                    continue.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: PARAMETER, PUBLIC
!
!       UNDEFINED:   Code specifying undefined completion status.
!                    UNITS:      N/A
!                    TYPE:       INTEGER
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: PARAMETER, PUBLIC
!
!
! MODULES:
!       File_Utility:     Module containing generic file utility routines.
!
! CONTAINS:
!       Display_Message:  Subroutine to display error/status messages either
!                         to standard output (default) or to a log file.
!
!       Open_Message_Log: Function to open the message log file.
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
! EXAMPLE:
!       USE Error_Handler
!       Error_Status = calculate_widget_size()
!       IF ( Error_Status /= SUCCESS ) THEN
!         CALL Display_Message( Routine_Name, &
!                               'Error calculating widget size', &
!                               Error_Status, &
!                               Message_Log = 'error_log.txt' )
!         RETURN
!       END IF
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2004 Paul van Delst
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

MODULE Error_Handler


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE File_Utility, ONLY: Get_Lun


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Display_Message
  PUBLIC :: Open_Message_Log


  ! ------------------------------------
  ! Definitions of public parameter data
  ! ------------------------------------
 
  ! -- Integer values that define the error state.
  ! -- Note: These values are totally arbitrary. 
  INTEGER, PARAMETER, PUBLIC :: SUCCESS     = 0
  INTEGER, PARAMETER, PUBLIC :: INFORMATION = 1
  INTEGER, PARAMETER, PUBLIC :: WARNING     = 2
  INTEGER, PARAMETER, PUBLIC :: FAILURE     = 3
  INTEGER, PARAMETER, PUBLIC :: UNDEFINED   = 4


  ! -----------------------------------
  ! Definitions of local parameter data
  ! -----------------------------------

  ! -- Character descriptors of the error states
  INTEGER,         PARAMETER :: MAX_N_STATES = 4
  CHARACTER( 11 ), PARAMETER, DIMENSION( 0:MAX_N_STATES ) :: &
    STATE_DESCRIPTOR = (/ 'SUCCESS    ', &
                          'INFORMATION', &
                          'WARNING    ', &
                          'FAILURE    ', &
                          'UNDEFINED  ' /)


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!S+
! NAME:
!       Display_Message
!
! PURPOSE:
!       RECURSIVE subroutine to display messages.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Display_Message( Routine_Name, &
!                             Message,      &
!                             Error_State,  &
!                             Message_Log = Message_Log )
!
! INPUT ARGUMENTS:
!       Routine_Name: Name of the routine in which the message originated.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       Message:      Message text
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       Error_State:  Flag corresponding to one of the defined error states.
!                     If not, the error state is set to UNDEFINED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! CALLS:
!      Open_Message_Log:  Function to open the message log file.
!
!      This routine calls itself if the optional argument Message_Log is passed
!      and an error occurs opening the output log file.
!
! FILES ACCESSED:
!      The user specified, if any, message log file.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       Output message format is:
!
!         "routine name"("state description") : "message"
!
!       For example, if an error occurs in this routine the output is:
!
!         "DISPLAY_MESSAGE(FAILURE) : Error opening message log file"
!S-
!----------------------------------------------------------------------------------

  RECURSIVE SUBROUTINE Display_Message ( Routine_Name, &
                                         Message,      &
                                         Error_State,  &
                                         Message_Log   )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),           INTENT( IN ) :: Routine_Name
    CHARACTER( * ),           INTENT( IN ) :: Message
    INTEGER,                  INTENT( IN ) :: Error_State
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: THIS_ROUTINE_NAME = 'Display_Message'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Error_State_To_Use
    INTEGER :: Log_To_File
    INTEGER :: File_ID
    INTEGER :: Error_Status

    CHARACTER( 28 ) :: Fmt_String



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK THE INPUT ERROR STATE --                      #
    !#--------------------------------------------------------------------------#

    Error_State_To_Use = Error_State
    IF ( Error_State < 0 .OR. Error_State > MAX_N_STATES ) THEN
      Error_State_To_Use = UNDEFINED
    END IF



    !#--------------------------------------------------------------------------#
    !#      -- SET THE MESSAGE LOG. IF NOT SPECIFIED, OUTPUT TO SCREEN --       #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Message_Log ) ) THEN

      Log_To_File = 1

      Error_Status = Open_Message_Log( TRIM( Message_Log ), File_ID )
      IF ( Error_Status /= 0 ) THEN
        CALL Display_Message( THIS_ROUTINE_NAME, &
                              'Error opening message log file', &
                              FAILURE )
        Log_To_File = 0
      END IF

    ELSE

      Log_To_File = 0

    END IF


    !#--------------------------------------------------------------------------#
    !#                         -- OUTPUT THE MESSAGE --                         #
    !#--------------------------------------------------------------------------#

    Fmt_String = '( 1x, a, "(", a, ") : ", a )'

    Log_Message: IF ( Log_To_File == 0 ) THEN
      WRITE( *, FMT = Fmt_String ) &
                TRIM( Routine_Name ), &
                TRIM( STATE_DESCRIPTOR( Error_State_To_Use ) ), &
                TRIM( Message )
    ELSE
      WRITE( File_ID, FMT = Fmt_String ) &
                      TRIM( Routine_Name ), &
                      TRIM( STATE_DESCRIPTOR( Error_State_To_Use ) ), &
                      TRIM( Message )
      CLOSE( File_ID )
    END IF Log_Message

  END SUBROUTINE Display_Message





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Open_Message_Log
!
! PURPOSE:
!       Function to open the message log file.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status =  open_Message_Log( Message_Log, &  ! Input
!                                         File_ID      )  ! Output
!
! INPUTS:
!       Message_Log:  Character string specifying the filename to open.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       None.
!
! OUTPUTS:
!       File_ID:      Logical unit number associated with the
!                     Message_Log file.
!                     Return value is undefined if an error occurs.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Message_Log file was successfully opened.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!      Get_Lun:       Function to return a free logical unit number for
!                     file access.
!                     SOURCE: FILE_UTILITY module
!
! SIDE EFFECTS:
!       The file is opened for SEQUENTIAL, FORMATTED access with
!       UNKNOWN status, position of APPEND, and action of READWRITE.
!       The latter is to assuage some compilers that seem to cough
!       when the specified position is APPEND and the action is WRITE.
!
!       Hopefully all of these options will not cause an existing file
!       to be inadvertantly overwritten.
!
! RESTRICTIONS:
!       None.
!
!S-
!----------------------------------------------------------------------------------

  FUNCTION Open_Message_Log( Message_Log, File_ID ) RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ), INTENT( IN ) :: Message_Log
    INTEGER,        INTENT( OUT) :: File_ID


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Lun
    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- OPEN ZE FILE --                            #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a file unit number
    ! ----------------------

    Lun = Get_Lun()

    IF ( Lun < 0 ) THEN
      Error_Status = FAILURE
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( Lun, FILE     = TRIM( Message_Log ), &
               ACCESS   = 'SEQUENTIAL', &
               FORM     = 'FORMATTED', &
               STATUS   = 'UNKNOWN', &
               POSITION = 'APPEND', &
               ACTION   = 'READWRITE', &
               IOSTAT   = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      RETURN
    END IF


    ! ------------------
    ! Return the file ID
    ! ------------------

    File_ID = Lun

  END FUNCTION Open_Message_Log

END MODULE Error_Handler


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Error_Handler.f90,v 1.2 2006/02/09 14:46:21 rtreadon Exp $
!
! $Date: 2006/02/09 14:46:21 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Error_Handler.f90,v $
! Revision 1.2  2006/02/09 14:46:21  rtreadon
! NCEP February 2006 update
!
! Revision 1.7  2004/08/11 20:34:41  paulv
! - Updated.
!
! Revision 1.6  2002/05/14 20:36:59  paulv
! - Added OPEN_MESSAGE_LOG function.
!
! Revision 1.5  2001/09/23 19:38:17  paulv
! - Added CVS "Name" to modification history keyword list.
!
! Revision 1.4  2001/09/17 20:13:16  paulv
! - Module now resides in the UTILITY module directory.
!
! Revision 1.3  2000/08/31 19:36:32  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.2  2000/08/24 15:27:18  paulv
! - The DISPLAY_MESSAGE subprogram was made RECURSIVE so it can call itself
!   if an error occurs opening the message log file defined by the optional
!   input argument MESSAGE_LOG.
! - The message log file is now closed after the message is written (as it
!   should have always been...oops).
! - Updated module and subprogram documentation.
!
! Revision 1.1  2000/07/12 16:08:10  paulv
! Initial checked in version
!
!
!
