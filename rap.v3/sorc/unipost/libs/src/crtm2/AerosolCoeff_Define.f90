!--------------------------------------------------------------------------------
!M+
! NAME:
!       AerosolCoeff_Define
!
! PURPOSE:
!       Module defining the AerosolCoeff data structure and containing routines
!       to manipulate it.
!       
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AerosolCoeff_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               check comparisons on input floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
! CONTAINS:
!       Associated_AerosolCoeff:      Function to test the association status
!                                     of the pointer members of a AerosolCoeff
!                                     structure.
!
!       Destroy_AerosolCoeff:         Function to re-initialize an AerosolCoeff
!                                     structure.
!
!       Allocate_AerosolCoeff:        Function to allocate the pointer members
!                                     of an AerosolCoeff structure.
!
!       Assign_AerosolCoeff:          Function to copy an AerosolCoeff structure.
!
!       Equal_AerosolCoeff:           Function to test if two AerosolCoeff
!                                     structures are equal.
!
!       Check_AerosolCoeff_Release:   Function to check the AerosolCoeff Release value.
!
!       Version_AerosolCoeff:         Subroutine to return a string containing
!                                     version and dimension information about
!                                     the AerosolCoeff data structure.
!
! DERIVED TYPES:
!       AerosolCoeff_type:   Definition of the AerosolCoeff data structure. Fields
!                            are...
!
!         Release:       Coefficient data release.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!         Version:       Coefficient data version.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!         n_Channel:     Total number of spectral channels.
!                        "L" dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!         Absorption:    Aerosol absorption coefficients.
!                        UNITS:      N/A
!                        TYPE:       REAL( Double )
!                        DIMENSION:  Rank-1 (n_Channels)
!                        ATTRIBUTES: POINTER
!
!         Scattering:    Aerosol scattering coefficients
!                        UNITS:      N/A
!                        TYPE:       REAL( Double )
!                        DIMENSION:  Rank-1 (n_Channels)
!                        ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the AerosolCoeff_type is PUBLIC and its members are not
!       encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, assign, and concatenate the structure using only the
!       routines in this module where possible to eliminate -- or at
!       least minimise -- the possibility of memory leakage since most
!       of the structure members are pointers.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
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
!--------------------------------------------------------------------------------

MODULE AerosolCoeff_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler
  USE Compare_Float_Numbers


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to manipulate the AerosolCoeff structure
  PUBLIC :: Associated_AerosolCoeff
  PUBLIC :: Destroy_AerosolCoeff
  PUBLIC :: Allocate_AerosolCoeff
  PUBLIC :: Assign_AerosolCoeff
  PUBLIC :: Equal_AerosolCoeff
  PUBLIC :: Check_AerosolCoeff_Release
  PUBLIC :: Version_AerosolCoeff


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: AerosolCoeff_Define.f90,v 1.1 2005/02/04 22:35:11 paulv Exp $'

  ! -- AerosolCoeff initialisation values
  INTEGER,        PRIVATE, PARAMETER :: IP_INIT = -1
  REAL( Double ), PRIVATE, PARAMETER :: FP_INIT = 0.0_Double

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Current valid release and version numbers
  INTEGER, PRIVATE, PARAMETER :: AEROSOLCOEFF_RELEASE = 1  ! This determines structure and file formats.
  INTEGER, PRIVATE, PARAMETER :: AEROSOLCOEFF_VERSION = 1  ! This is just the data version.


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- Number of AerosolCoeff pointer data items
  INTEGER( Long ), PUBLIC, PARAMETER :: N_AEROSOLCOEFF_ITEMS = 2_Long

  ! -- Data types of the AerosolCoeff pointer data
  !    7 = Character string
  !    5 = Double (i.e. 8-byte float)
  !    4 = Single (i.e. 4-byte float)
  !    3 = Long   (i.e. 4-byte integer)
  INTEGER( Long ), PUBLIC, PARAMETER, &
                   DIMENSION( N_AEROSOLCOEFF_ITEMS ) :: AEROSOLCOEFF_DATA_TYPE = &
                                                       (/ 5_Long, &  ! Absorption
                                                          5_Long /)  ! Scattering

  ! -- Names of the pointer data items (for error processing)
  CHARACTER( * ), PUBLIC, PARAMETER, &
                  DIMENSION( N_AEROSOLCOEFF_ITEMS ) :: AEROSOLCOEFF_DATA_NAME = &
                                                    (/ 'Absorption_Coefficient', &
                                                       'Scattering_Coefficient' /)


  ! ------------------------------
  ! AerosolCoeff data type definition
  ! ------------------------------

  TYPE, PUBLIC :: AerosolCoeff_type
    INTEGER :: n_Allocates = 0

    INTEGER( Long ) :: Release = AEROSOLCOEFF_RELEASE
    INTEGER( Long ) :: Version = AEROSOLCOEFF_VERSION

    INTEGER( Long ) :: n_Channels = 0
    REAL( Double ), POINTER, DIMENSION( : ) :: Absorption => NULL()
    REAL( Double ), POINTER, DIMENSION( : ) :: Scattering => NULL()

  END TYPE AerosolCoeff_type


CONTAINS




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_AerosolCoeff
!
! PURPOSE:
!       Subroutine to clear the scalar members of a AerosolCoeff structure.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_AerosolCoeff( AerosolCoeff ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff:  AerosolCoeff structure for which the scalar members have
!                      been cleared.
!                      UNITS:      N/A
!                      TYPE:       AerosolCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
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
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_AerosolCoeff( AerosolCoeff )

    TYPE( AerosolCoeff_type ), INTENT( IN OUT ) :: AerosolCoeff

    AerosolCoeff%n_Channels  = 0

  END SUBROUTINE Clear_AerosolCoeff





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
!       Associated_AerosolCoeff
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       AerosolCoeff structure.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = Associated_AerosolCoeff( AerosolCoeff,       &  ! Input
!                                                     ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       AerosolCoeff:        AerosolCoeff structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       AerosolCoeff_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            AerosolCoeff structure pointer members are associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the AerosolCoeff pointer members.
!                            .TRUE.  - if ALL the AerosolCoeff pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the AerosolCoeff pointer
!                                      members are associated.
!                            .FALSE. - some or all of the AerosolCoeff pointer
!                                      members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Associated_AerosolCoeff( AerosolCoeff, & ! Input
                                    ANY_Test )    & ! Optional input
                                  RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AerosolCoeff_type ), INTENT( IN ) :: AerosolCoeff

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN ) :: ANY_Test


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( AerosolCoeff%Absorption ) .AND. &
           ASSOCIATED( AerosolCoeff%Scattering )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( AerosolCoeff%Absorption ) .OR. &
           ASSOCIATED( AerosolCoeff%Scattering )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_AerosolCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Destroy_AerosolCoeff
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of AerosolCoeff
!       data structures.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_AerosolCoeff( AerosolCoeff,             &  ! Output
!                                            RCS_Id = RCS_Id,          &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff: Re-initialized AerosolCoeff structure.
!                     UNITS:      N/A
!                     TYPE:       AerosolCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE - an error occurred, or
!                                   - the structure internal allocation counter
!                                     is not equal to zero (0) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_AerosolCoeff( AerosolCoeff, &  ! Output
                                 No_Clear,     &  ! Optional input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( AerosolCoeff_type ), INTENT( IN OUT ) :: AerosolCoeff

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_AerosolCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is to clear scalar members...
    Clear = .TRUE.
    ! -- ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_AerosolCoeff( AerosolCoeff )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate Absorption
    IF ( ASSOCIATED( AerosolCoeff%Absorption ) ) THEN

      DEALLOCATE( AerosolCoeff%Absorption, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AerosolCoeff Absorption ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate Scattering
    IF ( ASSOCIATED( AerosolCoeff%Scattering ) ) THEN

      DEALLOCATE( AerosolCoeff%Scattering, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating AerosolCoeff Scattering ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    AerosolCoeff%n_Allocates = AerosolCoeff%n_Allocates - 1

    IF ( AerosolCoeff%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      AerosolCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_AerosolCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Allocate_AerosolCoeff
! 
! PURPOSE:
!       Function to allocate the pointer members of the AerosolCoeff
!       data structure.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_AerosolCoeff( n_Channels,               &  ! Input
!                                             AerosolCoeff,             &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:         Required dimension of AerosolCoeff structure pointer
!                           members.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff:       AerosolCoeff structure with allocated pointer members
!                           UNITS:      N/A
!                           TYPE:       AerosolCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the structure pointer allocations were
!                                         successful
!                              == FAILURE - an error occurred, or
!                                         - the structure internal allocation counter
!                                           is not equal to one (1) upon exiting this
!                                           function. This value is incremented and
!                                           decremented for every structure allocation
!                                           and deallocation respectively.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Associated_AerosolCoeff:  Function to test the association status of the
!                                 pointer members of a AerosolCoeff structure.
!
!       Destroy_AerosolCoeff:     Function to re-initialize the scalar and pointer
!                                 members of AerosolCoeff data structures.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_AerosolCoeff( n_Channels,   &  ! Input
                                  AerosolCoeff, &  ! Output
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( AerosolCoeff_type ), INTENT( IN OUT ) :: AerosolCoeff

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_AerosolCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! The dimensions
    ! --------------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( Associated_AerosolCoeff( AerosolCoeff, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_AerosolCoeff( AerosolCoeff, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating AerosolCoeff pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( AerosolCoeff%Absorption( n_Channels ), &
              AerosolCoeff%Scattering( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AerosolCoeff data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#            -- ASSIGN THE DIMENSIONS AND INITIALISE ARRAYS --             #
    !#--------------------------------------------------------------------------#

    AerosolCoeff%n_Channels = n_Channels

    AerosolCoeff%Absorption = FP_INIT
    AerosolCoeff%Scattering = FP_INIT



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    AerosolCoeff%n_Allocates = AerosolCoeff%n_Allocates + 1

    IF ( AerosolCoeff%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      AerosolCoeff%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_AerosolCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Assign_AerosolCoeff
!
! PURPOSE:
!       Function to copy valid AerosolCoeff structures.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Assign_AerosolCoeff( AerosolCoeff_in,          &  ! Input
!                                           AerosolCoeff_out,         &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AerosolCoeff_in:   AerosolCoeff structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AerosolCoeff_out:  Copy of the input structure, AerosolCoeff_in.
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Associated_AerosolCoeff:  Function to test the association status of the
!                                 pointer members of a AerosolCoeff structure.
!
!       Allocate_AerosolCoeff:    Function to allocate the pointer members of
!                                 the AerosolCoeff data structure.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The AerosolCoeff structure copy is performed only if *all* of the
!       pointer members of the input structure are associated. If this is
!       not the case, then the input structure is treated as "empty" and
!       the output structure is thus simply destroyed upon exit to reflect
!       this "empty" status.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AerosolCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_AerosolCoeff( AerosolCoeff_in,  &  ! Input
                                AerosolCoeff_out, &  ! Output
                                RCS_Id,           &  ! Revision control
                                Message_Log )     &  ! Error messaging
                              RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AerosolCoeff_type ), INTENT( IN )     :: AerosolCoeff_in

    ! -- Output
    TYPE( AerosolCoeff_type ), INTENT( IN OUT ) :: AerosolCoeff_out

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_AerosolCoeff'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------

    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff_In ) ) THEN

      Error_Status = Destroy_AerosolCoeff( AerosolCoeff_Out, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error deallocating output CRTM_AerosolCoeff pointer members.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      RETURN

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_AerosolCoeff( AerosolCoeff_in%n_Channels,  &
                                          AerosolCoeff_out, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AerosolCoeff arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    AerosolCoeff_out%Release = AerosolCoeff_in%Release
    AerosolCoeff_out%Version = AerosolCoeff_in%Version


    ! -----------------
    ! Assign array data
    ! -----------------

    AerosolCoeff_out%Absorption = AerosolCoeff_in%Absorption
    AerosolCoeff_out%Scattering = AerosolCoeff_in%Scattering

  END FUNCTION Assign_AerosolCoeff





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Equal_AerosolCoeff
!
! PURPOSE:
!       Function to test if two AerosolCoeff structures are equal.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Equal_AerosolCoeff( AerosolCoeff_LHS,         &  ! Input
!                                          AerosolCoeff_RHS,         &  ! Input
!                                          ULP_Scale   = ULP_Scale,  &  ! Optional input
!                                          Check_All   = Check_All,  &  ! Optional input
!                                          RCS_Id      = RCS_Id,     &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AerosolCoeff_LHS:  AerosolCoeff structure to be compared; equivalent to the
!                          left-hand side of a lexical comparison, e.g.
!                            IF ( AerosolCoeff_LHS == AerosolCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolCoeff_RHS:  AerosolCoeff structure to be compared to; equivalent to
!                          right-hand side of a lexical comparison, e.g.
!                            IF ( AerosolCoeff_LHS == AerosolCoeff_RHS ).
!                          UNITS:      N/A
!                          TYPE:       AerosolCoeff_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:         Unit of data precision used to scale the floating
!                          point comparison. ULP stands for "Unit in the Last Place,"
!                          the smallest possible increment or decrement that can be
!                          made using a machine's floating point arithmetic.
!                          Value must be positive - if a negative value is supplied,
!                          the absolute value is used. If not specified, the default
!                          value is 1.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Check_All:         Set this argument to check ALL the floating point
!                          channel data of the AerosolCoeff structures. The default
!                          action is return with a FAILURE status as soon as
!                          any difference is found. This optional argument can
!                          be used to get a listing of ALL the differences
!                          between data in AerosolCoeff structures.
!                          If == 0, Return with FAILURE status as soon as
!                                   ANY difference is found  *DEFAULT*
!                             == 1, Set FAILURE status if ANY difference is
!                                   found, but continue to check ALL data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the structures were equal
!                             == FAILURE - an error occurred, or
!                                        - the structures were different.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!       Associated_AerosolCoeff:  Function to test the association status of the
!                                 pointer members of a AerosolCoeff structure.
!
!       Compare_Float:            Function to compare floating point numbers
!                                 for equality.
!                                 SOURCE: COMPARE_FLOAT_NUMBERS module.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Equal_AerosolCoeff( AerosolCoeff_LHS, &  ! Input
                               AerosolCoeff_RHS, &  ! Input
                               ULP_Scale,        &  ! Optional input
                               Check_All,        &  ! Optional input
                               RCS_Id,           &  ! Revision control
                               Message_Log )     &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AerosolCoeff_type ), INTENT( IN )  :: AerosolCoeff_LHS
    TYPE( AerosolCoeff_type ), INTENT( IN )  :: AerosolCoeff_RHS

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )  :: ULP_Scale
    INTEGER,         OPTIONAL, INTENT( IN )  :: Check_All

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Equal_AerosolCoeff'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK THE OPTIONAL ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Test the ULP_Scale argument
    ! ---------------------------

    ! -- Default precision is a single unit in last place
    ULP = 1

    ! -- ... unless the ULP_Scale argument is set and positive
    IF ( PRESENT( ULP_Scale ) ) THEN
      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
    END IF


    ! ---------------------------
    ! Test the Check_All argument
    ! ---------------------------

    ! -- Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! -- ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! The LHS structure
    ! -----------------

    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff_LHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AerosolCoeff_LHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! The RHS structure
    ! -----------------

    IF ( .NOT. Associated_AerosolCoeff( AerosolCoeff_RHS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Some or all INPUT AerosolCoeff_RHS pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK SCALAR MEMBERS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Release/Version info
    ! --------------------

    IF ( ( AerosolCoeff_LHS%Release /= AerosolCoeff_RHS%Release ) .OR. &
         ( AerosolCoeff_LHS%Version /= AerosolCoeff_RHS%Version )      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Release/Version numbers are different : ", &
                        &i2, ".", i2.2, " vs. ", i2, ".", i2.2 )' ) &
                      AerosolCoeff_LHS%Release, AerosolCoeff_LHS%Version, &
                      AerosolCoeff_RHS%Release, AerosolCoeff_RHS%Version
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------
    ! Dimensions
    ! ----------

    IF ( AerosolCoeff_LHS%n_Channels /= AerosolCoeff_RHS%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "n_Channels dimensions are different : ", &
                        &i4, " vs. ", i4 )' ) &
                      AerosolCoeff_LHS%n_Channels, AerosolCoeff_RHS%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK POINTER MEMBERS BY ELEMENT --                  #
    !#                                                                          #
    !# Each structure member is tested separately. It's a bit of a brain dead   #
    !# way to do it, but easiest to implement since the data types differ.      #
    !# Also, each channel is tested explicitly, rather than using the ANY       #
    !# or ALL intrinsic functions, since I wanted to highlight the actual       #
    !# channel index where any difference occured so it would be very easy to   #
    !# track down the location of the difference.                               #
    !#--------------------------------------------------------------------------#

    Dimension_Loop: DO n = 1, AerosolCoeff_RHS%n_Channels


      ! ----------------------------
      ! Absorption coefficient array
      ! ----------------------------

      IF ( .NOT. ( Compare_Float( AerosolCoeff_LHS%Absorption(n), &
                                  AerosolCoeff_RHS%Absorption(n), &
                                  ULP = ULP                       ) ) ) THEN

        Error_Status = FAILURE
        WRITE( Message, '( "Absorption coefficient values are different, ", &
                          &i5, " vs. ", i5, ", for index # ", i4 )' ) &
                        AerosolCoeff_LHS%Absorption(n), &
                        AerosolCoeff_RHS%Absorption(n), &
                        n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF


      ! ----------------------------
      ! Scattering coefficient array
      ! ----------------------------

      IF ( .NOT. ( Compare_Float( AerosolCoeff_LHS%Scattering(n), &
                                  AerosolCoeff_RHS%Scattering(n), &
                                  ULP = ULP                       ) ) ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Scattering coefficient values are different, ", &
                          &f13.6, " vs. ", f13.6, ",  for index # ", i4 )' ) &
                        AerosolCoeff_LHS%Scattering(n), &
                        AerosolCoeff_RHS%Scattering(n), &
                        n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        IF ( Check_Once ) RETURN
      END IF

    END DO Dimension_Loop

  END FUNCTION Equal_AerosolCoeff





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Check_AerosolCoeff_Release
!
! PURPOSE:
!       Function to check the AerosolCoeff Release value.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Check_AerosolCoeff_Release( AerosolCoeff,             &  ! Input
!                                                  RCS_Id      = RCS_Id,     &  ! Revision control
!                                                  Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AerosolCoeff:  AerosolCoeff structure for which the Release member
!                      is to be checked.
!                      UNITS:      N/A
!                      TYPE:       AerosolCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure Release value is valid.
!                         == FAILURE the structure Release value is NOT valid
!                                    and either a data file file or software
!                                    update is required.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  FUNCTION Check_AerosolCoeff_Release( AerosolCoeff, &  ! Input
                                       RCS_Id,       &  ! Revision control
                                       Message_Log ) &  ! Error messaging
                                     RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AerosolCoeff_type ), INTENT( IN )  :: AerosolCoeff

    ! -- Optional output
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Check_AerosolCoeff_Release'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO OLD --                #
    !#--------------------------------------------------------------------------#

    IF ( AerosolCoeff%Release < AEROSOLCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An AerosolCoeff data update is needed. ", &
                        &"AerosolCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      AerosolCoeff%Release, AEROSOLCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#               -- CHECK THAT THE RELEASE IS NOT TOO NEW --                #
    !#--------------------------------------------------------------------------#

    IF ( AerosolCoeff%Release > AEROSOLCOEFF_RELEASE ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "An AerosolCoeff software update is needed. ", &
                        &"AerosolCoeff release is ", i2, &
                        &". Valid release is ",i2,"." )' ) &
                      AerosolCoeff%Release, AEROSOLCOEFF_RELEASE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Check_AerosolCoeff_Release





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Version_AerosolCoeff
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the AerosolCoeff data structure.
!
! CATEGORY:
!       Aerosol : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Version_AerosolCoeff( AerosolCoeff,   &  ! Input
!                                  Version_Info,   &  ! Output
!                                  RCS_Id = RCS_Id )  ! Revision control
!
! INPUT ARGUMENTS:
!       AerosolCoeff:  Filled AerosolCoeff structure.
!                      UNITS:      N/A
!                      TYPE:       AerosolCoeff_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Version_Info:  String containing version and dimension information
!                      about the passed AerosolCoeff data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      None
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Version_AerosolCoeff( AerosolCoeff, &  ! Input
                                   Version_Info, &  ! Output
                                   RCS_Id        )  ! Revision control



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( AerosolCoeff_type ), INTENT( IN )  :: AerosolCoeff

    ! -- Output
    CHARACTER( * ),            INTENT( OUT ) :: Version_Info

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id


    ! ----------
    ! Parameters
    ! ----------

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Long_String



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FILL THE VERSION INFO STRING --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Write the required data to the local string
    ! -------------------------------------------

    WRITE( Long_String, '( a,1x,"AerosolCoeff RELEASE.VERSION: ", i2, ".", i2.2, 2x, &
                           &"N_CHANNELS=",i4 )' ) &
                        ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                        AerosolCoeff%Release, AerosolCoeff%Version, &
                        AerosolCoeff%n_Channels


    ! ----------------------------
    ! Trim the output based on the
    ! dummy argument string length
    ! ----------------------------

    Version_Info = Long_String(1:MIN( LEN( Version_Info ), LEN_TRIM( Long_String ) ))

  END SUBROUTINE Version_AerosolCoeff

END MODULE AerosolCoeff_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: AerosolCoeff_Define.f90,v 1.1 2005/02/04 22:35:11 paulv Exp $
!
! $Date: 2005/02/04 22:35:11 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AerosolCoeff_Define.f90,v $
! Revision 1.1  2005/02/04 22:35:11  paulv
! Initial checkin.
!
!
!
