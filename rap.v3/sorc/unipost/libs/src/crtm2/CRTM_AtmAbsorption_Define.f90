!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmAbsorption_Define
!
! PURPOSE:
!       Module defining the CRTM AtmAbsorption structure and containing
!       routines to manipulate it.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmAbsorption_Define
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds
!                                of variable types.
!
!       Error_Handler:           Module to define simple error codes and
!                                handle error conditions
!                                USEs: FILE_UTILITY module
!
! CONTAINS:
!       CRTM_Associated_AtmAbsorption:  Function to test the association status
!                                       of the pointer members of an AtmAbsorption
!                                       structure.
!
!       CRTM_Destroy_AtmAbsorption:     Function to re-initialize a
!                                       CRTM_AtmAbsorption structure.
!
!       CRTM_Allocate_AtmAbsorption:    Function to allocate the pointer
!                                       members of a CRTM_AtmAbsorption
!                                       structure.
!
!       CRTM_Assign_AtmAbsorption:      Function to copy a valid 
!                                       CRTM_AtmAbsorption structure.
!
! DERIVED TYPES:
!       CRTM_AtmAbsorption_type
!       -----------------------
!         Definition of the CRTM gaseous absorption data structure.
!         Fields are:
!
!         n_Layers:         Number of atmospheric layers.
!                           "K" dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!         n_Predictors:     Number of absorption predictors.
!                           "I" dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!         n_Absorbers:      Number of absorbing species.
!                           "J" dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!         IntAbsorber:      The integrated absorber amounts for
!                           the absorbinbg species.
!                           ** NOTE: THIS IS CONSIDERED AN ALGORITHM   **
!                           **       SPECIFIC MEMBER OF THIS STRUCTURE **
!                           UNITS:      Absorber dependent
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-2, (0:K x J)
!
!         Predictor:        The predictor profiles for the absorption
!                           model.
!                           ** NOTE: THIS IS CONSIDERED AN ALGORITHM   **
!                           **       SPECIFIC MEMBER OF THIS STRUCTURE **
!                           UNITS:      Predictor dependent
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-2, (I x K)
!
!         Optical_Depth:    Optical depth for the layer due to the 
!                           gaseous absorption.
!                           ** NOTE: THIS IS A MANDATORY MEMBER **
!                           **       OF THIS STRUCTURE          **
!                           UNITS:      N/A
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-1, (K)
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_AtmAbsorption_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, and assign the structure using only the routines
!       in this module where possible to eliminate -- or at least
!       minimise -- the possibility of memory leakage since most
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
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
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

MODULE CRTM_AtmAbsorption_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Definition functions
  PUBLIC :: CRTM_Associated_AtmAbsorption
  PUBLIC :: CRTM_Destroy_AtmAbsorption
  PUBLIC :: CRTM_Allocate_AtmAbsorption
  PUBLIC :: CRTM_Assign_AtmAbsorption


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmAbsorption_Define.f90,v 1.9 2005/08/16 16:20:38 qliu Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! ----------------------------------
  ! AtmAbsorption data type definition
  ! ----------------------------------

  TYPE, PUBLIC :: CRTM_AtmAbsorption_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Layers     = 0  ! K dimension
    INTEGER :: n_Predictors = 0  ! I dimension
    INTEGER :: n_Absorbers  = 0  ! J dimension

    ! -- Algorithm specific members
    REAL( fp_kind )                             :: Secant_Sensor_Zenith = ZERO
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: IntAbsorber => NULL()   ! 0:K x J
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Predictor   => NULL()   ! I x K

    ! -- Mandatory members
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Optical_Depth => NULL() ! K
  END TYPE CRTM_AtmAbsorption_type


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
!       CRTM_Clear_AtmAbsorption
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_AtmAbsorption structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_AtmAbsorption( AtmAbsorption ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption:  CRTM_AtmAbsorption structure for which the scalar
!                       members have been cleared.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmAbsorption_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_AtmAbsorption( AtmAbsorption )

    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption

    AtmAbsorption%n_Layers     = 0
    AtmAbsorption%n_Predictors = 0
    AtmAbsorption%n_Absorbers  = 0

  END SUBROUTINE CRTM_Clear_AtmAbsorption





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
!       CRTM_Associated_AtmAbsorption
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       AtmAbsorption structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_AtmAbsorption( AtmAbsorption,      &  ! Input
!                                                           ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       AtmAbsorption:       CRTM_AtmAbsorption structure which is to have its
!                            pointer member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_AtmAbsorption_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_AtmAbsorption structure pointer members are
!                            associated.
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
!       Association_Status:  The return value is a logical value indicating
!                            the association status of the CRTM_AtmAbsorption
!                            pointer members.
!                            .TRUE.  - if ALL the CRTM_AtmAbsorption pointer
!                                      members are associated, or if the
!                                      ANY_Test argument is set and ANY of the
!                                      CRTM_AtmAbsorption pointer members are
!                                      associated.
!                            .FALSE. - some or all of the CRTM_AtmAbsorption
!                                      pointer members are NOT associated.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_AtmAbsorption( AtmAbsorption, & ! Input
                                          ANY_Test )     & ! Optional input
                                        RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN ) :: AtmAbsorption

    ! -- Optional input
    INTEGER,               OPTIONAL, INTENT( IN ) :: ANY_Test


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
    !#                                                                          #
    !#  Note that there is no difference betweent he ALL_Test and ANY_Test, but #
    !#  but the hooks are kept here for future expansion of the AtmAbsorption   #
    !#  structure.                                                              #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( AtmAbsorption%IntAbsorber   ) .AND. &
           ASSOCIATED( AtmAbsorption%Predictor     ) .AND. &
           ASSOCIATED( AtmAbsorption%Optical_Depth )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( AtmAbsorption%IntAbsorber   ) .OR. &
           ASSOCIATED( AtmAbsorption%Predictor     ) .OR. &
           ASSOCIATED( AtmAbsorption%Optical_Depth )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_AtmAbsorption





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_AtmAbsorption
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_AtmAbsorption data structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption,            &  ! Output
!                                                  RCS_Id = RCS_Id,          &  ! Revision control
!                                                  Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption:  Re-initialized CRTM_AtmAbsorption structure.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmAbsorption_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Clear_AtmAbsorption:       Subroutine to clear the scalar members
!                                       of a CRTM_AtmAbsorption structure.
!
!       CRTM_Associated_AtmAbsorption:  Function to test the association status
!                                       of the pointer members of a
!                                       CRTM_AtmAbsorption structure.
!
!       Display_Message:                Subroutine to output messages
!                                       SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_AtmAbsorption( AtmAbsorption, &  ! Output
                                       No_Clear,      &  ! Optional input
                                       RCS_Id,        &  ! Revision control
                                       Message_Log )  &  ! Error messaging
                                     RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption

    ! -- Optional input
    INTEGER,               OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_AtmAbsorption'


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

    IF ( Clear ) CALL CRTM_Clear_AtmAbsorption( AtmAbsorption )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_AtmAbsorption( AtmAbsorption ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the CRTM_AtmAbsorption IntAbsorber member
    IF ( ASSOCIATED( AtmAbsorption%IntAbsorber ) ) THEN

      DEALLOCATE( AtmAbsorption%IntAbsorber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmAbsorption IntAbsorber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the CRTM_AtmAbsorption X member
    IF ( ASSOCIATED( AtmAbsorption%Predictor ) ) THEN

      DEALLOCATE( AtmAbsorption%Predictor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmAbsorption X ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the CRTM_AtmAbsorption Optical_Depth profile
    IF ( ASSOCIATED( AtmAbsorption%Optical_Depth ) ) THEN

      DEALLOCATE( AtmAbsorption%Optical_Depth, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmAbsorption Optical_Depth ", &
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

    AtmAbsorption%n_Allocates = AtmAbsorption%n_Allocates - 1

    IF ( AtmAbsorption%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      AtmAbsorption%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_AtmAbsorption





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_AtmAbsorption
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_AtmAbsorption
!       data structure.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_AtmAbsorption( n_Layers,                 &  ! Input
!                                                   n_Predictors,             &  ! Input
!                                                   n_Absorbers,              &  ! Input
!                                                   AtmAbsorption,            &  ! Output
!                                                   RCS_Id = RCS_Id,          &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:          Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         n_Predictors:      Number of absorption predictors.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         n_Absorbers:       Number of atmospheric absorbers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption:       CRTM_AtmAbsorption structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_AtmAbsorption_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the ERROR_HANDLER module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_AtmAbsorption: Function to test the association status
!                                      of the pointer members of a CRTM_AtmAbsorption
!                                      structure.
!
!       Display_Message:               Subroutine to output messages
!                                      SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_AtmAbsorption( n_Layers,         &  ! Input
                                        n_Predictors,     &  ! Input
                                        n_Absorbers,      &  ! Input
                                        AtmAbsorption,    &  ! Output
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
    INTEGER,                         INTENT( IN )     :: n_Layers
    INTEGER,                         INTENT( IN )     :: n_Predictors
    INTEGER,                         INTENT( IN )     :: n_Absorbers

    ! -- Output
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_AtmAbsorption'


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

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Predictors < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Predictors must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Absorbers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Absorbers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_AtmAbsorption( AtmAbsorption, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption, &
                                                 No_Clear = SET, &
                                                 Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_AtmAbsorption pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( AtmAbsorption%IntAbsorber( 0:n_Layers, n_Absorbers ), &
              AtmAbsorption%Predictor( n_Predictors, n_Layers ), &
              AtmAbsorption%Optical_Depth( n_Layers ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AtmAbsorption data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSIONS AND INITALISE ARRAYS --             #
    !#--------------------------------------------------------------------------#

    AtmAbsorption%n_Layers     = n_Layers
    AtmAbsorption%n_Predictors = n_Predictors
    AtmAbsorption%n_Absorbers  = n_Absorbers

    AtmAbsorption%IntAbsorber   = ZERO
    AtmAbsorption%Predictor     = ZERO
    AtmAbsorption%Optical_Depth = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    AtmAbsorption%n_Allocates = AtmAbsorption%n_Allocates + 1

    IF ( AtmAbsorption%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      AtmAbsorption%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_AtmAbsorption





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_AtmAbsorption
!
! PURPOSE:
!       Function to copy valid CRTM_AtmAbsorption structures.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_AtmAbsorption( AtmAbsorption_in,         &  ! Input
!                                                 AtmAbsorption_out,        &  ! Output
!                                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmAbsorption_in:  CRTM_AtmAbsorption structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption_out: Copy of the input structure, CRTM_AtmAbsorption_in.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
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
!       CRTM_Associated_AtmAbsorption: Function to test the association status
!                                      of the pointer members of a
!                                      CRTM_AtmAbsorption structure.
!
!       CRTM_Destroy_AtmAbsorption:    Function to re-initialize CRTM_AtmAbsorption
!                                      structures.
!
!       CRTM_Allocate_AtmAbsorption:   Function to allocate the pointer members
!                                      of the CRTM_AtmAbsorption data structure.
!
!       Display_Message:               Subroutine to output messages
!                                      SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AtmScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_AtmAbsorption( AtmAbsorption_in,  &  ! Input
                                      AtmAbsorption_out, &  ! Output
                                      RCS_Id,            &  ! Revision control
                                      Message_Log )      &  ! Error messaging
                                    RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption_in

    ! -- Output
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_out

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_AtmAbsorption'



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
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------
    ! ALL *input* pointers must be associated.
    !
    ! If this test succeeds, then some or all of the
    ! input pointers are NOT associated, so destroy
    ! the output structure and return.
    ! ----------------------------------------------

    IF ( .NOT. CRTM_Associated_AtmAbsorption( AtmAbsorption_In ) ) THEN

      Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Out, &
                                                 Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_AtmAbsorption pointer members.', &
                              Error_Status,    &
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

    Error_Status = CRTM_Allocate_AtmAbsorption( AtmAbsorption_in%n_Layers, &
                                                AtmAbsorption_in%n_Predictors, &
                                                AtmAbsorption_in%n_Absorbers, &
                                                AtmAbsorption_out, &
                                                Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AtmAbsorption arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Assign array data
    ! -----------------

    AtmAbsorption_out%IntAbsorber   = AtmAbsorption_in%IntAbsorber
    AtmAbsorption_out%Predictor     = AtmAbsorption_in%Predictor
    AtmAbsorption_out%Optical_Depth = AtmAbsorption_in%Optical_Depth

  END FUNCTION CRTM_Assign_AtmAbsorption

END MODULE CRTM_AtmAbsorption_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_AtmAbsorption_Define.f90,v 1.9 2005/08/16 16:20:38 qliu Exp $
!
! $Date: 2005/08/16 16:20:38 $
!
! $Revision: 1.9 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmAbsorption_Define.f90,v $
! Revision 1.9  2005/08/16 16:20:38  qliu
! - Added secant sensor angle to structure definition for scaling of slant
!   path optical depths in Compute_AtmAbsorption() functions.
!
! Revision 1.8  2005/01/31 21:34:57  paulv
! - Arrays in the Allocate() function are now initialised to ZERO rather than
!   FP_INVALID.
!
! Revision 1.7  2004/11/03 20:26:18  paulv
! - Added FP_INVALID parameter for initialising floating point variables.
! - Altered the way the Assign() function handles unassociated input. Previously
!   an error was issued:
!     IF ( .NOT. CRTM_Associated_AtmAbsorption( AtmAbsorption_In ) ) THEN
!       Error_Status = FAILURE
!       RETURN
!     END IF
!   Now, rather than returning an error, the output structure is destroyed
!   (in case it is defined upon input), and a successful status is returned,
!     IF ( .NOT. CRTM_Associated_AtmAbsorption( AtmAbsorption_In ) ) THEN
!       Error_Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption_Out, &
!                                                  Message_Log = Message_Log )
!       RETURN
!     END IF
!
! Revision 1.6  2004/11/03 16:35:30  paulv
! - Upgraded to Fortran-95
! - Structure initialisation is now performed in the structure type
!   declaration. Removed Init() subroutine.
! - Made Associated() function PUBLIC.
! - Intent of output structures in the Clear(), Allocate(), and Assign()
!   routines changed from (OUT) to (IN OUT) to prevent memory leaks.
! - Allocate() function now destroys the output structure if any pointer
!   members are defined upon input.
! - Updated documentation.
!
! Revision 1.5  2004/08/06 19:00:33  paulv
! - Updated header documentation.
!
! Revision 1.4  2004/08/06 17:15:40  paulv
! - Removed intrinsic declarations.
! - Updated header documentation.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/06/23 14:33:17  paulv
! - Updated documentation headers.
!
! Revision 1.1  2004/06/18 20:15:02  paulv
! Initial checkin.
!
!
!
