!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Aerosol_Define
!
! PURPOSE:
!       Module defining the CRTM Aerosol structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Aerosol_Define
!
! MODULES:
!       Type_Kinds:               Module containing definitions for kinds
!                                 of variable types.
!
!       Error_Handler:            Module to define simple error codes and
!                                 handle error conditions
!                                 USEs: FILE_UTILITY module
!
! CONTAINS:
!       CRTM_Associated_Aerosol:  Function to test the association status
!                                 of the pointer members of an Aerosol
!                                 structure.
!
!       CRTM_Destroy_Aerosol:     Function to re-initialize an
!                                 CRTM_Aerosol structure.
!
!       CRTM_Allocate_Aerosol:    Function to allocate the pointer
!                                 members of an CRTM_Aerosol
!                                 structure.
!
!       CRTM_Assign_Aerosol:      Function to copy an CRTM_Aerosol
!                                 structure.
!
!       CRTM_WeightedSum_Aerosol: Function to perform a weighted sum of two
!                                 CRTM_Aerosol structures.
!
!       CRTM_Zero_Aerosol:        Subroutine to zero-out all members of a 
!                                 CRTM_Aerosol structure - both scalar and
!                                 pointer.
!
!
! DERIVED TYPES:
!       CRTM_Aerosol_type
!       --------------------
!         Definition of the CRTM scattering data structure.
!         Fields are:
!
!         n_Layers:                Number of atmospheric layers.
!                                  The "K" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Modes:                 Number of size distribution modes.
!                                  The "N" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Type:                    Flag value indicating the aerosol type.
!                                  See PUBLIC PARAMETERS for the valid
!                                  Aerosol types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Effective_Radius:        The effective radius of the aerosol particle
!                                  size distribution modes. The effective radius is
!                                  the ratio of the third moment of the size
!                                  distribution to the second moment.
!                                  UNITS:      microns (um)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (n_Layers x n_Modes)
!                                  ATTRIBUTES: POINTER
!
!         Effective_Variance:      The effective variance of the aerosol particle
!                                  size distribution modes. This variable
!                                  characterizes the width of the size distribution.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (n_Layers x n_Modes)
!                                  ATTRIBUTES: POINTER
!
!         Concentration:           The concentration of aerosols in the layer for
!                                  the various size distribution modes.
!                                  UNITS:      g.m^-2
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (n_Layers x n_Modes)
!                                  ATTRIBUTES: POINTER
!                                 
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_Aerosol_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, assign, and concatenate the structure using only
!       the routines in this module where possible to eliminate -- or
!       at least minimise -- the possibility of memory leakage since
!       most of the structure members are pointers.
!
! PUBLIC PARAMETERS:
!       1) The valid aerosol type values used in the Aerosol%Type field:
!
!              Aerosol Type      Parameter Name
!         --------------------------------------------------
!                 None           NO_AEROSOL   
!                 Dust           DUST_AEROSOL   
!                Sea salt        SEASALT_AEROSOL  
!           Dry organic carbon   DRY_ORGANIC_CARBON_AEROSOL
!           Wet organic carbon   WET_ORGANIC_CARBON_AEROSOL
!            Dry black carbon    DRY_BLACK_CARBON_AEROSOL
!            Wet black carbon    WET_BLACK_CARBON_AEROSOL
!                Sulfate         SULFATE_AEROSOL  
!
!       2) The number of valid aerosol types is specified by the 
!            N_VALID_AEROSOL_TYPES
!          parameter.
!
!       3) The character string array parameter
!            AEROSOL_TYPE_NAME
!          uses the above aerosol type definitions to provide a string value for
!          the type of aerosol. For example,
!            AEROSOL_TYPE_NAME( DRY_BLACK_CARBON_AEROSOL )
!          contains the string
!            'Dry black carbon'
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
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Feb-2005
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

MODULE CRTM_Aerosol_Define


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
  PUBLIC :: CRTM_Associated_Aerosol
  PUBLIC :: CRTM_Destroy_Aerosol
  PUBLIC :: CRTM_Allocate_Aerosol
  PUBLIC :: CRTM_Assign_Aerosol
  PUBLIC :: CRTM_WeightedSum_Aerosol
  PUBLIC :: CRTM_Zero_Aerosol


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Destroy_Aerosol
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Aerosol

  INTERFACE CRTM_Allocate_Aerosol
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank001
    MODULE PROCEDURE Allocate_Rank011
    MODULE PROCEDURE Allocate_Rank101
    MODULE PROCEDURE Allocate_Rank111
  END INTERFACE CRTM_Allocate_Aerosol

  INTERFACE CRTM_Assign_Aerosol
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Aerosol

  INTERFACE CRTM_WeightedSum_Aerosol
    MODULE PROCEDURE WeightedSum_Scalar
    MODULE PROCEDURE WeightedSum_Rank1
  END INTERFACE CRTM_WeightedSum_Aerosol

  INTERFACE CRTM_Zero_Aerosol
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Aerosol


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Aerosol_Define.f90,v 2.5 2005/08/17 18:00:03 paulv Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  
  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  INTEGER, PUBLIC, PARAMETER :: N_VALID_AEROSOL_TYPES = 7

  INTEGER, PUBLIC, PARAMETER ::                 NO_AEROSOL = 0
  INTEGER, PUBLIC, PARAMETER ::               DUST_AEROSOL = 1
  INTEGER, PUBLIC, PARAMETER ::            SEASALT_AEROSOL = 2
  INTEGER, PUBLIC, PARAMETER :: DRY_ORGANIC_CARBON_AEROSOL = 3
  INTEGER, PUBLIC, PARAMETER :: WET_ORGANIC_CARBON_AEROSOL = 4
  INTEGER, PUBLIC, PARAMETER ::   DRY_BLACK_CARBON_AEROSOL = 5
  INTEGER, PUBLIC, PARAMETER ::   WET_BLACK_CARBON_AEROSOL = 6
  INTEGER, PUBLIC, PARAMETER ::            SULFATE_AEROSOL = 7

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_AEROSOL_TYPES ) :: &
    AEROSOL_TYPE_NAME = (/ 'None              ', &
                           'Dust              ', &
                           'Sea salt          ', &
                           'Dry organic carbon', &
                           'Wet organic carbon', &
                           'Dry black carbon  ', &
                           'Wet black carbon  ', &
                           'Sulfate           ' /)


  ! ---------------------------------------
  ! Aerosol parameters data type definition
  ! ---------------------------------------

  TYPE, PUBLIC :: CRTM_Aerosol_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Layers  = 0  ! K dimension

    INTEGER :: Max_Modes = 0  ! Nm dimension
    INTEGER :: n_Modes   = 0  ! NmUse dimension

    ! -- Aerosol type
    INTEGER :: Type = NO_AEROSOL

    ! -- Particle size distribution parameters
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Effective_Radius   => NULL() ! K x Nm
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Effective_Variance => NULL() ! K x Nm

    ! -- Aerosol state variables
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Concentration => NULL()      ! K x Nm
  END TYPE CRTM_Aerosol_type


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
!       CRTM_Clear_Aerosol
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_Aerosol structure.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Aerosol( Aerosol ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Aerosol:  CRTM_Aerosol structure for which the scalar members have
!                 been cleared.
!                 UNITS:      N/A
!                 TYPE:       CRTM_Aerosol_type
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Aerosol( Aerosol )

    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: Aerosol

    Aerosol%n_Layers = 0

    Aerosol%Max_Modes = 0
    Aerosol%n_Modes   = 0

    Aerosol%Type = NO_AEROSOL

  END SUBROUTINE CRTM_Clear_Aerosol





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
!       CRTM_Associated_Aerosol
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_Aerosol structure.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Aerosol( Aerosol,            &  ! Input
!                                                     ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Aerosol:             CRTM_Aerosol structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Aerosol_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_Aerosol structure pointer members are associated.
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
!                            association status of the CRTM_Aerosol pointer members.
!                            .TRUE.  - if ALL the CRTM_Aerosol pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CRTM_Aerosol pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CRTM_Aerosol pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Aerosol( Aerosol,   & ! Input
                                    ANY_Test ) & ! Optional input
                                  RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Aerosol_type ), INTENT( IN ) :: Aerosol

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

      IF ( ASSOCIATED( Aerosol%Effective_Radius   ) .AND. &
           ASSOCIATED( Aerosol%Effective_Variance ) .AND. &
           ASSOCIATED( Aerosol%Concentration      )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Aerosol%Effective_Radius   ) .OR. &
           ASSOCIATED( Aerosol%Effective_Variance ) .OR. &
           ASSOCIATED( Aerosol%Concentration      )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF


  END FUNCTION CRTM_Associated_Aerosol





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_Aerosol
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_Aerosol data structure.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Aerosol( Aerosol,                  &  ! Output
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
!       Aerosol:      Re-initialized CRTM_Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
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
!       CRTM_Clear_Aerosol:       Subroutine to clear the scalar members
!                                 of a CRTM_Aerosol structure.
!
!       CRTM_Associated_Aerosol:  Function to test the association status
!                                 of the pointer members of a CRTM_Aerosol
!                                 structure.
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
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Aerosol,      &  ! Output
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
    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: Aerosol

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Aerosol(Scalar)'


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

    IF ( Clear ) CALL CRTM_Clear_Aerosol( Aerosol )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Effective_Radius profile
    IF ( ASSOCIATED( Aerosol%Effective_Radius ) ) THEN

      DEALLOCATE( Aerosol%Effective_Radius, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Aerosol Effective_Radius ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Effective_Variance profile
    IF ( ASSOCIATED( Aerosol%Effective_Variance ) ) THEN

      DEALLOCATE( Aerosol%Effective_Variance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Aerosol Effective_Variance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Concentration profile
    IF ( ASSOCIATED( Aerosol%Concentration ) ) THEN

      DEALLOCATE( Aerosol%Concentration, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Aerosol Concentration ", &
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

    Aerosol%n_Allocates = Aerosol%n_Allocates - 1

    IF ( Aerosol%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Aerosol%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Aerosol,      &  ! Output
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
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol

    ! -- Optional input
    INTEGER,                   OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Aerosol(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: n



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
    !#                       -- PERFORM REINITIALISATION --                     #
    !#--------------------------------------------------------------------------#

    DO n = 1, SIZE( Aerosol )

      Scalar_Status = Destroy_Scalar( Aerosol(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of Aerosol structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_Aerosol
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_Aerosol
!       data structure.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Aerosol( n_Layers,                 &  ! Input
!                                             n_Modes,                  &  ! Input
!                                             Aerosol,                  &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:   Number of atmospheric layers dimension.
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Aerosol argument
!                                 dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
!         n_Modes:    Number of size distribution modes dimension.
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Aerosol argument
!                                 dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
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
!       Aerosol:      CRTM_Aerosol structure with allocated pointer members. The
!                     following table shows the allowable dimension combinations
!                     for the calling routine, where N == number of aerosol types:
!
!                        Input       Input       Output
!                       n_Layers    n_Modes      Aerosol
!                       dimension   dimension   dimension
!                     -------------------------------------
!                        scalar      scalar       scalar
!                        scalar      scalar         N
!                        scalar        N            N
!                          N         scalar         N
!                          N           N            N
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar OR Rank-1
!                                 See table above.
!                     ATTRIBUTES: INTENT( IN OUT )
!
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
!                                     is not equal to one (1) upon exiting this
!                                     function. This value is incremented and
!                                     decremented for every structure allocation
!                                     and deallocation respectively.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Aerosol:  Function to test the association status
!                                 of the pointer members of a CRTM_Aerosol
!                                 structure.
!
!       CRTM_Destroy_Aerosol:     Function to re-initialize the scalar and
!                                 pointer members of a CRTM_Aerosol data
!                                 structure.
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
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            n_Modes,      &  ! Input
                            Aerosol,      &  ! Output
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
    INTEGER,                   INTENT( IN )     :: n_Layers
    INTEGER,                   INTENT( IN )     :: n_Modes

    ! -- Output
    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: Aerosol

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Scalar)'


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


    IF ( n_Modes < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Modes must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_Aerosol( Aerosol, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                           No_Clear = SET, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Aerosol pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Aerosol%Effective_Radius( n_Layers, n_Modes ), &
              Aerosol%Effective_Variance( n_Layers, n_Modes ), &
              Aerosol%Concentration( n_Layers, n_Modes ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating Aerosol data arrays. STAT = ", i5 )' ) &
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

    Aerosol%n_Layers  = n_Layers

    Aerosol%Max_Modes = n_Modes
    Aerosol%n_Modes   = n_Modes

    Aerosol%Effective_Radius   = ZERO
    Aerosol%Effective_Variance = ZERO
    Aerosol%Concentration      = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Aerosol%n_Allocates = Aerosol%n_Allocates + 1

    IF ( Aerosol%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Aerosol%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank001( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
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
    INTEGER,                                   INTENT( IN )     :: n_Layers
    INTEGER,                                   INTENT( IN )     :: n_Modes

    ! -- Output
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-001)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i



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
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, SIZE( Aerosol )

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Modes, &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank001


  FUNCTION Allocate_Rank011( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
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
    INTEGER,                                   INTENT( IN )     :: n_Layers
    INTEGER,                   DIMENSION( : ), INTENT( IN )     :: n_Modes

    ! -- Output
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-011)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n



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

    n = SIZE( Aerosol )

    IF ( SIZE( n_Modes ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Modes and CRTM_Aerosol arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       n_Modes(i), &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank011


  FUNCTION Allocate_Rank101( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
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
    INTEGER,                   DIMENSION( : ), INTENT( IN )     :: n_Layers
    INTEGER,                                   INTENT( IN )     :: n_Modes

    ! -- Output
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-101)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n



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

    n = SIZE( Aerosol )

    IF ( SIZE( n_Layers ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Aerosol arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       n_Modes, &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank101


  FUNCTION Allocate_Rank111( n_Layers,     &  ! Input
                             n_Modes,      &  ! Input
                             Aerosol,      &  ! Output
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
    INTEGER,                   DIMENSION( : ), INTENT( IN )     :: n_Layers
    INTEGER,                   DIMENSION( : ), INTENT( IN )     :: n_Modes

    ! -- Output
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Aerosol(Rank-111)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Scalar_Status
    INTEGER :: i, n



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

    n = SIZE( Aerosol )

    IF ( SIZE( n_Layers ) /= n .OR. &
         SIZE( n_Modes  ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers, n_Modes and CRTM_Aerosol '//&
                            'arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       n_Modes(i), &
                                       Aerosol(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank111





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_Aerosol
!
! PURPOSE:
!       Function to copy valid CRTM_Aerosol structures.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Aerosol( Aerosol_in,               &  ! Input
!                                           Aerosol_out,              &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Aerosol_in:      CRTM_Aerosol structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Scalar
!                                      OR
!                                    Rank1 array
!                        ATTRIBUTES: INTENT( IN )
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
!       Aerosol_out:     Copy of the input structure, CRTM_Aerosol_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Aerosol_type
!                        DIMENSION:  Same as Aerosol_in argument
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Aerosol:    Function to test the association status of the
!                                   pointer members of a CRTM_Aerosol structure.
!
!       CRTM_Destroy_Aerosol:       Function to re-initialize CRTM_Aerosol
!                                   structures.
!
!       CRTM_Allocate_Aerosol:      Function to allocate the pointer members of
!                                   the CRTM_Aerosol data structure.
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Aerosol_in,   &  ! Input
                          Aerosol_out,  &  ! Output
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
    TYPE( CRTM_Aerosol_type ), INTENT( IN )     :: Aerosol_in

    ! -- Output
    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: Aerosol_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Aerosol(Scalar)'



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

    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol_In ) ) THEN

      Error_Status = CRTM_Destroy_Aerosol( Aerosol_Out, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Aerosol pointer members.', &
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

    Error_Status = CRTM_Allocate_Aerosol( Aerosol_in%n_Layers, &
                                          Aerosol_in%Max_Modes, &
                                          Aerosol_out, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Aerosol arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    Aerosol_out%Type = Aerosol_in%Type


    ! -----------------
    ! Assign array data
    ! -----------------

    Aerosol_out%Effective_Radius   = Aerosol_in%Effective_Radius  
    Aerosol_out%Effective_Variance = Aerosol_in%Effective_Variance
    Aerosol_out%Concentration      = Aerosol_in%Concentration

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Aerosol_in,   &  ! Input
                         Aerosol_out,  &  ! Output
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
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN )     :: Aerosol_in

    ! -- Output
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol_out

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Aerosol(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, n



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
    !#                               -- TEST THE INPUT --                       #
    !#--------------------------------------------------------------------------#

    n = SIZE( Aerosol_in )

    IF ( SIZE( Aerosol_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Aerosol_in and Aerosol_out arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Scalar( Aerosol_in(i), &
                                     Aerosol_out(i), &
                                     Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Aerosol structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Assign_Rank1






!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_WeightedSum_Aerosol
!
! PURPOSE:
!       Function to perform a weighted sum of two valid CRTM_Aerosol
!       structures. The weighted summation performed is:
!         A = A + w1*B + w2
!       where A and B are the CRTM_Aerosol structures, and w1 and w2
!       are the weighting factors. Note that w2 is optional.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_WeightedSum_Aerosol( A,                        &  ! In/Output
!                                                B,                        &  ! Input
!                                                w1,                       &  ! Input
!                                                w2 = w2,                  &  ! Optional input
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       A:             Aerosol structure that is to be added to.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Scalar OR Rank-1
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       B:             Aerosol structure that is to be weighted and
!                      added to structure A.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Same as A
!                      ATTRIBUTES: INTENT( IN )
!
!       w1:            The first weighting factor used to multiply the
!                      contents of the input structure, B.
!                      UNITS:      N/A
!                      TYPE:       REAL( fp_kind )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       w2:            The second weighting factor used to multiply the
!                      contents of the input structure, B.
!                      UNITS:      N/A
!                      TYPE:       REAL( fp_kind )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       A:             Structure containing the weight sum result,
!                        A = A + w1*B + w2
!                      UNITS:      N/A
!                      TYPE:       CRTM_Aerosol_type
!                      DIMENSION:  Same as B
!                      ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure assignment was successful
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Associated_Aerosol:    Function to test the association status of
!                                   a CRTM_Aerosol structure.
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       The argument A is INTENT( IN OUT ) and is modified upon output.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION WeightedSum_Scalar( A,              &  ! Input/Output
                               B,              &  ! Input
                               w1,             &  ! Input
                               w2,             &  ! optional input
                               RCS_Id,         &  ! Revision control
                               Message_Log )   &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( CRTM_Aerosol_type ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Aerosol_type ), INTENT( IN )     :: B
    REAL( fp_kind ),           INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL, INTENT( IN )     :: w2

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Aerosol(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: w2_Local



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
    !#                          -- TEST THE STRUCTURES --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! ALL *input* pointers must be associated
    ! ---------------------------------------

    IF ( .NOT. CRTM_Associated_Aerosol( A ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument A appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( .NOT. CRTM_Associated_Aerosol( B ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument B appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! Check the dimensions
    ! --------------------

    IF ( A%n_Layers /= B%n_Layers .OR. &
         A%n_Modes  /= B%n_Modes       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure dimensions are different.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------
    ! Aerosol types must be the same
    ! ------------------------------

    IF ( A%Type /= B%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure Aerosol types are different.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- CHECK THE OPTIONAL WEIGHT ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    w2_Local = ZERO

    IF ( PRESENT( w2 ) ) w2_Local = w2



    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM THE WEIGHTED SUM --                      #
    !#--------------------------------------------------------------------------#

    A%Effective_Radius   = A%Effective_Radius   + (w1*B%Effective_Radius)   + w2_Local
    A%Effective_Variance = A%Effective_Variance + (w1*B%Effective_Variance) + w2_Local
    A%Concentration      = A%Concentration      + (w1*B%Concentration)      + w2_Local

  END FUNCTION WeightedSum_Scalar


  FUNCTION WeightedSum_Rank1( A,              &  ! Input/Output
                              B,              &  ! Input
                              w1,             &  ! Input
                              w2,             &  ! optional input
                              RCS_Id,         &  ! Revision control
                              Message_Log )   &  ! Error messaging
                            RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input/Output
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN )     :: B
    REAL( fp_kind ),                           INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL,                 INTENT( IN )     :: w2

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Aerosol(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, n



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
    !#                               -- TEST THE INPUT --                       #
    !#--------------------------------------------------------------------------#

    n = SIZE( A )

    IF ( SIZE( B )  /= n  ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input structure arguments have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = WeightedSum_Scalar( A(i), &
                                          B(i), &
                                          w1, &
                                          w2 = w2, &
                                          Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error computing weighted sum for element #", i5, &
                          &" of CRTM_Aerosol structure arrays." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION WeightedSum_Rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Zero_Aerosol
! 
! PURPOSE:
!       Subroutine to zero-out all members of a CRTM_Aerosol structure - both
!       scalar and pointer.
!
! CATEGORY:
!       CRTM : Atmosphere : Aerosol
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Aerosol( Aerosol )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Aerosol:      Zeroed out Aerosol structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Aerosol_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
!                     ATTRIBUTES: INTENT( IN OUT )
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
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Aerosol
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The dimension components of the structure are *NOT*
!         set to zero.
!
!       - The n_Modes component is set to the value of the Max_Modes
!         component.
!
!       - The aerosol type component is *NOT* reset.
!
! COMMENTS:
!       Note the INTENT on the output Aerosol argument is IN OUT rather than
!       just OUT. This is necessary because the argument must be defined upon
!       input.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Aug-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Aerosol )  ! Output
    TYPE( CRTM_Aerosol_type ),  INTENT( IN OUT ) :: Aerosol

    ! -- Reset the multi-dimensional scalar components
    Aerosol%n_Modes = Aerosol%Max_Modes

    ! -- Reset the array components
    Aerosol%Effective_Radius   = ZERO
    Aerosol%Effective_Variance = ZERO
    Aerosol%Concentration      = ZERO

  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Aerosol )  ! Output

    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), INTENT( IN OUT ) :: Aerosol
    INTEGER :: n

    DO n = 1, SIZE( Aerosol )
      CALL Zero_Scalar( Aerosol(n) )
    END DO

  END SUBROUTINE Zero_Rank1

END MODULE CRTM_Aerosol_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_Aerosol_Define.f90,v 2.5 2005/08/17 18:00:03 paulv Exp $
!
! $Date: 2005/08/17 18:00:03 $
!
! $Revision: 2.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Aerosol_Define.f90,v $
! Revision 2.5  2005/08/17 18:00:03  paulv
! - Modified the Zero() subroutines so that the dimensional structure members
!   are not set to zero. The only change to these occurs when there is a
!   corresponding Max dimension value--in this case the dimension value is
!   set to the maximum value.
! - Modified the Zero() subroutines so that the cloud and aerosol type flags
!   are not reset to zero. They retain their value.
!
! Revision 2.4  2005/08/17 17:10:30  paulv
! - Added structure zero subroutines.
!
! Revision 2.3  2005/06/15 23:12:29  paulv
! - Added WeightedSum() functions.
!
! Revision 2.2  2005/03/24 15:13:22  paulv
! - Updated header documentation.
!
! Revision 2.1  2005/02/25 17:35:52  paulv
! - Added Allocate_Rank001() specific function to generic allocate module
!   procedure list.
!
! Revision 2.0  2005/02/24 18:51:29  paulv
! - Category change.
!   This source is no longer part of the internal interface, but is now
!   part of the user interface for inputing aerosol properties into the
!   CRTM.
!
! Revision 1.2  2005/02/18 20:03:41  paulv
! - Changed Aerosol structure dimension from n_Channels to n_Layers (duh!).
!
! Revision 1.1  2005/02/16 15:26:24  paulv
! Initial checkin. Definition module structure just a placeholder.
!
!
!


