!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmScatter_Define
!
! PURPOSE:
!       Module defining the CRTM AtmScatter structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       CRTM : Scattering
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmScatter_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       CRTM_Associated_AtmScatter:  Function to test the association status
!                                    of the pointer members of an AtmScatter
!                                    structure.
!
!       CRTM_Destroy_AtmScatter:     Function to re-initialize an
!                                    CRTM_AtmScatter structure.
!
!       CRTM_Allocate_AtmScatter:    Function to allocate the pointer
!                                    members of an CRTM_AtmScatter
!                                    structure.
!
!       CRTM_Assign_AtmScatter:      Function to copy an CRTM_AtmScatter
!                                    structure.
!
!
! DERIVED TYPES:
!       CRTM_AtmScatter_type
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
!         Max_Legendre_Terms:      The maximum number of Legendre polynomial
!                                  terms defined for the AtmScatter structure.
!                                  The "Ic" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Legendre_Terms:        The number of Legendre polynomial terms used to
!                                  describe the scattering phase function. The
!                                  phase function may be written in the form
!
!                                                        __ 2N-1
!                                                       \
!                                    P(od,cos[Theta]) ~  >  (2l+1).g(l,od).P(l,cos[Theta])
!                                                       /__
!                                                          l=0
!
!                                  where od      = layer optical depth,
!                                        g(l,od) = the Legendre expansion coefficients
!
!                                                   /\+1 
!                                               1   |
!                                    g(l,od) = ---  |  P(od,cos[Theta]).P(l,cos[Theta]) dcos[Theta]
!                                               2   |
!                                                  \/ -1
!
!                                  The default is to set
!                                    n_Legendre_Terms = Max_Legendre_Terms
!                                  The "IcUse" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!
!         Max_Phase_Elements:      The maximum number of phase elements defined
!                                  for the AtmScatter structure.
!                                  The "Ip" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Phase_Elements:        The number of unique, non-zero elements in the
!                                  scattering matrix, *P*. The scattering matrix is a
!                                  4x4 matrix that connects the Stokes vectors of the
!                                  incident to the scattered radiation. With no
!                                  assumptions about shape or position of the scatter,
!                                  the phase matrix is
!
!                                          [ P(11)  P(12)  P(13)  P(14) ]
!                                          [                            ]
!                                          [ P(21)  P(22)  P(23)  P(24) ]
!                                    *P* = [                            ]
!                                          [ P(31)  P(32)  P(33)  P(34) ]
!                                          [                            ]
!                                          [ P(41)  P(42)  P(43)  P(44) ]
!
!
!                                  with 16 unique, non-zero elements. For spherical
!                                  particles, *P* becomes,
!
!
!                                          [ P(11)  P(12)    0      0   ]
!                                          [                            ]
!                                          [ P(12)  P(11)    0      0   ]
!                                    *P* = [                            ]
!                                          [   0      0    P(33)  P(34) ]
!                                          [                            ]
!                                          [   0      0   -P(34)  P(33) ]
!
!                                  and the number of unique, non-zero elements reduces
!                                  to 4. The default is to set
!                                    n_Phase_Elements = Max_Phase_Elements
!                                  The "IpUse" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!
!         Phase_Coefficient:       The coefficients of the Legendre polynomial
!                                  used to specify the scattering phase function.
!                                  ** NOTE: THIS IS CONSIDERED AN ALGORITHM   **
!                                  **       SPECIFIC MEMBER OF THIS STRUCTURE **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-3 ( K x Ic x Ip )
!
!         Optical_Depth:           Optical depth for the layer due to the 
!                                  prescence of scatters where,
!                                    od = k(e) * dA 
!                                  and
!                                    k(e) = k(s) + k(a)
!                                  where k(e) = total extinction coefficient
!                                        k(s) = scattering coefficient
!                                        k(a) = absorption coefficient
!                                        dA   = absorber amount
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 ( K )
!
!
!         Single_Scatter_Albedo:   The ratio of the scattering and extinction
!                                  coefficient,
!                                    ~    k(s)
!                                    w = ------
!                                         k(e)
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 ( K )
!
!         Asymmetry_Factor:        The first moment of the phase function
!                                  representing the degree of asymmetry of
!                                  the angular scattering, g(1,od)
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 ( K )
!                                 
!         Delta_Truncation:        The scaling factor associated with the
!                                  truncation of the forward scattering peak
!                                  in the scattering phase function. Value
!                                  ranges from 0 (no truncation, all Legendre
!                                  terms used) to 1 (full truncation, no scattering
!                                  only absorption.)
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 ( K )
!                                 
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_AtmScatter_type is PUBLIC and its members are
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

MODULE CRTM_AtmScatter_Define


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
  PUBLIC :: CRTM_Associated_AtmScatter
  PUBLIC :: CRTM_Destroy_AtmScatter
  PUBLIC :: CRTM_Allocate_AtmScatter
  PUBLIC :: CRTM_Assign_AtmScatter


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmScatter_Define.f90,v 1.15.2.1 2005/08/16 17:45:33 qliu Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  
  ! ------------------------------------------
  ! Scattering parameters data type definition
  ! ------------------------------------------

  TYPE, PUBLIC :: CRTM_AtmScatter_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Layers           = 0  ! K dimension

    INTEGER :: Max_Legendre_Terms = 0  ! Ic dimension
    INTEGER :: n_Legendre_Terms   = 0  ! IcUse dimension

    INTEGER :: Max_Phase_Elements = 0  ! Ip dimension
    INTEGER :: n_Phase_Elements   = 0  ! IpUse dimension
    ! -- Algorithm specific members
    REAL( fp_kind ), DIMENSION( : ,:, :), POINTER :: Phase_Coefficient => NULL()  ! 0:Ic x Ip x K
    INTEGER :: Offset_LegTerm = 0      ! start position of array for Legendre coefficients 

    ! -- Mandatory members
    REAL( fp_kind ), DIMENSION( : ),      POINTER :: Optical_Depth         => NULL() ! K
    REAL( fp_kind ), DIMENSION( : ),      POINTER :: Single_Scatter_Albedo => NULL() ! K
    REAL( fp_kind ), DIMENSION( : ),      POINTER :: Asymmetry_Factor      => NULL() ! K
    REAL( fp_kind ), DIMENSION( : ),      POINTER :: Delta_Truncation      => NULL() ! K

  END TYPE CRTM_AtmScatter_type


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
!       CRTM_Clear_AtmScatter
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_AtmScatter structure.
!
! CATEGORY:
!       CRTM : Scattering
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_AtmScatter( AtmScatter ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmScatter:  CRTM_AtmScatter structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_AtmScatter_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN OUT )
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
!       Note the INTENT on the output AtmScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_AtmScatter( AtmScatter )

    TYPE( CRTM_AtmScatter_type ), INTENT( IN OUT ) :: AtmScatter

    AtmScatter%n_Layers           = 0

    AtmScatter%Max_Legendre_Terms = 0
    AtmScatter%n_Legendre_Terms   = 0

    AtmScatter%Max_Phase_Elements = 0
    AtmScatter%n_Phase_Elements   = 0

  END SUBROUTINE CRTM_Clear_AtmScatter





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
!       CRTM_Associated_AtmScatter
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_AtmScatter structure.
!
! CATEGORY:
!       CRTM : Scattering
!
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_AtmScatter( AtmScatter,         &  ! Input
!                                                        ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       AtmScatter:          CRTM_AtmScatter structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_AtmScatter_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_AtmScatter structure pointer members are associated.
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
!                            association status of the CRTM_AtmScatter pointer members.
!                            .TRUE.  - if ALL the CRTM_AtmScatter pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CRTM_AtmScatter pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CRTM_AtmScatter pointer
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

  FUNCTION CRTM_Associated_AtmScatter( AtmScatter, & ! Input
                                       ANY_Test )  & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_AtmScatter_type ), INTENT( IN ) :: AtmScatter

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( AtmScatter%Optical_Depth         ) .AND. &
           ASSOCIATED( AtmScatter%Single_Scatter_Albedo ) .AND. &
           ASSOCIATED( AtmScatter%Asymmetry_Factor      ) .AND. &
           ASSOCIATED( AtmScatter%Delta_Truncation      ) .AND. &
           ASSOCIATED( AtmScatter%Phase_Coefficient     )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( AtmScatter%Optical_Depth         ) .OR. &
           ASSOCIATED( AtmScatter%Single_Scatter_Albedo ) .OR. &
           ASSOCIATED( AtmScatter%Asymmetry_Factor      ) .OR. &
           ASSOCIATED( AtmScatter%Delta_Truncation      ) .OR. &
           ASSOCIATED( AtmScatter%Phase_Coefficient     )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_AtmScatter





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_AtmScatter
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_AtmScatter data structure.
!
! CATEGORY:
!       CRTM : Scattering
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_AtmScatter( AtmScatter,               &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
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
!       AtmScatter:   Re-initialized CRTM_AtmScatter structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_AtmScatter_type
!                     DIMENSION:  Scalar OR Rank-1 array
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
!       CRTM_Clear_AtmScatter:       Subroutine to clear the scalar members
!                                    of a CRTM_AtmScatter structure.
!
!       CRTM_Associated_AtmScatter:  Function to test the association status
!                                    of the pointer members of a CRTM_AtmScatter
!                                    structure.
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_AtmScatter( AtmScatter,   &  ! Output
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
    TYPE( CRTM_AtmScatter_type ), INTENT( IN OUT ) :: AtmScatter

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_AtmScatter'


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

    IF ( Clear ) CALL CRTM_Clear_AtmScatter( AtmScatter )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_AtmScatter( AtmScatter ) ) RETURN


    ! ----------------------------------------
    ! Deallocate the MANDATORY pointer members
    ! ----------------------------------------

    ! -- Deallocate the CRTM_AtmScatter Optical_Depth
    IF ( ASSOCIATED( AtmScatter%Optical_Depth ) ) THEN

      DEALLOCATE( AtmScatter%Optical_Depth, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmScatter Optical_Depth ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_AtmScatter Single_Scatter_Albedo
    IF ( ASSOCIATED( AtmScatter%Single_Scatter_Albedo ) ) THEN

      DEALLOCATE( AtmScatter%Single_Scatter_Albedo, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmScatter Single_Scatter_Albedo ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_AtmScatter Asymmetry_Factor
    IF ( ASSOCIATED( AtmScatter%Asymmetry_Factor ) ) THEN

      DEALLOCATE( AtmScatter%Asymmetry_Factor, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmScatter Asymmetry_Factor ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_AtmScatter Delta_Truncation
    IF ( ASSOCIATED( AtmScatter%Delta_Truncation ) ) THEN

      DEALLOCATE( AtmScatter%Delta_Truncation, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmScatter Delta_Truncation ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -------------------------------------------------
    ! Deallocate the ALGORITHM-SPECIFIC pointer members
    ! -------------------------------------------------

    ! -- Deallocate the CRTM_AtmScatter Phase_Coefficient
    IF ( ASSOCIATED( AtmScatter%Phase_Coefficient ) ) THEN

      DEALLOCATE( AtmScatter%Phase_Coefficient, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_AtmScatter Phase_Coefficient ", &
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

    AtmScatter%n_Allocates = AtmScatter%n_Allocates - 1

    IF ( AtmScatter%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      AtmScatter%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_AtmScatter





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_AtmScatter
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_AtmScatter
!       data structure.
!
! CATEGORY:
!       CRTM : Scattering
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_AtmScatter( n_Layers,                 &  ! Input
!                                                n_Legendre_Terms,         &  ! Input
!                                                n_Phase_Elements,         &  ! Input
!                                                AtmScatter,               &  ! Output
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:          Number of atmospheric layers dimension.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         n_Legendre_Terms:  The number of Legendre polynomial terms dimension.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         n_Phase_Elements:  The number of phase elements dimension.
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
!                            UNITS:      None
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmScatter:          CRTM_AtmScatter structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_AtmScatter_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      None
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
!       CRTM_Associated_AtmScatter:  Function to test the association status
!                                    of the pointer members of a CRTM_AtmScatter
!                                    structure.
!
!       CRTM_Destroy_AtmScatter:     Function to re-initialize the scalar and
!                                    pointer members of a CRTM_AtmScatter data
!                                    structure.
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_AtmScatter( n_Layers,         &  ! Input
                                     n_Legendre_Terms, &  ! Input
                                     n_Phase_Elements, &  ! Input
                                     AtmScatter,       &  ! Output
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
    INTEGER,                      INTENT( IN )     :: n_Layers         
    INTEGER,                      INTENT( IN )     :: n_Legendre_Terms 
    INTEGER,                      INTENT( IN )     :: n_Phase_Elements 

    ! -- Output
    TYPE( CRTM_AtmScatter_type ), INTENT( IN OUT ) :: AtmScatter

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_AtmScatter'


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

    IF ( n_Legendre_Terms < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Legendre_Terms must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Phase_Elements < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Phase_Elements must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_AtmScatter( AtmScatter, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_AtmScatter( AtmScatter, &
                                              No_Clear = SET, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_AtmScatter pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( &
              ! -- MANDATORY structure members
              AtmScatter%Optical_Depth( n_Layers ),         &
              AtmScatter%Single_Scatter_Albedo( n_Layers ), &
              AtmScatter%Asymmetry_Factor( n_Layers ),      &
              AtmScatter%Delta_Truncation( n_Layers ),      &
              ! -- ALGORITHM-SPECIFIC structure members
              AtmScatter%Phase_Coefficient( 0:n_Legendre_Terms,         &
                                              n_Phase_Elements, &
                                              n_Layers  ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating AtmScatter data arrays. STAT = ", i5 )' ) &
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

    AtmScatter%n_Layers           = n_Layers

    AtmScatter%Max_Legendre_Terms = n_Legendre_Terms
    AtmScatter%n_Legendre_Terms   = n_Legendre_Terms

    AtmScatter%Max_Phase_Elements = n_Phase_Elements
    AtmScatter%n_Phase_Elements   = n_Phase_Elements

    ! -- MANDATORY structure members
    AtmScatter%Optical_Depth         = ZERO
    AtmScatter%Single_Scatter_Albedo = ZERO
    AtmScatter%Asymmetry_Factor      = ZERO
    AtmScatter%Delta_Truncation      = ZERO

    ! -- ALGORITHM-SPECIFIC structure members
    AtmScatter%Phase_Coefficient     = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    AtmScatter%n_Allocates = AtmScatter%n_Allocates + 1

    IF ( AtmScatter%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      AtmScatter%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_AtmScatter





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_AtmScatter
!
! PURPOSE:
!       Function to copy valid CRTM_AtmScatter structures.
!
! CATEGORY:
!       CRTM : Scattering
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_AtmScatter( AtmScatter_in,            &  ! Input
!                                              AtmScatter_out,           &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       AtmScatter_in:   CRTM_AtmScatter structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmScatter_type
!                        DIMENSION:  Scalar
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
!       AtmScatter_out:  Copy of the input structure, CRTM_AtmScatter_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_AtmScatter_type
!                        DIMENSION:  Scalar
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
!       CRTM_Associated_AtmScatter: Function to test the association status of the
!                                   pointer members of a CRTM_AtmScatter structure.
!
!       CRTM_Destroy_AtmScatter:    Function to re-initialize CRTM_AtmScatter
!                                   structures.
!
!       CRTM_Allocate_AtmScatter:   Function to allocate the pointer members of
!                                   the CRTM_AtmScatter data structure.
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
!       Note the INTENT on the output AtmScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_AtmScatter( AtmScatter_in,  &  ! Input
                                   AtmScatter_out, &  ! Output
                                   RCS_Id,         &  ! Revision control
                                   Message_Log )   &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_AtmScatter_type ), INTENT( IN )     :: AtmScatter_in

    ! -- Output
    TYPE( CRTM_AtmScatter_type ), INTENT( IN OUT ) :: AtmScatter_out

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_AtmScatter'



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

    IF ( .NOT. CRTM_Associated_AtmScatter( AtmScatter_In ) ) THEN

      Error_Status = CRTM_Destroy_AtmScatter( AtmScatter_Out, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_AtmScatter pointer members.', &
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

    Error_Status = CRTM_Allocate_AtmScatter( AtmScatter_in%n_Layers, &
                                             AtmScatter_in%Max_Legendre_Terms, &
                                             AtmScatter_in%Max_Phase_Elements, &
                                             AtmScatter_out, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output AtmScatter arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------------
    ! Assign the used-dimension scalar members
    ! ----------------------------------------

    AtmScatter_out%n_Legendre_Terms = AtmScatter_in%n_Legendre_Terms
    AtmScatter_out%n_Phase_Elements = AtmScatter_in%n_Phase_Elements


    ! -----------------
    ! Assign array data
    ! -----------------

    ! -- MANDATORY structure members
    AtmScatter_out%Optical_Depth         = AtmScatter_in%Optical_Depth
    AtmScatter_out%Single_Scatter_Albedo = AtmScatter_in%Single_Scatter_Albedo 
    AtmScatter_out%Asymmetry_Factor      = AtmScatter_in%Asymmetry_Factor      
    AtmScatter_out%Delta_Truncation      = AtmScatter_in%Delta_Truncation     

    ! -- ALGORITHM-SPECIFIC structure members
    AtmScatter_out%Phase_Coefficient     = AtmScatter_in%Phase_Coefficient     

  END FUNCTION CRTM_Assign_AtmScatter

END MODULE CRTM_AtmScatter_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_AtmScatter_Define.f90,v 1.15.2.1 2005/08/16 17:45:33 qliu Exp $
!
! $Date: 2005/08/16 17:45:33 $
!
! $Revision: 1.15.2.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmScatter_Define.f90,v $
! Revision 1.15.2.1  2005/08/16 17:45:33  qliu
! - Reorder phase coefficient dimensions from
!    K x Ic x Ip
!   to
!    0:Ic x Ip x K
!   Having the layer dimension last is more memory efficient.
!
! Revision 1.15  2005/03/23 15:45:31  paulv
! - Corrected definition of optical depth in header documentation.
!
! Revision 1.14  2005/02/28 21:52:06  paulv
! - Updated header docs describing the Delta_Truncation member.
!
! Revision 1.13  2005/02/28 21:12:59  paulv
! - Added Delta_Truncation component to structure.
!
! Revision 1.12  2005/02/16 15:33:53  paulv
! - Added "Max" values for the Legendre term and Phase array dimensions to
!   allow already allocated structures to be reused.
!
! Revision 1.11  2004/11/03 20:28:04  paulv
! - Added FP_INVALID parameter for initialising floating point variables.
! - Altered the way the Assign() function handles unassociated input. Previously
!   an error was issued:
!     IF ( .NOT. CRTM_Associated_AtmScatter( AtmScatter_In ) ) THEN
!       Error_Status = FAILURE
!       RETURN
!     END IF
!   Now, rather than returning an error, the output structure is destroyed
!   (in case it is defined upon input), and a successful status is returned,
!     IF ( .NOT. CRTM_Associated_AtmScatter( AtmScatter_In ) ) THEN
!       Error_Status = CRTM_Destroy_AtmScatter( AtmScatter_Out, &
!                                               Message_Log = Message_Log )
!       RETURN
!     END IF
!
! Revision 1.10  2004/11/03 17:37:41  paulv
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
! Revision 1.9  2004/08/06 18:22:14  paulv
! - Updated header documentation.
!
! Revision 1.8  2004/07/02 20:29:42  paulv
! - Weird. These files had a CRLF at the end of each line. Dunno how that
!   happened.
!
! Revision 1.7  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.6  2004/06/23 13:46:39  paulv
! - Split out the structure definition and utility routines to this separate
!   module.
!
! Revision 1.5  2004/06/18 20:17:55  paulv
! - Changed name of fuction shell to compute scattering properties to a
!   more generic form.
!
! Revision 1.4  2004/06/15 16:20:30  paulv
! - Added CRTM_ScatteringProperties() function shell.
!
! Revision 1.3  2004/06/14 21:14:47  paulv
! - Replaced extinction coefficient member with the layer optical depth.
!
! Revision 1.2  2004/06/08 15:54:31  paulv
! - Completed updates. Untested.
!
! Revision 1.1  2004/06/04 19:37:21  paulv
! Initial checkin. Incomplete.
!
!
!


