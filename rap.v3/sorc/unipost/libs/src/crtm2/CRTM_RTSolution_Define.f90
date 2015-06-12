!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_RTSolution_Define
!
! PURPOSE:
!       Module defining the CRTM RTSolution structure and containing routines
!       to manipulate it.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_RTSolution_Define
!
! MODULES:
!       Type_Kinds:           Module containing data type kind definitions.
!
!       Error_Handler:        Module to define simple error codes and
!                             handle error conditions
!                             USEs: FILE_UTILITY module
!
!
! CONTAINS:
!       CRTM_Clear_RTSolution:   Subroutine to clear the scalar members of a
!                                CRTM_RTSolution structure.
!
!
! DERIVED TYPES:
!       CRTM_RTSolution_type
!       --------------------
!         Definition of the CRTM radiative transfer solution data
!         structure. Fields are,
!
!         Radiance:                The top-of-atmosphere (TOA) radiative
!                                  transfer result in radiance units.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      mW/(m2.sr.cm-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Brightness_Temperature:  The top-of-atmosphere (TOA) radiative
!                                  transfer result in temperature units.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         ** Fields valid only for Forward model results:
!
!             Surface_Emissivity:  Surface Emissivity at the sensor's zenith
!                                  angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!            Layer_Optical_Depth:  Layer total optical depth at the sensor's
!                                  zenoth angle.   
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1
!
!                       n_Layers:  The number of atmospheric layers
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
! !
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
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-June-2004
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
!------------------------------------------------------------------------------

MODULE CRTM_RTSolution_Define


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Error_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures
  PUBLIC :: CRTM_Associated_RTSolution
  PUBLIC :: CRTM_Destroy_RTSolution
  PUBLIC :: CRTM_Allocate_RTSolution
  PUBLIC :: CRTM_Assign_RTSolution
  PUBLIC :: CRTM_Clear_RTSolution

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Destroy_RTSolution
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
    MODULE PROCEDURE Destroy_Rank2
  END INTERFACE CRTM_Destroy_RTSolution

  INTERFACE CRTM_Allocate_RTSolution
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank01
    MODULE PROCEDURE Allocate_Rank02
    MODULE PROCEDURE Allocate_Rank11
    MODULE PROCEDURE Allocate_Rank12
  END INTERFACE CRTM_Allocate_RTSolution

  INTERFACE CRTM_Assign_RTSolution
    MODULE PROCEDURE Assign_RTSolution_Scalar
    MODULE PROCEDURE Assign_RTSolution_Rank1
    MODULE PROCEDURE Assign_RTSolution_Rank2
  END INTERFACE CRTM_Assign_RTSolution


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_RTSolution_Define.f90,v 1.10 2005/09/26 13:25:37 yhan Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! -------------------------------
  ! RTSolution data type definition
  ! -------------------------------

  TYPE, PUBLIC :: CRTM_RTSolution_type
  
    ! -- Radiative transfer results for a single channel/node
    REAL( fp_kind ) :: Radiance               = ZERO
    REAL( fp_kind ) :: Brightness_Temperature = ZERO

    ! -- Forward radiative transfer intermediate results for a single channel
    !    These components are not defined when they are used as TL, AD
    !    and K variables

    INTEGER         :: n_Layers     = 0  ! K dimension

    REAL( fp_kind ) :: Surface_Emissivity     = ZERO
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Layer_Optical_Depth => NULL()  ! K

    INTEGER         :: n_Allocates = 0


    ! -- Internal variables. Users do not need to worry about these.
    INTEGER :: n_Full_Streams  = 0
    INTEGER :: Index_Sat_Ang   = 0
    LOGICAL :: Scattering_Flag = .TRUE. 
    INTEGER :: n_Stokes        = 0


  END TYPE CRTM_RTSolution_type


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!----------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Clear_RTSolution
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_RTSolution structure.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_RTSolution( RTSolution ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       RTSolution:  CRTM_RTSolution structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_RTSolution_type
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_RTSolution( RTSolution )
    TYPE( CRTM_RTSolution_type ), INTENT( IN OUT ) :: RTSolution
    RTSolution%n_Full_Streams  = 0
    RTSolution%Index_Sat_Ang   = 0
    RTSolution%Scattering_Flag = .TRUE.
    RTSolution%n_Stokes        = 0
    RTSolution%Radiance               = ZERO
    RTSolution%Brightness_Temperature = ZERO

    RTSolution%n_Layers        = 0
    RTSolution%Surface_Emissivity     = ZERO

  END SUBROUTINE CRTM_Clear_RTSolution


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Associated_RTSolution
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_RTSolution structure.
!
! CATEGORY:
!       CRTM : RTSolution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_RTSolution( RTSolution )  ! Input
!
! INPUT ARGUMENTS:
!       RTSolution: RTSolution structure which is to have its pointer
!                    member's association status tested.
!                    UNITS:      N/A
!                    TYPE:       CRTM_RTSolution_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating the
!                            association status of the RTSolution pointer
!                            members.
!                            .TRUE.  - if ALL the RTSolution pointer members
!                                      are associated, or if the ANY_Test argument
!                                      is set and ANY of the RTSolution
!                                      pointer members are associated.
!                            .FALSE. - some or all of the RTSolution pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_RTSolution( RTSolution ) & ! Input
                                      RESULT( Association_Status )


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_RTSolution_type ), INTENT( IN ) :: RTSolution

    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status



    IF ( ASSOCIATED( RTSolution%Layer_Optical_Depth ) ) THEN
      Association_Status = .TRUE.
    ELSE
      Association_Status = .FALSE.
    END IF


  END FUNCTION CRTM_Associated_RTSolution


!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_RTSolution
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of a CRTM
!       RTSolution data structures.
!
! CATEGORY:
!       CRTM : RTSolution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_RTSolution( RTSolution,              &  ! Output
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
! 
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       RTSolution:  Re-initialized RTSolution structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar, Rank-1, or Rank-2
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
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
!       CRTM_Clear_RTSolution:       Subroutine to clear the scalar members of a
!                                     CRTM RTSolution structure.
!
!       CRTM_Associated_RTSolution:  Function to test the association status of
!                                     the pointer members of a CRTM_RTSolution
!                                     structure.
!
!       Display_Message:              Subroutine to output messages
!                                     SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( RTSolution,   &  ! Output
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
    TYPE( CRTM_RTSolution_type ), INTENT( IN OUT )  :: RTSolution

    ! -- Optional input
    INTEGER,             OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),      OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_RTSolution(Scalar)'


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
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL CRTM_Clear_RTSolution( RTSolution )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

!!    IF ( .NOT. CRTM_Associated_RTSolution( RTSolution ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the RTSolution Layer_Optical_Depth member
    IF ( ASSOCIATED( RTSolution%Layer_Optical_Depth ) ) THEN

      DEALLOCATE( RTSolution%Layer_Optical_Depth, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_RTSolution Layer_Optical_Depth ", &
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

    RTSolution%n_Allocates = RTSolution%n_Allocates - 1

    IF ( RTSolution%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      RTSolution%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar

  FUNCTION Destroy_Rank1( RTSolution,   &  ! Output
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
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution

    ! -- Optional input
    INTEGER,                      OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_RTSolution(Rank-1)'


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

    DO n = 1, SIZE( RTSolution )

      Scalar_Status = Destroy_Scalar( RTSolution(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1


  FUNCTION Destroy_Rank2( RTSolution,   &  ! Output
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
    TYPE( CRTM_RTSolution_type ), DIMENSION( :, : ), INTENT( IN OUT ) :: RTSolution

    ! -- Optional input
    INTEGER,                      OPTIONAL,          INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,          INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,          INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_RTSolution(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, j



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

    DO j = 1, SIZE( RTSolution, DIM = 2 )
      DO i = 1, SIZE( RTSolution, DIM = 1 )

        Scalar_Status = Destroy_Scalar( RTSolution(i,j), &
                                        No_Clear = No_Clear, &
                                        Message_Log = Message_Log )

        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error destroying element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i,j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF

      END DO
    END DO

  END FUNCTION Destroy_Rank2


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_RTSolution
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_RTSolution
!       data structure.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_RTSolution( n_Layers,                 &  ! Input
!                                                RTSolution,               &  ! Output
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Layers:   Number of atmospheric layers 
!                     Must be > 0
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output RTSolution dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       RTSolution:   CRTM_RTSolution structure with allocated pointer members.
!                     Upon allocation, all pointer members are initialized to
!                     a value of zero.
!
!                     The following chart shows the allowable dimension
!                     combinations for the calling routine, where
!                       L == number of channels
!                       M == number of profiles
!
!                        Input                       Output
!                       n_Layers                    RTSolution
!                       dimension                   dimension
!                     ----------------------------------------------------------
!                        scalar      scalar, Rank-1 (L or M), or Rank-2 (L x M)
!                          L                 Rank-1 (L),      or Rank-2 (L x M)
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on how it's used.
!                     UNITS:      N/A
!                     TYPE:       CRTM_RTSolution_type
!                     DIMENSION:  Scalar, Rank-1, or Rank-2
!                     ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
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
!       CRTM_Associated_RTSolution: Function to test the association status of the
!                                   pointer members of a CRTM_RTSolution structure.
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
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            RTSolution,   &  ! Output
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
    INTEGER,                      INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_RTSolution_type ), INTENT( IN OUT ) :: RTSolution

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution'


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



    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_RTSolution( RTSolution ) ) THEN

      Error_Status = CRTM_Destroy_RTSolution( RTSolution, &
                                              No_Clear = SET, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_RTSolution pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( RTSolution%Layer_Optical_Depth( n_Layers ),    &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating RTSolution data arrays. STAT = ", i5 )' ) &
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

    RTSolution%n_Layers = n_Layers

    RTSolution%Layer_Optical_Depth = ZERO


    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    RTSolution%n_Allocates = RTSolution%n_Allocates + 1

    IF ( RTSolution%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      RTSolution%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar

  FUNCTION Allocate_Rank01( n_Layers,     &  ! Input, scalar
                           RTSolution,   &  ! Output, L or M   
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
    INTEGER,                                      INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-01)'


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
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO n = 1, SIZE( RTSolution )

      Scalar_Status = Allocate_Scalar( n_Layers,      & ! Input
                                       RTSolution(n), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank01


  FUNCTION Allocate_Rank02( n_Layers,     &  ! Input, scalar
                           RTSolution,   &  ! Output, L x M    
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
    INTEGER,                                      INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION(:,:), INTENT( IN OUT ) :: RTSolution

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-02)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, j



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

    DO j = 1, SIZE( RTSolution, DIM = 2 )
      DO i = 1, SIZE( RTSolution, DIM = 1 )

        Scalar_Status = Allocate_Scalar( n_Layers,        & ! Input
                                         RTSolution(i,j), & ! Output
                                         Message_Log = Message_Log )

        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error allocating element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i, j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF

      END DO
    END DO

  END FUNCTION Allocate_Rank02

  FUNCTION Allocate_Rank11( n_Layers,     &  ! Input, L
                            RTSolution,   &  ! Output, L
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
    INTEGER,                      DIMENSION(:), INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION(:), INTENT( IN OUT ) :: RTSolution

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,     INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,     INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-11)'


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

    ! ---------------------------
    ! Dimensions of n_layers and
    ! RTSolution must be the same
    ! ---------------------------

    n = SIZE( n_Layers )

    IF ( SIZE( RTSolution ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and RTSolution arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i),   & ! Input
                                       RTSolution(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank11


  FUNCTION Allocate_Rank12( n_Layers,     &  ! Input, L
                            RTSolution,   &  ! Output, L x M
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
    INTEGER,                      DIMENSION(:),   INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION(:,:), INTENT( IN OUT ) :: RTSolution

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_RTSolution(Rank-12)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, j, n



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

    ! ------------------------------------------
    ! Dimensions of n_layers and first dimension
    ! of RTSolution must be the same
    ! ------------------------------------------

    n = SIZE( n_Layers )

    IF ( SIZE( RTSolution, DIM = 1 ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers array and the first dimension of'//&
                            ' the RTSolution array have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO j = 1, SIZE( RTSolution, DIM = 2 )
      DO i = 1, n

        Scalar_Status = Allocate_Scalar( n_Layers(i),     & ! Input
                                         RTSolution(i,j), & ! Output
                                         Message_Log = Message_Log )

        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error allocating element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i, j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF

      END DO
    END DO

  END FUNCTION Allocate_Rank12




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_RTSolution
!
! PURPOSE:
!       Function to copy valid CRTM_RTSolution structures.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_RTSolution( RTSolution_in,            &  ! Input
!                                              RTSolution_out,           &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       RTSolution_in:   CRTM_RTSolution structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Scalar, Rank-1, or Rank-2
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       RTSolution_out:  Copy of the input structure, CRTM_RTSolution_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_RTSolution_type
!                        DIMENSION:  Same as input RTSolution_in
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
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
!       CRTM_Associated_RTSolution: Function to test the association status of the
!                                   pointer members of a CRTM_RTSolution structure.
!
!       CRTM_Allocate_RTSolution:   Function to allocate the pointer members of
!                                   the CRTM_RTSolution data structure.
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
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_RTSolution_Scalar( RTSolution_in,  &  ! Input
                                     RTSolution_out, &  ! Output
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
    TYPE( CRTM_RTSolution_type ), INTENT( IN )     :: RTSolution_in

    ! -- Output
    TYPE( CRTM_RTSolution_type ), INTENT( IN OUT ) :: RTSolution_out

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_RTSolution'



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

    IF ( .NOT. CRTM_Associated_RTSolution( RTSolution_In ) ) THEN

      Error_Status = CRTM_Destroy_RTSolution( RTSolution_Out, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_RTSolution pointer members.', &
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

    Error_Status = CRTM_Allocate_RTSolution( RTSolution_in%n_Layers, &
                                             RTSolution_out, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output RTSolution arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Assign array data
    ! -----------------

    RTSolution_out%Layer_Optical_Depth    = RTSolution_in%Layer_Optical_Depth             

  END FUNCTION Assign_RTSolution_Scalar

  FUNCTION Assign_RTSolution_Rank1( RTSolution_in,  &  ! Input
                                    RTSolution_out, &  ! Output
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
    TYPE( CRTM_RTSolution_type ), DIMENSION(:), INTENT( IN )     :: RTSolution_in

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION(:), INTENT( IN OUT ) :: RTSolution_out

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,     INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,     INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_RTSolution(Rank-1)'


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

    n = SIZE( RTSolution_in )

    IF ( SIZE( RTSolution_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input RTSolution_in and RTSolution_out arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_RTSolution_Scalar( RTSolution_in(i), &
                                                RTSolution_out(i), &
                                                Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of rank-1 CRTM_RTSolution structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Assign_RTSolution_Rank1


  FUNCTION Assign_RTSolution_Rank2( RTSolution_in,  &  ! Input
                                    RTSolution_out, &  ! Output
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
    TYPE( CRTM_RTSolution_type ), DIMENSION(:,:), INTENT( IN )     :: RTSolution_in

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION(:,:), INTENT( IN OUT ) :: RTSolution_out

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_RTSolution(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Scalar_Status
    INTEGER :: i, j, l, m



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

    l = SIZE( RTSolution_in, DIM = 1 )
    m = SIZE( RTSolution_in, DIM = 2 )

    IF ( SIZE( RTSolution_out, DIM = 1 ) /= l .AND. &
         SIZE( RTSolution_out, DIM = 2 ) /= m       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input RTSolution_in and RTSolution_out arrays'//&
                            ' have different dimension sizes', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO j = 1, m
      DO i = 1, l

        Scalar_Status = Assign_RTSolution_Scalar( RTSolution_in(i,j), &
                                                  RTSolution_out(i,j), &
                                                  Message_Log = Message_Log )


        IF ( Scalar_Status /= SUCCESS ) THEN
          Error_Status = Scalar_Status
          WRITE( Message, '( "Error copying element #", i5, ",", i5, &
                            &" of rank-2 CRTM_RTSolution structure array." )' ) i, j
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
        END IF

      END DO
    END DO

  END FUNCTION Assign_RTSolution_Rank2

END MODULE CRTM_RTSolution_Define



!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_RTSolution_Define.f90,v 1.10 2005/09/26 13:25:37 yhan Exp $
!
! $Date: 2005/09/26 13:25:37 $
!
! $Revision: 1.10 $
!
! $Name: CRTM_Sensor $
!
! $State: Exp $
!
! $Log: CRTM_RTSolution_Define.f90,v $
! Revision 1.10  2005/09/26 13:25:37  yhan
! --- Added members Surface_Emissivity, Layer_Optical_Depth and n_layers, and
!     corresponding routines to manipulate them.
!
! Revision 1.9  2005/08/16 20:47:52  qliu
! - Added temporary internal variables in structure definition.
!
! Revision 1.8  2005/08/16 20:33:28  qliu
! - Merged baseline changes into the main branch for CRTM_Sensor/CRTM_Spectral
!   branching.
!
! Revision 1.7.2.2  2005/08/16 20:29:34  qliu
! - Added initialisation expressions to structure definition.
!
! Revision 1.7.2.1  2005/06/20 21:08:45  paulv
! - Removed n_Stokes dimension. All structure components are now scalar. Thus
!   the Associated(), Destroy(), Allocate() and Assign() functions have been
!   removed. This was done because different polarisations are treated as
!   separate channels so the n_Stokes dimension of the RTSolution structure
!   was redundant.
!
! Revision 1.7  2005/02/16 15:46:32  paulv
! - Shortened specific function names.
!
! Revision 1.6  2005/02/01 16:02:16  paulv
! - Replaced initialisation of pointer members in the Allocate() function from
!   an "invalid" value to zero.
!
! Revision 1.5  2004/11/05 16:11:23  paulv
! - Upgraded to Fortran-95
! - Structure initialisation is now performed in the structure type
!   declaration. Removed Init() subroutine.
! - Made Associated() function PUBLIC.
! - Intent of output structures in the Clear(), Allocate(), and Assign()
!   routines changed from (OUT) to (IN OUT) to prevent memory leaks.
! - Allocate() function now destroys the output structure if any pointer
!   members are defined upon input.
! - Updated documentation.
! - Added FP_INVALID parameter for initialising floating point variables.
! - Altered the way the Assign() function handles unassociated input. Previously
!   an error was issued:
!     IF ( .NOT. CRTM_Associated_RTSolution( RTSolution_In ) ) THEN
!       Error_Status = FAILURE
!       RETURN
!     END IF
!   Now, rather than returning an error, the output structure is destroyed
!   (in case it is defined upon input), and a successful status is returned,
!     IF ( .NOT. CRTM_Associated_RTSolution( RTSolution_In ) ) THEN
!       Error_Status = CRTM_Destroy_RTSolution( RTSolution_Out, &
!                                               Message_Log = Message_Log )
!       RETURN
!     END IF
!
! Revision 1.4  2004/08/06 18:43:30  paulv
! - Updated header documentation.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/06/28 21:50:54  paulv
! - Cosmetic changes.
!
! Revision 1.1  2004/06/28 21:06:32  paulv
! Initial checkin.
!
!
!
