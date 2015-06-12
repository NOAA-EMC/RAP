!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Cloud_Define
!
! PURPOSE:
!       Module defining the CRTM_Cloud structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Cloud_Define
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
!       CRTM_Associated_Cloud:    Function to test the association status
!                                 of the pointer members of a CRTM_Cloud
!                                 structure.
!
!       CRTM_Destroy_Cloud:       Function to re-initialize a CRTM_Cloud
!                                 structure.
!
!       CRTM_Allocate_Cloud:      Function to allocate the pointer members
!                                 of a CRTM_Cloud structure.
!
!       CRTM_Assign_Cloud:        Function to copy a CRTM_Cloud structure.
!
!
!       CRTM_WeightedSum_Cloud:   Function to perform a weighted sum of two
!                                 CRTM_Cloud structures.
!
!       CRTM_Zero_Cloud:          Subroutine to zero-out all members of a 
!                                 CRTM_Cloud structure - both scalar and
!                                 pointer.
!
!
! DERIVED TYPES:
!       CRTM_Cloud_type
!       ---------------
!         Definition of the public CRTM_Cloud data structure.
!         Fields are,
!
!         n_Layers:                Number of atmospheric layers dimension of
!                                  the Cloud data.
!                                  This is the "K" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Type:                    Flag value indicating the cloud type.
!                                  See PUBLIC PARAMETERS for the valid
!                                  cloud types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Effective_Radius:        The effective radius of the cloud particle
!                                  size distribution. The effective radius is
!                                  the ratio of the third moment of the size
!                                  distribution to the second moment.
!                                  UNITS:      microns (um)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (n_Layers)
!                                  ATTRIBUTES: POINTER
!
!         Effective_Variance:      The effective variance of the cloud particle
!                                  size distribution. This variable characterizes
!                                  the width of the size distribution.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (n_Layers)
!                                  ATTRIBUTES: POINTER
!
!         Water_Content:           The water content of the cloud.
!                                  UNITS:      g.m^-2
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (n_Layers)
!                                  ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_Cloud_type is PUBLIC and its members are not
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
! PUBLIC PARAMETERS:
!       1) The valid cloud type values used in the Cloud%Type field:
!
!           Cloud Type      Parameter Name
!         ----------------------------------
!             None          NO_CLOUD
!             Water         WATER_CLOUD
!             Ice           ICE_CLOUD
!             Rain          RAIN_CLOUD
!             Snow          SNOW_CLOUD
!             Graupel       GRAUPEL_CLOUD
!             Hail          HAIL_CLOUD
!
!       2) The number of valid cloud types is specified by the 
!            N_VALID_CLOUD_TYPES
!          parameter.
!
!       3) The character string array parameter
!            CLOUD_TYPE_NAME
!          uses the above cloud type definitions to provide a string value for
!          the type of cloud. For example,
!            CLOUD_TYPE_NAME( GRAUPEL_CLOUD )
!          contains the string
!            'Graupel'
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
!                       20-Feb-2004
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

MODULE CRTM_Cloud_Define


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
  PUBLIC :: CRTM_Associated_Cloud
  PUBLIC :: CRTM_Destroy_Cloud
  PUBLIC :: CRTM_Allocate_Cloud
  PUBLIC :: CRTM_Assign_Cloud
  PUBLIC :: CRTM_WeightedSum_Cloud
  PUBLIC :: CRTM_Zero_Cloud


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Destroy_Cloud
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
  END INTERFACE CRTM_Destroy_Cloud

  INTERFACE CRTM_Allocate_Cloud
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank01
    MODULE PROCEDURE Allocate_Rank11
  END INTERFACE CRTM_Allocate_Cloud

  INTERFACE CRTM_Assign_Cloud
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Cloud

  INTERFACE CRTM_WeightedSum_Cloud
    MODULE PROCEDURE WeightedSum_Scalar
    MODULE PROCEDURE WeightedSum_Rank1
  END INTERFACE CRTM_WeightedSum_Cloud

  INTERFACE CRTM_Zero_Cloud
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Cloud


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Cloud_Define.f90,v 1.14 2005/08/17 18:00:03 paulv Exp $'

  ! -- Cloud member initialisation value
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  INTEGER, PUBLIC, PARAMETER :: N_VALID_CLOUD_TYPES = 6

  INTEGER, PUBLIC, PARAMETER ::      NO_CLOUD = 0
  INTEGER, PUBLIC, PARAMETER ::   WATER_CLOUD = 1
  INTEGER, PUBLIC, PARAMETER ::     ICE_CLOUD = 2
  INTEGER, PUBLIC, PARAMETER ::    RAIN_CLOUD = 3
  INTEGER, PUBLIC, PARAMETER ::    SNOW_CLOUD = 4
  INTEGER, PUBLIC, PARAMETER :: GRAUPEL_CLOUD = 5
  INTEGER, PUBLIC, PARAMETER ::    HAIL_CLOUD = 6

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_CLOUD_TYPES ) :: &
    CLOUD_TYPE_NAME = (/ 'None   ', &
                         'Water  ', &
                         'Ice    ', &
                         'Rain   ', &
                         'Snow   ', &
                         'Graupel', &
                         'Hail   ' /)


  ! --------------------------
  ! Cloud data type definition
  ! --------------------------

  TYPE, PUBLIC :: CRTM_Cloud_type
    INTEGER :: n_Allocates = 0

    ! -- Dimension values
    INTEGER :: n_Layers = 0  ! K dimension.

    ! -- Cloud type
    INTEGER :: Type = NO_CLOUD

    ! -- Particle size distribution parameters
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Effective_Radius   => NULL() ! K. Units are microns
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Effective_Variance => NULL() ! K. Units are Dimensionless

    ! -- Cloud state variables
    REAL( fp_kind ), DIMENSION( : ), POINTER :: Water_Content => NULL()      ! K. Units are g/m^2

  END TYPE CRTM_Cloud_type


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
!       CRTM_Clear_Cloud
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Cloud structure.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Cloud( Cloud ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Cloud:       Cloud structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Cloud_type
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
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Cloud( Cloud )

    TYPE( CRTM_Cloud_type ), INTENT( IN OUT ) :: Cloud

    Cloud%n_Layers      = 0
    Cloud%Type          = NO_CLOUD

  END SUBROUTINE CRTM_Clear_Cloud





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
!       CRTM_Associated_Cloud
!
! PURPOSE:
!       Function to test the association status of a CRTM_Cloud structure.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Cloud( Cloud,              &  ! Input
!                                                   ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Cloud:               Cloud structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Cloud_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Cloud structure pointer members are associated.
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
!                            association status of the Cloud pointer members.
!                            .TRUE.  - if ALL the Cloud pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Cloud pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Cloud pointer
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

  FUNCTION CRTM_Associated_Cloud( Cloud,     & ! Input
                                  ANY_Test ) & ! Optional input
                                RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Cloud_type ), INTENT( IN ) :: Cloud

    ! -- Optional input
    INTEGER,       OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( Cloud%Effective_Radius   ) .AND. &
           ASSOCIATED( Cloud%Effective_Variance ) .AND. &
           ASSOCIATED( Cloud%Water_Content      )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Cloud%Effective_Radius   ) .OR. &
           ASSOCIATED( Cloud%Effective_Variance ) .OR. &
           ASSOCIATED( Cloud%Water_Content      )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_Cloud





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_Cloud
! 
! PURPOSE:
!       Function to re-initialize CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Cloud( Cloud,                    &  ! Output
!                                          RCS_Id = RCS_Id,          &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
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
!       Cloud:        Re-initialized Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank1 array
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
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Cloud,        &  ! Output
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
    TYPE( CRTM_Cloud_type ),  INTENT( IN OUT ) :: Cloud

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Scalar)'


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

    IF ( Clear ) CALL CRTM_Clear_Cloud( Cloud )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the Effective_Radius profile
    IF ( ASSOCIATED( Cloud%Effective_Radius ) ) THEN

      DEALLOCATE( Cloud%Effective_Radius, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Effective_Radius ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the Effective_Variance profile
    IF ( ASSOCIATED( Cloud%Effective_Variance ) ) THEN

      DEALLOCATE( Cloud%Effective_Variance, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Effective_Variance ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the water content profile
    IF ( ASSOCIATED( Cloud%Water_Content ) ) THEN

      DEALLOCATE( Cloud%Water_Content, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Cloud Water_Content ", &
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

    Cloud%n_Allocates = Cloud%n_Allocates - 1

    IF ( Cloud%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Rank1( Cloud,        &  ! Output
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
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: Cloud

    ! -- Optional input
    INTEGER,                 OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Cloud(Rank-1)'


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

    DO n = 1, SIZE( Cloud )

      Scalar_Status = Destroy_Scalar( Cloud(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of Cloud structure array." )' ) n
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
!       CRTM_Allocate_Cloud
! 
! PURPOSE:
!       Function to allocate CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Cloud( n_Layers,                 &  ! Input
!                                           Cloud,                    &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers for which there is cloud data.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Cloud dimensionality chart
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
!       Cloud:        Cloud structure with allocated pointer members. The
!                     following table shows the allowable dimension combinations
!                     for the calling routine, where N == number of clouds:
!
!                        Input       Output
!                       n_Layers      Cloud
!                       dimension   dimension
!                     ---------------------------
!                        scalar       scalar
!                        scalar         N
!                          N            N
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar OR Rank-1
!                                 See table above.
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
!       CRTM_Associated_Cloud:   Function to test the association status of
!                                a CRTM_Cloud structure.
!
!       CRTM_Destroy_Cloud:      Function to re-initialize the scalar and pointer
!                                members of Cloud data structures.
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            Cloud,        &  ! Output
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
    INTEGER,                  INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_Cloud_type ),  INTENT( IN OUT ) :: Cloud

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Scalar)'


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

    ! ---------------
    ! Layer dimension
    ! ---------------

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

    IF ( CRTM_Associated_Cloud( Cloud, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_Cloud( Cloud, &
                                         No_Clear = SET, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Cloud pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Cloud%Effective_Radius( n_Layers ), &
              Cloud%Effective_Variance( n_Layers ), &
              Cloud%Water_Content( n_Layers ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Cloud data arrays. STAT = ", i5 )' ) &
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

    Cloud%n_Layers = n_Layers

    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Cloud%n_Allocates = Cloud%n_Allocates + 1

    IF ( Cloud%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Cloud%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank01( n_Layers,     &  ! Input
                            Cloud,        &  ! Output
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
    INTEGER,                                 INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: Cloud

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Rank-01)'


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

    DO i = 1, SIZE( Cloud )

      Scalar_Status = Allocate_Scalar( n_Layers, &
                                       Cloud(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank01


  FUNCTION Allocate_Rank11( n_Layers,     &  ! Input
                            Cloud,        &  ! Output
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
    INTEGER,                 DIMENSION( : ), INTENT( IN )     :: n_Layers

    ! -- Output
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: Cloud

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Cloud(Rank-11)'


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

    n = SIZE( n_Layers )

    IF ( SIZE( Cloud ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and CRTM_Cloud arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i), &
                                       Cloud(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank11





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_Cloud
!
! PURPOSE:
!       Function to copy valid CRTM_Cloud structures.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Cloud( Cloud_in,                 &  ! Input  
!                                         Cloud_out,                &  ! Output 
!                                         RCS_Id = RCS_Id,          &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Cloud_in:      Cloud structure which is to be copied.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar
!                                    OR
!                                  Rank1 array
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Cloud_out:     Copy of the input structure, Cloud_in.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Same as Cloud_in argument
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
!       CRTM_Associated_Cloud:    Function to test the association status of
!                                 a CRTM_Cloud structure.
!
!       CRTM_Destroy_Cloud:       Function to re-initialize CRTM_Cloud structures.
!
!       CRTM_Allocate_Cloud:      Function to allocate a CRTM_Cloud structure.
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
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Cloud_in,     &  ! Input
                          Cloud_out,    &  ! Output
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
    TYPE( CRTM_Cloud_type ),  INTENT( IN )     :: Cloud_in

    ! -- Output
    TYPE( CRTM_Cloud_type ),  INTENT( IN OUT ) :: Cloud_out

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Scalar)'



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

    IF ( .NOT. CRTM_Associated_Cloud( Cloud_In ) ) THEN

      Error_Status = CRTM_Destroy_Cloud( Cloud_Out, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Cloud pointer members.', &
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

    Error_Status = CRTM_Allocate_Cloud( Cloud_in%n_Layers, &
                                        Cloud_out, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Cloud arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    Cloud_out%Type = Cloud_in%Type


    ! -----------------
    ! Assign array data
    ! -----------------

    Cloud_out%Effective_Radius   = Cloud_in%Effective_Radius  
    Cloud_out%Effective_Variance = Cloud_in%Effective_Variance
    Cloud_out%Water_Content      = Cloud_in%Water_Content

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Cloud_in,     &  ! Input
                         Cloud_out,    &  ! Output
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
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN )     :: Cloud_in

    ! -- Output
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: Cloud_out

    ! -- Revision control
    CHARACTER( * ),          OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),          OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Cloud(Rank-1)'


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

    n = SIZE( Cloud_in )

    IF ( SIZE( Cloud_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Cloud_in and Cloud_out arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Scalar( Cloud_in(i), &
                                     Cloud_out(i), &
                                     Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Cloud structure array." )' ) i
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
!       CRTM_WeightedSum_Cloud
!
! PURPOSE:
!       Function to perform a weighted sum of two valid CRTM_Cloud
!       structures. The weighted summation performed is:
!         A = A + w1*B + w2
!       where A and B are the CRTM_Cloud structures, and w1 and w2
!       are the weighting factors. Note that w2 is optional.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_WeightedSum_Cloud( A,                        &  ! In/Output
!                                              B,                        &  ! Input
!                                              w1,                       &  ! Input
!                                              w2 = w2,                  &  ! Optional input
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       A:             Cloud structure that is to be added to.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
!                      DIMENSION:  Scalar OR Rank-1
!                      ATTRIBUTES: INTENT( IN OUT )
!
!       B:             Cloud structure that is to be weighted and
!                      added to structure A.
!                      UNITS:      N/A
!                      TYPE:       CRTM_Cloud_type
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
!                      TYPE:       CRTM_Cloud_type
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
!       CRTM_Associated_Cloud:    Function to test the association status of
!                                 a CRTM_Cloud structure.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
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
    TYPE( CRTM_Cloud_type ),   INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Cloud_type ),   INTENT( IN )     :: B
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Cloud(Scalar)'


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

    IF ( .NOT. CRTM_Associated_Cloud( A ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'On input, structure argument A appears empty.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( .NOT. CRTM_Associated_Cloud( B ) ) THEN
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

    IF ( A%n_Layers /= B%n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure dimensions are different.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF
         

    ! ----------------------------
    ! Cloud types must be the same
    ! ----------------------------

    IF ( A%Type /= B%Type ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure Cloud types are different.', &
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
    A%Water_Content      = A%Water_Content      + (w1*B%Water_Content)      + w2_Local


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
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN )     :: B
    REAL( fp_kind ),                         INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL,               INTENT( IN )     :: w2

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,               INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL,               INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Cloud(Rank-1)'


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
                          &" of CRTM_Cloud structure arrays." )' ) i
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
!       CRTM_Zero_Cloud
! 
! PURPOSE:
!       Subroutine to zero-out all members of a CRTM_Cloud structure - both
!       scalar and pointer.
!
! CATEGORY:
!       CRTM : Atmosphere : Cloud
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Cloud( Cloud )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Cloud:        Zeroed out Cloud structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
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
!         tests for pointer member association status. This means the Cloud
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The dimension components of the structure are *NOT*
!         set to zero.
!
!       - The cloud type component is *NOT* reset.
!
! COMMENTS:
!       Note the INTENT on the output Cloud argument is IN OUT rather than
!       just OUT. This is necessary because the argument must be defined upon
!       input.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Aug-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Cloud )  ! Output
    TYPE( CRTM_Cloud_type ),  INTENT( IN OUT ) :: Cloud

    ! -- Reset the array components
    Cloud%Effective_Radius   = ZERO
    Cloud%Effective_Variance = ZERO
    Cloud%Water_Content      = ZERO

  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Cloud )  ! Output

    TYPE( CRTM_Cloud_type ), DIMENSION( : ), INTENT( IN OUT ) :: Cloud
    INTEGER :: n

    DO n = 1, SIZE( Cloud )
      CALL Zero_Scalar( Cloud(n) )
    END DO

  END SUBROUTINE Zero_Rank1

END MODULE CRTM_Cloud_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_Cloud_Define.f90,v 1.14 2005/08/17 18:00:03 paulv Exp $
!
! $Date: 2005/08/17 18:00:03 $
!
! $Revision: 1.14 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Cloud_Define.f90,v $
! Revision 1.14  2005/08/17 18:00:03  paulv
! - Modified the Zero() subroutines so that the dimensional structure members
!   are not set to zero. The only change to these occurs when there is a
!   corresponding Max dimension value--in this case the dimension value is
!   set to the maximum value.
! - Modified the Zero() subroutines so that the cloud and aerosol type flags
!   are not reset to zero. They retain their value.
!
! Revision 1.13  2005/08/17 17:10:30  paulv
! - Added structure zero subroutines.
!
! Revision 1.12  2005/06/15 23:12:29  paulv
! - Added WeightedSum() functions.
!
! Revision 1.11  2005/03/24 15:13:48  paulv
! - Updated header documentation.
!
! Revision 1.10  2005/02/25 17:36:30  paulv
! - Removed unused variable definitions.
!
! Revision 1.9  2005/02/16 15:40:45  paulv
! - Added an extra specific allocation function for scalar n_Layers and rank-1
!   Cloud arguments.
!
! Revision 1.8  2004/11/03 20:31:31  paulv
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
!     IF ( .NOT. CRTM_Associated_Cloud( Cloud_In ) ) THEN
!       Error_Status = FAILURE
!       RETURN
!     END IF
!   Now, rather than returning an error, the output structure is destroyed
!   (in case it is defined upon input), and a successful status is returned,
!     IF ( .NOT. CRTM_Associated_Cloud( Cloud_In ) ) THEN
!       Error_Status = CRTM_Destroy_Cloud( Cloud_Out, &
!                                          Message_Log = Message_Log )
!       RETURN
!     END IF
!
! Revision 1.7  2004/09/24 17:32:50  paulv
! - Converted code to Fortran-95. Derived type initialisation now done in
!   definition.
!
! Revision 1.6  2004/08/06 16:41:06  paulv
! - Updated header documentation.
!
! Revision 1.5  2004/07/23 21:21:26  paulv
! - Removed intrinsic statements.
!
! Revision 1.4  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.3  2004/06/14 14:50:25  paulv
! - Removed pressure and temperature arrays from Cloud definition. It is now
!   assumed that the values in the parent Atmosphere structure correspond to
!   the same layering as that in the Cloud structure.
! - Replaced the particle size component with the effective radius and
!   effective variance to allow not only some measure of the cloud particle
!   size, but also the distribution.
!
! Revision 1.2  2004/06/04 20:11:23  paulv
! - Altered components for specification of the cloud particle size
!   distribution. Removed Particle_Size and replaced it with
!   Effective_Radius and Effective_Variance.
!
! Revision 1.1  2004/05/19 19:55:18  paulv
! Initial checkin.
!
!
!
!
