!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Forward_Module
!
! PURPOSE:
!       Module containing the CRTM forward model function.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Forward_Module
!
! MODULES:
!       Type_Kinds:                 Module to define kind types for variable
!                                   declaration.
!
!       Error_Handler:              Module to define error codes and handle
!                                   error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_Atmosphere_Define:     Module defining the CRTM Atmosphere
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_CLOUD_DEFINE module
!
!       CRTM_Surface_Define:        Module defining the CRTM Surface data
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_GeometryInfo_Define:   Module defining the CRTM GeometryInfo
!                                   data structure and containing routines
!                                   to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_PARAMETERS module
!
!       CRTM_ChannelInfo_Define:    Module defining the CRTM ChannelInfo
!                                   data structure and containing routines
!                                   to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_Options_Define:        Module defining the CRTM Options optional
!                                   argument data structure and containing
!                                   routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_PARAMETERS module
!
!       CRTM_AtmAbsorption:         Module continaing routines to compute
!                                   the optical depth profile due to gaseous
!                                   absorption.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_PARAMETERS module
!                                         CRTM_TAUCOEFF module
!                                         CRTM_ATMOSPHERE_DEFINE module
!                                         CRTM_GEOMETRYINFO_DEFINE module
!                                         CRTM_ATMABSORPTION_DEFINE module
!                                         CRTM_ATMABSORPTION_INTABSORBER module
!                                         CRTM_ATMABSORPTION_PREDICTOR module
!
!       CRTM_AerosolScatter:        Module containing routines to compute
!                                   aerosol absorption and scattering properties.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_CloudScatter:          Module containing routines to compute cloud
!                                   particle absorption and scattering properties.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_SfcOptics:             Module containing routines to compute 
!                                   surface emissivities and reflectivities.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_AtmOptics:             Module containing routines to combine the
!                                   optical properties from the CRTM AtmAbsorption,
!                                   CloudScatter and AerosolScatter structures.
!                                   USEs: TYPE_KINDS module
!                                         CRTM_PARAMETERS module
!                                         CRTM_ATMABSORPTION_DEFINE module
!                                         CRTM_ATMSCATTER_DEFINE module
!
!       CRTM_RTSolution:            Module containing the radiative transfer
!                                   solution routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
! CONTAINS:
!       CRTM_Forward:    Function that calculates top-of-atmosphere (TOA)
!                        radiances and brightness temperatures for an input
!                        atmospheric profile or profile set and user
!                        specified satellites/channels.Function to solve
!                        the forward radiative transfer problem.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Jun-2004
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

MODULE CRTM_Forward_Module


  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds, ONLY : fp_kind
  USE Error_Handler

  ! -- CRTM "global" parameters
  USE CRTM_Parameters

  ! -- Definition modules
  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Define
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Options_Define

  ! -- Application modules
  USE CRTM_AtmAbsorption
  USE CRTM_AerosolScatter
  USE CRTM_CloudScatter
  USE CRTM_SfcOptics
  USE CRTM_AtmOptics
  USE CRTM_RTSolution
  USE CRTM_RTlocal

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures
  PUBLIC :: CRTM_Forward


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE CRTM_Forward
    MODULE PROCEDURE CRTM_Forward_scalar
    MODULE PROCEDURE CRTM_Forward_rank1
  END INTERFACE CRTM_Forward


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Forward_Module.f90,v 1.14.2.10 2005/10/11 21:25:24 qliu Exp $'

  ! -- Keyword switch values
  INTEGER, PRIVATE, PARAMETER :: NOT_SET = 0
  INTEGER, PRIVATE, PARAMETER ::     SET = 1


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!  NONE





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
!       CRTM_Forward
!
! PURPOSE:
!       Function that calculates top-of-atmosphere (TOA) radiances
!       and brightness temperatures for an input atmospheric profile or
!       profile set and user specified satellites/channels.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Forward( Atmosphere,                &  ! Input    
!                                    Surface,                   &  ! Input    
!                                    GeometryInfo,              &  ! Input    
!                                    ChannelInfo,               &  ! Input    
!                                    RTSolution,                &  ! Output   
!                                    Options      = Options,    &  ! Optional input
!                                    RCS_Id       = RCS_Id,     &  ! Revision control
!                                    Message_Log  = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_GeometryInfo_type )
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sesnor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_ChannelInfo_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Options:        Options structure containing the optional arguments
!                       for the CRTM.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-1 (L)
!                                     or
!                                   Rank-2 (L x M)
!                                   See dimensionality table in COMMENTS below.
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
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!      CRTM_Allocate_AtmAbsorption:   Function to allocate AtmAbsorption
!                                     data structures.
!                                     SOURCE: CRTM_ATMABSORPTION_DEFINE module
!
!      CRTM_Allocate_AtmScatter:      Function to allocate AtmScatter data
!                                     structures.
!                                     SOURCE: CRTM_ATMSCATTER_DEFINE module
!
!      CRTM_Allocate_SfcOptics:       Function to allocate SfcOptics data
!                                     structures.
!                                     SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!      CRTM_SetUp_AtmAbsorption:      Function to prepare the AtmAbsorption
!                                     structure for gaseous absorption calculations.
!                                     SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AtmAbsorption:    Function to compute optical depths due
!                                     to gaseuos absorption.
!                                     SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AerosolScatter:   Function to compute aerosol absorption
!                                     and scattering properties.
!                                     SOURCE: CRTM_AEROSOLSCATTER module
!
!      CRTM_Compute_CloudScatter:     Function to compute cloud particle absorption
!                                     and scattering optical depths.
!                                     SOURCE: CRTM_CLOUDSCATTER module
!
!      CRTM_Compute_SfcOptics:        Function to compute surface emissivities
!                                     and reflectivities.
!                                     SOURCE: CRTM_SFCOPTICS module
!
!      CRTM_Compute_RTSolution:       Function to solve the radiative transfer
!                                     equation.
!                                     SOURCE: CRTM_RTSOLUTION module
!
!      Display_Message:               Subroutine to output messages
!                                     SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!      None.
!
! RESTRICTIONS:
!      None.
!
! COMMENTS:
!       - The folowing tables details the input/output argument dimensionality
!         association, where L == n_Channels, M == n_Profiles:
!
!                                           |   OPTIONAL  |
!                  INPUTS                   |    INPUT    |   OUTPUTS
!                                           |             |
!     Atmosphere   Surface   GeometryInfo   |   Options   |  RTSolution
!  -----------------------------------------+-------------+----------------
!       Scalar      Scalar      Scalar      |    Scalar   |      L
!                                           |             |
!         M           M           M         |      M      |    L x M
!
!         Thus one can process either a single profile or multiple profiles.
!         The routines for each specific case above have been overloaded to
!         the generic interface described in the header above.
!
!       - Note that the Options optional input structure argument contains
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the output
!         RTSolution structure.
!
!       - Note the INTENT on the output RTSolution argument is IN OUT rather
!         than just OUT. This is necessary because the argument may be defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------


  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Forward_rank1( Atmosphere,   &  ! Input, M
                               Surface,      &  ! Input, M    
                               GeometryInfo, &  ! Input, M    
                               ChannelInfo,  &  ! Input, Scalar    
                               RTSolution,   &  ! Output, L x M   
                               Options,      &  ! Optional input, M    
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( : ),    INTENT( IN OUT ) :: Atmosphere    ! M
    TYPE( CRTM_Surface_type ),           DIMENSION( : ),    INTENT( IN )     :: Surface       ! M
    TYPE( CRTM_GeometryInfo_type ),      DIMENSION( : ),    INTENT( IN OUT ) :: GeometryInfo  ! M
    TYPE( CRTM_ChannelInfo_type ),                          INTENT( IN )     :: ChannelInfo   ! Scalar 

    ! -- Output
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :, : ), INTENT( IN OUT ) :: RTSolution    ! L x M

    ! -- Optional input
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ),    INTENT( IN )     :: Options       ! M

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,                    INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),            OPTIONAL,                    INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Forward(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed
    LOGICAL :: Options_Present
    INTEGER :: m, n_Profiles



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
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
    !#          -- DETERMINE ARRAY DIMENSIONS AND CHECK THE VALUES --           #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------

    ! -- Number of atmospheric profiles.

    n_Profiles = SIZE( Atmosphere )

!    IF ( PRESENT( n_Input_Profiles ) ) THEN
!      IF ( n_Input_Profiles > 0 .AND. n_Input_Profiles <= n_Profiles ) THEN
!        n_Profiles = n_Input_Profiles
!      ELSE
!        WRITE( Message, '( "Invalid N_INPUT_PROFILES value: ", i5, &
!                          &". Using Atmosphere structure array dimension value of ", i5, "." )' ) &
!                        n_Input_Profiles, n_Profiles
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              WARNING,         &
!                              Message_Log = Message_Log )
!      END IF
!    END IF


    ! -- Check that the number of profiles is not greater than
    ! -- MAX_N_PROFILES. This is simply a limit to restrict the
    ! -- size of the input arrays so they're not TOO big.

    IF ( n_Profiles > MAX_N_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Value_Input,   '( i5 )' ) n_Profiles
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_PROFILES
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of passed profiles ('// &
                            TRIM( ADJUSTL( Value_Input ) )// &
                            ') > maximum number of profiles allowed ('// &
                            TRIM( ADJUSTL( Value_Allowed ) )//').', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    END IF


    ! -- Check the profile dimensionality
    ! -- of the other arguments

    IF ( SIZE( Surface ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Surface input argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    
    IF ( SIZE( GeometryInfo ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'GeomtryInfo input argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( SIZE( RTSolution, 2 ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'RTSolution output argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    Options_Present = .FALSE.
    IF ( PRESENT( Options ) ) THEN
      Options_Present = .TRUE.
      IF ( SIZE( Options ) /= n_Profiles ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Inconsistent profile dimensionality for '//&
                              'Options optional input argument.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF
    



    !#--------------------------------------------------------------------------#
    !#                           -- PROFILE LOOP --                             #
    !#                                                                          #
    !#  Note that there are two loops, one with the Options argument, and       #
    !#  one without. This is done so that, in the former case, the Options      #
    !#  argument can be indexed with the profile index variable, m, without     #
    !#  an IF(PRESENT(Options)) test inside the profile loop.                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Loop for Options argument
    ! -------------------------

    Options_Check: IF ( Options_Present ) THEN

      DO m = 1, n_Profiles
        Error_Status = CRTM_Forward_scalar( Atmosphere(m),            &  ! Input, Scalar
                                            Surface(m),               &  ! Input, Scalar
                                            GeometryInfo(m),          &  ! Input, Scalar
                                            ChannelInfo,              &  ! Input, Scalar
                                            RTSolution(:,m),          &  ! Output, L   
                                            Options = Options(m),     &  ! Optional input, Scalar
                                            Message_Log = Message_Log )  ! Error messaging
        IF ( Error_Status == FAILURE ) THEN
          WRITE( Message, '( "Error occured in CRTM_Forward(Scalar), ", &
                            &"with Options, for profile #", i5 )' ) m
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF
      END DO


    ! ----------------------------
    ! Loop for no Options argument
    ! ----------------------------

    ELSE

      DO m = 1, n_Profiles
        Error_Status = CRTM_Forward_scalar( Atmosphere(m),            &  ! Input, Scalar
                                            Surface(m),               &  ! Input, Scalar
                                            GeometryInfo(m),          &  ! Input, Scalar
                                            ChannelInfo,              &  ! Input, Scalar
                                            RTSolution(:,m),          &  ! Output, L   
                                            Message_Log = Message_Log )  ! Error messaging
        IF ( Error_Status == FAILURE ) THEN
          WRITE( Message, '( "Error occured in CRTM_Forward(Scalar), ", &
                            &"for profile #", i5 )' ) m
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF
      END DO

    END IF Options_Check

  END FUNCTION CRTM_Forward_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Forward_scalar( Atmosphere,   &  ! Input, Scalar
                                Surface,      &  ! Input, Scalar
                                GeometryInfo, &  ! Input, Scalar
                                ChannelInfo,  &  ! Input, Scalar    
                                RTSolution,   &  ! Output, L   
                                Options,      &  ! Optional input, Scalar    
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),                 INTENT( IN OUT ) :: Atmosphere    ! Scalar
    TYPE( CRTM_Surface_type ),                    INTENT( IN )     :: Surface       ! Scalar
    TYPE( CRTM_GeometryInfo_type ),               INTENT( IN OUT ) :: GeometryInfo  ! Scalar
    TYPE( CRTM_ChannelInfo_type ),                INTENT( IN )     :: ChannelInfo   ! Scalar

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution    ! L

    ! -- Optional input
    TYPE( CRTM_Options_type ),    OPTIONAL,       INTENT( IN )     :: Options       ! Scalar

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Forward(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status
    INTEGER :: l, n_Full_Streams, n_Layers
    TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type )    :: AerosolScatter
    TYPE( CRTM_AtmScatter_type )    :: CloudScatter
    TYPE( CRTM_AtmScatter_type )    :: AtmOptics 
    TYPE( CRTM_SfcOptics_type )     :: SfcOptics

    ! -- Internal variable output
    TYPE( CRTM_AOVariables_type ) :: AOV  ! AtmOptics
    TYPE( CRTM_RTVariables_type ) :: RTV  ! RTSolution



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
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
    !#            -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --              #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Check the number of channels
    ! ----------------------------

    ! -- If no channels, simply return
    IF ( ChannelInfo%n_Channels == 0 ) RETURN

    ! -- Output array too small
    IF ( SIZE( RTSolution ) < ChannelInfo%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output RTSolution structure array too small (", i5, &
                        &") to hold results for the number of requested channels (", i5, ")" )' ) &
                      SIZE( RTSolution ), ChannelInfo%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- CHECK THE OPTIONAL Options STRUCTURE ARGUMENT --           #
    !#                                                                          #
    !# Note the use of the "User_Emissivity" logical flag in this seciton is    #
    !# not intended as the generic "user options are present" flag. It is       #
    !# anticipated that future additions to the Options structure argument will #
    !# necessitate additional flag variables.                                   #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------
    ! Default action is NOT to use user specified Options
    ! ---------------------------------------------------

    User_Emissivity = .FALSE.


    ! --------------------------
    ! Check the Options argument
    ! --------------------------

    Options_Present: IF ( PRESENT( Options ) ) THEN

      ! -- Check if the supplied emissivity should be used
      Check_Emissivity: IF ( Options%Emissivity_Switch == SET ) THEN

        ! -- Are the channel dimensions consistent
        IF ( Options%n_Channels < ChannelInfo%n_Channels ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Input Options channel dimension (", i5, ") is less ", &
                            &"than the number of requested channels (",i5, ")" )' ) &
                          Options%n_Channels, ChannelInfo%n_Channels
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

        ! -- Set to use the supplied emissivity
        User_Emissivity = .TRUE.

        ! -- Check if the supplied direct reflectivity should be used
        User_Direct_Reflectivity = .FALSE.
        IF ( Options%Direct_Reflectivity_Switch == SET ) User_Direct_Reflectivity = .TRUE.

      END IF Check_Emissivity

    END IF Options_Present



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE DERIVED GEOMETRY FROM THE USER SPECIFIED --       #
    !#         -- COMPONENTS OF THE CRTM_GeometryInfo STRUCTURE        --       #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo, &
                                              Message_Log = Message_Log )


    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing derived GeometryInfo components', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- ALLOCATE ALL LOCAL STRUCTURES --                  #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! The AtmAbsorption structure
    ! ---------------------------

    Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                                MAX_N_PREDICTORS,         &  ! Input
                                                MAX_N_ABSORBERS,          &  ! Input
                                                AtmAbsorption,            &  ! Output
                                                Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------
    ! The CloudScatter structure
    ! --------------------------

    Error_Status = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                             MAX_N_LEGENDRE_TERMS,     &  ! Input
                                             MAX_N_PHASE_ELEMENTS,     &  ! Input
                                             CloudScatter,             &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating CloudScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! The AerosolScatter structure
    ! ----------------------------

    Error_Status = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                             MAX_N_LEGENDRE_TERMS,     &  ! Input
                                             MAX_N_PHASE_ELEMENTS,     &  ! Input
                                             AerosolScatter,           &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AerosolScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! -----------------------
    ! The AtmOptics structure
    ! -----------------------

    Error_Status = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                             MAX_N_LEGENDRE_TERMS,     &  ! Input
                                             MAX_N_PHASE_ELEMENTS,     &  ! Input
                                             AtmOptics,                &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

 
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating CloudScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------
    ! The SfcOptics structure
    ! -----------------------

    Error_Status = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,             &  ! Input
                                            MAX_N_STOKES,             &  ! Input
                                            SfcOptics,                &  ! Output
                                            Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating SfcOptics structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- PREPROCESS SOME INPUT DATA --                     #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------------------------
    ! Average surface skin temperature for multi-surface types
    ! --------------------------------------------------------

    CALL CRTM_Compute_SurfaceT( Surface, SfcOptics )



    !#--------------------------------------------------------------------------#
    !#                -- SET UP FOR GASEOUS ABSORPTION CALCS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_SetUp_AtmAbsorption( Atmosphere,               &  ! Input
                                             GeometryInfo,             &  ! Input
                                             AtmAbsorption,            &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error setting up AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- CHANNEL LOOP --                             #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels


      ! --------------------------------
      ! Compute the layer optical depths
      ! due to gaseous absorption
      ! --------------------------------

      Error_Status = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), & ! Input
                                                 AtmAbsorption,                & ! In/Output
                                                 Message_Log = Message_Log     ) ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing AtmAbsorption for ", a, ", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
!          OR
!        CYCLE Channel_Loop
      END IF


      ! ---------------------------------------------------------------
      ! Determine the number of streams (n_Full_Streams) in up+downward
      ! directions. Currently, n_Full_Streams is determined from the
      ! cloud parameters only. It will also use the aerosol parameters 
      ! when aerosol scattering is included.
      ! ---------------------------------------------------------------

      CALL CRTM_Compute_n_Streams( Atmosphere, &
                                   ChannelInfo%Channel_Index(l), &
                                   n_Full_Streams, &
                                   RTSolution(l) )

      ! -- Transfer the number of streams
      ! -- to all the scattering structures
      AtmOptics%n_Legendre_Terms      = n_Full_Streams

      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------

      IF( Atmosphere%n_Clouds > 0 ) THEN

        CloudScatter%n_Legendre_Terms   = n_Full_Streams
        Error_Status = CRTM_Compute_CloudScatter( Atmosphere, &
                                                  ChannelInfo%Channel_Index(l), &
                                                  CloudScatter )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error computing CloudScatter for ", a, &
                            &", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ----------------------------------------------------
      ! Compute the aerosol absorption/scattering properties
      ! ----------------------------------------------------

      IF ( Atmosphere%n_Aerosols > 0 ) THEN

        AerosolScatter%n_Legendre_Terms = n_Full_Streams
        Error_Status = CRTM_Compute_AerosolScatter( Atmosphere,                   & ! Input
                                                    GeometryInfo,                 & ! Input
                                                    ChannelInfo%Channel_Index(l), & ! Input
                                                    AerosolScatter,               & ! In/Output
                                                    Message_Log = Message_Log     ) ! Error messaging

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error computing AerosolScatter for ", a, ", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ---------------------------------------------------
      ! Compute the combined atmospheric optical properties
      ! ---------------------------------------------------

      CALL CRTM_Combine_AtmOptics( AtmAbsorption,  & ! Input
                                   CloudScatter,   & ! Input
                                   AerosolScatter, & ! Input
                                   AtmOptics,      & ! Output
                                   AOV             ) ! Internal variable output


      ! ------------------------------------
      ! Fill the SfcOptics structure for the
      ! optional emissivity input case.
      ! ------------------------------------

      ! -- Indicate SfcOptics ARE to be computed
      SfcOptics%Compute_Switch = SET

      ! -- Change SfcOptics emissivity/reflectivity
      ! -- contents/computation status
      IF ( User_Emissivity ) THEN
        SfcOptics%Compute_Switch  = NOT_SET

        SfcOptics%Emissivity(1,1)       = Options%Emissivity(l)
        SfcOptics%Reflectivity(1,1,1,1) = ONE - Options%Emissivity(l)

        IF ( User_Direct_Reflectivity ) THEN
          SfcOptics%Direct_Reflectivity(1,1) = Options%Direct_Reflectivity(l)
        ELSE
          SfcOptics%Direct_Reflectivity(1,1) = SfcOptics%Reflectivity(1,1,1,1)
        END IF

      END IF


      ! ------------------------------------
      ! Solve the radiative transfer problem
      ! ------------------------------------

      Error_Status = CRTM_Compute_RTSolution( Atmosphere,                   &  ! Input
                                              Surface,                      &  ! Input
                                              AtmOptics,                    &  ! Input
                                              SfcOptics,                    &  ! Input
                                              GeometryInfo,                 &  ! Input
                                              ChannelInfo%Channel_Index(l), &  ! Input
                                              RTSolution(l),                &  ! Output
                                              RTV,                          &  ! Internal variable output
                                              Message_Log = Message_Log     )  ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing RTSolution for ", a, &
                          &", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
!          OR
!        CYCLE Channel_Loop
      END IF

    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#                    -- DEALLOCATE LOCAL STRUCTURES --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! The SfcOptics structure
    ! -----------------------

    Status = CRTM_Destroy_SfcOptics( SfcOptics )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating SfcOptics structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -----------------------
    ! The AtmOptics structure
    ! -----------------------

    Status = CRTM_Destroy_AtmScatter( AtmOptics )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmOptics structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------------
    ! The AerosolScatter structure
    ! ----------------------------

    Status = CRTM_Destroy_AtmScatter( AerosolScatter )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AerosolScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! --------------------------
    ! The CloudScatter structure
    ! --------------------------

    Status = CRTM_Destroy_AtmScatter( CloudScatter )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating CloudScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ---------------------------
    ! The AtmAbsorption structure
    ! ---------------------------

    Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Forward_scalar

END MODULE CRTM_Forward_Module

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Forward_Module.f90,v 1.14.2.10 2005/10/11 21:25:24 qliu Exp $
!
! $Date: 2005/10/11 21:25:24 $
!
! $Revision: 1.14.2.10 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Forward_Module.f90,v $
! Revision 1.14.2.10  2005/10/11 21:25:24  qliu
! - Corrected bug in AOV variable definition.
!
! Revision 1.14.2.9  2005/10/11 18:55:13  paulv
! - Updated header documentation.
! - Removed USE of CRTM_CloudCoeff module - not required.
! - Added AOV internal variable for AtmOptics module.
!
! Revision 1.14.2.8  2005/10/06 21:27:33  qliu
! -- Added the local structure RTV and revised the call interface for RTSolution.
!
! Revision 1.14.2.7  2005/10/04 20:34:57  paulv
! - Replaced options present logical variable with specific options content
!   logical switches.
! - Now testing for user emissivity and direct reflectivity with respect to
!   the optional Options argument.
! - Filling the SfcOptics structure as needed according to the contents of the
!   optional Options argument.
!
! Revision 1.14.2.6  2005/09/27 02:03:14  paulv
! - Started modifications for Options optional input argument. Incomplete.
!
! Revision 1.14.2.5  2005/09/12 13:47:46  qliu
! -- Modification for fitting CRTM_Interpolation.
!
! Revision 1.14.2.4  2005/08/24 12:34:13  qliu
! -- Deleted unused variables.
!
! Revision 1.14.2.3  2005/08/23 22:04:26  qliu
! -- Deleted unused variable.
!
! Revision 1.14.2.2  2005/08/23 21:52:51  qliu
! -- Deleted unused variable.
!
! Revision 1.14.2.1  2005/08/19 20:29:31  qliu
! -- first working version
!
! Revision 1.14  2005/02/25 17:53:05  paulv
! - Updated scattering application routines, CloudScatter and AerosolScatter,
!   now being used instead of AtmScatter and Aerosol respectively. Both the
!   cloud particle and aerosol scattering structures are now the same type,
!   CRTM_AtmScatter_type. All the argument to the various function have
!   been updated to reflect this change.
!
! Revision 1.13  2005/02/18 23:17:56  paulv
! - Added Aerosol capability. RTSolution interfaces altered.
!
! Revision 1.12  2005/02/16 22:45:07  paulv
! - Added hooks for implementation of Aerosol absorption and scattering.
!   Right now, all of the Aerosol stuff is commented out awaiting a full
!   definition of the Aerosol structure.
!
! Revision 1.11  2005/02/16 15:29:23  paulv
! - Updated header documentation.
! - Started adding hooks for aerosol absorption and scattering function calls.
!   Not implemented yet as Aerosol structure definition is not yet mature enough.
!
! Revision 1.10  2005/02/01 15:53:44  paulv
! - Updated local structure allocation.
!
! Revision 1.9  2005/01/28 21:34:56  paulv
! - The module procedure interface block is now named in f95-style; with the
!   interface name also on the END INTERFACE line.
!
! Revision 1.8  2005/01/28 21:25:46  paulv
! - Added optional RCS_Id argument.
! - Updated header documentation.
! - Changed INTENT of RTSolution argument from OUT to IN OUT to prevent
!   memory leaks.
!
! Revision 1.7  2004/11/05 16:17:04  paulv
! - Removed all Init() routine calls.
!
! Revision 1.6  2004/08/06 18:59:33  paulv
! - Updated header documentation.
!
! Revision 1.5  2004/07/21 15:55:30  paulv
! - Added destroy function call for local AtmAbsorption structure in scalar
!   function.
!
! Revision 1.4  2004/07/02 21:12:06  paulv
! - Added some debug output statements.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/07/01 14:55:03  paulv
! - Completed routines using new interface routines. Tested to compilation.
!
! Revision 1.1  2004/06/04 19:37:21  paulv
! Initial checkin. Incomplete.
!
!
!
!
