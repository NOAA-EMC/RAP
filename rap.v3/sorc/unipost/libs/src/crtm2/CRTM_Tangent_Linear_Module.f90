!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Tangent_Linear_Module
!
! PURPOSE:
!       Module containing the CRTM tangent-linear model function.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Tangent_Linear_Module
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
!
! CONTAINS:
!       CRTM_Tangent_Linear:  Function that calculates top-of-atmosphere (TOA)
!                             tangent-linear radiances and brightness temperatures
!                             for an input atmospheric profile or profile set and
!                             user specified satellites/channels.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2005
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

MODULE CRTM_Tangent_Linear_Module


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
  PUBLIC :: CRTM_Tangent_Linear


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE CRTM_Tangent_Linear
    MODULE PROCEDURE CRTM_Tangent_Linear_scalar
    MODULE PROCEDURE CRTM_Tangent_Linear_rank1
  END INTERFACE CRTM_Tangent_Linear


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Tangent_Linear_Module.f90,v 1.7.2.10 2005/10/20 19:58:31 paulv Exp $'

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
!       CRTM_Tangent_Linear
!
! PURPOSE:
!       Function that calculates tangent-linear top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric
!       profile or profile set and user specified satellites/channels.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Tangent_Linear( Atmosphere,               &  ! FWD Input
!                                           Surface,                  &  ! FWD Input
!                                           Atmosphere_TL,            &  ! TL  Input
!                                           Surface_TL,               &  ! TL  Input
!                                           GeometryInfo,             &  ! Input
!                                           ChannelInfo,              &  ! Input
!                                           RTSolution,               &  ! FWD Output
!                                           RTSolution_TL,            &  ! TL  Output
!                                           Options     = Options,    &  ! Optional FWD input
!                                           RCS_Id      = RCS_Id,     &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:  Structure containing the tangent-linear Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface_TL:     Structure containing the tangent-linear Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                       ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_GeometryInfo_type )
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
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
!       Options:        Options structure containing the optional forward model
!                       arguments for the CRTM.
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
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       RTSolution_TL:  Structure containing the solution to the tangent-
!                       linear RT equation for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
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
!      CRTM_Allocate_AtmAbsorption:    Function to allocate AtmAbsorption
!                                      data structures.
!                                      SOURCE: CRTM_ATMABSORPTION_DEFINE module
!
!      CRTM_Allocate_AtmScatter:       Function to allocate AtmScatter data
!                                      structures.
!                                      SOURCE: CRTM_ATMSCATTER_DEFINE module
!
!      CRTM_Allocate_SfcOptics:        Function to allocate SfcOptics data
!                                      structures.
!                                      SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!      CRTM_SetUp_AtmAbsorption:       Function to prepare the AtmAbsorption
!                                      structure for gaseous absorption calculations.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_SetUp_AtmAbsorption_TL:    Function to prepare the tangent-linear
!                                      AtmAbsorption structure for gaseous absorption
!                                      calculations.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AtmAbsorption:     Function to compute optical depths due
!                                      to gaseuos absorption.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AtmAbsorption_TL:  Function to compute tangent-linear optical
!                                      depths due to gaseous absorption.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AerosolScatter:    Function to compute aerosol absorption
!                                      and scattering properties.
!                                      SOURCE: CRTM_AEROSOLSCATTER module
!
!      CRTM_Compute_AerosolScatter_TL: Function to compute tangent-linear aerosol
!                                      absorption and scattering properties.
!                                      SOURCE: CRTM_AEROSOLSCATTER module
!
!      CRTM_Compute_CloudScatter:      Function to compute cloud particle absorption
!                                      and scattering properties.
!                                      SOURCE: CRTM_CLOUDSCATTER module
!
!      CRTM_Compute_CloudScatter_TL:   Function to compute tangent-linear cloud
!                                      particle absorption and scattering properties.
!                                      SOURCE: CRTM_CLOUDSCATTER module
!
!      CRTM_Compute_SfcOptics:         Function to compute surface emissivities
!                                      and reflectivities.
!                                      SOURCE: CRTM_SFCOPTICS module
!
!      CRTM_Compute_SfcOptics_TL:      Function to compute tangent-linear surface
!                                      emissivities and reflectivities.
!                                      SOURCE: CRTM_SFCOPTICS module
!
!      CRTM_Compute_RTSolution:        Function to solve the radiative transfer
!                                      equation.
!                                      SOURCE: CRTM_RTSOLUTION module
!
!      CRTM_Compute_RTSolution_TL:     Function to solve the tangent-linear radiative
!                                      transfer equation.
!                                      SOURCE: CRTM_RTSOLUTION module
!
!      Display_Message:                Subroutine to output messages
!                                      SOURCE: ERROR_HANDLER module
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
!                                              |   OPTIONAL   |
!                  INPUTS                      |    INPUT     |   OUTPUTS
!                                              |              |
!     Atmosphere     Surface    GeometryInfo   |   Options    |   RTSolution
!    Atmosphere_TL  Surface_TL                 |              |  RTSolution_TL
!  --------------------------------------------+--------------+----------------
!       Scalar       Scalar        Scalar      |    Scalar    |      L
!                                              |              |
!         M            M             M         |      M       |    L x M
!
!         Thus one can process either a single profile or multiple profiles.
!         The routines for each specific case above have been overloaded to
!         the generic interface described in the header above.
!
!       - Note that the Options optional input structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the output
!         RTSolution structures.
!
!       - Note the INTENT on the output RTSolution arguments are IN OUT rather
!         than just OUT. This is necessary because the arguments may be defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------


  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Tangent_Linear_rank1( Atmosphere,    &  ! FWD Input, M
                                      Surface,       &  ! FWD Input, M
                                      Atmosphere_TL, &  ! TL  Input, M
                                      Surface_TL,    &  ! TL  Input, M
                                      GeometryInfo,  &  ! Input, M
                                      ChannelInfo,   &  ! Input, Scalar  
                                      RTSolution,    &  ! FWD Output, L x M  
                                      RTSolution_TL, &  ! TL  Output, L x M  
                                      Options,       &  ! Optional FWD input, M
                                      RCS_Id,        &  ! Revision control
                                      Message_Log )  &  ! Error messaging
                                    RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( : ),   INTENT( IN OUT ) :: Atmosphere    ! M
    TYPE( CRTM_Surface_type ),           DIMENSION( : ),   INTENT( IN )     :: Surface       ! M
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( : ),   INTENT( IN OUT)  :: Atmosphere_TL ! M
    TYPE( CRTM_Surface_type ),           DIMENSION( : ),   INTENT( IN )     :: Surface_TL    ! M
    TYPE( CRTM_GeometryInfo_type ),      DIMENSION( : ),   INTENT( IN OUT ) :: GeometryInfo  ! M
    TYPE( CRTM_ChannelInfo_type ),                         INTENT( IN )     :: ChannelInfo   ! Scalar

    ! -- Output
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: RTSolution    ! L x M
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: RTSolution_TL ! L x M

    ! -- Optional input
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ),   INTENT( IN )     :: Options       ! M

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,                   INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),            OPTIONAL,                   INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Tangent_Linear(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed
    LOGICAL :: Options_Present
    INTEGER :: Status_TL
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

    IF ( SIZE( Atmosphere_TL ) /= n_Profiles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Atmosphere input argument.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


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

    IF ( SIZE( Surface )    /= n_Profiles .OR. &
         SIZE( Surface_TL ) /= n_Profiles    ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'Surface input argument(s).', &
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

    IF ( SIZE( RTSolution,    2 ) /= n_Profiles .OR. &
         SIZE( RTSolution_TL, 2 ) /= n_Profiles      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent profile dimensionality for '//&
                            'RTSolution output argument(s).', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check the profile dimensionality
    ! -- of the FWD optional argument

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

    Options_Check: IF ( Options_Present ) THEN


      ! -------------------------
      ! Loop for Options argument
      ! -------------------------

      DO m = 1, n_Profiles
        Status_TL = CRTM_Tangent_Linear_scalar( Atmosphere(m),            &  ! Input, Scalar
                                                Surface(m),               &  ! Input, Scalar
                                                Atmosphere_TL(m),         &  ! Input, Scalar
                                                Surface_TL(m),            &  ! Input, Scalar
                                                GeometryInfo(m),          &  ! Input, Scalar
                                                ChannelInfo,              &  ! Input, Scalar
                                                RTSolution(:,m),          &  ! Output, L   
                                                RTSolution_TL(:,m),       &  ! Output, L   
                                                Options = Options(m),     &  ! Optional input, Scalar
                                                Message_Log = Message_Log )  ! Error messaging
        IF ( Status_TL /= SUCCESS ) THEN
          Error_Status = Status_TL
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_Tangent_Linear(Scalar,", &
                              &"with Options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF
        END IF
      END DO


    ELSE


      ! ----------------------------
      ! Loop for no Options argument
      ! ----------------------------

      DO m = 1, n_Profiles
        Status_TL = CRTM_Tangent_Linear_scalar( Atmosphere(m),            &  ! Input, Scalar
                                                Surface(m),               &  ! Input, Scalar
                                                Atmosphere_TL(m),         &  ! Input, Scalar
                                                Surface_TL(m),            &  ! Input, Scalar
                                                GeometryInfo(m),          &  ! Input, Scalar
                                                ChannelInfo,              &  ! Input, Scalar
                                                RTSolution(:,m),          &  ! Output, L   
                                                RTSolution_TL(:,m),       &  ! Output, L
                                                Message_Log = Message_Log )  ! Error messaging
        IF ( Status_TL /= SUCCESS ) THEN
          Error_Status = Status_TL
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_Tangent_Linear(Scalar,", &
                              &"no Options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF
        END IF
      END DO

    END IF Options_Check

  END FUNCTION CRTM_Tangent_Linear_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Tangent_Linear_scalar( Atmosphere,    &  ! Input, Scalar
                                       Surface,       &  ! Input, Scalar
                                       Atmosphere_TL, &  ! Input, Scalar
                                       Surface_TL,    &  ! Input, Scalar
                                       GeometryInfo,  &  ! Input, Scalar
                                       ChannelInfo,   &  ! Input, Scalar  
                                       RTSolution,    &  ! Output, L   
                                       RTSolution_TL, &  ! Output, L   
                                       Options,       &  ! Optional FWD input, Scalar
                                       RCS_Id,        &  ! Revision control
                                       Message_Log )  &  ! Error messaging
                                     RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),                 INTENT( IN OUT)  :: Atmosphere
    TYPE( CRTM_Surface_type ),                    INTENT( IN )     :: Surface
    TYPE( CRTM_Atmosphere_type ),                 INTENT( IN OUT ) :: Atmosphere_TL
    TYPE( CRTM_Surface_type ),                    INTENT( IN )     :: Surface_TL
    TYPE( CRTM_GeometryInfo_type ),               INTENT( IN OUT)  :: GeometryInfo
    TYPE( CRTM_ChannelInfo_type ),                INTENT( IN )     :: ChannelInfo

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution    ! L
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution_TL ! L

    ! -- Optional input
    TYPE( CRTM_Options_type ), OPTIONAL,          INTENT( IN )     :: Options

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,          INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),            OPTIONAL,          INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Tangent_Linear(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status_FWD, Status_TL
    INTEGER :: l, n_Full_Streams, n_Layers
    TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption,  AtmAbsorption_TL
    TYPE( CRTM_AtmScatter_type )    :: AerosolScatter, AerosolScatter_TL
    TYPE( CRTM_AtmScatter_type )    :: CloudScatter,   CloudScatter_TL
    TYPE( CRTM_AtmScatter_type )    :: AtmOptics,      AtmOptics_TL
    TYPE( CRTM_SfcOptics_type )     :: SfcOptics,      SfcOptics_TL

    ! -- Internal variables
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
    !#           -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --               #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Check the number of channels
    ! ----------------------------

    ! -- If no channels, simply return
    IF ( ChannelInfo%n_Channels == 0 ) RETURN

    ! -- Output array too small
    IF ( SIZE( RTSolution    ) < ChannelInfo%n_Channels .OR. &
         SIZE( RTSolution_TL ) < ChannelInfo%n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output RTSolution structure arrays too small (", i5, 1x, i5, &
                        &") to hold results for the number of requested channels (", i5, ")" )' ) &
                      SIZE( RTSolution ), SIZE( RTSolution_TL ), ChannelInfo%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- CHECK THE OPTIONAL STRUCTURE ARGUMENTS --                #
    !#                                                                          #
    !# Note the use of the "User_Emissivity" logical flag in this section is    #
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

    ! ----------------------------
    ! The AtmAbsorption structures
    ! ----------------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                              MAX_N_PREDICTORS,         &  ! Input
                                              MAX_N_ABSORBERS,          &  ! Input
                                              AtmAbsorption,            &  ! Output
                                              Message_Log = Message_Log )  ! Error messaging

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                             MAX_N_PREDICTORS,         &  ! Input
                                             MAX_N_ABSORBERS,          &  ! Input
                                             AtmAbsorption_TL,         &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmAbsorption structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           CloudScatter,             &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          CloudScatter_TL,          &  ! Output
                                          Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating CloudScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           AerosolScatter,           &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          AerosolScatter_TL,        &  ! Output
                                          Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AerosolScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! -----------------------
    ! The AtmOptics structure
    ! -----------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           AtmOptics,                &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                          MAX_N_LEGENDRE_TERMS,     &  ! Input
                                          MAX_N_PHASE_ELEMENTS,     &  ! Input
                                          AtmOptics_TL,             &  ! Output
                                          Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmOptics structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------
    ! The SfcOptics structure
    ! -----------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,             &  ! Input
                                          MAX_N_STOKES,             &  ! Input
                                          SfcOptics,                &  ! Output
                                          Message_Log = Message_Log )  ! Error messaging

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,             &  ! Input
                                         MAX_N_STOKES,             &  ! Input
                                         SfcOptics_TL,             &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating SfcOptics structures', &
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
    CALL CRTM_Compute_SurfaceT_TL( Surface, Surface_TL, SfcOptics_TL )



    !#--------------------------------------------------------------------------#
    !#                -- SET UP FOR GASEOUS ABSORPTION CALCS --                 #
    !#--------------------------------------------------------------------------#

    ! -- Forward
    Status_FWD = CRTM_SetUp_AtmAbsorption( Atmosphere,               &  ! Input
                                           GeometryInfo,             &  ! Input
                                           AtmAbsorption,            &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! -- Tangent-linear
    Status_TL = CRTM_SetUp_AtmAbsorption_TL( Atmosphere,               &  ! Input
                                             AtmAbsorption,            &  ! Input
                                             Atmosphere_TL,            &  ! Input
                                             GeometryInfo,             &  ! Input
                                             AtmAbsorption_TL,         &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error setting up AtmAbsorption structures', &
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

      ! -- Forward
      Status_FWD = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                               AtmAbsorption,                &  ! In/Output
                                               Message_Log = Message_Log     )  ! Error messaging

      ! -- Tangent-linear
      Status_TL = CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l), &  ! Input
                                                 AtmAbsorption,                &  ! Input
                                                 AtmAbsorption_TL,             &  ! In/Output
                                                 Message_Log = Message_Log     )  ! Error messaging


      IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
        Error_Status = FAILURE
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
      AtmOptics_TL%n_Legendre_Terms   = n_Full_Streams
      CloudScatter%n_Legendre_Terms   = n_Full_Streams
      AerosolScatter%n_Legendre_Terms = n_Full_Streams


      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------

      IF( Atmosphere%n_Clouds > 0 ) THEN

        ! -- Forward
        Status_FWD = CRTM_Compute_CloudScatter( Atmosphere,                   &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input
                                                CloudScatter                  )  ! Output

        ! -- Tangent-linear
        Status_TL = CRTM_Compute_CloudScatter_TL( Atmosphere,                   &  ! Input
                                                  CloudScatter,                 &  ! Input 
                                                  Atmosphere_TL,                &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  CloudScatter_TL               )  ! Output

        IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing CloudScatter for ", a, &
                            &", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
!            OR
!          CYCLE Channel_Loop
        END IF

      ENDIF


      ! ----------------------------------------------------
      ! Compute the aerosol absorption/scattering properties
      ! ----------------------------------------------------

      IF ( Atmosphere%n_Aerosols > 0 ) THEN

        ! -- Forward
        Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere,                   &  ! Input
                                                  GeometryInfo,                 &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  AerosolScatter,               &  ! Output
                                                  Message_Log = Message_Log     )  ! Error messaging

        ! -- Tangent-linear
        Status_TL = CRTM_Compute_AerosolScatter_TL( Atmosphere,                   &  ! Input
                                                    AerosolScatter,               &  ! Input
                                                    Atmosphere_TL,                &  ! Input
                                                    GeometryInfo,                 &  ! Input
                                                    ChannelInfo%Channel_Index(l), &  ! Input
                                                    AerosolScatter_TL,            &  ! Output
                                                    Message_Log = Message_Log     )  ! Error messaging

        IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error computing AerosolScatter for ", a, ", channel ", i4 )' ) &
                          TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                          ChannelInfo%Sensor_Channel(l)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
!            OR
!          CYCLE Channel_Loop
        END IF

      END IF


      ! ---------------------------------------------------
      ! Compute the combined atmospheric optical properties
      ! ---------------------------------------------------

      ! -- Forward
      CALL CRTM_Combine_AtmOptics( AtmAbsorption,  & ! Input
                                   CloudScatter,   & ! Input
                                   AerosolScatter, & ! Input
                                   AtmOptics,      & ! Output
                                   AOV             ) ! Internal variable output


      ! -- Tangent-linear
      CALL CRTM_Combine_AtmOptics_TL( AtmAbsorption,     & ! FWD Input
                                      CloudScatter,      & ! FWD Input
                                      AerosolScatter,    & ! FWD Input
                                      AtmOptics,         & ! FWD Input
                                      AtmAbsorption_TL,  & ! TL  Input
                                      CloudScatter_TL,   & ! TL  Input
                                      AerosolScatter_TL, & ! TL  Input
                                      AtmOptics_TL,      & ! TL  Output
                                      AOV                ) ! Internal variable input


      ! -------------------------------------
      ! Fill the SfcOptics structures for the
      ! optional emissivity input case.
      ! -------------------------------------

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

      ! -- Forward model
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
        WRITE( Message, '( "Error computing CRTM_Compute_RTSolution for ", a, &
                          &", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! -- Tangent-linear models
      Error_Status = CRTM_Compute_RTSolution_TL( Atmosphere,                   &  ! Input
                                                 Surface,                      &  ! Input
                                                 AtmOptics,                    &  ! Input
                                                 SfcOptics,                    &  ! Input/Output
                                                 RTSolution(l),                &  ! Input
                                                 Atmosphere_TL,                &  ! Input
                                                 Surface_TL,                   &  ! Input
                                                 AtmOptics_TL,                 &  ! Input
                                                 SfcOptics_TL,                 &  ! Input
                                                 GeometryInfo,                 &  ! Input
                                                 ChannelInfo%Channel_Index(l), &  ! Input
                                                 RTSolution_TL(l),             &  ! Output
                                                 RTV,                          &  ! Internal variable input
                                                 Message_Log = Message_Log     )  ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing CRTM_Compute_RTSolution_TL for ", a, &
                          &", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#                    -- DEALLOCATE LOCAL STRUCTURES --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! The SfcOptics structures
    ! ------------------------

    Status_FWD = CRTM_Destroy_SfcOptics( SfcOptics )
    Status_TL  = CRTM_Destroy_SfcOptics( SfcOptics_TL )

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating SfcOptics structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------
    ! The AtmOptics structures
    ! ------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( AtmOptics )
    Status_TL  = CRTM_Destroy_AtmScatter( AtmOptics_TL )

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmOptics structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( AerosolScatter )
    Status_TL  = CRTM_Destroy_AtmScatter( AerosolScatter_TL )

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AerosolScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ---------------------------
    ! The CloudScatter structures
    ! ---------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( CloudScatter )
    Status_TL  = CRTM_Destroy_AtmScatter( CloudScatter_TL )

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------------
    ! The AtmAbsorption structures
    ! ----------------------------

    Status_FWD = CRTM_Destroy_AtmAbsorption( AtmAbsorption )
    Status_TL  = CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )

    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Tangent_Linear_scalar

END MODULE CRTM_Tangent_Linear_Module


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Tangent_Linear_Module.f90,v 1.7.2.10 2005/10/20 19:58:31 paulv Exp $
!
! $Date: 2005/10/20 19:58:31 $
!
! $Revision: 1.7.2.10 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Tangent_Linear_Module.f90,v $
! Revision 1.7.2.10  2005/10/20 19:58:31  paulv
! - Only forward model options arguments are now accepted.
!
! Revision 1.7.2.9  2005/10/11 21:27:00  qliu
! - Corrected bug in AOV variable definition.
!
! Revision 1.7.2.8  2005/10/11 19:04:48  paulv
! - Updated header documentation.
! - Added Options and Options_TL optional structure arguments.
! - Now testing for user FWD and TL emissivity and direct reflectivity
!   with respect to the Options and Options_TL arguments.
! - The profile loop and scalar function call in the rank-1 function was
!   replaced with a SELECT CASE statement containing profile loops and
!   scalar function calls for each permutation of the prescence of the
!   FWD Options and TL Options_TL optional arguments. This was done to
!   prevent a profile-independent test inside the profile loop and to allow
!   the optional arguments to be indexed as needed.
! - Filling the FWD and TL SfcOptics structures as needed according to the
!   contents of the optional Options and Options_TL arguments.
! - AtmOptics AOV internal variable structure.
!   o Added AOV internal variable type declaration
!   o Added AOV argument to FWD and TL AtmOptics calls.
! - RTSolution RTV internal variable structure.
!   o Added RTV internal variable type declaration
!   o Added RTV argument to FWD and TL RTSolution calls.
!
! Revision 1.7.2.7  2005/10/06 21:31:09  qliu
! -- Added the local structure RTV. Added CALL CRTM_Compute_RTSolution for forward and revised arguments
!    for CALL CRTM_Compute_RTSolution_TL.
!
! Revision 1.7.2.6  2005/09/15 14:34:25  qliu
! -- Added error status checking.
!
! Revision 1.7.2.5  2005/09/12 13:49:37  qliu
! -- Modification for fitting CRTM_Interpolation.
!
! Revision 1.7.2.4  2005/08/24 12:36:25  qliu
! -- Deleted unused variables.
!
! Revision 1.7.2.3  2005/08/23 22:03:15  qliu
! -- Deleted unused variable.
!
! Revision 1.7.2.2  2005/08/23 21:56:54  qliu
! -- Deleted unused variable.
!
! Revision 1.7.2.1  2005/08/23 21:33:54  qliu
! -- Deleted unused variable.
!
! Revision 1.7  2005/02/25 17:53:05  paulv
! - Updated scattering application routines, CloudScatter and AerosolScatter,
!   now being used instead of AtmScatter and Aerosol respectively. Both the
!   cloud particle and aerosol scattering structures are now the same type,
!   CRTM_AtmScatter_type. All the argument to the various function have
!   been updated to reflect this change.
!
! Revision 1.6  2005/02/18 23:17:56  paulv
! - Added Aerosol capability. RTSolution interfaces altered.
!
! Revision 1.5  2005/02/16 22:45:07  paulv
! - Added hooks for implementation of Aerosol absorption and scattering.
!   Right now, all of the Aerosol stuff is commented out awaiting a full
!   definition of the Aerosol structure.
!
! Revision 1.4  2005/02/16 15:29:23  paulv
! - Updated header documentation.
! - Started adding hooks for aerosol absorption and scattering function calls.
!   Not implemented yet as Aerosol structure definition is not yet mature enough.
!
! Revision 1.3  2005/02/01 15:55:14  paulv
! - Updated local structure allocation.
! - Replaced CRTM_SetUp_AtmAbsorption function calls with subroutine calls.
!
! Revision 1.2  2005/01/28 21:34:56  paulv
! - The module procedure interface block is now named in f95-style; with the
!   interface name also on the END INTERFACE line.
!
! Revision 1.1  2005/01/28 21:32:25  paulv
! Initial checkin.
!
!
!
!
