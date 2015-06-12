!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_K_Matrix_Module
!
! PURPOSE:
!       Module containing the CRTM K-matrix model function.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_K_Matrix_Module
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
!       CRTM_K_Matrix:  Function that calculates the K-matrix of top-of-atmosphere
!                       (TOA) radiances and brightness temperatures for an input
!                       atmospheric profile or profile set and user specified
!                       satellites/channels.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Jan-2005
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

MODULE CRTM_K_Matrix_Module


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
  PUBLIC :: CRTM_K_Matrix


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE CRTM_K_Matrix
    MODULE PROCEDURE CRTM_K_Matrix_scalar
    MODULE PROCEDURE CRTM_K_Matrix_rank1
  END INTERFACE CRTM_K_Matrix


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_K_Matrix_Module.f90,v 1.5.2.11 2005/10/20 19:58:31 paulv Exp $'

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
!       CRTM_K_Matrix
!
! PURPOSE:
!       Function that calculates the K-matrix of top-of-atmosphere (TOA)
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
!       Error_Status = CRTM_K_Matrix( Atmosphere,               &  ! FWD Input
!                                     Surface,                  &  ! FWD Input
!                                     RTSolution_K,             &  ! K   Input
!                                     GeometryInfo,             &  ! Input
!                                     ChannelInfo,              &  ! Input
!                                     Atmosphere_K,             &  ! K   Output
!                                     Surface_K,                &  ! K   Output
!                                     RTSolution,               &  ! FWD Output
!                                     Options     = Options,    &  ! Optional FWD input,  M
!                                     Options_K   = Options_K,  &  ! Optional K   output, M
!                                     RCS_Id      = RCS_Id,     &  ! Revision control
!                                     Message_Log = Message_Log )  ! Error messaging
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
!                       DIMENSION:  Same as input Atmosphere argument.
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       RTSolution_K:   Structure containing the RT solution K-matrix inputs.
!                       **NOTE: On EXIT from this function, the contents of
!                               this structure may be modified (e.g. set to
!                               zero.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-1 (L)
!                                     or
!                                   Rank-2 (L x M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_GeometryInfo_type )
!                       DIMENSION:  Same as input Atmosphere argument
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
!       Atmosphere_K:   Structure containing the K-matrix Atmosphere data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_K:      Structure containing the tangent-linear Surface data.
!                       **NOTE: On ENTRY to this function, the contents of
!                               this structure should be defined (e.g.
!                               initialized to some value based on the
!                               position of this function in the call chain.)
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Same as input RTSolution_K argument
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Options_K:      Options structure containing the optional K-matrix
!                       model arguments for the CRTM.
!                       **NOTE: Unlike the Atmosphere_K and Surface_K output
!                               K-matrix arguments, on ENTRY to this function,
!                               the contents of this structure are not used in
!                               computing the emissivity/direct reflectivity
!                               adjoints. This structure is simply a container
!                               to hold the adjoint result should the user request
!                               it.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Options_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
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
!      CRTM_SetUp_AtmAbsorption_AD:    Function to process the adjoint
!                                      AtmAbsorption structure for gaseous absorption
!                                      calculations.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AtmAbsorption:     Function to compute optical depths due
!                                      to gaseuos absorption.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AtmAbsorption_AD:  Function to compute the gaseous absorption
!                                      optical depth adjoints.
!                                      SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AerosolScatter:    Function to compute aerosol absorption
!                                      and scattering properties.
!                                      SOURCE: CRTM_AEROSOLSCATTER module
!
!      CRTM_Compute_AerosolScatter_AD: Function to compute the adjoint of the aerosol
!                                      absorption and scattering properties.
!                                      SOURCE: CRTM_AEROSOLSCATTER module
!
!      CRTM_Compute_CloudScatter:      Function to compute cloud particle absorption
!                                      and scattering properties.
!                                      SOURCE: CRTM_CLOUDSCATTER module
!
!      CRTM_Compute_CloudScatter_AD:   Function to compute the adjoint of the cloud
!                                      particle absorption and scattering properties.
!                                      SOURCE: CRTM_CLOUDSCATTER module
!
!      CRTM_Compute_SfcOptics:         Function to compute surface emissivities
!                                      and reflectivities.
!                                      SOURCE: CRTM_SFCOPTICS module
!
!      CRTM_Compute_SfcOptics_AD:      Function to compute the surface
!                                      emissivities and reflectivity adjoints.
!                                      SOURCE: CRTM_SFCOPTICS module
!
!      CRTM_Compute_RTSolution:        Function to solve the radiative transfer
!                                      equation.
!                                      SOURCE: CRTM_RTSOLUTION module
!
!      CRTM_Compute_RTSolution_AD:     Function to solve the adjoint radiative
!                                      transfer equation.
!                                      SOURCE: CRTM_RTSOLUTION module
!
!      Display_Message:                Subroutine to output messages
!                                      SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!      Note that the input K-matrix arguments are modified upon exit, and
!      the output K-matrix arguments must be defined upon entry. This is
!      a consequence of the K-matrix formulation where, effectively, the
!      chain rule is being used and this funtion could reside anywhere
!      in the chain of derivative terms.
!
! RESTRICTIONS:
!      None.
!
! COMMENTS:
!       - The folowing tables details the input/output argument dimensionality
!         association, where L == n_Channels, M == n_Profiles:
!
!                                            | OPTIONAL |
!                  INPUTS                    |  INPUT   |         OUTPUTS
!                                            |          |
!     Atmosphere  RTSolution_K  GeometryInfo | Options  | RTSolution    Atmosphere_K
!      Surface                               |          |                Surface_K
!  ------------------------------------------+----------+----------------------------
!       Scalar         L           Scalar    |  Scalar  |     L             Scalar
!                                            |          |
!         M          L x M           M       |    M     |   L x M             M
!
!         Thus one can process either a single profile or multiple profiles.
!         The routines for each specific case above have been overloaded to
!         the generic interface described in the header above.
!
!       - Note that the Options optional structure arguments contain
!         spectral information (e.g. emissivity) that must have the same
!         spectral dimensionality (the "L" dimension) as the RTSolution
!         structures.
!
!       - Note the INTENT on the output RTSolution, Atmosphere_K, and Surface_K,
!         arguments are IN OUT rather than just OUT. This is necessary because
!         the arguments should be defined upon input. To prevent memory leaks,
!         the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------


  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_K_Matrix_rank1( Atmosphere,    &  ! FWD Input, M
                                Surface,       &  ! FWD Input, M
                                RTSolution_K,  &  ! K   Input, L x M   
                                GeometryInfo,  &  ! Input, M
                                ChannelInfo,   &  ! Input, Scalar  
                                Atmosphere_K,  &  ! K   Output, L x M
                                Surface_K,     &  ! K   Output, L x M
                                RTSolution,    &  ! FWD Output, L x M
                                Options,       &  ! Optional FWD input,  M
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
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( : ),   INTENT( IN OUT)  :: Atmosphere   ! M
    TYPE( CRTM_Surface_type ),           DIMENSION( : ),   INTENT( IN )     :: Surface      ! M
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: RTSolution_K ! L x M
    TYPE( CRTM_GeometryInfo_type ),      DIMENSION( : ),   INTENT( IN OUT ) :: GeometryInfo ! M
    TYPE( CRTM_ChannelInfo_type ),                         INTENT( IN )     :: ChannelInfo  ! Scalar

    ! -- Output
    TYPE( CRTM_Atmosphere_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: Atmosphere_K ! L x M
    TYPE( CRTM_Surface_type ),           DIMENSION( :,: ), INTENT( IN OUT ) :: Surface_K    ! L x M
    TYPE( CRTM_RTSolution_type ),        DIMENSION( :,: ), INTENT( IN OUT ) :: RTSolution   ! L x M

    ! -- Optional input
    TYPE( CRTM_Options_type ), OPTIONAL, DIMENSION( : ),   INTENT( IN )     :: Options      ! M

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_K_Matrix(Rank-1)'

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed
    LOGICAL :: Options_Present
    INTEGER :: Status_K
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

    IF ( SIZE( Atmosphere_K, 2 ) /= n_Profiles ) THEN
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

    IF ( SIZE( Surface )      /= n_Profiles .OR. &
         SIZE( Surface_K, 2 ) /= n_Profiles    ) THEN
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

    IF ( SIZE( RTSolution,   2 ) /= n_Profiles .OR. &
         SIZE( RTSolution_K, 2 ) /= n_Profiles      ) THEN
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
        Status_K = CRTM_K_Matrix_scalar( Atmosphere(m),            &  ! Input, Scalar
                                         Surface(m),               &  ! Input, Scalar
                                         RTSolution_K(:,m),        &  ! Input, L   
                                         GeometryInfo(m),          &  ! Input, Scalar
                                         ChannelInfo,              &  ! Input, Scalar
                                         Atmosphere_K(:,m),        &  ! Output, L
                                         Surface_K(:,m),           &  ! Output, L
                                         RTSolution(:,m),          &  ! Output, L
                                         Options = Options(m),     &  ! Optional input, Scalar
                                         Message_Log = Message_Log )  ! Error messaging
        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = Status_K
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_K_Matrix(Scalar,", &
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
        Status_K = CRTM_K_Matrix_scalar( Atmosphere(m),            &  ! Input, Scalar
                                         Surface(m),               &  ! Input, Scalar
                                         RTSolution_K(:,m),        &  ! Input, L   
                                         GeometryInfo(m),          &  ! Input, Scalar
                                         ChannelInfo,              &  ! Input, Scalar
                                         Atmosphere_K(:,m),        &  ! Output, L
                                         Surface_K(:,m),           &  ! Output, L
                                         RTSolution(:,m),          &  ! Output, L   
                                         Message_Log = Message_Log )  ! Error messaging
        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = Status_K
          IF ( Error_Status == FAILURE ) THEN
            WRITE( Message, '( "Error occured in CRTM_K_Matrix(Scalar,", &
                              &"no options) for profile #", i5 )' ) m
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
            RETURN
          END IF
        END IF
      END DO

    END IF Options_Check

  END FUNCTION CRTM_K_Matrix_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_K_Matrix_scalar( Atmosphere,   &  ! Input, Scalar
                                 Surface,      &  ! Input, Scalar
                                 RTSolution_K, &  ! Input, L   
                                 GeometryInfo, &  ! Input, Scalar
                                 ChannelInfo,  &  ! Input, Scalar  
                                 Atmosphere_K, &  ! Output, L
                                 Surface_K,    &  ! Output, L
                                 RTSolution,   &  ! Output, L
                                 Options,      &  ! Optional FWD input
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
    TYPE( CRTM_Atmosphere_type ),                 INTENT( IN OUT)  :: Atmosphere
    TYPE( CRTM_Surface_type ),                    INTENT( IN )     :: Surface
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution_K  ! L
    TYPE( CRTM_GeometryInfo_type ),               INTENT( IN OUT ) :: GeometryInfo
    TYPE( CRTM_ChannelInfo_type ),                INTENT( IN )     :: ChannelInfo

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere_K  ! L
    TYPE( CRTM_Surface_type ),    DIMENSION( : ), INTENT( IN OUT ) :: Surface_K     ! L
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution    ! L

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_K_Matrix(Scalar)'

    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: User_Emissivity
    LOGICAL :: User_Direct_Reflectivity
    INTEGER :: Status_FWD, Status_K
    INTEGER :: l, n_Full_Streams
    TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption,  AtmAbsorption_K
    TYPE( CRTM_AtmScatter_type )    :: AerosolScatter, AerosolScatter_K
    TYPE( CRTM_AtmScatter_type )    :: CloudScatter,   CloudScatter_K
    TYPE( CRTM_AtmScatter_type )    :: AtmOptics,      AtmOptics_K
    TYPE( CRTM_SfcOPtics_type )     :: SfcOptics,      SfcOptics_K

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

    ! -- Output structure arrays too small
    IF ( SIZE( RTSolution   ) < ChannelInfo%n_Channels .OR. &
         SIZE( RTSolution_K ) < ChannelInfo%n_Channels .OR. &
         SIZE( Atmosphere_K ) < ChannelInfo%n_Channels .OR. &
         SIZE( Surface_K    ) < ChannelInfo%n_Channels      ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output structure arrays too small to hold results ", &
                        &"for the number of requested channels (", i5, ")" )' ) &
                      ChannelInfo%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
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

    User_Emissivity   = .FALSE.


    ! ------------------------------
    ! Check the FWD Options argument
    ! ------------------------------

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

    ! -- Adjoint
    Status_K = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                            MAX_N_PREDICTORS,         &  ! Input
                                            MAX_N_ABSORBERS,          &  ! Input
                                            AtmAbsorption_K,          &  ! Output
                                            Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmAbsorption structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------
    ! The CloudScatter structures
    ! ---------------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                           MAX_N_LEGENDRE_TERMS,     &  ! Input
                                           MAX_N_PHASE_ELEMENTS,     &  ! Input
                                           CloudScatter,             &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    ! -- Adjoint
    Status_K = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                         MAX_N_LEGENDRE_TERMS,     &  ! Input
                                         MAX_N_PHASE_ELEMENTS,     &  ! Input
                                         CloudScatter_K,           &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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

    ! -- Adjoint
    Status_K = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                         MAX_N_LEGENDRE_TERMS,     &  ! Input
                                         MAX_N_PHASE_ELEMENTS,     &  ! Input
                                         AerosolScatter_K,         &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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

    ! -- Adjoint
    Status_K = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
                                         MAX_N_LEGENDRE_TERMS,     &  ! Input
                                         MAX_N_PHASE_ELEMENTS,     &  ! Input
                                         AtmOptics_K,              &  ! Output
                                         Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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

    ! -- Adjoint
    Status_K = CRTM_Allocate_SfcOptics( MAX_N_ANGLES,             &  ! Input
                                        MAX_N_STOKES,             &  ! Input
                                        SfcOptics_K,              &  ! Output
                                        Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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



    !#--------------------------------------------------------------------------#
    !#             -- SET UP FOR FORWARD GASEOUS ABSORPTION CALCS --            #
    !#--------------------------------------------------------------------------#

    Status_FWD = CRTM_SetUp_AtmAbsorption( Atmosphere,               &  ! Input
                                           GeometryInfo,             &  ! Input
                                           AtmAbsorption,            &  ! Output
                                           Message_Log = Message_Log )  ! Error messaging

    IF ( Status_FWD /= SUCCESS ) THEN
      Error_Status = FAILURE
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



      !#------------------------------------------------------------------------#
      !#                        -- FORWARD CALCULATIONS --                      #
      !#------------------------------------------------------------------------#

      ! --------------------------------
      ! Compute the layer optical depths
      ! due to gaseous absorption
      ! --------------------------------

      Status_FWD = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                               AtmAbsorption,                &  ! In/Output
                                               Message_Log = Message_Log     )  ! Error messaging

      IF ( Status_FWD /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_AtmAbsorption', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
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
      AtmOptics_K%n_Legendre_Terms    = n_Full_Streams


      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------

      IF( Atmosphere%n_Clouds > 0 ) THEN

        CloudScatter%n_Legendre_Terms   = n_Full_Streams
        Status_FWD = CRTM_Compute_CloudScatter( Atmosphere,                   &  ! Input
                                                ChannelInfo%Channel_Index(l), &  ! Input
                                                CloudScatter                  )  ! Output

        IF ( Status_FWD /= SUCCESS ) THEN
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

        AerosolScatter%n_Legendre_Terms = n_Full_Streams
        Status_FWD = CRTM_Compute_AerosolScatter( Atmosphere,                   &  ! Input
                                                  GeometryInfo,                 &  ! Input
                                                  ChannelInfo%Channel_Index(l), &  ! Input
                                                  AerosolScatter,               &  ! Output
                                                  Message_Log = Message_Log     )  ! Error messaging

        IF ( Status_FWD /= SUCCESS ) THEN
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

      CALL CRTM_Combine_AtmOptics( AtmAbsorption,  & ! Input
                                   CloudScatter,   & ! Input
                                   AerosolScatter, & ! Input
                                   AtmOptics,      & ! Output
                                   AOV             ) ! Internal variable output

                                  
      ! ---------------------------------------------
      ! Fill the SfcOptics structure for the optional
      ! emissivity input case.
      ! ---------------------------------------------

      ! -- Indicate SfcOptics ARE to be computed
      SfcOptics%Compute_Switch = SET

      ! -- Change FWD SfcOptics emissivity/reflectivity
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



      !#------------------------------------------------------------------------#
      !#                        -- ADJOINT CALCULATIONS --                      #
      !#------------------------------------------------------------------------#
                                   
      ! --------------------------------------------------
      ! Reinitialise profile independent adjoint variables
      ! --------------------------------------------------
      AtmOptics_K%Optical_Depth         = ZERO
      AtmOptics_K%Single_Scatter_Albedo = ZERO

      IF( AtmOptics%n_Legendre_Terms > 0 ) THEN
        AtmOptics_K%Phase_Coefficient     = ZERO
        AtmOptics_K%Asymmetry_Factor      = ZERO
        AtmOptics_K%Delta_Truncation      = ZERO
      END IF


      ! -------------------------------------
      ! The adjoint of the radiative transfer
      ! -------------------------------------

      Status_K = CRTM_Compute_RTSolution_AD( Atmosphere,                   &  ! FWD Input
                                             Surface,                      &  ! FWD Input
                                             AtmOptics,                    &  ! FWD Input
                                             SfcOptics,                    &  ! FWD Input
                                             RTSolution(l),                &  ! FWD Input
                                             RTSolution_K(l),              &  ! K  Input
                                             GeometryInfo,                 &  ! Input
                                             ChannelInfo%Channel_Index(l), &  ! Input
                                             Atmosphere_K(l),              &  ! K Output
                                             Surface_K(l),                 &  ! K Output
                                             AtmOptics_K,                  &  ! K Output
                                             SfcOptics_K,                  &  ! K Output
                                             RTV,                          &  ! Internal variable input
                                             Message_Log = Message_Log     )  ! Error messaging

      IF ( Status_K /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_RTSolution_AD', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------------------------------------------------------
      ! Compute the adjoint of the combined atmospheric optical properties
      ! ------------------------------------------------------------------

      CALL CRTM_Combine_AtmOptics_AD( AtmAbsorption,    &  ! FWD Input
                                      CloudScatter,     &  ! FWD Input
                                      AerosolScatter,   &  ! FWD Input
                                      AtmOptics,        &  ! FWD Input
                                      AtmOptics_K,      &  ! K Input
                                      AtmAbsorption_K,  &  ! K Output
                                      CloudScatter_K,   &  ! K Output
                                      AerosolScatter_K, &  ! K Output
                                      AOV               )  ! Internal variable input


      ! ------------------------------------------------------------
      ! Compute the adjoint aerosol absorption/scattering properties
      ! ------------------------------------------------------------

      IF ( Atmosphere%n_Aerosols > 0 ) THEN

        Status_K = CRTM_Compute_AerosolScatter_AD( Atmosphere,                   &  ! Input
                                                   AerosolScatter,               &  ! Input
                                                   AerosolScatter_K,             &  ! Input
                                                   GeometryInfo,                 &  ! Input
                                                   ChannelInfo%Channel_Index(l), &  ! Input
                                                   Atmosphere_K(l),              &  ! In/Output
                                                   Message_Log = Message_Log     )  ! Error messaging

        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = FAILURE
          CALL DIsplay_Message( ROUTINE_NAME, &
                                'Error in CRTM_Compute_AerosolScatter_AD', &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ----------------------------------------------------------
      ! Compute the adjoint cloud absorption/scattering properties
      ! ----------------------------------------------------------

      IF( Atmosphere%n_Clouds > 0 ) THEN

        Status_K = CRTM_Compute_CloudScatter_AD( Atmosphere,                   &  ! Input
                                                 CloudScatter,                 &  ! Input
                                                 CloudScatter_K,               &  ! Input
                                                 ChannelInfo%Channel_Index(l), &  ! Input
                                                 Atmosphere_K(l)               )  ! In/Output

        IF ( Status_K /= SUCCESS ) THEN
          Error_Status = FAILURE
          CALL DIsplay_Message( ROUTINE_NAME, &
                                'Error in CRTM_Compute_CloudScatter_AD', &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ----------------------------------------
      ! Compute the adjoint layer optical depths
      ! due to gaseous absorption
      ! ----------------------------------------

      ! -- The optical depth calculation
      Status_K = CRTM_Compute_AtmAbsorption_AD( ChannelInfo%Channel_Index(l), &  ! Input
                                                AtmAbsorption,                &  ! Input
                                                AtmAbsorption_K,              &  ! In/Output
                                                Message_Log = Message_Log     )  ! Error messaging

      IF ( Status_K /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_Compute_AtmAbsorption_AD', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! -- The predictor and absorber space calculation
      Status_K = CRTM_SetUp_AtmAbsorption_AD( Atmosphere,               &  ! Input
                                              AtmAbsorption,            &  ! Input
                                              AtmAbsorption_K,          &  ! Input
                                              GeometryInfo,             &  ! Input
                                              Atmosphere_K(l),          &  ! In/Output
                                              Message_Log = Message_Log )  ! Error messaging

      IF ( Status_K /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL DIsplay_Message( ROUTINE_NAME, &
                              'Error in CRTM_SetUp_AtmAbsorption_AD', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



      !#------------------------------------------------------------------------#
      !#                     -- POSTPROCESS SOME INPUT DATA --                  #
      !#------------------------------------------------------------------------#

      ! -------------------------------------------------------------------
      ! Adjoint of average surface skin temperature for multi-surface types
      ! -------------------------------------------------------------------

      CALL CRTM_Compute_SurfaceT_AD( Surface, SfcOptics_K, Surface_K(l) )  


    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#                    -- DEALLOCATE LOCAL STRUCTURES --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! The SfcOptics structures
    ! ------------------------

    Status_FWD = CRTM_Destroy_SfcOptics( SfcOptics )
    Status_K   = CRTM_Destroy_SfcOptics( SfcOptics_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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
    Status_K   = CRTM_Destroy_AtmScatter( AtmOptics_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating CloudScatter structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -----------------------------
    ! The AerosolScatter structures
    ! -----------------------------

    Status_FWD = CRTM_Destroy_AtmScatter( AerosolScatter )
    Status_K   = CRTM_Destroy_AtmScatter( AerosolScatter_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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
    Status_K   = CRTM_Destroy_AtmScatter( CloudScatter_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
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
    Status_K   = CRTM_Destroy_AtmAbsorption( AtmAbsorption_K )

    IF ( Status_FWD /= SUCCESS .OR. Status_K /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_K_Matrix_scalar

END MODULE CRTM_K_Matrix_Module


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_K_Matrix_Module.f90,v 1.5.2.11 2005/10/20 19:58:31 paulv Exp $
!
! $Date: 2005/10/20 19:58:31 $
!
! $Revision: 1.5.2.11 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_K_Matrix_Module.f90,v $
! Revision 1.5.2.11  2005/10/20 19:58:31  paulv
! - Only forward model options arguments are now accepted.
!
! Revision 1.5.2.10  2005/10/12 21:06:12  paulv
! - Zeroing the Options_K emissivity and reflectivity in the check if block,
!   not in the if block that assigns the results.
!
! Revision 1.5.2.9  2005/10/12 18:34:45  qliu
! - Fixed some cut-and-paste bugs.
! - Fixed bug in AerosolScatter and CloudScatter AD calls - the Atmosphere_K
!   array was incorrectly being passed, rather than a single channel element.
!
! Revision 1.5.2.8  2005/10/12 15:23:36  paulv
! - Updated header documentation.
! - Added Options and Options_K optional structure arguments.
! - Now testing for user FWD and K emissivity and direct reflectivity
!   with respect to the Options and Options_K arguments.
! - The profile loop and scalar function call in the rank-1 function was
!   replaced with a SELECT CASE statement containing profile loops and
!   scalar function calls for each permutation of the prescence of the
!   FWD Options and K Options_K optional arguments. This was done to
!   prevent a profile-independent test inside the profile loop and to allow
!   the optional arguments to be indexed as needed.
! - Filling the FWD and K SfcOptics structures as needed according to the
!   contents of the optional Options and Options_K arguments.
! - AtmOptics AOV internal variable structure.
!   o Added AOV internal variable type declaration
!   o Added AOV argument to FWD and K AtmOptics calls.
! - RTSolution RTV internal variable structure.
!   o Added RTV internal variable type declaration
!   o Added RTV argument to FWD and K RTSolution calls.
! - Added a call to the FWD RTSolution function. Previously the adjoint call
!   also did the forward computation. This has been changed (see the log for
!   CRTM_RTSolution.f90)
!
! Revision 1.5.2.7  2005/10/11 20:41:32  qliu
! -- Consistence with other part.
!
! Revision 1.5.2.6  2005/09/15 14:17:47  qliu
! -- Added checking Error_Status.
!
! Revision 1.5.2.5  2005/09/12 13:48:44  qliu
! -- Modification for fitting CRTM_Interpolation.
!
! Revision 1.5.2.4  2005/08/24 12:41:39  qliu
! -- Deleted unused variables.
!
! Revision 1.5.2.3  2005/08/23 22:07:31  qliu
! -- Deleted unused variable.
!
! Revision 1.5.2.2  2005/08/23 21:55:37  qliu
! -- Deleted unused variable.
!
! Revision 1.5.2.1  2005/08/23 21:24:50  qliu
! -- Deleted unused variable.
!
! Revision 1.5  2005/02/25 17:53:05  paulv
! - Updated scattering application routines, CloudScatter and AerosolScatter,
!   now being used instead of AtmScatter and Aerosol respectively. Both the
!   cloud particle and aerosol scattering structures are now the same type,
!   CRTM_AtmScatter_type. All the argument to the various function have
!   been updated to reflect this change.
!
! Revision 1.4  2005/02/18 23:17:56  paulv
! - Added Aerosol capability. RTSolution interfaces altered.
!
! Revision 1.3  2005/02/16 22:45:07  paulv
! - Added hooks for implementation of Aerosol absorption and scattering.
!   Right now, all of the Aerosol stuff is commented out awaiting a full
!   definition of the Aerosol structure.
!
! Revision 1.2  2005/02/16 15:29:23  paulv
! - Updated header documentation.
! - Started adding hooks for aerosol absorption and scattering function calls.
!   Not implemented yet as Aerosol structure definition is not yet mature enough.
!
! Revision 1.1  2005/01/31 21:33:20  paulv
! Initial checkin.
!
!
!
!
