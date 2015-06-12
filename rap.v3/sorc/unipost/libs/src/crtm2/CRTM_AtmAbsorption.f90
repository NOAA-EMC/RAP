!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_AtmAbsorption
!
! PURPOSE:
!       Module containing routines to compute the optical depth profile
!       due to gaseous absorption.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_AtmAbsorption
!
! MODULES:
!       Type_Kinds:                      Module containing definitions for kinds
!                                        of variable types.
!
!       Error_Handler:                   Module to define simple error codes and
!                                        handle error conditions
!                                        USEs: FILE_UTILITY module
!
!       CRTM_Parameters:                 Module of parameter definitions for the CRTM.
!                                        USEs: TYPE_KINDS module
!
!       CRTM_TauCoeff:                   Module containing the shared CRTM gas
!                                        absorption coefficients (TauCoeff)
!                                        and their load/destruction routines. 
!                                        USEs TYPE_KINDS module
!                                             ERROR_HANDLER module
!                                             TAUCOEFF_DEFINE module
!                                             TAUCOEFF_BINARY_IO module
!                                             CRTM_PARAMETERS module
!
!       CRTM_Atmosphere_Define:          Module defining the CRTM Atmosphere
!                                        structure and containing routines to 
!                                        manipulate it.
!                                        USEs: TYPE_KINDS module
!                                              ERROR_HANDLER module
!                                              CRTM_CLOUD_DEFINE module
!
!       CRTM_GeometryInfo_Define:        Module defining the CRTM GeometryInfo
!                                        structure and containing routines to 
!                                        manipulate it.
!                                        USEs: TYPE_KINDS module
!                                              ERROR_HANDLER module
!
!       CRTM_AtmAbsorption_Define:       Module defining the CRTM AtmAbsorption
!                                        structure and containing routines to 
!                                        manipulate it.
!                                        USEs: TYPE_KINDS module
!                                              ERROR_HANDLER module
!
!       CRTM_AtmAbsorption_IntAbsorber:  Module containing the routines to compute
!                                        the integrated absorber profiles for the
!                                        CRTM gas absorption model.
!                                        USEs: TYPE_KINDS module
!                                              ERROR_HANDLER module
!                                              CRTM_PARAMETERS module
!                                              CRTM_ATMOSPHERE_DEFINE module
!
!       CRTM_AtmAbsorption_Predictor:    Module containing the routines to compute
!                                        the predictor profiles for the CRTM gas
!                                        absorption model.
!                                        USEs: TYPE_KINDS module
!                                              ERROR_HANDLER module
!                                              CRTM_PARAMETERS module
!                                              CRTM_ATMOSPHERE_DEFINE module
!                                              CRTM_ATMABSORPTION_DEFINE module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!       CRTM_SetUp_AtmAbsorption:        Function to set-up the CRTM_AtmAbsorption_type
!                                        data structure for a supplied atmospheric
!                                        profile in preparation for multi-channel
!                                        optical depth computations via the
!                                        CRTM_Compute_AtmAbsorption function.
!
!       CRTM_Compute_AtmAbsorption:      Function to calculate the layer optical
!                                        depths due to gaseous absorption for a given
!                                        input atmospheric profile for a single channel.
!
!                                        This function must be called *after* the
!                                        CRTM_SetUp_AtmAbsorption function.
!
!       CRTM_SetUp_AtmAbsorption_TL:     Function to set-up the CRTM_AtmAbsorption_type
!                                        data structure for a supplied atmospheric
!                                        profile in preparation for multi-channel
!                                        tangent-linear optical depth computations
!                                        via the CRTM_Compute_AtmAbsorption_TL
!                                        function.
!
!       CRTM_Compute_AtmAbsorption_TL:   Function to calculate the tangent-linear
!                                        layer optical depths due to gaseous absorption
!                                        for a given input atmospheric profile for a
!                                        single channel.
!
!                                        This function must be called *after* the
!                                        CRTM_SetUp_AtmAbsorption_TL function.
!
!       CRTM_SetUp_AtmAbsorption_AD:     Function to compute the adjoint of the set-up
!                                        of the CRTM_AtmAbsorption_type data structure
!                                        for a supplied atmospheric profile after multi-
!                                        channel gas absorptionadjoint optical depth
!                                        computations via the CRTM_Compute_AtmAbsorption_AD
!                                        function.
!
!                                        This function must be called *after* the
!                                        CRTM_Compute_AtmAbsorption_AD function.
!
!
!       CRTM_Compute_AtmAbsorption_AD:   Function to calculate the layer optical depths
!                                        adjoint due to gaseous absorption for a given
!                                        input atmospheric profile for a single channel.
!
!                                        This routine must be called *before* the
!                                        CRTM_SetUp_AtmAbsorption_AD function.
!
!       PRIVATE subprograms
!       -------------------
!       None.
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Associated_AtmAbsorption:   Function to test the association status of
!                                        the pointer members of a AtmAbsorption
!                                        structure.
!                                        SOURCE: CRTM_ATMABSORPTION_DEFINE module
!
!       CRTM_Destroy_AtmAbsorption:      Function to re-initialize a
!                                        CRTM_AtmAbsorption structure.
!                                        SOURCE: CRTM_ATMABSORPTION_DEFINE module
!
!       CRTM_Allocate_AtmAbsorption:     Function to allocate the pointer
!                                        members of a CRTM_AtmAbsorption
!                                        structure.
!                                        SOURCE: CRTM_ATMABSORPTION_DEFINE module
!
!       CRTM_Assign_AtmAbsorption:       Function to copy a valid 
!                                        CRTM_AtmAbsorption structure.
!                                        SOURCE: CRTM_ATMABSORPTION_DEFINE module
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

MODULE CRTM_AtmAbsorption


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_TauCoeff
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type, H2O_ID, O3_ID
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  ! -- The AtmAbsorption structure definition module
  ! -- The PUBLIC entities in CRTM_AtmAbsorption_Define
  ! -- are also explicitly defined as PUBLIC here so 
  ! -- a user need only USE CRTM_AtmAbsorption.
  USE CRTM_AtmAbsorption_Define

  ! -- The AtmAbsorption algorithm support modules
  USE CRTM_AtmAbsorption_IntAbsorber
  USE CRTM_AtmAbsorption_Predictor


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_AtmAbsorption structure data type
  ! -- in the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_AtmAbsorption_type

  ! -- CRTM_AtmAbsorption structure routines inherited
  ! -- from the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_Associated_AtmAbsorption
  PUBLIC :: CRTM_Destroy_AtmAbsorption
  PUBLIC :: CRTM_Allocate_AtmAbsorption
  PUBLIC :: CRTM_Assign_AtmAbsorption

  ! -- Science routines in this modules
  PUBLIC :: CRTM_SetUp_AtmAbsorption
  PUBLIC :: CRTM_Compute_AtmAbsorption

  PUBLIC :: CRTM_SetUp_AtmAbsorption_TL
  PUBLIC :: CRTM_Compute_AtmAbsorption_TL

  PUBLIC :: CRTM_SetUp_AtmAbsorption_AD
  PUBLIC :: CRTM_Compute_AtmAbsorption_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmAbsorption.f90,v 2.5 2005/08/16 16:19:28 qliu Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS


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
!       CRTM_SetUp_AtmAbsorption
!
! PURPOSE:
!       Function to set-up the CRTM_AtmAbsorption_type data structure for a
!       supplied atmospheric profile in preparation for multi-channel
!       gas absorption optical depth computations via the
!       CRTM_Compute_AtmAbsorption function.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SetUp_AtmAbsorption( Atmosphere,               &  ! Input
!                                                GeometryInfo,             &  ! Input
!                                                AtmAbsorption,            &  ! Output
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM_Atmosphere structure containing the atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Atmosphere_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_GeometryInfo_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
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
!        AtmAbsorption:  CRTM_AtmAbsorption structure containing the
!                        gaseous absorption data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Compute_IntAbsorber:    Subroutine to compute the integrated
!                                    absorber profiles.  
!
!       CRTM_Compute_Predictors:     Subroutine to calculate the absorption
!                                    model predictors.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       No argument checking is performed. It is assumed that all the structures
!       are already allocated.
!
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_SetUp_AtmAbsorption( Atmosphere,    &  ! Input
                                     GeometryInfo,  &  ! Input
                                     AtmAbsorption, &  ! Output
                                     Message_Log )  &  ! Error messaging
                                   RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo

    ! -- Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption    

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log

    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_SetUp_AtmAbsorption'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers, & 
                                MAX_N_ABSORBERS        ) :: Nadir_IntAbsorber



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#                                                                          #
    !#        This routine is a function for symmetry purposes only. :o)        #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#           -- COMPUTE THE NADIR INTEGRATED ABSORBER PROFILES --           #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Compute_IntAbsorber( Atmosphere,       &  ! Input
                                   Nadir_IntAbsorber )  ! Output


    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE PREDICTORS FOR THE UPWELLING TRANSMITTANCE --      #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------------
    ! Modify absorber quantities by the angle secant
    ! Could put a loop here but here's hoping the compiler
    ! recognises this as a group of loops over layer.
    ! ------------------------------------------------------

    AtmAbsorption%IntAbsorber = GeometryInfo%Secant_Sensor_Zenith * Nadir_IntAbsorber


    ! -----------------------------------------------------
    ! Calculate the predictors for the satellite view angle
    ! -----------------------------------------------------

    CALL CRTM_Compute_Predictors( Atmosphere,   &  ! Input
                                  AtmAbsorption )  ! In/Output


    ! --- pass the secant angle to the AtmAbsorption structure    
    AtmAbsorption%Secant_Sensor_Zenith = GeometryInfo%Secant_Sensor_Zenith

  END FUNCTION CRTM_SetUp_AtmAbsorption




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_SetUp_AtmAbsorption_TL
!
! PURPOSE:
!       Function to set-up the CRTM_AtmAbsorption_type data structure for a
!       supplied atmospheric profile in preparation for multi-channel
!       gas absorption tangent-linear optical depth computations via the
!       CRTM_Compute_AtmAbsorption_TL function.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SetUp_AtmAbsorption_TL( Atmosphere,               &  ! Input
!                                                   AtmAbsorption,            &  ! Input
!                                                   Atmosphere_TL,            &  ! Input
!                                                   GeometryInfo,             &  ! Input
!                                                   AtmAbsorption_TL,         &  ! Output
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:      CRTM_AtmAbsorption structure containing the integrated
!                           absorber and gas absoprtion model predictor profiles.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:      CRTM Atmosphere structure containing the tangent-linear
!                           atmospheric state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:       CRTM_GeometryInfo structure containing the 
!                           view geometry information.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_GeometryInfo_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption_TL:  CRTM_AtmAbsorption structure containing the
!                           tangent-linear integrated absorber and gas
!                           absoprtion model predictor profiles.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Compute_IntAbsorber_TL: Subroutine to compute the tangent-linear
!                                    integrated absorber profiles.
!                                    SOURCE: CRTM_ATMABSORPTION_INTABSORBER module
!
!       CRTM_Compute_Predictors_TL:  Subroutine to calculate the gas absorption
!                                    model tangent-linear predictors.
!                                    SOURCE: CRTM_ATMABSORPTION_PREDICTOR module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_SetUp_AtmAbsorption_TL( Atmosphere,       &  ! Input
                                        AtmAbsorption,    &  ! Input
                                        Atmosphere_TL,    &  ! Input
                                        GeometryInfo,     &  ! Input
                                        AtmAbsorption_TL, &  ! Output
                                        Message_Log )     &  ! Error messaging
                                      RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere_TL
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo

    ! -- Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_TL

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_SetUp_AtmAbsorption_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers, & 
                                MAX_N_ABSORBERS        ) :: Nadir_IntAbsorber_TL



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#                                                                          #
    !#        This routine is a function for symmetry purposes only. :o)        #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#    -- COMPUTE THE TANGENT-LINEAR NADIR INTEGRATED ABSORBER PROFILES --   #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Compute_IntAbsorber_TL( Atmosphere,          &  ! Input
                                      Atmosphere_TL,       &  ! Input
                                      Nadir_IntAbsorber_TL )  ! Output



    !#--------------------------------------------------------------------------#
    !#                 -- CALCULATE THE TANGENT-LINEAR PREDICTORS --            #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------------
    ! Modify absorber quantities by the angle secant
    ! Could put a loop here but here's hoping the compiler
    ! recognises this as a group of loops over layer.
    ! ------------------------------------------------------

    AtmAbsorption_TL%IntAbsorber = GeometryInfo%Secant_Sensor_Zenith * Nadir_IntAbsorber_TL


    ! -----------------------------------------------------
    ! Calculate the predictors for the satellite view angle
    ! -----------------------------------------------------

    CALL CRTM_Compute_Predictors_TL( Atmosphere,      &  ! Input
                                     AtmAbsorption,   &  ! Input
                                     Atmosphere_TL,   &  ! Input
                                     AtmAbsorption_TL )  ! In/Output

  END FUNCTION CRTM_SetUp_AtmAbsorption_TL




!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_SetUp_AtmAbsorption_AD
!
! PURPOSE:
!       Function to perform the adjoint of the set-up of the
!       CRTM_AtmAbsorption_type data structure for a supplied atmospheric
!       profile after multi-channel gas absorption adjoint optical depth
!       computations via the CRTM_Compute_AtmAbsorption_AD function.
!
!       This function must be called *after* the CRTM_Compute_AtmAbsorption_AD
!       function.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_SetUp_AtmAbsorption_AD( Atmosphere,               &  ! Input
!                                                   AtmAbsorption,            &  ! Input
!                                                   AtmAbsorption_AD,         &  ! Input
!                                                   GeometryInfo,             &  ! Input
!                                                   Atmosphere_AD,            &  ! Output
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:      CRTM_AtmAbsorption structure containing the integrated
!                           absorber and gas absoprtion model predictor profiles.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption_AD:   CRTM_AtmAbsorption structure containing the
!                           adjoint integrated absorber and gas absoprtion model
!                           predictor profiles.
!                           **NOTE: On EXIT from this function, the contents of
!                                   this structure are set to zero.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:       CRTM_GeometryInfo structure containing the 
!                           view geometry information.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_GeometryInfo_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:      CRTM Atmosphere structure containing the adjoint
!                           atmospheric state data.
!                           **NOTE: On ENTRY to this function, the contents of
!                                   this structure should be defined (e.g.
!                                   initialized to some value based on the
!                                   position of this function in the call chain.)
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Compute_IntAbsorber_AD: Subroutine to compute the adjoint
!                                    integrated absorber profiles.
!                                    SOURCE: CRTM_ATMABSORPTION_INTABSORBER module
!
!       CRTM_Compute_Predictors_AD:  Subroutine to calculate the adjoint gas
!                                    absorption model predictors.
!                                    SOURCE: CRTM_ATMABSORPTION_PREDICTOR module
!
! SIDE EFFECTS:
!       The contents of the input AtmAbsorption_AD structure argument are set
!       to zero before exiting this routine.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       - Note the INTENT on the input AtmAbsorption_AD argument is IN OUT rather
!         than just OUT. This is because the structure components are set to zero
!         before exiting this routine.
!
!       - Note the INTENT on the output Atmosphere_AD argument is IN OUT rather
!         than just OUT. This is necessary because the argument is defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_SetUp_AtmAbsorption_AD( Atmosphere,       &  ! Input
                                        AtmAbsorption,    &  ! Input
                                        AtmAbsorption_AD, &  ! Input
                                        GeometryInfo,     &  ! Input
                                        Atmosphere_AD,    &  ! Output
                                        Message_Log )     &  ! Error messaging
                                      RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN )     :: Atmosphere
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_AD
    TYPE( CRTM_GeometryInfo_type ),  INTENT( IN )     :: GeometryInfo

    ! -- Outputs
    TYPE( CRTM_Atmosphere_type ),    INTENT( IN OUT)  :: Atmosphere_AD

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_SetUp_AtmAbsorption_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers, & 
                                MAX_N_ABSORBERS        ) :: Nadir_IntAbsorber_AD



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#                                                                          #
    !#        This routine is a function for symmetry purposes only. :o)        #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                   -- CALCULATE THE ADJOINT PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------------------------
    ! Calculate the adjoint predictors for the satellite view angle
    ! -------------------------------------------------------------

    CALL CRTM_Compute_Predictors_AD( Atmosphere,       &  ! Input
                                     AtmAbsorption,    &  ! Input
                                     AtmAbsorption_AD, &  ! In/Output
                                     Atmosphere_AD     )  ! Output


    ! ------------------------------------------------------
    ! Modify absorber quantities by the angle secant
    ! Could put a loop here but here's hoping the compiler
    ! recognises this as a group of loops over layer.
    ! ------------------------------------------------------

    Nadir_IntAbsorber_AD = GeometryInfo%Secant_Sensor_Zenith * AtmAbsorption_AD%IntAbsorber



    !#--------------------------------------------------------------------------#
    !#       -- COMPUTE THE ADJOINT NADIR INTEGRATED ABSORBER PROFILES --       #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Compute_IntAbsorber_AD( Atmosphere,           &  ! Input
                                      Nadir_IntAbsorber_AD, &  ! Input
                                      Atmosphere_AD         )  ! Output



    !#--------------------------------------------------------------------------#
    !#                  -- ZERO THE AtmAbsorption_AD STRUCTURE --               #
    !#--------------------------------------------------------------------------#

    AtmAbsorption_AD%IntAbsorber   = ZERO
    AtmAbsorption_AD%Predictor     = ZERO
    AtmAbsorption_AD%Optical_Depth = ZERO

  END FUNCTION CRTM_SetUp_AtmAbsorption_AD




!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_AtmAbsorption
!
! PURPOSE:
!       Function to calculate the layer optical depths due to gaseous
!       absorption for a given input atmospheric profile for a single
!       channel.
!
!       This routine must be called *after* the CRTM_SetUp_AtmAbsorption
!       routine.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AtmAbsorption( Channel_Index,            &  ! Input, scalar
!                                                  AtmAbsorption,            &  ! In/Output
!                                                  Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!        AtmAbsorption:  Upon INPUT, this structure contains valid integrated
!                        absorber and predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
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
!        AtmAbsorption:  Upon OUTPUT, this structure contains valid optical
!                        depth profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       The CRTM_AtmAbsorption_type structure argument is not checked in this
!       routine as it called once for every channel - for possibly many channels
!       so constant rechecking of the AtmAbsorption argument in this routine
!       is a bit of a time-waster.
!
!       It is important that the CRTM_SetUp_AtmAbsorption() function is called
!       *before* this routine *outside* any channel specific loops.
!
! COMMENTS:
!       Note the INTENT on the AtmAbsorption argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AtmAbsorption( Channel_Index, &  ! Input, scalar
                                       AtmAbsorption, &  ! In/Output
                                       Message_Log )  &  ! Error messaging
                                     RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Channel_Index

    ! -- In/Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption    

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: n                         ! Polynomial index

    REAL( fp_kind ) :: ave_IntAbsorber
    REAL( fp_kind ) :: d_IntAbsorber
    REAL( fp_kind ) :: IntAbsorber_Level
    REAL( fp_kind ) :: LN_Chi
    REAL( fp_kind ) :: Absorption_Coefficient

    ! -- Polynomial derived coefficients
    REAL( fp_kind ), DIMENSION( 0:MAX_N_PREDICTORS_USED ) :: b



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#                                                                          #
    !#        This routine is a function for symmetry purposes only. :o)        #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Assign the channel index to a short name
    ! ----------------------------------------

    l = Channel_Index


    ! ---------------------------
    ! Initilise the optical depth
    ! ---------------------------

    AtmAbsorption%Optical_Depth = ZERO


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------

    Absorber_Loop: DO j = 1, AtmAbsorption%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the absorber loop.
      ! -----------------------------------------

      IF ( TC%Predictor_Index( 0, j, l ) <= 0 ) CYCLE Absorber_Loop



      !#------------------------------------------------------------------------#
      !#                    -- BEGIN LOOP OVER LAYERS --                        #
      !#------------------------------------------------------------------------#

      Layer_Loop: DO k = 1, AtmAbsorption%n_Layers


        ! -----------------------------------
        ! Calculate the current layer average
        ! absorber amount and difference
        ! -----------------------------------

        ave_IntAbsorber = POINT_5 * ( AtmAbsorption%IntAbsorber( k,   j ) + &
                                      AtmAbsorption%IntAbsorber( k-1, j )   )
        d_IntAbsorber   = AtmAbsorption%IntAbsorber( k,   j ) - &
                          AtmAbsorption%IntAbsorber( k-1, j )

        ! ----------------------------------------------------------
        ! Calculate absorber space level associated with the average
        ! absorber amount
        ! 
        ! Absorber level, k, to amount 
        ! 
        !     A(k) = C1.exp(Alpha * k) + C2
        ! 
        ! Absorber amount to level 
        ! 
        !           1      A - C2
        !     k = ----- LN ------
        !         Alpha      C1
        ! 
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        ! ----------------------------------------------------------

        IntAbsorber_Level = LOG( ( ave_IntAbsorber - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !                   ------------------------------------------------------------
                                                TC%Alpha(j)



        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          n
        !   b(i) =  > c(n,i).k
        !          /__
        !             n=0
        !
        ! NOTE:
        ! 1) The coefficient array, c(n,i), corresponds to the array
        !    TC%C(n,i,j,l) for the given absorber, j, and channel, l.
        !
        ! 2) The summation maximum, N, corresponds to the value in
        !    TC%Order_Index(i,j,l) for the given absorber, j, and
        !    channel, l. If
        !      TC%Order_Index(i,j,l) = 0
        !    then
        !      b(i) = c(0,i)
        !
        ! 3) The computation of the b(i) below uses Horner's method,
        !    that's why the polynomial calculation *begins* at the
        !    maximum polynomial order, TC%Order_Index( i, j, l ).
        !
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Initialise the polynomial sum.
          ! -- TC%Order_Index( i, j, l ) contains the polynomial
          ! -- order to be used in reconstructing the b(i) coefficients
          ! -- for the current predictor, i, for absorber j and channel l.
          ! -- This value is used to access the coefficient array, TC%C.
          b(i) = TC%C( TC%Order_Index( i, j, l ), i, j, l )

          ! -- Loop over the polynomial orders and accumulate the
          ! -- coefficient value. Note that if
          ! --   TC%Order_Index( 0, j, l ) == 0
          ! -- then this loop is not entered. If your compiler has
          ! -- a "zero trip" loop option where DO loops are *always*
          ! -- executed at least once, regardless of the loop indices,
          ! -- make sure it's not on by default!
          DO n = TC%Order_Index( i, j, l ) - 1, 0, -1
            b(i) = ( b(i) * IntAbsorber_Level ) + TC%C( n, i, j, l )
          END DO

        END DO


        ! ---------------------------------------------------------
        ! Compute the logarithm of the absorption coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! ---------------------------------------------------------

        ! -- Initialise the log of the absorption coefficient
        LN_Chi = b(0)

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          LN_Chi = LN_Chi + ( b(i) * AtmAbsorption%Predictor( ip, k ) )

        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------

        IF( LN_Chi > LIMIT_EXP ) THEN
          Absorption_Coefficient = LIMIT_LOG
        ELSE IF( LN_Chi < -LIMIT_EXP ) THEN
          Absorption_Coefficient = ZERO
        ELSE
          Absorption_Coefficient = EXP( LN_Chi )
        ENDIF


        ! -----------------------
        ! Calculate optical_depth
        ! -----------------------

        AtmAbsorption%Optical_Depth( k ) = AtmAbsorption%Optical_Depth( k ) + &
                                           ( Absorption_Coefficient * d_IntAbsorber )

      END DO Layer_Loop

    END DO Absorber_Loop

    ! --- Compute nadir optical depth profile
    AtmAbsorption%Optical_Depth = AtmAbsorption%Optical_Depth / AtmAbsorption%Secant_Sensor_Zenith


  END FUNCTION CRTM_Compute_AtmAbsorption




!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Function to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given input atmospheric profile for a
!       single channel.
!
!       This routine must be called *after* the CRTM_SetUp_AtmAbsorption_TL
!       routine.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AtmAbsorption_TL( Channel_Index,            &  ! Input
!                                                     AtmAbsorption,            &  ! Input     
!                                                     AtmAbsorption_TL,         &  ! In/Output 
!                                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access
!                           the shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!        AtmAbsorption:     CRTM_AtmAbsorption structure containing the
!                           gaseous absorption data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption_TL:  CRTM_AtmAbsorption structure containing the
!                           tangent-linear gaseous absorption data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       The CRTM_AtmAbsorption_type structure arguments are not checked in this
!       routine as it called once for every channel - for possibly many channels
!       so constant rechecking of the AtmAbsorption argument in this routine
!       is a bit of a time-waster.
!
!       It is important that the CRTM_SetUp_AtmAbsorption_TL() function is
!       called *before* this routine *outside* any channel specific loops.
!
! COMMENTS:
!       Note the INTENT on the output AtmAbsorption_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AtmAbsorption_TL( Channel_Index,    &  ! Input
                                          AtmAbsorption,    &  ! Input
                                          AtmAbsorption_TL, &  ! In/Output
                                          Message_Log )     &  ! Error messaging
                                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Channel_Index
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption    

    ! -- In/Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_TL

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: n       ! Polynomial index

    REAL( fp_kind ) :: ave_IntAbsorber,        ave_IntAbsorber_TL
    REAL( fp_kind ) :: d_IntAbsorber,          d_IntAbsorber_TL
    REAL( fp_kind ) :: IntAbsorber_Level,      IntAbsorber_Level_TL
    REAL( fp_kind ) :: LN_Chi,                 LN_Chi_TL
    REAL( fp_kind ) :: Absorption_Coefficient, Absorption_Coefficient_TL

    ! -- Polynomial derived coefficients
    REAL( fp_kind ), DIMENSION( 0:MAX_N_PREDICTORS_USED ) :: b, b_TL



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#                                                                          #
    !#        This routine is a function for symmetry purposes only. :o)        #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Assign the channel index to a short name
    ! ----------------------------------------

    l = Channel_Index


    ! ------------------------------------------
    ! Initilise the tangent-linear optical depth
    ! ------------------------------------------

    AtmAbsorption_TL%Optical_Depth = ZERO


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------

    Absorber_Loop: DO j = 1, AtmAbsorption%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the absorber loop.
      ! -----------------------------------------

      IF ( TC%Predictor_Index( 0, j, l ) <= 0 ) CYCLE Absorber_Loop



      !#------------------------------------------------------------------------#
      !#                    -- BEGIN LOOP OVER LAYERS --                        #
      !#------------------------------------------------------------------------#

      Layer_Loop: DO k = 1, AtmAbsorption%n_Layers


        ! -----------------------------------
        ! Calculate the current layer average
        ! absorber amount and difference
        ! -----------------------------------

        ave_IntAbsorber    = POINT_5 * ( AtmAbsorption%IntAbsorber( k,   j ) + &
                                         AtmAbsorption%IntAbsorber( k-1, j )   )
        ave_IntAbsorber_TL = POINT_5 * ( AtmAbsorption_TL%IntAbsorber( k,   j ) + &
                                         AtmAbsorption_TL%IntAbsorber( k-1, j )   )

        d_IntAbsorber    = AtmAbsorption%IntAbsorber( k,   j ) - &
                           AtmAbsorption%IntAbsorber( k-1, j )
        d_IntAbsorber_TL = AtmAbsorption_TL%IntAbsorber( k,   j ) - &
                           AtmAbsorption_TL%IntAbsorber( k-1, j )



        ! ----------------------------------------------------------
        ! Calculate absorber space level associated with the average
        ! absorber amount
        ! 
        ! Absorber level, k, to amount 
        ! 
        !     A(k) = C1.exp(Alpha * k) + C2
        ! 
        ! Absorber amount to level 
        ! 
        !           1      A - C2
        !     k = ----- LN ------
        !         Alpha      C1
        ! 
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        !
        ! The tangent-linear equation is
        !
        !                   dA
        !     dk(A) = ----------------
        !             Alpha.( A - C2 )
        !
        ! ----------------------------------------------------------

        IntAbsorber_Level = LOG( ( ave_IntAbsorber - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !                   ------------------------------------------------------------
                                                TC%Alpha(j)

        IntAbsorber_Level_TL =                 ave_IntAbsorber_TL / &
        !                      ------------------------------------------------------
                               ( TC%Alpha(j) * ( ave_IntAbsorber - TC%Alpha_C2(j) ) )



        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          n
        !   b(i) =  > c(n,i).k
        !          /__
        !             n=0
        !
        ! The tangent-linear form is thus
        !
        !            __ N
        !           \            n-1
        !   db(i) =  > c(n,i).n.k    dk
        !           /__
        !              n=0
        !
        ! NOTE:
        ! 1) Note the actual computation of the b(i) and db(i) use a 
        !    recurrance relation, Horner's method, starting at the
        !    maximum polynomial order, N, to minimise round off error.
        !    So for a given predictor index i, we accumulate the value
        !    of b for successive orders of the N'th degree polynomial:
        !
        !      N:   b[N]   = c[N]
        !      N-1: b[N-1] = b[N].k + c[N-1]
        !                  = c[N].k + c[N-1]
        !      N-2: b[N-2] = b[N-1].k + c[N-2]
        !                  = (c[N].k + c[N-1]).k + c[N-1]
        !      N-3: b[N-3] = b[N-2].k + c[N-3]
        !                  = ((c[N].k + c[N-1]).k + c[N-1]).k + c[N-3]
        !    etc.
        !
        !    So for any polynomial order, n,
        !
        !      b[n] = b[n-1].k + c(n)
        !
        !    Thus the tangent linear form for db[n] is,
        !
        !     db[n] = b[n-1].dk  +  db[n-1].k
        !
        !    This means the tangent linear form, db[n] must be computed
        !    BEFORE the b[n-1] is updated to the b[n] value. This is
        !    noted in the code below also.
        !
        ! 2) The coefficient array, c(n,i), corresponds to the array
        !    TC%C(n,i,j,l) for the given absorber, j, and channel, l.
        !
        ! 3) The summation maximum, N, corresponds to the value in
        !    TC%Order_Index(i,j,l) for the given absorber, j, and
        !    channel, l. If
        !      TC%Order_Index(i,j,l) = 0
        !    then
        !      b(i)    = c(0,i)
        !    and
        !      b_TL(i) = 0.0
        !
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Initialise the polynomial sum
          ! -- TC%Order_Index( i, j, l ) contains the polynomial
          ! -- order to be used in reconstructing the b(i) coefficients
          ! -- and their tangent-linear form for the current predictor,
          ! -- i, for absorber j and channel l. This value is used to
          ! -- access the coefficient array, TC%C.
          b(i)    = TC%C( TC%Order_Index( i, j, l ), i, j, l )
          b_TL(i) = ZERO

          ! -- Loop over the polynomial orders and accumulate the
          ! -- coefficient value. Note that if
          ! --   TC%Order_Index( 0, j, l ) == 0
          ! -- then this loop is not entered. If your compiler has
          ! -- a "zero trip" loop option where DO loops are *always*
          ! -- executed at least once, regardless of the loop indices,
          ! -- make sure it's not on by default!
          ! --
          ! -- NOTE: The tangent-linear term is calculated FIRST
          ! --       See explanation note 1) above.
          DO n = TC%Order_Index( i, j, l ) - 1, 0, -1
            b_TL(i) = ( b(i) * IntAbsorber_Level_TL ) + ( b_TL(i) * IntAbsorber_Level )
            b(i)    = ( b(i) * IntAbsorber_Level ) + TC%C( n, i, j, l )
          END DO

        END DO


        ! ---------------------------------------------------------
        ! Compute the logarithm of the absorption coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! The tangent-linear form is
        !
        !               __Iuse
        !              \
        !   dLN(chi) =  >  (b(i).dPred(i)) + (db(i).Pred(i)) 
        !              /__
        !                 i=1
        !
        ! ---------------------------------------------------------

        ! -- Initialise the log of the absorption coefficient
        LN_Chi = b(0)
        LN_Chi_TL = b_TL(0)

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          LN_Chi    = LN_Chi    + ( b(i) * AtmAbsorption%Predictor( ip, k ) )
          LN_Chi_TL = LN_Chi_TL + ( b(i)    * AtmAbsorption_TL%Predictor( ip, k ) ) + &
                                  ( b_TL(i) * AtmAbsorption%Predictor(    ip, k ) )

        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------

        IF( LN_Chi > LIMIT_EXP ) THEN
          Absorption_Coefficient    = LIMIT_LOG
          Absorption_Coefficient_TL = ZERO
        ELSE IF( LN_Chi < -LIMIT_EXP ) THEN
          Absorption_Coefficient    = ZERO
          Absorption_Coefficient_TL = ZERO
        ELSE
          Absorption_Coefficient    = EXP( LN_Chi )
          Absorption_Coefficient_TL = Absorption_Coefficient * LN_Chi_TL
        ENDIF


        ! ------------------------------------------
        ! Calculate the tangent-linear optical depth
        ! ------------------------------------------

        AtmAbsorption_TL%Optical_Depth( k ) = AtmAbsorption_TL%Optical_Depth( k ) + &
                                              ( Absorption_Coefficient_TL * d_IntAbsorber    ) + &
                                              ( Absorption_Coefficient    * d_IntAbsorber_TL )

      END DO Layer_Loop

    END DO Absorber_Loop

    ! --- Compute tangent-linear nadir optical depth profile
    AtmAbsorption_TL%Optical_Depth = AtmAbsorption_TL%Optical_Depth / AtmAbsorption%Secant_Sensor_Zenith

  END FUNCTION CRTM_Compute_AtmAbsorption_TL





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Function to calculate the layer optical depths adjoint due
!       to gaseous absorption for a given input atmospheric profile for a
!       single channel.
!
!       This routine must be called *before* the CRTM_SetUp_AtmAbsorption_AD
!       routine.
!
! CATEGORY:
!       CRTM : Gas Absorption
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AtmAbsorption_AD( Channel_Index,            &  ! Input
!                                                     AtmAbsorption,            &  ! Input
!                                                     AtmAbsorption_AD,         &  ! In/Output
!                                                     Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access
!                           the shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:      CRTM_AtmAbsorption structure containing the
!                           gaseous absorption data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption_AD:   Upon entry, this CRTM_AtmAbsorption structure
!                           contains the adjoint optical depth profiles.
!                           ** NOTE: THIS STRUCTURE IS MODIFIED WITHIN **
!                           **       THIS FUNCTION                   **
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmAbsorption_AD:   Upon exit, this CRTM_AtmAbsorption structure
!                           contains the adjoint integrated absorber and
!                           predictor profiles. The adjoint optical depth
!                           component has been zeroed.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmAbsorption_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this function.
!
! RESTRICTIONS:
!       Their is no valid structure argument checking done in this routine.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AtmAbsorption_AD( Channel_Index,    &  ! Input
                                          AtmAbsorption,    &  ! Input
                                          AtmAbsorption_AD, &  ! In/Output
                                          Message_Log )     &  ! Error messaging
                                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    INTEGER,                         INTENT( IN )     :: Channel_Index
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN )     :: AtmAbsorption    

    ! -- In/Outputs
    TYPE( CRTM_AtmAbsorption_type ), INTENT( IN OUT ) :: AtmAbsorption_AD

    ! - Error messaging
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: n       ! Polynomial index

    REAL( fp_kind ) :: ave_IntAbsorber,        ave_IntAbsorber_AD
    REAL( fp_kind ) :: d_IntAbsorber,          d_IntAbsorber_AD
    REAL( fp_kind ) :: IntAbsorber_Level,      IntAbsorber_Level_AD
    REAL( fp_kind ) :: LN_Chi,                 LN_Chi_AD
    REAL( fp_kind ) :: Absorption_Coefficient, Absorption_Coefficient_AD

    ! -- Polynomial derived coefficients
    REAL( fp_kind ), DIMENSION( 0:MAX_N_ORDERS, &
                                0:MAX_N_PREDICTORS_USED  ) :: b
    REAL( fp_kind ), DIMENSION( 0:MAX_N_PREDICTORS_USED  ) :: b_AD



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#                                                                          #
    !#        This routine is a function for symmetry purposes only. :o)        #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                       -- SOME INITIALISATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Assign the channel index to a short name
    ! ----------------------------------------

    l = Channel_Index


    ! -------------------------------------
    ! Initilise the local adjoint variables
    ! -------------------------------------

    IntAbsorber_Level_AD = ZERO

    ! --------------------------------------------
    ! Compute Adjoint nadir optical depth profile
    ! --------------------------------------------
    
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth / AtmAbsorption%Secant_Sensor_Zenith

    !#--------------------------------------------------------------------------#
    !#                        -- LOOP OVER ABSORBERS --                         #
    !#--------------------------------------------------------------------------#

    Absorber_loop: DO j = 1, AtmAbsorption%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the Absorber loop.
      ! -----------------------------------------

      IF ( TC%Predictor_index( 0, j, l ) == 0 ) CYCLE Absorber_loop



      !#------------------------------------------------------------------------#
      !#                        -- LOOP OVER LAYERS --                          #
      !#------------------------------------------------------------------------#

      Layer_Loop: DO k = AtmAbsorption%n_Layers, 1, -1


        ! -----------------------------------
        ! Calculate the current layer average
        ! absorber amount and differences
        ! -----------------------------------

        ave_IntAbsorber = POINT_5 * ( AtmAbsorption%IntAbsorber( k,   j ) + &
                                      AtmAbsorption%IntAbsorber( k-1, j )   )
        d_IntAbsorber   = AtmAbsorption%IntAbsorber( k,   j ) - &
                          AtmAbsorption%IntAbsorber( k-1, j )



        !#----------------------------------------------------------------------#
        !#           -- HERE REPEAT THE FORWARD CALCULATION OF THE   --         #
        !#           -- ABSORPTION COEFFICIENT FOR THE CURRENT LAYER --         #
        !#----------------------------------------------------------------------#

        ! ----------------------------------------------------------
        ! Calculate absorber space level associated with the average
        ! absorber amount
        ! 
        ! Absorber level, k, to amount 
        ! 
        !     A(k) = C1.exp(Alpha * k) + C2
        ! 
        ! Absorber amount to level 
        ! 
        !           1      A - C2
        !     k = ----- LN ------
        !         Alpha      C1
        ! 
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        ! ----------------------------------------------------------

        IntAbsorber_Level = LOG( ( ave_IntAbsorber - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !                   ------------------------------------------------------------
                                                   TC%Alpha(j)


        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          n
        !   b(i) =  > c(n,i).k
        !          /__
        !             n=0
        !
        ! NOTE:
        ! 1) The actual b coefficient array is dimensioned as b(n,i) as
        !    we will need the accumulated b(i) values at each stage of the
        !    polynomial summation for computing the adjoint value.
        !
        ! 2) The coefficient array, c(n,i), corresponds to the array
        !    TC%C(n,i,j,l) for the given absorber, j, and channel, l.
        !
        ! 3) The summation maximum, N, corresponds to the value in
        !    TC%Order_Index(i,j,l) for the given absorber, j, and
        !    channel, l. If
        !      TC%Order_Index(i,j,l) = 0
        !    then
        !      b(i) = c(0,i)
        !
        ! 4) The computation of the b(i) below uses Horner's method,
        !    that's why the polynomial calculation *begins* at the
        !    maximum polynomial order, TC%Order_Index( i, j, l ).
        !
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Initialise the polynomial sum.
          ! -- TC%Order_Index( i, j, l ) contains the polynomial
          ! -- order to be used in reconstructing the b(i) coefficients
          ! -- for the current predictor, i, for absorber j and channel l.
          ! -- This value is used to access the coefficient array, TC%C.
          b( TC%Order_Index( i, j, l ), i) = TC%C( TC%Order_Index( i, j, l ), i, j, l )

          ! -- Loop over the polynomial orders and accumulate the
          ! -- coefficient value. Note that if
          ! --   TC%Order_Index( 0, j, l ) == 0
          ! -- then this loop is not entered. If your compiler has
          ! -- a "zero trip" loop option where DO loops are *always*
          ! -- executed at least once, regardless of the loop indices,
          ! -- make sure it's not on by default!
          DO n = TC%Order_Index( i, j, l ) - 1, 0, -1
            b(n,i) = ( b(n+1,i) * IntAbsorber_Level ) + TC%C( n, i, j, l )
          END DO

        END DO


        ! ---------------------------------------------------------
        ! Compute the logarithm of the absorption coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! Note that only the final, accumulated results for the
        ! b coefficients, the b(0,i) are used. The b(1:N,i) are
        ! used in the adjoint form of the calculation that produced
        ! the b coefficient values.
        ! ---------------------------------------------------------

        ! -- Initialise the log of the absorption coefficient
        LN_Chi = b(0,0)

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          LN_Chi = LN_Chi + ( b(0,i) * AtmAbsorption%Predictor( ip, k ) )

        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------

        IF( LN_Chi > LIMIT_EXP ) THEN
          Absorption_Coefficient = LIMIT_LOG
        ELSE IF( LN_Chi < -LIMIT_EXP ) THEN
          Absorption_Coefficient = ZERO
        ELSE
          Absorption_Coefficient = EXP( LN_Chi )
        ENDIF



        !#----------------------------------------------------------------------#
        !#                  -- BEGIN ADJOINT CALCULATIONS --                    #
        !#----------------------------------------------------------------------#

        ! ---------------------------------------------------------------
        ! Adjoints of the optical depth.
        !
        ! These quantities are local to the Layer_Loop and are equal to
        ! zero at this point so a straight initialisation is used, i.e.
        ! there is no
        !   d_IntAbsorber_AD          = d_IntAbsorber_AD + (...)
        !   Absorption_Coefficient_AD = Absorption_Coefficient_AD + (...)
        ! This also eliminates the need to zero out the two
        ! quanitities later in the loop once they no longer
        ! have an impact on the gradient vector result.
        !
        ! Also not that there is no
        !   AtmAbsorption_AD%Optical_Depth( k ) = ZERO
        ! because
        !   AtmAbsorption_TL%Optical_Depth( k ) = AtmAbsorption_TL%Optical_Depth( k ) + (....)
        ! ---------------------------------------------------------------

        d_IntAbsorber_AD = Absorption_Coefficient * AtmAbsorption_AD%Optical_Depth( k )   ! .... (1)
        Absorption_Coefficient_AD = d_IntAbsorber * AtmAbsorption_AD%Optical_Depth( k )



        ! ----------------------------------------
        ! Initialise the LOCAL adjoint variable,
        !   LN_Chi_AD.
        ! Note that the reinitialisaiton of the
        ! LOCAL adjoint variable
        !   Absorption_Coefficient_AD
        ! is implied since for each layer it is
        ! reassigned in the preceding line of code
        ! ----------------------------------------

        IF( ABS( LN_Chi ) > LIMIT_EXP ) THEN
          LN_Chi_AD = ZERO
        ELSE
          LN_Chi_AD = Absorption_Coefficient * Absorption_Coefficient_AD
        ENDIF


        ! ---------------------------------------------------------
        ! Compute the adjoint of the logarithm of the absorption
        ! coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! The tangent-linear form is
        !
        !               __Iuse
        !              \
        !   dLN(chi) =  >  (b(i).dPred(i)) + (db(i).Pred(i)) 
        !              /__
        !                 i=1
        !
        ! So the adjoint forms are for each predictor index i,
        !               
        !    *           *                *
        !   d Pred(i) = d Pred(i) + b(i).d LN(chi)
        !
        !
        ! and,
        !
        !
        !    *                *
        !   d b(i) = Pred(i).d LN(chi)
        !
        !            *
        ! where the d  indicates an adjoint variable. Note two
        ! things:
        ! 1) the order of the loop is not important.
        ! 2) the b coefficient adjoints are local adjoint variables
        !    and are thus initialised to their value on each
        !    iteration. I.e. no b_AD = ZERO before the loop.
        !
        ! ---------------------------------------------------------

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          AtmAbsorption_AD%Predictor(ip,k) = AtmAbsorption_AD%Predictor( ip,k ) + &
                                             ( b(0,i) * LN_Chi_AD )
          b_AD(i) = AtmAbsorption%Predictor(ip,k) * LN_Chi_AD

        END DO 

        ! -- Initialise the b(0) adjoint and zero the LN(Chi) adjoint
        b_AD(0) = LN_Chi_AD
        LN_Chi_AD = ZERO


        ! ----------------------------------------------------------------
        ! Compute the adjoints of the coefficients use with the
        ! atmospheric predictors.
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !                    __ N
        !                   \          n
        !   b(i) = c(0,i) +  > c(n,i).k
        !                   /__
        !                      n=1
        !
        ! The tangent-linear form is thus
        !
        !            __ N
        !           \            n-1
        !   db(i) =  > c(n,i).n.k    dk
        !           /__
        !              n=1
        !
        ! and the adjoint forms are,
        !
        !
        !             __ 1
        !    *       \          *
        !   d k(i) =  > b(n,i).d b
        !            /__
        !               n=N
        !
        ! and
        !
        !    *           *
        !   d b(i) = k.d b(i)
        !
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Loop over the polynomial orders in the reverse order
          ! -- Note that the order of the IntAbsorber_Level_AD and b_AD
          ! -- calculation are important
          DO n = 0, TC%Order_Index( i, j, l ) - 1
            IntAbsorber_Level_AD = IntAbsorber_Level_AD + ( b(n+1,i) * b_AD(i) )
            b_AD(i) = IntAbsorber_Level * b_AD(i)
          END DO

          ! -- Reset the b coefficient adjoint
          b_AD(i) = ZERO

        END DO


        ! ----------------------------------------------------------
        ! Calculate the adjoint of the absorber space level
        ! associated with average absorber amount
        !
        ! Absorber level to amount
        !
        !     A(k) = C1 exp(Alpha * k) + C2
        !
        ! Absorber amount to level
        !
        !              1      A - C2
        !     k(A) = ----- ln ------
        !            Alpha      C1
        !
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        !
        ! The tangent-linear equation is
        !
        !                   dA
        !     dk(A) = ----------------
        !             Alpha.( A - C2 )
        !
        ! and the adjoint form is
        !
        !                *
        !    *          d k
        !   d A = ----------------
        !         Alpha.( A - C2 )
        !
        ! ----------------------------------------------------------

        ave_IntAbsorber_AD =               IntAbsorber_Level_AD / &
        !                    ------------------------------------------------------  ....(2)
                             ( TC%Alpha(j) * ( ave_IntAbsorber - TC%Alpha_C2(j) ) )

        IntAbsorber_Level_AD = ZERO



        ! ---------------------------------------------------
        ! Adjoints of the current layer average absorber
        ! amount and difference.
        !
        ! Neither d_IntAbsorber_AD nor ave_IntAbsorber_AD need
        ! to be set to zero after this as they are explicitly
        ! reassigned each layer iteration at (1) and (2) above
        ! respectively.
        ! ---------------------------------------------------

        AtmAbsorption_AD%IntAbsorber( k-1, j ) = AtmAbsorption_AD%IntAbsorber( k-1, j ) - &
                                                 d_IntAbsorber_AD + ( POINT_5 * ave_IntAbsorber_AD )
        AtmAbsorption_AD%IntAbsorber( k,   j ) = AtmAbsorption_AD%IntAbsorber( k,   j ) + &
                                                 d_IntAbsorber_AD + ( POINT_5 * ave_IntAbsorber_AD )

      END DO Layer_Loop

    END DO Absorber_Loop

  END FUNCTION CRTM_Compute_AtmAbsorption_AD

END MODULE CRTM_AtmAbsorption


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_AtmAbsorption.f90,v 2.5 2005/08/16 16:19:28 qliu Exp $
!
! $Date: 2005/08/16 16:19:28 $
!
! $Revision: 2.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AtmAbsorption.f90,v $
! Revision 2.5  2005/08/16 16:19:28  qliu
! - Scaling the slant-path optical depth to nadir. The RTSolution code requires
!   nadir optical depths.
!
! Revision 2.3  2005/02/16 22:43:23  paulv
! - Made all routines functions. Preparing for different error handling
!   within main component modules.
!
! Revision 2.2  2005/01/31 21:37:43  paulv
! - The allocation of the AtmAbsorption structure in the SetUp() functions was
!   removed. It is now assumed that all structures are allocated before
!   entry to the SetUp() and Compute() routines.
! - Since there is no allocation in the SetUp() routines, they were changed
!   from functions to subroutines.
!
! Revision 2.1  2005/01/28 21:08:55  paulv
! - Fixed some naming errors.
! - Removed the structure deallocation from the adjoint routine. Need to
!   address this further.
!
! Revision 2.0  2005/01/21 18:37:53  paulv
! - Removed the integrated absorber and predictor routines and placed them
!   in their respective modules.
! - Added the tangent-linear and adjoint routines for the CRTM_SetUp_AtmAbsorption()
!   and CRTM_Compute_AtmAbsorption() routines.
! - Untested.
!
! Revision 1.7  2004/12/29 20:56:37  paulv
! - Added tangent-linear absorber profile and predictor routines. UNTESTED.
!
! Revision 1.6  2004/11/05 16:41:59  paulv
! - Corrected name of integrated absorber amount variable in predictor
!   calculation.
!
! Revision 1.5  2004/11/03 17:32:21  paulv
! - Upgraded to Fortran-95
! - Removed Init() routine from PUBLIC list.
! - Now using a correct test for computing the inverse of powers of the
!   absorber amount in the integrated predictor routine. A minimum absorber
!   amount parameter has been defined to ensure that Inverse_1, Inverse_2,
!   Inverse_3, and Inverse_4 are all representable.
! - Intent of output structures in the SetUp() and Compute() routines
!   changed from (OUT) to (IN OUT) to prevent memory leaks.
! - Updated documentation.
!
! Revision 1.4  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.3  2004/06/29 17:00:39  paulv
! - Corrected documentation error.
!
! Revision 1.2  2004/06/23 14:34:21  paulv
! - Finished the set up function.
! - Updated documentation headers.
!
! Revision 1.1  2004/06/18 20:15:02  paulv
! Initial checkin.
!
!
!
