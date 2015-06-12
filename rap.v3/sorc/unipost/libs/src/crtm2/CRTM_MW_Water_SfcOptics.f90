!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_MW_Water_SfcOptics
!
! PURPOSE:
!       Module to compute the surface optical properties for WATER surfaces at
!       microwave frequencies required for determining the WATER surface
!       contribution to the radiative transfer.
!
!       This module is provided to allow developers to "wrap" their existing
!       codes inside the provided functions to simplify integration into
!       the main CRTM_SfcOptics module.
!       
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_MW_Water_SfcOptics
!
! MODULES:
!       Type_Kinds:                Module containing definitions for kinds
!                                  of variable types.
!
!       Error_Handler:             Module to define simple error codes and
!                                  handle error conditions
!                                  USEs: FILE_UTILITY module
!
!       CRTM_Parameters:           Module of parameter definitions for the CRTM.
!                                  USEs: TYPE_KINDS module
!
!       CRTM_SpcCoeff:             Module containing the shared CRTM spectral
!                                  coefficients (SpcCoeff) and their
!                                  load/destruction routines. 
!                                  USEs TYPE_KINDS module
!                                       ERROR_HANDLER module
!                                       SPCCOEFF_DEFINE module
!                                       SPCCOEFF_BINARY_IO module
!                                       CRTM_PARAMETERS module
!
!       CRTM_Surface_Define:       Module defining the CRTM Surface
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       CRTM_GeometryInfo_Define:  Module defining the CRTM GeometryInfo
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       CRTM_SfcOptics_Define:     Module defining the CRTM SfcOptics
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!       Compute_MW_Water_SfcOptics:    Function to compute the surface emissivity
!                                      and reflectivity at microwave frequencies
!                                      over a water surface.
!
!       Compute_MW_Water_SfcOptics_TL: Function to compute the tangent-linear
!                                      surface emissivity and reflectivity at
!                                      microwave frequencies over a water surface.
!
!       Compute_MW_Water_SfcOptics_AD: Function to compute the adjoint surface
!                                      emissivity and reflectivity at microwave
!                                      frequencies over a water surface.
!
!       PRIVATE subprograms
!       -------------------
!       None.
!
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
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!M-
!--------------------------------------------------------------------------------

MODULE CRTM_MW_Water_SfcOptics

  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_SpcCoeff,            ONLY: SC
  USE CRTM_Surface_Define
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type

  ! -- Ocean surface emission module
  USE NESDIS_OCEANEM_Module
  USE CRTM_Fastem1

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Science routines
  PUBLIC :: Compute_MW_Water_SfcOptics  
  PUBLIC :: Compute_MW_Water_SfcOptics_TL
  PUBLIC :: Compute_MW_Water_SfcOptics_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! Switch parameter whether use Fastem1 or NESDIS models.
  LOGICAL, PARAMETER :: FASTEM = .TRUE.

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_MW_Water_SfcOptics.f90,v 1.4 2005/08/16 20:11:37 qliu Exp $'
 
  ! -- Intermediate variables to hold analytic Jacobians of Emissivity to
  ! -- surface temperature and wind speed.  
  INTEGER, PARAMETER :: MAX_N_ANLGES = 128
  REAL(fp_kind), SAVE, DIMENSION(MAX_N_ANLGES) :: dEH_dTs, &
                                                  dEH_dWindSpeed, &
                                                  dEV_dTs, &
                                                  dEV_dWindSpeed
CONTAINS



!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_MW_Water_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at microwave
!       frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Water_SfcOptics( Surface,                  &  ! Input
!                                                  GeometryInfo,             &  ! Input
!                                                  Channel_Index,            &  ! Input, scalar
!                                                  SfcOptics,                &  ! Output     
!                                                  Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
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
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
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
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       None of the input or output structures are checked in this routine as
!       it will be called for every channel.
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT as it is assumed to contain some data upon input.
!
!S-
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Water_SfcOptics( Surface,       &  ! Input
                                       GeometryInfo,  &  ! Input
                                       Channel_Index, &  ! Input
                                       SfcOptics,     &  ! Output
                                       Message_Log )  &  ! Error messaging
                                     RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- In/Output 
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics'

    ! ---------------
    ! Local variables
    ! ---------------

    REAL(fp_kind), PARAMETER :: LOW_SALINITY = 10.0_fp_kind
    REAL(fp_kind)            :: salinity
    REAL(fp_kind)            :: Frequency
    INTEGER                  :: i
    CHARACTER(256)           :: Message
    REAL(fp_kind)            :: Emissivity(4), Reflectivity(4)

    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

    ! -- frequency GHz
    Frequency = SC%Frequency(Channel_Index)

    IF( Surface%Water_Type == SEA_WATER )THEN

      salinity = Surface%Salinity

    ELSE IF( Surface%Water_Type == FRESH_WATER )THEN

      salinity = LOW_SALINITY

    ELSE

      Error_Status = FAILURE
      WRITE( Message, '( "Invalid water surface type: ", i4 )' ) Surface%water_Type
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    ENDIF
    
    SfcOptics%Reflectivity = ZERO
    dEH_dTs = ZERO
    dEV_dTs = ZERO
    
    DO i = 1, SfcOptics%n_Angles

      IF( FASTEM ) THEN
        CALL Fastem1_OCeanEM(SC%Frequency(Channel_Index),                       & ! INPUT
                             SfcOptics%Angle(i),                                & ! INPUT
                             Surface%Water_Temperature,                         & ! INPUT
                             Surface%Wind_Speed,                                & ! INPUT
                             Emissivity,                                        & ! OUTPUT
                             dEH_dWindSpeed(i),                                 & ! OUTPUT)  
                             dEV_dWindSpeed(i))

      SfcOptics%Emissivity(i, 1) = Emissivity(1)
      SfcOptics%Emissivity(i, 2) = Emissivity(2)

      ELSE
        CALL NESDIS_OCeanEM(SC%Frequency(Channel_Index),          & ! INPUT, GHz
                          SfcOptics%Angle(i),                     & ! INPUT, Degree
                          Surface%Water_Temperature,              & ! INPUT, K
                          Surface%Wind_Speed ,                    & ! INPUT, m/s
                          Surface%Salinity,                       & ! INPUT, 1/Thousand
                          SfcOptics%Emissivity(i, 2),             & ! OUTPUT, H component
                          SfcOptics%Emissivity(i, 1),             & ! OUTPUT, V component
                          dEH_dTs(i),                             & ! OUTPUT, not used in FW model
                          dEH_dWindSpeed(i),                      & ! OUTPUT, not used in FW model
                          dEV_dTs(i),                             & ! OUTPUT, not used in FW model
                          dEV_dWindSpeed(i))                        ! OUTPUT, not used in FW model
   
    END IF
      ! -- Assume specular surface
      SfcOptics%Reflectivity(i, 1, i, 1) = ONE - SfcOptics%Emissivity(i, 1)
      SfcOptics%Reflectivity(i, 2, i, 2) = ONE - SfcOptics%Emissivity(i, 2)

    ENDDO

  END FUNCTION Compute_MW_Water_SfcOptics


!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_MW_Water_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at microwave frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Water_SfcOptics_TL( Surface,                  &  ! Input
!                                                     SfcOptics,                &  ! Input     
!                                                     Surface_TL,               &  ! Input
!                                                     GeometryInfo,             &  ! Input
!                                                     Channel_Index,            &  ! Input, scalar
!                                                     SfcOptics_TL,             &  ! Output     
!                                                     Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear 
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
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
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
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
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       None of the input or output structures are checked in this routine.
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!----------------------------------------------------------------------------------


  FUNCTION Compute_MW_Water_SfcOptics_TL( Surface,       &  ! Input
                                          SfcOptics,     &  ! Input     
                                          Surface_TL,    &  ! Input
                                          GeometryInfo,  &  ! Input
                                          Channel_Index, &  ! Input, scalar
                                          SfcOptics_TL,  &  ! Output     
                                          Message_Log )  &  ! Error messaging 
                                        RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface_TL
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN )     :: SfcOptics
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- In/Output 
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics_TL

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics_TL'

    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER                   :: i
    CHARACTER(256)            :: Message

    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

    IF( Surface%Water_Type /= SEA_WATER .AND. Surface%Water_Type /= FRESH_WATER)THEN

      Error_Status = FAILURE
      WRITE( Message, '( "Invalid water surface type: ", i4 )' ) Surface%water_Type
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    ENDIF

    SfcOptics_TL%Reflectivity = ZERO

    DO i = 1, SfcOptics%n_Angles

      SfcOptics_TL%Emissivity(i, 2) = dEH_dTs(i)*Surface_TL%Water_Temperature + &
                                      dEH_dWindSpeed(i)*Surface_TL%Wind_Speed

      SfcOptics_TL%Emissivity(i, 1) = dEV_dTs(i)*Surface_TL%Water_Temperature + &
                                      dEV_dWindSpeed(i)*Surface_TL%Wind_Speed

      SfcOptics_TL%Reflectivity(i, 1, i, 1) = -SfcOptics_TL%Emissivity(i, 1)
      SfcOptics_TL%Reflectivity(i, 2, i, 2) = -SfcOptics_TL%Emissivity(i, 2)

    ENDDO

  END FUNCTION Compute_MW_Water_SfcOptics_TL

!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_MW_Water_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at microwave frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Water_SfcOptics_AD( Surface,                  &  ! Input
!                                                     SfcOptics,                &  ! Input     
!                                                     SfcOptics_AD,             &  ! Input     
!                                                     GeometryInfo,             &  ! Input
!                                                     Channel_Index,            &  ! Input, scalar
!                                                     Surface_AD,               &  ! Output
!                                                     Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties required for the adjoint
!                        radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_SfcOptics_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_GeometryInfo_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
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
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Surface_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!       None of the input or output structures are checked in this routine.
!
! COMMENTS:
!       Note the INTENT on the input SfcOptics_AD argument is IN OUT rather
!       than just OUT. This is necessary because components of this argument
!       may need to be zeroed out upon output.
!
!       Note the INTENT on the output Surface_AD argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!----------------------------------------------------------------------------------


  FUNCTION Compute_MW_Water_SfcOptics_AD( Surface,       &  ! Input
                                          SfcOptics,     &  ! Input     
                                          SfcOptics_AD,  &  ! Input
                                          GeometryInfo,  &  ! Input
                                          Channel_Index, &  ! Input, scalar
                                          Surface_AD,    &  ! Output     
                                          Message_Log )  &  ! Error messaging 
                                        RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN )     :: SfcOptics
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics_AD
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- In/Output 
    TYPE( CRTM_Surface_type ),      INTENT( IN OUT ) :: Surface_AD

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Water_SfcOptics_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    !### USER LOCAL VARIABLES ARE DECLARED HERE ###
    INTEGER                  :: i
    CHARACTER(256)           :: Message


    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

    IF( Surface%Water_Type /= SEA_WATER .AND. Surface%Water_Type /= FRESH_WATER)THEN

      Error_Status = FAILURE
      WRITE( Message, '( "Invalid water surface type: ", i4 )' ) Surface%water_Type
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    ENDIF

    DO i = SfcOptics%n_Angles, 1, -1

      SfcOptics_AD%Emissivity(i, 2) = SfcOptics_AD%Emissivity(i, 2) - &
                                      SfcOptics_AD%Reflectivity(i, 2, i, 2)
      SfcOptics_AD%Reflectivity(i, 2, i, 2) = ZERO

      SfcOptics_AD%Emissivity(i, 1) = SfcOptics_AD%Emissivity(i, 1) - &
                                      SfcOptics_AD%Reflectivity(i, 1, i, 1)
      SfcOptics_AD%Reflectivity(i, 1, i, 1) = ZERO

      Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
                                       dEV_dTs(i)*SfcOptics_AD%Emissivity(i, 1)
      
      Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
                                      dEV_dWindSpeed(i)*SfcOptics_AD%Emissivity(i, 1)
      SfcOptics_AD%Emissivity(i, 1) = ZERO

      Surface_AD%Water_Temperature  = Surface_AD%Water_Temperature + &
                                       dEH_dTs(i)*SfcOptics_AD%Emissivity(i, 2)
      
      Surface_AD%Wind_Speed         = Surface_AD%Wind_Speed + &
                                      dEH_dWindSpeed(i)*SfcOptics_AD%Emissivity(i, 2)
      SfcOptics_AD%Emissivity(i, 2) = ZERO
            
    ENDDO      

  END FUNCTION Compute_MW_Water_SfcOptics_AD

END MODULE CRTM_MW_Water_SfcOptics


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_MW_Water_SfcOptics.f90,v 1.4 2005/08/16 20:11:37 qliu Exp $
!
! $Date: 2005/08/16 20:11:37 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_MW_Water_SfcOptics.f90,v $
! Revision 1.4  2005/08/16 20:11:37  qliu
! - First working version of SfcOptics modules.
!
! Revision 1.3  2005/06/29 20:24:52  paulv
! - Removed example forward model calls.
!
! Revision 1.2  2005/06/27 13:16:01  paulv
! - Add functionality to wrapper functions. Taken from Quanhua's mods to the
!   original CRTM_SfcOptics module.
!
! Revision 1.1  2005/06/26 15:25:26  paulv
! Initial checkin.
!
!
!
!
