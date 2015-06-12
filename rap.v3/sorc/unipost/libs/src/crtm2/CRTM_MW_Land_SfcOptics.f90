!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_MW_Land_SfcOptics
!
! PURPOSE:
!       Module to compute the surface optical properties for LAND surfaces at
!       microwave frequencies required for determining the LAND surface
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
!       USE CRTM_MW_Land_SfcOptics
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
!       Compute_MW_Land_SfcOptics:    Function to compute the surface emissivity
!                                     and reflectivity at microwave frequencies
!                                     over a land surface.
!
!       Compute_MW_Land_SfcOptics_TL: Function to compute the tangent-linear
!                                     surface emissivity and reflectivity at
!                                     microwave frequencies over a land surface.
!
!       Compute_MW_Land_SfcOptics_AD: Function to compute the adjoint surface
!                                     emissivity and reflectivity at microwave
!                                     frequencies over a land surface.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!M-
!--------------------------------------------------------------------------------

MODULE CRTM_MW_Land_SfcOptics

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
  USE CRTM_SensorInfo
                                                                                                                  
  ! Land emission model
  USE NESDIS_LandEM_Module,  ONLY: NESDIS_LandEM

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
  PUBLIC :: Compute_MW_Land_SfcOptics  
  PUBLIC :: Compute_MW_Land_SfcOptics_TL
  PUBLIC :: Compute_MW_Land_SfcOptics_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_MW_Land_SfcOptics.f90,v 1.6 2005/08/16 20:11:37 qliu Exp $'


CONTAINS



!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_MW_Land_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at microwave
!       frequencies over a land surface.
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
!       Error_Status = Compute_MW_Land_SfcOptics( Surface,                  &  ! Input
!                                                 GeometryInfo,             &  ! Input
!                                                 Channel_Index,            &  ! Input, scalar
!                                                 SfcOptics,                &  ! Output     
!                                                 Message_Log = Message_Log )  ! Error messaging 
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

  FUNCTION Compute_MW_Land_SfcOptics( Surface,       &  ! Input
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER                   :: i, WMO_Sensor_ID
    REAL(fp_kind)             :: Frequency

    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

    ! -- frequency GHz
    Frequency = SC%Frequency(Channel_Index)  

    SfcOptics%Reflectivity = ZERO

    WMO_Sensor_ID = SC%WMO_Sensor_ID(Channel_Index)

    DO i = 1, SfcOptics%n_Angles

        IF( Frequency < 80.0 ) THEN 
          CALL NESDIS_LandEM(SfcOptics%Angle(i),                & ! INPUT, Degree
                             Frequency,                         & ! INPUT, GHz
                             Surface%Soil_Moisture_Content,     & ! INPUT, g.cm^-3
                             Surface%Vegetation_Fraction,       & ! Input
                             Surface%Soil_Temperature,          & ! Input, K
                             Surface%Land_Temperature,          & ! Input, K
                             ZERO,                              & ! Input, Snow depth, mm
                             SfcOptics%Emissivity(i, 2),        & ! OUTPUT, H component
                             SfcOptics%Emissivity(i, 1))          ! OUTPUT, V component
        ELSE
          SfcOptics%Emissivity(i, 1) = 0.95
          SfcOptics%Emissivity(i, 2) = 0.95
        END IF

      ! -- Assume specular surface
      SfcOptics%Reflectivity(i, 1, i, 1) = ONE - SfcOptics%Emissivity(i, 1)
      SfcOptics%Reflectivity(i, 2, i, 2) = ONE - SfcOptics%Emissivity(i, 2)

    ENDDO


  END FUNCTION Compute_MW_Land_SfcOptics

!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_MW_Land_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at microwave frequencies over a land surface.
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
!       Error_Status = Compute_MW_Land_SfcOptics_TL( Surface,                  &  ! Input
!                                                    SfcOptics,                &  ! Input     
!                                                    Surface_TL,               &  ! Input
!                                                    GeometryInfo,             &  ! Input
!                                                    Channel_Index,            &  ! Input, scalar
!                                                    SfcOptics_TL,             &  ! Output     
!                                                    Message_Log = Message_Log )  ! Error messaging 
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


  FUNCTION Compute_MW_Land_SfcOptics_TL( Surface,       &  ! Input
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    !### USER LOCAL VARIABLES ARE DECLARED HERE ###

     SfcOptics_TL%Reflectivity = ZERO
     SfcOptics_TL%Emissivity = ZERO

    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS





    !###USER CODE GOES HERE ###





  END FUNCTION Compute_MW_Land_SfcOptics_TL





!----------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_MW_Land_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at microwave frequencies over a land surface.
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
!       Error_Status = Compute_MW_Land_SfcOptics_AD( Surface,                  &  ! Input
!                                                    SfcOptics,                &  ! Input     
!                                                    SfcOptics_AD,             &  ! Input     
!                                                    GeometryInfo,             &  ! Input
!                                                    Channel_Index,            &  ! Input, scalar
!                                                    Surface_AD,               &  ! Output
!                                                    Message_Log = Message_Log )  ! Error messaging 
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


  FUNCTION Compute_MW_Land_SfcOptics_AD( Surface,       &  ! Input
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Land_SfcOptics_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    !### USER LOCAL VARIABLES ARE DECLARED HERE ###



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS





    !###USER CODE GOES HERE ###





  END FUNCTION Compute_MW_Land_SfcOptics_AD

END MODULE CRTM_MW_Land_SfcOptics


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_MW_Land_SfcOptics.f90,v 1.6 2005/08/16 20:11:37 qliu Exp $
!
! $Date: 2005/08/16 20:11:37 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_MW_Land_SfcOptics.f90,v $
! Revision 1.6  2005/08/16 20:11:37  qliu
! - First working version of SfcOptics modules.
!
! Revision 1.5  2005/06/29 20:24:52  paulv
! - Removed example forward model calls.
!
! Revision 1.4  2005/06/27 13:52:21  paulv
! - Corrected bug in LandEm calls. I used the wrong kind type in converting
!   arguments.
!
! Revision 1.3  2005/06/27 13:16:01  paulv
! - Add functionality to wrapper functions. Taken from Quanhua's mods to the
!   original CRTM_SfcOptics module.
!
! Revision 1.2  2005/06/26 15:30:13  paulv
! - Corrected usage docs.
!
! Revision 1.1  2005/06/24 16:51:55  paulv
! Initial checkin.
!
!
!
!
