!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_LifeCycle
!
! PURPOSE:
!       Module containing CRTM life cycle functions to initialize and destroy
!       the CRTM space.
!
! CATEGORY:
!       CRTM
!
! CALLING SEQUENCE:
!       USE CRTM_LifeCycle
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Error_Handler:      Module to define error codes and handle
!                           error conditions.
!                           USEs: FILE_UTILITY module
!
!       CRTM_SpcCoeff:      Module containing the shared CRTM spectral
!                           coefficients and their load/destruction
!                           routines.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 SPCCOEFF_DEFINE module
!                                 SPCCOEFF_BINARY_IO module
!                                 CRTM_PARAMETERS module
!
!       CRTM_TauCoeff:      Module containing the shared CRTM gas absorption
!                           coefficients and their load/destruction
!                           routines.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 TAUCOEFF_DEFINE module
!                                 TAUCOEFF_BINARY_IO module
!                                 CRTM_PARAMETERS module
!
!       CRTM_AerosolCoeff:  Module containing the shared CRTM aerosol
!                           coefficients and their load/destruction
!                           routines. 
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 AEROSOLCOEFF_DEFINE module
!                                 AEROSOLCOEFF_BINARY_IO module
!                                 CRTM_PARAMETERS module
!
!       CRTM_CloudCoeff:  Module containing the shared CRTM scattering
!                           coefficients and their load/destruction
!                           routines.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 CloudCoeff_DEFINE module
!                                 CloudCoeff_BINARY_IO module
!                                 CRTM_PARAMETERS module
!
!       CRTM_EmisCoeff:     Module containing the shared CRTM IRSSE
!                           emissivity coefficients and their load/destruction
!                           routines.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 EMISCOEFF_DEFINE module
!                                 EMISCOEFF_BINARY_IO module
!                                 CRTM_PARAMETERS module
!
!       CRTM_ChannelInfo:   Module containing routines to populate the CRTM
!                           ChannelInfo structure.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 CRTM_CHANNELINFO_DEFINE module
!
! CONTAINS:
!       CRTM_Init:         Function to initialise the CRTM.
!
!       CRTM_Destroy:      Function to destroy the CRTM space.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       Various shared data structures are allocated and filled with data
!       from file.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
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

MODULE CRTM_LifeCycle


  ! ----------
  ! Module use
  ! ----------

  USE Error_handler

  USE CRTM_SpcCoeff
  USE CRTM_TauCoeff
  USE CRTM_AerosolCoeff
  USE CRTM_CloudCoeff
  USE CRTM_EmisCoeff

  USE CRTM_ChannelInfo


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Init
  PUBLIC :: CRTM_Destroy


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_LifeCycle.f90,v 1.7.2.3 2005/08/23 13:06:03 yhan Exp $'


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Init
!
! PURPOSE:
!       Function to initialise the CRTM.
!
! CATEGORY:
!       CRTM
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Init( ChannelInfo,                           &  ! Output
!                                 SpcCoeff_File     = SpcCoeff_File,     &  ! Optional input
!                                 TauCoeff_File     = TauCoeff_File,     &  ! Optional input
!                                 AerosolCoeff_File = AerosolCoeff_File, &  ! Optional input
!                                 CloudCoeff_File = CloudCoeff_File, &  ! Optional input
!                                 EmisCoeff_File    = EmisCoeff_File,    &  ! Optional input
!                                 Sensor_Descriptor = Sensor_Descriptor, &  ! Optional input
!                                 NCEP_Sensor_ID    = NCEP_Sensor_ID,    &  ! Optional input
!                                 Sensor_Channel    = Sensor_Channel,    &  ! Optional input
!                                 File_Path         = File_Path,         &  ! Optional input
!                                 Quiet             = Quiet,             &  ! Optional input
!                                 Process_ID        = Process_ID,        &  ! Optional input
!                                 Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                 RCS_Id            = RCS_Id,            &  ! Revision control
!                                 Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       SpcCoeff_File:      Name of the CRTM Binary format SpcCoeff file
!                           containing the spectral coefficient data. If not
!                           specified, "SpcCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       TauCoeff_File:      Name of the CRTM Binary format TauCoeff file
!                           containing the gas absorption coefficient data. If not
!                           specified, "TauCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       AerosolCoeff_File:  Name of the CRTM Binary format AerosolCoeff file
!                           containing the aerosol absorption and scattering
!                           coefficient data. If not specified, "AerosolCoeff.bin"
!                           is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       CloudCoeff_File:  Name of the CRTM Binary format CloudCoeff file
!                           containing the scattering coefficient data. If not
!                           specified, "CloudCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       EmisCoeff_File:     Name of the CRTM Binary format EmisCoeff file
!                           containing the IRSSE emissivity coefficient data. If not
!                           specified, "EmisCoeff.bin" is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Descriptor:  List of satellite/sensor descriptors for
!                           each channel the user wants to process. If
!                           not specified, all the channels defined by
!                           the SpcCoeff data will be processed. Used
!                           with the Sensor_Channel argument.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Rank-1, same as Sensor_Channel argument
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       NCEP_Sensor_ID:     List of NCEP sensor ids for each channel
!                           the user wants to process. If not specified,    
!                           all the channels defined by the SpcCoeff data   
!                           will be processed. Used with the Sensor_Channel 
!                           argument. Ignored if the Sensor_Descriptor      
!                           argument is passed.                             
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, same as Sensor_Channel argument
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
!       Sensor_Channel:     List of channel numbers for each sensor
!                           the user wants to process. Used with
!                           either the Sensor_Descriptor or
!                           NCEP_Sensor_Id argument, and ignored
!                           if neither of these are present.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       File_Path:          Character string specifying a file path for the
!                           input data files. If not specified, the current
!                           directory is the default.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Quiet:              Set this argument to suppress INFORMATION messages
!                           being printed to standard output (or the message
!                           log file if the Message_Log optional argument is
!                           used.) By default, INFORMATION messages are printed.
!                           If QUIET = 0, INFORMATION messages are OUTPUT.
!                              QUIET = 1, INFORMATION messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all INFORMATION messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the INFORMATION messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to the screen.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:        ChannelInfo structure populated based on the contents
!                           of the coefficient files and the user inputs.
!                           UNITS:      N/A
!                           TYPE:       CRTM_ChannelInfo_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the CRTM initialisation was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
!       CRTM_Load_SpcCoeff:      Function to load the SpcCoeff spectral
!                                coefficient data into the public data
!                                structure SC. 
!                                SOURCE: CRTM_SPCCOEFF module
!
!       CRTM_Load_TauCoeff:      Function to load the TauCoeff gas absorption
!                                coefficient data into the public data
!                                structure TC. 
!                                SOURCE: CRTM_TAUCOEFF module
!
!       CRTM_Load_AerosolCoeff:  Function to load the AerosolCoeff absorption
!                                and scattering coefficient data into the public
!                                data structure AeroC. 
!                                SOURCE: CRTM_AEROSOLCOEFF module
!
!       CRTM_Load_CloudCoeff:  Function to load the CloudCoeff scattering
!                                coefficient data into the public data
!                                structure ScatC. 
!                                SOURCE: CRTM_CloudCoeff module
!
!       CRTM_Load_EmisCoeff:     Function to load the EmisCoeff IRSSE emissivity
!                                coefficient data into the public data
!                                structure EmisC. 
!                                SOURCE: CRTM_EMISCOEFF module
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       All public data arrays accessed by this module and its dependencies
!       are overwritten.
!
! RESTRICTIONS:
!       If specified, the length of the combined file path and filename strings
!       cannot exceed 512 characters.
!
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Init( ChannelInfo,       &  ! Output
                      SpcCoeff_File,     &  ! Optional input
                      TauCoeff_file,     &  ! Optional input
                      AerosolCoeff_File, &  ! Optional input
                      CloudCoeff_File, &  ! Optional input
                      EmisCoeff_File,    &  ! Optional input
                      File_Path,         &  ! Optional input
                      Sensor_Descriptor, &  ! Optional input
                      NCEP_Sensor_ID,    &  ! Optional input
                      Sensor_Channel,    &  ! Optional input
                      Quiet,             &  ! Optional input
                      Process_ID,        &  ! Optional input
                      Output_Process_ID, &  ! Optional input
                      RCS_Id,            &  ! Revision control
                      Message_Log )      &  ! Error messaging
                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Output
    TYPE( CRTM_ChannelInfo_type ),            INTENT( IN OUT ) :: ChannelInfo

    ! -- Optional input
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: SpcCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: TauCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: AerosolCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: CloudCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: EmisCoeff_File
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: File_Path
    CHARACTER( * ), OPTIONAL, DIMENSION( : ), INTENT( IN )     :: Sensor_Descriptor
    INTEGER,        OPTIONAL, DIMENSION( : ), INTENT( IN )     :: NCEP_Sensor_ID
    INTEGER,        OPTIONAL, DIMENSION( : ), INTENT( IN )     :: Sensor_Channel
    INTEGER,        OPTIONAL,                 INTENT( IN )     :: Quiet
    INTEGER,        OPTIONAL,                 INTENT( IN )     :: Process_ID
    INTEGER,        OPTIONAL,                 INTENT( IN )     :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,                 INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,                 INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Init'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 512 ) :: Default_SpcCoeff_File
    CHARACTER( 512 ) :: Default_TauCoeff_File
    CHARACTER( 512 ) :: Default_AerosolCoeff_File
    CHARACTER( 512 ) :: Default_CloudCoeff_File
    CHARACTER( 512 ) :: Default_EmisCoeff_File

    INTEGER :: l



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK THE OPTIONAL FILE NAME/PATH ARGUMENTS --            #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Specify the default filenames
    ! -----------------------------

    Default_SpcCoeff_File     = 'SpcCoeff.bin'
    Default_TauCoeff_File     = 'TauCoeff.bin'
    Default_AerosolCoeff_File = 'AerosolCoeff.bin'
    Default_CloudCoeff_File = 'CloudCoeff.bin'
    Default_EmisCoeff_File    = 'EmisCoeff.bin'


    ! -------------------------------
    ! Were other filenames specified?
    ! -------------------------------

    IF ( PRESENT( SpcCoeff_File ) ) &
      Default_SpcCoeff_File = TRIM( ADJUSTL( SpcCoeff_File ) )

    IF ( PRESENT( TauCoeff_File ) ) &
      Default_TauCoeff_File = TRIM( ADJUSTL( TauCoeff_File ) )

    IF ( PRESENT( AerosolCoeff_File ) ) &
      Default_AerosolCoeff_File = TRIM( ADJUSTL( AerosolCoeff_File ) )

    IF ( PRESENT( CloudCoeff_File ) ) &
      Default_CloudCoeff_File = TRIM( ADJUSTL( CloudCoeff_File ) )

    IF ( PRESENT( EmisCoeff_File ) ) &
      Default_EmisCoeff_File = TRIM( ADJUSTL( EmisCoeff_File ) )


    ! ---------------------
    ! Was a path specified?
    ! ---------------------

    IF ( PRESENT( File_Path ) ) THEN
      Default_SpcCoeff_File     = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_SpcCoeff_File )
      Default_TauCoeff_File     = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_TauCoeff_File )
      Default_AerosolCoeff_File = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_AerosolCoeff_File )
      Default_CloudCoeff_File = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_CloudCoeff_File )
      Default_EmisCoeff_File    = TRIM( ADJUSTL( File_Path ) ) // TRIM( Default_EmisCoeff_File )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- LOAD THE SPECTRAL COEFFICIENTS --                #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_SpcCoeff( TRIM( Default_SpcCoeff_File ), &
                                       Quiet             = Quiet, &
                                       Process_ID        = Process_ID, &
                                       Output_Process_ID = Output_Process_ID, &
                                       Message_Log       = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading SpcCoeff data from '//&
                            TRIM( Default_SpcCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- LOAD THE GAS ABSORPTION COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_TauCoeff( TRIM( Default_TauCoeff_File ), &
                                       Quiet             = Quiet, &
                                       Process_ID        = Process_ID, &
                                       Output_Process_ID = Output_Process_ID, &
                                       Message_Log       = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading TauCoeff data from '//&
                            TRIM( Default_TauCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#        -- TEST THE CONGRUENCY OF THE SpcCoeff and TauCoeff DATA --       #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! The channel dimension must be the same
    ! --------------------------------------

    IF ( SC%n_Channels /= TC%n_Channels ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff and TauCoeff data have different channel dimensions.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------------------
    ! The sensor IDs and channel numbers must be the same
    ! ---------------------------------------------------

    IF ( ANY( (SC%NCEP_Sensor_ID   - TC%NCEP_Sensor_ID)   /= 0 ) .OR. &
         ANY( (SC%WMO_Satellite_ID - TC%WMO_Satellite_ID) /= 0 ) .OR. &
         ANY( (SC%WMO_Sensor_ID    - TC%WMO_Sensor_ID)    /= 0 ) .OR. &
         ANY( (SC%Sensor_Channel   - TC%Sensor_Channel)   /= 0 )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff and TauCoeff data Sensor ID/channel mismatch.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- LOAD THE AEROSOL COEFFICIENTS --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_AerosolCoeff( TRIM( Default_AerosolCoeff_File ), &
                                           Quiet             = Quiet, &
                                           Process_ID        = Process_ID, &
                                           Output_Process_ID = Output_Process_ID, &
                                           Message_Log       = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading AerosolCoeff data from '//&
                            TRIM( Default_AerosolCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- LOAD THE SCATTERING COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_CloudCoeff( TRIM( Default_CloudCoeff_File ), &
                                           Quiet             = Quiet, &
                                           Process_ID        = Process_ID, &
                                           Output_Process_ID = Output_Process_ID, &
                                           Message_Log       = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading CloudCoeff data from '//&
                            TRIM( Default_CloudCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#                -- LOAD THE IRSSE EMISSIVITY COEFFICIENTS --              #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Load_EmisCoeff( TRIM( Default_EmisCoeff_File ), &
                                        Quiet             = Quiet, &
                                        Process_ID        = Process_ID, &
                                        Output_Process_ID = Output_Process_ID, &
                                        Message_Log       = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error loading EmisCoeff data from '//&
                            TRIM( Default_EmisCoeff_File ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- LOAD THE ChannelInfo STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Using the Sensor_Descriptor string
    ! ----------------------------------

    IF ( PRESENT( Sensor_Descriptor ) .AND. &
         PRESENT( Sensor_Channel    )       ) THEN


      ! ---------------------------------
      ! Get the requested channel indices
      ! ---------------------------------

      Error_Status = CRTM_Index_ChannelInfo( SC%Sensor_Descriptor, &
                                             SC%Sensor_Channel, &
                                             Sensor_Descriptor, &
                                             Sensor_Channel, &
                                             ChannelInfo, &
                                             Message_Log = Message_Log )

      IF ( Error_Status  /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error indexing ChannelInfo', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



    ! ------------------------------
    ! Using the NCEP_Sensor_ID array
    ! ------------------------------

    ELSE IF ( PRESENT( NCEP_Sensor_ID ) .AND. &
              PRESENT( Sensor_Channel )       ) THEN


      ! ---------------------------------
      ! Get the requested channel indices
      ! ---------------------------------

      Error_Status = CRTM_Index_ChannelInfo( SC%NCEP_Sensor_ID, &
                                             SC%Sensor_Channel, &
                                             NCEP_Sensor_ID, &
                                             Sensor_Channel, &
                                             ChannelInfo, &
                                             Message_Log = Message_Log )

      IF ( Error_Status  /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error indexing ChannelInfo', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF



    ! -------------------------------------
    ! Default action is to use ALL channels
    ! -------------------------------------

    ELSE


      ! ----------------------------------
      ! Allocate the ChannelInfo structure
      ! ----------------------------------

      Error_Status = CRTM_Allocate_ChannelInfo( SC%n_Channels, &
                                                ChannelInfo, &
                                                Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Allocation of ChannelInfo(2) structure failed.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------------------
      ! Fill the Channel_Index component
      ! --------------------------------

      ChannelInfo%Channel_Index = (/ ( l, l = 1, SC%n_Channels ) /)

    END IF



    ! ------------------------------------------
    ! Fill the rest of the ChannelInfo structure
    ! ------------------------------------------

    CALL CRTM_Fill_ChannelInfo( ChannelInfo )

  END FUNCTION CRTM_Init





!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy
!
! PURPOSE:
!       Function to deallocate all the shared data arrays allocated and
!       populated during the CRTM initialization.
!
! CATEGORY:
!       CRTM
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy( ChannelInfo,              &  ! Output
!                                    Process_ID  = Process_ID, &  ! Optional input
!                                    RCS_Id      = RCS_Id,     &  ! Revision control
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       Process_ID:   Set this argument to the MPI process ID that this
!                     function call is running under. This value is used
!                     solely for controlling message output. If MPI is not
!                     being used, ignore this argument.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       ChannelInfo:  Reinitialized ChannelInfo structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_ChannelInfo_type
!                     DIMENSION:  Scalar
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
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     ERROR_HANDLER module.
!                     If == SUCCESS the CRTM deallocations were successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
!       CRTM_Destroy_SpcCoeff:       Function to deallocate the public data
!                                    structure SC containing the CRTM SpcCoeff
!                                    spectral coefficient data.
!                                    SOURCE: CRTM_SPCCOEFF module
!
!       CRTM_Destroy_TauCoeff:       Function to deallocate the public data
!                                    structure TC containing the CRTM TauCoeff
!                                    gas absorption coefficient data.
!                                    SOURCE: CRTM_TAUCOEFF module
!
!       CRTM_Destroy_AerosolCoeff:   Function to deallocate the public data
!                                    structure AeroC containing the CRTM
!                                    AerosolCoeff aerosol absorption and
!                                    scattering  coefficient data.
!                                    SOURCE: CRTM_AEROSOLCOEFF module
!
!       CRTM_Destroy_CloudCoeff:   Function to deallocate the public data
!                                    structure ScatC containing the CRTM
!                                    CloudCoeff scattering coefficient data.
!                                    SOURCE: CRTM_CloudCoeff module
!
!       CRTM_Destroy_EmisCoeff:      Function to deallocate the public data
!                                    structure EmisC containing the CRTM
!                                    IRSSE emissivity coefficient data.
!                                    SOURCE: CRTM_EMISCOEFF module
!
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       All CRTM shared data arrays and structures are deallocated.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ChannelInfo argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy( ChannelInfo,  &  ! Output
                         Process_ID,   &  ! Optional input
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- In/Output
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN OUT ) :: ChannelInfo

    ! -- Optional input
    INTEGER,        OPTIONAL,      INTENT( IN )     :: Process_ID

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,      INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,      INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy'



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- DESTROY THE ChannelInfo STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_ChannelInfo( ChannelInfo, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying ChannelInfo structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- DEALLOCATE THE IRSSE EMISSIVITY COEFFICIENT STRUCTURE --       #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_EmisCoeff( Process_ID  = Process_ID, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared EmisCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- DEALLOCATE THE SCATTERING COEFFICIENT STRUCTURE --          #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_CloudCoeff( Process_ID  = Process_ID, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared CloudCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- DEALLOCATE THE AEROSOL COEFFICIENT STRUCTURE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_AerosolCoeff( Process_ID  = Process_ID, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared AerosolCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- DEALLOCATE THE GAS ABSORPTION COEFFICIENT STRUCTURE --         #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_TauCoeff( Process_ID  = Process_ID, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared TauCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DEALLOCATE THE SPECTRAL COEFFICIENT STRUCTURE --           #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_Destroy_SpcCoeff( Process_ID  = Process_ID, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating shared SpcCoeff data structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION CRTM_Destroy

END MODULE CRTM_LifeCycle


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_LifeCycle.f90,v 1.7.2.3 2005/08/23 13:06:03 yhan Exp $
!
! $Date: 2005/08/23 13:06:03 $
!
! $Revision: 1.7.2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_LifeCycle.f90,v $
! Revision 1.7.2.3  2005/08/23 13:06:03  yhan
! -- The code segment to fill rest of the ChannelInfo structure in CRTM_Init() is replaced
!    with a call to CRTM_Fill_ChannelInfo, defined in CRTM_ChannelInfo.
!
! Revision 1.7.2.2  2005/08/19 20:32:29  qliu
! -- change "ScatterCoeff" to "CloudCoeff".
!
! Revision 1.7.2.1  2005/08/11 20:47:18  paulv
! - Added IRSSE EmisCoeff datafile/structure load and destruction capability.
!
! Revision 1.7  2005/02/16 22:47:45  paulv
! - Updated header documentation.
! - Corrected bug in the CRTM_Init() call. The ChannelInfo argument had the
!   INTENT( OUT ) rather than INTENT( IN OUT ) attribute.
! - Corrected bug in AerosolCoeff load function call. The wrong filename
!   argument was being used.
!
! Revision 1.6  2005/02/16 15:27:59  paulv
! - Added AerosolCoeff shared structure initialization.
!
! Revision 1.5  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.4  2004/06/24 19:03:14  paulv
! - Added code to initialize and destroy the CloudCoeff structure.
! - Added code to check the congruency of the SpcCoeff and TauCoeff data
!   structures. This will probably change in the future but currently
!   they must agree.
!
! Revision 1.3  2004/06/15 21:59:51  paulv
! - Added optional NCEP_Sensor_ID argument to Init() function.
! - Corrected misnaming of ChannelInfo indexing function calls.
!
! Revision 1.2  2004/06/15 21:44:31  paulv
! - Added calls to ChannelInfo indexing functions in the Init() function.
! - Deallocation of ChannelInfo added to Destroy() function.
!
! Revision 1.1  2004/05/21 20:36:55  paulv
! Initial checkin.
!
!
!
