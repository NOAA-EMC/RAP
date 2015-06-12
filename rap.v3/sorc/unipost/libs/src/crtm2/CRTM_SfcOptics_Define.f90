!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_SfcOptics_Define
!
! PURPOSE:
!       Module defining the CRTM SfcOptics structure and containing
!       routines to manipulate it.
!       
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_SfcOptics_Define
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
!       CRTM_Associated_SfcOptics:  Function to test the association status
!                                   of the pointer members of a SfcOptics
!                                   structure.
!
!       CRTM_Destroy_SfcOptics:     Function to re-initialize an
!                                   CRTM_SfcOptics structure.
!
!       CRTM_Allocate_SfcOptics:    Function to allocate the pointer
!                                   members of an CRTM_SfcOptics
!                                   structure.
!
!       CRTM_Assign_SfcOptics:      Function to copy an CRTM_SfcOptics
!                                   structure.
!
!
! DERIVED TYPES:
!       CRTM_SfcOptics_type
!       --------------------
!         Definition of the CRTM SfcOptics data structure.
!         Fields are:
!
!         n_Angles:                Number of angles for which surface optical
!                                  data are required.
!                                  This is the "I" dimension
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Stokes:                Number of Stokes parameters used to
!                                  represent the propagating radiation.
!                                  This is the "Ls" dimension
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Surface_Temperature:     The surface temperature to be used in the
!                                  radiative transfer. This value is an average
!                                  of the various surface type temperatures
!                                  weighted by their coverage fraction.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Angle:                   The angle values. Typically these will be
!                                  the stream angles + the satellite view angle.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!
!         Weight:                  The Weight equals the product of the cosine 
!                                  angle and quardrature weight.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!
!         Emissivity:              The surface emissivity.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2, (I x Ls)
!
!         Reflectivity:            The surface reflectivity.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-4, (I x Ls x I x Ls)
!            *** Note ***
!
!       The physical meaning of Reflectivity(:,:,:,:) is the following:
!
!          Given a pair of polarization indexes for the incident and reflected 
!          radiances, ip and rp, assuming there is no cross contributions from 
!          incident radiances with different polarization, Reflectivity(:, rp, :, ip) 
!          is defined as a reflectivity matrix with
!             
!             Reflectivity(:, rp, :, ip) = 0,  if rp /= ip,   and 
!              
!             I(angle_r, p) = SUM( Reflectivity(angle_r, p, :, p) * I(:, p)), if rp=ip=p
!
!          where I(angle_r, p) is the reflected radiance at zenith angle with index angle_r,
!          and I(:, p) is the incident radiances and the summation is over the number of
!          incident angles.  Thus, if BRDF(angle_r, p, angle_in, p) is the
!          bidirectional reflectivity distribution function, then
!
!              Reflectivity(angle_r, p, angle_in, p) = &
!                   BRDF(angle_r, p, angle_in, p)*cos(angle_in)*w(angle_in)
!
!          where w(angle_in) is the quadrature weight.               
!
!          *** a special case ***       
!
!          For a Lambertian surface, if only one angle is given, then 
!              I_r = Reflectivity(1, rp, 1, ip) * I_diff
!          where I_r is the reflected radiance, constant at all angles, I_diff
!          is the incident radiance at the diffusivity angle.
!
!            *** End of Note ***
!
!         Direct_Reflectivity:     The surface reflectivity at a source (e.g. solar)
!                                  direction.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2, (I x Ls)
!            *** Note ***
!                  
!            If I(angle_r, p) is the reflected radiance at the zenith angle with index                    
!            angle_r and F_direct(angle_in) is the direct incident irradiance at the surface,                
!            then Direct_Reflectivity(angle_r, p) is defined as                                           
!                 I(angle_r, p) = Direct_Reflectivity(angle_r, p) * &
!                                 cos(angle_in) * F_direct(angle_in) 
!
!            *** End of note ***
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_SfcOptics_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
!       scope of this module. This makes it possible to manipulate
!       the structure and its data directly rather than, for e.g., via
!       get() and set() functions. This was done to eliminate the
!       overhead of the get/set type of structure access in using the
!       structure. *But*, it is recommended that the user destroy,
!       allocate, and assign the structure using only the routines
!       in this module where possible to eliminate -- or at least
!       minimise -- the possibility of memory leakage since most
!       of the structure members are pointers.
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

MODULE CRTM_SfcOptics_Define


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
  PUBLIC :: CRTM_Associated_SfcOptics
  PUBLIC :: CRTM_Destroy_SfcOptics
  PUBLIC :: CRTM_Allocate_SfcOptics
  PUBLIC :: CRTM_Assign_SfcOptics


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_SfcOptics_Define.f90,v 1.12 2005/10/12 17:16:22 paulv Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  
  ! ------------------------------------------
  ! Scattering parameters data type definition
  ! ------------------------------------------

  TYPE, PUBLIC :: CRTM_SfcOptics_type
    INTEGER :: n_Allocates = 0

    ! -- Dimensions
    INTEGER :: n_Angles = 0 ! I
    INTEGER :: n_Stokes = 0 ! Ls

    ! -- Flag to determine if the sfc optics require calculation.
    ! -- Default is to compute the sfc optics.
    INTEGER :: Compute_Switch = SET
    INTEGER :: Index_Sat_Ang = 1      ! Index for sensor zenith angle

    ! -- Mandatory members
    REAL( fp_kind ) :: Surface_Temperature = ZERO

    REAL( fp_kind ), DIMENSION( : ),          POINTER :: Angle               => NULL() ! I
    REAL( fp_kind ), DIMENSION( : ),          POINTER :: Weight              => NULL() ! I

    REAL( fp_kind ), DIMENSION( :, : ),       POINTER :: Emissivity          => NULL() ! I x Ls
    REAL( fp_kind ), DIMENSION( :, :, :, : ), POINTER :: Reflectivity        => NULL() ! I x Ls x I x Ls
    REAL( fp_kind ), DIMENSION( :, : ),       POINTER :: Direct_Reflectivity => NULL() ! I x Ls

  END TYPE CRTM_SfcOptics_type


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
!       CRTM_Clear_SfcOptics
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_SfcOptics structure.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_SfcOptics( SfcOptics ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       SfcOptics:   CRTM_SfcOptics structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_SfcOptics_type
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
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_SfcOptics( SfcOptics )

    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics

    SfcOptics%n_Angles = 0
    SfcOptics%n_Stokes = 0

    SfcOptics%Compute_Switch = SET

    SfcOptics%Surface_Temperature = ZERO

  END SUBROUTINE CRTM_Clear_SfcOptics





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
!       CRTM_Associated_SfcOptics
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       SfcOptics structure.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_SfcOptics( SfcOptics,          &  ! Input
!                                                       ANY_Test = Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       SfcOptics:           CRTM_SfcOptics structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_SfcOptics_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            CRTM_SfcOptics structure pointer members are associated.
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
!                            association status of the CRTM_SfcOptics pointer members.
!                            .TRUE.  - if ALL the CRTM_SfcOptics pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the CRTM_SfcOptics pointer
!                                      members are associated.
!                            .FALSE. - some or all of the CRTM_SfcOptics pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_SfcOptics( SfcOptics, & ! Input
                                      ANY_Test ) & ! Optional input
                                    RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_SfcOptics_type ), INTENT( IN ) :: SfcOptics

    ! -- Optional input
    INTEGER,           OPTIONAL, INTENT( IN ) :: ANY_Test


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

      IF ( ASSOCIATED( SfcOptics%Angle               ) .AND. &
           ASSOCIATED( SfcOptics%Weight              ) .AND. &
           ASSOCIATED( SfcOptics%Emissivity          ) .AND. &
           ASSOCIATED( SfcOptics%Reflectivity        ) .AND. &
           ASSOCIATED( SfcOptics%Direct_Reflectivity )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( SfcOptics%Angle               ) .OR. &
           ASSOCIATED( SfcOptics%Weight              ) .OR. &
           ASSOCIATED( SfcOptics%Emissivity          ) .OR. &
           ASSOCIATED( SfcOptics%Reflectivity        ) .OR. &
           ASSOCIATED( SfcOptics%Direct_Reflectivity )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION CRTM_Associated_SfcOptics





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_SfcOptics
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a CRTM_SfcOptics data structure.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_SfcOptics( SfcOptics,                &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
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
!       SfcOptics:    Re-initialized CRTM_SfcOptics structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_SfcOptics_type
!                     DIMENSION:  Scalar OR Rank-1 array
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
!       CRTM_Clear_SfcOptics:        Subroutine to clear the scalar members
!                                    of a CRTM_SfcOptics structure.
!
!       CRTM_Associated_SfcOptics:   Function to test the association status
!                                    of the pointer members of a
!                                    CRTM_SfcOptics structure.
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
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Destroy_SfcOptics( SfcOptics,    &  ! Output
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
    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics

    ! -- Optional input
    INTEGER,           OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),    OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_SfcOptics'


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

    IF ( Clear ) CALL CRTM_Clear_SfcOptics( SfcOptics )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! -- Deallocate the CRTM_SfcOptics Angle
    IF ( ASSOCIATED( SfcOptics%Angle ) ) THEN

      DEALLOCATE( SfcOptics%Angle, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Angle ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_SfcOptics Weight 
    IF ( ASSOCIATED( SfcOptics%Weight ) ) THEN

      DEALLOCATE( SfcOptics%Weight, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Weight ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_SfcOptics Emissivity
    IF ( ASSOCIATED( SfcOptics%Emissivity ) ) THEN

      DEALLOCATE( SfcOptics%Emissivity, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Emissivity ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_SfcOptics Reflectivity
    IF ( ASSOCIATED( SfcOptics%Reflectivity ) ) THEN

      DEALLOCATE( SfcOptics%Reflectivity, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Reflectivity ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! -- Deallocate the CRTM_SfcOptics Direct_Reflectivity
    IF ( ASSOCIATED( SfcOptics%Direct_Reflectivity ) ) THEN

      DEALLOCATE( SfcOptics%Direct_Reflectivity, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_SfcOptics Direct_Reflectivity ", &
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

    SfcOptics%n_Allocates = SfcOptics%n_Allocates - 1

    IF ( SfcOptics%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      SfcOptics%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Destroy_SfcOptics





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_SfcOptics
! 
! PURPOSE:
!       Function to allocate the pointer members of the CRTM_SfcOptics
!       data structure.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_SfcOptics( n_Angles,                 &  ! Input
!                                               n_Stokes,                 &  ! Input
!                                               SfcOptics,                &  ! Output
!                                               RCS_Id = RCS_Id,          &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!         n_Angles:          Number of angles for which surface optical
!                            data are represented.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         n_Stokes:          Number of Stokes parameters used to represent the
!                            propagating radiation.
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
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SfcOptics:           CRTM_SfcOptics structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       CRTM_SfcOptics_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
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
!       CRTM_Associated_SfcOptics:  Function to test the association status of the
!                                   pointer members of a CRTM_SfcOptics structure.
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
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Allocate_SfcOptics( n_Angles,     &  ! Input
                                    n_Stokes,     &  ! Input
                                    SfcOptics,    &  ! Output
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
    INTEGER,                     INTENT( IN )     :: n_Angles
    INTEGER,                     INTENT( IN )     :: n_Stokes

    ! -- Output
    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics

    ! -- Revision control
    CHARACTER( * ),    OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_SfcOptics'


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

    IF ( n_Angles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Angles must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Stokes < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Stokes must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_SfcOptics( SfcOptics, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_SfcOptics( SfcOptics, &
                                             No_Clear = SET, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_SfcOptics pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( SfcOptics%Angle( n_Angles ), SfcOptics%Weight( n_Angles ), &
              SfcOptics%Emissivity( n_Angles, n_Stokes ), &
              SfcOptics%Reflectivity( n_Angles, n_Stokes, n_Angles, n_Stokes), &
              SfcOptics%Direct_Reflectivity( n_Angles, n_Stokes ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating SfcOptics data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#            -- ASSIGN THE DIMENSIONS AND INITALISE VARIABLES --           #
    !#--------------------------------------------------------------------------#

    SfcOptics%n_Angles = n_Angles
    SfcOptics%n_Stokes = n_Stokes

    SfcOptics%Compute_Switch = SET
    SfcOptics%Surface_Temperature = ZERO

    SfcOptics%Angle               = ZERO
    SfcOptics%Weight              = ZERO
    SfcOptics%Emissivity          = ZERO
    SfcOptics%Reflectivity        = ZERO
    SfcOptics%Direct_Reflectivity = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    SfcOptics%n_Allocates = SfcOptics%n_Allocates + 1

    IF ( SfcOptics%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      SfcOptics%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Allocate_SfcOptics





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_SfcOptics
!
! PURPOSE:
!       Function to copy valid CRTM_SfcOptics structures.
!
! CATEGORY:
!       CRTM : Surface Optical Properties
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_SfcOptics( SfcOptics_in,             &  ! Input
!                                             SfcOptics_out,            &  ! Output
!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       SfcOptics_in:    CRTM_SfcOptics structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
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
!       SfcOptics_out:   Copy of the input structure, CRTM_SfcOptics_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
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
!       CRTM_Associated_SfcOptics:  Function to test the association status of the
!                                   pointer members of a CRTM_SfcOptics structure.
!
!       CRTM_Allocate_SfcOptics:    Function to allocate the pointer members of
!                                   the CRTM_SfcOptics data structure.
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
!       Note the INTENT on the output SfcOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Assign_SfcOptics( SfcOptics_in,  &  ! Input
                                  SfcOptics_out, &  ! Output
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_SfcOptics_type ), INTENT( IN )     :: SfcOptics_in

    ! -- Output
    TYPE( CRTM_SfcOptics_type ), INTENT( IN OUT ) :: SfcOptics_out

    ! -- Revision control
    CHARACTER( * ),    OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),    OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_SfcOptics'



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

    IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics_In ) ) THEN

      Error_Status = CRTM_Destroy_SfcOptics( SfcOptics_Out, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_SfcOptics pointer members.', &
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

    Error_Status = CRTM_Allocate_SfcOptics( SfcOptics_in%n_Angles, &
                                            SfcOptics_in%n_Stokes, &
                                            SfcOptics_out, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output SfcOptics arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Assign scalar data
    ! ------------------

    SfcOptics_out%Compute_Switch = SfcOptics_in%Compute_Switch
    SfcOptics_out%Surface_Temperature = SfcOptics_in%Surface_Temperature


    ! -----------------
    ! Assign array data
    ! -----------------

    SfcOptics_out%Angle               = SfcOptics_in%Angle
    SfcOptics_out%Weight              = SfcOptics_in%Weight
    SfcOptics_out%Emissivity          = SfcOptics_in%Emissivity
    SfcOptics_out%Reflectivity        = SfcOptics_in%Reflectivity
    SfcOptics_out%Direct_Reflectivity = SfcOptics_in%Direct_Reflectivity 

  END FUNCTION CRTM_Assign_SfcOptics

END MODULE CRTM_SfcOptics_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_SfcOptics_Define.f90,v 1.12 2005/10/12 17:16:22 paulv Exp $
!
! $Date: 2005/10/12 17:16:22 $
!
! $Revision: 1.12 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_SfcOptics_Define.f90,v $
! Revision 1.12  2005/10/12 17:16:22  paulv
! - Corrected bug in Compute_Switch assignment in Assign() function.
!
! Revision 1.11  2005/10/12 17:13:47  paulv
! - Completed addition of Compute_Switch member by including assignments in
!   Clear(), Allocate(), and Assign() functions.
!
! Revision 1.10  2005/10/04 19:44:49  qliu
! -- Added Compute_Switch = SET.
!
! Revision 1.9  2005/08/16 19:35:16  qliu
! - Added Weight component for eventual integration of reflected radiance
!   in the RTSolution code.
!
! Revision 1.8  2005/06/24 16:52:19  paulv
! - Updated some header documentation.
!
! Revision 1.7  2005/06/21 21:57:44  paulv
! - Added Surface_Temperature component.
! - Updated documentation.
! - Changed initialisation of pointer array components from an "invliad" value
!   to zero.
!
! Revision 1.6  2004/11/05 16:04:30  paulv
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
!     IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics_In ) ) THEN
!       Error_Status = FAILURE
!       RETURN
!     END IF
!   Now, rather than returning an error, the output structure is destroyed
!   (in case it is defined upon input), and a successful status is returned,
!     IF ( .NOT. CRTM_Associated_SfcOptics( SfcOptics_In ) ) THEN
!       Error_Status = CRTM_Destroy_SfcOptics( SfcOptics_Out, &
!                                              Message_Log = Message_Log )
!       RETURN
!     END IF
!
! Revision 1.5  2004/08/06 18:35:43  paulv
! - Added Direct_Reflectivity member
! - Changed Secant_Angle member to just Angle.
! - Updated header documentation
!
! Revision 1.4  2004/07/02 20:31:44  paulv
! - Like the AtmScatter modules, these also had CRLF at the end of each line.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/06/29 16:25:27  paulv
! - Added the n_Streams dimension to the structure.
! - Added the Secant_Angle member to the structure.
! - Increased the rank of the emissivity and reflectivity structure members
!   from 1 to 2 to handle more than one stream if required.
!
! Revision 1.1  2004/06/28 21:50:17  paulv
! Initial checkin.
!
!
!
!


