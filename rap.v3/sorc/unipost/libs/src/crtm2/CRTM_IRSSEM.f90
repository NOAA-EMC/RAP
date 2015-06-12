!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_IRSSEM
!
! PURPOSE:
!       Module containing function to invoke the CRTM Spectral Infrared
!       Sea Surface Emissivity Model (IRSSEM).
!
! CATEGORY:
!       CRTM : SfcOptics : IRSSEM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_IRSSEM
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:             Module containing data type kind definitions.
!
!       Error_Handler:          Module to define error codes and handle
!                               error conditions.
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               and relational comparisons on floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
!       CRTM_EmisCoeff:         Module containing the Infrared Sea Surface
!                               Emissivity Model (IRSSEM) emissivity coefficients
!                               and their load/destruction routines. 
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     EMISCOEFF_DEFINE module
!                                     EMISCOEFF_BINARY_IO module
!
!
! CONTAINS:
!       CRTM_Compute_IRSSEM:      Function to compute the CRTM infrared sea
!                                 surface emissivity (IRSSE) for input wind
!                                 speed, frequency, and angles.
!
!       CRTM_Compute_IRSSEM_TL:   Function to compute the tangent-linear CRTM
!                                 infrared sea surface emissivity (IRSSE) for
!                                 input wind speed, frequency, and angles.
!
!       CRTM_Compute_IRSSEM_AD:   Function to compute the adjoint of the CRTM
!                                 infrared sea surface emissivity (IRSSE) for
!                                 input wind speed, frequency, and angles.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jun-2005
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
!--------------------------------------------------------------------------------

MODULE CRTM_IRSSEM


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler
  USE Compare_Float_Numbers

  ! -- CRTM IRSSEM coefficient module
  USE CRTM_EmisCoeff


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE

  ! -- Inherited CRTM_EmisCoeff module routines
  PUBLIC :: CRTM_Load_EmisCoeff
  PUBLIC :: CRTM_Destroy_EmisCoeff

  ! -- Science routines in this modules
  PUBLIC :: CRTM_Compute_IRSSEM
  PUBLIC :: CRTM_Compute_IRSSEM_TL
  PUBLIC :: CRTM_Compute_IRSSEM_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_IRSSEM.f90,v 1.4 2005/08/02 20:51:35 paulv Exp $'

  ! -- Keyword set values
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Literal values
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE  = 1.0_fp_kind


CONTAINS



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Bisection_Search
!
! PURPOSE:
!       Function to search an array using the bisection method. This function
!       is an adaptation from Numerical Recipes and is most efficient across
!       multiple calls when the value to be searched for in the array occurs
!       randomly.
!
! CATEGORY:
!       Interpolation
!
! CALLING SEQUENCE:
!       Index = Bisection_Search( x, u,            &  ! Input
!                                 xLower = xLower, &  ! Optional input
!                                 xUpper = xUpper  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x:         The array to be searched.
!                  UNITS:      N/A
!                  TYPE:       REAL( fp_kind )
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT( IN )
!
!       u:         The value to be searched for in the array.
!                  UNITS:      N/A
!                  TYPE:       Same as input array, x
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       xLower:    Set this optional argument to the INDEX of the input
!                  array corresponding to the LOWER search boundary.
!                  If not specified, the default value is 1.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       xUpper:    Set this optional argument to the INDEX of the input
!                  array corresponding to the UPPER search boundary.
!                  If not specified, the default value is SIZE(x).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Index:     The lower index of the two values in the input array, x,
!                  that bracket the input value, u, i.e.
!                    x(Index) < u < x(Index+1)
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
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
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Bisection_Search( x, u,     &  ! Input
                             xLower,   &  ! Optional input
                             xUpper  ) &  ! Optional input
                           RESULT( j )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN ) :: x
    REAL( fp_kind ),                 INTENT( IN ) :: u

    ! -- Optional inputs
    INTEGER,         OPTIONAL,       INTENT( IN ) :: xLower
    INTEGER,         OPTIONAL,       INTENT( IN ) :: xUpper


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: j


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n
    INTEGER :: jLower
    INTEGER :: jMiddle
    INTEGER :: jUpper



    !#--------------------------------------------------------------------------#
    !#                -- INITIALIZE THE SIZE AND EXTENT VARIABLES --            #
    !#--------------------------------------------------------------------------#

    n = SIZE( x )


    ! ------------------------------------
    ! Initialise upper and lower limits to
    ! the valid maximums, 1 and n
    ! ------------------------------------

    IF ( PRESENT( xLower ) ) THEN
      jLower = xLower
    ELSE
      jLower = 1
    END IF

    IF ( PRESENT( xUpper ) ) THEN
      jUpper = xUpper
    ELSE
      jUpper = n
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- SEARCH FOR THE REQUIRED INDEX BY BISECTION --            #
    !#--------------------------------------------------------------------------#

    Search_Loop: DO


      ! ----------------------------------------------
      ! If the index ranges have converged, we're done
      ! ----------------------------------------------

      IF ( ( jUpper - jLower ) <= 1 ) EXIT Search_Loop


      ! ---------------------
      ! Define a middle point
      ! ---------------------

      jMiddle = ( jLower + jUpper ) / 2


      ! ----------------------------------------------
      ! Which half is the required value in?
      !
      ! The following produces the result 
      !   x(i) <= x < x(i+1)
      ! when x == x(i).
      ! 
      ! To get the equivalent of
      !   x(i) < x <= x(i+1)
      ! for x == x(i+1), change the logical expression
      !   u < x( jMiddle )
      ! to
      !   u > x( jMiddle )
      ! ----------------------------------------------

      IF ( ( x(n) > x(1) ) .EQV. ( u < x( jMiddle ) ) ) THEN

        ! -- The "lower" half
        jUpper = jMiddle

      ELSE

        ! -- The "upper" half
        jLower = jMiddle

      END IF

    END DO Search_Loop


    ! -----------------------
    ! Define the return value
    ! -----------------------

    j = jLower

  END FUNCTION Bisection_Search





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
!       CRTM_Compute_IRSSEM
!
! PURPOSE:
!       Function to compute the CRTM infrared sea surface emissivity (IRSSE)
!       for input wind speed, frequency, and angles.
!
! CATEGORY:
!       CRTM : SfcOptics : IRSSEM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM( Wind_Speed,                 &  ! Input
!                                           Frequency,                  &  ! Input 
!                                           Angle,                      &  ! Input 
!                                           Emissivity,                 &  ! Output
!                                           Message_Log = Message_Log ) &  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Wind_Speed:     Wind speed, V, at sea surface. Valid range
!                       is 0 to 15 m/s.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Frequency:      Frequency, F, at which the emissivity is required.
!                       Valid range is 600 to 3000cm^-1
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Angle:          Angles, Z, at which the emissivity is to be calculated.
!                       Valid range is -65 to 65 degrees, although the value
!                       used in the calculations is the absolute value, |Z|.
!                       UNITS:      Degrees
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Emissivity:     sea surface emissivities for the 
!                       requested wind speed, frequency, and angles.
!                       UNITS:      None
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Same as input ANGLE argument.
!                       ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the emissivity computation was successful.
!                        == WARNING the inputs were outside their valid ranges.
!                                   In these cases the computation continues
!                                   but using the relevant wind speed, frequency,
!                                   or angle boundary values.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:  Subroutine to output messages
!                         SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       No input validation is performed in this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM( Wind_Speed,   &  ! Input
                                Frequency,    &  ! Input
                                Angle,        &  ! Input
                                Emissivity,   &  ! Output
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ),                 INTENT( IN )  :: Wind_Speed
    REAL( fp_kind ),                 INTENT( IN )  :: Frequency
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Angle

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Emissivity

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM'

  
    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: n_Angles, i

    REAL( fp_kind ), SAVE :: Old_Wind_Speed = -99.0_fp_kind ! Old wind speed value

    INTEGER,         SAVE :: iv = 1     ! Wind speed index into EmisC%Wind_Speed
    REAL( fp_kind ), SAVE :: v  = ZERO  ! Wind speed interpolation factor

    INTEGER               :: iu         ! Frequency index into EmisC%Frequency
    REAL( fp_kind )       :: u          ! Frequency interpolation factor

    INTEGER               :: it         ! Angle index into EmisC%Angle
    REAL( fp_kind )       :: t          ! Angle interpolation factor



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK THE INPUTS --                        #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    n_Angles = SIZE( Angle )

    IF ( SIZE( Emissivity ) /= n_Angles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Angle and output Emissivity array dimensions inconsistent.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !# -- COMPUTE THE WIND SPEED INDEX AND INTERPOLATION FACTOR IF NECESSARY -- #
    !#--------------------------------------------------------------------------#

    Update_Wind_Speed: IF ( .NOT. Compare_Float( Wind_Speed, Old_Wind_Speed ) ) THEN


      ! -------------------------
      ! Update the old wind speed
      ! -------------------------
 
      Old_Wind_Speed = Wind_Speed


      ! -----------------------------------------
      ! Compute the index of the input wind speed
      ! into the EmisC%Wind_Speed array. Result
      ! must be from 1 to n_Wind_Speeds-1
      ! -----------------------------------------

      iv = Bisection_Search( EmisC%Wind_Speed, Wind_Speed )


      ! --------------------------------
      ! Compute the linear interpolation
      ! factor for the input wind speed
      ! --------------------------------

      v = (      Wind_Speed        - EmisC%Wind_Speed(iv) ) / &
      !   -------------------------------------------------
          ( EmisC%Wind_Speed(iv+1) - EmisC%Wind_Speed(iv) )

    END IF Update_Wind_Speed



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FREQUENCY INDEX AND INTERPOLATION FACTOR --       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! Compute the index of the input frequency
    ! into the EmisC%Frequency array. Result
    ! must be from 1 to n_Frequencies-1
    ! -----------------------------------------

    iu = Bisection_Search( EmisC%Frequency, Frequency )


    ! --------------------------------
    ! Compute the linear interpolation
    ! factor for the input frequency
    ! --------------------------------

    u = (      Frequency        - EmisC%Frequency(iu) ) / &
    !   -----------------------------------------------
        ( EmisC%Frequency(iu+1) - EmisC%Frequency(iu) )



    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN LOOP OVER ANGLES --                     #
    !#--------------------------------------------------------------------------#

    DO i = 1, n_Angles


      ! --------------------------------------------
      ! Compute the index of the current input angle
      ! into the EmisC%Angle array. Result must be
      ! from 1 to n_Angles-1
      ! --------------------------------------------

      it = Bisection_Search( EmisC%Angle, ABS(Angle(i)) )


      ! ----------------------------------
      ! Compute the linear interpolation
      ! factor for the current input angle
      ! ----------------------------------

      t = (   ABS(Angle(i))   - EmisC%Angle(it) ) / &
      !   ---------------------------------------
          ( EmisC%Angle(it+1) - EmisC%Angle(it) )


      ! ------------------------------------
      ! Perform the tri-linear interpolation
      ! ------------------------------------

      Emissivity(i) = ( (ONE-t)*(ONE-u)*(ONE-v)*EmisC%Emissivity(it,  iu,  iv  ) ) + &
                      (    t   *(ONE-u)*(ONE-v)*EmisC%Emissivity(it+1,iu,  iv  ) ) + &
                      (    t   *   u   *(ONE-v)*EmisC%Emissivity(it+1,iu+1,iv  ) ) + &
                      ( (ONE-t)*   u   *(ONE-v)*EmisC%Emissivity(it,  iu+1,iv  ) ) + &
                      ( (ONE-t)*(ONE-u)*   v   *EmisC%Emissivity(it,  iu,  iv+1) ) + &
                      (    t   *(ONE-u)*   v   *EmisC%Emissivity(it+1,iu,  iv+1) ) + &
                      (    t   *   u   *   v   *EmisC%Emissivity(it+1,iu+1,iv+1) ) + &
                      ( (ONE-t)*   u   *   v   *EmisC%Emissivity(it,  iu+1,iv+1) )


    END DO

  END FUNCTION CRTM_Compute_IRSSEM





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_IRSSEM_TL
!
! PURPOSE:
!       Function to compute the tangent-linear CRTM infrared sea surface
!       emissivity (IRSSE) for input wind speed, frequency, and angles.
!
! CATEGORY:
!       CRTM : SfcOptics : IRSSEM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM_TL( Wind_Speed,                 &  ! Input
!                                              Frequency,                  &  ! Input 
!                                              Angle,                      &  ! Input 
!                                              Wind_Speed_TL,              &  ! Input
!                                              Emissivity_TL,              &  ! Output
!                                              Message_Log = Message_Log ) &  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Wind_Speed:     Wind speed, V, at sea surface. Valid range
!                       is 0 to 15 m/s.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Frequency:      Frequency, F, at which the emissivity is required.
!                       Valid range is 600 to 3000cm^-1
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Angle:          Angles, Z, at which the emissivity is to be calculated.
!                       Valid range is -65 to 65 degrees, although the value
!                       used in the calculations is the absolute value, |Z|.
!                       UNITS:      Degrees
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT( IN )
!
!       Wind_Speed_TL:  The tangent-linear wind speed, dV, at sea surface.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Emissivity_TL:  Tangent-linear sea surface emissivities for the 
!                       requested wind speed, frequency, and angles, due 
!                       to the input tangent-linear wind speed.
!                       UNITS:      None
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Same as input ANGLE argument
!                       ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the emissivity computation was successful.
!                        == WARNING the inputs were outside their valid ranges.
!                                   In these cases the computation continues
!                                   but using the relevant wind speed, frequency,
!                                   or angle boundary values.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:  Subroutine to output messages
!                         SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       No input validation is performed in this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM_TL( Wind_Speed,    &  ! Input
                                   Frequency,     &  ! Input
                                   Angle,         &  ! Input
                                   Wind_Speed_TL, &  ! Input
                                   Emissivity_TL, &  ! Output
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ),                 INTENT( IN )  :: Wind_Speed
    REAL( fp_kind ),                 INTENT( IN )  :: Frequency
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Angle
    REAL( fp_kind ),                 INTENT( IN )  :: Wind_Speed_TL

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Emissivity_TL

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM_TL'

  
    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: n_Angles, i

    REAL( fp_kind ), SAVE :: Old_Wind_Speed = -99.0_fp_kind ! Old wind speed value

    INTEGER,         SAVE :: iv   = 1     ! Wind speed index into EmisC%Wind_Speed
    REAL( fp_kind )       :: v_TL         ! Tangent-linear wind speed interpolation factor

    INTEGER               :: iu           ! Frequency index into EmisC%Frequency
    REAL( fp_kind )       :: u            ! Frequency interpolation factor

    INTEGER               :: it           ! Angle index into EmisC%Angle
    REAL( fp_kind )       :: t            ! Angle interpolation factor



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK THE INPUTS --                        #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    n_Angles = SIZE( Angle )

    IF ( SIZE( Emissivity_TL ) /= n_Angles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Angle and output Emissivity_TL array dimensions inconsistent.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- COMPUTE THE WIND SPEED INDEX --                   #
    !#--------------------------------------------------------------------------#

    Update_Wind_Speed: IF ( .NOT. Compare_Float( Wind_Speed, Old_Wind_Speed ) ) THEN


      ! -------------------------
      ! Update the old wind speed
      ! -------------------------
 
      Old_Wind_Speed = Wind_Speed


      ! -----------------------------------------
      ! Compute the index of the input wind speed
      ! into the EmisC%Wind_Speed array. Result
      ! must be from 1 to n_Wind_Speeds-1
      ! -----------------------------------------

      iv = Bisection_Search( EmisC%Wind_Speed, ABS( Wind_Speed ) )

    END IF Update_Wind_Speed



    !#--------------------------------------------------------------------------#
    !#     -- COMPUTE THE WIND SPEED TANGENT-LINEAR INTERPOLATION FACTOR --     #
    !#--------------------------------------------------------------------------#

    v_TL =                  Wind_Speed_TL                    / &
    !      -------------------------------------------------
           ( EmisC%Wind_Speed(iv+1) - EmisC%Wind_Speed(iv) )



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FREQUENCY INDEX AND INTERPOLATION FACTOR --       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! Compute the index of the input frequency
    ! into the EmisC%Frequency array. Result
    ! must be from 1 to n_Frequencies-1
    ! -----------------------------------------

    iu = Bisection_Search( EmisC%Frequency, Frequency )


    ! --------------------------------
    ! Compute the linear interpolation
    ! factor for the input frequency
    ! --------------------------------

    u = (      Frequency        - EmisC%Frequency(iu) ) / &
    !   -----------------------------------------------
        ( EmisC%Frequency(iu+1) - EmisC%Frequency(iu) )



    !#--------------------------------------------------------------------------#
    !#                         -- BEGIN LOOP OVER ANGLES --                     #
    !#--------------------------------------------------------------------------#

    DO i = 1, n_Angles


      ! --------------------------------------------
      ! Compute the index of the current input angle
      ! into the EmisC%Angle array. Result must be
      ! from 1 to n_Angles-1
      ! --------------------------------------------

      it = Bisection_Search( EmisC%Angle, ABS(Angle(i)) )


      ! ----------------------------------
      ! Compute the linear interpolation
      ! factor for the current input angle
      ! ----------------------------------

      t = (   ABS(Angle(i))   - EmisC%Angle(it) ) / &
      !   ---------------------------------------
          ( EmisC%Angle(it+1) - EmisC%Angle(it) )


      ! ------------------------------------
      ! Perform the tri-linear interpolation
      ! ------------------------------------

      Emissivity_TL(i) = ( (ONE-t)*(ONE-u)*(-v_TL)*EmisC%Emissivity(it,  iu,  iv  ) ) + &
                         (    t   *(ONE-u)*(-v_TL)*EmisC%Emissivity(it+1,iu,  iv  ) ) + &
                         (    t   *   u   *(-v_TL)*EmisC%Emissivity(it+1,iu+1,iv  ) ) + &
                         ( (ONE-t)*   u   *(-v_TL)*EmisC%Emissivity(it,  iu+1,iv  ) ) + &
                         ( (ONE-t)*(ONE-u)*  v_TL *EmisC%Emissivity(it,  iu,  iv+1) ) + &
                         (    t   *(ONE-u)*  v_TL *EmisC%Emissivity(it+1,iu,  iv+1) ) + &
                         (    t   *   u   *  v_TL *EmisC%Emissivity(it+1,iu+1,iv+1) ) + &
                         ( (ONE-t)*   u   *  v_TL *EmisC%Emissivity(it,  iu+1,iv+1) )

    END DO

  END FUNCTION CRTM_Compute_IRSSEM_TL






!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_IRSSEM_AD
!
! PURPOSE:
!       Function to compute the adjoint of the CRTM infrared sea surface
!       emissivity (IRSSE) for input wind speed, frequency, and angles.
!
! CATEGORY:
!       CRTM : SfcOptics : IRSSEM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM_AD( Wind_Speed,                 &  ! Input
!                                              Frequency,                  &  ! Input 
!                                              Angle,                      &  ! Input 
!                                              Emissivity_AD,              &  ! Input
!                                              Wind_Speed_AD,              &  ! Output
!                                              Message_Log = Message_Log ) &  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Wind_Speed:     Wind speed, V, at sea surface. Valid range
!                       is 0 to 15 m/s.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Frequency:      Frequency, F, at which the emissivity adjoint
!                       is required.
!                       Valid range is 600 to 3000cm^-1
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Angle:          Angles, Z, over which the emissivity adjoint is
!                       calculated and summed. See the output Wind_Speed_AD
!                       argument description for more information about the
!                       angle summation.
!                       Valid range is -65 to 65 degrees, although the value
!                       used in the calculations is the absolute value, |Z|.
!                       UNITS:      Degrees
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT( IN )
!
!       Emissivity_AD:  Adjoint sea surface emissivities for the 
!                       requested wind speed, frequency, and angles.
!                       UNITS:      None
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Same as input ANGLE argument
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Wind_Speed_AD:  Adjoint wind speed, de/dV, at sea surface, SUMMED
!                       OVER THE INPUT ANGLES. The sum over angle is done because
!                       this model was designed for use with a multi-stream radiative
!                       transfer scheme. As such, the sum over angle produces
!                       a result, e.g. dRadiance/dV for a single frequency.
!                       For emissivity-only calculations for a range of angles,
!                       the adjoint function must be called separately for each
!                       angle. Alternatively, you can use the tangent-linear
!                       function to compute derivatives for multiple angles.
!                       UNITS:      per metres per second, (m.s^-1)^-1
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the emissivity computation was successful.
!                        == WARNING the inputs were outside their valid ranges.
!                                   In these cases the computation continues
!                                   but using the relevant wind speed, frequency,
!                                   or angle boundary values.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:  Subroutine to output messages
!                         SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       No input validation is performed in this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-May-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM_AD( Wind_Speed,    &  ! Input
                                   Frequency,     &  ! Input
                                   Angle,         &  ! Input
                                   Emissivity_AD, &  ! Input
                                   Wind_Speed_AD, &  ! Output
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ),                 INTENT( IN )     :: Wind_Speed
    REAL( fp_kind ),                 INTENT( IN )     :: Frequency
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )     :: Angle
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Emissivity_AD

    ! -- Outputs
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Wind_Speed_AD

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL,        INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM_AD'

  
    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: n_Angles, i

    REAL( fp_kind ), SAVE :: Old_Wind_Speed = -99.0_fp_kind ! Old wind speed value

    INTEGER,         SAVE :: iv   = 1     ! Wind speed index into EmisC%Wind_Speed
    REAL( fp_kind )       :: v_AD         ! Adjoint wind speed interpolation factor

    INTEGER               :: iu           ! Frequency index into EmisC%Frequency
    REAL( fp_kind )       :: u            ! Frequency interpolation factor

    INTEGER               :: it           ! Angle index into EmisC%Angle
    REAL( fp_kind )       :: t            ! Angle interpolation factor



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK THE INPUTS --                        #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    n_Angles = SIZE( Angle )

    IF ( SIZE( Emissivity_AD ) /= n_Angles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Angle and Emissivity_AD array dimensions inconsistent.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- COMPUTE THE WIND SPEED INDEX --                   #
    !#--------------------------------------------------------------------------#

    Update_Wind_Speed: IF ( .NOT. Compare_Float( Wind_Speed, Old_Wind_Speed ) ) THEN


      ! -------------------------
      ! Update the old wind speed
      ! -------------------------
 
      Old_Wind_Speed = Wind_Speed


      ! -----------------------------------------
      ! Compute the index of the input wind speed
      ! into the EmisC%Wind_Speed array. Result
      ! must be from 1 to n_Wind_Speeds-1
      ! -----------------------------------------

      iv = Bisection_Search( EmisC%Wind_Speed, ABS( Wind_Speed ) )

    END IF Update_Wind_Speed



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FREQUENCY INDEX AND INTERPOLATION FACTOR --       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! Compute the index of the input frequency
    ! into the EmisC%Frequency array. Result
    ! must be from 1 to n_Frequencies-1
    ! -----------------------------------------

    iu = Bisection_Search( EmisC%Frequency, Frequency )


    ! --------------------------------
    ! Compute the linear interpolation
    ! factor for the input frequency
    ! --------------------------------

    u = (      Frequency        - EmisC%Frequency(iu) ) / &
    !   -----------------------------------------------
        ( EmisC%Frequency(iu+1) - EmisC%Frequency(iu) )



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE WIND SPEED INTERPOLATION FACTOR ADJOINT --        #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Initialise the local adjoint variable
    ! -------------------------------------

    v_AD = ZERO


    ! -----------------------------
    ! Sum local adjoint over angles
    ! -----------------------------

    DO i = 1, n_Angles


      ! --------------------------------------------
      ! Compute the index of the current input angle
      ! into the EmisC%Angle array. Result must be
      ! from 1 to n_Angles-1
      ! --------------------------------------------

      it = Bisection_Search( EmisC%Angle, ABS(Angle(i)) )


      ! ----------------------------------
      ! Compute the linear interpolation
      ! factor for the current input angle
      ! ----------------------------------

      t = (   ABS(Angle(i))   - EmisC%Angle(it) ) / &
      !   ---------------------------------------
          ( EmisC%Angle(it+1) - EmisC%Angle(it) )


      ! ------------------------------------
      ! Compute the local wind speed adjoint
      ! ------------------------------------

      v_AD = v_AD + ( Emissivity_AD(i) * &
                      ( ( (-ONE)*(ONE-t)*(ONE-u)*EmisC%Emissivity(it,  iu,  iv  ) ) + &
                        ( (-ONE)*   t   *(ONE-u)*EmisC%Emissivity(it+1,iu,  iv  ) ) + &
                        ( (-ONE)*   t   *   u   *EmisC%Emissivity(it+1,iu+1,iv  ) ) + &
                        ( (-ONE)*(ONE-t)*   u   *EmisC%Emissivity(it,  iu+1,iv  ) ) + &
                        (        (ONE-t)*(ONE-u)*EmisC%Emissivity(it,  iu,  iv+1) ) + &
                        (           t   *(ONE-u)*EmisC%Emissivity(it+1,iu,  iv+1) ) + &
                        (           t   *   u   *EmisC%Emissivity(it+1,iu+1,iv+1) ) + &
                        (        (ONE-t)*   u   *EmisC%Emissivity(it,  iu+1,iv+1) )   ) )

      Emissivity_AD(i) = ZERO

    END DO



    !#--------------------------------------------------------------------------#
    !#                     -- COMPUTE THE WIND SPEED ADJOINT --                 #
    !#--------------------------------------------------------------------------#

    Wind_Speed_AD = Wind_Speed_AD + (                        v_AD                       / &
    !                                 -------------------------------------------------
                                      ( EmisC%Wind_Speed(iv+1) - EmisC%Wind_Speed(iv) ) )

  END FUNCTION CRTM_Compute_IRSSEM_AD

END MODULE CRTM_IRSSEM


!--------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!--------------------------------------------------------------------------------
!
! $Id: CRTM_IRSSEM.f90,v 1.4 2005/08/02 20:51:35 paulv Exp $
!
! $Date: 2005/08/02 20:51:35 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_IRSSEM.f90,v $
! Revision 1.4  2005/08/02 20:51:35  paulv
! - Replaced the linear search for the look up table indices using the
!   MIN(MAX(MAXLOC())) method with a bisection search. The latter is more
!   efficient than a linear search and is more portable since the result
!   of MAXLOC when all elements of the MASK argument are false is
!   processor/compiler dependent.
!
! Revision 1.3  2005/07/27 18:49:51  paulv
! - Added tangent-linear and adjoint functions.
!
! Revision 1.2  2005/07/21 16:39:56  paulv
! - Changed indexing algorithm from
!     idx = MAX( MIN( MINLOC( EmisC%X - X, &
!                             MASK = ( EmisC%X - X ) >= ZERO, &
!                             DIM  = 1 ), &
!                     EmisC%n_X - 1 ), &
!                1 )
!   to
!     idx = MAX( MIN( MAXLOC( EmisC%X - X, &
!                             MASK = ( EmisC%X - X ) <= ZERO, &
!                             DIM  = 1 ), &
!                     EmisC%n_X - 1 ), &
!                1 )
! - Made CRTM_EmisCoeff module Load() and Destroy() functions visible from
!   this module.
! - Updated documentation.
!
! Revision 1.1  2005/07/19 21:58:27  paulv
! Initial checkin. Forward model only. Untested.
!
!
!
