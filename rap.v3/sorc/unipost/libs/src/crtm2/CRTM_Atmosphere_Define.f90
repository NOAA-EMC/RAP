!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Atmosphere_Define
!
! PURPOSE:
!       Module defining the CRTM Atmosphere structure and containing routines to 
!       manipulate it.
!       
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Atmosphere_Define
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Error_Handler:          Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       CRTM_Cloud_Define:      Module defining the CRTM Cloud structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       CRTM_Aerosol_Define:    Module defining the CRTM Aerosol structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Associated_Atmosphere:   Function to test the association status
!                                       of the pointer members of an Atmosphere
!                                       structure.
!
!         CRTM_Destroy_Atmosphere:      Function to re-initialize a CRTM_Atmosphere
!                                       structure.
!
!         CRTM_Allocate_Atmosphere:     Function to allocate the pointer members
!                                       of a CRTM_Atmosphere structure.
!
!         CRTM_Assign_Atmosphere:       Function to copy a CRTM_Atmosphere structure.
!
!
!         CRTM_WeightedSum_Atmosphere:  Function to perform a weighted sum of two
!                                       CRTM_Atmosphere structures.
!
!         CRTM_Zero_Atmosphere:         Subroutine to zero-out all members of a 
!                                       CRTM_Atmosphere structure - both scalar and
!                                       pointer.
!
!       PRIVATE subprograms
!       -------------------
!         CRTM_Clear_Atmosphere:        Subroutine to clear the scalar members of a
!                                       CRTM_Atmosphere structure.
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Associated_Cloud:          Function to test the association status
!                                       of the pointer members of a CRTM_Cloud
!                                       structure.
!                                       SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_Destroy_Cloud:             Function to re-initialize a CRTM_Cloud
!                                       structure.
!                                       SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_Allocate_Cloud:            Function to allocate the pointer members
!                                       of a CRTM_Cloud structure.
!                                       SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_Assign_Cloud:              Function to copy a CRTM_Cloud structure.
!                                       SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_WeightedSum_Cloud:         Function to perform a weighted sum of two
!                                       CRTM_Cloud structures.
!
!       CRTM_Zero_Cloud:                Subroutine to zero-out all members of a 
!                                       CRTM_Cloud structure - both scalar and
!                                       pointer.
!
!       CRTM_Associated_Aerosol:        Function to test the association status
!                                       of the pointer members of a CRTM_Aerosol
!                                       structure.
!                                       SOURCE: CRTM_AEROSOL_DEFINE module
!
!       CRTM_Destroy_Aerosol:           Function to re-initialize a CRTM_Aerosol
!                                       structure.
!                                       SOURCE: CRTM_AEROSOL_DEFINE module
!
!       CRTM_Allocate_Aerosol:          Function to allocate the pointer members
!                                       of a CRTM_Aerosol structure.
!                                       SOURCE: CRTM_AEROSOL_DEFINE module
!
!       CRTM_Assign_Aerosol:            Function to copy a CRTM_Aerosol structure.
!                                       SOURCE: CRTM_AEROSOL_DEFINE module
!
!       CRTM_WeightedSum_Aerosol:       Function to perform a weighted sum of two
!                                       CRTM_Aerosol structures.
!
!       CRTM_Zero_Aerosol:              Subroutine to zero-out all members of a 
!                                       CRTM_Aerosol structure - both scalar and
!                                       pointer.
!
!
! DERIVED TYPES:
!       CRTM_Atmosphere_type
!       --------------------
!         Definition of the public CRTM_Atmosphere data structure.
!         Fields are,
!
!         Max_Layers:              The maximum number of atmospheric layers
!                                  defined for the Atmosphere data structure.
!                                  This is the "K" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Layers:                The number of atmospheric layers of the
!                                  Atmosphere data structure to use. The
!                                  default is to set n_Layers = Max_Layers,
!                                  i.e. use all the layers.
!                                  This is the "Kuse" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Absorbers:             Number of atmospheric absorbers defined 
!                                  for the Atmosphere data structure.
!                                  This is the "J" dimension.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Max_Clouds:              The maximum number of clouds defined for
!                                  the Atmosphere data structure.
!                                  This is the "Nc" dimension.
!                                  Note: Can be = 0 (i.e. clear sky)
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Clouds:                The number of clouds in the Atmosphere
!                                  structure to use. The default is to
!                                  set n_Clouds = Max_Clouds, i.e. use all
!                                  the cloud data.
!                                  This is the "NcUse" dimension.
!                                  Note: Can be = 0 (i.e. clear sky)
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Max_Aerosols:            The maximum number of aerosols types defined
!                                  for the Atmosphere data structure.
!                                  This is the "Na" dimension.
!                                  Note: Can be = 0 (i.e. clear sky)
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         n_Aerosols:              The number of aerosols in the Atmosphere
!                                  structure to use. The default is to
!                                  set n_Aerosols = Max_Aerosols, i.e. use
!                                  all the aerosol data.
!                                  This is the "NaUse" dimension.
!                                  Note: Can be = 0 (i.e. clear sky)
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Level_Temperature_Input: Flag to indicate whether or not the Level_Temperature
!                                  data has been supplied.
!                                  If Level_Temperature_Input = 0, the level temperature
!                                                                  profile HAS NOT been
!                                                                  supplied.
!                                     Level_Temperature_Input = 1, the level temperature
!                                                                  profile HAS been
!                                                                  supplied. [DEFAULT]
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Climatology:             The climatology model associated with the 
!                                  Atmosphere profile data. This will be used
!                                  to supplement absorber amounts with their
!                                  climatological profiles if other data is
!                                  not available.
!                                  See PUBLIC PARAMETERS for the valid
!                                  climatology types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Absorber_ID:             A flag value used to identify a molecular
!                                  species in the absorber profile array.
!                                  See PUBLIC PARAMETERS for the valid
!                                  absorber ID values.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Rank-1 (n_Absorbers)
!                                  ATTRIBUTES: POINTER
!
!         Absorber_Units:          A flag value used to identify the units
!                                  of the absorber amounts in the absorber
!                                  profile array. These will be used to
!                                  convert input absorber amount units to
!                                  those expected by code requiring absorber
!                                  amount data.
!                                  See PUBLIC PARAMETERS for the valid
!                                  absorber units values.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Rank-1 (n_Absorbers)
!                                  ATTRIBUTES: POINTER
!
!         Level_Pressure:          The LEVEL pressures for the atmospheric
!                                  profile data.
!                                  UNITS:      hectoPascals (hPa)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (0:Max_Layers)
!                                  ATTRIBUTES: POINTER
!
!         Level_Temperature:       The LEVEL temperatures for the atmospheric
!                                  profile data. This is optional data. If not
!                                  specified, the LEVEL_TEMPERATURE_INPUT flag
!                                  should be set to NO (0).
!                                  UNITS:      Kelvin (K)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (0:Max_Layers)
!                                  ATTRIBUTES: POINTER
!
!         Pressure:                The LAYER pressures for the atmospheric
!                                  profile data.
!                                  UNITS:      hectoPascals (hPa)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (Max_Layers)
!                                  ATTRIBUTES: POINTER
!
!         Temperature:             The LAYER temperatures for the atmospheric
!                                  profile data.
!                                  UNITS:      Kelvin (K)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (Max_Layers)
!                                  ATTRIBUTES: POINTER
!
!         Absorber:                The LAYER absorber amounts for the atmospheric
!                                  profile data.
!                                  UNITS:      Varies with absorber
!                                              See ABSORBER_UNITS member.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (Max_Layers x n_Absorbers)
!                                  ATTRIBUTES: POINTER
!
!         Cloud:                   Structure containing cloud parameters for
!                                  the current atmospheric profile.
!                                  See the CRTM_Cloud_Define module.
!                                  UNITS:      N/A
!                                  TYPE:       TYPE( CRTM_Cloud_type )
!                                  DIMENSION:  Rank-1 (Max_Clouds)
!                                  ATTRIBUTES: POINTER
!
!         Aerosol:                 Structure containing aerosol parameters for
!                                  the current atmospheric profile.
!                                  See the CRTM_Aerosol_Define module.
!                                  UNITS:      N/A
!                                  TYPE:       TYPE( CRTM_Aerosol_type )
!                                  DIMENSION:  Rank-1 (Max_Aerosols)
!                                  ATTRIBUTES: POINTER
!
!       *!IMPORTANT!*
!       -------------
!       Note that the CRTM_Atmosphere_type is PUBLIC and its members are
!       not encapsulated; that is, they can be fully accessed outside the
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
!
!
! PUBLIC PARAMETERS:
!
!       CLIMATOLOGY
!       -----------
!
!         1) The valid climatology values used in the Atmosphere%Climatology field:
!
!                 Climatology Type        Parameter Name
!           ------------------------------------------------------
!                     Invalid             INVALID_MODEL         
!                     Tropical            TROPICAL              
!                Midlatitude summer       MIDLATITUDE_SUMMER    
!                Midlatitude winter       MIDLATITUDE_WINTER    
!                 Subarctic summer        SUBARCTIC_SUMMER      
!                 Subarctic winter        SUBARCTIC_WINTER      
!             U.S. Standard Atmosphere    US_STANDARD_ATMOSPHERE
!
!         2) The number of valid climatology models is specified by the 
!              N_VALID_CLIMATOLOGY_MODELS
!            parameter.
!
!         3) The valid climatology names are specified by the 
!             CLIMATOLOGY_MODEL_NAME
!            parameter. It used the above climatology type definitions to provide a
!            string identifying the climatology type. For example,
!              CLIMATOLOGY_MODEL_NAME( MIDLATITUDE_SUMMER )
!            contains the string
!              'Midlatitude summer'
!
!
!       ABSORBER IDENTIFICATION
!       -----------------------
!
!         1) The valid absorber ID values used in the Atmosphere%Absorber_ID field:
!
!             Molecular species        Parameter Name
!           ------------------------------------------------
!                 Invalid              INVALID_ABSORBER_ID
!                   H2O                H2O_ID
!                   CO2                CO2_ID
!                   O3                 O3_ID
!                   N2O                N2O_ID
!                   CO                 CO_ID
!                   CH4                CH4_ID
!                   O2                 O2_ID
!                   NO                 NO_ID
!                   SO2                SO2_ID
!                   NO2                NO2_ID
!                   NH3                NH3_ID
!                  HNO3                HNO3_ID
!                   OH                 OH_ID
!                   HF                 HF_ID
!                   HCl                HCl_ID
!                   HBr                HBr_ID
!                   HI                 HI_ID
!                   ClO                CLO_ID
!                   OCS                OCS_ID
!                  H2CO                H2CO_ID
!                  HOCl                HOCL_ID
!                   N2                 N2_ID
!                   HCN                HCN_ID
!                  CH3l                CH3L_ID
!                  H2O2                H2O2_ID
!                  C2H2                C2H2_ID
!                  C2H6                C2H6_ID
!                   PH3                PH3_ID
!                  COF2                COF2_ID
!                   SF6                SF6_ID
!                   H2S                H2S_ID
!                  HCOOH               HCOOH_ID
!
!         2) The number of valid absorber units is specified by the 
!              N_VALID_ABSORBER_UNITS
!            parameter.
!
!         3) The character string array parameter
!              ABSORBER_ID_NAME
!            uses the above absorber ID definitions to provide a string value for
!            the absorber. For example,
!              ABSORBER_ID_NAME( N2O_ID )
!            contains the string
!              'N2O'
!
!
!       ABSORBER UNITS IDENTIFICATION
!       -----------------------------
!
!         1) The valid absorber units flag values used in the Atmosphere%Absorber_Units field:
!
!             Absorber Units                         Parameter Name
!           -----------------------------------------------------------------------
!             Invalid                                INVALID_ABSORBER_UNITS
!             Volume mixing ratio, ppmv              VOLUME_MIXING_RATIO_UNITS
!             Number density, cm^-3                  NUMBER_DENSITY_UNITS
!             Mass mixing ratio, g/kg                MASS_MIXING_RATIO_UNITS
!             Mass density, g.m^-3                   MASS_DENSITY_UNITS
!             Partial pressure, hPa                  PARTIAL_PRESSURE_UNITS
!             Dewpoint temperature, K  (H2O ONLY)    DEWPOINT_TEMPERATURE_K_UNITS
!             Dewpoint temperature, C  (H2O ONLY)    DEWPOINT_TEMPERATURE_C_UNITS
!             Relative humidity, %     (H2O ONLY)    RELATIVE_HUMIDITY_UNITS
!             Specific amount, g/g                   SPECIFIC_AMOUNT_UNITS
!             Integrated path, mm                    INTEGRATED_PATH_UNITS
!
!         2) The number of valid absorber IDs is specified by the 
!              N_VALID_ABSORBER_IDS
!            parameter.
!
!         3) The character string array parameter
!              ABSORBER_UNITS_NAME
!            uses the above absorber units definitions to provide a string value for
!            the absorber units. For example,
!              ABSORBER_UNITS_NAME( MASS_MIXING_RATIO_UNITS )
!            contains the string
!              'Mass mixing ratio, g/kg'
!
!         4) The parameter array
!              H2O_ONLY_UNITS_FLAG
!            contains a list of flags for each absorber units type that idenitifies
!            it as being valid for *all* absorbers (0) or valid for water vapour
!            only (1). For example the values of
!              H2O_ONLY_UNITS_FLAG( RELATIVE_HUMIDITY_UNITS )
!            and
!              H2O_ONLY_UNITS_FLAG( SPECIFIC_AMOUNT_UNITS )
!            are 1 and 0 respectively indicating that the former is a valid unit for
!            water vapour ONLY but the latter can be used for any absorber.
!
!
! USE ASSOCIATED PUBLIC PARAMETERS:
!
!       CRTM_Cloud_Define MODULE
!       ------------------------
!
!         1) The valid cloud type values used in the Cloud%Type field:
!
!             Cloud Type      Parameter Name
!           ----------------------------------
!               None          NO_CLOUD
!               Water         WATER_CLOUD
!               Ice           ICE_CLOUD
!               Rain          RAIN_CLOUD
!               Snow          SNOW_CLOUD
!               Graupel       GRAUPEL_CLOUD
!               Hail          HAIL_CLOUD
!
!         2) The number of valid cloud types is specified by the 
!              N_VALID_CLOUD_TYPES
!            parameter.
!
!         3) The character string array parameter
!              CLOUD_TYPE_NAME
!            uses the above cloud type definitions to provide a string value for
!            the type of cloud. For example,
!              CLOUD_TYPE_NAME( GRAUPEL_CLOUD )
!            contains the string
!              'Graupel'
!
!
!       CRTM_Aerosol_Define MODULE
!       --------------------------
!
!         1) The valid aerosol type values used in the Aerosol%Type field:
!
!                Aerosol Type      Parameter Name
!           --------------------------------------------------
!                   None           NO_AEROSOL   
!                   Dust           DUST_AEROSOL   
!                  Sea salt        SEASALT_AEROSOL  
!             Dry organic carbon   DRY_ORGANIC_CARBON_AEROSOL
!             Wet organic carbon   WET_ORGANIC_CARBON_AEROSOL
!              Dry black carbon    DRY_BLACK_CARBON_AEROSOL
!              Wet black carbon    WET_BLACK_CARBON_AEROSOL
!                  Sulfate         SULFATE_AEROSOL  
!
!         2) The number of valid aerosol types is specified by the 
!              N_VALID_AEROSOL_TYPES
!            parameter.
!
!         3) The character string array parameter
!              AEROSOL_TYPE_NAME
!            uses the above aerosol type definitions to provide a string value for
!            the type of aerosol. For example,
!              AEROSOL_TYPE_NAME( DRY_BLACK_CARBON_AEROSOL )
!            contains the string
!              'Dry black carbon'
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
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
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
!--------------------------------------------------------------------------------

MODULE CRTM_Atmosphere_Define


  !#----------------------------------------------------------------------------#
  !#                             -- MODULES USED --                             #
  !#----------------------------------------------------------------------------#

  USE Type_Kinds
  USE Error_Handler

  USE CRTM_Cloud_Define
  USE CRTM_Aerosol_Define



  !#----------------------------------------------------------------------------#
  !#                       -- DISABLE IMPLICIT TYPING --                        #
  !#----------------------------------------------------------------------------#

  IMPLICIT NONE



  !#----------------------------------------------------------------------------#
  !#                              -- VISIBILITIES --                            #
  !#----------------------------------------------------------------------------#

  ! -----------------------------
  ! Everything private by default
  ! -----------------------------

  PRIVATE


  ! -------------------------------------
  ! CRTM_Cloud_Define module visibilities
  ! -------------------------------------

  ! -- CRTM_Cloud parameters
  PUBLIC :: N_VALID_CLOUD_TYPES

  PUBLIC :: NO_CLOUD
  PUBLIC :: WATER_CLOUD
  PUBLIC :: ICE_CLOUD
  PUBLIC :: RAIN_CLOUD
  PUBLIC :: SNOW_CLOUD
  PUBLIC :: GRAUPEL_CLOUD
  PUBLIC :: HAIL_CLOUD

  PUBLIC :: CLOUD_TYPE_NAME

  ! -- CRTM_Cloud structure data type
  ! -- in the CRTM_Cloud_Define module
  PUBLIC :: CRTM_Cloud_type

  ! -- CRTM_Cloud structure routines inherited
  ! -- from the CRTM_Cloud_Define module
  PUBLIC :: CRTM_Associated_Cloud
  PUBLIC :: CRTM_Destroy_Cloud
  PUBLIC :: CRTM_Allocate_Cloud
  PUBLIC :: CRTM_Assign_Cloud


  ! ---------------------------------------
  ! CRTM_Aerosol_Define module visibilities
  ! ---------------------------------------

  ! -- CRTM_Aerosol parameters
  PUBLIC :: N_VALID_AEROSOL_TYPES

  PUBLIC :: NO_AEROSOL
  PUBLIC :: DUST_AEROSOL
  PUBLIC :: SEASALT_AEROSOL
  PUBLIC :: DRY_ORGANIC_CARBON_AEROSOL
  PUBLIC :: WET_ORGANIC_CARBON_AEROSOL
  PUBLIC :: DRY_BLACK_CARBON_AEROSOL
  PUBLIC :: WET_BLACK_CARBON_AEROSOL
  PUBLIC :: SULFATE_AEROSOL  

  PUBLIC :: AEROSOL_TYPE_NAME

  ! -- CRTM_Aerosol structure data type
  ! -- in the CRTM_Aerosol_Define module
  PUBLIC :: CRTM_Aerosol_type

  ! -- CRTM_Aerosol structure routines inherited
  ! -- from the CRTM_Aerosol_Define module
  PUBLIC :: CRTM_Associated_Aerosol
  PUBLIC :: CRTM_Destroy_Aerosol
  PUBLIC :: CRTM_Allocate_Aerosol
  PUBLIC :: CRTM_Assign_Aerosol

  ! -- CRTM_Atmosphere routines in this module
  PUBLIC :: CRTM_Associated_Atmosphere
  PUBLIC :: CRTM_Destroy_Atmosphere
  PUBLIC :: CRTM_Allocate_Atmosphere
  PUBLIC :: CRTM_Assign_Atmosphere
  PUBLIC :: CRTM_WeightedSum_Atmosphere
  PUBLIC :: CRTM_Zero_Atmosphere



  !#----------------------------------------------------------------------------#
  !#                          -- PROCEDURE OVERLOADING --                       #
  !#----------------------------------------------------------------------------#

  INTERFACE CRTM_Destroy_Atmosphere
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Scalar_Multi
    MODULE PROCEDURE Destroy_Rank1
    MODULE PROCEDURE Destroy_Rank1_Multi
  END INTERFACE CRTM_Destroy_Atmosphere

  INTERFACE CRTM_Allocate_Atmosphere
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank00001
    MODULE PROCEDURE Allocate_Rank00011
    MODULE PROCEDURE Allocate_Rank00101
    MODULE PROCEDURE Allocate_Rank00111
    MODULE PROCEDURE Allocate_Rank10001
    MODULE PROCEDURE Allocate_Rank10101
    MODULE PROCEDURE Allocate_Rank10111
  END INTERFACE CRTM_Allocate_Atmosphere

  INTERFACE CRTM_Assign_Atmosphere
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Atmosphere

  INTERFACE CRTM_WeightedSum_Atmosphere
    MODULE PROCEDURE WeightedSum_Scalar
    MODULE PROCEDURE WeightedSum_Rank1
  END INTERFACE CRTM_WeightedSum_Atmosphere

  INTERFACE CRTM_Zero_Atmosphere
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Atmosphere



  !#----------------------------------------------------------------------------#
  !#                         -- PUBLIC MODULE PARAMETERS --                     #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------
  ! The absorber IDs. Use HITRAN definitions
  ! ----------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_VALID_ABSORBER_IDS = 32

  INTEGER, PUBLIC, PARAMETER :: INVALID_ABSORBER_ID =  0
  INTEGER, PUBLIC, PARAMETER ::   H2O_ID =  1
  INTEGER, PUBLIC, PARAMETER ::   CO2_ID =  2
  INTEGER, PUBLIC, PARAMETER ::    O3_ID =  3
  INTEGER, PUBLIC, PARAMETER ::   N2O_ID =  4
  INTEGER, PUBLIC, PARAMETER ::    CO_ID =  5
  INTEGER, PUBLIC, PARAMETER ::   CH4_ID =  6
  INTEGER, PUBLIC, PARAMETER ::    O2_ID =  7
  INTEGER, PUBLIC, PARAMETER ::    NO_ID =  8
  INTEGER, PUBLIC, PARAMETER ::   SO2_ID =  9
  INTEGER, PUBLIC, PARAMETER ::   NO2_ID = 10
  INTEGER, PUBLIC, PARAMETER ::   NH3_ID = 11
  INTEGER, PUBLIC, PARAMETER ::  HNO3_ID = 12
  INTEGER, PUBLIC, PARAMETER ::    OH_ID = 13
  INTEGER, PUBLIC, PARAMETER ::    HF_ID = 14
  INTEGER, PUBLIC, PARAMETER ::   HCl_ID = 15
  INTEGER, PUBLIC, PARAMETER ::   HBr_ID = 16
  INTEGER, PUBLIC, PARAMETER ::    HI_ID = 17
  INTEGER, PUBLIC, PARAMETER ::   ClO_ID = 18
  INTEGER, PUBLIC, PARAMETER ::   OCS_ID = 19
  INTEGER, PUBLIC, PARAMETER ::  H2CO_ID = 20
  INTEGER, PUBLIC, PARAMETER ::  HOCl_ID = 21
  INTEGER, PUBLIC, PARAMETER ::    N2_ID = 22
  INTEGER, PUBLIC, PARAMETER ::   HCN_ID = 23
  INTEGER, PUBLIC, PARAMETER ::  CH3l_ID = 24
  INTEGER, PUBLIC, PARAMETER ::  H2O2_ID = 25
  INTEGER, PUBLIC, PARAMETER ::  C2H2_ID = 26
  INTEGER, PUBLIC, PARAMETER ::  C2H6_ID = 27
  INTEGER, PUBLIC, PARAMETER ::   PH3_ID = 28
  INTEGER, PUBLIC, PARAMETER ::  COF2_ID = 29
  INTEGER, PUBLIC, PARAMETER ::   SF6_ID = 30
  INTEGER, PUBLIC, PARAMETER ::   H2S_ID = 31
  INTEGER, PUBLIC, PARAMETER :: HCOOH_ID = 32

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_IDS ) :: &
    ABSORBER_ID_NAME = (/ 'Invalid', &
                          'H2O    ', 'CO2    ', 'O3     ', 'N2O    ', &
                          'CO     ', 'CH4    ', 'O2     ', 'NO     ', &
                          'SO2    ', 'NO2    ', 'NH3    ', 'HNO3   ', &
                          'OH     ', 'HF     ', 'HCl    ', 'HBr    ', &
                          'HI     ', 'ClO    ', 'OCS    ', 'H2CO   ', &
                          'HOCl   ', 'N2     ', 'HCN    ', 'CH3Cl  ', &
                          'H2O2   ', 'C2H2   ', 'C2H6   ', 'PH3    ', &
                          'COF2   ', 'SF6    ', 'H2S    ', 'HCOOH  ' /)



  ! ---------------------------------------------------------
  ! The absorber units. Use LBLRTM definitions and then some.
  ! ---------------------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_VALID_ABSORBER_UNITS = 10

  INTEGER, PUBLIC, PARAMETER ::       INVALID_ABSORBER_UNITS =  0
  INTEGER, PUBLIC, PARAMETER ::    VOLUME_MIXING_RATIO_UNITS =  1
  INTEGER, PUBLIC, PARAMETER ::         NUMBER_DENSITY_UNITS =  2
  INTEGER, PUBLIC, PARAMETER ::      MASS_MIXING_RATIO_UNITS =  3
  INTEGER, PUBLIC, PARAMETER ::           MASS_DENSITY_UNITS =  4
  INTEGER, PUBLIC, PARAMETER ::       PARTIAL_PRESSURE_UNITS =  5
  INTEGER, PUBLIC, PARAMETER :: DEWPOINT_TEMPERATURE_K_UNITS =  6 ! H2O only
  INTEGER, PUBLIC, PARAMETER :: DEWPOINT_TEMPERATURE_C_UNITS =  7 ! H2O only
  INTEGER, PUBLIC, PARAMETER ::      RELATIVE_HUMIDITY_UNITS =  8 ! H2O only
  INTEGER, PUBLIC, PARAMETER ::        SPECIFIC_AMOUNT_UNITS =  9
  INTEGER, PUBLIC, PARAMETER ::        INTEGRATED_PATH_UNITS = 10

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_UNITS ) :: &
    ABSORBER_UNITS_NAME = (/ 'Invalid units                      ', &
                             'Volume mixing ratio, ppmv          ', &
                             'Number density, cm^-3              ', &
                             'Mass mixing ratio, g/kg            ', &
                             'Mass density, g.m^-3               ', &
                             'Partial pressure, hPa              ', &
                             'Dewpoint temperature, K  (H2O ONLY)', &
                             'Dewpoint temperature, C  (H2O ONLY)', &
                             'Relative humidity, %     (H2O ONLY)', &
                             'Specific amount, g/g               ', &
                             'Integrated path, mm                ' /)

  INTEGER, PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ABSORBER_UNITS ) :: &
    H2O_ONLY_UNITS_FLAG = (/ 0, &  ! None
                             0, &  ! Volume mixing ratio, ppmv
                             0, &  ! Number density, cm^-3
                             0, &  ! Mass mixing ratio, g/kg
                             0, &  ! Mass density, g.m^-3
                             0, &  ! Partial pressure, hPa
                             1, &  ! Dewpoint temperature, K  (H2O ONLY)
                             1, &  ! Dewpoint temperature, C  (H2O ONLY)
                             1, &  ! Relative humidity, %     (H2O ONLY)
                             0, &  ! Specific amount, g/g
                             0 /)  ! Integrated path, mm



  ! ----------------------
  ! The climatology models
  ! ----------------------

  INTEGER, PUBLIC, PARAMETER :: N_VALID_CLIMATOLOGY_MODELS = 6

  INTEGER, PUBLIC, PARAMETER :: INVALID_MODEL          = 0
  INTEGER, PUBLIC, PARAMETER :: TROPICAL               = 1
  INTEGER, PUBLIC, PARAMETER :: MIDLATITUDE_SUMMER     = 2
  INTEGER, PUBLIC, PARAMETER :: MIDLATITUDE_WINTER     = 3
  INTEGER, PUBLIC, PARAMETER :: SUBARCTIC_SUMMER       = 4
  INTEGER, PUBLIC, PARAMETER :: SUBARCTIC_WINTER       = 5
  INTEGER, PUBLIC, PARAMETER :: US_STANDARD_ATMOSPHERE = 6 

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_CLIMATOLOGY_MODELS ) :: &
    CLIMATOLOGY_MODEL_NAME = (/ 'Invalid                 ', &
                                'Tropical                ', &
                                'Midlatitude summer      ', &
                                'Midlatitude winter      ', &
                                'Subarctic summer        ', &
                                'Subarctic winter        ', &
                                'U.S. Standard Atmosphere' /)

  ! ----------------
  ! The YES/NO flags
  ! ----------------

  INTEGER, PUBLIC, PARAMETER :: NO    = 0
  INTEGER, PUBLIC, PARAMETER :: YES   = 1



  !#----------------------------------------------------------------------------#
  !#                      -- PRIVATE MODULE PARAMETERS --                       #
  !#----------------------------------------------------------------------------#

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Atmosphere_Define.f90,v 1.18 2005/09/20 15:17:30 yhan Exp $'

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind

  ! -- Keyword set values
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1

  ! -- The maximum number of aerosol size distribution modes
  INTEGER, PRIVATE, PARAMETER :: MAX_N_AEROSOL_MODES = 4



  !#----------------------------------------------------------------------------#
  !#                     -- CRTM_Atmosphere_type DEFINITION --                  #
  !#----------------------------------------------------------------------------#

  TYPE, PUBLIC :: CRTM_Atmosphere_type
    INTEGER :: n_Allocates = 0

    ! -- Dimension values
    INTEGER :: Max_Layers   = 0  ! K dimension
    INTEGER :: n_Layers     = 0  ! Kuse dimension

    INTEGER :: n_Absorbers  = 0  ! J dimension

    INTEGER :: Max_Clouds   = 0  ! Nc dimension
    INTEGER :: n_Clouds     = 0  ! NcUse dimension

    INTEGER :: Max_Aerosols = 0  ! Na dimension
    INTEGER :: n_Aerosols   = 0  ! NaUse dimension

    ! -- Flag to determine if level temperatures have been supplied.
    ! -- Default is to assume that they HAVE been supplied.
    INTEGER :: Level_Temperature_Input = YES

    ! -- Climatology model associated with the profile
    INTEGER :: Climatology = INVALID_MODEL

    ! -- Absorber ID and units
    INTEGER, DIMENSION( : ), POINTER :: Absorber_ID    => NULL() ! J
    INTEGER, DIMENSION( : ), POINTER :: Absorber_Units => NULL() ! J

    ! -- Profile LEVEL and LAYER quantities
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Level_Pressure    => NULL()  ! 0:K
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Level_Temperature => NULL()  ! 0:K
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Pressure          => NULL()  ! K
    REAL( fp_kind ), DIMENSION( : ),    POINTER :: Temperature       => NULL()  ! K
    REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Absorber          => NULL()  ! K x J

    ! -- Clouds associated with each profile
    TYPE( CRTM_Cloud_type ),   DIMENSION( : ), POINTER :: Cloud   => NULL()  ! Nc

    ! -- Aerosols associated with each profile
    TYPE( CRTM_Aerosol_type ), DIMENSION( : ), POINTER :: Aerosol => NULL()  ! Na

  END TYPE CRTM_Atmosphere_type


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
!       CRTM_Clear_Atmosphere
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM_Atmosphere structure.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Atmosphere( Atmosphere ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Atmosphere:  Atmosphere structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Atmosphere_type
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
!       Note the INTENT on the output Atmosphere argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Atmosphere( Atmosphere )

    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT ) :: Atmosphere

    ! -- Dimensions
    Atmosphere%Max_Layers   = 0
    Atmosphere%n_Layers     = 0

    Atmosphere%n_Absorbers  = 0

    Atmosphere%Max_Clouds   = 0
    Atmosphere%n_Clouds     = 0

    Atmosphere%Max_Aerosols = 0
    Atmosphere%n_Aerosols   = 0

    ! -- Flag to determine if level temperatures have been supplied.
    ! -- Default is to assume that they HAVE been supplied.
    Atmosphere%Level_Temperature_Input = YES

    ! -- Climatology model associated with the profile
    Atmosphere%Climatology = INVALID_MODEL

  END SUBROUTINE CRTM_Clear_Atmosphere






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
!       CRTM_Associated_Atmosphere
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       CRTM_Atmosphere structure.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Association_Status = CRTM_Associated_Atmosphere( Atmosphere,                 &  ! Input
!                                                        ANY_Test     = Any_Test,    &  ! Optional input
!                                                        Skip_Cloud   = Skip_Cloud,  &  ! Optional input
!                                                        Skip_Aerosol = Skip_Aerosol )  ! Optional input
!
! INPUT ARGUMENTS:
!       Atmosphere:          Structure which is to have its pointer
!                            member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       CRTM_Atmosphere_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Atmosphere structure pointer members are associated.
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
!       Skip_Cloud:          Set this argument to not include the Cloud
!                            member in the association test. This is required
!                            because a valid Atmosphere structure can be
!                            cloud-free.
!                            If Skip_Cloud = 0, the Cloud member association
!                                               status is tested.  (DEFAULT)
!                               Skip_Cloud = 1, the Cloud member association
!                                               status is NOT tested.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Skip_Aerosol:        Set this argument to not include the Aerosol
!                            member in the association test. This is required
!                            because a valid Atmosphere structure can be
!                            aerosol-free.
!                            If Skip_Aerosol = 0, the Aerosol member association
!                                                 status is tested.  (DEFAULT)
!                               Skip_Aerosol = 1, the Aerosol member association
!                                                 status is NOT tested.
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
!                            association status of the Atmosphere pointer members.
!                            .TRUE.  - if ALL the Atmosphere pointer members are
!                                      associated, or if the ANY_Test argument
!                                      is set and ANY of the Atmosphere pointer
!                                      members are associated.
!                            .FALSE. - some or all of the Atmosphere pointer
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
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Associated_Atmosphere( Atmosphere,    & ! Input
                                       ANY_Test,      & ! Optional input
                                       Skip_Cloud,    & ! Optional input
                                       Skip_Aerosol ) & ! Optional input
                                     RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ), INTENT( IN ) :: Atmosphere

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN ) :: ANY_Test
    INTEGER,            OPTIONAL, INTENT( IN ) :: Skip_Cloud
    INTEGER,            OPTIONAL, INTENT( IN ) :: Skip_Aerosol


    ! ---------------
    ! Function result
    ! ---------------

    LOGICAL :: Association_Status


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: ALL_Test
    LOGICAL :: Include_Cloud
    LOGICAL :: Include_Aerosol



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! The ANY_Test optional argument
    ! ------------------------------

    ! -- Default is to test ALL the pointer members
    ! -- for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF


    ! --------------------------------
    ! The Skip_Cloud optional argument
    ! --------------------------------

    ! -- Default is to include the Cloud member
    ! -- in the association test....
    Include_Cloud = .TRUE.

    ! ...unless the Skip_Cloud argument is set.
    IF ( PRESENT( Skip_Cloud ) ) THEN
      IF ( Skip_Cloud == SET ) Include_Cloud = .FALSE.
    END IF


    ! ----------------------------------
    ! The Skip_Aerosol optional argument
    ! ----------------------------------

    ! -- Default is to include the Aerosol member
    ! -- in the association test....
    Include_Aerosol = .TRUE.

    ! ...unless the Skip_Aerosol argument is set.
    IF ( PRESENT( Skip_Aerosol ) ) THEN
      IF ( Skip_Aerosol == SET ) Include_Aerosol = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Initialise a result
    ! -------------------

    Association_Status = .FALSE.


    ! ----------------------------------------
    ! Test the members that MUST be associated
    ! ----------------------------------------

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( Atmosphere%Absorber_ID       ) .AND. &
           ASSOCIATED( Atmosphere%Absorber_Units    ) .AND. &
           ASSOCIATED( Atmosphere%Level_Pressure    ) .AND. &
           ASSOCIATED( Atmosphere%Level_Temperature ) .AND. &
           ASSOCIATED( Atmosphere%Pressure          ) .AND. &
           ASSOCIATED( Atmosphere%Temperature       ) .AND. &
           ASSOCIATED( Atmosphere%Absorber          )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( Atmosphere%Absorber_ID       ) .OR. &
           ASSOCIATED( Atmosphere%Absorber_Units    ) .OR. &
           ASSOCIATED( Atmosphere%Level_Pressure    ) .OR. &
           ASSOCIATED( Atmosphere%Level_Temperature ) .OR. &
           ASSOCIATED( Atmosphere%Pressure          ) .OR. &
           ASSOCIATED( Atmosphere%Temperature       ) .OR. &
           ASSOCIATED( Atmosphere%Absorber          )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF


    ! ---------------------------------------
    ! Test the members that MAY be associated
    ! ---------------------------------------

    ! -- Clouds
    IF ( Include_Cloud ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status             .AND. &
             ASSOCIATED( Atmosphere%Cloud )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status             .OR. &
             ASSOCIATED( Atmosphere%Cloud )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF


    ! -- Aerosols
    IF ( Include_Aerosol ) THEN

      IF ( ALL_Test ) THEN

        IF ( Association_Status               .AND. &
             ASSOCIATED( Atmosphere%Aerosol )       ) THEN
          Association_Status = .TRUE.
        END IF

      ELSE

        IF ( Association_Status               .OR. &
             ASSOCIATED( Atmosphere%Aerosol )      ) THEN
          Association_Status = .TRUE.
        END IF

      END IF

    END IF

  END FUNCTION CRTM_Associated_Atmosphere





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Destroy_Atmosphere
! 
! PURPOSE:
!       Function to re-initialize CRTM_Atmosphere data structures.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Atmosphere( Atmosphere1, [ Atmosphere2, ..., Atmosphere10, ] &  ! Output
!                                               RCS_Id = RCS_Id,                                 &  ! Revision control
!                                               Message_Log = Message_Log                        )  ! Error messaging
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
!       Atmosphere1, [ Atmosphere2, ..., Atmosphere10 ]:
!                     Re-initialized Atmosphere structure(s). At least one
!                     structure or structure array must be specified, and
!                     no more than 10 structures or structure arrays must
!                     be specified.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar
!                                   OR
!                                 Rank-1 array
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
!       CRTM_Clear_Atmosphere:       Subroutine to clear the scalar members of a
!                                    CRTM_Atmosphere structure.
!
!       CRTM_Associated_Atmosphere:  Function to test the association status of
!                                    the pointer members of a CRTM_Atmosphere
!                                    structure.
!
!       CRTM_Destroy_Cloud:          Function to re-initialize CRTM_Cloud
!                                    structures.
!                                    SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_Destroy_Aerosol:        Function to re-initialize CRTM_Aerosol
!                                    structures.
!                                    SOURCE: CRTM_AEROSOL_DEFINE module
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
!       Note the INTENT on the output Atmosphere argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Feb-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Atmosphere,   &  ! Output
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
    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT ) :: Atmosphere

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Atmosphere(Scalar)'


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

    IF ( Clear ) CALL CRTM_Clear_Atmosphere( Atmosphere )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere ) ) RETURN


    ! ------------------------------------
    ! Deallocate the array pointer members
    ! ------------------------------------

    ! -- Deallocate the Atmosphere Absorber_ID member
    IF ( ASSOCIATED( Atmosphere%Absorber_ID ) ) THEN

      DEALLOCATE( Atmosphere%Absorber_ID, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Absorber_ID ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Absorber_Units member
    IF ( ASSOCIATED( Atmosphere%Absorber_Units ) ) THEN

      DEALLOCATE( Atmosphere%Absorber_Units, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Absorber_Units ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Level_Pressure member
    IF ( ASSOCIATED( Atmosphere%Level_Pressure ) ) THEN

      DEALLOCATE( Atmosphere%Level_Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Level_Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Level_Temperature member
    IF ( ASSOCIATED( Atmosphere%Level_Temperature ) ) THEN

      DEALLOCATE( Atmosphere%Level_Temperature, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Level_Temperature ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Pressure member
    IF ( ASSOCIATED( Atmosphere%Pressure ) ) THEN

      DEALLOCATE( Atmosphere%Pressure, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Pressure ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Temperature member
    IF ( ASSOCIATED( Atmosphere%Temperature ) ) THEN

      DEALLOCATE( Atmosphere%Temperature, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Temperature ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Deallocate the Atmosphere Absorber member
    IF ( ASSOCIATED( Atmosphere%Absorber ) ) THEN

      DEALLOCATE( Atmosphere%Absorber, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere Absorber ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! ---------------------------------------------------
    ! Deallocate the Cloud structure array pointer member
    ! ---------------------------------------------------

    IF ( ASSOCIATED( Atmosphere%Cloud ) ) THEN


      ! -- Destroy the cloud structure(s)
      Error_Status = CRTM_Destroy_Cloud( Atmosphere%Cloud, &
                                         No_Clear = No_Clear, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying CRTM_Atmosphere Cloud structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( Atmosphere%Cloud, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere cloud ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -----------------------------------------------------
    ! Deallocate the Aerosol structure array pointer member
    ! -----------------------------------------------------

    IF ( ASSOCIATED( Atmosphere%Aerosol ) ) THEN


      ! -- Destroy the aerosol structure(s)
      Error_Status = CRTM_Destroy_Aerosol( Atmosphere%Aerosol, &
                                           No_Clear = No_Clear, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error destroying CRTM_Atmosphere Aerosol structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF


      ! -- Deallocate the array
      DEALLOCATE( Atmosphere%Aerosol, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error deallocating CRTM_Atmosphere cloud ", &
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

    Atmosphere%n_Allocates = Atmosphere%n_Allocates - 1

    IF ( Atmosphere%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Atmosphere%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar


  FUNCTION Destroy_Scalar_Multi( Atmosphere1,  &  ! Output
                                 Atmosphere2,  &  ! Output
                                 Atmosphere3,  &  ! Optional Output
                                 Atmosphere4,  &  ! Optional Output
                                 Atmosphere5,  &  ! Optional Output
                                 Atmosphere6,  &  ! Optional Output
                                 Atmosphere7,  &  ! Optional Output
                                 Atmosphere8,  &  ! Optional Output
                                 Atmosphere9,  &  ! Optional Output
                                 Atmosphere10, &  ! Optional Output
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
    TYPE( CRTM_Atmosphere_type ),           INTENT( IN OUT ) :: Atmosphere1
    TYPE( CRTM_Atmosphere_type ),           INTENT( IN OUT ) :: Atmosphere2

    ! -- Optional output
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere3
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere4
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere5
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere6
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere7
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere8
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere9
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, INTENT( IN OUT ) :: Atmosphere10

    ! -- Optional input
    INTEGER,                      OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Atmosphere(Scalar,Multi)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Destroy_Status



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
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! The mandatory arguments
    ! -----------------------

    Destroy_Status = Destroy_Scalar( Atmosphere1, &
                                     No_Clear = No_Clear, &
                                     Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying first Atmosphere structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    Destroy_Status = Destroy_Scalar( Atmosphere2, &
                                     No_Clear = No_Clear, &
                                     Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying second Atmosphere structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! The optional arguments
    ! ----------------------

    IF ( PRESENT( Atmosphere3 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere3, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying third Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere4 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere4, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fourth Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere5 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere5, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fifth Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere6 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere6, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying sixth Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere7 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere7, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying seventh Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere8 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere8, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying eighth Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere9 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere9, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ninth Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere10 ) ) THEN
      Destroy_Status = Destroy_Scalar( Atmosphere10, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying tenth Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF

  END FUNCTION Destroy_Scalar_Multi


  FUNCTION Destroy_Rank1( Atmosphere,   &  ! Output
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
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Optional input
    INTEGER,                      OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Atmosphere(Rank-1)'


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

    DO n = 1, SIZE( Atmosphere )

      Scalar_Status = Destroy_Scalar( Atmosphere(n), &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1

  FUNCTION Destroy_Rank1_Multi( Atmosphere1,  &  ! Output
                                Atmosphere2,  &  ! Output
                                Atmosphere3,  &  ! Optional Output
                                Atmosphere4,  &  ! Optional Output
                                Atmosphere5,  &  ! Optional Output
                                Atmosphere6,  &  ! Optional Output
                                Atmosphere7,  &  ! Optional Output
                                Atmosphere8,  &  ! Optional Output
                                Atmosphere9,  &  ! Optional Output
                                Atmosphere10, &  ! Optional Output
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
    TYPE( CRTM_Atmosphere_type ),           DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere1
    TYPE( CRTM_Atmosphere_type ),           DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere2

    ! -- Optional output
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere3
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere4
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere5
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere6
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere7
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere8
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere9
    TYPE( CRTM_Atmosphere_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere10

    ! -- Optional input
    INTEGER,                      OPTIONAL,                 INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,                 INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,                 INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Atmosphere(Rank-1,Multi)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Destroy_Status



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
    !#                      -- PERFORM REINITIALISATION --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! The mandatory arguments
    ! -----------------------

    Destroy_Status = Destroy_Rank1( Atmosphere1, &
                                    No_Clear = No_Clear, &
                                    Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying first Atmosphere structure array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    Destroy_Status = Destroy_Rank1( Atmosphere2, &
                                    No_Clear = No_Clear, &
                                    Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying second Atmosphere structure array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! The optional arguments
    ! ----------------------

    IF ( PRESENT( Atmosphere3 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere3, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying third Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere4 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere4, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fourth Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere5 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere5, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fifth Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere6 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere6, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying sixth Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere7 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere7, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying seventh Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere8 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere8, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying eighth Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere9 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere9, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ninth Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Atmosphere10 ) ) THEN
      Destroy_Status = Destroy_Rank1( Atmosphere10, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying tenth Atmosphere structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF

  END FUNCTION Destroy_Rank1_Multi





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_Atmosphere
! 
! PURPOSE:
!       Function to allocate CRTM_Atmosphere data structures.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Atmosphere( n_Layers,                 &  ! Input
!                                                n_Absorbers,              &  ! Input
!                                                n_Clouds,                 &  ! Input
!                                                n_Aerosols,               &  ! Input
!                                                Atmosphere,               &  ! Output
!                                                RCS_Id      = RCS_Id,     &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Layers:     Number of layers dimension.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Atmosphere dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Absorbers:  Number of absorbers dimension. This will be the same for
!                     all elements if the Atmosphere argument is an array.
!                     Must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Clouds:     Number of clouds dimension of Atmosphere data.
!                     ** Note: Can be = 0 (i.e. clear sky). **
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Atmosphere dimensionality chart
!                     ATTRIBUTES: INTENT( IN )
!
!       n_Aerosols:   Number of aerosol types dimension of Atmosphere data.
!                     ** Note: Can be = 0 (i.e. no aerosols). **
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Atmosphere dimensionality chart
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
!       Atmosphere:   Atmosphere structure with allocated pointer members. The
!                     following table shows the allowable dimension combinations
!                     for the calling routine, where M == number of profiles:
!
!                        Input       Input       Input      Input        Output
!                       n_Layers   n_Absorbers  n_Clouds  n_Aerosols    Atmosphere
!                       dimension   dimension   dimension  dimension    dimension
!                     --------------------------------------------------------------
!                        scalar      scalar      scalar     scalar       scalar
!                        scalar      scalar      scalar     scalar         M
!                        scalar      scalar      scalar       M            M
!                        scalar      scalar        M        scalar         M
!                        scalar      scalar        M          M            M
!                          M         scalar      scalar     scalar         M
!                          M         scalar      scalar       M            M
!                          M         scalar        M        scalar         M
!                          M         scalar        M          M            M
!
!                     Note the number of absorbers cannot vary with the profile.
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or Rank-1
!                                 See chart above.
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
!       CRTM_Associated_Atmosphere: Function to test the association status of the
!                                   pointer members of a Atmosphere structure.
!
!       CRTM_Destroy_Atmosphere:    Function to re-initialize CRTM_Atmosphere
!                                   structures.
!
!       CRTM_Allocate_Cloud:        Function to allocate CRTM_Cloud structures.
!                                   SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_Allocate_Aerosol:      Function to allocate CRTM_Aerosol structures.
!                                   SOURCE: CRTM_AEROSOL_DEFINE module
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
!       Note the INTENT on the output Atmosphere argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Mar-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Layers,     &  ! Input
                            n_Absorbers,  &  ! Input
                            n_Clouds,     &  ! Input
                            n_Aerosols,   &  ! Input
                            Atmosphere,   &  ! Output
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
    INTEGER,                      INTENT( IN )     :: n_Layers    
    INTEGER,                      INTENT( IN )     :: n_Absorbers 
    INTEGER,                      INTENT( IN )     :: n_Clouds    
    INTEGER,                      INTENT( IN )     :: n_Aerosols    

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Scalar)'


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

    ! -- Number of layers
    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of absorbers
    IF ( n_Absorbers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Absorbers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of clouds. Can be == 0.
    IF ( n_Clouds < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Clouds must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Number of aerosols. Can be == 0.
    IF ( n_Aerosols < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Aerosols must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Check if ANY pointers are already associated
    ! If they are, deallocate them but leave scalars.
    ! -----------------------------------------------

    IF ( CRTM_Associated_Atmosphere( Atmosphere, ANY_Test = SET ) ) THEN

      Error_Status = CRTM_Destroy_Atmosphere( Atmosphere, &
                                              No_Clear = SET, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating CRTM_Atmosphere pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! The intrinsic type arrays
    ! -------------------------

    ALLOCATE( Atmosphere%Absorber_ID( n_Absorbers ), &
              Atmosphere%Absorber_Units( n_Absorbers ), &
              Atmosphere%Level_Pressure( 0:n_Layers ), &
              Atmosphere%Level_Temperature( 0:n_Layers ), &
              Atmosphere%Pressure( n_Layers ), &
              Atmosphere%Temperature( n_Layers ), &
              Atmosphere%Absorber( n_Layers, n_Absorbers ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating CRTM_Atmosphere data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! The cloud structure array
    ! -------------------------

    IF ( n_Clouds > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( Atmosphere%Cloud( n_Clouds ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating CRTM_Atmosphere Cloud structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Allocate the individual structures
      Error_Status = CRTM_Allocate_Cloud( n_Layers,         &
                                          Atmosphere%Cloud, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error allocating CRTM_Cloud structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    ! ---------------------------
    ! The aerosol structure array
    ! ---------------------------

    IF ( n_Aerosols > 0 ) THEN

      ! -- Allocate the structure array
      ALLOCATE( Atmosphere%Aerosol( n_Aerosols ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error allocating CRTM_Atmosphere Aerosol structure array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Allocate the individual structures
      Error_Status = CRTM_Allocate_Aerosol( n_Layers, &
                                            MAX_N_AEROSOL_MODES, &
                                            Atmosphere%Aerosol, &
                                            Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error allocating CRTM_Aerosol structure(s).', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSIONS AND INITALISE ARRAYS --             #
    !#--------------------------------------------------------------------------#

    Atmosphere%Max_Layers   = n_Layers
    Atmosphere%n_Layers     = n_Layers

    Atmosphere%n_Absorbers  = n_Absorbers

    Atmosphere%Max_Clouds   = n_Clouds
    Atmosphere%n_Clouds     = n_Clouds

    Atmosphere%Max_Aerosols = n_Aerosols
    Atmosphere%n_Aerosols   = n_Aerosols

    Atmosphere%Absorber_ID    = INVALID_ABSORBER_ID
    Atmosphere%Absorber_Units = INVALID_ABSORBER_UNITS

    Atmosphere%Level_Pressure    = ZERO
    Atmosphere%Level_Temperature = ZERO
    Atmosphere%Pressure          = ZERO
    Atmosphere%Temperature       = ZERO
    Atmosphere%Absorber          = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    Atmosphere%n_Allocates = Atmosphere%n_Allocates + 1

    IF ( Atmosphere%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Atmosphere%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_Scalar


  FUNCTION Allocate_Rank00001( n_Layers,     &  ! Input, scalar
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input, scalar
                               n_Aerosols,   &  ! Input, scalar
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                                      INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                                      INTENT( IN )     :: n_Clouds
    INTEGER,                                      INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-00001)'


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

    DO i = 1, SIZE( Atmosphere )

      Scalar_Status = Allocate_Scalar( n_Layers,      & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds,      & ! Input
                                       n_Aerosols,    & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank00001


  FUNCTION Allocate_Rank00011( n_Layers,     &  ! Input, scalar
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input, scalar
                               n_Aerosols,   &  ! Input,  M
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                                      INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                                      INTENT( IN )     :: n_Clouds
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-00011)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Aerosols ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Aerosols and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers,      & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds,      & ! Input
                                       n_Aerosols(i), & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank00011


  FUNCTION Allocate_Rank00101( n_Layers,     &  ! Input, scalar
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input,  M
                               n_Aerosols,   &  ! Input, scalar
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                                      INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Clouds
    INTEGER,                                      INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-00101)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Clouds ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Clouds and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers,      & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds(i),   & ! Input
                                       n_Aerosols,    & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank00101


  FUNCTION Allocate_Rank00111( n_Layers,     &  ! Input, scalar
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input,  M
                               n_Aerosols,   &  ! Input,  M
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                                      INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Clouds
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-00111)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Clouds   ) /= n .OR. &
         SIZE( n_Aerosols ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Clouds, n_Aerosols and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers,      & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds(i),   & ! Input
                                       n_Aerosols(i), & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank00111


  FUNCTION Allocate_Rank10001( n_Layers,     &  ! Input,  M
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input, scalar
                               n_Aerosols,   &  ! Input, scalar
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                                      INTENT( IN )     :: n_Clouds
    INTEGER,                                      INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-10001)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Layers ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i),   & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds,      & ! Input
                                       n_Aerosols,    & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank10001


  FUNCTION Allocate_Rank10011( n_Layers,     &  ! Input,  M
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input, scalar
                               n_Aerosols,   &  ! Input,  M
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                                      INTENT( IN )     :: n_Clouds
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-10011)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Layers   ) /= n .OR. &
         SIZE( n_Aerosols ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers, n_Aerosols and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i),   & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds,      & ! Input
                                       n_Aerosols(i), & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank10011


  FUNCTION Allocate_Rank10101( n_Layers,     &  ! Input,  M
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input,  M
                               n_Aerosols,   &  ! Input, scalar
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Clouds
    INTEGER,                                      INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-10101)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Layers ) /= n .OR. &
         SIZE( n_Clouds ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers, n_Clouds and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i),   & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds(i),   & ! Input
                                       n_Aerosols,    & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank10101


  FUNCTION Allocate_Rank10111( n_Layers,     &  ! Input,  M
                               n_Absorbers,  &  ! Input, scalar
                               n_Clouds,     &  ! Input,  M
                               n_Aerosols,   &  ! Input,  M
                               Atmosphere,   &  ! Output, M
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
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Layers
    INTEGER,                                      INTENT( IN )     :: n_Absorbers
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Clouds
    INTEGER,                      DIMENSION( : ), INTENT( IN )     :: n_Aerosols

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Atmosphere(Rank-10111)'


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

    ! ----------------------------
    ! Array arguments must conform
    ! ----------------------------

    n = SIZE( Atmosphere )

    IF ( SIZE( n_Layers   ) /= n .OR. &
         SIZE( n_Clouds   ) /= n .OR. &
         SIZE( n_Aerosols ) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Layers, n_Clouds, n_Aerosols and Atmosphere arrays'//&
                            ' do not conform', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Layers(i),   & ! Input
                                       n_Absorbers,   & ! Input
                                       n_Clouds(i),   & ! Input
                                       n_Aerosols(i), & ! Input
                                       Atmosphere(i), & ! Output
                                       Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank10111





!--------------------------------------------------------------------------------
!S+
! NAME:
!       cRTM_Assign_Atmosphere
!
! PURPOSE:
!       Function to copy valid CRTM_Atmosphere structures.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Atmosphere( Atmosphere_in,            &  ! Input
!                                              Atmosphere_out,           &  ! Output
!                                              RCS_Id = RCS_Id,          &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere_in:   Atmosphere structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar OR Rank-1
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
!       Atmosphere_out:  Copy of the input structure, Atmosphere_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Same as Atmosphere_in
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
!       CRTM_Associated_Atmosphere:  Function to test the association status of the
!                                    pointer members of a CRTM Atmosphere structure.
!
!       CRTM_Destroy_Atmosphere:     Function to re-initialize CRTM_Atmosphere data
!                                    structures.
!
!       CRTM_Allocate_Atmosphere:    Function to allocate the pointer members of
!                                    the CRTM Atmosphere data structure.
!
!       CRTM_Assign_Cloud:           Function to copy valid CRTM Cloud structures.
!                                    SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_Assign_Aerosol:         Function to copy valid CRTM Aerosol structures.
!                                    SOURCE: CRTM_AEROSOL_DEFINE module
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
!       Note the INTENT on the output Atmosphere argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Mar-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Atmosphere_in,  &  ! Input
                          Atmosphere_out, &  ! Output
                          RCS_Id,         &  ! Revision control
                          Message_Log )   &  ! Error messaging
                        RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ), INTENT( IN )     :: Atmosphere_in

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT ) :: Atmosphere_out

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Atmosphere(Scalar)'



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

    ! --------------------------------------------
    ! ALL *input* pointers must be associated
    ! BUT the Cloud and/or Aerosol pointer members
    ! may not be.
    ! --------------------------------------------

    IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere_In, &
                                           Skip_Cloud   = SET, &
                                           Skip_Aerosol = SET  ) ) THEN

      Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_Out, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating output CRTM_Atmosphere pointer members.', &
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

    Error_Status = CRTM_Allocate_Atmosphere( Atmosphere_in%Max_Layers, &
                                             Atmosphere_in%n_Absorbers, &
                                             Atmosphere_in%Max_Clouds, &
                                             Atmosphere_in%Max_Aerosols, &
                                             Atmosphere_out, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating output CRTM_Atmosphere arrays.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------------
    ! Assign the used-dimension scalar members
    ! ----------------------------------------

    Atmosphere_out%n_Layers   = Atmosphere_in%n_Layers
    Atmosphere_out%n_Clouds   = Atmosphere_in%n_Clouds
    Atmosphere_out%n_Aerosols = Atmosphere_in%n_Aerosols


    ! -----------------------------------
    ! Assign non-dimension scalar members
    ! -----------------------------------

    Atmosphere_out%Level_Temperature_Input = Atmosphere_in%Level_Temperature_Input
    Atmosphere_out%Climatology             = Atmosphere_in%Climatology


    ! -----------------
    ! Assign array data
    ! -----------------

    Atmosphere_out%Absorber_ID       = Atmosphere_in%Absorber_ID
    Atmosphere_out%Absorber_Units    = Atmosphere_in%Absorber_Units
    Atmosphere_out%Level_Pressure    = Atmosphere_in%Level_Pressure
    Atmosphere_out%Level_Temperature = Atmosphere_in%Level_Temperature
    Atmosphere_out%Pressure          = Atmosphere_in%Pressure
    Atmosphere_out%Temperature       = Atmosphere_in%Temperature
    Atmosphere_out%Absorber          = Atmosphere_in%Absorber


    ! ---------------------
    ! Assign structure data
    ! ---------------------

    ! -- Copy Cloud structure
    IF ( Atmosphere_in%Max_Clouds > 0 ) THEN

      Error_Status = CRTM_Assign_Cloud( Atmosphere_in%Cloud, &
                                        Atmosphere_out%Cloud, &
                                        Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying Cloud structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

    ! -- Copy Aerosol structure
    IF ( Atmosphere_in%Max_Aerosols > 0 ) THEN

      Error_Status = CRTM_Assign_Aerosol( Atmosphere_in%Aerosol, &
                                          Atmosphere_out%Aerosol, &
                                          Message_Log = Message_Log)

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying Aerosol structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Atmosphere_in,  &  ! Input
                         Atmosphere_out, &  ! Output
                         RCS_Id,         &  ! Revision control
                         Message_Log )   &  ! Error messaging
                       RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN )     :: Atmosphere_in

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere_out

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Atmosphere(Rank-1)'


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

    n = SIZE( Atmosphere_in )

    IF ( SIZE( Atmosphere_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Atmosphere_in and Atmosphere_out arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Scalar( Atmosphere_in(i), &
                                     Atmosphere_out(i), &
                                     Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of CRTM_Atmosphere structure array." )' ) i
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
!       CRTM_WeightedSum_Atmosphere
!
! PURPOSE:
!       Function to perform a weighted sum of two valid CRTM_Atmosphere
!       structures. The weighted summation performed is:
!         A = A + w1*B + w2
!       where A and B are the CRTM_Atmosphere structures, and w1 and w2
!       are the weighting factors. Note that w2 is optional.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_WeightedSum_Atmosphere( A,                        &  ! In/Output
!                                                   B,                        &  ! Input
!                                                   w1,                       &  ! Input
!                                                   w2 = w2,                  &  ! Optional input
!                                                   RCS_Id = RCS_Id,          &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       A:               Atmosphere structure that is to be added to.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar OR Rank-1
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       B:               Atmosphere structure that is to be weighted and
!                        added to structure A.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Same as A
!                        ATTRIBUTES: INTENT( IN )
!
!       w1:              The first weighting factor used to multiply the
!                        contents of the input structure, B.
!                        UNITS:      N/A
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       w2:              The second weighting factor used to multiply the
!                        contents of the input structure, B.
!                        UNITS:      N/A
!                        TYPE:       REAL( fp_kind )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
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
!       A:               Structure containing the weight sum result,
!                          A = A + w1*B + w2
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Same as B
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
!       CRTM_Associated_Atmosphere:  Function to test the association status of the
!                                    pointer members of a CRTM Atmosphere structure.
!
!       CRTM_WeightedSum_Cloud:      Function to perform a weighted sum of two
!                                    valid CRTM_Cloud structures
!                                    SOURCE: CRTM_CLOUD_DEFINE module
!
!       CRTM_WeightedSum_Aerosol:    Function to perform a weighted sum of two
!                                    valid CRTM_Aerosol structures
!                                    SOURCE: CRTM_AEROSOL_DEFINE module
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2005
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
    TYPE( CRTM_Atmosphere_type ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Atmosphere_type ), INTENT( IN )     :: B
    REAL( fp_kind ),              INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ),    OPTIONAL, INTENT( IN )     :: w2

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Atmosphere(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: w2_Local
    INTEGER :: jA


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


    ! -------------------------
    ! Check the used dimensions
    ! -------------------------

    IF ( A%n_Layers    /= B%n_Layers    .OR. &
         A%n_Absorbers /= B%n_Absorbers .OR. &
         A%n_Clouds    /= B%n_Clouds    .OR. &
         A%n_Aerosols  /= B%n_Aerosols       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'A and B structure dimensions are different.', &
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

    ! --------------------
    ! The array components
    ! --------------------

    A%Temperature(:A%n_Layers) = A%Temperature(:A%n_Layers) + (w1*B%Temperature(:A%n_Layers)) + w2_Local


    DO jA = 1, A%n_Absorbers
      A%Absorber(:A%n_Layers,jA) = A%Absorber(:A%n_Layers,jA) + (w1*B%Absorber(:A%n_Layers,jA)) + w2_Local
    END DO

    ! -------------------
    ! The Cloud structure
    ! -------------------

    IF ( ASSOCIATED( A%Cloud ) ) THEN
      Error_Status = CRTM_WeightedSum_Cloud( A%Cloud, &
                                             B%Cloud, &
                                             w1, &
                                             w2 = w2, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Weighted sum of Cloud structure components failed.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ---------------------
    ! The Aerosol structure
    ! ---------------------

    IF ( ASSOCIATED( A%Aerosol ) ) THEN
      Error_Status = CRTM_WeightedSum_Aerosol( A%Aerosol, &
                                               B%Aerosol, &
                                               w1, &
                                               w2 = w2, &
                                               Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Weighted sum of Aerosol structure components failed.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

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
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN )     :: B
    REAL( fp_kind ),                              INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ),    OPTIONAL,                 INTENT( IN )     :: w2

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightSum_Atmosphere(Rank-1)'


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
                          &" of CRTM_Atmosphere structure arrays." )' ) i
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
!       CRTM_Zero_Atmosphere
! 
! PURPOSE:
!       Subroutine to zero-out all members of a CRTM_Atmosphere structure - both
!       scalar and pointer.
!
! CATEGORY:
!       CRTM : Atmosphere
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Atmosphere( Atmosphere )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Atmosphere:   Zeroed out Atmosphere structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
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
!         tests for pointer member association status. This means the Atmosphere
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - The Absorber_ID and Absorber_Units components are *NOT* zeroed out
!         in this routine.
!
!       - The n_Layers, n_Clouds, and n_Aerosols components are set to the value
!         of the Max_Layers, Max_Clouds, and Max_Aerosols components respectively.
!
! COMMENTS:
!       Note the INTENT on the output Atmosphere argument is IN OUT rather than
!       just OUT. This is necessary because the argument must be defined upon
!       input.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Aug-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Atmosphere )  ! Output
    TYPE( CRTM_Atmosphere_type ),  INTENT( IN OUT ) :: Atmosphere

    ! -- Reset the structure components
    IF ( Atmosphere%Max_Clouds   > 0 ) CALL CRTM_Zero_Cloud(   Atmosphere%Cloud   )
    IF ( Atmosphere%Max_Aerosols > 0 ) CALL CRTM_Zero_Aerosol( Atmosphere%Aerosol )

    ! -- Reset the multi-dimensional scalar components
!    Atmosphere%n_Layers   = Atmosphere%Max_Layers
!    Atmosphere%n_Clouds   = Atmosphere%Max_Clouds
!    Atmosphere%n_Aerosols = Atmosphere%Max_Aerosols

    ! -- Reset the array components
    Atmosphere%Level_Pressure    = ZERO
    Atmosphere%Level_Temperature = ZERO
    Atmosphere%Pressure          = ZERO
    Atmosphere%Temperature       = ZERO
    Atmosphere%Absorber          = ZERO

  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Atmosphere )  ! Output

    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN OUT ) :: Atmosphere
    INTEGER :: n

    DO n = 1, SIZE( Atmosphere )
      CALL Zero_Scalar( Atmosphere(n) )
    END DO

  END SUBROUTINE Zero_Rank1

END MODULE CRTM_Atmosphere_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_Atmosphere_Define.f90,v 1.18 2005/09/20 15:17:30 yhan Exp $
!
! $Date: 2005/09/20 15:17:30 $
!
! $Revision: 1.18 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Atmosphere_Define.f90,v $
! Revision 1.18  2005/09/20 15:17:30  yhan
! --- Lightened function WeightedSum_Scalar:
!     (1) Removed most of the checks (only dimension checks remained)
!     (2) Removed summation for pressure profiles
!     (3) Limited the layer loop from 1 to n_layers
!
! Revision 1.17  2005/09/15 18:28:54  yhan
! -- Took out the assignments for the dimension variables in the CRTM_Zero_Atmosphere
!    routine.  In most cases such as the weighted-sum for K-matrix calculations, those
!    dimensions should remain the same as before, but these assignments change the values.
!
! Revision 1.16  2005/08/17 18:00:03  paulv
! - Modified the Zero() subroutines so that the dimensional structure members
!   are not set to zero. The only change to these occurs when there is a
!   corresponding Max dimension value--in this case the dimension value is
!   set to the maximum value.
! - Modified the Zero() subroutines so that the cloud and aerosol type flags
!   are not reset to zero. They retain their value.
!
! Revision 1.15  2005/08/17 17:10:30  paulv
! - Added structure zero subroutines.
!
! Revision 1.14  2005/06/15 23:12:29  paulv
! - Added WeightedSum() functions.
!
! Revision 1.13  2005/03/28 19:11:42  paulv
! - Made YES/NO flags public.
!
! Revision 1.12  2005/03/28 16:11:35  paulv
! - Declared public parameters from the CRTM_Cloud_Define and CRTM_Aerosol_Define
!   modules public in this module.
! - Increased the default maximum number of aerosol modes from 2 to 4.
! - Changed the dimensions of the Level_Pressure structure member from
!   (1:n_Layers) to (0:n_Layers). Previously the TOA level, level 0, was
!   assumed to be 0.005hPa. The user is now required to input this value.
! - Added the Level_Temperature array component and the Level_Temperature_Input
!   flag component. The latter is used to determine if the former has been
!   supplied by the user. The default is to assume it has been supplied.
! - Updated header documentation.
!
! Revision 1.11  2005/02/25 17:39:12  paulv
! - Added CRTM_Aerosol_Define USE statement and made the CRTM_Aerosol_type and
!   routines PUBLIC.
! - Added Allocate_Rank00111() specific routine.
! - Corrected bug in Allocate_Rank10111() function name at END statement.
!
! Revision 1.10  2005/02/24 18:54:41  paulv
! - Added Aerosol component to structure.
! - Association, destruction, allocation and assignment function have been
!   modified to reflect the addition of the the Aerosol component.
! - The Allocate() function went throught the largest change. There are
!   now additional specific functions to accomodate the varying dimensionality
!   of the inputs since there is now an extra dimension (n_Aerosols) to take
!   into account.
!
! Revision 1.9  2005/02/16 15:38:44  paulv
! - Allocation of the Cloud structure is now performed in the CRTM_Allocate_Atmosphere()
!   function. The CLoud structure is allocated to the same number of layers as
!   the other atmosphere data (e.g. pressure, temperature etc). This is to
!   make the use of the Cloud structure in the RTSolution code simpler by assuming
!   there is cloud data at every level. If the Cloud data value(s) is(are) below some
!   threshold, then that portion of the atmosphere is considered "clear" in the
!   RTSolution.
!
! Revision 1.8  2005/02/11 21:49:02  paulv
! - Updated documentation.
!
! Revision 1.7  2005/02/01 16:00:16  paulv
! - Replaced initialisation of pointer members in the Allocate() function from
!   an "invalid" value to zero.
!
! Revision 1.6  2005/01/28 21:22:08  paulv
! - Added Max_Layers structure component. This allows the structure to be
!   allocated to a number of levels (Max_Layers) greater than the number of
!   layers used (n_Layers) at any one time.
! - Renamed all the specific function to shorter names.
!
! Revision 1.5  2004/11/03 21:26:33  paulv
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
! - Added specific interfaces for multiple structure input Destroy() functions.
! - Altered the way the Assign() function handles unassociated input. Previously
!   an error was issued:
!     IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere_In, Skip_Cloud = SET ) ) THEN
!       Error_Status = FAILURE
!       RETURN
!     END IF
!   Now, rather than returning an error, the output structure is destroyed
!   (in case it is defined upon input), and a successful status is returned,
!     IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere_In, Skip_Cloud = SET ) ) THEN
!       Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_Out, &
!                                               Message_Log = Message_Log )
!       RETURN
!     END IF
!
! Revision 1.4  2004/09/24 17:32:50  paulv
! - Converted code to Fortran-95. Derived type initialisation now done in
!   definition.
!
! Revision 1.3  2004/08/06 16:41:06  paulv
! - Updated header documentation.
!
! Revision 1.2  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.1  2004/05/19 19:55:11  paulv
! Initial checkin.
!
!
!
!
