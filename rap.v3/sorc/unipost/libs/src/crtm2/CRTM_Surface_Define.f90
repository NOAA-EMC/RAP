!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Surface_Define
!
! PURPOSE:
!       Module defining the CRTM Surface data structure and containing routines
!       to manipulate it.
!       
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Surface_Define
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds
!                                of variable types.
!
!       Error_Handler:           Module to define simple error codes and
!                                handle error conditions
!                                USEs: FILE_UTILITY module
!
!       CRTM_SensorData_Define:  Module defining the SensorData data structure
!                                and containing routines to manipulate it.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Destroy_Surface:        Function to re-initialize a Surface
!                                      structure.
!
!         CRTM_Allocate_Surface:       Function to allocate CRTM_Surface data
!                                      structures.
!                                      NOTE: This function is wrapper for the
!                                            CRTM_SensorData allocation routine
!                                            to provide the functionality and
!                                            convenience for allocation of both
!                                            scalar and rank-1 Surface structures
!                                            in the same manner as for
!                                            CRTM_Atmosphere_type structures.
!
!         CRTM_Assign_Surface:         Function to copy a Surface structure.
!
!         CRTM_WeightedSum_Surface:    Function to perform a weighted sum of two
!                                      CRTM_Surface structures.
!
!         CRTM_Zero_Surface:           Subroutine to zero-out members of a 
!                                      CRTM_Surface structure.
!
!       PRIVATE subprograms
!       -------------------
!         CRTM_Clear_Surface:          Subroutine to clear the scalar members of
!                                      a CRTM Surface structure.
!
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Associated_SensorData:    Function to test the association status
!                                      of the pointer members of a SensorData
!                                      structure.
!                                      SOURCE: CRTM_SENSORDATA_DEFINE module
!
!       CRTM_Destroy_SensorData:       Function to re-initialize an SensorData
!                                      structure.
!                                      SOURCE: CRTM_SENSORDATA_DEFINE module
!
!       CRTM_Allocate_SensorData:      Function to allocate the pointer members
!                                      of a SensorData structure.
!                                      SOURCE: CRTM_SENSORDATA_DEFINE module
!
!       CRTM_Assign_SensorData:        Function to copy an SensorData structure.
!                                      SOURCE: CRTM_SENSORDATA_DEFINE module
!
!
! DERIVED TYPES:
!       CRTM_Surface_type
!       -----------------
!         Definition of the public CRTM_Surface data structure.
!         Fields are,
!
!         Land_Coverage:           Fraction of surface that is of the 
!                                  land surface type.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Water_Coverage:          Fraction of surface that is of the 
!                                  water surface type.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Snow_Coverage:           Fraction of surface that is of the 
!                                  snow surface type.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Ice_Coverage:            Fraction of surface that is of the 
!                                  ice surface type.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Wind_Speed:              Surface wind speed.
!                                  UNITS:      m.s^-1
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Land_Type:               The land surface type. See PUBLIC PARAMETERS
!                                  for the valid types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Land_Temperature:        The land surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Soil_Moisture_Content:   The volumetric water content of the soil.
!                                  UNITS:      g.cm^-3
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Canopy_Water_Content:    The gravimetric water content of the canopy
!                                  UNITS:      g.cm^-3
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Vegetation_Fraction:     The vegetation fraction of the surface.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Soil_Temperature:        The soil temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Water_Type:              The water surface type. See PUBLIC PARAMETERS
!                                  for the valid types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Water_Temperature:       The water surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Wind_Direction:          Surface wind direction. Measured in degrees
!                                  east from north.
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Salinity:                Water salinity.
!                                  UNITS:      ppmv
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Snow_Type:               The snow surface type. See PUBLIC PARAMETERS
!                                  for the valid types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Snow_Temperature:        The snow surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Snow_Depth:              The snow depth.
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Snow_Density:            The snow density
!                                  UNITS:      g.cm^-3
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Snow_Grain_Size:         The snow grain size.
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Ice_Type:                The ice surface type. See PUBLIC PARAMETERS
!                                  for the valid types.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
!         Ice_Temperature:         The ice surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Ice_Thickness:           The thickness of the ice.
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Ice_Density:             The ice density
!                                  UNITS:      g.cm^-3
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Ice_Roughness:           Measure of the surface roughness of the ice
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         SensorData:              Satellite sensor data required for some
!                                  surface algorithms. Can be left empty.
!                                  UNITS:      N/A
!                                  TYPE:       TYPE( CRTM_SensorData_type )
!                                  DIMENSION:  Scalar
!
! PUBLIC PARAMETERS:
!       1) The type of land surface using in the Surface%Land_Type field:
!
!                  Land type                Parameter Name
!         --------------------------------------------------------
!                   Invalid                 INVALID_LAND            
!                Compacted soil             COMPACTED_SOIL          
!                  Tilled soil              TILLED_SOIL             
!                    Sand                   SAND                      
!                    Rock                   ROCK                      
!           Irrigated low vegetation        IRRIGATED_LOW_VEGETATION
!                 Meadow grass              MEADOW_GRASS            
!                    Scrub                  SCRUB                     
!               Broadleaf forest            BROADLEAF_FOREST        
!                 Pine forest               PINE_FOREST             
!                   Tundra                  TUNDRA                   
!                 Grass soil                GRASS_SOIL              
!             Broadleaf-pine forest         BROADLEAF_PINE_FOREST   
!                 Grass scrub               GRASS_SCRUB             
!                  Oil grass                OIL_GRASS               
!                Urban concrete             URBAN_CONCRETE          
!                  Pine brush               PINE_BRUSH              
!                Broadleaf brush            BROADLEAF_BRUSH         
!                   Wet soil                WET_SOIL                 
!                  Scrub soil               SCRUB_SOIL              
!            Broadleaf(70)-Pine(30)         BROADLEAF70_PINE30
!
!
!
!       2) The type of water surface using in the Surface%Water_Type field:
!
!           Water type        Parameter Name
!         ------------------------------------
!             Invalid         INVALID_WATER 
!            Sea water        SEA_WATER     
!           Fresh water       FRESH_WATER   
!
!
!
!       3) The type of snow surface using in the Surface%Snow_Type field:
!
!               Snow type            Parameter Name
!         -----------------------------------------------
!
!                Invalid             INVALID_SNOW       
!               Wet snow             WET_SNOW           
!            Grass after snow        GRASS_AFTER_SNOW   
!              Powder snow           POWDER_SNOW        
!               RS snow(A)           RS_SNOW_A          
!               RS snow(B)           RS_SNOW_B          
!               RS snow(C)           RS_SNOW_C          
!               RS snow(D)           RS_SNOW_D          
!               RS snow(E)           RS_SNOW_E          
!            Thin Crust snow         THIN_CRUST_SNOW    
!            Thick crust snow        THICK_CRUST_SNOW   
!              Shallow snow          SHALLOW_SNOW       
!               Deep snow            DEEP_SNOW          
!              Crust snow            CRUST_SNOW         
!              Medium snow           MEDIUM_SNOW        
!           Bottom crust snow(A)     BOTTOM_CRUST_SNOW_A
!           Bottom crust snow(B)     BOTTOM_CRUST_SNOW_B
!
!
!
!       4) The type of ice surface using in the Surface%Ice_Type field:
!
!                  Ice type           Parameter Name
!         ------------------------------------------------
!                  Invalid            INVALID_ICE       
!                 Fresh ice           FRESH_ICE         
!             First year sea ice      FIRST_YEAR_SEA_ICE
!           Multiple year sea ice     MULTI_YEAR_SEA_ICE
!                 Ice floe            ICE_FLOE            
!                 Ice ridge           ICE_RIDGE           
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
!       Written by:  Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                    Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                    Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                    07-May-2004
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

MODULE CRTM_Surface_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Error_Handler

  USE CRTM_SensorData_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_SensorData structure routiens inherited
  ! -- from the CRTM_SensorData_Define module
  PUBLIC :: CRTM_Associated_SensorData
  PUBLIC :: CRTM_Destroy_SensorData
  PUBLIC :: CRTM_Allocate_SensorData
  PUBLIC :: CRTM_Assign_SensorData

  ! -- CRTM_Surface routines in this module
  PUBLIC :: CRTM_Destroy_Surface
  PUBLIC :: CRTM_Allocate_Surface
  PUBLIC :: CRTM_Assign_Surface
  PUBLIC :: CRTM_WeightedSum_Surface
  PUBLIC :: CRTM_Zero_Surface


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Destroy_Surface
    MODULE PROCEDURE Destroy_Scalar
    MODULE PROCEDURE Destroy_Rank1
    MODULE PROCEDURE Destroy_Scalar_Multi
    MODULE PROCEDURE Destroy_Rank1_Multi
  END INTERFACE CRTM_Destroy_Surface

  INTERFACE CRTM_Allocate_Surface
    MODULE PROCEDURE Allocate_Scalar
    MODULE PROCEDURE Allocate_Rank01
    MODULE PROCEDURE Allocate_Rank11
  END INTERFACE CRTM_Allocate_Surface

  INTERFACE CRTM_Assign_Surface
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_Surface

  INTERFACE CRTM_WeightedSum_Surface
    MODULE PROCEDURE WeightedSum_Scalar
    MODULE PROCEDURE WeightedSum_Rank1
  END INTERFACE CRTM_WeightedSum_Surface

  INTERFACE CRTM_Zero_Surface
    MODULE PROCEDURE Zero_Scalar
    MODULE PROCEDURE Zero_Rank1
  END INTERFACE CRTM_Zero_Surface


  ! ------------------------
  ! PUBLIC Module parameters
  ! ------------------------

  ! -- The gross surface types. These are used for
  ! -- cross-checking with the coverage fractions
  ! -- of each gross surface types.
  INTEGER, PUBLIC, PARAMETER :: INVALID_SURFACE = 0
  INTEGER, PUBLIC, PARAMETER :: LAND_SURFACE    = 1
  INTEGER, PUBLIC, PARAMETER :: WATER_SURFACE   = 2
  INTEGER, PUBLIC, PARAMETER :: SNOW_SURFACE    = 4
  INTEGER, PUBLIC, PARAMETER :: ICE_SURFACE     = 8

  INTEGER, PUBLIC, PARAMETER :: N_VALID_SURFACE_TYPES = LAND_SURFACE + &
                                                        WATER_SURFACE + &
                                                        SNOW_SURFACE + &
                                                        ICE_SURFACE

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_SURFACE_TYPES ) :: &
    SURFACE_TYPE_NAME = (/ 'Invalid surface type     ', &  ! 0
                           'Land                     ', &  ! 1
                           'Water                    ', &  ! 2
                           'Land + water             ', &  ! 3
                           'Snow                     ', &  ! 4
                           'Land + snow              ', &  ! 5
                           'Water + snow             ', &  ! 6
                           'Land + water + snow      ', &  ! 7
                           'Ice                      ', &  ! 8
                           'Land + ice               ', &  ! 9
                           'Water + ice              ', &  ! 10
                           'Land + water + ice       ', &  ! 11
                           'Snow + ice               ', &  ! 12
                           'Land + snow + ice        ', &  ! 13
                           'Water + snow + ice       ', &  ! 14
                           'Land + water + snow + ice' /)  ! 15


  ! -- For land, the land types
  INTEGER, PUBLIC, PARAMETER :: N_VALID_LAND_TYPES = 20

  INTEGER, PUBLIC, PARAMETER :: INVALID_LAND             =  0
  INTEGER, PUBLIC, PARAMETER :: COMPACTED_SOIL           =  1
  INTEGER, PUBLIC, PARAMETER :: TILLED_SOIL              =  2
  INTEGER, PUBLIC, PARAMETER :: SAND                     =  3
  INTEGER, PUBLIC, PARAMETER :: ROCK                     =  4
  INTEGER, PUBLIC, PARAMETER :: IRRIGATED_LOW_VEGETATION =  5
  INTEGER, PUBLIC, PARAMETER :: MEADOW_GRASS             =  6
  INTEGER, PUBLIC, PARAMETER :: SCRUB                    =  7
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_FOREST         =  8
  INTEGER, PUBLIC, PARAMETER :: PINE_FOREST              =  9
  INTEGER, PUBLIC, PARAMETER :: TUNDRA                   = 10
  INTEGER, PUBLIC, PARAMETER :: GRASS_SOIL               = 11  ! Default value
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_PINE_FOREST    = 12
  INTEGER, PUBLIC, PARAMETER :: GRASS_SCRUB              = 13
  INTEGER, PUBLIC, PARAMETER :: OIL_GRASS                = 14
  INTEGER, PUBLIC, PARAMETER :: URBAN_CONCRETE           = 15
  INTEGER, PUBLIC, PARAMETER :: PINE_BRUSH               = 16
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF_BRUSH          = 17
  INTEGER, PUBLIC, PARAMETER :: WET_SOIL                 = 18
  INTEGER, PUBLIC, PARAMETER :: SCRUB_SOIL               = 19
  INTEGER, PUBLIC, PARAMETER :: BROADLEAF70_PINE30       = 20
  
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_LAND_TYPES ) :: &
    LAND_TYPE_NAME = (/ 'Invalid land surface type', &
                        'Compacted soil           ', &
                        'Tilled soil              ', &
                        'Sand                     ', &
                        'Rock                     ', &
                        'Irrigated low vegetation ', &
                        'Meadow grass             ', &
                        'Scrub                    ', &
                        'Broadleaf forest         ', &
                        'Pine forest              ', &
                        'Tundra                   ', &
                        'Grass soil               ', &
                        'Broadleaf-pine forest    ', &
                        'Grass scrub              ', &
                        'Oil grass                ', &
                        'Urban concrete           ', &
                        'Pine brush               ', &
                        'Broadleaf brush          ', &
                        'Wet soil                 ', &
                        'Scrub soil               ', &
                        'Broadleaf(70)-Pine(30)   ' /)


  ! -- For water, the water types
  INTEGER, PUBLIC, PARAMETER :: N_VALID_WATER_TYPES = 2

  INTEGER, PUBLIC, PARAMETER :: INVALID_WATER  =  0
  INTEGER, PUBLIC, PARAMETER :: SEA_WATER      =  1  ! Default value
  INTEGER, PUBLIC, PARAMETER :: FRESH_WATER    =  2
  
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_WATER_TYPES ) :: &
    WATER_TYPE_NAME = (/ 'Invalid water surface type', &
                         'Sea water                 ', &
                         'Fresh water               ' /)


  ! -- For snow, the snow types.
  INTEGER, PUBLIC, PARAMETER :: N_VALID_SNOW_TYPES = 16

  INTEGER, PUBLIC, PARAMETER :: INVALID_SNOW        =  0
  INTEGER, PUBLIC, PARAMETER :: WET_SNOW            =  1
  INTEGER, PUBLIC, PARAMETER :: GRASS_AFTER_SNOW    =  2
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_A           =  3
  INTEGER, PUBLIC, PARAMETER :: POWDER_SNOW         =  4
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_B           =  5
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_C           =  6
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_D           =  7
  INTEGER, PUBLIC, PARAMETER :: THIN_CRUST_SNOW     =  8
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_E           =  9
  INTEGER, PUBLIC, PARAMETER :: BOTTOM_CRUST_SNOW_A = 10
  INTEGER, PUBLIC, PARAMETER :: SHALLOW_SNOW        = 11
  INTEGER, PUBLIC, PARAMETER :: DEEP_SNOW           = 12
  INTEGER, PUBLIC, PARAMETER :: CRUST_SNOW          = 13
  INTEGER, PUBLIC, PARAMETER :: MEDIUM_SNOW         = 14
  INTEGER, PUBLIC, PARAMETER :: BOTTOM_CRUST_SNOW_B = 15
  INTEGER, PUBLIC, PARAMETER :: THICK_CRUST_SNOW    = 16

  INTEGER, PUBLIC, PARAMETER :: NEW_SNOW         =  POWDER_SNOW  ! Default value
  INTEGER, PUBLIC, PARAMETER :: OLD_SNOW         =  THICK_CRUST_SNOW
  


  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_SNOW_TYPES ) :: &
    SNOW_TYPE_NAME = (/ 'Invalid snow surface type     ', &
                        'Wet snow                      ', &
                        'Grass after snow              ', &
                        'RS snow(A)                    ', &
                        'Powder snow (or new snow)     ', &
                        'RS snow(B)                    ', &
                        'RS snow(C)                    ', &
                        'RS snow(D)                    ', &
                        'Thin Crust snow               ', &
                        'RS snow(E)                    ', &
                        'Bottom crust snow(A)          ', &
                        'Shallow snow                  ', &
                        'Deep snow                     ', &
                        'Crust snow                    ', &
                        'Medium snow                   ', &
                        'Bottom crust snow(B)          ', &
                        'Thick crust snow (or old snow)' /)


  ! -- For ice, the ice types.
  INTEGER, PUBLIC, PARAMETER :: N_VALID_ICE_TYPES = 5

  INTEGER, PUBLIC, PARAMETER :: INVALID_ICE        =  0
  INTEGER, PUBLIC, PARAMETER :: FRESH_ICE          =  1  ! Default value
  INTEGER, PUBLIC, PARAMETER :: FIRST_YEAR_SEA_ICE =  2
  INTEGER, PUBLIC, PARAMETER :: MULTI_YEAR_SEA_ICE =  3
  INTEGER, PUBLIC, PARAMETER :: ICE_FLOE           =  4
  INTEGER, PUBLIC, PARAMETER :: ICE_RIDGE          =  5

  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( 0:N_VALID_ICE_TYPES ) :: &
     ICE_TYPE_NAME = (/ 'Invalid ice surface type ', &
                        'Fresh ice                ', &
                        'First year sea ice       ', &
                        'Multiple year sea ice    ', &
                        'Ice floe                 ', &
                        'Ice ridge                '/)


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_Surface_Define.f90,v 1.15 2005/10/19 18:40:33 paulv Exp $'

  ! -- Surface scalar member invalid values
  INTEGER, PRIVATE, PARAMETER ::    INVALID = -1
  INTEGER, PRIVATE, PARAMETER :: FP_INVALID = -1.0_fp_kind

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind


  ! ------------------------
  ! Default value parameters
  ! ------------------------

  ! -- Surface type independent data
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_WIND_SPEED = 5.0_fp_kind  ! m/s

  ! -- Land surface type data
  INTEGER,         PRIVATE, PARAMETER :: DEFAULT_LAND_TYPE             = GRASS_SOIL
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_LAND_TEMPERATURE      = 283.0_fp_kind  ! K
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SOIL_MOISTURE_CONTENT = 0.05_fp_kind   ! g/cm^3
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_CANOPY_WATER_CONTENT  = 0.05_fp_kind   ! g/cm^3
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_VEGETATION_FRACTION   = 0.3_fp_kind    ! 30%
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SOIL_TEMPERATURE      = 283.0_fp_kind  ! K

  ! -- Water type data
  INTEGER,         PRIVATE, PARAMETER :: DEFAULT_WATER_TYPE        = SEA_WATER
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_WATER_TEMPERATURE = 283.0_fp_kind   ! K
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_WIND_DIRECTION    = 0.0_fp_kind     ! North
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SALINITY          = 33.0_fp_kind    ! ppmv

  ! -- Snow surface type data
  INTEGER,         PRIVATE, PARAMETER :: DEFAULT_SNOW_TYPE        = NEW_SNOW
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SNOW_TEMPERATURE = 263.0_fp_kind   ! K
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SNOW_DEPTH       = 50.0_fp_kind    ! mm
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SNOW_DENSITY     = 0.2_fp_kind     ! g/cm^3
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_SNOW_GRAIN_SIZE  = 2.0_fp_kind     ! mm

  ! -- Ice surface type data
  INTEGER,         PRIVATE, PARAMETER :: DEFAULT_ICE_TYPE        = FRESH_ICE
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_ICE_TEMPERATURE = 263.0_fp_kind  ! K
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_ICE_THICKNESS   = 10.0_fp_kind   ! mm
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_ICE_DENSITY     = 0.9_fp_kind    ! g/cm^3
  REAL( fp_kind ), PRIVATE, PARAMETER :: DEFAULT_ICE_ROUGHNESS   = ZERO


  ! ----------------------------
  ! Surface data type definition
  ! ----------------------------

  TYPE, PUBLIC :: CRTM_Surface_type
    INTEGER :: n_Allocates = 0

    ! -- Dimension values
    INTEGER :: Max_Sensors  = 0  ! N dimension
    INTEGER :: n_Sensors    = 0  ! Nuse dimension


    ! -- Gross type of surface determined by coverage
    REAL( fp_kind ) :: Land_Coverage  = ZERO
    REAL( fp_kind ) :: Water_Coverage = ZERO
    REAL( fp_kind ) :: Snow_Coverage  = ZERO
    REAL( fp_kind ) :: Ice_Coverage   = ZERO

    ! -- Surface type independent data
    REAL( fp_kind ) :: Wind_Speed = DEFAULT_WIND_SPEED

    ! -- Land surface type data
    INTEGER         :: Land_Type             = DEFAULT_LAND_TYPE
    REAL( fp_kind ) :: Land_Temperature      = DEFAULT_LAND_TEMPERATURE
    REAL( fp_kind ) :: Soil_Moisture_Content = DEFAULT_SOIL_MOISTURE_CONTENT
    REAL( fp_kind ) :: Canopy_Water_Content  = DEFAULT_CANOPY_WATER_CONTENT
    REAL( fp_kind ) :: Vegetation_Fraction   = DEFAULT_VEGETATION_FRACTION
    REAL( fp_kind ) :: Soil_Temperature      = DEFAULT_SOIL_TEMPERATURE

    ! -- Water type data
    INTEGER         :: Water_Type        = DEFAULT_WATER_TYPE
    REAL( fp_kind ) :: Water_Temperature = DEFAULT_WATER_TEMPERATURE
    REAL( fp_kind ) :: Wind_Direction    = DEFAULT_WIND_DIRECTION
    REAL( fp_kind ) :: Salinity          = DEFAULT_SALINITY

    ! -- Snow surface type data
    INTEGER         :: Snow_Type        = DEFAULT_SNOW_TYPE
    REAL( fp_kind ) :: Snow_Temperature = DEFAULT_SNOW_TEMPERATURE
    REAL( fp_kind ) :: Snow_Depth       = DEFAULT_SNOW_DEPTH
    REAL( fp_kind ) :: Snow_Density     = DEFAULT_SNOW_DENSITY
    REAL( fp_kind ) :: Snow_Grain_Size  = DEFAULT_SNOW_GRAIN_SIZE

    ! -- Ice surface type data
    INTEGER         :: Ice_Type        = DEFAULT_ICE_TYPE
    REAL( fp_kind ) :: Ice_Temperature = DEFAULT_ICE_TEMPERATURE
    REAL( fp_kind ) :: Ice_Thickness   = DEFAULT_ICE_THICKNESS
    REAL( fp_kind ) :: Ice_Density     = DEFAULT_ICE_DENSITY
    REAL( fp_kind ) :: Ice_Roughness   = DEFAULT_ICE_ROUGHNESS

    ! -- SensorData containing channel brightness temperatures
    TYPE( CRTM_SensorData_Type ) :: SensorData  ! N

  END TYPE CRTM_Surface_type


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
!       CRTM_Clear_Surface
!
! PURPOSE:
!       Subroutine to clear the scalar members of a CRTM Surface structure to
!       their default values.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_Surface( Surface ) ! Output
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Surface:     Surface structure for which the scalar members have
!                    been cleared.
!                    UNITS:      N/A
!                    TYPE:       CRTM_Surface_type
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
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-May-2004
!                       paul.vandelst@ssec.wisc.edu
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_Surface( Surface )

    TYPE( CRTM_Surface_type ), INTENT( IN OUT ) :: Surface

    ! -- Gross surface type
    Surface%Land_Coverage  = ZERO
    Surface%Water_Coverage = ZERO
    Surface%Snow_Coverage  = ZERO
    Surface%Ice_Coverage   = ZERO

    ! -- Surface type independent data
    Surface%Wind_Speed = DEFAULT_WIND_SPEED

    ! -- Land surface type data
    Surface%Land_Type             = DEFAULT_LAND_TYPE
    Surface%Land_Temperature      = DEFAULT_LAND_TEMPERATURE
    Surface%Soil_Moisture_Content = DEFAULT_SOIL_MOISTURE_CONTENT
    Surface%Canopy_Water_Content  = DEFAULT_CANOPY_WATER_CONTENT
    Surface%Vegetation_Fraction   = DEFAULT_VEGETATION_FRACTION
    Surface%Soil_Temperature      = DEFAULT_SOIL_TEMPERATURE

    ! -- Water surface type data
    Surface%Water_Type        = DEFAULT_WATER_TYPE
    Surface%Water_Temperature = DEFAULT_WATER_TEMPERATURE
    Surface%Wind_Direction    = DEFAULT_WIND_DIRECTION
    Surface%Salinity          = DEFAULT_SALINITY

    ! -- Snow surface type data
    Surface%Snow_Type        = DEFAULT_SNOW_TYPE
    Surface%Snow_Temperature = DEFAULT_SNOW_TEMPERATURE
    Surface%Snow_Depth       = DEFAULT_SNOW_DEPTH
    Surface%Snow_Density     = DEFAULT_SNOW_DENSITY
    Surface%Snow_Grain_Size  = DEFAULT_SNOW_GRAIN_SIZE

    ! -- Snow surface type data
    Surface%Ice_Type        = DEFAULT_ICE_TYPE
    Surface%Ice_Temperature = DEFAULT_ICE_TEMPERATURE
    Surface%Ice_Thickness   = DEFAULT_ICE_THICKNESS
    Surface%Ice_Density     = DEFAULT_ICE_DENSITY
    Surface%Ice_Roughness   = DEFAULT_ICE_ROUGHNESS

  END SUBROUTINE CRTM_Clear_Surface





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
!       CRTM_Destroy_Surface
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of Surface
!       data structures.
!
!       NOTE: This function is mostly a wrapper for the CRTM_SensorData
!             destruction routine to provide the functionality and convenience
!             of allocation of both scalar and rank-1 Surface structures in
!             the same manner as for CRTM_Atmosphere_type structures.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Destroy_Surface( Surface1, [ Surface2, ..., Surface10, ] &  ! Output
!                                            RCS_Id      = RCS_Id,                   &  ! Revision control
!                                            Message_Log = Message_Log               )  ! Error messaging
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
!       Surface1, [ Surface2, ..., Surface10 ]:
!                     Re-initialized Surface structure(s). At least one
!                     structure or structure array must be specified, and
!                     no more than 10 structures or structure arrays must
!                     be specified.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
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
!       CRTM_Clear_Surface:       Subroutine to clear the scalar members of a
!                                 CRTM Surface structure.
!
!       CRTM_Destroy_SensorData:  Function to re-initialize the scalar and
!                                 pointer members of SensorData data structures.
!                                 SOURCE: CRTM_SENSORDATA_DEFINE module
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-May-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Scalar( Surface,      &  ! Output
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
    TYPE( CRTM_Surface_type ), INTENT( IN OUT ) :: Surface

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Surface(Scalar)'


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

    IF ( Clear ) CALL CRTM_Clear_Surface( Surface )


    ! ------------------------------------------
    ! Destroy the SensorData structure component
    ! ------------------------------------------

    Error_Status = CRTM_Destroy_SensorData( Surface%SensorData, &
                                            No_Clear = No_Clear, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error destroying CRTM_Surface SensorData structure.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_Scalar

  FUNCTION Destroy_Scalar_Multi( Surface1,     &  ! Output
                                 Surface2,     &  ! Output
                                 Surface3,     &  ! Optional Output
                                 Surface4,     &  ! Optional Output
                                 Surface5,     &  ! Optional Output
                                 Surface6,     &  ! Optional Output
                                 Surface7,     &  ! Optional Output
                                 Surface8,     &  ! Optional Output
                                 Surface9,     &  ! Optional Output
                                 Surface10,    &  ! Optional Output
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
    TYPE( CRTM_Surface_type ),           INTENT( IN OUT ) :: Surface1
    TYPE( CRTM_Surface_type ),           INTENT( IN OUT ) :: Surface2

    ! -- Optional output
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface3
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface4
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface5
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface6
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface7
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface8
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface9
    TYPE( CRTM_Surface_type ), OPTIONAL, INTENT( IN OUT ) :: Surface10

    ! -- Optional input
    INTEGER,                   OPTIONAL, INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Surface(Scalar,Multi)'


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

    Destroy_Status = Destroy_Scalar( Surface1, &
                                     No_Clear = No_Clear, &
                                     Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying first Surface structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    Destroy_Status = Destroy_Scalar( Surface2, &
                                     No_Clear = No_Clear, &
                                     Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying second Surface structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! The optional arguments
    ! ----------------------

    IF ( PRESENT( Surface3 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface3, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying third Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface4 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface4, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fourth Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface5 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface5, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fifth Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface6 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface6, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying sixth Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface7 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface7, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying seventh Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface8 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface8, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying eighth Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface9 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface9, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ninth Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface10 ) ) THEN
      Destroy_Status = Destroy_Scalar( Surface10, &
                                       No_Clear = No_Clear, &
                                       Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying tenth Surface structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF

  END FUNCTION Destroy_Scalar_Multi

  FUNCTION Destroy_Rank1( Surface,      &  ! Output
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
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: Surface

    ! -- Optional input
    INTEGER,                   OPTIONAL,       INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Surface(Rank-1)'


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

    DO n = 1, SIZE( Surface )

      Scalar_Status = Destroy_Scalar( Surface(n), &
                                              No_Clear = No_Clear, &
                                              Message_Log = Message_Log )

      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error destroying element #", i5, &
                          &" of Surface structure array." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Destroy_Rank1

  FUNCTION Destroy_Rank1_Multi( Surface1,     &  ! Output
                                Surface2,     &  ! Output
                                Surface3,     &  ! Optional Output
                                Surface4,     &  ! Optional Output
                                Surface5,     &  ! Optional Output
                                Surface6,     &  ! Optional Output
                                Surface7,     &  ! Optional Output
                                Surface8,     &  ! Optional Output
                                Surface9,     &  ! Optional Output
                                Surface10,    &  ! Optional Output
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
    TYPE( CRTM_Surface_type ),           DIMENSION( : ), INTENT( IN OUT ) :: Surface1
    TYPE( CRTM_Surface_type ),           DIMENSION( : ), INTENT( IN OUT ) :: Surface2

    ! -- Optional output
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface3
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface4
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface5
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface6
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface7
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface8
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface9
    TYPE( CRTM_Surface_type ), OPTIONAL, DIMENSION( : ), INTENT( IN OUT ) :: Surface10

    ! -- Optional input
    INTEGER,                   OPTIONAL,                 INTENT( IN )     :: No_Clear

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,                 INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,                 INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Destroy_Surface(Rank-1,Multi)'


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

    Destroy_Status = Destroy_Rank1( Surface1, &
                                    No_Clear = No_Clear, &
                                    Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying first Surface structure array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    Destroy_Status = Destroy_Rank1( Surface2, &
                                    No_Clear = No_Clear, &
                                    Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Error_Status = Destroy_Status
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying second Surface structure array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! The optional arguments
    ! ----------------------

    IF ( PRESENT( Surface3 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface3, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying third Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface4 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface4, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fourth Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface5 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface5, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying fifth Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface6 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface6, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying sixth Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface7 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface7, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying seventh Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface8 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface8, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying eighth Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface9 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface9, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying ninth Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF


    IF ( PRESENT( Surface10 ) ) THEN
      Destroy_Status = Destroy_Rank1( Surface10, &
                                      No_Clear = No_Clear, &
                                      Message_Log = Message_Log )
      IF ( Destroy_Status /= SUCCESS ) THEN
        Error_Status = Destroy_Status
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying tenth Surface structure array.', &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF

  END FUNCTION Destroy_Rank1_Multi





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Allocate_Surface
! 
! PURPOSE:
!       Function to allocate CRTM_Surface data structures.
!
!       NOTE: This function is wrapper for the CRTM_SensorData allocation 
!             routine to provide the functionality and convenience for
!             allocation of both scalar and rank-1 Surface structures in
!             the same manner as for CRTM_Atmosphere_type structures.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Allocate_Surface( n_Channels,               &  ! Input
!                                             Surface,                  &  ! Output
!                                             RCS_Id      = RCS_Id,     &  ! Revision control
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:   Number of channels dimension of Surface%SensorData
!                     structure
!                     ** Note: Can be = 0 (i.e. no sensor data). **
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar OR Rank-1
!                                 See output Surface dimensionality table
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
!       Surface:      Surface structure with allocated SensorData pointer
!                     members. The following table shows the allowable dimension
!                     combinations for the calling routine, where M == number of
!                     profiles/surface locations:
!
!                        Input       Output
!                      n_Channels   Surface
!                       dimension   dimension
!                     --------------------------
!                        scalar      scalar
!                        scalar        M
!                          M           M
!
!                     These multiple interfaces are supplied purely for ease of
!                     use depending on what data is available.
!                     
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
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
!       CRTM_Allocate_SensorData:   Function to allocate the pointer members of a
!                                   CRTM SensorData data structure.
!                                   SOURCE: CRTM_SENSORDATA_DEFINE module
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
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Scalar( n_Channels,   &  ! Input
                            Surface,      &  ! Output
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
    INTEGER,                   INTENT( IN )     :: n_Channels   

    ! -- Output
    TYPE( CRTM_Surface_type ), INTENT( IN OUT ) :: Surface

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Surface(Scalar)'


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

    ! -- Number of channels. Can be == 0.
    IF ( n_Channels < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels must be > or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! The SensorData structure
    ! ------------------------

    IF ( n_Channels > 0 ) THEN

      Error_Status = CRTM_Allocate_SensorData( n_Channels,         &
                                               Surface%SensorData, &
                                               Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error allocating CRTM_SensorData structure.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

  END FUNCTION Allocate_Scalar

  FUNCTION Allocate_Rank01( n_Channels,   &  ! Input
                            Surface,      &  ! Output
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
    INTEGER,                                   INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: Surface

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Surface(Rank-01)'


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
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, SIZE( Surface )

      Scalar_Status = Allocate_Scalar( n_Channels, &
                                       Surface(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Surface structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank01


  FUNCTION Allocate_Rank11( n_Channels,   &  ! Input
                            Surface,      &  ! Output
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
    INTEGER,                   DIMENSION( : ), INTENT( IN )     :: n_Channels

    ! -- Output
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: Surface

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Allocate_Surface(Rank-11)'


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

    n = SIZE( n_Channels )

    IF ( SIZE( Surface ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Channels and CRTM_Surface arrays have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Allocate_Scalar( n_Channels(i), &
                                       Surface(i), &
                                       Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error allocating element #", i5, &
                          &" of CRTM_Surface structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END DO

  END FUNCTION Allocate_Rank11





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Assign_Surface
!
! PURPOSE:
!       Function to copy valid Surface structures.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_Surface( Surface_in,  &  ! Input
!                                           Surface_out, &  ! Output
!                                           RCS_Id = RCS_Id,          &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Surface_in:      Surface structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
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
!       Surface_out:     Copy of the input structure, Surface_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Same as Surface_in
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
!       CRTM_Assign_SensorData:   Function to copy valid CRTM SensorData structures.
!
!       Display_Message:          Subroutine to output messages
!                                 SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!       
! COMMENTS:
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-MAy-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( Surface_in,   &  ! Input
                          Surface_out,  &  ! Output
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
    TYPE( CRTM_Surface_type ), INTENT( IN )     :: Surface_in

    ! -- Output
    TYPE( CRTM_Surface_type ), INTENT( IN OUT ) :: Surface_out

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Surface(Scalar)'



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
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    Surface_out%Land_Coverage  = Surface_in%Land_Coverage
    Surface_out%Water_Coverage = Surface_in%Water_Coverage
    Surface_out%Snow_Coverage  = Surface_in%Snow_Coverage
    Surface_out%Ice_Coverage   = Surface_in%Ice_Coverage

    Surface_out%Wind_Speed = Surface_in%Wind_Speed

    Surface_out%Land_Type             = Surface_in%Land_Type
    Surface_out%Land_Temperature      = Surface_in%Land_Temperature
    Surface_out%Soil_Moisture_Content = Surface_in%Soil_Moisture_Content
    Surface_out%Canopy_Water_Content  = Surface_in%Canopy_Water_Content
    Surface_out%Vegetation_Fraction   = Surface_in%Vegetation_Fraction
    Surface_out%Soil_Temperature      = Surface_in%Soil_Temperature

    Surface_out%Water_Type        = Surface_in%Water_Type
    Surface_out%Water_Temperature = Surface_in%Water_Temperature
    Surface_out%Wind_Direction    = Surface_in%Wind_Direction
    Surface_out%Salinity          = Surface_in%Salinity

    Surface_out%Snow_Type        = Surface_in%Snow_Type
    Surface_out%Snow_Temperature = Surface_in%Snow_Temperature
    Surface_out%Snow_Depth       = Surface_in%Snow_Depth
    Surface_out%Snow_Density     = Surface_in%Snow_Density
    Surface_out%Snow_Grain_Size  = Surface_in%Snow_Grain_Size

    Surface_out%Ice_Type        = Surface_in%Ice_Type
    Surface_out%Ice_Temperature = Surface_in%Ice_Temperature
    Surface_out%Ice_Thickness   = Surface_in%Ice_Thickness
    Surface_out%Ice_Density     = Surface_in%Ice_Density
    Surface_out%Ice_Roughness   = Surface_in%Ice_Roughness


    ! ----------------------------------
    ! Deep copy the SensorData structure
    ! ----------------------------------

    IF ( Surface_In%SensorData%n_Channels > 0 ) THEN

      ! -- If there is data to copy, then do it....
      Error_Status = CRTM_Assign_SensorData( Surface_In%SensorData, &
                                             Surface_Out%SensorData, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying Surface SensorData structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE

      ! -- ...otherwise simply clear the structure
      Error_Status = CRTM_Destroy_SensorData( Surface_Out%SensorData, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error destroying output Surface SensorData structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF

  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( Surface_in,   &  ! Input
                         Surface_out,  &  ! Output
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
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN )     :: Surface_in

    ! -- Output
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: Surface_out

    ! -- Revision control
    CHARACTER( * ),            OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),            OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_Surface(Rank-1)'


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

    n = SIZE( Surface_in )

    IF ( SIZE( Surface_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Surface_in and Surface_out arrays'//&
                            ' have different dimensions', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Scalar_Status = Assign_Scalar( Surface_in(i), &
                                     Surface_out(i), &
                                     Message_Log = Message_Log )


      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( Message, '( "Error copying element #", i5, &
                          &" of Surface structure array." )' ) i
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
!       CRTM_WeightedSum_Surface
!
! PURPOSE:
!       Function to perform a weighted sum of two valid CRTM_Surface
!       structures. The weighted summation performed is:
!         A = A + w1*B + w2
!       where A and B are the CRTM_Surface structures, and w1 and w2
!       are the weighting factors. Note that w2 is optional.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_WeightedSum_Surface( A,                        &  ! In/Output
!                                                B,                        &  ! Input
!                                                w1,                       &  ! Input
!                                                w2 = w2,                  &  ! Optional input
!                                                RCS_Id = RCS_Id,          &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       A:               Surface structure that is to be added to.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar OR Rank-1
!                        ATTRIBUTES: INTENT( IN OUT )
!
!       B:               Surface structure that is to be weighted and
!                        added to structure A.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
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
!                        TYPE:       CRTM_Surface_type
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
    TYPE( CRTM_Surface_type ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Surface_type ), INTENT( IN )     :: B
    REAL( fp_kind ),           INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL, INTENT( IN )     :: w2

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightedSum_Surface(Scalar)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    REAL( fp_kind ) :: w2_Local



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
    !#                         -- TEST THE STRUCTURES --                        #
    !#--------------------------------------------------------------------------#

    ! What tests?  Verify the coverage types?



    !#--------------------------------------------------------------------------#
    !#                 -- CHECK THE OPTIONAL WEIGHT ARGUMENT --                 #
    !#--------------------------------------------------------------------------#

    w2_Local = ZERO

    IF ( PRESENT( w2 ) ) w2_Local = w2



    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM THE WEIGHTED SUM --                      #
    !#--------------------------------------------------------------------------#

    A%Wind_Speed = A%Wind_Speed + (w1*B%Wind_Speed) + w2_Local

    A%Land_Temperature      = A%Land_Temperature      + (w1*B%Land_Temperature     ) + w2_Local
    A%Soil_Moisture_Content = A%Soil_Moisture_Content + (w1*B%Soil_Moisture_Content) + w2_Local
    A%Canopy_Water_Content  = A%Canopy_Water_Content  + (w1*B%Canopy_Water_Content ) + w2_Local
    A%Vegetation_Fraction   = A%Vegetation_Fraction   + (w1*B%Vegetation_Fraction  ) + w2_Local
    A%Soil_Temperature      = A%Soil_Temperature      + (w1*B%Soil_Temperature     ) + w2_Local

    A%Water_Temperature = A%Water_Temperature + (w1*B%Water_Temperature) + w2_Local
    A%Wind_Direction    = A%Wind_Direction    + (w1*B%Wind_Direction   ) + w2_Local
    A%Salinity          = A%Salinity          + (w1*B%Salinity         ) + w2_Local

    A%Snow_Temperature = A%Snow_Temperature + (w1*B%Snow_Temperature) + w2_Local
    A%Snow_Depth       = A%Snow_Depth       + (w1*B%Snow_Depth      ) + w2_Local
    A%Snow_Density     = A%Snow_Density     + (w1*B%Snow_Density    ) + w2_Local
    A%Snow_Grain_Size  = A%Snow_Grain_Size  + (w1*B%Snow_Grain_Size ) + w2_Local

    A%Ice_Temperature = A%Ice_Temperature + (w1*B%Ice_Temperature) + w2_Local
    A%Ice_Thickness   = A%Ice_Thickness   + (w1*B%Ice_Thickness  ) + w2_Local
    A%Ice_Density     = A%Ice_Density     + (w1*B%Ice_Density    ) + w2_Local
    A%Ice_Roughness   = A%Ice_Roughness   + (w1*B%Ice_Roughness  ) + w2_Local

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
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: A

    ! -- Input only
    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN )     :: B
    REAL( fp_kind ),                           INTENT( IN )     :: w1

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL,                 INTENT( IN )     :: w2

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT )    :: RCS_Id

    ! - Error messaging
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_WeightSum_Surface(Rank-1)'


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
                          &" of CRTM_Surface structure arrays." )' ) i
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
!       CRTM_Zero_Surface
! 
! PURPOSE:
!       Subroutine to zero-out members of a CRTM_Surface structure.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Zero_Surface( Surface )
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Surface:      Zeroed out Surface structure.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
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
!       - No checking of the input structure is performed.
!
!       - The Surface coverage members are *NOT* set to zero.
!
!       - The Surface type components (land, water, snow, and ice) are *NOT*
!         reset.
!
!       - The SensorData dimension and structure components are *NOT*
!         reset.
!
! COMMENTS:
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument must be defined upon
!       input.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Aug-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Scalar( Surface )  ! Output
    TYPE( CRTM_Surface_type ),  INTENT( IN OUT ) :: Surface

    Surface%Wind_Speed            = ZERO

    Surface%Land_Temperature      = ZERO
    Surface%Soil_Moisture_Content = ZERO
    Surface%Canopy_Water_Content  = ZERO
    Surface%Vegetation_Fraction   = ZERO
    Surface%Soil_Temperature      = ZERO

    Surface%Water_Temperature     = ZERO
    Surface%Wind_Direction        = ZERO
    Surface%Salinity              = ZERO

    Surface%Snow_Temperature      = ZERO
    Surface%Snow_Depth            = ZERO
    Surface%Snow_Density          = ZERO
    Surface%Snow_Grain_Size       = ZERO

    Surface%Ice_Temperature       = ZERO
    Surface%Ice_Thickness         = ZERO
    Surface%Ice_Density           = ZERO
    Surface%Ice_Roughness         = ZERO

  END SUBROUTINE Zero_Scalar


  SUBROUTINE Zero_Rank1( Surface )  ! Output

    TYPE( CRTM_Surface_type ), DIMENSION( : ), INTENT( IN OUT ) :: Surface
    INTEGER :: n

    DO n = 1, SIZE( Surface )
      CALL Zero_Scalar( Surface(n) )
    END DO

  END SUBROUTINE Zero_Rank1

END MODULE CRTM_Surface_Define


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_Surface_Define.f90,v 1.15 2005/10/19 18:40:33 paulv Exp $
!
! $Date: 2005/10/19 18:40:33 $
!
! $Revision: 1.15 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Surface_Define.f90,v $
! Revision 1.15  2005/10/19 18:40:33  paulv
! - Added default surface parameters.
! - Surface structure definition now assigns default values to scalar
!   components.
! - Private Clear() routine now assigns default values to scalar
!   components.
!
! Revision 1.14  2005/09/21 17:02:03  yhan
! --- Add parameters OLD_SNOW and NEW_SNOW.
!
! Revision 1.13  2005/08/18 22:13:47  paulv
! - Added Surface Zero subroutines.
!
! Revision 1.12  2005/06/29 01:11:36  paulv
! - Removed the gross surface type flag from the structure. The various coverage
!   fractions are now used to process the surface types.
!
! Revision 1.11  2005/06/15 23:51:25  paulv
! - Added WeightedSum() functions.
!
! Revision 1.10  2005/02/16 15:44:40  paulv
! - Added allocation routines to allow the SensorData component to be allocated
!   via a Surface call (similar to how Cloud structures can be allocated in
!   the Atmosphere structure.)
! - Initialization of structure components changed from an "invalid" value
!   to zero.
!
! Revision 1.9  2004/11/05 15:53:42  paulv
! - Upgraded to Fortran-95
! - Structure initialisation is now performed in the structure type
!   declaration. Removed Init() subroutine.
! - Removed Association() and Allocation() routines since all the structure
!   components are scalar. The SensorData dimensionality was placed inside
!   the SensorData structure.
! - Updated documentation.
! - Added FP_INVALID parameter for initialising floating point variables.
!
! Revision 1.8  2004/08/05 21:57:09  paulv
! - Updated header documentation.
!
! Revision 1.7  2004/08/05 17:38:10  paulv
! - Updated main surface type to accept combinations of inidividual types.
! - Added coverage fractions for all allowed types.
! - Added ice surface type information.
!
! Revision 1.5  2004/07/23 21:20:42  paulv
! - Adding in CRTM_SensorData module usage. Incomplete.
!
! Revision 1.4  2004/07/22 19:40:07  paulv
! - Added "multi" specific routines for the Init() and Destroy() generic routines
!   to allow users to specify up to 10 surface structures or structure arrays
!   when itinitalising or destroying them (rather than calling the function
!   for each individual structure.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/06/16 14:55:25  paulv
! - Added snow type definitions.
! - Added CRTM_ prefix to procedure and type names.
! - Removed location information from CRTM_Surface_type derived type.
!
! Revision 1.1  2004/06/04 19:37:21  paulv
! Initial checkin. Incomplete.
!
! Revision 1.1  2004/05/12 16:41:03  paulv
! Initial checkin. Untested.
!
!
!
!
