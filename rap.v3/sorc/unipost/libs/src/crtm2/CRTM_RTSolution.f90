!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_RTSolution
!
! PURPOSE:
!       Module containing the raditive transfer solution routines.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_RTSolution
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_SpcCoeff:              Module containing the shared CRTM spectral
!                                   coefficients (SpcCoeff) and their
!                                   load/destruction routines. 
!                                   USEs TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        SPCCOEFF_DEFINE module
!                                        SPCCOEFF_BINARY_IO module
!                                        CRTM_PARAMETERS module
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
!       CRTM_AtmAbsorption_Define:  Module defining the CRTM AtmAbsorption
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_AtmScatter_Define:     Module defining the CRTM AtmScatter
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_SfcOptics_Define:      Module to compute the surface optical
!                                   properties required for determining
!                                   the surface contribution to the radiative
!                                   transfer.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_RTSolution_Define:     Module defining the CRTM RTSolution
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Compute_RTSolution:     Function to solve the radiative transfer 
!                                      problem.
!
!         CRTM_Compute_RTSolution_TL:  Function to solve the tangent-linear
!                                      radiative transfer problem.
!
!         CRTM_Compute_RTSolution_AD:  Function to solve the adjoint
!                                      radiative transfer problem.
!
!       PRIVATE subprograms
!       -------------------
!       
!         *** USERS ADD INFO HERE FOR ANY PRIVATE SUBPROGRAMS ***
!
!
!
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Clear_RTSolution:   Subroutine to clear the scalar members of a
!                                CRTM_RTSolution structure.
!                                SOURCE: CRTM_RTSOLUTION_DEFINE module
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu,    QSS at JCSDA;    Quanhua.Liu@noaa.gov 
!                       Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2004
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

MODULE CRTM_RTSolution


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Error_Handler

  ! -- CRTM parameters and shared data
  USE CRTM_Parameters
  USE CRTM_SpcCoeff

  ! -- CRTM Input data structures
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  ! -- CRTM Internal structures
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmScatter_type

  ! -- Source function module(s)
  USE CRTM_Planck_Functions

  ! -- The surface optical properties module.
  ! -- Special case since the Compute_SfcOptics()
  ! -- function *may* be called in the Compute_RTSolution()
  ! -- function in this module for SOI schemes.
  USE CRTM_SfcOptics

  ! -- CRTM Output data structure
  ! -- The PUBLIC entities in CRTM_RTSOlution_Define
  ! -- are also explicitly defined as PUBLIC here so 
  ! -- a user need only USE CRTM_RTSolution.
  USE CRTM_RTSolution_Define

  ! -- Utility module containing integration routine
  USE CRTM_Utility

  ! Local variables holder
  USE CRTM_RTlocal
  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibilities
  ! ------------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_RTSolution structure data type
  ! -- in the CRTM_RTSolution_Define module
  PUBLIC :: CRTM_RTSolution_type

  ! -- CRTM_RTSolution structure routines inherited
  ! -- from the CRTM_RTSolution_Define module
  PUBLIC :: CRTM_Clear_RTSolution

  ! -- Public procedures
  PUBLIC :: CRTM_Compute_RTSolution
  PUBLIC :: CRTM_Compute_RTSolution_TL
  PUBLIC :: CRTM_Compute_RTSolution_AD

  PUBLIC :: CRTM_Compute_n_Streams


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &
  '$Id: CRTM_RTSolution.f90,v 1.13.2.12 2005/10/21 18:56:08 paulv Exp $'

  ! -- Keyword switch values
  INTEGER, PRIVATE, PARAMETER :: NOT_SET = 0
  INTEGER, PRIVATE, PARAMETER ::     SET = 1

  ! -- Threshold for determing if an additional stream
  ! -- angle is required for the satellite zenith angle
  REAL( fp_kind ), PRIVATE, PARAMETER :: ANGLE_THRESHOLD = 1.0e-7_fp_kind

  ! -- Small positive value used to replace negative
  ! -- values in the computed phase function
  REAL( fp_kind ), PRIVATE, PARAMETER :: PHASE_THRESHOLD = 1.0e-7_fp_kind

  ! -- The maximum number of doubling processes in the
  ! -- the doubling-adding scheme.
  INTEGER,         PRIVATE, PARAMETER :: MAX_NUMBER_DOUBLING = 25

  ! -- The optical depth difference used to obtain the
  ! -- number of doubling processes for a layer, where
  ! --   Optical_depth = 2**n . DELTA_OPTICAL_DEPTH
  ! -- and n == number of doubling layers.
  REAL( fp_kind ), PRIVATE, PARAMETER :: DELTA_OPTICAL_DEPTH = 1.0e-3_fp_kind


  ! --------------------------------------
  ! Strucutre definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------


CONTAINS



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

      SUBROUTINE CRTM_Emission(n_Layers, & ! INPUT  number of atmospheric layers
                               n_Angles, & ! number angles used in SfcOptics
                        Diffuse_Surface, & ! INPUT  TRUE: Lambertian, FALSE: specular
                                      u, & ! INPUT  cosine of local viewing angle
                                   T_OD, & ! INPUT  nadir layer optical depth
                      Planck_Atmosphere, & ! INPUT  atmospheric layer Planck radiance
                         Planck_Surface, & ! INPUT  surface Planck radiance 
                             emissivity, & ! INPUT  surface emissivity
                           reflectivity, & ! INPUT  surface reflectivity matrix
                    direct_reflectivity, & ! INPUT  reflectivity for direct irradiance 
                      cosmic_background, & ! INPUT  cosmic background radiance
                       Solar_irradiance, & ! INPUT  Solar spectral irradiance
                       Is_Solar_Channel, & ! INPUT  Indicate solar affected channel
                   Source_Zenith_Radian, & ! INPUT  Point source (e.g. solar) zenith angle
                                    RTV)   ! OUTPUT TOA radiance and others
! ----------------------------------------------------------------------------- !
!  FUNCTION: Compute IR/MW upward radiance at the top of the profile.           !
!    This code heritages the concept from previous operational code.            !
!    It starts from cosmic background downward.                                 !
!    The downward radiance at the lower level is the transmitted radiance       !
!    from upper level adding the layer downward source function.                !
!    The downward angle is either the same as satellite viewing zenith for a    !
!    specular surface or the diffuse angle for a lambertian surface. The upward !
!    radiance at the surface is the surface emission term adding from surface   !
!    reflected downward radiance. Then, the upward radiance is the sum of       !
!    from the lower level transmitted radiance adding the upward layer          !
!    source function.                                                           !
!                                                                               !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                        !
! ----------------------------------------------------------------------------- !

      IMPLICIT NONE
      LOGICAL, INTENT(IN) :: Diffuse_Surface
      INTEGER, INTENT(IN) :: n_Layers, n_Angles, Is_Solar_Channel
      REAL ( fp_kind ), INTENT(IN) :: Solar_irradiance, Source_Zenith_Radian

    !   Structure RTV carried in/out variables in forward. The variables
    !   may be used in tangent-linear or adjoint calculations.
      TYPE( CRTM_RTVariables_type ), INTENT( INOUT) :: RTV

      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  T_OD, emissivity
      REAL ( fp_kind ), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) :: direct_reflectivity 
      REAL ( fp_kind ), INTENT(IN), DIMENSION( 0: ) ::  Planck_Atmosphere
      REAL ( fp_kind ), INTENT(IN) :: Planck_Surface,cosmic_background,u
    !  internal variables
      REAL ( fp_kind ) :: layer_source_up, layer_source_down, Total_OD, cosine_u0 
      INTEGER :: k

    !#--------------------------------------------------------------------------#
    !#                -- Thermal Downwelling radiance   --                      #
    !#--------------------------------------------------------------------------#

    ! Determing secant downward angle from surface behavior
      IF( Diffuse_Surface ) THEN
        RTV%Secant_Down_Angle = SECANT_DIFFUSIVITY 
      ELSE
        RTV%Secant_Down_Angle = ONE/u
      ENDIF

    !  start from the top of the atmosphere
      RTV%e_Level_Rad_DOWN(0) = cosmic_background 
      Total_OD = ZERO

    ! loop from top layer to bottom layer
      DO k = 1, n_Layers
       ! accumulate optical depth 
       Total_OD = Total_OD + T_OD(k)
       ! layer downward transmittance
       RTV%e_Layer_Trans_DOWN(k) = exp(-T_OD(k) * RTV%Secant_Down_Angle)

       layer_source_down = Planck_Atmosphere(k) * ( ONE - RTV%e_Layer_Trans_DOWN(k) )
       
       ! downward radiance  
       RTV%e_Level_Rad_DOWN(k) = RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k) &
                               + layer_source_down 
      ENDDO

      RTV%Total_OD = Total_OD
    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

       ! upward radiance at the surface ( emission part + reflection part)
       RTV%e_Level_Rad_UP(n_Layers) = emissivity(n_Angles)*Planck_Surface  &
         + reflectivity(1,1)*RTV%e_Level_Rad_DOWN(n_Layers)

       ! solar contribution to the upward radiance at the surface
       IF( Is_Solar_Channel > 0 ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        RTV%e_Level_Rad_UP(n_Layers) = RTV%e_Level_Rad_UP(n_Layers)  &
          + cosine_u0*Solar_Irradiance/PI*direct_reflectivity(1)  &
          * exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF
    !#--------------------------------------------------------------------------#
    !#                -- Upwelling radiance   --                                #
    !#--------------------------------------------------------------------------#

      DO k = n_Layers, 1, -1

       ! layer upward transmittance
       RTV%e_Layer_Trans_UP(k) = exp(-T_OD(k)/u)

       layer_source_up = Planck_Atmosphere(k) * ( ONE - RTV%e_Layer_Trans_UP(k) )

       ! upward radiance 
       RTV%e_Level_Rad_UP(k-1) = RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k) &
         + layer_source_up 
      ENDDO
!
      RETURN
      END SUBROUTINE CRTM_Emission 
!
!
   SUBROUTINE CRTM_Emission_TL(n_Layers, & ! INPUT  number of atmospheric layers
                               n_Angles, & ! number angles used in SfcOptics
                                      u, & ! INPUT  cosine of local viewing angle
                                   T_OD, & ! INPUT  nadir layer optical depth
                      Planck_Atmosphere, & ! INPUT  atmospheric layer Planck radiance
                         Planck_Surface, & ! INPUT  surface Planck radiance 
                             emissivity, & ! INPUT  surface emissivity
                           reflectivity, & ! INPUT  surface reflectivity matrix
                    direct_reflectivity, & ! INPUT  reflectivity for direct irradiance 
                       Solar_irradiance, & ! INPUT  Solar spectral irradiance
                       Is_Solar_Channel, & ! INPUT  Indicate solar affected channel 
                   Source_Zenith_Radian, & ! INPUT  Point source (e.g. solar) zenith angle
                                    RTV, & ! INPUT  Structure containing forward part results 
                                T_OD_TL, & ! INPUT  tangent-linear of layer optical depth
                   Planck_Atmosphere_TL, & ! INPUT  TL atmospheric layer Planck radiance
                      Planck_Surface_TL, & ! INPUT  TL surface Planck radiance
                          emissivity_TL, & ! INPUT  TL surface emissivity
                        reflectivity_TL, & ! INPUT  TL surface reflectivity matrix
                 direct_reflectivity_TL, & ! INPUT  TL surface ditrct reflectivity
                              up_rad_TL)   ! OUTPUT TL TOA radiance
! --------------------------------------------------------------------------- !
!  FUNCTION: Compute tangent-linear upward radiance at the top of the         !
!    atmosphere using carried results in RTV structure from forward           !
!    calculation.                                                             !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                      !
! --------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers, n_Angles, Is_Solar_Channel
      REAL ( fp_kind ), INTENT(IN) :: Solar_irradiance, Source_Zenith_Radian
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  T_OD, emissivity,T_OD_TL,emissivity_TL
      REAL ( fp_kind ), INTENT(IN), DIMENSION( :,: ) :: reflectivity ,reflectivity_TL
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) :: direct_reflectivity,direct_reflectivity_TL
      REAL ( fp_kind ), INTENT(IN), DIMENSION( 0: ) :: Planck_Atmosphere,Planck_Atmosphere_TL
      REAL ( fp_kind ), INTENT(IN) :: Planck_Surface,u,Planck_Surface_TL
      REAL ( fp_kind ), INTENT(INOUT) :: up_rad_TL

    !   Structure RTV carried in variables from forward calculation. 
      TYPE( CRTM_RTVariables_type ), INTENT( IN) :: RTV
    !  internal variables
      REAL ( fp_kind ) :: layer_source_up_TL, layer_source_down_TL,a_TL,down_rad_TL
      REAL ( fp_kind ) :: Total_OD, Total_OD_TL
      INTEGER :: k
      REAL( fp_kind) :: cosine_u0

    !#--------------------------------------------------------------------------#
    !#                -- Downwelling TL radiance   --                           #
    !#--------------------------------------------------------------------------#

      down_rad_TL = ZERO 
      Total_OD_TL = ZERO
    
      Total_OD = RTV%Total_OD
 
      DO k = 1, n_Layers
       ! accumulate tangent-linear optical depth
       Total_OD_TL = Total_OD_TL + T_OD_TL(k)
       a_TL = -T_OD_TL(k) * RTV%Secant_Down_Angle

       layer_source_down_TL = Planck_Atmosphere_TL(k) * ( ONE - RTV%e_Layer_Trans_DOWN(k) ) &
                            - Planck_Atmosphere(k) * RTV%e_Layer_Trans_DOWN(k) * a_TL
 
     ! downward tangent-linear radiance
     !    down_rad(k) = down_rad(k-1) * layer_trans(k) + layer_source_down 
       down_rad_TL = down_rad_TL*RTV%e_Layer_Trans_DOWN(k)  &
       +RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)*a_TL+layer_source_down_TL
      ENDDO

    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

      ! upward tangent-linear radiance at the surface 
       up_rad_TL =emissivity_TL(n_Angles)*Planck_Surface+emissivity(n_Angles)*Planck_Surface_TL &
       +reflectivity_TL(1,1)*RTV%e_Level_Rad_DOWN(n_Layers)+reflectivity(1,1)*down_rad_TL

      ! point source (e.g. solar radiation)
       IF( Is_Solar_Channel > 0 ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        up_rad_TL = up_rad_TL + cosine_u0*Solar_Irradiance/PI &
                  * direct_reflectivity_TL(1) * exp(-Total_OD/cosine_u0)   &
                  - Solar_Irradiance/PI * direct_reflectivity(1)    &
                  * Total_OD_TL * exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF

    !#--------------------------------------------------------------------------#
    !#            -- Upwelling TL radiance   --                                 #
    !#--------------------------------------------------------------------------#

      DO k = n_Layers, 1, -1
       a_TL = -T_OD_TL(k)/u 
       layer_source_up_TL = Planck_Atmosphere_TL(k) * ( ONE - RTV%e_Layer_Trans_UP(k) ) &
                          - Planck_Atmosphere(k) * RTV%e_Layer_Trans_UP(k) * a_TL
  
      ! upward tangent linear radiance
       up_rad_TL=up_rad_TL*RTV%e_Layer_Trans_UP(k)  &
       +RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)*a_TL+layer_source_up_TL 
      ENDDO
!
      RETURN
      END SUBROUTINE CRTM_Emission_TL 
!
!
      SUBROUTINE CRTM_Emission_AD(n_Layers, & ! INPUT  number of atmospheric layers
                                  n_Angles, & ! number angles used in SfcOptics
                                         u, & ! INPUT  cosine of local viewing angle
                                      T_OD, & ! INPUT  nadir layer optical depth
                         Planck_Atmosphere, & ! INPUT  atmospheric layer Planck radiance
                            Planck_Surface, & ! INPUT  surface Planck radiance 
                                emissivity, & ! INPUT  surface emissivity
                              reflectivity, & ! INPUT  surface reflectivity matrix 
                       direct_reflectivity, & ! INPUT  surface reflectivity matrix 
                          Solar_irradiance, & ! INPUT  Solar spectral irradiance
                          Is_Solar_Channel, & ! INPUT  Indicate solar affected channel 
                      Source_Zenith_Radian, & ! INPUT  Point source (e.g. solar) zenith angle
                                       RTV, & ! INPUT  Structure containing forward part results 
                              up_rad_AD_in, & ! INPUT  adjoint radiance at the top
                                   T_OD_AD, & ! OUTPUT AD layer optical depth
                      Planck_Atmosphere_AD, & ! OUTPUT AD atmospheric layer Planck radiance
                         Planck_Surface_AD, & ! OUTPUT AD surface Planck radiance
                             emissivity_AD, & ! OUTPUT AD surface emissivity
                           reflectivity_AD, & ! OUTPUT AD surface reflectivity matrix
                    direct_reflectivity_AD)   ! OUTPUT AD surface direct reflectivity
! --------------------------------------------------------------------------- !
!  FUNCTION: Compute adjoint upward radiance at the top of the                !
!    atmosphere using carried results in RTV structure from forward           !
!    calculation.                                                             !
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                      !
! --------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers, n_Angles, Is_Solar_Channel
      REAL ( fp_kind ), INTENT(IN) :: Solar_Irradiance, Source_Zenith_Radian
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  T_OD, emissivity
      REAL ( fp_kind ), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) :: direct_reflectivity 
      REAL ( fp_kind ), INTENT(IN), DIMENSION( 0: ) ::  Planck_Atmosphere
      REAL ( fp_kind ), INTENT(IN) :: Planck_Surface,u
      REAL ( fp_kind ), INTENT(IN) :: up_rad_AD_in
      REAL ( fp_kind ), INTENT(IN OUT), DIMENSION( : ) ::  T_OD_AD,emissivity_AD
      REAL ( fp_kind ), INTENT(IN OUT), DIMENSION( :,: ) :: reflectivity_AD
      REAL ( fp_kind ), INTENT(IN OUT), DIMENSION( : ) :: direct_reflectivity_AD
      REAL ( fp_kind ), INTENT(IN OUT), DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL ( fp_kind ), INTENT(IN OUT) :: Planck_Surface_AD
    TYPE( CRTM_RTVariables_type ), INTENT( IN) :: RTV
    !  internal variables
      REAL ( fp_kind ) :: layer_source_up_AD, layer_source_down_AD,a_AD,down_rad_AD
      REAL ( fp_kind ) :: cosine_u0, up_rad_AD, Total_OD, Total_OD_AD
      INTEGER :: k
!
    ! Initialize variables
      Total_OD_AD = ZERO
      T_OD_AD = ZERO
      Planck_Atmosphere_AD = ZERO
      Planck_Surface_AD = ZERO
      emissivity_AD = ZERO
      reflectivity_AD = ZERO
      direct_reflectivity_AD = ZERO
      up_rad_AD = up_rad_AD_in

    ! Total column optical depth carried from forward part
      Total_OD = RTV%Total_OD 

    !#--------------------------------------------------------------------------#
    !#                -- Upwelling adjoint radiance   --                        #
    !#--------------------------------------------------------------------------#
!
      DO k = 1, n_Layers
       a_AD = RTV%e_Level_Rad_UP(k)*RTV%e_Layer_Trans_UP(k)*up_rad_AD
       layer_source_up_AD = up_rad_AD
       up_rad_AD = up_rad_AD * RTV%e_Layer_Trans_UP(k)

       Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + &
              layer_source_up_AD * (ONE - RTV%e_Layer_Trans_UP(k))
       a_AD = a_AD - Planck_Atmosphere(k) * RTV%e_Layer_Trans_UP(k)* layer_source_up_AD
 
       T_OD_AD(k) = T_OD_AD(k) - a_AD/u 
      ENDDO
    !#--------------------------------------------------------------------------#
    !#                -- at surface   --                                        #
    !#--------------------------------------------------------------------------#

       IF( Is_Solar_Channel > 0 ) THEN
        cosine_u0 = cos(Source_Zenith_Radian)
        IF( cosine_u0 > ZERO) THEN
        Total_OD_AD = -Solar_Irradiance/PI * direct_reflectivity(1) &
                    * up_rad_AD * exp(-Total_OD/cosine_u0)
        direct_reflectivity_AD(1) = cosine_u0 * Solar_Irradiance/PI &
                    * up_rad_AD* exp(-Total_OD/cosine_u0)
        ENDIF
       ENDIF

      emissivity_AD(n_Angles)=up_rad_AD*Planck_Surface
      Planck_Surface_AD = emissivity(n_Angles)*up_rad_AD
      reflectivity_AD(1,1)=up_rad_AD*RTV%e_Level_Rad_DOWN(n_Layers)
      down_rad_AD = reflectivity(1,1)*up_rad_AD
!
    !#--------------------------------------------------------------------------#
    !#                -- Downward adjoint radiance   --                         #
    !#--------------------------------------------------------------------------#
      DO k = n_Layers, 1, -1

       a_AD = RTV%e_Level_Rad_DOWN(k-1)*RTV%e_Layer_Trans_DOWN(k)*down_rad_AD
       layer_source_down_AD = down_rad_AD
       down_rad_AD = down_rad_AD*RTV%e_Layer_Trans_DOWN(k)

       Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + layer_source_down_AD * &
                                 (ONE - RTV%e_Layer_Trans_DOWN(k))
       a_AD = a_AD - Planck_Atmosphere(k) * RTV%e_Layer_Trans_DOWN(k)* layer_source_down_AD
 

       T_OD_AD(k) = T_OD_AD(k) - a_AD * RTV%Secant_Down_Angle

       T_OD_AD(k) = T_OD_AD(k) + Total_OD_AD
      ENDDO

      down_rad_AD = ZERO 

      RETURN
      END SUBROUTINE CRTM_Emission_AD 
!
!
   SUBROUTINE CRTM_ADA(n_Layers, & ! INPUT  number of atmospheric layers
                              w, & ! INPUT  layer scattering albedo
                              g, & ! INPUT  layer asymmetry factor
                           T_OD, & ! INPUT  layer optical depth
              cosmic_background, & ! INPUT  cosmic background radiance
                     emissivity, & ! INPUT  surface emissivity
                   reflectivity, & ! INPUT  surface reflectivity matrix 
                            RTV)   ! IN/OUTPUT upward radiance and others
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW radiance at the top of the atmosphere  !
!   including atmospheric scattering. The scheme will include solar part.   !
!   The ADA algorithm computes layer reflectance and transmittance as well  !
!   as source function by the subroutine CRTM_Doubling_layer, then uses     !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      INTEGER nZ
      TYPE( CRTM_RTVariables_type ), INTENT( INOUT ) :: RTV
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  g,w,T_OD
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL ( fp_kind ), INTENT(IN), DIMENSION( :,: ) :: reflectivity 
      REAL ( fp_kind ), INTENT(IN) ::  cosmic_background

   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL ( fp_kind ), DIMENSION(RTV%n_Angles, RTV%n_Angles) :: temporal_matrix

      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      INTEGER :: i, j, k, Error_Status
!
       nZ = RTV%n_Angles
       RTV%s_Layer_Trans = ZERO
       RTV%s_Layer_Refl = ZERO
       RTV%s_Level_Refl_UP = ZERO
       RTV%s_Level_Rad_UP = ZERO
       RTV%s_Layer_Source_UP = ZERO
       RTV%s_Layer_Source_DOWN = ZERO
!
       RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,n_Layers)=reflectivity(1:RTV%n_Angles,1:RTV%n_Angles)
       RTV%s_Level_Rad_UP(1:RTV%n_Angles,n_Layers ) = emissivity(1:RTV%n_Angles)*RTV%Planck_Surface
!
!         UPWARD ADDING LOOP STARTS FROM BOTTOM LAYER TO ATMOSPHERIC TOP LAYER.
       DO 10 k = n_Layers, 1, -1
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 

       !  ----------------------------------------------------------- !
       !    CALL  multiple-stream algorithm for computing layer       !
       !    transmission, reflection, and source functions.           !
       !  ----------------------------------------------------------- !

!
       call CRTM_Doubling_layer(RTV%n_Streams,RTV%n_Angles,k,w(k),g(k),T_OD(k),RTV%COS_Angle, & ! INPUT
              RTV%COS_Weight,RTV%Pff(:,:,k),RTV%Pbb(:,:,k),        & ! INPUT
              RTV%Planck_Atmosphere(k), & ! INPUT 
              RTV)                       ! OUTPUT
!
!         Adding method to add the layer to the present level

       !  ----------------------------------------------------------- !
       !    Adding method to add the layer to the present level       !
       !    to compute upward radiances and reflection matrix         !
       !    at new level.                                             !
       !  ----------------------------------------------------------- !

         temporal_matrix = -matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                            RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k))
          DO i = 1, RTV%n_Angles 
              temporal_matrix(i,i) = ONE + temporal_matrix(i,i)
          ENDDO

         RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k) = matinv(temporal_matrix, Error_Status)
         RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k) =   &
          matmul(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k), RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))
         refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                                       RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))

         RTV%s_Level_Rad_UP(1:RTV%n_Angles,k-1 )=RTV%s_Layer_Source_UP(1:RTV%n_Angles,k)+ &
         matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),refl_down(1:RTV%n_Angles,k) &
               +RTV%s_Level_Rad_UP(1:RTV%n_Angles,k ))
         RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k), &
               RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))
         RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k-1)=RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k) + &
         matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)) 

      ELSE
         DO i = 1, RTV%n_Angles 
           RTV%s_Layer_Trans(i,i,k) = exp(-T_OD(k)/RTV%COS_Angle(i))
           RTV%s_Layer_Source_UP(i,k) = RTV%Planck_Atmosphere(k) * (ONE - RTV%s_Layer_Trans(i,i,k) )
           RTV%s_Layer_Source_DOWN(i,k) = RTV%s_Layer_Source_UP(i,k)

         ENDDO

!         Adding method
         DO i = 1, RTV%n_Angles 
         RTV%s_Level_Rad_UP(i,k-1 )=RTV%s_Layer_Source_UP(i,k)+ &
         RTV%s_Layer_Trans(i,i,k)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)*RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))  &
           +RTV%s_Level_Rad_UP(i,k ))
        ENDDO
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
         RTV%s_Level_Refl_UP(i,j,k-1)=RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*RTV%s_Layer_Trans(j,j,k)
        ENDDO
        ENDDO
      ENDIF
   10     CONTINUE
!
!  Adding reflected cosmic background radiation
      DO i = 1, RTV%n_Angles 
      RTV%s_Level_Rad_UP(i,0)=RTV%s_Level_Rad_UP(i,0)+sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,0))*cosmic_background
      ENDDO
!
     !   Forward part End

      RETURN
      END SUBROUTINE CRTM_ADA 
!
!
   SUBROUTINE CRTM_ADA_TL(n_Layers, & ! INPUT  number of atmospheric layers
                                 w, & ! INPUT  layer scattering albedo
                                 g, & ! INPUT  layer asymmetry factor
                              T_OD, & ! INPUT  layer optical depth
                 cosmic_background, & ! INPUT  cosmic background radiance
                        emissivity, & ! INPUT  surface emissivity
                                      !   reflectivity is stored in RTV !
                               RTV, & ! INPUT  structure containing forward part results 
              Planck_Atmosphere_TL, & ! INPUT  tangent-linear atmospheric layer Planck radiance 
                 Planck_Surface_TL, & ! INPUT  TL surface Planck radiance
                              w_TL, & ! INPUT  TL layer scattering albedo
                              g_TL, & ! INPUT  TL layer asymmetry factor
                           T_OD_TL, & ! INPUT  TL layer optical depth
                     emissivity_TL, & ! INPUT  TL surface emissivity
                   reflectivity_TL, & ! INPUT  TL  reflectivity
                            Pff_TL, & ! INPUT  TL forward phase matrix
                            Pbb_TL, & ! INPUT  TL backward phase matrix
                         s_rad_up_TL) ! OUTPUT TL upward radiance 
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW tangent-linear radiance at the top of  !
!   the atmosphere including atmospheric scattering. The structure RTV      !
!   carried in forward part results.                                        !
!   The CRTM_ADA_TL algorithm computes layer tangent-linear reflectance and !
!   transmittance as well as source function by the subroutine              !
!   CRTM_Doubling_layer as source function by the subroutine                !
!   CRTM_Doubling_layer, then uses                                          !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      TYPE( CRTM_RTVariables_type ), INTENT( IN ) :: RTV
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  g,w,T_OD
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL ( fp_kind ), INTENT(IN) ::  cosmic_background

      REAL ( fp_kind ),INTENT(IN),DIMENSION( :,:,: ) ::  Pff_TL, Pbb_TL
      REAL ( fp_kind ),INTENT(IN),DIMENSION( : ) ::  g_TL,w_TL,T_OD_TL
      REAL ( fp_kind ),INTENT(IN),DIMENSION( 0: ) ::  Planck_Atmosphere_TL
      REAL ( fp_kind ),INTENT(IN) ::  Planck_Surface_TL
      REAL ( fp_kind ),INTENT(IN),DIMENSION( : ) ::  emissivity_TL
      REAL ( fp_kind ),INTENT(IN),DIMENSION( :,: ) :: reflectivity_TL 
      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_TL 
   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL ( fp_kind ), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: temporal_matrix_TL

      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL ( fp_kind ), DIMENSION( RTV%n_Angles ) :: s_source_up_TL,s_source_down_TL,refl_down_TL
 
      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, RTV%n_Angles ) :: s_trans_TL,s_refl_TL,Refl_Trans_TL 
      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, RTV%n_Angles ) :: s_refl_up_TL,Inv_Gamma_TL,Inv_GammaT_TL
      INTEGER :: i, j, k
!
       Refl_Trans_TL = ZERO
       s_refl_up_TL = reflectivity_TL
       s_rad_up_TL = emissivity_TL * RTV%Planck_Surface + emissivity * Planck_Surface_TL
       DO 10 k = n_Layers, 1, -1
         s_source_up_TL = ZERO
         s_source_down_TL = ZERO
         s_trans_TL = ZERO
         s_refl_TL = ZERO
         Inv_GammaT_TL = ZERO
         Inv_Gamma_TL = ZERO
         refl_down_TL = ZERO
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 
       !  ----------------------------------------------------------- !
       !    CALL Doubling algorithm to computing forward and tagent   !
       !    layer transmission, reflection, and source functions.     !
       !  ----------------------------------------------------------- !
        
      call CRTM_Doubling_layer_TL(RTV%n_Streams,RTV%n_Angles,k,w(k),g(k),T_OD(k), & !INPUT
         RTV%COS_Angle(1:RTV%n_Angles),RTV%COS_Weight(1:RTV%n_Angles),            & !INPUT
         RTV%Pff(1:RTV%n_Angles,1:RTV%n_Angles,k),                                & !INPUT
         RTV%Pbb(1:RTV%n_Angles,1:RTV%n_Angles,k),RTV%Planck_Atmosphere(k),       & !INPUT
         w_TL(k),g_TL(k),T_OD_TL(k),Pff_TL(:,:,k),     & !INPUT
         Pbb_TL(:,:,k),Planck_Atmosphere_TL(k),RTV,     & !INPUT
         s_trans_TL,s_refl_TL,s_source_up_TL,s_source_down_TL)                      !OUTPUT

!         Adding method
         temporal_matrix_TL = -matmul(s_refl_up_TL,RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                            - matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_refl_TL)

         temporal_matrix_TL = matmul(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k),temporal_matrix_TL)
         Inv_Gamma_TL = -matmul(temporal_matrix_TL,RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))

         Inv_GammaT_TL = matmul(s_trans_TL, RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                       + matmul(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k), Inv_Gamma_TL)

         refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                               RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))
         refl_down_TL(:) = matmul(s_refl_up_TL,RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k)) &
                           + matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_source_down_TL(:))
         s_rad_up_TL(1:RTV%n_Angles)=s_source_up_TL(1:RTV%n_Angles)+ &
         matmul(Inv_GammaT_TL,refl_down(:,k)+RTV%s_Level_Rad_UP(1:RTV%n_Angles,k))  &
         +matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),refl_down_TL(1:RTV%n_Angles)+s_rad_up_TL(1:RTV%n_Angles))

         Refl_Trans_TL = matmul(s_refl_up_TL,RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                       + matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),s_trans_TL)

         s_refl_up_TL=s_refl_TL+matmul(Inv_GammaT_TL,RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k))  &
                     +matmul(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k),Refl_Trans_TL)

         Refl_Trans_TL = ZERO
      ELSE

         DO i = 1, RTV%n_Angles
           s_trans_TL(i,i) = -T_OD_TL(k)/RTV%COS_Angle(i) * RTV%s_Layer_Trans(i,i,k)
           s_source_up_TL(i) = Planck_Atmosphere_TL(k) * (ONE - RTV%s_Layer_Trans(i,i,k) ) &
                             - RTV%Planck_Atmosphere(k) * s_trans_TL(i,i)
           s_source_down_TL(i) = s_source_up_TL(i)
         ENDDO

!         Adding method
        DO i = 1, RTV%n_Angles 
        s_rad_up_TL(i)=s_source_up_TL(i) &
        +s_trans_TL(i,i)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)  &
        *RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))+RTV%s_Level_Rad_UP(i,k)) &
        +RTV%s_Layer_Trans(i,i,k)  &
        *(sum(s_refl_up_TL(i,1:RTV%n_Angles)*RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k)  &
        +RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)*s_source_down_TL(1:RTV%n_Angles))+s_rad_up_TL(i))

        ENDDO
                                       
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
        s_refl_up_TL(i,j)=s_trans_TL(i,i)*RTV%s_Level_Refl_UP(i,j,k)  &
        *RTV%s_Layer_Trans(j,j,k) &
        +RTV%s_Layer_Trans(i,i,k)*s_refl_up_TL(i,j)*RTV%s_Layer_Trans(j,j,k)  &
        +RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*s_trans_TL(j,j)
        ENDDO
        ENDDO

      ENDIF
   10     CONTINUE
!
!  Adding reflected cosmic background radiation
      DO i = 1, RTV%n_Angles 
      s_rad_up_TL(i)=s_rad_up_TL(i)+sum(s_refl_up_TL(i,:))*cosmic_background
      ENDDO
!
      RETURN
      END SUBROUTINE CRTM_ADA_TL
!
!
   SUBROUTINE CRTM_ADA_AD(n_Layers, & ! INPUT  number of atmospheric layers
                                 w, & ! INPUT  layer scattering albedo
                                 g, & ! INPUT  layer asymmetry factor
                              T_OD, & ! INPUT  layer optical depth
                 cosmic_background, & ! INPUT  cosmic background radiance
                        emissivity, & ! INPUT  surface emissivity
                                      !   surface reflectivity is stored in RTV !
                               RTV, & ! INPUT  structure containing forward results 
                       s_rad_up_AD, & ! INPUT  adjoint upward radiance 
              Planck_Atmosphere_AD, & ! OUTPUT AD atmospheric layer Planck radiance
                 Planck_Surface_AD, & ! OUTPUT AD surface Planck radiance
                              w_AD, & ! OUTPUT AD layer scattering albedo
                              g_AD, & ! OUTPUT AD layer asymmetry factor
                           T_OD_AD, & ! OUTPUT AD layer optical depth
                     emissivity_AD, & ! OUTPUT AD surface emissivity
                   reflectivity_AD, & ! OUTPUT AD surface reflectivity
                            Pff_AD, & ! OUTPUT AD forward phase matrix
                            Pbb_AD)   ! OUTPUT AD backward phase matrix
! ------------------------------------------------------------------------- !
! FUNCTION:                                                                 !
!   This subroutine calculates IR/MW adjoint radiance at the top of         !
!   the atmosphere including atmospheric scattering. The structure RTV      !
!   carried in forward part results.                                        !
!   The CRTM_ADA_AD algorithm computes layer tangent-linear reflectance and !
!   transmittance as well as source function by the subroutine              !
!   CRTM_Doubling_layer as source function by the subroutine                !
!   CRTM_Doubling_layer, then uses                                          !
!   an adding method to integrate the layer and surface components.         !
!                                                                           ! 
!    Quanhua Liu    Quanhua.Liu@noaa.gov                                    !
! ------------------------------------------------------------------------- !
      IMPLICIT NONE
      INTEGER, INTENT(IN) :: n_Layers
      TYPE( CRTM_RTVariables_type ), INTENT( IN ) :: RTV
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  g,w,T_OD
      REAL ( fp_kind ), INTENT(IN), DIMENSION( : ) ::  emissivity
      REAL ( fp_kind ), INTENT(IN) ::  cosmic_background

      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( :,:,: ) ::  Pff_AD, Pbb_AD
      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( : ) ::  g_AD,w_AD,T_OD_AD
      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( 0: ) ::  Planck_Atmosphere_AD
      REAL ( fp_kind ),INTENT(INOUT) ::  Planck_Surface_AD
      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( : ) ::  emissivity_AD
      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( :,: ) :: reflectivity_AD 
      REAL ( fp_kind ),INTENT(INOUT),DIMENSION( : ) :: s_rad_up_AD 
   ! -------------- internal variables --------------------------------- !
   !  Abbreviations:                                                     !
   !      s: scattering, rad: radiance, trans: transmission,             !
   !         refl: reflection, up: upward, down: downward                !
   ! --------------------------------------------------------------------!
      REAL ( fp_kind ), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: temporal_matrix_AD

      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, n_Layers) :: refl_down 
      REAL ( fp_kind ), DIMENSION( RTV%n_Angles ) :: s_source_up_AD,s_source_down_AD,refl_down_AD
 
      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, RTV%n_Angles) :: s_trans_AD,s_refl_AD,Refl_Trans_AD
      REAL ( fp_kind ), DIMENSION( RTV%n_Angles, RTV%n_Angles) :: s_refl_up_AD,Inv_Gamma_AD,Inv_GammaT_AD
      REAL ( fp_kind ) :: sum_s_AD, sums_AD
      INTEGER :: i, j, k
!
       s_trans_AD = ZERO
       Planck_Atmosphere_AD = ZERO
       Planck_Surface_AD = ZERO

      Pff_AD = ZERO
      Pbb_AD = ZERO
      T_OD_AD = ZERO
!  Adding reflected cosmic background radiation
      DO i = 1, RTV%n_Angles 
      sum_s_AD = s_rad_up_AD(i)*cosmic_background
      DO j = 1, RTV%n_Angles 
      s_refl_up_AD(i,j) = sum_s_AD
      ENDDO
      ENDDO
!
       DO 10 k = 1, n_Layers 
       s_source_up_AD = ZERO
       s_source_down_AD = ZERO
       s_trans_AD = ZERO
!
!      Compute tranmission and reflection matrices for a layer
      IF(w(k) > SCATTERING_ALBEDO_THRESHOLD) THEN 

        refl_down(1:RTV%n_Angles,k) = matmul(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k),  &
                               RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))

        s_refl_AD = s_refl_up_AD
        Inv_GammaT_AD = matmul(s_refl_up_AD,transpose(RTV%Refl_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        Refl_Trans_AD = matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_refl_up_AD)

        s_refl_up_AD=matmul(Refl_Trans_AD,transpose(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        s_trans_AD=matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),Refl_Trans_AD)
 
        s_source_up_AD(1:RTV%n_Angles) = s_rad_up_AD(1:RTV%n_Angles)

        DO i = 1, RTV%n_Angles 
        sums_AD = s_rad_up_AD(i)
        DO j = 1, RTV%n_Angles 
        Inv_GammaT_AD(i,j)=Inv_GammaT_AD(i,j)+sums_AD*(refl_down(j,k)+RTV%s_Level_Rad_UP(j,k))
        ENDDO 
        ENDDO

        refl_down_AD(1:RTV%n_Angles)=matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_rad_up_AD(1:RTV%n_Angles))
        s_rad_up_AD(1:RTV%n_Angles)=matmul(transpose(RTV%Inv_GammaT(1:RTV%n_Angles,1:RTV%n_Angles,k)),s_rad_up_AD(1:RTV%n_Angles))

        DO i = 1, RTV%n_Angles 
        sums_AD = refl_down_AD(i)
        DO j = 1, RTV%n_Angles 
        s_refl_up_AD(i,j)=s_refl_up_AD(i,j)+sums_AD*RTV%s_Layer_Source_DOWN(j,k)
        ENDDO 
        ENDDO

        s_source_down_AD=matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),refl_down_AD(:)) 

        s_trans_AD=s_trans_AD+matmul(Inv_GammaT_AD,transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        Inv_Gamma_AD= matmul(transpose(RTV%s_Layer_Trans(1:RTV%n_Angles,1:RTV%n_Angles,k)),Inv_GammaT_AD)

        temporal_matrix_AD= -matmul(Inv_Gamma_AD,transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        temporal_matrix_AD=matmul(transpose(RTV%Inv_Gamma(1:RTV%n_Angles,1:RTV%n_Angles,k)),temporal_matrix_AD)

        s_refl_up_AD=s_refl_up_AD-matmul(temporal_matrix_AD,transpose(RTV%s_Layer_Refl(1:RTV%n_Angles,1:RTV%n_Angles,k)))
        s_refl_AD=s_refl_AD-matmul(transpose(RTV%s_Level_Refl_UP(1:RTV%n_Angles,1:RTV%n_Angles,k)),temporal_matrix_AD)
       !  ----------------------------------------------------------- !
       !    CALL Doubling algorithm to computing forward and tagent   !
       !    layer transmission, reflection, and source functions.     !
       !  ----------------------------------------------------------- !

      call CRTM_Doubling_layer_AD(RTV%n_Streams,RTV%n_Angles,k,w(k),g(k),T_OD(k),      & !INPUT
         RTV%COS_Angle,RTV%COS_Weight,RTV%Pff(:,:,k),RTV%Pbb(:,:,k),RTV%Planck_Atmosphere(k),    & !INPUT
         s_trans_AD,s_refl_AD,s_source_up_AD,   & 
         s_source_down_AD,RTV,w_AD(k),g_AD(k),T_OD_AD(k),Pff_AD(:,:,k), & 
         Pbb_AD(:,:,k),Planck_Atmosphere_AD(k))  !OUTPUT
!
      ELSE
        DO i = 1, RTV%n_Angles 
        DO j = 1, RTV%n_Angles 
        s_trans_AD(j,j)=s_trans_AD(j,j)+RTV%s_Layer_Trans(i,i,k)*RTV%s_Level_Refl_UP(i,j,k)*s_refl_up_AD(i,j)
        s_trans_AD(i,i)=s_trans_AD(i,i)+s_refl_up_AD(i,j)*RTV%s_Level_Refl_UP(i,j,k)*RTV%s_Layer_Trans(j,j,k)
        s_refl_up_AD(i,j)=RTV%s_Layer_Trans(i,i,k)*s_refl_up_AD(i,j)*RTV%s_Layer_Trans(j,j,k)
        ENDDO
        ENDDO
!         Adding method
       DO i = 1, RTV%n_Angles 

         s_source_up_AD(i)=s_rad_up_AD(i)
         s_trans_AD(i,i)=s_trans_AD(i,i)+s_rad_up_AD(i)*(sum(RTV%s_Level_Refl_UP(i,1:RTV%n_Angles,k)  &
         *RTV%s_Layer_Source_DOWN(1:RTV%n_Angles,k))+RTV%s_Level_Rad_UP(i,k))

         sum_s_AD=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)
         DO j = 1, RTV%n_Angles 
          s_refl_up_AD(i,j)=s_refl_up_AD(i,j)+sum_s_AD*RTV%s_Layer_Source_DOWN(j,k)
         ENDDO
                                            
         sum_s_AD=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)
         DO j = 1, RTV%n_Angles 
          s_source_down_AD(j)=s_source_down_AD(j)+sum_s_AD*RTV%s_Level_Refl_UP(i,j,k)
         ENDDO
         s_rad_up_AD(i)=RTV%s_Layer_Trans(i,i,k)*s_rad_up_AD(i)

       ENDDO

         DO i = RTV%n_Angles, 1, -1
           s_source_up_AD(i) = s_source_up_AD(i) +  s_source_down_AD(i)
           s_trans_AD(i,i) = s_trans_AD(i,i) - RTV%Planck_Atmosphere(k) * s_source_up_AD(i)
           Planck_Atmosphere_AD(k) = Planck_Atmosphere_AD(k) + s_source_up_AD(i) * (ONE - RTV%s_Layer_Trans(i,i,k) )
           s_source_up_AD(i) = ZERO

           T_OD_AD(k)=T_OD_AD(k)-s_trans_AD(i,i)/RTV%COS_Angle(i)*RTV%s_Layer_Trans(i,i,k)
         ENDDO

      ENDIF
   10     CONTINUE

      emissivity_AD = s_rad_up_AD * RTV%Planck_Surface
      Planck_Surface_AD = sum(emissivity(:) * s_rad_up_AD(:) )
      reflectivity_AD = s_refl_up_AD
!
       s_rad_up_AD = ZERO
       s_refl_up_AD = ZERO
      RETURN
      END SUBROUTINE CRTM_ADA_AD
!
!
      SUBROUTINE CRTM_Doubling_layer(n_streams, & ! INPUT, number of streams
                                          NANG, & ! INPUT, number of angles
                                            KL, & ! INPUT, KL-th layer 
                                 single_albedo, & ! INPUT, single scattering albedo
                              asymmetry_factor, & ! INPUT, asymmetry factor
                                 optical_depth, & ! INPUT, layer optical depth
                                     COS_Angle, & ! INPUT, COSINE of ANGLES
                                    COS_Weight, & ! INPUT, GAUSSIAN Weights
                                            ff, & ! INPUT, Phase matrix (forward part)
                                            bb, & ! INPUT, Phase matrix (backward part)
                                   Planck_Func, & ! INPUT, Planck for layer temperature
                                           RTV)   ! OUTPUT, layer transmittance, reflectance, and source 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Method and References
!   It is a common doubling method and its theoretical basis is referred to
!   Hansen, J. E., 1971: Multiple scattering of polarized light in 
!   planetary atmosphere. Part I. The doubling method, J. ATmos. Sci., 28, 120-125.
!
!   see also ADA method.
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,NANG,KL
     TYPE( CRTM_RTVariables_type ), INTENT( INOUT ) :: RTV
     REAL( fp_kind ), INTENT(IN), DIMENSION(:,:) :: ff,bb
     REAL( fp_kind ), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
     REAL( fp_kind ) :: single_albedo,asymmetry_factor,optical_depth,Planck_Func

     ! internal variables
     REAL( fp_kind ), DIMENSION(NANG,NANG) :: term2,term3,term4,trans,refl
     REAL( fp_kind ), DIMENSION(NANG) :: C1, C2, source_up,source_down 
     REAL( fp_kind ) :: s, c
     INTEGER :: i,j,k,L
     INTEGER :: Error_Status
!

      !  Forward part beginning

      IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
        RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = ZERO
        DO i = 1, NANG
          RTV%s_Layer_Trans(i,i,KL) = ONE
        ENDDO
        RTV%s_Layer_Refl(1:NANG,1:NANG,KL) = ZERO 
        RTV%s_Layer_Source_DOWN(1:NANG,KL) = ZERO 
        RTV%s_Layer_Source_UP(1:NANG,KL) = ZERO 
        RETURN
      ENDIF

        ! -------------------------------------------------------------- !
        !  Determining number of doubling processes and constructing     !
        !  initial transmission and reflection matrix 
        !  --------------------------------------------------------------!
        RTV%Number_Doubling(KL)=INT(log(optical_depth/DELTA_OPTICAL_DEPTH)/log(TWO))+1
        IF( RTV%Number_Doubling(KL) < 1 ) RTV%Number_Doubling(KL) = 1
        RTV%Delta_Tau(KL) = optical_depth/(TWO**RTV%Number_Doubling(KL))
        IF(RTV%Number_Doubling(KL) > MAX_NUMBER_DOUBLING) THEN
           RTV%Number_Doubling(KL)=MAX_NUMBER_DOUBLING
           RTV%Delta_Tau(KL) = DELTA_OPTICAL_DEPTH
        ENDIF
        s = RTV%Delta_Tau(KL) * single_albedo
        DO i = 1, NANG
        c = s/COS_Angle(i)
        DO j = 1, NANG
        RTV%Refl(i,j,0,KL) = c * bb(i,j) * COS_Weight(j)
        RTV%Trans(i,j,0,KL) = c * ff(i,j) * COS_Weight(j)
        ENDDO
        RTV%Trans(i,i,0,KL) = RTV%Trans(i,i,0,KL) + ONE - RTV%Delta_Tau(KL)/COS_Angle(i)
        ENDDO

        ! -------------------------------------------------------------- !
        !  Doubling divided sub-layers                                   !
        !  --------------------------------------------------------------!
        DO L = 1, RTV%Number_Doubling(KL)
          DO i = 1, NANG
          DO j = 1, NANG
          term4(i,j) = ZERO
          DO k = 1, NANG
          term4(i,j) = term4(i,j) - RTV%Refl(i,k,L-1,KL)*RTV%Refl(k,j,L-1,KL)
          ENDDO
          ENDDO
          term4(i,i) = term4(i,i) + ONE
          ENDDO

        RTV%Inv_BeT(1:NANG,1:NANG,L,KL) = matinv(term4, Error_Status)
        IF( Error_Status /= SUCCESS ) THEN
          print *,' error at matinv in CRTM_Doubling_layer '
          RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = ZERO
          DO i = 1, NANG
            RTV%s_Layer_Trans(i,i,KL) = exp(-optical_depth/COS_Angle(i)) 
          ENDDO
          RTV%s_Layer_Refl(1:NANG,1:NANG,KL) = ZERO 
          RTV%s_Layer_Source_DOWN(1:NANG,KL) = ZERO 
          RTV%s_Layer_Source_UP(1:NANG,KL) = ZERO 
          RETURN
        ENDIF

        term2 = matmul(RTV%Trans(1:NANG,1:NANG,L-1,KL), RTV%Inv_BeT(1:NANG,1:NANG,L,KL))
        term3 = matmul(term2, RTV%Refl(1:NANG,1:NANG,L-1,KL))
        term3 = matmul(term3, RTV%Trans(1:NANG,1:NANG,L-1,KL))
 
        RTV%Refl(1:NANG,1:NANG,L,KL) = RTV%Refl(1:NANG,1:NANG,L-1,KL) + term3
        RTV%Trans(1:NANG,1:NANG,L,KL) = matmul(term2, RTV%Trans(1:NANG,1:NANG,L-1,KL))
        ENDDO

        trans = RTV%Trans(1:NANG,1:NANG,RTV%Number_Doubling(KL),KL)
        refl  = RTV%Refl(1:NANG,1:NANG,RTV%Number_Doubling(KL),KL)
!
!   computing source function at up and downward directions.
      DO i = 1, NANG
        C1(i) = ZERO
        C2(i) = ZERO
       DO j = 1, n_Streams 
        C1(i) = C1(i) + trans(i,j) 
        C2(i) = C2(i) + refl(i,j) 
       ENDDO
      IF(i == NANG .AND. NANG == (n_Streams+1)) THEN
        C1(i) = C1(i)+trans(NANG,NANG)
      ENDIF
      ENDDO

      DO i = 1, NANG
        source_up(i) = (ONE-C1(i)-C2(i))*Planck_Func
        source_down(i) = source_up(i)
      ENDDO

        RTV%C1( 1:NANG,KL ) = C1( : )
        RTV%C2( 1:NANG,KL ) = C2( : )
        RTV%s_Layer_Trans(1:NANG,1:NANG,KL) = trans(:,:)
        RTV%s_Layer_Refl(1:NANG,1:NANG,KL) = refl(:,:)
        RTV%s_Layer_Source_DOWN(1:NANG,KL) = source_down(:)
        RTV%s_Layer_Source_UP(1:NANG,KL) = source_up(:)
       
      RETURN

      END SUBROUTINE CRTM_Doubling_layer
!
!
      SUBROUTINE CRTM_Doubling_layer_TL(n_streams, & ! INPUT, number of streams
                                             NANG, & ! INPUT, number of angles
                                               KL, & ! INPUT, number of angles
                                    single_albedo, & ! INPUT, single scattering albedo
                                 asymmetry_factor, & ! INPUT, asymmetry factor
                                    optical_depth, & ! INPUT, layer optical depth
                                        COS_Angle, & ! INPUT, COSINE of ANGLES
                                       COS_Weight, & ! INPUT, GAUSSIAN Weights
                                               ff, & ! INPUT, Phase matrix (forward part)
                                               bb, & ! INPUT, Phase matrix (backward part)
                                      Planck_Func, & ! INPUT, Planck for layer temperature
                                 single_albedo_TL, & ! INPUT, tangent-linear single albedo
                              asymmetry_factor_TL, & ! INPUT, TL asymmetry factor
                                 optical_depth_TL, & ! INPUT, TL layer optical depth
                                            ff_TL, & ! INPUT, TL forward Phase matrix
                                            bb_TL, & ! INPUT, TL backward Phase matrix
                                    Planck_Func_TL, & ! INPUT, TL Planck for layer temperature
                                              RTV, & ! INPUT, structure containing forward results 
                                         trans_TL, & ! OUTPUT, layer tangent-linear trans 
                                          refl_TL, & ! OUTPUT, layer tangent-linear refl 
                                     source_up_TL, & ! OUTPUT, layer tangent-linear source_up 
                                   source_down_TL)   ! OUTPUT, layer tangent-linear source_down 
! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute tangent-linear layer transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------
     IMPLICIT NONE
     INTEGER, INTENT(IN) :: n_streams,NANG,KL
      TYPE( CRTM_RTVariables_type ), INTENT( IN ) :: RTV
      REAL( fp_kind ), INTENT(IN), DIMENSION(:,:) :: ff,bb
      REAL( fp_kind ), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
      REAL( fp_kind ) :: single_albedo,asymmetry_factor,optical_depth,Planck_Func

      ! Tangent-Linear Part
      REAL( fp_kind ), INTENT( OUT ), DIMENSION( :,: ) :: trans_TL,refl_TL
      REAL( fp_kind ), INTENT( OUT ), DIMENSION( : ) :: source_up_TL,source_down_TL
      REAL( fp_kind ), INTENT( IN ) :: single_albedo_TL,asymmetry_factor_TL
      REAL( fp_kind ), INTENT( IN ) :: optical_depth_TL,Planck_Func_TL
      REAL( fp_kind ), INTENT(IN), DIMENSION(:,:) :: ff_TL,bb_TL

      ! internal variables
      REAL( fp_kind ), DIMENSION(NANG,NANG) :: term1,term2,term3,term4,term5_TL
      REAL( fp_kind ) :: s, c
      REAL( fp_kind ) :: s_TL, c_TL, d_TL, f_TL, Delta_Tau_TL
      REAL( fp_kind ), DIMENSION(NANG) :: C1_TL, C2_TL
      INTEGER :: i,j,L
!
      ! Tangent-Linear Beginning

      IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
        trans_TL = ZERO
        refl_TL = ZERO
        source_up_TL = ZERO
        source_down_TL = ZERO
        RETURN
      ENDIF

        s = RTV%Delta_Tau(KL) * single_albedo
        Delta_Tau_TL = optical_depth_TL/(TWO**RTV%Number_Doubling(KL))
        s_TL = Delta_Tau_TL * single_albedo + RTV%Delta_Tau(KL) * single_albedo_TL
        DO i = 1, NANG
        c = s/COS_Angle(i)
        c_TL = s_TL/COS_Angle(i)
        DO j = 1, NANG
        refl_TL(i,j) = c_TL*bb(i,j)*COS_Weight(j)+c*bb_TL(i,j)*COS_Weight(j)
        trans_TL(i,j) =c_TL*ff(i,j)*COS_Weight(j)+c*ff_TL(i,j)*COS_Weight(j)
        ENDDO
        trans_TL(i,i) =trans_TL(i,i) - Delta_Tau_TL/COS_Angle(i)
        ENDDO

        DO L = 1, RTV%Number_Doubling(KL) 

        term1 = matmul(RTV%Trans(1:NANG,1:NANG,L-1,KL),RTV%Inv_BeT(1:NANG,1:NANG,L,KL))
        term2 = matmul(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Refl(1:NANG,1:NANG,L-1,KL))
        term3 = matmul(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Trans(1:NANG,1:NANG,L-1,KL))
        term4 = matmul(term2,RTV%Trans(1:NANG,1:NANG,L-1,KL))
        term5_TL = matmul(refl_TL,RTV%Refl(1:NANG,1:NANG,L-1,KL))  &
              + matmul(RTV%Refl(1:NANG,1:NANG,L-1,KL),refl_TL)

        refl_TL=refl_TL+matmul(matmul(term1,term5_TL),term4)+matmul(trans_TL,term4) &
               +matmul(matmul(term1,refl_TL),RTV%Trans(1:NANG,1:NANG,L-1,KL)) 
        refl_TL=refl_TL+matmul(matmul(term1,RTV%Refl(1:NANG,1:NANG,L-1,KL)),trans_TL)

        trans_TL=matmul(trans_TL,term3)  &
                +matmul(matmul(term1,term5_TL),term3)+matmul(term1,trans_TL)

        ENDDO

!   computing source function at up and downward directions.

      DO i = 1, NANG
        C1_TL(i) = ZERO
        C2_TL(i) = ZERO
       DO j = 1, n_Streams 
        C1_TL(i) = C1_TL(i) + trans_TL(i,j) 
        C2_TL(i) = C2_TL(i) + refl_TL(i,j) 
       ENDDO
      IF(i == NANG .AND. NANG == (n_Streams+1)) THEN
        C1_TL(i) = C1_TL(i)+trans_TL(NANG,NANG)
      ENDIF
      ENDDO

      DO i = 1, NANG
        source_up_TL(i) = -(C1_TL(i)+C2_TL(i))*Planck_Func  &
         + (ONE-RTV%C1(i,KL)-RTV%C2(i,KL))*Planck_Func_TL
        source_down_TL(i) = source_up_TL(i)
      END DO

      RETURN

      END SUBROUTINE CRTM_Doubling_layer_TL


! ---------------------------------------------------------------------------------------
!   FUNCTION
!    Compute layer adjoint transmission, reflection matrices and source function 
!    at the top and bottom of the layer.
!
!   Quanhua Liu
!   Quanhua.Liu@noaa.gov
! ----------------------------------------------------------------------------------------

  SUBROUTINE CRTM_Doubling_layer_AD(n_streams, & ! INPUT, number of streams
                                         NANG, & ! INPUT, number of angles
                                           KL, & ! INPUT, number of angles
                                single_albedo, & ! INPUT, single scattering albedo
                             asymmetry_factor, & ! INPUT, asymmetry factor
                                optical_depth, & ! INPUT, layer optical depth
                                    COS_Angle, & ! INPUT, COSINE of ANGLES
                                   COS_Weight, & ! INPUT, GAUSSIAN Weights
                                           ff, & ! INPUT, Phase matrix (forward part)
                                           bb, & ! INPUT, Phase matrix (backward part)
                                  Planck_Func, & ! INPUT, Planck for layer temperature
                                     trans_AD, & ! INPUT, layer tangent-linear trans 
                                      refl_AD, & ! INPUT, layer tangent-linear refl 
                                 source_up_AD, & ! INPUT, layer tangent-linear source_up 
                               source_down_AD, & ! INPUT, layer tangent-linear source_down 
                                          RTV, & ! INPUT, structure containing forward results 
                             single_albedo_AD, & ! OUTPUT adjoint single scattering albedo
                          asymmetry_factor_AD, & ! OUTPUT AD asymmetry factor
                             optical_depth_AD, & ! OUTPUT AD layer optical depth
                                        ff_AD, & ! OUTPUT AD forward Phase matrix
                                        bb_AD, & ! OUTPUT AD backward Phase matrix
                               Planck_Func_AD)   ! OUTPUT AD Planck for layer temperature


    INTEGER, INTENT(IN) :: n_streams,NANG,KL
    TYPE( CRTM_RTVariables_type ), INTENT( IN ) :: RTV
    REAL( fp_kind ), INTENT(IN), DIMENSION(:,:) :: ff,bb
    REAL( fp_kind ), INTENT(IN), DIMENSION(:) :: COS_Angle, COS_Weight 
    REAL( fp_kind ) :: single_albedo,asymmetry_factor,optical_depth,Planck_Func

    ! Tangent-Linear Part
    REAL( fp_kind ), INTENT( INOUT ), DIMENSION( :,: ) :: trans_AD,refl_AD
    REAL( fp_kind ), INTENT( INOUT ), DIMENSION( : ) :: source_up_AD,source_down_AD
    REAL( fp_kind ), INTENT( INOUT ) :: single_albedo_AD,asymmetry_factor_AD
    REAL( fp_kind ), INTENT( INOUT ) :: optical_depth_AD,Planck_Func_AD
    REAL( fp_kind ), INTENT(INOUT), DIMENSION(:,:) :: ff_AD,bb_AD

    ! internal variables
    REAL( fp_kind ), DIMENSION(NANG,NANG) :: term1,term2,term3,term4,term5_AD
    REAL( fp_kind ) :: s, c
    REAL( fp_kind ) :: s_AD, c_AD, d_AD, f_AD, Delta_Tau_AD
    REAL( fp_kind ), DIMENSION(NANG) :: C1_AD, C2_AD
    INTEGER :: i,j,L

    ! Tangent-Linear Beginning

    IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
      trans_AD = ZERO
      refl_AD = ZERO
      source_up_AD = ZERO
      source_down_AD = ZERO
      RETURN
    ENDIF

    DO i = NANG, 1, -1
      source_up_AD(i) = source_up_AD(i) + source_down_AD(i)
      source_down_AD(i) = ZERO
      C2_AD(i) = - source_up_AD(i)*Planck_Func
      C1_AD(i) = - source_up_AD(i)*Planck_Func
      Planck_Func_AD = Planck_Func_AD + (ONE-RTV%C1(i,KL)-RTV%C2(i,KL))*source_up_AD(i)
    END DO

    ! -- Compute the source function in the up and downward directions.
    DO i = NANG, 1, -1

      IF(i == NANG .AND. NANG == (n_Streams+1)) THEN
        trans_AD(NANG,NANG)=trans_AD(NANG,NANG)+C1_AD(i)
      ENDIF

      DO j = n_Streams, 1, -1 
        refl_AD(i,j)=refl_AD(i,j)+C2_AD(i)
        trans_AD(i,j)=trans_AD(i,j)+C1_AD(i)
      END DO

    END DO

    DO L = RTV%Number_Doubling(KL), 1, -1 

      term1 = MATMUL(RTV%Trans(1:NANG,1:NANG,L-1,KL),RTV%Inv_BeT(1:NANG,1:NANG,L,KL))
      term2 = MATMUL(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Refl(1:NANG,1:NANG,L-1,KL))
      term3 = MATMUL(RTV%Inv_BeT(1:NANG,1:NANG,L,KL),RTV%Trans(1:NANG,1:NANG,L-1,KL))
      term4 = MATMUL(term2,RTV%Trans(1:NANG,1:NANG,L-1,KL))

      term5_AD = MATMUL(MATMUL(TRANSPOSE(term1),trans_AD),TRANSPOSE(term3))
      trans_AD = MATMUL(trans_AD,TRANSPOSE(term3))+MATMUL(TRANSPOSE(term1),trans_AD)
    
      trans_AD=trans_AD+MATMUL(TRANSPOSE(MATMUL(term1,RTV%Refl(1:NANG,1:NANG,L-1,KL))),refl_AD) 

      term5_AD =term5_AD+MATMUL(MATMUL(TRANSPOSE(term1),refl_AD),TRANSPOSE(term4))
      trans_AD = trans_AD+MATMUL(refl_AD,TRANSPOSE(term4)) 
      refl_AD = refl_AD+MATMUL(MATMUL(TRANSPOSE(term1),refl_AD),TRANSPOSE(RTV%Trans(1:NANG,1:NANG,L-1,KL)))
      refl_AD = refl_AD+MATMUL(term5_AD,TRANSPOSE(RTV%Refl(1:NANG,1:NANG,L-1,KL)))
      refl_AD = refl_AD+MATMUL(TRANSPOSE(RTV%Refl(1:NANG,1:NANG,L-1,KL)),term5_AD)

    ENDDO

    s = RTV%Delta_Tau(KL) * single_albedo
    c_AD = ZERO
    s_AD = ZERO
    Delta_Tau_AD=ZERO

    DO i = NANG, 1, -1

      c = s/COS_Angle(i)
      Delta_Tau_AD = Delta_Tau_AD - trans_AD(i,i)/COS_Angle(i)

      DO j = NANG, 1, -1
        c_AD = c_AD + trans_AD(i,j)*ff(i,j)*COS_Weight(j)
        ff_AD(i,j)=ff_AD(i,j)+trans_AD(i,j)*c*COS_Weight(j)
        c_AD = c_AD + refl_AD(i,j)*bb(i,j)*COS_Weight(j)
        bb_AD(i,j)=bb_AD(i,j) + refl_AD(i,j)*c*COS_Weight(j)
      END DO

      s_AD = s_AD + c_AD/COS_Angle(i) 
      c_AD = ZERO

    ENDDO

    Delta_Tau_AD = Delta_Tau_AD + s_AD* single_albedo
    single_albedo_AD = single_albedo_AD+RTV%Delta_Tau(KL) * s_AD
    optical_depth_AD = optical_depth_AD + Delta_Tau_AD/(TWO**RTV%Number_Doubling(KL))

  END SUBROUTINE CRTM_Doubling_layer_AD


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Phase_Matrix
!
! PURPOSE:
!       Subroutine to calculate the phase function for the scattering model.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Phase_Matrix( AtmOptics,  &  ! Input
!                               RTVariables )  ! Internal variable
!
! INPUT ARGUMENTS:
!       AtmOptics:      Structure containing the atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       RTVariables:    Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       RTVariables:    Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note that the INTENT on the RTVariables argument is IN OUT as it 
!       contains data prior to this call and is filled with data within this
!       routine.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Phase_Matrix( AtmOptics, &  ! Input
                                RTV        )  ! Internal variable



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_AtmScatter_type ),  INTENT( IN )     :: AtmOptics

    ! -- Internal variable
    TYPE( CRTM_RTVariables_type ), INTENT( IN OUT ) :: RTV


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Phase_Matrix'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, k, l



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTING LEGENDRE FUNCTION --                      #
    !#                   Pplus  for positive cosine of angles                   #
    !#                   Pminus for negative cosine of angles                   #
    !#--------------------------------------------------------------------------#

    DO i = 1, RTV%n_Angles 
       CALL Legendre(AtmOptics%n_Legendre_Terms,  RTV%COS_Angle(i), RTV%Pplus(0:,i) )
       CALL Legendre(AtmOptics%n_Legendre_Terms, -RTV%COS_Angle(i), RTV%Pminus(0:,i))
    END DO



    !#--------------------------------------------------------------------------#
    !#                    -- COMPUTE THE PHASE MATRICES --                      #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO  k = 1, RTV%n_Layers


      ! ------------------------------    
      ! Only proceed if the scattering    
      ! coefficient is significant        
      ! ------------------------------    

      Significant_Scattering: IF( AtmOptics%Single_Scatter_Albedo(k) > SCATTERING_ALBEDO_THRESHOLD) THEN

        DO j = 1, RTV%n_Angles
          DO i = 1, RTV%n_Angles

            RTV%Off(i,j,k)=ZERO
            RTV%Obb(i,j,k)=ZERO

            DO l = 0, AtmOptics%n_Legendre_Terms - 1
              RTV%Off(i,j,k) = RTV%Off(i,j,k) + ( AtmOptics%Phase_Coefficient(l,1,k)*RTV%Pplus(l,i)*RTV%Pplus(l,j) )
              RTV%Obb(i,j,k) = RTV%Obb(i,j,k) + ( AtmOptics%Phase_Coefficient(l,1,k)*RTV%Pplus(l,i)*RTV%Pminus(l,j) )
            END DO

            RTV%Pff(i,j,k) = RTV%Off(i,j,k)
            RTV%Pbb(i,j,k) = RTV%Obb(i,j,k)

            ! -- For intensity, the phase matrix element must >= ZERO             
            IF(RTV%Pff(i,j,k) < ZERO) RTV%Pff(i,j,k) = PHASE_THRESHOLD
            IF(RTV%Pbb(i,j,k) < ZERO) RTV%Pbb(i,j,k) = PHASE_THRESHOLD

          END DO
        END DO

        ! -- Normalization to ensure energy conservation
        CALL Normalize_Phase( k, RTV )
             
      END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_Phase_Matrix


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Phase_Matrix_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear phase function for the
!       scattering model.
!
! CATEGORY:
!       CRTM : RT Solution                                                 
!
! LANGUAGE:
!       Fortran-95                                                         
!
! CALLING SEQUENCE:                                                        
!       CALL CRTM_Phase_Matrix_TL( AtmOptics,    &  ! FWD Input
!                                  AtmOptics_TL, &  ! TL  Input
!                                  Pff_TL,       &  ! TL Output
!                                  Pff_TL,       &  ! TL Output
!                                  RTVariables   )  ! Internal variable
!
! INPUT ARGUMENTS:                                                         
!       AtmOptics:      Structure containing the atmospheric optical         
!                       parameters                                         
!                       UNITS:      N/A                                    
!                       TYPE:       CRTM_AtmScatter_type                     
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT( IN )                             
!
!       AtmOptics_TL:   Structure containing the tangent-linear atmospheric  
!                       optical parameters                                 
!                       UNITS:      N/A                                    
!                       TYPE:       CRTM_AtmScatter_type                     
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT( IN )                             
!
!       RTVariables:    Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.               
!                       UNITS:      N/A                                    
!                       TYPE:       CRTM_RTVariables_type                    
!                       DIMENSION:  Scalar                                 
!                       ATTRIBUTES: INTENT( IN )                             
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Pff_TL:         Array containing the tangent-linear of the 
!                       forward phase matrix.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT( OUT )
!
!       Pbb_TL:         Array containing the tangent-linear of the 
!                       backward phase matrix.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Phase_Matrix_TL( AtmOptics,    &  ! FWD Input
                                   AtmOptics_TL, &  ! TL  Input
                                   Pff_TL,       &  ! TL Output
                                   Pbb_TL,       &  ! TL Output
                                   RTV           )  ! Internal variable



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN ) :: AtmOptics
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN ) :: AtmOptics_TL

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: Pff_TL ! n_Angles x n_Angles x n_Layers
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: Pbb_TL ! n_Angles x n_Angles x n_Layers

    ! -- Internal variable
    TYPE( CRTM_RTVariables_type ),         INTENT( IN )  :: RTV

    
    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Phase_Matrix_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, k, l, nZ
    REAL( fp_kind ), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: Lff, Lbb



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE TANGENT-LINEAR PHASE MATRICES --              #
    !#--------------------------------------------------------------------------#

    nZ = RTV%n_Angles

    Layer_Loop: DO  k = 1, RTV%n_Layers
    
    
      ! ------------------------------    
      ! Only proceed if the scattering    
      ! coefficient is significant        
      ! ------------------------------    

      Significant_Scattering: IF( AtmOptics%Single_Scatter_Albedo(k) > SCATTERING_ALBEDO_THRESHOLD) THEN
            
        Lff(1:nZ,1:nZ) = RTV%Off(1:nZ,1:nZ,k)
        Lbb(1:nZ,1:nZ) = RTV%Obb(1:nZ,1:nZ,k)

        DO j = 1, RTV%n_Angles
          DO i = 1, RTV%n_Angles

            Pff_TL(i,j,k) = ZERO
            Pbb_TL(i,j,k) = ZERO    
                
            DO l = 0, AtmOptics%n_Legendre_Terms - 1
              Pff_TL(i,j,k) = Pff_TL(i,j,k) + ( AtmOptics_TL%Phase_Coefficient(l,1,k)*RTV%Pplus(l,i)*RTV%Pplus(l,j) )
              Pbb_TL(i,j,k) = Pbb_TL(i,j,k) + ( AtmOptics_TL%Phase_Coefficient(l,1,k)*RTV%Pplus(l,i)*RTV%Pminus(l,j) )
            END DO
              
            ! -- For intensity, the FWD phase matrix element must >= ZERO
            ! -- so the TL form is always == zero.
            IF ( RTV%Off(i,j,k) < ZERO ) THEN
              Pff_TL(i,j,k) = ZERO
              Lff(i,j)      = PHASE_THRESHOLD 
            END IF

            IF ( RTV%Obb(i,j,k) < ZERO ) THEN
              Pbb_TL(i,j,k) = ZERO
              Lbb(i,j) = PHASE_THRESHOLD 
            END IF
             
          END DO
        END DO

        ! -- Normalisation for energy conservation
        CALL Normalize_Phase_TL( k, RTV, &
                                 Lff,           & ! FWD Input
                                 Lbb,           & ! FWD Input
                                 Pff_TL(:,:,k), & ! TL  Output
                                 Pbb_TL(:,:,k)  ) ! TL  Output
             
      END IF Significant_Scattering
    
    END DO Layer_Loop

  END SUBROUTINE CRTM_Phase_Matrix_TL


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Phase_Matrix_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint of the phase function for the
!       scattering model.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL CRTM_Phase_Matrix_AD( AtmOptics,    &  ! FWD Input
!                                  Pff_AD,       &  ! AD Input
!                                  Pbb_AD,       &  ! AD Input
!                                  AtmOptics_AD, &  ! AD Output
!                                  RTVariables   )  ! Internal variable
!
! INPUT ARGUMENTS:
!       AtmOptics:      Structure containing the atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Pff_AD:         Array containing the adjoint of the 
!                       forward phase matrix.
!                       ** NOTE: This argument will be zeroed upon exit
!                       **       from this routine.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       Pbb_AD:         Array containing the adjoint of the 
!                       backward phase matrix.
!                       ** NOTE: This argument will be zeroed upon exit
!                       **       from this routine.
!                       UNITS:      N/A
!                       TYPE:       REAL
!                       DIMENSION:  Rank-3, n_Angles x n_Angles x n_Layers
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       RTVariables:    Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTVariables_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       AtmOptics_AD:   Structure containing the adjoint atmospheric optical
!                       parameters
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       There is no argument checking or structure allocation performed in
!       this subroutine.
!
! COMMENTS:
!       Note the INTENT on the output AtmOptics argument is IN OUT rather than
!       just OUT. This is necessary because the argument MUST be defined upon
!       input. To prevent memory leaks, and in this case errors in accessing
!       unallocated memory, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Phase_Matrix_AD( AtmOptics,    &  ! FWD Input
                                   Pff_AD,       &  ! AD Input
                                   Pbb_AD,       &  ! AD Input
                                   AtmOptics_AD, &  ! AD Output
                                   RTV           )  ! Internal variable
 


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: AtmOptics
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( IN OUT ) :: Pff_AD ! n_Angles x n_Angles x n_Layers
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( IN OUT ) :: Pbb_AD ! n_Angles x n_Angles x n_Layers

    ! -- Outputs
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN OUT ) :: AtmOptics_AD

    ! -- Internal variable
    TYPE( CRTM_RTVariables_type ),         INTENT( IN )     :: RTV


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Phase_Matrix_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, k, l, nZ
    REAL( fp_kind ), DIMENSION(RTV%n_Angles,RTV%n_Angles) :: Lff, Lbb



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE ADJOINT OF THE PHASE MATRICES --              #
    !#--------------------------------------------------------------------------#

    nZ = RTV%n_Angles

    Layer_Loop: DO  k = 1, RTV%n_Layers
    
    
      ! ------------------------------    
      ! Only proceed if the scattering    
      ! coefficient is significant        
      ! ------------------------------    

      Significant_Scattering: IF( AtmOptics%Single_Scatter_Albedo(k) > SCATTERING_ALBEDO_THRESHOLD) THEN


        ! -- AD normalization to ensure energy conservation
        AtmOptics_AD%Phase_Coefficient(0:,1,k) = ZERO
        
        Lff(1:nZ,1:nZ) = RTV%Off(1:nZ,1:nZ,k)
        Lbb(1:nZ,1:nZ) = RTV%Obb(1:nZ,1:nZ,k)

        DO j = 1, RTV%n_Angles
          DO i = 1, RTV%n_Angles
         
            ! -- For intensity, the FWD phase matrix element must >= ZERO
            ! -- so the TL, and thus the AD, for is always == zero.
            IF ( RTV%Off(i,j,k) < ZERO) Lff(i,j) = PHASE_THRESHOLD
            IF ( RTV%Obb(i,j,k) < ZERO) Lbb(i,j) = PHASE_THRESHOLD
          END DO
        END DO

        CALL Normalize_Phase_AD(k, RTV, &
                                Lff, Lbb,      & ! FWD Input
                                Pff_AD(:,:,k), & ! AD  Output
                                Pbb_AD(:,:,k)  ) ! AD  Output

        DO j = 1, RTV%n_Angles
          DO i = 1, RTV%n_Angles
         
            ! -- For intensity, the FWD phase matrix element must >= ZERO
            ! -- so the TL, and thus the AD, for is always == zero.
            IF ( RTV%Off(i,j,k) < ZERO) Pff_AD(i,j,k) = ZERO
            IF ( RTV%Obb(i,j,k) < ZERO) Pbb_AD(i,j,k) = ZERO

            DO l = 0, AtmOptics%n_Legendre_Terms - 1
               AtmOptics_AD%Phase_Coefficient(l,1,k) = AtmOptics_AD%Phase_Coefficient(l,1,k) + &
                                                       ( Pff_AD(i,j,k)*RTV%Pplus(l,i)*RTV%Pplus(l,j) )
               AtmOptics_AD%Phase_Coefficient(l,1,k) = AtmOptics_AD%Phase_Coefficient(l,1,k) + &
                                                       ( Pbb_AD(i,j,k)*RTV%Pplus(l,i)*RTV%Pminus(l,j) ) 
            END DO

            Pff_AD(i,j,k) = ZERO
            Pbb_AD(i,j,k) = ZERO           

          END DO
        END DO

      END IF Significant_Scattering

    END DO Layer_Loop

  END SUBROUTINE CRTM_Phase_Matrix_AD


  SUBROUTINE Normalize_Phase( k, RTV )
    INTEGER,                       INTENT( IN )     :: k
    TYPE( CRTM_RTVariables_type ), INTENT( IN OUT ) :: RTV
    INTEGER :: i,j, nZ                                           

    nZ = RTV%n_Angles


    ! -------------------------------
    ! Normalisation for stream angles
    ! -------------------------------

    RTV%Sum_Fac(0,k)=ZERO

    DO i = 1, nZ-1
      RTV%n_Factor(i,k)=ZERO
      DO j = i,nZ
        RTV%n_Factor(i,k)=RTV%n_Factor(i,k)+(RTV%Pff(i,j,k)+RTV%Pbb(i,j,k))*RTV%COS_Weight(j)
      END DO

      DO j=i,nZ
        RTV%Pff(i,j,k)=RTV%Pff(i,j,k)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
        RTV%Pbb(i,j,k)=RTV%Pbb(i,j,k)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
      END DO
      
      DO j=i,nZ
        RTV%Pff(j,i,k)=RTV%Pff(i,j,k)
        RTV%Pbb(j,i,k)=RTV%Pbb(i,j,k)
      END DO

      RTV%Sum_Fac(i,k)=ZERO
      IF(i < nZ) THEN
        DO j = 1, i
          RTV%Sum_Fac(i,k)=RTV%Sum_Fac(i,k) + (RTV%Pff(i+1,j,k)+RTV%Pbb(i+1,j,k))*RTV%COS_Weight(j)
        END DO
      END IF

    END DO


    ! -------------------------------------
    ! Normalisation for sensor zenith angle
    ! -------------------------------------

    RTV%n_Factor(nZ,k) = ZERO                               
    DO j = 1, nZ-1                               
      RTV%n_Factor(nZ,k)=RTV%n_Factor(nZ,k)+(RTV%Pff(nZ,j,k)+RTV%Pbb(nZ,j,k))*RTV%COS_Weight(j)    
    END DO                                      

    DO j = 1, nZ                
      RTV%Pff(nZ,j,k)=RTV%Pff(nZ,j,k)/RTV%n_Factor(nZ,k)    
      RTV%Pbb(nZ,j,k)=RTV%Pbb(nZ,j,k)/RTV%n_Factor(nZ,k)    
    END DO      
                    
    DO j = 1, nZ                
      RTV%Pff(j,nZ,k)=RTV%Pff(nZ,j,k)          
      RTV%Pbb(j,nZ,k)=RTV%Pbb(nZ,j,k)          
    END DO                     

  END SUBROUTINE Normalize_Phase


  SUBROUTINE Normalize_Phase_TL( k, RTV, Pff, Pbb, Pff_TL, Pbb_TL )
    INTEGER,                           INTENT( IN )     :: k
    TYPE( CRTM_RTVariables_type ),     INTENT( IN )     :: RTV
    REAL( fp_kind ), DIMENSION( :,: ), INTENT( IN )     :: Pff, Pbb
    REAL( fp_kind ), DIMENSION( :,: ), INTENT( IN OUT ) :: Pff_TL, Pbb_TL
    REAL( fp_kind ) :: n_Factor, n_Factor_TL
    
    REAL( fp_kind ), DIMENSION( 0: RTV%n_Angles ) :: Sum_Fac_TL

    INTEGER :: i,j, nZ                                           

    nZ = RTV%n_Angles
 

    ! -------------------------------
    ! Normalisation for stream angles
    ! -------------------------------

    Sum_Fac_TL(0) = ZERO

    DO i = 1, nZ-1
      n_Factor_TL = ZERO
      DO j = i,nZ
        n_Factor_TL=n_Factor_TL+(Pff_TL(i,j)+Pbb_TL(i,j))*RTV%COS_Weight(j)
      END DO

      DO j=i,nZ
      
        Pff_TL(i,j)=Pff_TL(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))  &
                   -Pff(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k)*n_Factor_TL*(ONE-RTV%Sum_Fac(i-1,k))  &
                   -Pff(i,j)/RTV%n_Factor(i,k)*Sum_Fac_TL(i-1)
        
        Pbb_TL(i,j)=Pbb_TL(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))  &
                   -Pbb(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k)*n_Factor_TL*(ONE-RTV%Sum_Fac(i-1,k))  &
                   -Pbb(i,j)/RTV%n_Factor(i,k)*Sum_Fac_TL(i-1)

      END DO

      Sum_Fac_TL(i)=ZERO
 
        DO j = 1, i
          Sum_Fac_TL(i)=Sum_Fac_TL(i) + (Pff_TL(j,i+1)+Pbb_TL(j,i+1))*RTV%COS_Weight(j)
        END DO
       
    END DO

    IF( RTV%n_Streams == nZ ) THEN 
      ! Sensor viewing angle can be represented by one of the Gaussian angles
      n_Factor_TL = (Pff_TL(nZ,nZ) + Pbb_TL(nZ,nZ) )*RTV%COS_Weight(nZ)  
          
      Pff_TL(nZ,nZ) = Pff_TL(nZ,nZ)/RTV%n_Factor(nZ,k)*(ONE-RTV%Sum_Fac(nZ-1,k)) &
                    - Pff(nZ,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*n_Factor_TL*(ONE-RTV%Sum_Fac(nZ-1,k)) &
                    - Pff(nZ,nZ)/RTV%n_Factor(nZ,k)*Sum_Fac_TL(nZ-1)

      Pbb_TL(nZ,nZ) = Pbb_TL(nZ,nZ)/RTV%n_Factor(nZ,k)*(ONE-RTV%Sum_Fac(nZ-1,k)) &
                    - Pbb(nZ,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*n_Factor_TL*(ONE-RTV%Sum_Fac(nZ-1,k)) &
                    - Pbb(nZ,nZ)/RTV%n_Factor(nZ,k)*Sum_Fac_TL(nZ-1)
  
    ELSE
      ! Sensor viewing angle differs from the Gaussian angles
      n_Factor_TL = ZERO
      DO j = 1,nZ-1
        n_Factor_TL = n_Factor_TL+(Pff_TL(j,nZ)+Pbb_TL(j,nZ))*RTV%COS_Weight(j)
      END DO

      DO j = 1, nZ
      
        Pff_TL(j,nZ) = Pff_TL(j,nZ)/RTV%n_Factor(nZ,k)  &
                     - Pff(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*n_Factor_TL
                     
        Pbb_TL(j,nZ) = Pbb_TL(j,nZ)/RTV%n_Factor(nZ,k)  &
                     - Pbb(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*n_Factor_TL             

      END DO
      
    END IF


    ! -------------------
    ! Symmetric condition
    ! -------------------
                 
    DO i = 1, nZ
    
      DO j = i, nZ
        IF( i /= j ) THEN
          Pff_TL(j,i) = Pff_TL(i,j)
          Pbb_TL(j,i) = Pbb_TL(i,j)
        END IF
      END DO
      
    ENDDO
                     

  END SUBROUTINE Normalize_Phase_TL


  SUBROUTINE Normalize_Phase_AD( k, RTV, Pff, Pbb, Pff_AD, Pbb_AD )
    INTEGER,                           INTENT( IN )     :: k
    TYPE( CRTM_RTVariables_type ),     INTENT( IN )     :: RTV
    REAL( fp_kind ), DIMENSION( :,: ), INTENT( IN )     :: Pff, Pbb
    REAL( fp_kind ), DIMENSION( :,: ), INTENT( IN OUT ) :: Pff_AD, Pbb_AD

    INTEGER :: i,j, nZ                                           
    REAL( fp_kind ) :: n_Factor, n_Factor_AD
    REAL( fp_kind ), DIMENSION( 0: RTV%n_Angles ) :: Sum_Fac_AD


    nZ = RTV%n_Angles
    Sum_Fac_AD = ZERO

    
    ! -------------------
    ! Symmetric condition
    ! -------------------

    DO i = nZ, 1, -1
      DO j = nZ, i, -1
        IF( i /= j ) THEN
       
          Pff_AD(i,j) = Pff_AD(i,j) + Pff_AD(j,i)
          Pbb_AD(i,j) = Pbb_AD(i,j) + Pbb_AD(j,i)
          Pff_AD(j,i) = ZERO
          Pbb_AD(j,i) = ZERO
          
        END IF
      END DO
    ENDDO


    n_Factor_AD = ZERO

    IF( RTV%n_Streams == nZ ) THEN 

      ! Sensor viewing angle can be represented by one of the Gaussian angles

      Sum_Fac_AD(nZ-1) = Sum_Fac_AD(nZ-1) - Pbb(nZ,nZ)/RTV%n_Factor(nZ,k)*Pbb_AD(nZ,nZ)
      n_Factor_AD = n_Factor_AD - Pbb(nZ,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*Pbb_AD(nZ,nZ) &
                  * (ONE-RTV%Sum_Fac(nZ-1,k))
      Pbb_AD(nZ,nZ) = Pbb_AD(nZ,nZ)/RTV%n_Factor(nZ,k)*(ONE-RTV%Sum_Fac(nZ-1,k))
      
      Sum_Fac_AD(nZ-1) = Sum_Fac_AD(nZ-1) - Pff(nZ,nZ)/RTV%n_Factor(nZ,k)*Pff_AD(nZ,nZ)
      n_Factor_AD = n_Factor_AD - Pff(nZ,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*Pff_AD(nZ,nZ) &
                  * (ONE-RTV%Sum_Fac(nZ-1,k))
      Pff_AD(nZ,nZ) = Pff_AD(nZ,nZ)/RTV%n_Factor(nZ,k)*(ONE-RTV%Sum_Fac(nZ-1,k))
      
      Pbb_AD(nZ,nZ) = Pbb_AD(nZ,nZ) + n_Factor_AD* RTV%COS_Weight(nZ)
      Pff_AD(nZ,nZ) = Pff_AD(nZ,nZ) + n_Factor_AD* RTV%COS_Weight(nZ)
      
    ELSE
      ! Sensor viewing angle diffs from the Gaussian angles
      
      DO j = nZ, 1, -1
      
        n_Factor_AD = n_Factor_AD - Pff(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*Pff_AD(j,nZ)
        Pff_AD(j,nZ) = Pff_AD(j,nZ)/RTV%n_Factor(nZ,k)
                     
        n_Factor_AD = n_Factor_AD - Pbb(j,nZ)/RTV%n_Factor(nZ,k)/RTV%n_Factor(nZ,k)*Pbb_AD(j,nZ)
        Pbb_AD(j,nZ) = Pbb_AD(j,nZ)/RTV%n_Factor(nZ,k)             
        
      END DO   
      
      
      DO j = nZ-1, 1, -1
        
        Pff_AD(j,nZ) = Pff_AD(j,nZ) + n_Factor_AD*RTV%COS_Weight(j)
        Pbb_AD(j,nZ) = Pbb_AD(j,nZ) + n_Factor_AD*RTV%COS_Weight(j)
        
      END DO

      n_Factor_AD = ZERO
      
    END IF
    
    DO i = nZ-1, 1, -1 
 
      DO j = i, 1, -1
       
        Pbb_AD(j,i+1) = Pbb_AD(j,i+1) + Sum_Fac_AD(i)*RTV%COS_Weight(j)
        Pff_AD(j,i+1) = Pff_AD(j,i+1) + Sum_Fac_AD(i)*RTV%COS_Weight(j)
        
      END DO
       
      Sum_Fac_AD(i) = ZERO
    
      DO j = nZ, i, -1
      
        Sum_Fac_AD(i-1) = Sum_Fac_AD(i-1) - Pbb(i,j)/RTV%n_Factor(i,k)*Pbb_AD(i,j)
        n_Factor_AD = n_Factor_AD -Pbb(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k) &
                    * Pbb_AD(i,j)*(ONE-RTV%Sum_Fac(i-1,k))
        Pbb_AD(i,j) = Pbb_AD(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
        
        Sum_Fac_AD(i-1) = Sum_Fac_AD(i-1) - Pff(i,j)/RTV%n_Factor(i,k)*Pff_AD(i,j)
        n_Factor_AD = n_Factor_AD -Pff(i,j)/RTV%n_Factor(i,k)/RTV%n_Factor(i,k) &
                    * Pff_AD(i,j)*(ONE-RTV%Sum_Fac(i-1,k))
        Pff_AD(i,j) = Pff_AD(i,j)/RTV%n_Factor(i,k)*(ONE-RTV%Sum_Fac(i-1,k))
        
      END DO
         
      DO j = nZ, i, -1
        
        Pbb_AD(i,j) = Pbb_AD(i,j) + n_Factor_AD*RTV%COS_Weight(j)
        Pff_AD(i,j) = Pff_AD(i,j) + n_Factor_AD*RTV%COS_Weight(j)
        
      END DO
      
      n_Factor_AD = ZERO

    END DO    
    
    Sum_Fac_AD(0) = ZERO
 
  END SUBROUTINE Normalize_Phase_AD



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
!       CRTM_Compute_RTSolution
!
! PURPOSE:
!       Function to solve the radiative transfer equation.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_RTSolution( Atmosphere,     &  ! Input
!                                               Surface,        &  ! Input
!                                               AtmOptics,      &  ! Input
!                                               SfcOptics,      &  ! Input
!                                               GeometryInfo,   &  ! Input
!                                               Channel_Index,  &  ! Input
!                                               RTSolution,     &  ! Output
!                                               RTVariables,    &  ! Internal variable output
!                                               Message_Log )   &  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AtmOptics:      Structure containing the combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:      Structure containing the surface optical properties
!                       data. Argument is defined as INTENT (IN OUT ) as
!                       different RT algorithms may compute the surface
!                       optics properties before this routine is called.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:  Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       RTVariables:    Structure containing internal variables required for
!                       subsequent tangent-linear or adjoint model calls.
!                       The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
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
!  
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument is defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_RTSolution( Atmosphere,     &  ! Input
                                    Surface,        &  ! Input
                                    AtmOptics,      &  ! Input
                                    SfcOptics,      &  ! Input
                                    GeometryInfo,   &  ! Input
                                    Channel_Index,  &  ! Input
                                    RTSolution,     &  ! Output
                                    RTV,            &  ! Internal variable output
                                    Message_Log )   &  ! Error messaging
                                  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN )     :: AtmOptics 
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- Output 
    TYPE( CRTM_RTSolution_type ),   INTENT( IN OUT ) :: RTSolution

    ! -- Internal variable output
    TYPE( CRTM_RTVariables_type ),  INTENT( IN OUT )    :: RTV

    ! -- Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    INTEGER :: i, k, nZ
    REAL( fp_kind ) :: u       ! COS( sensor zenith angle )
    REAL( fp_kind ) :: Factor  ! SfcOptics quadrature weights normalisation factor
    REAL( fp_kind ) :: User_Emissivity, Direct_Reflectivity



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                       -- ASSIGN SOME VALUES --                           #
    !#--------------------------------------------------------------------------#

    ! -- Short named local copies
    u             = ONE / GeometryInfo%Secant_Sensor_Zenith

    ! -- Populate the RTV structure
    RTV%n_Streams = RTSolution%n_Full_Streams/2
    RTV%n_Layers  = Atmosphere%n_Layers

    ! -- Save the optical depth if required
    IF ( ASSOCIATED( RTSolution%Layer_Optical_Depth ) ) THEN
      RTSolution%Layer_Optical_Depth( 1:RTV%n_Layers ) = AtmOptics%Optical_Depth( 1:RTV%n_Layers )
    END IF



    !#--------------------------------------------------------------------------#
    !#   -- DETERMINE THE SURFACE EMISSION BEHAVIOR (SPECULAR OR DIFFUSE) --    #
    !#                                                                          #
    !#   The surface is specular if microwave sensor                            #
    !#--------------------------------------------------------------------------#

    !IF( Surface%Water_Coverage          >= POINT_5          .AND. &
    !    SC%Sensor_Type( Channel_Index ) == MICROWAVE_SENSOR       ) THEN

    IF( SC%Sensor_Type( Channel_Index ) == MICROWAVE_SENSOR       ) THEN

      RTV%Diffuse_Surface = .FALSE.

    ELSE

      RTV%Diffuse_Surface = .TRUE.

    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DETERMINE THE QUADRATURE ANGLES AND WEIGHTS --             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Default is to assume scattering RT
    ! ----------------------------------

    RTV%Scattering_RT = .FALSE.


    ! -------------------------------
    ! Check for scattering conditions
    ! -------------------------------

    Determine_Stream_Angles: IF( RTSolution%Scattering_FLAG .AND. &
                                 MAXVAL(AtmOptics%Single_Scatter_Albedo) >= SCATTERING_ALBEDO_THRESHOLD ) THEN 



      !#------------------------------------------------------------------------#
      !#                   -- MULTIPLE STREAMS FOR SCATTERING --                #
      !#------------------------------------------------------------------------#

      ! --------------------------
      ! Set the scattering RT flag
      ! --------------------------

      RTV%Scattering_RT = .TRUE.


      ! ---------------------------------------
      ! Compute the Gaussian angles and weights
      ! ---------------------------------------

      CALL Double_Gauss_Quadrature( RTV%n_Streams, RTV%COS_Angle, RTV%COS_Weight )


      ! ----------------------------------------------------------------
      ! Is an additional stream required for the satellite zenith angle?
      ! That is, is the satellite zenith angle sufficiently different
      ! from the stream angles?
      ! ----------------------------------------------------------------

      ! -- Search for a matching angle
      DO i = 1, RTV%n_Streams
        IF( ABS( RTV%COS_Angle(i) - u ) < ANGLE_THRESHOLD ) THEN
          RTSolution%Index_Sat_Ang = i         ! Identify the sensor zenith angle
          RTV%n_Angles = RTV%n_Streams
          GO TO 100
        END IF
      END DO

      ! -- No match found, so flag an additional
      ! -- spot for the satellite zenith angle
      RTV%n_Angles = RTV%n_Streams + 1
      RTSolution%Index_Sat_Ang   = RTV%n_Angles      ! Identify the sensor zenith angle
      RTV%COS_Angle( RTV%n_Angles )  = u
      RTV%COS_Weight( RTV%n_Angles ) = ZERO

      100 CONTINUE


      ! ------------------------
      ! Compute the phase matrix
      ! ------------------------

      CALL CRTM_Phase_Matrix( AtmOptics, & ! Input
                              RTV        ) ! Internal variable


    ELSE IF( RTV%Diffuse_Surface) THEN



      !#------------------------------------------------------------------------#
      !#                      -- LAMBERTIAN SURFACE --                          #
      !#------------------------------------------------------------------------#

      ! -- The default diffusivity angle
      RTV%n_Streams       = 1
      RTV%COS_Angle( 1 )  = ONE/SECANT_DIFFUSIVITY
      RTV%COS_Weight( 1 ) = ONE

      ! -- The satellite zenith angle
      RTV%n_Angles                   = RTV%n_Streams + 1
      RTSolution%Index_Sat_Ang       = RTV%n_Angles      ! Identify the sensor zenith angle
      RTV%COS_Angle( RTV%n_Angles )  = u
      RTV%COS_Weight( RTV%n_Angles ) = ZERO


    ELSE


      !#------------------------------------------------------------------------#
      !#                       -- SPECULAR SURFACE --                           #
      !#------------------------------------------------------------------------#

      RTV%n_Streams = 0
      RTV%n_Angles  = 1
      RTV%COS_Angle( RTV%n_Angles ) = u
      RTSolution%Index_Sat_Ang      = RTV%n_Angles

    END IF Determine_Stream_Angles


    ! -------------------------------------------------
    ! Copy the RTV angles to a short name for use below
    ! -------------------------------------------------

    nZ = RTV%n_Angles



    !#--------------------------------------------------------------------------#
    !#                -- PERFORM THE SURFACE OPTICS COMPUTATION --              #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------------------------
    ! Assign the number of Stokes parameters. Currently, this is set to 1
    ! for decoupled polarization between surface and atmosphere.
    ! Remember for polarised microwave instruments, each frequency's Stokes
    ! parameter is treated as a separate channel.
    ! ---------------------------------------------------------------------

    SfcOptics%n_Stokes = 1

    
    ! ----------------------------------------------------
    ! Assign the angles and weights to SfcOptics structure
    ! ----------------------------------------------------

    SfcOptics%n_Angles = RTV%n_Angles
    Factor = ZERO

    DO i = 1, nZ
      SfcOptics%Angle(i)  = ACOS( RTV%COS_Angle(i) ) / DEGREES_TO_RADIANS
      SfcOptics%Weight(i) = RTV%COS_Angle(i) * RTV%COS_Weight(i)

      ! -- Compute the weight normalisation factor
      Factor = Factor + SfcOptics%Weight(i)

    END DO

    ! -- Normalise the quadrature weights                                               
    IF( Factor > ZERO) SfcOptics%Weight(1:nZ) = SfcOptics%Weight(1:nZ) / &
    !                                           ----------------------    
                                                        Factor


    ! --------------------------------
    ! Populate the SfcOptics structure
    ! --------------------------------

    SfcOptics%Index_Sat_Ang = RTSolution%Index_Sat_Ang

    IF ( SfcOptics%Compute_Switch == SET ) THEN

      ! -- Compute the values
      Error_Status = CRTM_Compute_SfcOptics( Surface,                  & ! Input
                                             GeometryInfo,             & ! Input
                                             Channel_Index,            & ! Input
                                             SfcOptics,                & ! In/Output
                                             Message_Log = Message_Log ) ! Error messaging
                                                                                        
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing SfcOptics for ", i4 )' ) &
                        Channel_Index
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE

      IF( RTV%Scattering_RT ) THEN
        ! -- Replicate the user emissivity for all angles
        SfcOptics%Reflectivity = ZERO
        User_Emissivity = SfcOptics%Emissivity(1,1)
        SfcOptics%Emissivity(1,1) = ZERO

        Direct_Reflectivity = SfcOptics%Direct_Reflectivity(1,1)/PI
       
        SfcOptics%Emissivity(1:nZ,1) = User_Emissivity

        ! -- Replicate the user reflectivities for all angles
        SfcOptics%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity

        IF( RTV%Diffuse_Surface) THEN
          DO i = 1, nZ 
            SfcOptics%Reflectivity(1:nZ, 1, i, 1) = (ONE-SfcOptics%Emissivity(i,1))*SfcOptics%Weight(i)   
          END DO
        ELSE ! Specular surface
          DO i = 1, nZ 
            SfcOptics%Reflectivity(i, 1, i, 1) = (ONE-SfcOptics%Emissivity(i,1))
          END DO
        END IF

      ELSE
        User_Emissivity = SfcOptics%Emissivity(1,1)
        SfcOptics%Emissivity( RTSolution%Index_Sat_Ang,1 ) = User_Emissivity
        SfcOptics%Reflectivity(1,1,1,1) = ONE - User_Emissivity

      END IF

    END IF


    ! ------------------------------------------------------
    ! Save the emissivity in the RTSolution output structure
    ! ------------------------------------------------------

    RTSolution%Surface_Emissivity = SfcOptics%Emissivity( RTSolution%Index_Sat_Ang, 1 )



    !#--------------------------------------------------------------------------#
    !#                     -- COMPUTE PLANCK RADIANCES --                       #
    !#--------------------------------------------------------------------------#

    ! -- Atmospheric layer radiances
    DO k = 1, Atmosphere%n_Layers 
      CALL CRTM_Planck_Radiance( Channel_Index,                   & ! Input
                                 Atmosphere%Temperature(k),       & ! Input
                                 RTV%Planck_Atmosphere(k)         ) ! Output
    END DO

    ! -- Surface radiance
    CALL CRTM_Planck_Radiance( Channel_Index,                 & ! Input
                               SfcOptics%Surface_Temperature, & ! Input
                               RTV%Planck_Surface             ) ! Output




    !#--------------------------------------------------------------------------#
    !#                   -- PERFORM THE RADIATIVE TRANSFER --                   #
    !#--------------------------------------------------------------------------#

    ! -- Select the RT model
    IF( RTV%Scattering_RT ) THEN


      ! -----------------------------------------------------
      ! Scattering RT. NESDIS advanced adding-doubling method 
      ! -----------------------------------------------------

      CALL CRTM_ADA( Atmosphere%n_Layers,                          & ! Input, number of atmospheric layers
                     AtmOptics%Single_Scatter_Albedo,              & ! Input, layer single scattering albedo
                     AtmOptics%Asymmetry_Factor,                   & ! Input, layer asymmetry factor
                     AtmOptics%Optical_Depth,                      & ! Input, layer optical depth
                     SC%Cosmic_Background_Radiance(Channel_Index), & ! Input, cosmic background radiation
                     SfcOptics%Emissivity( 1:nZ, 1 ),              & ! Input, surface emissivity
                     SfcOptics%Reflectivity( 1:nZ, 1, 1:nZ, 1 ),   & ! Input, surface reflectivity
                     RTV                                           ) ! Output, Internal variables

      ! -- The output radiance
      RTSolution%Radiance = RTV%s_Level_Rad_UP( RTSolution%Index_Sat_Ang, 0 )
 
    ELSE


      ! -----------------
      ! Emission model RT
      ! -----------------

      CALL CRTM_Emission( Atmosphere%n_Layers,                          & ! Input, number of atmospheric layers
                          RTV%n_Angles,                                 & ! Input, number of discrete zenith angles
                          RTV%Diffuse_Surface,                          & ! Input, surface behavior
                          u,                                            & ! Input, cosine of sensor zenith angle
                          AtmOptics%Optical_Depth,                      & ! Input, layer optical depth
                          RTV%Planck_Atmosphere,                        & ! Input, layer radiances
                          RTV%Planck_Surface,                           & ! Input, surface radiance
                          SfcOptics%Emissivity( 1:nZ, 1 ),              & ! Input, surface emissivity
                          SfcOptics%Reflectivity( 1:nZ, 1, 1:nZ, 1 ),   & ! Input, surface reflectivity                
                          SfcOptics%Direct_Reflectivity( 1:nZ, 1 ),     & ! Input, surface reflectivity for a point source
                          SC%Cosmic_Background_Radiance(Channel_Index), & ! Input, cosmic background radiation 
                          SC%Solar_Irradiance(Channel_Index),           & ! Input, Source irradiance at TOA
                          SC%Is_Solar_Channel(Channel_Index),           & ! Input, Source sensitive channel info.
                          GeometryInfo%Source_Zenith_Radian,            & ! Input, Source zenith angle
                          RTV                                           ) ! Output, Internal variables

      ! -- The output radiance
      RTSolution%Radiance = RTV%e_Level_Rad_UP(0)

    END IF



    !#--------------------------------------------------------------------------#
    !#          -- COMPUTE THE CORRESPONDING BRIGHTNESS TEMPERATURE --          #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Planck_Temperature( Channel_Index,                    & ! Input
                                  RTSolution%Radiance,              & ! Input
                                  RTSolution%Brightness_Temperature ) ! Output

  END FUNCTION CRTM_Compute_RTSolution





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_RTSolution_TL
!
! PURPOSE:
!       Function to solve the tangent-linear radiative transfer equation.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!      Error_Status = CRTM_Compute_RTSolution_TL( Atmosphere,               &  ! FWD Input
!                                                 Surface,                  &  ! FWD Input
!                                                 AtmOptics,                &  ! FWD Input
!                                                 SfcOptics,                &  ! FWD Input
!                                                 RTSolution,               &  ! FWD Input
!                                                 Atmosphere_TL,            &  ! TL Input
!                                                 Surface_TL,               &  ! TL Input
!                                                 AtmOptics_TL,             &  ! TL Input
!                                                 SfcOptics_TL,             &  ! TL Input 
!                                                 GeometryInfo,             &  ! Input
!                                                 Channel_Index,            &  ! Input
!                                                 RTSolution_TL,            &  ! TL Output
!                                                 RTVariables,              &  ! Internal variable input
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AtmOptics:      Structure containing the combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:      Structure containing the surface optical properties
!                       data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:  Structure containing the tangent-linear atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface_TL:     Structure containing the tangent-linear surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AtmOptics_TL:   Structure containing the tangent-linear atmospheric  
!                       optical properties.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       SfcOptics_TL:   Structure containing the tangent-linear surface optical
!                       properties. Argument is defined as INTENT (IN OUT ) as
!                       different RT algorithms may compute the surface optics
!                       properties before this routine is called.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT)
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:  Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       RTVariables:    Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUT ARGUMENTS:
!       RTSolution_TL:  Structure containing the solution to the tangent-linear
!                       RT equation for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output RTSolution_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_RTSolution_TL( Atmosphere,     &  ! FWD Input
                                       Surface,        &  ! FWD Input
                                       AtmOptics,      &  ! FWD Input
                                       SfcOptics,      &  ! FWD Input
                                       RTSolution,     &  ! FWD Input
                                       Atmosphere_TL,  &  ! TL Input
                                       Surface_TL,     &  ! TL Input
                                       AtmOptics_TL,   &  ! TL Input
                                       SfcOptics_TL,   &  ! TL Input 
                                       GeometryInfo,   &  ! Input
                                       Channel_Index,  &  ! Input     
                                       RTSolution_TL,  &  ! TL Output
                                       RTV,            &  ! Internal variable input
                                       Message_Log )   &  ! Error messaging
                                     RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- FWD Input
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN )     :: AtmOptics 
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN )     :: SfcOptics
    TYPE( CRTM_RTSolution_type ),   INTENT( IN )     :: RTSolution

    ! -- TL Input
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere_TL
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface_TL 
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN )     :: AtmOptics_TL
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics_TL

    ! -- Other input
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- TL Output
    TYPE( CRTM_RTSolution_type ),   INTENT( IN OUT ) :: RTSolution_TL

    ! -- Internal variable input
    TYPE( CRTM_RTVariables_type ),  INTENT( IN )     :: RTV

    ! -- Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    INTEGER :: i, k, nZ
    REAL( fp_kind ) :: u       ! COS( sensor zenith angle )
    REAL( fp_kind ) :: User_Emissivity_TL, Direct_Reflectivity_TL
    REAL( fp_kind )                                     :: Planck_Surface_TL    ! Surface TL radiance
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers ) :: Planck_Atmosphere_TL ! *LAYER* TL radiances


    ! -- The following variables are RT model specific
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                Atmosphere%n_Layers ) :: Pff_TL ! Forward scattering TL phase matrix

    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                Atmosphere%n_Layers ) :: Pbb_TL ! Backward scattering TL phase matrix

    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES ) :: Scattering_Radiance_TL



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- ASSIGN SOME VALUES --                          #
    !#--------------------------------------------------------------------------#

    ! -- Short named local varibale copies
    u  = ONE / GeometryInfo%Secant_Sensor_Zenith
    nz = RTV%n_Angles

    ! -- Set the sensor zenith angle index in RTSolution_TL
    RTSolution_TL%Index_Sat_Ang = RTSolution%Index_Sat_Ang

    ! -- Save the TL optical depth if required
    IF ( ASSOCIATED( RTSolution_TL%Layer_Optical_Depth ) ) THEN
      RTSolution_TL%Layer_Optical_Depth( 1:RTV%n_Layers ) = &
        AtmOptics_TL%Optical_Depth( 1:RTV%n_Layers )
    END IF




    !#--------------------------------------------------------------------------#
    !#        -- COMPUTE THE TANGENT-LINEAR PHASE MATRIX IF REQUIRED --         #
    !#--------------------------------------------------------------------------#

    IF ( RTV%Scattering_RT ) THEN

      CALL CRTM_Phase_Matrix_TL( AtmOptics,    & ! Input
                                 AtmOptics_TL, & ! Input
                                 Pff_TL,       & ! Output - TL forward scattering
                                 Pbb_TL,       & ! Output - TL backward scattering
                                 RTV           ) ! Internal variable

    END IF



    !#--------------------------------------------------------------------------#
    !#                -- PERFORM THE SURFACE OPTICS COMPUTATION --              #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------------------------
    ! Assign the number of Stokes parameters. Currently, this is set to 1
    ! for decoupled polarization between surface and atmosphere.
    ! Remember for polarised microwave instruments, each frequency's Stokes
    ! parameter is treated as a separate channel.
    ! ---------------------------------------------------------------------

    SfcOptics_TL%n_Stokes = 1

    
    ! ----------------------------------------------------
    ! Assign the angles and weights to SfcOptics structure
    ! ----------------------------------------------------

    SfcOptics_TL%n_Angles = SfcOptics%n_Angles
    SfcOptics_TL%Angle    = SfcOptics%Angle 
    SfcOptics_TL%Weight   = SfcOptics%Weight


    ! -----------------------------------------
    ! Populate the SfcOptics_TL structure based
    ! on FORWARD model SfcOptics Compute_Switch
    ! -----------------------------------------

    IF ( SfcOptics%Compute_Switch == SET ) THEN

      ! -- Compute the values
      Error_Status = CRTM_Compute_SfcOptics_TL( Surface,                  & ! Input
                                                SfcOptics,                & ! Input
                                                Surface_TL,               & ! Input
                                                GeometryInfo,             & ! Input
                                                Channel_Index,            & ! Input
                                                SfcOptics_TL,             & ! In/Output
                                                Message_Log = Message_Log ) ! Error messaging
                                               
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing SfcOptics_TL for ", i4 )' ) &
                        Channel_Index 
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE


      IF( RTV%Scattering_RT ) THEN
        ! -- Replicate the user emissivity for all angles
        SfcOptics_TL%Reflectivity = ZERO
        User_Emissivity_TL = SfcOptics_TL%Emissivity(1,1)
        SfcOptics_TL%Emissivity(1,1) = ZERO

        Direct_Reflectivity_TL = SfcOptics_TL%Direct_Reflectivity(1,1)/PI
       
        SfcOptics_TL%Emissivity(1:nZ,1) = User_Emissivity_TL

        ! -- Replicate the user reflectivities for all angles
        SfcOptics_TL%Direct_Reflectivity(1:nZ,1) = Direct_Reflectivity_TL

        IF( RTV%Diffuse_Surface) THEN
          DO i = 1, nZ 
            SfcOptics_TL%Reflectivity(1:nZ, 1, i, 1) = -SfcOptics_TL%Emissivity(i,1)*SfcOptics%Weight(i)   
          END DO
        ELSE ! Specular surface
          DO i = 1, nZ 
            SfcOptics_TL%Reflectivity(i, 1, i, 1) = -SfcOptics_TL%Emissivity(i,1)
          END DO
        END IF

      ELSE
        User_Emissivity_TL = SfcOptics_TL%Emissivity(1,1)
        SfcOptics_TL%Emissivity( RTSolution%Index_Sat_Ang,1 ) = User_Emissivity_TL
        SfcOptics_TL%Reflectivity(1,1,1,1) = - User_Emissivity_TL

      END IF




    END IF


    ! ------------------------------------------------------------
    ! Save the TL emissivity in the RTSolution_TL output structure
    ! ------------------------------------------------------------

    RTSolution_TL%Surface_Emissivity = SfcOptics_TL%Emissivity( RTSolution_TL%Index_Sat_Ang, 1 )



    !#--------------------------------------------------------------------------#
    !#            -- COMPUTE THE TANGENT-LINEAR PLANCK RADIANCES --             #
    !#--------------------------------------------------------------------------#

    ! -- Atmospheric layer TL radiances
    DO k = 1, Atmosphere%n_Layers 
      CALL CRTM_Planck_Radiance_TL( Channel_Index,                      & ! Input
                                    Atmosphere%Temperature(k),          & ! Input
                                    Atmosphere_TL%Temperature(k),       & ! Input
                                    Planck_Atmosphere_TL(k)             ) ! Output
    END DO

    ! -- Surface TL radiance
    CALL CRTM_Planck_Radiance_TL( Channel_Index,                    & ! Input
                                  SfcOptics%Surface_Temperature,    & ! Input
                                  SfcOptics_TL%Surface_Temperature, & ! Input
                                  Planck_Surface_TL                 ) ! Output



    !#--------------------------------------------------------------------------#
    !#             -- PERFORM THE TANGENT-LINEAR RADIATIVE TRANSFER --          #
    !#--------------------------------------------------------------------------#

    ! -- Select the RT model
    IF( RTV%Scattering_RT ) THEN


      ! -----------------------------------------------------
      ! Scattering RT. NESDIS advanced adding-doubling method 
      ! -----------------------------------------------------

      CALL CRTM_ADA_TL( Atmosphere%n_Layers,                           & ! Input, number of atmospheric layers
                        AtmOptics%Single_Scatter_Albedo,               & ! Input, FWD layer single scattering albedo
                        AtmOptics%Asymmetry_Factor,                    & ! Input, FWD layer asymmetry factor
                        AtmOptics%Optical_Depth,                       & ! Input, FWD layer optical depth
                        SC%Cosmic_Background_Radiance(Channel_Index),  & ! cosmic background radiation
                        SfcOptics%Emissivity( 1:nZ, 1 ),               & ! Input, FWD surface emissivity
                        RTV,                                           & ! Input, structure containing forward results 
                        Planck_Atmosphere_TL,                          & ! Input, TL layer radiances
                        Planck_Surface_TL,                             & ! Input, TL surface radiance
                        AtmOptics_TL%Single_Scatter_Albedo,            & ! Input, TL layer single scattering albedo
                        AtmOptics_TL%Asymmetry_Factor,                 & ! Input, TL layer asymmetry factor
                        AtmOptics_TL%Optical_Depth,                    & ! Input, TL layer optical depth
                        SfcOptics_TL%Emissivity( 1:nZ, 1 ),            & ! Input, TL surface emissivity
                        SfcOptics_TL%Reflectivity( 1:nZ, 1, 1:nZ, 1 ), & ! Input, TL surface reflectivity
                        Pff_TL( 1:nZ, 1:nZ, : ),                       & ! Input, TL layer forward phase matrix
                        Pbb_TL( 1:nZ, 1:nZ, : ),                       & ! Input, TL layer backward phase matrix
                        Scattering_Radiance_TL(1:nZ)                   ) ! Output, TL radiances

      ! -- The output TL radiance for the sensor zenith angle
      RTSolution_TL%Radiance = Scattering_Radiance_TL( RTSolution%Index_Sat_Ang )

    ELSE


      ! -----------------
      ! Emission model RT
      ! -----------------

      CALL CRTM_Emission_TL( Atmosphere%n_Layers,                           & ! Input, number of atmospheric layers
                             RTV%n_Angles,                                  & ! Input, number of discrete zenith angles
                             u,                                             & ! Input, cosine of sensor zenith angle
                             AtmOptics%Optical_Depth,                       & ! Input, FWD layer optical depth
                             RTV%Planck_Atmosphere,                         & ! Input, FWD layer radiances
                             RTV%Planck_Surface,                            & ! Input, FWD surface radiance
                             SfcOptics%Emissivity( 1:nZ, 1 ),               & ! Input, FWD surface emissivity
                             SfcOptics%Reflectivity( 1:nZ, 1, 1:nZ, 1 ),    & ! Input, FWD surface reflectivity
                             SfcOptics%Direct_Reflectivity( 1:nZ, 1 ),      & ! Input, FWD surface reflectivity for a point source
                             SC%Solar_Irradiance(Channel_Index),            & ! Input, Source irradiance at TOA
                             SC%Is_Solar_Channel(Channel_Index),            & ! Input, Source sensitive channel info.
                             GeometryInfo%Source_Zenith_Radian,             & ! Input, Source zenith angle
                             RTV,                                           & ! Input, internal variables
                             AtmOptics_TL%Optical_Depth,                    & ! Input, TL layer optical depth
                             Planck_Atmosphere_TL,                          & ! Input, TL layer radiances
                             Planck_Surface_TL,                             & ! Input, TL surface radiance
                             SfcOptics_TL%Emissivity( 1:nZ, 1 ),            & ! Input, TL surface emissivity
                             SfcOptics_TL%Reflectivity( 1:nZ, 1, 1:nZ, 1 ), & ! Input, TL surface reflectivity
                             SfcOptics_TL%Direct_Reflectivity( 1:nZ, 1 ),   & ! Input, TL surface reflectivity for a point source
                             RTSolution_TL%Radiance                         ) ! Output, TL radiances

    END IF



    !#--------------------------------------------------------------------------#
    !#   -- COMPUTE THE CORRESPONDING TANGENT-LINEAR BRIGHTNESS TEMPERATURE --  #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Planck_Temperature_TL( Channel_Index,                       & ! Input
                                     RTSolution%Radiance,                 & ! Input
                                     RTSolution_TL%Radiance,              & ! Input
                                     RTSolution_TL%Brightness_Temperature ) ! Output

  END FUNCTION CRTM_Compute_RTSolution_TL


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_RTSolution_AD
!
! PURPOSE:
!       Function to solve the adjoint radiative transfer equation.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!      Error_Status = CRTM_Compute_RTSolution_AD( Atmosphere,               &  ! FWD Input
!                                                 Surface,                  &  ! FWD Input
!                                                 AtmOptics,                &  ! FWD Input
!                                                 SfcOptics,                &  ! FWD Input
!                                                 RTSolution,               &  ! FWD Input
!                                                 RTSolution_AD,            &  ! AD Input
!                                                 GeometryInfo,             &  ! Input
!                                                 Channel_Index,            &  ! Input
!                                                 Atmosphere_AD,            &  ! AD Output
!                                                 Surface_AD,               &  ! AD Output
!                                                 AtmOptics_AD,             &  ! AD Output
!                                                 SfcOptics_AD,             &  ! AD Output
!                                                 RTVariables,              &  ! Internal variable input
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AtmOptics:      Structure containing the combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       SfcOptics:      Structure containing the surface optical properties
!                       data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       RTSolution:     Structure containing the solution to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       RTSolution_AD:  Structure containing the RT solution adjoint inputs. 
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:  Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       RTVariables:    Structure containing internal forward model variables
!                       required for subsequent tangent-linear or adjoint model
!                       calls. The contents of this structure are NOT accessible
!                       outside of the CRTM_RTSolution module.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:  Structure containing the adjoint atmospheric
!                       state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_AD:     Structure containing the adjoint surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       AtmOptics_AD:   Structure containing the adjoint combined atmospheric
!                       optical properties for gaseous absorption, clouds,
!                       and aerosols.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
!       SfcOptics_AD:   Structure containing the adjoint surface optical
!                       properties data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_RTSolution_AD( Atmosphere,    &  ! FWD Input
                                       Surface,       &  ! FWD Input
                                       AtmOptics,     &  ! FWD Input
                                       SfcOptics,     &  ! FWD Input
                                       RTSolution,    &  ! FWD Input
                                       RTSolution_AD, &  ! AD Input
                                       GeometryInfo,  &  ! Input
                                       Channel_Index, &  ! Input
                                       Atmosphere_AD, &  ! AD Output
                                       Surface_AD,    &  ! AD Output
                                       AtmOptics_AD,  &  ! AD Output
                                       SfcOptics_AD,  &  ! AD Output
                                       RTV,           &  ! Internal variable input
                                       Message_Log )  &  ! Error messaging
                                     RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- FWD Input
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Surface_type ),      INTENT( IN )     :: Surface
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN )     :: AtmOptics
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN )     :: SfcOptics
    TYPE( CRTM_RTSolution_type ),   INTENT( IN )     :: RTSolution

    ! -- AD Input
    TYPE( CRTM_RTSolution_type ),   INTENT( IN OUT ) :: RTSolution_AD

    ! -- Other input
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- AD Output 
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN OUT ) :: Atmosphere_AD
    TYPE( CRTM_Surface_type ),      INTENT( IN OUT ) :: Surface_AD
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: AtmOptics_AD
    TYPE( CRTM_SfcOptics_type ),    INTENT( IN OUT ) :: SfcOptics_AD

    ! -- Internal variable input
    TYPE( CRTM_RTVariables_type ),  INTENT( IN )     :: RTV

    ! -- Error messaging
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    INTEGER :: i, k, nZ
    REAL( fp_kind ) :: u       ! COS( sensor zenith angle )
    REAL( fp_kind )                                     :: Planck_Surface_AD    ! Surface AD radiance
    REAL( fp_kind ), DIMENSION( 0:Atmosphere%n_Layers ) :: Planck_Atmosphere_AD ! *LAYER* AD radiances

    REAL( fp_kind ) :: User_Emissivity_AD, Direct_Reflectivity_AD    ! Temporary adjoint variable for SfcOptics calcs.

    ! -- The following variables are RT model specific
    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                Atmosphere%n_Layers ) :: Pff_AD ! Forward scattering AD phase matrix

    REAL( fp_kind ), DIMENSION( MAX_N_ANGLES, &
                                MAX_N_ANGLES, &
                                Atmosphere%n_Layers ) :: Pbb_AD ! Backward scattering AD phase matrix

    REAL ( fp_kind ),DIMENSION( MAX_N_ANGLES ) :: Scattering_Radiance_AD



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                           -- ASSIGN SOME VALUES --                       #
    !#--------------------------------------------------------------------------#

    ! -- Short named local copies
    u  = ONE / GeometryInfo%Secant_Sensor_Zenith
    nZ = RTV%n_Angles

    ! -- Set the sensor zenith angle index in RTSolution_AD
    RTSolution_AD%Index_Sat_Ang = RTSolution%Index_Sat_Ang


    ! ----------------------------------
    ! Initialise local adjoint variables
    ! ----------------------------------

    Planck_Surface_AD    = ZERO
    Planck_Atmosphere_AD = ZERO



    !#--------------------------------------------------------------------------#
    !#              -- COMPUTE THE BRIGHTNESS TEMPERATURE ADJOINT --            #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Planck_Temperature_AD( Channel_Index,                        & ! Input
                                     RTSolution%Radiance,                  & ! Input
                                     RTSolution_AD%Brightness_Temperature, & ! Input
                                     RTSolution_AD%Radiance                ) ! Output

    RTSolution_AD%Brightness_Temperature = ZERO 



    !#--------------------------------------------------------------------------#
    !#               -- PERFORM THE ADJOINT RADIATIVE TRANSFER --               #
    !#--------------------------------------------------------------------------#

    ! -- Select the RT model
    IF( RTV%Scattering_RT ) THEN


      ! -----------------------------------------------------
      ! Scattering RT. NESDIS advanced adding-doubling method 
      ! -----------------------------------------------------

      ! -- Initialise the input adjoint radiance
      Scattering_Radiance_AD = ZERO
      Scattering_Radiance_AD( RTSolution%Index_Sat_Ang ) = RTSolution_AD%Radiance
      RTSolution_AD%Radiance = ZERO

      ! -- Call the RT Solver
      CALL CRTM_ADA_AD( Atmosphere%n_Layers,                           & ! Input, number of atmospheric layers
                        AtmOptics%Single_Scatter_Albedo,               & ! Input, FWD layer single scattering albedo
                        AtmOptics%Asymmetry_Factor,                    & ! Input, FWD layer asymmetry factor
                        AtmOptics%Optical_Depth,                       & ! Input, FWD layer optical depth
                        SC%Cosmic_Background_Radiance(Channel_Index),  & ! Input, cosmic background radiation
                        SfcOptics%Emissivity( 1:nZ, 1 ),               & ! Input, FWD surface emissivity
                        RTV,                                           & ! In/Output, internal variables
                        Scattering_Radiance_AD(1:nZ),                  & ! Input, AD radiances
                        Planck_Atmosphere_AD,                          & ! Output, AD layer radiances
                        Planck_Surface_AD,                             & ! Output, AD surface radiance
                        AtmOptics_AD%Single_Scatter_Albedo,            & ! Output, AD layer single scattering albedo
                        AtmOptics_AD%Asymmetry_Factor,                 & ! Output, AD layer asymmetry factor
                        AtmOptics_AD%Optical_Depth,                    & ! Output, AD layer optical depth
                        SfcOptics_AD%Emissivity( 1:nZ, 1 ),            & ! Output, AD surface emissivity
                        SfcOptics_AD%Reflectivity( 1:nZ, 1, 1:nZ, 1 ), & ! Output, AD surface reflectivity
                        Pff_AD( 1:nZ, 1:nZ, : ),                       & ! Output, AD layer forward phase matrix
                        Pbb_AD( 1:nZ, 1:nZ, : )                        ) ! Output, AD layer backward phase matrix

    ELSE


      ! -----------------
      ! Emission model RT
      ! -----------------

      CALL CRTM_Emission_AD( Atmosphere%n_Layers,                           & ! Input, number of atmospheric layers
                             RTV%n_Angles,                                  & ! Input, number of discrete zenith angles
                             u,                                             & ! Input, cosine of sensor zenith angle
                             AtmOptics%Optical_Depth,                       & ! Input, FWD layer optical depth
                             RTV%Planck_Atmosphere,                         & ! Input, FWD layer radiances
                             RTV%Planck_Surface,                            & ! Input, FWD surface radiance
                             SfcOptics%Emissivity( 1:nZ, 1 ),               & ! Input, FWD surface emissivity
                             SfcOptics%Reflectivity( 1:nZ, 1, 1:nZ, 1 ),    & ! Input, FWD surface reflectivity
                             SfcOptics%Direct_Reflectivity( 1:nZ, 1 ),      & ! Input, FWD surface reflectivity for a point source
                             SC%Solar_Irradiance(Channel_Index),            & ! Input, Source irradiance at TOA
                             SC%Is_Solar_Channel(Channel_Index),            & ! Input, Source sensitive channel info.
                             GeometryInfo%Source_Zenith_Radian,             & ! Input, Source zenith angle
                             RTV,                                           & ! Input, internal variables
                             RTSolution_AD%Radiance,                        & ! Input, AD radiance
                             AtmOptics_AD%Optical_Depth,                    & ! Output, AD layer optical depth
                             Planck_Atmosphere_AD,                          & ! Output, AD layer radiances
                             Planck_Surface_AD,                             & ! Output, AD surface radiance
                             SfcOptics_AD%Emissivity( 1:nZ, 1 ),            & ! Output, AD surface emissivity
                             SfcOptics_AD%Reflectivity( 1:nZ, 1, 1:nZ, 1 ), & ! Output, AD surface reflectivity
                             SfcOptics_AD%Direct_Reflectivity( 1:nZ, 1 )    ) ! Output, AD surface reflectivity for a point source

    END IF



    !#--------------------------------------------------------------------------#
    !#                -- COMPUTE THE ADJOINT PLANCK RADIANCES --                #
    !#--------------------------------------------------------------------------#

    ! -- Surface AD radiance
    CALL CRTM_Planck_Radiance_AD( Channel_Index,                   & ! Input
                                  SfcOptics%Surface_Temperature,   & ! Input
                                  Planck_Surface_AD,               & ! Input
                                  SfcOptics_AD%Surface_Temperature ) ! In/Output
    Planck_Surface_AD = ZERO

    ! -- Atmospheric layer AD radiances
    DO k = 1, Atmosphere%n_Layers
      CALL CRTM_Planck_Radiance_AD( Channel_Index,                     & ! Input
                                    Atmosphere%Temperature(k),   & ! Input
                                    Planck_Atmosphere_AD(k),           & ! Input
                                    Atmosphere_AD%Temperature(k) ) ! In/Output
      Planck_Atmosphere_AD(k) = ZERO
    END DO



    !#--------------------------------------------------------------------------#
    !#                -- PERFORM THE SURFACE OPTICS COMPUTATION --              #
    !#--------------------------------------------------------------------------#


    ! ---------------------------------------------------------------------
    ! Assign the number of Stokes parameters. Currently, this is set to 1
    ! for decoupled polarization between surface and atmosphere.
    ! Remember for polarised microwave instruments, each frequency's Stokes
    ! parameter is treated as a separate channel.
    ! ---------------------------------------------------------------------

    SfcOptics_AD%n_Stokes = 1

    
    ! ----------------------------------------------------
    ! Assign the angles and weights to SfcOptics structure
    ! ----------------------------------------------------

    SfcOptics_AD%n_Angles = SfcOptics%n_Angles
    SfcOptics_AD%Angle    = SfcOptics%Angle 
    SfcOptics_AD%Weight   = SfcOptics%Weight


    ! --------------------------------------------------------------------------------
    ! This part is designed for specific requirement for the total sensitivity
    ! of emissivity. The requirement is regardless whether having user input or not.
    ! Therefore, this part is common part with/without optional input.
    ! The outputs of CRTM_Compute_RTSolution are the partial derivative
    ! of emissivity and reflectivity. Since SfcOptics_AD is used as input only in 
    ! CRTM_Compute_SfcOptics_AD, the total sensitivity of the emissivity is taken into
    ! account for here.
    ! --------------------------------------------------------------------------------

      IF( RTV%Scattering_RT ) THEN

        User_Emissivity_AD = ZERO
        IF( RTV%Diffuse_Surface) THEN
          DO i = nZ, 1, -1
            User_Emissivity_AD = User_Emissivity_AD &
              - SUM(SfcOptics_AD%Reflectivity(1:nZ, 1, i, 1))*SfcOptics%Weight(i)
          END DO
        ELSE ! Specular surface
          DO i = nZ, 1, -1
            User_Emissivity_AD = User_Emissivity_AD - SfcOptics_AD%Reflectivity(i, 1, i, 1)
          END DO
        END IF

!        Direct_Reflectivity_AD = SUM(SfcOptics_AD%Direct_Reflectivity(1:nZ,1))
!        SfcOptics_AD%Direct_Reflectivity(1,1) = SfcOptics_AD%Direct_Reflectivity(1,1) &
!           + Direct_Reflectivity_AD/PI

        RTSolution_AD%Surface_Emissivity = User_Emissivity_AD

      ELSE
      
        RTSolution_AD%Surface_Emissivity = SfcOptics_AD%Emissivity( RTSolution_AD%Index_Sat_Ang, 1 ) &
           - SfcOptics_AD%Reflectivity(1, 1, 1, 1)

      END IF


    ! -----------------------------------------
    ! Populate the SfcOptics_AD structure based
    ! on FORWARD model SfcOptics Compute_Switch
    ! -----------------------------------------

    IF ( SfcOptics%Compute_Switch == SET ) THEN

      ! -- Compute the values
      Error_Status = CRTM_Compute_SfcOptics_AD( Surface,                  & ! Input
                                                SfcOptics,                & ! Input
                                                SfcOptics_AD,             & ! Input
                                                GeometryInfo,             & ! Input
                                                Channel_Index,            & ! Input
                                                Surface_AD,               & ! In/Output
                                                Message_Log = Message_Log ) ! Error messaging



                                               
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing SfcOptics_AD for ", i4 )' ) &
                        Channel_Index 
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE

      ! -- Extract out a single emissivity/user value for all angles/Stokes
      ! -- This part is moved above as the common part.

    END IF



    !#--------------------------------------------------------------------------#
    !#            -- COMPUTE THE ADJOINT PHASE MATRIX IF REQUIRED --            #
    !#--------------------------------------------------------------------------#

    IF ( RTV%Scattering_RT ) THEN

      CALL CRTM_Phase_Matrix_AD( AtmOptics,    &  ! Input - FWD
                                 Pff_AD,       &  ! Input - AD forward scattering
                                 Pbb_AD,       &  ! Input - AD backward scattering
                                 AtmOptics_AD, &  ! Output - AD
                                 RTV           )  ! Internal variable

    END IF


    !#--------------------------------------------------------------------------#
    !#                -- SAVE THE AD OPTICAL DEPTH IF REQUIRED --               #
    !#--------------------------------------------------------------------------#

    IF ( ASSOCIATED( RTSolution_AD%Layer_Optical_Depth ) ) THEN
      RTSolution_AD%Layer_Optical_Depth( 1:RTV%n_Layers ) = &
        AtmOptics_AD%Optical_Depth( 1:RTV%n_Layers )
    END IF

  END FUNCTION CRTM_Compute_RTSolution_AD





  SUBROUTINE CRTM_Compute_n_Streams( Atmosphere,   &  ! Input, Scalar
                                      Channel_Index,   &  ! Input, Scalar
                                          n_Streams,   &  ! OUTPUT 
                                         RTSolution)      ! INPUT/OUTOUT
! ---------------------------------------------------------------------------------------------
!   FUNCTION
!    Determining number of streams. 
! ---------------------------------------------------------------------------------------------
      IMPLICIT NONE
    TYPE( CRTM_Atmosphere_type ),  INTENT( IN ) :: Atmosphere    ! Scalar
    TYPE( CRTM_RTSolution_type ),  INTENT( IN OUT ) :: RTSolution
    INTEGER, INTENT( IN ) :: Channel_Index
    INTEGER, INTENT( OUT ) :: n_Streams
    ! ----------------
    ! Local parameters
    REAL( fp_kind ) :: Mie_parameter
    INTEGER :: n
!
       n_Streams = 0
       RTSolution%n_full_Streams = n_Streams
       RTSolution%Scattering_FLAG = .FALSE.

       IF(Atmosphere%n_Clouds == 0 ) RETURN
!      In this place, the n_Streams is determined by Mie parameter

       Mie_parameter = 0.0_fp_kind
       DO n = 1, Atmosphere%n_Clouds
       IF( maxval(Atmosphere%Cloud(n)%Effective_Radius(:)) > Mie_parameter) &
         Mie_parameter = maxval(Atmosphere%Cloud(n)%Effective_Radius(:)) 
       ENDDO

       Mie_parameter = TWO * PI * Mie_parameter * SC%Wavenumber(Channel_Index)/10000.0_fp_kind
       IF( Mie_parameter < 0.01_fp_kind ) THEN
         n_Streams = 2
       ELSE IF( Mie_parameter < ONE ) THEN
         n_Streams = 4
!       ELSE IF( Mie_parameter < 10.0_fp_kind ) THEN
       ELSE
         n_Streams = 6
!       ELSE
!         n_Streams = 8
       ENDIF

       RTSolution%Scattering_FLAG = .TRUE.
!       RTSolution%n_full_Streams = n_Streams 
       RTSolution%n_full_Streams = n_Streams + 2

       RETURN
    END SUBROUTINE CRTM_Compute_n_Streams





!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_RTSolution.f90,v 1.13.2.12 2005/10/21 18:56:08 paulv Exp $
!
! $Date: 2005/10/21 18:56:08 $
!
! $Revision: 1.13.2.12 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_RTSolution.f90,v $
! Revision 1.13.2.12  2005/10/21 18:56:08  paulv
! - Added in TL and AD calls for phase matrix normalisation in the
!   CRTM_Phase_Matrix() functions.
! - Updated the phase matrix normalisation routines.
!
! Revision 1.13.2.11  2005/10/20 17:57:40  paulv
! - Changes made to save the TL and AD emissivities and optical depths in
!   the corresponding output RTSolution structure array.
!
! Revision 1.13.2.10  2005/10/17 14:23:02  paulv
! - Removed USE of SpcCoeff_Define module.
! - Removed USE of CRTM_Compute_Phase.f90 module.
! - The RTV structure has been modified with additional components required
!   for the Compute_Phase() functions:
!   o Off and Obb - original forward and backward scattering phase matrices
!     to allow the correct
!   o Pplus and Pminus - the +ve and -ve cosine angle Legendre phase functions.
!   o n_Factor and sum_fac - used in normalising the phase functions after they
!     have been corrected for negative values.
! - The routines in the CRTM_Compute_Phase module have been moved into this
!   module as PRIVATE routines:
!   o CRTM_Compute_Phase
!   o CRTM_Compute_Phase_TL
!   o CRTM_Compute_Phase_AD
!   o Normalisation routines.
!   The interfaces to these routines have been altered to use the RTV internal
!   varibale structure.
! - The calls to the Compute_Phase routines in the RTSolution routines have been
!   updated to reflect the new interfaces.
!
! Revision 1.13.2.9  2005/10/14 22:07:45  paulv
! - Added n_Layers dimension to RTV structure.
! - Corrected assignment of user input emissivities and reflectivities to
!   the SfcOptics structure. Previosuly the reflectivities were not being
!   scaled correctly.
! - Added output of surface emissivity and layer optical depth to RTSolution
!   structure (Forward only.)
!
! Revision 1.13.2.8  2005/10/12 18:07:42  qliu
! -- More documentation.
!
! Revision 1.13.2.7  2005/10/04 19:32:02  qliu
! - Changed intent of RTV argument in private routines from IN OUT to IN.
! - Removed computation of radiance at the surface in the TL functions as
!   it is available in the RTV structure.
! - Removed definitions of logical flags Diffuse_Surface and Scattering_RT
!   from the TL and AD routines as they are available via the RTV structure.
! - Removed definitions of surface and atmosphere Planck radiances, the
!   Gaussian quadrature angle and weight arrays, and the scattering phase functions
!   in the TL and AD routines as they are available via the RTV structure.
! - Altered radiative transfer routine calls to use *only* the first Stokes
!   parameter for the emissivity and reflectivities. Replaced the 1:nL dimensioning
!   with just 1. This will need to be changed when atmospheric residual polarisation
!   is considered.
! - Removed unused variables from various routines.
! - Corrected invalid references to components of the RTV structure in various
!   routines.
! - Corrected adjoint SfcOptics reflectivity dimensioning.
!
! Revision 1.13.2.6  2005/10/03 19:28:09  paulv
! - Added and updated documentation.
! - Altered code to follow convention that all capital names correspond to
!   parameters.
! - Added ANGLE_THRESHOLD module parameter to use in search for matching
!   stream/sensor zenith angles.
! - Moved private routines from end of file into private routine section.
! - Changed CRTM_ADA_type name to CRTM_RTVariables_type. Also added a number
!   of forward model RTSolution variables. This structure is now used as a
!   generic container for RTSolution forward variables that are used in
!   subsequent tangent-linear and adjoint routine calls.
! - Removed all references to the variables w, g, and Total_OD (and their TL and
!   AD forms) in all the RTSolution routines. Now the AtmOptics components are
!   used directly. This greatly simplifies the adjoint RTSolution routine.
! - In CRTM_Compute_RTSolution:
!   o Rearranged argument order to follow same conventionas other CRTM calls.
!   o Added CRTM_RTVariables_type argument output.
!   o Replaced some local variable references with those for the CRTM_RTVariables_type
!     components that are required to be output.
!   o Renamed n_factor weight normalisation variable to Factor.
!   o Added logic to populate the SfcOptics structure based on user
!     input emissivity.
!   o Removed local Emissivity, Reflectivity, and Direct_Reflectivity variables.
!     Now using the SfcOptics components in the calls to CRTM_ADA and CRTM_Emission.
!     Short names for the angle and Stokes parameters dimensions were added to
!     make the references to the surface optics in these calls manageable.
! - In CRTM_Compute_RTSolution_TL:
!   o Rearranged argument order to follow same conventionas other CRTM calls.
!   o Added CRTM_RTVariables_type argument input.
!   o Removed the re-computation of the forward RTSolution. The contents of the
!     CRTM_RTVariables argument, RTV, is used.
!   o Added logic to populate the SfcOptics_TL structure based on user
!     input emissivity.
!   o Removed local Emissivity, Reflectivity, and Direct_Reflectivity variables.
!     Now using the SfcOptics(_TL) components in the calls to CRTM_ADA and CRTM_Emission.
!     Short names for the angle and Stokes parameters dimensions were added to
!     make the references to the surface optics in these calls manageable.
! - In CRTM_Compute_RTSolution_AD:
!   o Rearranged argument order to follow same conventionas other CRTM calls.
!   o Added CRTM_RTVariables_type argument input.
!   o Removed the re-computation of the forward RTSolution. The contents of the
!     CRTM_RTVariables argument, RTV, is used.
!   o Added logic to populate the SfcOptics_AD structure based on user
!     input emissivity adjoints.
!
! Revision 1.13.2.4  2005/09/27 16:15:42  qliu
! -- A bug fixed.
!
! Revision 1.13.2.3  2005/08/23 15:28:20  qliu
! -- Deleted unused variables.
!
! Revision 1.13.2.2  2005/08/22 15:22:58  qliu
! -- Replacing Diffuse_Factor by SECANT_DIFFUSIVITY.
!
! Revision 1.13.2.1  2005/08/17 21:07:36  qliu
! -- Deleted "USE CRTM_CloudScatter, ONLY : HGphase".
!    Added documentation.
!
! Revision 1.13  2005/08/16 20:59:25  qliu
! - Initial working version.
!
! Revision 1.11  2005/02/25 17:56:07  paulv
! - All the instances (in definitions and interfaces) of the following
!   structures
!     TYPE( CRTM_Aerosol_type )    -- Aerosol
!     TYPE( CRTM_AtmScatter_type ) -- AtmScatter
!   have been changed to
!     TYPE( CRTM_AtmScatter_type ) -- AerosolScatter
!     TYPE( CRTM_AtmScatter_type ) -- CloudScatter
!   to reflect the use of the same data type for both the cloud particle
!   and aerosol scattering codes.
!
! Revision 1.10  2005/02/18 23:17:56  paulv
! - Added Aerosol capability. RTSolution interfaces altered.
!
! Revision 1.9  2005/02/17 23:06:43  paulv
! - Corrected adjoint function documentation name.
!
! Revision 1.8  2005/02/01 16:02:47  paulv
! - Added adjoint shell function.
!
! Revision 1.7  2005/01/28 21:14:51  paulv
! - Added CRTM_Compute_RTSolution_TL() function shell.
! - Rearranged some of the RTSolution arguments to make their ordering
!   consistent in the CRTM source codes.
!
! Revision 1.6  2004/11/05 16:13:00  paulv
! - Replaced Init() with Associated() in PUBLIC list.
! - Changed output RTSolution structure intent from (OUT) to (IN OUT) to
!   prevent memory leaks.
!
! Revision 1.5  2004/08/06 18:45:53  paulv
! - Updated header documentation.
!
! Revision 1.4  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.3  2004/06/29 16:58:59  paulv
! - Made SfcOptics argument in Compute_RTSolution() optional.
! - Updated documentation.
!
! Revision 1.2  2004/06/28 22:22:38  paulv
! - Removed RTSolution structure definition and methods to separate routine.
! - Added all required arguments.
! - Updated header documentation.
!
! Revision 1.1  2004/06/15 16:20:53  paulv
! Initial checkin.
!
!
!
END MODULE CRTM_RTSolution
!
