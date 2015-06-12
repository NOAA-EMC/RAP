!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Interpolation
!
! PURPOSE:
!       Module containing routines for data interpolation
!
! CATEGORY:
!       CRTM : Interpolation
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Interpolation
!
! OUTPUTS:
!       None.
!
! MODULES:
!       Type_Kinds:             Module containing data type kind definitions.
!
!
! CONTAINS:
!       Interpolate_Profile:       Subroutine to perform profile interpolation
!
!       Interpolate_Profile_TL:    Subroutine to perform the tangent-linear
!                                  profile interpolation.
!
!       Interpolate_Profile_AD:    Subroutine to perform the adjoint
!                                  profile interpolation.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! RESTRICTIONS:
!       These functions are called frequently so no input checking is
!       performed.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 27-July-2005
!                       Yong.Han@noaa.gov
!
!M-
!------------------------------------------------------------------------------

MODULE CRTM_Interpolation

  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE

  PUBLIC  :: Interpolate_Profile
  PUBLIC  :: Interpolate_Profile_TL
  PUBLIC  :: Interpolate_Profile_AD
  PUBLIC  :: Compute_Interp_Index
 
  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Interpolate_Profile
    MODULE PROCEDURE Interpolate_Profile_F1
    MODULE PROCEDURE Interpolate_Profile_F2
  END INTERFACE Interpolate_Profile

  INTERFACE Interpolate_Profile_TL
    MODULE PROCEDURE Interpolate_Profile_F1_TL
    MODULE PROCEDURE Interpolate_Profile_F2_TL
  END INTERFACE Interpolate_Profile_TL

  INTERFACE Interpolate_Profile_AD
    MODULE PROCEDURE Interpolate_Profile_F1_AD
    MODULE PROCEDURE Interpolate_Profile_F2_AD
  END INTERFACE Interpolate_Profile_AD

  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  REAL(fp_kind),    PARAMETER :: ZERO = 0.0_fp_kind 
  REAL(fp_kind),    PARAMETER :: ONE  = 1.0_fp_kind 
  REAL(fp_kind),    PARAMETER :: TOLERANCE = EPSILON( ONE ) 

CONTAINS

!---------------------------------------------------------------------------------------------  
! NAME: Interpolate_Profile                                                                      
!                                                                                                 
! PURPOSE:
!    Given x and u that are ascending arrays, it interpolates y with the abscissa x
!    on the abscissa u using the following algorithm:
!       y_int(i) = y(1)  if u(i) < x(1)
!       y_int(i) = y(nx) if u(i) > x(nx)
!       y_int(i) = y(ix1) + (y(ix2)-y(ix1))*(u(i) - x(ix1))/(x(ix2)-x(ix1))
!                        if x(ix1) <= u(i) <= x(ix2)
!
!    IThe index array interp_index contains the following content 
!
!      interp_index(i, 1) = 1 and interp_index(i, 2) = 1, if u(i) < x(1)
!      interp_index(i, 1) = nx and interp_index(i, 2) = nx, if u(i) > x(nx), 
!                                                          where nx = SIZE(x)
!      x(interp_index(i, 1)) <= u(i) <= x(interp_index(i, 2)) if x(1) <= u(i) <= x(nx)
!
! CALLING SEQUENCE:
!            CALL Interpolate_Profile(y, x, u, y_int) 
!      or    CALL Interpolate_Profile(interp_index, y, x, u, y_int)
!
! INPUT ARGUMENTS:
!       y:            The data array to be interpolated.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       x:            The abscissa values for y and they must be monotonically 
!                     ascending.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       u:            The abscissa values for the results
!                     and they must be monotonically ascending
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!    interp_index:    The index array of dimension (nu x 2), where nu = SIZE(u) 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT( IN )                         
!
! OUTPUT ARGUMENTS:
!       y_int:        The array contains the results 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT( OUT )                         
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 07-May-2004
!-----------------------------------------------------------------------------------

  !----------------------------------------------------------------------------
  !  Interpolation routine with interperlation index array already calculated.
  !----------------------------------------------------------------------------
  SUBROUTINE Interpolate_Profile_F1(interp_index, y, x, u, y_int)

    INTEGER,            INTENT(IN)     :: interp_index(:,:)
    REAL(fp_kind),      INTENT(IN)     :: y(:), x(:)
    REAL(fp_kind),      INTENT(IN)     :: u(:)

    REAL(fp_kind),      INTENT(OUT)    :: y_int(:)

    Integer :: i, k1, k2

    DO i = 1, SIZE(u)

      k1 = interp_index(i, 1)
      k2 = interp_index(i, 2)
      IF( k1 == k2)THEN
        y_int(i) = y(k1)
      ELSE
        CALL Interp_linear(y(k1), x(k1), y(k2), x(k2), u(i), y_int(i))
      ENDIF

    ENDDO

  END SUBROUTINE Interpolate_Profile_F1

  !----------------------------------------------------------------------------
  !  Interpolation routine with the interperlation index array not supplied
  !----------------------------------------------------------------------------

  SUBROUTINE Interpolate_Profile_F2(y, x, u, y_int)

    REAL(fp_kind),      INTENT(IN)     :: y(:), x(:)
    REAL(fp_kind),      INTENT(IN)     :: u(:)

    REAL(fp_kind),      INTENT(OUT)    :: y_int(:)

    !Local
    INTEGER :: interp_index_local(SIZE(u), 2)

    ! --- Compute the index array for indexing the two interpolation points
    CALL Compute_Interp_Index(x, u, interp_index_local)

    CALL Interpolate_Profile_F1(interp_index_local, y, x, u, y_int)

  END SUBROUTINE Interpolate_Profile_F2

!---------------------------------------------------------------------------------------------  
! NAME: Interpolate_Profile_TL
!                                                                                                 
! PURPOSE:
!     The Tangent_Linear routine of Interpolate_Profile
! CALLING SEQUENCE:
!            CALL Interpolate_Profile_TL(y, x, u, y_TL, y_int_TL) 
!      or    CALL Interpolate_Profile_TL(interp_index, y, x, u, y_TL, y_int_TL)
!
! INPUT ARGUMENTS:
!       y:            The data array to be interpolated.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       x:            The abscissa values for y and they must be monotonically 
!                     ascending.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       u:            The abscissa values for the results
!                     and they must be monotonically ascending
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       y_TL:         The Tangent-linear data array of y
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!    interp_index:    The index array of dimension (nu x 2), where nu = SIZE(u) 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT( IN )                         
!
! OUTPUT ARGUMENTS:
!       y_int_TL:     The Tangent-linear array of y_int 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT( OUT )                         
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 07-May-2004
!-----------------------------------------------------------------------------------
    
  SUBROUTINE Interpolate_Profile_F1_TL(interp_index, y, x, u, y_TL, y_int_TL)

    INTEGER,       INTENT(IN)     :: interp_index(:,:)
    REAL(fp_kind), INTENT(IN)     :: y(:), x(:)
    REAL(fp_kind), INTENT(IN)     :: u(:)
    REAL(fp_kind), INTENT(IN)     :: y_TL(:) 

    REAL(fp_kind), INTENT(OUT)    :: y_int_TL(:)

    !Local
    Integer :: i, k1, k2

    DO i = 1, SIZE(u)

      k1 = interp_index(i, 1)
      k2 = interp_index(i, 2)
      IF( k1 == k2)THEN
        y_int_TL(i) = y_TL(k1)
      ELSE
        CALL Interp_linear_TL(y(k1), x(k1), y(k2), x(k2), u(i), y_TL(k1), y_TL(k2), y_int_TL(i))
      ENDIF

    ENDDO

  END SUBROUTINE Interpolate_Profile_F1_TL

  SUBROUTINE Interpolate_Profile_F2_TL(y, x, u, y_TL, y_int_TL)

    REAL(fp_kind), INTENT(IN)     :: y(:), x(:)
    REAL(fp_kind), INTENT(IN)     :: u(:)
    REAL(fp_kind), INTENT(IN)     :: y_TL(:) 

    REAL(fp_kind), INTENT(OUT)    :: y_int_TL(:)

    !Local
    INTEGER :: interp_index_local(SIZE(u), 2)

    ! --- Compute the index array for indexing the two interpolation points
    CALL Compute_Interp_Index(x, u, interp_index_local)

    CALL Interpolate_Profile_F1_TL(interp_index_local, y, x, u, y_TL, y_int_TL)

  END SUBROUTINE Interpolate_Profile_F2_TL

!---------------------------------------------------------------------------------------------  
! NAME: Interpolate_Profile_AD
!                                                                                                 
! PURPOSE:
!     The Adjoint routine of Interpolate_Profile
!
! CALLING SEQUENCE:
!            CALL Interpolate_Profile_AD(y, x, u, y_int_AD, y_AD) 
!      or    CALL Interpolate_Profile_AD(interp_index, y, x, u, y_int_AD, y_AD)
!
! INPUT ARGUMENTS:
!       y:            The data array to be interpolated.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       x:            The abscissa values for y and they must be monotonically 
!                     ascending.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       u:            The abscissa values for the results
!                     and they must be monotonically ascending
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       y_int_AD:     The Adjoint array of y_int 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-1
!                     ATTRIBUTES: INTENT( IN )                         
!
!    interp_index:    The index array of dimension (nu x 2), where nu = SIZE(u) 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT( IN )                         
!
! IN/OUTPUT ARGUMENTS:
!       y_AD:         The Adjoint data array of y
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 07-May-2004
!-----------------------------------------------------------------------------------
    
  SUBROUTINE Interpolate_Profile_F1_AD(interp_index, y, x, u, y_int_AD, y_AD)

    INTEGER,       INTENT(IN)     :: interp_index(:,:)
    REAL(fp_kind), INTENT(IN)     :: y(:), x(:)
    REAL(fp_kind), INTENT(IN)     :: u(:)
    REAL(fp_kind), INTENT(INOUT)  :: y_int_AD(:) 

    REAL(fp_kind), INTENT(INOUT)  :: y_AD(:)

    !Local
    Integer :: i, k1, k2

    DO i = SIZE(u), 1, -1

      k1 = interp_index(i, 1)
      k2 = interp_index(i, 2)
      IF( k1 == k2)THEN
        y_AD(k1) = y_AD(k1) + y_int_AD(i)
        y_int_AD(i) = ZERO
      ELSE
        CALL Interp_linear_AD(y(k1), x(k1), y(k2), x(k2), u(i), y_int_AD(i), y_AD(k1), y_AD(k2))
      ENDIF

    ENDDO

  END SUBROUTINE Interpolate_Profile_F1_AD

  SUBROUTINE Interpolate_Profile_F2_AD(y, x, u, y_int_AD, y_AD)

    REAL(fp_kind), INTENT(IN)     :: y(:), x(:)
    REAL(fp_kind), INTENT(IN)     :: u(:)
    REAL(fp_kind), INTENT(INOUT)  :: y_int_AD(:) 

    REAL(fp_kind), INTENT(INOUT)  :: y_AD(:)

    INTEGER :: interp_index_local(SIZE(u), 2)

    ! --- Compute the index array for indexing the two interpolation points
    CALL Compute_Interp_Index(x, u, interp_index_local)

    CALL Interpolate_Profile_F1_AD(interp_index_local, y, x, u, y_int_AD, y_AD)

  END SUBROUTINE Interpolate_Profile_F2_AD

!---------------------------------------------------------------------------------------------  
! NAME: Compute_Interp_Index                                                                      
!                                                                                                 
! PURPOSE:
!    Given x and u that are ascending arrays, it computes an index array, interp_index,
!    such that
!    
!      interp_index(i, 1) = 1 and interp_index(i, 2) = 1, if u(i) < x(1)
!      interp_index(i, 1) = nx and interp_index(i, 2) = nx, if u(i) > x(nx), 
!                                                          where nx = SIZE(x)
!      x(interp_index(i, 1)) <= u(i) <= x(interp_index(i, 2)) if x(1) <= u(i) <= x(nx)
!           
! CALLING SEQUENCE:
!            CALL Compute_Interp_Index(x, u, interp_index)
!
! INPUT ARGUMENTS:
!       x:            The abscissa values for the data to be interpolated and
!                     they must be monotonically ascending.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
!       u:            The abscissa values on which the data are interpolated
!                     they must be monotonically ascending
!                     the elements of array x.
!                     UNITS:      N/A                            
!                     TYPE:       fp_kind         
!                     DIMENSION:  rank-1                        
!                     ATTRIBUTES: INTENT( IN )                   
!
! OUTPUT ARGUMENTS:
!       interp_index: The index array of dimension (nu x 2), where nu = SIZE(u) 
!                     UNITS:      N/A                                  
!                     TYPE:       Integer         
!                     DIMENSION:  rank-2
!                     ATTRIBUTES: INTENT( OUT )                         
!
! RESTRICTIONS:
!     To be efficient, this routine does not check that x and u are both
!     monotonically ascending and the index bounds.
!
! CREATION HISTORY:
!       Written by:     Yong Han, 07-May-2004
!-----------------------------------------------------------------------------------

  SUBROUTINE Compute_Interp_Index(x, u, interp_index)

    REAL(fp_kind), INTENT(IN)  :: x(:)
    REAL(fp_kind), INTENT(IN)  :: u(:)
    INTEGER,       INTENT(OUT) :: interp_index(:, :)
    
    !Local
    INTEGER :: nx, nu, ix, iu, j, k1, k2

    nx = SIZE(x)
    nu = SIZE(u)

    ! --- Set the indexes to 1 for the elements in u that are smaller than x(1)
  
    k1 = nu + 1
    DO iu = 1, nu

      IF(u(iu) < x(1))THEN
        interp_index(iu, 1) = 1
        interp_index(iu, 2) = 1
      ELSE
        k1 = iu
        EXIT
      ENDIF

    ENDDO

    ! --- Set the indexes to nx for the elements in u that are larger than x(nx)

    k2 = 0
    DO iu = nu, k1, -1

      IF(u(iu) > x(nx))THEN
        interp_index(iu, 1) = nx
        interp_index(iu, 2) = nx
      ELSE
        k2 = iu
        EXIT
      ENDIF

    ENDDO

    ! --- Set the indexes for the elements in u that are in the range
    ! --- between x1(1) and x(nx)

    j = 1
    DO iu = k1, k2
      DO ix = j, nx-1
        IF(u(iu) >= x(ix) .AND. u(iu) <= x(ix+1))THEN
          interp_index(iu, 1) = ix
          interp_index(iu, 2) = ix+1
          j = ix
          EXIT
        ENDIF
      ENDDO
    ENDDO
  
  END SUBROUTINE Compute_Interp_Index

  !-----------------------------------------
  ! Function for two points linear interpolation
  !-----------------------------------------    
  SUBROUTINE Interp_linear(y1, x1, y2, x2, x, y)

    REAL(fp_kind), INTENT(IN)     :: y1, x1, y2, x2, x
    REAL(fp_kind), INTENT(OUT)  :: y

    y = y1 + (y2-y1)*(x - x1)/(x2 - x1)

  END SUBROUTINE Interp_linear

  SUBROUTINE Interp_linear_TL(y1, x1, y2, x2, x, y1_TL, y2_TL, y_TL)

    REAL(fp_kind), INTENT(IN)     :: y1, x1, y2, x2, x, y1_TL, y2_TL
    REAL(fp_kind), INTENT(OUT)    :: y_TL

    y_TL = y1_TL + (y2_TL-y1_TL)*(x - x1)/(x2 - x1)

  END SUBROUTINE Interp_linear_TL

  SUBROUTINE Interp_linear_AD(y1, x1, y2, x2, x, y_AD, y1_AD, y2_AD)

    ! -- INPUTS
    REAL(fp_kind), INTENT(IN)     :: y1, x1, y2, x2, x
    REAL(fp_kind), INTENT(INOUT)  :: y_AD
    ! -- Outputs
    REAL(fp_kind), INTENT(INOUT)  :: y1_AD, y2_AD

    ! Local
    REAL(fp_kind) :: fac

    fac = (x - x1)/(x2 - x1)  
    y1_AD = y1_AD + y_AD      
    y1_AD = y1_AD - fac*y_AD  
    y2_AD = y2_AD + fac*y_AD  

    y_AD = ZERO

  END SUBROUTINE Interp_linear_AD

  !-----------------------------------------
  ! Function for two points log_log interpolation
  !-----------------------------------------    

  SUBROUTINE Interp_loglog(y1, x1, y2, x2, x, y)

    REAL(fp_kind), INTENT(IN)     :: y1, x1, y2, x2, x
    REAL(fp_kind), INTENT(OUT)    :: y

    ! Local
    REAL(fp_kind) :: xx

    IF(x1 <= TOLERANCE .OR. x2 <= TOLERANCE .OR. x <= TOLERANCE .OR. &
       y1 <= TOLERANCE .OR. y2 <= TOLERANCE)THEN  ! linear interpolation
      CALL Interp_linear(y1, x1, y2, x2, x, y)
    ELSE 
      xx = Log(x/x1) / Log(x2/x1)
      y = y1 * (y2/y1)**xx
    ENDIF

  END SUBROUTINE Interp_loglog

  SUBROUTINE Interp_loglog_TL(y1, x1, y2, x2, x, y1_TL, y2_TL, y_TL)

    REAL(fp_kind), INTENT(IN)     :: y1, x1, y2, x2, x, y1_TL, y2_TL
    REAL(fp_kind), INTENT(OUT)    :: y_TL

    ! Local
    REAL(fp_kind) :: xx, r, rxx

    IF(x1 <= TOLERANCE .OR. x2 <= TOLERANCE .OR. x <= TOLERANCE .OR. &
       y1 <= TOLERANCE .OR. y2 <= TOLERANCE)THEN  ! linear interpolation
      CALL Interp_linear_TL(y1, x1, y2, x2, x, y1_TL, y2_TL, y_TL)
    ELSE 
      xx  = Log(x/x1) / Log(x2/x1)
      r   = (y2/y1)
      rxx = r**xx

      y_TL = (ONE - xx)*rxx*y1_TL + xx*(rxx/r)*y2_TL
    ENDIF

  END SUBROUTINE Interp_loglog_TL

  SUBROUTINE Interp_loglog_AD(y1, x1, y2, x2, x, y_AD, y1_AD, y2_AD)

    ! -- INPUTS
    REAL(fp_kind), INTENT(IN)     :: y1, x1, y2, x2, x
    REAL(fp_kind), INTENT(INOUT)  :: y_AD
    ! -- Outputs
    REAL(fp_kind), INTENT(INOUT)  :: y1_AD, y2_AD

    ! Local
    REAL(fp_kind) :: xx, r, rxx

    IF(x1 <= TOLERANCE .OR. x2 <= TOLERANCE .OR. x <= TOLERANCE .OR. &
       y1 <= TOLERANCE .OR. y2 <= TOLERANCE)THEN  ! linear interpolation
      CALL Interp_linear_AD(y1, x1, y2, x2, x, y_AD, y1_AD, y2_AD)
    ELSE 
      xx  = Log(x/x1) / Log(x2/x1)
      r   = (y2/y1)
      rxx = r**xx

      y1_AD = y1_AD + (ONE - xx)*rxx*y_AD
      y2_AD = y2_AD + xx*(rxx/r)*y_AD

      y_AD = ZERO
    ENDIF

  END SUBROUTINE Interp_loglog_AD

END MODULE CRTM_Interpolation


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Interpolation.f90,v 1.7 2005/09/12 13:53:11 qliu Exp $
!
! $Date: 2005/09/12 13:53:11 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Interpolation.f90,v $
! Revision 1.7  2005/09/12 13:53:11  qliu
! -- Deleted errors on IBM.
!
! Revision 1.6  2005/09/08 20:09:15  yhan
! --- Removed the optional argument interp_index from function
!     Interpolate_Profile_F2.
!
! Revision 1.5  2005/08/17 20:22:03  paulv
! - Merged versions to remove 1.2 revisions. No longer need this for
!   pressure Jacobians.
!
! Revision 1.4  2005/08/17 18:29:00  yhan
! - Correct error in the header document.
!
! Revision 1.3  2005/08/17 15:47:10  qliu
! - Updated header and modification documentation.
!
!
!
!
!

