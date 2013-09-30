	SUBROUTINE ADD_WMO  ( gbmin, type, lunkwb, luntim,
     +			      lunprm, lungrd, lunlvl, gbmout, lenout,
     +			      iret )
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .                                       .
! SUBPROGRAM: ADD_WMO        ADD WMO HEADER TO A GRIB MESSAGE
!   PRGMMR: BRILL            ORG: W/NP22    DATE: 97-07-01
!
! ABSTRACT: ADDS THE WMO HEADER FOR A GRIB MESSAGE AFTER MAKING IT
!   USING THE PDS INFORMATION.
!
! PROGRAM HISTORY LOG:
!
! USAGE:    CALL MAKWMO( GBMIN, TYPE, LUNKWB, LUNTIM, LUNPRM, LUNGRD,
!		         LUNLVL, GBMOUT, LENOUT, IRET )
!  INPUT ARGUMENT LIST:
!	See below.
!
!   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
!	See below.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!   MACHINE:  PORTABLE
!
!$$$
!************************************************************************
!* ADD_WMO								*
!*									*
!* This subroutine adds a WMO header to a GRIB message.  The input and	*
!* output arrays may be the same.					*
!*									*
!* The Look Up Table (LUT) files are already assumed to be connected	*
!* to the input unit numbers.						*
!*									*
!* ADD_WMO  ( GBMIN, TYPE, LUNKWB, LUNTIM, LUNPRM, LUNGRD, LUNLVL,	*
!*	      GBMOUT, LENOUT, IRET )					*
!*									*
!* Input parameters:							*
!*	GBMIN (*)	CHAR*1		GRIB message			* 
!*	TYPE		CHAR*1		Message type:			*
!*						H = international	*
!*						A = AWIPS		*
!*	LUNKWB		INTEGER		Unit # of Model Designator LUT	*
!*	LUNTIM		INTEGER		Unit # of F hour Designator LUT *
!*	LUNPRM		INTEGER		Unit # of Parm Designator LUT	*
!*	LUNGRD		INTEGER		Unit # of Grid Designator LUT	*
!*	LUNLVL		INTEGER		Unit # of Level Designator LUT  *
!*									*
!* Output parameters:							*
!*	GBMOUT (LENOUT) CHAR*1		GRIB message with WMO header	*
!*	LENOUT		INTEGER		Number of bytes in output messg *
!*	IRET		INTEGER		Return code			*
!*					  0 = normal return		*
!*					 -1 = GBMIN is not a GRIB messg *
!*					 -2 = cannot set octet 11	*
!*					 -3 = cannot make WMO header	*
!*					-21 = cannot make F hour label	*
!*					-22 = cannot make parm label	*
!*					-23 = cannot make grid label	*
!*					-24 = cannot make level label	*
!**									*
!* Log:									*
!* K. Brill/EMC		 6/97						*
!************************************************************************
	CHARACTER*1	gbmin (*), gbmout (*), type
!*
	INTEGER		ipds (28)
	CHARACTER*1	oct11
	CHARACTER*6	hedr6
	CHARACTER*132   wmohdr
!*
!------------------------------------------------------------------------
        iret = 0
!
!*	Compute the length of the GRIB message.
!
        is5 = mova2i ( gbmin (5) )
        is6 = mova2i ( gbmin (6) )
        is7 = mova2i ( gbmin (7) )
        lng = ( is5 * 256 + is6 ) * 256 + is7
!
!*	Get the GRIB PDS information.
!

	CALL GET_PDS ( gbmin, ipds, iret )
	IF ( iret .ne. 0 ) RETURN
!
!*	Set the byte that follows KWB.
!
	CALL SET_O11 ( lunkwb, ipds (6), oct11, ier )
	IF ( iret .ne. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
!
!*	Construct the 6-character header.
!
	CALL SET_WMO ( ipds, luntim, lunprm, lungrd, lunlvl, type,
     +		       hedr6, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -20 + ier
	    RETURN
	END IF
!
!*	Build entire WMO header string.
!
	CALL MAK_WMO ( ipds, hedr6, oct11, wmohdr, lnghdr, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -3
	    RETURN
	END IF
!
!*	Add the header to the GRIB message.
!
	lenout = lng + lnghdr
	icin = lng
	DO i = lenout, ( lnghdr + 1 ), -1
	    gbmout (i) = gbmin (icin)
	    icin = icin - 1
	END DO
	DO i = 1, lnghdr
	    gbmout (i) = wmohdr (i:i)
	END DO
!*
	RETURN
	END
