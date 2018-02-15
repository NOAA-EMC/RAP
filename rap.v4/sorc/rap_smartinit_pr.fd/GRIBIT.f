      SUBROUTINE GRIBIT(ID,RITEHD,GRID,DATE,IFHR,DECI)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GRIBIT      POST FIELDS IN GRIB1
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-06-18       
C     
C ABSTRACT:
C     THIS ROUTINE POSTS THE DATA IN THE PASSED ARRAY GRID
C     TO THE OUTPUT FILE IN GRIB1 FORMAT.
C     
C PROGRAM HISTORY LOG:
C   93-06-18  RUSS TREADON
C   93-11-23  RUSS TREADON - REMOVED CODE GENERATING GRIB INDEX FILE.
C   98-07-17  MIKE BALDWIN - REMOVED LABL84, NOW USING ID
C   02-06-17  MIKE BALDWIN - WRF VERSION
C   05-12-05  H CHUANG - ADD CAPABILITY TO OUTPUT OFF-HOUR FORECAST WHICH HAS
c               NO INPACTS ON ON-HOUR FORECAST
C     
C USAGE:    CALL GRIBIT(IFLD,ILVL,GRID,IMOUT,JMOUT)
C   INPUT ARGUMENT LIST:
C     IFLD     - FIELD ID TAG.
C     ILVL     - INTEGER TAG FOR LEVEL OF FIELD.
C     GRID     - FIELD TO BE POSTED IN GRIB.
C     IMOUT    - FIRST DIMENSION OF OUTPUT GRID.
C     JMOUT    - SECOND DIMENSION OF OUTPUT GRID.
C
C   OUTPUT ARGUMENT LIST: 
C     
C   OUTPUT FILES:
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C     GETENV   - CRAY SUBROUTINE TO GET VALUE OF ENVIRONMENT VARIABLE.
C     MINMAX   - DETERMINES MIN/MAX VALUES IN AN ARRAY.
C     WRYTE    - WRITE DATA OUT BY BYTES.
C     GET_BITS   - COMPUTE NUMBER OF BITS 
C     VARIOUS W3LIB ROUTINES
C     LIBRARY:
C       COMMON   - CTLBLK
C                  RQSTFLD
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     INCLUDE GRID DIMENSIONS.  SET/DERIVE PARAMETERS.
C

C     
C     GRIB1 PARAMETERS.
C        MNBIT  = MINIMUM NUMBER OF BITS TO USE IN PACKING.
C        MXBIT  = MAXIMUM NUMBER OF BITS TO USE IN PACKING.
C        LENPDS = LENGTH OF GRIB1 PDS.
C        LENGDS = LENGTH OF GRIB1 GDS.
C     
      PARAMETER (MNBIT=0,MXBIT=16,LENPDS=28,LENGDS=32)
      PARAMETER(IM=177,JM=129,MAXLEV=50)
      PARAMETER (SMALL=1.E-6,SPVAL=9.9E10)
C
C     
      LOGICAL RUN,FIRST,RESRT,SIGMA,OLDRD,STRD,RITEHD
      LOGICAL NORTH
      CHARACTER*1 KBUF(30+LENPDS+LENGDS+IM*JM*(MXBIT+2)/8)
      CHARACTER*1 KBUF_S(30+LENPDS+LENGDS+IM*JM*(MXBIT+2)/8)
      CHARACTER*1  IFLAG
      CHARACTER*4  RESTHR,BLANK
      CHARACTER*6  CRUN,PROJ,DATSET
      CHARACTER*10  DESCR2,DESCR3
      CHARACTER*28 PDS
      CHARACTER*50 ENVAR
      CHARACTER*80 FNAME,FNAME_S,OPATH
      CHARACTER*90 CMD
      INTEGER IBDSFL(9)
      INTEGER IGRD(IM,JM),IGDS(18),IBMASK(IM,JM),ID(25)
      INTEGER ICENT,IYY,IMM,IDD,IHRST
      REAL GRID(IM,JM)
      INTEGER DXVAL,DYVAL,CENLAT,CENLON,TRUELAT1,TRUELAT2,STANDLON
      INTEGER	LATSTART,LONSTART
C     
C     THE BELOW VARIABLE ARE ONLY NEEDED FOR THE CALL TO W3FI63.
!      REAL DATAFLD(IMOUT,JMOUT)
      REAL DATAFLD(IM,JM)
      INTEGER IBMAP(IM,JM)
      INTEGER KGDS(20),KPTR(16)
!      LOGICAL KBMS(IMOUT,JMOUT)
      LOGICAL KBMS(IM,JM)
      LOGICAL DONE, NEWFILE, NEWFILE_S
      INTEGER IH(5),DATE
      INTEGER ICHECK, ILOAD
      INTEGER LUNOUT
C     
C     SET DEFAULT GRIB1 PARAMETERS.  
C     PARAMETERS MNBIT, MXBIT, IBX, AND NBIT ARE USED 
C     IN THE CALL TO GET_BITS.
C        IBX    = DESIRED BINARY PRECISION.
C        NBIT   = NUMBER OF BITS TO USE IN PACKING DATA.
C     
      DATA IBX,NBIT / 0, 12 /
      DATA BLANK /'    '/
      DATA DONE /.FALSE./
      DATA ICHECK / 1 /
      DATA ILOAD / 1 /
      SAVE OPATH
C
C*****************************************************************************
C     START GRIBIT HERE.
C
C     ALL TASKS MUST CALL COLLECT BUT ONLY TASK 0 CAN EXECUTE THE REMAINDER 
C      OF GRIBIT
C
      DATSET='RAPPR'
      LUNOUT=70
      DO J=1,JM
      DO I=1,IM
        IGRD(I,J)=0.
      ENDDO
      ENDDO
c      CALL COLLECT(GRID,GRIDO)

c      IF ( ME .EQ. 0 ) THEN
!      ist = rtc()

      NEWFILE = .FALSE.
C     SET NUMBER OF OUTPUT GRID POINTS.
      IJOUT = IM*JM
C     
C     PREPARE GRIB PDS
C     
C     SET ARRAY ID VALUES TO GENERATE GRIB1 PDS.  
C        ID(1)  = NUMBER OF BYTES IN PRODUCT DEFINITION SECTION (PDS)
C        ID(2)  = PARAMETER TABLE VERSION NUMBER
C        ID(3)  = IDENTIFICATION OF ORIGINATING CENTER
C        ID(4)  = MODEL IDENTIFICATION (ALLOCATED BY ORIGINATING CENTER)
C        ID(5)  = GRID IDENTIFICATION
C        ID(6)  = 0 IF NO GDS SECTION, 1 IF GDS SECTION IS INCLUDED
C        ID(7)  = 0 IF NO BMS SECTION, 1 IF BMS SECTION IS INCLUDED
C        ID(8)  = INDICATOR OF PARAMETER AND UNITS (TABLE 2)
C        ID(9)  = INDICATOR OF TYPE OF LEVEL       (TABLE 3)
C        ID(10) = VALUE 1 OF LEVEL (=0 FOR 1-100,102,103,105,107,
C          109,111,113,115,117,119,125,160,200,201 LEVEL IS IN ID WORD 11)
C        ID(11) = VALUE 2 OF LEVEL
C        ID(12) = YEAR OF CENTURY
C        ID(13) = MONTH OF YEAR
C        ID(14) = DAY OF MONTH
C        ID(15) = HOUR OF DAY
C        ID(16) = MINUTE OF HOUR   (IN MOST CASES SET TO 0)
C        ID(17) = FCST TIME UNIT
C        ID(18) = P1 PERIOD OF TIME
C        ID(19) = P2 PERIOD OF TIME
C        ID(20) = TIME RANGE INDICATOR
C        ID(21) = NUMBER INCLUDED IN AVERAGE
C        ID(22) = NUMBER MISSING FROM AVERAGES
C        ID(23) = CENTURY
C        ID(24) = RESERVED - SET TO 0
C        ID(25) = SCALING POWER OF 10
C
C     
C        PREPARE DATE PART OF GRIB PDS RECORD.

      iyy=int(date/1000000)
      icent=(iyy-1)/100 + 1
      imm=int(int(mod(date,1000000)/100)/100)
      idd=int(mod(date,10000)/100)
      ihrst=mod(date,100)
      print *, 'inside gribit ', ID(8)

c         ICENT      = (SDAT(3)-1)/100 + 1
c         IYY        = SDAT(3) - (ICENT-1)*100
c         IMM        = SDAT(1)
c         IDD        = SDAT(2)
         AYEAR0     = IYY
         AMNTH0     = IMM
         ADAY0      = IDD
         AGMT0      = IHRST
         ID(01)     = 28
         IF (ID(2) .NE. 129)THEN
          IF(ID(2).NE. 130)THEN
           ID(2)     = 2
          END IF
         END IF 
         ID(03)     = 7
         ID(12)     = MOD(IYY,100)
         ID(13)     = IMM
         ID(14)     = IDD
         ID(15)     = IHRST
         ID(16)     = 0
C         ID(16)     = IMIN
         ID(17)     = 1
C
C    ASSUMING ID(18-20), (P1, P2, TIME RANGE INDICATOR) 
C    ARE PASSED IN CORRECTLY IF NOT AN INSTANTANEOUS FIELD
C   
         IF (ID(20).EQ.0) THEN
          ID(18)     = IFHR 
          ID(19)     = 0
         ENDIF
! CHUANG: TO OUTPUT OFF-HOUR FORECAST, I USED MIN INSTEAD OF HOUR AS FORECAST UNIT
! ALOS, SINCE ONLT TIME RANGE TYPE 10 USES 2 BYTES TO STORE TIME, MODIFICATION WAS
! MADE TO USE TYPE 10 AS TIME RANGE INDICATOE WHEN FORECST MINS ARE LARGER THAN 254,	
! WHICH MEANS ALL THE ACCUMULATED AND TIME-AVERAGED QUANTITY ARE VERIFIED AT ONE TIME
! INSTEAD OF AT A TIME RANGE. 
         print *, 'ifmin check ', IFMIN
	 IF(IFMIN .GE. 1)THEN
	   ID(17)     = 0
	   TOTMIN=IFHR*60+IFMIN
	   IF(TOTMIN .LE. 256)THEN  	     
	     IF (ID(20).EQ.0)ID(18)=IFHR*60+IFMIN
           ELSE
	     ID(20)=10
	     ID(18)=IFHR*60+IFMIN     
	   END IF 
	  END IF

         ID(21)     = 0
         ID(22)     = 0
         ID(23)     = ICENT
         ID(24)     = 0

         print *, 'ID check ', ID(17), ID(20)
C
C     
C        SET OUTPUT GRID TYPE.  WE ASSUME KGYTPE HOLDS THE GRIB
C        ID FOR THE OUTPUT GRID.  
C
c         KGTYP = KGTYPE
          KGTYP = 195 
C     
C        SET GRID TYPE ID(5)
C        GENERATING PROGRAM ID(4)
C
!         IJOUT      = IMOUT*JMOUT
         IJOUT      = IM*JM
         ID(4) = 105 
         ID(5) = KGTYP
C
C        ID(6) =0 IF NO GDS SECTION, =1 IF GDS INCLUDED, 
C                 ALWAYS INCLUDE GDS
C
         ID(6) = 1
C     
C        SET DATA TYPE ID(8) AND SURFACE ID(9).
C
C     DON'T SET PARAMETER IF PRECIP TYPE, SINCE THERE ARE
C     4 PARAMETER NUMBERS FOR THE SAME IFLD
C
!         IF (ID(8).LT.140.OR.ID(8).GT.143) ID(8) = IQ(IDENT(IFLD))
C   05-08-24  GEOFF MANIKIN - ADDED IN DOMINANT PRECIP TYPE
C                              TO PTYPE IF STATEMENT
c         IF (ID(8).LT.140.OR.ID(8).GT.143) THEN
c            IF (ID(8).LT.203.OR.ID(8).GT.206
c     *          .OR.ID(2).NE.129) THEN
c               ID(8)=IQ(IDENT(IFLD))
c            ENDIF
c          ENDIF

c         IF (ID(9).EQ.0) ID(9) = IS(IDENT(IFLD))
C     
C     END OF GRIB PDS LABEL PREPARATION.
C
C     
C     SET DECIMAL SCALING (IDECI) FROM LIST IN INCLUDE FILE 
C     RQSTFLD.  A CALL TO GET_BITS WILL COMPUTE THE NUMBER OF
C     BITS NECESSARY TO PACK THE DATA BASED ON THE RANGE OF 
C     THE FIELD.  THE FIELD IS SCALED TO THIS PRECISION AND
C     RETURNED FOR PACKING BY THE GRIB PACKER.
C     
      IBM = 0
      IBITM = 0
      SGDG  = DECI 
c     set bitmap
      DO J=1,JM
      DO I=1,IM
        IF(ABS(GRID(I,J)-SPVAL).GT.SMALL) THEN
             ibmap(i,j) = 1
             ibitm = ibitm+1
        ELSE
             ibmap(i,j) = 0
        ENDIF
      ENDDO
      ENDDO
!     set bitmap
C
C        ID(7) =0 IF NO BMS SECTION, =1 IF BMS INCLUDED
C
      IF (IBITM.EQ.IJOUT) THEN
        ID(7) = 0
        IBM = 0
      ELSE
        ID(7) = 1
        IBM = 1
      ENDIF
      print *, 'before get_bits ', GRID(75,26), DECI 
      CALL GET_BITS(IBM,SGDG,IJOUT,IBMAP,GRID,
     &                IDECI,GRID,GMIN,GMAX,NBIT)
      print *, 'after get_bits ', GRID(75,26), IDECI
C
C        ID(25) = SCALING POWER OF 10
C
      ID(25) = IDECI
C     
C     GENERATE COMPLETE GRIB1 MESSAGE USING W3FI72.
C        ITYPE  = 0 SPECIFIES REAL DATA TO BE PACKED.
C        IGRD   = DUMMY ARRAY FOR INTEGER DATA.
C        IBITL  = NBIT TELLS W3FI72 TO PACK DATA USING NBIT BITS.
C        IPFLAG = 0 IS PDS INFORMATION IN USER ARRAY ID.
C                 1 IS PDS (GENERATED ABOVE BY W3FP12).
C        ID     = (DUMMY) ARRAY FOR USER DEFINED PDS.
C        IGFLAG = 0 TELLS W3FI72 TO MAKE GDS USING IGRID.
C                 1 IS GDS GENERATED BY USER IN ARRAY IGDS
C        IGRID  = GRIB1 GRID TYPE (TABLE B OF ON388).
C        IGDS   = ARRAY FOR USER DEFINED GDS.
C        ICOMP  = 0 FOR EARTH ORIENTED WINDS,
C                 1 FOR GRID ORIENTED WINDS.
C        IBFLAG = 0 TELLS W3FI72 TO MAKE BIT MAP FROM USER
C                 SUPPLIED DATA.
C        IBMASK = ARRAY CONTAINING USER DEFINED BIT MAP.
C        IBLEN  = LENGTH OF ARRAY IBMASK.
C        IBDSFL = ARRAY CONTAINING TABLE 11 (ON388) FLAG INFORMATION.
C        NPTS   = LENGTH OF ARRAY GRID OR IGRD.  MUST AGREE WITH IBLEN.
C     
C     INTIALIZE VARIABLES.

      ITYPE  = 0
C
      IBITL  = MIN(NBIT,MXBIT)
C
      IPFLAG = 0
C
!MEB  IGFLAG = 0
      IGFLAG = 1  ! set to 1 so that IGDS is defined here instead of w3lib
      IGRID  = ID(5)
      IF (IGRID.EQ.26) IGRID=6
      DO 20 K = 1,18
         IGDS(K) = 0
 20   CONTINUE
      CALL W3FI71(195,IGDS,IERR)
	write(6,*) 'IGDS= ', IGDS
C       LAMBERT CONFORMAL:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
C           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
C           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
C           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
C                                              1=SOUTH POLE ON PLANE,
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = NOT USED
C           IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
C                      SECANT CONE CUTS THE SPERICAL EARTH
C           IGDS(16) = SECOND LATITUDE ...
C           IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
C           IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
C
      ICOMP  = 1
      IF (INDEX(PROJ,'LOLA').NE.0) ICOMP = 0
      IBFLAG = 0
      IBLEN  = IJOUT
      DO 30 K = 1,9
         IBDSFL(K) = 0
 30   CONTINUE
C
      print *, 'before w3fi72 ', GRID(75,26)
      CALL W3FI72(ITYPE,GRID,IGRD,IBITL,
     X            IPFLAG,ID,PDS,
     X            IGFLAG,IGRID,IGDS,ICOMP,
     X            IBFLAG,IBMAP,IBLEN,
     X            IBDSFL,
     X            NPTS,KBUF,ITOT,IER)
      print *, 'after w3fi72 ', GRID(75,26)
C     
C     EXPLICITLY SET BYTE 12 OF KBUF (BYTE 4 OF THE PDS)
C     TO 2.  THIS WILL REFER ALL QUANTITIES TO PARAMETER
C     TABLE VERSION 2 OF WHICH TABLE VERSION 1 IS A SUBSET.
C     THIS IS NEEDED BECAUSE THE W3 ROUTINES HARDWIRE THIS
C     VALUE TO 1 YET SOME OF THE OUTPUT VARIABLES ARE ONLY 
C     DEFINED IN VERSION 2 OF THE PARAMETER TABLE.
C
!--- Comment out; BYTE 4 (PDS Octet 4) = 2 or 129 (see ON388, Table 2)
!
!!      KBUF(12)=CHAR(2)
C
      IF (IER.NE.0) THEN
         WRITE(6,*)'GRIBIT:  W3FI72 ERROR DID NOT POST THIS FIELD'
         RETURN
      ENDIF
C     
C     ON FIRST ENTRY MAKE OUTPUT DIRECTORY.  SET SWITCH (RITEHD)
C     TO FALSE FOR SUBSEQUENT ENTRIES.
      IF (RITEHD) THEN
C
C        PUT FORECAST HOUR INTO DIR PREFIX FOR GRIB FILE.
         IHR = IFHR
C     
C        GET FULL PATH FOR OUTPUT FILE FROM ENVIRONMENT VARIABLE
C        COMSP WHICH IS SET IN THE SCRIPT RUNNING THE MODEL.
C     
C        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
         ENVAR = ' '
         RESTHR = ' '
         CALL GETENV('COMSP',ENVAR)
c         CALL GETENV('tmmark',RESTHR)
         RESTHR='tm00'
         KDAT = INDEX(DATSET,' ') -1
         IF (KDAT.LE.0) KDAT = LEN(DATSET)
         KENV = INDEX(ENVAR,' ') -1
         IF (KENV.LE.0) KENV = LEN(ENVAR)
         KTHR = INDEX(RESTHR,' ') -1
         IF (KTHR.LE.0) KTHR = LEN(RESTHR)
C     
C        CONSTRUCT FULL PATH-FILENAME FOR OUTPUT FILE
C     
         IF (ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).EQ.BLANK) THEN
	  IF(IFMIN .GE. 1)THEN
	   WRITE(DESCR2,1011) IHR
	   WRITE(DESCR3,1011) IFMIN
	   FNAME = DATSET(1:KDAT) // DESCR2  //':'// DESCR3(1:2)
          ELSE 	  
           IF(IHR.LT.100)THEN
            WRITE(DESCR2,1011) IHR
           ELSE
            WRITE(DESCR2,1013) IHR
           END IF
 1011      FORMAT('.GrbF',I2.2)
 1013      FORMAT('.GrbF',I3.3)
           FNAME = DATSET(1:KDAT) // DESCR2
	  END IF
C
         ELSEIF(ENVAR(1:4).EQ.BLANK.AND.RESTHR(1:4).NE.BLANK) THEN
	  IF(IFMIN .GE. 1)THEN
	   WRITE(DESCR3,1012) IFMIN
           IF (IHR.LT.100) THEN
	      WRITE(DESCR2,1012) IHR
              FNAME = DATSET(1:KDAT) // DESCR2(1:2)  //':'// DESCR3(1:2)
     &	         //'.'// RESTHR
           ELSE
	      WRITE(DESCR2,1014) IHR
              FNAME = DATSET(1:KDAT) // DESCR2(1:3)  //':'// DESCR3(1:2)
     &	         //'.'// RESTHR
           ENDIF
	  ELSE
           IF (IHR.LT.100) THEN
             WRITE(DESCR2,1012) IHR
             FNAME = DATSET(1:KDAT) // DESCR2(1:2)  //'.'// RESTHR
           ELSE
             WRITE(DESCR2,1014) IHR
             FNAME = DATSET(1:KDAT) // DESCR2(1:3)  //'.'// RESTHR
           ENDIF
          end if
         ELSE
	  IF(IFMIN .GE. 1)THEN
	   WRITE(DESCR3,1012) IFMIN
           IF (IHR.LT.100) THEN
	     WRITE(DESCR2,1012) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:2)  
     &	     //':'// DESCR3(1:2) //'.'// RESTHR
           ELSE
	     WRITE(DESCR2,1014) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:3)  
     &	     //':'// DESCR3(1:2) //'.'// RESTHR
           ENDIF
	  ELSE
           IF (IHR.LT.100) THEN
             WRITE(DESCR2,1012) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:2)
     &              //'.'// RESTHR
 1012        FORMAT(I2.2)
 1014        FORMAT(I3.3)
           ELSE
             WRITE(DESCR2,1014) IHR
             FNAME = ENVAR(1:KENV) // DATSET(1:KDAT) // DESCR2(1:3)
     &              //'.'// RESTHR
           ENDIF
          end if
         ENDIF
C
C        ASSIGN AND OPEN UNIT FOR GRIB DATA FILE.
c         if ( num_servers .eq. 0 ) then
         CLOSE(LUNOUT)
         CALL BAOPEN(LUNOUT,'fort.70',IER)
         IF (IER.NE.0) WRITE(6,*)
     X        'GRIBIT:  BAOPEN ERROR FOR GRIB DATA ',
     X        'FILE.  IER=',IER
         WRITE(6,*)'GRIBIT:  OPENED ',LUNOUT,
     X        ' FOR GRIB DATA '
c         end if
C     
C        SET OPEN-UNIT FLAGS TO FALSE.
         RITEHD = .FALSE.
         NEWFILE = .TRUE.
      ENDIF
C
C     WRITE GRIB1 MESSAGE TO OUTPUT FILE.
      CALL WRYTE(LUNOUT,ITOT,KBUF)
C     
C     WRITE DIAGNOSTIC MESSAGE.
C        ID(8)  = INDICATOR OF PARAMETER AND UNITS (TABLE 2)
C        ID(9)  = INDICATOR OF TYPE OF LEVEL       (TABLE 3)
C        ID(10) = VALUE 1 OF LEVEL  (0 FOR 1-100,102,103,105,107
C              111,160   LEVEL IS IN ID WORD 11)
C        ID(11) = VALUE 2 OF LEVEL
c666   WRITE(6,1050) ID(8),FIELD(IFLD),ID(9),ID(10),ID(11)
 1050 FORMAT('GRIBIT:  ',I3,1X,A20,1X,I3,1X,I5,1X,I5)
C     
C     END OF ROUTINE.
C     
c      END IF
!      time_output = time_output + rtc() - ist
      print *, 'leaving gribit ', GRID(75,26)
      RETURN
      END
C        IGDS VARIES DEPENDING ON GRID REPRESENTATION TYPE.
C
C       LAT/LON GRID:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG A LATITUDE
C           IGDS( 5) = NO. OF POINTS ALONG A LONGITUDE MERIDIAN
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH - IVE)
C           IGDS( 7) = LONGITUDE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LATITUDE OF EXTREME POINT (SOUTH - IVE)
C           IGDS(10) = LONGITUDE OF EXTREME POINT (WEST - IVE)
C           IGDS(11) = LATITUDE INCREMENT
C           IGDS(12) = LONGITUDE INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) =   ... NOT USED FOR THIS GRID
C           IGDS(19) - IGDS(91) FOR GRIDS 37-44, NUMBER OF POINTS
C                      IN EACH OF 73 ROWS.
C
C       GAUSSIAN GRID:
C           IGDS( 1) = ... THROUGH ...
C           IGDS(10) =   ... SAME AS LAT/LON GRID
C           IGDS(11) = NUMBER OF LATITUDE LINES BETWEEN A POLE
C                      AND THE EQUATOR
C           IGDS(12) = LONGITUDE INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) =   ... NOT USED FOR THIS GRID
C
C       SPHERICAL HARMONICS:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = J - PENTAGONAL RESOLUTION PARAMETER
C           IGDS( 5) = K - PENTAGONAL RESOLUTION PARAMETER
C           IGDS( 6) = M - PENTAGONAL RESOLUTION PARAMETER
C           IGDS( 7) = REPRESENTATION TYPE (CODE TABLE 9)
C           IGDS( 8) = REPRESENTATION MODE (CODE TABLE 10)
C           IGDS( 9) = ... THROUGH ...
C           IGDS(18) =   ... NOT USED FOR THIS GRID
C
C       POLAR STEREOGRAPHIC:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
C           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
C           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
C           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
C                                              1=SOUTH POLE ON PLANE,
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) =   .. NOT USED FOR THIS GRID
C
C       MERCATOR:
C           IGDS( 1) = ... THROUGH ...
C           IGDS(12) =   ... SAME AS LAT/LON GRID
C           IGDS(13) = LATITUDE AT WHICH PROJECTION CYLINDER
C                        INTERSECTS EARTH
C           IGDS(14) = SCANNING MODE FLAGS
C           IGDS(15) = ... THROUGH ...
C           IGDS(18) =   .. NOT USED FOR THIS GRID
C
C       LAMBERT CONFORMAL:
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6)
C           IGDS( 4) = NO. OF POINTS ALONG X-AXIS
C           IGDS( 5) = NO. OF POINTS ALONG Y-AXIS
C           IGDS( 6) = LATITUDE OF ORIGIN (SOUTH -IVE)
C           IGDS( 7) = LONGITUTE OF ORIGIN (WEST -IVE)
C           IGDS( 8) = RESOLUTION FLAG (CODE TABLE 7)
C           IGDS( 9) = LONGITUDE OF MERIDIAN PARALLEL TO Y-AXIS
C           IGDS(10) = X-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(11) = Y-DIRECTION GRID LENGTH (INCREMENT)
C           IGDS(12) = PROJECTION CENTER FLAG (0=NORTH POLE ON PLANE,
C                                              1=SOUTH POLE ON PLANE,
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = NOT USED
C           IGDS(15) = FIRST LATITUDE FROM THE POLE AT WHICH THE
C                      SECANT CONE CUTS THE SPERICAL EARTH
C           IGDS(16) = SECOND LATITUDE ...
C           IGDS(17) = LATITUDE OF SOUTH POLE (MILLIDEGREES)
C           IGDS(18) = LONGITUDE OF SOUTH POLE (MILLIDEGREES)
C
C       ARAKAWA SEMI-STAGGERED E-GRID ON ROTATED LAT/LON GRID
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [201]
C           IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
C                            INCLUDED ON GRID
C           IGDS( 5) = NJ  - DUMMY SECOND DIMENSION; SET=1
C           IGDS( 6) = LA1 - LATITUDE  OF FIRST GRID POINT
C           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
C           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
C           IGDS( 9) = LA2 - NUMBER OF MASS POINTS ALONG
C                            SOUTHERNMOST ROW OF GRID
C           IGDS(10) = LO2 - NUMBER OF ROWS IN EACH COLUMN
C           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
C           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) = ... NOT USED FOR THIS GRID (SET TO ZERO)
C
C       ARAKAWA FILLED E-GRID ON ROTATED LAT/LON GRID
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [202]
C           IGDS( 4) = NI  - TOTAL NUMBER OF ACTUAL DATA POINTS
C                            INCLUDED ON GRID
C           IGDS( 5) = NJ  - DUMMY SECOND DIMENTION; SET=1
C           IGDS( 6) = LA1 - LATITUDE LATITUDE OF FIRST GRID POINT
C           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
C           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
C           IGDS( 9) = LA2 - NUMBER OF (ZONAL) POINTS IN EACH ROW
C           IGDS(10) = LO2 - NUMBER OF (MERIDIONAL) POINTS IN EACH
C                            COLUMN
C           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
C           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) = ... NOT USED FOR THIS GRID
C
C       ARAKAWA STAGGERED E-GRID ON ROTATED LAT/LON GRID
C           IGDS( 1) = NUMBER OF VERTICAL COORDINATES
C           IGDS( 2) = PV, PL OR 255
C           IGDS( 3) = DATA REPRESENTATION TYPE (CODE TABLE 6) [203]
C           IGDS( 4) = NI  - NUMBER OF DATA POINTS IN EACH ROW
C           IGDS( 5) = NJ  - NUMBER OF ROWS
C           IGDS( 6) = LA1 - LATITUDE OF FIRST GRID POINT
C           IGDS( 7) = LO1 - LONGITUDE OF FIRST GRID POINT
C           IGDS( 8) = RESOLUTION AND COMPONENT FLAG (CODE TABLE 7)
C           IGDS( 9) = LA2 - CENTRAL LATITUDE
C           IGDS(10) = LO2 - CENTRAL LONGTITUDE
C           IGDS(11) = DI  - LONGITUDINAL DIRECTION INCREMENT
C           IGDS(12) = DJ  - LATITUDINAL  DIRECTION INCREMENT
C           IGDS(13) = SCANNING MODE FLAGS (CODE TABLE 8)
C           IGDS(14) = ... THROUGH ...
C           IGDS(18) = ... NOT USED FOR THIS GRID
C
