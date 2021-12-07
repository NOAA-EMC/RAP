       PROGRAM MAKEPRECIP

        USE GRIB_MOD
!                .      .    .                                       .
! SUBPROGRAM:   MAKEPRECIP 
!   PRGMMR: MANIKIN        ORG: W/NP22     DATE:  07-03-07

! ABSTRACT: PRODUCES 3,6 or 12-HOUR TOTAL AND CONVECTIVE PRECIPITATION BUCKETS
!              AS WELL AS SNOWFALL ON THE ETA NATIVE GRID FOR SMARTINIT 

! PROGRAM HISTORY LOG:
!   07-03-07  GEOFF MANIKIN 
!   10-25-12  JEFF MCQUEEN
!   15-03-16  E. Rogers uses this code for NAM makeprecip for GRIB2
!   input
! REMARKS:
!   10-25-12 JTM UNIFIED make and add precip for different accum hours
!                addprecip6, addprecip12 and makeprecip all combined in
!                smartprecip
!                To call, must set all 4 fhrs
!                for 3 or 6 hour buckets, set fhr3,fh4 to -99
!                For 12 hour buckets: 
!                    smartprecip  fhr fhr-3 fhr-6 fhr-9 
! ATTRIBUTES:
!   LANGUAGE: FORTRAN-90
!   MACHINE:  WCOSS     
!======================================================================
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER FHR0,FHR1, FHR2, IARW, ISNOW
      CHARACTER*80 FNAME
      LOGICAL*1 LSUB

!C grib2
      INTEGER :: LUGB,LUGI,J,JDISC,JPDTN,JGDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
      INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD(200),PDS_RAIN_HOLD(200), &
                              PDS_CPCP_HOLD(200), PDS_GRAUPEL_HOLD(200)
      INTEGER,DIMENSION(:) :: PDS_SNOW_HOLD_EARLY(200), &
                              PDS_RAIN_HOLD_EARLY(200), &
                              PDS_CPCP_HOLD_EARLY(200), &
                              PDS_GRAUPEL_HOLD_EARLY(200)
      LOGICAL :: UNPACK
      INTEGER :: K,IRET,SUBINTVL,SUBSTART
      TYPE(GRIBFIELD) :: GFLD
!C grib2

      REAL,     ALLOCATABLE :: GRID(:)
      REAL,     ALLOCATABLE :: APCP1(:),APCP2(:)
      REAL,     ALLOCATABLE :: CAPCP1(:),CAPCP2(:)
      REAL,     ALLOCATABLE :: SNOW1(:),SNOW2(:)
      REAL,     ALLOCATABLE :: GRAUPEL1(:),GRAUPEL2(:)
      REAL,     ALLOCATABLE :: APCPOUT(:),CAPCPOUT(:),SNOWOUT(:)
      REAL,     ALLOCATABLE :: GRAUPELOUT(:)
      LOGICAL,  ALLOCATABLE :: MASK(:)
!--------------------------------------------------------------------------

      print *, 'into RAPSUB'
      FNAME='fort.  '
!====================================================================
!     FHR3 = -99 signals a 6 hour summation requested
!     FHR4 GT 00 signals a 12 hour summation requested
!     FHR1 GT FHR2 signals do a 3 hour subtraction of files
!     ISNOW = 1 do accumulated snow
      READ (5,*) FHR1, FHR2,IARW,ISNOW
!====================================================================

!==>  Make 3 hour buckets by subtracting fhr3 - fhr files
      LSUB=.FALSE.

      print *,  'enter FHR1, FHR2 ', &
                          FHR1, FHR2 

      IF (FHR1.GT.FHR2) THEN
        write(0,*) 'subtracting'
        SUBINTVL=FHR1-FHR2
        SUBSTART=FHR2
       FHR0=FHR2
       FHR2=FHR1
       FHR1=FHR0
       LSUB=.TRUE.
       write(0,*) 'reset so FHR0, FHR1, FHR2 are: ', FHR0, FHR1, FHR2
       ELSE

!==>  sum up precip files
        if (fhr3 .lt. 0) FHR3=FHR1-3
      ENDIF

        write(0,*) 'here with LSUB: ', LSUB


      LUGB=13;LUGI=14; LUGB2=15;LUGI2=16
! LUGB5=accum non-convective precip
! LUGB6=accum conv precip
! LUGB7=accum snow
      LUGB5=50; LUGB6=51; LUGB7=52; LUGB8=53; LUGB9=54

      ISTAT = 0

! -== GET SURFACE FIELDS ==-

!        allocate(gfld%fld(1200*1200))
        allocate(gfld%idsect(200))
        allocate(gfld%igdtmpl(200))
        allocate(gfld%ipdtmpl(200))
        allocate(gfld%idrtmpl(200))
!        allocate(gfld%discipline)
!        allocate(gfld%bmap(1200*1200))

        JIDS=-9999
        JPDTN=-1
        JPDT=-9999
        JGDTN=-1
        JGDT=-9999
        UNPACK=.false.

        WRITE(FNAME(6:7),FMT='(I2)')LUGB
        CALL BAOPENR(LUGB,FNAME,IRETGB)

        WRITE(FNAME(6:7),FMT='(I2)')LUGB2
        CALL BAOPENR(LUGB2,FNAME,IRETGB)

        write(0,*) 'trim(fname): ', trim(fname)

        write(0,*) 'IRETGB on BAOPEN: ', IRETGB

        call getgb2(LUGB2,LUGI2,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        write(0,*) 'IRET from init getgb2 call: ', IRET

        NUMVAL=gfld%ngrdpts

        write(0,*) 'NUMVAL ', NUMVAL

        if (IRET .ne. 0) STOP

        UNPACK=.true.
        
      ALLOCATE (MASK(NUMVAL),GRID(NUMVAL),STAT=kret)
      ALLOCATE (APCP1(NUMVAL),CAPCP1(NUMVAL),SNOW1(NUMVAL), &
           GRAUPEL1(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!   TOTAL PRECIP 

        write(0,*) 'have NUMVAL : ', NUMVAL

        write(0,*) 'allocate again?'
        allocate(gfld%fld(NUMVAL))
        allocate(gfld%bmap(NUMVAL))

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=9
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(LUGB,LUGI,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET_EARLY)

        write(0,*) 'IRET from GETGB2: ', IRET_EARLY

        if (IRET_EARLY .ne. 0) THEN
        
        write(0,*) 'set APCP1 to zero'
        APCP1=0.

        else
        
        write(0,*) 'size(APCP1): ', size(APCP1)
        write(0,*) 'size(gfld%fld): ', size(gfld%fld)

        APCP1=gfld%fld

        do K=1,200
        PDS_RAIN_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo

        endif

!  CONVECTIVE PRECIP
       J = 0;JPDS = -1;JPDS(3) = IGDNUM
       JPDS(5) = 063;JPDS(6) = 001
!      JPDS(13) = 1

        if (IARW .eq. 0) then

      write(0,*), 'FHR3 ', FHR3
      JPDS(14) = FHR3
      JPDS(15) = FHR1
        write(0,*) 'FHR0: ', FHR0
      if (fhr4.gt.0) JPDS(14)=FHR0
        write(0,*) 'JPDS(14) for cpcp now: ', JPDS(14)

        endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=10
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(LUGB,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        if (IRET .ne. 0) THEN

        write(0,*) 'set CAPCP1 to zero'
        CAPCP1=0.

        else

        CAPCP1=gfld%fld

        do K=1,200
        PDS_CPCP_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo

        endif

      if (ISNOW .eq. 1) THEN
 
!  SNOWFALL 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001

        if (IARW .eq. 0) then
         JPDS(14) = FHR3
         JPDS(15) = FHR1
        endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(LUGB,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        if (IRET .ne. 0) THEN
        
        write(0,*) 'set SNOW1 to zero'
        SNOW1=0.

        else

        SNOW1=gfld%fld

        do K=1,200
        PDS_SNOW_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo
        endif
! GRAUPEL 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 194;JPDS(6) = 001

        if (IARW .eq. 0) then
         JPDS(14) = FHR3
         JPDS(15) = FHR1
        endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=227
        JGDTN=-1
        JGDT=-9999
        UNPACK=.true.

        call getgb2(LUGB,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        if (IRET .ne. 0) THEN

        write(0,*) 'set GRAUPEL1 to zero'
        GRAUPEL1=0.

        else

        GRAUPEL1=gfld%fld

        do K=1,200
        PDS_GRAUPEL_HOLD_EARLY(K)=gfld%ipdtmpl(K)
        enddo
        
        endif

!end ISNOW check
        ENDIF

      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      ENDIF

!=======================================================
!  READ 2nd file
!=======================================================

      ALLOCATE (APCP2(NUMVAL),CAPCP2(NUMVAL),SNOW2(NUMVAL), &
           GRAUPEL2(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!     ACCUMULATED PRECIP 

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=8
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB2,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)
        APCP2=gfld%fld

        do K=1,200
        PDS_RAIN_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!     ACCUMULATED CONVECTIVE PRECIP
       J = 0;JPDS = -1;JPDS(3) = IGDNUM
       JPDS(5) = 063;JPDS(6) = 001
!      JPDS(13) = 1

        if (IARW .eq. 0) then
      JPDS(14) = FHR1
      JPDS(15) = FHR2
      IF (LSUB) JPDS(14)=FHR3
        endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=10
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB2,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        CAPCP2=gfld%fld

        do K=1,200
        PDS_CPCP_HOLD(K)=gfld%ipdtmpl(K)
        enddo
!
      IF(ISNOW .eq. 1) then

!     SNOWFALL
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 065;JPDS(6) = 001
        if (IARW .eq. 0) then
      JPDS(14) = FHR1
      JPDS(15) = FHR2
      IF (LSUB) JPDS(14)=FHR3
        endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=13
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB2,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        SNOW2=gfld%fld

        do K=1,200
        PDS_SNOW_HOLD(K)=gfld%ipdtmpl(K)
        enddo

!     GRAUPEL 
      J = 0;JPDS = -1;JPDS(3) = IGDNUM
      JPDS(5) = 194;JPDS(6) = 001
        if (IARW .eq. 0) then
      JPDS(14) = FHR1
      JPDS(15) = FHR2
      IF (LSUB) JPDS(14)=FHR3
        endif

        JIDS=-9999
        JPDTN=8
        JPDT=-9999
        JPDT(2)=227
        JGDTN=-1
        JGDT=-9999

        call getgb2(LUGB2,0,0,0,JIDS,JPDTN,JPDT,JGDTN,JGDT, &
                    UNPACK,K,GFLD,IRET)

        GRAUPEL2=gfld%fld

        do K=1,200
        PDS_GRAUPEL_HOLD(K)=gfld%ipdtmpl(K)
        enddo

! end ISNOW check
        endif

      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF

!=======================================================
!      OUTPUT 2 or 3 HR PRECIP BUCKETS
!=======================================================
      ALLOCATE (APCPOUT(NUMVAL),CAPCPOUT(NUMVAL),SNOWOUT(NUMVAL), &
         GRAUPELOUT(NUMVAL),STAT=kret)
      IF(kret.ne.0)THEN
       WRITE(*,*)'ERROR allocation source location: ',numval
       STOP
      END IF
   
!! LSUB --> 3 h total

      IF (LSUB) THEN
       APCPOUT=APCP2-APCP1
       CAPCPOUT=CAPCP2-CAPCP1
       if(isnow.eq.1) then
         SNOWOUT=SNOW2-SNOW1
         GRAUPELOUT=GRAUPEL2-GRAUPEL1
       endif
      ELSE
       APCPOUT=APCP2+APCP1
       CAPCPOUT=CAPCP2+CAPCP1
       if(isnow.eq.1) then
         SNOWOUT=SNOW2+SNOW1
         GRAUPELOUT=GRAUPEL2+GRAUPEL1
       endif
      ENDIF
 
! convert these to GRIB2 equivs

        if (IRET_EARLY .ne. 0) then
          gfld%ipdtmpl=PDS_RAIN_HOLD
        else
          gfld%ipdtmpl=PDS_RAIN_HOLD_EARLY
        endif

        gfld%ipdtmpl(9)=ihrs1

        do J=16,21
        gfld%ipdtmpl(J)=PDS_RAIN_HOLD(J)
        enddo

        gfld%ipdtmpl(22)=1

!        if (LSUB) then

        gfld%ipdtmpl(27)=SUBINTVL
        gfld%ipdtmpl(9)=SUBSTART


!! use of (27) here looks wrong!

!        write(0,*) 'gfld%ipdtmpl(27) bef: ', &
!                    gfld%ipdtmpl(27)
!        write(0,*) 'gfld%ipdtmpl(9) bef: ', &
!                    gfld%ipdtmpl(9)

!        gfld%ipdtmpl(27)=3
!        gfld%ipdtmpl(9)=FHR1

       write(0,*) 'gfld%ipdtmpl(27) aft: ', &
                   gfld%ipdtmpl(27)
       write(0,*) 'gfld%ipdtmpl(9) aft: ', &
                   gfld%ipdtmpl(9)

!        endif

      gfld%fld=APCPOUT
      gfld%ipdtmpl(2)=8
      IF (LSUB) KPDS(14)=FHR1
      gfld%discipline=0
      gfld%idrtnum=0
      gfld%idrtmpl=0
      gfld%idrtmpl(3)=2

      KPDS(5)=61
      print *, 'writing precip', KPDS(5),KPDS(14),KPDS(15),LUGB5,MAXVAL(APCPOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB5
      CALL BAOPEN(LUGB5,FNAME,IRETGB)
      call putgb2(LUGB5,GFLD,IRET)
      CALL BACLOSE(LUGB5,IRET)

      gfld%discipline=0
      gfld%ipdtmpl(2)=10
      gfld%fld=CAPCPOUT

      KPDS(5)=63
      print *, 'writing CAPCP', KPDS(5),KPDS(14),KPDS(15),LUGB7 , MAXVAL(CAPCPOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB7
      CALL BAOPEN(LUGB7,FNAME,IRET)
      call putgb2(LUGB7,GFLD,IRET)
      CALL BACLOSE(LUGB7,IRET)

      if(ISNOW.eq.1) then

      gfld%discipline=0
      gfld%ipdtmpl(2)=13
      gfld%fld=SNOWOUT

      KPDS(5)=65
      print *, 'writing SNOW', KPDS(5),KPDS(14),KPDS(15),LUGB8, MAXVAL(SNOWOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB8
      CALL BAOPEN(LUGB8,FNAME,IRET)
      call putgb2(LUGB8,GFLD,IRET)
      CALL BACLOSE(LUGB8,IRET)

      gfld%discipline=0
      gfld%ipdtmpl(2)=227
      gfld%fld=GRAUPELOUT

      KPDS(5)=194
      print *, 'writing GRAUPEL', KPDS(5),KPDS(14),KPDS(15),LUGB9, &
            MAXVAL(GRAUPELOUT)
      WRITE(FNAME(6:7),FMT='(I2)')LUGB9
      CALL BAOPEN(LUGB9,FNAME,IRET)
      call putgb2(LUGB9,GFLD,IRET)
      CALL BACLOSE(LUGB9,IRET)

!end ISNOW check
      endif

      STOP
      END
