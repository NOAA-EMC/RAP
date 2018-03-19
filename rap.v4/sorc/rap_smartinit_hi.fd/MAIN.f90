     PROGRAM SMARTINIT
       use grddef
       use rdgrib
       use GRIB_MOD
       use pdstemplates

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    SMARTINITRAPPR    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 11-10-06
!
! ABSTRACT:   THIS CODE TAKES NATIVE RAP FILES AND GENERATES
!          2.5 KM OUTPUT OVER PUERTO RICO CONTAINING NDFD ELEMENTS
!
! PROGRAM HISTORY LOG:
!   11-10-06   G MANIKIN  - ADAPT FOR PUERTO RICO OUTPUT 
!
      PARAMETER(IM=321,JM=225,MAXLEV=50)
      PARAMETER(ITOT=IM*JM)
      PARAMETER (CAPA=0.28589641)
      REAL,PARAMETER :: SPVAL=9.9E10
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER DATE
      INTEGER P, PP, R, RR, S, SS, W, WW, X, XX
      LOGICAL*1 VALIDPT(IM,JM)
!      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
      LOGICAL RITEHD,NEED12,BITMAP(ITOT)
      CHARACTER *50 WXSTRING(IM,JM)
      CHARACTER *80 FNAMEOUT
!
      PARAMETER(MBUF=2000000,JF=1000000)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86, &
        PQ0=379.90516,P1000=100000.0)

      DIMENSION ID(25)
      INTEGER FHR,CYC,HOUR
      REAL, ALLOCATABLE :: HGHT(:,:,:), T(:,:,:), &
        Q(:,:,:),UWND(:,:,:),VWND(:,:,:), &
        PSFC(:,:),PMID(:,:,:),ZSFC(:,:), &
        RH(:,:,:),PBLMARK(:,:),REFC(:,:), &
        T2(:,:),Q2(:,:),BLI(:,:),U10(:,:),V10(:,:), &
        CWR(:,:),TCLD(:,:),WETFRZ(:,:),BLR(:,:), &
        LCLD(:,:),MCLD(:,:),HCLD(:,:), SKY(:,:), &
        DIRTRANS(:,:),T1(:,:),D2(:,:),BASEZ(:,:), &
        WX(:,:),THUNDER(:,:),VIS(:,:),GUST(:,:), &
        TOPO(:,:),WGUST(:,:),MIXHGT(:,:), &
        MGTRANS(:,:),LAL(:,:),CEIL(:,:),SLP(:,:)
      REAL, ALLOCATABLE :: TOPO_NDFD(:,:),ROUGH(:,:),COAST(:,:), &
        DOWNT(:,:),DOWNDEW(:,:),DOWNU(:,:),DOWNP(:,:), &
        DOWNV(:,:),DOWNQ(:,:),GRIDWX(:,:) 
      TYPE (GINFO) :: GDIN
      TYPE (GRIBFIELD) :: GFLD, GFLD8

      allocate (hght(im,jm,maxlev),t(im,jm,maxlev),q(im,jm,maxlev), &
         uwnd(im,jm,maxlev),vwnd(im,jm,maxlev),psfc(im,jm), &
         zsfc(im,jm),rh(im,jm,maxlev),pblmark(im,jm), &
         u10(im,jm),v10(im,jm),d2(im,jm),cwr(im,jm),gust(im,jm), &
         blr(im,jm),t2(im,jm),q2(im,jm),bli(im,jm), &
         tcld(im,jm),wetfrz(im,jm),dirtrans(im,jm),t1(im,jm), &
         lcld(im,jm),mcld(im,jm),hcld(im,jm),sky(im,jm), &
         wx(im,jm),thunder(im,jm),vis(im,jm),refc(im,jm), &
         topo(im,jm),wgust(im,jm),pmid(im,jm,maxlev))
      allocate (topo_ndfd(im,jm),rough(im,jm),coast(im,jm), &
         downt(im,jm), &
         downdew(im,jm),downu(im,jm),downv(im,jm),downq(im,jm), &
         gridwx(im,jm),mixhgt(im,jm),basez(im,jm),downp(im,jm), &
         mgtrans(im,jm),lal(im,jm),ceil(im,jm),slp(im,jm))

      print *, 'start '
      READ (5,*) FHR
      READ (5,*) CYC
      print *, 'into main ', FHR
      print *, 'cyc ', CYC
      FHR3=FHR-3
      FHR6=FHR-6
      FHR12=FHR-12
!      SPVAL=9.9E10

!  READ THE GRIB FILES FROM THE RAP.  WE NEED TO READ A
!   FULL COMPLEMENT OF DATA EVERY 3 HOURS.  FOR THE IN-BETWEEN
!   FCST HOURS, WE ONLY NEED TO KEEP TRACK OF DOWNSCALED TEMP
!   AND DEW POINT (FOR MIN/MAX PURPOSES), SO WE NEED ONLY A VERY
!   LIMITED AMOUNT OF DATA.   FOR THE ANALYSIS TIME, WE NEED A
!   SPECIAL CALL OF THE FULL DATA SET BUT WITHOUT PRECIP

        print *, 'before getgrib'
        CALL GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND, &
         T2,Q2,D2,U10,V10,COAST,BLI,WETFRZ,VIS,GUST, &
         REFC,TCLD,LCLD,MCLD,HCLD,BASEZ,CEIL,SLP,DATE,FHR,GDIN, &
         GFLD,GFLD8)
        print *, 'after getgrib'

!  CALL THE DOWNSCALING CODE 
       CALL NDFDgrid(PSFC,ZSFC,T,HGHT,Q,UWND,VWND,PMID, &
           T2,Q2,D2,U10,V10,COAST,DOWNT,DOWNDEW,DOWNU,DOWNV, &
           DOWNQ,DOWNP,TOPO)
       FNAMEOUT='fort.  '

       LUB = 71
!      Open grib2 file for writing
       WRITE(FNAMEOUT(6:7),FMT='(I2)')LUB
         write(0,*) 'call baopen: ', lub
       CALL BAOPEN(LUB,FNAMEOUT,IRET)

       print *,'IRET from BAOPEN of 71: ',IRET

!      Write 2-m temperature to grib2

       DEC=-2.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNT)
       print *,'template number',GFLD%ipdtmpl
       print *,'idrtemplate number',GFLD%idrtmpl
       GFLD%ipdtnum=0
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(9)=FHR
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       print *,'template number after',GFLD%ipdtmpl

       GFLD%idrtnum=40 !JPEG2000
       GFLD%idrtmpl(2)=DEC
       GFLD%idrtmpl(5)=0
       GFLD%idrtmpl(6)=0
       GFLD%idrtmpl(7)=-1
       GFLD%idrtmpl(1)=0

       GFLD%ibmap=255


       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write 2-m dew point to grib2

       DEC=-2.0 
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNDEW)
       GFLD%ipdtmpl(1)=0
       GFLD%ipdtmpl(2)=6
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write 2-m Q to grib2
       DEC=4.0 
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNQ)
       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write 10-m u wind to grib2
       DEC=-2.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNU)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=002
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write 10-m v wind to grib2
       DEC=-2.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNV)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=003
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

 

       DO J=1,JM
       DO I=1,IM
          SPEED=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J))
          WGUST(I,J)=MAX(GUST(I,J),SPEED)
       ENDDO
       ENDDO

!      Write wind gust to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,WGUST)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=022
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


!      Write surface pressure to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DOWNP)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write surface pressure not downscaled to grib2
!       DEC=3.0
!       CALL FILL_FLD(GFLD,ITOT,IM,JM,PSFC)
!       GFLD%ipdtmpl(1)=3
!       GFLD%ipdtmpl(2)=0
!       GFLD%ipdtmpl(10)=103
!       GFLD%ipdtmpl(12)=0
!       GFLD%idrtmpl(2)=DEC
!
!       CALL set_scale(gfld,DEC)
!       CALL PUTGB2(71,GFLD,IRET)

!      Write topography to grib2
       DEC=-2.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,TOPO)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

       print *, 'past topo '
! GSM  for boundary layer computations, find the number of
!       levels within the lowest 180 mb

       DO J=1,JM
       DO I=1,IM
         PBLMARK(I,J)=1
         TOP=PSFC(I,J)-18000.
         DO L=35,1,-1
          IF(PMID(I,J,L).GT.TOP)THEN
           PBLMARK(I,J)=L
           GOTO 60
          ENDIF 
         ENDDO
 60     CONTINUE
       ENDDO
       ENDDO

!  compute RH
       DO J=1,JM
       DO I=1,IM
         DO L=1,MAXLEV
           QC=PQ0/PMID(I,J,L) &
               *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
           RH(I,J,L)=Q(I,J,L)/QC
         ENDDO
       ENDDO
       ENDDO

        DO J=1,JM
        DO I=1,IM
           SKYTMP=AMAX1(LCLD(I,J),MCLD(I,J))
           SKY(I,J)=AMAX1(SKYTMP,HCLD(I,J))
!      need to prevent values like 2E-43 which cause w3fi63 to fail
           IF (SKY(I,J) .LT. 1.) THEN
             SKY(I,J)=0.
           ENDIF
        ENDDO
        ENDDO

!      Write computed TCLD to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,SKY)
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write cloud base height to grib2
        GFLD%ibmap=255
        DO KK = 1, ITOT
          IF(MOD(KK,IM).EQ.0) THEN
            M=IM
            N=INT(KK/IM)
          ELSE
            M=MOD(KK,IM)
            N=INT(KK/IM) + 1
          ENDIF
          IF (BASEZ(M,N).EQ. 0.) THEN
            BASEZ(M,N)=20000
          ENDIF
        ENDDO
        DEC=-3.0

       CALL FILL_FLD(GFLD,ITOT,IM,JM,BASEZ)
       
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=2
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write ceiling height to grib2
        GFLD%ibmap=255
        DO KK = 1, ITOT
          IF(MOD(KK,IM).EQ.0) THEN
            M=IM
            N=INT(KK/IM)
          ELSE
            M=MOD(KK,IM)
            N=INT(KK/IM) + 1
          ENDIF
          IF (CEIL(M,N).EQ. 0.) THEN
            CEIL(M,N)=20000
          ENDIF
        ENDDO
!        DO J=1,JM
!        DO I=1,IM
!          IF (CEIL(I,J).LE. 0.) THEN
!            CEIL(I,J)=SPVAL
!          ENDIF
!        ENDDO
!        ENDDO
       DEC=-3.0

       CALL FILL_FLD(GFLD,ITOT,IM,JM,CEIL)
       
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=215
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!      Write SLP to grib2
       GFLD%ibmap=255
       DEC=-3.0

       CALL FILL_FLD(GFLD,ITOT,IM,JM,SLP)
       
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=198
       GFLD%ipdtmpl(10)=101
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!       Write model total cloud to grib2
!       Never a bitmap
       DEC=-3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,TCLD)
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=200
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!       Write model low cloud to grib2
       DEC=-3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,LCLD)
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=3
       GFLD%ipdtmpl(10)=214
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!       Write model mid cloud to grib2
       DEC=-3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,MCLD)
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=4
       GFLD%ipdtmpl(10)=224
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!       Write model high cloud to grib2
       DEC=-3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,HCLD)
       GFLD%ipdtmpl(1)=6
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=234
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


!       Write composite reflectivity to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,REFC)
       GFLD%ipdtmpl(1)=16
       GFLD%ipdtmpl(2)=196
       GFLD%ipdtmpl(10)=200
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


!========================================================================
! calcSnowLevel - takes sounding of the wetbulb temperature and finds the
!   lowest elevation (above ground) where wetbulb crosses from
!   above freezing to below freezing. When top wetbulb is above
!   freezing - puts in height of top level.   We now use this
!   field straight out of the RAP. 
!
!       Write wet bulb zero to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,WETFRZ)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=5
       GFLD%ipdtmpl(10)=245
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


!     Write visibility to grib2
       DEC=2.7
       CALL FILL_FLD(GFLD,ITOT,IM,JM,VIS)
       GFLD%ipdtmpl(1)=19
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


!==========================================================================
!  TransWind - the average winds in the layer between the surface
!              and the mixing height.
!--------------------------------------------------------------------------

      DO J=1,JM
      DO I=1,IM
         MGD=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J)) 
         UTOT=0.
         VTOT=0.
         COUNT=0.
         LMBL=PBLMARK(I,J)
         DO L=1,LMBL
           UTOT=UTOT+UWND(I,J,L)
           VTOT=VTOT+VWND(I,J,L)
           COUNT=COUNT+1
         ENDDO
         UTRANS=UTOT/COUNT
         VTRANS=VTOT/COUNT
         MGTRANS(I,J)=SQRT(UTRANS*UTRANS+VTRANS*VTRANS) 
         IF (MGTRANS(I,J).EQ.0.) THEN
           DIRTRANS(I,J)=0.
         ELSE
         DIRTRANS(I,J)=ATAN2(-UTRANS,-VTRANS) / 0.0174
         ENDIF
         IF(DIRTRANS(I,J).LT.0.) DIRTRANS(I,J)=DIRTRANS(I,J)+360.0
         IF(DIRTRANS(I,J).GT.360.) DIRTRANS(I,J)=DIRTRANS(I,J)-360.0    
      ENDDO
      ENDDO
!     Write PBL wind direction to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,DIRTRANS)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=0
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!     Write PBL wind speed to grib2
       DEC=-3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,MGTRANS)
       GFLD%ipdtmpl(1)=2
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!  compute BL RH

      DO J=1,JM
      DO I=1,IM
        BLH=PBLMARK(I,J)
        SUM=0.
        LEVS=0.
        DO L=1, BLH
         SUM=SUM+RH(I,J,L)
         LEVS=LEVS+1
        ENDDO
         BLR(I,J)=(SUM/LEVS)*100.
      ENDDO
      ENDDO
      CALL BOUND(BLR,0.,100.)

!     Write PBL RH to grib2
       DEC=3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,BLR)
       GFLD%ipdtmpl(1)=1
       GFLD%ipdtmpl(2)=1
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)

!========================================================================
!  MixHgt - the height to which a parcel above a 'fire' would rise
!    (in height) above ground level (in feet).
!
!  Calculated by assuming a parcel above a fire is VERY hot - but the fire
!  is very small - so that entrainment quickly makes it only a few degrees
!  warmer than the environment.  Ideally would want to consider moisture
!  and entrainment - but this is a very simple first guess.

      DO J=1,JM
      DO I=1,IM
       MIXHGT(I,J)=SPVAL
        firetheta=((P1000/PSFC(I,J))**CAPA)*(T2(I,J)+2.0)
        DO L=2,MAXLEV
          theta=((P1000/PMID(I,J,L))**CAPA)*(T(I,J,L))
          IF (theta.gt.firetheta) THEN
           MIXHGT(I,J)=HGHT(I,J,L)-ZSFC(I,J)
           GOTO 321
          ENDIF
        ENDDO
        MIXHGT(I,J)=HGHT(I,J,40)+300.
 321   CONTINUE
      ENDDO
      ENDDO
!     Write PBL height to grib2
       DEC=-3.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,MIXHGT)
       GFLD%ipdtmpl(1)=3
       GFLD%ipdtmpl(2)=6
       GFLD%ipdtmpl(10)=220
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


!--------------------------------------------------------------------------
! LAL - Based mainly on lifted index.  Adds more when RH at top of BL is
!       high, but RH at bottom of BL is low.
!--------------------------------------------------------------------------

      DO J=1,JM
      DO I=1,IM
       LAL(I,J)=SPVAL
        IF (BLI(I,J).LT.-5.) THEN
          LLAL=4.
        ELSE IF (BLI(I,J).LT.-3) THEN
          LLAL=3.
        ELSE IF (BLI(I,J).LT.0) THEN
          LLAL=2.
        ELSE
          LLAL=1.
        ENDIF

!   Add more when RH at top of BL is greater than
!      than 70% and RH at bottom of BL is less than 30

        RH1TOT=0.
        RH1SUM=0.
        RH2TOT=0.
        RH2SUM=0.
        DO L=1,MAXLEV
         IF(PSFC(I,J)-PMID(I,J,L).LT.3000.) THEN
           RH1TOT=RH1TOT+RH(I,J,L)
           RH1SUM=RH1SUM+1.
         ENDIF
         IF(PSFC(I,J)-PMID(I,J,L).LT.18000. .AND. &
           PSFC(I,J)-PMID(I,J,L).GT.12000.) THEN
           RH2TOT=RH2TOT+RH(I,J,L)
           RH2SUM=RH2SUM+1.
         ENDIF
        ENDDO
        RH1=RH1TOT/RH1SUM
        RH2=RH2TOT/RH2SUM 
        IF (RH2.GT.0.8 .AND. RH1.LT.0.2) THEN
         LAL(I,J)=LLAL+1.
        ELSE
         LAL(I,J)=LLAL
        ENDIF
        IF (LAL(I,J).LT.-18.) THEN
         LAL(I,J)=1.
        ENDIF
      ENDDO
      ENDDO
!     Write best lifted index to grib2
       DEC=2.0
       CALL FILL_FLD(GFLD,ITOT,IM,JM,LAL)
       GFLD%ipdtmpl(1)=7
       GFLD%ipdtmpl(2)=11
       GFLD%ipdtmpl(10)=1
       GFLD%ipdtmpl(12)=0
       GFLD%idrtmpl(2)=DEC

       CALL set_scale(gfld,DEC)
       CALL PUTGB2(71,GFLD,IRET)


       print *, 'completed main'
      deallocate (hght,t,q,uwnd,vwnd,psfc,zsfc,rh,pblmark,blr, &
         cwr,sky,tcld,wetfrz,dirtrans,t1,wx,thunder,vis,gust,d2, &
         lcld,mcld,hcld,topo,wgust,u10,v10,refc,t2,q2,bli)
      deallocate (topo_ndfd,rough,coast,downt,downdew,downu,downv, &
                 downq,downp,gridwx,mixhgt,mgtrans,lal,ceil,slp)

      STOP
      END PROGRAM smartinit

! -------------------------
        SUBROUTINE FILL_FLD(GFLD,NUMV,IM,JM,ARRAY2D)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        INTEGER :: NUMV, IM, JM, KK
        REAL :: ARRAY2D(IM,JM)
        
        DO KK = 1, NUMV
          IF(MOD(KK,IM).EQ.0) THEN
            M=IM
            N=INT(KK/IM)
          ELSE
            M=MOD(KK,IM)
            N=INT(KK/IM) + 1
          ENDIF
          GFLD%FLD(KK)=ARRAY2D(M,N) 
!        if (mod(KK,25000) .eq. 0) then    
!        write(0,*) 'M,N, ARRAY2D from gfld: ', M,N, GFLD%FLD(KK)
!        endif
        ENDDO
        END SUBROUTINE FILL_FLD

! -------------------------
        SUBROUTINE SET_SCALE(GFLD,DEC)
        USE GRIB_MOD
        USE pdstemplates
        TYPE (GRIBFIELD)  :: GFLD
        LOGICAL*1, allocatable:: locbmap(:)
        real :: DEC



        allocate(locbmap(size(GFLD%fld)))

        if (GFLD%ibmap .eq. 0 .or. GFLD%ibmap .eq. 254) then
        locbmap=GFLD%bmap
        write(0,*) 'used GFLD bmap'
        else
        write(0,*) 'hardwire locbmap to true'
        locbmap=.true.
        endif
        
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack



        call g2getbits(GFLD%ibmap,DEC,size(GFLD%fld),locbmap,GFLD%fld, &
                      GFLD%idrtmpl(1),GFLD%idrtmpl(2),GFLD%idrtmpl(3),GFLD%idrtmpl(4))

        write(0,*) 'gfld%idrtmpl(2:3) defined, inumbits: ', gfld%idrtmpl(2:4)

        END SUBROUTINE SET_SCALE

! --------------------------------

       subroutine g2getbits(ibm,scl,len,bmap,g,gmin,ibs,ids,nbits)
!$$$
!   This subroutine is changed from w3 lib getbit to compute the total number of
!   bits,
!   The argument list is modified to have ibm,scl,len,bmap,g,ibs,ids,nbits
!
!  Progrma log:
!    Jun Wang  Apr, 2010
!
! INPUT
!   ibm: integer, bitmap flag (grib2 table 6.0)
!   scl: real, significant digits,OR binary precision if < 0
!   len: integer, field and bitmap length
!   bmap: logical(len), bitmap (.true.: keep, bitmap (.true.: keep, .false.
!   skip)
!   fld: real(len), datafield
! OUTPUT
!   ibs: integer, binary scale factor
!   ids: integer, decimal scale factor
!   nbits: integer, number of bits to pack
!
      IMPLICIT NONE
!
      INTEGER,INTENT(IN)   :: IBM,LEN
      LOGICAL*1,INTENT(IN) :: BMAP(LEN)
      REAL,INTENT(IN)      :: scl,G(LEN)
      INTEGER,INTENT(OUT)  :: IBS,IDS,NBITS
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      INTEGER,PARAMETER    :: MXBIT=16
!
!  NATURAL LOGARITHM OF 2 AND 0.5 PLUS NOMINAL SAFE EPSILON
      real,PARAMETER :: ALOG2=0.69314718056,HPEPS=0.500001
!
!local vars
      INTEGER :: I,I1,icnt,ipo,le,irange
      REAL    :: GROUND,GMIN,GMAX,s,rmin,rmax,range,rr,rng2,po,rln2
!
      DATA       rln2/0.69314718/


! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  ROUND FIELD AND DETERMINE EXTREMES WHERE BITMAP IS ON
      IF(IBM == 255) THEN
        GMAX = G(1)
        GMIN = G(1)
        DO I=2,LEN
          GMAX = MAX(GMAX,G(I))
          GMIN = MIN(GMIN,G(I))
        ENDDO
      ELSE
        do i1=1,len
          if (bmap(i1)) exit
        enddo
!       I1 = 1
!       DO WHILE(I1 <= LEN .AND. .not. BMAP(I1))
!         I1=I1+1
!       ENDDO
        IF(I1 <= LEN) THEN
          GMAX = G(I1)
          GMIN = G(I1)
          DO I=I1+1,LEN
            IF(BMAP(I)) THEN
              GMAX = MAX(GMAX,G(I))
              GMIN = MIN(GMIN,G(I))
            ENDIF
          ENDDO
        ELSE
          GMAX = 0.
          GMIN = 0.
        ENDIF
      ENDIF
      write(0,*)' GMIN=',GMIN,' GMAX=',GMAX
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE NUMBER OF BITS
      icnt = 0
      ibs = 0
      ids = 0
      range = GMAX - GMIN
!      IF ( range .le. 0.00 ) THEN
      IF ( range .le. 1.e-30 ) THEN
        nbits = 8
        return
      END IF
!*
      IF ( scl .eq. 0.0 ) THEN
          nbits = 8
          RETURN
      ELSE IF ( scl  >  0.0 ) THEN
          ipo = INT (ALOG10 ( range ))
!jw: if range is smaller than computer precision, set nbits=8
          if(ipo<0.and.ipo+scl<-20) then
            print *,'for small range,ipo=',ipo,'ipo+scl=',ipo+scl,'scl=',scl
            nbits=8
            return
          endif

          IF ( range .lt. 1.00 ) ipo = ipo - 1
          po = float(ipo) - scl + 1.
          ids = - INT ( po )
          rr = range * 10. ** ( -po )
          nbits = INT ( ALOG ( rr ) / rln2 ) + 1
      ELSE
          ibs = -NINT ( -scl )
          rng2 = range * 2. ** (-ibs)
          nbits = INT ( ALOG ( rng2 ) / rln2 ) + 1
      END IF
!     write(0,*)'in g2getnits,ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',range
!*
      IF(nbits <= 0) THEN
        nbits = 0
        IF(ABS(GMIN) >= 1.) THEN
          ids = -int(alog10(abs(gmin)))
        ELSE IF (ABS(GMIN) < 1.0.AND.ABS(GMIN) > 0.0) THEN
          ids = -int(alog10(abs(gmin)))+1
        ELSE
          ids = 0
        ENDIF
      ENDIF
      nbits = min(nbits,MXBIT)
!     write(0,*)'in g2getnits ibs=',ibs,'ids=',ids,'nbits=',nbits
!
      IF ( scl > 0.0 ) THEN
        s=10.0 ** ids
        IF(IBM == 255) THEN
          GROUND = G(1)*s
          GMAX   = GROUND
          GMIN   = GROUND
          DO I=2,LEN
            GMAX = MAX(GMAX,G(I)*s)
            GMIN = MIN(GMIN,G(I)*s)
          ENDDO
        ELSE
          do i1=1,len
            if (bmap(i1)) exit
          enddo
 !        I1=1
 !        DO WHILE(I1.LE.LEN.AND..not.BMAP(I1))
 !          I1=I1+1
 !        ENDDO
          IF(I1 <= LEN) THEN
            GROUND = G(I1)*s
            GMAX   = GROUND
            GMIN   = GROUND
            DO I=I1+1,LEN
              IF(BMAP(I)) THEN
                GMAX = MAX(GMAX,G(I)*S)
                GMIN = MIN(GMIN,G(I)*S)
              ENDIF
            ENDDO
          ELSE
            GMAX = 0.
            GMIN = 0.
          ENDIF
        ENDIF

        range = GMAX-GMIN
        if(GMAX == GMIN) then
          ibs = 0
        else
          ibs = nint(alog(range/(2.**NBITS-0.5))/ALOG2+HPEPS)
        endif
!
      endif
        write(0,*) 'leave g2getbits with GMIN: ', GMIN
!        GFLD%idrtmpl(1)=GMIN
!     write(0,*)'in g2getnits,2ibs=',ibs,'ids=',ids,'nbits=',nbits,'range=',&
!                range, 'scl=',scl,'data=',maxval(g),minval(g)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END subroutine g2getbits

