C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    SMARTINITRAPAJN    CREATES NDFD FILES 
C   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 11-10-06
C
C ABSTRACT:   THIS CODE TAKES NATIVE RAP FILES AND GENERATES
C       1.5 KM OUTPUT OVER THE JUNEAU, AK REGION CONTAINING NDFD ELEMENTS
C
C PROGRAM HISTORY LOG:
C   11-10-06   G MANIKIN  - ADAPT FOR PUERTO RICO OUTPUT 
C
      PARAMETER(IM=655,JM=855,MAXLEV=50)
      PARAMETER(ITOT=IM*JM)
      PARAMETER (CAPA=0.28589641)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER DATE
      INTEGER P, PP, R, RR, S, SS, W, WW, X, XX
      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
      LOGICAL RITEHD,NEED12
      CHARACTER *50 WXSTRING(IM,JM)
C
      PARAMETER(MBUF=2000000,JF=1000000)
      PARAMETER (A2=17.2693882,A3=273.16,A4=35.86,
     &  PQ0=379.90516,P1000=100000.0)

      DIMENSION ID(25)
      INTEGER FHR,CYC,HOUR
      REAL, ALLOCATABLE :: HGHT(:,:,:), T(:,:,:),
     X   Q(:,:,:),UWND(:,:,:),VWND(:,:,:),
     X   PSFC(:,:),PMID(:,:,:),ZSFC(:,:),
     X   RH(:,:,:),PBLMARK(:,:),REFC(:,:),
     X   T2(:,:),Q2(:,:),BLI(:,:),U10(:,:),V10(:,:),
     X   CWR(:,:),TCLD(:,:),WETFRZ(:,:),BLR(:,:),
     X   LCLD(:,:),MCLD(:,:),HCLD(:,:), SKY(:,:),
     X   DIRTRANS(:,:),T1(:,:),D2(:,:),BASEZ(:,:),
     X   WX(:,:),THUNDER(:,:),VIS(:,:),GUST(:,:),
     X   TOPO(:,:),WGUST(:,:),MIXHGT(:,:),
     X   MGTRANS(:,:),LAL(:,:),SNOD(:,:)   
      REAL, ALLOCATABLE :: TOPO_NDFD(:,:),ROUGH(:,:),COAST(:,:), 
     X   DOWNT(:,:),DOWNDEW(:,:),DOWNU(:,:),DOWNP(:,:),
     X   DOWNV(:,:),DOWNQ(:,:),GRIDWX(:,:)

      allocate (hght(im,jm,maxlev),t(im,jm,maxlev),q(im,jm,maxlev),
     X    uwnd(im,jm,maxlev),vwnd(im,jm,maxlev),psfc(im,jm),
     X    zsfc(im,jm),rh(im,jm,maxlev),pblmark(im,jm),
     x    u10(im,jm),v10(im,jm),d2(im,jm),cwr(im,jm),gust(im,jm),
     X    blr(im,jm),t2(im,jm),q2(im,jm),bli(im,jm),basez(im,jm), 
     X    tcld(im,jm),wetfrz(im,jm),dirtrans(im,jm),t1(im,jm),
     X    lcld(im,jm),mcld(im,jm),hcld(im,jm),sky(im,jm),snod(im,jm),
     X    wx(im,jm),thunder(im,jm),vis(im,jm),refc(im,jm),
     X    topo(im,jm),wgust(im,jm),pmid(im,jm,maxlev))
      allocate (topo_ndfd(im,jm),rough(im,jm),coast(im,jm),downt(im,jm),
     X    downdew(im,jm),downu(im,jm),downv(im,jm),downq(im,jm),
     X    downp(im,jm),gridwx(im,jm),mixhgt(im,jm),
     X    mgtrans(im,jm),lal(im,jm))

      print *, 'start '
      READ (5,*) FHR
      READ (5,*) CYC
      print *, 'into main ', FHR
      print *, 'cyc ', CYC
      FHR3=FHR-3
      FHR6=FHR-6
      FHR12=FHR-12
      SPVAL=9.9E10

c  READ THE GRIB FILES FROM THE RAP.  WE NEED TO READ A
c   FULL COMPLEMENT OF DATA EVERY 3 HOURS.  FOR THE IN-BETWEEN
c   FCST HOURS, WE ONLY NEED TO KEEP TRACK OF DOWNSCALED TEMP
c   AND DEW POINT (FOR MIN/MAX PURPOSES), SO WE NEED ONLY A VERY
c   LIMITED AMOUNT OF DATA.   FOR THE ANALYSIS TIME, WE NEED A
c   SPECIAL CALL OF THE FULL DATA SET BUT WITHOUT PRECIP

        print *, 'before getgrib'
        CALL GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND,
     X    T2,Q2,D2,U10,V10,COAST,BLI,WETFRZ,VIS,GUST,SNOD,
     X    REFC,TCLD,LCLD,MCLD,HCLD,BASEZ,DATE,FHR)
        print *, 'after getgrib'

c  CALL THE DOWNSCALING CODE 
       CALL NDFDgrid(PSFC,ZSFC,T,HGHT,Q,UWND,VWND,PMID,
     X      T2,Q2,D2,U10,V10,COAST,DOWNT,DOWNDEW,DOWNU,DOWNV,
     X      DOWNQ,DOWNP,TOPO)

       RITEHD = .TRUE.
       ID(1:25) = 0
       ID(8)=11
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNT,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=17
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNDEW,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=51
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,DOWNQ,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=33
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNU,DATE,FHR,DEC)
 
       ID(1:25) = 0
       ID(8)=34
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,DOWNV,DATE,FHR,DEC)

       DO J=1,JM
       DO I=1,IM
          SPEED=SQRT(DOWNU(I,J)*DOWNU(I,J)+DOWNV(I,J)*DOWNV(I,J))
          WGUST(I,J)=MAX(GUST(I,J),SPEED)
       ENDDO
       ENDDO

       ID(1:25) = 0
       ID(8)=180
       ID(9)=1
       DEC=3.0 
       CALL GRIBIT(ID,RITEHD,WGUST,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=66
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,SNOD,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=1
       ID(9)=1
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,DOWNP,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=1
       ID(9)=105
       DEC=3.0
       CALL GRIBIT(ID,RITEHD,PSFC,DATE,FHR,DEC)

       ID(1:25) = 0
       ID(8)=7
       ID(9)=1
       DEC=-2.0
       CALL GRIBIT(ID,RITEHD,TOPO,DATE,FHR,DEC)
       print *, 'past topo '
C GSM  for boundary layer computations, find the number of
C       levels within the lowest 180 mb

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

c  compute RH
       DO J=1,JM
       DO I=1,IM
         DO L=1,MAXLEV
           QC=PQ0/PMID(I,J,L)
     1          *EXP(A2*(T(I,J,L)-A3)/(T(I,J,L)-A4))
           RH(I,J,L)=Q(I,J,L)/QC
         ENDDO
       ENDDO
       ENDDO

        DO J=1,JM
        DO I=1,IM
           SKYTMP=AMAX1(LCLD(I,J),MCLD(I,J))
           SKY(I,J)=AMAX1(SKYTMP,HCLD(I,J))
           IF (SKY(I,J) .LT. 1.) THEN 
             SKY(I,J)=0.
           ENDIF
        ENDDO
        ENDDO

        ID(1:25) = 0
        ID(8)=71
        ID(9)=1
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,SKY,DATE,FHR,DEC)

        ID(1:25) = 0
        ID(8)=7
        ID(9)=2
        DEC=-3.0
        DO J=1,JM
        DO I=1,IM
          IF (BASEZ(I,J).LE. 0.) THEN
            BASEZ(I,J)=SPVAL
          ENDIF
        ENDDO
        ENDDO
        CALL GRIBIT(ID,RITEHD,BASEZ,DATE,FHR,DEC)

        ID(1:25) = 0
        ID(8)=71
        ID(9)=200
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,TCLD,DATE,FHR,DEC)

        ID(1:25) = 0
        ID(8)=73
        ID(9)=214
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,LCLD,DATE,FHR,DEC)

        ID(1:25) = 0
        ID(8)=74
        ID(9)=224
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,MCLD,DATE,FHR,DEC)

        ID(1:25) = 0
        ID(8)=75
        ID(9)=234
        DEC=-3.0
        CALL GRIBIT(ID,RITEHD,HCLD,DATE,FHR,DEC)

        ID(1:25) = 0
        ID(2)=129
        ID(8)=212
        ID(9)=200
        DEC=3.0
        CALL GRIBIT(ID,RITEHD,REFC,DATE,FHR,DEC)

c========================================================================
c calcSnowLevel - takes sounding of the wetbulb temperature and finds the
c   lowest elevation (above ground) where wetbulb crosses from
c   above freezing to below freezing. When top wetbulb is above
c   freezing - puts in height of top level.   We now use this
c   field straight out of the NAM. 
c

      ID(1:25) = 0
      ID(8)=7
      ID(9)=245
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,WETFRZ,DATE,FHR,DEC)

C VISIBILITY

      ID(1:25) = 0
      ID(8)=20
      ID(9)=1
      DEC=2.7
      CALL GRIBIT(ID,RITEHD,VIS,DATE,FHR,DEC)

c==========================================================================
c  TransWind - the average winds in the layer between the surface
c              and the mixing height.
c--------------------------------------------------------------------------

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

      ID(1:25) = 0
      ID(8)=31
      ID(9)=220
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,DIRTRANS,DATE,FHR,DEC)

      ID(1:25) = 0
      ID(8)=32
      ID(9)=220
      DEC=-3.0
      CALL GRIBIT(ID,RITEHD,MGTRANS,DATE,FHR,DEC)

c  compute BL RH

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

      ID(1:25) = 0
      ID(8)=52
      ID(9)=220
      DEC=3.0
      CALL GRIBIT(ID,RITEHD,BLR,DATE,FHR,DEC)

c========================================================================
c  MixHgt - the height to which a parcel above a 'fire' would rise
c    (in height) above ground level (in feet).
c
c  Calculated by assuming a parcel above a fire is VERY hot - but the fire
c  is very small - so that entrainment quickly makes it only a few degrees
c  warmer than the environment.  Ideally would want to consider moisture
c  and entrainment - but this is a very simple first guess.

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

      ID(1:25) = 0
      ID(8)=8
      ID(9)=220
      DEC=-3.0
      CALL GRIBIT(ID,RITEHD,MIXHGT,DATE,FHR,DEC)

c--------------------------------------------------------------------------
c LAL - Based mainly on lifted index.  Adds more when RH at top of BL is
c       high, but RH at bottom of BL is low.
c--------------------------------------------------------------------------

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

c   Add more when RH at top of BL is greater than
c      than 70% and RH at bottom of BL is less than 30

        RH1TOT=0.
        RH1SUM=0.
        RH2TOT=0.
        RH2SUM=0.
        DO L=1,MAXLEV
         IF(PSFC(I,J)-PMID(I,J,L).LT.3000.) THEN
           RH1TOT=RH1TOT+RH(I,J,L)
           RH1SUM=RH1SUM+1.
         ENDIF
         IF(PSFC(I,J)-PMID(I,J,L).LT.18000. .AND.
     x      PSFC(I,J)-PMID(I,J,L).GT.12000.) THEN
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

      ID(1:25) = 0
      ID(8)=132
      ID(9)=1
      DEC=2.0
      CALL GRIBIT(ID,RITEHD,LAL,DATE,FHR,DEC)

       print *, 'completed main'
      deallocate (hght,t,q,uwnd,vwnd,psfc,zsfc,rh,pblmark,blr,
     X    cwr,sky,tcld,wetfrz,dirtrans,t1,wx,thunder,vis,gust,d2,
     X    lcld,mcld,hcld,topo,wgust,u10,v10,refc,t2,q2,bli)
      deallocate (topo_ndfd,rough,coast,downt,downdew,downu,downv,
     X    downq,downp,gridwx,mixhgt,mgtrans,lal,snod)

      STOP
      END

