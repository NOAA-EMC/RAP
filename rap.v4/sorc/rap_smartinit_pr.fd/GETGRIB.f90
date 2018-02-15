      SUBROUTINE GETGRIB(PSFC,ZSFC,PMID,HGHT,T,Q,UWND,VWND, &
      T2,Q2,D2,U10,V10,COAST,BLI,WETFRZ,VIS,GUST,  &
      REFC,TCLD,LCLD,MCLD,HCLD,BASEZ,CEIL,MSLP,DATE, &
      IFHR,GDIN,GFLD,GFLD8)

       use grddef
       use rdgrib
       USE GRIB_MOD
       USE pdstemplates

!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GETGRIB    CREATES NDFD FILES 
!   PRGRMMR: MANIKIN           ORG: W/NP22     DATE: 11-09-30
!
! ABSTRACT:
!   .
!
! PROGRAM HISTORY LOG:
!   11-09-30  G MANIKIN  - ADAPT CODE TO RAPID REFRESH 
!
! USAGE:    CALL SMARTINIT 
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!     NONE
!
!   OUTPUT FILES:
!     NONE
      TYPE (GINFO) :: GDIN
      PARAMETER(ILIM=177,JLIM=129,MAXLEV=50)
!      PARAMETER(ITOT=ILIM*JLIM)
!      REAL, ALLOCATABLE :: GRID(:)
      DIMENSION DIFF(5)
      DIMENSION INCDAT(8),JNCDAT(8)
      INTEGER JPDS(200),JGDS(200),KPDS(200),KGDS(200)
      INTEGER LEVS(MAXLEV),IVAR(5),YEAR,MON,DAY,IHR,DATE,IFHR
      INTEGER NUMVAL, IMAX, JMAX, KMAX, NUMLEV, ITOT, KRET, &
                 ISSREF,JDISC,JPDTN
      INTEGER,DIMENSION(:) :: JIDS(200),JPDT(200),JGDT(200)
!      LOGICAL*1 MASK(ITOT), MASK2(ITOT)
      LOGICAL*1 VALIDPT(ILIM,JLIM)
!
      PARAMETER(MBUF=2000000,JF=1000000)
      CHARACTER CBUF(MBUF)
      CHARACTER CBUF2(MBUF)
      CHARACTER*11 ENVVAR
      CHARACTER*80 FNAME
      CHARACTER*4 DUM1
      LOGICAL*1 LB(JF)
      REAL F(JF)
      PARAMETER(MSK1=32000,MSK2=4000)
      INTEGER JENS(200),KENS(200)
      INTEGER ISNOW(ILIM,JLIM),IRAIN(ILIM,JLIM),IIP(ILIM,JLIM), &
            IZR(ILIM,JLIM)
      REAL ZSFC(ILIM,JLIM),T(ILIM,JLIM,MAXLEV), &
       PSFC(ILIM,JLIM), &
       Q(ILIM,JLIM,MAXLEV),PMID(ILIM,JLIM,MAXLEV),WETFRZ(ILIM,JLIM), &
       UWND(ILIM,JLIM,MAXLEV),VWND(ILIM,JLIM,MAXLEV),VIS(ILIM,JLIM), &
       T2(ILIM,JLIM),Q2(ILIM,JLIM),D2(ILIM,JLIM),BASEZ(ILIM,JLIM), &
       U10(ILIM,JLIM),V10(ILIM,JLIM),TCLD(ILIM,JLIM), &
       HGHT(ILIM,JLIM,MAXLEV),BLI(ILIM,JLIM),P12M(ILIM,JLIM), &
       T950(ILIM,JLIM),T850(ILIM,JLIM),T700(ILIM,JLIM), &
       T500(ILIM,JLIM),RH850(ILIM,JLIM),RH700(ILIM,JLIM),  &
       COAST(ILIM,JLIM),REFC(ILIM,JLIM),GUST(ILIM,JLIM), &
       MSLP(ILIM,JLIM),CEIL(ILIM,JLIM),VTYPE(ILIM,JLIM) 
      REAL LCLD(ILIM,JLIM),MCLD(ILIM,JLIM), &
       HCLD(ILIM,JLIM), &
       DEPSN(ILIM,JLIM)
      TYPE(GRIBFIELD)::GFLD,GFLD_S,GFLD8,GFLD8_S
!
!      allocate(grid(itot))
!      NUMLEV=MAXLEV

!  ASSIGN UNIT NUMBERS 
!
!  FOR 12-hr TIMES, WE NEED 3 AND 6-HR BUCKETS AND MAX/MIN TEMP
!   DATA FOR THE PREVIOUS 11 HOURS
       LUGB=11
       LUGI=12

      OPEN(49,file='DATE',form='formatted')
      READ(49,200) DUM1,DATE
      CLOSE(49)
 200  FORMAT(A4,2X,I10)
      year=int(date/1000000)
      mon=int(int(mod(date,1000000)/100)/100)
      day=int(mod(date,10000)/100)
      ihr=mod(date,100)
      print *, 'date ', DATE,YEAR,MON,DAY,IHR 

! GSM  READ RAP FILE 
!  READ INDEX FILE TO GET GRID SPECS
!
      IRGI = 1
      IRGS = 1
      KMAX = 0
!      CALL BAOPEN(LUGB,'fort.11',IRETGB)
!      CALL BAOPEN(LUGI,'fort.12',IRETGI)
      write(0,*) 'call RDHDRS'
      CALL RDHDRS_g2(LUGB,LUGI,IGDNUM,GDIN,NUMVAL)
      GDIN%KMAX=MAXLEV
      IMAX=GDIN%IMAX;JMAX=GDIN%JMAX;KMAX=GDIN%KMAX
      NUMLEV=GDIN%KMAX
      ITOT=IMAX*JMAX
      print *,'imax,jmax,kmax,numlev,igdnum,numval'
      print *,imax,jmax,kmax,numlev,igdnum,numval
!      CALL GETGI(LUGI,KSKIP,MBUF,CBUF,NLEN,NNUM,IRGI)
!      write(6,*)' IRET FROM GETGI ',IRGI
!      IF(IRGI .NE. 0) THEN
!        WRITE(6,*)' PROBLEMS READING GRIB INDEX FILE SO ABORT'
!        ISTAT = IRGI
!        RETURN
!      ENDIF
!      REWIND LUGI

      ALLOCATE (GRID(ITOT),MASK(ITOT),STAT=kret)
      print *,'GRID ALLOCATED',ITOT,' kret ',kret
  
      JIDS=-9999
      JPDTN=-1
      JPDT=-9999
      JGDTN=-1
      JGDT=-9999
!     JPDTN needs to be 0 to match specific records
      JPDTN = 0
!     JDISC matches discipline table
      JDISC  = 0
      ISSREF=0

!     Get vertical profile of pressure
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 003
        JPDT(2) = 000
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,PMID(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K
        print*,'minval ',minval(PMID(:,:,LL))
        print*,'maxval',maxval(PMID(:,:,LL))

      ENDDO
        
!     Get vertical profile of height
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 003
        JPDT(2) = 005
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,HGHT(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

      ENDDO
  
!     Get vertical profile of temperature
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 000
        JPDT(2) = 000
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,T(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

      ENDDO

!     Get vertical profile of q
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 001
        JPDT(2) = 000
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,Q(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

      ENDDO


!     Get vertical profile of u
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 002
        JPDT(2) = 002
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,UWND(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

      ENDDO

!     Get vertical profile of v
      J=0
      DO LL=1,MAXLEV

        JPDT(1) = 002
        JPDT(2) = 003
        JPDT(10) = 105
        JPDT(12) = LL
           
        CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VWND(:,:,LL),GFLD, &
                     ISSREF,IRET,ISTAT)
        J=K  

      ENDDO

!     Get surface height
      JPDT=-9999
!      JPDT(1) = 0
      JPDT(2) = 005
      JPDT(10) = 1
!      JPDT(12) = 2
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,ZSFC,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval zsfc',minval(zsfc)
       print*,'maxval zsfc',maxval(zsfc)
       WHERE (ZSFC < 0.0) ZSFC=0.0

!     Get surface pressure
      JPDT(1) = 003
      JPDT(2) = 000
      JPDT(10) = 1
!      JPDT(12) = 2
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,PSFC,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval psfc',minval(psfc)
       print*,'maxval psfc',maxval(psfc)
 
!     Get lowest wet bulb zero
      JPDT(1) = -9999
      JPDT(2) = 005
      JPDT(10) = 245
!      JPDT(12) = 2
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,WETFRZ,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval wet0',minval(wetfrz)
       print*,'maxval wet0',maxval(wetfrz)

!     Get visibility
      JPDT(1) = 19
      JPDT(2) = 000
      JPDT(10) = 1
!      JPDT(12) = 2
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,VIS,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval vis',minval(vis)
       print*,'maxval vis',maxval(vis)

!     Get 2-m Q
      JPDT(1) = 1
      JPDT(2) = 000
      JPDT(10) = 103
      JPDT(12) = 2
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,Q2,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval q2',minval(q2)
       print*,'maxval q2',maxval(q2)

!     Get 2-m dew point
      JPDT(1) = 0
      JPDT(2) = 006
      JPDT(10) = 103
      JPDT(12) = 2
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,D2,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval d2',minval(d2)
       print*,'maxval d2',maxval(d2)

!     Get 10-m u wind
      JPDT(1) = 2
      JPDT(2) = 002
      JPDT(10) = 103
      JPDT(12) = 10
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,U10,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval u10',minval(u10)
       print*,'maxval u10',maxval(u10)

!     Get 10-m v wind
      JPDT(1) = 2
      JPDT(2) = 003
      JPDT(10) = 103
      JPDT(12) = 10
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,V10,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval v10',minval(v10)
       print*,'maxval v10',maxval(v10)

!     Get best lifted index
      JPDT(1) = 7
      JPDT(2) = 193
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,BLI,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval bli',minval(bli)
       print*,'maxval bli',maxval(bli)

!     Get surface wind gust
      JPDT(1) = 002
      JPDT(2) = 022
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,GUST,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval gust',minval(gust)
       print*,'maxval gust',maxval(gust)


!     Get composite reflectivity
      JPDT(1) = 16
      JPDT(2) = 196
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,REFC,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval refc',minval(refc)
       print*,'maxval refc',maxval(refc)

!     Get sea level pressure
      JPDT(1) = 3
      JPDT(2) = 198
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,MSLP,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval mslp',minval(mslp)
       print*,'maxval mslp',maxval(mslp)
       print*,'gfld ibmap: ',gfld%ibmap

!     Get total cloud fraction
      JPDT(1) = 6
      JPDT(2) = 1
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,TCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval tcld',minval(tcld)
       print*,'maxval tcld',maxval(tcld)

!     Get low cloud fraction
      JPDT(1) = 6
      JPDT(2) = 3
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,LCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval lcld',minval(lcld)
       print*,'maxval lcld',maxval(lcld)

!     Get mid cloud fraction
      JPDT(1) = 6
      JPDT(2) = 4
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,MCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval mcld',minval(mcld)
       print*,'maxval mcld',maxval(mcld)

!     Get high cloud fraction
      JPDT(1) = 6
      JPDT(2) = 5
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,HCLD,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval hcld',minval(hcld)
       print*,'maxval hcld',maxval(hcld)


!     Get sea level pressure
      JPDT(1) = 3
      JPDT(2) = 198
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,MSLP,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval mslp',minval(mslp)
       print*,'maxval mslp',maxval(mslp)
       print*,'gfld ibmap: ',gfld%ibmap

!     Get land mask (actually vegetation type; used for changing land to
!     water and vice versa)
      JDISC  = 2
      JPDT(1) = 0
!     would be 0 if we wanted land/sea classification
!     JPDT(2) = 0
      JPDT(2) = 198
      JPDT(10) = -9999
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,COAST,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval coast',minval(coast)
       print*,'maxval coast',maxval(coast)
!     Get ceiling
      JDISC  = 0
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 215
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,CEIL,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval ceil',minval(ceil)
       print*,'maxval ceil',maxval(ceil)
       print*,'gfld ibmap: ',gfld%ibmap
!     Get 2-m temperature 
!     JPDT(1) is table 4.1
!     JPDT(2) is parameter number
!     JPDT(10) is level type in table 4.5
!     JPDT(12) is level (e.g., 2 for 2-m or 30 for hybrid level 30)

      JPDT(1) = 0
      JPDT(2) = 000
      JPDT(10) = 103
      JPDT(12) = 2

      JDISC=0
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,T2,GFLD, &
                     ISSREF,IRET,ISTAT)

      print*,'minval ',minval(t2)
      print*,'maxval ',maxval(t2)
      print *,'2-m template number',GFLD%ipdtmpl
      print *,'2-m idrtemplate number',GFLD%idrtmpl
!     Get cloud base height
      JPDT(1) = 3
      JPDT(2) = 5
      JPDT(10) = 2
      JPDT(12) = -9999
      J=0
      CALL SETVAR_g2(LUGB,LUGI,NUMVAL,J,JDISC,JIDS,JPDTN,JPDT,JGDTN, &
                     JGDT,KF,K,KPDS,KGDS,MASK,GRID,BASEZ,GFLD, &
                     ISSREF,IRET,ISTAT)
       print*,'minval basez',minval(basez)
       print*,'maxval basez',maxval(basez)
       print*,'gfld ibmap: ',gfld%ibmap
       print*,'GFLD%fld',GFLD%fld(1)
       print*,'GFLD%fld',GFLD%fld(1000)
     print *, 'done with unpacking'

     RETURN
     END
