#! /bin/sh
#
# Metafile Script : rap_meta_loop.sh
#
# Log :
# J. Carr/PMB       11/04   Introduced into production via HPC 
# M. Klein/WPC      03/13   Create 24-hour loops of each parameter.
#
set -xa

export PS4='RAP_META_LOOP:$SECONDS + '

#PDY=20041129
#cycle=23
#cyc=23

# SET UP A WORKING DIRECTORY.
workdir="${DATA}/loop_working"
mkdir ${workdir}
cd ${workdir}

# COPY OVER RELEVANT TABLES.
cp $FIXgempak/datatype.tbl datatype.tbl
cp ${DATA}/* .

metaname="rap_loop.meta"
device="nc | ${metaname}"

PDY2=`echo $PDY | cut -c3-`
MDL=RAP
mdl=rap
COMIN="$COMAWP/${mdl}.${PDY}/gempak"

if [ "$envir" = "para" ] ; then
   export m_title="RAPP"
else
   export m_title="RAP"
fi

gdattim=f000

# DETERMINE CURRENT CYCLE BEING PROCESSED
latest_rap_datetime=`ls -1tr $COMAWP/rap.${PDY}/gempak/rap_*f000 | awk -F"/" '{print $NF}' | tail -1 | cut -c5-14`
#XXW latest_rap_datetime=`ls -1tr $COMIN/rap_*f000 | awk -F"/" '{print $NF}' | tail -1 | cut -c5-14`
fullddate=`echo ${latest_rap_datetime} | cut -c1-8`
ddate=`echo ${latest_rap_datetime} | cut -c3-8`
cycle=`echo ${latest_rap_datetime} | cut -c9-10`

# SET UP WHILE LOOP TO GO FROM 23 HOURS AGO TO CURRENT CYCLE (I.E. 24-HOUR LOOP)
cnt=23

while [ $cnt -ge 0 ]
do

    export pgm=gdplot2_nc;. prep_step; startmsg

    pfullddate=`datetime -s ${ddate}/${cycle}00 $cnt "%Y%m%d"`
    pddate=`datetime -s ${ddate}/${cycle}00 $cnt "%y%m%d"`
    pcycle=`datetime -s ${ddate}/${cycle}00 $cnt "%H"`

    COMIN="$COMAWP/${mdl}.${pfullddate}/gempak"
    #COMIN="$COMIN"


#$GEMEXE/gdplot2_nc << EOF
gdplot2_nc << EOF
GDFILE  = F-RAP | ${pddate}/${pcycle}00
GDATTIM = ${gdattim}
DEVICE  = ${device}
PANEL   = 0
TEXT    = 1/21//hw
CONTUR  = 2
GAREA   = us
PROJ    =
MAP     = 1/1/1/yes
CLEAR   = yes
CLRBAR  = 1
latlon  = 0
filter  = yes

glevel  = 30:0!30:0
gvcord  = pdly!pdly
skip    = 0
scale   = 0
gdpfun  = sm5s(relh)!sm5s(relh)!kntv(wnd)
type    = c/f !c   !b
cint    = 80;90!10;20;30;40;50;60;70
line    = 32//2!3//2
fint    = 70;80;90
fline   = 0;23;22;3
hilo    = 0
hlsym   = 0
clrbar  = 1!0
wind    = bk0!bk0!bk9/0.7/2/112
refvec  =
title   = 1/-2/~ ? ${MDL} BL RH & WIND|~BL RH!0
list
run

GLEVEL  = 850
GVCORD  = pres
SKIP    = 0/2;2
SCALE   = 0         !0         !0      !-1
GDPFUN  = sm5s(tmpc)!sm5s(tmpc)!tmpc   !sm5s(hght)!kntv(wnd)
TYPE    = c/f       !c         !c      !c         !b
CINT    = 3/-99/0   !3/3/18    !3/21/99!3
LINE    = 27/1/2 !2/1/2    !16/1/2     !20/1/2/1
FINT    = -24;-18;-12;-6;0 !
FLINE   = 29;30;28;24;25;0 !
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    =        !         !                !     !Bk9/0.7/2/212
TITLE   = 1/-2/~ ? ${MDL} @ HGT, TEMP AND WIND (KTS)|~@ HGHT,TMP,WIND!0
list
run

glevel  = 850!850!850
gvcord  = pres!pres!pres
skip    = 0
scale   = 0!0!-1
gdpfun  = sm5s(dwpc)!sm5s(dwpc)!sm5s(hght)!kntv(wnd)
type    = c/f !c   !c         !b
cint    = -4;-2;0;2;4!2/6/28!3
line    = 3//1!32//1!6//3
fint    = 4;8;12;16;20
fline   = 0;23;22;30;14;2
hilo    = 0!0!6/H#;L#
hlsym   = 0!0!1;1//22;22/2;2/hw
clrbar  = 1/V/LL!0
wind    = bk0!bk0!bk0!bk9/0.7/2/212
title   = 1/-2/~ ? ${MDL} @ DEW POINT, WIND, AND HGHT|~@ DEW POINT!0
list
run

glevel   = 0
gvcord   = none               !none
skip     = 0
scale    = 0
gdpfun   = sm5s(quo(pwtr;25.4)!sm5s(quo(pwtr;25.4)!kntv(wnd@850%PRES)
type     = c                  !c/f                !b
cint     = 0.25/0.25/0.5      !0.25/0.75/6.0
line     = 22///2             !32//2/2
fint     =                    !0.5;1.0;1.5;2.0
fline    =                    !0;23;22;21;2
hilo     = 0
HLSYM    = 0
clrbar   = 0                  !1/V/LL
wind     = bk0                !bk0                !bk9/0.8/2/112
refvec   =
title    = 1/-2/~ ? ${MDL} 850 MB WIND AND PRECIP WATER|~850 MB WIND & PW!0
run

glevel  = 850
gvcord  = pres
skip    = 0
scale   = 2                  !-1/2      !-1/2
gdpfun  = mag(smul(mixr;wnd))!sm5s(hght)!smul(mixr;wnd)
type    = c/f                !c         !a
cint    = 3                  !3
line    = 3/1/2              !1/1/2
fint    = 6;12;18;24;30;36
fline   = 0;23;22;21;14;15;2
hilo    = 0                  !1/H#;L#/160-400;60-150/5/50;50!0
hlsym   = 0                  !1/22/2/hw
clrbar  = 1/V/LL             !0
wind    = bk0                !bk0!am16/0.9/2/211/0.5
refvec  = 10
title   = 1/-2/~ ? ${MDL} @ MOIST TRNSPT, HGHT|~@ H2O TRANSPORT!0
list
run

glevel  = 850
gvcord  = pres
skip    = 0
scale   = 4                             !0
gdpfun  = adv(thte(pres;tmpc;dwpc),wind)!sm5s(thte(pres;tmpc;dwpc)//te!te!te     !kntv(wnd)
type    = c/f                           !c                       !c        !c    !b
cint    = 2                             !4/4/304                 !4/308/324!4/328
line    = 32/1/1                        !23/10/2                 !22/10/2  !21/1/1
fint    = -14;-10;-6;-2;2;6;10;14
fline   = 7;29;30;24;0;14;15;18;5
hilo    = 0
hlsym   = 0
clrbar  = 1/V/LL!0
wind    = bk0                           !bk0                     !bk0      !bk0   !bk9/0.7/2/112
title   = 1/-2/~ ? ${MDL} @ THTAE ADV, THTAE & WIND|~@ THTAE ADVECTION!0
refvec  = 
list
r

GLEVEL  = 850
GVCORD  = pres
SKIP    = 0
SCALE   = 0              !0              !-1
GDPFUN  = sm5s(relh)     !sm5s(relh)     !sm5s(hght)
TYPE    = c/f            ! c
CINT    = 10;20;80;90    !30;40;50;60;70 !3
LINE    = 32//2/1        !23//2/2        !20/1/2/1
FINT    = 10;30;70;90
FLINE   = 18;8;0;22;23
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    = 0
TITLE   = 1/-2/~ ? ${MDL} @ HGT, RH|~@ HGHT AND RH!0
list
run

GLEVEL  = 700
GVCORD  = pres
SKIP    = 0
SCALE   = 0      !0        !0               !-1
GDPFUN  = sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)  !sm5s(hght)    !kntv(wnd)
TYPE    = c/f    !c        !c               !c             !b
CINT    = 3/-99/0!3/3/18   !3/21/99         !3
LINE    = 27/1/2 !2/1/2    !16/1/2          !20/1/2/1
FINT    = -24;-18;-12;-6;0 !
FLINE   = 29;30;28;24;25;0 !
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    =        !         !                !        !Bk9/0.8/2/112
TITLE   = 1/-2/~ ? ${MDL} @ HGT, TEMP AND WIND (KTS)|~@ HGHT,TMP,WIND!0
list
run

GLEVEL  = 700
GVCORD  = PRES
SKIP    = 0              !0              !0
SCALE   = 0              !0              !-1
GDPFUN  = sm5s(relh)     !sm5s(relh)     !sm5s(hght)
TYPE    = c/f            ! c
CINT    = 10;20;80;90    !30;40;50;60;70 !3
LINE    = 32//2/1        !23//2/2        !20/1/2/1
FINT    = 10;30;70;90
FLINE   = 18;8;0;22;23
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    =
TITLE   = 1/-2/~ ? ${MDL} @ HGT AND RH|~@ HGHT AND RH!0
list
run

GLEVEL  = 700        !700 !700  !850  !850  !30:0 !30:0
GVCORD  = PRES       !PRES!PRES !PRES !PRES !pdly !pdly
SKIP    = 0
SCALE   = 0
GDPFUN  = sm5s(relh)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)!sm5s(tmpc)
TYPE    = c/f        !c
CINT    = 70;90;95   !2;-2!200;0!2;-2 !200;0!2;-2 !-100;0;100
LINE    = 32//1/0    !6/3/2!6/1/2 !2/3/2!2/1/2 !20/3/2!20/1/2
FINT    = 70;90;95
FLINE   = 0;24;23;22
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    =
TITLE   = 1/-2/~ ? ${MDL} @ RH, TEMP(BL yel,850 red,700 cyan)|~@ RH,R/S TEMP!0
list
run

restore $USHgempak/restore/500mb_hght_absv.2.nts
CLRBAR  = 1
TEXT    = 1/21//hw
TITLE   = 1/-2/~ ? ${MDL} @ HGT AND VORTICITY|~@ HGHT AND VORTICITY!0
MAP     = 1
list
run

!GLEVEL  = 300
!TITLE   = 1/-2/~ ? ${MDL} @ HGT AND VORTICITY|~@ HGHT AND VORTICITY!0
!MAP     = 1
!list
!run

GLEVEL  = 300
GVCORD  = PRES
SKIP    = 0/3;3
SCALE   = 0                    !-1
GDPFUN  = knts((mag(wnd)       !sm5s(hght)!kntv(wnd)
TYPE    = c/f                  !c         !b
CINT    = 10/10//              !12
LINE    = 27/5/2/1             !20/1/2/1
FINT    = 70;90;110;130;150
FLINE   = 0;25;24;29;7;15
HILO    =
HLSYM   =
CLRBAR  = 1
WIND    = bk0                  !bk0        !Bk9/0.8/2/112
TITLE   = 1/-2/~ ? ${MDL} @ HGT, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
list
run

GLEVEL  = 250
GDPFUN  = knts((mag(wnd)       !sm5s(hght)!kntv(wnd)
TITLE   = 1/-2/~ ? ${MDL} @ HGT, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
list
run

GLEVEL  = 200
GDPFUN  = knts((mag(wnd)       !sm5s(hght)!kntv(wnd)
TITLE   = 1/-2/~ ? ${MDL} @ HGT, ISOTACHS AND WIND (KTS)|~@ HGHT AND WIND!0
list
run

glevel   = 300
gvcord   = pres
SKIP     = 0/3;3
scale    = 0                       !5/0               !5/0    !-1        !5/0
gdpfun   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)!age(hght)
type     = c/f                     !c                 !c      !c         !a
cint     = 70;90;110;130;150;170   !-11;-9;-7;-5;-3;-1!2/2/100!12
line     = 32/1                    !20/-2/2           !3/1/2  !1//2
fint     = 70;90;110;130;150;170;190!
fline    = 0;24;25;30;28;14;2;1    !
hilo     = 0                       !0                 !0      !1/H#;L#/3
hlsym    = 0                       !0                 !0      !1.3//22/2/hw
clrbar   = 1/V/LL                  !0
wind     = bk0                     !bk0               !bk0    !bk0       !am16/0.4//211/0.4
refvec   = 10
title    = 1/-1/~ ? ${MDL} @ DIV(GREEN),ISOTACHS & AGEO WND|~@ AGEO & DIVERG!0
filter   = no
list
run

GLEVEL   = 250
GDPFUN   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)!age(hght)
list
run

!glevel   = 200
!gdpfun   = sm5s(mag(kntv(wnd))//jet!sm5s(div(wnd)//dvg!dvg    !sm5s(hght)!age(hght)
!list
!run

GLEVEL   = 1000!30:0
GVCORD   = pres!pdly
PANEL    = 0
SKIP     = 0/1;2
SCALE    = 7!0!0
GDPFUN   = sm5s(sdiv(mixr;wnd)!sm5s(dwpf)!sm5s(dwpf)!sm5s(dwpf)!kntv(wnd)
TYPE     = f!c!c!c!b
CINT     = 1//-1!4//32!4/36/56!4/60
LINE     = 32!19!5/1/1!6/1/1
FINT     = -7;-5;-3;-1
FLINE    = 2;23;22;3;0
HILO     = 0
HLSYM    = 0!1;1//22;22/2;2/hw
CLRBAR   = 1/V/LL!0
WIND     = bk0!bk0!bk0!bk0!bk9/0.7/2/112
REFVEC   =
TITLE    = 1/-2/~ ? ${MDL} 1000 MB MOIST CONV,SFC DEWPOINT, WIND|~1000 MB MST CONV!0
TEXT     = 1/22/2/hw
FILTER   = yes
run

GLEVEL   = 500:1000
GVCORD   = pres
SCALE      = 0
GDPFUN   = sm5s(lift)!sm5s(lift)!sm5s(lift)!kntv(wnd@30:0%pdly)
TYPE     = c/f!c!c!b
CONTUR   = 1
CINT     = 2/2!-10000;0.05!2/-100/-2
LINE     = 20/-32/1!0;5//0;4/0;0!32//2
FINT     = -8;-6;-4;-2;0.05;10
FLINE    = 2;15;21;22;23;0;24
HILO     = 0
HLSYM    = 0
CLRBAR   = 1/V/LL!0
WIND     = !!!bk10/0.9/2/112!bk0
TITLE    = 1/-2/~ ? ${MDL} 1000-500 MB LI& BL (0-30MB AGL)WND|~1000-500 LI!0
list
run

glevel  = 255:0
gvcord  = pdly
gdpfun  = sm5s(cape)!sm5s(cins)!sm5s(cins)!sm5s(cins)
type    = c/f!c!c
cint    = 250/250/1000!30/-60/-30!30/-210/-90!-2000;-250
line    = 22/1/2!6/10/3!6/1/2!25/1/3
fint    = 1000;2000;3000;4000;5000
fline   = 0;23;30;15;2;5
hlsym   = 1;1//22;22/2;2/hw
wind    = bk0
title   = 1/-2/~ ? ${MDL} BEST CAPE (jkg-1) & CIN (cyan)|~BEST CAPE & CIN!0
list
run

GLEVEL  = 30:0!30:0!30:0!0
GVCORD  = pdly!pdly!pdly!none
SCALE   = 0
GDPFUN  = sm5s(dwpf)!sm5s(dwpf)!sm5s(dwpf)!sm5s(mmsl)!kntv(wnd@30:0%pdly)
TYPE    = c/f!c!c!c!b
CINT    = 4/52!4/32/48!4/0/28!4
LINE    = 32/1/2/1!3/1/2!22/1/1!5//3
FINT    = 50;56;62;68;74
FLINE   = 0;23;22;30;14;2
HILO    = 0!0!0!5/H#;L#/1020-1060;900-1012
HLSYM   = !!!1.3;1.3//22/3/hw
CLRBAR  = 1/V/LL!0
WIND    = !!!!bk9/0.8/2/112
TITLE   = 1/-2/~ ? ${MDL} BL DWPT, WND(30MB AGL), MMSL|~SFC DEWPOINT!0
list
run

exit
EOF

    export err=$?;err_chk
    cnt=`expr $cnt - 1`
done

#####################################################
# GEMPAK DOES NOT ALWAYS HAVE A NON ZERO RETURN CODE
# WHEN IT CAN NOT PRODUCE THE DESIRED GRID.  CHECK
# FOR THIS CASE HERE.
#####################################################
ls -l ${metaname}
export err=$?;export pgm="GEMPAK CHECK FILE";err_chk

if [ $SENDCOM = "YES" ] ; then
  mv ${metaname} ${COMOUT}/rap_${PDY}_${cyc}_anlloop
  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MODEL ${DBN_ALERT_TYPE} $job \
     $COMOUT/rap_${PDY}_${cyc}_anlloop
  fi
fi
