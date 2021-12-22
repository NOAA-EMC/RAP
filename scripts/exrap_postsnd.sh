#! /bin/ksh -l
###################################################
#  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exrap_sndpost.ecf
# Script description:  Create RAP bufr sounding files
#
# Author:        Geoff Manikin       Org: NP22      
#
# Script history log:
# 2018-01-25  B Blake / C Guastini / G Manikin - RAPv4

set -x

cd $DATA

export PS4='SNDP $SECONDS + '

cp $PARMrap/rap_bufr.tbl .
cp $PARMrap/rap_modtop.parm .
cp $PARMrap/rap_sndp.parm.mono sndp.parm 


fhr=00
typeset -Z2 fhr

if [ $cyc -eq 03 -o $cyc -eq 09 -o $cyc -eq 15 -o $cyc -eq 21 ] ; then
  endfhr=51
else 
  endfhr=21
fi


rm -rf profilm*

while [ $fhr -le $endfhr ]
do
   PROFDIR=$DATAROOT/rap_wrfbufr_${envir}_${cyc}_f${fhr}
   cp ${PROFDIR}/profilm.c1.f${fhr} .
   if [ $fhr -ne 0 ]; then
     let "fhr1=fhr-1"
     typeset -Z2 fhr1
     cat profilm_f${fhr1} profilm.c1.f${fhr} > profilm_f${fhr}
   else
     cp profilm.c1.f${fhr} profilm_f${fhr}
   fi
   let "fhr=fhr+1"
   typeset -Z2 fhr
done

mv profilm_f${endfhr} profilm.c1.${tmmark}

ln -sf sndp.parm    fort.11
ln -sf rap_bufr.tbl fort.32
ln -sf profilm.c1.${tmmark} fort.66
ln -sf class1.bufr fort.78
${EXECrap}/rap_sndp < rap_modtop.parm  > sndp.out
export err=$?; err_chk

#need to manipulate the file to get it to be compatible
#  with gempak on wcoss
cwordsh unblk class1.bufr class1.bufr_unblock
cwordsh block class1.bufr_unblock class1.bufr_block

if [ $SENDCOM == "YES" ]; then
  cp class1.bufr_block ${COMOUT}/rap.t${cyc}z.class1.bufr.${tmmark}
fi

#Send bufr file
if test "$SENDDBN" = 'YES'
  then
   $DBNROOT/bin/dbn_alert MODEL RAP_BUFRSND $job $COMOUT/rap.t${cyc}z.class1.bufr.${tmmark}
fi
### break out bufr file into individual station files
 cat <<EOF > stnmlist_input
1
$DATA/class1.bufr
$DATA/bufr.${cycle}/bufr
EOF

   mkdir -p ${DATA}/bufr.${cycle}

   ln -sf $DATA/class1.bufr fort.20
   export DIRD=${DATA}/bufr.${cycle}/bufr

  pgm=rap_stnmlist
  startmsg
  $EXECrap/rap_stnmlist < stnmlist_input >> $pgmout 2> errfile
  err=$?;export err ;err_chk

# Tar and gzip the individual bufr files and send them to /com
cd ${DATA}/bufr.${cycle}
tar -cf - . | /usr/bin/gzip > ../rap.${cycle}.bufrsnd.tar.gz
cd ${DATA}
if test "$SENDCOM" = 'YES'
then
   cp rap.${cycle}.bufrsnd.tar.gz ${COMOUT}/rap.${cycle}.bufrsnd.tar.gz
fi

#Send the alerts
if test "$SENDDBN" = 'YES'
  then
   $DBNROOT/bin/dbn_alert MODEL RAP_BUFRSND_TAR $job ${COMOUT}/rap.${cycle}.bufrsnd.tar.gz
fi

# make gempak files
$USHrap/rap_bfr2gpk.sh

exit
