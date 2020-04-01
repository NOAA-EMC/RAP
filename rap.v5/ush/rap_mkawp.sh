################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_mkawp.sh
# Script description:  To generate the AWIPS products for the Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2011-10-06
#
# Script history log:
# 2011-10-06  G Manikin  -- new script
# 2013-01-10  G Manikin  - adapted for WCOSS
# 2018-01-29  B Blake / C Guastini - RAPv4
#################################################################################

set -xa

fhr=$1

run236="00 01 02 03 04 05 06 07 08 09 12 15 18 21 24 27 30 33 36 39"
if  echo $run236 |grep $fhr;
then
  # Processing AWIPS 236 grids
  export pgm=tocgrib
#  . prep_step
#  startmsg

#######################################################
# Generate 3-hour precip and snow water equivalent.
#######################################################

  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o \
       $fhr -eq 30 -o $fhr -eq 33 -o $fhr -eq 36 -o $fhr -eq 39 ] ; then
       #$fhr -eq 18 -o $fhr -eq 21 ] ; then

    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp236pgrbf${fhr}.grib2 rap.t${cyc}z.awp236pgrbf${fhr}.grib2.idxbin
    cp ${COMOUT}/rap.t${cyc}z.awp236pgrbf${fhr}.grib2 awp236pgrbf${fhr}.grib2
    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
    while [ ! -r $INPUT_DATA/postdone_236_f${fhr3}_${cyc} ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done
    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp236pgrbf${fhr3}.grib2 rap.t${cyc}z.awp236pgrbf${fhr3}.grib2.idxbin

# start grid 236 processing

    cp rap.t${cyc}z.awp236pgrbf${fhr}.grib2.idxbin awp236pgrbf${fhr}.grib2.idxbin
    cp $COMOUT/rap.t${cyc}z.awp236pgrbf${fhr3}.grib2 awp236pgrbf${fhr3}.grib2
    cp rap.t${cyc}z.awp236pgrbf${fhr3}.grib2.idxbin awp236pgrbf${fhr3}.grib2.idxbin

    IARW=0
    ISNOW=1
    pfhr1=${fhr}
    pfhr2=${fhr3}

#    export pgm=rap_subflds_g2;. prep_step
    ln -sf awp236pgrbf${fhr3}.grib2      fort.13
    ln -sf awp236pgrbf${fhr3}.grib2.idxbin  fort.14
    ln -sf awp236pgrbf${fhr}.grib2       fort.15
    ln -sf awp236pgrbf${fhr}.grib2.idxbin   fort.16
    ln -sf precip236.${fhr}           fort.50
    ln -sf ncprecip236.${fhr}         fort.51
    ln -sf cprecip236.${fhr}          fort.52
    ln -sf weasd236.${fhr}            fort.53
    ln -sf graupel236.${fhr}            fort.54
pgmout=rap236_3
#$EXECrap/rap_subflds_g2 << EOF >> $DATA/$pgmout 2>errfile
$EXECrap/rap_subflds_g2 << EOF
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
    export err=$?;err_chk

    cat precip236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat ncprecip236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat cprecip236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat weasd236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat graupel236.${fhr} >> awp236pgrbf${fhr}.grib2
  if test "$SENDCOM" = 'YES'
  then
    cp awp236pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2
    ${WGRIB2} $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2 -s >$COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2.idx
  fi
# end grid 236 processing
  if [ $SENDDBN = YES ]
    then
      ALERT_TYPE=RAP_PG40_GB2

      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2
      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2.idx
  fi
fi
  if [ $fhr -eq 03 -o $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o $fhr -eq 30 -o \
       $fhr -eq 33 -o $fhr -eq 36 ] ; then
    echo done >$INPUT_DATA/postdone_236_f${fhr}_${cyc}
  fi
  
  if [ $fhr -eq 00 -o $fhr -eq 01 -o $fhr -eq 02 -o $fhr -eq 03 -o $fhr -eq 04 -o \
       $fhr -eq 05 -o $fhr -eq 06 -o $fhr -eq 07 -o $fhr -eq 08 -o $fhr -eq 09 \
       -o $fhr -eq 12 ] ; then

 #XXW $CNVGRIB -g21 $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2 RAP40PRS
# Change grid 255 to grid 236 in grib1
 $CNVGRIB -g21 $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2 tmp${fhr}.grib1
 ln -sf  tmp${fhr}.grib1   fort.11

$OVERGRIDID << EOF
236
EOF

 mv fort.51  RAP40PRS
 rm fort.11 

 $GRBINDEX RAP40PRS RAP40PRSI

  export FORTREPORTS=unit_vars=yes 
  export FORT11=RAP40PRS
  export FORT12=RAP40PRSI
  export FORT51=xtrn.${cycle}.faarap${fhr}
pgmout=rap236_tocgrib
  $TOCGRIB <$PARMrap/wmo/rap_faa${fhr}.236 parm='KWBG' >> tocgrib1.out
  #err=$?;export err ;err_chk

  if test "$SENDCOM" = 'YES'
  then
    cp xtrn.${cycle}.faarap${fhr} $WMO/rap.${cycle}.g236xtrn.f${fhr}
  fi
  if test "$SENDDBN_NTC" = 'YES'
  then
    $DBNROOT/bin/dbn_alert MODEL RAP_WPG $job $WMO/rap.${cycle}.g236xtrn.f${fhr}
  fi

  if test "$SENDDBN_NTC" = 'YES'
  then
   if test `expr ${cyc} % 3` -ne 0
    then
     if [ fhr -le 03 ]
      then
       $DBNROOT/bin/dbn_alert GRIB_LOW rap $job $WMO/rap.${cycle}.g236xtrn.f${fhr}
     fi
    else
       $DBNROOT/bin/dbn_alert GRIB_LOW rap $job $WMO/rap.${cycle}.g236xtrn.f${fhr}
    fi
  fi
  fi
fi   # end grid 236 processing

run130="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39"
if  echo $run130 |grep $fhr
then

cp ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr}.grib2 awp130pgrbf${fhr}.grib2 
#cp awp130pgrbf${fhr}.grib2 wmo.awp130pgrbf${fhr}.grib2
#######################################################
# Generate 3-hour precip and snow water equivalent for on-time runs.
#######################################################

  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o \
       $fhr -eq 30 -o $fhr -eq 33 -o $fhr -eq 36 -o $fhr -eq 39 ] ; then
       #$fhr -eq 18 -o $fhr -eq 21 ] ; then

    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr}.grib2 rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin
    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
    while [ ! -r $INPUT_DATA/postdone_130_f${fhr3}_${cyc} ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done
    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr3}.grib2 rap.t${cyc}z.awp130pgrbf${fhr3}.grib2.idxbin

# start grid 130 processing

    cp rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin awp130pgrbf${fhr}.grib2.idxbin
    cp $COMOUT/rap.t${cyc}z.awp130pgrbf${fhr3}.grib2 awp130pgrbf${fhr3}.grib2
    cp rap.t${cyc}z.awp130pgrbf${fhr3}.grib2.idxbin awp130pgrbf${fhr3}.grib2.idxbin

    IARW=0
    ISNOW=1
    pfhr1=${fhr}
    pfhr2=${fhr3}

#    export pgm=rap_subflds_g2;. prep_step
    ln -sf awp130pgrbf${fhr3}.grib2      fort.13
    ln -sf awp130pgrbf${fhr3}.grib2.idxbin  fort.14
    ln -sf awp130pgrbf${fhr}.grib2       fort.15
    ln -sf awp130pgrbf${fhr}.grib2.idxbin   fort.16
    ln -sf precip130.${fhr}           fort.50
    ln -sf ncprecip130.${fhr}         fort.51
    ln -sf cprecip130.${fhr}          fort.52
    ln -sf weasd130.${fhr}            fort.53
    ln -sf graupel130.${fhr}            fort.54
pgmout=rap130_3
#$EXECrap/rap_subflds_g2 << EOF >> $DATA/$pgmout 2>errfile
$EXECrap/rap_subflds_g2 << EOF 
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
    #export err=$?;err_chk

    cat precip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat ncprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat cprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat weasd130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat graupel130.${fhr} >> awp130pgrbf${fhr}.grib2
  if test "$SENDCOM" = 'YES'
  then
    cp awp130pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2
    ${WGRIB2} $COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2 -s >$COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2.idx
  fi
  if [ $SENDDBN = YES ]
    then
       ALERT_TYPE=RAP_PG13_GB2
       $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2
       $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2.idx
  fi  
fi # 3-hr check
  if [ $fhr -eq 03 -o $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o $fhr -eq 30 -o \
       $fhr -eq 33 -o $fhr -eq 36 ] ; then
       #$fhr -eq 18 ] ; then
    echo done >$INPUT_DATA/postdone_130_f${fhr}_${cyc}
  fi

#######################################################
# Generate 2-hour precip and snow water equivalent for on-time runs.
#######################################################

#  if [ $fhr -eq 05 -o $fhr -eq 08 -o $fhr -eq 11 -o $fhr -eq 14 -o \
#       $fhr -eq 17 -o $fhr -eq 20 ] ; then
#
#    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr}.grib2 rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin
#    let fhr2=fhr-2
#    typeset -Z2 fhr2
#
# Make sure fhr2 prdgen files are available before
# proceeding.
#
#    ic=0
#    while [ ! -r $INPUT_DATA/postdone_130_f${fhr2}_${cyc} ] ; do
#      let ic=ic+1
#      if [ $ic -gt 180 ] ; then
#        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr2 files"
#      fi
#      sleep 15
#    done
#    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr2}.grib2 rap.t${cyc}z.awp130pgrbf${fhr2}.grib2.idxbin

# start grid 130 processing

#    cp rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin awp130pgrbf${fhr}.grib2.idxbin
#    cp $COMOUT/rap.t${cyc}z.awp130pgrbf${fhr2}.grib2 awp130pgrbf${fhr2}.grib2
#    cp rap.t${cyc}z.awp130pgrbf${fhr2}.grib2.idxbin awp130pgrbf${fhr2}.grib2.idxbin

#    IARW=0
#    ISNOW=1
#    pfhr1=${fhr}
#    pfhr2=${fhr2}

#    export pgm=rap_subflds_g2;. prep_step
#    ln -sf awp130pgrbf${fhr2}.grib2      fort.13
#    ln -sf awp130pgrbf${fhr2}.grib2.idxbin  fort.14
#    ln -sf awp130pgrbf${fhr}.grib2       fort.15
#    ln -sf awp130pgrbf${fhr}.grib2.idxbin   fort.16
#    ln -sf precip130.${fhr}           fort.50
#    ln -sf ncprecip130.${fhr}         fort.51
#    ln -sf cprecip130.${fhr}          fort.52
#    ln -sf weasd130.${fhr}            fort.53
#    ln -sf graupel130.${fhr}            fort.54
#$EXECrap/rap_subflds_g2 << EOF >> $DATA/$pgmout 2>errfile
#$pfhr1 $pfhr2 $IARW $ISNOW
#EOF
#    export err=$?;err_chk

#    cat precip130.${fhr} >> awp130pgrbf${fhr}.grib2
#    cat ncprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
#    cat cprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
#    cat weasd130.${fhr} >> awp130pgrbf${fhr}.grib2
#    cat graupel130.${fhr} >> awp130pgrbf${fhr}.grib2

#  do NOT need to copy this new version back to COMOUT, as while do need to
#   send the 2-hr accums to AWIPS, we are ending the generation of them in all
#   other files

#fi # 2-hr check

# Processing AWIPS 130 grids
#  pgm=tocgrib2
#  export pgm;. prep_step
#  startmsg


  export FORTREPORTS=unit_vars=yes
  export FORT11=awp130pgrbf${fhr}.grib2      
  export FORT31="";
  export FORT51=grib2.${cycle}.awprap13f${fhr}   
  pgmout=rap130tocgrib
  $TOCGRIB2 <$PARMrap/wmo/grib2_awprap13f${fhr} >> tocgrib2.out
  #err=$?;export err ;err_chk
  if test "$SENDCOM" = 'YES'
  then
    cp grib2.${cycle}.awprap13f${fhr} $WMO/grib2.${cycle}.awprap13f${fhr}
  fi
# GSM  had been sending alerts for all fhr < 13 for every
#   3rd cycle and all fhr < 10 for all other cycles;  changed
#   10/9/12 to alert all forecast hours for all cycles  
  if  [ $SENDDBN_NTC = 'YES' -a $fhr -le 21 ] 
  then
    # rap130 grib2 files to AWIPS NCF
     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $WMO/grib2.${cycle}.awprap13f${fhr}
  fi
fi # 130 processing

#######################################################
# Generate 3-hour precip and snow water equivalent for grid 252
#   no AWIPS processing here.   just making buckets
#######################################################

  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o \
       $fhr -eq 30 -o $fhr -eq 33 -o $fhr -eq 36 -o $fhr -eq 39 ] ; then
       #$fhr -eq 18 -o $fhr -eq 21 ] ; then

    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr}.grib2 rap.t${cyc}z.awp252pgrbf${fhr}.grib2.idxbin
    cp ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr}.grib2 awp252pgrbf${fhr}.grib2
    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
    while [ ! -r $INPUT_DATA/postdone_252_f${fhr3}_${cyc} ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done
    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr3}.grib2 rap.t${cyc}z.awp252pgrbf${fhr3}.grib2.idxbin

# start grid 252 processing

    cp rap.t${cyc}z.awp252pgrbf${fhr}.grib2.idxbin awp252pgrbf${fhr}.grib2.idxbin
    cp $COMOUT/rap.t${cyc}z.awp252pgrbf${fhr3}.grib2 awp252pgrbf${fhr3}.grib2
    cp rap.t${cyc}z.awp252pgrbf${fhr3}.grib2.idxbin awp252pgrbf${fhr3}.grib2.idxbin

    IARW=0
    ISNOW=1
    pfhr1=${fhr}
    pfhr2=${fhr3}

#    export pgm=rap_subflds_g2;. prep_step
    ln -sf awp252pgrbf${fhr3}.grib2      fort.13
    ln -sf awp252pgrbf${fhr3}.grib2.idxbin  fort.14
    ln -sf awp252pgrbf${fhr}.grib2       fort.15
    ln -sf awp252pgrbf${fhr}.grib2.idxbin   fort.16
    ln -sf precip252.${fhr}           fort.50
    ln -sf precip252.${fhr}           fort.50
    ln -sf ncprecip252.${fhr}         fort.51
    ln -sf cprecip252.${fhr}          fort.52
    ln -sf weasd252.${fhr}            fort.53
    ln -sf graupel252.${fhr}            fort.54
pgmout=rap252_3
#$EXECrap/rap_subflds_g2 << EOF >> $DATA/$pgmout 2>errfile
$EXECrap/rap_subflds_g2 << EOF 
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
    #export err=$?;err_chk

    cat precip252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat ncprecip252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat cprecip252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat weasd252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat graupel252.${fhr} >> awp252pgrbf${fhr}.grib2

    if test "$SENDCOM" = 'YES'
      then
       cp awp252pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp252pgrbf${fhr}.grib2
       ${WGRIB2} $COMOUT/rap.${cycle}.awp252pgrbf${fhr}.grib2 -s > $COMOUT/rap.${cycle}.awp252pgrbf${fhr}.grib2.idx
    fi

    if [ $SENDDBN = YES ]; then
       $DBNROOT/bin/dbn_alert MODEL RAP_PG20_GB2 $job ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr}.grib2
       $DBNROOT/bin/dbn_alert MODEL RAP_PG20_GB2_WIDX $job $COMOUT/rap.${cycle}.awp252pgrbf${fhr}.grib2.idx
    fi

fi # end 252 processing
  if [ $fhr -eq 03 -o $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18  -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o $fhr -eq 30 -o \
       $fhr -eq 33 -o $fhr -eq 36 ] ; then
    echo done >$INPUT_DATA/postdone_252_f${fhr}_${cyc}
  fi

#process 200
run200="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39"
if  echo $run200 |grep $fhr
then
 pgm=tocgrib2
#  export pgm;. prep_step
#  startmsg

  export FORTREPORTS=unit_vars=yes
  export FORT11=$COMOUT/rap.${cycle}.awp200f${fhr}.grib2
  export FORT31="";
  export FORT51=grib2.${cycle}.awprap200f${fhr}

  $TOCGRIB2 <$PARMrap/wmo/grib2_awprap200f${fhr} >> tocgrib3.out

  #err=$?;export err ;#err_chk
  if test "$SENDCOM" = 'YES'
  then
    cp grib2.${cycle}.awprap200f${fhr} $WMO/grib2.${cycle}.awprap200f${fhr}
  fi
# GSM  had been sending alerts for all fhr < 13 for every
#   3rd cycle and all fhr < 10 for all other cycles;  changed
#   10/9/12 to alert all forecast hours for all cycles  
  if [ $SENDDBN_NTC = 'YES' -a $fhr -le 21 ];
  then
    # rap200 grib2 files to AWIPS NCF
     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $WMO/grib2.${cycle}.awprap200f${fhr}
  fi
fi # 200 processing

export PROCGTG=${PROCGTG:-YES};
if [ $PROCGTG = "YES" ]; then
#======================================================
# Apply TOCGRIB2 to GTG 130 when forecast hour is 0 1 2 3 6 9 12 15 18
#======================================================
fhrN=$(( 10#$fhr ))
fhrNs=" 0 1 2 3 6 9 12 15 18 "
if [[ $fhrNs =~ " $fhrN " ]] ; then

   grib_input=rap.${cycle}.gtg130f${fhr}.grib2
   nwstg_bull=grib2.gtg.${cycle}.${fhr}

   pgm=tocgrib2
   . prep_step
   export FORT11=${grib_input}
   export FORT31=""
   export FORT51=${nwstg_bull}
   startmsg
   $TOCGRIB2 < $PARMrap/wmo/gtg_grib2.f${fhr} >>$pgmout 2>errfile
   export err=$?; err_chk

   if [ "$SENDCOM" == 'YES' ]; then
       cp $nwstg_bull ${WMO}/.

       if [ "$SENDDBN" == 'YES' ]; then
           ${DBNROOT}/bin/dbn_alert NTC_LOW gtg $job $WMO/$nwstg_bull
       fi
   fi
fi
else
  echo "PROCGTG is NO, skip processing GTG ..."
fi
