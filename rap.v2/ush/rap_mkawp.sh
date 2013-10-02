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
#################################################################################

set -xa

fhr=$1

export TOCGRIB=${TOCGRIB:-/nwprod/util/exec/tocgrib}
export TOCGRIB2=${TOCGRIB2:-/nwprod/util/exec/tocgrib2}
export PARMutil=${PARMutil:-/nwprod/util/parm}

run236="00 01 02 03 04 05 06 07 08 09 12"
if  echo $run236 |grep $fhr;
then
  # Processing AWIPS 236 grids
  $GRBINDEX RAP40PRS1B RAP40PRS1BI
  export pgm=tocgrib
  . prep_step
  startmsg

  export FORTREPORTS=unit_vars=yes 
  export FORT11=RAP40PRS1B 
  export FORT12=RAP40PRS1BI
  export FORT51=xtrn.${cycle}.faarap${fhr}

  $TOCGRIB <$PARMutil/rap_faa${fhr}.236 parm='KWBG' >> $pgmout 2> errfile
  err=$?;export err ;#err_chk

  if test "$SENDCOM" = 'YES'
  then
    cp xtrn.${cycle}.faarap${fhr} $COMOUT/rap.${cycle}.g236xtrn.f${fhr}
    cp xtrn.${cycle}.faarap${fhr} $pcom/rap.${cycle}.g236xtrn.f${fhr}
  fi

  if test "$SENDDBN" = 'YES'
  then
    $DBNROOT/bin/dbn_alert MODEL RAP_WPG $job $COMOUT/rap.${cycle}.g236xtrn.f${fhr}
  fi

  if test "$SENDDBN_NTC" = 'YES'
  then
   if test `expr ${cyc} % 3` -ne 0
    then
     if [ fhr -le 03 ]
      then
       $DBNROOT/bin/dbn_alert GRIB_LOW rap $job $pcom/rap.${cycle}.g236xtrn.f${fhr}
     fi
    else
       $DBNROOT/bin/dbn_alert GRIB_LOW rap $job $pcom/rap.${cycle}.g236xtrn.f${fhr}
    fi
  fi
fi

run130="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18"
if  echo $run130 |grep $fhr
then
  # Processing AWIPS 130 grids
  pgm=tocgrib2
  export pgm;. prep_step
  startmsg

  export FORTREPORTS=unit_vars=yes
  export FORT11=$COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2      
  export FORT31="";
  export FORT51=grib2.${cycle}.awprap13f${fhr}   

  $TOCGRIB2 <$PARMutil/grib2_awprap13f${fhr} >> $pgmout 2> errfile
  err=$?;export err ;#err_chk

  if test "$SENDCOM" = 'YES'
  then
    cp grib2.${cycle}.awprap13f${fhr} $pcom/grib2.${cycle}.awprap13f${fhr}.$job
  fi

# GSM  had been sending alerts for all fhr < 13 for every
#   3rd cycle and all fhr < 10 for all other cycles;  changed
#   10/9/12 to alert all forecast hours for all cycles  
  if test "$SENDDBN_NTC" = 'YES'
  then
    # rap130 grib2 files to AWIPS NCF
     $DBNROOT/bin/dbn_alert NTC_LOW $NET $job $pcom/grib2.${cycle}.awprap13f${fhr}.$job
  fi
fi
