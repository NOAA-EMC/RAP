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

export TOCGRIB=${TOCGRIB:-/gpfs/gp1/nco/ops/nwprod/util/exec/tocgrib}
export TOCGRIB2=${TOCGRIB2:-/gpfs/gp1/nco/ops/nwprod/util/exec/tocgrib2}
export GRBINDEX=$utilexec/grbindex

run236="00 01 02 03 04 05 06 07 08 09 12"
if  echo $run236 |grep $fhr;
then
  # Processing AWIPS 236 grids
  export pgm=tocgrib
#  . prep_step
#  startmsg

#################################################################
# Generate 3-hour accumulations for precip, snow, graupel fields 
################################################################

  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 ] ; then

    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp236pgrbf${fhr}.grib2 rap.t${cyc}z.awp236pgrbf${fhr}.grib2.idxbin
    cp ${COMOUT}/rap.t${cyc}z.awp236pgrbf${fhr}.grib2 awp236pgrbf${fhr}.grib2
    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp236pgrbf${fhr3}.grib2 rap.t${cyc}z.awp236pgrbf${fhr3}.grib2.idxbin
    while [ ! -r rap.t${cyc}z.awp236pgrbf${fhr3}.grib2.idxbin -o \
            ! -r $COMOUT/rap.t${cyc}z.awp236pgrbf${fhr3}.grib2 ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done

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
    ln -sf graupel236.${fhr}          fort.54
$EXECrap/rap_subflds_g2 << EOF >> subfld.out
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
#    export err=$?;err_chk

    cat precip236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat ncprecip236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat cprecip236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat weasd236.${fhr} >> awp236pgrbf${fhr}.grib2
    cat graupel236.${fhr} >> awp236pgrbf${fhr}.grib2

    cp awp236pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2
# end grid 236 processing
fi

 $utilexec/cnvgrib -g21 $COMOUT/rap.${cycle}.awp236pgrbf${fhr}.grib2 RAP40PRS
 $GRBINDEX RAP40PRS RAP40PRSI

  export FORTREPORTS=unit_vars=yes 
  export FORT11=RAP40PRS
  export FORT12=RAP40PRSI
  export FORT51=xtrn.${cycle}.faarap${fhr}

  $TOCGRIB <$PARMutil/rap_faa${fhr}.236 parm='KWBG' >> tocgrib.out 
#  err=$?;export err ;#err_chk

  if test "$SENDCOM" = 'YES'
  then
    cp xtrn.${cycle}.faarap${fhr} $pcom/rap.${cycle}.g236xtrn.f${fhr}
#    cp xtrn.${cycle}.faarap${fhr} $pcom/rap.${cycle}.g236xtrn.f${fhr}
  fi
fi   # end grid 236 processing

run130="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21"
if  echo $run130 |grep $fhr
then

cp ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr}.grib2 awp130pgrbf${fhr}.grib2 
#################################################################
# Generate 3-hour accumulations for precip, snow, graupel fields
################################################################

  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 ] ; then

    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr}.grib2 rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin
    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr3}.grib2 rap.t${cyc}z.awp130pgrbf${fhr3}.grib2.idxbin
    while [ ! -r rap.t${cyc}z.awp130pgrbf${fhr3}.grib2.idxbin -o \
            ! -r $COMOUT/rap.t${cyc}z.awp130pgrbf${fhr3}.grib2 ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done

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
    ln -sf graupel130.${fhr}          fort.54
$EXECrap/rap_subflds_g2 << EOF >> subfld.out
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
#    export err=$?;err_chk

    cat precip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat ncprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat cprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat weasd130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat graupel130.${fhr} >> awp130pgrbf${fhr}.grib2
    cp awp130pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2
fi # 3-hr check

#################################################################
# Generate 2-hour accumulations for precip, snow, graupel fields
################################################################

  if [ $fhr -eq 05 -o $fhr -eq 08 -o $fhr -eq 11 -o $fhr -eq 14 -o \
       $fhr -eq 17 -o $fhr -eq 20 ] ; then

    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr}.grib2 rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin
    let fhr2=fhr-2
    typeset -Z2 fhr2
#
# Make sure fhr2 prdgen files are available before
# proceeding.
#
    ic=0
    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp130pgrbf${fhr2}.grib2 rap.t${cyc}z.awp130pgrbf${fhr2}.grib2.idxbin
    while [ ! -r rap.t${cyc}z.awp130pgrbf${fhr2}.grib2.idxbin -o \
            ! -r $COMOUT/rap.t${cyc}z.awp130pgrbf${fhr2}.grib2 ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr2 files"
      fi
      sleep 15
    done

# start grid 130 processing

    cp rap.t${cyc}z.awp130pgrbf${fhr}.grib2.idxbin awp130pgrbf${fhr}.grib2.idxbin
    cp $COMOUT/rap.t${cyc}z.awp130pgrbf${fhr2}.grib2 awp130pgrbf${fhr2}.grib2
    cp rap.t${cyc}z.awp130pgrbf${fhr2}.grib2.idxbin awp130pgrbf${fhr2}.grib2.idxbin

    IARW=0
    ISNOW=1
    pfhr1=${fhr}
    pfhr2=${fhr2}

#    export pgm=rap_subflds_g2;. prep_step
    ln -sf awp130pgrbf${fhr2}.grib2      fort.13
    ln -sf awp130pgrbf${fhr2}.grib2.idxbin  fort.14
    ln -sf awp130pgrbf${fhr}.grib2       fort.15
    ln -sf awp130pgrbf${fhr}.grib2.idxbin   fort.16
    ln -sf precip130.${fhr}           fort.50
    ln -sf ncprecip130.${fhr}         fort.51
    ln -sf cprecip130.${fhr}          fort.52
    ln -sf weasd130.${fhr}            fort.53
    ln -sf graupel130.${fhr}          fort.54
$EXECrap/rap_subflds_g2 << EOF >> subfld.out
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
#    export err=$?;err_chk

    cat precip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat ncprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat cprecip130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat weasd130.${fhr} >> awp130pgrbf${fhr}.grib2
    cat graupel130.${fhr} >> awp130pgrbf${fhr}.grib2
#  do NOT need to copy this new version back to COMOUT, as while do need to
#   send the 2-hr accums to AWIPS, we are ending the generation of them in all
#   other files
#  cp awp130pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp130pgrbf${fhr}.grib2
fi # 2-hr check

# Processing AWIPS 130 grids
  pgm=tocgrib2
#  export pgm;. prep_step
#  startmsg

 
  export FORTREPORTS=unit_vars=yes
  #export FORT11=rap.${cycle}.awp130pgrbf${fhr}.grib2      
  export FORT11=awp130pgrbf${fhr}.grib2      
  export FORT31="";
  export FORT51=grib2.${cycle}.awprap13f${fhr}   

  $TOCGRIB2 <$PARMutil/grib2_awprap13f${fhr} >> tocgrib2.out 
  cp grib2.${cycle}.awprap13f${fhr} $pcom/grib2.${cycle}.awprap13f${fhr}
#  err=$?;export err ;#err_chk
fi # 130 processing

###############################################################
# Generate 3-hour precip, snow, and graupel fields for grid 252
#   no AWIPS processing here.   just making buckets
###############################################################

  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 ] ; then

    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr}.grib2 rap.t${cyc}z.awp252pgrbf${fhr}.grib2.idxbin
    cp ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr}.grib2 awp252pgrbf${fhr}.grib2
    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
    $utilexec/grb2index ${COMOUT}/rap.t${cyc}z.awp252pgrbf${fhr3}.grib2 rap.t${cyc}z.awp252pgrbf${fhr3}.grib2.idxbin
    while [ ! -r rap.t${cyc}z.awp252pgrbf${fhr3}.grib2.idxbin -o \
            ! -r $COMOUT/rap.t${cyc}z.awp252pgrbf${fhr3}.grib2 ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done

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
    ln -sf graupel252.${fhr}          fort.54
$EXECrap/rap_subflds_g2 << EOF >> subfld.out
$pfhr1 $pfhr2 $IARW $ISNOW
EOF
#    export err=$?;err_chk

    cat precip252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat ncprecip252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat cprecip252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat weasd252.${fhr} >> awp252pgrbf${fhr}.grib2
    cat graupel252.${fhr} >> awp252pgrbf${fhr}.grib2

    cp awp252pgrbf${fhr}.grib2 $COMOUT/rap.${cycle}.awp252pgrbf${fhr}.grib2
fi # end 252 processing

#  process AWIPS 200 grids


run200="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21"
#run200="10 11 12 13 14 15 16 17 18 19 20 21"
if  echo $run200 |grep $fhr
then
 pgm=tocgrib2
#  export pgm;. prep_step
#  startmsg

  export FORTREPORTS=unit_vars=yes
  export FORT11=$COMOUT/rap.${cycle}.awp200f${fhr}.grib2
  export FORT31="";
  export FORT51=grib2.${cycle}.awprap200f${fhr}

  $TOCGRIB2 <$PARMutil/grib2_awprap200f${fhr} >> tocgrib3.out
  cp grib2.${cycle}.awprap200f${fhr} $pcom/grib2.${cycle}.awprap200f${fhr}
#  err=$?;export err ;#err_chk
fi # 200 processing
