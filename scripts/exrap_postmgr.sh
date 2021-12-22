#! /bin/ksh -l
#########################################################################
# Script name:         exrap_postmgr.sh.sms
#
#  This script monitors the progress of the rap_fcst job
#
# Script history log:
# 2018-01-25  B Blake / C Guastini - RAPv4

set -x

cd $DATA

export PS4='$SECONDS + '

hour=00
typeset -Z2 hour
if [ $cyc -eq 03 -o $cyc -eq 09 -o $cyc -eq 15 -o $cyc -eq 21 ] ; then
TCP=52
TEND=51
else
TCP=22
TEND=21
fi

# make a space for saving all smoke products from one forecast
mkdir -p smoke
DATAsmoke=$(pwd)/smoke

if [ -e posthours ]; then
   rm -f posthours
fi

while [ $hour -lt $TCP ]; 
do
  echo $hour >>posthours
     let "hour=hour+1"
done
postjobs=`cat posthours`

# Compute date & time components for the forecast
START_TIME=${START_TIME:-$PDY$cyc}
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`

#
# Wait for all fcst hours to finish 
#
icnt=1
while [ $icnt -lt 1000 ]
do
  for fhr in $postjobs
  do
    if [ -s $INPUT_DATA/fcstdone${fhr}00.${shh} ]
    then
	    # need to pass ${DATAsmoke} into post job submission.
      ecflow_client --event release_post${fhr}
#      qsub -v DATAsmoke=${DATAsmoke} ${HOMErap}/sms/post/jrap_post_f${fhr}_${shh}.bsub
#      qsub ${HOMErap}/sms/wrfbufr/jrap_wrfbufr_f${fhr}_${shh}.bsub
      # Remove current fhr from list
      postjobs=`echo $postjobs | sed s/${fhr}//g`
    fi
  done
  
  result_check=`echo $postjobs | wc -w`
  if [ $result_check -eq 0 ]
  then
     break
  fi

  sleep 10
  icnt=$((icnt + 1))
  if [ $icnt -ge 240 ]
  then
    msg="FATAL ERROR: ABORTING after 40 minutes of waiting for RAP FCST hours $postjobs."
    err_exit $msg
  fi

done


echo Exiting $0

exit
