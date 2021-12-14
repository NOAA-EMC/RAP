#!/bin/ksh -l
################################################################################
####  UNIX Script Documentation Block
#
# Script name:         rap_update_bc.sh.sms
# Script description:  runs the update_bc for RAP full cycle
#
# Author:     Julia Zhu / Ming Hu / Geoff Manikin   Org: EMC    Date: 2011-10-17
#
# Script history log:
# 2011-10-17  J Zhu / M Hu / G Manikin
# 2018-01-25  B Blake / M Hu / C Guastini / G Manikin / C Alexander - RAPv4

set -x

cd $DATA

# Set up some constants
export WPSNAMELIST=namelist.wps
START_TIME=$PDY$cyc
echo $START_TIME >STARTTIME

# Create the ram work directory and cd into it
workdir=$DATA/adjustbc
rm -rf ${workdir}
mkdir -p ${workdir}
cd ${workdir}

#######################################
# Process Update BC
#######################################

# Set up some constants
export FCST_LENGTH="36"
export FCST_INTERVAL="3"
export METGRIDPROC="METGRID_PROC"

# Set up the work directory and cd into it
workdir=$DATA/adjustbc
mkdir -p ${workdir}
cd ${workdir}

START_TIME=`cat ../STARTTIME`
echo $START_TIME

# Calculate start and end time date strings
END_TIME=`$NDATE +${FCST_LENGTH} $START_TIME`
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`

eyyyy=`echo ${END_TIME} | cut -c1-4`
emm=`echo ${END_TIME} | cut -c5-6`
edd=`echo ${END_TIME} | cut -c7-8`
ehh=`echo ${END_TIME} | cut -c9-10`
start_yyyymmdd_hhmmss=${syyyy}-${smm}-${sdd}_${shh}:00:00
end_yyyymmdd_hhmmss=${eyyyy}-${emm}-${edd}_${ehh}:00:00

# Calculate the forecast interval in seconds
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))

# Print run parameters
echo
echo "START TIME = "${START_TIME}"
echo "  END TIME = "${END_TIME}"
echo

 if [ -r ${DATA}/gsiprd/wrf_inout ]; then
  echo " Initial condition ${DATA}/gsiprd/wrf_inout"
  ln -sf ${DATA}/gsiprd/wrf_inout wrfinput_d01
  ln -sf ${DATA}/gsiprd/wrf_inout wrfvar_output
 else
   echo "FATAL ERROR: ${DATA}/gsiprd/wrf_inout does not exist.  Run will stop."
   err_exit
 fi

## boundary condition searching
## boundary condition searching
currentime=${syyyy}${smm}${sdd}${shh}00
set -A XX `ls ${RAPBC}/wrfbdy_d01.*00 | sort -r`
maxnum=${#XX[*]}
bdtime=`echo ${XX[0]} |awk 'BEGIN {FS="/"} {print $NF}'|cut -c12-`

if [[ ${currentime} -ge ${bdtime} ]]; then
   echo "using latest ${XX[0]} as boundary condition "
   cp ${XX[0]} wrfbdy_d01
else
   nn=1
   bdtime=`echo ${XX[$nn]} |awk 'BEGIN {FS="/"} {print $NF}'|cut -c12-`
   until [[ ${currentime} -ge ${bdtime} || ${nn} -eq ${maxnum} ]];do
      ((nn += 1))
       bdtime=`echo ${XX[$nn]} |awk 'BEGIN {FS="/"} {print $NF}'|cut -c12-`
   done

   if [[ ${nn} -eq ${maxnum} ]]; then
        echo "FATAL ERROR: can not find boundary conditions for ${currentime} !!!"
        err_exit
   else
      echo " using old ${XX[$nn]} as boundary condition"
      cp ${XX[$nn]} wrfbdy_d01
   fi
fi
## end of boundary condition searching

# Get the start and end time components
start_year=`echo ${START_TIME} | cut -c1-4`
start_month=`echo ${START_TIME} | cut -c5-6`
start_day=`echo ${START_TIME} | cut -c7-8`
start_hour=`echo ${START_TIME} | cut -c9-10`
start_minute="00"
start_second="00"
end_year=`echo ${END_TIME} | cut -c1-4`
end_month=`echo ${END_TIME} | cut -c5-6`
end_day=`echo ${END_TIME} | cut -c7-8`
end_hour=`echo ${END_TIME} | cut -c9-10`
end_minute="00"
end_second="00"
start_YYYYMMDDHHMM=${start_year}${start_month}${start_day}${start_hour}${start_minute}

# update bc
echo "begin update bc"
cp ${PARMrap}/rap_update_bc_parame.in parame.in
ln -s wrfinput_d01  wrfinputd1
ln -s wrfvar_output wrfvar_out
cp ${EXECrap}/rap_update_bc .
runline="aprun -n 1 -N 1 ./rap_update_bc"
$runline >> $DATA/$pgmout 2>errfile
export err=$?; err_chk

echo "end update bc"

cp wrfbdy_d01 ${COMOUT}/rap.t${cyc}z.wrfbdy 

exit 0
