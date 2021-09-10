#!/bin/ksh

set -x

msg="JOB $job HAS BEGUN"
postmsg "$jlogfile" "$msg"

cd $DATA
rm $DATA/poescript
#printf %02d fhr

################################################################
# Create a script to be poe'd
#
#  Note:  The number of scripts to be run MUST match the number
#  of total_tasks set in the ecf script, or the job will fail.
#
################################################################

export n=1
#for script in `cat $FIXgempak/rap_meta`
#   do
#     eval "echo $script &> output.$n " >> $DATA/poescript
#     export n=`expr $n + 1`
#done

while read line
do 
  #echo "$line &> output.$n" >>$DATA/poescript
  eval echo "$line &> output.$n" >>$DATA/poescript
  export n=`expr $n + 1`
done <$FIXgempak/rap_meta

num=`cat $DATA/poescript |wc -l` 

while [ $num -lt $numproc ] ; do
   echo "hostname" >>poescript
   num=`expr $num + 1`
done

chmod 775 $DATA/poescript
export MP_PGMMODEL=mpmd
export MP_CMDFILE=$DATA/poescript

# Execute the script.

# poe
#aprun -N $PTILE -n $((NODES*PTILE)) cfp poescript
mpirun.lsf
#####################################################################
# GOOD RUN
set +x
echo "**************JOB RAP_META COMPLETED NORMALLY on the IBM-SP"
echo "**************JOB RAP_META COMPLETED NORMALLY on the IBM-SP"
echo "**************JOB RAP_META COMPLETED NORMALLY on the IBM-SP"
set -x
#####################################################################

echo EXITING $0
exit
#
