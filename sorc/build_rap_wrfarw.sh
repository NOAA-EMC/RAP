set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0
module list

cd ${BASE}/rap_wrfarw.fd


./wcoss_doit.sh configure
./wcoss_doit.sh compile

cp WRFV3.9/main/real.exe ${BASE}/../exec/rap_wrfarw_real
cp WRFV3.9/main/wrf.exe ${BASE}/../exec/rap_wrfarw_fcst

##############################
