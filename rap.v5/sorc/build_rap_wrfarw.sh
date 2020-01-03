set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/RAP/v5.0.0

module list

cd ${BASE}/rap_wrfarw.fd


./wcoss_doit.sh configure
./wcoss_doit.sh compile

cp WRFV3.9/main/real.exe ${BASE}/../exec/rap_wrfarw_real
cp WRFV3.9/main/wrf.exe ${BASE}/../exec/rap_wrfarw_fcst

module unload craype-haswell
module load craype-sandybridge

##############################
