set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/RAP/v4.0.0

module list

cd ${BASE}/rap_wrfarw.fd/WRFV3.8.1
./clean -aa
./clean -a
./clean
cp configure.wrf.optim configure.wrf
./compile em_real
cp main/real.exe ${BASE}/../exec/rap_wrfarw_real
cp main/wrf.exe ${BASE}/../exec/rap_wrfarw_fcst

module unload craype-haswell
module load craype-sandybridge

##############################
