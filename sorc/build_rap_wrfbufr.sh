set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/RAP/v5.0.0.da
module unload PNetCDF-intel-sandybridge/1.5.0
module list

cd ${BASE}/rap_wrfbufr.fd
make clean
make

##############################
