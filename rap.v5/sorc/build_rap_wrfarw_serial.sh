set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/RAP/v5.0.0
module unload PNetCDF-intel-sandybridge/1.5.0

cd ${BASE}/rap_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean
cp configure.wrf.serial configure.wrf
./compile em_real

##############################


