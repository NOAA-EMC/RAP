set -x

##############################

export BASE=`pwd`
cd $BASE

source $BASE/build_rap_module_load.sh

cd ${BASE}/rap_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean
cp configure.wrf.serial configure.wrf
./compile em_real

##############################
