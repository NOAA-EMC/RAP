set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0
module list

cd ${BASE}/rap_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean
cp configure.wrf.serial configure.wrf
./compile em_real

##############################
