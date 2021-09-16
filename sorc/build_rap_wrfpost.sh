set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load RAP/v5.0.0
module list

cd ${BASE}/rap_wrfpost.fd
make clean
make

##############################
