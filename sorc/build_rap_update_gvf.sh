set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load RAP/v5.0.0.da
module list

cd ${BASE}/rap_update_gvf.fd
make clean
make

##############################
