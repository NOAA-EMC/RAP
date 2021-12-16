set -x

##############################

export BASE=`pwd`
cd $BASE

module purge
module load envvar/1.0

module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0
module list

cd ${BASE}/rap_full_cycle_surface.fd
make clean
make

##############################
