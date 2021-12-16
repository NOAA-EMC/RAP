set -x

##############################

export BASE=`pwd`

module purge
module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0
module list

cd ${BASE}/rap_update_fields.fd
make clean
make

##############################
