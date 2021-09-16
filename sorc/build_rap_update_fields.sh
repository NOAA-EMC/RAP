set -x

##############################

export BASE=`pwd`

module purge
module load envvar/1.0
module use $BASE/../modulefiles
module load RAP/v5.0.0.da
module list

cd ${BASE}/rap_update_fields.fd
make clean
make

##############################
