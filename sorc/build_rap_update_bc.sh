set -x

##############################

export BASE=`pwd`

source $BASE/build_rap_module_load.sh

cd ${BASE}/rap_update_bc.fd
make clean
make

##############################
