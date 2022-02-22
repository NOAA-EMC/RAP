set -x

##############################

export BASE=`pwd`
cd $BASE

source $BASE/build_rap_module_load.sh

cd ${BASE}/rap_full_cycle_surface.fd
make clean
make

##############################
