set -x

##############################

export BASE=`pwd`
cd $BASE

source $BASE/build_rap_module_load.sh

sleep 1

##############################

cd ${BASE}/rap_smartinit.fd
make clean
make

##############################
