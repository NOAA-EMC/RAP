set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-haswell
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/RAP/v4.0.0.da
module list

export CRAYPE_LINK_TYPE=dynamic

cd ${BASE}/rap_gsi.fd
make clean
cd ${BASE}/rap_gsi.fd/gsdcloud
make clean
make
cd ${BASE}/rap_gsi.fd
make
make install
make library

module unload craype-haswell
module load craype-sandybridge

unset CRAYPE_LINK_TYPE

##############################
