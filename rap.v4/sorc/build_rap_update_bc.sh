set -x

##############################

export BASE=`pwd`

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/RAP/v4.0.0
module unload PNetCDF-intel-sandybridge/1.5.0

module list

cd ${BASE}/rap_update_bc.fd
make clean
make

##############################
