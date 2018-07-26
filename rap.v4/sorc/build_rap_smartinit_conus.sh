set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/RAP/v4.0.0.da
module list

cd ${BASE}/rap_smartinit_conus.fd
make clean
make

##############################
