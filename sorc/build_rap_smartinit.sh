set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/RAP/v5.0.0.da
module unload craype-sandybridge
module load craype-hashwell
#module load craype-sandybridge
module list

sleep 1

##############################

cd ${BASE}/rap_smartinit.fd
make clean
make

##############################
