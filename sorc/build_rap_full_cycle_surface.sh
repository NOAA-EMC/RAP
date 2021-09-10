set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles
module load $BASE/../modulefiles/RAP/v5.0.0.da
module list

cd ${BASE}/rap_full_cycle_surface.fd
make clean
make

##############################
