set -x

##############################

export BASE=`pwd`
cd $BASE

module purge

# Load modules
export COMP=ftn
export COMP_MP=ftn
export COMP_MPI=ftn

export C_COMP=cc
export C_COMP_MP=cc

set COMPILER intel

setenv FFLAGS_COM "-fp-model strict"
setenv LDFLAGS_COM " "

module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0

module list

cd ${BASE}/rap_gsi.fd
rm -fr build
mkdir build
cd ${BASE}/rap_gsi.fd/build
cmake -DENKF_MODE=WRF -DBUILD_ENKF_PREPROCESS_ARW=ON -DBUILD_GSDCLOUD_ARW=ON ../.
make -j1

cp bin/gsi.x        ${BASE}/../exec/rap_gsi
cp bin/enkf_wrf.x   ${BASE}/../exec/rap_enkf
cp bin/enspreproc.x ${BASE}/../exec/rap_process_enkf
cp bin/initialens.x ${BASE}/../exec/rap_initialens

##############################
