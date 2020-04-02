set -x

##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
# Load modules
module use -a /opt/cray/craype/default/modulefiles
module use -a /opt/cray/ari/modulefiles
module use -a /gpfs/hps/nco/ops/nwprod/modulefiles
module load PrgEnv-intel
module load cray-mpich
module load zlib-intel-haswell/1.2.7
module load HDF5-serial-intel-haswell/1.8.9
module load NetCDF-intel-haswell/4.2
module load grib_util/1.0.3
module load prod_util
module load prod_envir

module use -a /usrx/local/dev/modulefiles
module load cmake

module load craype-hugepages32M

export CRAYPE_LINK_TYPE=dynamic

cd ${BASE}/rap_gsi.fd
rm -fr build
mkdir build
cd ${BASE}/rap_gsi.fd/build
cmake -DENKF_MODE=WRF -DBUILD_ENKF_PREPROCESS_ARW=ON -DBUILD_GSDCLOUD_ARW=ON ../.
make -j8

cp bin/gsi.x        ${BASE}/../exec/rap_gsi
cp bin/enkf_wrf.x   ${BASE}/../exec/rap_enkf
cp bin/enspreproc.x ${BASE}/../exec/rap_process_enkf
cp bin/initialens.x ${BASE}/../exec/rap_initialens

##############################
