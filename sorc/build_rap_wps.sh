set -x

##############################

export BASE=`pwd`

module purge
module load envvar/1.0
module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0
module list

cd ${BASE}/rap_wps.fd/WPSV3.9.1
./clean -aa
./clean -a
./clean
cp configure.wps.optim configure.wps
./compile
cp ungrib/src/ungrib.exe ${BASE}/../exec/rap_wps_ungrib
cp metgrid/src/metgrid.exe ${BASE}/../exec/rap_wps_metgrid

##############################
