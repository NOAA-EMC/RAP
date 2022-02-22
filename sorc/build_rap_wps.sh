set -x

##############################

export BASE=`pwd`

source $BASE/build_rap_module_load.sh

cd ${BASE}/rap_wps.fd/WPSV3.9.1
./clean -aa
./clean -a
./clean
cp configure.wps.optim configure.wps
./compile
cp ungrib/src/ungrib.exe ${BASE}/../exec/rap_wps_ungrib
cp metgrid/src/metgrid.exe ${BASE}/../exec/rap_wps_metgrid

##############################
