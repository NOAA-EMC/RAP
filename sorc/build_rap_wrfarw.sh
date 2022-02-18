set -x

##############################

export BASE=`pwd`
cd $BASE

source $BASE/build_rap_module_load.sh

cd ${BASE}/rap_wrfarw.fd


./wcoss_doit.sh configure
./wcoss_doit.sh compile

cp WRFV3.9/main/real.exe ${BASE}/../exec/rap_wrfarw_real
cp WRFV3.9/main/wrf.exe ${BASE}/../exec/rap_wrfarw_fcst

##############################
