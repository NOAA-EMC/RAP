set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_rap_wrfarw_serial=no
export BUILD_rap_update_bc=no
export BUILD_rap_wps=no
export BUILD_rap_wrfpost=no
export BUILD_rap_wrfarw=no
export BUILD_rap_gsi=no
export BUILD_rap_process_enkf=no
export BUILD_rap_full_cycle_surface=no
export BUILD_rap_process_cloud=no
export BUILD_rap_process_imssnow=no
export BUILD_rap_process_mosaic=no
export BUILD_rap_process_sst=no
export BUILD_rap_process_lightning=no
export BUILD_rap_update_gvf=no
export BUILD_rap_update_fields=no
export BUILD_rap_sndp=no
export BUILD_rap_subflds_g2=no
export BUILD_rap_wrfbufr=no
export BUILD_rap_stnmlist=no
export BUILD_rap_smartinit=yes

 . /opt/modules/default/init/ksh

module purge
module load ncep
module load craype-sandybridge
module use -a /opt/cray/modulefiles

module load $BASE/../modulefiles/RAP/v4.0.0
module unload PNetCDF-intel-sandybridge/1.5.0

module list

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1

##############################

if [ $BUILD_rap_wrfarw_serial = yes ] ; then

echo " .... Building rap_wrfarw_serial .... "
$BASE/build_rap_wrfarw_serial.sh > $logs_dir/build_rap_wrfarw_serial.log 2>&1

fi

##############################

if [ $BUILD_rap_update_bc = yes ] ; then

echo " .... Building rap_update_bc .... "
$BASE/build_rap_update_bc.sh > $logs_dir/build_rap_update_bc.log 2>&1

fi

##############################

if [ $BUILD_rap_wps = yes ] ; then

echo " .... Building rap_wps .... "
$BASE/build_rap_wps.sh > $logs_dir/build_rap_wps.log 2>&1

fi

##############################

if [ $BUILD_rap_wrfpost = yes ] ; then

echo " .... Building rap_wrfpost .... "
$BASE/build_rap_wrfpost.sh > $logs_dir/build_rap_wrfpost.log 2>&1

fi

##############################

if [ $BUILD_rap_wrfarw = yes ] ; then

echo " .... Building rap_wrfarw .... "

$BASE/build_rap_wrfarw.sh > $logs_dir/build_rap_wrfarw.log 2>&1

fi

##############################

if [ $BUILD_rap_gsi = yes ] ; then

echo " .... Building rap_gsi .... "

$BASE/build_rap_gsi.sh > $logs_dir/build_rap_gsi.log 2>&1

fi

##############################

if [ $BUILD_rap_process_enkf = yes ] ; then

echo " .... Building rap_process_enkf .... "
$BASE/build_rap_process_enkf.sh > $logs_dir/build_rap_process_enkf.log 2>&1

fi

##############################

if [ $BUILD_rap_full_cycle_surface = yes ] ; then

echo " .... Building rap_full_cycle_surface .... "
$BASE/build_rap_full_cycle_surface.sh > $logs_dir/build_full_cycle_surface.log 2>&1

fi

##############################

if [ $BUILD_rap_process_cloud = yes ] ; then

echo " .... Building rap_process_cloud .... "
$BASE/build_rap_process_cloud.sh > $logs_dir/build_process_cloud.log 2>&1

fi

##############################

if [ $BUILD_rap_process_imssnow = yes ] ; then

echo " .... Building rap_process_imssnow .... "
$BASE/build_rap_process_imssnow.sh > $logs_dir/build_process_imssnow.log 2>&1

fi

##############################

if [ $BUILD_rap_process_mosaic = yes ] ; then

echo " .... Building rap_process_mosaic .... "
$BASE/build_rap_process_mosaic.sh > $logs_dir/build_process_mosaic.log 2>&1

fi

##############################

if [ $BUILD_rap_process_sst = yes ] ; then

echo " .... Building rap_process_sst .... "
$BASE/build_rap_process_sst.sh > $logs_dir/build_process_sst.log 2>&1

fi

##############################

if [ $BUILD_rap_process_lightning = yes ] ; then

echo " .... Building rap_process_lightning .... "
$BASE/build_rap_process_lightning.sh > $logs_dir/build_process_lightning.log 2>&1

fi

##############################

if [ $BUILD_rap_update_fields = yes ] ; then

echo " .... Building rap_update_fields .... "
$BASE/build_rap_update_fields.sh > $logs_dir/build_update_fields.log 2>&1

fi

##############################

if [ $BUILD_rap_update_gvf = yes ] ; then

echo " .... Building rap_update_gvf .... "
$BASE/build_rap_update_gvf.sh > $logs_dir/build_update_gvf.log 2>&1

fi

##############################

if [ $BUILD_rap_subflds_g2 = yes ] ; then

echo " .... Building rap_subflds_g2 .... "
$BASE/build_rap_subflds_g2.sh > $logs_dir/build_subflds_g2.log 2>&1

fi

##############################

if [ $BUILD_rap_sndp = yes ] ; then

echo " .... Building rap_sndp .... "
$BASE/build_rap_sndp.sh > $logs_dir/build_sndp.log 2>&1

fi

##############################

if [ $BUILD_rap_wrfbufr = yes ] ; then

echo " .... Building rap_wrfbufr .... "
$BASE/build_rap_wrfbufr.sh > $logs_dir/build_wrfbufr.log 2>&1

fi

##############################

if [ $BUILD_rap_stnmlist = yes ] ; then

echo " .... Building rap_stnmlist .... "
$BASE/build_rap_stnmlist.sh > $logs_dir/build_stnmlist.log 2>&1

fi

##############################

if [ $BUILD_rap_smartinit = yes ] ; then

echo " .... Building rap_smartinit .... "
$BASE/build_rap_smartinit.sh > $logs_dir/build_smartinit.log 2>&1

fi

##############################

cd $BASE
