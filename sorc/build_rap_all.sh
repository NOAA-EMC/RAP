set -x

export BASE=`pwd`
cd $BASE

#build switches

export BUILD_rap_wrfarw_serial=yes
export BUILD_rap_wps=yes
export BUILD_rap_update_bc=yes
export BUILD_rap_wrfpost=yes
export BUILD_rap_wrfarw=yes
export BUILD_rap_gsi=yes
export BUILD_rap_prep_smoke=yes

export BUILD_rap_full_cycle_surface=yes
export BUILD_rap_process_cloud=yes
export BUILD_rap_process_imssnow=yes
export BUILD_rap_process_mosaic=yes
export BUILD_rap_process_sst=yes
export BUILD_rap_process_lightning=yes
export BUILD_rap_update_gvf=yes
export BUILD_rap_update_fields=yes

export BUILD_rap_sndp=yes
export BUILD_rap_subflds_g2=yes
export BUILD_rap_wrfbufr=yes
export BUILD_rap_stnmlist=yes
export BUILD_rap_smartinit=yes


module purge
module load envvar/1.0

module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/v5.0.0

module list

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1

##############################

if [ $BUILD_rap_prep_smoke = yes ] ; then

echo " .... Building rap_prep_smoke .... "
$BASE/build_rap_prep_smoke.sh > $logs_dir/build_rap_prep_smoke.log 2>&1

fi

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
