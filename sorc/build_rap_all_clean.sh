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


source $BASE/build_rap_module_load.sh

mkdir $BASE/logs
export logs_dir=$BASE/logs

sleep 1

cd ${BASE}
for exec in rap_stnmlist rap_subflds_g2 rap_wrfbufr rap_wrfpost; do
 mv ../exec/${exec}  ../exec/${exec}.org
done

cd $BASE
cd ${BASE}/rap_full_cycle_surface.fd
make clean

cd ${BASE}/rap_prep_smoke.fd/process-obs/QC
make clean

cd ${BASE}/rap_prep_smoke.fd/prep-chem/Prep_smoke_FRP/bin/build
make clean

cd ${BASE}/rap_prep_smoke.fd/process-obs/RAP-Smoke/src
make clean

cd ${BASE}/rap_process_cloud.fd
make clean

cd ${BASE}/rap_process_enkf.fd
make clean

cd ${BASE}/rap_process_imssnow.fd
make clean

cd ${BASE}/rap_process_lightning.fd
make clean

cd ${BASE}/rap_process_mosaic.fd
make clean

cd ${BASE}/rap_process_sst.fd
make clean

cd ${BASE}/rap_smartinit.fd
make clean

cd ${BASE}/rap_sndp.fd
make clean

cd ${BASE}/rap_stnmlist.fd
make clean

cd ${BASE}/rap_subflds_g2.fd
make clean

cd ${BASE}/rap_update_bc.fd
make clean

cd ${BASE}/rap_update_fields.fd
make clean

cd ${BASE}/rap_update_gvf.fd
make clean

cd ${BASE}/rap_wrfbufr.fd
make clean

cd ${BASE}/rap_wrfpost.fd
make clean

cd ${BASE}/rap_wrfarw.fd/WRFV3.9
./clean -aa
./clean -a
./clean

cd ${BASE}/rap_wps.fd/WPSV3.9.1
./clean -aa
./clean -a
./clean

cd ${BASE}/rap_wrfbufr.fd
make clean

cd ${BASE}
for exec in rap_stnmlist rap_subflds_g2 rap_wrfbufr rap_wrfpost; do
 mv ../exec/${exec}.org  ../exec/${exec}
done
