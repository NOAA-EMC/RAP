##############################

export BASE=`pwd`
cd $BASE

 . /opt/modules/default/init/ksh
module purge
module load intel
module load NetCDF-intel-haswell/4.2
module load HDF5-serial-intel-haswell/1.8.9
module list

cd ${BASE}/rap_prep_smoke.fd/process-obs/QC
make clean
make 
cp -fp qc_modis.exe ${BASE}/../exec/rap_smoke_qc_modis
cp -fp qc_viirs.exe ${BASE}/../exec/rap_smoke_qc_viirs

cd ${BASE}/rap_prep_smoke.fd/prep-chem/cycle_netcdf
./mk-wrf-wcoss-cray
cp -fp "cycle_netcdf.x" ${BASE}/../exec/rap_fires_cycle_netcdf

cd ${BASE}/rap_prep_smoke.fd/prep-chem/fires_ncfmake
./mk-wrf-wcoss-cray
cp -fp "fires_ncfmake.x" ${BASE}/../exec/rap_fires_ncfmake

cd ${BASE}/rap_prep_smoke.fd/prep-chem/Prep_smoke_FRP/bin/build
make clean
./mk-wrf-wcoss
cp -fp ../prep_chem_sources_RADM_WRF_FIM_.exe ${BASE}/../exec/rap_prep_chem_sources

cd ${BASE}/rap_prep_smoke.fd/process-obs/RAP-Smoke/src
make clean
make
cp -fp FRE_BBM_RAP_v4.exe "${BASE}/../exec/rap_smoke_fre_bbm"
cp -fp merge_FRP_RAP_v3.exe "${BASE}/../exec/rap_smoke_merge_frp"
cp -fp proc_J01_FRP_RAP_v3.exe "${BASE}/../exec/rap_smoke_proc_j01_frp"
cp -fp proc_MODIS_FRP_RAP_v3.exe "${BASE}/../exec/rap_smoke_proc_modis_frp"
cp -fp proc_NPP_FRP_RAP_v3.exe "${BASE}/../exec/rap_smoke_proc_npp_frp"

##############################
