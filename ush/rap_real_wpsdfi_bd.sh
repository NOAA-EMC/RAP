#!/bin/ksh -l
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:        rap_real_wpsdfi_bd.sh
# Script description:  Runs REAL to get the RAP forecast input files
#
# Authors:        Ming Hu/Geoff Manikin         Date: 2011-08-24
#
# Abstract: Run REAL
#
# Script history log:
# 2011-08-24  G Manikin - new script 
# 2011-09-14  Julia Zhu -- modified from Geoff's script
# 2013-01-10  G Manikin - modified for WCOSS
#

set -x
cd $DATA

## Get the forecast valid time 
HH=$1

# Set up some constants
export FCST_LENGTH="54"
export FCST_INTERVAL="3"
export FCST_VALID=$HH
export RUNLENGTH_DFI_FWD="10"
export RUNLENGTH_DFI_BCK="20"
export SOURCE=gfs
export DATE=${DATE:-/bin/date}
export real_prefix=${real_prefix:-met_em}
export SOURCE_PATH=${SOURCE_PATH:-$DATA}
export INPUT_DATAROOT=${INPUT_DATAROOT:-$DATA}
export CONSTANTS=sst
export CONSTANT_PATH=${CONSTANT_PATH:-$DATA}

# Set the pathname of the WRF namelist
WRF_NAMELIST=namelist.input

# Default to the non-cycling version of the executable
# We will modify it later if cycling is turned on
#pgm=$EXECrap/rap_real

# Set the input format
if [ ! "${INPUT_FORMAT}" ]; then
  INPUT_FORMAT=NETCDF
fi
if [ "${INPUT_FORMAT}" == "NETCDF" ]; then
  real_suffix=".nc"
elif [ "${INPUT_FORMAT}" == "BINARY" ]; then :
  real_suffix=""
else
  echo "ERROR: Unsupported INPUT_FORMAT, '${INPUT_FORMAT}'"
  err_exit
fi

# Initialize an array of WRF DAT files that need to be linked
set -A WRF_DAT_FILES ${PARMrap}/rap_run_LANDUSE.TBL          \
                     ${PARMrap}/rap_run_RRTM_DATA            \
                     ${PARMrap}/rap_run_RRTM_DATA_DBL        \
                     ${PARMrap}/rap_run_RRTMG_LW_DATA        \
                     ${PARMrap}/rap_run_RRTMG_LW_DATA_DBL    \
                     ${PARMrap}/rap_run_RRTMG_SW_DATA        \
                     ${PARMrap}/rap_run_RRTMG_SW_DATA_DBL    \
                     ${PARMrap}/rap_run_VEGPARM.TBL          \
                     ${PARMrap}/rap_run_GENPARM.TBL          \
                     ${PARMrap}/rap_run_SOILPARM.TBL         \
                     ${PARMrap}/rap_run_MPTABLE.TBL          \
                     ${PARMrap}/rap_run_URBPARM.TBL          \
                     ${PARMrap}/rap_run_URBPARM_UZE.TBL      \
                     ${PARMrap}/rap_run_ETAMPNEW_DATA        \
                     ${PARMrap}/rap_run_ETAMPNEW_DATA.expanded_rain        \
                     ${PARMrap}/rap_run_ETAMPNEW_DATA.expanded_rain_DBL    \
                     ${PARMrap}/rap_run_ETAMPNEW_DATA_DBL    \
                     ${PARMrap}/rap_run_co2_trans            \
                     ${PARMrap}/rap_run_ozone.formatted      \
                     ${PARMrap}/rap_run_ozone_lat.formatted  \
                     ${PARMrap}/rap_run_ozone_plev.formatted \
                     ${PARMrap}/rap_run_tr49t85              \
                     ${PARMrap}/rap_run_tr49t67              \
                     ${PARMrap}/rap_run_tr67t85              \
                     ${PARMrap}/rap_run_grib2map.tbl         \
                     ${PARMrap}/rap_run_gribmap.txt          \
                     ${PARMrap}/rap_run_aerosol.formatted      \
                     ${PARMrap}/rap_run_aerosol_lat.formatted  \
                     ${PARMrap}/rap_run_aerosol_lon.formatted  \
                     ${PARMrap}/rap_run_aerosol_plev.formatted \
                     ${PARMrap}/rap_run_bulkdens.asc_s_0_03_0_9  \
                     ${PARMrap}/rap_run_bulkradii.asc_s_0_03_0_9 \
                     ${PARMrap}/rap_run_capacity.asc           \
                     ${PARMrap}/rap_run_CCN_ACTIVATE.BIN       \
                     ${PARMrap}/rap_run_coeff_p.asc            \
                     ${PARMrap}/rap_run_coeff_q.asc            \
                     ${PARMrap}/rap_run_constants.asc          \
                     ${PARMrap}/rap_run_kernels.asc_s_0_03_0_9 \
                     ${PARMrap}/rap_run_kernels_z.asc          \
                     ${PARMrap}/rap_run_masses.asc             \
                     ${PARMrap}/rap_run_termvels.asc           \
                     ${PARMrap}/rap_run_wind-turbine-1.tbl     \
                     ${PARMrap}/rap_run_freezeH2O.dat          \
                     ${PARMrap}/rap_run_qr_acr_qg.dat          \
                     ${PARMrap}/rap_run_qr_acr_qs.dat

# Check to make sure WRF DAT files exist
for file in ${WRF_DAT_FILES[@]}; do
  if [ ! -s ${file} ]; then
    echo "ERROR: ${file} either does not exist or is empty"
    err_exit
  fi
done

# Make sure START_TIME is defined and in the correct format

START_TIME=`cat STARTTIME`
START_TIME=`$NDATE +${FCST_VALID} $START_TIME`
echo $START_TIME

# Calculate the forecast end time
END_TIME=`$NDATE +${FCST_LENGTH} $START_TIME`
syyyy=`echo ${START_TIME} | cut -c1-4`
smm=`echo ${START_TIME} | cut -c5-6`
sdd=`echo ${START_TIME} | cut -c7-8`
shh=`echo ${START_TIME} | cut -c9-10`
eyyyy=`echo ${END_TIME} | cut -c1-4`
emm=`echo ${END_TIME} | cut -c5-6`
edd=`echo ${END_TIME} | cut -c7-8`
ehh=`echo ${END_TIME} | cut -c9-10`
start_yyyymmdd_hhmmss=${syyyy}-${smm}-${sdd}_${shh}:00:00
end_yyyymmdd_hhmmss=${eyyyy}-${emm}-${edd}_${ehh}:00:00

# Calculate the DFI forward end time
DFIFWD_END_TIME=`$NDATE +0 $START_TIME`

# Calculate the DFI backward end time
DFIBCK_END_TIME=`$NDATE -1 $START_TIME`

# Check to make sure the real input files (e.g. met_em.d01.*) are available
# and make links to them
fcst=0
while [ ${fcst} -le ${FCST_LENGTH} ]; do
      datestr_temp=`$NDATE +${fcst} $START_TIME`
      yyyy=`echo ${datestr_temp} | cut -c1-4`
      mm=`echo ${datestr_temp} | cut -c5-6`
      dd=`echo ${datestr_temp} | cut -c7-8`
      hh=`echo ${datestr_temp} | cut -c9-10`
      time_str=${yyyy}-${mm}-${dd}_${hh}:00:00
      time_str_real=${yyyy}-${mm}-${dd}_${hh}_00_00
  if [ ! -r "${INPUT_DATAROOT}/${real_prefix}.d01.${time_str}${real_suffix}" ]; then
    echo "ERROR: Input file '${INPUT_DATAROOT}/${real_prefix}.d01.${time_str}${real_suffix}' is missing"
    err_exit
  fi
#  rm -f ${real_prefix}.d01.${time_str}${real_suffix}
  ln -s ${INPUT_DATAROOT}/${real_prefix}.d01.${time_str}${real_suffix} ${real_prefix}.d01.${time_str_real}${real_suffix}
  (( fcst = fcst + ${FCST_INTERVAL} ))
done

#***************************************************
#*** Start of code specific to cycling           ***
#*** Will run only if $CYCLE_FCSTS is set.       ***
#*** No need to comment out, even if your        ***
#*** application does not use cycling.           ***
#***************************************************

# Look for the prepbufr file to make sure there are obs to cycle with
if [ "${CYCLE_FCSTS}" ]; then

  prepbufr_file=`${DATE} +"%Y%j%H00.rap.t%Hz.prepbufr.tm00.%Y%m%d" -d "${START_TIME}"`

  # If the prepbufr file exists, link to the most recent previous forecast
  if [ -r "${PREPBUFR}/${prepbufr_file}" ]; then

    wrfout1=wrfout_d01_`${DATE} +"%Y-%m-%d_%H:%M:%S" -d "${START_TIME}"`
    wrfout2=wrfout.d01.`${DATE} +"%Y-%m-%d_%H:%M:%S" -d "${START_TIME}"`
    rm -f ${wrfout1} ${wrfout2}
    found=0
    for fcst in ${CYCLE_FCSTS}; do

      # First look for it in INPUT_DATAROOT 
      echo -n "Looking for a previous ${fcst}hr forecast ...   "
      fcst_file1=${INPUT_DATAROOT}/`${DATE} +"%Y%m%d%H" -d "${START_TIME} ${fcst} hours ago"`/wrfprd/${wrfout1}
      fcst_file2=${INPUT_DATAROOT}/`${DATE} +"%Y%m%d%H" -d "${START_TIME} ${fcst} hours ago"`/wrfprd/${wrfout2}
      if [ -r ${fcst_file1} ]; then
        ln -s ${fcst_file1} ${wrfout2}
        echo "Found"
        found=1
        break
      elif [ -r ${fcst_file2} ]; then
        ln -s ${fcst_file2} ${wrfout2}
        echo "Found"
        found=1
        break
      else
        echo "Not found"
      fi
    done

    if [ ${found} -eq 0 ]; then
      echo "************************************************************************"
      echo "* WARNING!  Did not find a previous forecast.  Performing a COLD START *"
      echo "************************************************************************"
    else
      # Set the executable to the cycling version since we found a previous forecast
      REAL=${REAL_CYCLE}
    fi

  else
    echo "************************************************************************"
    echo "* WARNING!  Did not find a prepbufr file.  Performing a COLD START *"
    echo "************************************************************************"
  fi

fi

#***************************************
#*** End of code specific to cycling ***
#***************************************


# Make links to the WRF DAT files
for file in ${WRF_DAT_FILES[@]}; do
  tempfile=`basename ${file}`
  tempname=`echo ${tempfile} | sed s/rap_run_//`
  rm -f ${tempname}
  ln -s ${file} ${tempname}
done

# Get the start and end time components

start_year=`echo ${START_TIME} | cut -c1-4`
start_month=`echo ${START_TIME} | cut -c5-6`
start_day=`echo ${START_TIME} | cut -c7-8`
start_hour=`echo ${START_TIME} | cut -c9-10`
start_minute="00"
start_second="00"
end_year=`echo ${END_TIME} | cut -c1-4`
end_month=`echo ${END_TIME} | cut -c5-6`
end_day=`echo ${END_TIME} | cut -c7-8`
end_hour=`echo ${END_TIME} | cut -c9-10`
end_minute="00"
end_second="00"
start_YYYYMMDDHHMM=${start_year}${start_month}${start_day}${start_hour}${start_minute}

# for DFI
dfi_fwdstop_year=`echo ${DFIFWD_END_TIME} | cut -c1-4`
dfi_fwdstop_month=`echo ${DFIFWD_END_TIME} | cut -c5-6`
dfi_fwdstop_day=`echo ${DFIFWD_END_TIME} | cut -c7-8`
dfi_fwdstop_hour=`echo ${DFIFWD_END_TIME} | cut -c9-10`
dfi_fwdstop_minute=`echo ${RUNLENGTH_DFI_FWD}`
dfi_fwdstop_second="00"
dfi_bckstop_year=`echo ${DFIBCK_END_TIME} | cut -c1-4`
dfi_bckstop_month=`echo ${DFIBCK_END_TIME} | cut -c5-6`
dfi_bckstop_day=`echo ${DFIBCK_END_TIME} | cut -c7-8`
dfi_bckstop_hour=`echo ${DFIBCK_END_TIME} | cut -c9-10`
(( BCK_MIN = 60 - ${RUNLENGTH_DFI_BCK} ))
dfi_bckstop_minute=`echo ${BCK_MIN}`
dfi_bckstop_second="00"
real_fi_opt=3


# Compute number of days and hours for the run
#(( run_days = ${FCST_LENGTH} / 24 ))
#(( run_hours = ${FCST_LENGTH} % 24 ))
(( run_days = 0 ))
(( run_hours = 0 ))

# Create patterns for updating the wrf namelist
run=[Rr][Uu][Nn]
equal=[[:blank:]]*=[[:blank:]]*
start=[Ss][Tt][Aa][Rr][Tt]
end=[Ee][Nn][Dd]
year=[Yy][Ee][Aa][Rr]
month=[Mm][Oo][Nn][Tt][Hh]
day=[Dd][Aa][Yy]
hour=[Hh][Oo][Uu][Rr]
minute=[Mm][Ii][Nn][Uu][Tt][Ee]
second=[Ss][Ee][Cc][Oo][Nn][Dd]
interval=[Ii][Nn][Tt][Ee][Rr][Vv][Aa][Ll]
#for DFI
dfi=[Dd][Ff][Ii]
opt=[Oo][Pp][Tt]
fwdstop=[Ff][Ww][Dd][Ss][Tt][Oo][Pp]
bckstop=[Bb][Cc][Kk][Ss][Tt][Oo][Pp]
run00=[Rr][Uu][Nn][Ll][Ee][Nn][Gg][Tt][Hh]
fwd=[Ff][Ww][Dd]
bck=[Bb][Cc][Kk]

# Copy the wrf namelist to the workdir as namelist.input
cp ${PARMrap}/rap_wrfbc.nl ${WRF_NAMELIST}

# Update the run_days in wrf namelist.input
cat ${WRF_NAMELIST} | sed "s/\(${run}_${day}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_days}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update the run_hours in wrf namelist
cat ${WRF_NAMELIST} | sed "s/\(${run}_${hour}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${run_hours}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update the start time in wrf namelist
cat ${WRF_NAMELIST} | sed "s/\(${start}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${start_year}/" \
   | sed "s/\(${start}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${start_month}/"                   \
   | sed "s/\(${start}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${start_day}/"                       \
   | sed "s/\(${start}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${start_hour}/"                     \
   | sed "s/\(${start}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${start_minute}/"                 \
   | sed "s/\(${start}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${start_second}/"                 \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update end time in namelist
cat ${WRF_NAMELIST} | sed "s/\(${end}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${end_year}/" \
   | sed "s/\(${end}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${end_month}/"                   \
   | sed "s/\(${end}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${end_day}/"                       \
   | sed "s/\(${end}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${end_hour}/"                     \
   | sed "s/\(${end}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${end_minute}/"                 \
   | sed "s/\(${end}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${end_second}/"                 \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update DFI forward end time in namelist
cat ${WRF_NAMELIST} | sed "s/\(${dfi}_${fwdstop}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${dfi_fwdstop_year}/" \
   | sed "s/\(${dfi}_${fwdstop}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_fwdstop_month}/"     \
   | sed "s/\(${dfi}_${fwdstop}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_fwdstop_day}/"         \
   | sed "s/\(${dfi}_${fwdstop}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_fwdstop_hour}/"       \
   | sed "s/\(${dfi}_${fwdstop}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_fwdstop_minute}/"   \
   | sed "s/\(${dfi}_${fwdstop}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_fwdstop_second}/"   \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update DFI backward end time in namelist
cat ${WRF_NAMELIST} | sed "s/\(${dfi}_${bckstop}_${year}\)${equal}[[:digit:]]\{4\}/\1 = ${dfi_bckstop_year}/" \
   | sed "s/\(${dfi}_${bckstop}_${month}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_bckstop_month}/"     \
   | sed "s/\(${dfi}_${bckstop}_${day}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_bckstop_day}/"         \
   | sed "s/\(${dfi}_${bckstop}_${hour}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_bckstop_hour}/"       \
   | sed "s/\(${dfi}_${bckstop}_${minute}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_bckstop_minute}/"   \
   | sed "s/\(${dfi}_${bckstop}_${second}\)${equal}[[:digit:]]\{2\}/\1 = ${dfi_bckstop_second}/"   \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update runlenfth_dfi_fwd  in namelist
cat ${WRF_NAMELIST} | sed "s/\(${run00}_${dfi}_${fwd}\)${equal}[[:digit:]]\{1,\}/\1 = ${RUNLENGTH_DFI_FWD}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update runlenfth_dfi_bck  in namelist
cat ${WRF_NAMELIST} | sed "s/\(${run00}_${dfi}_${bck}\)${equal}[[:digit:]]\{1,\}/\1 = ${RUNLENGTH_DFI_BCK}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update dfi_opt  in namelist
cat ${WRF_NAMELIST} | sed "s/\(${dfi}_${opt}\)${equal}[[:digit:]]\{1,\}/\1 = ${real_fi_opt}/" \
   > ${WRF_NAMELIST}.new
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Update interval in namelist
(( fcst_interval_sec = ${FCST_INTERVAL} * 3600 ))
cat ${WRF_NAMELIST} | sed "s/\(${interval}${second}[Ss]\)${equal}[[:digit:]]\{1,\}/\1 = ${fcst_interval_sec}/" \
   > ${WRF_NAMELIST}.new 
mv ${WRF_NAMELIST}.new ${WRF_NAMELIST}

# Move existing rsl files to a subdir if there are any
echo "Checking for pre-existing rsl files"
if [ -f "rsl.out.0000" ]; then
  rsldir=rsl.`ls -l --time-style=+%Y%m%d%H%M%S rsl.out.0000 | ${CUT} -d" " -f 7`
  mkdir ${rsldir}
  echo "Moving pre-existing rsl files to ${rsldir}"
  mv rsl.out.* ${rsldir}
  mv rsl.error.* ${rsldir}
else
  echo "No pre-existing rsl files were found"
fi

# Get the current time
now=`${DATE} +%Y%m%d%H%M%S`

# Run real
export pgm=$EXECrap/rap_wrfarw_real
#. prep_step

cp ${EXECrap}/rap_wrfarw_real .
#runline="aprun -n 144 -N 24 ./rap_wrfarw_real"
runline="mpiexec -n 128 -ppn 128 ./rap_wrfarw_real"
$runline
export err=$?;err_chk

echo "real_wps.ksh completed successfully at `${DATE}`"

# Save a copy of the RSL files
rsldir=rsl.real.${now}
mkdir ${rsldir}
mv rsl.out.* ${rsldir}
mv rsl.error.* ${rsldir}

bdyfile=${RAPBC}/wrfbdy_d01.${start_YYYYMMDDHHMM}
echo "${bdyfile}"
cpfs wrfbdy_d01 ${bdyfile}
cp wrfbdy_d01 wrfbdy_d01.${start_YYYYMMDDHHMM}
outputfile=${RAPBC}/wrfinput_d01_${start_year}-${start_month}-${start_day}_${start_hour}_00_00
echo "${outputfile}"
cpfs wrfinput_d01 ${outputfile}
cp wrfinput_d01 wrfinput_d01_${start_year}-${start_month}-${start_day}_${start_hour}_00_00

if [ ! -r "wrfinput_d01" ]; then
  echo "ERROR: wrfinput_d01 does not exist!"
  err_exit
fi

mkdir tar
cp wrfbdy_d01.* tar/.
cp wrfinput_d01_* tar/.
