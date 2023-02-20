#!/bin/ksh -l
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:       exrap_analysis_pcyc.sh.ecf
# Script description:  runs the GSI and boundary update for the RAP partial cycle
#
# Author:   Ming Hu / Geoff Manikin / Julia Zhu  Org: EMC        Date: 2011-10-01
#
# Script history log:
# 2011-10-01  M Hu / G Manikin / J Zhu
# 2018-01-25  B Blake / C Guastini / G Manikin / C Alexander - RAPv4


set -x

cd $DATA

export WPSNAMELIST=namelist.wps
START_TIME=$PDY$cyc
echo $START_TIME >STARTTIME

ECHO="${ECHO:-/bin/echo}"
#export ATP_ENABLED=0
#export MALLOC_MMAP_MAX=0
#export MALLOC_TRIM_THRESHOLD=134217728
#export OMP_STACKSIZE=500M
#export KMP_STACKSIZE=1024m
#export MP_COLLECTIVE_OFFLOAD=no

# Compute date & time components for the analysis time
YYYY=`echo ${START_TIME} | cut -c1-4`
MM=`echo ${START_TIME} | cut -c5-6`
DD=`echo ${START_TIME} | cut -c7-8`

# Create the ram work directory and cd into it
workdir=$DATA/gsiprd
rm -rf ${workdir}
mkdir -p ${workdir}
cd ${workdir}

logfile=gsi.out

# Bring over background field (it's modified by GSI so we can't link to it)
time_str=${YYYY}-${MM}-${DD}_${cyc}_00_00
echo " time_str = ${time_str}"

#  check to see if a first guess is available in the pcyc guess directory 
PCYC_TIME=`$NDATE -1 $START_TIME`
if [ -r ${RAPGES_PCYC}/rap_${PCYC_TIME}f001 ]; then
   cp ${RAPGES_PCYC}/rap_${PCYC_TIME}f001 ./wrf_inout
   echo " Cycle ${PDY}${cyc}: GSI partially cycled start with rap_${PCYC_TIME}f001"
fi

# attempt cold start if no guess has been found;  this option will be used
#   at the start of the partial cycle (03 and 15z);   note that if there is
#   no pcyc guess available at a cycle time not divisible by 3, there isn't
#   anything that can be done.   The best hope is to restart the partial cycle
#   with a cold start at 06 or 18z

YYYYMMDDHH=${YYYY}${MM}${DD}${cyc}
if [ ! -r wrf_inout ]; then
   if [ -r "${RAPBC}/wrfinput_d01_${time_str}" ]; then
      echo "Cold Start with ${RAPBC}/wrfinput_d01_${time_str} "
      cp ${RAPBC}/wrfinput_d01_${time_str} ./wrf_inout
      echo " Cycle ${PDY}${cyc}: GSI cold start with wrfout_d01_${time_str}"

## cycle surface variables
      targetsize=6858968708 # rapv5 size
      targetsize2=6871551620  # rapv5 size working dir on Dell2
      targetsize3=6854865028  # rapv5 size working dir on WCOSS2
      counter=1
         while [[ $counter -lt 22 ]]; do
         counterhr=$counter
         typeset -Z2 counterhr
         FCYC_TIME=`$NDATE -${counter} $START_TIME`
         if [ -r ${RAPGES_FCYC}/rap_${FCYC_TIME}f0$counterhr ]; then
            filesize=$(stat -c%s ${RAPGES_FCYC}/rap_${FCYC_TIME}f0$counterhr)
            echo $filesize
#            if [ $filesize -eq $targetsize ]; then 
            if [[ $filesize -eq $targetsize || $filesize -eq $targetsize2 ]] || [ $filesize -eq $targetsize3 ]; then
               echo "cycle surface fields based on ${RAPGES_FCYC}/rap_${FCYC_TIME}f0$counterhr"
               cp ${RAPGES_FCYC}/rap_${FCYC_TIME}f0$counterhr ./wrfout_d01_save
               break
            fi
         fi
         counter=`expr $counter + 1 `
      done
     
      if [[ $counter -eq 22 ]]; then
         echo "WARNING: no surface data cycled for background at ${time_str}!"
      fi
      cp ${EXECrap}/rap_full_cycle_surface .
      runline="mpiexec -n 1 -ppn 1 ./rap_full_cycle_surface"
      $runline >> $DATA/$pgmout 2>errfile
      export err=$?; err_chk

# check consistence of the surface parameters
      cp ${FIXrap}/rap_geo_em.d01.nc  ./geo_em.d01.nc
      cp ${EXECrap}/rap_surface_consistcheck .
      runline="mpiexec -n 1 -ppn 1 ./rap_surface_consistcheck"
      $runline >> $DATA/$pgmout 2>errfile
     # export err=$?; err_chk
   else
      echo "${RAPBC}/wrfinput_d01_${time_str} does not exist!!"
      echo "FATAL ERROR: no background file for analaysis at ${time_str}!!!!"
      echo " Cycle ${PDY}${cyc}: GSI failed because of no background"
      err_exit
   fi
fi

# Update GVF with real-time data
if [[ ${cyc} -eq '03' || ${cyc} -eq '15' ]]; then
latestGVF=`ls ${GVF}/GVF-WKL-GLB_v3r0_npp_s*_e${PDYm1}_c${PDY}*.grib2`
latestGVF2=`ls ${GVF}/GVF-WKL-GLB_v3r0_npp_s*_e${PDYm2}_c${PDYm1}*.grib2`
if [ -r "${latestGVF}" ]; then
   cp ${EXECrap}/rap_update_gvf .
   cp ${FIXrap}/rap_gvf_VIIRS_4KM.MIN.1gd4r.new gvf_VIIRS_4KM.MIN.1gd4r.new
   cp ${FIXrap}/rap_gvf_VIIRS_4KM.MAX.1gd4r.new gvf_VIIRS_4KM.MAX.1gd4r.new
   cp ${latestGVF} GVF-WKL-GLB.grib2
   runline="mpiexec -n 1 -ppn 1 ./rap_update_gvf"
   $runline >> update_gvf.out
elif [ -r "${latestGVF2}" ]; then
   cp ${EXECrap}/rap_update_gvf .
   cp ${FIXrap}/rap_gvf_VIIRS_4KM.MIN.1gd4r.new gvf_VIIRS_4KM.MIN.1gd4r.new
   cp ${FIXrap}/rap_gvf_VIIRS_4KM.MAX.1gd4r.new gvf_VIIRS_4KM.MAX.1gd4r.new
   cp ${latestGVF2} GVF-WKL-GLB.grib2
   runline="mpiexec -n 1 -ppn 1 ./rap_update_gvf"
   $runline >> update_gvf.out
else
   ${ECHO} "${GVF}/latest.GVF does not exist!!"
   ${ECHO} "Warning: No GVF real-time data available for background at ${PDY}${cyc}!!!!"
fi
fi

# Copy cycled satellite radiance bias correction
  satcounter=1
  if [[ ${cyc} -eq '03' || ${cyc} -eq '15' ]]; then
    partial_or_fullrap=rap
    partial_or_fullout=out
  else
    partial_or_fullrap=rap_p
    partial_or_fullout=out_p
  fi
  while [[ $satcounter -lt 22 ]]; do
    SAT_TIME=`$NDATE -${satcounter} $START_TIME`
    if [[ -r ${RAPGES_SATBIAS}/radstat.${partial_or_fullrap}_${SAT_TIME} && -r ${RAPGES_SATBIAS}/satbias_${partial_or_fullout}_${SAT_TIME} && -r ${RAPGES_SATBIAS}/satbias_pc.${partial_or_fullout}_${SAT_TIME} ]]; then
      echo " using satellite bias from ${SAT_TIME}"
      cp ${RAPGES_SATBIAS}/radstat.${partial_or_fullrap}_${SAT_TIME} ./radstat.rap
      cp ${RAPGES_SATBIAS}/satbias_${partial_or_fullout}_${SAT_TIME} ./satbias_in
      cp ${RAPGES_SATBIAS}/satbias_pc.${partial_or_fullout}_${SAT_TIME} ./satbias_pc
      break
    fi
    satcounter=` expr $satcounter + 1 `
  done

# prepbufr section
ln -s ${PREPDIR}/${RUN}.${PDY}/${RUN}.t${cyc}z.prepbufr.tm00 ./prepbufr

if [ -r "${PREPDIR2}/rap.${PDY}/rap.t${cyc}z.mosaic.bufr" ]; then
  ln -s ${PREPDIR2}/rap.${PDY}/rap.t${cyc}z.mosaic.bufr ./refInGSI
else
  echo "Warning: Processed Mosaic data not available"
fi

if [ -r "${PREPDIR2}/rap.${PDY}/rap.t${cyc}z.nasacloud.bufr" ]; then
  ln -s ${PREPDIR2}/rap.${PDY}/rap.t${cyc}z.nasacloud.bufr ./larcInGSI
else
  echo "Warning: Processed Cloud data not available"
fi

if [ -r "${PREPDIR2}/rap.${PDY}/rap.t${cyc}z.lghtng.bufr" ]; then
  ln -s ${PREPDIR2}/rap.${PDY}/rap.t${cyc}z.lghtng.bufr ./lghtInGSI
else
  echo "Warning: Processed Lightning data not available"
fi

#add radial velocity data
#if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.radwnd.tm00.bufr_d" ]; then
#  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.radwnd.tm00.bufr_d ./radarbufr
#else
#  echo "Warning: ${PREPDIR}/rap.${PDY}/rap_p.t${cyc}z.radwnd.tm00.bufr_d is not available"
#fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.nexrad.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.nexrad.tm00.bufr_d ./l2rwbufr
else
  echo "Warning: ${PREPDIR}/rap.${PDY}/rap_p.t${cyc}z.nexrad.tm00.bufr_d is not available"
fi

#add SATWND data
if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.satwnd.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.satwnd.tm00.bufr_d ./satwndbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.satwnd.tm00.bufr_d is not available"
fi

#
if [ -r "${PREPDIR}/${RUN}.${PDY}/rap_p.t${cyc}z.1bamua.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bamua.tm00.bufr_d ./amsuabufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bamua.tm00.bufr_d is not available
"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bamub.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bamub.tm00.bufr_d ./amsubbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bamub.tm00.bufr_d is not available
"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bhrs3.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bhrs3.tm00.bufr_d ./hirs3bufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bhrs3.tm00.bufr_d is not available
"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bhrs4.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bhrs4.tm00.bufr_d ./hirs4bufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bhrs4.tm00.bufr_d is not available
"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bmhs.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bmhs.tm00.bufr_d ./mhsbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.1bmhs.tm00.bufr_d is not available"
fi

#new platforms
if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.sevcsr.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.sevcsr.tm00.bufr_d ./seviribufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.sevcsr.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.atms.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.atms.tm00.bufr_d ./atmsbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.atms.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.cris.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.cris.tm00.bufr_d ./crisbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.atms.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.airsev.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.airsev.tm00.bufr_d ./airsbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.airsev.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.ssmisu.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.ssmisu.tm00.bufr_d ./ssmisbufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.ssmisu.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.mtiasi.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.mtiasi.tm00.bufr_d ./iasibufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.mtiasi.tm00.bufr_d is not available"
fi

#add goes sounder data
if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.goesnd.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.goesnd.tm00.bufr_d ./gsnd1bufr
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.goesnd.tm00.bufr_d is not available"
fi

#add rars data
if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esamua.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esamua.tm00.bufr_d ./amsuabufrears
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esamua.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esmhs.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esmhs.tm00.bufr_d ./mhsbufrears
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esmhs.tm00.bufr_d is not available"
fi

#new rars
if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esatms.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esatms.tm00.bufr_d ./atmsbufrears
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esatms.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.escrsf.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.escrsf.tm00.bufr_d ./crisfsbufrears
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.escrsf.tm00.bufr_d is not available"
fi

if [ -r "${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esiasi.tm00.bufr_d" ]; then
  ln -s ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esiasi.tm00.bufr_d ./iasibufrears
else
  echo "Warning: ${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.esiasi.tm00.bufr_d is not available"
fi

#new directbroadcast
fileobs=${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.atmsdb.tm00.bufr_d
if [ -r "${fileobs}" ]; then
  ln -s ${fileobs} ./atmsbufr_db
else
  echo "Warning: ${fileobs} is not available"
fi

fileobs=${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.crsfdb.tm00.bufr_d
if [ -r "${fileobs}" ]; then
  ln -s ${fileobs} ./crisfsbufr_db
else
  echo "Warning: ${fileobs} is not available"
fi

fileobs=${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.iasidb.tm00.bufr_d
if [ -r "${fileobs}" ]; then
  ln -s ${fileobs} ./iasibufr_db
else
  echo "Warning: ${fileobs} is not available"
fi

##for rapv5 updates   
fileobs=${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.crisf4.tm00.bufr_d
if [ -r "${fileobs}" ]; then
  ln -s ${fileobs} ./crisfsbufr
else
  echo "Warning: ${fileobs} is not available"
fi

fileobs=${PREPDIR}/rap_p.${PDY}/rap_p.t${cyc}z.gsrcsr.tm00.bufr_d
if [ -r "${fileobs}" ]; then
  ln -s ${fileobs} ./abibufr   
else
  echo "Warning: ${fileobs} is not available"
fi

# Set fixed files
#   berror   = forecast model background error statistics
#   specoef  = CRTM spectral coefficients
#   trncoef  = CRTM transmittance coefficients
#   emiscoef = CRTM coefficients for IR sea surface emissivity model
#   aerocoef = CRTM coefficients for aerosol effects
#   cldcoef  = CRTM coefficients for cloud effects
#   satinfo  = text file with information about assimilation of brightness temperatures
#   satangl  = angle dependent bias correction file (fixed in time)
#   pcpinfo  = text file with information about assimilation of prepcipitation rates
#   ozinfo   = text file with information about assimilation of ozone data
#   errtable = text file with obs error for conventional data (regional only)
#   convinfo = text file with information about assimilation of conventional data
#   bufrtable= text file ONLY needed for single obs test (oneobstest=.true.)
#   bftab_sst= bufr table for sst ONLY needed for sst retrieval (retrieval=.true.)

anavinfo=${FIXrap}/rap_anavinfo_arw_netcdf
BERROR=${FIXrap}/rap_berror_stats_global
SATANGL=${FIXrap}/rap_global_satangbias.txt
SATINFO=${FIXrap}/rap_global_satinfo.txt

RTMAERO=${FIXcrtm}/AerosolCoeff.bin
RTMCLDS=${FIXcrtm}/CloudCoeff.bin
RTMEMIS_IRwater=${FIXcrtm}/Nalli.IRwater.EmisCoeff.bin
RTMEMIS_IRice=${FIXcrtm}/NPOESS.IRice.EmisCoeff.bin
RTMEMIS_IRland=${FIXcrtm}/NPOESS.IRland.EmisCoeff.bin
RTMEMIS_IRsnow=${FIXcrtm}/NPOESS.IRsnow.EmisCoeff.bin
RTMEMIS_VISice=${FIXcrtm}/NPOESS.VISice.EmisCoeff.bin
RTMEMIS_VISland=${FIXcrtm}/NPOESS.VISland.EmisCoeff.bin
RTMEMIS_VISsnow=${FIXcrtm}/NPOESS.VISsnow.EmisCoeff.bin
RTMEMIS_VISwater=${FIXcrtm}/NPOESS.VISwater.EmisCoeff.bin
RTMEMIS_MWwater=${FIXcrtm}/FASTEM6.MWwater.EmisCoeff.bin

CONVINFO=${FIXrap}/rap_nam_regional_convinfo
OZINFO=${FIXrap}/rap_global_ozinfo.txt    
PCPINFO=${FIXrap}/rap_global_pcpinfo.txt
ATMS_BEAMWIDTH=${FIXrap}/rap_atms_beamwidth.txt

OBERROR=${FIXrap}/rap_nam_errtable.r3dv
AIRCRAFTREJECTLIST=${FIXrap}/rap_current_bad_aircraft.txt   # aircraft reject list
SURFACEUSELIST=${FIXrap}/rap_current_mesonet_uselist.txt # mesonet use list
SURFACEPROVIDERLIST=${FIXrap}/rap_gsd_sfcobs_provider.txt # mesonet network use list

# Fixed fields
 cp $anavinfo anavinfo
 cp $BERROR   berror_stats
# cp $SATANGL  satbias_angle
 cp $SATINFO  satinfo
 cp $CONVINFO convinfo
 cp $OZINFO   ozinfo
 cp $PCPINFO  pcpinfo
 cp $OBERROR  errtable
 cp $AIRCRAFTREJECTLIST current_bad_aircraft
 cp $SURFACEUSELIST gsd_sfcobs_uselist.txt
 cp $SURFACEPROVIDERLIST gsd_sfcobs_provider.txt
 cp $ATMS_BEAMWIDTH atms_beamwidth.txt

# 
#    # CRTM Spectral and Transmittance coefficients
ln -s $RTMAERO  ./AerosolCoeff.bin
ln -s $RTMCLDS  ./CloudCoeff.bin
ln -s $RTMEMIS_IRwater ./Nalli.IRwater.EmisCoeff.bin
ln -s $RTMEMIS_IRice ./NPOESS.IRice.EmisCoeff.bin
ln -s $RTMEMIS_IRsnow ./NPOESS.IRsnow.EmisCoeff.bin
ln -s $RTMEMIS_IRland ./NPOESS.IRland.EmisCoeff.bin
ln -s $RTMEMIS_VISice ./NPOESS.VISice.EmisCoeff.bin
ln -s $RTMEMIS_VISland ./NPOESS.VISland.EmisCoeff.bin
ln -s $RTMEMIS_VISsnow ./NPOESS.VISsnow.EmisCoeff.bin
ln -s $RTMEMIS_VISwater ./NPOESS.VISwater.EmisCoeff.bin
ln -s $RTMEMIS_MWwater ./FASTEM6.MWwater.EmisCoeff.bin

# Copy CRTM coefficient files based on entries in satinfo file
for file in `awk '{if($1!~"!"){print $1}}' ./satinfo | sort | uniq` ;do
   ln -s ${FIXcrtm}/${file}.SpcCoeff.bin ./
   ln -s ${FIXcrtm}/${file}.TauCoeff.bin ./
done

# Only need this file for single obs test
 bufrtable=${PARMrap}/rap_prepobs.bufrtable
 cp $bufrtable ./prepobs_prep.bufrtable

## 
## Find closest GFS EnKF forecast to analysis time
##
CYCSTART_TIME=`echo "${START_TIME}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`

stampcycle=`date -d "${CYCSTART_TIME}" +%s`
minHourDiff=100
loops="009"
for loop in $loops; do
  for timelist in `ls ${COMINgfs}/enkfgdas.*/*/atmos/mem080/gdas*.atmf${loop}.nc`; do
    availtimeyyyymmdd=`echo ${timelist} | cut -d'/' -f9 | cut -c 10-17`
    availtimehh=`echo ${timelist} | cut -d'/' -f10`
    availtime=${availtimeyyyymmdd}${availtimehh}
    AVAIL_TIME=`echo "${availtime}" | sed 's/\([[:digit:]]\{2\}\)$/ \1/'`
    AVAIL_TIME=`date -d "${AVAIL_TIME}"`

    stamp_avail=`date -d "${AVAIL_TIME} ${loop} hours" +%s`

    hourDiff=`echo "($stampcycle - $stamp_avail) / (60 * 60 )" | bc`;
    if [[ ${stampcycle} -lt ${stamp_avail} ]]; then
       hourDiff=`echo "($stamp_avail - $stampcycle) / (60 * 60 )" | bc`;
    fi

    if [[ ${hourDiff} -lt ${minHourDiff} ]]; then
       minHourDiff=${hourDiff}
       enkfcstname=gdas.t${availtimehh}z.atmf${loop}
       EYYYYMMDD=$(echo ${availtime} | cut -c1-8)
       EHH=$(echo ${availtime} | cut -c9-10)
    fi
  done
done
ls ${COMINgfs}/enkfgdas.${EYYYYMMDD}/${EHH}/atmos/mem???/${enkfcstname}.nc > filelist03

## 
# Determine if hybrid option is available
beta1_inv=1.0
ifhyb=.false.
nummem=`more filelist03 | wc -l`
nummem=$((nummem - 3 ))
if [[ ${nummem} -eq 80 ]]; then
  echo "Do hybrid with ${memname}"
  beta1_inv=0.15
  ifhyb=.true.
  echo " Cycle ${PDY}${cyc}: GSI hybrid uses ${memname} with n_ens=${nummem}"
fi

# Set some parameters for use by the GSI executable and to build the namelist
export JCAP=62
export LEVS=60
export DELTIM=${DELTIM:-$((3600/($JCAP/20)))}
ndatrap=62
grid_ratio=2
cloudanalysistype=5
ifsoilnudge=.true.

# Build the GSI namelist on-the-fly
. ${PARMrap}/rap_gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF

# cp ${FIXrap}/rap_ndas.t06z.satbias.tm03 ./satbias_in
## satellite bias correction
if [ -r radstat.rap ]; then
  listdiag=`tar xvf radstat.rap | cut -d' ' -f2 | grep _ges`
  for type in $listdiag; do
       diag_file=`echo $type | cut -d',' -f1`
       fname=`echo $diag_file | cut -d'.' -f1`
       date=`echo $diag_file | cut -d'.' -f2`
       gunzip $diag_file
       fnameanl=$(echo $fname|sed 's/_ges//g')
       mv $fname.$date $fnameanl
  done
  mv radstat.rap radstat.rap.for_this_cycle
fi

export pgm=rap_gsi
. prep_step

startmsg
cp ${EXECrap}/rap_gsi .
runline="mpiexec -n 192 -ppn 64 --cpu-bind core --depth 2 ./rap_gsi"
$runline < gsiparm.anl >> $DATA/$pgmout 2>errfile
export err=$?; err_chk

# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

loops="01 03"
for loop in $loops; do

case $loop in
  01) string=ges;;
  03) string=anl;;
   *) string=$loop;;
esac

#  Collect diagnostic files for obs types (groups) below
   listall="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsua_n18 amsua_n19 amsua_metop-a amsua_metop-b amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 pcp_ssmi_dmsp pcp_tmi_trmm conv sbuv2_n16 sbuv2_n17 sbuv2_n18 omi_aura ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a mhs_n18 mhs_n19 mhs_metop-a mhs_metop-b amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 iasi_metop-a iasi_metop-b seviri_m08 seviri_m09 seviri_m10 seviri_m11 cris_npp atms_npp ssmis_f17 cris-fsr_npp cris-fsr_n20 atms_n20 abi_g16"
   for type in $listall; do
      count=`ls pe*.${type}_${loop}* | wc -l`
      if [[ $count -gt 0 ]]; then
         `cat pe*.${type}_${loop}* > diag_${type}_${string}.${PDY}${cyc}`
      fi
   done
done

##tar diag files for next cycle run for enhanced radiance bias correction 
tar -cvzf radstat.rap *diag*ges*.${PDY}${cyc}
mv ./radstat.rap radstat.rap_p_${PDY}${cyc}
cp ./radstat.rap_p_${PDY}${cyc} ${RAPGES_SATBIAS}/

mv ./satbias_out satbias_out_p_${PDY}${cyc}
cp ./satbias_out_p_${PDY}${cyc} ${RAPGES_SATBIAS}/

mv ./satbias_pc.out satbias_pc.out_p_${PDY}${cyc}
cp ./satbias_pc.out_p_${PDY}${cyc} ${RAPGES_SATBIAS}/

export err=$?; err_chk

cp gsiparm.anl gsiparm.anl_var
mv fort.201 fit_p1
mv fort.202 fit_w1
mv fort.203 fit_t1
mv fort.204 fit_q1
mv fort.205 fit_pw1
mv fort.206 fit_oz1
mv fort.207 fit_rad1
mv fort.208 fit_pcp1
mv fort.209 fit_rw1
mv fort.213 fit_sst1

cat fit_p1 fit_w1 fit_t1 fit_q1 fit_pw1 fit_rad1 fit_rw1 > ${COMOUT}/rap_p.t${cyc}z.fits.${tmmark}
cat fort.210 fort.211 fort.212 fort.214 fort.215 fort.217 fort.220 > ${COMOUT}/rap_p.t${cyc}z.fits2.${tmmark}

# cp satbias_out ${COMOUT}/rap.t${cyc}z.satbias.${tmmark}

ndatrap=67
grid_ratio=1
cloudanalysistype=6
ifhyb=.false.
ifsoilnudge=.true.

mv sigf03 sigf03_step1
mv siganl sigf03

# Build the GSI namelist on-the-fly
. ${PARMrap}/rap_gsiparm.anl.sh
cat << EOF > gsiparm.anl
$gsi_namelist
EOF
cp gsiparm.anl gsiparm.anl_cloud

export pgm=rap_gsi
startmsg
runline="mpiexec -n 192 -ppn 64 --cpu-bind core --depth 2 ./rap_gsi"
$runline < gsiparm.anl >> $DATA/$pgmout 2>errfile
export err$?; err_chk

cp wrf_inout ${COMOUT}/rap.t${cyc}z.wrf_inout_pcyc

msg="JOB $job HAS COMPLETED NORMALLY."
postmsg "$jlogfile" "$msg"

exit 0
