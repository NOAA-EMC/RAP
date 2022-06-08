#!/bin/ksh
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         exrap_post.ecf
# Script description:  Run rap post jobs
#
# Author:      G Manikin / M Hu    EMC/GSD         Date: 2011-09-15
#
# Abstract: This script runs the RAP post jobs for the 21-h RAP forecast
#
# Script history log:
# 2011-09-15  G Manikin / M Hu  -- new script 
# 2018-01-25  B Blake / C Guastini / G Manikin / C Alexander - RAPv4

set -xa
cd $DATA 

msg="$job HAS BEGUN"
postmsg $jlogfile "$msg"

# fhr is passed from the SMS script
fhr=$fhr

# Set up some constants
export XLFRTEOPTS="unit_vars=yes"
START_TIME=$PDY$cyc
echo $START_TIME >STARTTIME
export CORE=RAPR
export OUTTYP=netcdf
export MP_BINDPROC=NO

export MP_SHARED_MEMORY=yes
export RSTFNL=${DATA}/
export COMSP=${DATA}/
export SPLNUM=47
export SPL=2.,5.,7.,10.,20.,30.\
,50.,70.,75.,100.,125.,150.,175.,200.,225.\
,250.,275.,300.,325.,350.,375.,400.,425.,450.\
,475.,500.,525.,550.,575.,600.,625.,650.\
,675.,700.,725.,750.,775.,800.,825.,850.\
,875.,900.,925.,950.,975.,1000.,1013.2

targetsize=6854865028
#targetsize=6858968708
targetsize2=6871551620
icnt=1
  while [ $icnt -lt 1000 ]
  do
    POST_TIME=`$NDATE +${fhr} $START_TIME`
    YYYY=`echo ${POST_TIME} | cut -c1-4`
    MM=`echo ${POST_TIME} | cut -c5-6`
    DD=`echo ${POST_TIME} | cut -c7-8`
    HH=`echo ${POST_TIME} | cut -c9-10`
    yy=`echo ${POST_TIME} | cut -c3-4`
    timestr=${YYYY}-${MM}-${DD}_${HH}:00:00
    timestr2=${YYYY}-${MM}-${DD}_${HH}_00_00

    if [ -s $INPUT_DATA/wrfout_d01_${timestr2} ]
    then
      counter=1
      while [[ $counter -lt 20 ]]; do
      filesize=$(stat -c%s $INPUT_DATA/wrfout_d01_${timestr2})
#     if [ $filesize -eq $targetsize ]; then
      if [[ $filesize -eq $targetsize  || $filesize -eq $targetsize2 ]]; then
        break
      else 
        sleep 3
        counter=` expr $counter + 1 `
      fi
      done
      break
    else
      icnt=$((icnt + 1))
      sleep 5
    fi
    if [ $icnt -ge 180 ]
    then
      msg="FATAL ERROR: ABORTING after 15 minutes of waiting for RAP FCST F${fhr} to end."
      err_exit $msg
    fi
  done

# copy wrfout file to guess directory for use by later cycles
# only copying first 6 forecast hours to save disk space
  if [ $fhr -lt 07 ]
  then 
    counter=1
    while [[ $counter -lt 36 ]]; do
    filesize=$(stat -c%s $INPUT_DATA/wrfout_d01_${timestr2})
    echo $filesize
#   if [ $filesize -eq $targetsize ]; then
    if [[ $filesize -eq $targetsize  || $filesize -eq $targetsize2 ]]; then
      cpfs $INPUT_DATA/wrfout_d01_${timestr2} ${RAPGES_FCYC}/rap_${START_TIME}f0${fhr}
      break
    else 
      sleep 5
      counter=` expr $counter + 1 `
    fi
    done
    if [ $counter -eq 36 ]; then
    echo "WARNING: Forecast Hour f0${fhr} still short on node! Copy anyway."
      cpfs $INPUT_DATA/wrfout_d01_${timestr2} ${RAPGES_FCYC}/rap_${START_TIME}f0${fhr}
    fi 
  fi

##  save sfc files for partial cycle initialization and restart purposes
timeHH=${HH}
#if [ ${fhr} -lt 19 ]; then
#  cp ${INPUT_DATA}/wrfout_d01_${timestr2} ${RAPGES_SFC}/wrfout_d01_${timeHH}
#fi
fcstfile=${INPUT_DATA}/wrfout_d01_${timestr2}

# use pre-DFI analysis file for posting
time_analysis='00'
rm wrfoutd01
if [ ${fhr} -eq ${time_analysis} ]; then
  fcstfile=${INPUT_DATA}/wrfinput_d01
  echo 'use GSI analysis',${fcstfile}
  cp ${fcstfile} wrfinput_d01
  rm wrfout_analyzed_d01
  ln -s ${INPUT_DATA}/wrfout_analyzed_d01 wrfout_analyzed_d01
  cp ${EXECrap}/rap_update_fields .
  runline="mpiexec -n 48 -ppn 64 ./rap_update_fields"
  $runline >> update_fields.out
  ln -s wrfinput_d01 wrfoutd01
  export err=$?; err_chk
else
  ln -s ${fcstfile} wrfoutd01
fi

rm -f fort.*
ln -s ${PARMrap}/rap_post_avblflds.xml post_avblflds.xml
ln -s ${PARMrap}/rap_params_grib2_tbl_new params_grib2_tbl_new
ln -s ${PARMrap}/rap_gtg.config gtg.config

#use files without bucket fields for f00 and f01 to avoid duplication
if [ $fhr -eq 00 -o $fhr -eq 01 ]
then
  ln -s ${PARMrap}/rap_postcntrl_anl.xml postcntrl.xml
  ln -s ${PARMrap}/rap_postxconfig_anl-NT.txt postxconfig-NT.txt
else
  ln -s ${PARMrap}/rap_postcntrl.xml postcntrl.xml
  ln -s ${PARMrap}/rap_postxconfig-NT.txt postxconfig-NT.txt
fi

cp ${PARMrap}/rap_run_ETAMPNEW_DATA eta_micro_lookup.dat
ln -s ${CRTM}/imgr_g11.SpcCoeff.bin imgr_g11.SpcCoeff.bin
ln -s ${CRTM}/imgr_g12.SpcCoeff.bin imgr_g12.SpcCoeff.bin
ln -s ${CRTM}/imgr_g13.SpcCoeff.bin imgr_g13.SpcCoeff.bin
ln -s ${CRTM}/imgr_g15.SpcCoeff.bin imgr_g15.SpcCoeff.bin
ln -s ${CRTM}/imgr_mt1r.SpcCoeff.bin imgr_mt1r.SpcCoeff.bin
ln -s ${CRTM}/imgr_mt2.SpcCoeff.bin imgr_mt2.SpcCoeff.bin
ln -s ${CRTM}/amsre_aqua.SpcCoeff.bin amsre_aqua.SpcCoeff.bin
ln -s ${CRTM}/tmi_trmm.SpcCoeff.bin tmi_trmm.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f13.SpcCoeff.bin ssmi_f13.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f14.SpcCoeff.bin ssmi_f14.SpcCoeff.bin
ln -s ${CRTM}/ssmi_f15.SpcCoeff.bin ssmi_f15.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f16.SpcCoeff.bin ssmis_f16.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f17.SpcCoeff.bin ssmis_f17.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f18.SpcCoeff.bin ssmis_f18.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f19.SpcCoeff.bin ssmis_f19.SpcCoeff.bin
ln -s ${CRTM}/ssmis_f20.SpcCoeff.bin ssmis_f20.SpcCoeff.bin
ln -s ${CRTM}/seviri_m10.SpcCoeff.bin seviri_m10.SpcCoeff.bin
ln -s ${CRTM}/v.seviri_m10.SpcCoeff.bin v.seviri_m10.SpcCoeff.bin
#ln -s ${FIXrap}/rap_imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin
ln -s ${CRTM}/imgr_insat3d.SpcCoeff.bin imgr_insat3d.SpcCoeff.bin

ln -s ${CRTM}/imgr_g11.TauCoeff.bin imgr_g11.TauCoeff.bin
ln -s ${CRTM}/imgr_g12.TauCoeff.bin imgr_g12.TauCoeff.bin
ln -s ${CRTM}/imgr_g13.TauCoeff.bin imgr_g13.TauCoeff.bin
ln -s ${CRTM}/imgr_g15.TauCoeff.bin imgr_g15.TauCoeff.bin
ln -s ${CRTM}/imgr_mt1r.TauCoeff.bin imgr_mt1r.TauCoeff.bin
ln -s ${CRTM}/imgr_mt2.TauCoeff.bin imgr_mt2.TauCoeff.bin
ln -s ${CRTM}/amsre_aqua.TauCoeff.bin amsre_aqua.TauCoeff.bin
ln -s ${CRTM}/tmi_trmm.TauCoeff.bin tmi_trmm.TauCoeff.bin
ln -s ${CRTM}/ssmi_f13.TauCoeff.bin ssmi_f13.TauCoeff.bin
ln -s ${CRTM}/ssmi_f14.TauCoeff.bin ssmi_f14.TauCoeff.bin
ln -s ${CRTM}/ssmi_f15.TauCoeff.bin ssmi_f15.TauCoeff.bin
ln -s ${CRTM}/ssmis_f16.TauCoeff.bin ssmis_f16.TauCoeff.bin
ln -s ${CRTM}/ssmis_f17.TauCoeff.bin ssmis_f17.TauCoeff.bin
ln -s ${CRTM}/ssmis_f18.TauCoeff.bin ssmis_f18.TauCoeff.bin
ln -s ${CRTM}/ssmis_f19.TauCoeff.bin ssmis_f19.TauCoeff.bin
ln -s ${CRTM}/ssmis_f20.TauCoeff.bin ssmis_f20.TauCoeff.bin
ln -s ${CRTM}/seviri_m10.TauCoeff.bin seviri_m10.TauCoeff.bin
ln -s ${CRTM}/seviri_m10.TauCoeff.bin v.seviri_m10.TauCoeff.bin
#ln -s ${FIXrap}/rap_imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin
ln -s ${CRTM}/imgr_insat3d.TauCoeff.bin imgr_insat3d.TauCoeff.bin

ln -s ${CRTM}/NPOESS.IRice.EmisCoeff.bin NPOESS.IRice.EmisCoeff.bin 
ln -s ${CRTM}/NPOESS.IRland.EmisCoeff.bin NPOESS.IRland.EmisCoeff.bin
ln -s ${CRTM}/NPOESS.IRsnow.EmisCoeff.bin NPOESS.IRsnow.EmisCoeff.bin
ln -s ${CRTM}/Nalli.IRwater.EmisCoeff.bin Nalli.IRwater.EmisCoeff.bin

ln -s ${CRTM}/FASTEM6.MWwater.EmisCoeff.bin FASTEM6.MWwater.EmisCoeff.bin

ln -s ${CRTM}/CloudCoeff.bin CloudCoeff.bin
ln -s ${CRTM}/AerosolCoeff.bin AerosolCoeff.bin
#ln -s ${CRTM}/Nalli.EK-PDF.W_W-RefInd.EmisCoeff.bin EmisCoeff.bin
ln -s ${CRTM}/Nalli.IRwater.EmisCoeff.bin EmisCoeff.bin

cat > itag <<EOF
wrfoutd01
netcdf
grib2
${timestr}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

  pgm=rap_wrfpost
  export pgm;. prep_step

  startmsg
  cp ${EXECrap}/rap_wrfpost .
#  runline="mpiexec -n 48 -ppn 64 ./rap_wrfpost"
  runline="mpiexec -n 64 -ppn 64 ./rap_wrfpost"
#  runline="mpiexec -n 48 -ppn 48 ./rap_wrfpost"

$runline >> wrfpost.out
  err=$?;export err ;err_chk

  if test "$SENDCOM" = 'YES'
    then
      mv WRFNAT${fhr}.tm00 ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2
      mv WRFPRS${fhr}.tm00 ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2
      mv WRFMSL${fhr}.tm00 ${COMOUT}/rap.${cycle}.wrfmslf${fhr}.grib2
       ${WGRIB2} ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2.idx
  fi

  if [ $SENDDBN = YES ]
    then
    $DBNROOT/bin/dbn_alert MODEL RAP_NAT $job $COMOUT/rap.${cycle}.wrfnatf${fhr}.grib2
    $DBNROOT/bin/dbn_alert MODEL RAP_PRS $job $COMOUT/rap.${cycle}.wrfprsf${fhr}.grib2
    $DBNROOT/bin/dbn_alert MODEL RAP_MSL $job $COMOUT/rap.${cycle}.wrfmslf${fhr}.grib2
    $DBNROOT/bin/dbn_alert MODEL RAP_PRS_WIDX $job $COMOUT/rap.${cycle}.wrfprsf${fhr}.grib2.idx
  fi
  if [ $fhr -eq 18 ] || [ $fhr -eq 21 ]; then
    ecflow_client --event wrfgrib2_ready #for lmp 01z/13z use due to late start on rap 00z and 12z
  fi

# use post-DFI analysis file to generate extra analysis for EMC 
#   initialization applications
if [ ${fhr} -eq ${time_analysis} ]; then
  rm wrfoutd01
  echo 'use post-DFI file ',${fcstfile}
  fcstfile=${INPUT_DATA}/wrfout_d01_${timestr2}
  ln -s ${fcstfile} wrfoutd01
  export err=$?; err_chk

rm -f fort.*
rm -f itag

cat > itag <<EOF
wrfoutd01
netcdf
grib2
${timestr}
${CORE}
${SPLNUM}
${SPL}
${VALIDTIMEUNITS}
EOF

  pgm=rap_wrfpost
  export pgm;. prep_step

  startmsg
#  runline="mpiexec -n 48 -ppn 64 ./rap_wrfpost"
  runline="mpiexec -n 64 -ppn 64 ./rap_wrfpost"
#    runline="mpiexec -n 48 -ppn 48 ./rap_wrfpost"

  $runline >> wrfpost2.out
  err=$?;export err ;err_chk


    mv WRFPRS${fhr}.tm00 $COMOUT/rap.${cycle}.wrfprsdf${fhr}.grib2
    mv WRFNAT${fhr}.tm00 $COMOUT/rap.${cycle}.wrfnatdf${fhr}.grib2
    mv WRFMSL${fhr}.tm00 $COMOUT/rap.${cycle}.wrfmsldf${fhr}.grib2
  
  ${WGRIB2} $COMOUT/rap.${cycle}.wrfnatdf${fhr}.grib2 -s > $COMOUT/rap.${cycle}.wrfnatdf${fhr}.grib2.idx
  if [ $fhr -eq "00" ]; then
    ecflow_client --event wrfnatdgrib2_ready #for hrrrdas_getguess 09z/21z use to start early
  fi

fi

# get surface fields
ln -s $COMOUT/rap.${cycle}.wrfnatf${fhr}.grib2 rap.${cycle}.wrfnatf${fhr}.grib2
${HOMErap}/ush/rap_grib2sfc.sh ${cycle} ${fhr} rap.${cycle}.wrfnatf${fhr}.grib2
mv wrftwo.grib2 $COMOUT/rap.${cycle}.wrftwof${fhr}.grib2

# Full domain
# 32 km
export grid_specs_221="lambert:253:50.000000 214.500000:349:32463.000000 1.000000:277:32463.000000"

# CONUS
# 2.5 km smart init
export grid_specs_187="lambert:265:25.000000 233.723448:2345:2539.703000 19.228976:1597:2539.703000"
# 13 km
export grid_specs_130="lambert:265:25.000000 233.862000:451:13545.000000 16.281000:337:13545.000000"
# 20 km
export grid_specs_252="lambert:265:25.000000 233.862000:301:20318.000000 16.281000:225:20318.000000"
# 40 km
export grid_specs_236="lambert:265:25.000000 233.862000:151:40635.000000 16.281000:113:40635.000000"

# Alaska
export grid_specs_242="nps:225:60.000000 187.000000:553:11250.000000 30.000000:425:11250.000000"

# Hawaii
export grid_specs_243="latlon 190.0:126:0.400 10.000:101:0.400"

# Puerto Rico
export grid_specs_200="lambert:253:50.000000 285.720000:108:16232.000000 16.201000:94:16232.000000"

# Loop across all sub-domain grid types
for grid in 130 242 252 236
do

   # Load sub-domain grid specification
   eval grid_specs=\${grid_specs_${grid}}

   # Loop across vertical level types
   for leveltype in nat prs
   do

      # Remove temporary files
      rm -f tmp.inv tmp.grib2 tmpuv.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

      # DFI 130 native grid
      if [[ ${grid} -eq 130 || ${grid} -eq 242 ]] && [[ ${fhr} -eq ${time_analysis} && ${leveltype} = "nat" && -s ${PARMrap}/rap_parmlist_${leveltype}_${grid}.txt ]]; then

         # Create subset of fields to be posted
         ${WGRIB2} -inv tmp.inv ${COMOUT}/rap.${cycle}.wrf${leveltype}df${fhr}.grib2
         grep < tmp.inv "`cat ${PARMrap}/rap_parmlist_${leveltype}_${grid}.txt`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrf${leveltype}df${fhr}.grib2 -grib tmp.grib2

         # Merge vector field records in subset
         ${WGRIB2} tmp.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv.grib2

         # Interpolate fields to new grid
         ${WGRIB2} tmpuv.grib2 -set_bitmap 1 -set_grib_type jpeg -new_grid_winds grid \
         -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" \
         -new_grid_interpolation bilinear \
         -if "`cat ${PARMrap}/rap_budget_fields.txt`" -new_grid_interpolation budget -fi \
         -if "`cat ${PARMrap}/rap_neighbor_fields.txt`" -new_grid_interpolation neighbor -fi \
         -new_grid ${grid_specs} tmp_${grid}.grib2

         # Merge vector field records
         ${WGRIB2} tmp_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv_${grid}.grib2

         # Move grid to final location and index
         mv tmpuv_${grid}.grib2 ${COMOUT}/rap.${cycle}.awp${grid}bgrbf${fhr}df
         ${WGRIB2} ${COMOUT}/rap.${cycle}.awp${grid}bgrbf${fhr}df -s > ${COMOUT}/rap.${cycle}.awp${grid}bgrbf${fhr}df.idx

          if [ $SENDDBN = YES ]
           then
          $DBNROOT/bin/dbn_alert MODEL RAP_ANL_RADAR $job $COMOUT/rap.t${cyc}z.awp130bgrbf${fhr}df
         fi
      fi

      # Remove temporary files
      rm -f tmp.inv tmp.grib2 tmpuv.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

      #  All other files
      if [[ -s ${PARMrap}/rap_parmlist_${leveltype}_${grid}.txt ]]; then

         # Create subset of fields to be posted
         ${WGRIB2} -inv tmp.inv ${COMOUT}/rap.${cycle}.wrf${leveltype}f${fhr}.grib2
         grep < tmp.inv "`cat ${PARMrap}/rap_parmlist_${leveltype}_${grid}.txt`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrf${leveltype}f${fhr}.grib2 -grib tmp.grib2

         # Merge vector field records in subset
         ${WGRIB2} tmp.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv.grib2

         # Interpolate fields to new grid
         ${WGRIB2} tmpuv.grib2 -set_bitmap 1 -set_grib_type jpeg -new_grid_winds grid \
         -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" \
         -new_grid_interpolation bilinear \
         -if "`cat ${PARMrap}/rap_budget_fields.txt`" -new_grid_interpolation budget -fi \
         -if "`cat ${PARMrap}/rap_neighbor_fields.txt`" -new_grid_interpolation neighbor -fi \
         -new_grid ${grid_specs} tmp_${grid}.grib2

         # Merge vector field records
         ${WGRIB2} tmp_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv_${grid}.grib2

         # Move grid to final location and index
         if test "$SENDCOM" = 'YES'
          then
           if [[ ${leveltype} = "nat" ]]; then
             mv tmpuv_${grid}.grib2 ${COMOUT}/rap.${cycle}.awp${grid}bgrbf${fhr}.grib2
             ${WGRIB2} ${COMOUT}/rap.${cycle}.awp${grid}bgrbf${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.awp${grid}bgrbf${fhr}.grib2.idx
               if [ $SENDDBN = YES ]
                 then
                  fil=awp${grid}bgrb
                  case $fil in
                   awp130bgrb)  ALERT_TYPE=RAP_NG13_GB2;;
                   awp252bgrb)  ALERT_TYPE=RAP_NG20_GB2;;
                  esac
                 if [ $fil != awp242bgrb ]; then # Do not send out awp242bgrb files
                 $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp${grid}bgrbf${fhr}.grib2
                 $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp${grid}bgrbf${fhr}.grib2.idx
                 fi
               fi  # SENDBN
           else
             mv tmpuv_${grid}.grib2 ${COMOUT}/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2
             ${WGRIB2} ${COMOUT}/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2.idx 
# complicated!!   only want to send pgrb alerts if FHR is not 3,6,9,12,15,18,21   Othwerwise, we still
#    need to tack on 3-hr buckets generated in the mkawp ush script and will alert then
             if [ $fhr -ne 06 -a $fhr -ne 09 -a $fhr -ne 12 -a $fhr -ne 15 -a \
                  $fhr -ne 18 -a $fhr -ne 21 -a $fhr -ne 24 -a $fhr -ne 27 -a \
                  $fhr -ne 30 -a $fhr -ne 33 -a $fhr -ne 36 -a $fhr -ne 39 ] ; then
               if [ $SENDDBN = YES ]
                 then
                  fil=awp${grid}pgrb
                  case $fil in
                   awp252pgrb)  ALERT_TYPE=RAP_PG20_GB2
                      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2
                      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2.idx
                      ;;
                   awp236pgrb)  ALERT_TYPE=RAP_PG40_GB2
                      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2
                      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2.idx
                      ;;
                   awp130pgrb)  ALERT_TYPE=RAP_PG13_GB2
                      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2
                      $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp${grid}pgrbf${fhr}.grib2.idx
                      ;;
                  esac
               fi  # SENDBN
              fi  #fhr check


           fi   # leveltype
         fi   # SENDCOM
      fi  #parmlist
   done
done

for grid in 221 242 200 243
do

   # Load sub-domain grid specification
   eval grid_specs=\${grid_specs_${grid}}

   # Remove temporary files
   rm -f tmp.inv tmp.grib2 tmpuv.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

      # DFI awip32 grid
      if [[ ${fhr} -eq ${time_analysis} && ${grid} -eq 221 && -s ${PARMrap}/rap_parmlist_${grid}.txt ]]; then

         # Create subset of fields to be posted
         ${WGRIB2} -inv tmp.inv ${COMOUT}/rap.${cycle}.wrfprsdf${fhr}.grib2
         grep < tmp.inv "`cat ${PARMrap}/rap_parmlist_${grid}.txt`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrfprsdf${fhr}.grib2 -grib tmp.grib2

         # Merge vector field records in subset
         ${WGRIB2} tmp.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv.grib2

         # Interpolate fields to new grid
         ${WGRIB2} tmpuv.grib2 -set_bitmap 1 -set_grib_type jpeg -new_grid_winds grid \
         -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" \
         -new_grid_interpolation bilinear \
         -if "`cat ${PARMrap}/rap_budget_fields.txt`" -new_grid_interpolation budget -fi \
         -if "`cat ${PARMrap}/rap_neighbor_fields.txt`" -new_grid_interpolation neighbor -fi \
         -new_grid ${grid_specs} tmp_${grid}.grib2

         # Merge vector field records
         ${WGRIB2} tmp_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv_${grid}.grib2

         # Move grid to final location and index
         mv tmpuv_${grid}.grib2 ${COMOUT}/rap.${cycle}.awip32f${fhr}df.grib2
         ${WGRIB2} ${COMOUT}/rap.${cycle}.awip32f${fhr}df.grib2 -s > ${COMOUT}/rap.${cycle}.awip32f${fhr}df.grib2.idx
      fi

   rm -f tmp.inv tmp.grib2 tmpuv.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

   if [[ -s ${PARMrap}/rap_parmlist_${grid}.txt ]]; then

       # Create subset of fields to be posted
       ${WGRIB2} -inv tmp.inv ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2
       grep < tmp.inv "`cat ${PARMrap}/rap_parmlist_${grid}.txt`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2 -grib tmp.grib2
       $GRB2INDEX  ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2  ${COMOUT}/rap.${cycle}.wrfprsf${fhr}.grib2.idx

       # Merge vector field records in subset
       ${WGRIB2} tmp.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv.grib2

       # Interpolate fields to new grid
       ${WGRIB2} tmpuv.grib2 -set_bitmap 1 -set_grib_type jpeg -new_grid_winds grid \
       -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" \
       -new_grid_interpolation bilinear \
       -if "`cat ${PARMrap}/rap_budget_fields.txt`" -new_grid_interpolation budget -fi \
       -if "`cat ${PARMrap}/rap_neighbor_fields.txt`" -new_grid_interpolation neighbor -fi \
       -new_grid ${grid_specs} tmp_${grid}.grib2

       # Merge vector field records
       ${WGRIB2} tmp_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv_${grid}.grib2

#######################################################
# Generate 3-hour precip, snow, and graupel fields
#######################################################

  echo 'check for 3-hr'
  if [ $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o \
       $fhr -eq 30 -o $fhr -eq 33 -o $fhr -eq 36 -o $fhr -eq 39 ] ; then

    let fhr3=fhr-3
    typeset -Z2 fhr3
#
# Make sure fhr3 prdgen files are available before
# proceeding.
#
    ic=0
# 221 grids have different naming convention (awip32), so need separate loop
    if [ $grid -ne 221 ] ; then
    while [ ! -r $INPUT_DATA/postdone_${grid}_f${fhr3}_${cyc} ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "FATAL ERROR: F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done
    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awp${grid}f${fhr3}.grib2 rap.t${cyc}z.awp${grid}f${fhr3}.grib2.idxbin
    else
    while [ ! -r $INPUT_DATA/postdone_221_f${fhr3}_${cyc} ] ; do
      let ic=ic+1
      if [ $ic -gt 180 ] ; then
        err_exit "FATAL ERROR: F$fhr SUB PROCESSING GIVING UP AFTER 45 MINUTES WAITING FOR F$fhr3 files"
      fi
      sleep 15
    done
    $GRB2INDEX ${COMOUT}/rap.t${cyc}z.awip32f${fhr3}.grib2 awip32f${fhr3}.grib2.idxbin
    fi   # end 221 test

# start grid processing

    echo 'process ' ${grid}
    if [ ${grid} -eq 221 ] ; then
     cp tmpuv_${grid}.grib2 awip32f${fhr}.grib2
     $GRB2INDEX tmpuv_${grid}.grib2 awip32f${fhr}.grib2.idxbin
     cp $COMOUT/rap.t${cyc}z.awip32f${fhr3}.grib2 awip32f${fhr3}.grib2
 
     IARW=0
     ISNOW=1
     pfhr1=${fhr}
     pfhr2=${fhr3}
    export pgm=rap_subflds_g2;. prep_step
     ln -sf awip32f${fhr3}.grib2      fort.13
     ln -sf awip32f${fhr3}.grib2.idxbin  fort.14
     ln -sf awip32f${fhr}.grib2       fort.15
     ln -sf awip32f${fhr}.grib2.idxbin   fort.16
     ln -sf precip221.${fhr}           fort.50
     ln -sf precip221.${fhr}           fort.50
     ln -sf ncprecip221.${fhr}         fort.51
     ln -sf cprecip221.${fhr}          fort.52
     ln -sf weasd221.${fhr}            fort.53
     ln -sf graupel221.${fhr}            fort.54
       $EXECrap/rap_subflds_g2 << EOF >> $DATA/$pgmout 2>errfile
       $pfhr1 $pfhr2 $IARW $ISNOW
EOF
    export err=$?;err_chk

     cat precip221.${fhr} >> awip32f${fhr}.grib2 
     cat ncprecip221.${fhr} >> awip32f${fhr}.grib2
     cat cprecip221.${fhr} >> awip32f${fhr}.grib2 
     cat weasd221.${fhr} >> awip32f${fhr}.grib2 
     cat graupel221.${fhr} >> awip32f${fhr}.grib2 

     if  [ $SENDCOM = YES ]; then
       mv awip32f${fhr}.grib2 ${COMOUT}/rap.${cycle}.awip32f${fhr}.grib2
       ${WGRIB2} ${COMOUT}/rap.${cycle}.awip32f${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.awip32f${fhr}.grib2.idx

       if [ $SENDDBN = YES ]
       then
         ALERT_TYPE=RAP_FULL32_GB2
         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awip32f${fhr}.grib2
         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awip32f${fhr}.grib2.idx
       fi
     fi     

#  process 200 242 243
    else
     cp tmpuv_${grid}.grib2 awp${grid}f${fhr}.grib2
     $GRB2INDEX tmpuv_${grid}.grib2 awp${grid}f${fhr}.grib2.idxbin
     cp $COMOUT/rap.t${cyc}z.awp${grid}f${fhr3}.grib2 awp${grid}f${fhr3}.grib2
     cp rap.t${cyc}z.awp${grid}f${fhr3}.grib2.idxbin awp${grid}f${fhr3}.grib2.idxbin

     IARW=0
     if [ ${grid} -ne 200 ] ; then
     ISNOW=1
     else
     ISNOW=0
     fi
     pfhr1=${fhr}
     pfhr2=${fhr3}

    export pgm=rap_subflds_g2;. prep_step
     ln -sf awp${grid}f${fhr3}.grib2      fort.13
     ln -sf awp${grid}f${fhr3}.grib2.idxbin  fort.14
     ln -sf awp${grid}f${fhr}.grib2       fort.15
     ln -sf awp${grid}f${fhr}.grib2.idxbin   fort.16
     ln -sf precip${grid}.${fhr}           fort.50
     ln -sf precip${grid}.${fhr}           fort.50
     ln -sf ncprecip${grid}.${fhr}         fort.51
     ln -sf cprecip${grid}.${fhr}          fort.52
     ln -sf weasd${grid}.${fhr}            fort.53
     ln -sf graupel${grid}.${fhr}            fort.54
       $EXECrap/rap_subflds_g2 << EOF >> subfld2.out
       $pfhr1 $pfhr2 $IARW $ISNOW
EOF
    export err=$?;err_chk

     cat precip${grid}.${fhr} >> awp${grid}f${fhr}.grib2
     cat ncprecip${grid}.${fhr} >> awp${grid}f${fhr}.grib2
     cat cprecip${grid}.${fhr} >> awp${grid}f${fhr}.grib2
#  no snow/graupel needed for Puerto Rico
     if [ ${grid} -ne 200 ] ; then
     cat weasd${grid}.${fhr} >> awp${grid}f${fhr}.grib2
     cat graupel${grid}.${fhr} >> awp${grid}f${fhr}.grib2
     fi
     mv awp${grid}f${fhr}.grib2 ${COMOUT}/rap.${cycle}.awp${grid}f${fhr}.grib2
     ${WGRIB2} ${COMOUT}/rap.${cycle}.awp${grid}f${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.awp${grid}f${fhr}.grib2.idx
      if [ $SENDDBN = YES ]
         then
          fil=awp${grid}
          case $fil in
           awp243)      ALERT_TYPE=RAP_HI_GB2;;
           awp242)      ALERT_TYPE=RAP_AK11_GB2;;
           awp200)      ALERT_TYPE=RAP_PR16_GB2;;
          esac

         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp${grid}f${fhr}.grib2
         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp${grid}f${fhr}.grib2.idx
      fi
    fi # end 221 test
  elif [ ${grid} -eq 221 ] ; then
      if test "$SENDCOM" = 'YES'
       then
        mv tmpuv_${grid}.grib2 ${COMOUT}/rap.${cycle}.awip32f${fhr}.grib2
        ${WGRIB2} ${COMOUT}/rap.${cycle}.awip32f${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.awip32f${fhr}.grib2.idx
      fi
      if [ $SENDDBN = YES ]
         then
         ALERT_TYPE=RAP_FULL32_GB2
         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awip32f${fhr}.grib2
         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awip32f${fhr}.grib2.idx
      fi
  else
      if test "$SENDCOM" = 'YES'; then
       mv tmpuv_${grid}.grib2 ${COMOUT}/rap.${cycle}.awp${grid}f${fhr}.grib2
       ${WGRIB2} ${COMOUT}/rap.${cycle}.awp${grid}f${fhr}.grib2 -s > ${COMOUT}/rap.${cycle}.awp${grid}f${fhr}.grib2.idx
      fi
      if [ $SENDDBN = YES ]
         then
          fil=awp${grid}
          case $fil in
           awp243)      ALERT_TYPE=RAP_HI_GB2;;
           awp242)      ALERT_TYPE=RAP_AK11_GB2;;
           awp200)      ALERT_TYPE=RAP_PR16_GB2;;
          esac

         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE} $job $COMOUT/rap.${cycle}.awp${grid}f${fhr}.grib2
         $DBNROOT/bin/dbn_alert MODEL ${ALERT_TYPE}_WIDX $job $COMOUT/rap.${cycle}.awp${grid}f${fhr}.grib2.idx
      fi
  fi # check for 3-hr
  #alert that grids necessary for making buckets are in final form
  if [ $fhr -eq 03 -o $fhr -eq 06 -o $fhr -eq 09 -o $fhr -eq 12 -o $fhr -eq 15 -o \
       $fhr -eq 18 -o $fhr -eq 21 -o $fhr -eq 24 -o $fhr -eq 27 -o $fhr -eq 30 -o \
       $fhr -eq 33 -o $fhr -eq 36 ] ; then
    echo done >$INPUT_DATA/postdone_${grid}_f${fhr}_${cyc}
  fi
fi
done

#======================================================
# For GTG on MSL, convert to grid 130
#======================================================
leveltype=msl
grid_specs_gtg130="lambert:265:25.000000 233.861999:451:13545.087000 16.281000:337:13545.087000"
$WGRIB2 $COMOUT/rap.${cycle}.wrf${leveltype}f${fhr}.grib2 \
        -new_grid_winds earth \
        -new_grid_interpolation bilinear \
        -new_grid $grid_specs_gtg130 rap.${cycle}.gtg130f${fhr}.grib2
if test "$SENDCOM" = 'YES'
then
    cp rap.${cycle}.gtg130f${fhr}.grib2 ${COMOUT}/.
    if [ "$SENDDBN" == 'YES' ]; then
       ${DBNROOT}/bin/dbn_alert MODEL GTG_GB2 $job $COMOUT/rap.${cycle}.gtg130f${fhr}.grib2
    fi
fi


#Now do the AWIPS grid conversion
  $USHrap/rap_mkawp.sh $fhr
#End of AWIPS grid generation

#ecflow_client --event grib_ready

if [ $fhr -gt 0 -a $fhr -lt 7 ]
then
  echo $USHrap/rap_smartinit.sh $fhr 187 CS >>poescript
  echo $USHrap/rap_smartinit.sh $fhr 198 AK >>poescript
  echo $USHrap/rap_smartinit.sh $fhr 196 HI >>poescript
  echo $USHrap/rap_smartinit.sh $fhr 194 PR >>poescript

  LSB_DJOB_NUMPROC=8
  export tasks=`cat poescript |wc -l`
  while [ $tasks -lt ${LSB_DJOB_NUMPROC} ]; do
    echo hostname >>poescript
    export tasks=`cat poescript |wc -l`
  done
 
  chmod 775 poescript
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=poescript
  #aprun -cmdfile poescript -pgmmodel mpmd -labelio yes -stdoutmode ordered
  postdate=${YYYY}${MM}${DD}${HH}
  echo "DATE  ${postdate}0000WASHINGTON" > DATE_SMARTINIT
  mpiexec -np 8 --cpu-bind core cfp ./poescript
fi

if [ $cyc -eq 03 ]
then
  echo $USHrap/rap_smoke.sh $fhr 227 CS >>poescript_smoke
  echo $USHrap/rap_smoke.sh $fhr 198 AK >>poescript_smoke
  echo $USHrap/rap_smoke.sh $fhr 196 HI >>poescript_smoke

  LSB_DJOB_NUMPROC=8
  export tasks=`cat poescript_smoke |wc -l`
  while [ $tasks -lt ${LSB_DJOB_NUMPROC} ]; do
    echo hostname >>poescript_smoke
    export tasks=`cat poescript_smoke |wc -l`
  done

  chmod 775 poescript
  export MP_PGMMODEL=mpmd
  export MP_CMDFILE=poescript
  mkdir -p smoke
  export DATAsmoke=$(pwd)/smoke
  mpiexec -np 8 --cpu-bind core cfp ./poescript_smoke
fi

if [ $cyc -eq 03 -a $fhr -eq 51 ]
then
  sleep 60
# Retrieve smoke output from previous forecast hours
  hours="00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50"
  for hour in $hours; do
    cp $DATAROOT/rap_post_${envir}_${cyc}_f${hour}/smoke/rap* ${DATAsmoke}
  done

  numfiles=$(ls ${DATAsmoke}/rap.${cycle}.smoke* | wc -l)
  if [[ $numfiles -eq 156 ]]; then
    break
  else
    echo "WARNING: Not all smoke output was produced."
    exit
  fi
  $USHrap/rap_smoke_wmo.sh
fi

postmsg $jlogfile "RAP POST done for F${fhr}"

echo EXITING $0
exit
