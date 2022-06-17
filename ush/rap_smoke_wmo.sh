################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_smoke_wmo.sh
# Script description:  To generate the HYSPLIT look-alike smoke products for the Rapid Refresh Model
#
# Author:      M Hu, G Manikin /  GSL, EMC         Date: 2021-12-06
#
# Script history log:
# 2021-12-06  M Hu, G Manikin  -- new script
# 2022-06-17  B Blake          -- Send smoke output to COMOUT and add DBN alerts

set -xa

cd ${DATAsmoke}
rm -rf 227 198 196

for type in sfc pbl
do

mkdir 227
cat rap.${cycle}.smokeCSf00_${type} rap.${cycle}.smokeCSf01_${type} rap.${cycle}.smokeCSf02_${type} rap.${cycle}.smokeCSf03_${type} rap.${cycle}.smokeCSf04_${type} rap.${cycle}.smokeCSf05_${type} rap.${cycle}.smokeCSf06_${type} rap.${cycle}.smokeCSf07_${type} rap.${cycle}.smokeCSf08_${type} rap.${cycle}.smokeCSf09_${type} rap.${cycle}.smokeCSf10_${type} rap.${cycle}.smokeCSf11_${type} rap.${cycle}.smokeCSf12_${type} rap.${cycle}.smokeCSf13_${type} rap.${cycle}.smokeCSf14_${type} rap.${cycle}.smokeCSf15_${type} rap.${cycle}.smokeCSf16_${type} rap.${cycle}.smokeCSf17_${type} rap.${cycle}.smokeCSf18_${type} rap.${cycle}.smokeCSf19_${type} rap.${cycle}.smokeCSf20_${type} rap.${cycle}.smokeCSf21_${type} rap.${cycle}.smokeCSf22_${type} rap.${cycle}.smokeCSf23_${type} rap.${cycle}.smokeCSf24_${type} rap.${cycle}.smokeCSf25_${type} rap.${cycle}.smokeCSf26_${type} rap.${cycle}.smokeCSf27_${type} rap.${cycle}.smokeCSf28_${type} rap.${cycle}.smokeCSf29_${type} rap.${cycle}.smokeCSf30_${type} rap.${cycle}.smokeCSf31_${type} rap.${cycle}.smokeCSf32_${type} rap.${cycle}.smokeCSf33_${type} rap.${cycle}.smokeCSf34_${type} rap.${cycle}.smokeCSf35_${type} rap.${cycle}.smokeCSf36_${type} rap.${cycle}.smokeCSf37_${type} rap.${cycle}.smokeCSf38_${type} rap.${cycle}.smokeCSf39_${type} rap.${cycle}.smokeCSf40_${type} rap.${cycle}.smokeCSf41_${type} rap.${cycle}.smokeCSf42_${type} rap.${cycle}.smokeCSf43_${type} rap.${cycle}.smokeCSf44_${type} rap.${cycle}.smokeCSf45_${type} rap.${cycle}.smokeCSf46_${type} rap.${cycle}.smokeCSf47_${type} rap.${cycle}.smokeCSf48_${type} rap.${cycle}.smokeCSf49_${type} rap.${cycle}.smokeCSf50_${type} rap.${cycle}.smokeCSf51_${type} > 227/rap.${cycle}.smokeCS_${type}_all
$GRB2INDEX 227/rap.${cycle}.smokeCS_${type}_all 227/rap.${cycle}.smokeCS_${type}_allI

cp 227/rap.${cycle}.smokeCS_${type}_all ${COMOUT}/rap_smokeCS.${cycle}.${type}.1hr_227.grib2
if [ "$SENDDBN" == 'YES' ]; then
  ${DBNROOT}/bin/dbn_alert MODEL RAP_SMOKE $job $COMOUT/rap_smokeCS.${cycle}.${type}.1hr_227.grib2
fi

mkdir 198
cat rap.${cycle}.smokeAKf00_${type} rap.${cycle}.smokeAKf01_${type} rap.${cycle}.smokeAKf02_${type} rap.${cycle}.smokeAKf03_${type} rap.${cycle}.smokeAKf04_${type} rap.${cycle}.smokeAKf05_${type} rap.${cycle}.smokeAKf06_${type} rap.${cycle}.smokeAKf07_${type} rap.${cycle}.smokeAKf08_${type} rap.${cycle}.smokeAKf09_${type} rap.${cycle}.smokeAKf10_${type} rap.${cycle}.smokeAKf11_${type} rap.${cycle}.smokeAKf12_${type} rap.${cycle}.smokeAKf13_${type} rap.${cycle}.smokeAKf14_${type} rap.${cycle}.smokeAKf15_${type} rap.${cycle}.smokeAKf16_${type} rap.${cycle}.smokeAKf17_${type} rap.${cycle}.smokeAKf18_${type} rap.${cycle}.smokeAKf19_${type} rap.${cycle}.smokeAKf20_${type} rap.${cycle}.smokeAKf21_${type} rap.${cycle}.smokeAKf22_${type} rap.${cycle}.smokeAKf23_${type} rap.${cycle}.smokeAKf24_${type} rap.${cycle}.smokeAKf25_${type} rap.${cycle}.smokeAKf26_${type} rap.${cycle}.smokeAKf27_${type} rap.${cycle}.smokeAKf28_${type} rap.${cycle}.smokeAKf29_${type} rap.${cycle}.smokeAKf30_${type} rap.${cycle}.smokeAKf31_${type} rap.${cycle}.smokeAKf32_${type} rap.${cycle}.smokeAKf33_${type} rap.${cycle}.smokeAKf34_${type} rap.${cycle}.smokeAKf35_${type} rap.${cycle}.smokeAKf36_${type} rap.${cycle}.smokeAKf37_${type} rap.${cycle}.smokeAKf38_${type} rap.${cycle}.smokeAKf39_${type} rap.${cycle}.smokeAKf40_${type} rap.${cycle}.smokeAKf41_${type} rap.${cycle}.smokeAKf42_${type} rap.${cycle}.smokeAKf43_${type} rap.${cycle}.smokeAKf44_${type} rap.${cycle}.smokeAKf45_${type} rap.${cycle}.smokeAKf46_${type} rap.${cycle}.smokeAKf47_${type} rap.${cycle}.smokeAKf48_${type} rap.${cycle}.smokeAKf49_${type} rap.${cycle}.smokeAKf50_${type} rap.${cycle}.smokeAKf51_${type} > 198/rap.${cycle}.smokeAK_${type}_all
$GRB2INDEX 198/rap.${cycle}.smokeAK_${type}_all 198/rap.${cycle}.smokeAK_${type}_allI

cp 198/rap.${cycle}.smokeAK_${type}_all ${COMOUT}/rap_smokeAK.${cycle}.${type}.1hr_198.grib2
if [ "$SENDDBN" == 'YES' ]; then
  ${DBNROOT}/bin/dbn_alert MODEL RAP_SMOKE $job $COMOUT/rap_smokeAK.${cycle}.${type}.1hr_198.grib2
fi

mkdir 196
cat rap.${cycle}.smokeHIf00_${type} rap.${cycle}.smokeHIf01_${type} rap.${cycle}.smokeHIf02_${type} rap.${cycle}.smokeHIf03_${type} rap.${cycle}.smokeHIf04_${type} rap.${cycle}.smokeHIf05_${type} rap.${cycle}.smokeHIf06_${type} rap.${cycle}.smokeHIf07_${type} rap.${cycle}.smokeHIf08_${type} rap.${cycle}.smokeHIf09_${type} rap.${cycle}.smokeHIf10_${type} rap.${cycle}.smokeHIf11_${type} rap.${cycle}.smokeHIf12_${type} rap.${cycle}.smokeHIf13_${type} rap.${cycle}.smokeHIf14_${type} rap.${cycle}.smokeHIf15_${type} rap.${cycle}.smokeHIf16_${type} rap.${cycle}.smokeHIf17_${type} rap.${cycle}.smokeHIf18_${type} rap.${cycle}.smokeHIf19_${type} rap.${cycle}.smokeHIf20_${type} rap.${cycle}.smokeHIf21_${type} rap.${cycle}.smokeHIf22_${type} rap.${cycle}.smokeHIf23_${type} rap.${cycle}.smokeHIf24_${type} rap.${cycle}.smokeHIf25_${type} rap.${cycle}.smokeHIf26_${type} rap.${cycle}.smokeHIf27_${type} rap.${cycle}.smokeHIf28_${type} rap.${cycle}.smokeHIf29_${type} rap.${cycle}.smokeHIf30_${type} rap.${cycle}.smokeHIf31_${type} rap.${cycle}.smokeHIf32_${type} rap.${cycle}.smokeHIf33_${type} rap.${cycle}.smokeHIf34_${type} rap.${cycle}.smokeHIf35_${type} rap.${cycle}.smokeHIf36_${type} rap.${cycle}.smokeHIf37_${type} rap.${cycle}.smokeHIf38_${type} rap.${cycle}.smokeHIf39_${type} rap.${cycle}.smokeHIf40_${type} rap.${cycle}.smokeHIf41_${type} rap.${cycle}.smokeHIf42_${type} rap.${cycle}.smokeHIf43_${type} rap.${cycle}.smokeHIf44_${type} rap.${cycle}.smokeHIf45_${type} rap.${cycle}.smokeHIf46_${type} rap.${cycle}.smokeHIf47_${type} rap.${cycle}.smokeHIf48_${type} rap.${cycle}.smokeHIf49_${type} rap.${cycle}.smokeHIf50_${type} rap.${cycle}.smokeHIf51_${type} > 196/rap.${cycle}.smokeHI_${type}_all
$GRB2INDEX 196/rap.${cycle}.smokeHI_all 196/rap.${cycle}.smokeHI_allI

cp 196/rap.${cycle}.smokeHI_${type}_all ${COMOUT}/rap_smokeHI.${cycle}.${type}.1hr_196.grib2
if [ "$SENDDBN" == 'YES' ]; then
  ${DBNROOT}/bin/dbn_alert MODEL RAP_SMOKE $job $COMOUT/rap_smokeHI.${cycle}.${type}.1hr_196.grib2
fi

done


for reg in CS AK HI
 do
 for type in sfc pbl
 do
   grib_input=rap.${cycle}.smoke${reg}_${type}_all
   temp_grid_grib2=rap.${type}smoke${reg}.tmp
   if [ ${reg} == 'CS' ]; then
      grdid=227
   elif [ ${reg} == 'AK' ]; then
      grdid=198
   else
      grdid=196
   fi
   smoke_grid_grib2=rap_smoke${reg}.${cycle}.${type}.1hr_${grdid}.grib2

   cd ${DATAsmoke}/${grdid}
   echo $grdid

   pgm=tocgrib2super
   echo 0 > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=${grib_input}
   export FORT12="filesize"
   export FORT31=
   export FORT51=${temp_grid_grib2}
   ${TOCGRIB2SUPER} < $PARMrap/wmo/rap_grib2_${type}smoke_${grdid} >> $DATA/$pgmout 2>errfile

#   echo `ls -l ${grib_input} | awk '{print $5} '` > filesize
   echo `ls -l ${temp_grid_grib2} | awk '{print $5} '` > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=${temp_grid_grib2}
   export FORT12="filesize"
   export FORT31=
   export FORT51=${smoke_grid_grib2}
   ${TOCGRIB2SUPER} < $PARMrap/wmo/rap_grib2_${type}smoke_${grdid} >> $DATA/$pgmout 2>errfile

   cp ${smoke_grid_grib2} $COMOUT/wmo

   if [ "$SENDDBN" == 'YES' ]; then
      ${DBNROOT}/bin/dbn_alert NTC_LOW RAP_SMOKE $job $COMOUT/wmo/${smoke_grid_grib2}
   fi

done
done

