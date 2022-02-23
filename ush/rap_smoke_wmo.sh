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

set -xa

cd ${DATAsmoke}
rm -rf 227 198 196

mkdir 227
cat rap.${cycle}.smokeCSf00 rap.${cycle}.smokeCSf01 rap.${cycle}.smokeCSf02 rap.${cycle}.smokeCSf03 rap.${cycle}.smokeCSf04 rap.${cycle}.smokeCSf05 rap.${cycle}.smokeCSf06 rap.${cycle}.smokeCSf07 rap.${cycle}.smokeCSf08 rap.${cycle}.smokeCSf09 rap.${cycle}.smokeCSf10 rap.${cycle}.smokeCSf11 rap.${cycle}.smokeCSf12 rap.${cycle}.smokeCSf13 rap.${cycle}.smokeCSf14 rap.${cycle}.smokeCSf15 rap.${cycle}.smokeCSf16 rap.${cycle}.smokeCSf17 rap.${cycle}.smokeCSf18 rap.${cycle}.smokeCSf19 rap.${cycle}.smokeCSf20 rap.${cycle}.smokeCSf21 rap.${cycle}.smokeCSf22 rap.${cycle}.smokeCSf23 rap.${cycle}.smokeCSf24 rap.${cycle}.smokeCSf25 rap.${cycle}.smokeCSf26 rap.${cycle}.smokeCSf27 rap.${cycle}.smokeCSf28 rap.${cycle}.smokeCSf29 rap.${cycle}.smokeCSf30 rap.${cycle}.smokeCSf31 rap.${cycle}.smokeCSf32 rap.${cycle}.smokeCSf33 rap.${cycle}.smokeCSf34 rap.${cycle}.smokeCSf35 rap.${cycle}.smokeCSf36 rap.${cycle}.smokeCSf37 rap.${cycle}.smokeCSf38 rap.${cycle}.smokeCSf39 rap.${cycle}.smokeCSf40 rap.${cycle}.smokeCSf41 rap.${cycle}.smokeCSf42 rap.${cycle}.smokeCSf43 rap.${cycle}.smokeCSf44 rap.${cycle}.smokeCSf45 rap.${cycle}.smokeCSf46 rap.${cycle}.smokeCSf47 rap.${cycle}.smokeCSf48 rap.${cycle}.smokeCSf49 rap.${cycle}.smokeCSf50 rap.${cycle}.smokeCSf51 > 227/rap.${cycle}.smokeCS_all
$GRB2INDEX 227/rap.${cycle}.smokeCS_all 227/rap.${cycle}.smokeCS_allI

mkdir 198
cat rap.${cycle}.smokeAKf00 rap.${cycle}.smokeAKf01 rap.${cycle}.smokeAKf02 rap.${cycle}.smokeAKf03 rap.${cycle}.smokeAKf04 rap.${cycle}.smokeAKf05 rap.${cycle}.smokeAKf06 rap.${cycle}.smokeAKf07 rap.${cycle}.smokeAKf08 rap.${cycle}.smokeAKf09 rap.${cycle}.smokeAKf10 rap.${cycle}.smokeAKf11 rap.${cycle}.smokeAKf12 rap.${cycle}.smokeAKf13 rap.${cycle}.smokeAKf14 rap.${cycle}.smokeAKf15 rap.${cycle}.smokeAKf16 rap.${cycle}.smokeAKf17 rap.${cycle}.smokeAKf18 rap.${cycle}.smokeAKf19 rap.${cycle}.smokeAKf20 rap.${cycle}.smokeAKf21 rap.${cycle}.smokeAKf22 rap.${cycle}.smokeAKf23 rap.${cycle}.smokeAKf24 rap.${cycle}.smokeAKf25 rap.${cycle}.smokeAKf26 rap.${cycle}.smokeAKf27 rap.${cycle}.smokeAKf28 rap.${cycle}.smokeAKf29 rap.${cycle}.smokeAKf30 rap.${cycle}.smokeAKf31 rap.${cycle}.smokeAKf32 rap.${cycle}.smokeAKf33 rap.${cycle}.smokeAKf34 rap.${cycle}.smokeAKf35 rap.${cycle}.smokeAKf36 rap.${cycle}.smokeAKf37 rap.${cycle}.smokeAKf38 rap.${cycle}.smokeAKf39 rap.${cycle}.smokeAKf40 rap.${cycle}.smokeAKf41 rap.${cycle}.smokeAKf42 rap.${cycle}.smokeAKf43 rap.${cycle}.smokeAKf44 rap.${cycle}.smokeAKf45 rap.${cycle}.smokeAKf46 rap.${cycle}.smokeAKf47 rap.${cycle}.smokeAKf48 rap.${cycle}.smokeAKf49 rap.${cycle}.smokeAKf50 rap.${cycle}.smokeAKf51 > 198/rap.${cycle}.smokeAK_all
$GRB2INDEX 198/rap.${cycle}.smokeAK_all 198/rap.${cycle}.smokeAK_allI

mkdir  196
cat rap.${cycle}.smokeHIf00 rap.${cycle}.smokeHIf01 rap.${cycle}.smokeHIf02 rap.${cycle}.smokeHIf03 rap.${cycle}.smokeHIf04 rap.${cycle}.smokeHIf05 rap.${cycle}.smokeHIf06 rap.${cycle}.smokeHIf07 rap.${cycle}.smokeHIf08 rap.${cycle}.smokeHIf09 rap.${cycle}.smokeHIf10 rap.${cycle}.smokeHIf11 rap.${cycle}.smokeHIf12 rap.${cycle}.smokeHIf13 rap.${cycle}.smokeHIf14 rap.${cycle}.smokeHIf15 rap.${cycle}.smokeHIf16 rap.${cycle}.smokeHIf17 rap.${cycle}.smokeHIf18 rap.${cycle}.smokeHIf19 rap.${cycle}.smokeHIf20 rap.${cycle}.smokeHIf21 rap.${cycle}.smokeHIf22 rap.${cycle}.smokeHIf23 rap.${cycle}.smokeHIf24 rap.${cycle}.smokeHIf25 rap.${cycle}.smokeHIf26 rap.${cycle}.smokeHIf27 rap.${cycle}.smokeHIf28 rap.${cycle}.smokeHIf29 rap.${cycle}.smokeHIf30 rap.${cycle}.smokeHIf31 rap.${cycle}.smokeHIf32 rap.${cycle}.smokeHIf33 rap.${cycle}.smokeHIf34 rap.${cycle}.smokeHIf35 rap.${cycle}.smokeHIf36 rap.${cycle}.smokeHIf37 rap.${cycle}.smokeHIf38 rap.${cycle}.smokeHIf39 rap.${cycle}.smokeHIf40 rap.${cycle}.smokeHIf41 rap.${cycle}.smokeHIf42 rap.${cycle}.smokeHIf43 rap.${cycle}.smokeHIf44 rap.${cycle}.smokeHIf45 rap.${cycle}.smokeHIf46 rap.${cycle}.smokeHIf47 rap.${cycle}.smokeHIf48 rap.${cycle}.smokeHIf49 rap.${cycle}.smokeHIf50 rap.${cycle}.smokeHIf51 > 196/rap.${cycle}.smokeHI_all
$GRB2INDEX 196/rap.${cycle}.smokeHI_all 196/rap.${cycle}.smokeHI_allI

for reg in CS AK HI
 do
 for type in sfc pbl
 do
   grib_input=rap.${cycle}.smoke${reg}_all
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

   echo `ls -l ${temp_grid_grib2} | awk '{print $5} '` > filesize
   export XLFRTEOPTS="unit_vars=yes"
   export FORT11=${temp_grid_grib2}
   export FORT12="filesize"
   export FORT31=
   export FORT51=${smoke_grid_grib2}
   ${TOCGRIB2SUPER} < $PARMrap/wmo/rap_grib2_${type}smoke_${grdid} >> $DATA/$pgmout 2>errfile

   cp ${smoke_grid_grib2} $COMOUT/wmo

done
done
