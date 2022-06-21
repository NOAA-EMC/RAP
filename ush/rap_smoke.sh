################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_smoke.sh
# Script description:  To generate the HYSPLIT look-alike smoke products for the Rapid Refresh Model
#
# Author:      M Hu, G Manikin /  GSL, EMC         Date: 2021-12-06
#
# Script history log:
# 2021-12-06  M Hu, G Manikin  -- new script
# 2022-06-17  B Blake          -- separate smoke records into sfc and pbl files

set -xa

fhr=$1
grid=$2
ndfdstring=$3

mkdir -p $DATA/smoke/${grid}
cd $DATA/smoke/${grid}

# smoke grids
# CONUS
export grid_specs_227="lambert:265:25:25 226.541:1473:5079 12.190:1025:5079"
# Alaska
export grid_specs_198="nps:210:60 181.429:825:5953 40.53:553:5953"
# Hawaii
export grid_specs_196="mercator:20.000000 198.475000:321:2500:206.131000 18.073000:225:2500:23.088000"

# Create subset of fields to be posted
for type in sfc pbl
do
    ${WGRIB2} -inv tmp1_${grid}_${type}.inv ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 
    grep < tmp1_${grid}_${type}.inv "`cat ${PARMrap}/rap_smokeparms_${type}`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 -grib tmp1_${grid}_${type}.grib2 

# Interpolate fields to new grid
    eval grid_specs=\${grid_specs_${grid}}
    ${WGRIB2} tmp1_${grid}_${type}.grib2 -set_bitmap 1 -set_grib_type jpeg \
       -new_grid_winds grid -new_grid_interpolation bilinear \
       -new_grid ${grid_specs} tmp_${grid}_${type}.grib2 

# Move grid to final location and index
    mv tmp_${grid}_${type}.grib2 rap.smoke${grid}${fhr}_${type}
    ${WGRIB2} rap.smoke${grid}${fhr}_${type} -s > rap.smoke${grid}${fhr}_${type}.idx

# Remove temporary files
    rm -f tmp_${grid}_${type}.inv tmp1_${grid}_${type}.grib2 tmpuv1_${grid}_${type}.grib2 tmp_${grid}_${type}.grib2 tmpuv_${grid}_${type}.grib2

    mv rap.smoke${grid}${fhr}_${type} rap.${cycle}.smoke${ndfdstring}f${fhr}_${type}

    $GRB2INDEX rap.${cycle}.smoke${ndfdstring}f${fhr}_${type} rap.${cycle}.smoke${ndfdstring}f${fhr}_${type}I

    cp rap.${cycle}.smoke${ndfdstring}f${fhr}_${type} ${DATAsmoke}/rap.${cycle}.smoke${ndfdstring}f${fhr}_${type}

done
