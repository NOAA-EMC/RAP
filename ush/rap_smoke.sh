################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_smartinit.sh
# Script description:  To generate the smartinit products for the Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2011-10-06
#
# Script history log:
# 2021-11-10  G Manikin  -- new script

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
export grid_specs_198="nps:210.000000:60.000000 181.429000:1649:2976.563000 40.530101:1105:2976.563000"
# Hawaii
export grid_specs_196="mercator:20.000000 198.474999:321:2500.000000:206.130999 18.072699:225:2500.000000:23.087799"


# Create subset of fields to be posted
    ${WGRIB2} -inv tmp1_${grid}.inv ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 

    grep < tmp1_${grid}.inv "`cat ${PARMrap}/rap_smokeparms`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 -grib tmp1_${grid}.grib2 

# Interpolate fields to new grid
eval grid_specs=\${grid_specs_${grid}}
    ${WGRIB2} tmp1_${grid}.grib2 -set_bitmap 1 -set_grib_type jpeg -new_grid_winds grid \
       -new_grid_interpolation bilinear \
       -new_grid ${grid_specs} tmp_${grid}.grib2 

# Move grid to final location and index
      mv tmp_${grid}.grib2 rap.smoke${grid}${fhr}
      ${WGRIB2} rap.smoke${grid}${fhr} -s > rap.smoke${grid}${fhr}.idx

# Remove temporary files
    rm -f tmp_${grid}.inv tmp1_${grid}.grib2 tmpuv1_${grid}.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

mv rap.smoke${grid}${fhr} rap.${cycle}.smoke${ndfdstring}f${fhr}

$GRB2INDEX rap.${cycle}.smoke${ndfdstring}f${fhr} rap.${cycle}.smoke${ndfdstring}f${fhr}I

cp RAP${ndfdstring}${fhr}.grib2 rap.t${cyc}z.smoke${domain}f${fhr}.grib2
#cp RAP${ndfdstring}${fhr}.grib2 $DATAsmoke/rap.t${cyc}z.smoke${domain}f${fhr}.grib2
