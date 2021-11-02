################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_smartinit.sh
# Script description:  To generate the smartinit products for the Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2011-10-06
#
# Script history log:
# 2011-10-06  G Manikin  -- new script
# 2018-01-29  B Blake / C Guastini - RAPv4
# 2019-10-18  A Gibbs - RAPv5

set -xa

fhr=$1
grid=$2
ndfdstring=$3

mkdir -p $DATA/smart/${grid}
cd $DATA/smart/${grid}

echo $fhr

case $ndfdstring in
   CS) domain=conus
       cp ${FIXrap}/rap_conus_elevtiles.grib2 TOPONDFDCS
       cp ${FIXrap}/rap_conus_slmask.grib2 LANDNDFDCS
       $GRB2INDEX TOPONDFDCS TOPONDFDCSI
       $GRB2INDEX LANDNDFDCS LANDNDFDCSI ;;
   AK) domain=ak
       cp ${FIXrap}/rap_smarttopoak3.grb2 TOPONDFDAK
       cp ${FIXrap}/rap_smartmaskak3.grb2 LANDNDFDAK
       $GRB2INDEX TOPONDFDAK TOPONDFDAKI
       $GRB2INDEX LANDNDFDAK LANDNDFDAKI ;;
   PR) domain=pr
       cp $FIXrap/rap_smarttopopr.grib2 TOPONDFDPR
       cp $FIXrap/rap_smartmaskpr.grib2 LANDNDFDPR
       $GRB2INDEX TOPONDFDPR TOPONDFDPRI
       $GRB2INDEX LANDNDFDPR LANDNDFDPRI ;;
   HI) domain=hi
       cp $FIXrap/rap_smarttopohi.grib2 TOPONDFDHI
       cp $FIXrap/rap_smartmaskhi.grib2 LANDNDFDHI
       $GRB2INDEX TOPONDFDHI TOPONDFDHII
       $GRB2INDEX LANDNDFDHI LANDNDFDHII ;;
esac
# smartinit grids
# CONUS
export grid_specs_187="lambert:265:25.000000 233.723448:2345:2539.703000 19.228976:1597:2539.703000"
# Alaska
export grid_specs_198="nps:210.000000:60.000000 181.429000:1649:2976.563000 40.530101:1105:2976.563000"
# Hawaii
export grid_specs_196="mercator:20.000000 198.474999:321:2500.000000:206.130999 18.072699:225:2500.000000:23.087799"
# Puerto Rico Original 2.5 km grid
#export grid_specs_194="mercator:20.000000 291.804687:177:2500.000000:296.027600 16.828685:129:2500.000000:19.747399"
# Puerto Rico New 1.25 km grid
export grid_specs_194="mercator:20.000000 291.804700:353:1250.000000:296.015500 16.828700:257:1250.000000:19.736200"

# Remove temporary files
    rm -f tmp_${grid}.inv tmp1_${grid}.grib2 tmpuv1_${grid}.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

# Create subset of fields to be posted
    ${WGRIB2} -inv tmp1_${grid}.inv ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 

    grep < tmp1_${grid}.inv "`cat ${PARMrap}/rap_smartparms`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 -grib tmp1_${grid}.grib2 

# Merge vector field records in subset
    ${WGRIB2} tmp1_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv1_${grid}.grib2

# Interpolate fields to new grid
eval grid_specs=\${grid_specs_${grid}}
    ${WGRIB2} tmpuv1_${grid}.grib2 -set_bitmap 1 -set_grib_type jpeg -new_grid_winds grid \
       -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" \
       -new_grid_interpolation bilinear \
       -if "`cat ${PARMrap}/rap_budget_fields.txt`" -new_grid_interpolation budget -fi \
       -if "`cat ${PARMrap}/rap_neighbor_fields.txt`" -new_grid_interpolation neighbor -fi \
       -new_grid ${grid_specs} tmp_${grid}.grib2 

# Merge vector field records
    ${WGRIB2} tmp_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv_${grid}.grib2

# Move grid to final location and index
      mv tmpuv_${grid}.grib2 rap.NDFD${grid}${fhr}
      ${WGRIB2} rap.NDFD${grid}${fhr} -s > rap.NDFD${grid}${fhr}.idx

# Remove temporary files
    rm -f tmp_${grid}.inv tmp1_${grid}.grib2 tmpuv1_${grid}.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

cp ../../DATE_SMARTINIT DATE

mv rap.NDFD${grid}${fhr} rap.NDFD${ndfdstring}f${fhr}

$GRB2INDEX rap.NDFD${ndfdstring}f${fhr} rap.NDFD${ndfdstring}f${fhr}I

export pgm=rap_smartinit
. prep_step

ln -sf rap.NDFD${ndfdstring}f${fhr}     fort.11
ln -sf rap.NDFD${ndfdstring}f${fhr}I    fort.12
ln -sf TOPONDFD${ndfdstring}            fort.46
ln -sf TOPONDFD${ndfdstring}I           fort.47
ln -sf LANDNDFD${ndfdstring}            fort.48
ln -sf LANDNDFD${ndfdstring}I           fort.49
ln -sf RAP${ndfdstring}${fhr}           fort.70
ln -sf RAP${ndfdstring}${fhr}.grib2     fort.71

cp ${EXECrap}/rap_smartinit .
runline="mpiexec -n 1 -ppn 1 rap_smartinit"
rap_smartinit <<EOF >> smartinit${domain}.out${fhr}
RAP
$ndfdstring
GRIB2
$fhr
$cyc
EOF

export err=$?; err_chk
cp RAP${ndfdstring}${fhr}.grib2 $COMOUT/rap.t${cyc}z.smartrap${domain}f${fhr}.grib2
