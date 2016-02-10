################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_smartinit.sh
# Script description:  To generate the smartinit products for the Rapid Refresh Model
#
# Author:      G Manikin /  EMC         Date: 2011-10-06
#
# Script history log:
# 2111-10-06  G Manikin  -- new script
#

set -xa

fhr=$1

mkdir -p $DATA/smart
cd $DATA/smart
/nwprod/util/ush/setup.sh

echo $fhr
export GRBINDEX=${GRBINDEX:-/nwprod/util/exec/grbindex}
export GRB2INDEX=${GRB2INDEX:-/nwprod/util/exec/grb2index}
export WGRIB2=${WGRIB2:-$HOMErap/exec/rap_wgrib2}
export utilexec=/nwprod/util/exec

cp ${COMOUT}/rap.t${cyc}z.wrfnatf${fhr}.grib2 WRFNAT${fhr}.grib2

cp ${FIXrap}/rap_conus_elevtiles.grb TOPONDFDCS
cp ${FIXrap}/rap_conus_slmask.grb LANDNDFDCS
$GRBINDEX TOPONDFDCS TOPONDFDCSI
$GRBINDEX LANDNDFDCS LANDNDFDCSI

cp ${FIXrap}/rap_smarttopoak3.grb TOPONDFDAK
cp ${FIXrap}/rap_smartmaskak3.grb LANDNDFDAK
$GRBINDEX TOPONDFDAK TOPONDFDAKI
$GRBINDEX LANDNDFDAK LANDNDFDAKI

cp $FIXrap/rap_smarttopopr.grb TOPONDFDPR
cp $FIXrap/rap_smartmaskpr.grb LANDNDFDPR
$GRBINDEX TOPONDFDPR TOPONDFDPRI
$GRBINDEX LANDNDFDPR LANDNDFDPRI

cp $FIXrap/rap_smarttopohi.grb TOPONDFDHI
cp $FIXrap/rap_smartmaskhi.grb LANDNDFDHI
$GRBINDEX TOPONDFDHI TOPONDFDHII
$GRBINDEX LANDNDFDHI LANDNDFDHII

cp ${FIXrap}/rap_smarttopoajn.grb TOPONDFDAJN
cp ${FIXrap}/rap_smartmaskajn.grb LANDNDFDAJN
/nwprod/util/exec/grbindex TOPONDFDAJN TOPONDFDAJNI
/nwprod/util/exec/grbindex LANDNDFDAJN LANDNDFDAJNI

# smartinit grids
# CONUS
export grid_specs_187="lambert:265:25.000000 233.723448:2345:2539.703000 19.228976:1597:2539.703000"
# Alaska
export grid_specs_198="nps:210.000000:60.000000 181.429000:1649:2976.563000 40.530101:1105:2976.563000"
# Hawaii
export grid_specs_196="mercator:20.000000 198.474999:321:2500.000000:206.130999 18.072699:225:2500.000000:23.087799"
# Puerto Rico
export grid_specs_194="mercator:20.000000 291.804687:177:2500.000000:296.027600 16.828685:129:2500.000000:19.747399"
# Juneau
export grid_specs_189="nps:225.000000:60.000000 217.500000:655:1488.281500 51.500000:855:1488.281500"

for grid in 187 189 198 196 194
#for grid in 187 198
do

eval grid_specs=\${grid_specs_${grid}}
# Remove temporary files
    rm -f tmp.inv tmp.grib2 tmpuv.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2

# Create subset of fields to be posted
    ${WGRIB2} -inv tmp.inv ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 
    grep < tmp.inv "`cat ${PARMrap}/rap_smartparms`" | ${WGRIB2} -i ${COMOUT}/rap.${cycle}.wrfnatf${fhr}.grib2 -grib tmp.grib2

# Merge vector field records in subset
    ${WGRIB2} tmp.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv.grib2

# Interpolate fields to new grid
    ${WGRIB2} tmpuv.grib2 -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid \
       -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" \
       -new_grid_interpolation bilinear \
       -if "`cat ${PARMrap}/rap_budget_fields.txt`" -new_grid_interpolation budget -fi \
       -if "`cat ${PARMrap}/rap_neighbor_fields.txt`" -new_grid_interpolation neighbor -fi \
       -new_grid ${grid_specs} tmp_${grid}.grib2

# Merge vector field records
    ${WGRIB2} tmp_${grid}.grib2 -new_grid_vectors "`cat ${PARMrap}/rap_vector_fields.txt`" -submsg_uv tmpuv_${grid}.grib2

# Move grid to final location and index
      mv tmpuv_${grid}.grib2 rap.NDFD${grid}${fhr}
      ${WGRIB2} rap.NDFD${grid}${fhr} -s > rap.NDFD${grid}${fhr}_idx

# Remove temporary files
    rm -f tmp.inv tmp.grib2 tmpuv.grib2 tmp_${grid}.grib2 tmpuv_${grid}.grib2
done

cp /com/date/t${cyc}z DATE
$utilexec/cnvgrib -g21 rap.NDFD187${fhr} rap.NDFDCSf${fhr}
$utilexec/cnvgrib -g21 rap.NDFD189${fhr} rap.NDFDAJNf${fhr}
$utilexec/cnvgrib -g21 rap.NDFD198${fhr} rap.NDFDAKf${fhr}
$utilexec/cnvgrib -g21 rap.NDFD196${fhr} rap.NDFDHIf${fhr}
$utilexec/cnvgrib -g21 rap.NDFD194${fhr} rap.NDFDPRf${fhr}
$GRBINDEX rap.NDFDCSf${fhr} rap.NDFDCSf${fhr}I
$GRBINDEX rap.NDFDAJNf${fhr} rap.NDFDAJNf${fhr}I
$GRBINDEX rap.NDFDAKf${fhr} rap.NDFDAKf${fhr}I
$GRBINDEX rap.NDFDHIf${fhr} rap.NDFDHIf${fhr}I
$GRBINDEX rap.NDFDPRf${fhr} rap.NDFDPRf${fhr}I

export pgm=rap_smartinit_conus
. prep_step

ln -sf rap.NDFDCSf${fhr}     fort.11
ln -sf rap.NDFDCSf${fhr}I    fort.12
ln -sf TOPONDFDCS            fort.46
ln -sf TOPONDFDCSI           fort.47
ln -sf LANDNDFDCS            fort.48
ln -sf LANDNDFDCSI           fort.49
ln -sf RAPCS${fhr}           fort.70

$EXECrap/rap_smartinit_conus <<EOF >> smartinitcs.out${fhr}
$fhr
$cyc
EOF
export err=$?; err_chk
cp RAPCS${fhr} $COMOUT/rap.t${cyc}z.smartrapconus${fhr}

mv rap.NDFDAK${fhr} rap.NDFDAKf${fhr}
$GRBINDEX rap.NDFDAKf${fhr} rap.NDFDAKf${fhr}I

export pgm=rap_smartinit_ak
. prep_step

ln -sf rap.NDFDAKf${fhr}     fort.11
ln -sf rap.NDFDAKf${fhr}I    fort.12
ln -sf TOPONDFDAK            fort.46
ln -sf TOPONDFDAKI           fort.47
ln -sf LANDNDFDAK            fort.48
ln -sf LANDNDFDAKI           fort.49
ln -sf RAPAK${fhr}           fort.70
$EXECrap/rap_smartinit_ak <<EOF >> smartinitiak.out${fhr}
$fhr
$cyc
EOF

export err=$?; err_chk
cp RAPAK${fhr} $COMOUT/rap.t${cyc}z.smartrapak${fhr}

mv rap.NDFDPR${fhr} rap.NDFDPRf${fhr}
$GRBINDEX rap.NDFDPRf${fhr} rap.NDFDPRf${fhr}I

export pgm=rap_smartinit_pr
. prep_step

ln -sf rap.NDFDPRf${fhr}     fort.11
ln -sf rap.NDFDPRf${fhr}I    fort.12
ln -sf TOPONDFDPR            fort.46
ln -sf TOPONDFDPRI           fort.47
ln -sf LANDNDFDPR            fort.48
ln -sf LANDNDFDPRI           fort.49
ln -sf RAPPR${fhr}           fort.70
$EXECrap/rap_smartinit_pr <<EOF >> smartinitipr.out${fhr}
$fhr
$cyc
EOF

export err=$?; err_chk
cp RAPPR${fhr} $COMOUT/rap.t${cyc}z.smartrappr${fhr}

mv rap.NDFDAJN${fhr} rap.NDFDAJNf${fhr}
/nwprod/util/exec/grbindex rap.NDFDAJNf${fhr} rap.NDFDAJNf${fhr}I

ln -sf rap.NDFDAJNf${fhr}     fort.11
ln -sf rap.NDFDAJNf${fhr}I    fort.12
ln -sf TOPONDFDAJN            fort.46
ln -sf TOPONDFDAJNI           fort.47
ln -sf LANDNDFDAJN            fort.48
ln -sf LANDNDFDAJNI           fort.49
ln -sf RAPAJN${fhr}           fort.70 
$EXECrap/rap_smartinit_ajn <<EOF >> smartinitiajn.out${fhr}
$fhr
$cyc
EOF

cp RAPAJN${fhr} $COMOUT/rap.t${cyc}z.smartrapajn${fhr}

mv rap.NDFDHI${fhr} rap.NDFDHIf${fhr}
/nwprod/util/exec/grbindex rap.NDFDHIf${fhr} rap.NDFDHIf${fhr}I

ln -sf rap.NDFDHIf${fhr}     fort.11
ln -sf rap.NDFDHIf${fhr}I    fort.12
ln -sf TOPONDFDHI            fort.46
ln -sf TOPONDFDHII           fort.47
ln -sf LANDNDFDHI            fort.48
ln -sf LANDNDFDHII           fort.49
ln -sf RAPHI${fhr}           fort.70 
$EXECrap/rap_smartinit_hi <<EOF >> smartinitihi.out${fhr}
$fhr
$cyc
EOF

cp RAPHI${fhr} $COMOUT/rap.t${cyc}z.smartraphi${fhr}
exit
