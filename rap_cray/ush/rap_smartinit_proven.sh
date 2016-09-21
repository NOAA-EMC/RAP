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

export tmmark=tm00
export GRBINDEX=${GRBINDEX:-/nwprod/util/exec/grbindex}
export GRB2INDEX=${GRB2INDEX:-/nwprod/util/exec/grb2index}
export WGRIB2=${WGRIB2:-/meso/save/Eric.Rogers/utils/wgrib2_wesley_fornam/wgrib2new}

#cp ${COMIN}/rap.t${cyc}z.wrfnat${fhr}.tm00 WRFNAT${fhr}.tm00
cp ${COMIN}/rap.t${cyc}z.wrfnatf${fhr}.grib2 WRFNAT${fhr}.tm00.grib2
$utilexec/cnvgrib -g21 WRFNAT${fhr}.tm00.grib2 WRFNAT${fhr}.tm00
#$GRBINDEX WRFNAT${fhr}.tm00 WRFNAT${fhr}i.tm00

cp ${PARMrap}/rap_mastersmart.ctl master${fhr}.ctl
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
$GRBINDEX TOPONDFDPR TOPONDFDHII
$GRBINDEX LANDNDFDPR LANDNDFDHII

cp ${FIXrap}/rap_smarttopoajn.grb TOPONDFDAJN
cp ${FIXrap}/rap_smartmaskajn.grb LANDNDFDAJN
/nwprod/util/exec/grbindex TOPONDFDAJN TOPONDFDAJNI
/nwprod/util/exec/grbindex LANDNDFDAJN LANDNDFDAJNI

cat >input${fhr}.prd <<EOF5
WRFNAT${fhr}.tm00
$fhr
EOF5

export gridspecs_187="lambert:265:25.000000 233.723448:2345:2539.703000 19.228976:1597:2539.703000"

export pgm=rap_prdgen
. prep_step

ln -sf master${fhr}.ctl         fort.10
ln -sf ${FIXrap}/rap_wgt_91     fort.21
ln -sf ${FIXrap}/rap_wgt_187    fort.22
ln -sf ${FIXrap}/rap_wgt_189    fort.23
ln -sf ${FIXrap}/rap_wgt_195    fort.24
ln -sf input${fhr}.prd          fort.621
${EXECrap}/rap_prdgen > prdgen.out${fhr}
export err=$?; #err_chk
#
echo 'complete prdgen'

$GRB2INDEX WRFNAT${fhr}.tm00.grib2 WRFNATi
${WGRIB2} WRFNAT${fhr}.tm00.grib2 | grep -F -f ${PARMrap}/rap_smartparms | ${WGRIB2} -i -grib tmpfile WRFNAT${fhr}.tm00.grib2
${WGRIB2} tmpfile -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv inputs.grib.uv
$WGRIB2 inputs.grib.uv -set_bitmap 1 -set_grib_type c3 -new_grid_winds grid -new_grid_vectors "UGRD:VGRD:USTM:VSTM" \
    -new_grid_interpolation bilinear -if ":(WEASD|APCP|NCPCP|ACPCP|SNOD):" -new_grid_interpolation budget -fi \
    -if ":(TMP:surface|VEG|CCOND|SFEXC|PRES:tropopause|LAI|HPBL|HGT:planetary boundary layer):" -new_grid_interpolation neighbor -fi \
    -new_grid ${gridspecs_187} rap.t${cyc}z.awp187${fhr}.tm00.uv
$WGRIB2 rap.t${cyc}z.awp187${fhr}.tm00.uv -new_grid_vectors "UGRD:VGRD:USTM:VSTM" -submsg_uv rap.NDFDCS${fhr}.tm00.grib2
$utilexec/cnvgrib -g21 rap.NDFDCS${fhr}.tm00.grib2 rap.NDFDCS${fhr}.tm00

#$GRB2INDEX WRFNAT${fhr}.tm00.grib2 WRFNATi
#$utilexec/copygb2 -g "91" -x WRFNAT${fhr}.tm00.grib2 rap.NDFDAK${fhr}.tm00
#$utilexec/copygb2 -g "189" -x WRFNAT${fhr}.tm00.grib2 rap.NDFDAJN${fhr}.tm00
#$utilexec/copygb2 -g "195" -x WRFNAT${fhr}.tm00.grib2 rap.NDFDPR${fhr}.tm00
#$utilexec/copygb2 -g "$g2id187" -i0 WRFNAT${fhr}.tm00.grib2 WRFNATi rap.NDFDCS${fhr}.tm00



mv rap.NDFDCS${fhr}.tm00 rap.NDFDCSf${fhr}
$GRBINDEX rap.NDFDCSf${fhr} rap.NDFDCSf${fhr}I

cp /com/date/t${cyc}z DATE

export pgm=rap_smartinit_conus
. prep_step

ln -sf rap.NDFDCSf${fhr}     fort.11
ln -sf rap.NDFDCSf${fhr}I    fort.12
ln -sf TOPONDFDCS            fort.46
ln -sf TOPONDFDCSI           fort.47
ln -sf LANDNDFDCS            fort.48
ln -sf LANDNDFDCSI           fort.49
ln -sf RAPCS${fhr}.tm00      fort.70

$EXECrap/rap_smartinit_conus <<EOF >> smartinitcs.out${fhr}
$fhr
$cyc
EOF
export err=$?; #err_chk
cp RAPCS${fhr}.tm00 $COMOUT/rap.t${cyc}z.smartrapconus${fhr}

mv rap.NDFDAK${fhr}.tm00 rap.NDFDAKf${fhr}
$GRBINDEX rap.NDFDAKf${fhr} rap.NDFDAKf${fhr}I

export pgm=rap_smartinit_ak
. prep_step

ln -sf rap.NDFDAKf${fhr}     fort.11
ln -sf rap.NDFDAKf${fhr}I    fort.12
ln -sf TOPONDFDAK            fort.46
ln -sf TOPONDFDAKI           fort.47
ln -sf LANDNDFDAK            fort.48
ln -sf LANDNDFDAKI           fort.49
ln -sf RAPAK${fhr}.tm00      fort.70
$EXECrap/rap_smartinit_ak <<EOF >> smartinitiak.out${fhr}
$fhr
$cyc
EOF

export err=$?; #err_chk
cp RAPAK${fhr}.tm00 $COMOUT/rap.t${cyc}z.smartrapak${fhr}

mv rap.NDFDPR${fhr}.tm00 rap.NDFDPRf${fhr}
$GRBINDEX rap.NDFDPRf${fhr} rap.NDFDPRf${fhr}I

export pgm=rap_smartinit_pr
. prep_step

ln -sf rap.NDFDPRf${fhr}     fort.11
ln -sf rap.NDFDPRf${fhr}I    fort.12
ln -sf TOPONDFDPR            fort.46
ln -sf TOPONDFDPRI           fort.47
ln -sf LANDNDFDPR            fort.48
ln -sf LANDNDFDPRI           fort.49
ln -sf RAPPR${fhr}.tm00      fort.70
$EXECrap/rap_smartinit_pr <<EOF >> smartinitipr.out${fhr}
$fhr
$cyc
EOF
export err=$?; #err_chk
cp RAPPR${fhr}.tm00 $COMOUT/rap.t${cyc}z.smartrappr${fhr}

mv rap.NDFDAJN${fhr}.tm00 rap.NDFDAJNf${fhr}
/nwprod/util/exec/grbindex rap.NDFDAJNf${fhr} rap.NDFDAJNf${fhr}I

ln -sf rap.NDFDAJNf${fhr}     fort.11
ln -sf rap.NDFDAJNf${fhr}I    fort.12
ln -sf TOPONDFDAJN            fort.46
ln -sf TOPONDFDAJNI           fort.47
ln -sf LANDNDFDAJN            fort.48
ln -sf LANDNDFDAJNI           fort.49
ln -sf RAPAJN${fhr}.tm00      fort.70 
$EXECrap/rap_smartinit_ajn <<EOF >> smartinitiajn.out${fhr}
$fhr
$cyc
EOF

cp RAPAJN${fhr}.tm00 $COMOUT/rap.t${cyc}z.smartrapajn${fhr}
exit
