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
export FIXnam=${FIXnam:-/nwprod/fix}
export PARMnam=${PARMnam:-/nwprod/parm}

cp ${COMIN}/rap.t${cyc}z.wrfnat${fhr}.tm00 WRFNAT${fhr}.tm00
$GRBINDEX WRFNAT${fhr}.tm00 WRFNAT${fhr}i.tm00

cp ${PARMrap}/rap_mastersmart.ctl master${fhr}.ctl
cp ${FIXrap}/rap_smarttopoconus2p5.grb TOPONDFDCS
cp ${FIXrap}/rap_smartmaskconus2p5.grb LANDNDFDCS
$GRBINDEX TOPONDFDCS TOPONDFDCSI
$GRBINDEX LANDNDFDCS LANDNDFDCSI

cp ${FIXnam}/nam_smarttopoak3.grb TOPONDFDAK
cp ${FIXnam}/nam_smartmaskak3.grb LANDNDFDAK
$GRBINDEX TOPONDFDAK TOPONDFDAKI
$GRBINDEX LANDNDFDAK LANDNDFDAKI

cp $FIXnam/nam_smarttopopr.grb TOPONDFDPR
cp $FIXnam/nam_smartmaskpr.grb LANDNDFDPR
$GRBINDEX TOPONDFDPR TOPONDFDPRI
$GRBINDEX LANDNDFDPR LANDNDFDPRI

cp ${FIXrap}/rap_smarttopoajn.grb TOPONDFDAJN
cp ${FIXrap}/rap_smartmaskajn.grb LANDNDFDAJN
/nwprod/util/exec/grbindex TOPONDFDAJN TOPONDFDAJNI
/nwprod/util/exec/grbindex LANDNDFDAJN LANDNDFDAJNI

cat >input${fhr}.prd <<EOF5
WRFNAT${fhr}.tm00
$fhr
EOF5

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

echo 'complete prdgen'

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
