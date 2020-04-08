#!/bin/ksh -l
################################################################################
####  UNIX Script Documentation Block
#                      .                                             .
# Script name:         rap_grib2sfc.sh
# Script description:  To generate a subset of grib2 file
#
# Author:      Ming Hu /  GSD        Date: 2019-11-13
#
# Script history log:
# 2019-11-13  M Hu 

set -xa

cycle=$1
fhr=$2
filein=$3
fileout=wrftwo.grib2

#mkdir -p $DATA/smart/${grid}
#cd $DATA/smart/${grid}

echo "rap_grib2sfc.sh cycle= ${cycle}"
echo "rap_grib2sfc.sh fhr  = ${fhr}"
echo "rap_grib2sfc.sh file = ${filein}"

set -A wrftwo_files VIS:surfac          \
                    GUST:surface        \
                    PRES:surface        \
                    HGT:surface         \
                    TMP:surface         
#                    ASNOW:surface       \
#                    CNWAT:surface       \
#                    WEASD:surface       \
#                    SNOD:surface       \
#                    PRATE:surface       \
#                    APCP:surface       \
#                    WEASD:surface       \
#                    FROZR:surface       \
#                    SSRUN:surface       \
#                    BGRUN:surface       \
#                    ACPCP:surface       \
#                    CSNOW:surface


$WGRIB2 -s ${filein} | grep 'REFC:entire atmosphere'  | $WGRIB2 -i ${filein} -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'MSLMA:mean sea level'    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}

for field in ${wrftwo_files[@]}; do
  $WGRIB2 -s ${filein} | grep ${field}    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
done

#surfacelist="CICEP CFRZR CRAIN SFCR FRICVW SHTFL LHTFL GFLUX VGTYP SOTYP DSWRF DLWRF USWRF ULWRF CFNSF VBDSF VDDSF HPBL LTNG LAND HPBL"
#
#for field in ${surfacelist}; do
#  $WGRIB2 -s ${filein} | grep ${field}:surface    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#done

#$WGRIB2 -s ${filein} | grep 'TSOIL:0-0 m below ground'    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'TMP:2 m above ground'    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'SPFH:2 m above ground'    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'DPT:2 m above ground'    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'MASSDEN:8 m above ground'    | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'UGRD:10 m above ground'  | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'VGRD:10 m above ground'  | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'HGT:lowest level of the wet bulb zero' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'PWAT:entire atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'AOTK:entire atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'COLMD:entire atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'TCOLWold:entire atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'TCOLIold:entire atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'LCDC:low cloud layer' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'MCDC:middle cloud layer' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'HCDC:high cloud layer' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'TCDC:entire atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'HGT:cloud ceiling' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'HGT:cloud base' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'PRES:cloud base' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'PRES:cloud top' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'HGT:cloud top' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'TMP:cloud top' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'ULWRF:top of atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'USWRF:top of atmosphere' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'PRES:tropopause' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'POT:tropopause' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'UGRD:tropopause' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep 'VGRD:tropopause' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
#$WGRIB2 -s ${filein} | grep '4LFTX:180-0 mb above ground' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'CAPE:255-0 mb above ground' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}
$WGRIB2 -s ${filein} | grep 'CIN:255-0 mb above ground' | $WGRIB2 -i ${filein} -append -grib_out ${fileout}

exit 0
