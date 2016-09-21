#!/bin/sh -l
#########################################################################
#									#
# Script:  rap_bfr2gpk							#
#									#
#  This script reads RAP BUFR output and transfers it into GEMPAK	#
#  surface and sounding data files.					#
#									#
# Log:									#
# K. Brill/HPC		11/28/01					#
# G. Manikin/EMC        10/25/11  adapt for Rapid Refresh               #
#########################################################################  
set -x


# Set GEMPAK paths.

. /nwprod/gempak/.gempak

#  Go to a working directory.

cd $DATA
cp $PARMrap/rap_snrap.prm snrap.prm
cp $PARMrap/rap_sfrap.prm_aux sfrap.prm_aux
cp $PARMrap/rap_sfrap.prm sfrap.prm

#  Set input file name.

INFILE=$COMOUT/rap.t${cyc}z.class1.bufr.tm00
export INFILE

#  Set output directory:

OUTDIR=${COMAWP:-/ptmp/Geoffrey.Manikin/com/nawips/para/rap.${PDY}}

outfilbase=rap_${PDY}${cyc}

namsnd << EOF > /dev/null
SNBUFR   = $INFILE
SNOUTF   = ${outfilbase}.snd
SFOUTF   = ${outfilbase}.sfc+
SNPRMF   = snrap.prm
SFPRMF   = sfrap.prm
TIMSTN   = 85/2000
r

exit
EOF

/bin/rm *.nts

snd=${outfilbase}.snd
sfc=${outfilbase}.sfc
aux=${outfilbase}.sfc_aux
cp $snd $OUTDIR/.$snd
cp $sfc $OUTDIR/.$sfc
cp $aux $OUTDIR/.$aux
mv $OUTDIR/.$snd $OUTDIR/$snd
mv $OUTDIR/.$sfc $OUTDIR/$sfc
mv $OUTDIR/.$aux $OUTDIR/$aux

if [ "$SENDDBN" = 'YES' ] ; then
  $DBNROOT/bin/dbn_alert MODEL SFC_RAP $job $OUTDIR/$sfc
  $DBNROOT/bin/dbn_alert MODEL SFC_RAP $job $OUTDIR/$aux
  $DBNROOT/bin/dbn_alert MODEL SND_RAP $job $OUTDIR/$snd
fi

