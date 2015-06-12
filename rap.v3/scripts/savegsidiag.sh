#!/bin/ksh
set -x

# Set script variables
DATA=/ptmpd3/Corey.Guastini/rap_anl_prod_12/gsiprd
CDATE=2014031812

PDY=`echo $CDATE | cut -c1-8`
cyc=`echo $CDATE | cut -c9-10`

CNVSTAT=/ptmpd3/Corey.Guastini/rap.t${cyc}z.cnvstat
PCPSTAT=/dev/hull
OZNSTAT=/dev/null
RADSTAT=/ptmpd3/Corey.Guastini/rap.t${cyc}z.radstat

lrun_subdir=.false.
wc=/usr/bin/wc
DIAG_SUFFIX=""
DIAG_COMPRESS=YES
COMPRESS=gzip
DIAG_TARBALL=YES


# Scripting below taken from /nwprod/ush/exglobal_analysis.sh.ecf
#-------------------------------------------------------------------------


# Loop over first and last outer loops to generate innovation
# diagnostic files for indicated observation types (groups)
#
# NOTE:  Since we set miter=2 in GSI namelist SETUP, outer
#        loop 03 will contain innovations with respect to 
#        the analysis.  Creation of o-a innovation files
#        is triggered by write_diag(3)=.true.  The setting
#        write_diag(1)=.true. turns on creation of o-g
#        innovation files.
#

cd $DATA    # we should already be in $DATA, but extra cd to be sure.

# Set up lists and variables for various types of diagnostic files.
ntype=3

diagtype[0]="conv"
diagtype[1]="pcp_ssmi_dmsp pcp_tmi_trmm"
diagtype[2]="sbuv2_n16 sbuv2_n17 sbuv2_n18 sbuv2_n19 gome_metop-a gome_metop-b omi_aura mls_aura"
diagtype[3]="hirs2_n14 msu_n14 sndr_g08 sndr_g11 sndr_g12 sndr_g13 sndr_g08_prep sndr_g11_prep sndr_g12_prep sndr_g13_prep sndrd1_g11 sndrd2_g11 sndrd3_g11 sndrd4_g11 sndrd1_g12 sndrd2_g12 sndrd3_g12 sndrd4_g12 sndrd1_g13 sndrd2_g13 sndrd3_g13 sndrd4_g13 sndrd1_g14 sndrd2_g14 sndrd3_g14 sndrd4_g14 sndrd1_g15 sndrd2_g15 sndrd3_g15 sndrd4_g15 hirs3_n15 hirs3_n16 hirs3_n17 amsua_n15 amsua_n16 amsua_n17 amsub_n15 amsub_n16 amsub_n17 hsb_aqua airs_aqua amsua_aqua imgr_g08 imgr_g11 imgr_g12 imgr_g14 imgr_g15 ssmi_f13 ssmi_f14 ssmi_f15 hirs4_n18 hirs4_metop-a amsua_n18 amsua_metop-a mhs_n18 mhs_metop-a amsre_low_aqua amsre_mid_aqua amsre_hig_aqua ssmis_las_f16 ssmis_uas_f16 ssmis_img_f16 ssmis_env_f16 ssmis_las_f17 ssmis_uas_f17 ssmis_img_f17 ssmis_env_f17 ssmis_las_f18 ssmis_uas_f18 ssmis_img_f18 ssmis_env_f18 ssmis_las_f19 ssmis_uas_f19 ssmis_img_f19 ssmis_env_f19 ssmis_las_f20 ssmis_uas_f20 ssmis_img_f20 ssmis_env_f20 iasi_metop-a hirs4_n19 amsua_n19 mhs_n19 seviri_m08 seviri_m09 seviri_m10 cris_npp atms_npp hirs4_metop-b amsua_metop-b mhs_metop-b iasi_metop-b"

diaglist[0]=listcnv
diaglist[1]=listpcp
diaglist[2]=listozn
diaglist[3]=listrad

diagfile[0]=$CNVSTAT
diagfile[1]=$PCPSTAT
diagfile[2]=$OZNSTAT
diagfile[3]=$RADSTAT

numfile[0]=0
numfile[1]=0
numfile[2]=0
numfile[3]=0


# Set diagnostic file prefix based on lrun_subdirs variable
if [ $lrun_subdirs = ".true." ]; then
   prefix=" dir.*/"
else
   prefix="pe*"
fi


# Collect diagnostic files as a function of loop and type.
loops="01 03"
for loop in $loops; do
   case $loop in
      01) string=ges;;
      03) string=anl;;
       *) string=$loop;;
   esac
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      for type in `echo ${diagtype[n]}`; do
         count=`ls ${prefix}${type}_${loop}* | $wc -l`
         if [ $count -gt 0 ]; then
            cat ${prefix}${type}_${loop}* > diag_${type}_${string}.${CDATE}${DIAG_SUFFIX}
            echo "diag_${type}_${string}.${CDATE}*" >> ${diaglist[n]}
            numfile[n]=`expr ${numfile[n]} + 1`
         fi
      done
   done
done

cd $DATA    # we should already be in $DATA, but extra cd to be sure.

# If requested, compress diagnostic files
if [[ $DIAG_COMPRESS = YES ]]; then
   for file in `ls diag_*${CDATE}${DIAG_SUFFIX}`; do
      $COMPRESS $file
   done
fi

# If requested, create diagnostic file tarballs
if [[ $DIAG_TARBALL = YES ]]; then
   n=-1
   while [ $((n+=1)) -le $ntype ] ;do
      TAROPTS="-uvf"
      if [ ! -s ${diagfile[n]} ]; then
         TAROPTS="-cvf"
      fi
      if [ ${numfile[n]} -gt 0 ]; then
         tar $TAROPTS ${diagfile[n]} `cat ${diaglist[n]}`
      fi
   done

#  Restrict CNVSTAT 
   chmod 750 $CNVSTAT
   chgrp rstprod $CNVSTAT
fi
