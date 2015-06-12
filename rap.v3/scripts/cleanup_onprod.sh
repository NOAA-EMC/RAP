set -x

export cyc=$1
export PDY=$2
export PDYp1=$3

export cycle=t${cyc}z

##/nwprod/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/nam/test 5 ndas
##/nwprod/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/nam/test 6 nam
###/nwprod/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/dgex/test 10 dgex
##/nwprod/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/hourly/test 4 nam_pcpn_anal

/meso/save/Eric.Rogers/namplls/nwpara2/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/nam/test 5 ndas
/meso/save/Eric.Rogers/namplls/nwpara2/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/nam/test 6 nam
###/nwprod/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/dgex/test 10 dgex
/meso/save/Eric.Rogers/namplls/nwpara2/util/ush/cleanup_rmdir.sh $PDY /meso/noscrub/Eric.Rogers/com/hourly/test 4 nam_pcpn_anal
