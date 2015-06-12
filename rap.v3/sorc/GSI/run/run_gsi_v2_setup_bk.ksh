#######
#  set up for linux cluster using PBS
######

#$ -S /bin/ksh
#$ -N GSI
#$ -cwd
#$ -r y
#$ -pe comp 4
#$ -l h_rt=0:20:00
#$ -A wrfruc


## big endian conversion options for use with Intel compilers
##  export F_UFMTENDIAN="big;little:10,15,66"
  export F_UFMTENDIAN="big;little:10,13,15,66"
  export GMPIENVVAR=F_UFMTENDIAN

  export MV2_ON_DEMAND_THRESHOLD=256

#######
#  set up for IBM AIX
######
##
## Below (IBM queueing system) commands
#BSUB -P ???????
#BSUB -a poe              
#BSUB -x                  # exlusive use of node (not_shared)
#BSUB -n   12             # number of total tasks
#BSUB -R "span[ptile=2]"  # how many tasks per node (up to 8)
#BSUB -J gsi              # job name
#BSUB -o gsi.out          # output filename (%J to add job id)
#BSUB -e gsi.err          # error filename
#BSUB -W 00:02
#BSUB -q regular          # queue

set -x

# Set environment variables for NCEP IBM
export MP_SHARED_MEMORY=yes
export MEMORY_AFFINITY=MCM
export BIND_TASKS=yes


# Set environment variables for threads
export SPINLOOPTIME=10000
export YIELDLOOPTIME=40000
export AIXTHREAD_SCOPE=S
export MALLOCMULTIHEAP=true
export XLSMPOPTS="parthds=1:spins=0:yields=0:stack=128000000"


# Set environment variables for user preferences
export XLFRTEOPTS="nlwidth=80"
export MP_LABELIO=yes

#
##################################################################################
# set up path and file that need for GSI analysis
#
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working direcotry, where GSI runs
# BK_FILE  = path and name of background file
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable 

# bkcv_option= which background error covariance and parameter will be used (GLOBAL or NAM)
# GSIPROC = processor number used for GSI analysis

######
#  Linux workstation
#####
  ANAL_TIME=2008051112
  WORK_ROOT=/d1/mhu/gsi/case
  BK_FILE=/d1/mhu/gsi/case/2008051112/bkARW/wrfout_d01_2008-05-11_12:00:00
  OBS_ROOT=/d1/mhu/gsi/case
  PREPBUFR=/d1/mhu/gsi/tutorialcases/data/newgblav.gdas1.t12z.prepbufr.nr
  FIX_ROOT=/home/mhu/GSI/comGSI/test/release_V2/fix
  GSI_EXE=/home/mhu/GSI/comGSI/test/release_V2/run/gsi.exe


  bk_core=ARW
  bkcv_option=NAM
  GSIPROC=1
  BYTE_ORDER=Big_Endian

######
#  Bluefire
#####
  ANAL_TIME=2007081500
  WORK_ROOT=/ptmp/mhu/test_Q1FY09/run/tmpreg_arw_${ANAL_TIME}
  PREPBUFR=/ptmp/mhu/blueice/t8/obs/gdas1.t00z.prepbufr.nr
  BK_FILE=/ptmp/mhu/blueice/t8/t8_case_2007081500/wrfinput_d01_2007081500
  OBS_ROOT=/ptmp/mhu/blueice/t8/obs
  FIX_ROOT=/ptmp/mhu/test_Q1FY09/fix
  GSI_EXE=/ptmp/mhu/test_Q1FY09/sorc/global_gsi.fd/global_gsi

  bk_core=ARW
  bkcv_option=NAM
  GSIPROC=12
  BYTE_ORDER=Little_Endian

######
#  Liunx cluster
#####
# ANAL_TIME= analysis time  (YYYYMMDDHH)
# WORK_ROOT= working direcotry, where GSI runs
# BK_FILE  = path and name of background file
# OBS_ROOT = path of observations files
# FIX_ROOT = path of fix files
# GSI_EXE  = path and name of the gsi executable

  ANAL_TIME=2008051112
  WORK_ROOT=arw_${ANAL_TIME}
  PREPBUFR=/lfs0/projects/wrfruc/mhu/save/tg0/tutorialcases/DOMAINS/May11ARW_cycle/2008051112_org/obsprd/newgblav.2
0080511.ruc2a.t12z.prepbufr
##  BK_FILE=/lfs0/projects/wrfruc/mhu/save/tg0/tutorialcases/DOMAINS/May11NMM_cycle/boundary/wrfinput_d01_2008-05-1
1_12:00:00
  BK_FILE=/lfs0/projects/wrfruc/mhu/save/tg0/tutorialcases/DOMAINS/May11ARW_cycle/boundary/wrfout_d01_2008-05-11_12
:00:00
  OBS_ROOT=/lfs0/projects/wrfruc/mhu/save/tg0/tutorialcases/DOMAINS/May11ARW_cycle/2008051112_org/obsprd
  FIX_ROOT=/mnt/lfs0/projects/wrfruc/mhu/GSI/comGSI/test/release_V2/fix
  GSI_EXE=/mnt/lfs0/projects/wrfruc/mhu/GSI/comGSI/test/release_V2/run/gsi.exe

# bk_core= which WRF core is used as background (NMM or ARW)
# bkcv_option= which background error covariance and parameter will be used
#              (GLOBAL or NAM)
# GSIPROC = processor number used for GSI analysis
# BYTE_ORDER= set up the order of bite for reading CRTM coefficient
#          Linux pgi = Big_Endian
#          IBM = Little_Endian
#          linux ifort =  Little_Endian
  bk_core=ARW
  bkcv_option=NAM
  GSIPROC=4
  BYTE_ORDER=Little_Endian
#


