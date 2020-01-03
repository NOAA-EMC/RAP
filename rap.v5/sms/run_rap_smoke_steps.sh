#! /bin/bash --login

#BSUB -J rap-smoke-steps
#BSUB -o rap-smoke-steps-%J.log
#BSUB -q dev
#BSUB -P RAP-T2O
#BSUB -cwd .
#BSUB -M 2000
#BSUB -extsched 'CRAYLINUX[]'
#BSUB -W 01:59
#BSUB -L /bin/sh
export NODES=1

set -xue

# For a given cycle, runs the JRAP_PREP_SMOKE to generate a prepared
# smoke binary file, and then runs the rap_smoke_wrfinput.ksh to
# pasted the data into the wrfinput_d01.  The script requires a date,
# a scrub space directory, and the location of the wrfinput_d01 file.

YYYYMMDDHH="${1:-2019061318}"
SCRUB_SPACE="${2:-/gpfs/hps2/ptmp/$USER/rap-smoke-steps}"
WRFINPUT="${3:-/gpfs/dell2/emc/obsproc/noscrub/Samuel.Trahan/prep_chem/wrfinput/RAPX/cycle/$YYYYMMDDHH/wrfprd/wrfinput_d01}"

#################################
# Set up mandatory NCEP variables
#################################

export PDY="${YYYYMMDDHH:0:8}"
export cyc="${YYYYMMDDHH:8:2}"
export cycle="t${cyc}z"

hex=$( printf '%02x%02x%02x%02x' \
    $(( RANDOM % 256 )) \
    $(( RANDOM % 256 )) \
    $(( RANDOM % 256 )) \
    $(( RANDOM % 256 )) )
export DATAROOT="$SCRUB_SPACE/test.$hex"

export HOMErap=$( cd .. ; pwd -P )
export EXECrap=$HOMErap/exec

test -d "$EXECrap"

export gespath=$DATAROOT/gespath
export COMROOT=$DATAROOT/com

export envir=prod
export NET=rap
export RUN=rap

##################################
# Locations of observations on Jet
##################################

if [[ -d /lfs1 ]] ; then
    # Real-time paths on Jet
    export OBS_DIR=/public/data
    export MODIS_DIR="$OBS_DIR/sat/firms/global"
    export DCOM_VIIRS_NPP="$OBS_DIR/sat/nesdis/viirs/fire_pda"
    export DCOM_VIIRS_J01="$OBS_DIR/sat/nesdis/noaa20/fire_pda"
else
    # Dev areas on WCOSS Cray
    export MODIS_DIR="/gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/MODISfiredata/datafiles/FIRMS/c6/Global/"
    export DCOM_VIIRS_NPP="/gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/dcom_af_viirs/prod/"
    export DCOM_VIIRS_J01="/gpfs/dell2/emc/obsproc/noscrub/Sudhir.Nadiga/dcom_af_viirs/prod/"
fi

#####################################
# Make our scrub space and cd into it
#####################################

mkdir -p "$DATAROOT"
cd "$DATAROOT"

###############
# Load modules
###############

set +xu
module purge

if [[ -d /lfs1 ]] ; then
    # Jet modules
    module load szip/2.1
    module load intel/18.0.5.274
    module load impi/2018.4.274
    module load pnetcdf/1.6.1
    module load hdf5/1.8.9
    module load netcdf/4.2.1.1

    module use /lfs3/projects/hwrf-vd/soft/modulefiles/
    module load prod_util   # for setpdy.sh
else
    # WCOSS Cray modules
    module load modules
    module load switch
    module load craype-network-aries
    module load ncep/1.0
    module load xt-lsfhpc
    module load craype
    module load PrgEnv-intel
    module load craype-sandybridge

    module load intel
    module load NetCDF-intel-haswell/4.2
    module load HDF5-serial-intel-haswell/1.8.9

    module load prod_util   # for setpdy.sh

    # Modules needed for parallel execution at runtime:
    module load rca
    module load alps
    module load xpmem
    module load gni-headers
    module load udreg
    module load ugni

    module load mpiserial
fi

set -xu

############################
# Process smoke observations
############################

"$HOMErap/jobs/JRAP_PREP_SMOKE"

###############################################
# Create a work area for rap_smoke_wrfinput.ksh
###############################################

export COMOUT=${COMOUT:-${COMROOT}/${NET}/${envir}/${RUN}.${PDY}}

mkdir "$DATAROOT/rap_smoke_wrfinput"
cd "$DATAROOT/rap_smoke_wrfinput"
cp -fp "$WRFINPUT" ./wrfinput_d01
$HOMErap/ush/rap_smoke_wrfinput.ksh

##########
# Rejoice
##########

exit 0
