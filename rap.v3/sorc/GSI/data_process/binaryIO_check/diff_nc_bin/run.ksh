#!/bin/ksh --login

# Set the queueing options 
#PBS -l procs=1
#PBS -l walltime=0:10:00
#PBS -A wrfruc
#PBS -N wrf_gsi
#PBS -q debug
#PBS -j oe

set -x
np=$PBS_NP

module load intel
module load mpt

WORKPATH=/scratch1/portfolios/BMC/wrfruc/mhu/rapid_refresh/GSI_r745/data_process/binaryIO_check/diff_nc_bin

cd ${WORKPATH}

mpiexec_mpt -np $np ./diff_nc_bin.exe > stdout_test 2>&1

exit

