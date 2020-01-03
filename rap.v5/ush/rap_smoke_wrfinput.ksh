#! /bin/ksh

set -xue

wrfinput_d01="$1"
STAT_TIME="${START_TIME:-$PDY$cyc}"
EXEC="$EXECrap/rap_fires_ncfmake"

# Wrapper to start prep-chem-sources.  Use aprun if this is a MAMU node:
RUNSERIAL='aprun -d 1 -n 1 -N 1 -j 1'

# Make sure we are using GMT time zone for time computations:
export TZ="GMT"

# Set up paths to shell commands
ECHO="${ECHO:-/bin/echo}"
DATE="${DATE:-/bin/date}"

# Run the command:
${ECHO} "rap_smoke_wrfinput.ksh started at `${DATE}`"

$RUNSERIAL ${EXEC} ./wrfinput_d01 "$COMOUT/rap.t${cyc}z.smoke.sources.bin"

${ECHO} "rap_smoke_wrfinput.ksh finished at `${DATE}`"
