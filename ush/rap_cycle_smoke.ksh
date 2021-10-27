#! /bin/ksh

set -xue

export DATE=${DATE:-/bin/date}

echo "rap_cycle_smoke.ksh started at `${DATE}`"

to_file="$1"
now="${2:-${START_TIME:-$PDY$cyc}}"

maxback="${maxback:-9}" # how many hours ago to search for wrfout in nwges
minback="${minback:-1}" # minimum number of cycles to go back for smoke in nwges
NWGES=${gespath}

gesdir="$NWGES/rap/rapges"
SPAWN="mpiexec -n 1 -ppn 1"
#SPAWN="echo"
#CYCLE_NETCDF="$EXECrap/rap_cycle_netcdf"
CYCLE_NETCDF="$EXECrap/rap_fires_cycle_netcdf"

set +x

if [[ ! -d "$gesdir" ]] ; then
    echo "ERROR: Odd.  The rapges dir doesn't exist: $gesdir" 1>&2
    echo "ERROR: Maybe check the \$NWGES and \$envir variables?" 1>&2
    echo "ERROR: This just isn't your day, is it?" 1>&2
    exit 1
fi

saw=NO

for back in $( seq "$minback" "$maxback" ) ; do
    set -x
    before=$( $NDATE -$back $now )
    from_file="$gesdir/rap_${before}f$( printf %03d $back )"

    if [[ ! -s "$from_file" ]] ; then
        echo "File $back hours back is unavailable: $from_file"
        continue
    fi

    if ( $SPAWN $CYCLE_NETCDF smoke "$from_file" "$to_file" ) ; then
        postmsg "Hurray! Cycle $now WRF input has an initial smoke state from \"$from_file\""
        echo "rap_cycle_smoke.ksh finished at $( date )"
        exit 0
    else
        echo "Non-zero exit status from cycle_netcdf.  Will try the next file." 1>&2
        SAW=YES
    fi
done

if [[ "$saw" == NO ]] ; then
    echo "WARNING: Looked back $maxback hours and saw no ges files." 1>&2
fi

postmsg "ERROR: Unable to cycle smoke for RAP at $now"

exit 1
