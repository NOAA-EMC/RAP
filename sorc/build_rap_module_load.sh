set -x

export BASE=`pwd`
cd $BASE

module reset
source $BASE/../versions/build.ver
module use $BASE/../modulefiles
source $BASE/../modulefiles/RAP/${rap_build_ver}
module list
