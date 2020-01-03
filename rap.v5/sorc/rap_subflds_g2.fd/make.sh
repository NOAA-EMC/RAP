#!/bin/sh --login

#---------------------------------------------------------------------------------
#  The driver script for compiling the emcsfc_snow2mdl program.  Loads nceplib
#  module files and exports environment variables required by the makefile
#  Then, invokes the makefile.  
#
#  Only tested on Zeus and the NCEP WCOSS machines.
#
#  To invoke: type 'make.sh' from the command line.  If successfully built, 
#  the executable will be installed the ../../exec subdirectory.
#
#  See the README.build file for more details.
#---------------------------------------------------------------------------------

#set -x

mac=$(hostname | cut -c1-1)

if [ $mac = t -o $mac = g ] ; then  #For WCOSS

  echo
  echo "BUILD PROGRAM ON WCOSS"
  echo

  module purge
  module load ./module.build.wcoss

  make clean
  make all
  rc=$?

elif [ $mac = f ] ; then  #For Zeus

  echo
  echo "BUILD EMCSFC_SNOW2MDL PROGRAM ON ZEUS."
  echo

  module purge

# load intel compiler
# load ncep library modules

  module load ip/v2.0.0
  module load sp/v2.0.2
  module load w3nco/v2.0.6
  module load bacio/v2.0.1
  module load jasper
  module load z
  module load png
  module load g2/v2.5.0
  module load landsfcutil/v2.0.0

  make clean
  make
  rc=$?

else

  echo "MACHINE OPTION NOT FOUND. EXIT."

fi

if ((rc != 0));then
  echo "BUILD FAILED."
else
  echo "BUILD SUCCEDED."
fi
