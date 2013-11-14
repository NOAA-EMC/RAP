# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE

WRFPATH  = /nwprod/sorc/wrf_shared.fd
WRFPATH  = /gpfs/gd1/emc/meso/save/Ming.Hu/svn/trunk/rap.v2/sorc/rap_wrf.fd/WRFV3.4.1
INCLD = -I${NETCDF}/include
NWPLIBS = # /nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_int/libwrfio_int.a \
        $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o \
        ${WRFPATH}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf

OBJS = kinds.o count_recs_wrf_binary_file.o nc2bin.o \
       module_binary_io.o blockIO.o native_endianness.o 

all: sync_nc2bin_geo

sync_nc2bin_geo: ${OBJS} sync_nc2bin_geo.o
	${FC} -o rap_sync_nc2bin_geo ${FLAGS} sync_nc2bin_geo.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_sync_nc2bin_geo
