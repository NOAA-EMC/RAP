# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

NETCDFPATH = /u/wx20rv/lib_tmp/netcdf/lib
FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE

WRFPATH  = /nwprod/sorc/wrf_shared.fd
W3FLAGS = -I/nwprod/lib/incmod/w3emc_v2.0.3_4
INCLD =
NWPLIBS =/nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

OBJS = kinds.o count_recs_wrf_binary_file.o full_cycle_varaible_binary_change.o \
       module_binary_io.o blockIO.o native_endianness.o 

all: binary_sample_IO

binary_sample_IO: ${OBJS} binary_sample_IO.o
	${FC} -o rap_binary_sample_IO ${FLAGS} binary_sample_IO.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_binary_sample_IO
