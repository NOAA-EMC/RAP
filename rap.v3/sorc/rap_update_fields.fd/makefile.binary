# makefile for update_height 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = mpfort
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
OPTS     = -O3 -assume byterecl -convert big_endian
LIST     =
FREE     = -FR
TRAPS    =
PROFILE  =
DEBUG    = # -g # -traceback -fpe0 -ftrapuv # -check all
MEM      =
MAP      =
ESSL     =
MASS     =

W3FLAGS = -I/nwprod/lib/incmod/w3emc_v2.0.3_4
NEMSIOFLAG = -I/nwprod/lib/incmod/nemsio
WRFPATH    = /nwprod/sorc/wrf_shared.fd
NWPLIBS =/nwprod/lib -lw3_4 -lbacio_4 

LIBS =  \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

OBJS = kinds.o \
       update_binary_mass.o module_binary_io.o blockIO.o native_endianness.o

all: replace_height_binary_mass

replace_height_binary_mass: ${OBJS} replace_height_binary_mass.o
	${FC} -o rap_update_height ${FLAGS} replace_height_binary_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_update_height
