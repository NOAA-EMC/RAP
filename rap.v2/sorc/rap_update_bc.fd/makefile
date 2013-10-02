# makefile for nssl_porcess
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

LIBS =  \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

OBJS = kinds.o  \
       da_module_couple_uv.o \
       da_updateBC_interface.o \
       module_binary_io.o blockIO.o \
       native_endianness.o 

all: da_update_bc_bin

da_update_bc_bin: ${OBJS} da_update_bc_bin.o
	${FC} -o rap_update_bc ${FLAGS} da_update_bc_bin.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_process_updatebc
