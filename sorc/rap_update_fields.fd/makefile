# makefile for update_fields 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
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

W3FLAGS = -I${W3EMC_INC4}
NEMSIOFLAG = -I${NEMSIO_INC4}
#WRFPATH    = /nwprod/sorc/wrf_shared.fd
WRFPATH    = /gpfs/hps/nco/ops/nwprod/wrf_shared.v1.1.0-intel
NWPLIBS =${W3EMC_LIB4} ${BACIO_LIB4}

INCLD= -I${NETCDF}/include -I$(WRFPATH)/frame

LIBS =  \
       $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
       -L${NETCDF}/lib -lnetcdf

OBJS = kinds.o

all: replace_fields_netcdf_mass

replace_fields_netcdf_mass: ${OBJS} replace_fields_netcdf_mass.o
	${FC} -o ../../exec/rap_update_fields ${FLAGS} replace_fields_netcdf_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_update_fields
