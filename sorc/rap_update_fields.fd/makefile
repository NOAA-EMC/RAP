# makefile for update_fields 
#! /bin/sh -v

# Define the fortran compiler and options

FC       = ftn
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
OPTS     = -O3 -assume byterecl -convert big_endian
DEBUG    = # -g # -traceback -fpe0 -ftrapuv # -check all

FLAGS = -qopenmp
NEMSIOFLAG = -I${NEMSIO_INC4} -I${WRF_IO_INC} -I${W3NCO_INC4}
NWPLIBS =${W3NCO_LIB4} ${BACIO_LIB4}

INCLD= -I${NETCDF}/include -I${WRF_IO_INC}

LIBS = ${WRF_IO_LIB} -L${NETCDF}/lib -lnetcdf -lnetcdff

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
