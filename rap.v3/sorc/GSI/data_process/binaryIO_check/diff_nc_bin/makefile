# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

WRFDIR=/scratch1/portfolios/BMC/wrfruc/mhu/rapid_refresh/WRFV3.4.1_r701
FC   =  mpif90 -free         
CC  = cc
FLAGS= -O3 -fp-model precise -assume byterecl -fpe0 -g -traceback $(LDFLAGS) -DLINUX -convert big_endian
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long'
INCLD=  -I${NETCDF}/include -I../../../include 
LIBS =  \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a \
        ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf

OBJS =  wrf_binary_interface.o \
       mpi_file_IO_swap.o \
        blockIO.o native_endianness.o

all: check_fields test_fields diff_nc_bin

check_fields: ${OBJS} check_fields.o
	${FC} -o check_fields.exe ${FLAGS} check_fields.o ${OBJS} ${LIBS} 

test_fields: ${OBJS} example_binary_mass.o
	${FC} -o test_fields.exe ${FLAGS} example_binary_mass.o ${OBJS} ${LIBS} 

diff_nc_bin: ${OBJS} diff_nc_bin.o
	${FC} -o diff_nc_bin.exe ${FLAGS} diff_nc_bin.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
