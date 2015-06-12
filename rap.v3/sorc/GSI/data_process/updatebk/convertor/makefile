# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I../../../include  -I${NETCDF}/include
LIBS =  ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a \
        -L../../../lib -lw3

OBJS = full_cycle_bin2ncd.o full_cycle_ncd2bin.o \
       count_recs_wrf_binary_file.o \
       mpi_file_IO_swap.o module_binary_io.o blockIO.o next_buf.o

all: full_cycle_surface_convertor

full_cycle_surface_convertor: ${OBJS} full_cycle_surface_convertor.o
	${FC} -o full_cycle_surface_convertor.exe ${FLAGS} full_cycle_surface_convertor.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
