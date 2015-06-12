# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I../../../include 
LIBS =  \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a \
        -L../../../lib -lw3

OBJS = count_recs_wrf_binary_file.o \
       mpi_file_IO_swap.o \
       update_binary_mass.o module_binary_io.o blockIO.o next_buf.o

all: replace_height_binary_mass

replace_height_binary_mass: ${OBJS} replace_height_binary_mass.o
	${FC} -o replace_height_binary_mass.exe ${FLAGS} replace_height_binary_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
