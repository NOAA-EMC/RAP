# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I../../../include 
LIBS =  \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a \
        -L../../../lib -lw3

OBJS = update_binary_mass_snowT.o partial_cycle_varaible_binary.o count_recs_wrf_binary_file.o \
       mpi_file_IO_swap.o module_binary_io_swap.o \
       full_cycle_varaible_binary.o full_cycle_varaible_binary_swap.o \
       module_binary_io.o blockIO.o next_buf.o

all: full_cycle_surface full_cycle_surface_swap full_cycle_snow

full_cycle_surface: ${OBJS} full_cycle_surface_binary_mass.o
	${FC} -o full_cycle_surface.exe ${FLAGS} full_cycle_surface_binary_mass.o ${OBJS} ${LIBS} 

full_cycle_surface_swap: ${OBJS} full_cycle_surface_binary_mass_swap.o
	${FC} -o full_cycle_surface_swap.exe ${FLAGS} full_cycle_surface_binary_mass_swap.o ${OBJS} ${LIBS} 
full_cycle_snow: ${OBJS} full_cycle_binary_mass.o
	${FC} -o full_cycle_snow.exe ${FLAGS} full_cycle_binary_mass.o ${OBJS} ${LIBS}


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
