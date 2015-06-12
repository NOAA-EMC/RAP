# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I../../../include 
LIBS =  \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a

OBJS =  \
       da_module_couple_uv.o \
       da_updateBC_interface.o \
       mpi_file_IO_swap.o \
       module_binary_io.o blockIO.o \
       next_buf.o

all: da_update_bc_bin

da_update_bc_bin: ${OBJS} da_update_bc_bin.o
	${FC} -o da_update_bc_bin.exe ${FLAGS} da_update_bc_bin.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
