# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options


WRFPATH    = /nwprod/sorc/wrf_shared.fd
NETCDFPATH = /usrx/local/NetCDF/3.6.3
FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE


W3FLAGS = -I/nwprod/lib/incmod/w3emc_v2.0.3_4
INCLD =
NWPLIBS =/nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

OBJS = kinds.o count_recs_wrf_binary_file.o \
       mpi_file_IO_swap.o module_binary_io_swap.o \
       read_varaible_binary.o read_varaible_binary_int.o \
       module_binary_io.o blockIO.o native_endianness.o ## next_buf.o

all: read_binary read_binary_bdy

read_binary: ${OBJS} read_binary_mass.o
	${FC} -o read_binary.exe ${FLAGS} read_binary_mass.o ${OBJS} ${LIBS} 

read_binary_bdy: ${OBJS} read_binary_bdy.o
	${FC} -o read_binary_bdy.exe ${FLAGS} read_binary_bdy.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod 
