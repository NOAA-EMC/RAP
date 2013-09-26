# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

NETCDFPATH = /u/wx20rv/lib_tmp/netcdf/lib
FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE


## WRFPATH  = /meso/save/Dusan.Jovic/WRFV3
WRFPATH  = /meso/save/Ming.Hu/nwprod/rap.v2.0.0/sorc_v2/rap_wrf.fd/WRFV3.4.1_r732
W3FLAGS = -I/nwprod/lib/incmod/w3emc_v2.0.3_4
INCLD =
NWPLIBS =/nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

## OBJS = kinds.o update_binary_mass_snowT.o partial_cycle_varaible_binary.o count_recs_wrf_binary_file.o \
##        mpi_file_IO_swap.o module_binary_io_swap.o \
##        full_cycle_varaible_binary.o full_cycle_varaible_binary_swap.o \
##        module_binary_io.o blockIO.o native_endianness.o ## next_buf.o
OBJS = kinds.o partial_cycle_varaible_binary.o count_recs_wrf_binary_file.o \
       full_cycle_varaible_binary.o module_binary_io.o blockIO.o native_endianness.o ## next_buf.o

all: rap_full_cycle_surface

rap_full_cycle_surface: ${OBJS} full_cycle_surface_binary_mass.o
	${FC} -o rap_full_cycle_surface ${FLAGS} full_cycle_surface_binary_mass.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_full_cycle_surface
