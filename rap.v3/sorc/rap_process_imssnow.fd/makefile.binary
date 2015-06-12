# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

WRFPATH=/nwprod/sorc/wrf_shared.fd
## NETCDFPATH = /u/wx20rv/lib_tmp/netcdf/lib
NETCDFPATH = /usrx/local/NetCDF/3.6.3/lib
FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS =/nwprod/lib -lip_4 -lsp_4 -lw3nco_4 -lbacio_4

LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

OBJS = kinds.o update_SNOWICE_binary_mass.o module_binary_io.o blockIO.o native_endianness.o

all: process_NESDIS_imssnow

process_NESDIS_imssnow: ${OBJS} process_NESDIS_imssnow.o
	${FC} -o rap_process_imssnow ${FLAGS} process_NESDIS_imssnow.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod rap_process_imssnow
