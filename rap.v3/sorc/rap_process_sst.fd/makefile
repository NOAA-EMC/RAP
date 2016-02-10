# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options

FC       = mpfort -g -traceback
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS =/nwprod/lib -lw3emc_4 -lw3nco_4 -lbacio_4 

WRFPATH    = /nwprod/sorc/wrf_shared.fd
W3FLAGS = -I/nwprod/lib/incmod/w3emc_v2.0.3_4
INCLD= -I${NETCDF}/include -I$(WRFPATH)/frame
LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_netcdf/libwrfio_nf.a ${WRFPATH}/frame/pack_utils.o \
        -L${NETCDF}/lib -lnetcdf

OBJS = kinds.o process_SST.o sst14k.o sstGlobal2RR.o netCDFsub_geo.o update_SST_netcdf_mass.o

all: process_SST

process_SST: ${OBJS}
	${FC} -o ../../exec/rap_process_sst ${FLAGS} ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<


clean:
	/bin/rm -f *.o *.exe *.mod rap_process_sst
