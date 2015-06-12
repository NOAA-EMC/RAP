# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ./configure.process
NETCDF= /apps/netcdf/3.6.3/intel

INCLD=  -I${NETCDF}/include -I/scratch1/portfolios/BMC/wrfruc/phofmann/GSI_exps/rh_bias/GSI/include
LIBS =  -L${NETCDF}/lib -lnetcdf -L${GSILIBS} \
        /scratch1/portfolios/BMC/wrfruc/phofmann/GSI_exps/rh_bias/GSI/src/main/constants.o

OBJS = netCDFsub.o netCDFsub_geo.o \
       cio.o module_debug.o misc_definitions_module.o constants_module.o \
       module_map_utils.o gridmod_gsi_map_utils.o \
       write_bufr_CLAVR.o process_CLAVR_cloud.o

all: process_CLAVR_cloud

process_CLAVR_cloud: ${OBJS}
	${FC} -o process_CLAVR_cloud.exe ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
