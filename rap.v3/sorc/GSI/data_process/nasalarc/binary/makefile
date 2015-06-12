# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I../../../include -I./
LIBS =  -L${GSILIBS} ../../../src/main/constants.o \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a 


OBJS = cio.o module_debug.o gridmod_gsi_map_utils.o \
       misc_definitions_module.o constants_module.o module_map_utils.o \
       write_bufr_NASALaRC.o process_NASALaRC_cloud.o \
       module_binary_geo.o module_binary_io.o blockIO.o

all: process_NASALaRC_cloud

process_NASALaRC_cloud: ${OBJS}
	${FC} -o process_NASALaRC_cloud.exe ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
