# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I${NETCDF}/include -I../../../include
LIBS =  -L${NETCDF}/lib -lnetcdf -L${GSILIBS} \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a

OBJS = netCDFsub.o netCDFsub_geo.o write_bufr_ref.o process_NSSL_mosaic.o \
       module_binary_geo.o module_binary_io.o blockIO.o

all: process_NSSL_mosaic

process_NSSL_mosaic: ${OBJS}
	${FC} -o process_NSSL_mosaic.exe ${FLAGS} ${OBJS} ${LIBS} 

read_bufr: readbufr.o
	${FC} -o read_bufr.exe ${FLAGS} readbufr.o ${LIBS} 

write_bufr: write_bufr_ref.o write_bufr_test.o
	${FC} -o write_bufr.exe ${FLAGS} write_bufr_ref.o write_bufr_test.o  ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe
