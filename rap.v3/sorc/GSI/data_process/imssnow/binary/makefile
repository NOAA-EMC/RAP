# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I../../../include 
LIBS =  \
        ${WRFDIR}/external/io_int/module_internal_header_util.o \
        ${WRFDIR}/frame/pack_utils.o ${WRFDIR}/main/libwrflib.a \
        -L${GSILIBS}

##        /mnt/lfs0/projects/wrfruc/mhu/uni-post/unipost_r259/libs/libw3.a

OBJS = update_SNOWICE_binary_mass.o module_binary_io.o blockIO.o

all: process_NESDIS_imssnow

process_NESDIS_imssnow: ${OBJS} process_NESDIS_imssnow.o
	${FC} -o process_NESDIS_imssnow.exe ${FLAGS} process_NESDIS_imssnow.o ${OBJS} ${LIBS} 

.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
