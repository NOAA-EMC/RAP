# makefile for nssl_porcess
#!/bin/sh -v

# Define the fortran compiler and options

WRFPATH    = /nwprod/sorc/wrf_shared.fd
FC       = mpfort -g 
CPP      = /lib/cpp -P
ARCH     =
CPPFLAGS =  -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -DFortranLlong='long long' -D_UNDERSCORE
NWPLIBS =/nwprod/lib -lbufr_d_64 

LIBS =  -L${NWPLIBS} \
        $(WRFPATH)/external/io_int/libwrfio_int.a $(WRFPATH)/frame/module_machine.o \
        ${WRFPATH}/frame/pack_utils.o

OBJS = kinds.o constants.o write_bufr_ref.o process_NSSL_mosaic.o \
       module_binary_geo.o module_binary_io.o blockIO.o 

all: process_mosaic

process_mosaic: ${OBJS}
	${FC} -o rap_process_mosaic ${FLAGS} ${OBJS} ${LIBS} 

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
	/bin/rm -f *.o *.exe *.mod rap_process_mosaic
