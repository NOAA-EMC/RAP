#! /bin/sh -v

# Define the fortran compiler and options
#
include ../../../configure.gsi

FC   =  ${SFC}
FLAGS=  -O2  
INCLD=  -I../../../include
LIBS =  -L../../../lib -lbufr_i4r8 -lw3emc_i4r8 -lw3nco_i4r8

all: bufr_decode_l2rwbufr bufr_encode_l2rwbufr bufr_encode_radarbufr bufr_decode_l2rwbufr_inventory

work: bufr_decode_l2rwbufr_inventory

bufr_decode_l2rwbufr: bufr_decode_l2rwbufr.o
	${FC} -o bufr_decode_l2rwbufr.exe ${FLAGS} bufr_decode_l2rwbufr.o ${LIBS} 
	rm bufr_decode_l2rwbufr.o

bufr_encode_l2rwbufr: bufr_encode_l2rwbufr.o
	${FC} -o bufr_encode_l2rwbufr.exe ${FLAGS} bufr_encode_l2rwbufr.o ${LIBS} 
	rm bufr_encode_l2rwbufr.o

bufr_decode_l2rwbufr_inventory: bufr_decode_l2rwbufr_inventory.o
	${FC} -o bufr_decode_l2rwbufr_inventory.exe ${FLAGS} bufr_decode_l2rwbufr_inventory.o ${LIBS} 
	rm bufr_decode_l2rwbufr_inventory.o

bufr_encode_radarbufr: bufr_encode_radarbufr.o
	${FC} -o bufr_encode_radarbufr.exe ${FLAGS} bufr_encode_radarbufr.o ${LIBS} 
	rm bufr_encode_radarbufr.o

.SUFFIXES : .f90  .o

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

clean:
	/bin/rm -f *.o *.exe
