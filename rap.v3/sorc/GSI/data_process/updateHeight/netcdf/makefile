# makefile for nssl_porcess
#! /bin/sh -v

# Define the fortran compiler and options

include ../../configure.process

INCLD=  -I${NETCDF}/include -I../../../include
LIBS =  ${WRFDIR}/external/io_netcdf/libwrfio_nf.a -L${NETCDF}/lib -lnetcdf

OBJS = replace_fields_netcdf_mass.o

all: replace_fields_netcdf_mass

replace_fields_netcdf_mass: ${OBJS}
	${FC} -o replace_fields_netcdf_mass.exe ${FLAGS} ${OBJS} ${LIBS} 


.SUFFIXES : .F90 .f90 .f .c .o

.F90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.f90.o :
	${FC} ${FLAGS} ${INCLD} -c $<

.c.o :
	${CC} ${CPPFLAGS} -c $<

clean:
	/bin/rm -f *.o *.exe *.mod
