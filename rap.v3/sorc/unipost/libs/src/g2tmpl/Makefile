SHELL=/bin/sh

LIB     = ./libg2tmpl.a

INCMOD  = ./incmod/g2tmpl

FC =	ncepxlf90
CC =	ncepxlc
FFLAGS  = -qnosave -O3 -q free=f90 -I$(INCMOD)
AR      = ar
ARFLAGS = -rv -X64

$(LIB): $(LIB)(process_4dot2.o) $(LIB)(grib2_all_tables_module.o) $(LIB)(gdtsec3.o)

$(LIB)(process_4dot2.o):	process_4dot2.c
	$(CC) -O3 -c $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o

.f.a:
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o
	mv $*.mod $(INCMOD)

clean:
	rm -f process_4dot2.o

realclean:	clean
	rm -f test_* core
