SHELL=/bin/sh

include ../../../configure.gsi
LIB = ../../../lib/libprepdecode_i4r8.a

OBJS = mova2i.o

all:	$(LIB)


$(LIB):	$(OBJS)
	$(AR) -ruv $(LIB) $(OBJS)

.SUFFIXES :
.SUFFIXES : .F .f .a .c .o .fpp

.f.o:
	$(SFC) -c $(FFLAGS_BUFR)  $<

.c.o:
	$(SCC) -c  $(CFLAGS_BUFR) $<

clean:
	rm -f *.o *.mod $(LIB) *.b bufrlib.prm

