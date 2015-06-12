SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libbacio.a
INCMOD  = ../../../include

INC=clib.h

$(LIB):	$(LIB)(bacio.o baciof.o bafrio.o byteswap.o chk_endianc.o)

$(LIB)(bacio.o):       bacio.c $(INC)
	$(SCC) -c $(CFLAGS2) bacio.c
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) bacio.o

$(LIB)(byteswap.o):     byteswap.c $(INC)
	$(SCC) -c $(CFLAGS2) byteswap.c
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) byteswap.o

$(LIB)(baciof.o):   baciof.f
	$(SFC) -c $(FFLAGS_BACIO) baciof.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) baciof.o 
	$(CP) *.mod $(INCMOD)

$(LIB)(bafrio.o):   bafrio.f
	$(SFC) -c $(FFLAGS_BACIO) bafrio.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) bafrio.o 

$(LIB)(chk_endianc.o):   chk_endianc.f
	$(SFC) -c $(FFLAGS_BACIO) chk_endianc.f
	$(AR) -rv $(ARFLAGS_BACIO) $(LIB) chk_endianc.o

clean:
	$(RM) baciof.o bafrio.o $(LIB) bacio.o byteswap.o chk_endianc.o *.mod $(INCMOD)/bacio_module.mod

