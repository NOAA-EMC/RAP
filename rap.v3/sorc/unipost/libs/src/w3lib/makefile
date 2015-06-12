SHELL=/bin/sh

FC=mpif90
FFLAGS=-O3 -xT -fp-model precise -assume byterecl -convert big_endian -fpe0 -g -traceback
CC=icc
CFLAGS=-O0 -DLINUX



LIB = ../../libw3.a

$(LIB):	$(LIB)(w3fa09.o w3fb06.o w3fb11.o baciof.o bacio_module.o errexit.o errmsg.o fparsei.o fparser.o gbytes_char.o gbytes.o getbit.o getgb1re.o getgb1r.o getgb1s.o getgbe.o getgbeh.o getgbem.o getgbemh.o getgbemp.o getgbens.o getgbep.o getgbex.o getgbexm.o getgb.o getgbh.o getgbm.o getgbmh.o getgbmp.o getgbp.o getgi.o getgir.o idsdef.o instrument.o iw3jdn.o ixgb.o lengds.o mova2i.o pdsens.o pdseup.o putgbe.o putgben.o putgbens.o putgbex.o putgb.o putgbn.o r63w72.o sbyte.o sbytes.o skgb.o start.o summary.o w3valdat.o w3difdat.o w3doxdat.o w3fi01.o w3fi58.o w3fi59.o w3fi63.o w3fi68.o w3fi71.o w3fi72.o w3fi73.o w3fi74.o w3fi75.o w3fi76.o w3fi82.o w3fi83.o w3fs21.o w3fs26.o w3locdat.o w3log.o w3movdat.o w3reddat.o w3tagb.o w3utcdat.o bacio.o mova2i.o mersenne_twister.o bafrio.o)

.f.a:
	$(FC) -c $(FFLAGS) $<
	ar -ruv $(AFLAGS) $@ $*.o
	rm -f $*.o

.c.a:
	$(CC) -c $(CFLAGS) $<
	ar -ruv  $(AFLAGS) $@ $*.o
	rm -f $*.o

clean:
	rm -f *.o *.mod $(LIB)

