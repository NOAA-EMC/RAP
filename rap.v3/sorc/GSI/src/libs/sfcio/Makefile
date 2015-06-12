SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libsfcio_i4r4.a
INCMOD  = ../../../include

OBJ = sfcio_module.o
SRC = $(OBJ:.o=.f90)

$(LIB): $(OBJ)
	$(AR) -ruv $(ARFLAGS_SFCIO) $(LIB) $(OBJ)
	$(CP) *.mod $(INCMOD)

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(SF90) $(FFLAGS_SFCIO) -c $(SRC)

clean:
	$(RM) $(OBJ) $(LIB) *.mod $(INCMOD)/sfcio_module.mod

