SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libsigio_i4r4.a
INCMOD  = ../../../include

OBJ = sigio_module.o sigio_r_module.o
SRC = $(OBJ:.o=.f90)

$(LIB): $(OBJ)
	$(AR) -ruv $(ARFLAGS_SIGIO) $(LIB) $(OBJ) 
	$(CP) *.mod $(INCMOD)

.SUFFIXES:
.SUFFIXES:  .f .f90 .o


.f90.o:
	$(SF90) $(FFLAGS_SIGIO) -c $(SRC)

clean:
	$(RM) $(OBJ) $(LIB) *.mod $(INCMOD)/sigio_*.mod
