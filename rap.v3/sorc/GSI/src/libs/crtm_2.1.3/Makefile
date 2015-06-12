SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib
LIB = $(LIBDIR)/libcrtm.a
INCMOD = ../../../include

MODLIST = $(INCMOD)/crtm*.mod $(INCMOD)/nesdis*.mod $(INCMOD)/od*.mod $(INCMOD)/*binary_io.mod $(INCMOD)/*define.mod $(INCMOD)/parameters.mod $(INCMOD)/*utility.mod $(INCMOD)/compare_float_numbers.mod $(INCMOD)/fundamental_constants.mod $(INCMOD)/ocean_permittivity.mod $(INCMOD)/*_parameters.mod $(INCMOD)/type_kinds.mod $(INCMOD)/fresnel.mod  $(INCMOD)/message_handler.mod  $(INCMOD)/spectral_units_conversion.mod

include make.filelist
 

.SUFFIXES: .f90 .o

all:	$(LIB)

$(LIB):	$(OBJ_FILES)
	$(AR) crvs $(LIB) $(OBJ_FILES)
	$(CP) *.mod $(INCMOD)

.f90.o:
	$(SF90) -c $(FFLAGS_CRTM) $(LFLAGS_CRTM) $<

clean:
	$(RM) -f $(OBJ_FILES) *.mod $(LIB)

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

include make.dependencies

clean:
	rm -f *.o $(LIB) *.mod $(MODLIST)
