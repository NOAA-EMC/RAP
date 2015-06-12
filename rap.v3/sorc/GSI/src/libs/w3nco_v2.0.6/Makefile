SHELL=/bin/sh
include ../../../configure.gsi
LIBDIR = ../../../lib/
LIB = $(LIBDIR)/libw3nco_i4r8.a
INCMOD  = ../../../include

# The file definitions. This include must occur before targets.
include make.filelist

all:    $(LIB)

$(LIB): $(OBJS)
	$(AR) -ruv $(LIB) $(OBJS)
#$(CP) *.mod $(INCMOD)


.SUFFIXES :
.SUFFIXES : .f .a .c .o

.f.o:
	$(SFC) -c $(FFLAGS_W3) $(FFLAGS_i4r8) $<

.c.o:
	$(SCC) -c $(CFLAGS) $<

clean:
	$(RM) $(OBJS) *.mod $(INCMOD)/args_mod.mod  $(INCMOD)/gblevn_module.mod  $(INCMOD)/mersenne_twister.mod $(LIB)

