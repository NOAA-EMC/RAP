LIB     = ../../libmersenne_4.a
INCMOD  = ../../incmod
FC	= mpif90
FFLAGS	= -O3 -I$(INCMOD)
FFLAGB	= -O3  
AR	= ar
ARFLAGS	= -rv 

$(LIB):	$(LIB)(mersenne_twister.o) 
$(LIB)(mersenne_twister.o):	mersenne_twister.f

.f.a:
	$(FC) -c $(FFLAGS) $<
	$(AR) $(ARFLAGS) $@ $*.o
	rm -f $*.o
	mv $*.mod $(INCMOD)
