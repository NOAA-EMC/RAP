#!/bin/sh
#
OBJS=MAKEPRECIP.o

FC =   ftn -convert big_endian -g -traceback -assume noold_ldout_format

LIBS = ${G2TMPL_LIB} ${G2_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB} \
       ${IP_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4} ${BACIO_LIB4}

INCG2=-I${G2_INC4} -I${G2TMPL_INC}

FFLAGS = -O -fp-model strict -list
LDFLAGS =-FR -assume byterecl

CMD =	../../exec/rap_subflds_g2    
PROFLIB =	-lprof

all:		$(CMD)

$(OBJS): %.o: %.f90
	$(FC) $(INCG2) -c $*.f90

$(CMD):		$(OBJS) 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS) $(CMD)
