SHELL=/bin/sh

SUBDIRS = libs

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ) ; \
        done
	( cd main; echo "Making $@ in `pwd`" ; make -f makefile_DTC ) ;

clean: $(SUBDIRS) clean_main
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@ ) ; \
        done

clean_main:
	( cd main; echo "Making $@ in `pwd`" ; make -f makefile_DTC clean ) ;
