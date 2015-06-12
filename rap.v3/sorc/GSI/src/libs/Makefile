SHELL=/bin/sh

SUBDIRS = bacio bufr sp sfcio sigio gsdcloud nemsio misc crtm_2.1.3 w3emc_v2.0.5 w3nco_v2.0.6

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make ) ; \
        done

clean: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make $@ ) ; \
        done
