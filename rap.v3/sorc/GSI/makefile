#SHELL=/bin/sh
#

include ./configure.gsi


SUBDIRS = src

all: $(SUBDIRS)
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; make); \
        done

clean: $(SUBDIRS)
	echo "subdirs is: $(SUBDIRS)"
	@for dir in $(SUBDIRS); do \
           ( cd $$dir; echo "Making $@ in `pwd`" ; \
                make $@) ; \
        done
