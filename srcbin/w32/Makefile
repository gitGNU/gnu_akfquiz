SHELL = /bin/sh

all: 

wquizchooser:
	$(MAKE) -C .. wquizchooser

mostlyclean:
	-rm -f *.or *.o *.ow *.old
	-rm -f *.rc
	-rm -f COPYING.txt COPYING.de.txt

clean: mostlyclean

distclean: clean
	-rm -f innosetup.iss
	-rm -f *.akfquiz

new: distclean
