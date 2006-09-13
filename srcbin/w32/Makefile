SHELL = /bin/sh
.SUFFIXES: .res .rc

all: 

wquizchooser:
	$(MAKE) -C .. wquizchooser

%.res: %.rc
	windres -i $< -o $@

w32res: cgiquiz.res grquiz.res linequiz.res mkquiz.res \
        scrquiz.res wquizchooser.res

mostlyclean:
	-rm -f *.or *.o *.ow *.old
	-rm -f *.res
	-rm -f COPYING.txt COPYING.de.txt

clean: mostlyclean

distclean: clean
	-rm -f innosetup.iss
	-rm -f *.akfquiz
	-rm -f *.rc

new: distclean
