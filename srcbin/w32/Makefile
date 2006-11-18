SHELL = /bin/sh
.SUFFIXES: .res .rc

all: 

wquizchooser:
	$(MAKE) -C .. wquizchooser

%.res: %.rc
	windres -i $< -o $@

w32res: cgiquiz.res grquiz.res linequiz.res mkquiz.res scrquiz.res 

mostlyclean:
	-rm -f *.or *.o *.ow *.old
	-rm -f *.res
	-rm -f COPYING.txt COPYING.de.txt

clean: mostlyclean

distclean: clean
	-rm -f akfquiz.nsh
	-rm -f *.akfquiz
	-rm -f *.rc
	-rm -f *.exe

new: distclean
