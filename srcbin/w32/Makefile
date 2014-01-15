SHELL = /bin/sh
.SUFFIXES: .res .rc

WINDRES = $(CROSS_PREFIX)windres

all: 

wquizchooser:
	$(MAKE) -C .. wquizchooser

%.res: %.rc
	$(WINDRES) -i $< -o $@

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
