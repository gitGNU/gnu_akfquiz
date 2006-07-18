# this Makefile is just a glue to the others
# if you want to configure things, use the script configure 
# and edit the Makefiles in srcbin

VERSION=4.3.2
SVERSION=432

# Name of TARGET as used as extension for makefile in srcbin
# can be: fpc, gpc, xw32, xarm
TARGET=fpc
SYS=Linux
ARCH=x86

# all for POSIX machines with GTK+ and probably SDL installed
default: all

all:
	$(MAKE) -C srcbin -f Makefile.$(TARGET)

# just text-oriented programs
most:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) most

# programs for windows (either native or cross-compilation)
windows:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) windows

# install for POSIX machines
install:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) install

# reverse of install 
uninstall:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) uninstall

# install CGI program
# please make sure the path is correctly set up!
install-cgi:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) install-cgi

# shouldn't be used
install-quizzes:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) install-quizzes

doc: FORCE
	$(MAKE) -C doc

html: FORCE
	$(MAKE) -C doc html

txt:
	$(MAKE) -C doc txt

pdf:
	$(MAKE) -C doc pdf

clean:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) clean
	$(MAKE) -C doc clean
	-rm -f *.old
	-rm -f autopackage/*.old

veryclean:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) veryclean
	$(MAKE) -C doc clean
	-rm -f *.old
	-rm -f autopackage/*.old

distclean:
	$(MAKE) -C srcbin -f Makefile.$(TARGET) distclean
	$(MAKE) -C doc distclean
	-rm -f *.old
	-rm -f autopackage/*.old

new: distclean

config:
	./configure



# following targets are useful for my system
# they need not fit your needs, so be careful!

pkg: distclean
	env GZIP=--best tar czvCf .. \
	  ../akfquiz-$(VERSION).main.tar.gz \
	      akfquiz/start \
	      akfquiz/configure \
	      akfquiz/Makefile \
	      akfquiz/desktop/ \
	      akfquiz/share/akfquiz/Linux-en.akfquiz \
	      akfquiz/share/akfquiz/Linux-de.akfquiz \
	      akfquiz/share/akfquiz/Christentum-de.akfquiz \
	      akfquiz/share/akfquiz/Schokolade-de.akfquiz \
	      akfquiz/share/akfquiz/Landtechnik.akfquiz \
	      akfquiz/doc/ \
	      akfquiz/html/ \
	      akfquiz/srcbin/

pkg-bin: all
	env GZIP=--best tar czvCf .. \
	  ../akfquiz-$(VERSION).bin.$(SYS).$(ARCH).tar.gz \
	      akfquiz/start \
	      akfquiz/doc/english/INSTALL \
	      akfquiz/doc/deutsch/INSTALL \
	      akfquiz/doc/COPYING \
	      akfquiz/doc/english/LICENSE.txt \
	      akfquiz/doc/deutsch/LIZENZ.txt \
	      akfquiz/srcbin/mkquiz \
	      akfquiz/srcbin/scrquiz \
	      akfquiz/srcbin/cgiquiz \
	      akfquiz/srcbin/diaquiz \
	      akfquiz/srcbin/linequiz \
	      akfquiz/srcbin/grquiz \
	      akfquiz/srcbin/gtkquizchooser \
	      akfquiz/srcbin/quizstat \
	      akfquiz/srcbin/akfquiz

pkg-mostbin: most
	env GZIP=--best tar czvCf .. \
	  ../akfquiz-$(VERSION).bin.$(SYS).$(ARCH).tar.gz \
	      akfquiz/start \
	      akfquiz/doc/english/INSTALL \
	      akfquiz/doc/deutsch/INSTALL \
	      akfquiz/doc/COPYING \
	      akfquiz/doc/english/LICENSE.txt \
	      akfquiz/doc/deutsch/LIZENZ.txt \
	      akfquiz/srcbin/mkquiz \
	      akfquiz/srcbin/scrquiz \
	      akfquiz/srcbin/cgiquiz \
	      akfquiz/srcbin/diaquiz \
	      akfquiz/srcbin/linequiz \
	      akfquiz/srcbin/quizstat \
	      akfquiz/srcbin/akfquiz

# INNO Setup
pkg-windows: html pdf
	$(MAKE) -C srcbin -f Makefile.$(TARGET) veryclean
	# export and convert textfiles
	cp doc/COPYING srcbin/w32/COPYING.txt
	cp doc/deutsch/COPYING.de srcbin/w32/COPYING.de.txt
	unix2dos srcbin/w32/*.txt
	unix2dos share/akfquiz/*.akfquiz
	iscc srcbin/w32/akfquiz.iss

# under windows you could use MSYS or Cygwin for scripts
#
# for CrossCompilation "iscc" can be a script for wine:
#     #!/bin/sh
#     exec wine 'C:\Program Files\Inno Setup 5\ISCC.exe' "$@"

FORCE:
