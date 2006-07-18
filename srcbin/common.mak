
#####################################################################
# change here:
#####################################################################

PREFIX   = /usr/local
BINDIR   = $(PREFIX)/bin
ICONDIR  = $(PREFIX)/share/pixmaps/
DOCDIR   = $(PREFIX)/share/doc/akfquiz
GETQUIZ  = $(BINDIR)/getquiz
QUIZPATH = $(PREFIX)/share/akfquiz
HTMLPATH = $(QUIZPATH)/html

CGIPATH  = /usr/lib/cgi-bin

#MANPATH = $(PREFIX)/man
MANPATH = $(PREFIX)/share/man
MAN1DIR = $(MANPATH)/man1
MAN5DIR = $(MANPATH)/man5
MAN8DIR = $(MANPATH)/man8
MAN1DEDIR = $(MANPATH)/de/man1
MAN5DEDIR = $(MANPATH)/de/man5
MAN8DEDIR = $(MANPATH)/de/man8

######################################################################

# "all" is for Unix with all libraries
all: scrquiz linequiz diaquiz mkquiz cgiquiz grquiz gtkquizchooser

# "most" leaves some critical progs away
most: scrquiz linequiz mkquiz cgiquiz diaquiz 

# using FreePascal and DJGPP
dos: scrquiz linequiz mkquiz # grquiz

# compile for Windows
windows: wquizchooser scrquiz linequiz mkquiz cgiquiz grquiz

doc:
	$(MAKE) -C ../doc 

html:
	$(MAKE) -C ../doc html

txt:
	$(MAKE) -C ../doc txt

pdf:
	$(MAKE) -C ../doc pdf

mkquiz: mkquiz.pas qsys.pas qmsgs.pas uakfquiz.pas htmlquiz.pas
	$(PC) $(PFLAGS) $(POPT) $(DEFINES) mkquiz.pas

scrquiz: scrquiz.pas qsys.pas qmsgs.pas uakfquiz.pas
	$(PC) $(PFLAGS) $(POPT) $(DEFINES) scrquiz.pas

linequiz: linequiz.pas qsys.pas qmsgs.pas uakfquiz.pas
	$(PC) $(PFLAGS) $(POPT) $(DEFINES) linequiz.pas

cgiquiz: cgiquiz.pas uakfquiz.pas qmsgs.pas qsys.pas htmlquiz.pas
	$(PC) $(PFLAGS) $(POPT) $(DEFINES) cgiquiz.pas

introsnd.inc: introsnd.ub
	bin2obj -c IntroSound -o introsnd.inc introsnd.ub

infosnd.inc: infosnd.ub
	bin2obj -c InfoSound -o infosnd.inc infosnd.ub

errorsnd.inc: errorsnd.ub
	bin2obj -c ErrorSound -o errorsnd.inc errorsnd.ub

rightsnd.inc: rightsnd.ub
	bin2obj -c RightSound -o rightsnd.inc rightsnd.ub

wrongsnd.inc: wrongsnd.ub
	bin2obj -c WrongSound -o wrongsnd.inc wrongsnd.ub

neutralsnd.inc: neutralsnd.ub
	bin2obj -c NeutralSound -o neutralsnd.inc neutralsnd.ub

ppm2pas: ppm2pas.pas
	$(NATIVEPC) $(NATIVEPFLAGS) $(DEFINES) ppm2pas.pas

titimg.inc: titimg.ppm ppm2pas
	./ppm2pas titimg.ppm TitleImage > titimg.inc

titimg.o: titimg.ppm
	binobj titimg.ppm titimg.o TitleImage

quizhg.inc: quizhg.ppm ppm2pas
	./ppm2pas quizhg.ppm AKFQuizHG > quizhg.inc

quizhg.o: quizhg.ppm
	binobj quizhg.ppm quizhg.o AKFQuizHg

grquiz: grquiz.pas qsys.pas qmsgs.pas uakfquiz.pas clgrph.pas \
        sdlgrph.pas sdlsnd.pas $(IMAGEOBJ) $(SOUNDOBJ) font.inc
	-$(PC) $(PFLAGS) $(POPT) $(DEFINES) $(GUIFLAG) grquiz.pas

diaquiz: diaquiz.pas qsys.pas qmsgs.pas uakfquiz.pas dialog.pas
	$(PC) $(PFLAGS) $(POPT) $(DEFINES) diaquiz.pas

# just for Unix, just FPC
gtkquizchooser: gtkquizchooser.pas
	-$(PC) $(PFLAGS) $(POPT) $(DEFINES) $(GUIFLAG) gtkquizchooser.pas

# just for Windows, just FPC
wquizchooser: w32/wquizchooser.pas
	$(PC) $(PFLAGS) $(POPT) $(DEFINES) $(GUIFLAG) -FE. w32/wquizchooser.pas

clean:
	-rm -f *.o *.or *.a a.out *.gpi *.gpm *.ppu *.old *~
	-rm -f *.aw *.ow *.owr *.ppw *.res
	-rm -f *.AW *.OW *.OWR *.PPW *.RES
	-$(MAKE) -C w32 clean

veryclean: clean
	-rm -f quizhg.inc titimg.inc ppm2pas
	-rm -f errorsnd.inc infosnd.inc introsnd.inc rightsnd.inc \
	       wrongsnd.inc neutralsnd.inc

distclean: veryclean
	-rm -f mkquiz scrquiz cgiquiz akfquiz.cgi diaquiz linequiz \
	       grquiz gtkquizchooser
	-rm -f *.exe
	-$(MAKE) -C w32 distclean
	-dos2unix ../share/akfquiz/*.akfquiz

new: distclean

# just for Unix
install: all
	@echo "*"
	@echo "* creating directories"
	@echo "*"
	test -d $(BINDIR)   || install -d $(BINDIR)
	test -d $(ICONDIR)  || install -d $(ICONDIR)
	test -d $(DOCDIR)   || install -d $(DOCDIR)
	test -d $(MAN1DIR)  || install -d $(MAN1DIR)
	test -d $(MAN5DIR)  || install -d $(MAN5DIR)
	test -d $(MAN8DIR)  || install -d $(MAN8DIR)
	test -d $(MAN1DEDIR)  || install -d $(MAN1DEDIR)
	test -d $(MAN5DEDIR)  || install -d $(MAN5DEDIR)
	test -d $(MAN8DEDIR)  || install -d $(MAN8DEDIR)
	test -d $(QUIZPATH) || install -d -m 1777 $(QUIZPATH)
	test -d $(HTMLPATH) || install -d $(HTMLPATH)
	@echo "*"
	@echo "* installing programs"
	@echo "*"
	install -m 0755 -s mkquiz $(BINDIR)
	install -m 0755 -s scrquiz $(BINDIR)
	install -m 0755 -s diaquiz $(BINDIR)
	install -m 0755 -s linequiz $(BINDIR)
	-install -m 0755 -s grquiz $(BINDIR)
	#-install -o root -m 4755 -s grquiz $(BINDIR)
	-install -m 0755 gtkquizchooser $(BINDIR)
	install -m 0755 akfquiz $(BINDIR)
	install -m 0755 quizstat $(BINDIR)
	@echo "*"
	@echo "* installing manpages"
	@echo "*"
	install -m 0644 ../doc/english/mkquiz.1 $(MAN1DIR)
	install -m 0644 ../doc/english/scrquiz.1 $(MAN1DIR)
	install -m 0644 ../doc/english/grquiz.1 $(MAN1DIR)
	install -m 0644 ../doc/english/linequiz.1 $(MAN1DIR)
	install -m 0644 ../doc/english/gtkquizchooser.1 $(MAN1DIR)
	install -m 0644 ../doc/english/cgiquiz.8 $(MAN8DIR)
	install -m 0644 ../doc/english/akfquiz.5 $(MAN5DIR)
	install -m 0644 ../doc/deutsch/mkquiz.1 $(MAN1DEDIR)
	install -m 0644 ../doc/deutsch/scrquiz.1 $(MAN1DEDIR)
	install -m 0644 ../doc/deutsch/grquiz.1 $(MAN1DEDIR)
	install -m 0644 ../doc/deutsch/linequiz.1 $(MAN1DEDIR)
	install -m 0644 ../doc/deutsch/gtkquizchooser.1 $(MAN1DEDIR)
	install -m 0644 ../doc/deutsch/akfquiz.5 $(MAN5DEDIR)
	@echo "*"
	@echo "* installing Icon"
	@echo "*"
	install -m 0644 AKFQuiz.xpm $(ICONDIR)
	@echo "*"
	@echo "* installing documentation"
	@echo "*"
	install -m 0644 ../doc/COPYING $(DOCDIR)
	install -m 0644 ../doc/deutsch/COPYING.de $(DOCDIR)
	install -m 0644 ../doc/CHANGELOG $(DOCDIR)
	install -m 0644 ../doc/FAQ.html $(DOCDIR)
	install -m 0644 ../doc/TODO $(DOCDIR)
	install -m 0644 ../doc/english/template $(DOCDIR)/template-en
	install -m 0644 ../doc/deutsch/template $(DOCDIR)/template-de
	install -m 0644 ../doc/english/INSTALL $(DOCDIR)/INSTALL-en
	install -m 0644 ../doc/deutsch/INSTALL $(DOCDIR)/INSTALL-de
	@echo "*"
	@echo "* installing example quiz-files to $(QUIZPATH)"
	@echo "*"
	install -m 0644 ../share/akfquiz/*.akfquiz $(QUIZPATH)
	@echo "*"
	@echo "* installing files needed for HTML to $(HTMLPATH)"
	@echo "*"
	install -m 0755 -d $(HTMLPATH)
	install -m 0644 ../html/*.js ../html/*.png ../html/*.css $(HTMLPATH)
	install -m 0644 ../html/schulnote.html $(HTMLPATH)
	@echo "*"
	@echo "* creating getquiz"
	@echo "*"
	echo >  $(GETQUIZ) "#!/bin/sh"
	echo >> $(GETQUIZ) "cp -i $(HTMLPATH)/* ."
	chmod 755 $(GETQUIZ)
	@echo "*"
	@echo "* HINT: install the CGI program with 'make install-cgi'"
	@echo "* the CGI-path ist configured to $(CGIPATH)"
	@echo "*"

install-cgi: cgiquiz
	install -d $(CGIPATH)
	install -m 0755 -s cgiquiz $(CGIPATH)

# searches for Quizfiles and installs them as programs
# just for Unix
install-quizzes:
	for quiz in ../quiz/*.akfquiz; \
	  do \
	  TARGET=$(BINDIR)/$$(basename $$quiz .akfquiz); \
	  echo '#! /usr/bin/env akfquiz' > $$TARGET; \
	  echo >> $$TARGET; \
	  cat $$quiz >> $$TARGET; \
	  chmod 755 $$TARGET; \
	  echo "$$TARGET installed"; \
	done

# just for Unix
uninstall:
	-rm $(BINDIR)/mkquiz
	-rm $(BINDIR)/scrquiz
	-rm $(BINDIR)/linequiz
	-rm $(BINDIR)/diaquiz
	-rm $(BINDIR)/grquiz
	-rm $(BINDIR)/quizstat
	-rm $(BINDIR)/gtkquizchooser
	-rm -f $(BINDIR)/xgrquiz
	-rm -f $(BINDIR)/nscrquiz
	-rm $(GETQUIZ)
	-rm $(BINDIR)/akfquiz
	-rm $(MAN1DIR)/mkquiz.1
	-rm $(MAN1DIR)/scrquiz.1
	-rm $(MAN1DIR)/grquiz.1
	-rm $(MAN1DIR)/linequiz.1
	-rm $(MAN1DIR)/gtkquizchooser.1
	-rm $(MAN8DIR)/cgiquiz.8
	-rm $(MAN5DIR)/akfquiz.5
	-rm $(MAN1DEDIR)/mkquiz.1
	-rm $(MAN1DEDIR)/scrquiz.1
	-rm $(MAN1DEDIR)/grquiz.1
	-rm $(MAN1DEDIR)/linequiz.1
	-rm $(MAN1DEDIR)/gtkquizchooser.1
	-rm $(MAN8DEDIR)/cgiquiz.8
	-rm $(MAN5DEDIR)/akfquiz.5
	-rm $(ICONDIR)/AKFQuiz.xpm
	-rm -r $(HTMLPATH)
	-rm -r $(QUIZPATH)
	-rm -r $(DOCDIR)

pkg-dos: veryclean
	move *.exe ..\..\bin
	cd ..\..
	-zip -9vr aqz$(SVERSION)-b.zip bin doc share
	-zip -9vr aqz$(SVERSION)-s.zip source

# End
