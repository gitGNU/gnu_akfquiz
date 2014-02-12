# $Id: Makefile,v 1.7 2006/12/02 16:39:43 akf Exp $

TXTFILES=mkquiz.txt scrquiz.txt grquiz.txt akfquiz.txt cgiquiz.txt \
         linequiz.txt quizstat.txt

HTMLFILES=mkquiz.html scrquiz.html grquiz.html akfquiz.html cgiquiz.html \
          linequiz.html quizstat.html

PDFFILES=mkquiz.pdf scrquiz.pdf grquiz.pdf akfquiz.pdf cgiquiz.pdf \
         linequiz.pdf quizstat.pdf

SHELL = /bin/sh


TIDY = tidy -ascii -q --show-warnings 0 \
            --doctype loose \
            --drop-proprietary-attributes 1 

## Use this, if you don't have the program "tidy"
#TIDY = cat

all: txt html pdf

txt: $(TXTFILES)
html: $(HTMLFILES)
pdf: $(PDFFILES)

clean:
	-rm -f $(TXTFILES) $(HTMLFILES) $(PDFFILES)

mkquiz.html: mkquiz.1
	-groff -man -Thtml mkquiz.1 | $(TIDY) > mkquiz.html

scrquiz.html: scrquiz.1
	-groff -man -Thtml scrquiz.1 | $(TIDY) > scrquiz.html

grquiz.html: grquiz.1
	-groff -man -Thtml grquiz.1 | $(TIDY) > grquiz.html

akfquiz.html: akfquiz.5
	-groff -man -Thtml akfquiz.5 | $(TIDY) > akfquiz.html

cgiquiz.html: cgiquiz.8
	-groff -man -Thtml cgiquiz.8 | $(TIDY) > cgiquiz.html

linequiz.html: linequiz.1
	-groff -man -Thtml linequiz.1 | $(TIDY) > linequiz.html

diaquiz.html: diaquiz.1
	-groff -man -Thtml diaquiz.1 | $(TIDY) > diaquiz.html

quizstat.html: quizstat.1
	-groff -man -Thtml quizstat.1 | $(TIDY) > quizstat.html


mkquiz.txt: mkquiz.1
	groff -man -Tutf8 mkquiz.1 | col -bx > mkquiz.txt

scrquiz.txt: scrquiz.1
	groff -man -Tutf8 scrquiz.1 | col -bx > scrquiz.txt

grquiz.txt: grquiz.1
	groff -man -Tutf8 grquiz.1 | col -bx > grquiz.txt

akfquiz.txt: akfquiz.5
	groff -man -Tutf8 akfquiz.5 | col -bx > akfquiz.txt

cgiquiz.txt: cgiquiz.8
	groff -man -Tutf8 cgiquiz.8 | col -bx > cgiquiz.txt

linequiz.txt: linequiz.1
	groff -man -Tutf8 linequiz.1 | col -bx > linequiz.txt

diaquiz.txt: diaquiz.1
	groff -man -Tutf8 diaquiz.1 | col -bx > diaquiz.txt

quizstat.txt: quizstat.1
	groff -man -Tutf8 quizstat.1 | col -bx > quizstat.txt

mkquiz.pdf: mkquiz.1
	groff -man -Tps mkquiz.1 | ps2pdf - mkquiz.pdf

scrquiz.pdf: scrquiz.1
	groff -man -Tps scrquiz.1 | ps2pdf - scrquiz.pdf

grquiz.pdf: grquiz.1
	groff -man -Tps grquiz.1 | ps2pdf - grquiz.pdf

akfquiz.pdf: akfquiz.5
	groff -man -Tps akfquiz.5 | ps2pdf - akfquiz.pdf

cgiquiz.pdf: cgiquiz.8
	groff -man -Tps cgiquiz.8 | ps2pdf - cgiquiz.pdf

linequiz.pdf: linequiz.1
	groff -man -Tps linequiz.1 | ps2pdf - linequiz.pdf

diaquiz.pdf: diaquiz.1
	groff -man -Tps diaquiz.1 | ps2pdf - diaquiz.pdf

quizstat.pdf: quizstat.1
	groff -man -Tps quizstat.1 | ps2pdf - quizstat.pdf


# End
