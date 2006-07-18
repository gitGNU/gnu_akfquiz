TXTFILES=mkquiz.txt scrquiz.txt grquiz.txt akfquiz.txt cgiquiz.txt \
         linequiz.txt gtkquizchooser.txt

HTMLFILES=mkquiz.html scrquiz.html grquiz.html akfquiz.html cgiquiz.html \
          linequiz.html gtkquizchooser.html

PDFFILES=mkquiz.pdf scrquiz.pdf grquiz.pdf akfquiz.pdf cgiquiz.pdf \
         linequiz.pdf gtkquizchooser.pdf


all: txt html pdf

txt: $(TXTFILES)
html: $(HTMLFILES)
pdf: $(PDFFILES)

clean:
	-rm -f $(TXTFILES) $(HTMLFILES) $(PDFFILES)

mkquiz.html: mkquiz.1
	groff -man -Thtml mkquiz.1 > mkquiz.html

scrquiz.html: scrquiz.1
	groff -man -Thtml scrquiz.1 > scrquiz.html

grquiz.html: grquiz.1
	groff -man -Thtml grquiz.1 > grquiz.html

akfquiz.html: akfquiz.5
	groff -man -Thtml akfquiz.5 > akfquiz.html

cgiquiz.html: cgiquiz.8
	groff -man -Thtml cgiquiz.8 > cgiquiz.html

linequiz.html: linequiz.1
	groff -man -Thtml linequiz.1 > linequiz.html

gtkquizchooser.html: gtkquizchooser.1
	groff -man -Thtml gtkquizchooser.1 > gtkquizchooser.html

mkquiz.txt: mkquiz.1
	groff -man -Tlatin1 mkquiz.1 | col -bx > mkquiz.txt

scrquiz.txt: scrquiz.1
	groff -man -Tlatin1 scrquiz.1 | col -bx > scrquiz.txt

grquiz.txt: grquiz.1
	groff -man -Tlatin1 grquiz.1 | col -bx > grquiz.txt

akfquiz.txt: akfquiz.5
	groff -man -Tlatin1 akfquiz.5 | col -bx > akfquiz.txt

cgiquiz.txt: cgiquiz.8
	groff -man -Tlatin1 cgiquiz.8 | col -bx > cgiquiz.txt

linequiz.txt: linequiz.1
	groff -man -Tlatin1 linequiz.1 | col -bx > linequiz.txt

gtkquizchooser.txt: gtkquizchooser.1
	groff -man -Tlatin1 gtkquizchooser.1 | col -bx > gtkquizchooser.txt

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

gtkquizchooser.pdf: gtkquizchooser.1
	groff -man -Tps gtkquizchooser.1 | ps2pdf - gtkquizchooser.pdf


# End
