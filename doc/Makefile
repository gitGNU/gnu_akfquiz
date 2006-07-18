
all: en de

en:
	$(MAKE) -C english

de:
	$(MAKE) -C deutsch

txt:
	$(MAKE) -C english txt
	$(MAKE) -C deutsch txt

html:
	$(MAKE) -C english html
	$(MAKE) -C deutsch html

pdf:
	$(MAKE) -C english pdf
	$(MAKE) -C deutsch pdf

clean:
	$(MAKE) -C english clean
	$(MAKE) -C deutsch clean

distclean: clean

new: distclean

# End
