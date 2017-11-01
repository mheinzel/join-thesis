TEX=pdflatex -output-directory=tmp
BIB=bibtex

TEXFILES=thesis.tex
BIBFILES=bibliography/thesis.bib


.PHONY: all thesis clean-logs clean

all: thesis

thesis: thesis.pdf clean-logs

thesis.pdf: $(TEXFILES) $(BIBFILES)
	$(TEX) $(TEXFILES)
	$(BIB) tmp/thesis
	$(TEX) $(TEXFILES)
	$(TEX) $(TEXFILES)
	mv tmp/thesis.pdf .

clean: clean-logs
	rm -f tmp/*
