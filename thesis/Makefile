TEX= pdflatex -output-directory=tmp
BIB= bibtex

TEXFILES= thesis.tex \
		  background/pi.tex
BIBFILES= bibliography/thesis.bib
STYFILES= style/thesis.sty \
		  style/grammar.sty


.PHONY: all thesis clean-logs clean

all: thesis

thesis: thesis.pdf clean-logs

thesis.pdf: $(TEXFILES) $(BIBFILES) $(STYFILES)
	$(TEX) $(TEXFILES)
	$(BIB) tmp/thesis
	$(TEX) $(TEXFILES)
	$(TEX) $(TEXFILES)
	mv tmp/thesis.pdf .

clean: clean-logs
	rm -f tmp/*
