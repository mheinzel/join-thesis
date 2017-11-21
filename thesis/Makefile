TEX= pdflatex -output-directory=tmp
BIB= bibtex

TEXFILES= thesis.tex \
		  titlepage/titlepage.tex \
		  background/pi.tex \
		  background/join.tex \
		  bibliography/wrapper.tex
BIBFILES= bibliography/thesis.bib
STYFILES= style/thesis.sty \
		  style/terms.sty \
		  style/math.sty


.PHONY: all thesis clean

all: thesis

thesis: thesis.pdf

thesis.pdf: $(TEXFILES) $(BIBFILES) $(STYFILES)
	$(TEX) $(TEXFILES)
	$(BIB) tmp/thesis
	$(TEX) $(TEXFILES)
	$(TEX) $(TEXFILES)
	mv tmp/thesis.pdf .

clean:
	rm -f tmp/*
	rm -f thesis.pdf

