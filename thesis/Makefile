TEX= pdflatex -output-directory=tmp
BIB= bibtex

TEXFILES= thesis.tex \
		  prelude/titlepage.tex \
		  prelude/authorship.tex \
		  preliminaries/pi.tex \
		  preliminaries/join.tex \
		  preliminaries/actor.tex \
		  encoding/encoding.tex \
		  implementation/basic.tex \
		  implementation/distributed.tex \
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

