TEX= pdflatex -output-directory=tmp
BIB= bibtex

TEXFILES= thesis.tex \
		  prelude/titlepage.tex \
		  prelude/authorship.tex \
		  abstract/german.tex \
		  abstract/english.tex \
		  introduction/introduction.tex \
		  preliminaries/join.tex \
		  preliminaries/actor.tex \
		  encoding/encoding.tex \
		  implementation/basic.tex \
		  implementation/distributed.tex \
		  implementation/code/full.tex \
		  conclusion/conclusion.tex \
		  conclusion/future_work.tex \
		  bibliography/wrapper.tex
BIBFILES= bibliography/thesis.bib
STYFILES= style/thesis.sty \
		  style/terms.sty \
		  style/listings.sty \
		  style/math.sty
CODEFILES=implementation/code/*


.PHONY: all thesis clean

all: thesis

thesis: thesis.pdf

thesis.pdf: $(TEXFILES) $(BIBFILES) $(STYFILES) $(CODEFILES)
	$(TEX) $(TEXFILES)
	$(BIB) tmp/thesis
	$(TEX) $(TEXFILES)
	$(TEX) $(TEXFILES)
	mv tmp/thesis.pdf .

clean:
	rm -f tmp/*
	rm -f thesis.pdf

