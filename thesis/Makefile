TEX= pdflatex
THESISARGS= -output-directory=tmp
SLIDESARGS= -output-directory=slidestmp
BIB= bibtex

TEXFILES= prelude/titlepage.tex \
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


.PHONY: all thesis slides clean

all: thesis slides

thesis: thesis.pdf

thesis.pdf: thesis.tex $(TEXFILES) $(BIBFILES) $(STYFILES) $(CODEFILES)
	$(TEX) $(THESISARGS) thesis.tex $(TEXFILES)
	$(BIB) tmp/thesis
	$(TEX) $(THESISARGS) thesis.tex $(TEXFILES)
	$(TEX) $(THESISARGS) thesis.tex $(TEXFILES)
	mv tmp/thesis.pdf .

slides: slides.pdf

slides.pdf: slides.tex $(STYFILES) $(CODEFILES)
	$(TEX) $(SLIDESARGS) slides.tex
	$(TEX) $(SLIDESARGS) slides.tex
	mv slidestmp/slides.pdf .

clean:
	rm -f tmp/*
	rm -f slidestmp/*
	rm -f thesis.pdf
	rm -f slides.pdf
