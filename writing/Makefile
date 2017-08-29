TEX=pdflatex
BIB=bibtex

all: expose.pdf

clean:
	rm -f *.log *.aux *.out *.blg *.toc *.bbl *.pdf

expose.pdf: expose.tex
	$(TEX) expose.tex
	$(BIB) expose
	$(TEX) expose.tex
	$(TEX) expose.tex
	rm -f *.log *.aux *.out *.toc *.blg
