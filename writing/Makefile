TEX=pdflatex
BIB=bibtex

all: expose.pdf

clean:
	rm -f *.log *.aux *.toc *.pdf

expose.pdf: expose.tex
	$(TEX) expose.tex
	$(TEX) expose.tex
	rm -f *.log *.aux *.toc
