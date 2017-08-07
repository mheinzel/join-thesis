TEX=pdflatex
BIB=bibtex

all: questions.pdf expose.pdf

clean:
	rm -f *.log *.aux *.toc *.pdf

questions.pdf: questions.tex
	$(TEX) questions.tex
	$(TEX) questions.tex
	rm -f *.log *.aux *.toc

expose.pdf: expose.tex
	$(TEX) expose.tex
	$(TEX) expose.tex
	rm -f *.log *.aux *.toc
