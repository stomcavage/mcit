.SUFFIXES: .dot .png .text .pdf

all: hw4.pdf

.tex.pdf:
	pdflatex $*.tex
	pdflatex $*.tex

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
