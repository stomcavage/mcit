.SUFFIXES: .dot .png .text .pdf

all: hw6.pdf

hw6.pdf: hw6.tex 
	pdflatex hw6.tex 

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
