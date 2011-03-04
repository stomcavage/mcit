.SUFFIXES: .dot .png .text .pdf

all: hw5.pdf

hw5.pdf: hw5.tex 
	pdflatex hw5.tex 
	pdflatex hw5.tex 

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
