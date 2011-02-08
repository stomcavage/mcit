.SUFFIXES: .dot .png 

all: hw2.pdf

hw2.pdf: hw2.tex 3.png
	pdflatex hw2.tex 
	pdflatex hw2.tex 

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
