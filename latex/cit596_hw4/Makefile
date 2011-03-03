.SUFFIXES: .dot .png .text .pdf

all: hw4.pdf

hw4.pdf: hw4.tex 2_5_b.png 2_5_c.png 2_5_e.png
	pdflatex hw4.tex 
	pdflatex hw4.tex 

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
