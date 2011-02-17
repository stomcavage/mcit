.SUFFIXES: .dot .png 

all: hw2.pdf

hw2.pdf: hw3.tex 1_13 1_16.png 1_17 1_19.png 1_21 1_28.png
	pdflatex hw3.tex 
	pdflatex hw3.tex 

1_13: 1_13_a.png 1_13_b.png

1_17: 1_17_a.png 1_17_b.png

1_21: 1_21_a.png

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
