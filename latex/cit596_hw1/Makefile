.SUFFIXES: .dot .png 

all: hw1.pdf

hw1.pdf: hw1.tex 1_4_e
	pdflatex hw1.tex 

1_4_e: 1_4_e_a.png 1_4_e_b.png 1_4_e.png

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
