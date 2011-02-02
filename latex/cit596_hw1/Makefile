.SUFFIXES: .dot .png 

all: hw1.pdf

hw1.pdf: hw1.tex 1_4_e 1_4_f 1_4_g 1_5_c 1_5_e 1_5_f 1_6
	pdflatex hw1.tex 

1_4_e: 1_4_e_a.png 1_4_e_b.png 1_4_e.png 

1_4_f: 1_4_f_a.png 1_4_f_b.png 1_4_f.png 

1_4_g: 1_4_g_a.png 1_4_g_b.png 1_4_g.png 

1_5_c: 1_5_c_a.png 1_5_c.png 

1_5_e: 1_5_e_a.png 1_5_e.png 

1_5_f: 1_5_f_a.png 1_5_f.png 

1_6: 1_6_c.png 1_6_e.png 1_6_g.png 1_6_i.png 1_6_j.png

.dot.png:
	rm -f $@
	dot $*.dot -Tpng > $*.png

clean:
	rm -f *.aux
	rm -f *.log
	rm -f *.png
