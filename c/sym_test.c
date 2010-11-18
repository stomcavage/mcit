#include <stdio.h>
#include "sym_table.h"

int main() {
	/* Insert some nodes */
	sym_table_insert("START", 100);
	sym_table_insert("END", 300);
	sym_table_insert("MIDDLE", 200);
	/* Retrieve the nodes */
	fprintf(stdout, "START is at %d\n", sym_table_lookup("START"));
	fprintf(stdout, "MIDDLE is at %d\n", sym_table_lookup("MIDDLE"));
	fprintf(stdout, "END is at %d\n", sym_table_lookup("END"));
	/* This should generate an error */
	sym_table_insert("START", 100);
	return(0);
}

