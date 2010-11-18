/*        File: proc2.c
 *      Author: Steven Tomcavage
 *        Date: April, 2008
 * Description: Takes a string command and reverses the case of each letter.
 */
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
	char* string = NULL;
	char* temp = NULL;
	
	/* Check the arguments input by the user */
	if (argc < 2) {
		fprintf(stderr, "Usage: proc2 <string>\n");
		exit(1);
	}

	/* Since we're modifying a string in place, copy it to a local variable */
	if ((string = strdup(argv[1])) == NULL) {
		fprintf(stderr, "ERROR: Unable to copy input string\n");
		exit(1);
	}

	/* Go though each char in the string and convert its case */
	for (temp = string; *temp != '\0'; temp++) {
		if (isupper(*temp)) {
			*temp = tolower(*temp);
		}
		else {
			*temp = toupper(*temp);
		}
	}

	fprintf(stdout, "%s", string);
	
	return 0;
}
