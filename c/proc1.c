/*        File: proc1.c
 *      Author: Steven Tomcavage
 *        Date: April, 2008
 * Description: Takes a string command and reverses it, then passes the
 *              reversed string to proc2 as a new process.
 */
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char* argv[]) {
	char* string = NULL;
	char temp;
	int i, j, status;
	pid_t pid, child_pid;
	
	/* Check the arguments input by the user */
	if (argc < 2) {
		fprintf(stderr, "Usage: proc1 <string>\n");
		exit(1);
	}

	/* Since we're modifying a string in place, copy it to a local variable */
	if ((string = strdup(argv[1])) == NULL) {
		fprintf(stderr, "ERROR: Unable to copy input string\n");
		exit(1);
	}

	/* Go through the string from both ends simultaneously, swapping chars */
	for (i = 0, j = strlen(string) - 1; i < j; i++, j--) {
		temp = string[i];
		string[i] = string[j];
		string[j] = temp;
	}

	/* Now create a new process */
	pid = fork();

	/* Set the child process to execute proc2 using the modified string value */
	if (pid == 0) { 
		if(execl("./proc2", "proc2", string, NULL) < 0){
			fprintf(stderr, "ERROR: Command proc2 not found\n");
			exit(1);
		}
		exit(0);
	}
	/* Set the parent process to wait for the child to finish */
	else if (pid > 0) {
		child_pid = wait(&status);
		if (child_pid < 0) {
			fprintf(stderr, "ERROR: An error happened in the process proc2\n");
		}
	}
	/* Handle the case where the fork failed */
	else {
		fprintf(stderr, "ERROR: Unable to create new process.\n");
		exit(1);
	}
	
	return 0;
}
