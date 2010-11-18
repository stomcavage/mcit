/*        File: thread.c
 *      Author: Steven Tomcavage
 *        Date: April, 2008
 * Description: Creates a thread pool where each thread processes its own file.
 */
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_FILENAME 80
#define MAX_LINE_LEN 80
#define MIN_THREADS 1
#define MAX_THREADS 4

/* Global variable accessible by all threads */
int global_sum = 0;

/* Declare a mutex for thread locking */
pthread_mutex_t thread_mutex;

/* Setup a struct and type to hold thread arguments */
typedef struct thread_arg_struct thread_arg_t;

struct thread_arg_struct {
	int thread_num;
	FILE *input_file;
};

/* function prototypes here */
void *process_files(void *);
void print_usage();
void fatal_err(const char *);
FILE *get_thread_file(int);

int main(int argc, char* argv[]) {
	int num_threads, i;
	pthread_attr_t thread_attr;
	pthread_t threads[MAX_THREADS];
	thread_arg_t thread_args[MAX_THREADS];
	void *status;

	/* Validate that the user input some value */
	if (argc < 2) {
		print_usage();
	}
	
	/* Validate the input param is valid */
	num_threads = atoi(argv[1]);
	if (num_threads < MIN_THREADS || num_threads > MAX_THREADS) {
		print_usage();
	}

	/* Init the error number to zero (no error) */
	errno = 0;

	/* Init the thread attributes to create joinable threads */
	pthread_attr_init(&thread_attr);
	pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_JOINABLE);

	/* Init the thread mutex */
	pthread_mutex_init(&thread_mutex, NULL);
	
	for (i = 0; i < num_threads; i++) {
		
		/* Save the number of this thread as a thread argument */
		thread_args[i].thread_num = i;

		/* Get a file handle for this thread */
		thread_args[i].input_file = get_thread_file(i);
		
		/* Create a thread and pass it its number and file handle */
		if (pthread_create(
			&threads[i], 
			&thread_attr, 
			process_files, 
			(void *)&thread_args[i]
		) != 0) {
			fatal_err("ERROR: unable to create thread");
		}
	}
	
	/* Free the memory used by the thread attributes */
	pthread_attr_destroy(&thread_attr);

	/* Wait for all threads to end before continuing */
	for (i = 0; i < num_threads; i++) {
		if (pthread_join(threads[i], &status) != 0) {
			fatal_err("ERROR: failed to join to thread");
		}
	}

	/* Close all the open file handles */
	for (i = 0; i < num_threads; i++) {
		fclose(thread_args[i].input_file);
	}
	
	/* Let the user know what the sum from all threads is */
	fprintf(stdout, "The total sum from all threads is %d\n", global_sum);
	
	/* Free the memory used by the thread mutex */
	pthread_mutex_destroy(&thread_mutex);

	pthread_exit(NULL);
	
	return 0;
}

/*
 * Associates a file with a thread and opens it for reading.
 */
FILE *get_thread_file(int thread_num) {
	char filename[MAX_FILENAME];
	FILE *file_handle;
	int rc;

	/* Get the name of the file to associate with this thread */
	rc = snprintf(filename, MAX_FILENAME, "input%d.txt", thread_num);
	if (rc < 0 || rc >= MAX_FILENAME) {
		fatal_err("ERROR: unable to format filename for reading");
	}
		
	/* Open this thread's file for reading and save it as an argument */
	if ( (file_handle  = fopen(filename, "r")) == NULL) {
		fatal_err("ERROR: cannot open file for input");
	}

	return file_handle;
}

/*
 * For a thread, reads each line from the file provided and adds the numeric
 * value on each line to a local total for this thread and to a global total.
 */
void *process_files(void *args) {
	char line[MAX_LINE_LEN];
	int local_sum = 0;
	int lines_read = 0;
	int single_value;
	thread_arg_t *my_thread_args;
	
	my_thread_args = (thread_arg_t *)args;

	/* Read each line in the file associated with this thread */
	while (fgets(line, MAX_LINE_LEN, my_thread_args->input_file) != NULL) {
		lines_read++;
		/* If this line has a number in it, add it to the local total */
		if (sscanf(line, "%d", &single_value) > 0) {
			local_sum += single_value;
		}
	}
	
	 /* Lock the global variable and update it */
	 pthread_mutex_lock(&thread_mutex);
	 global_sum += local_sum;
	 pthread_mutex_unlock(&thread_mutex);

	/* Let the user know what the results are for this thread */
	fprintf(
		stdout, 
		"Thread %d processed %d lines and produced a total sum of %d\n",
		my_thread_args->thread_num, 
		lines_read,
		local_sum
	);

	/* Let the calling routine know that this thread exited properly */
	pthread_exit((void *) 0);
}

/* 
 * Prints the usage message if user doesn't provide correct arguments and
 * then exits with a 1 value.
 */ 
void print_usage() {
	fprintf(
		stderr, 
		"\nUsage: thread <number of threads>\n"
	);
	fprintf(
		stderr, 
		"\nwhere: <number of threads> is a number between %d and %d, inclusive\n\n", 
		MIN_THREADS,
		MAX_THREADS
	);			
	exit(1);
}

/* 
 * Prints an error message (including system defined error string, if
 * available), and then exits with -1 value.
 */ 
void fatal_err(const char *local_msg) {
	if (errno != 0) {
		perror(local_msg);
	}
	else {
		fprintf(stderr, "%s\n", local_msg);
	}
	exit(-1);
}

