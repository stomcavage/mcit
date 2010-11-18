/*
**     Program: tcpserver.c
**      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
**        Date: September 28, 2009
** Description: Demonstration of multi-threaded socket programming. 
*/

#include <errno.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#define SERVER_PORT 14749
#define MAX_PENDING 5
#define MAX_LINE 256

void *thread_main(void *);

struct Thread_Args {
	int sock;
};

int main () {

	struct sockaddr_in sin;
	int s, new_s, rv;
	pthread_t thread_id;
	socklen_t sin_size;
	struct Thread_Args *thread_args;

	/* build address data structure using this machine's IP */
	memset((char *)&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(SERVER_PORT);

	/* Create a socket to use TCP */
	if ((s = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}

	/* Bind the new socket to SERVER_PORT on this machine */
	if ((bind(s, (struct sockaddr *)&sin, sizeof(sin))) < 0) {
		perror("bind");
		exit(1);
	}

	/* Wait for an incoming connection */
	listen(s, MAX_PENDING);

	while(1) {

		/* Accept all connection requests that come in from a client */
		sin_size = sizeof(sin);
		if ((new_s = accept(s, (struct sockaddr *)&sin, &sin_size)) < 0) {
			perror("accept");
			exit(1);
		}

		/* Create the arguments to pass to a new thread */
		if ((thread_args = (struct Thread_Args*)malloc(sizeof(struct Thread_Args))) == NULL) {
			fprintf(stderr, "memory failure");
			exit(1);
		}
		thread_args->sock = new_s;

		/* Create a thread to communicate with the client */
		if ((rv = pthread_create(&thread_id, NULL, thread_main, thread_args)) != 0)  {
			fprintf(stderr, "thread failure");
			exit(1);
		}
	}

	/* Close the socket once we're done with it*/
	close(new_s);

	return 0;
}

void *thread_main(void *thread_args) {

	char buf[MAX_LINE];
	char msg[MAX_LINE];
	int len, s, seq, saved_seq;

	/* Get the arguments for this thread */
	s = ((struct Thread_Args *)thread_args)->sock;
	free(thread_args);

	/* Receive text from the connected client */
	if ((len = recv(s, buf, sizeof(buf), 0)) < 0) {
		perror("receive");
		exit(1);
	}
	fputs(buf, stdout);

	/* Get the sequence number out of the received message */
	if (sscanf(buf, "%s %d", msg, &seq) == 2) {
		saved_seq = ++seq;
	}
	else {
		fprintf(stderr, "invalid message from client: %s\n", buf);
		pthread_exit(NULL);
	}

	/* clear out the buffer for re-use */
	memset(buf, 0, sizeof(buf));

	/* Send a message back to the client */
	len = sprintf(buf, "HELLO %d\n", seq);
	if ((send(s, buf, len, 0)) < 0) {
		perror("send");
		pthread_exit(NULL);
	}

	/* Receive a message from the client */
	if ((len = recv(s, buf, sizeof(buf), 0)) < 0) {
		perror("receive");
		exit(1);
	}
	fputs(buf, stdout);

	/* Get the sequence number out of the received message */
	if (sscanf(buf, "%s %d", msg, &seq) != 2) {
		fprintf(stderr, "invalid message from client: %s\n", buf);
		pthread_exit(NULL);
	}

	/* Validate that the received message is correct */
	if (seq != saved_seq + 1) {
		fprintf(stderr, "ERROR\n");
	}

	/* Close the socket */
	close(s);

	/* Close this thread */
	pthread_exit(NULL);
}

