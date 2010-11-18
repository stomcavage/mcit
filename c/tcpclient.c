/*
**     Program: tcpclient.c
**      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
**        Date: September 28, 2009
** Description: Demonstration of socket programming. Send message with integer 
**              back and forth between client and server, incrementing
**              integer as it goes.
*/

#include <errno.h>
#include <limits.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

#define SERVER_PORT 14749
#define MAX_LINE 256

int strtoi(char *);

int main (int argc, char *argv[]) {

	char *host, *num_string;
	char buf[MAX_LINE];
	char msg[MAX_LINE];
	int s, len, seq, saved_seq;
	struct hostent *hp;
	struct sockaddr_in sin;

	/* Process user arguments */
	if (argc == 3) {
		host = argv[1];
		num_string = argv[2];
		seq = strtoi(num_string);
	}
	else {
		fprintf(stderr, "usage: %s host sequence\n", argv[0]);
		exit(1);
	}

	/* Translate host's name into an IP address */
	hp = gethostbyname(host);
	if (!hp) {
		fprintf(stderr, "%s: unknown host: %s\n", argv[0], host);
		exit(1);
	}

	/* build host's address data structure */
	memset((char *)&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	memmove((char *)&sin.sin_addr, hp->h_addr, hp->h_length);
	sin.sin_port = htons(SERVER_PORT);

	/* Create a socket to use TCP */
	if ((s = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}

	/* Connect the socket using the host's data structure */
	if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
		perror("connect");
		close(s);
		exit(1);
	}

	/* Send the entered text to the client */
	/* Note: buf is large enough to hold this text without an overflow */
	saved_seq = seq;
	len = sprintf(buf, "HELLO %d\n", seq) ;
	if ((send(s, buf, len, 0)) < 0) {
		perror("send");
		exit(1);
	}

	/* clear out the buffer for re-use */
	memset(buf, 0, sizeof(buf));

	/* Receive and print text from the server */
	if ((len = recv(s, buf, sizeof(buf), 0)) < 0) {
		perror("receive");
		exit(1);
	}
	fputs(buf, stdout);

	/* Get the sequence number out of the received message */
	if (sscanf(buf, "%s %d", msg, &seq) != 2) {
		fprintf(stderr, "invalid message from server: %s\n", buf);
		exit(1);
	}

	/* If the received message is valid, send a message back to the server */
	if (seq == saved_seq + 1) {
		memset(buf, 0, sizeof(buf));
		len = sprintf(buf, "HELLO %d\n", seq + 1);
		if ((send(s, buf, len, 0)) < 0) {
			perror("send");
			exit(1);
		}
	}
	else {
		fprintf(stderr, "ERROR\n");
	}

	/* Close the socket */
	close(s);

	return 0;
}

int strtoi(char *num_string) {

	char *end_ptr;
	long num_long;

	/* Convert user's input into a long (but constrain it to an int)*/
	errno = 0;
	num_long = strtol(num_string, &end_ptr, 0);
	
	if (ERANGE == errno) { /* number is larger or smaller than a long */
		fprintf(stderr, "sequence must be between %d and %d\n", INT_MIN, INT_MAX);
		exit(1);
	}
	else if (num_long > INT_MAX) { /* number is larger than an int */
		fprintf(stderr, "sequence must be %d or less\n", INT_MAX);
		exit(1);
	}
	else if (num_long < INT_MIN) { /* number is smaller than an int */
		fprintf(stderr, "sequence must be %d or greater\n", INT_MIN);
		exit(1);
	}
	else if (end_ptr == num_string) { /* number isn't numeric */
		fprintf(stderr, "sequence is not a valid number\n");
		exit(1);
	}

	return (int)num_long;
}

