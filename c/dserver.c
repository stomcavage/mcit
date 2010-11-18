/*
**     Program: dserver.c
**      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
**        Date: September 28, 2009
** Description: Demonstration of socket programming. This program receives 
**              some text from the client program, prints it, and echoes it
**              back to the client.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <unistd.h>

#define SERVER_PORT 14749
#define MAX_PENDING 5
#define MAX_LINE 256

int main () {

	struct sockaddr_in sin;
	char buf[MAX_LINE];
	int len;
	int s, new_s;
	socklen_t sin_size;

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

		/* Receive text from the connected client */
		while ((len = recv(new_s, buf, sizeof(buf), 0))) {

			fputs(buf, stdout);

			/* Echo the received text back to the client */
			if ((send(new_s, buf, len, 0)) < 0) {
				perror("send");
				exit(1);
			}
		}
	}

	/* Close the socket once we're done with it*/
	close(new_s);

	return 0;
}

