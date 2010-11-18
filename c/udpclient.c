/*
**     Program: udpclient.c
**      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
**        Date: September 28, 2009
** Description: Demonstration of socket programming. Sends user input to 
**              server, which server then echoes back to this program for 
**              printing. Works over UDP.
*/

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

int main (int argc, char *argv[]) {

	char *host;
	char buf[MAX_LINE];
	int s, len;
	socklen_t fromlen;
	struct hostent *hp;
	struct sockaddr_in sin;
	struct sockaddr_storage addr;

	/* Process user arguments */
	if (argc == 2) {
		host = argv[1];
	}
	else {
		fprintf(stderr, "usage: %s host\n", argv[0]);
		exit(1);
	}

	/* Translate host's name into an IP address */
	hp = gethostbyname(host);
	if (!hp) {
		fprintf(stderr, "unknown host: %s\n", host);
		exit(1);
	}

	/* build host's address data structure */
	memset((char *)&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	memmove((char *)&sin.sin_addr, hp->h_addr, hp->h_length);
	sin.sin_port = htons(SERVER_PORT);

	/* Create a socket to use UDP */
	if ((s = socket(PF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}

	while (fgets(buf, sizeof(buf), stdin)) { 

		/* Send the entered text to the client */
		buf[MAX_LINE - 1] = '\0';
		len = strlen(buf) + 1;
		sendto(s, buf, len, 0, (struct sockaddr*)&sin, sizeof(sin));

		/* clear out the buffer for re-use */
		memset(buf, 0, sizeof(buf));

		/* Receive and print text from the server */
		fromlen = sizeof(addr);
		if ((len = recvfrom(s, buf, sizeof(buf), 0, (struct sockaddr*)&addr, 
				&fromlen)) < 0) 
		{
			perror("receive");
			exit(1);
		}
		fputs(buf, stdout);
	}

	return 0;
}

