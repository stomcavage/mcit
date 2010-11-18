/*
**     Program: udpserver.c
**      Author: Steven Tomcavage (stomcava@seas.upenn.edu)
**        Date: September 28, 2009
** Description: Demonstration of socket programming. This program receives 
**              some text from the client program, prints it, and echoes it
**              back to the client. Works over UDP.
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
#define MAX_PENDING 5
#define MAX_LINE 256

int main () {

	char buf[MAX_LINE];
	int len, s;
	socklen_t fromlen;
	struct sockaddr_in sin;
	struct sockaddr_storage addr;

	/* build address data structure using this machine's IP */
	memset((char *)&sin, 0, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	sin.sin_port = htons(SERVER_PORT);

	/* Create a socket to use TCP */
	if ((s = socket(PF_INET, SOCK_DGRAM, 0)) < 0) {
		perror("socket");
		exit(1);
	}

	/* Bind the new socket to SERVER_PORT on this machine */
	if ((bind(s, (struct sockaddr *)&sin, sizeof(sin))) < 0) {
		perror("bind");
		exit(1);
	}

	while(1) {

		/* Receive text from the client */
		fromlen = sizeof(addr);
		if ((len = recvfrom(s, buf, sizeof(buf), 0, (struct sockaddr*)&addr, 
				&fromlen)) < 0) {
			perror("receive");
			exit(1);
		}

		fputs(buf, stdout);

		/* Echo the received text back to the client */
		if ((len = sendto(s, buf, len, 0, (struct sockaddr*)&addr, 
				sizeof(addr))) < 0) {
			perror("send");
			exit(1);
		}
	}

	return 0;
}

