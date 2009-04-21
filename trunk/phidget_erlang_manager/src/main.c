/**
 * main.c
 *
 *  Created on: 2009-04-17
 *      Author: Jean-Lou Dupont
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>

#include "erl_interface.h"
#include "ei.h"

#include "../includes/main.h"

// ================================================


int main(int argc, char **argv) {

	int port=-1;
	char *cookie;  int _cookie=0;

	// Extract command-line parameters
	if (argc<3) {
		showHelp( MSG_MISSING_ARGUMENTS );
		return 1;
	}

	// validate port
	port = atoi( argv[1] );
	if (port == 0) {
		showHelp( MSG_PORT_INTEGER );
		return 1;
	}

	// extract 'cookie' string...
	//  shouldn't be an integer
	cookie = argv[2];
	_cookie = atoi(cookie);
	if (_cookie!=0) {
		showHelp( MSG_COOKIE_STRING );
		return 1;
	}

	// Launch daemon


}//[/main]




/*
 * Displays on stdout a help message
 */
void showHelp(int msg_id) {

	const char msg[] = "\
\nPhidget Manager / Erlang Server\
\n Erlang Node Server to Phidget Manager\
\n\
\nUsage:\
\n======\
\n       phidget_erl_manager PORT COOKIE\
\n\n";

	printf( "%s", msg );

	printf( "%s", messages[msg_id] );

}//[/showHelp]




/**
 * Opens the local socket port
 */
int open_port(int port) {

  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);

  listen(listen_fd, 5);

  return listen_fd;
}//[/open_port]

