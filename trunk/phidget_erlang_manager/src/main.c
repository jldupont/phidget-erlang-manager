/**
 * main.c
 *
 *  Created on: 2009-04-17
 *      Author: Jean-Lou Dupont
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>

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
