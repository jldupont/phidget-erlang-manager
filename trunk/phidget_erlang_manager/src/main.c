/**
 * main.c
 *
 *  Created on: 2009-04-17
 *      Author: Jean-Lou Dupont
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/types.h>

#include "../includes/logger.h"
#include "../includes/main.h"
#include "../includes/manager.h"
#include "../includes/queuer.h"
#include "../includes/server.h"

// ================================================


int main(int argc, char **argv) {

	int port=0;
	char *cookie;  int _cookie=0;
	pthread_t sThread;

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

	printf("port: %u", port);

	// Launch daemon

	// pass along some parameters to the server thread
	server_params params;
	params.port = port;
	params.cookie = cookie;

	int result = pthread_create(&sThread, NULL, &server_thread, (void *) &params);
	if (result) {
		showMessage( MSG_ERROR_SERVER_THREAD );
		return 1;
	}

	queuer_init();
	CPhidgetManagerHandle phidm;
	phidm = manager_create( (void*) queuer_queue );

	pthread_join( sThread, NULL );

	return 0;

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
 * Show message
 */
void showMessage(int msg_id) {

	printf( "%s", messages[msg_id] );

}// showMessage


