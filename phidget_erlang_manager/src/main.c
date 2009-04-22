/**
 * main.c
 *
 *  Created on: 2009-04-17
 *      Author: Jean-Lou Dupont
 */

#include <getopt.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/types.h>


#include "../includes/logger.h"
#include "../includes/main.h"
#include "../includes/manager.h"
#include "../includes/queuer.h"
#include "../includes/server.h"
#include "../includes/signals.h"
#include "../includes/daemon.h"


typedef enum _CMDLINE_ERRORS {

	CMDLINE_OK = 0,
	CMDLINE_MISSING_ARGUMENTS,
	CMDLINE_MISSING_PORT,
	CMDLINE_MISSING_COOKIE,
	CMDLINE_INVALID_COMMAND,
	CMDLINE_INVALID_PORT,
	CMDLINE_INVALID_COOKIE,

} CmdLineError;

// PRIVATE
int getOptionsAndCommand(int argc, char **argv, int *port, char *cookie, char *command);
int validatePortCookie(int *port, char *cookie);
void handleDaemonErrorCode(DaemonErrorCode code);

// ================================================


int main(int argc, char **argv) {

	int result;
	int port=0;
	char *cookie, *command;
	pthread_t sThread;


	// Extract command-line parameters
	if (argc<3) {
		showHelp( MSG_MISSING_ARGUMENTS );
		return 1;
	}

	result = getOptionsAndCommand(argc, argv, &port, &cookie, &command);
	if (0!=result) {
		return 1;
	}

	result = validatePortCookie(port, cookie);
	if (0!=result) {
		return 1;
	}

	result = daemon_validate_command(command);
	if (0!=result) {
		showMessage( MSG_INVALID_COMMAND );
		return 1;
	}

	// before any threads are started...
	signals_init();

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

	//  DAEMON
	// *!*!*!*!
	DaemonErrorCode code = daemon_handle_command("phidgetmanager", command);
	if (DAEMON_CODE_OK != code) {
		handleDaemonErrorCode( code );
		return 1;
	}



	pthread_join( sThread, NULL );

	return 0;

}//[/main]



/*
 * Displays on stdout a help message
 */
void showHelp(int msg_id) {

	const char msg[] = "\
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

void showMessageString(int msg_id, char *string) {
	printf( messages[msg_id], string );
}
/**
 * Retrieves the options and the command
 *  from the command line
 *
 */
int getOptionsAndCommand(int argc, char **argv, int *port, char *cookie, char *command) {

	//worst case
	port = NULL;
	cookie = NULL;
	command = NULL;

	char _current_option = '';
	int  _options_scanning=0;

	opterr = 0;

	while ((c = getopt (argc, argv, "pc:")) != -1)
	   switch (c)	{
	   case 'p':
		   char *_port = optarg;
		   *port = atoi( _port );
		   break;
	   case 'c':
		   cookie = optarg;
		   break;
	   case '?':
		   _options_scanning = -1; //error
		   _current_option = optopt;
		   break;
	   default:
		   abort();
	   }

	if (0!=_options_scanning) {
		if (('p' == _current_option) || ('c' == _current_option)) {
			showMessageString( MSG_MISSING_OPTION_ARGUMENT, _current_option);
		} else {
			showMessage( MSG_INVALID_OPTION );
		}
		return 1;
	}

	if ((optind > 0) && (optind<argc)) {
		command = argv[optind];
	} else {
		showMessage( MSG_INVALID_COMMAND );
		return 1;
	}


	return 0; // OK
}//getOptionsAndCommand

/**
 * Crude validation of parameters
 */
int validatePortCookie(int *port, char *cookie) {

	if (*port == 0) {
		showHelp( MSG_PORT_INTEGER );
		return 1;
	}

	// validate (somewhat) cookie parameter
	if (NULL!=cookie) {
		showHelp( MSG_COOKIE_STRING );
		return 1;
	}

	return 0;
}

/**
 *  Prints an error message
 */
void handleDaemonErrorCode(DaemonErrorCode code) {

}
