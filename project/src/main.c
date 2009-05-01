/**
 * @file main.c
 *
 * @date   2009-04-17
 * @author Jean-Lou Dupont
 */

#include <getopt.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/types.h>


#include "helpers.h"
#include "logger.h"
#include "main.h"
#include "manager.h"
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
int getOptionsAndCommand(int argc, char **argv, int *port, char **cookie, char **command);
int validatePortCookie(int port, char *cookie);
void handleDaemonErrorCode(DaemonErrorCode code);
int codeToMsg(DaemonErrorCode code);
void showHelp(int msg_id);
void showMessage(int msg_id);
void showMessageEx(int msg_id, ...);



// ================================================


int main(int argc, char **argv) {

	int result;
	int port=0;
	char *cookie, *command;
	pthread_t sThread;


	result = getOptionsAndCommand(argc, argv, &port, &cookie, &command);
	if (0!=result) {
		return 1;
	}

	DEBUG_MSG("DEBUG: port    [%u]\n", port);
	DEBUG_MSG("DEBUG: cookie  [%s]\n", cookie);
	DEBUG_MSG("DEBUG: command [%s]\n", command);

	result = daemon_validate_command(command);
	if (0!=result) {
		showMessage( MSG_INVALID_COMMAND );
		return 1;
	}

	// if we have a ``start`` command, we need parameters:
	if (daemon_is_start_command(command)) {
		result = validatePortCookie(port, cookie);
		if (0!=result) {
			return 1;
		}
	}

	// before any threads are started...
	signals_init();

	//  DAEMON
	// *!*!*!*!
	DaemonErrorCode code = daemon_handle_command("phidgetmanager", command);

	// should we exit right now?
	if (DAEMON_CODE_EXITING == code) {
		return 1;
	}

	// the command was most probably a ``start``
	//  but there was an error...
	if (DAEMON_CODE_OK != code) {
		handleDaemonErrorCode( code );
		return 1;
	}



	// *!*!*!*!* START SUCCESSFUL !@!@!@@!@!!@!!
	// =========================================


	/*
	 *  COMMUNICATIONS
	 *  ==============
	 *
	 *  PhidgetManager  < -- > Erlang Cnode
	 */

	//{
	qpipe *queue_pipe;

	queue_pipe = qpipe_create();
	if (NULL==queue_pipe) {
		doLog(LOG_ERR, "unable to create qpipe");
		goto _close;
	}

	qport_context *qp_phm, *qp_erl;

	qp_phm = qport_init( queue_pipe, QPORT_0 );
	qp_erl = qport_init( queue_pipe, QPORT_1 );

	if (NULL == qp_phm) {
		doLog(LOG_ERR, "unable to create manager port");
		goto _close;
	}

	if (NULL == qp_erl) {
		doLog(LOG_ERR, "unable to create server port");
		goto _close;
	}
	//}


	//{
	server_params params;

	params.port   = port;
	params.cookie = cookie;
	params.qpc    = qp_erl;


	int result2 = pthread_create(&sThread, NULL, &server_thread, (void *) &params);
	if (result2) {
		showMessage( MSG_ERROR_SERVER_THREAD );
		return 1;
	}
	//}


	CPhidgetManagerHandle phidm;
	phidm = manager_create( qp_phm );

	// running in the server & manager threads
	// at this point

	int signal_caught;

_close:
//====

	do {
		signal_caught = signals_get_signal();
	} while(signal_caught!=SIGTERM || signal_caught!=SIGKILL);

	// TODO send kill message to server thread


	pthread_join( sThread, NULL );

	doLog(LOG_INFO, "server thread exited");

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

	printf( "%s", main_messages[msg_id] );

}//[/showHelp]


/**
 * Show message
 */
void showMessage(int msg_id) {

	printf( "%s", main_messages[msg_id] );

}// showMessage

void showMessageEx(int msg_id, ...) {

	int _MAX_SIZE = 1024;
	int n;

	char *msg_format = main_messages[msg_id];
	char *msg_args;

	// if we fail for that small buffer,
	//  then the system as a much bigger problem ;-)
	msg_args = malloc( _MAX_SIZE * sizeof(char) );
	va_list ap;

	va_start(ap, msg_id);
		n = vsnprintf(msg_args, _MAX_SIZE, msg_format, ap);
	va_end(ap);

	printf( "%s", msg_args );
}// showMessageEx


/**
 * Retrieves the options and the command
 *  from the command line
 *
 */
int getOptionsAndCommand(int argc, char **argv, int *port, char **cookie, char **command) {

	//worst case
	*port = 0;
	*cookie = NULL;
	*command = NULL;

	int c;
	char _current_option = ' ';
	int  _options_scanning=0;

	opterr = 0;

	while ((c = getopt (argc, argv, "p:c:")) != -1)
	   switch (c)	{
	   case 'p':
		   if (NULL!=optarg)
			   *port = atoi( optarg );
		   break;
	   case 'c':
		   if (NULL!=optarg)
			   *cookie = optarg;
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
			showMessageEx( MSG_MISSING_OPTION_ARGUMENT, _current_option);
		} else {
			showMessageEx( MSG_INVALID_OPTION, _current_option );
		}
		return 1;
	}

	if ((optind > 0) && (optind<argc)) {
		*command = argv[optind];
	} else {
		showMessage( MSG_INVALID_COMMAND );
		return 1;
	}


	return 0; // OK
}//getOptionsAndCommand

/**
 * Crude validation of parameters
 */
int validatePortCookie(int port, char *cookie) {

	if (0 == port) {
		showHelp( MSG_PORT_INTEGER );
		return 1;
	}

	// validate (somewhat) cookie parameter
	if (NULL==cookie) {
		showHelp( MSG_COOKIE_STRING );
		return 1;
	}

	return 0;
}

/**
 *  Prints an error message
 */
void handleDaemonErrorCode(DaemonErrorCode code) {

	int msg_id = codeToMsg( code );
	showMessage( msg_id );

}// handleDaemonErrorCode

/**
 * TODO handle this mapping more elegantly...
 */
int codeToMsg(DaemonErrorCode code) {

	switch(code) {
	case DAEMON_CODE_OK: 					return MSG_OK;
	case DAEMON_CODE_NO_PID_FILE:			return MSG_NO_PID_FILE;
	case DAEMON_CODE_INVALID_NAME:  		return MSG_INVALID_NAME;
	case DAEMON_CODE_READING_PID_FILE:		return MSG_READING_PID_FILE;
	case DAEMON_CODE_INVALID_PID:			return MSG_INVALID_PID;
	case DAEMON_CODE_READING_PROC_CMDLINE:	return MSG_READING_PROC_CMDLINE;
	case DAEMON_CODE_PROC_CMDLINE_NOMATCH:	return MSG_CMDLINE_NOMATCH;
	case DAEMON_CODE_INVALID_COMMAND:		return MSG_INVALID_COMMAND;
	case DAEMON_CODE_KILL_FAILED:			return MSG_KILL_FAILED;
	case DAEMON_CODE_DAEMON_EXIST:			return MSG_DAEMON_EXIST;
	case DAEMON_CODE_WRITING_PID_FILE:		return MSG_WRITING_PID_FILE;
	case DAEMON_CODE_EXITING:				return MSG_OK; //shouldn't need to use this.
	}

	return MSG_CODE_NOT_FOUND;
}// codeToMsg
