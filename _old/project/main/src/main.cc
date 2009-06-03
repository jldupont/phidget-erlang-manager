/**
 * @file main.c
 *
 * @date   2009-04-17
 * @author Jean-Lou Dupont
 *
 * @mainpage	Welcome to the Phidget Erlang Manager Documention - version $version
 *
 * \section		Dependencies
 *
 *				<b>Mandatory</b>
 *				- Phidget21 library  (available at http://www.phidgets.com/)
 *				- litm library       (available at http://litm.googlecode.com/)
 *				- pthread library    (usually available on most Linux distros)
 *
 * 				<b>To build from source:</b>
 *				- scons
 *				- python >= 2.5
 *
 *
 * \section Usage Usage
 *
 *				phidget_manager -c cookie -s server_name [start|stop]
 *
 *
 * \todo add ref counter to messages/states
 * \todo exit daemon gracefully when error hooking to LITM, opening socket etc.
 *
 */

#include <errno.h>
#include <getopt.h>
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <signal.h>
#include <sys/types.h>
#include <unistd.h>


#include "helpers.h"
#include "logger.h"
#include "main.h"
#include "manager.h"
#include "server.h"
#include "signals.h"
#include "daemon.h"
#include "stimer.h"
#include "messages.h"

#include <litm.h>

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
int getOptionsAndCommand(int argc, char **argv, char **server, char **cookie, char **command);
int validateServerCookie(char *server, char *cookie);
void handleDaemonErrorCode(DaemonErrorCode code);
int codeToMsg(DaemonErrorCode code);
void showHelp(int msg_id);
void showMessage(int msg_id);
void showMessageEx(int msg_id, ...);



// ================================================


int main(int argc, char **argv) {

	int result;
	char *server;
	char *cookie, *command;


	result = getOptionsAndCommand(argc, argv, &server, &cookie, &command);
	if (0!=result) {
		return 1;
	}

	DEBUG_MSG("DEBUG: server  [%s]\n", server);
	DEBUG_MSG("DEBUG: cookie  [%s]\n", cookie);
	DEBUG_MSG("DEBUG: command [%s]\n", command);

	result = daemon_validate_command(command);
	if (0!=result) {
		showMessage( MSG_INVALID_COMMAND );
		return 1;
	}

	// if we have a ``start`` command, we need parameters:
	if (daemon_is_start_command(command)) {
		result = validateServerCookie(server, cookie);
		if (0!=result) {
			return 1;
		}
	}


	//  DAEMON
	// *!*!*!*!
	DaemonErrorCode dcode = daemon_handle_command("phidgetmanager", command);

	doLog(LOG_INFO, "daemon_handle_command: dcode[%i]", dcode );

	// should we exit right now?
	if (DAEMON_CODE_EXITING == dcode) {
		return 1;
	}

	// the command was most probably a ``start``
	//  but there was an error...
	if (DAEMON_CODE_WRITING_PID_FILE == dcode) {
		doLog(LOG_ERR, "cannot write to pid file. Maybe you are trying to execute proper rights?");
		return 1;
	}

	if (DAEMON_CODE_OK != dcode) {
		handleDaemonErrorCode( dcode );
		return 1;
	}



	// *!*!*!*!* START SUCCESSFUL !@!@!@@!@!!@!!
	// =========================================


	// before any threads are started...
	signals_init();


	// open a litm connection for the signals thread
	litm_connection *conn;
	litm_code code;

	code = litm_connect_ex( &conn, LITM_ID_MAIN );

	// pass along the connection parameters
	if (LITM_CODE_OK != code ) {
		showMessage(MSG_LITM_CONNECTION_ERROR);
		doLog(LOG_ERR, "main: cannot connect to LITM");
		return 1;
	}


	code = litm_subscribe( conn, LITM_BUS_SYSTEM );
	if (LITM_CODE_OK != code ) {
		showMessage(MSG_LITM_SUBSCRIBE_ERROR);
		doLog(LOG_ERR, "main: cannot subscribe to LITM");
		return 1;
	}




	stimer_init();

	messages_init();

	manager_init();

	/*
	 *  COMMUNICATIONS
	 *  ==============
	 *
	 *  PhidgetManager  < -- > Erlang Cnode
	 */

	//{
	server_params params;

	params.server_name = server;
	params.cookie      = cookie;

	server_init( &params );


	doLog(LOG_INFO, "main: STARTING loop, pid[%u]", getpid() );

	litm_envelope *e;
	bus_message  *msg;
	int type;

	while(1) {

		code = litm_receive_wait_timer( conn, &e, 250*1000 );
		if (LITM_CODE_OK==code) {

			//doLog(LOG_INFO, "main: RX message");

			//we just respond to shutdown here
			msg = (bus_message *) litm_get_message( e, &type );
			if (NULL!=msg) {

				litm_release( conn, e );

				if (LITM_MESSAGE_TYPE_SHUTDOWN==type) {
					break;
				}
			}
		}

	}

	litm_wait_shutdown();

	doLog(LOG_INFO, "main: END thread");

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

	const char *msg_format = main_messages[msg_id];
	char *msg_args;

	// if we fail for that small buffer,
	//  then the system as a much bigger problem ;-)
	msg_args = (char *) malloc( _MAX_SIZE * sizeof(char) );
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
int getOptionsAndCommand(int argc, char **argv, char **server, char **cookie, char **command) {

	//worst case
	*server  = NULL;
	*cookie  = NULL;
	*command = NULL;

	int c;
	char _current_option = ' ';
	int  _options_scanning=0;

	opterr = 0;

	while ((c = getopt (argc, argv, "s:c:")) != -1)
	   switch (c)	{
	   case 's':
		   if (NULL!=optarg)
			   *server = optarg;
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
		if (('s' == _current_option) || ('c' == _current_option)) {
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
int validateServerCookie(char *server, char *cookie) {

	if (NULL == server) {
		showHelp( MSG_SERVER_STRING );
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
