/**
 * @file drv.cc
 *
 * @date   2009-06-03
 * @author Jean-Lou Dupont
 *
 *  - Get Serial from command line argument 1
 *  - Try to OpenDevice
 *    - Success: report device parameters
 *    - Failure: send message {}, exit.
 *
 *  MAIN LOOP:
 *  - Read msg from stdin
 *    - Decode msg
 *    -
 */

#include <pthread.h>
#include <unistd.h>
#include <sys/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <ei.h>
#include <phidget21.h>

#include "types.h"
#include "logger.h"
#include "utils.h"

// PROTOTYPES
void pipe_action_function(int num);

// STATE
volatile bool _terminate = false;
const char *ident = "drv_ifk";

//MAIN
//####
int main(int argc, char **argv) {

	loggerSetIdentity(ident);

	if (2!=argc) {
		const char *msg_missing = "*** missing argument [serial] argc[%i]\n";
		fprintf(stderr, msg_missing, argc );
		doLog(LOG_ERR, msg_missing, argc);
		return 1;
	}
	int serial = atoi( argv[1] );
	if (0==serial) {
		const char *msg_invalid = "*** invalid argument [serial] [%s]\n";
		fprintf(stderr, msg_invalid, argv[1]);
		doLog(LOG_ERR, msg_invalid, argv[1]);
		return 1;
	}

	int dout, din;

	dout=fileno(stdout);
	din=fileno(stdin);

	DEBUG_LOG(LOG_DEBUG,"drv_PhidgetInterfaceKit: BEGIN, stdin[%i] stdout[%i] serial[%i]", din, dout, serial);

	int counter=0;
	int signals;

	setup_signal_action(SIGPIPE, pipe_action_function);

	while(!_terminate) {
		//read

		//write
	}//

	DEBUG_LOG(LOG_DEBUG,"drv_PhidgetInterfaceKit: END");
}//

void pipe_action_function(int num) {
	_terminate = true;
}

