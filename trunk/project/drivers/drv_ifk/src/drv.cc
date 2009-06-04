/**
 * @file drv.cc
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 *
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


//MAIN
//####
int main(int argc, char **argv) {

	if (1!=argc) {
		perror("missing argument [serial]\n");
		return 0;
	}

	int dout, din;

	dout=fileno(stdout);
	din=fileno(stdin);

	DEBUG_LOG(LOG_DEBUG,"drv_PhidgetInterfaceKit: BEGIN, stdin[%i] stdout[%i]", din, dout);

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

