/*
 * daemon.h
 *
 *  Created on: 2009-04-22
 *      Author: Jean-Lou Dupont
 */

#ifndef DAEMON_H_
#define DAEMON_H_

#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>



	// Error codes
	typedef enum {

		DAEMON_CODE_PROC_CMDLINE_NOMATCH = -6,
		DAEMON_CODE_READING_PROC_CMDLINE = -5,
		DAEMON_CODE_INVALID_PID          = -4,
		DAEMON_CODE_READING_PID_FILE     = -3,
		DAEMON_CODE_INVALID_NAME         = -2,
		DAEMON_CODE_NO_PID_FILE          = -1,
		// ===============
		DAEMON_CODE_OK = 0

	} DaemonErrorCode;


	// PROTOTYPES - API
	DaemonErrorCode daemon_get_pid_from_file(char *name);


#endif /* DAEMON_H_ */
