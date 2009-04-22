/*
 * daemon.c
 *
 *  Created on: 2009-04-22
 *      Author: Jean-Lou Dupont
 */

#include "../includes/daemon.h"

// PRIVATE PROTOTYPES
// ==================
DaemonErrorCode daemon_verify_match(char *name, pid_t pid);
DaemonErrorCode daemon_get_pid_from_file(char *name);


// -------------------------------------------------------
//
// PUBLIC
//


/**
 *  Handles the 'start / stop' commands
 */
int daemon_handle_command(char *cmd) {


}// daemon_handle_command


// --------------------------------------------------------
//
// PRIVATE
//



/**
 * Retrieves the PID from the filesystem /var/run/$name
 *  Returns <0 on error
 */
DaemonErrorCode daemon_get_pid_from_file(char *name) {

	char filename[1024];
	char read_buffer[16];
	FILE *file;

	if (name==NULL) {
		return DAEMON_CODE_INVALID_NAME;
	}

	snprintf(filename, sizeof(filename)*sizeof(char), "/var/run/%s", name);

	file = fopen( filename, "r" );

	char *result = \
		fgets(read_buffer, sizeof(buffer)*sizeof(char), file );

	fclose( file );

	if (result==read_buffer) {
		return DAEMON_CODE_OK;
	}

	return DAEMON_CODE_READING_PID_FILE;
}// daemon_get_pid_from_file


/**
 * Verifies that the process of PID $pid matches
 *  the command line $name.
 *
 *  If there isn't a match, it is probably because:
 *  1) the daemon is non-existent / killed
 *  2) the command-line that started the daemon isn't what
 *     it is expected... renamed probably?
 *
 *  This function reads the command-line string found
 *  in /proc/$pid/cmdline and matches it with $name
 *
 */
DaemonErrorCode daemon_verify_match(char *name, pid_t pid) {

	char filename[1024];
	char read_buffer[1024];
	FILE *file;
	int  rc;
	regex_t * myregex = calloc(1, sizeof(regex_t));


	if (name==NULL) {
		return DAEMON_CODE_INVALID_NAME;
	}

	if (pid==0) {
		return DAEMON_CODE_INVALID_PID;
	}

	snprintf(filename, sizeof(filename)*sizeof(char), "/proc/%u/cmdline", pid);

	file = fopen( filename, "r" );

	char *result = \
		fgets(read_buffer, sizeof(buffer)*sizeof(char), file );

	fclose( file );

	if (result!=read_buffer) {
		return DAEMON_CODE_READING_PROC_CMDLINE;
	}

	// perform sub-string match test
	rc = regcomp( myregex, name, REG_EXTENDED | REG_NOSUB );
	if (0!=rc) {
		// if we have trouble compiling the regex,
		//  then the daemon name is not appropriate;
		//  anyhow, the daemon name SHOULD be simple!
		return DAEMON_CODE_INVALID_NAME;
	}

	rc = regexec( myregex, read_buffer, 0, 0, 0 );
	if (0!=rc) {
		return DAEMON_CODE_PROC_CMDLINE_NOMATCH;
	}

	return DAEMON_CODE_OK;
}// daemon_verify_match

