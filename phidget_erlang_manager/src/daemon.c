/*
 * daemon.c
 *
 *  Created on: 2009-04-22
 *      Author: Jean-Lou Dupont
 */

#include "../includes/daemon.h"

#define COMMAND_INVALID -1
#define COMMAND_STOP     0
#define COMMAND_START    1


// PRIVATE PROTOTYPES
// ==================
DaemonErrorCode __daemon_verify_match(char *name, pid_t pid);
DaemonErrorCode __daemon_get_pid_from_file(char *name, pid_t *pid);
DaemonErrorCode __daemon_handle_stop(char *name);
DaemonErrorCode __daemon_handle_start(char *name);
DaemonErrorCode __daemon_write_pid_file(char *name);
int   __daemon_translate_command(char *cmd);
void  __daemon_delete_pid_file(char *name);
char *__daemon_construct_pid_filename(char *name);



// -------------------------------------------------------
//
// PUBLIC
//


/**
 *  Handles the 'start / stop' commands
 *  @param name: the daemon's name
 *  @param cmd:  the command, either 'start' or 'stop'
 *
 */
DaemonErrorCode daemon_handle_command(char *name, char *cmd) {

	int command = __daemon_translate_command(cmd);
	DaemonErrorCode command_result;

	pid_t pid = getpid();

	switch(command) {
	case COMMAND_STOP:
		command_result = __daemon_handle_stop(name, pid);
		break;
	case COMMAND_START:
		command_result = __daemon_handle_start(name, pid);
		break;
	default:
		return DAEMON_CODE_INVALID_COMMAND;
	}

	return command_result;
}// daemon_handle_command


// --------------------------------------------------------
//
// PRIVATE
//

/**
 * Handles the ``stop`` command
 *
 *  We need to determine if the daemon is running;
 *  this is done by inspecting the PID file as well as
 *  the /proc/$pid/cmdline that started the process pointed
 *  to in /var/run/$name
 *
 */
DaemonErrorCode __daemon_handle_stop(char *name) {

	DaemonErrorCode result;
	pid_t pid;

	// GET the PID from /var/run
	result =__daemon_get_pid_from_file(name, &pid);
	if (DAEMON_CODE_OK != result) {
		return result;
	}

	// COMPARE with the command-line that should
	//  have started the daemon
	result = __daemon_verify_match(name, pid);
	if (DAEMON_CODE_OK != result) {
		return result;
	}

	// SEND the kill signal...
	int kill_result = kill( pid, SIGTERM);

	if (0!=kill_result) {
		return DAEMON_CODE_KILL_FAILED;
	}

	__daemon_delete_pid_file(name);
	return DAEMON_CODE_OK;
} // STOP

/**
 * Handles the ``start`` command
 *
 *  The daemon must not already exist (!)
 */
DaemonErrorCode __daemon_handle_start(char *name) {

	DaemonErrorCode result;
	pid_t pid;

	// GET the PID from /var/run
	result =__daemon_get_pid_from_file(name, &pid);
	if (DAEMON_CODE_OK == result) {

		// we got a PID... is the daemon really running?
		result = __daemon_verify_match(name, pid);
		if (DAEMON_CODE_OK == result) {
			return DAEMON_CODE_DAEMON_EXIST;
		}

		//get rid of stale PID file
		__daemon_delete_pid_file(name);
	}

	// DEMON START
	daemon(0,0);

	result = __daemon_write_pid_file(name);


	return DAEMON_CODE_OK;
}// START

/**
 * Writes the PID file for the current daemon
 */
DaemonErrorCode __daemon_write_pid_file(char *name) {

	char *filename;
	pid_t pid = getpid();

	filename = __daemon_construct_pid_filename(name);


}// __daemon_write_pid_file


void __daemon_delete_pid_file(char *name) {
	char *filename;

	filename = __daemon_construct_pid_filename(name);

	remove(filename);
	free(filename);

}// __daemon_delete_pid_file

/**
 * Translates a string command to an integer
 *  whilst verifying the validity of the said command.
 *
 */
int __daemon_translate_command(char *cmd) {

	if (NULL==cmd) {
		return COMMAND_INVALID;
	}

	if (0==strncasecmp(cmd, "start", sizeof("start"))) {
		return COMMAND_START;
	}

	if (0==strncasecmp(cmd, "stop", sizeof("stop"))) {
		return COMMAND_START;
	}

	return COMMAND_INVALID;
}// __daemon_translate_command


/**
 * Retrieves the PID from the filesystem /var/run/$name
 *  Returns <0 on error
 */
DaemonErrorCode __daemon_get_pid_from_file(char *name, pid_t *pid) {

	char *filename;
	char read_buffer[16];
	FILE *file;

	if (name==NULL) {
		return DAEMON_CODE_INVALID_NAME;
	}

	filename = __daemon_construct_pid_filename(name);

	file = fopen( filename, "r" );

	char *result = \
		fgets(read_buffer, sizeof(buffer)*sizeof(char), file );

	fclose( file );
	free(filename);

	if (result==read_buffer) {
		return DAEMON_CODE_OK;
	}

	pid_t _pid = (pid_t) atoi( read_buffer );
	if (0==_pid) {
		return DAEMON_CODE_INVALID_PID;
	}

	*pid = _pid;

	return DAEMON_CODE_OK;
}// daemon_get_pid_from_file


/**
 * Constructs the PID filename
 *  The client of this function is responsible for
 *  freeing the memory buffer.
 */
char *__daemon_construct_pid_filename(char *name) {

	char *filename = calloc(1024*sizeof(char));

	snprintf(filename, 1024*sizeof(char), "/var/run/%s", name);

	return filename;
}//__daemon_construct_pid_filename


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
DaemonErrorCode __daemon_verify_match(char *name, pid_t pid) {

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

