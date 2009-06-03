/**
 * @file main.h
 *
 * @date 2009-04-20
 * @author: Jean-Lou Dupont
 */

#ifndef MAIN_H_
#define MAIN_H_


	// List of Messages
	// ================
	enum _msgs {
		MSG_OK,
		MSG_MISSING_ARGUMENTS,
		MSG_PORT_INTEGER,
		MSG_SERVER_STRING,
		MSG_COOKIE_STRING,
		MSG_ERROR_SERVER_THREAD,
		MSG_INVALID_OPTION,
		MSG_MISSING_OPTION_ARGUMENT,
		MSG_INVALID_COMMAND,
		MSG_CODE_NOT_FOUND,
		MSG_LITM_CONNECTION_ERROR,
		MSG_LITM_SUBSCRIBE_ERROR,

		// daemon related
		MSG_NO_PID_FILE,
		MSG_INVALID_NAME,
		MSG_READING_PID_FILE,
		MSG_INVALID_PID,
		MSG_READING_PROC_CMDLINE,
		MSG_CMDLINE_NOMATCH,
		MSG_KILL_FAILED,
		MSG_DAEMON_EXIST,
		MSG_WRITING_PID_FILE,

	} msgs;



	// Messages table
	// ==============
	const char *main_messages[] = {
			"OK",
			"ERROR: missing arguments\n",
			"ERROR: 'port' argument must be an integer [1;65535]\n",
			"ERROR: 'server' argument must be a string\n",
			"ERROR: 'cookie' argument must be a string\n",
			"ERROR: cannot start server thread\n",
			"ERROR: invalid option [%c]\n",
			"ERROR: missing argument for option [%c]\n",
			"ERROR: invalid command\n",
			"APPLICATION ERROR: error code not found\n",
			"ERROR: cannot connect to 'litm'",
			"ERROR: cannot subscribe to a litm bus",

			//daemon related
			"ERROR: PID file not found\n",
			"ERROR: invalid daemon name\n",
			"ERROR: reading PID file; is the daemon really running?\n",
			"ERROR: invalid PID\n",
			"ERROR: reading /proc/$pid/cmdline\n",
			"ERROR: cannot find daemon through 'procfs'\n",
			"ERROR: stop (kill) command failed\n",
			"ERROR: daemon is already running\n",
			"ERROR: writing PID file; permission(s) sufficient?\n",
	};




#endif /* MAIN_H_ */
