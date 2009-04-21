/*
 * main.h
 *
 *  Created on: 2009-04-20
 *      Author: Jean-Lou Dupont
 */

#ifndef MAIN_H_
#define MAIN_H_

	// List of Messages
	// ================
	enum _msgs {
		MSG_MISSING_ARGUMENTS,
		MSG_PORT_INTEGER,
		MSG_COOKIE_STRING,
		MSG_ERROR_SERVER_THREAD,
	} msgs;



	// Messages table
	// ==============
	char *messages[] = {
			"ERROR: missing arguments\n",
			"ERROR: 'port' argument must be an integer [1;65535]\n",
			"ERROR: 'cookie' argument must be a string\n",
			"ERROR: cannot start server thread\n",
	};


	//Prototypes
	//==========
	void showHelp(int msg_id);
	void showMessage(int msg_id);


#endif /* MAIN_H_ */
