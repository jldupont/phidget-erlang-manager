/*
 * server.h
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#ifndef SERVER_H_
#define SERVER_H_

	typedef struct {

		int port;
		char *cookie;

	} server_params;


	// PROTOTYPES
	void *server_thread(void *params);
	int server_open_port(int port);


#endif /* SERVER_H_ */
