/*
 * server.h
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#ifndef SERVER_H_
#define SERVER_H_

#include "erl_interface.h"
#include "ei.h"

#include "../includes/logger.h"
#include "../includes/queuer.h"
#include "../includes/manager.h"  //for PhidgetManagerMessage type

	typedef struct {

		int port;
		char *cookie;

	} server_params;


	// PROTOTYPES
	void *server_thread(void *params);
	int server_open_port(int port);


#endif /* SERVER_H_ */
