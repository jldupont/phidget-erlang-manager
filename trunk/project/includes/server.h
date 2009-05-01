/*
 * server.h
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#ifndef SERVER_H_
#define SERVER_H_

#ifndef _REENTRANT
#define _REENTRANT
#endif
#define PTHREADS

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

//include "erl_interface.h"
#include "ei.h"

#include "../includes/logger.h"
#include "../includes/qport.h"
#include "../includes/manager.h"  //for PhidgetManagerMessage type

	typedef struct {

		int port;
		char *cookie;
		qport_context *qpc;

	} server_params;


	// PROTOTYPES
	void *server_thread(void *params);
	int server_open_port(int port);


#endif /* SERVER_H_ */
