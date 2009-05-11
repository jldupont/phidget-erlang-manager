/**
 * @file server.h
 *
 * @date   2009-04-21
 * @author Jean-Lou Dupont
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

#include "ei.h"

#include "logger.h"
#include "manager.h"  //for PhidgetManagerMessage type

	typedef struct {

		int port;
		char *cookie;

	} server_params;


	// PROTOTYPES
	void *server_thread(void *params);
	int server_open_port(int port);


#endif /* SERVER_H_ */
