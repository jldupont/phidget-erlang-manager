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


	//#define SERVER_RETRY_COUNT 20  // assuming 250ms timebase => 5secs


	typedef struct {

		char *server_name;
		char *cookie;

	} server_params;


	/**
	 * Messages destined to this server
	 */
	typedef struct {

		int type;

	} server_message;


	// PROTOTYPES
	void server_init(server_params *params);


#endif /* SERVER_H_ */
