/*
 * qpipe.h
 *
 *  Created on: 2009-04-23
 *      Author: Jean-Lou Dupont
 *
 *  Communication Pipe based on ``queue.c``
 *
 */

#ifndef QPIPE_H_
#define QPIPE_H_

#include <sys/types.h>

#include "queue.h"

	//PUBLIC

	typedef struct {

		queue *server_to_client;
		queue *client_to_server;

	} qpipe;

	qpipe *qpipe_create(void);

	// SENDING
	int qpipe_send_to_server(qpipe *qp, void *msg);
	int qpipe_send_to_client(qpipe *qp, void *msg);

	// RECEIVING
	void *qpipe_receive_from_server(qpipe *qp);
	void *qpipe_receive_from_client(qpipe *qp);


#endif /* QPIPE_H_ */
