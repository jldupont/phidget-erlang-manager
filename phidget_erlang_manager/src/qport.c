/*
 * qport.c
 *
 *  Created on: 2009-04-23
 *      Author: Jean-Lou Dupont
 *
 *  Inter-thread communication through ``qpipe.c``
 *
 *  QPORT_0  corresponds to the direction Client side
 *  QPORT_1  corresponds to the direction Server side
 *
 */

#include "../includes/helpers.h"
#include "../includes/qport.h"


/**
 * Initializes a context
 *
 * The qpipe must be initialized prior.
 */
qport_context *qport_init(qpipe *qp, qport port) {
	DEBUG_LOG_NULL_PTR(qp, "qport_init [null qp]");

	qport_context *tmp;

	tmp = malloc( sizeof(qport_context));
	if (NULL!=tmp) {
		tmp->pipe = qp;
		tmp->port = port;
	}

	return tmp;
}// init

/**
 * Sends through a port
 */
int qport_send(qport_context *qpc, void *msg) {
	DEBUG_LOG_NULL_PTR(qp, "qport_send [null qpc]");

	int result;

	switch( qpc->port ) {

		// Client_to_server
	case QPORT_0:
		result = qpipe_send_to_server(qpc->pipe, msg);
		break;

		// Server to Client
	case QPORT_1:
		result = qpipe_send_to_client(qpc->pipe, msg);
		break;

	default:
		DEBUG_LOG("qport_send: invalid port");
		break;
	}

	return result;
}// send

/**
 * Receives through a port
 */
void *qport_receive(qport_context *qpc) {
	DEBUG_LOG_NULL_PTR(qp, "qport_receive [null qpc]");

	void *result;

	switch(qpc->port) {

		// Client to Server
	case QPORT_0:
		result = qpipe_receive_from_server(qpc->pipe);
		break;

		// Server to Client
	case QPORT_1:
		result = qpipe_receive_from_client(qpc->pipe);
		break;

	default:
		DEBUG_LOG("qport_receive: invalid port");
		break;
	}

	return result;
}// receive
