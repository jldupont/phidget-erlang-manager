/*
 * qport.c
 *
 *  Created on: 2009-04-23
 *      Author: Jean-Lou Dupont
 *
 *  Inter-thread communication through ``qpipe.c``
 *
 *  QPORT_CLIENT  corresponds to the direction Client side
 *  QPORT_SERVER  corresponds to the direction Server side
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
	DEBUG_LOG_NULL_PTR(qp, LOG_ERR, "qport_init [null qp]");

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
	DEBUG_LOG_NULL_PTR(qpc, LOG_ERR, "qport_send [null qpc]");
	DEBUG_LOG(LOG_DEBUG, "qport_send: BEGIN, context[%u]", qpc );

	int result;

	switch( qpc->port ) {

		// Client_to_server
	case QPORT_CLIENT:
		result = qpipe_send_to_server(qpc->pipe, msg);
		break;

		// Server to Client
	case QPORT_SERVER:
		result = qpipe_send_to_client(qpc->pipe, msg);
		break;

	default:
		DEBUG_LOG(LOG_ERR, "qport_send: invalid port");
		break;
	}

	DEBUG_LOG(LOG_DEBUG, "qport_send: END" );
	return result;
}// send

/**
 * Receives through a port
 */
void *qport_receive(qport_context *qpc) {
	DEBUG_LOG_NULL_PTR(qpc, LOG_ERR, "qport_receive [null qpc]");
	DEBUG_LOG(LOG_DEBUG, "qport_receive: BEGIN, context[%u]", qpc );

	void *result;

	switch(qpc->port) {

		// Client to Server
	case QPORT_CLIENT:
		result = qpipe_receive_from_server(qpc->pipe);
		break;

		// Server to Client
	case QPORT_SERVER:
		result = qpipe_receive_from_client(qpc->pipe);
		break;

	default:
		DEBUG_LOG(LOG_ERR, "qport_receive: invalid port");
		break;
	}

	DEBUG_LOG(LOG_DEBUG, "qport_receive: END" );
	return result;
}// receive
