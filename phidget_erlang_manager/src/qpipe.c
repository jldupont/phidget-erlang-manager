/*
 * qpipe.c
 *
 *  Created on: 2009-04-23
 *      Author: Jean-Lou Dupont
 *
 *
 *                 server_to_client
 *      SERVER    ------------------>    CLIENT
 *
 *                 client_to_server
 *                <------------------
 *
 */

#include "../includes/helpers.h"
#include "../includes/qpipe.h"


// PRIVATE
void qpipe_safe_free(queue *q);


// ============= API ==================

qpipe *qpipe_create(void) {

	qpipe *tmp;
	queue *tqe, *tqi;

	tmp = malloc(sizeof(qpipe));
	if (NULL!=tmp) {

		tqe = queue_create();
		tmp->server_to_client = tqe;

		tqi = queue_create();
		tmp->client_to_server = tqi;

		// if either one isn't created,
		//  better have nothing
		if ((NULL==tqe) || (NULL==tqi)) {
			queue_destroy(tqe);
			queue_destroy(tqi);
			free(tmp);
			tmp = NULL;
		}
	}

	return tmp;
}// create


/**
 * Sends a message from the Client TO Server
 *
 */
int qpipe_send_to_server(qpipe *qp, void *msg) {
	DEBUG_LOG_NULL_PTR(qp,  "qpipe:qpipe_send_to_server [null qp]");
	DEBUG_LOG_NULL_PTR(msg, "qpipe:qpipe_send_to_server [null msg]");

	return queue_put(qp->client_to_server, msg);

} // server_send


/**
 * Sends a message to the Client FROM Server
 */
int qpipe_send_to_client(qpipe *qp, void *msg) {
	DEBUG_LOG_NULL_PTR(qp,  "qpipe:qpipe_send_to_client [null qp]");
	DEBUG_LOG_NULL_PTR(msg, "qpipe:qpipe_send_to_client [null msg]");

	return queue_put(qp->server_to_client, msg);
} // server_receive


/**
 * Receives a message from the Server
 */
void *qpipe_receive_from_server(qpipe *qp) {
	DEBUG_LOG_NULL_PTR(qp,  "qpipe:client_send [null qp]");

	return queue_get(qp->client_to_server);
}// client_send


/**
 * Receives a message from the Client
 */
void *qpipe_receive_from_client(qpipe *qp) {
	DEBUG_LOG_NULL_PTR(qp, "qpipe:qpipe_receive_from_client [null qp]");

	return queue_get(qp->server_to_client);
}// client_receive




// ===================== PRIVATE =========================




void qpipe_safe_free(queue *q) {

	if (NULL!=q)
		free(q);

} // safe_free
