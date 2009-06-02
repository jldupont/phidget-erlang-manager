/**
 * @file server.c
 *
 * @date   2009-04-21
 * @author Jean-Lou Dupont
 *
 * Connects to an Erlang server node
 *  and bridges messages to/from the LITM bus.
 *
 *
 */
#include <fcntl.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

#include <litm.h>
#include "messages.h"
#include "server.h"
#include "logger.h"
#include "helpers.h"

#define SERVER_CONNECT_TIMEOUT     25
#define SERVER_RX_TIMEOUT          10
#define SERVER_TX_TIMEOUT          25
#define SERVER_CONNECT_RETRY_COUNT 8


typedef struct {

	int state;
	int fd;
	char *server_name;
	ei_cnode node;

} node_params;



// PRIVATE
pthread_t sThread;
void *server_thread(void *params);

//int server_open_port(int port);

bool server_get_bus_message(litm_connection *conn, litm_envelope **e, int *counter);
void server_process_bus_message(litm_connection *conn, litm_envelope *e, node_params *node, int counter);

void server_send_message_to_server(node_params *node_params, bus_message *msg, int type);
void server_send_to_server_phidget_device(node_params *node_params, bus_message *msg);

void server_get_conn_message(node_params *node, server_message **msg);
void server_process_conn_message(litm_connection *conn, server_message *msg);


enum _server_states {

	SS_INVALID = 0,
	SS_WAIT_CONNECT,
	SS_CONNECTED,

};



/**
 * Initialize the server thread
 */
void server_init(server_params *params) {

	pthread_create(&sThread, NULL, &server_thread, (void *) params);

}//

/**
 * Erlang Server Thread
 *
 * Handles requests coming from an Erlang client
 *
 */
void *server_thread(void *params) {

	litm_connection *conn=NULL;
	litm_code code;

	server_params *parameters = (server_params *) params;

	doLog(LOG_DEBUG,"Server Thread Started, server[%s], pid[%u]", parameters->server_name, getpid());


	code = litm_connect_ex(&conn, LITM_ID_SERVER);
	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "server: cannot connect to LITM");
		return NULL;
	}

	code = litm_subscribe( conn, LITM_BUS_SYSTEM );
	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "server: cannot subscribe to LITM");
		return NULL;
	}

	code = litm_subscribe( conn, LITM_BUS_MESSAGES );
	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "server: cannot subscribe to LITM");
		return NULL;
	}

	node_params node_params;
	node_params.fd = 0;
	node_params.state = SS_WAIT_CONNECT;
	node_params.server_name = parameters->server_name;

	// connect to the Erlang subsystem
	int eicode;
	eicode = ei_connect_init(&(node_params.node), "phidgetmanager", parameters->cookie, 0);
	if (eicode<0) {
		doLog(LOG_ERR, "server: error initializing c-node");
		return NULL;
	}


	int counter;
	int __exit=0;
	server_message *smsg;
	litm_envelope *e;

	//main loop
	while(!__exit) {

		__exit = server_get_bus_message(conn, &e, &counter);
		server_process_bus_message(conn, e, &node_params, counter);

		server_get_conn_message(&node_params, &smsg);
		server_process_conn_message(conn, smsg);
	}


	return NULL;
}// server_thread

/**
 * @return true for exit
 */
bool server_get_bus_message(litm_connection *conn, litm_envelope **e, int *counter) {

	bus_message *msg;
	bool returnCode = false;
	litm_code code;
	int type;

	code = litm_receive_wait_timer( conn, e, 10*1000 );
	if (LITM_CODE_OK==code) {
		msg  = (bus_message *) litm_get_message( *e, &type );

		if (LITM_MESSAGE_TYPE_SHUTDOWN==type) {
			returnCode = true;
			litm_release( conn, *e);
			*e = NULL;
		}

		if (LITM_MESSAGE_TYPE_TIMER==type) {
			*counter = msg->message_body.mt.counter;
			litm_release( conn, *e);
			*e = NULL;
		}
	} else {

		// no message
		*e = NULL;
	}

	return returnCode;
}


/**
 * If a message is present:
 * - adapt it to the target client
 * - send it across to the target client
 *
 * If the connection breaks, we'll catch
 * it when we try to receive from the Erlang server.
 *
 */
void server_process_bus_message(litm_connection *conn, litm_envelope *e, node_params *node_params, int counter) {

	//DEBUG_LOG(LOG_DEBUG, "server: BEGIN server_process_bus_message, env[%x] counter[%i]", e, counter);

	bool new_count = false;
	static int current_counter = -1;
	int type;
	bus_message *msg;

	if (current_counter != counter) {
		current_counter = counter;
		new_count = true;
	}

	switch(node_params->state) {

	//try/retry connection to server
	case SS_WAIT_CONNECT:

		//no TIMER message
		if (0>current_counter)
			break;

		if (!new_count)
			break;

		if (0!=current_counter % SERVER_CONNECT_RETRY_COUNT) {
			node_params->fd = ei_connect_tmo( &(node_params->node), node_params->server_name, SERVER_CONNECT_TIMEOUT );
			if (node_params->fd<0) {
				DEBUG_LOG(LOG_DEBUG, "server: attempt to connect FAILED, counter[%i]", current_counter);
				break;
			} else {
				node_params->state = SS_CONNECTED;
				doLog(LOG_INFO, "server: CONNECTED to erlang server");
				//pass-through
			}
		}

	// verify/receive message from Erlang server
	case SS_CONNECTED:
		if (NULL!=e) {

			msg = (bus_message *)litm_get_message(e, &type);
			server_send_message_to_server(node_params, msg, type);
		}

		break;
	}//switch


	//always release
	if (NULL!=e) {
		litm_release( conn, e );
	}

	new_count = false;

	//DEBUG_LOG(LOG_DEBUG, "server: END server_process_bus_message, env[%x] counter[%i]", e, counter);
}//

/**
 * Translate a message from the bus & send to Erlang server
 */
void server_send_message_to_server(node_params *node_params, bus_message *msg, int type) {

	switch(type) {
	case MESSAGE_PHIDGET_DEVICE:
		server_send_to_server_phidget_device(node_params, msg);
		break;

	case MESSAGE_PHIDGET_DIGITAL_STATE:
		// TODO implement MESSAGE_PHIDGET_DIGITAL_STATE
		break;

	default:
		doLog(LOG_ERR, "server: unsupported message for server");
		break;
	}

}//

/**
 * Encodes the message "Phidget Device" and sends it along
 *
 * erlang tuple:  { device, serial, type }
 * where 'device' is an atom, 'serial' is an int, 'type' is an atom
 *
 */
void server_send_to_server_phidget_device(node_params *node_params, bus_message *msg) {

	int r;
	ei_x_buff x;

	int serial = (msg->message_body.mpd.device)->serial;
	char *type = (msg->message_body.mpd.device)->type;

	r=ei_x_new_with_version(&x);
	if (r) {
		doLog(LOG_ERR, "server: CANNOT create new element");
		goto cleanup;
	}

	r=ei_x_encode_atom(&x, "device");
	if (r) {
		doLog(LOG_ERR, "server: CANNOT encode atom");
		goto cleanup;
	}

	r=ei_x_encode_ulong(&x, (unsigned long) serial);
	if (r) {
		doLog(LOG_ERR, "server: CANNOT encode atom");
		goto cleanup;
	}

	r=ei_x_encode_atom(&x, type);
	if (r) {
		doLog(LOG_ERR, "server: CANNOT encode atom for 'type'");
		goto cleanup;
	}

	r=ei_reg_send_tmo(&node_params->node, node_params->fd, node_params->server_name, x.buff, x.index, SERVER_TX_TIMEOUT);
	if (r) {
		doLog(LOG_ERR, "server: CANNOT send message to Erlang server");
		goto cleanup;
	}

cleanup:
	ei_x_free(&x);

	DEBUG_LOG(LOG_DEBUG, "server: sent message to Erlang server {PhidgetDevice, %i, %s}", serial, type);
}//


/**
 * Receives a message from the Erlang server
 */
void server_get_conn_message(node_params *node_params, server_message **msg) {

	int code;
	static erlang_msg emsg;
	static ei_x_buff buf;

	// do we have a connection at all?
	if (node_params->state!=SS_CONNECTED) {
		return;
	}

	code = ei_xreceive_msg_tmo( node_params->fd, &emsg, &buf, SERVER_RX_TIMEOUT );

	//just a poll... get out
	if (ERL_TICK==code)
		return;

	//probably no messages
	if (EAGAIN==code || ETIMEDOUT==code) {
		return;
	}

	if (0>code) {
		doLog(LOG_ERR,"server: error whilst receiving from Erlang server");
		return;
	}

	//from here on, we should have a valid message
	// TODO implement message receive from Erlang server
}//



void server_process_conn_message(litm_connection *conn, server_message *msg) {

}

/**
 * Opens the local socket port
 *
 * @return -1 on error
 * @return listed file descriptor
 */
/*
int server_open_port(int port) {

  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  // not sure this is necessary since Erlang's ei interface
  // is supposed to handle all socket options for async/non-blocking
  // operation
  fcntl(listen_fd, F_SETFL, O_NONBLOCK);

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);

  listen(listen_fd, 5);

  return listen_fd;
}//[/open_port]
*/
