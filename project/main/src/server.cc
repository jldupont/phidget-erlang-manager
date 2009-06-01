/**
 * @file server.c
 *
 * @date   2009-04-21
 * @author Jean-Lou Dupont
 *
 * States:
 *  - Waiting to  Publish
 *  - Waiting to  Accept
 *  - Waiting to  Receive
 *
 */
#include <fcntl.h>
#include <pthread.h>
#include <sys/types.h>
#include <unistd.h>

#include <litm.h>
#include "messages.h"
#include "server.h"


// PRIVATE
pthread_t sThread;
void *server_thread(void *params);
int server_open_port(int port);

bool server_get_bus_message(litm_connection *conn, bus_message **msg, int *counter);
void server_process_bus_message(ei_cnode *node, bus_message *msg, int counter);

void server_get_conn_message(ei_cnode *node, server_message **msg);
void server_process_conn_message(litm_connection *conn, server_message *msg);

enum _server_states {

	SS_INVALID = 0,
	SS_PUBLISH,
	SS_ACCEPT,
	SS_RX,
};


/**
 * Initialize the server thread
 */
void server_init(server_params *params) {

	pthread_create(&sThread, NULL, &server_thread, (void *) &params);

}//

/**
 * Erlang Server Thread
 *
 * Handles requests coming from an Erlang client
 *
 */
void *server_thread(void *params) {
	int listen_socket = 0;
	litm_connection *conn=NULL;
	litm_code code;

	server_params *parameters = (server_params *) params;

	doLog(LOG_DEBUG,"Server Thread Started, port[%u], pid[%u]", parameters->port, getpid());


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

	// open server socket
	listen_socket = server_open_port( parameters->port );
	if (listen_socket <= 0) {
		doLog(LOG_ERR, "cannot open server socket");
		return NULL;
	}

	// connect to the Erlang subsystem
	ei_cnode node;

	if (ei_connect_init(&node, "phidget_manager", parameters->cookie, 0) < 0) {
		doLog(LOG_ERR, "cannot connect to the Erlang subsystem");
		return NULL;
	}

	// publish our server port through Erlang EPMD
	if (ei_publish(&node, parameters->port) == -1) {
		doLog(LOG_ERR, "cannot publish server port with Erlang EPMD");
		return NULL;
	}

	//if ((fd = erl_accept(listen, &conn)) == ERL_ERROR) {
	//	doLog(LOG_ERR, "cannot publish server port with Erlang EPMD");
	//	return 1;
	//}


	int counter;
	int __exit=0;
	bus_message    *bmsg;
	server_message *smsg;

	//main loop
	while(!__exit) {

		__exit = server_get_bus_message(conn, &bmsg, &counter);
		server_process_bus_message(node, bmsg);

		server_get_conn_message(node, &smsg);
		server_process_conn_message(conn, smsg);
	}



	return 0;
}// server_thread

/**
 * @return true for exit
 */
bool server_get_bus_message(litm_connection *conn, bus_message **msg, int *counter) {

	bool returnCode = false;
	litm_code code;
	int type;

	*counter = -1;

	code = litm_receive_wait_timer( conn, &e, 10*1000 );
	if (LITM_CODE_OK==code) {
		*msg  = (bus_message *) litm_get_message( e, &type );

		if (LITM_MESSAGE_TYPE_SHUTDOWN==type) {
			returnCode = true;
			litm_release( conn, e);
			*msg = NULL;
		}

		if (LITM_MESSAGE_TYPE_TIMER==type) {
			*counter = msg->message_body.mt.counter;
			litm_release( conn, e);
			*msg = NULL;
		}
	}

	return returnCode;
}


/**
 * If a message is present, send it across to the Erlang client
 */
void server_process_bus_message(ei_cnode *node, bus_message *msg, int counter) {

	static int server_state = SS_PUBLISH;




	litm_release( conn, e);
}

void server_get_conn_message(ei_cnode *node, server_message **msg) {

}

void server_process_conn_message(litm_connection *conn, server_message *msg) {

}

/**
 * Opens the local socket port
 *
 * @return -1 on error
 * @return listed file descriptor
 */
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

