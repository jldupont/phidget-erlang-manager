/**
 * @file server.c
 *
 * @date   2009-04-21
 * @author Jean-Lou Dupont
 */
#include "server.h"

/**
 * Erlang Server Thread
 *
 * Handles requests coming from an Erlang client
 *
 */
void *server_thread(void *params) {
	int listen_socket = 0;

	server_params *parameters = (server_params *) params;

	doLog(LOG_DEBUG,"Server Thread Started, port[%u]", parameters->port);

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


	//main loop
	while(1) {
		sleep(5);
	}



	return 0;
}// server_thread


/**
 * Opens the local socket port
 */
int server_open_port(int port) {

  int listen_fd;
  struct sockaddr_in addr;
  int on = 1;

  if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    return (-1);

  setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));

  memset((void*) &addr, 0, (size_t) sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0)
    return (-1);

  listen(listen_fd, 5);

  return listen_fd;
}//[/open_port]

