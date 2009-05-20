/**
 * @file signals.c
 *
 * Signal handling for the whole process
 *
 *  @date   2009-04-22
 *  @author Jean-Lou Dupont
 *
 *
 * This module confines all signals from the process
 * to the handler thread: signals are then dispatched
 * to all interested subscribers through <b>litm</b>.
 *
 */

#include <stdlib.h>
#include "signals.h"
#include "helpers.h"
#include "messages.h"
#include "litm.h"
#include "utils.h"


// PRIVATE
pthread_t		__signal_thread;

// PRIVATE PROTOTYPES
void *__signals_handler_thread(void* arg);


/**
 * Init the module
 */
void signals_init(int bus_id) {

	DEBUG_MSG("DEBUG: signals_init: BEGIN\n");

	// Confine all process signals
	//  to one thread: easier management

	/* block all signals */
	sigset_t 	signal_set;

	sigfillset( &signal_set );
	pthread_sigmask( SIG_BLOCK, &signal_set, NULL );

	/* create the signal handling thread */
	pthread_create( &__signal_thread, NULL, __signals_handler_thread, NULL );

	DEBUG_MSG("DEBUG: signals_init: END\n");

}// signal_init



// -----------------------------------------------------------------------------
//
// PRIVATE
//


/**
 * Signal Handler for the whole process
 */
void *__signals_handler_thread(void* params) {

	litm_connection *conn = NULL;
	litm_code code;

	// TODO LITM connect error: is there a better way to handle this??
	code = litm_connect_ex_wait( &conn, LITM_ID_SIGNALS, 0);
	if (LITM_CODE_OK != code)
		DEBUG_LOG(LOG_ERR, "cannot connect to LITM");

	bus_message shutdown_message;

	shutdown_message.type = MESSAGE_SHUTDOWN;


	sigset_t signal_set;
	int sig, __exit=0;

	for(;;) {
		/* wait for any and all signals */
		sigfillset( &signal_set );
		sigwait( &signal_set, &sig );

		switch( sig ) {

		case SIGTERM:
			DEBUG_LOG(LOG_DEBUG, "received SIGTERM");
			// void_cleaner in utils.c
			if (NULL!=conn)
				litm_send_shutdown( conn, LITM_ID_SIGNALS, &shutdown_message, &void_cleaner );
			__exit = 1;
			break;

		case SIGQUIT:
			DEBUG_LOG(LOG_DEBUG, "received SIGQUIT");
			break;

		 case SIGINT:
			DEBUG_LOG(LOG_DEBUG, "received SIGINT");
			break;

		 case SIGVTALRM:
		 case SIGALRM:
			// TODO generate timer message on litm
			DEBUG_LOG(LOG_DEBUG, "received SIGVLARM");
			break;

		default:
			DEBUG_LOG(LOG_DEBUG, "received unsupported signal, sig[%i]", sig);
			break;
		}

		if (1==__exit) {
			break;
		}
	}

	litm_wait_shutdown();

	return (void*)0;
}//signal_handler_thread

