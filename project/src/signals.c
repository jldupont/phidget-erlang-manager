/**
 * @file signals.c
 *
 * Signal handling for the whole process
 *
 *  @date   2009-04-22
 *  @author Jean-Lou Dupont
 */

#include "signals.h"
#include "helpers.h"
#include "litm.h"


// PRIVATE
pthread_t		__signal_thread;
int				__caught_signal = -1;
pthread_mutex_t __signals_mutex = PTHREAD_MUTEX_INITIALIZER;

// PRIVATE PROTOTYPES
void *__signals_handler_thread(void* arg);


/**
 * Init the module
 */
void signals_init(litm_connection *conn) {

	DEBUG_MSG("DEBUG: signals_init: BEGIN\n");

	// Confine all process signals
	//  to one thread: easier management

	/* block all signals */
	sigset_t 	signal_set;

	sigfillset( &signal_set );
	pthread_sigmask( SIG_BLOCK, &signal_set, NULL );

	/* create the signal handling thread */
	pthread_create( &__signal_thread, NULL, __signals_handler_thread, (void*) conn );

	DEBUG_MSG("DEBUG: signals_init: END\n");

}// signal_init


/**
 * Returns the ``caught`` signal
 *  or -1 if none.
 */
int signals_get_signal(void) {

	int result;

	pthread_mutex_lock( &__signals_mutex );
	//{

		result = __caught_signal;

	//}
	pthread_mutex_unlock(&__signals_mutex);

	return result;
}// signal_get_signal


// -----------------------------------------------------------------------------
//
// PRIVATE
//


/**
 * Signal Handler for the whole process
 */
void *__signals_handler_thread(void* _conn) {

	litm_connection *conn = _conn;

	// TODO subscribe to bus, retry on failure

	sigset_t signal_set;
	int sig;

	for(;;) {
			/* wait for any and all signals */
			sigfillset( &signal_set );
			sigwait( &signal_set, &sig );

			switch( sig ) {

			case SIGTERM:
				DEBUG_LOG(LOG_DEBUG, "received SIGTERM");
				pthread_mutex_lock(&__signals_mutex);
				__caught_signal = SIGTERM;
				pthread_mutex_unlock(&__signals_mutex);
				break;

			case SIGQUIT:
				DEBUG_LOG(LOG_DEBUG, "received SIGQUIT");
				pthread_mutex_lock(&__signals_mutex);
				__caught_signal = SIGQUIT;
				pthread_mutex_unlock(&__signals_mutex);
				break;

			 case SIGINT:
				DEBUG_LOG(LOG_DEBUG, "received SIGINT");
				pthread_mutex_lock(&__signals_mutex);
				__caught_signal = SIGINT;
				pthread_mutex_unlock(&__signals_mutex);
				break;

			 case SIGVTALRM:
			 case SIGALRM:
				// TODO generate timer message on litm
				DEBUG_LOG(LOG_DEBUG, "received SIGVLARM");
				pthread_mutex_lock(&__signals_mutex);
				__caught_signal = SIGVTALRM;
				pthread_mutex_unlock(&__signals_mutex);
				break;

			default:
				DEBUG_LOG(LOG_DEBUG, "received unsupported signal, sig[%i]", sig);
				pthread_mutex_lock(&__signals_mutex);
				__caught_signal = 0;
				pthread_mutex_unlock(&__signals_mutex);
				break;
			}
	}

	return (void*)0;
}//signal_handler_thread

