/**
 * @file   stimer.c
 *
 * @date   2009-05-11
 * @author Jean-Lou Dupont
 */

#include <pthread.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

#include "messages.h"
#include <litm.h>
#include "logger.h"
#include "utils.h"



void *__stimer_thread_function(void *params) {

	litm_connection *conn=NULL;
	litm_code code;

	doLog(LOG_INFO, "stimer: BEGIN thread" );

	code = litm_connect_ex(&conn, LITM_ID_STIMER);
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "messages: cannot connect to LITM" );
		return NULL;
	}

	code = litm_subscribe( conn, LITM_BUS_SYSTEM);

	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "cannot subscribe to system bus");
		return NULL;
	}

	static bus_message alarm_message;
	litm_envelope *e;
	bus_message *msg;
	int type;


	while(1) {

		code = litm_receive_wait_timer(conn, &e, 10*1000);
		if (LITM_CODE_OK==code) {

			msg = (bus_message *) litm_get_message( e, &type );
			if (NULL!=msg) {

				// must release no matter what
				litm_release(conn, e);

				if ( LITM_MESSAGE_TYPE_SHUTDOWN == type )
					break;
			}
		}

		usleep(250*1000);

		litm_send( conn, LITM_BUS_SYSTEM, &alarm_message, &void_cleaner, LITM_MESSAGE_TYPE_TIMER );
		alarm_message.message_body.mt.counter += 1;

	}//while


	doLog(LOG_INFO, "stimer: END thread" );
}//

/**
 * Initializes the interval timer for the system
 */
int stimer_init(void) {


	pthread_t __stimer_thread;
	pthread_create( &__stimer_thread, NULL, __stimer_thread_function, NULL );

	/*
	struct itimerval itimer;

	itimer.it_interval.tv_sec=1;
	itimer.it_interval.tv_usec=0;//1000*1000;
	itimer.it_value.tv_sec=1;
	itimer.it_value.tv_usec=0;//1000*1000;
	int result = setitimer(ITIMER_VIRTUAL, &itimer, NULL);

	return result;
	*/

}//
