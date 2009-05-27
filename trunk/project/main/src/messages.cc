/**
 * @file   messages.c
 *
 * @date   2009-05-20
 * @author Jean-Lou Dupont
 */

#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <sys/time.h>

#include "messages.h"
#include "litm.h"
#include "logger.h"

/**
 * Maps message types to
 * a human readable form
 */
const char *messages_text[] = {
	"**invalid**",

	"shutdown",
	"timer",

	"phidget_device",
	"phidget_digital_states",
	"phidget_digital_set_states"

};


// PRIVATE
const char *__logFilePath = "/var/log/phidgetmanager";
pthread_t __messages_thread;

void __messages_log_message(bus_message_type type);
void __messages_log(const char *fmt, ... );
void *__messages_thread_function(void* arg);



void messages_init(void) {

	pthread_create( &__messages_thread, NULL, __messages_thread_function, NULL );

}//

void *__messages_thread_function(void* arg) {

	litm_connection *conn=NULL;
	litm_code code, code1, code2;

	doLog(LOG_INFO, "messages: BEGIN thread" );

	code = litm_connect_ex(&conn, LITM_ID_MESSAGES);
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "messages: cannot connect to LITM" );
		return NULL;
	}

	code1 = litm_subscribe( conn, LITM_BUS_MESSAGES);
	code2 = litm_subscribe( conn, LITM_BUS_SYSTEM);

	if (LITM_CODE_OK!=code1) {
		doLog(LOG_ERR, "cannot subscribe to messages bus");
		return NULL;
	}

	if (LITM_CODE_OK!=code2) {
		doLog(LOG_ERR, "cannot subscribe to system bus");
		return NULL;
	}


	litm_envelope *e;
	bus_message *msg;
	int type;


	while(1) {

		code = litm_receive_wait_timer(conn, &e, 250*1000);
		if (LITM_CODE_OK==code) {

			//doLog(LOG_INFO, "messages: RX message" );

			msg = (bus_message *) litm_get_message( e, &type );
			if (NULL!=msg) {
				bus_message_type type;

				//too many timer messages
				if (LITM_MESSAGE_TYPE_TIMER!=type)
					__messages_log_message( type );

				// must release no matter what
				litm_release(conn, e);

				if ( LITM_MESSAGE_TYPE_SHUTDOWN == type )
					break;
			}
		}


	}//while

	// TODO better shutdown procedure with LITM

	doLog(LOG_INFO, "messages: END thread" );

}//function


void __messages_log_message(bus_message_type type) {

	const char *base = "%s: message type[%s]";
	time_t t;
	struct tm tm;
	char date[50];

	time(&t);
	localtime_r(&t, &tm);

	if (!strftime(date, sizeof (date), "%c", &tm))
		strncpy(date, "?", sizeof (date));

	//doLog(LOG_DEBUG,"__messages_log_message: type[%i]", type);
	__messages_log(base, date, messages_text[type] );

}//function


void __messages_log( const char *fmt, ... ) {

	va_list va;
	FILE *file = NULL;

	file = fopen( __logFilePath, "a" );
	if (NULL!=file) {

		va_start(va, fmt);
		vfprintf(file, fmt, va);
		va_end(va);
		fprintf(file, "\n");
		fflush(file);

	} else {
		doLog( LOG_ERR, "cannot write to log file" );
 	}

	if (NULL!=file)
		fclose( file );

}//function
