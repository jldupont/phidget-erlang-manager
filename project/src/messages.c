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

// PRIVATE
char *__logFilePath = "/var/log/phidgetmanager";
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

	code = litm_connect_ex_wait(&conn, LITM_ID_MESSAGES, 0);
	if (LITM_CODE_OK!=code) {
		DEBUG_LOG( LOG_ERR, "messages: cannot connect to LITM" );
		return NULL;
	}

	code1 = litm_subscribe_wait( conn, LITM_BUS_MESSAGES, 0);
	code2 = litm_subscribe_wait( conn, LITM_BUS_SYSTEM,   0);

	if (LITM_CODE_OK!=code1) {
		DEBUG_LOG(LOG_ERR, "cannot subscribe to messages bus");
		return NULL;
	}

	if (LITM_CODE_OK!=code2) {
		DEBUG_LOG(LOG_ERR, "cannot subscribe to system bus");
		return NULL;
	}


	litm_envelope *e;
	bus_message *msg;

	while(1) {

		code = litm_receive_wait(conn, &e);
		if (LITM_CODE_OK==code) {

			msg = litm_get_message( e );
			if (NULL!=msg) {
				bus_message_type type;

				type = msg->type;
				__messages_log_message( type );

				// must release no matter what
				litm_release(conn, e);

				if ( MESSAGE_SHUTDOWN == type )
					break;
			}
		}


	}//while

	// TODO better shutdown procedure with LITM

}//function


void __messages_log_message(bus_message_type type) {

	const char *base = "%d: message type[%i]";
	time_t t;
	struct tm tm;
	char date[50];

	time(&t);
	localtime_r(&t, &tm);

	if (!strftime(date, sizeof (date), "%c", &tm))
		strncpy(date, "?", sizeof (date));
	else
		date[0] = '\0';

	__messages_log(base, &date, &messages_text[type] );

}//function


void __messages_log( const char *fmt, ... ) {

	va_list va;
	FILE *file = NULL;

	file = fopen( __logFilePath, "a" );
	if (NULL!=file) {

		va_start(va, fmt);
		vfprintf(file, fmt, va);
		va_end(va);
		fprintf(file, "\"\n");
		fflush(file);

	} else {
 		DEBUG_LOG( LOG_ERR, "cannot write to log file" );
 	}

	if (NULL!=file)
		fclose( file );

}//function
