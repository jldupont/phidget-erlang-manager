/**
 * @file   msg.cc
 *
 * @date   2009-06-04
 * @author Jean-Lou Dupont
 *
 * ATTACH / DETACH:
 *
 *   {ATOM(msg_type), INT(Serial), INT(Version), STRING(type), STRING(name), STRING(label)}
 *
 * DIN/DOUT:
 *
 *   {ATOM(msg_type), INT(Serial), INT(index), INT(value)}
 *
 * ERROR:
 *
 *   {ATOM(msg_type), INT(code)}
 *
 */
#include <sys/epoll.h>
#include <stdlib.h>
#include <ei.h>
#include "msg.h"
#include "event.h"
#include "logger.h"
#include "utils.h"


//PROTOTYPES
ei_x_buff *_msg_build(int size);
const char *_msg_type_to_atom(EventType type);
ei_x_buff *_msg_build(int size, EventType type, int serial);
int _msg_send1(int fd, EventType type, Event *event);
int _msg_send2(int fd, EventType type, Event *event);

/**
 * Synchronous message sending
 *
 * @return -1 invalid event
 * @return -2
 */
int msg_send(int fd, Event *event) {

	if (!event_validate(event)) {
		doLog(LOG_ERR, "msg_send: invalid event");
		return -1;
	}
	int result;

	switch(event->type) {
	case EVENT_ATTACH:
	case EVENT_DETACH:
		result=_msg_send1(fd, event->type, event);
		break;

	case EVENT_DIN:
	case EVENT_DOUT:
		result=_msg_send2(fd, event->type, event);
		break;
	}

	return result;
}//

/**
 * Handles EVENT_ATTACH & EVENT_DETACH
 *
 * {ATOM(msg_type), INT(Serial), INT(Version), STRING(type), STRING(name), STRING(label)}
 *
 * @return 0 FAILURE
 * @return 1 SUCCESS
 *
 */
int _msg_send1(int fd, EventType type, Event *event) {

	int serial = event->body.pd->serial;
	ei_x_buff *r;
	r= _msg_build( 6, type, serial );
	if (NULL==r) {
		return 0;
	}

	 if (ei_x_encode_long(r, (long) event->body.pd->version)) {
		 doLog(LOG_ERR, "_msg_send1: CANNOT encode LONG(Version)");
		 ei_x_free( r );
		 return 0;
	 }

	 if (ei_x_encode_string(r, event->body.pd->type)) {
		 doLog(LOG_ERR, "_msg_send1: CANNOT encode STRING(Type)");
		 ei_x_free( r );
		 return 0;
	 }

	 if (ei_x_encode_string(r, event->body.pd->name)) {
		 doLog(LOG_ERR, "_msg_send1: CANNOT encode STRING(Name)");
		 ei_x_free( r );
		 return 0;
	 }

	 if (ei_x_encode_string(r, event->body.pd->label)) {
		 doLog(LOG_ERR, "_msg_send1: CANNOT encode STRING(Label)");
		 ei_x_free( r );
		 return 0;
	 }

	 int returnCode=1;

	 if (write_msg(fd, r)<0) {
		doLog(LOG_ERR, "_msg_send1: ERROR writing to output, code[%i]", errno);
		returnCode = 0;
	 }

	 ei_x_free( r );

	 return returnCode;
}//

/**
 * Handles EVENT_DIN & EVENT_DOUT
 *
 * {ATOM(msg_type), INT(Serial), INT(index), INT(value)}
 */
int _msg_send2(int fd, EventType type, Event *event) {

	int serial = event->serial;
	ei_x_buff *r;
	r= _msg_build( 4, type, serial );
	if (NULL==r) {
		return 0;
	}

	 if (ei_x_encode_long(r, (long) event->body.ds.index)) {
		 doLog(LOG_ERR, "_msg_send2: CANNOT encode LONG(Index)");
		 ei_x_free( r );
		 return 0;
	 }

	 if (ei_x_encode_long(r, (long) event->body.ds.value)) {
		 doLog(LOG_ERR, "_msg_send2: CANNOT encode LONG(Value)");
		 ei_x_free( r );
		 return 0;
	 }

	 int returnCode=1;

	 if (write_msg(fd, r)<0) {
		doLog(LOG_ERR, "_msg_send2: ERROR writing to output, code[%i]", errno);
		returnCode = 0;
	 }

	 ei_x_free( r );

	 return returnCode;

}//

/**
 *  {ATOM(msg_type), INT(Serial), ... }
 */
ei_x_buff *_msg_build(int size, EventType type, int serial) {

	ei_x_buff *result;

	result = (ei_x_buff *) malloc(sizeof(ei_x_buff));
	if (NULL==result)
		return NULL;

	 if (ei_x_new_with_version(result)) {
		 doLog(LOG_ERR, "_msg_build: CANNOT create new result buffer");
		 return NULL;
	 }

	 if (ei_x_encode_tuple_header(result, size)) {
		 doLog(LOG_ERR, "_msg_build: CANNOT create tuple header");
		 ei_x_free(result);
		 return NULL;
	 }

	 const char *en;
	 en = _msg_type_to_atom(type);

	 if (ei_x_encode_atom(result, en)) {
		 doLog(LOG_ERR, "_msg_build: CANNOT encode ATOM(msg_type)");
		 ei_x_free(result);
		 return NULL;
	 }
	 if (ei_x_encode_long(result, serial)) {
		 doLog(LOG_ERR, "_msg_build: CANNOT encode INT(Serial)");
		 ei_x_free(result);
		 return NULL;
	 }

	return result;
}//


/**
 * NOTE: Checks on type must have been performed
 *       prior to using this function.
 */
const char *_msg_type_to_atom(EventType type) {

	char *result;

	switch(type) {
	case EVENT_ATTACH:
		static const char *a = "attach";
		result = (char *) a;
		break;

	case EVENT_DETACH:
		static const char *d = "detach";
		result = (char *) d;
		break;

	case EVENT_DIN:
		static const char *i = "din";
		result = (char *) i;
		break;

	case EVENT_DOUT:
		static const char *o = "dout";
		result = (char *) o;
		break;

	case EVENT_ERROR:
		static const char *e = "error";
		result = (char *) e;
		break;

	}

	return (const char *) result;
}//


int msg_setup_read(int fd, int *epfd, epoll_event **epv) {

	*epfd = epoll_create(1);
	if (epfd < 0)
		return 2;

	*event = (epoll_event *) malloc(sizeof(epoll_event));
	if (NULL==event)
		return 1;

	int ret;

	*event->data.fd = fd;
	*event->events = EPOLLIN;

	ret = epoll_ctl( *epfd, EPOLL_CTL_MOD, fd, *epv );

	return ret;
}//

int msg_read_wait(int epfd, epoll_event *events, int usec_timeout) {

	int nr_events = epoll_wait (epfd, events, 1, usec_timeout);
	  if (nr_events < 0) {
			  perror ("epoll_wait");
			  free (events);
			  return 1;
	  }

}//
