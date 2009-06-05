/**
 * @file   msg.cc
 *
 * @date   2009-06-04
 * @author Jean-Lou Dupont
 *
 * \section messages_out Messages generated
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
 *
 * \section messages_in Messages Supported
 *
 * DOUT:
 *
 *    {ATOM(msg_type), INT(Serial), INT(index), INT(value)}
 *
 */
#include <sys/epoll.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ei.h>
#include "msg.h"
#include "event.h"
#include "logger.h"
#include "utils.h"

#define MSG_MAX_TYPE_SIZE  16


//PRIVATE

typedef struct {

	//message type
	char type[MSG_MAX_TYPE_SIZE];

	//phidget serial
	long int serial;

	//internal
	int version;
	int arity;
	int index;

	int size;
	byte *buf;

} mbuf;


//PROTOTYPES
ei_x_buff *_msg_build(int size);
const char *_msg_type_to_atom(EventType type);
ei_x_buff *_msg_build(int size, EventType type, int serial);
int _msg_send1(int fd, EventType type, Event *event);
int _msg_send2(int fd, EventType type, Event *event);

msg *_msg_factory(msg_type type);
int _msg_read_wait(msg_read_context *c);
int _msg_read_decode_header(mbuf *b);
int _msg_translate_type(char *etype);
int _msg_read_decode_variant1(mbuf *b, long int *i1, long int *i2);
int _msg_decode_body(mbuf *mb, msg **m);
void _msg_destroy_mbuf(mbuf *mb);

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


/**
 * @return >=1 FAILURE
 */
int msg_setup_read(int fd, int usec_timeout, msg_read_context **c) {

	*c = (msg_read_context *) malloc(sizeof(msg_read_context));
	if (NULL==*c) {
		DEBUG_LOG(LOG_ERR, "msg_setup_read: CANNOT malloc for msg_read_context");
		return 1;
	}

	(*c)->fd = fd;
	(*c)->usec_timeout = usec_timeout;

	(*c)->epfd = epoll_create(1);
	if ((*c)->epfd < 0) {
		DEBUG_LOG(LOG_ERR, "msg_setup_read: ERROR with epoll_create");
		free(*c);
		return 2;
	}

	int ret;

	(*c)->epv.data.fd = fd;
	(*c)->epv.events = EPOLLIN;

	ret = epoll_ctl( (*c)->epfd, EPOLL_CTL_MOD, fd, &((*c)->epv) );
	if (ret) {
		DEBUG_LOG(LOG_ERR, "msg_setup_read: ERROR during epoll_ctl");
		free(*c);
	}

	return ret;
}//

int _msg_read_wait(msg_read_context *c) {

	int nr_events = epoll_wait (c->epfd, &(c->epv), 1, c->usec_timeout);
	if (nr_events < 0) {
		DEBUG_LOG(LOG_ERR, "msg_setup_read: ERROR during epoll_wait");
		return 1;
	}

	return nr_events;
}//


/**
 * Wait for a message
 *
 * @return 1   SUCCESS
 * @return 0   no message ready
 * @return -1  ERROR
 * @return -2  MALLOC ERROR
 * @return -3  ERROR packet header
 * @return -4  ERROR unsupported message type
 */
int msg_rx_wait(msg_read_context *c, msg **m) {

	int count = _msg_read_wait( c );
	if (0==count)
		return 0;

	mbuf *mb = (mbuf *) malloc(sizeof(mbuf));
	if (NULL==mb) {
		return -2;
	}

	// something happened on the 'wire'...
	int r=read_packet(c->fd, &(mb->buf), &(mb->size));
	if (r<=0) {
		doLog(LOG_ERR, "msg_rx_wait: ERROR receiving packet");
		free(mb);
		return -1;
	}

	// we've got the packet ok... let's start decoding it
	int r2=_msg_read_decode_header( mb );
	if (r2<0) {
		doLog(LOG_ERR, "msg_rx_wait: ERROR decoding packet header");
		free(mb);
		return -3;
	}

	return _msg_decode_body(mb, m);
}//


int msg_rx(int fd, msg **m) {

	mbuf *mb = (mbuf *) malloc(sizeof(mbuf));
	if (NULL==mb) {
		return -2;
	}

	DEBUG_LOG(LOG_INFO,"msg_rx: BEFORE read_packet");

	mb->buf = (byte *) malloc(1024*sizeof(byte));
	if (NULL==mb->buf) {
		return -2;
	}
	mb->size = 1024;

	// something happened on the 'wire'...
	int r=read_packet(fd, &(mb->buf), &(mb->size));
	if (r<=0) {
		doLog(LOG_ERR, "msg_rx: ERROR receiving packet, code[%i]", r);
		_msg_destroy_mbuf( mb );
		return -1;
	}

	DEBUG_LOG(LOG_INFO,"msg_rx: BEFORE decode_header");


	// we've got the packet ok... let's start decoding it
	int r2=_msg_read_decode_header( mb );
	if (r2<0) {
		doLog(LOG_ERR, "msg_rx: ERROR decoding packet header");
		_msg_destroy_mbuf( mb );
		return -3;
	}

	return _msg_decode_body(mb, m);
}//



/**
 * Decodes a message based on type
 *
 * @return  1  SUCCESS
 * @return  0  no message ready
 * @return -1  ERROR
 * @return -2  MALLOC ERROR
 * @return -3  ERROR packet header
 * @return -4  ERROR unsupported message type
 */
int _msg_decode_body(mbuf *mb, msg **m) {

	*m = (msg *) malloc( sizeof(msg) );
	if (NULL==(*m)) {
		DEBUG_LOG(LOG_ERR, "_msg_decode_body: MALLOC ERROR");
		_msg_destroy_mbuf( mb );
		return -2;
	}

	msg_type mt = _msg_translate_type(mb->type);
	(*m)->type = mt;

	int r;
	switch(mt) {
	case MSG_DOUT:
		(*m)->body.dout.serial = mb->serial;
		r=_msg_read_decode_variant1( mb, &((*m)->body.dout.index), &((*m)->body.dout.value) );
		break;

	default:
		doLog(LOG_ERR, "unsupported message type");
		_msg_destroy_mbuf( mb );
	}//switch

	if (r<0) {
		_msg_destroy_mbuf( mb );
		msg_destroy( *m );
		return -1;
	}

	_msg_destroy_mbuf( mb );

	return 1;
}//

/**
 * Decodes a message variant
 * {... INT, INT }
 *
 * @return 0  SUCCESS
 * @return <0 FAILURE
 */
int _msg_read_decode_variant1(mbuf *b, long int *i1, long int *i2) {

	if (ei_decode_long((const char *) b->buf, &(b->index), i1)) {
		DEBUG_LOG(LOG_ERR, "_msg_read_decode_variant1: ERROR decoding INT at index 0");
		return -1;
	}

	if (ei_decode_long((const char *) b->buf, &(b->index), i2)) {
		DEBUG_LOG(LOG_ERR, "_msg_read_decode_variant1: ERROR decoding INT at index 1");
		return -2;
	}

	return 0;
}//


/**
 * @return 0  SUCCESS
 * @return <0 ERROR
 */
int _msg_read_decode_header(mbuf *b) {

	if (ei_decode_version((const char *) b->buf, &(b->index), &(b->version))) {
		DEBUG_LOG(LOG_ERR, "_msg_read_decode_header: ERROR decoding version");
		return -1;
	}

	if (ei_decode_tuple_header((const char *)b->buf, &(b->index), &(b->arity))) {
		DEBUG_LOG(LOG_ERR, "_msg_read_decode_header: ERROR decoding header/arity");
		return -2;
	}

	if (ei_decode_atom((const char *)b->buf, &(b->index), (char *) &(b->type))) {
		DEBUG_LOG(LOG_ERR, "_msg_read_decode_header: ERROR decoding message type");
		return -3;
	}

	if (ei_decode_long((const char *) b->buf, &(b->index), &(b->serial))) {
		DEBUG_LOG(LOG_ERR, "_msg_read_decode_header: ERROR decoding serial");
		return -4;
	}

	return 0;
}//


/**
 * Message Factory
 *
 * Basic for now as we have only one message
 * type to support.
 *
 */
msg *_msg_factory(msg_type type) {

	msg *m= (msg *) malloc(sizeof(msg));
	if (NULL==m) {
		doLog(LOG_ERR, "_msg_factory: MALLOC error");
		return NULL;
	}

	switch(type) {

	//not much to do more
	case MSG_DOUT:
		break;

	default:
		doLog(LOG_ERR, "_msg_factory: INVALID type");
		free(m);
		m=NULL;
		break;
	}

	return m;
}//


void msg_destroy(msg *m) {

	if (NULL==m) {
		doLog(LOG_ERR, "msg_destroy: MALLOC error");

	} else {

	// we might need more sophistication here
	// when more message types are supported
		free(m);
	}
}//


/**
 * Translates a string representing the message
 * type to an integer id.
 *
 */
int _msg_translate_type(char *etype) {

	if (NULL==etype) {
		DEBUG_LOG(LOG_ERR, "_msg_translate_type: NULL pointer");
		return MSG_INVALID;
	}

	if (0==strcmp(etype, "dout")) {
		return MSG_DOUT;
	}

	return MSG_INVALID;
}//


void _msg_destroy_mbuf(mbuf *mb) {

	free(mb->buf);
	free(mb);
}
