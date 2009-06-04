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
#include <stdlib.h>
#include <ei.h>
#include "msg.h"
#include "event.h"
#include "logger.h"
#include "utils.h"


//PROTOTYPES
ei_x_buff *_msg_build(int size);
const char *_msg_type_to_atom(EventType type);


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
 */
int _msg_send1(int fd, EventType type, Event *event) {


}//

/**
 * Handles EVENT_DIN & EVENT_DOUT
 */
int _msg_send2(int fd, EventType type, Event *event) {


}//

/**
 *
 */
ei_x_buff *_msg_build(int size) {

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
		result = (const char *) a;
		break;

	case EVENT_DETACH:
		static const char *d = "detach";
		result = (const char *) d;
		break;

	case EVENT_DIN:
		static const char *i = "din";
		result = (const char *) i;
		break;

	case EVENT_DOUT:
		static const char *o = "dout";
		result = (const char *) o;
		break;

	case EVENT_ERROR:
		static const char *e = "error";
		result = (const char *) e;
		break;

	}

	return (const char *) result;
}//

	 if (ei_x_encode_atom(&result, "phidgetdevice")) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode ATOM(phidgetdevice)");
		 return;
	 }
	 int serial=pd->serial;

	 if (ei_x_encode_long(&result, serial)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode LONG(serial)");
		 return;
	 }

	 char *type=pd->type;
	 if (ei_x_encode_atom(&result, type)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode ATOM(device_type)");
		 return;
	 }

	 static const char _deviceActive[]="device_active";
	 static const char _deviceInactive[]="device_inactive";
	 char *st;

	 switch(state) {
	 case PHIDGET_DEVICE_STATUS_ACTIVE:
		 st=(char *) &_deviceActive;
		 break;
	 default:
		 st=(char *) &_deviceInactive;
		 break;
	 }

	 if (ei_x_encode_atom(&result, st)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode ATOM(device_state)");
		 return;
	 }

	 if (write_msg(fd, &result)<0) {
		doLog(LOG_ERR, "drv_mng: ERROR writing to output, code[%i]", errno);
	 }

	 //DEBUG_LOG(LOG_DEBUG, "drv_mng: END send");

}//
