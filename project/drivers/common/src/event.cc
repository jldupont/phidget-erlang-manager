/**
 * @file event.cc
 *
 * @date   2009-06-04
 * @author Jean-Lou Dupont
 */
#include <stdlib.h>
#include <stdarg.h>
#include "event.h"
#include "logger.h"
#include "types.h"
#include "device.h"


/**
 * Creates an Event of a specific type
 *
 * @return event *
 */
Event *event_create(EventType type, ...) {

	// can't really have problems here...
	// but if this fails, we have anyhow
	// much bigger problems looming...
	Event *e = (Event *) malloc(sizeof(Event));
	if (NULL==e) {
		DEBUG_LOG(LOG_ERR, "event_create: CANNOT create event");
		return NULL;
	}

	va_list args;
	va_start(args, type);

	e->type = type;

		switch(type) {
		case EVENT_ERROR:
			e->body.error = va_arg(args, int);
			break;

		case EVENT_ATTACH:
		case EVENT_DETACH:
			e->body.pd = va_arg(args, PhidgetDevice *);
			break;

		case EVENT_DIN:
		case EVENT_DOUT:
			e->serial        = va_arg(args, int);
			e->body.ds.index = va_arg(args, int);
			e->body.ds.value = va_arg(args, int);
			break;

		case EVENT_READ_THREAD_ERROR:
			break;

		case EVENT_MSG:
			e->body.m = va_arg(args, msg *);
			break;

		default:
			DEBUG_LOG(LOG_ERR, "event_create: invalid type[%i]", type);
			free(e);
			return NULL;
		}

	va_end(args);

	return e;
}//

/**
 * Destroys an Event
 */
void event_destroy(Event *ev) {

	if (NULL==ev) {
		DEBUG_LOG(LOG_ERR, "event_destroy: CANNOT create event");
		return;
	}

	switch(ev->type) {
	case EVENT_ATTACH:
	case EVENT_DETACH:
		destroy_device_info( ev->body.pd );
	case EVENT_DIN:
	case EVENT_DOUT:
		free(ev);
		break;

	case EVENT_MSG:
		free(ev->body.m);
		//pass through
	case EVENT_READ_THREAD_ERROR:
		free(ev);
		break;

	default:
		DEBUG_LOG(LOG_ERR, "event_destroy: invalid type[%i]", ev->type);
		free(ev);
		break;
	}

}//

const char *event_details[] = {
	"INVALID",
	"EVENT_READ_THREAD_ERROR",
	"EVENT_MSG",
	"EVENT_ATTACH",
	"EVENT_DETACH",
	"EVENT_DIN",
	"EVENT_DOUT",
	"EVENT_ERROR"
};

const char *event_translate(EventType type) {

	if ((_EVENT_LAST<=type) || (0>=type)){
		return event_details[0];
	}

	return event_details[type];
}//

bool event_validate(Event *e) {

	if (NULL==e) {
		return false;
	}
	if ((_EVENT_LAST<=e->type) || (0>=e->type))
		return false;

	return true;
}
