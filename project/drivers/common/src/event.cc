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
		case EVENT_ATTACH:
		case EVENT_DETACH:
			e->body.pd = va_arg(args, PhidgetDevice *);
			break;

		case EVENT_DIN:
		case EVENT_DOUT:
			e->body.ds.index = va_arg(args, int);
			e->body.ds.value = va_arg(args, int);
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

	default:
		DEBUG_LOG(LOG_ERR, "event_destroy: invalid type[%i]", ev->type);
		free(ev);
		break;
	}

}//
