/*
 * event.h
 *
 *  Created on: 2009-06-04
 *      Author: Jean-Lou Dupont
 */

#ifndef EVENT_H_
#define EVENT_H_

#include "types.h"

	Event *event_create(EventType type, ...);
	void event_destroy(Event *ev);
	const char *event_translate(EventType type);

#endif /* EVENT_H_ */
