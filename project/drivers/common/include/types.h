/*
 * types.h
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 */

#ifndef TYPES_H_
#define TYPES_H_

	/**
	 * Device status
	 */
	typedef enum {

		PHIDGET_DEVICE_STATUS_UNKNOWN = -1,
		PHIDGET_DEVICE_STATUS_ACTIVE   = 1,
		PHIDGET_DEVICE_STATUS_INACTIVE

	} phidget_device_state;


	/*
	 * Device Information structure
	 */
	typedef struct _PhidgetDevice {

		int version;
		int serial;
		char *type;
		char *name;
		char *label;

	} PhidgetDevice;

	/**
	 * Digital State
	 */
	typedef struct {

		int index;
		int value;

	} DigitalState;

	typedef enum {

		EVENT_INVALID = 0,
		EVENT_ATTACH,
		EVENT_DETACH,
		EVENT_DIN,
		EVENT_DOUT,
		EVENT_ERROR,
		_EVENT_LAST

	} EventType;


	/**
	 * Event type
	 */
	typedef struct {

		EventType type;

		union {
			DigitalState ds;
			PhidgetDevice *pd;
			int error;
		} body;

	} Event;


#endif /* TYPES_H_ */
