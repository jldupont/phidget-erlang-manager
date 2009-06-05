/*
 * types.h
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 */

#ifndef TYPES_H_
#define TYPES_H_


	typedef int msg_type;

	/**
	 * Messages supported
	 */
	enum _msgs {
		MSG_INVALID = 0,
		MSG_DOUT,
	};



	/**
	 * DOUT message from Erlang
	 */
	typedef struct {

		long int serial;
		long int index;
		long int value;

	} msg_dout;

	typedef struct {
		msg_type type;

		union {
			msg_dout dout;
		} body;

	} msg;


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
		EVENT_READ_THREAD_ERROR,
		EVENT_MSG,		// message from Erlang side
		EVENT_ATTACH,
		EVENT_DETACH,
		EVENT_DIN,
		EVENT_DOUT,
		EVENT_ERROR,
		EVENT_STATUS,
		_EVENT_LAST

	} EventType;


	/**
	 * Event type
	 */
	typedef struct {

		EventType type;
		int serial;

		union _body {
			int           state;  // for EVENT_STATUS
			msg           *m;     // message received from Erlang
			DigitalState   ds;    // DIN / DOUT
			PhidgetDevice *pd;    // ATTACH/DETACH
			int error;
		} body;

	} Event;


#endif /* TYPES_H_ */
