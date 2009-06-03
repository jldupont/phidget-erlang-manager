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


	// Device Information structure
	// ============================
	typedef struct _PhidgetDevice {

		int version;
		int serial;
		char *type;
		char *name;
		char *label;

	} PhidgetDevice;


#endif /* TYPES_H_ */
