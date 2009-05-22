/**
 * @file manager_thread.h
 *
 * @date 2009-04-20
 * @author Jean-Lou Dupont
 */

#ifndef MANAGER_THREAD_H_
#define MANAGER_THREAD_H_

#include <phidget21.h>
#include "litm.h"

	/**
	 * Device status
	 */
	typedef enum {

		PHIDGET_DEVICE_STATUS_UNKNOWN =1,
		PHIDGET_DEVICE_STATUS_ACTIVE   = 1,
		PHIDGET_DEVICE_STATUS_INACTIVE

	} phidget_device_state;


	// Device Information structure
	// ============================
	typedef struct _PhidgetDevice {

		phidget_device_state state;
		int version;
		int serial;
		char *type;
		char *name;
		char *label;

	} PhidgetDevice;


	// Message structure
	// =================
	typedef struct _PhidgetManagerMessage {

		int type;
		PhidgetDevice *pd;

	} PhidgetManagerMessage;


	// Message Types
	// =============
	typedef enum _PhidgetManagerMessageType {
		MESSAGE_ATTACH,
		MESSAGE_DETACH
	} PhidgetManagerMessageType;


	// PROTOTYPES
	// ==========
	void manager_init(void);

#endif /* MANAGER_THREAD_H_ */
