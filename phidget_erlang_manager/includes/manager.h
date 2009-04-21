/*
 * manager_thread.h
 *
 *  Created on: 2009-04-20
 *      Author: Jean-Lou Dupont
 */

#ifndef MANAGER_THREAD_H_
#define MANAGER_THREAD_H_

#include <phidget21.h>

	// Device Information structure
	// ============================
	typedef struct _PhidgetDevice {

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
		PhidgetDevice pd;

	} PhidgetManagerMessage;


	// Message Types
	// =============
	typedef enum _PhidgetManagerMessageType {
		MESSAGE_ATTACH,
		MESSAGE_DETACH
	} PhidgetManagerMessageType;


	// PROTOTYPES
	// ==========
	CPhidgetManagerHandle manager_create(void);
	int manager_gotAttach(CPhidgetHandle phid, void *ptr);
	int manager_gotDetach(CPhidgetHandle phid, void *ptr);
	PhidgetDevice *manager_create_device(CPhidgetHandle phid);
	void manager_destroy_device(PhidgetDevice *pd);
	void manager_push_message(PhidgetManagerMessageType type, PhidgetDevice *pd);

#endif /* MANAGER_THREAD_H_ */
