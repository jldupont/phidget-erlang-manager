/**
 * @file manager_thread.h
 *
 * @date 2009-04-20
 * @author Jean-Lou Dupont
 */

#ifndef MANAGER_THREAD_H_
#define MANAGER_THREAD_H_

#include <phidget21.h>
#include "qport.h"

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
	CPhidgetManagerHandle manager_create(qport_context *qpc);

	int manager_gotAttach(CPhidgetHandle phid, void *qpc);
	int manager_gotDetach(CPhidgetHandle phid, void *qpc);

	PhidgetDevice *manager_create_device(CPhidgetHandle phid);
	void manager_destroy_device(PhidgetDevice *pd);

	void manager_push_message(PhidgetManagerMessageType type, PhidgetDevice *pd, qport_context *qpc);
	PhidgetManagerMessage *manager_create_message(PhidgetManagerMessageType type, PhidgetDevice *pd);
	void manager_destroy_message(PhidgetManagerMessage *msg);

#endif /* MANAGER_THREAD_H_ */
