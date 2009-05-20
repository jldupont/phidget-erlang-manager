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
	void manager_init(void);

#endif /* MANAGER_THREAD_H_ */
