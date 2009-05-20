/**
 * @file manager.c
 *
 * @date   2009-04-17
 * @author Jean-Lou Dupont
 *
 *
 * \section Theory_Operation Theory of Operation
 *
 *			- A Phidget Manager instance is created with the onAttach/onDetach events registered)
 *				- For each device attach event, the corresponding device is _opened_ and the event handlers registered
 *
 *			- Phidget devices status is published on a regular basis
 *			- One _litm_ message bus connection is created and shared for all devices
 *
 * \section Device_Supported Device Supported
 *
 * 			For this release, only the *InterfaceKit* device family is supported.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include <phidget21.h>

#include "helpers.h"
#include "logger.h"
#include "manager.h"
#include "litm.h"
#include "messages.h"

// PRIVATE
pthread_t manager_thread;

void *__manager_thread_function(void *params);


int manager_gotAttach(CPhidgetHandle phid, void *conn);
int manager_gotDetach(CPhidgetHandle phid, void *conn);

PhidgetDevice *manager_create_device(CPhidgetHandle phid);
void manager_destroy_device(PhidgetDevice *pd);

void manager_push_message(PhidgetManagerMessageType type, PhidgetDevice *pd, litm_connection *conn);
PhidgetManagerMessage *manager_create_message(PhidgetManagerMessageType type, PhidgetDevice *pd);
void manager_destroy_message(PhidgetManagerMessage *msg);



// ==========================================================
// ==========================================================

void manager_init(void) {

	pthread_create(&sThread, NULL, &__manager_thread_function, (void *) NULL);

}//

void *__manager_thread_function(void *params) {

	litm_connection *conn;
	litm_code code;
	CPhidgetManagerHandle phidm;

	doLog(LOG_DEBUG, "manager: BEGIN thread");

	code = litm_connect_ex_wait(&conn, LITM_ID_MANAGER, 0);
	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "manager: cannot connect to LITM");
		return NULL;
	}

	code = litm_subscribe( conn, LITM_BUS_SYSTEM );
	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "manager: cannot subscribe to LITM");
		return NULL;
	}



	// open up the Phidget Manager
	CPhidgetManager_create(&phidm);
	CPhidgetManager_set_OnAttach_Handler(phidm, manager_gotAttach, (void *)conn);
	CPhidgetManager_set_OnDetach_Handler(phidm, manager_gotDetach, (void *)conn);



	CPhidgetManager_open(phidm);


	doLog(LOG_DEBUG,"manager: END thread");
}//thread




/**
 * Pushes a message on the communication queue
 */
void manager_push_message(PhidgetManagerMessageType type, PhidgetDevice *pd, litm_connection *conn) {

	DEBUG_LOG(LOG_INFO, "Pushing message");


}//[/manager_push_message]


/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *qpc) {
	PhidgetDevice *pd;

	doLog(LOG_DEBUG, "Device attached [%u]", pd->serial);

	pd = manager_create_device(phid);

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *qpc) {
	PhidgetDevice *pd;

	doLog(LOG_INFO, "Device detached [%d]", pd->serial);

	pd = manager_create_device(phid);

	return 0;
}//[/manager_gotDetach]


/**
 * Creates a device description object
 */
PhidgetDevice *manager_create_device(CPhidgetHandle phid) {

	PhidgetDevice *pd;
	const char *type, *name, *label;

	DEBUG_LOG(LOG_DEBUG, "Creating Device");
	pd = malloc(sizeof(PhidgetDevice));

	CPhidget_getSerialNumber(phid, &pd->serial);
  	CPhidget_getDeviceVersion(phid, &pd->version);
	CPhidget_getDeviceType(phid, (const char **) &type);
	CPhidget_getDeviceName(phid, (const char **) &name);
	CPhidget_getDeviceLabel(phid, (const char **)&label);

	//perform copies
	size_t sz_char = sizeof(char);

	size_t sz_type  = strlen( type )  + sz_char;
	size_t sz_name  = strlen( name )  + sz_char;
	size_t sz_label = strlen( label ) + sz_char;

	pd->type  = malloc( sz_type  * sizeof(char) );
	pd->name  = malloc( sz_name  * sizeof(char) );
	pd->label = malloc( sz_label * sizeof(char) );

	strncpy( pd->type,  type,  sz_type  );
	strncpy( pd->name,  name,  sz_name  );
	strncpy( pd->label, label, sz_label );

	DEBUG_LOG(LOG_DEBUG, "Finished creating Device, type[%s]", pd->type);

	return pd;
}//[/manager_create_device]


/**
 * Destroys a ``PhidgetDevice`` structure
 */
void manager_destroy_device(PhidgetDevice *pd) {

	DEBUG_LOG(LOG_DEBUG, "Destroying device");

	free( pd->type );
	free( pd->name );
	free( pd->label );
	free( pd );

	DEBUG_LOG(LOG_DEBUG, "Finished destroying device");

}//[/manager_destroy_device]



/**
 * Creates a message
 */
PhidgetManagerMessage *manager_create_message(PhidgetManagerMessageType type, PhidgetDevice *pd) {

	PhidgetManagerMessage *msg;

	msg = malloc(sizeof( PhidgetManagerMessage ));

	msg->type = type;
	msg->pd = pd;

	return msg;
}//[/manager_create_message]



/**
 * Destroys a message
 */
void manager_destroy_message(PhidgetManagerMessage *msg) {

	// not much to do except destroying the device
	manager_destroy_device( msg->pd );

	free(msg);

}//[/manager_destroy_message]

