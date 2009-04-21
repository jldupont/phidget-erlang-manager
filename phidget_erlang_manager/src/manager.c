/*
 * manager.c
 *
 *  Created on: 2009-04-17
 *      Author: Jean-Lou Dupont
 */
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>

#include <phidget21.h>

#include "../includes/logger.h"
#include "../includes/manager.h"



/**
 * Phidget Manager controller thread
 */
CPhidgetManagerHandle manager_create(void) {

	CPhidgetManagerHandle phidm;

	doLog(LOG_DEBUG, "Starting Manager Thread", NULL);

	// open up the Phidget Manager
	CPhidgetManager_create(&phidm);
	CPhidgetManager_set_OnAttach_Handler(phidm, manager_gotAttach, NULL);
	CPhidgetManager_set_OnDetach_Handler(phidm, manager_gotDetach, NULL);

	doLog(LOG_DEBUG, "Opening Phidget Manager", NULL);
	CPhidgetManager_open(phidm);

	return phidm;
}//[/manager_thread]


/**
 * Pushes a message on the communication queue
 */
void manager_push_message(PhidgetManagerMessageType type, PhidgetDevice *pd) {

	doLog(LOG_INFO, "Pushing message", NULL);

	//cleanup
	manager_destroy_device(pd);

}//[/manager_push_message]


/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *ptr) {
	PhidgetDevice *pd;

	doLog(LOG_INFO, "Device attached [%d]", (void *) pd->serial);

	pd = manager_create_device(phid);
	manager_push_message(MESSAGE_ATTACH, pd);

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *ptr) {
	PhidgetDevice *pd;

	doLog(LOG_INFO, "Device detached [%d]", (void *) pd->serial);

	pd = manager_create_device(phid);
	manager_push_message(MESSAGE_DETACH, pd);

	return 0;
}//[/manager_gotDetach]


/**
 * Creates a device description object
 */
PhidgetDevice *manager_create_device(CPhidgetHandle phid) {

	PhidgetDevice *pd;
	const char *type, *name, *label;

	doLog(LOG_DEBUG, "Creating Device", NULL);
	pd = malloc(sizeof(PhidgetDevice));

	CPhidget_getSerialNumber(phid, &pd->serial);
  	CPhidget_getDeviceVersion(phid, &pd->version);
	CPhidget_getDeviceType(phid, (const char **) &type);
	CPhidget_getDeviceName(phid, (const char **) &name);
	CPhidget_getDeviceLabel(phid, (const char **)&label);

	//perform copies
	size_t sz_type  = strlen( type )  +1;
	size_t sz_name  = strlen( name )  +1;
	size_t sz_label = strlen( label ) +1;

	pd->type  = malloc( sz_type  * sizeof(char) );
	pd->name  = malloc( sz_name  * sizeof(char) );
	pd->label = malloc( sz_label * sizeof(char) );

	strncpy( pd->type,  type,  sz_type  );
	strncpy( pd->name,  name,  sz_name  );
	strncpy( pd->label, label, sz_label );

	doLog(LOG_DEBUG, "Finished creating Device, type[%s]", (void *)pd->type);

	return pd;
}//[/manager_create_device]


/**
 * Destroys a ``PhidgetDevice`` structure
 */
void manager_destroy_device(PhidgetDevice *pd) {

	doLog(LOG_DEBUG, "Destroying device", NULL);

	free( pd->type );
	free( pd->name );
	free( pd->label );
	free( pd );

	doLog(LOG_DEBUG, "Finished destroying device", NULL);

}//[/manager_destroy_device]
