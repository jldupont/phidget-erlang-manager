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
CPhidgetManagerHandle manager_create(void *MessageQueuer) {

	CPhidgetManagerHandle phidm;

	// open up the Phidget Manager
	CPhidgetManager_create(&phidm);
	CPhidgetManager_set_OnAttach_Handler(phidm, manager_gotAttach, MessageQueuer);
	CPhidgetManager_set_OnDetach_Handler(phidm, manager_gotDetach, MessageQueuer);

	doLog(LOG_DEBUG, "Opening Phidget Manager", NULL);
	CPhidgetManager_open(phidm);

	return phidm;
}//[/manager_thread]


/**
 * Pushes a message on the communication queue
 */
void manager_push_message(PhidgetManagerMessageType type, PhidgetDevice *pd, void *MessageQueuer) {

	doLog(LOG_INFO, "Pushing message", NULL);

	PhidgetManagerMessage *msg;

	msg = manager_create_message(type, pd);

	// for clarity
	void (*mq)( PhidgetManagerMessage *) = (void (*)(PhidgetManagerMessage *)) MessageQueuer;

	(*mq)( msg );

}//[/manager_push_message]


/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *MessageQueuer) {
	PhidgetDevice *pd;

	doLog(LOG_INFO, "Device attached [%u]", (void *) pd->serial);

	pd = manager_create_device(phid);
	manager_push_message(MESSAGE_ATTACH, pd, MessageQueuer);

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *MessageQueuer) {
	PhidgetDevice *pd;

	doLog(LOG_INFO, "Device detached [%d]", (void *) pd->serial);

	pd = manager_create_device(phid);
	manager_push_message(MESSAGE_DETACH, pd, MessageQueuer);

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

