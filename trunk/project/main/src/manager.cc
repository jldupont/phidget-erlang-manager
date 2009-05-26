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
#include <sys/types.h>
#include <unistd.h>

#include <phidget21.h>

#include "helpers.h"
#include "logger.h"
#include "manager.h"
#include "litm.h"
#include "messages.h"
#include "drivers.h"

// PRIVATE
pthread_t manager_thread;

void *__manager_thread_function(void *params);


PhidgetDevice* manager_create_device_info(CPhidgetHandle phid);

void manager_destroy_device(PhidgetDevice *pd);
int manager_gotAttach(CPhidgetHandle phid, void *conn);
int manager_gotDetach(CPhidgetHandle phid, void *conn);

void __manager_send_message(litm_connection *conn, PhidgetDevice *pd,  phidget_device_state state );
bus_message *__manager_create_message_phidget_device(PhidgetDevice *pd, phidget_device_state state);
void __manager_clean_message_phidget_device(void *msg);

void __manager_handle_timer(litm_connection *conn, CPhidgetManagerHandle phim);


// ==========================================================
// ==========================================================

void manager_init(void) {

	pthread_create(&manager_thread, NULL, &__manager_thread_function, (void *) NULL);

}//

void *__manager_thread_function(void *params) {

	litm_connection *conn;
	litm_code code;
	CPhidgetManagerHandle phidm;

	doLog(LOG_DEBUG, "manager: BEGIN thread, pid[%u]", getpid() );

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

	litm_envelope *e;
	bus_message   *msg;
	bus_message_type type;
	int __exit = 0;

	while(!__exit) {

		code = litm_receive_nb( conn, &e );
		if (LITM_CODE_OK==code) {
			msg  = (bus_message *) litm_get_message( e );
			type = msg->type;

			if (MESSAGE_SHUTDOWN==type) {
				__exit = 1;
			}

			if (MESSAGE_TIMER==type) {
				__manager_handle_timer(conn, phidm);
			}

			litm_release( conn, e);
		}

	}//while


	doLog(LOG_DEBUG,"manager: END thread");
}//thread




/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *conn) {

	PhidgetDevice *pd;

	pd = manager_create_device_info(phid);

	doLog(LOG_DEBUG, "manager: device attached [%x]", phid);

	__manager_send_message( (litm_connection *) conn, pd, PHIDGET_DEVICE_STATUS_ACTIVE );

	char *type_name = pd->type;

	drivers_handle_type( type_name, LITM_BUS_MESSAGES, LITM_BUS_SYSTEM );

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *conn) {

	PhidgetDevice *pd;

	pd = manager_create_device_info(phid);

	doLog(LOG_INFO, "manager: device detached [%x]", phid);

	__manager_send_message( (litm_connection *) conn, pd, PHIDGET_DEVICE_STATUS_INACTIVE );

	return 0;
}//[/manager_gotDetach]






void __manager_send_message(litm_connection *conn, PhidgetDevice *pd,  phidget_device_state state ) {

	litm_code   code;
	bus_message *msg;

	msg = __manager_create_message_phidget_device( pd, state );


	code = litm_send( conn, LITM_BUS_MESSAGES, msg, __manager_clean_message_phidget_device );
	if (LITM_CODE_OK!=code)
		doLog(LOG_ERR, "manager: error sending message through LITM");
}//



/**
 * Creates from scratch a 'message_phidget_device' message
 */
bus_message *__manager_create_message_phidget_device(PhidgetDevice *pd, phidget_device_state state) {

	bus_message *msg = (bus_message *) malloc( sizeof(bus_message) );
	if (NULL==msg)
		return NULL;

	pd->state = state;
	msg->type = MESSAGE_PHIDGET_DEVICE;
	msg->message_body.mpd.count = 1;
	msg->message_body.mpd.devices[0] = pd;

	return msg;
}


void __manager_clean_message_phidget_device(void *msg) {

	if (NULL==msg) {
		doLog(LOG_ERR, "manager: clean_message_phidget_device: NULL pointer");
		return;
	}

	bus_message *m = (bus_message *)msg;

	int i,count;

	count = m->message_body.mpd.count;

	for (i=0;i<count;i++)
		manager_destroy_device(m->message_body.mpd.devices[i]);

	free( m );

}//

/**
 * Creates a device description object
 */
PhidgetDevice* manager_create_device_info(CPhidgetHandle phid) {

	PhidgetDevice *pd;
	const char *type, *name, *label;

	//DEBUG_LOG(LOG_DEBUG, "manager: creating device");

	// if malloc fails, we have a much bigger problem
	pd = (PhidgetDevice *) malloc(sizeof(PhidgetDevice));

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

	pd->type  = (char *) malloc( sz_type  * sizeof(char) );
	pd->name  = (char *) malloc( sz_name  * sizeof(char) );
	pd->label = (char *) malloc( sz_label * sizeof(char) );

	strncpy( pd->type,  type,  sz_type  );
	strncpy( pd->name,  name,  sz_name  );
	strncpy( pd->label, label, sz_label );

	DEBUG_LOG(LOG_DEBUG, "manager: created device, type[%s]", pd->type);

	return pd;
}//[/manager_create_device]


/**
 * Destroys a ``PhidgetDevice`` structure
 */
void manager_destroy_device(PhidgetDevice *pd) {

	//DEBUG_LOG(LOG_DEBUG, "manager: destroying device");

	free( pd->type );
	free( pd->name );
	free( pd->label );
	free( pd );

	//DEBUG_LOG(LOG_DEBUG, "manager: finished destroying device");

}//[/manager_destroy_device]


void __manager_handle_timer(litm_connection *conn, CPhidgetManagerHandle phim) {

	int count, result;
	CPhidgetHandle (*devices[MESSAGE_MAX_DEVICES]);

	result = CPhidgetManager_getAttachedDevices(phim, devices, &count);
	if (EPHIDGET_OK!=result) {
		doLog(LOG_ERR, "manager: error getting attached devices" );
		return;
	}

	bus_message *msg = (bus_message *) malloc( sizeof(bus_message) );
	if (NULL==msg) {
		CPhidgetManager_freeAttachedDevicesArray( *devices );
		return;
	}

	msg->type = MESSAGE_PHIDGET_DEVICE;


	int i, done = 0;
	PhidgetDevice *device;
	CPhidgetHandle hdevice;

	for (i=0; (i<count) && (i<MESSAGE_MAX_DEVICES) ; i++ ) {
		hdevice = *devices[i];
		device = manager_create_device_info( hdevice );
		msg->message_body.mpd.devices[i] = device;
		done++;
	}

	msg->message_body.mpd.count = done;

	litm_code code;
	code = litm_send( conn, LITM_BUS_MESSAGES, msg, __manager_clean_message_phidget_device );
	if (LITM_CODE_OK!=code)
		doLog(LOG_ERR, "manager: error sending message through LITM");

	CPhidgetManager_freeAttachedDevicesArray( *devices );
}//
