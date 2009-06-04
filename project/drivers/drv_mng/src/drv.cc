/**
 * @file drv.cc
 *
 * @date   2009-06-03
 * @author Jean-Lou Dupont
 *
 * Phidget Manager Driver
 *
 * This driver only sources to an Erlang process.
 * One message is defined:
 *
 * 'phidgetdevice' : device description
 *
 * This message is sent in response to:
 * - 'attach' event
 * - periodic enumeration of attached devices
 *
 * This driver uses the default 'stdout' to communicate.
 */
#include <pthread.h>
#include <unistd.h>
#include <sys/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#include <ei.h>
#include <phidget21.h>

#include "types.h"
#include "logger.h"
#include "utils.h"
#include "device.h"

#define  MESSAGE_MAX_DEVICES  32
#define  TIMEOUT              250*1000
#define  TIME_WHEEL           20

//PROTOS
void manager_destroy_device(PhidgetDevice *pd);
int manager_gotAttach(CPhidgetHandle phid, void *conn);
int manager_gotDetach(CPhidgetHandle phid, void *conn);

void __manager_send_message(int fd, PhidgetDevice *pd,  phidget_device_state state );
void __manager_clean_message_phidget_device(void *msg);

void __manager_handle_timer(int fd, CPhidgetManagerHandle phim, int count);

void pipe_action_function(int num);


const char *ident = "drv_mng";
volatile bool _terminate = false;

//MAIN
//####
int main(int argc, char **argv) {

	loggerSetIdentity(ident);

	int dout, din;

	dout=fileno(stdout);
	din=fileno(stdin);

	DEBUG_LOG(LOG_DEBUG,"drv_mng: BEGIN, stdout[%i]", dout);

	DEBUG_BEGIN
		int i;
		for (i=0; i<argc;i++)
			doLog(LOG_DEBUG, "parameter [%i] value[%s]", i, argv[i]);
	DEBUG_END

	CPhidgetManagerHandle phidm;

	// open up the Phidget Manager
	CPhidgetManager_create(&phidm);
	CPhidgetManager_set_OnAttach_Handler(phidm, manager_gotAttach, (void *)dout);
	CPhidgetManager_set_OnDetach_Handler(phidm, manager_gotDetach, (void *)dout);

	CPhidgetManager_open(phidm);

	int counter=0;

	setup_signal_action(SIGPIPE, pipe_action_function);

	struct timespec ts;

	ts.tv_sec  = 0;
	ts.tv_nsec = 250*1000*1000;

	int signal;
	sigset_t set;
	sigaddset( &set, SIGPIPE );

	while(!_terminate) {

		signal = sigtimedwait( &set, NULL, &ts);
		if (SIGPIPE==signal) {
			doLog(LOG_ERR, "drv_mng: caught SIGPIPE");
			break;
		}
		__manager_handle_timer(dout, phidm, counter++);
	}//

	DEBUG_LOG(LOG_DEBUG,"drv_mng: END");
}//

void pipe_action_function(int num) {
	_terminate = true;
}


// ==========================================================
// ==========================================================



/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *fd) {

	PhidgetDevice *pd;

	pd = create_device_info(phid);
	if (NULL==pd) {
		doLog(LOG_ERR, "ATTACH: NULL device");
		return 0;
	}

	doLog(LOG_DEBUG, "ATTACH [%x][%s]", phid, pd->type);

	__manager_send_message( (int)fd, pd, PHIDGET_DEVICE_STATUS_ACTIVE );

	destroy_device_info( pd );

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *fd) {

	PhidgetDevice *pd;

	pd = create_device_info(phid);

	doLog(LOG_INFO, "drv_mng: device detached [%x]", phid);

	__manager_send_message( (int)fd, pd, PHIDGET_DEVICE_STATUS_INACTIVE );

	destroy_device_info( pd );

	return 0;
}//[/manager_gotDetach]




void __manager_handle_timer(int fd, CPhidgetManagerHandle phim, int counter) {

	int count, result;
	CPhidgetHandle (*devices[MESSAGE_MAX_DEVICES]);

	result = CPhidgetManager_getAttachedDevices(phim, devices, &count);
	if (EPHIDGET_OK!=result) {
		doLog(LOG_ERR, "drv_mng: error getting attached devices" );
		return;
	}

	// circular
	int ccount = counter % TIME_WHEEL;
	if (ccount>=count) {
		CPhidgetManager_freeAttachedDevicesArray( *devices );
		return;
	}

	PhidgetDevice *device;
	CPhidgetHandle hdevice;

	hdevice = *devices[ccount];
	device  = create_device_info( hdevice );

	//send message
	__manager_send_message(fd, device, PHIDGET_DEVICE_STATUS_ACTIVE);

	destroy_device_info( device );
	CPhidgetManager_freeAttachedDevicesArray( *devices );
}//


/**
 * Message tuple:
 *
 * 		{ATOM(message_type), LONG(device_serial), ATOM(device_type), ATOM(device_state)}
 *
 * 		header: 4 elements
 */
void __manager_send_message(int fd, PhidgetDevice *pd,  phidget_device_state state ) {

	//DEBUG_LOG(LOG_DEBUG, "drv_mng: BEGIN send");

	if (NULL==pd) {
		doLog(LOG_ERR, "drv_mng: NULL pointer for [pd] in send");
		return;
	}

	 ei_x_buff result;
	 if (ei_x_new_with_version(&result)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT create new result buffer");
		 return;
	 }

	 if (ei_x_encode_tuple_header(&result, 4)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT create tuple header");
		 return;
	 }

	 if (ei_x_encode_atom(&result, "phidgetdevice")) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode ATOM(phidgetdevice)");
		 return;
	 }
	 int serial=pd->serial;

	 if (ei_x_encode_long(&result, serial)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode LONG(serial)");
		 return;
	 }

	 char *type=pd->type;
	 if (ei_x_encode_atom(&result, type)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode ATOM(device_type)");
		 return;
	 }

	 static const char _deviceActive[]="device_active";
	 static const char _deviceInactive[]="device_inactive";
	 char *st;

	 switch(state) {
	 case PHIDGET_DEVICE_STATUS_ACTIVE:
		 st=(char *) &_deviceActive;
		 break;
	 default:
		 st=(char *) &_deviceInactive;
		 break;
	 }

	 if (ei_x_encode_atom(&result, st)) {
		 doLog(LOG_ERR, "drv_mng: CANNOT encode ATOM(device_state)");
		 return;
	 }

	 if (write_msg(fd, &result)<0) {
		doLog(LOG_ERR, "drv_mng: ERROR writing to output, code[%i]", errno);
	 }

	 //DEBUG_LOG(LOG_DEBUG, "drv_mng: END send");
}//

