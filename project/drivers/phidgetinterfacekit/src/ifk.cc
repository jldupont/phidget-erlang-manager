/**
 * @file   ifk.c
 *
 * @date   2009-05-21
 * @author Jean-Lou Dupont
 *
 * Phidget Manager driver for InterfaceKit phidget
 *
 * 1- Receive LITM message 'message_phidget_device'
 *    type == PhidgetInterfaceKit ?
 *
 * 2- Device already opened?  (based on serial number)
 *    no?  open device, add to list
 *    yes  continue
 *
 * 3- shutdown?
 *
 */
#include <map>
#include "drivers_common.h"
#include "drivers_types.h"
#include "messages.h"

using namespace std;


// PRIVATE
pthread_t driver_thread;

typedef pair<int, CPhidgetInterfaceKitHandle> PairIFK;

std::map<int, CPhidgetInterfaceKitHandle> _activeSerials;

void main_loop(litm_connection *conn);
int isShutdown(bus_message *msg);
int isPhidgetDeviceMessage(bus_message *msg);
int handle_messages(litm_connection *conn);
void handleOpen(bus_message *msg);
void handleClose(bus_message *msg);


int IFK_AttachHandler(CPhidgetHandle IFK, void *userptr);
int IFK_DetachHandler(CPhidgetHandle IFK, void *userptr);
int IFK_ErrorHandler(CPhidgetHandle IFK, void *userptr, int ErrorCode, const char *unknown);
int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *userptr, int Index, int Value);
int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *userptr, int Index, int Value);

/**
 * Entry Point
 */
void init(litm_bus msg, litm_bus sys) {

	driver_thread_params *params;
	params = (driver_thread_params *) malloc( sizeof(driver_thread_params) );

	params->msg = msg;
	params->sys = sys;

	pthread_create(&driver_thread, NULL, DTF_CAST &driver_thread_function, (void *) params);

}//


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

/**
 * Driver Thread function
 */
void *driver_thread_function(driver_thread_params *params) {

	litm_bus bmsg = params->msg;
	litm_bus bsys = params->sys;

	DEBUG_LOG(LOG_DEBUG, "ifk: BEGIN thread");

	litm_connection *conn=NULL;
	litm_code        code;

	//default timeout
	code = litm_connect_ex_wait( &conn, LITM_DRIVER_IFK_ID, 0);
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "driver ifk: cannot connect to LITM");
		return NULL;
	}

	code = litm_subscribe_wait( conn, bmsg, 0 );
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "driver ifk: cannot subscribe to LITM [messages]");
		return NULL;
	}

	code = litm_subscribe_wait( conn, bsys, 0 );
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "driver ifk: cannot subscribe to LITM [system]");
		return NULL;
	}

	// we are good to go!
	main_loop( conn );


	DEBUG_LOG(LOG_DEBUG, "ifk: END thread");
}//thread




void main_loop(litm_connection *conn) {


	int __exit = 0;
	while(!__exit) {

		__exit = handle_messages( conn );
		sleep(1);

	}//while

}//

/**
 *
 * Handles messages:
 *
 *  1) message_phidget_device:  for opening/closing devices
 *  2) shutdown
 *  3) timer (?)
 *
 *@return 1 for shutdown
 */
int handle_messages(litm_connection *conn) {


	litm_code code;
	litm_envelope *e=NULL;
	bus_message *msg;

	code = litm_receive_nb(conn, &e);
	if (LITM_CODE_OK==code) {

		msg = (bus_message *) litm_get_message( e );

		if (isShutdown(msg)) {
			litm_release( conn, e );
			return 1;
		}

		if (!isPhidgetDeviceMessage(msg)) {
			litm_release( conn, e );
			return 0;
		}
		//instead of having another level
		// of 'if's here....
		handleOpen(msg);
		handleClose(msg);

		litm_release( conn, e );
	}

	return 0;
}//

int isShutdown(bus_message *msg) {

	return (MESSAGE_SHUTDOWN == msg->type);

}

int isPhidgetDeviceMessage(bus_message *msg) {

	return (MESSAGE_PHIDGET_DEVICE == msg->type );
}//

/**
 * Verifies if the device(s) is/are already opened
 *  - if not opened, open the device & configure callbacks
 *	- already opened, bail out
 *
 */
void handleOpen(bus_message *msg) {

}//

void handleClose(bus_message *msg) {

}//


void openDevice(int serial) {

	CPhidgetInterfaceKitHandle IFK=0;

	CPhidgetInterfaceKit_create(&IFK);

	CPhidgetInterfaceKit_set_OnInputChange_Handler(IFK, IFK_InputChangeHandler, NULL);
	CPhidgetInterfaceKit_set_OnOutputChange_Handler(IFK, IFK_OutputChangeHandler, NULL);
	CPhidget_set_OnAttach_Handler((CPhidgetHandle)IFK, IFK_AttachHandler, NULL);
	CPhidget_set_OnDetach_Handler((CPhidgetHandle)IFK, IFK_DetachHandler, NULL);
	CPhidget_set_OnError_Handler((CPhidgetHandle)IFK, IFK_ErrorHandler, NULL);

	CPhidget_open((CPhidgetHandle)IFK, serial);

	_activeSerials.insert( PairIFK(serial, IFK) );

}//

void closeDevices(void) {

}//


int IFK_AttachHandler(CPhidgetHandle IFK, void *userptr)
{
	return 0;
}

int IFK_DetachHandler(CPhidgetHandle IFK, void *userptr)
{
	return 0;
}

int IFK_ErrorHandler(CPhidgetHandle IFK, void *userptr, int ErrorCode, const char *unknown)
{
	return 0;
}

int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *userptr, int Index, int Value)
{
	return 0;
}

int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *userptr, int Index, int Value)
{
	return 0;
}



