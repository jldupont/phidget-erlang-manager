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
#include "messages.h"

using namespace std;

// PRIVATE
pthread_t driver_thread;

typedef pair<int, CPhidgetHandle> PairIFK;
typedef std::map<int, CPhidgetHandle> IFKMap;

IFKMap _activeSerials;

void main_loop(driver_thread_params *params);
void openDevice(driver_thread_params *params, int serial);
void handleOpen(driver_thread_params *params, bus_message *msg);
int isShutdown(bus_message *msg);
int isPhidgetDeviceMessage(bus_message *msg);
int handle_messages(driver_thread_params *params);

void IFK_SendDigitalState(driver_thread_params *conn, int serial, int index, int value);


int IFK_AttachHandler(CPhidgetHandle IFK, void *userptr);
int IFK_DetachHandler(CPhidgetHandle IFK, void *userptr);
int IFK_ErrorHandler(CPhidgetHandle IFK, void *userptr, int ErrorCode, const char *unknown);
int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *userptr, int Index, int Value);
int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *userptr, int Index, int Value);

/**
 * Entry Point
 */
void init(litm_bus msg, litm_bus sys) {

	DEBUG_LOG(LOG_INFO,"drivers:ifk: init() BEGIN");

	driver_thread_params *params;
	params = (driver_thread_params *) malloc( sizeof(driver_thread_params) );

	params->conn = NULL;
	params->msg = msg;
	params->sys = sys;

	pthread_create(&driver_thread, NULL, DTF_CAST &driver_thread_function, (void *) params);

	DEBUG_LOG(LOG_INFO,"drivers:ifk: init() END");
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

	//yield
	sleep(1);

	//default timeout
	code = litm_connect_ex_wait( &conn, LITM_DRIVER_IFK_ID, 0);
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "driver ifk: cannot connect to LITM");
		return NULL;
	} else {
		DEBUG_LOG(LOG_INFO, "drivers:ifk: connected to LITM");
	}

	params->conn = conn;

	code = litm_subscribe_wait( conn, bmsg, 0 );
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "driver ifk: cannot subscribe to LITM [messages]");
		return NULL;
	} else {
		DEBUG_LOG(LOG_INFO, "drivers:ifk: subscribed to LITM messages bus");
	}

	code = litm_subscribe_wait( conn, bsys, 0 );
	if (LITM_CODE_OK!=code) {
		doLog( LOG_ERR, "driver ifk: cannot subscribe to LITM [system]");
		return NULL;
	} else {
		DEBUG_LOG(LOG_INFO, "drivers:ifk: subscribed to LITM system bus");
	}

	DEBUG_LOG(LOG_INFO, "drivers:ifk: BEFORE main loop");

	// we are good to go!
	main_loop( params );


	DEBUG_LOG(LOG_DEBUG, "ifk: END thread");
}//thread




void main_loop(driver_thread_params *params) {


	int __exit = 0;
	while(!__exit) {

		__exit = handle_messages( params );
		usleep(250*1000);

	}//while

}//

/**
 *
 * Handles messages:
 *
 *  1) message_phidget_device:  for opening/closing devices
 *  2) shutdown
 *
 * @return 1 for shutdown
 */
int handle_messages(driver_thread_params *params) {

	litm_connection *conn=params->conn;
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
		handleOpen(params, msg);

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
void handleOpen(driver_thread_params *params, bus_message *msg) {

	//DEBUG_LOG(LOG_INFO,"drivers:ifk:handleOpen");

	int serial;
	int index;
	IFKMap::iterator it;

	for (index=0; index<msg->message_body.mpd.count; index++) {
		serial = msg->message_body.mpd.devices[index]->serial;
		it = _activeSerials.find(serial);

		if (it==_activeSerials.end()) {
			openDevice( params, serial );
		}
	}

}//



void openDevice(driver_thread_params *params, int serial) {

	DEBUG_LOG(LOG_INFO,"drivers:ifk:openDevice, serial[%i]", serial);

	CPhidgetInterfaceKitHandle IFK=0;

	CPhidgetInterfaceKit_create(&IFK);

	CPhidgetInterfaceKit_set_OnInputChange_Handler(IFK, IFK_InputChangeHandler, (void*)params);
	CPhidgetInterfaceKit_set_OnOutputChange_Handler(IFK, IFK_OutputChangeHandler, (void*)params);

	CPhidget_set_OnAttach_Handler((CPhidgetHandle)IFK, IFK_AttachHandler, (void*)params);
	CPhidget_set_OnDetach_Handler((CPhidgetHandle)IFK, IFK_DetachHandler, (void*)params);
	CPhidget_set_OnError_Handler((CPhidgetHandle)IFK, IFK_ErrorHandler, (void*)params);

	CPhidget_open((CPhidgetHandle)IFK, serial);


}//



int IFK_AttachHandler(CPhidgetHandle IFK, void *params)
{
	int serial;

	CPhidget_getSerialNumber(IFK, &serial);

	_activeSerials.insert( PairIFK(serial, IFK) );

	DEBUG_LOG(LOG_INFO, "drivers:ifk: AttachHandler, serial[%i]", serial);
	return 0;
}

int IFK_DetachHandler(CPhidgetHandle IFK, void *params)
{
	int serial;
	IFKMap::iterator it;

	CPhidget_getSerialNumber(IFK, &serial);

	it = _activeSerials.find( serial );
	if (it!=_activeSerials.end()) {

		_activeSerials.erase( it );
		DEBUG_LOG(LOG_INFO, "drivers:ifk: DetachHandler, serial[%i]", serial);
	} else {
		DEBUG_LOG(LOG_ERR, "drivers:ifk: DetachHandler, serial[%i] NOT FOUND", serial);
	}


	return 0;
}

int IFK_ErrorHandler(CPhidgetHandle IFK, void *params, int ErrorCode, const char *unknown)
{
	doLog(LOG_ERR, "drivers:ifk: error[%i]", ErrorCode);
	return 0;
}

int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *params, int Index, int Value)
{
	doLog(LOG_ERR, "drivers:ifk: output changed, index[%i] value[%i]", Index, Value);
	return 0;
}

int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *params, int Index, int Value)
{
	if (NULL==IFK) {
		doLog(LOG_ERR, "drivers:ifk: input changed handler: NULL");
		return 0;
	}
	int serial;
	CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);

	IFK_SendDigitalState((driver_thread_params *)params, serial, Index, Value);
	return 0;
}

void IFK_SendDigitalState(driver_thread_params *params, int serial, int index, int value) {

	litm_code code;
	bus_message *msg = (bus_message *) malloc(sizeof(bus_message));

	msg->type = MESSAGE_PHIDGET_DIGITAL_STATE;
	msg->message_body.mps.serial = serial;
	msg->message_body.mps.index = index;
	msg->message_body.mps.value = value;

	//default cleaner, default timeout
	code = litm_send_wait(params->conn, params->msg, msg, NULL, 0);

	if (LITM_CODE_OK!=code) {
		doLog(LOG_ERR, "drivers:ifk:SendDigitalState: error sending message");
	}

}//

