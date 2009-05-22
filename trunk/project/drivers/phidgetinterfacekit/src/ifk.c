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

#include "drivers_common.h"
#include "messages.h"


// PRIVATE
pthread_t driver_thread;

/**
 * Entry Point
 */
void init(litm_bus msg, litm_bus sys) {

	driver_thread_params *params;
	params = malloc( sizeof(driver_thread_params) );

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



	DEBUG_LOG(LOG_DEBUG, "ifk: END thread");
}//thread
