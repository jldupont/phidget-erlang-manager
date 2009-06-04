/**
 * @file drv.cc
 *
 * @date   2009-06-03
 * @author Jean-Lou Dupont
 *
 *  - Get Serial from command line argument 1
 *  - Try to OpenDevice
 *    - Success: report device parameters
 *    - Failure: send message {}, exit.
 *
 *  MAIN LOOP:
 *  - Read msg from stdin
 *    - Decode msg
 *    -
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
#include "queue.h"

// PROTOTYPES
void pipe_action_function(int num);

	//PHIDGET RELATED
void openDevice(queue *equeue, int serial);
int IFK_AttachHandler(CPhidgetHandle IFK, void *equeue);
int IFK_DetachHandler(CPhidgetHandle IFK, void *equeue);
int IFK_ErrorHandler(CPhidgetHandle IFK, void *equeue, int ErrorCode, const char *unknown);
int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *equeue, int Index, int Value);
int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *equeue, int Index, int Value);


// STATE
volatile bool _terminate = false;
const char *ident = "drv_ifk";

//MAIN
//####
int main(int argc, char **argv) {

	loggerSetIdentity(ident);

	if (2!=argc) {
		const char *msg_missing = "*** missing argument [serial] argc[%i]\n";
		doLogEx(LOG_ERR, msg_missing, argc);
		return 1;
	}
	int serial = atoi( argv[1] );
	if (0==serial) {
		const char *msg_invalid = "*** invalid argument [serial] [%s]\n";
		doLogEx(LOG_ERR, msg_invalid, argv[1]);
		return 1;
	}

	int dout, din;

	dout=fileno(stdout);
	din=fileno(stdin);

	DEBUG_LOG(LOG_DEBUG,"drv_PhidgetInterfaceKit: BEGIN, stdin[%i] stdout[%i] serial[%i]", din, dout, serial);

	queue *equeue;
	equeue = queue_create(1);
	if (NULL==equeue) {
		const char *msg_invalid = "*** cannot create event queue [%x]\n";
		doLogEx(LOG_ERR, msg_invalid, equeue);
		return 1;
	}


	int counter=0;
	int signals;

	setup_signal_action(SIGPIPE, pipe_action_function);

	while(!_terminate) {
		//read

		//write
	}//

	DEBUG_LOG(LOG_DEBUG,"drv_PhidgetInterfaceKit: END");
}//

void pipe_action_function(int num) {
	_terminate = true;
}





void openDevice(queue *equeue, int serial) {

	DEBUG_LOG(LOG_INFO,"drivers:ifk:openDevice, serial[%i]", serial);

	CPhidgetInterfaceKitHandle IFK=0;

	CPhidgetInterfaceKit_create(&IFK);

	CPhidgetInterfaceKit_set_OnInputChange_Handler(IFK, IFK_InputChangeHandler, (void*)equeue);
	CPhidgetInterfaceKit_set_OnOutputChange_Handler(IFK, IFK_OutputChangeHandler, (void*)equeue);

	CPhidget_set_OnAttach_Handler((CPhidgetHandle)IFK, IFK_AttachHandler, (void*)equeue);
	CPhidget_set_OnDetach_Handler((CPhidgetHandle)IFK, IFK_DetachHandler, (void*)equeue);
	CPhidget_set_OnError_Handler((CPhidgetHandle)IFK, IFK_ErrorHandler, (void*)equeue);

	CPhidget_open((CPhidgetHandle)IFK, serial);

}//



int IFK_AttachHandler(CPhidgetHandle IFK, void *equeue)
{
	int serial;

	CPhidget_getSerialNumber(IFK, &serial);

	DEBUG_LOG(LOG_INFO, "drivers:ifk: AttachHandler, serial[%i]", serial);
	return 0;
}

int IFK_DetachHandler(CPhidgetHandle IFK, void *equeue)
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

int IFK_ErrorHandler(CPhidgetHandle IFK, void *equeue, int ErrorCode, const char *unknown)
{
	doLog(LOG_ERR, "drivers:ifk: error[%i]", ErrorCode);
	return 0;
}

int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *equeue, int Index, int Value)
{
	int serial, result;
	result = CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);
	doLog(LOG_ERR, "drivers:ifk: output changed, serial[%i] index[%i] value[%i] result[%i]", serial, Index, Value, result);
	return 0;
}

int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *equeue, int Index, int Value)
{
	if (NULL==IFK) {
		doLog(LOG_ERR, "drivers:ifk: input changed handler: NULL");
		return 0;
	}
	int serial;
	CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);

	doLog(LOG_ERR, "drivers:ifk: input changed, serial[%i] index[%i] value[%i]", serial, Index, Value);

	IFK_SendDigitalState((driver_thread_params *)params, serial, Index, Value);
	return 0;
}
