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
 *      - Msg "dout"
 *      - other -> error
 *
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
#include "event.h"
#include "device.h"
#include "msg.h"


// PROTOTYPES
void pipe_action_function(int num);
int genEvent(CPhidgetHandle IFK, EventType type, void *equeue);
int genIOEvent(CPhidgetInterfaceKitHandle IFK, EventType type, void *equeue, int index, int value);
int queue_event(Event *e, queue *q, const char *en);

	//PHIDGET RELATED
bool openDevice(queue *equeue, int serial);
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

	bool response = openDevice(equeue, serial);
	if (!response) {
		return 1;
	}

	int counter=0;
	int signals;

	setup_signal_action(SIGPIPE, pipe_action_function);

	int waiting;
	Event *e;
	const char *en;
	while(!_terminate) {

		//read
		waiting = queue_wait_timer(equeue, 10*1000 );
		if (!waiting) {
			e = (Event *) queue_get_nb( equeue );
			if (NULL!=e) {
				en = event_translate(e->type);
				doLog(LOG_INFO, "dequeued event, type[%s]",en);
				msg_send( dout, e );
				event_destroy( e );
			}
		}
		//write
	}//

	DEBUG_LOG(LOG_DEBUG,"drv_PhidgetInterfaceKit: END");
	return 0;
}//

void pipe_action_function(int num) {
	_terminate = true;
}



/**
 * @return true if SUCCESS
 * @return false if FAILURE
 */
bool openDevice(queue *equeue, int serial) {

	//DEBUG_LOG(LOG_INFO,"openDevice, serial[%i]", serial);

	CPhidgetInterfaceKitHandle IFK=0;

	int result;
	result = CPhidgetInterfaceKit_create(&IFK);
	if (EPHIDGET_OK!=result) {

		doLog(LOG_ERR, "CANNOT create device, serial[%i]", serial);
		return false;
	}

	CPhidgetInterfaceKit_set_OnInputChange_Handler(IFK, IFK_InputChangeHandler, (void*)equeue);
	CPhidgetInterfaceKit_set_OnOutputChange_Handler(IFK, IFK_OutputChangeHandler, (void*)equeue);

	CPhidget_set_OnAttach_Handler((CPhidgetHandle)IFK, IFK_AttachHandler, (void*)equeue);
	CPhidget_set_OnDetach_Handler((CPhidgetHandle)IFK, IFK_DetachHandler, (void*)equeue);
	CPhidget_set_OnError_Handler((CPhidgetHandle)IFK, IFK_ErrorHandler, (void*)equeue);

	result = CPhidget_open((CPhidgetHandle)IFK, serial);
	if (EPHIDGET_OK!=result) {

		doLog(LOG_ERR, "CANNOT open device, serial[%i]", serial);
		return false;
	}

	return true;
}//



int IFK_AttachHandler(CPhidgetHandle IFK, void *equeue) {

	return genEvent(IFK, EVENT_ATTACH, equeue);
}

int IFK_DetachHandler(CPhidgetHandle IFK, void *equeue) {

	return genEvent(IFK, EVENT_DETACH, equeue);

}

int genEvent(CPhidgetHandle IFK, EventType type, void *equeue) {

	const char *en;

	en = event_translate(type);

	if (NULL==equeue) {
		doLog(LOG_ERR, "invalid queue pointer, type[%s]", en);
		return 0;
	}
	queue *q = (queue *) equeue;
	int serial;

	CPhidget_getSerialNumber(IFK, &serial);

	int result;
	Event *e;
	PhidgetDevice *pd;
	pd = create_device_info( IFK );
	if (NULL!=pd) {

		e = event_create( type, pd);

		queue_event(e, (queue *)equeue, en);

	} else {
		doLog(LOG_ERR, "CANNOT create device, type[%s]", en);
		return 0;
	}

	DEBUG_LOG(LOG_INFO, "serial[%i] event[%x] type[%s]", serial, e, en);
	return 0;

}//

/**
 * @return 0 ERROR
 * @return 1 SUCCESS
 */
int queue_event(Event *e, queue *q, const char *en) {

	if (NULL==e) {
		doLog(LOG_ERR, "attempt to queue NULL event");
		return 0;
	}

	int result = queue_put((queue *)q, (void*) e);
	if (!result) {
		event_destroy( e );
		doLog(LOG_ERR, "could not queue event, type[%s]", en);
		return 0;
	}

	return 1;
}//


int IFK_ErrorHandler(CPhidgetHandle IFK, void *equeue, int ErrorCode, const char *unknown) {

	const char *en;

	en = event_translate(EVENT_ERROR);

	if (NULL==equeue) {
		doLog(LOG_ERR, "invalid queue pointer, type[%s]", en);
		return 0;
	}
	queue *q = (queue *) equeue;
	int serial;

	CPhidget_getSerialNumber(IFK, &serial);

	Event *e = event_create( EVENT_ERROR, ErrorCode );
	queue_event( e, (queue *) equeue, en);

}

int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *equeue, int Index, int Value)
{
	queue *q = (queue *) equeue;
	int serial, result;

	result = CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);

	genIOEvent(IFK, EVENT_DOUT, equeue, Index, Value);

	doLog(LOG_ERR, "OUTPUT CHANGED, serial[%i] index[%i] value[%i] result[%i]", serial, Index, Value, result);
	return 0;
}

int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *equeue, int Index, int Value)
{
	queue *q = (queue *) equeue;
	int serial;
	CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);

	genIOEvent(IFK, EVENT_DIN, equeue, Index, Value);

	doLog(LOG_ERR, "INPUT CHANGED, serial[%i] index[%i] value[%i]", serial, Index, Value);

	return 0;
}


int genIOEvent(CPhidgetInterfaceKitHandle IFK, EventType type, void *equeue, int index, int value) {

	const char *en;

	en = event_translate(type);

	if (NULL==equeue) {
		doLog(LOG_ERR, "invalid queue pointer, type[%s]", en);
		return 0;
	}
	queue *q = (queue *) equeue;
	int serial;

	CPhidget_getSerialNumber((CPhidgetHandle) IFK, &serial);

	int result;
	Event *e;
	PhidgetDevice *pd;
	pd = create_device_info( (CPhidgetHandle) IFK );
	if (NULL!=pd) {

		e = event_create( type, index, value );
		e->serial = serial;

		queue_event(e, (queue *)equeue, en);

	} else {
		doLog(LOG_ERR, "CANNOT create device, type[%s]", en);
		return 0;
	}

	DEBUG_LOG(LOG_INFO, "serial[%i] event[%x] type[%s]", serial, e, en);
	return 0;

}//
