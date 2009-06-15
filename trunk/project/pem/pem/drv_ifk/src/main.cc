/**
 * @file main.cc
 *
 * @date   2009-06-15
 * @author Jean-Lou Dupont
 *
 *
 *
 */

#include "ifk.h"

//PROTOTYPES
bool openDevice(drvIfk *drv, int serial, CPhidgetHandle *handle);


/**
 *
 * Requires 'serial' integer for target device
 */
int main(int argc, char **argv) {

	loggerSetIdentity("drv_ifk");

	DEBUG_LOG(LOG_DEBUG,"drv_ifk: BEGIN");

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


	int result;
	drvIfk *drv = new drvIfk();

	drv->serial = serial;
	result = openDevice(drv, serial, &(drv->ph));
	if (result) {
		const char *msg_device_error = "*** error opening device, serial[%i]\n";
		doLogEx(LOG_ERR, msg_device_error, serial);
		return 1;
	}

	drv->start();

	//MAIN LOOP
	//=========
	while(true) {



	}//while

	DEBUG_LOG(LOG_DEBUG,"drv_ifk: END");
}//


bool openDevice(drvIfk *drv, int serial, CPhidgetHandle *handle) {

	CPhidgetInterfaceKitHandle IFK=0;

	int result = CPhidgetInterfaceKit_create(&IFK);
	if (EPHIDGET_OK!=result) {
		return false;
	}

	CPhidgetInterfaceKit_set_OnInputChange_Handler(IFK, IFK_InputChangeHandler, (void*)drv);
	CPhidgetInterfaceKit_set_OnOutputChange_Handler(IFK, IFK_OutputChangeHandler, (void*)drv);

	CPhidget_set_OnAttach_Handler((CPhidgetHandle)IFK, IFK_AttachHandler, (void*)drv);
	CPhidget_set_OnDetach_Handler((CPhidgetHandle)IFK, IFK_DetachHandler, (void*)drv);
	CPhidget_set_OnError_Handler((CPhidgetHandle)IFK, IFK_ErrorHandler, (void*)drv);

	result = CPhidget_open((CPhidgetHandle)IFK, serial);
	if (EPHIDGET_OK!=result) {
		return false;
	}

	*handle = (CPhidgetHandle) IFK;

	return true;
}//


int IFK_AttachHandler(CPhidgetHandle IFK, void *equeue) {

}//

int IFK_DetachHandler(CPhidgetHandle IFK, void *equeue) {


}//


int IFK_ErrorHandler(CPhidgetHandle IFK, void *drv, int ErrorCode, const char *unknown) {

	int serial;

	CPhidget_getSerialNumber(IFK, &serial);


	return 0;
}

int IFK_OutputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *drv, int Index, int Value)
{
	queue *q = (queue *) equeue;
	int serial, result;

	result = CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);


	doLog(LOG_ERR, "OUTPUT CHANGED, serial[%i] index[%i] value[%i] result[%i]", serial, Index, Value, result);
	return 0;
}

int IFK_InputChangeHandler(CPhidgetInterfaceKitHandle IFK, void *drv, int Index, int Value)
{
	queue *q = (queue *) equeue;
	int serial;
	CPhidget_getSerialNumber((CPhidgetHandle)IFK, &serial);


	doLog(LOG_ERR, "INPUT CHANGED, serial[%i] index[%i] value[%i]", serial, Index, Value);

	return 0;
}
