/**
 * @file   mng.cc
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 *
 */

#include "mng.h"

drvMng::drvMng() : drvBase() {
	DEBUG_LOG(LOG_INFO,"drvMng::drvMng()");
	error = false;
}//

drvMng::~drvMng() {
	DEBUG_LOG(LOG_INFO,"drvMng::~drvMng()");
}//

void
drvMng::init(void) {

	DEBUG_LOG(LOG_INFO,"drvMng::init()");

	// Message: {phidgetdevice,{Serial, State}}
	mh->registerType(MNG_MSG_PHIDGET_DEVICE, "phidgetdevice", "LL");

}//

void
drvMng::txPhidgetDeviceMsg(phDevice *phd, bool state) {

	DEBUG_LOG(LOG_INFO,"drvMng::txPhidgetDeviceMsg()");

	int result = mh->send(MNG_MSG_PHIDGET_DEVICE, phd->serial, (long int) state);
	if (result) {
		doLog(LOG_ERR, "drv_mng: ERROR sending message [%s]", mh->strerror());
		error = true;
	}
}//

//PROTOTYPES
int manager_gotAttach(CPhidgetHandle phid, void *drv);
int manager_gotDetach(CPhidgetHandle phid, void *drv);


const char *ident = "drv_mng";

int main(int argc, char **argv) {

	loggerSetIdentity(ident);

	DEBUG_LOG(LOG_DEBUG,"drv_mng: BEGIN");

	drvMng *drv = new drvMng();
	CPhidgetManagerHandle phidm;

	drv->start();

	// open up the Phidget Manager
	CPhidgetManager_create(&phidm);
	CPhidgetManager_set_OnAttach_Handler(phidm, manager_gotAttach, (void *)drv);
	CPhidgetManager_set_OnDetach_Handler(phidm, manager_gotDetach, (void *)drv);

	CPhidgetManager_open(phidm);

	while(1) {

		usleep(250*1000);
		if (drv->error) {
			break;
		}

	}//

	DEBUG_LOG(LOG_DEBUG,"drv_mng: END");
}//


/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *drv) {

	DEBUG_LOG(LOG_DEBUG,"drv_mng: manager_gotAttach");

	drvMng   *dr = (drvMng *) drv;
	phDevice *dv = new phDevice(phid);

	dv->init();
	dr->txPhidgetDeviceMsg(dv, MNG_STATE_ACTIVE);
	doLog(LOG_DEBUG, "ATTACH [%i]", dv->serial);

	delete dv;

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *drv) {

	DEBUG_LOG(LOG_DEBUG,"drv_mng: manager_gotDetach");

	drvMng   *dr = (drvMng *) drv;
	phDevice *dv = new phDevice(phid);

	dv->init();
	dr->txPhidgetDeviceMsg(dv, MNG_STATE_INACTIVE);
	doLog(LOG_INFO, "DETACH [%i]", dv->serial);

	delete dv;

	return 0;
}//[/manager_gotDetach]



