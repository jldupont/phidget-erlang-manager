/**
 * @file   mng.cc
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 *
 */

#include "mng.h"

const char *drvMng::ATOM_ACTIVE   = "active";
const char *drvMng::ATOM_INACTIVE = "inactive";

drvMng::drvMng() : drvBase() {
	//DEBUG_LOG(LOG_INFO,"drvMng::drvMng()");
	error = false;
}//

drvMng::~drvMng() {
	//DEBUG_LOG(LOG_INFO,"drvMng::~drvMng()");
}//

void
drvMng::init(void) {

	//DEBUG_LOG(LOG_INFO,"drvMng::init()");

	// Message: {phidgetdevice,{Serial, Type, state}}
	mh->registerType(MNG_MSG_PHIDGET_DEVICE, "phidgetdevice", "LSA");

}//

void
drvMng::txPhidgetDeviceMsg(phDevice *phd, int state) {

	DEBUG_LOG(LOG_INFO,"drvMng::txPhidgetDeviceMsg()");

	const char *atom;
	switch(state) {
	case MNG_STATE_ACTIVE:
		atom = ATOM_ACTIVE;
		break;
	case MNG_STATE_INACTIVE:
		atom = ATOM_INACTIVE;
		break;
	}

	int result = mh->send(MNG_MSG_PHIDGET_DEVICE, phd->serial, phd->type, atom);
	if (result) {
		doLog(LOG_ERR, "drv_mng: ERROR sending message [%s]", mh->strerror());
		error = true;
	}
}//

void
drvMng::setPhim(CPhidgetManagerHandle _phim) {

	phim = _phim;
}//

void
drvMng::handleTimer(int counter) {

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

	phDevice       *dv;
	CPhidgetHandle hdevice;

	hdevice = *devices[ccount];
	dv = new phDevice(hdevice);

	dv->init();

	//send message
	txPhidgetDeviceMsg(dv,MNG_STATE_ACTIVE);

	delete dv;

	CPhidgetManager_freeAttachedDevicesArray( *devices );

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
	drv->setPhim(phidm);

	int counter=0;
	while(1) {

		usleep(250*1000);
		if (drv->error) {
			break;
		}
		drv->handleTimer(counter++);
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

