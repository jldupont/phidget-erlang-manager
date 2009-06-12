/**
 * @file   mng.cc
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 *
 */

#include "mng.h"

drvMng::drvMng() : drvBase() {
}//

drvMng::~drvMng() {
}//

void
drvMng::init(void) {
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

	// open up the Phidget Manager
	CPhidgetManager_create(&phidm);
	CPhidgetManager_set_OnAttach_Handler(phidm, manager_gotAttach, (void *)drv);
	CPhidgetManager_set_OnDetach_Handler(phidm, manager_gotDetach, (void *)drv);

	CPhidgetManager_open(phidm);

	while(1) {

		usleep(250*1000);
	}//

	DEBUG_LOG(LOG_DEBUG,"drv_mng: END");
}//


/**
 * Attach Event handler
 */
int manager_gotAttach(CPhidgetHandle phid, void *drv) {

	drvMng   *dr = (drvMng *) drv;
	phDevice *dv = new phDevice(phid);

	dv->init();

	doLog(LOG_DEBUG, "ATTACH [%i]", dv->serial);

	return 0;
}//[/manager_gotAttach]

/**
 * Detach Event Handler
 */
int manager_gotDetach(CPhidgetHandle phid, void *drv) {

	drvMng   *dr = (drvMng *) drv;
	phDevice *dv = new phDevice(phid);

	dv->init();

	doLog(LOG_INFO, "drv_mng: device detached [%i]", dv->serial);

	return 0;
}//[/manager_gotDetach]



