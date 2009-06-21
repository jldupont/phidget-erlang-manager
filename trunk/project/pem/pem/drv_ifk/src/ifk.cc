/**
 * @file   ifk.cc
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 *
 */
#include "ifk.h"

const char *drvIfk::ATOM_ACTIVE   = "active";
const char *drvIfk::ATOM_INACTIVE = "inactive";

void
drvIfk::handleMsg(void) {

	Msg *m;
	int ret = waitMsg(&m, 250*1000);

	switch(ret) {
	case 0:
		switch(m->getType()) {

		case MSG_DOUTPUT_SET:
			int serial, index, value;
			int r1,r2, r3;
			char f1, f2, f3;

			r1 = m->getParam(0, &f1, &serial);
			r2 = m->getParam(1, &f2, &index);
			r3 = m->getParam(2, &f3, &value);

			if (r1 || r2 || r3) {
				doLog(LOG_ERR, "drv_ifk: ERROR retrieving msg [DOUTPUT_SET] parameters");
				break;
			}

			setDigitalOut(index, value);
			break;

		default:
			doLog(LOG_ERR, "drv_ifk: ERROR, unhandled message, type[%i]", m->getType());
		}//switch

		delete m;
		break;

	// no message
	case -1:
		break;

	default:
	case 1:
		error = true;
		break;


	}
}//


void
drvIfk::setDigitalOut(int index, int value) {

	int result = CPhidgetInterfaceKit_setOutputState((CPhidgetInterfaceKitHandle)ph, index, value);
	if (EPHIDGET_OK!=result) {
		doLog(LOG_ERR, "drv_ifk: ERROR setting digital output, serial[%i] index[%i] value[%i]", serial, index, value);
	}
}//

drvIfk::drvIfk() : drvBase() {

	error=false;
}//

drvIfk::~drvIfk() {

}//

void
drvIfk::init(void) {

	// {phidgeterror,{Serial, Code, String}}
	mh->registerType(MSG_PHIDGETERROR, "phidgeterror", "LLS");

	// {phidgetifk,{Serial, state}}
	//mh->registerType(MSG_PHIDGETIFK, "phidgetifk", "LA");

	// {device,{Serial,Version,Type,Name,Label}}
	mh->registerType(MSG_DEVICE, "device", "LLSSS");

	// {din,{Serial, Index, Value}}
	mh->registerType(MSG_DINPUT_CHANGED, "din", "LLL");

	// {dout,{Serial, Index, Value}}
	mh->registerType(MSG_DOUTPUT_CHANGED, "dout", "LLL");

	// {sout,{Serial, Index, Value}}
	mh->registerType(MSG_DOUTPUT_SET, "sdout", "LLL");
}//

void
drvIfk::txInputChanged(int index, int value) {

	if (mh->send(MSG_DINPUT_CHANGED, serial, index, value)) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending input changed message");
		error = true;
	}
}//

void
drvIfk::txOutputChanged(int index, int value) {

	if (mh->send(MSG_DOUTPUT_CHANGED, serial, index, value)) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending output changed message");
		error = true;
	}
}//

void drvIfk::txAttach(CPhidgetHandle h) {

	//paranoia
	if (NULL!=dv)
		delete dv;

	ph = h;
	dv = new phDevice(h);

	// {device,{Serial,Version,Type,Name,Label}}
	if (mh->send(MSG_DEVICE, serial, dv->version, dv->type, dv->name, dv->label, "active")) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending phidgetdevice ATTACH message");
		error = true;
	}

}//

void drvIfk::txDetach(CPhidgetHandle h) {

	//TODO maybe handle this in a better way?
	if (NULL==dv)
		return;

	// {device,{Serial,Version,Type,Name,Label}}
	if (mh->send(MSG_DEVICE, serial, dv->version, dv->type, dv->name, dv->label, "inactive")) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending phidgetdevice DETACH message");
		error = true;
	}

	if (NULL!=dv) {
		delete dv;
		dv = NULL;
	}
}//

void
drvIfk::txError(int code, const char *string) {

	if (mh->send(MSG_PHIDGETERROR, serial, code, string)) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending phidgeterror message");
		error = true;
	}

}//
