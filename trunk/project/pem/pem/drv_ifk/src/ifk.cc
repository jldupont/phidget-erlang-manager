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
	if (waitMsg(&m, 250*1000)==0) {

		delete m;
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

	// {phidgetdevice,{Serial, state}}
	mh->registerType(MSG_PHIDGETDEVICE, "phidgetdevice", "LA");

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

void drvIfk::txAttach(void) {

	if (mh->send(MSG_PHIDGETDEVICE, "active")) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending phidgetdevice ATTACH message");
		error = true;
	}
}//

void drvIfk::txDetach(void) {

	if (mh->send(MSG_PHIDGETDEVICE, "inactive")) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending phidgetdevice DETACH message");
		error = true;
	}
}//

void
drvIfk::txError(int code, const char *string) {

	if (mh->send(MSG_PHIDGETERROR, serial, code, string)) {
		doLog(LOG_ERR, "drv_ifk: ERROR sending phidgeterror message");
		error = true;
	}

}//
