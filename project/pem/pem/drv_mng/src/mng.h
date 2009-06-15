/*
 * mng.h
 *
 *  Created on: 10-Jun-2009
 *      Author: EDUPJEA
 */

#ifndef MNG_H_
#define MNG_H_

	#include "base.h"

	//Msg ID
	#define MNG_MSG_PHIDGET_DEVICE 1

	#define MNG_STATE_ACTIVE   1
	#define MNG_STATE_INACTIVE 0


	class drvMng: public drvBase {

	public:
		bool error;

	public:

		drvMng();
		~drvMng();

		void init(void);

		void txPhidgetDeviceMsg(phDevice *phd, bool state);

	};

#endif /* MNG_H_ */
