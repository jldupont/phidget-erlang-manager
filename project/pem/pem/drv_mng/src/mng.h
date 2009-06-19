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
		CPhidgetManagerHandle phim;
		bool error;

		static const int TIME_WHEEL=240;
		static const int MESSAGE_MAX_DEVICES = 32;

		static const char *ATOM_ACTIVE;
		static const char *ATOM_INACTIVE;

	public:

		drvMng();
		~drvMng();

		void setPhim(CPhidgetManagerHandle phim);

		void init(void);

		void txPhidgetDeviceMsg(phDevice *phd, int state);

		void handleTimer(int count);
	};

#endif /* MNG_H_ */
