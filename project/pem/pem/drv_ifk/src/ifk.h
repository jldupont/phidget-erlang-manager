/*
 * ifk.h
 *
 *  Created on: 2009-06-15
 *      Author: Jean-Lou Dupont
 */

#ifndef IFK_H_
#define IFK_H_

	#include "base.h"

	class drvIfk: public drvBase {

	public:
		enum _msg_types {
			MSG_PHIDGETERROR,
			MSG_PHIDGETDEVICE,
			MSG_DEVICE,
			MSG_DINPUT_CHANGED,
			MSG_DOUTPUT_CHANGED,
			MSG_DOUTPUT_SET,
		};

		static const char *ATOM_ACTIVE;
		static const char *ATOM_INACTIVE;

	public:
		bool error;
		int serial;
		CPhidgetHandle ph;

	public:
		drvIfk();
		~drvIfk();

		void init(void);

		void handleMsg(void);

		void txInputChanged(int index, int value);
		void txOutputChanged(int index, int value);

		void txAttach(void);
		void txDetach(void);

		void txError(int code, const char *string);

		void setDigitalOut(int index, int value);
	};


#endif /* IFK_H_ */
