/*
 * mng.h
 *
 *  Created on: 10-Jun-2009
 *      Author: EDUPJEA
 */

#ifndef MNG_H_
#define MNG_H_

#include "pem/base.h"

	class drvMng: public drvBase {

	public:

		void init(void);

		void handleMsg(Msg *m);
	};

#endif /* MNG_H_ */
