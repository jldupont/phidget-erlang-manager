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
		bool error;
		int serial;
		CPhidgetHandle ph;

	public:
		drvIfk();
		~drvIfk();

		void init(void);

	};


#endif /* IFK_H_ */
