/**
 * @file epapi.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 *
 */

#ifndef EPAPI_H_
#define EPAPI_H_

#include <map>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <ei.h>


	enum _epapi_error_codes {

		EEPAPI_OK   = 0,
		EEPAPI_ERRNO,
		EEPAPI_NULL,
		EEPAPI_BADINDEX,
		EEPAPI_BADFORMAT,
		//EEPAPI_,
		//EEPAPI_
	};

	class epapiBase {

	protected:
		int last_error;

	};

	class epapiErr {

	public:
		static const char *errors[];

	public:
		/**
		 * Returns a human readable string
		 * corresponding to the last error
		 * set in the supplied object
		 */
		static const char *strerror(epapiBase *o);
	};


	#include "epapi_pkt.h"
	#include "epapi_msg.h"


#endif /* EPAPI_H_ */
