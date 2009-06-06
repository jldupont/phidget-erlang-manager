/**
 * @file epapi.h
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 *
 *
 * \note Only packet header with length field=2 is supported.
 */

#ifndef EPAPI_H_
#define EPAPI_H_

#include <map>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include <ei.h>

#ifdef _DEBUG
#include <syslog.h>
void doLog(int priority, const char *message, ...);
#define DBGBEGIN
#define DBGEND
#define DBGMSG(...) printf(__VA_ARGS__)
#define DBGLOG(...) doLog(__VA_ARGS__)
#define DBGLOG_NULL_PTR(ptr, ...) if (NULL==ptr) doLog(__VA_ARGS__)
#else
#define DBGBEGIN if(0){
#define DBGEND   }
#define DBGMSG(...)
#define DBGLOG(...)
#define DBGLOG_NULL_PTR(ptr, ...)
#endif


	enum _epapi_error_codes {

		EEPAPI_OK   = 0,
		EEPAPI_ERR,
		EEPAPI_ERRNO,
		EEPAPI_NULL,
		EEPAPI_BADINDEX,
		EEPAPI_BADFORMAT,
		EEPAPI_MALLOC,
		EEPAPI_REALLOC,
		EEPAPI_NOTFOUND,
		//EEPAPI_,
		//EEPAPI_
	};

	class epapiBase {

	protected:
		int last_error=0;

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
