/**
 * @file epapi_base.cc
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */

#include "epapi.h"

const char *
epapiErr::errors[] = {
	"OK",            //EEPAPI_OK
	"???",           //EEPAPI_ERR
	"check errno",   //EEPAPI_ERRNO
	"null pointer",  //EEPAPI_NULL
	"bad index",     //EEPAPI_BADINDEX
	"bad format",    //EEPAPI_BADFORMAT
	"malloc error",  //EEPAPI_MALLOC
	"realloc error", //EEPAPI_REALLOC
	"not found",     //EEPAPI_NOTFOUND

};

static const char *
epapiErr::strerror(epapiBase *o) {

	if (NULL==o)
		return NULL;

	if (num>(sizeof(epapiErr::errors)/sizeof(int)))
		return epapiErr::errors[1];

	int num=o->last_error;

	//if we need to look-up system errno
	if (EEPAPI_ERRNO==num) {

		return strerror(num);
	}

	return epapiErr::errors[num];
}//


#ifdef _DEBUG
/**
 * Crude logging function
 */
void doLog(int priority, const char *message, ...) {

	openlog("epapi", LOG_PID, LOG_LOCAL1);

	char buffer[2048];
	va_list ap;

	va_start(ap, message);
		vsnprintf (buffer, sizeof(buffer), message, ap);
	va_end(ap);

	syslog(priority, buffer, NULL);

	closelog();

}
#endif
