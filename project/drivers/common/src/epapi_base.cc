/**
 * @file epapi_base.cc
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */

#include "epapi.h"

const char *
epapiErr::errors[] = {
	"OK",
	"???",
	"check errno",
	"null pointer",
	"bad index",
	"bad format",

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
