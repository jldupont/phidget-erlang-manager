/**
 * @file device.cc
 *
 * @date 2009-06-04
 * @author Jean-Lou Dupont
 */
#include <phidget21.h>
#include <stdlib.h>
#include <string.h>
#include "msg.h"
#include "types.h"
#include "device.h"
#include "logger.h"

/**
 * Creates a device description object
 */
PhidgetDevice* create_device_info(CPhidgetHandle phid) {

	PhidgetDevice *pd;
	const char *type, *name, *label;

	// if malloc fails, we have a much bigger problem
	pd = (PhidgetDevice *) malloc(sizeof(PhidgetDevice));
	if (NULL!=pd) {

		CPhidget_getSerialNumber(phid, &pd->serial);
		CPhidget_getDeviceVersion(phid, &pd->version);
		CPhidget_getDeviceType(phid, (const char **) &type);
		CPhidget_getDeviceName(phid, (const char **) &name);
		CPhidget_getDeviceLabel(phid, (const char **)&label);

		//perform copies
		size_t sz_char = sizeof(char);

		size_t sz_type  = strlen( type )  + sz_char;
		size_t sz_name  = strlen( name )  + sz_char;
		size_t sz_label = strlen( label ) + sz_char;

		pd->type  = (char *) malloc( sz_type  * sizeof(char) );
		pd->name  = (char *) malloc( sz_name  * sizeof(char) );
		pd->label = (char *) malloc( sz_label * sizeof(char) );

		strncpy( pd->type,  type,  sz_type  );
		strncpy( pd->name,  name,  sz_name  );
		strncpy( pd->label, label, sz_label );
	}
	return pd;
}//


/**
 * Destroys a ``PhidgetDevice`` structure
 */
void destroy_device_info(PhidgetDevice *pd) {

	if (NULL==pd) {
		doLog(LOG_ERR, "destroy_device_info: NULL pointer");

	} else {
	//DEBUG_LOG(LOG_DEBUG, "manager: destroying device");

		free( pd->type );
		free( pd->name );
		free( pd->label );
		free( pd );
	}
	//DEBUG_LOG(LOG_DEBUG, "manager: finished destroying device");

}//
