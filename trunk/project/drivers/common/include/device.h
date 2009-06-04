/*
 * device.h
 *
 *  Created on: 2009-06-04
 *      Author: Jean-Lou Dupont
 */

#ifndef DEVICE_H_
#define DEVICE_H_

#include <phidget21.h>
#include "types.h"

	/**
	 * Creates a PhidgetDevice info structure
	 */
	PhidgetDevice* create_device_info(CPhidgetHandle phid);
	void destroy_device_info(PhidgetDevice *pd);


#endif /* DEVICE_H_ */
