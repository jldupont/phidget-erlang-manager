/**
 * @file   drivers_common.h
 *
 * @date   2009-05-21
 * @author Jean-Lou Dupont
 */

#ifndef DRIVERS_COMMON_H_
#define DRIVERS_COMMON_H_

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>

#include <litm.h>
#include <phidget21.h>

#include "helpers.h"

#define DTF_CAST ( void * (*)(void *) )

// DRIVER ID's on LITM - mainly used for debugging purposes
#	define LITM_DRIVER_IFK_ID	100


	/**
	 * Parameters for the driver thread function
	 */
	typedef struct {

		litm_bus msg;
		litm_bus sys;

	} driver_thread_params;


	/**
	 * Prototype of the entry point function for the drivers
	 *
	 * @param message_bus_id the LITM bus_id for the messages
	 * @param system_bus_id  the LITM bus_id for the system messages (eg. shutdown, timer etc.)
	 *
	 */
	void init(litm_bus messages_bus_id, litm_bus system_bus_id);

	/**
	 * Prototype of the thread function
	 */
	void *driver_thread_function(driver_thread_params *params);


#endif /* DRIVERS_COMMON_H_ */
