/**
 * @file   drivers_common.h
 *
 * @date   2009-05-21
 * @author Jean-Lou Dupont
 */

#ifndef DRIVERS_COMMON_H_
#define DRIVERS_COMMON_H_

#include <litm.h>

	/**
	 * Prototype of the entry point function for the drivers
	 *
	 * @param message_bus_id the LITM bus_id for the messages
	 * @param system_bus_id  the LITM bus_id for the system messages (eg. shutdown, timer etc.)
	 *
	 */
	void (*init)(litm_bus messages_bus_id, litm_bus system_bus_id);


#endif /* DRIVERS_COMMON_H_ */
