/**
 * @file   drivers.h
 *
 * @date   2009-05-21
 * @author Jean-Lou Dupont
 *
 * Dynamically loads phidget drivers
 *
 * \section conventions Library Conventions:
 *
 * 		\subsection naming Naming
 *
 *		The name of the library should follow the pattern:
 *		\code
 *				libpm{phidget_type_name}.so
 *		\endcode
 *		where <i>phidget_type_name</i> is in lower-case.
 *
 */

#ifndef DRIVERS_H_
#define DRIVERS_H_

#	include <litm.h>

#	define DRIVERS_MAX_LIBS 32

	/**
	 * Handles the dynamic library
	 * associated with the phidget type
	 *
	 * @param type_name string
	 * @param message_bus_id  the LITM bus_id for the general messages
	 * @param system_bus_id   the LITM bus_id for the system messages (eg. shutdown, timer)
	 */
	void drivers_handle_type( char (*type_name)[], litm_bus message_bus_id, litm_bus system_bus_id );


#endif /* DRIVERS_H_ */
