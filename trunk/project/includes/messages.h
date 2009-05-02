/**
 * @file   messages.h
 *
 * @date   2009-05-01
 * @author Jean-Lou Dupont
 *
 * \section Messages_Description Description of the Messages on the internal ``litm`` bus
 *
 *
 */

#ifndef MESSAGES_H_
#define MESSAGES_H_


	/**
	 * Message type field definition
	 */
	typedef enum _bus_message_type {



	} bus_message_type;


	/**
	 * Definition of a _phidget device_
	 *
	 * @param serial serial number
	 * @param name   device name
	 * @param type   device type
	 */
	typedef struct {

		char serial[32];
		char name[32];
		char type[32];

	} phidget_device;

			/**
			 * Definition of message _phidget_devices_
			 *
			 * @param count the number of devices listed in the message
			 *
			 */
			typedef struct {
				phidget_device device;
			} message_phidget_device;


			/**
			 * Definition of message _phidget_states_
			 */
			typedef struct {

			} message_phidget_states;


			/**
			 * Definition message _phidget_set_states_
			 */
			typedef struct {

			} message_phidget_set_states;


	/**
	 * Message envelope definition
	 */
	typedef struct {

		bus_message_type type;
		union {
			message_phidget_device     pd;
			message_phidget_states     ps;
			message_phidget_set_states pss;
		};

	} bus_message;


#endif /* MESSAGES_H_ */
