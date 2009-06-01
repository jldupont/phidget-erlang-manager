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

#include "manager.h"

#	define LITM_BUS_MESSAGES 1
#	define LITM_BUS_SYSTEM   2

#	define LITM_ID_MAIN      1
#	define LITM_ID_SIGNALS   2
#	define LITM_ID_SERVER    3
#	define LITM_ID_MANAGER   4
#	define LITM_ID_MESSAGES  5
#	define LITM_ID_STIMER    6

#	define MESSAGE_MAX_DEVICES 32


	/**
	 * Message type field definition
	 */
	typedef int bus_message_type;


	/**
	 * Digital State type
	 *
	 * \note NOTE: Packing efficiency is not important.
	 *
	 */
	typedef unsigned char phidget_digital_state;

	/**
	 * Digital States definition
	 *
	 */
	typedef enum _digital_states {

		// output
		DIGITAL_STATE_O_T = 1,	// tri-state
		DIGITAL_STATE_O_X,		// don't care
		DIGITAL_STATE_O_I,		// invert
		DIGITAL_STATE_O_0,
		DIGITAL_STATE_O_1,

		// input
		DIGITAL_STATE_I_Q,		// hi-q
		DIGITAL_STATE_I_X,		// don't care
		DIGITAL_STATE_I_I,		// invert
		DIGITAL_STATE_I_0,
		DIGITAL_STATE_I_1,

	} digital_states;



			/**
			 * Definition of shutdown message
			 */
			typedef struct {
				//nothing really to add here
			} message_shutdown;

			/**
			 * Definition of timer message
			 *
			 * TODO define timer message
			 */
			typedef struct {
				int counter;
			} message_timer;


			/**
			 * Definition of message _phidget_device_
			 *
			 * @param count the number of devices listed in the message
			 *
			 */
			typedef struct {
				PhidgetDevice *device;
			} message_phidget_device;


			/**
			 * Definition of message _phidget_digital_state_
			 */
			typedef struct {

				int serial;

				int index;
				int value;

			} message_phidget_digital_state;


			/**
			 * Definition message _phidget_digital_set_states_
			 */
			typedef struct {
				PhidgetDevice* device;

				int count;
				digital_states (*states)[];
			} message_phidget_digital_set_states;


	/**
	 * Message types
	 *
	 * 0 is reserved
	 */
	typedef enum _message_types {

		MESSAGE_PHIDGET_DEVICE = LITM_MESSAGE_TYPE_USER_START,
		MESSAGE_PHIDGET_DIGITAL_STATE,
		MESSAGE_PHIDGET_DIGITAL_SET_STATES

	} message_types;


	/**
	 * Message envelope definition
	 *
	 *
	 * @param type message type
	 * @param serial phidget device serial id
	 * @param message_body message body for the specific message type
	 *
	 */
	typedef struct {

		union _message_body {

			message_shutdown					ms;
			message_timer						mt;
			message_phidget_device				mpd;
			message_phidget_digital_state       mps;
			message_phidget_digital_set_states  mpss;

		} message_body;

	} bus_message;


	// PROTOTYPES
	// ==========

	void messages_init(void);

#endif /* MESSAGES_H_ */
