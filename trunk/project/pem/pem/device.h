/**
 * @file   device.h
 *
 * @date   2009-06-12
 * @author Jean-Lou Dupont
 */

#ifndef DEVICE_H_
#define DEVICE_H_

	#include <phidget21.h>

	/**
	 * Class representing a Phidget device
	 */
	class phDevice {

	public:
		CPhidgetHandle phid;
		int  serial;
		int  version;
		char *type;
		char *name;
		char *label;

	public:
		phDevice(CPhidgetHandle phid);
		~phDevice();

		/**
		 * Used to initialize the instance
		 */
		void init(void);
	};


#endif /* DEVICE_H_ */
