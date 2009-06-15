/**
 * @file   base.h
 *
 * @date   10-Jun-2009
 * @author Jean-Lou Dupont
 */


#ifndef BASE_H_
#define BASE_H_

#include <pthread.h>
#include <epapi.h>

#include "logger.h"
#include "queue.h"
#include "device.h"

	/**
	 * Event type definition
	 */
	typedef enum {
		EVENT_MSG,
		EVENT_READ_ERROR
	} eventType;

	/**
	 * Event definition
	 */
	typedef struct {
		eventType type;
		Msg        *m;
	} event;

	/**
	 * Base class for drivers
	 */
	class drvBase {

	protected:

		PktHandler *ph;
		MsgHandler *mh;

		pthread_t   readThread;
		queue      *eq;

	public:
		/**
		 * Constructor
		 */
		drvBase();


		/**
		 * Destructor
		 */
		~drvBase();


	protected:


		/**
		 * Called to perform class
		 * specific initialization
		 */
		virtual void init(void) = 0;


		/**
		 * Wait for a message (with timeout)
		 *
		 * @param m message pointer
		 * @param usec_timeout timeout
		 *
		 * @return -1 no message
		 * @return  0 SUCCESS
		 * @return  1 FAILURE
		 *
		 */
		int waitMsg(Msg **m, int usec_timeout);


		/**
		 * Wait for a message (blocking)
		 *
		 * @param m message pointer
		 *
		 * @return -1 no message
		 * @return  0 SUCCESS
		 * @return  1 FAILURE
		 */
		int waitMsg(Msg **m);


	public:

		/**
		 * Starts the driver
		 */
		void start(void);

		void startReadThread(void);

		static void *readThreadFun( void *params );

		static event *createEvent(eventType type);

	};


	/**
	 * Class for passing parameters
	 * to the read thread function
	 */
	class drvReadThreadParams {

	public:
		drvBase    *drv;
		MsgHandler *mh;
		queue      *eq;
	};

#endif /* BASE_H_ */
