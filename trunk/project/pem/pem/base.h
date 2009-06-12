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

#include "queue.h"
#include "device.h"

	/**
	 * Class for passing parameters
	 * to the read thread function
	 */
	class drvReadThreadParams {

	public:
		MsgHandler *mh;
		queue      *eq;
	};

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
		void init(void);


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

		void startReadThread(void);

		static void *readThreadFun( void *params );

		static event *createEvent(eventType type);

	};


	#ifdef _DEBUG
	#include <syslog.h>
	void doLog(int priority, const char *message, ...);
	#define DBGBEGIN
	#define DBGEND
	#define DBGMSG(...) printf(__VA_ARGS__)
	#define DBGLOG(...) doLog(__VA_ARGS__)
	#define DBGLOG_NULL_PTR(ptr, ...) if (NULL==ptr) doLog(__VA_ARGS__)
	#define DEBUG_LOG DBGLOG
	#else
	#define DBGBEGIN if(0){
	#define DBGEND   }
	#define DBGMSG(...)
	#define DBGLOG(...)
	#define DBGLOG_NULL_PTR(ptr, ...)
	#define DEBUG_LOG DBGLOG
	#endif


#endif /* BASE_H_ */
