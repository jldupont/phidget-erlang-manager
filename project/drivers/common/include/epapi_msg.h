/**
 * @file msg.h
 *
 * @date 2009-06-04
 * @author Jean-Lou Dupont
 */

#ifndef MSG_H_
#define MSG_H_

//dev support
#ifndef EPAPI_H_
	#include "epapi.h"
#endif


	/**
	 * Message Type
	 */
	typedef int msg_type;



	/**********************************************
	 * Msg Class
	 */
	class Msg: public epapiBase {

	public:
		static const int MAX_PARAMS = 16;

	protected:

		msg_type type;
		int size;
		char mformat[MAX_PARAMS];

		//dirty... but quick!
		char   *atoms[MAX_PARAMS];
		long    longs[MAX_PARAMS];
		char *strings[MAX_PARAMS];

	public:
		Msg(void);
		~Msg();

		/**
		 * Returns the type of the message
		 */
		msg_type getType(void);

		/**
		 * Return the size of the parameter list
		 */
		int getSize(void);

		/**
		 * Returns the parameter @ index
		 *
		 * @param index
		 * @param **format [A|L|S]
		 * @param **param  value
		 */
		int getParam(int index, char *format, ...);

		/**
		 * Sets a parameter @ index
		 *
		 * Each time a parameter is set,
		 * the internal 'size' variable
		 * is incremented: thus, if this
		 * method is used on a parameter more
		 * than once, the 'size' count will
		 * be incorrect.
		 */
		int setParam(int index, char format, ...);

	};




	typedef std::pair<msg_type, const char *> PairTypeMap;
	typedef std::map<msg_type, const char*> TypeMap;




	/**
	 * Handler for messages to/from Erlang
	 */
	class MsgHandler: public epapiBase {

	protected:
		int ifd;
		int ofd;
		int usec_timeout;

		TypeMap map;


	public:
		MsgHandler(int ifd, int ofd, int usec_timeout);
		~MsgHandler();

		/**
		 * Registers a message type
		 *
		 * @param type
		 * @param signature (combination of [A|L|S])
		 */
		void registerType(msg_type type, const char *signature);

		/**
		 * Returns the signature corresponding
		 * to a type
		 *
		 * @return NULL not found
		 */
		const char *getSignature(msg_type type);


		/**
		 * Generic send message
		 *
		 * @param msg_type
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int send(msg_type type, ...);

		/**
		 * Generic receive message
		 *
		 * @param **m  Msg
		 *
		 * @return 0 SUCCESS
		 * @return 1 FAILURE
		 */
		int rx(Msg **m);

	};


#endif /* MSG_H_ */
