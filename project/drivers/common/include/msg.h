/**
 * @file msg.h
 *
 * @date 2009-06-04
 * @author Jean-Lou Dupont
 */

#ifndef MSG_H_
#define MSG_H_

#include <map>
#include <sys/epoll.h>
#include <stdlib.h>
#include "types.h"

	/*********************************************
	 * MsgBase Class
	 */
	class MsgBase {

	private:
		static const char *errors[];

	protected:
		int last_error;

	public:
		/**
		 * Returns the pointer
		 * to the human readable
		 * message corresponding
		 * to the last error
		 */
		const char *strerror(void);

		/**
		 * Returns the error code
		 * of the last error
		 */
		int error(void);

	};


	/**********************************************
	 * Msg Class
	 */
	class Msg: public MsgBase {

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
		 * Return size of parameter list
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
	class MsgHandler: public MsgBase {

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
		 * Returns the code of the last error
		 */
		int errno(void);

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

	/**
	 * Sends an Event message through the
	 * file pipe
	 *
	 * @param fd    file descriptor
	 * @param event event pointer
	 *
	 * @return 0 FAILURE
	 * @return 1 SUCCESS
	 */
	int msg_send(int fd, Event *event);


	/**
	 * Blocking message wait
	 *
	 * @return 1   SUCCESS, m is valid
	 * @return 0   no message
	 * @return <0  ERROR
	 */
	int msg_rx(int fd, msg **m);


	/**
	 * Destroys a message
	 */
	void msg_destroy(msg *m);


	/**
	 * Generic message send
	 *
	 * Format string:
	 *  A: Atom
	 *  S: String
	 *  L: Long
	 */
	int msg_send_generic(int fd,  const char *format, ...);

#endif /* MSG_H_ */
