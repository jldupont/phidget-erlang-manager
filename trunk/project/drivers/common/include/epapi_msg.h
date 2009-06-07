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

	/**
	 * Message text as expected
	 * from the Erlang side.
	 * Must be in ATOM format
	 * ie. all lower-case
	 */
	typedef const char *msg_type_text;


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
		char    *atoms[MAX_PARAMS];
		long     longs[MAX_PARAMS];
		double doubles[MAX_PARAMS];
		char  *strings[MAX_PARAMS];

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
	typedef std::pair<msg_type_text, const char *> PairTypeTextMap;

	typedef std::map<msg_type, const char*> TypeMap;
	typedef std::map<msg_type_text, const char*> TypeTextMap;




	/**
	 * Handler for messages to/from Erlang
	 */
	class MsgHandler: public epapiBase {

	public:
		/**
		 * Maximum length the 'message type'
		 * field can be.
		 */
		static const int MAX_TYPE_LENGTH = 32;

		static const int MAX_ATOM_SIZE   = 32;

		static const int MAX_STRING_SIZE = 4000;


	protected:

		PktHandler *ph;
		TypeMap map;
		TypeTextMap tmap;


	public:
		MsgHandler(PktHandler *ph);
		~MsgHandler();

		/**
		 * Registers a message type
		 *
		 * @param type
		 * @param signature (combination of [A|L|S])
		 */
		void registerType(	msg_type type,
							msg_type_text *ttype,
							const char *signature);

		/**
		 * Returns the signature corresponding
		 * to a type.
		 *
		 * Used during message transmission/encoding.
		 *
		 * @return NULL not found
		 */
		const char *getSignature(msg_type type);

		/**
		 * Returns the signature corresponding
		 * to a type text.
		 *
		 * Used during message reception/decoding.
		 */
		const char *getSignatureFromTypeText(msg_type_text *ttype);
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
