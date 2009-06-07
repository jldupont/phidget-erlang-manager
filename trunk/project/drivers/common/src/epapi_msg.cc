/**
 * @file   epapi_msg.cc
 *
 * @date   2009-06-04
 * @author Jean-Lou Dupont
 *
 *
 */
#include "epapi.h"



// =========================================
// Msg class
// =========================================



Msg::Msg(void) {
	type = 0;
	size = 0;

	for (int i=0;i<MAX_PARAMS;i++) {
		mformat[i] = '\0';
		atoms[i]   = NULL;
		strings[i] = NULL;
		longs[i]   = 0;
		doubles[i] = 0.0;
	}

}//

Msg::~Msg() {

	for (int i=0;i<MAX_PARAMS;i++) {
		if (NULL!=atoms[i])
			free(atoms[i]);
		if (NULL!=strings[i])
			free(strings[i]);
	}
}//

	int
Msg::getSize(void) {
	return size;
}//

	msg_type
Msg::getType(void) {
	return type;
}//

	int
Msg::getParam(int index, char *format, ...) {

	if ((index>Msg::MAX_PARAMS)|| (index>size)) {
		last_error = EEPAPI_BADINDEX;
		return 1;
	}

	int result = 0; //optimistic

	*format = mformat[index];

	va_list args;
	va_start(args, format);

	switch(mformat[index]) {
	case 'a':
	case 'A':
		char **a;
		a = va_arg(args, char**);
		*a = atoms[index];
		break;
	case 'l':
	case 'L':
		long *l;
		l = va_arg(args, long *);
		*l = longs[index];
		break;
	case 'd':
	case 'D':
		double *d;
		d = va_arg(args, double *);
		*d = doubles[index];
		break;

	case 's':
	case 'S':
		char **s;
		s = va_arg(args, char**);
		*s = strings[index];
		break;
	default:
		last_error = EEPAPI_BADFORMAT;
		result = 1;
		break;
	}
	va_end(args);

	return result;
}//

	int
Msg::setParam(int index, char format, ...) {

	if ((index>Msg::MAX_PARAMS)|| (index>size)) {
		last_error = EEPAPI_BADINDEX;
		return 1;
	}

	// careful...
	size++;

	mformat[index] = format;

	int result = 0;

	va_list args;
	va_start(args, format);

	switch(format) {
	case 'a':
	case 'A':
		atoms[index] = va_arg(args, char *);
		break;
	case 'l':
	case 'L':
		longs[index] = va_arg(args, long);
		break;
	case 'd':
	case 'D':
		doubles[index] = va_arg(args, double);
		break;
	case 's':
	case 'S':
		strings[index] = va_arg(args, char *);
		break;
	default:
		last_error = EEPAPI_BADFORMAT;
		result = 1;
		break;
	}

	va_end(args);

	return result;
}//



// =========================================
// MsgHandler class
// =========================================
MsgHandler::MsgHandler(PktHandler *_ph) {
	ph  = _ph;
}//

MsgHandler::~MsgHandler() {
	if (NULL!=ph) {
		delete *ph;
	}
}//

	void
MsgHandler::registerType(	msg_type type,
							msg_type_text *ttype,
							const char *signature) {

	map.insert( PairTypeMap(type, signature) );
	tmap.insert( PairTypeTextMap(ttype, signature) );

}//

	const char *
MsgHandler::getSignature(msg_type type) {

	TypeMap::iterator it;

	it = map.find(type);
	if (it!=map.end()) {

		return it->second;
	}

	last_error = EEPAPI_NOTFOUND;
	return NULL;
}//

	const char *
MsgHandler::getSignatureFromTypeText(msg_type_text *ttype) {

	TypeTextMap::iterator it;

	it = tmap.find(type);
	if (it!=tmap.end()) {

		return it->second;
	}

	last_error = EEPAPI_NOTFOUND;
	return NULL;
}//

/**
 * Builds a 'packet' according to the
 * specified type. The type must have
 * been previously registered.
 *
 * @param msg_type
 *
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
	int
MsgHandler::send(msg_type type, ...) {

	//retrieve signature
	const char *sig = getSignature(type);
	if (NULL==sig)
		return 1;

	//get ourselves a Tx packet
	Pkt *p = new Pkt();
	ei_x_buff *b = p->getTxBuf();
	if (NULL==b) {
		delete *p;
		last_error = EEPAPI_MALLOC;
		return 1;
	}

	//tuple size
	int len=strlen( sig );

	if (len<=0) {
		last_error = EEPAPI_BADFORMAT;
		delete *p;
		ei_x_free(b);
		return 1;
	}

	 if (ei_x_new_with_version(b)) {
		 last_error = EEPAPI_NEWEIBUF;
		 delete *p;
		 ei_x_free(b);
		 return 1;
	 }


	 if (ei_x_encode_tuple_header(b, len)) {
		 last_error = EEPAPI_EIENCODE;
		 delete *p;
		 ei_x_free(b);
		 return 1; //<===================
	 }

	int index=0;
	int result=0; //optimistic
	va_list args;

	va_start(args, format);

	for(;index<len;index++) {
		switch(sig[index]) {

		case 's':
		case 'S':
			char *string;
			string=va_arg(args, char *);
			result= ei_x_encode_string(b, (const char *)string);
			break;
		case 'a':
		case 'A':
			char *atom;
			atom=va_arg(args, char *);
			result = ei_x_encode_atom(b, atom);
			break;
		case 'd':
		case 'D':
			double *d;
			d = va_arg(args, double *);
			result = ei_x_encode_double(b, d);
			break;
		case 'l':
		case 'L':
			long lint;
			lint = va_arg(args, long);
			result = ei_x_encode_long(b, lint);
			break;
		default:
			last_error = EEPAPI_BADFORMAT;
			result = 1;
			break;
		}//switch

		if (result) {
			last_error = EEPAPI_EIENCODE;
			result = 1; //precaution
			break;
		}
	}//for

	va_end(args);

	if (!result) {
		int tr = ph->tx( p );
		if (tr) {

			// the PktHandler layer
			// will tell us what we need...
			result = 1;
			last_error = p->last_error;
		}
	}

	delete *p;
	ei_x_free( b );

	return result;
}//

int
MsgHandler::rx(Msg **m) {

	Pkt *p = new Pkt();

	int r = ph->rx(&p);
	if (r) {
		last_error = ph->last_error;
		delete *p;
		return 1;
	}

	msg_type type;
	char stype[MAX_TYPE_LENGTH];
	int version;
	int arity;
	msg_type type;

	int index;
	char *b = (char *) p->getBuf();

	//we count on the first element of the
	//received tuple to contain an ATOM
	//which corresponds to the message type
	if (ei_decode_version((const char *) b, &index, version)) {
		delete *p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}
	if (ei_decode_tuple_header((const char *)b, &index, arity)) {
		delete *p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}

	if (ei_decode_atom((const char *)b, &index, &stype)) {
		delete *p;
		last_error=EEPAPI_EIDECODE;
		return 1;
	}

	if (arity>Msg::MAX_PARAMS) {
		last_error = EEPAPI_TOOBIG;
		delete *p;
		return 1;
	}

	Msg m = new Msg();

	//find a corresponding msg_type
	//so that we can decode the message
	const char *sig = getSignatureFromTypeText(stype);
	if (NULL==sig) {
		delete *p;
		//last_error already set
		return 1;
	}

	int len = strlen( sig );
	int result;

	for (int i=0;i<len;i++) {
		switch(sig[i]) {
		case 's':
		case 'S':
			char *string = (char *)malloc(MAX_STRING_SIZE);
			if (NULL==string) {
				last_error = EEPAPI_MALLOC;
				result = 1;
				break;
			}
			result = ei_decode_string(b, &index, string);
			break;
		case 'a':
		case 'A':
			char *atom = (char *)malloc(MAX_ATOM_SIZE);
			if (NULL==atom) {
				last_error = EEPAPI_MALLOC;
				result=1;
				break;
			}
			result = ei_decode_atom(b, &index, atom);
			break;
		case 'd':
		case 'D':
			double d;
			result = ei_decode_double(b, &index, &d);
			break;
		case 'l':
		case 'L':
			long lint;
			result = ei_decode_long(b, &index, &lint);
			break;
		default:
			last_error = EEPAPI_BADFORMAT;
			result = 1;
			break;
		}//switch

		if (result) {
			break;
		}

		// update message
		switch(sig[i]) {
		case 's':
		case 'S':
			m.
			break;
		case 'a':
		case 'A':
			break;
		case 'd':
		case 'D':
			break;
		case 'l':
		case 'L':
			break;
		}//switch



	}//for


	return 0;
}//


