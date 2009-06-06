/**
 * @file pkt.cc
 *
 * @date   2009-06-06
 * @author Jean-Lou Dupont
 */
#include "stdlib.h"
#include "pkt.h"

// =========================================
// PktBase class
// =========================================

const char *
PktBase::strerror(void) {

	return (const char *) errors[last_error];

}//

int
PktBase::error(void) {
	return last_error;
}

const char *
PktBase::errors[] = {
	"???",           //0
	"success",       //1
	"malloc error",  //2
	"invalid index", //3
	"null pointer",  //4
	"invalid format",//5
	"realloc error", //6
};


// =========================================
// Pkt Class
// =========================================

Pkt::Pkt() {
	sz=0;
	buf=NULL;
}//


Pkt::~Pkt() {
	if (NULL!=buf)
		free(buf);
}//


unsigned char *
Pkt::getBuf(void) {

	if (NULL!=buf)
		return buf;

	if (0==sz) {
		buf = (unsigned char *) malloc(Pkt::DSZ);
	}
	if (NULL!=buf) {
		sz = Pkt::DSZ;
	} else {
		last_error = 2;
	}

	return buf;
}//

unsigned char *
Pkt::getBuf(int size) {

	if (0==sz) {
		buf = (unsigned char *) malloc(size);
		if (NULL!=buf)
			sz=size;
		else {
			last_error = 2;
			return NULL;
		}
	}

	unsigned char *tmp;
	if (size>sz) {
		tmp = (unsigned char *) realloc(buf, size);
		if (NULL!=tmp) {
			sz = size;
			buf=tmp;
		} else {
			last_error = 6;
		}
	}
	return buf;
}//




// =========================================
// PktHandler Class
// =========================================

PktHandler::PktHandler() {

}//

PktHandler::PktHandler(int _ifd, int _ofd) {
	ifd = _ifd;
	ofd = _ofd;
}

PktHandler::~PktHandler() {

}//


int
PktHandler::rx_exact(unsigned char *, int len) {

}//

int
PktHandler::tx_exact(unsigned char *, int len) {

}//


int
PktHandler::rx(Pkt **p) {

}//


int
PktHandler::tx(Pkt *p) {

}//
