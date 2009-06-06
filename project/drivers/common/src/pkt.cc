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
	"check errno",   //7

};


// =========================================
// Pkt Class
// =========================================

/**
 * Creates RX packet type
 */
Pkt::Pkt() {
	sz=0;
	buf=NULL;
	tbuf=NULL;
}//

/**
 * Creates TX packet type
 */
Pkt::Pkt(int lenSz) {

}//

Pkt::~Pkt() {

	if (NULL!=buf)
		free(buf);

}//

ei_x_buff *
Pkt::getTxBuf(void) {

	return &tbuf;
}

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

	//requested size fits the
	//current size? else realloc
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

void
Pkt::setLength(int _len) {
	len = _len;
}

int
Pkt::getLength(void) {
	return len;
}

// =========================================
// PktHandler Class
// =========================================

PktHandler::PktHandler() {
	sz = 2;
}//

PktHandler::PktHandler(int size) {
	sz = size;
}//

PktHandler::PktHandler(int _ifd, int _ofd) {
	sz  = 2;
	ifd = _ifd;
	ofd = _ofd;
}

PktHandler::PktHandler(int size, int _ifd, int _ofd) {
	sz  = size;
	ifd = _ifd;
	ofd = _ofd;
}

PktHandler::~PktHandler() {

}//


/**
 * @return >=0 len retrieved
 * @return <0  ERROR eg. EPIPE
 */
int
PktHandler::rx_exact(Pkt **p, int len) {

	unsigned char *buf;
	buf=(*p)->getBuf();

	int i, got=0;

	do {
		if ((i = read(ifd, buf+got, len-got)) < 0) {
			return i;
		}
		got += i;
	} while (got<len);

	//DEBUG_LOG(LOG_INFO, "read_exact: END, got[%i]", got);

	return len;
}//

int
PktHandler::tx_exact(Pkt *p, int len) {


	unsigned char *buf = p->getBuf();

	int i, wrote = 0;

	do {
		if ((i = write(ofd, buf+wrote, len-wrote)) <= 0)
			return i;
		wrote += i;
	} while (wrote<len);

	return len;
}//

/**
 * @return 0 SUCCESS
 * @return 1 FAILURE
 */
int
PktHandler::rx(Pkt **p) {

	*p = new Pkt();

	//read length field first
	int result=rx_exact(p, sz);
	if (result<0) {
		last_error = 7; //check errno
		return 1;
	}

	int l = ((*buf)[0] << 8) | (*buf)[1];
	unsigned char *buf = (*p)->getBuf(l);

	// the packet length gave us
	// the information we needed
	// to extract the right count
	// of bytes from the pipe
	result = rx_exact(p, l);
	if (result<0) {
		last_error = 7;
		return 1;
	}

	(*p)->setLength( result );

	return 0;
}//


int
PktHandler::tx(Pkt *p) {

	unsigned char *buf = p->getBuf();
	unsigned char li;
	int len = p->getLength();

	li = (len >> 8) & 0xff;
	write_exact(ofd, &li, 1);
	li = len & 0xff;
	write_exact(ofd, &li, 1);

	return write_exact(fd, (byte *)buff->buff, buff->index);

}//
