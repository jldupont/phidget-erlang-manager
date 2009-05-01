/*
 * qport.h
 *
 *  Created on: 2009-04-23
 *      Author: Jean-Lou Dupont
 */

#ifndef QPORT_H_
#define QPORT_H_

#include <sys/types.h>
#include "qpipe.h"

#define QPORT_CLIENT 0
#define QPORT_SERVER 1

#define QPORT_0  0
#define QPORT_1  1


	typedef struct {

		int   port;
		qpipe *pipe;

	} qport_context;

	typedef int qport;

	// API
	qport_context *qport_init(qpipe *qp, qport port);
	int            qport_send(qport_context *qpc, void *msg);
	void          *qport_receive(qport_context *qpc);


#endif /* QPORT_H_ */
