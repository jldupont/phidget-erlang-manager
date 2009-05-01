/*
 * queuer.h
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#ifndef QUEUER_H_
#define QUEUER_H_

#include "../includes/manager.h"


	typedef struct node {
		PhidgetManagerMessage *msg;
		struct node *next;
	} node;

	typedef struct _queue {
		node *head, *tail;
	} queue;


	// Prototypes
	// ==========
	void queuer_init(void);
	void queuer_queue(PhidgetManagerMessage *msg);
	PhidgetManagerMessage *queuer_dequeue(void);
	void queuer_release(PhidgetManagerMessage *msg);



#endif /* QUEUER_H_ */
