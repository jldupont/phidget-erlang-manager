/*
 * queuer.h
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#ifndef QUEUER_H_
#define QUEUER_H_

#include "../includes/manager.h"


	// Prototypes
	// ==========
	void queuer_init(void);
	void queuer_queue(PhidgetManagerMessage *msg);
	PhidgetManagerMessage *queuer_dequeue(void);
	void queuer_release(PhidgetManagerMessage *msg);



#endif /* QUEUER_H_ */
