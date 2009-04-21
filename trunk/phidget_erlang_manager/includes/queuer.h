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
	void MessageQueuer(PhidgetManagerMessage *msg);
	PhidgetManagerMessage *MessageDequeue(void);



#endif /* QUEUER_H_ */
