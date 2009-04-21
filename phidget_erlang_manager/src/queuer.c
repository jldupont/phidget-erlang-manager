/*
 * queuer.c
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#include <unistd.h>

#include "../includes/queuer.h"
#include "../includes/logger.h"
#include "../includes/manager.h"

/**
 * Queues a message in the communication queue
 */
void MessageQueuer(PhidgetManagerMessage *msg) {

	doLog(LOG_DEBUG,"MessageQueuer: BEGIN",NULL);

	manager_destroy_message( msg );

}//[/MessageQueuer]


/**
 * Retrieves the next message from the communication queue
 * Returns NULL if none.
 */
PhidgetManagerMessage *MessageDequeue(void) {


}//[/MessageDequeue]
