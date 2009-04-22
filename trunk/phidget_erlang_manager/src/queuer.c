/*
 * queuer.c
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>

#include "../includes/helpers.h"
#include "../includes/queuer.h"
#include "../includes/logger.h"
#include "../includes/manager.h"

//GLOBALS
pthread_mutex_t queue_mutex = PTHREAD_MUTEX_INITIALIZER;

// Message Queue
// ------------
queue MessageQueue;


// =======================================================================
// =======================================================================


/**
 * Initialize queue
 */
void queuer_init(void) {

	MessageQueue.head = NULL;
	MessageQueue.tail = NULL;

}// init


/**
 * Queues a message in the communication queue
 */
void queuer_queue(PhidgetManagerMessage *msg) {

	node *tmp=NULL;

	DEBUG_LOG(LOG_DEBUG,"queuer_queue: BEGIN");

	pthread_mutex_lock( &queue_mutex );

		tmp = (node *) malloc(sizeof(node));
		tmp->msg = msg;

		//perform the ''put'' operation {
			//no next yet...
			tmp->next=NULL;

			//insert pointer to element first
			if (MessageQueue.tail!=NULL)
				(MessageQueue.tail)->next=tmp;

			// insert element
			MessageQueue.tail = tmp;

			//was the queue empty?
			if (MessageQueue.head==NULL)
				MessageQueue.head=tmp;
		//}

	pthread_mutex_unlock( &queue_mutex );

	//DEBUG: manager_destroy_message( msg );

	DEBUG_LOG(LOG_DEBUG,"queuer_queue: END");
}//[/queuer_queue]


/**
 * Retrieves the next message from the communication queue
 * Returns NULL if none.
 */
PhidgetManagerMessage *queuer_dequeue(void) {

	DEBUG_LOG(LOG_DEBUG,"queuer_dequeue: BEGIN");

	PhidgetManagerMessage *msg=NULL;

	pthread_mutex_lock( &queue_mutex );

		// perform the ``get`` operation {
		node *tmp = NULL;
		tmp = MessageQueue.head;
		if (tmp!=NULL) {
			MessageQueue.head = MessageQueue.head->next;
			msg = tmp->msg;
			free(tmp);
			doLog(LOG_DEBUG,"queuer_dequeue: MESSAGE PRESENT");
		}
		//}

	pthread_mutex_unlock( &queue_mutex );

	DEBUG_LOG(LOG_DEBUG,"queuer_dequeue: END");

	return msg;
}//[/queuer_dequeue]


/**
 * Releases a message
 *  Performs the appropriate clean-up
 *
 *  This function is sort-of trivial but it decouples
 *  the client from the underlying server.
 *
 */
void queuer_release(PhidgetManagerMessage *msg) {

	DEBUG_LOG(LOG_DEBUG,"queuer_release");

	manager_destroy_message( msg );

}//[/queuer_release]

