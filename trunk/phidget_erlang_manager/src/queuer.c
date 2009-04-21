/*
 * queuer.c
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#include <unistd.h>
#include <pthread.h>
#include <list.h>

#include "../includes/queuer.h"
#include "../includes/logger.h"
#include "../includes/manager.h"

//GLOBALS
pthread_mutex_t queue_mutex = PTHREAD_MUTEX_INITIALIZER;

struct message_list_struct {
	PhidgetManagerMessage *msg;
	struct list_head list;
};

// Message List
// ------------
struct message_list_struct \
	messages;



// =======================================================================
// =======================================================================


/**
 * Initialize queue
 */
void queuer_init(void) {

	INIT_LIST_HEAD( &messages.list );

}// init


/**
 * Queues a message in the communication queue
 */
void queuer_queue(PhidgetManagerMessage *msg) {

	struct message_list_struct tmp;

	doLog(LOG_DEBUG,"MessageQueuer: BEGIN",NULL);

	pthread_mutex_lock( &queue_mutex );

		tmp = malloc(sizeof(struct message_list_struct));
		tmp->msg = msg;
		list_add( &(tmp->list), &(messages.list) );

	pthread_mutex_unlock( &queue_mutex );

	//DEBUG: manager_destroy_message( msg );

	doLog(LOG_DEBUG,"MessageQueuer: END",NULL);
}//[/queuer_queue]


/**
 * Retrieves the next message from the communication queue
 * Returns NULL if none.
 */
PhidgetManagerMessage *queuer_dequeue(void) {

	pthread_mutex_lock( &queue_mutex );

		if (list_empty( &(messages.list) ) == 1) {
			return NULL;
		}


	pthread_mutex_unlock( &queue_mutex );

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

	manager_destroy_message( msg );

}//[/queuer_release]

