/**
 * @file msg.h
 *
 * @date 2009-06-04
 * @author Jean-Lou Dupont
 */

#ifndef MSG_H_
#define MSG_H_

#include <sys/epoll.h>
#include <stdlib.h>
#include "types.h"

	typedef struct {

		int fd;
		int epfd;
		int usec_timeout;
		struct epoll_event epv;

	} msg_read_context;

	/**
	 * Sends an Event message through the
	 * file pipe
	 *
	 * @param fd    file descriptor
	 * @param event event pointer
	 *
	 * @return 0 FAILURE
	 * @return 1 SUCCESS
	 */
	int msg_send(int fd, Event *event);

	/**
	 * Setup the 'read' (input) pipe
	 *
	 * @param fd file descriptor
	 * @param usec_timeout
	 * @param **msg_read_context
	 *
	 * @return >=1 ERROR
	 * @return 1 malloc error
	 * @return 2 epoll_create error
	 */
	int msg_setup_read(int fd, int usec_timeout, msg_read_context **c);


	/**
	 * Waits for a message, use a timeout
	 *
	 * Message must be destroyed through msg_destroy
	 *
	 * @param c    context initialized through msg_setup_read
	 * @param m    message
	 *
	 * @return 1   SUCCESS
	 * @return 0   No message ready
	 * @return <0  ERROR
	 */
	int msg_rx_wait(msg_read_context *c, msg **m);


	/**
	 * Blocking message wait
	 *
	 * @return 1   SUCCESS, m is valid
	 * @return 0   no message
	 * @return <0  ERROR
	 */
	int msg_rx(int fd, msg **m);


	/**
	 * Destroys a message
	 */
	void msg_destroy(msg *m);



#endif /* MSG_H_ */
