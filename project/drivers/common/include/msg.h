/**
 * @file msg.h
 *
 * @date 2009-06-04
 * @author Jean-Lou Dupont
 */

#ifndef MSG_H_
#define MSG_H_

#include "types.h"

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
	 * @param *epfd epoll_create id
	 * @param **epv epoll_event
	 *
	 * @return >=1 ERROR
	 * @return 1 malloc error
	 * @return 2 epoll_create error
	 */
	int msg_setup_read(int fd, int *epfd, epoll_event **epv);


#endif /* MSG_H_ */
