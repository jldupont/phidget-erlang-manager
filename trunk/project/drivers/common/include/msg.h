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


#endif /* MSG_H_ */
