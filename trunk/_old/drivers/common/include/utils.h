/**
 * @file utils.h
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 */

#ifndef UTILS_H_
#define UTILS_H_

#include <signal.h>


	typedef unsigned char byte;

	/**
	 * Write an Erlang message to a
	 * file pipe
	 */
	int write_msg(int fd, ei_x_buff *buff);

	/**
	 * Read an Erlang packet from a
	 * file pipe
	 *
	 * @param size the packet size found
	 *
	 * @return -1   ERROR getting length
	 * @return -2   MALLOC ERROR
	 * @return <=0  ERROR
	 * @return >0   SUCCESS (packet size)
	 */
	int read_packet(int fd, byte **buf, int *size);

	/**
	 * Read an exact number of bytes from
	 * a file pipe
	 *
	 * @return number of bytes read
	 */
	int read_exact(int fd, byte *buf, int len);


	/**
	 * Write an exact number of bytes to
	 * a file pipe
	 */
	int write_exact(int fd, byte *buf, int len);


	/**
	 * Setup & enable an action handler for a signal
	 */
	void setup_signal_action( int signum, __sighandler_t fnc );


#endif /* UTILS_H_ */
