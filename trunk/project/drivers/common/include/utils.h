/**
 * @file utils.h
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 */

#ifndef UTILS_H_
#define UTILS_H_

	typedef unsigned char byte;

	/**
	 * Write an Erlang message to a
	 * file pipe
	 */
	int write_msg(int fd, ei_x_buff *buff);

	/**
	 * Read an Erlang message from a
	 * file pipe
	 */
	int read_msg(int fd, byte **buf, int *size);

	/**
	 * Write an exact number of bytes to
	 * a file pipe
	 */
	int write_exact(int fd, byte *buf, int len);

	/**
	 * Read an exact number of bytes from
	 * a file pipe
	 */
	int read_exact(int fd, byte *buf, int len);

	/**
	 * Setup & enable an action handler for a signal
	 */
	void setup_signal_action( int signum, __sighandler_t fnc );


#endif /* UTILS_H_ */
