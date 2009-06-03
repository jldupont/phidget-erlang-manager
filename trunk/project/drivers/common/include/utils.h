/**
 * @file utils.h
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 */

#ifndef UTILS_H_
#define UTILS_H_

	typedef unsigned char byte;

	int write_msg(int fd, ei_x_buff *buff);
	int read_msg(int fd, byte **buf, int *size);


	int write_exact(int fd, byte *buf, int len);
	int read_exact(int fd, byte *buf, int len);


#endif /* UTILS_H_ */
