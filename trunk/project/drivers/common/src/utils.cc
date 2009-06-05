/**
 * @file utils.cc
 *
 *  Created on: 2009-06-03
 *      Author: Jean-Lou Dupont
 */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ei.h>
#include <signal.h>
#include "utils.h"
#include "logger.h"

/**
 * Reads a 'packet' from the file
 * pointed to by 'fd' descriptor.
 *
 * @param fd file-descriptor
 * @param buf   pointer to buffer
 * @param size  pointer to size of resulting buffer
 *
 * @return -1 if error reading the 'len' header
 * @return -2 MALLOC ERROR
 * @return  <0  eg. EAGAIN, EBADF, EFAULT, EINTR, EINVAL, EIO, EISDIR
 * @return  >=0  number of bytes read
 */

int read_packet(int fd, byte **buf, int *size) {

  int len;

  //DEBUG_LOG(LOG_INFO, "read_packet: BEGIN, fd[%i]", fd);

  // need at least the packet length!
  if (read_exact(fd, *buf, 2) != 2)
    return(-1);

  //DEBUG_LOG(LOG_INFO, "read_packet: BEFORE len");

  len = ((*buf)[0] << 8) | (*buf)[1];

  //DEBUG_LOG(LOG_INFO, "read_packet: len[%i]", len);

  if (len > *size) {
    byte* tmp = (byte *) realloc(*buf, len);
    if (tmp == NULL)
      return -2;
    else
      *buf = tmp;

    *size = len;
  }

  //DEBUG_LOG(LOG_INFO, "read_packet: END");

  return read_exact(fd, *buf, len);
}


int read_exact(int fd, byte *buf, int len) {

	//DEBUG_LOG(LOG_INFO, "read_exact: BEGIN, fd[%i]", fd);

	int i, got=0;

	do {
		if ((i = read(fd, buf+got, len-got)) <= 0) {
			DEBUG_LOG(LOG_ERR, "read_exact: errno[%i][%s]", errno, strerror(errno));
			//DEBUG_LOG(LOG_ERR, "read_exact: errno[%i]", errno);
			//DEBUG_LOG(LOG_ERR, "read_exact: ERROR, i[%i]", i);
			return i;
		}
		got += i;
	} while (got<len);

	//DEBUG_LOG(LOG_INFO, "read_exact: END, got[%i]", got);

	return len;
}//


/**
 * Writes a 'message' to the 'file'
 * pointed to by 'fd' descriptor.
 *
 * @param fd file descriptor
 * @param buff erlang ei_x_buff
 *
 * @return >=0 number of bytes-written
 * @return <0  error: EAGAIN, EBADF, EFAULT, EINTR, EINVAL, EIO, EISDIR
 */
int write_msg(int fd, ei_x_buff *buff) {

  byte li;

  li = (buff->index >> 8) & 0xff;
  write_exact(fd, &li, 1);
  li = buff->index & 0xff;
  write_exact(fd, &li, 1);

  return write_exact(fd, (byte *)buff->buff, buff->index);
}





int write_exact(int fd, byte *buf, int len) {

  int i, wrote = 0;

  do {
    if ((i = write(fd, buf+wrote, len-wrote)) <= 0)
      return i;
    wrote += i;
  } while (wrote<len);

  return len;
}


void setup_signal_action( int signum, __sighandler_t fnc ) {

	sigset_t set;
	struct sigaction action;

	action.sa_handler = fnc;
	action.sa_flags=0;
	sigaction( signum, &action, NULL);

	sigfillset( &set );
	sigaddset( &set, signum );
	sigprocmask( SIG_UNBLOCK, &set, NULL );

}//
