/**
 * @file   stimer.c
 *
 * @date   2009-05-11
 * @author Jean-Lou Dupont
 */

#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>

/**
 * Initializes the interval timer for the system
 */
void stimer_init(void) {

	struct itimerval itimer;

	itimer.it_interval.tv_sec=0;
	itimer.it_interval.tv_usec=1000*1000;
	itimer.it_value.tv_sec=0;
	itimer.it_value.tv_usec=1000*1000;
	setitimer(ITIMER_VIRTUAL, &itimer, NULL);

}//
