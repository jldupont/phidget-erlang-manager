/**
 * @file signals.h
 *
 * @date   2009-04-22
 * @author Jean-Lou Dupont
 */

#ifndef SIGNALS_H_
#define SIGNALS_H_

#include <stdio.h>
#include <pthread.h>
#include <signal.h>

		//PROTOTYPES//
		//==========//
	void signals_init(void);
	int signals_get_signal(void);


#endif /* SIGNALS_H_ */
