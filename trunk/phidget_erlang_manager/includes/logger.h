/*
 * logger.h
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#ifndef LOGGER_H_
#define LOGGER_H_

#include <syslog.h>

	// Prototypes
	// ==========
	void doLog(int priority, char *message);
	void doLogInteger(int priority, char *message, int param);
	void doLogString(int priority, char *message, char *param);



#endif /* LOGGER_H_ */
