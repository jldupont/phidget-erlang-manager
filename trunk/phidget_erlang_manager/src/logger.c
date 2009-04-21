/*
 * logger.c
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */

#include "../includes/logger.h"

/**
 * Crude logging function
 */
void doLog(int priority, char *message, void *param) {

	char *_LOGGER_IDENTITY = "phidget_erl_manager";

	openlog(_LOGGER_IDENTITY, LOG_PID, LOG_LOCAL1);

	syslog(priority, message, param);

	closelog();

}// [/doLog]
