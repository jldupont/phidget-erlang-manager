/*
 * logger.c
 *
 *  Created on: 2009-04-21
 *      Author: Jean-Lou Dupont
 */
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include "../includes/logger.h"

char *_LOGGER_IDENTITY = "phidgetmanager";

/**
 * Crude logging function
 */
void doLog(int priority, char *message, ...) {

	openlog(_LOGGER_IDENTITY, LOG_PID, LOG_LOCAL1);

	char buffer[2048];
	va_list ap;

	va_start(ap, message);
		vsnprintf (buffer, sizeof(buffer), message, ap);
	va_end(ap);

	syslog(priority, buffer, NULL);

	closelog();

}

void doLogString(int priority, char *message, char *param) {

	openlog(_LOGGER_IDENTITY, LOG_PID, LOG_LOCAL1);

	syslog(priority, message, param);

	closelog();
}


void doLogInteger(int priority, char *message, int param) {

	openlog(_LOGGER_IDENTITY, LOG_PID, LOG_LOCAL1);

	syslog(priority, message, param);

	closelog();

}// [/doLog]
