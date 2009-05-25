/**
 * @file logger.h
 *
 * @date   2009-04-21
 * @author Jean-Lou Dupont
 */

#ifndef LOGGER_H_
#define LOGGER_H_

#include <syslog.h>


	#ifdef __cplusplus
		extern "C" {
	#endif

		// Prototypes
		// ==========
		void doLog(int priority, const char *message, ...);

	#ifdef __cplusplus
		}
	#endif


#endif /* LOGGER_H_ */
