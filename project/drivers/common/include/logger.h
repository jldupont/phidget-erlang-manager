/**
 * @file logger.h
 *
 * @date   2009-04-21
 * @author Jean-Lou Dupont
 */

#ifndef LOGGER_H_
#define LOGGER_H_

#include <syslog.h>

#ifndef NULL
#define NULL (void *) 0
#endif

#ifdef _DEBUG
#define DEBUG_BEGIN
#define DEBUG_END
#define DEBUG_MSG(...) printf(__VA_ARGS__)
#define DEBUG_LOG(...) doLog(__VA_ARGS__)
#define DEBUG_LOG_NULL_PTR(ptr, ...) if (NULL==ptr) doLog(__VA_ARGS__)
#else
#define DEBUG_BEGIN if(0){
#define DEBUG_END   }
#define DEBUG_MSG(...)
#define DEBUG_LOG(...)
#define DEBUG_LOG_NULL_PTR(ptr, ...)
#endif

	#ifdef __cplusplus
		extern "C" {
	#endif

		// Prototypes
		// ==========
		void loggerSetIdentity(const char *ident);
		void doLog(int priority, const char *message, ...);

	#ifdef __cplusplus
		}
	#endif


#endif /* LOGGER_H_ */
