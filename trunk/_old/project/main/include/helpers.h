/**
 * @file helpers.h
 *
 * @date 2009-04-22
 * @author Jean-Lou Dupont
 */

#ifndef HELPERS_H_
#define HELPERS_H_

#include "logger.h"

#ifndef NULL
#define NULL (void *) 0
#endif

#ifdef _DEBUG
#define DEBUG_MSG(...) printf(__VA_ARGS__)
#define DEBUG_LOG(...) doLog(__VA_ARGS__)
#define DEBUG_LOG_NULL_PTR(ptr, ...) if (NULL==ptr) doLog(__VA_ARGS__)
#else
#define DEBUG_MSG(...)
#define DEBUG_LOG(...)
#define DEBUG_LOG_NULL_PTR(ptr, ...)
#endif


#endif /* HELPERS_H_ */
