/*
 * helpers.h
 *
 *  Created on: 2009-04-22
 *      Author: Jean-Lou Dupont
 */

#ifndef HELPERS_H_
#define HELPERS_H_


#ifdef _DEBUG
#define DEBUG_MSG(...) printf(__VA_ARGS__)
#else
#define DEBUG_MSG(...)
#endif


#endif /* HELPERS_H_ */
