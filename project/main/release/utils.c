/**
 * @file   utils.c
 *
 * @date   2009-05-19
 * @author Jean-Lou Dupont
 */
#include <ctype.h>
#include <string.h>
#include "utils.h"

/**
 * Do nothing
 */
void void_cleaner(void *msg) {
	//
}

void string_tolower(char *string) {

	int i, len;

	len=strlen( string );

	for (i=0;i<len;i++)
		string[i] = tolower( string[i] );

}
