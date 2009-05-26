/**
 * @file   drivers.c
 *
 * @date   2009-05-21
 * @author Jean-Lou Dupont
 *
 *
 *
 */

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <litm.h>
#include "drivers.h"
#include "logger.h"
#include "helpers.h"
#include "utils.h"

typedef void * lib_handle;

typedef struct _loaded_lib {

	char (*type_name)[];
	lib_handle h;

} loaded_lib;

int __drivers_initialized = 0;
const char base_path[] = "/usr/lib/libpm";


/**
 * Contains the list of loaded libraries
 */
loaded_lib __loaded_libs[DRIVERS_MAX_LIBS];


// PROTOTYPES
// ==========
void __drivers_init(void);
void __drivers_load_lib(char (*type_name)[], litm_bus message_bus_id, litm_bus system_bus_id);
lib_handle __drivers_search_type(char (*type_name)[]);
int __drivers__add_lib(char (*type_name)[], lib_handle h);


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




/**
 * This functions handles the loading (if not already)
 * of the dynamic library associated with a given
 * phidget type
 */
void drivers_handle_type(	char (*type_name)[],
							litm_bus message_bus_id,
							litm_bus system_bus_id ) {

	//low frequency enough
	__drivers_init();

	lib_handle lh=NULL;

	lh = __drivers_search_type( type_name );
	if (NULL!=lh) {
		//already loaded
		return;
	}

	__drivers_load_lib( type_name, message_bus_id, system_bus_id );

}//

void __drivers_init(void) {

	if (0==__drivers_initialized) {
		__drivers_initialized = 1;

		int i;
		for (i=0;i<DRIVERS_MAX_LIBS;i++) {
			__loaded_libs[i].type_name = NULL;
			__loaded_libs[i].h = NULL;
		}

	}
}//

/**
 * Loads & Inits a library
 */
void __drivers_load_lib(char (*type_name)[],
						litm_bus message_bus_id,
						litm_bus system_bus_id) {

	static char path[255];

	snprintf((char*)&path, sizeof(path), "%s%s.so", (char*) &base_path, (char*)type_name );
	string_tolower( (char *) path );

	doLog(LOG_INFO, "drivers: attempting to load driver[%s]", path);

	lib_handle lh=NULL;

	lh= dlopen( path, RTLD_LAZY);
	if (NULL==lh) {
		doLog(LOG_ERR, "drivers: failed to load driver[%s]", path);
		return;
	}

	int index=__drivers__add_lib( type_name, lh );

	if (-1==index) {
		doLog(LOG_ERR, "drivers: could not add driver to list");
		return;
	}



	char *error;
	void (*init)( litm_bus message_bus_id, litm_bus system_bus_id );

	init  = dlsym( lh, "init" );
	error = dlerror();

	if ( NULL!= error ) {
		doLog(LOG_ERR, "drivers: failed to find 'init' function in library [%s]", path);
		dlclose( lh );
		return;
	}

	doLog(LOG_INFO,"drivers: initializing library [%s]", path);
	(*init)(message_bus_id, system_bus_id);
	doLog(LOG_INFO,"drivers: finished initializing library [%s]", path);

}//


/**
 * Searches for a lib associated with a type
 *
 * @return lib_handle or NULL
 */
lib_handle __drivers_search_type(char (*type_name)[]) {

	DEBUG_LOG(LOG_DEBUG,"drivers: __drivers_search_type [%s]", type_name );

	lib_handle result = NULL;

	char (*sptr)[] = NULL;
	int i, len;
	len = strlen((char *) type_name);

	for (i=0; i< DRIVERS_MAX_LIBS; i++) {
		sptr = __loaded_libs[i].type_name;
		if (NULL==sptr)
			continue;

		if (0==strncmp((char *)type_name, (char*) sptr, len )) {
			result = __loaded_libs[i].h;
		}
	}

	return result;
}//


/**
 * Adds a type to the list of loaded libs
 */
int __drivers__add_lib(char (*type_name)[], lib_handle h) {

	DEBUG_LOG(LOG_DEBUG,"drivers: __drivers__add_lib [%s]", type_name );

	char (*tmp)[] = malloc( strlen((char*)type_name) * sizeof(char) );

	strcpy( (char *) tmp, (char*) type_name );

	int i, done=-1;
	for (i=0; i< DRIVERS_MAX_LIBS; i++) {
		if (NULL==__loaded_libs[i].h) {
			__loaded_libs[i].h = h;
			__loaded_libs[i].type_name = tmp;
			done=i;
			break;
		}
	}
	if (-1==done)
		doLog(LOG_ERR, "drivers: could not add library");

	return done;
}//
