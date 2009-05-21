/**
 * @file   drivers.c
 *
 * @date   2009-05-21
 * @author Jean-Lou Dupont
 *
 *
 *
 */
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <litm.h>
#include "drivers.h"
#include "logger.h"

typedef void * lib_handle;

typedef struct _loaded_lib {

	char (*type_name)[];
	lib_handle h;

} loaded_lib;

int __drivers_initialized = 0;


/**
 * Contains the list of loaded libraries
 */
loaded_lib __loaded_libs[DRIVERS_MAX_LIBS];


// PROTOTYPES
// ==========
void __drivers_init(void);
void __drivers_load_lib(char (*type_name)[]);
lib_handle __drivers_search_type(char *type_name);



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

	__drivers_load_lib( type_name );

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
void __drivers_load_lib(char (*type_name)[]) {


}//


/**
 * Searches for a lib associated with a type
 *
 * @return lib_handle or NULL
 */
lib_handle __drivers_search_type(char (*type_name)[]) {

	lib_handle result = NULL;

	int i;
	for (i=0; i< DRIVERS_MAX_LIBS; i++) {
		if (0==strncmp(type_name, __loaded_libs[i].type_name)) {
			result = __loaded_libs[i].h;
		}
	}

	return result;
}//


/**
 * Adds a type to the list of loaded libs
 */
void __drivers__add_lib(char (*type_name)[], lib_handle h) {

	int i, done=0;
	for (i=0; i< DRIVERS_MAX_LIBS; i++) {
		if (NULL==__loaded_libs[i].h) {
			__loaded_libs[i].h = h;
			__loaded_libs[i].type_name = type_name;
			done++;
			break;
		}
	}
	if (!done)
		doLog(LOG_ERR, "drivers: could not add library");

}//
