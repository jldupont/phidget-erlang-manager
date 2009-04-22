/*
 * test_daemon.c
 *
 *  Created on: 2009-04-22
 *      Author: Jean-Lou Dupont
 */

#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

int main(int argc, char **argv) {

	pid_t pid;

	pid = getpid();

	printf("\nPID: %u\n", pid);

	// ---
	char filename[255];

	snprintf(filename, sizeof(filename), "/proc/%u/cmdline", pid );

	FILE *pid_cmdline;

	char buffer[255];

	pid_cmdline = fopen( filename, "r" );

	fgets(buffer, sizeof(buffer), pid_cmdline );

	fclose( pid_cmdline );

	printf("cmdline: %s\n", buffer);

	// ===============

	int rc;
	regex_t * myregex = calloc(1, sizeof(regex_t));

	rc = regcomp( myregex, "daemon$", REG_EXTENDED | REG_NOSUB );

	printf("regcomp/rc: %u", rc);

	rc = regexec(myregex, buffer, 0 , 0 , 0 );

	printf("regexec/rc: %u", rc);
}
