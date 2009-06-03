/**
 * @file drv.cc
 *
 * @date   2009-06-03
 * @author Jean-Lou Dupont
 *
 * Phidget Manager Driver
 *
 * This driver only sources to an Erlang process.
 * One message is defined:
 *
 * 'device' : device description
 *            This message is sent in response to:
 *            - 'attach' event
 *            - periodic enumeration of attached devices
 *
 * This driver uses the default 'stdout' to communicate.
 */

#include <stdio.h>
#include <unistd.h>

int main() {


}//
