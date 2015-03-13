# Principles #

  * Devices are discovered dynamically
  * The state changes "device attach" and "device detach" are published
    * A list of _active_ devices is published at a regular interval

  * Each device publishes its state changes
  * Each device publishes its current state at a regular interval


# System Components #

The architecture consists roughly of the major blocks:

  * manager: Publishes the state of devices
  * device drivers: type specific drivers (eg. InterfaceKit)

## Manager ##

  * Implemented as an Erlang Port Driver using stdin/stdout as communication pipe
  * Discovers phidget devices
  * Hooks on _attach_ and _detach_ events
  * Generates **phidgetdevice** message


## Drivers ##

  * Implemented as an Erlang Port Driver using stdin/stdout as communication pipe
  * Generate input/output changed messages
    * eg. InterfaceKit: _din_, _dout_, _sdout_
  * Generate _device_ message upon _attach_ event
  * Generate _phidgetdevice_ upon _detach_ event

## Messages ##

  * _phidgetdevice_     : active/inactive device announcement
  * din                 : digital input changed
  * dout                : digital output changed
  * sdout               : set digital output (command issued from Erlang side)
