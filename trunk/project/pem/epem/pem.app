{application, pem,
 [{description, "Phidget Erlang Manager"},
  {vsn, "1"},
  {modules, [pem_app, pem_sup, ifk, journal, manager, reflector]},
  {registered, [reflector, manager, ifk, journal]},
  {applications, [kernel, stdlib]},
  {mod, {pem_app,[]}},
  {env, []}
 ]
}.