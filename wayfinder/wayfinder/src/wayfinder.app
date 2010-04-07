{application, wayfinder,
 [{description, "wayfinder"},
  {vsn, "0.01"},
  {modules, [
    wayfinder,
    wayfinder_app,
    wayfinder_sup,
    wayfinder_web,
    wayfinder_deps
  ]},
  {registered, []},
  {mod, {wayfinder_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
