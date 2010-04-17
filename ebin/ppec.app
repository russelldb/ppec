{application, ppec,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             ppec_app,
             ppec_sup,
	     ppec,
             util
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { ppec_app, []}},
  {env, []}
 ]}.
