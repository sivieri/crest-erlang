{application, crest,
 [{description, "crest"},
  {vsn, "0.01"},
  {modules, [
    crest,
    crest_app,
    crest_sup,
    crest_web,
    crest_deps
  ]},
  {registered, []},
  {mod, {crest_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
