{application, crest,
 [{description, "crest"},
  {vsn, "0.1"},
  {modules, [
    crest,
    crest_app,
    crest_sup,
    crest_web,
    crest_deps,
    crest_server,
    crest_process,
    crest_utils,
    crest_uuid,
    wordlist
  ]},
  {registered, []},
  {mod, {crest_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
