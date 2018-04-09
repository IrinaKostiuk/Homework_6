{application, cache_server,
 [
  {description, "Home Task 6"},
  {vsn, "0.1.0"},
  {id, ""},
  {registered, []},
  {modules, ['cache_server','cache_server_app','cache_server_hendler','cache_server_srv','cache_server_sup']},
  {applications, [
                  kernel,
                  stdlib,
                  sasl, 
                  cowboy,
                  jsx
                 ]},
  {mod, { cache_server_app, []}},
  {env, []}
 ]}.