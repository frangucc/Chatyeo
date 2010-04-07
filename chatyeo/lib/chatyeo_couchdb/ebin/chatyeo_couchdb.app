%% This is the application resource file (.app file) for the chatyeo,
%% application.
{application, chatyeo_couchdb, 
  [{description, "Chatyeo CouchDB App"},
   {vsn, "0.1.0"},
   {modules, [chatyeo_couchdb_app,
              chatyeo_couchdb_sup,
              couchdb_server,
              
              mochijson2,
              mochiweb_util,

              couchdb,
              erlang_couchdb
             ]},
   {registered,[chatyeo_couchdb_sup]},
   {applications, [kernel, stdlib]},
   {mod, {chatyeo_couchdb_app,[]}},
   {start_phases, []}]}.

