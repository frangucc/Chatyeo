%% This is the application resource file (.app file) for the chatyeo,
%% application.
{application, chatyeo_db, 
  [{description, "Chatyeo DB Backend"},
   {vsn, "0.1.0"},
   {modules, [chatyeo_db_app,   
              chatyeo_db_sup,   
              db_connection_server,             
              db_interface
              ]},
   {registered,[chatyeo_db_sup, p1]},
   {applications, [kernel, stdlib, sasl, gas, mysql]},
   {mod, {chatyeo_db_app,[]}},
   {start_phases, []}]}.

