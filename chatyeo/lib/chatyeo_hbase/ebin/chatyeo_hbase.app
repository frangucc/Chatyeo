%% This is the application resource file (.app file) for the chatyeo,
%% application.
{application, chatyeo_hbase, 
  [{description, "Chatyeo HBase Connection"},
   {vsn, "0.1.0"},
   {modules, [chatyeo_hbase_app,
              chatyeo_hbase_sup,
              chatyeo_hbase,

              hbase_users,
              hbase_rooms,

              hbase_types,
              hbase_thrift
              ]},
   {registered,[chatyeo_hbase_sup, chatyeo_hbase]},
   {applications, [kernel, stdlib, sasl, gas, ewlib]},
   {mod, {chatyeo_hbase_app,[]}},
   {start_phases, []}]}.

