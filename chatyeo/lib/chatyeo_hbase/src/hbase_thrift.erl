%%
%% Autogenerated by Thrift
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(hbase_thrift).
-behaviour(thrift_service).


-include("hbase_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
% enableTable(This, TableName)
function_info('enableTable', params_type) ->
  {struct, [{1, string}]}
;
function_info('enableTable', reply_type) ->
  {struct, []};
function_info('enableTable', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% disableTable(This, TableName)
function_info('disableTable', params_type) ->
  {struct, [{1, string}]}
;
function_info('disableTable', reply_type) ->
  {struct, []};
function_info('disableTable', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% isTableEnabled(This, TableName)
function_info('isTableEnabled', params_type) ->
  {struct, [{1, string}]}
;
function_info('isTableEnabled', reply_type) ->
  bool;
function_info('isTableEnabled', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% getTableNames(This)
function_info('getTableNames', params_type) ->
  {struct, []}
;
function_info('getTableNames', reply_type) ->
  {list, string};
function_info('getTableNames', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% getColumnDescriptors(This, TableName)
function_info('getColumnDescriptors', params_type) ->
  {struct, [{1, string}]}
;
function_info('getColumnDescriptors', reply_type) ->
  {map, string, {struct, {'hbase_types', 'columnDescriptor'}}};
function_info('getColumnDescriptors', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% getTableRegions(This, TableName)
function_info('getTableRegions', params_type) ->
  {struct, [{1, string}]}
;
function_info('getTableRegions', reply_type) ->
  {list, {struct, {'hbase_types', 'tRegionInfo'}}};
function_info('getTableRegions', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% createTable(This, TableName, ColumnFamilies)
function_info('createTable', params_type) ->
  {struct, [{1, string},
  {2, {list, {struct, {'hbase_types', 'columnDescriptor'}}}}]}
;
function_info('createTable', reply_type) ->
  {struct, []};
function_info('createTable', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}},
  {3, {struct, {'hbase_types', 'alreadyExists'}}}]}
;
% deleteTable(This, TableName)
function_info('deleteTable', params_type) ->
  {struct, [{1, string}]}
;
function_info('deleteTable', reply_type) ->
  {struct, []};
function_info('deleteTable', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% get(This, TableName, Row, Column)
function_info('get', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string}]}
;
function_info('get', reply_type) ->
  {struct, {'hbase_types', 'tCell'}};
function_info('get', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% getVer(This, TableName, Row, Column, NumVersions)
function_info('getVer', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string},
  {4, i32}]}
;
function_info('getVer', reply_type) ->
  {list, {struct, {'hbase_types', 'tCell'}}};
function_info('getVer', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% getVerTs(This, TableName, Row, Column, Timestamp, NumVersions)
function_info('getVerTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string},
  {4, i64},
  {5, i32}]}
;
function_info('getVerTs', reply_type) ->
  {list, {struct, {'hbase_types', 'tCell'}}};
function_info('getVerTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% getRow(This, TableName, Row)
function_info('getRow', params_type) ->
  {struct, [{1, string},
  {2, string}]}
;
function_info('getRow', reply_type) ->
  {struct, {'hbase_types', 'tRowResult'}};
function_info('getRow', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% getRowWithColumns(This, TableName, Row, Columns)
function_info('getRowWithColumns', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, {list, string}}]}
;
function_info('getRowWithColumns', reply_type) ->
  {struct, {'hbase_types', 'tRowResult'}};
function_info('getRowWithColumns', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% getRowTs(This, TableName, Row, Timestamp)
function_info('getRowTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, i64}]}
;
function_info('getRowTs', reply_type) ->
  {struct, {'hbase_types', 'tRowResult'}};
function_info('getRowTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% getRowWithColumnsTs(This, TableName, Row, Columns, Timestamp)
function_info('getRowWithColumnsTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, {list, string}},
  {4, i64}]}
;
function_info('getRowWithColumnsTs', reply_type) ->
  {struct, {'hbase_types', 'tRowResult'}};
function_info('getRowWithColumnsTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'notFound'}}}]}
;
% mutateRow(This, TableName, Row, Mutations)
function_info('mutateRow', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, {list, {struct, {'hbase_types', 'mutation'}}}}]}
;
function_info('mutateRow', reply_type) ->
  {struct, []};
function_info('mutateRow', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}}]}
;
% mutateRowTs(This, TableName, Row, Mutations, Timestamp)
function_info('mutateRowTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, {list, {struct, {'hbase_types', 'mutation'}}}},
  {4, i64}]}
;
function_info('mutateRowTs', reply_type) ->
  {struct, []};
function_info('mutateRowTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}}]}
;
% mutateRows(This, TableName, RowBatches)
function_info('mutateRows', params_type) ->
  {struct, [{1, string},
  {2, {list, {struct, {'hbase_types', 'batchMutation'}}}}]}
;
function_info('mutateRows', reply_type) ->
  {struct, []};
function_info('mutateRows', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}}]}
;
% mutateRowsTs(This, TableName, RowBatches, Timestamp)
function_info('mutateRowsTs', params_type) ->
  {struct, [{1, string},
  {2, {list, {struct, {'hbase_types', 'batchMutation'}}}},
  {3, i64}]}
;
function_info('mutateRowsTs', reply_type) ->
  {struct, []};
function_info('mutateRowsTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}}]}
;
% atomicIncrement(This, TableName, Row, Column, Value)
function_info('atomicIncrement', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string},
  {4, i64}]}
;
function_info('atomicIncrement', reply_type) ->
  i64;
function_info('atomicIncrement', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}}]}
;
% deleteAll(This, TableName, Row, Column)
function_info('deleteAll', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string}]}
;
function_info('deleteAll', reply_type) ->
  {struct, []};
function_info('deleteAll', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% deleteAllTs(This, TableName, Row, Column, Timestamp)
function_info('deleteAllTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string},
  {4, i64}]}
;
function_info('deleteAllTs', reply_type) ->
  {struct, []};
function_info('deleteAllTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% deleteAllRow(This, TableName, Row)
function_info('deleteAllRow', params_type) ->
  {struct, [{1, string},
  {2, string}]}
;
function_info('deleteAllRow', reply_type) ->
  {struct, []};
function_info('deleteAllRow', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% deleteAllRowTs(This, TableName, Row, Timestamp)
function_info('deleteAllRowTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, i64}]}
;
function_info('deleteAllRowTs', reply_type) ->
  {struct, []};
function_info('deleteAllRowTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% scannerOpen(This, TableName, StartRow, Columns)
function_info('scannerOpen', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, {list, string}}]}
;
function_info('scannerOpen', reply_type) ->
  i32;
function_info('scannerOpen', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% scannerOpenWithStop(This, TableName, StartRow, StopRow, Columns)
function_info('scannerOpenWithStop', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string},
  {4, {list, string}}]}
;
function_info('scannerOpenWithStop', reply_type) ->
  i32;
function_info('scannerOpenWithStop', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% scannerOpenTs(This, TableName, StartRow, Columns, Timestamp)
function_info('scannerOpenTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, {list, string}},
  {4, i64}]}
;
function_info('scannerOpenTs', reply_type) ->
  i32;
function_info('scannerOpenTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% scannerOpenWithStopTs(This, TableName, StartRow, StopRow, Columns, Timestamp)
function_info('scannerOpenWithStopTs', params_type) ->
  {struct, [{1, string},
  {2, string},
  {3, string},
  {4, {list, string}},
  {5, i64}]}
;
function_info('scannerOpenWithStopTs', reply_type) ->
  i32;
function_info('scannerOpenWithStopTs', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}}]}
;
% scannerGet(This, Id)
function_info('scannerGet', params_type) ->
  {struct, [{1, i32}]}
;
function_info('scannerGet', reply_type) ->
  {struct, {'hbase_types', 'tRowResult'}};
function_info('scannerGet', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}},
  {3, {struct, {'hbase_types', 'notFound'}}}]}
;
% scannerClose(This, Id)
function_info('scannerClose', params_type) ->
  {struct, [{1, i32}]}
;
function_info('scannerClose', reply_type) ->
  {struct, []};
function_info('scannerClose', exceptions) ->
  {struct, [{1, {struct, {'hbase_types', 'iOError'}}},
  {2, {struct, {'hbase_types', 'illegalArgument'}}}]}
;
function_info(xxx, dummy) -> dummy.

