-ifndef(_hbase_types_included).
-define(_hbase_types_included, yeah).

-record(tCell, {value, timestamp}).

-record(columnDescriptor, {name, maxVersions, compression, inMemory, maxValueLength, bloomFilterType, bloomFilterVectorSize, bloomFilterNbHashes, blockCacheEnabled, timeToLive}).

-record(tRegionInfo, {startKey, endKey, id, name, version}).

-record(mutation, {isDelete, column, value}).

-record(batchMutation, {row, mutations}).

-record(tRowResult, {row, columns}).

-record(iOError, {message}).

-record(illegalArgument, {message}).

-record(notFound, {message}).

-record(alreadyExists, {message}).

-endif.
