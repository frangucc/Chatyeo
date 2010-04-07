%%%-------------------------------------------------------------------
%%% File    : freebase.erl
%%% Author  : Jay<jdmundrawala@gmail.com>
%%% Description : 
%%%
%%% Created : 14 Aug 2009
%%%-------------------------------------------------------------------
%%% Example Usage:
%%% freebase:init(),
%%% freebase:read({obj, [{"type",<<"/music/album">>},{"artist",<<"The Police">>},{"name",null}]}, []).
%%%
%%%

-module(json_traverse).
-export([
        read/1,
        contains/2,
        enumerate_ids/1,
        find_intersection/2
    ]).

-define(READ, "/api/service/mqlread").
-define(SEARCH, "/api/service/search").
-define(DOWNLOAD, "/api/trans/raw").
-define(BLURB, "/api/trans/blurb").
-define(THUMB, "/api/trans/image_thumb").
-define(BASE, "http://api.freebase.com").
-define(OK, "/api/status/ok").
-define(LIMIT, 15).


%%-------------------------------------------------------------------
%% @doc
%% Submit one or more MQL queries to a Metaweb database, using any
%% named options to override the option defaults. If there is
%% a single query, return the results of that query. Otherwise, return
%% an array of query results. Raises ServiceError if there were problems
%% with any of the queries.
%% @end
%%-------------------------------------------------------------------
read(File) ->
    {ok, Contents} = file:read_file(File),
    {ok, Json, []} = rfc4627:decode(Contents),
    Json.

contains(Id, Json) ->
    contains_traverse(Id, Json).
contains_traverse(Id, {_Key, List}) when is_list(List) ->
    lists:any(fun(Item) -> contains_traverse(Id, Item) end, List)
    ;
contains_traverse(Id, {Key, Id}) ->
    true;
contains_traverse(Id, {Key, Value}) ->
    false;
contains_traverse(Id, Id) ->
    true;
contains_traverse(Id, Value) ->
    false.

find_intersection(IdA, IdB) ->
    ListA = enumerate_ids(freebase:details(IdA,[{ignore_domain,[common, boot]}])),
    ListB = enumerate_ids(freebase:details(IdB,[{ignore_domain,[common, boot]}])),
    SetA  = sets:from_list(ListA),
    SetB  = sets:from_list(ListB),
    Intersect = sets:intersection(SetA, SetB),
    sets:to_list(Intersect).

enumerate_ids(Json) ->
    enumerate(Json, []).
enumerate([Head | Tail], Acc) ->
    enumerate(Tail, enumerate(Head, Acc));
enumerate([], Acc) ->
    Acc;
enumerate({_Key, List}, Acc) when is_list(List) ->
    enumerate(List, Acc);
enumerate({"id", Value}, Acc) ->
    [Value | Acc];
enumerate({"guid", Value}, Acc) ->
    [Value | Acc];
enumerate({Key, Value}, Acc) ->
    Acc;
enumerate(Value, Acc) ->
    Acc.
