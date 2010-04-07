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

-module(freebase).
-export([init/0,
        %content_url/1,
        %search/2
        %blurb_url/3,
        %thumbnail_url/3,
        details/2,
        read/2
    ]).

-define(READ, "/api/service/mqlread").
-define(SEARCH, "/api/service/search").
-define(DOWNLOAD, "/api/trans/raw").
-define(BLURB, "/api/trans/blurb").
-define(THUMB, "/api/trans/image_thumb").
-define(BASE, "http://api.freebase.com").
-define(OK, "/api/status/ok").
-define(LIMIT, 15).

init() ->
    ibrowse:start().

%%-------------------------------------------------------------------
%% @doc
%% Submit one or more MQL queries to a Metaweb database, using any
%% named options to override the option defaults. If there is
%% a single query, return the results of that query. Otherwise, return
%% an array of query results. Raises ServiceError if there were problems
%% with any of the queries.
%% @end
%%-------------------------------------------------------------------
read({obj, Query}, Options) ->
    read(Query, Options);
read(Query, _Options) ->
    read_traverse(rfc4627:encode(rfc4627:set_field({obj, Query},'limit',?LIMIT)), true, []).
%%-------------------------------------------------------------------
read_traverse(Query, Cursor, Acc) when Cursor /= false ->
    WebQuery = lists:flatten(io_lib:format("{\"cursor\":~s, \"query\": [~s]}",[Cursor, Query])),
    Url = ?BASE++?READ++"?query="++ibrowse_lib:url_encode(WebQuery),
    {ok, _S, _ResponseHeader, Results} = ibrowse:send_req(Url,[], get),
    {ok, Json, []} = rfc4627:decode(Results),
    Code = rfc4627:get_field(Json, "code"),
    case Code of
        {ok, <<?OK>>} ->
            {ok, NewCursor} = rfc4627:get_field(Json, "cursor"),
            {ok, Result} = rfc4627:get_field(Json, "result"),
            case NewCursor of
                false ->
                    {ok, Acc ++ Result};
                _ ->
                    read_traverse(Query, "\""++binary_to_list(NewCursor)++"\"", Acc ++ Result)
            end;
        _ ->
            {error, Json}
    end.

%details(Id, _Options) ->
    %Query = {obj, [{"id", Id},{"type", []}]},
    %{ok, [{obj, [{"id",_}, {"type", Types}]}]} = read(Query,[]),
    %ErlJson = lists:zipwith(fun(Prefix, Type) -> {[Prefix] ++ ":type", Type} end, lists:seq($A, $A+length(Types)-1), Types),
    %DetailQuery = {obj, [{"id", Id} | ErlJson]},
    %DQ2 = [DetailQuery | [{"*",[{obj,[]}]}|[]]], 
    %io:format("~p~n", [DQ2]),
    %read(DQ2, []).

%%-------------------------------------------------------------------
%% Get all the details about a given Id
%%-------------------------------------------------------------------
details(Id, _Options) ->
    TypeQuery = {obj, [{"id", Id},{"type",[]}]},
    {ok, [{obj, [{"id",_}, {"type", Types}]}]} = read(TypeQuery,[]),
    lists:foldl(fun(Type, AccIn) -> 
                    Query = {obj, [{"id",Id}, {"type", Type}, {"*",[{obj,[]}]}]},
                    {ok, [{obj, List}]} = read(Query, []),
                    AccIn ++ List
                end, [], Types).
