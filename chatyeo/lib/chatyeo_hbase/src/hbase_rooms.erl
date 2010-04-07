%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(hbase_rooms).

-compile(export_all).
 
-include("config.hrl").
-include("hbase_thrift.hrl").
-include("hbase_types.hrl").
-include("hbase_constants.hrl").

%% @spec add_room_topic(RoomName::string(), Topic::string()) -> void()
%%
%% @doc
add_room_topic(Client, RoomName, Topic) ->
    thrift_client:call(Client, mutateRow, [?HBASE_ROOM_TABLE, RoomName,
                                          [#mutation{isDelete=false,column="topic:"++Topic, value=""}]]).

%% @spec get_room_topics(RoomName::string()) -> [string()]
%%
%% @doc
get_room_topics(Client, RoomName) ->
    thrift_client:call(Client, getRow, [?HBASE_ROOM_TABLE, RoomName]).    

%% @spec room_has_topic(RoomName::string(), Topic::string()) -> true | false
%%
%% @doc
room_has_topic(_Client, _RoomName, _Topic) ->
    false.
