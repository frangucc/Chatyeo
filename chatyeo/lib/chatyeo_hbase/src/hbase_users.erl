%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(hbase_users).

-compile(export_all).

-include("config.hrl").
-include("hbase_thrift.hrl").
-include("hbase_types.hrl").
-include("hbase_constants.hrl").

%% @spec add_user_topic_of_interest(UserName::string(), Topic::string()) -> void()
%%
%% @doc
add_user_topic_of_interest(Client, UserName, Topic) ->
    thrift_client:call(Client, mutateRow, [?HBASE_USER_TABLE, UserName,
                                          [#mutation{isDelete=false,column="topic:"++Topic, value=""}]]).
    
%% @spec get_user_topics_of_interest(UserName::string()) -> [string()]
%%
%% @doc
get_user_topics_of_interest(Client, UserName) ->
    thrift_client:call(Client, getRow, [?HBASE_USER_TABLE, UserName]).

%% @spec user_has_topic_of_interest(UserName::string(), Topic::string()) -> true | false
%%
%% @doc
user_has_topic_of_interest(_Client, _UserName, _Topic) ->
    false.
