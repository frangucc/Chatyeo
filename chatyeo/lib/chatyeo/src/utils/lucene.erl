-module(lucene).
-export([init/0, index_chat_history/0, add_chat_history/0, index_rssml/1, add_rssml/1, search_chat_history/1, search_rss/1]).

%% @doc Initialize Lucene server
%% @spec init()() -> ok
%% @todo Add code that actually starts up java process

init() ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 501, "blah"},
    receive
        Val -> ok
    end,
    Val.

%% @doc Indexes chat history data into Lucene to new index
%% @spec index_chat_history() -> ok

index_chat_history() ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 101, "blah"},
    receive
        Val -> ok
    end,
    Val.

%% @doc Adds most recent chat history data to already existing Lucene index
%% @spec add_chat_history() -> ok

add_chat_history() ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 101, "blah"},
    receive
        Val -> ok
    end,
    Val.

%% @doc Indexes a directory of rssml data into Lucene to new index
%% @spec index_rssml(RSSMLDirectory::directory()) -> ok

index_rssml(RSSMLDirectory) ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 102, RSSMLDirectory},
    receive
        Val -> ok
    end,
    Val.

%% @doc Adds directory of rssml data to already existing Lucene index
%% @spec add_rssml(RSSMLDirectory::directory()) -> ok

add_rssml(RSSMLDirectory) ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 102, RSSMLDirectory},
    receive
        Val -> ok
    end,
    Val.

%% @doc Searches chat history index
%% @spec search(Query::string()) -> result
%% @todo Set up nice way of doing node/server name

search_chat_history(Query) ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 1, Query},
    receive
        {ok, Res} ->
            Val = Res;
        {error, Res} ->
            Val = Res
    end,
    Val.

%% @doc Searches rss index
%% @spec search(Query::string()) -> result
%% @todo Set up nice way of doing node/server name

search_rss(Query) ->
    {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 2, Query},
    receive
        {ok, Res} ->
            Val = Res;
        {error, Res} ->
            Val = Res
    end,
    Val.
