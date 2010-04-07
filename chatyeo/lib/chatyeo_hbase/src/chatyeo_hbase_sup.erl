%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(chatyeo_hbase_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_hbase_thrift/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(HBASE_SERVER, "127.0.0.1").
-define(HBASE_PORT, 9090).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

start_hbase_thrift() ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    
    {ok, Client} = supervisor:start_child (?SERVER, {thrift_client, {thrift_client, start_link, [?HBASE_SERVER, ?HBASE_PORT, hbase_thrift]}, Restart, Shutdown, Type, [thrift_client]}),

    supervisor:start_child(?SERVER, {chatyeo_hbase, {chatyeo_hbase, start_link, [Client]}, Restart, Shutdown, Type, [chatyeo_hbase]}).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},    
    
    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
