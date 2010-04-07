%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start/1, bot_login/6]).

%% Supervisor callbacks
-export([init/1]).
 
-define(READER_PASSWORD, "temp4now").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

start(SimplePid) ->
    supervisor:start_child (SimplePid, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @spec reader_login(pid(), string(), pid()) -> {ok, pid()}
%%
%% @doc Ceates a reader_server which logs in to jabber.
bot_login (Pid, Module, Name, Password, Room, Nodes) ->
    supervisor:start_child (Pid, {{bot, Name}, {reader_server, start_link, [Module, Name, Password, Room, Nodes]}, transient, 2000, worker, [reader_server]}).

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
    
%%====================================================================
%% Internal functions
%%====================================================================
