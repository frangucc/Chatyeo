%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created :  3 Sep 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_manager).

-behaviour(gen_server).

%% API
-export([start_link/2, add_bot/3, notify/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("config.hrl").

-record(state, {supervisor, room, meta_pid, bots=[]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Supervisor, Room) ->
    gen_server:start_link(?MODULE, [Supervisor, Room], []).
 
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @spec add_bot(pid(), atom(), pid()) -> void()
%%
%% @doc Adds a bot, a gen_server specified by the moduale atom name to a room.
add_bot(Pid, Module, MetaPid) ->
    gen_server:cast(Pid, {add_bot, Module, MetaPid}).

%% @spec notify(pid(), string()) -> void()
%%
%% @doc Notifies all bots registered with this manage of a message.
notify(Pid, Message) ->
    gen_server:cast(Pid, {notify, Message}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Supervisor, Room]) ->
    {ok, #state{supervisor=Supervisor, room=Room}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_bot, Module, MetaPid}, State) ->
    {ok, Pid} = bot_sup:add_bot(State#state.supervisor, State#state.room, Module, MetaPid),
    {noreply, State#state{bots=[{Module, Pid}|State#state.bots]}};
handle_cast({notify, Message}, State) ->
    lists:foreach(fun ({Module, Pid}) ->
                          Module:notification(Pid, Message)
                  end, State#state.bots),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
