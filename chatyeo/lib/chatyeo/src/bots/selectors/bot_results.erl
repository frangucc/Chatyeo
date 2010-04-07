%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_results).

-behaviour(gen_server).

-export([start_link/2, notification/2]).

-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).

-include("config.hrl").

-record(state, {room, proxy_pid, user_pids=[], current_items}).

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
start_link(Room, MetaPid) ->
    gen_server:start_link(?MODULE, [Room, MetaPid], []).

notification(Pid, Message) ->
    gen_server:cast(Pid, Message).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([Room, ProxyPid]) ->
    CurrentItems = ets:new(nil, [public]),
    %%% Read in last displayed items for room
    Items = storage:get_items(Room, lists:flatten(io_lib:format("~p", [bot_utils:get_unix_timestamp()]))),

    lists:foreach(fun(X) ->
                          {ok, Function} = rfc4627:get_field(X, "Function"),
                          {ok, Row} = rfc4627:get_field(X, "Row"),
                          {ok, Depth} = rfc4627:get_field(X, "Depth"),
                          {ok, Id} = rfc4627:get_field(X, "_id"),
                          {ok, [Time, _]} = rfc4627:get_field(X, "key"),
                          ets:insert(CurrentItems, {{Function, Row, Depth}, binary_to_list(Id), Time, rfc4627:encode(X)})
                  end, Items),
    {ok, #state{room=Room, proxy_pid=ProxyPid, current_items=CurrentItems}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_cast({message_from, Msg}, State) ->
    ?INFO_MSG("bot_results got msg: ~p~n", [Msg]),
    {From, Value} = Msg,
    case From of
        results ->
            Time = bot_utils:get_unix_timestamp(),
            {Id, Data} = Value,
            %lists:foreach (fun(X) -> user_server:results(X, Id, Data) end, State#state.user_pids),
                        
            {ok, Json, _} = rfc4627:decode(Data),
            Type = rfc4627:get_field(Json, "Function"),
            Row = rfc4627:get_field(Json, "Row"),
            Depth = rfc4627:get_field(Json, "Depth"),

            case ets:lookup(State#state.current_items, {Type, Row, Depth}) of
                [{_, OldId, StartTime}] ->
                    storage:expire_item(State#state.room, OldId, StartTime, Time);
                _ ->
                    new_item
            end,
            
            ?INFO_MSG("bot_results ets insert: ~p~n", [Data]),

            ets:insert(State#state.current_items, {{Type, Row, Depth}, Id, Time, Data});
        %bot_topic ->            
            %lists:foreach (fun(X) -> user_server:new_topic(X, Value) end, State#state.user_pids);   
        _Unknown ->
            unknown
    end,
    {noreply, State};
handle_cast({message_from, _From, _Message}, State) ->
    %?INFO_MSG("Bot Result got: ~w/~w~n", [From, Message]),
    {noreply, State};
handle_cast({new_user, Pid}, State) ->    
    lists:foreach(fun({{_Type, _Row, _Depth}, Id, _Time, Data}) ->
                          user_server:results(Pid, Id, Data)
                  end, ets:tab2list(State#state.current_items)),
    UserPids = [Pid|State#state.user_pids],
    {noreply, State#state{user_pids=UserPids}};
handle_cast({remove_user, Pid}, State) ->    
    {noreply, State#state{user_pids=lists:delete(Pid, State#state.user_pids)}};
handle_cast(_Event, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
