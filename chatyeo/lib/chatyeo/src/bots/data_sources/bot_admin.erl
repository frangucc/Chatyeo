%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2009, Tristan
%%% @doc 
%%%              
%%% @end
%%% Created : 10 Aug 2009 by Jay
%%%-------------------------------------------------------------------
-module(bot_admin).

-behaviour(gen_server).

%% API
-export([start_link/2, notification/2]).

-export([init/1, handle_cast/2, handle_call/3, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {room, room_id, meta_pid, window}).

-include("config.hrl").

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
init([Room, MetaPid]) ->
    Result = db_interface:get_room_id(Room),
    {ok, RoomID} = Result,
    {ok, #state{room=Room, room_id=RoomID, meta_pid=MetaPid, window=0}}.

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
    {Type, _Data} = Msg,
    case Type of
        std_msg ->
            ?INFO_MSG("Window size: ~p~n", [State#state.window]),
            if 
                State#state.window  == 2 ->
                    Reply = topic_detector:get_admin_data(State#state.room_id),           
                    ?INFO_MSG("ADMIN REPLY: ~p~n", [Reply]),
                    case Reply of
                        nil -> do_nothing;
                        List ->
                            {ok, Json, _} = rfc4627:decode(List),
                            reader_server:send_muc_message(State#state.meta_pid,
                                {results, rfc4627:encode({obj, [{'Function', ?TOPIC_JS}, {'Data', Json}]})})

                    end,
                    NewState = State#state{window=0};
                true ->
                    NewState = State#state{window=(State#state.window + 1)}
            end;
        _ ->
            NewState = State
    end,
    {noreply, NewState};
handle_cast({remove_user, _Pid}, State) ->  
    {noreply, State};
handle_cast(_Unknown, State) ->
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
