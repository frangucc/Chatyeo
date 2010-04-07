%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_wayfair).

-behaviour(gen_server).

-export([start_link/2, notification/2]).

-export([init/1, handle_cast/2, handle_call/3,
         handle_info/2, terminate/2, code_change/3]).

-include("config.hrl").

-record(state, {room, meta_pid, window}).

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
init([Room, MetaPid]) ->
    {ok, #state{room=Room, meta_pid=MetaPid, window=[]}}.

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
    {Type, _Topic} = Msg,
    case Type of
        bot_topic ->
            Finds = [],
            lists:mapfoldl(fun({Title, ImgUrl, Subtitle, Desc}, Row) ->
                                   Depth = 1,
                                   Json = rfc4627:encode(make_json_wayfair (Row, Depth, Title, ImgUrl, Subtitle, Desc)),  
                                   reader_server:send_muc_message(State#state.meta_pid, {results, {bot_utils:get_hash(Json), Json}}),
                                   {[], Row+1} 
                           end, 1, Finds);
        _ ->
            ignore
    end,
    {noreply, State};
handle_cast({message_from, _From, _Message}, State) ->
    {noreply, State};
handle_cast({remove_user, _Pid}, State) ->  
    {noreply, State};
handle_cast({new_user, _User}, State) ->
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
make_json_wayfair (Row, Depth, Title, ImgUrl, Subtitle, Desc) ->
    Time = bot_utils:get_unix_timestamp(),
    {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?WAYFAIR_JS}, {'Row', Row}, {'Depth', Depth}, {'Title', list_to_binary(Title)}, {'ImgUrl', list_to_binary(ImgUrl)}, {'Subtitle', list_to_binary(Subtitle)}, {'Desc', list_to_binary(Desc)}]}.
