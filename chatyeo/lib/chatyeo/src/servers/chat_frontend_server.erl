%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(chat_frontend_server).

-behaviour(gen_server).

%% API
-export([start/1, logout/1, results/3, leaving_user/2, unpause_chatyeo/2,
         new_user/2, message/2, offer_change_room/3, modal_message/2, new_topics/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("nitrogen/include/wf.inc"). 
-include("config.hrl").

-record(state, {comet_pid}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start(CurrentState) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(CurrentState) ->
    gen_server:start(?MODULE, [CurrentState], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @docLogs a user out from jabber and terminates this server.
%% @spec logout(Pid::pid()) -> logging_out
%%
logout(Pid) ->
    gen_server:call(Pid, logout).

%% @doc Unpauses the results grid.
%% @spec unpause_chatyeo(Pid::pid(), Room::string()) -> ok
%%
unpause_chatyeo(Pid, Room) ->
    gen_server:cast(Pid, {unpause_chatyeo, Room}).

%% @doc Displays a message to the users web page.
%% @spec message(Pid::pid(), Msg::{std_msg, string()}) -> void()
%%
message(Pid, Msg) ->
    gen_server:cast(Pid, {message, Msg}). 

%% @doc Adds a message to the modal popup.
%% @spec modal_message(Pid::pid(), Msg::{std_msg, string()}) -> void()
%%
modal_message(Pid, Msg) ->
    gen_server:cast(Pid, {modal_message, Msg}). 

%% @doc Calls the javascript to offer a user a change of room.
%% @spec offer_change_room(Pid::pid(), Topic::string(), Room:string()) -> void()
%%
offer_change_room(Pid, Topic, Room) ->
    gen_server:cast(Pid, {offer_change_room, Topic, Room}).

%% @doc Calls the javasript that updates necessary pieces of the page when a new user joins.
%% @spec new_user(Pid::pid(), User::string()) -> void()
%%
new_user(Pid, User) ->
    gen_server:cast(Pid, {new_user, User}). 

%% @doc Calls the javasript that updates necessary pieces of the page when a user leaves.
%% @spec leaving_user(Pid::pid(), User::string()) -> void()
%%
leaving_user(Pid, User) ->
    gen_server:cast(Pid, {leaving_user, User}). 

%% @doc Sends the new result to the javascript code.
%% @spec results(Pid::pid(), Id::binary(), Data::binary()) -> void()
%%
results(Pid, Id, Data) ->
    gen_server:cast(Pid, {results, Id, Data}).

%% @doc Tells the frontend to update the you're discussing graphic
%% @spec new_topic(Pid::pid()) -> void()
new_topics(Pid, Topics) ->
    gen_server:cast(Pid, {new_topics, Topics}).

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
init([CurrentState]) ->
    %% Comet stuff so we can add actions
    [put(X, Y) || {X, Y} <- CurrentState],
    wf_comet:reset_nitrogen_state(),		
    wf:state(has_flash, true),
    CometPid = wf:state(wf_comet_pid),
    erlang:link(CometPid),

    {ok, #state{comet_pid=CometPid}}.

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
handle_call(logout, _From, State) ->
    wf:comet_flush(),
    erlang:unlink(State#state.comet_pid),
    erlang:exit(State#state.comet_pid, logging_out),
    {stop, logout, ok, State}.

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
handle_cast({message, Msg}, State) ->    
    wf:wire("client.AddChat("++Msg++")"),
    wf:comet_flush(),
    {noreply, State};
handle_cast({modal_message, Msg}, State) ->    
    wf:wire("client.AddModalChat("++Msg++")"),
    wf:comet_flush(),
    {noreply, State};
handle_cast({results, Id, Data}, State) ->
    %?INFO_MSG("chat_frontend_server got results: ~p~n", [Data]),
    
    wf:wire("client.AddResults(\""++Id++"\", "++Data++")"),
    wf:comet_flush(),
    {noreply, State};
handle_cast({new_user, User}, State) ->
    wf:wire("client.userEnteredChat(\""++User++"\")"),
    wf:comet_flush(),
    {noreply, State};
handle_cast({leaving_user, User}, State) ->
    wf:wire("client.userLeftChat(\""++User++"\")"),
    wf:comet_flush(),
    {noreply, State};
handle_cast({offer_change_room, Topic, Room}, State) ->
    wf:wire("client.SuggestTopic(\""++Topic++"\", \""++Room++"\")"),
    wf:comet_flush(),
    {noreply, State};
handle_cast({new_topics, Topics}, State) ->
    %Topics = db_interface:get_topics_for_room(user_server:get_current_room_name(wf:session(proxy_pid))),
    Text = #p{body=
              "You're discussing: " ++ lists:flatmap(fun(X) ->
                                                             X ++ ", "
                                                     end, Topics)++" ..."},
    wf:update(popads, Text),
    wf:comet_flush(),
    {noreply, State};
handle_cast({unpause_chatyeo, Room}, State) ->
    CurrentState = storage:get_items(Room, lists:flatten(io_lib:format("~p", [bot_utils:get_unix_timestamp()]))),
    lists:foreach(fun(Json) ->
                          {ok, Id} = rfc4627:get_field(Json, "_id"),
                          JsonEncoded = rfc4627:encode(Json),
                          wf:wire("client.AddResults(\""++binary_to_list(Id)++"\", "++JsonEncoded++")"),
                          timer:sleep(200),
                          wf:comet_flush()
                  end, CurrentState),
    %% Complete, now make a call to wipe out remaining loading spinners
    
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
handle_info({'EXIT', _, stop_comet}, State) ->
    {stop, left_page, State};
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
    wf:comet_flush(),
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
