%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(reader_server).

-behaviour(gen_server).

%% API
-export([start_link/5, send_message/3, send_muc_message/2, leave_room/2,
         move_reader_to_room/1, logout/1, publish/3, subscribe_reader_to_nodes/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("config.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-record(state, {bot_state, jid, module, nodes, username, password, gravatar, session, current_room}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module, UserName, Password, Room, Nodes) ->
    gen_server:start_link(?MODULE, [Module, UserName, Password, Room, Nodes], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc
%% @spec logout(Pid::pid()) -> logging_out
%%
logout(Pid) ->
    gen_server:call (Pid, logout).

%% @doc
%% @spec move_reader_to_room(Pid::pid()) -> ok
%%
move_reader_to_room (Pid) ->
    gen_server:call (Pid, move_reader_to_room).

subscribe_reader_to_nodes(Pid, Room) ->
    gen_server:call (Pid, {subscribe_reader_to_nodes, Room}).

%% @doc
%% @spec leave_room(Pid::pid(), Room::string()) -> ok
%%
leave_room (Pid, Room) ->
    gen_server:call (Pid, {leave_room, Room}).

%% @doc
%% @spec send_muc_message(Pid::pid(), Message::string()) -> void()
%%
send_muc_message (Pid, Message) ->
    gen_server:cast (Pid, {send_muc_message, Message}).
 
%% @doc
%% @spec send_message(Pid::pid(), To::string(), Message::string()) -> void()
%%
send_message (Pid, To, Message) ->
    gen_server:cast (Pid, {send_message, To, Message}).

%% @spec publish(Pid::pid(), Data::string()) -> void()
%%
%% @doc
publish(Pid, NodeType, Data) ->
    gen_server:cast(Pid, {publish, NodeType, Data}).

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
init([Module, UserName, Password, CurrentRoom, Nodes]) ->
    ?INFO_MSG ("Login: ~s", [UserName]),    

    jabber_utils:jabber_register (UserName, Password),
    {ok, Session, Jid} = jabber_utils:connect(UserName, Password),
    {ok, Room} = jabber_utils:move_bot_to_room(Session, UserName, CurrentRoom),    
    BotState = Module:moving_to_room(Room, Module:init(Session)),
    
    {ok, #state{bot_state=BotState, jid=Jid, module=Module, nodes=Nodes, gravatar="", session=Session, username=UserName, password=Password, current_room=jabber_utils:parse_room_name(Room)}}.

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
    ?INFO_MSG ("Logout: ~s", [State#state.username]),
    {stop, logging_out, logging_out, State};
handle_call(move_reader_to_room, _From, State) ->
    #state {
             session      = Session,
             username     = UserName,
             current_room = CurrentRoom,
             module       = Module
            } = State,
    
    ?INFO_MSG ("Moving ~s to ~s", [State#state.username, CurrentRoom]),

    jabber_utils:leave_room (Session, CurrentRoom, UserName),
    {ok, Room} = jabber_utils:move_bot_to_room(Session, UserName, CurrentRoom),

    NewBotState = Module:moving_to_room(Room, State#state.bot_state),
    
    {reply, ok, State#state{current_room=Room, bot_state=NewBotState}};
handle_call({subscribe_reader_to_nodes, RoomUnparsed}, _From, State) ->
    #state {
             session      = Session,
             nodes        = Nodes,
             module       = Module,
             jid          = Jid
            } = State,

    Room = jabber_utils:parse_room_name(RoomUnparsed),

    lists:foreach(fun(NodeType) ->
                          pubsub_util:subscribe(Session, Jid, Room, NodeType)
                  end, Nodes),

    NewBotState = Module:moving_to_room(Room, State#state.bot_state),
    
    {reply, ok, State#state{current_room=Room, bot_state=NewBotState}};
handle_call({leave_room, RoomUnparsed}, _From, State) -> 
    ?INFO_MSG ("~s is leaving ~s", [State#state.username, RoomUnparsed]),
    Room = jabber_utils:parse_room_name(RoomUnparsed),
    jabber_utils:leave_room (Room, State),
    {reply, ok, State#state{current_room=undefined}}.

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
handle_cast({send_message, To, Message}, State) ->
    #state {
             session  = Session,
             username = UserName,
             gravatar = Gravatar
            } = State,
    
    jabber_utils:send_message(Session, To, UserName, Gravatar, Message),
    {noreply, State};
handle_cast({send_muc_message, Message}, State) ->
    #state {
             session      = Session,
             current_room = CurrentRoom,
             username     = UserName,
             gravatar     = Gravatar
            } = State,
    
    jabber_utils:send_muc_message(Session, CurrentRoom, UserName, Gravatar, Message),
    {noreply, State};
handle_cast({publish, NodeType, Data}, State) ->
    pubsub_util:publish(State#state.session, State#state.current_room, atom_to_list(NodeType), Data),
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
handle_info(#received_packet{packet_type=message, from="pubsub.localhost", raw_packet=Info}, State) ->
    Module = State#state.module,
    %io:format ("~nGOT ~p~n",[Info]),
    case exmpp_xml:get_element(Info, "event") of
        undefined ->
            ok;
        Event ->
            case exmpp_xml:get_element(Event, "items") of 
                undefined ->
                    ok;
                Items ->
                    Item = exmpp_xml:get_element(Items, "item"),
                    case exmpp_xml:get_element(Item, "result") of
                        undefined ->
                            case exmpp_xml:get_element(Item, "topics") of
                                undefined ->
                                    case exmpp_xml:get_element(Item, "question") of
                                        undefined ->
                                            ok;
                                        Question ->
                                            io:format ("~nGOT QUESTION~n"),
                                            QuestionCdata = exmpp_xml:get_cdata_as_list(Question),
                                            Module:update(question, QuestionCdata, State#state.bot_state)
                                    end;
                                TopicsElement ->
                                    Topics = lists:map(fun(X) -> exmpp_xml:get_cdata_as_list(X) end, exmpp_xml:get_elements(TopicsElement, "topic")),
                                    Module:update(topics, Topics, State#state.bot_state)
                            end;
                        Result ->
                            Id = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Result, "id")),
                            Json = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Result, "json")),
                            Module:update(result, {Id, Json}, State#state.bot_state)
                    end
            end
    end,
    
    {noreply, State};
handle_info(#received_packet{packet_type=message, type_attr="groupchat", raw_packet=Info}, State) ->
    Module = State#state.module,
    Message = exmpp_xml:get_cdata(exmpp_xml:get_element(Info, "body")),
    NewBotState = Module:update(message, Message, State#state.bot_state),
    {noreply, State#state{bot_state=NewBotState}};
handle_info(#received_packet{packet_type=presence, raw_packet=_Info}, State) ->
    %Module = State#state.module,
    %Module:update(presence, self(), Info),
    {noreply, State};
handle_info(_Info, State) ->
    %io:format ("~nGot INFO ~p~n", [_Info]),
    {noreply, State}.

%%-------------------------------------------------------------------
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
terminate(_Reason, State) ->
    #state {
           session      = Session,
           current_room = CurrentRoom,
           username     = UserName,
           jid          = Jid,
           nodes        = Nodes
          } = State,

    case CurrentRoom of
        undefined ->
            jabber_utils:leave_room(Session, CurrentRoom, UserName),
            exmpp_session:stop(Session),
            ok;
        _ ->
            lists:foreach(fun(X) ->
                                  pubsub_util:unsubscribe(Session, Jid, CurrentRoom, X)
                          end, Nodes),
            jabber_utils:leave_room(Session, CurrentRoom, UserName),
            exmpp_session:stop(Session),
            ok
    end.

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
