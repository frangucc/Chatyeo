%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(user_server).

-behaviour(gen_server).

%% API
-export([start/3, start_link/2, send_message/3, send_muc_message/2, send_modal_message/2,
         leave_room/2, move_user_to_room/2, logout/1, change_anon_username/2,
         get_current_room_name/1, update_pid/2, pause_chatyeo/1, kill_old_frontend/1,
         unpause_chatyeo/1, get_current_grid_state/1, move_user_to_modal_room/2,
         leave_modal_room/1, populate_grid_with_lastest/1, add_friend_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("config.hrl").
-include_lib("nitrogen/include/wf.inc").
-include_lib("nitrogen/include/element_interface.inc").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-record(state, {db_username, jid, username, password, email, anon, paused=false, 
                gravatar, fullname, session, current_room, web_pid, modal_room}).

-define(USER_NODE_TYPES, ["results", "topics"]).

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
start_link(UserName, Password) ->
    gen_server:start_link(?MODULE, [UserName, Password], []).

start(Super, UserName, Password) ->
    supervisor:start_child (Super, [UserName, Password]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Logs a user out from jabber and terminates this server.
%% @spec logout(Pid::pid()) -> logging_out
%%
logout(Pid) ->
    gen_server:call (Pid, logout).

%% @doc Changes the anonymous username for the user to the given string.
%% @spec change_anon_username(Pid::pid(), NewUserName::string()) -> ok
%%
change_anon_username(Pid, NewUserName) ->
    gen_server:call (Pid, {change_anon_username, NewUserName}).

%% @doc Moves a user to a new room and leaves the current room, if there is one.
%% @spec move_user_to_room(Pid::pid(), Room::string()) -> ok
%%
move_user_to_room (Pid, Room) ->
    gen_server:call (Pid, {move_user_to_room, Room}).

%% @doc Moves the user to a modal popup room but leaves them logged in to the current main room.
%% @spec move_user_to_modal_room(Pid::pid(), Room::string()) -> ok
%%
move_user_to_modal_room (Pid, Room) ->
    gen_server:call (Pid, {move_user_to_modal_room, Room}).

%% @doc Moves a user out of a room.
%% @spec leave_room(Pid::pid(), Room::string()) -> ok
%%
leave_room (Pid, Room) ->
    gen_server:call (Pid, {leave_room, Room}).

%% @doc Moves a user out of a modal popup room.
%% @spec leave_modal_room(Pid::pid()) -> ok
%%
leave_modal_room (Pid) ->
    gen_server:call (Pid, leave_modal_room).

%% @doc Sends a groupchat message to the current room the user is logged into.
%% @spec send_muc_message(Pid::pid(), Message::string()) -> void()
%%
send_muc_message (Pid, Message) ->
    gen_server:cast (Pid, {send_muc_message, Message}).
 
%% @doc Sends a normal chat messages to a user.
%% @spec send_message(Pid::pid(), To::string(), Message::string()) -> void()
%%
send_message (Pid, To, Message) ->
    gen_server:cast (Pid, {send_message, To, Message}).

%% @doc Sends a groupchat message to the modal popup room the user is currnetly in.
%% @spec send_modal_message(Pid::pid(), Message::string()) -> void()
%%
send_modal_message (Pid, Message) ->
    gen_server:cast (Pid, {send_modal_message, Message}).

%% @doc Returns the unparsed name of the room the user is currently in.
%% @spec get_current_room_name(Pid::pid()) -> string()
%%
get_current_room_name (Pid) ->
    gen_server:call (Pid, get_current_room_name).    

%% @doc Chanages the stored pid for the web frontend.
%% @spec update_pid(Pid::pid(), WebPid::pid()) -> void()
%%
update_pid (Pid, WebPid) ->
    gen_server:cast (Pid, {update_pid, WebPid}).

%% @doc Pauses the results from changing on the frontend.
%% @spec pause_chatyeo(Pid::pid()) -> void()
%%
pause_chatyeo(Pid) ->
    gen_server:cast(Pid, pause_chatyeo).

%% @doc Unpauses the results grid.
%% @spec unpause_chatyeo(Pid::pid()) -> void()
%%
unpause_chatyeo(Pid) ->
    gen_server:cast(Pid, unpause_chatyeo).

%% @doc Inserts lastest items for the current room into the grid
%% @spec populate_grid_with_latest(Pid::pid()) -> void()
%%
populate_grid_with_lastest(Pid) ->
    gen_server:cast(Pid, populate_grid_with_lastest).

%% @doc Returns the current items in the grid for the current room.
%% @spec get_current_grid_state(Pid::pid()) -> [json_term()]
%%
get_current_grid_state(Pid) ->
    gen_server:call(Pid, get_current_grid_state).

%% @doc Adds a friend request 
%% @spec add_friend_request(Pid::pid(), UserName::string())
%%
add_friend_request(Pid, UserName) ->
    gen_server:cast(Pid, {add_friend_request, UserName}).

%% @doc Kills the old frontend if it is alive
%% @spec kill_old_frontend(Pid::pid()) -> ok
%%
kill_old_frontend(Pid) ->
    gen_server:call(Pid, kill_old_frontend).

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
init([UserName, Password]) ->       
    ?INFO_MSG ("Login: ~s", [UserName]),    

    %% Anon = if 
    %%            Password == "" ->
    %%                true;
    %%            true ->
    %%                false
    %%        end,
    
    {ok, EmailAddress} = db_interface:get_email_address(UserName),

    {ok, Session, Jid} = jabber_utils:connect(UserName, Password),
    
    {ok, #state{session=Session, jid=Jid, db_username=UserName, username=UserName, password=Password, email=EmailAddress, gravatar=element_gravatar:gravatar_icon(#gravatar{email=EmailAddress, size="35"}), anon=false}}.

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
handle_call({change_anon_username, NewUserName}, _From, State) ->
    ?INFO_MSG ("Changing ~s to ~s for channels", [State#state.username, NewUserName]), 
    db_interface:change_anon_username(State#state.username, NewUserName),
    {reply, ok, State#state{username=NewUserName}};
handle_call(get_current_room_name, _From, State) ->
    {reply, State#state.current_room, State};
handle_call({move_user_to_room, RoomUnparsed}, _From, State) -> 
    #state {
             session      = Session,
             current_room = CurrentRoom,
             username     = UserName,
             jid          = Jid
            } = State,

    ?INFO_MSG ("Moving ~s to ~s", [UserName, RoomUnparsed]),
    
    case CurrentRoom of
        undefined->
            nothing;
        _ ->
            jabber_utils:leave_room (Session, CurrentRoom, UserName),
            lists:foreach(fun(Node) -> pubsub_util:unsubscribe(Session, Jid, CurrentRoom, Node) end, ["results", "topics"])
    end,
    
    {ok, Room} = jabber_utils:move_user_to_room(Session, UserName, CurrentRoom, RoomUnparsed),
    lists:foreach(fun(Node) -> pubsub_util:subscribe(Session, Jid, Room, Node) end, ["results", "topics"]),
    
    {reply, ok, State#state{current_room=Room}};
handle_call({move_user_to_modal_room, RoomUnparsed}, _From, State) ->
    #state {
             session      = Session,
             current_room = CurrentRoom,
             username     = UserName
            } = State,
    
    ?INFO_MSG ("Moving ~s to Modal ~s", [UserName, RoomUnparsed]),
    {ok, Room} = jabber_utils:move_user_to_room(Session, UserName, CurrentRoom, RoomUnparsed),
    {reply, ok, State#state{modal_room=Room}};
handle_call({leave_room, RoomUnparsed}, _From, State) -> 
    ?INFO_MSG ("~s is leaving ~s", [State#state.username, RoomUnparsed]),

    #state {
                session  = Session,
                username = UserName,
                jid      = Jid
               } = State,

    case jabber_utils:parse_room_name(RoomUnparsed) of
        undefined ->
            nothing;
        Room ->
            jabber_utils:leave_room (Session, Room, UserName),
            lists:foreach(fun(Node) -> pubsub_util:unsubscribe(Session, Jid, Room, Node) end, ?USER_NODE_TYPES)
    end,
    
    {reply, ok, State#state{current_room=undefined}};
handle_call(leave_modal_room, _From, State) -> 
    #state {
                session      = Session,
                username     = UserName,
                modal_room   = Room
               } = State,

    ?INFO_MSG ("~s is leaving ~s", [UserName, Room]),
    
    jabber_utils:leave_room (Session, Room, UserName),
    {reply, ok, State#state{modal_room=undefined}};
handle_call(get_current_grid_state, _From, State) -> 
    Time = bot_utils:get_unix_timestamp(),
    Start = lists:flatten(io_lib:format("[0,~p]", [Time])),
    End =  lists:flatten(io_lib:format("[~p]", [Time])),
    CurrentState = couchdb_server:run_view(State#state.current_room, "items", "all", [{"startkey", Start}, {"endkey", End}]),
    {reply, CurrentState, State};
handle_call(kill_old_frontend, _From, State) when State#state.web_pid == undefined ->
    {reply, ok, State};
handle_call(kill_old_frontend, _From, State) ->
    WebPid = State#state.web_pid,
    case is_process_alive(WebPid) of
        true ->
            chat_frontend_server:logout(WebPid),
            {reply, ok, State#state{web_pid=undefined}};
        false ->
            {reply, ok, State#state{web_pid=undefined}}
    end.

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
handle_cast({send_modal_message, Message}, State) ->
    #state {
             session      = Session,
             modal_room   = ModalRoom,
             username     = UserName,
             gravatar     = Gravatar
            } = State,
    
    jabber_utils:send_muc_message(Session, ModalRoom, UserName, Gravatar, Message),
    {noreply, State};
handle_cast({update_pid, Pid}, State) ->    
    {noreply, State#state{web_pid=Pid}};
handle_cast(pause_chatyeo, State) ->
    #state {
             session      = Session,
             current_room = Room,
             jid          = Jid
            } = State,

    case Room of
        undefined ->
            {noreply, State#state{paused=true}};
        _ ->
            lists:foreach(fun(Node) -> pubsub_util:unsubscribe(Session, Jid, Room, Node) end, ?USER_NODE_TYPES),
            {noreply, State#state{paused=true}}
    end;
handle_cast(unpause_chatyeo, State) ->
    #state {
             session      = Session,
             current_room = Room,
             jid          = Jid
            } = State,
    
    chat_frontend_server:unpause_chatyeo(State#state.web_pid, State#state.current_room),
    lists:foreach(fun(Node) -> pubsub_util:subscribe(Session, Jid, Room, Node) end, ?USER_NODE_TYPES),
    {noreply, State#state{paused=false}};
handle_cast(populate_grid_with_lastest, State) ->
    chat_frontend_server:unpause_chatyeo(State#state.web_pid, State#state.current_room),
    {noreply, State};
handle_cast({add_friend_request, FriendUserName}, State) ->
    #state {
             username = UserName
            } = State,
    
    case db_interface:add_friend_request(UserName, FriendUserName) of
        ok ->
            send_friend_request_email(UserName, UserName, FriendUserName),
            {noreply, State};
        failed ->
            {noreply, State}
    end.

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
handle_info(#received_packet{packet_type=message, from="pubsub.localhost", raw_packet=Info}, State) when State#state.paused == false ->    
    case exmpp_xml:get_element(Info, "event") of
        undefined ->
            ok;
        Event -> 
            case exmpp_xml:get_element(Event, "items") of 
                undefined ->
                    ok;
                Items ->
                    case exmpp_xml:get_element(exmpp_xml:get_element(Items, "item"), "result") of
                        undefined ->
                            case exmpp_xml:get_element(exmpp_xml:get_element(Items, "item"), "topics") of
                                undefined ->
                                    ok;
                                TopicsElement ->
                                    Topics = lists:map(fun(X) -> exmpp_xml:get_cdata_as_list(X) end, exmpp_xml:get_elements(TopicsElement, "topic")),
                                    chat_frontend_server:new_topics(State#state.web_pid, Topics),
                                    chatyeo_hbase:add_user_topics_of_interest(State#state.username, Topics)
                            end;
                        Result ->
                            Id = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Result, "id")),
                            Json = exmpp_xml:get_cdata_as_list(exmpp_xml:get_element(Result, "json")),
                            chat_frontend_server:results(State#state.web_pid, Id, Json)
                    end
            end
    end,
    {noreply, State};
handle_info(#received_packet{packet_type=message, from=From, type_attr="groupchat", raw_packet=Info}, State) ->
    Message = exmpp_xml:get_cdata(exmpp_xml:get_element(Info, "body")),
    Room = lists:reverse(lists:nth(2, string:tokens(lists:reverse(From), "@"))),
    CurrentRoom = State#state.current_room,
    case Room of
        CurrentRoom ->
            chat_frontend_server:message(State#state.web_pid, binary_to_list(Message));
        _ ->
            chat_frontend_server:modal_message(State#state.web_pid, binary_to_list(Message))
    end,
    {noreply, State};
handle_info(#received_packet{packet_type=message, raw_packet=Info}, State) ->
    MessageXML = exmpp_xml:get_cdata(exmpp_xml:get_element(Info, "body")),
    case exmpp_xml:get_element(MessageXML, "move_room") of 
        undefined ->
            ok;
        MoveRoom ->
            Room = exmpp_xml:get_element(MoveRoom, "room"),
            db_interface:add_future_room (State#state.username, Room),
            chat_frontend_server:offer_change_room(State#state.web_pid, Room, Room)
    end,
    {noreply, State};
handle_info(#received_packet{packet_type=presence, raw_packet=Info}, State) ->        
    ThisUser = State#state.username,
    CurrentRoom = State#state.current_room,
    Children = exmpp_xml:get_child_elements (Info),
    case Children of
        [{xmlel, 'jabber:client', [], status, [], _}] ->
            nothing;
        _ ->
            FullUser = exmpp_xml:get_attribute(Info, from, <<"unknown">>),            
            [FullRoom, User] = string:tokens(binary_to_list(FullUser), "/"),
            [Room, _Server] = string:tokens(FullRoom, "@"),
            case hd(User) of
                $_ ->
                    not_a_user;
                _ ->
                    case jabber_utils:parse_room_name(Room) of
                       CurrentRoom ->
                            IsExit = exmpp_xml:get_attribute(Info, type, <<"new_user">>),
                            case IsExit of
                                <<"unavailable">> ->
                                    case User of                                
                                        ThisUser ->
                                            its_you;
                                        _ ->
                                            chat_frontend_server:leaving_user(State#state.web_pid, User)
                                    end;
                                <<"new_user">> ->
                                    chat_frontend_server:new_user(State#state.web_pid, User)
                            end;
                        _ ->
                            must_be_modal                                                       
                    end            
            end
    end,
    {noreply, State};
handle_info(_Info, State) ->
    %io:format ("~nGOT ~p~n", [_Info]),
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
terminate(_Reason, State) ->
    #state {
           web_pid      = WebPid,
           session      = Session,
           username     = UserName,
           current_room = Room
          } = State,
    
    jabber_utils:leave_room (Session, Room, UserName),    
    chat_frontend_server:logout(WebPid),
    exmpp_session:stop(State#state.session),

%% If its an anonymous user, delete from the db
    %% if 
    %%     State#state.anon == true ->
    %%         db_interface:delete_user(State#state.db_username);
    %%     true ->
    %%         nothing
    %% end,
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

send_friend_request_email(_UserName, FullName, Friend) ->
    {ok, FriendEmailBinary} = db_interface:get_email_address(Friend),
    FriendEmail = binary_to_list(FriendEmailBinary),
    Subject = friend_request_subject(FullName),
    Body = friend_request_body(FullName),
    chatyeo_email:send(Subject, Body, [FriendEmail], [FriendEmail]).

friend_request_subject(FullName) ->
    FullName ++ " has requested to be your friend on Chatyeo!".

friend_request_body(FullName) ->
    FullName ++ " has requested to be your friend on Chatyeo!".

%%%%%%%%%%%%%%%%
%% Unit Tests %%
%%%%%%%%%%%%%%%%
