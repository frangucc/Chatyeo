%%%-------------------------------------------------------------------
%%% File    : jabber_utils.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  8 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(jabber_utils).
-compile(export_all).

-include("config.hrl").
-include("eunit.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(BOT_JABBER_PASSWORD, "temp4now").
-define(USER_JABBER_PASSWORD, "temp4now").
-define(NODE_CREATION_USER, "_create_room_nodes_user").

%% @spec connect(string(), string() -> {ok, #exmpp_session(), string()}
%%
%% @doc Connects to the jabber server 
connect(UserName, Password) ->
    MySession = exmpp_session:start(),
    MyJid = {_,Jid,_,_,_} = exmpp_jid:make(UserName, ?JABBER_HOST, random),
    
    exmpp_session:auth_basic (MySession, MyJid, Password),
    _StreamId = exmpp_session:connect_TCP(MySession, ?JABBER_HOST, ?JABBER_PORT),
    try exmpp_session:login(MySession),
        %% We explicitely send presence:
        exmpp_session:send_packet(MySession,
                                  exmpp_presence:set_status(
                                    exmpp_presence:available(), "Ready"))
    catch
        throw:{auth_error, 'not-authorized'} ->
            ?INFO_MSG ("Not authorized to login: ~s", [UserName]),
            {stop, login_failed};
        throw:Reason ->
            ?INFO_MSG ("Failed to login: ~s", [Reason]),
            {stop, login_failed}
    end,
    
    {ok, MySession, Jid}.

move_user_to_room(Session, UserName, CurrentRoom, RoomUnparsed) ->
    Room = jabber_utils:parse_room_name(RoomUnparsed),
    %case string:equal (Room, CurrentRoom) of
    %    true ->
    %        {ok, Room};
    %    false ->
            MoveMSG = create_move_room (Room, UserName),
            exmpp_session:send_packet(Session, MoveMSG),               
            db_interface:add_user_in_room (UserName, Room),            
            {ok, Room}. 
    %end.

move_bot_to_room(Session, UserName, RoomUnparsed) ->
    %case string:equal (Room, CurrentRoom) of
    %    true ->
    %        {ok, Room};
    %    false ->
    Room = jabber_utils:parse_room_name(RoomUnparsed),
    MoveMSG = jabber_utils:create_move_room (Room, UserName),
    exmpp_session:send_packet(Session, MoveMSG),    
    {ok, Room}.
    %end.

send_message(Session, To, MessageXML) ->        
    try     
        Msg = create_message (To, MessageXML),
        exmpp_session:send_packet(Session, Msg)
    catch
        throw:Error ->
            ?ERROR_MSG ("Unable to send jabber message ~p", [Error])
    end.

send_muc_message(Session, CurrentRoom, UserName, Gravatar, Message) ->
    try 
        Msg = create_muc_message (CurrentRoom, UserName, Gravatar, Message),
        exmpp_session:send_packet(Session, Msg)
    catch
        throw:Error ->
            ?ERROR_MSG ("Unable to send jabber message ~p", [Error])
    end.   

%% @spec parse_room_name(string()) -> string()
%%
%% @doc Replaces all spaces in a string with underscores and makes the string all lowercase
%% so the room name can be used in jabber and couchdb.
parse_room_name (RoomUnparsed) ->
    re:replace(string:to_lower(RoomUnparsed), " ", "_", [global, {return, list}]).

%% @spec unparse_room_name(binary()) -> binary()
%%
%% @doc Replaces all underscores with spaces.
unparse_room_name (Room) when is_binary(Room) ->
    RoomSpaces = re:replace(Room, "_", " ", [global, {return, list}]),
    list_to_binary(string:join(lists:map(fun(S) -> to_caps(S) end, string:tokens(RoomSpaces, " ")), " "));

%% @spec unparse_room_name(string()) -> string()
%%
%% @doc Replaces all underscores with spaces.
unparse_room_name (Room) ->
    RoomSpaces = re:replace(Room, "_", " ", [global, {return, list}]),
    string:join(lists:map(fun(S) -> to_caps(S) end, string:tokens(RoomSpaces, " ")), " ").

create_room (RoomUnparsed, Category) ->
    {ok, Bots} = gas:get_env(chatyeo, bots, []),
    create_room(RoomUnparsed, Category, Bots).

%% @type bot() = {atom(), atom()}. First atom is all, main or meta and the second is the module name
%%
%% @spec create_room(string(), string(), [bot()]) -> void()
%%
%% @doc Creates a room if it is not already started by logging in jabber readers to the room and
%% adding it to the databases and starting the necessary bots.

create_room (RoomUnparsed, Category, Bots) ->
    Room = parse_room_name(RoomUnparsed),

    case room_supervisor_table_utils:get_room_supervisor(Room) of
        undefined ->
            ?INFO_MSG("Creating room: ~s", [RoomUnparsed]),
            
            {ok, SimplePid} = chatyeo_sup:room_sup_start(Room), %bot_simple_one_for_one:start_link(),
            {ok, Pid} = bot_sup:start(SimplePid),

            room_supervisor_table_utils:add_room_supervisor(Room, Pid),
            
            db_interface:add_room (Room, Category),
            couchdb_server:create_db (Room),
            
            create_pubsubs(Room),
    
            lists:foreach (fun ({Type, BotCat, Bot, Nodes}) ->
                                   case BotCat of
                                       all ->
                                           add_bot(Pid, Bot, Type, Room, Nodes);
                                       Category ->
                                           add_bot(Pid, Bot, Type, Room, Nodes);
                                       _ ->
                                           do_not_add
                                   end
                           end, Bots);
            
            %db_interface:room_has_started(Room);
        {ok, _Pid} ->
            already_started
    end.

create_pubsubs(Room) ->
    {ok, Session, _Jid} = connect(?NODE_CREATION_USER, ?BOT_JABBER_PASSWORD),
    receive
        X -> X
    end,
    pubsub_util:create_room_nodes(Session, Room),
    exmpp_session:stop(Session).    

%% @spec get_bot_user_name(string(), string())
%%
%% @doc Returns the string user name for jabber for a bot in a certain room
get_bot_user_name(Bot, Room) ->
    "_"++atom_to_list(Bot)++Room.

%% @spec add_bot(bot(), atom(), string(), pid()) -> void()
%%
%% @doc Adds a bot to a specific room
add_bot (Pid, Bot, Type, Room, Nodes) ->
    BotUserName = get_bot_user_name(Bot, Room),
    {ok, BotPid} = bot_sup:bot_login(Pid, Bot, BotUserName, ?BOT_JABBER_PASSWORD, Room, Nodes),
    case Type of 
        main ->
            nothing;
            %reader_server:move_reader_to_room(BotPid);
        meta ->
            reader_server:subscribe_reader_to_nodes(BotPid, Room);
        both ->
            %reader_server:move_reader_to_room(BotPid),
            reader_server:subscribe_reader_to_nodes(BotPid, Room)
    end.

%% @spec register_user(string(), string(), string(), string()) -> void()
%%
%% @doc Registers a user with jabber and adds them to the database.
register_user (Username, FullName, EmailAddress, Password) ->
    jabber_register(Username, ?USER_JABBER_PASSWORD),
    db_interface:add_user (Username, FullName, EmailAddress, Password).

%% @spec register_reader(string()) -> void()
%%
%% @doc Registers a rooms reader with jabber.
register_reader (Name) ->
    jabber_register(Name, ?BOT_JABBER_PASSWORD).

%% @spec jabber_register(string(), string()) -> void()
%%
%% @doc Connects to jabber and sends a request to register a user.
jabber_register (Username, Password) ->
    MySession = exmpp_session:start(),
    MyJID = exmpp_jid:make(Username, ?JABBER_HOST, random),    
    exmpp_session:auth_basic (MySession, MyJID, Password),
    _StreamId = exmpp_session:connect_TCP(MySession, ?JABBER_HOST, ?JABBER_PORT),
   
    try exmpp_session:register_account(MySession, Password)
    catch
        throw:{register_error, conflict} ->
            ?INFO_MSG ("~s already registered", [Username]),
            already_registered;
          throw:Reason ->
            ?INFO_MSG ("~s", [Reason]),
            aborted
    end,
    
    exmpp_session:stop(MySession).

%% @spec internal_leave_room(string(), state()) -> void()
%%
%% @doc Sends a message to jabber to log the user out and notifies the bot manager
leave_room (_Session, undefined, _UserName) ->
    nothing_to_do;
leave_room (Session, Room, UserName) ->
    LeaveMSG = create_leave_room (Room, UserName),
    exmpp_session:send_packet(Session, LeaveMSG),
    db_interface:user_leaving_room (UserName, Room),

%% Leave Meta Room
    LeaveMSG2 = create_leave_room (bot_utils:get_meta_reader_name(Room), UserName),
    exmpp_session:send_packet(Session, LeaveMSG2).

%% @spec create_move_room(string(), string()) -> xmlel()
%%
%% @doc Returns the XML record necessary to move a user to a room in jabber.
create_move_room (Room, Username) ->
    #xmlel {name=presence, attrs=[#xmlattr{name=to, value=list_to_binary(Room++"@"++?JABBER_MUC_HOST++"/"++Username)}], children=[#xmlel{name=x, attrs=[#xmlattr{name=xmlns, value=?MUC}]}]}.

%% @spec create_leave_room(string(), string()) -> xmlel()
%%
%% @doc Returns the XML record necessary to move a user from a room in jabber.
create_leave_room (Room, Username) ->
    #xmlel {name=presence, attrs=[#xmlattr{name=to, value=list_to_binary(Room++"@"++?JABBER_MUC_HOST++"/"++Username)}, #xmlattr{name=type, value = <<"unavailable">>}]}.

%% @spec message_to_json(string(), string(), string()) -> json()
%%
%% @doc Takes a gravatar url, username and message and builds a json structure for being sent to the web frontend from it.
message_to_json (Gravatar, Username, Body) ->
    rfc4627:encode({obj, [{'ImgUrl', list_to_binary(Gravatar)}, {username, list_to_binary(Username)}, {body, list_to_binary(Body)}]}).

create_message (To, MessageXML) ->
    exmpp_xml:element(undefined, message,
                      [exmpp_xml:attribute(to, To++"@"++?JABBER_MUC_HOST)], 
                      [exmpp_xml:element(undefined, body, [],
                                         [MessageXML])]). 

%% @spec create_message(string(), string(), string(), string()) -> xmlel()
%%
%% @doc Returns the XML necessary to send a message through jabber.
create_muc_message (To, UserName, Gravatar, Message) ->
    exmpp_xml:element(undefined, message,
                      [exmpp_xml:attribute(to, To++"@"++?JABBER_MUC_HOST), 
                       exmpp_xml:attribute(type, "groupchat")],
                      [exmpp_xml:element(undefined, body, [],
                                         [exmpp_xml:cdata(message_to_json(Gravatar, UserName, Message))])]).

%% @spec to_caps(string()) -> string()
%%
%% @doc Capitalizes just the first letter of each word in a string.
to_caps(S) -> to_caps(S, $ , $ ).

to_caps(S, From, To) when is_atom(S) -> to_caps(atom_to_list(S), From, To);
to_caps(S, From, To) when is_list(S) -> to_caps(S, From, To, [], $ ).

to_caps([], _From, _To, Acc, _Last) ->
    lists:flatten(lists:reverse(Acc));
to_caps([From | T], From, To, Acc, _Last) ->
    to_caps(T, From, To, [To | Acc], To);
to_caps([H | T], From, To, Acc, Last)
  when Last >= $a, Last =< $z; Last >= $A, Last =< $Z ->
    to_caps(T, From, To, [string:to_lower([H]) | Acc], H);
to_caps([H | T], From, To, Acc, _Last) ->
    to_caps(T, From, To, [string:to_upper([H]) | Acc], H).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    
