-module(web_chat).
-include_lib("nitrogen/include/wf.inc").
-include_lib("nitrogen/include/element_interface.inc").
-include("config.hrl").
-compile(export_all).

%% @spec title() -> string()
%%
%% @doc Returns the title of the page.
title() -> "Chatyeo!". 

%% @spec main() -> void()
%%
%% @doc Initial function called on page loads. This function checks
%% if the user is logged in, if not it creates and anonymous user
%% account for the user to use and displays an input box for them to
%% provide a nickname for their anonymous account or to actually register.
main() ->
    %% case wf:session(valid) of
    %%     1 -> 
    case wf:user() of
        undefined ->
            wf:redirect("/web/register");
                  %wf:wire("showAnonReg();"),                  
                   %% UserName = randoms:get_string(),
                   %% db_interface:add_user (UserName, "Mr. Nobody", UserName, ""),
                   %% wf:user(UserName),   
                   %% {ok, ProxyPid} = chatyeo_sup:jabber_anon_login (UserName),
                   %% wf:session(proxy_pid, ProxyPid),
                   %% UserName;
        User ->
            case wf:session(proxy_pid) of
                undefined ->
                    wf:clear_user(),
                    wf:redirect("/web/login");
                ProxyPid -> 
                    case is_process_alive(ProxyPid) of
                        true ->
                            user_server:kill_old_frontend(wf:session(proxy_pid)),
                            Pid = wf:comet_no_spawn(fun(CurrentState) ->
                                                            chat_frontend_server:start(CurrentState)
                                                    end),    
                            
                            user_server:update_pid(wf:session(proxy_pid), Pid),
                            {ok, LastRoom} = db_interface:get_last_room(User),
                            move_user_to_room (LastRoom), 
                            
                            #template {file=nitrogen:get_wwwroot()++"/user_template.html"};
                        false ->
                            wf:clear_user(),
                            wf:redirect("/web/login")
                    end
            end
    end.
    %%     _ ->
    %%         wf:redirect("/web/auth")
    %% end.

%% @spec callbacks() -> #interface
%%
%% @doc This function is called in the html template where it wants 
%% the javascript for js to Erlang calls to be placed. All interface
%% functions must be defined in callbacks/0
callbacks() ->
    #interface{functions=[#fType{name="sendMessage",params=["chatMsg"]},
                          #fType{name="popupAddChat",params=["chatMsg"]},
                          #fType{name="changeAnonymousUserName",params=["newUserName"]},
                          %#fType{name="registerUser",params=["username", "fullname", "password", "email"]},
                          #fType{name="pauseChatyeo",params=[]},
                          #fType{name="unpauseChatyeo",params=[]},
                          #fType{name={sync, "searchYourConversationSuggester"},params=["value", "wildCard", "caseSensitive", "notCharacter"]},
                          #fType{name={sync, "getRealtimeSearchResults"},params=["searchStr"]},
                          %#fType{name={sync, "getTagCloud"},params=["startTime","endTime"]},
                          #fType{name={sync, "getTopics"},params=[]},
                          #fType{name={sync, "getCurrentItems"},params=[]},
                          #fType{name="changeConversation",params=["conversationName"]},
                          #fType{name="popupConversation",params=["conversationName"]},
                          #fType{name="addConversation",params=["conversationName","topic"]},
                          #fType{name="logout",params=[]},  
                          #fType{name="requestFriend",params=["userName"]},
                          #fType{name="removeFriend",params=["userName"]},
                          #fType{name="leavePopUpChat",params=[]}
                       ],parentModule=?MODULE}.

%% @spec event({atom(), atom()})  -> {json, string()} | void()
%%
%% @doc Handles a specific event from the web page.
%% sendMessage - Queries or the chatMsg and sends it to the backend to be sent to the current room.
%% popupAddChat - Queries or the chatMsg and sends it to the backend to be sent to the current open popup.
%% changeAnonymousUserName -
%% registerUser - 
%% addConversation - 
%% changeConversation -
%% popupConversation - 
%% getTagCloud -
%% pauseChatyeo -
%% unpauseChatyeo -
%% searchYourConversationSuggester - 
%% getRealtimeSearchResults - 
%% logout -
%% requestFriend - 
%% removeFriend - 
%% leavePopUpChat - 
event(sendMessage)->
    [ChatMsg]=wf:q(chatMsg),
    case length(ChatMsg) of
        0 -> ignore;
        _ ->
            case send_message(html:encode(ChatMsg)) of
                failed ->
                    wf:flash ("Failed to send message, try again");
                ok ->
                    sent
            end            
    end;
event(popupAddChat)->
    [ChatMsg]=wf:q(chatMsg),
    case length(ChatMsg) of
        0 -> ignore;
        _ ->
            case send_modal_message(html:encode(ChatMsg)) of
                failed ->
                    wf:flash ("Failed to send message, try again");
                ok ->
                    sent
            end            
    end;
event(leavePopUpChat) ->
    user_server:leave_modal_room(wf:session(proxy_pid));
event(changeAnonymousUserName) ->
    [NewUserName] = wf:q(newUserName),
    case db_interface:is_username_used (NewUserName) of
        false ->
            wf:wire("usernameIsNotAvailable();");
        true ->
            wf:wire("hideAnonReg();"),
            user_server:change_anon_username(wf:session(proxy_pid), NewUserName)
    end;    
%% event(registerUser) ->
%%     [UserName] = wf:q(username), 
%%     [FullName] = wf:q(fullname), 
%%     [Password] = wf:q(password),
%%     [Email] = wf:q(email),

%%     case jabber_utils:register_user (UserName, FullName, Email, Password) of
%%         ok ->                        
%%             wf:user(UserName),   
%%             {ok, Pid} = chatyeo_sup:jabber_login (UserName),
%%             wf:session(proxy_pid, Pid),          
%%             wf:redirect("chat");
%%         {aborted, Reason} ->
%%             wf:flash (io_lib:format("Error: ~p", [Reason]))    
%%     end;
event(addConversation) ->
    [ConversationName]=wf:q(conversationName),
		[Topic]=wf:q(topic),
    move_user_to_room (ConversationName, Topic);
event(changeConversation) ->
    [ConversationName]=wf:q(conversationName),
    move_user_to_room (ConversationName);
event(popupConversation) ->
    [ConversationName]=wf:q(conversationName),
    move_user_to_modal_room (ConversationName);
event(getTopics) ->
    {ok, Rooms} = db_interface:get_all_rooms(),
    {json, Rooms};
%% event(getTagCloud) ->
%%     Start=wf:q(starttime),
%%     End=wf:q(endtime),
%%     {json, db_interface:get_topics_for_tag_cloud(user_server:get_current_room_name(wf:session(proxy_pid)), Start, End)};	
event(pauseChatyeo) ->
    user_server:pause_chatyeo(wf:session(proxy_pid));
event(unpauseChatyeo) ->
    user_server:unpause_chatyeo(wf:session(proxy_pid)),
    update_discussing();
event(getRealtimeSearchResults) ->
    [Value] = wf:q(searchStr),
    Results = db_interface:search_chatyeo(Value),
    {json, Results};
event(searchYourConversationSuggester) ->
    [Value] = wf:q(value),
    Results = db_interface:convo_search(wf:user(), Value),
    {json, Results};
event(getCurrentItems) ->
    Results = user_server:get_current_grid_state(wf:session(proxy_pid)),
    {json, Results};
event(logout) ->
    ProxyPid = wf:session(proxy_pid),
    UserName = wf:user(),
    case ProxyPid of
        undefined ->
            wf:redirect ("login");
        _Other ->
            case is_process_alive(ProxyPid) of
                true ->
                    wf:logout(),
                    chatyeo_sup:jabber_logout (ProxyPid, UserName),
                    wf:redirect ("login");
                false ->
                    wf:logout(),
                    wf:redirect ("login")
            end
    end;
event(requestFriend) ->
    [UserName] = wf:q(userName),
    user_server:add_friend_request(wf:session(proxy_pid), UserName);
event(removeFriend) ->
    [UserName] = wf:q(userName),
    db_interface:remove_friend(wf:user(), UserName).

%% @spec send_message(string()) -> void()
%%
%% @doc Takes a string to be sent to the current room and passes
%% it on to the users jabber connected server to be sent.
send_message(Msg) ->
    user_server:send_muc_message(wf:session(proxy_pid), Msg).        

%% @spec send_modal_message(string()) -> void()
%%
%% @doc Takes a string to be sent to the current popup room and passes
%% it on to the users jabber connected server to be sent.
send_modal_message(Msg) ->
    user_server:send_modal_message(wf:session(proxy_pid), Msg).        

%% @spec move_user_to_modal_room(string()) -> void()
%%
%% @doc Takes te name of an item to move the user to its modal popup room.
move_user_to_modal_room(ConversationName) ->
    ConversationNameParsed = jabber_utils:parse_room_name(ConversationName),
    jabber_utils:create_room (ConversationNameParsed, "Modal", []),             
    user_server:move_user_to_modal_room (wf:session(proxy_pid), ConversationNameParsed).

%% @spec move_user_to_room(string()) -> void()
%%
%% @doc Takes the name of a room and moves the user to that room.
move_user_to_room(ConversationName) ->
    ConversationNameParsed = jabber_utils:parse_room_name(ConversationName),
    {ok, Cat} = db_interface:get_cat_for_room(ConversationNameParsed),        
    move_user_to_room(ConversationName, Cat).

%% @spec move_user_to_room(string(), string()) -> void()
%%
%% @doc Takes the name of a room and its category and moves them to that room.
move_user_to_room(ConversationName, Cat) ->
    ProxyPid = wf:session(proxy_pid),
    UserName = wf:user(),
    
    jabber_utils:create_room (ConversationName, Cat),             
    user_server:move_user_to_room (ProxyPid, ConversationName),

    {ok, Past} = db_interface:get_past_rooms (UserName),
    {ok, Future} = db_interface:get_future_rooms (UserName),

    update_discussing(),

    wf:wire("client.onRoomChange(\""++ConversationName++"\", "++Past++", "++Future++", "++num_users_as_string(ConversationName)++")"),

    user_server:populate_grid_with_lastest(ProxyPid).

%% @spec num_users_as_string(string()) -> string()
%%
%% @doc Takes the name of a room and returns the number of users in that room as a string.
num_users_as_string (Room) ->
    {ok, Num} = db_interface:number_of_users_in_room(jabber_utils:parse_room_name(Room)),
    io_lib:format("~p", [Num-1]).

%% @spec current_room_name() -> string()
%%
%% @doc Returns the name of the current room the user who is logged in is in.
current_room_name() ->
    jabber_utils:unparse_room_name(user_server:get_current_room_name(wf:session(proxy_pid))).

%% @spec update_discussing() -> void()
%%
%% @doc Updates the list of topics being talked about in the room on the page.
update_discussing() ->
    Topics = db_interface:get_topics_for_room(user_server:get_current_room_name(wf:session(proxy_pid))),
    Text = #p{body=
              "You're discussing: " ++ lists:flatmap(fun([X]) ->
                                                             binary_to_list(X) ++ ", "
                                                     end, Topics)++" ..."},
    wf:update(popads, Text).

%% @spec chatyeo_search() -> [json()]
%%
%% @doc Returns a json list of {id:RoomId, text:RoomName}
chatyeo_search () ->
    {ok, Rooms} = db_interface:get_all_rooms(),
    Rooms.
