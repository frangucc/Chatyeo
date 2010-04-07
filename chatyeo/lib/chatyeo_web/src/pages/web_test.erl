-module(web_test).
-include_lib("nitrogen/include/wf.inc").
-include_lib("config.hrl").
-include_lib("elements.inc").
-compile(export_all).
  
render_chat () ->
    Body=[     
               #chatbox {id=pane3}
              ],
    wf:render(Body).

render_chat_textarea () ->
    Body=[     
               #textarea { id=messageTextArea, class="message_textarea_input" }
              ],
    wf:wire(messageTextArea, #event {type=enterkey, postback=chat}),     
    wf:render(Body).

main() ->
   case wf:user() of
        undefined ->
            wf:redirect("/web/register");
        _ -> ok
    end,
    event(join),
    #template {file="./wwwroot/test.html"}.

 
event(chat) ->   
    UserName = wf:user(),
    [Message] = wf:q(messageTextArea),
    case length(Message) of
        0 -> ignore;
        _ ->
            case send_message(UserName, Message) of
                failed ->
                    wf:flash ("Failed to send message, try again");
                ok ->
                    sent
            end            
    end,
    wf:wire("obj('messageTextArea').focus(); obj('messageTextArea').select();"),
    wf:session(message_loop) ! {update_input, ""};

event(join) ->
    EmailAddress = db_interface:get_email_address(wf:user()),
    Pid = wf:comet(fun() -> listen_for_messages(EmailAddress) end),
    wf:session(message_loop, Pid),
    case wf:user() of
        undefined -> 
            ignore;
        _ -> 
            Server = list_to_atom (wf:user()),
            user_pid_server:update_pid(wf:user(), Pid),
            jabber_proxy:move_user_to_room (Server, "test_room"), 
            erlang:monitor(process, Pid)
    end;

event(simulate) ->
    wf:insert_bottom(chatboxfeed, #chatbox_message_wrapper{body="this is a test", email="kungfooguru@gmail.com"});
event(_) ->
    io:format ("~nWOOOOO~n").

listen_for_messages(EmailAddress) ->
     receive		
         {results, _Results} ->
             wf:comet_flush();
         {topic, Topic} ->
             Terms = [
                      #p{},
                      #span { text="bot_topic", class=username }, ": ",
                      #span { text=Topic, class=message }
                     ],
             wf:insert_bottom(metaChatHistory, Terms),
             wf:comet_flush();                     
         {message_from, RoomUser, Msg} ->
             [_To, UserName] = string:tokens(RoomUser, "/"),
             {From, Data} = Msg,
             case From of
                 "std_msg" ->
                     wf:insert_bottom(chatboxfeed, #chatbox_message_wrapper{body=Data, user=UserName, email=db_interface:get_email_address(UserName)}),
                     wf:wire("$pane2 = $('#page__pane3'); $('.scroll-pane').load('lipsum.html', '', reinitialiseScrollPane);"),

                     wf:comet_flush();
                 Unknown ->
                     ?INFO_MSG ("Got unknown message from ~s~n", [Unknown])
             end;
         {new_user, EmailAddress} ->
             io:format ("~nNew User: ~p~n", [EmailAddress]);
         {user_leaving, EmailAddress} ->
             io:format ("~nLeaving User: ~p~n", [EmailAddress]);
         {update_input, Msg} ->
             wf:wire(io_lib:format("Nitrogen.$set_value(obj('messageTextArea'),'~s')", [Msg])),
             wf:comet_flush();
         {highlight_input} ->
             wf:wire("obj('messageTextArea').focus(); obj('messageTextArea').select();");
         {connection_successful, UserName} ->
            jabber_proxy:move_user_to_room (list_to_atom (UserName), "test_room");
         Error ->
             ?ERROR_MSG ("Error: message loop got: ~p~n", [Error])
    end,
    listen_for_messages(EmailAddress).

send_message(UserName, Msg) ->
   jabber_proxy:send_muc_message(list_to_atom (UserName), Msg).        
