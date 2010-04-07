-module(web_requests).
-include_lib("nitrogen/include/wf.inc").
-include_lib("nitrogen/include/element_interface.inc").
-include("config.hrl").
-compile(export_all).

title() -> "Chatyeo Friend Requests". 

main() ->
    case wf:user() of
        undefined ->         
            wf:redirect("/web/login");
        _ ->
            #template { file=nitrogen:get_wwwroot()++"/new_user_template.html" }
    end.

headline() -> "Validation". 

body() -> 
    FriendRequests = db_interface:get_all_friend_requests(wf:user()),

    Body = lists:map(fun([UserName]) ->
                             List = binary_to_list(UserName),
                             #panel {id=UserName, body=[#br{},
                                                        #label{text=List},
                                                        #button { text="Accept", postback={accept, List} },
                                                        #button { text="Deny", postback={deny, List} }]}
                     end, FriendRequests),

    io:format ("~nBODY ~p~n", [hd(Body)]),

    wf:render(Body).

event({accept, UserName}) ->
    wf:wire(UserName, #hide { effect=blind, speed=250 }),
    db_interface:accept_friend_request(wf:user(), UserName);
event({deny, UserName}) ->
    wf:wire(UserName, #hide { effect=blind, speed=250 }),
    db_interface:deny_friend_request(wf:user(), UserName).
