-module (web_auth).
-include_lib ("nitrogen/include/wf.inc").
-include_lib ("config.hrl").
-compile(export_all).

main() ->  
    case wf:user() of
        undefined -> 
            #template { file=nitrogen:get_wwwroot()++"/new_user_template.html" };
        _ ->             
            case is_process_alive(wf:session(proxy_pid)) of
                true -> 
                    wf:redirect("chat");
                false ->
                    #template { file=nitrogen:get_wwwroot()++"/new_user_template.html" }
            end
    end.

title() -> "Chatyeo".
headline() -> "Validation". 

body() -> 
    Cell1 = #tablecell { body = ["Login",
                                 #br{}] },
    Cell2a = #tablecell { body = ["Username: "] },
    Cell2b = #tablecell { body = [#textbox { id=username, postback=login }] },
    Cell3a = #tablecell { body = ["Password: "] },
    Cell3b = #tablecell { body = [#password { id=pass, postback=login }] },
   
    Row1 =  #tablerow {cells = [Cell1] },
    Row2 =  #tablerow {cells = [Cell2a,Cell2b] },
    Row3 =  #tablerow {cells = [Cell3a,Cell3b] },

    Body = [#table {rows=[Row1, Row2, Row3]},
            #button { id=submit, text="Login", postback=login },
            #br{},
            #p{},
            "Notice: This demo is for data gathering purposes only. Thanks for your help!"],
     
    wf:wire(submit, username, #validate { validators=[
                                                      #is_required { text="Required." }
                                                     ]}),
    
    wf:render(Body).

event (login) ->
    Username = hd(wf:q(username)),
    Password = hd(wf:q(pass)),
    
    case {Username, Password} of
        {"chatyeo", "temp4now"} ->
            wf:session(valid, 1),          
            wf:redirect("chat");
        _ ->
            wf:flash ("Incorrect")
    end;

event (_) -> 
    ok.
