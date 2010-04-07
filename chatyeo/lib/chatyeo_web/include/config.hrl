-ifndef(chatyeo_web_config_hrl).
-define(chatyeo_web_config_hrl, ok).

-define(PASSWORD, "temp4now").

-define (ERROR_MSG(Format, Args),                
         error_logger:error_msg ("E(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).
                                                                         
-define (INFO_MSG(Format, Args),
	       error_logger:info_msg ("I(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).

-record(users, { username, email_address, password, date_joined, last_logged_in, verified=false }).
-record(verification_codes, { email_address, verification_code }).
 
-endif.
