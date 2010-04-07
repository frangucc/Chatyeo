-ifndef(chatyeo_db_config_hrl).
-define(chatyeo_db_config_hrl, ok).

-define(ERROR_MSG(Format, Args),                
        error_logger:error_msg ("E(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).
                                                                                                        
-define(INFO_MSG(Format, Args),
        error_logger:info_msg ("I(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).

-endif.
