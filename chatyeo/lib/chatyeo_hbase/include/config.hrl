-ifndef(chatyeo_hbase_config_hrl).
-define(chatyeo_hbase_config_hrl, ok).

-define(HBASE_USER_TABLE, "users").
-define(HBASE_ROOM_TABLE, "rooms").
-define(HBASE_MESSAGES_TABLE, "messages").

-define(ERROR_MSG(Format, Args),                
        error_logger:error_msg ("E(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).
                                                                                                        
-define(INFO_MSG(Format, Args),
        error_logger:info_msg ("I(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).

-endif.
