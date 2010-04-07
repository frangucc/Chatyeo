-module(session).
-export([init/0,ensure/1,locate/1,create/2,touch/1]).

-record(chatyeo_session_cache,{sessionid,processid,dated}).



get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

store(SESSIONID,Pid)->
    Tran = fun()-> 
                   Dated = get_unix_timestamp(erlang:now()),
                   mnesia:write(#chatyeo_session_cache{sessionid=SESSIONID,processid=Pid, dated= Dated})
           end,
    mnesia:transaction(Tran).


init()->
    mnesia:create_schema([node()]),
    mnesia:start(),
    try
        mnesia:table_info(chatyeo_session_cache,type)
    catch
        exit: _ ->
            mnesia:create_table(chatyeo_session_cache,
               [{attributes, record_info(fields,chatyeo_session_cache)},
               {type,set},{ram_copies,[node()]}])
    end,
    ok.

recall_header([])-> undefined;
recall_header([H|_T])-> 
    {chatyeo_session_cache,_SESSIONID,ProcessID,_Dated}    = H,
    ProcessID.

recall(SESSIONID,MaxAgeInSeconds)-> 
    Tran = fun()->
                   Now = get_unix_timestamp(erlang:now()),
                   Oldest = Now - MaxAgeInSeconds,
                   Match = #chatyeo_session_cache{sessionid=SESSIONID,processid='$3',dated='$4'},
                   Guard = [{'>','$4',Oldest}],
                   Result= ['$_'],
                   MatchSpec = [{Match, Guard, Result}],
                   mnesia:select(chatyeo_session_cache,MatchSpec)
           end, 
    {atomic,Results} = mnesia:transaction(Tran), 
    io:format("~p~n",[Results]),
    recall_header(Results).


ensure([])-> wf:guid();
ensure(undefined)-> wf:guid();
ensure(SESSIONID)->SESSIONID.

touch(SESSIONID)->  
   Pid = recall(SESSIONID,9000),
   store(SESSIONID,Pid).    

create(SESSIONID,PID)->  
    store(SESSIONID,PID).    

locate(SESSIONID)-> 
  ID =  ensure(SESSIONID), 
  Pid = recall(ID,9000),
  touch(SESSIONID),
  Pid.
 
