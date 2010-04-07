-module(results_cache).
-include_lib("stdlib/include/qlc.hrl").
-record(xml_results_cache,{botname,class,qry,xml,dated}).
-export([init/0,store/4,recall/4]).

get_unix_timestamp(TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
        calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).

%% @doc Store result list into cache
%% @spec store(BotName::string(), ClassName::string(), Query::string(), List::list()) -> ok

store(BotName,ClassName,Query,List)->
    Tran = fun()-> 
                   Dated = get_unix_timestamp(erlang:now()),
                   mnesia:write(#xml_results_cache{botname=BotName,class=ClassName, qry=Query, xml= List, dated= Dated})
           end,
    mnesia:transaction(Tran).


recall_header([])-> "";
recall_header({atomic,[H|_T]})-> 
    {xml_results_cache,_BotName,_ClassName,_Query,XML,_Dated}    = H,
    XML;
recall_header(_generic) -> "".

%% @doc Recall result if it is within max age
%% @spec recall(BotName::string(), ClassName::string(), Query::string(), MaxAgeInSeconds::integer()) -> Result
%% Result = string()

recall(BotName,ClassName,Query,MaxAgeInSeconds)-> 
    Tran = fun()->
                   Now = get_unix_timestamp(erlang:now()),
                   Oldest = Now - MaxAgeInSeconds,
                   Match = #xml_results_cache{botname=BotName,class=ClassName,qry=Query,xml='$3',dated='$4'},
                   Guard = [{'>','$4',Oldest}],
                   Result= ['$_'],
                   MatchSpec = [{Match, Guard, Result}],
                   mnesia:select(xml_results_cache,MatchSpec)
           end,
    Results = mnesia:transaction(Tran), 
    recall_header(Results).

%% @doc Initialize results_cache data, must be called before any other function
%% @spec init() -> ok

init()->
    try
        mnesia:table_info(xml_results_cache,type)
    catch
        exit: _ ->
            mnesia:create_table(xml_results_cache,[{attributes, record_info(fields,xml_results_cache)}, {type,bag},{disc_copies,[node()]}])
    end,
    ok.
