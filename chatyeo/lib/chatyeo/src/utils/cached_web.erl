-module(cached_web).
-export([fetch/5]).  

http_return({ error, _IgnoreME})-> [];
http_return({ ok, {_Status, _Headers, Body }})-> Body;
http_return(_)-> [].

raw_query_web(BotName,ClassName,Query,URL)->
   % io:format("~p~n",[URL]),
    REQ = http:request(URL),
    Body = http_return(REQ),
    results_cache:store(BotName,ClassName,Query,Body),
    Body.     
  
find_web_xml(BotName,ClassName,Query,URL,[])->raw_query_web(BotName,ClassName,Query,URL);
find_web_xml(_BotName,_ClassName,_Query,_URL,Cached)-> Cached.
     
%% @doc Fetch a webpage, if it is in cache, it returns cached page, otherwise downloads page first
%% @spec fetch(BotName::string(), ClassName::string(), Query::string(), URL::string(), MaxAgeInSeconds::integer()) -> string()

fetch(_BotName,_ClassName,_Query,[],_MaxAgeInSeconds)-> "";
fetch(_BotName,_ClassName,_Query,[[]],_MaxAgeInSeconds)-> "";
fetch(BotName,ClassName,Query,URL,MaxAgeInSeconds)-> 
    XML = find_web_xml(BotName,ClassName,Query,URL,results_cache:recall(BotName,ClassName,Query,MaxAgeInSeconds)),
    XML.

 
