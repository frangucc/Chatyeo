%%%-------------------------------------------------------------------
%%% File    : google_suggest.erl
%%% Author  : Jay<jdmundrawala@gmail.com>
%%% Description : 
%%%
%%% Created : 11 September 2009
%%%-------------------------------------------------------------------
%%% Example Usage:
%%%
%%%

-module(google_suggest).
-export([init/0,
        get_suggestions/1
    ]).

-define(BASE, "http://google.com/complete/search?q=").
-define(SUGGEST_TXT_XPATH, "//CompleteSuggestion/suggestion/@data").

init() ->
    ibrowse:start().

%%-------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------
get_suggestions(Query) ->
    Url = ?BASE++ibrowse_lib:url_encode(Query)++"&output=toolbar",
    {ok, _S, _ResponseHeader, Results} = ibrowse:send_req(Url,[], get),
    {Xml, []} = xmerl_scan:string(Results),
    Suggestions = xmerl_xpath:string(?SUGGEST_TXT_XPATH, Xml),
    lists:map(fun({_,_,_,_,_,_,_,_,Text,_}) -> Text end, Suggestions).
