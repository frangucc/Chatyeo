%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_docs).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-record(state, {session, room}).

init(Session) ->
    #state{session=Session}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(message, _Message, State) ->
    State;

update(topics, Topics, State) ->
    web_search (Topics, State);

update(result, {_Id, _Json}, State) ->
    State.

web_search (Topics, State) ->
    ?INFO_MSG("IN BOT_DOCS~n....................................................", []),
    lists:mapfoldl(fun(Topic, Row) -> 
                           UrlDoc = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s%20filetype:doc", [?GOOGLE_URL, ?WEB_TYPE, ?VERSION, ibrowse_lib:url_encode(Topic)])),
                           UrlPdf = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s%20filetype:pdf", [?GOOGLE_URL, ?WEB_TYPE, ?VERSION, ibrowse_lib:url_encode(Topic)])),
                           UrlXls = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s%20filetype:xls", [?GOOGLE_URL, ?WEB_TYPE, ?VERSION, ibrowse_lib:url_encode(Topic)])),
                           ?INFO_MSG("UrlDoc=~s~n",[UrlDoc]),
                           ?INFO_MSG("UrlPdf=~s~n",[UrlPdf]),
                           ?INFO_MSG("UrlXls=~s~n",[UrlXls]),
                           {ok, ResultsDoc, _} = rfc4627:decode(restful_querying:http_g_request(UrlDoc)),
                           {ok, ResultsPdf, _} = rfc4627:decode(restful_querying:http_g_request(UrlPdf)),
                           {ok, ResultsXls, _} = rfc4627:decode(restful_querying:http_g_request(UrlXls)),
                           web_parse (ResultsDoc, (Row rem 3) + 1, State),
                           web_parse (ResultsPdf, ((Row + 1) rem 3) + 1, State),
                           web_parse (ResultsXls, ((Row + 2) rem 3) + 1, State),
                           {[], Row+3}
                   end, 1, Topics). 

web_parse({obj,[{"responseData", {obj,[{"results", Results}, _]}}, _, _]}, Row, State) ->
    lists:mapfoldl (fun(X, Depth) ->
                            {ok, URL} = rfc4627:get_field(X, "url"),
                            {ok, Title} = rfc4627:get_field(X, "title"),
                            {ok, Content} = rfc4627:get_field(X, "content"),
                            Json = rfc4627:encode(bot_utils:make_json_docs(Row, Depth, URL, Title, Content)),  
                            XML = pubsub_util:build_results_xml(bot_utils:get_hash(Json), Json),
                            pubsub_util:publish(State#state.session, State#state.room, "results", XML),
                            {[], Depth+1}
                    end, 1, lists:sublist(Results,1)).
