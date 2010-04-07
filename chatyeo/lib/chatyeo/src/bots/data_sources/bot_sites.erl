%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_sites).

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
    lists:mapfoldl(fun(Topic, Row) -> 
                           Url = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s", [?GOOGLE_URL, ?WEB_TYPE, ?VERSION, ibrowse_lib:url_encode(Topic)])),
                           {ok, Results, _} = rfc4627:decode(restful_querying:http_g_request(Url)),
                           web_parse (Results, Row, State),
                           {[], Row+1}
                   end, 1, Topics). 

web_parse({obj,[{"responseData", {obj,[{"results", Results}, _]}}, _, _]}, Row, State) ->
    lists:mapfoldl (fun(X, Depth) ->
                            {ok, URL} = rfc4627:get_field(X, "url"),
                            {ok, Title} = rfc4627:get_field(X, "title"),
                            {ok, Content} = rfc4627:get_field(X, "content"),
                            Json = rfc4627:encode(bot_utils:make_json_sites(Row, Depth, URL, Title, Content)),  
                            XML = pubsub_util:build_results_xml(bot_utils:get_hash(Json), Json),
                            pubsub_util:publish(State#state.session, State#state.room, "results", XML),
                            {[], Depth+1}
                    end, 1, lists:sublist(Results,1)).
