%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_youtube_videos).

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
    video_search (Topics, State);

update(result, {_Id, _Json}, State) ->
    State.

video_search (Topics, State) ->
    lists:mapfoldl(fun(Topic, Row) -> 
                           Url = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s", [?GOOGLE_URL, ?VIDEO_TYPE, ?VERSION, ibrowse_lib:url_encode(Topic)])),
                           {ok, Results, _} = rfc4627:decode(restful_querying:http_g_request(Url)),
                           video_parse (Results, Row, State), 
                           {[], Row+1}
                   end, 1, Topics).

video_parse({obj,[{"responseData", {obj,[{"results", Results}, _]}}, _, _]}, Row, State) ->
    lists:mapfoldl(fun(X, Depth) ->
                           {ok, URL} = rfc4627:get_field(X, "url"),
                           {ok, Image} = rfc4627:get_field(X, "tbUrl"),
                           {ok, Title} = rfc4627:get_field(X, "title"),
                           Json = rfc4627:encode(bot_utils:make_json_google_videos(Row, Depth, URL, Image, Title)),
                           XML = pubsub_util:build_results_xml(bot_utils:get_hash(Json), Json),
                           pubsub_util:publish(State#state.session, State#state.room, "results", XML),
                           {[], Depth+1}
                   end, 1, lists:sublist(Results,1)).
