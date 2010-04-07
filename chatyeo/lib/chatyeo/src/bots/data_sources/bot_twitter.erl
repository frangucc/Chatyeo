%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_twitter).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-record(state, {session, room}).

init(Session) ->
    #state{session=Session}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(message, _Message, State) ->
    State;

update(result, {_Id, _Json}, State) ->
    State;

update(topics, Topics, State) ->    
    {List, _} = lists:mapfoldl(fun(Topic, Row) ->
                                       {twitter:search(Topic, Row, 1), Row+1}
                               end, 1, Topics),
    
    Results = lists:flatten(List),
    
    lists:foreach(fun(X) ->
                          Json = rfc4627:encode(X),                                
                          XML = pubsub_util:build_results_xml(bot_utils:get_hash(Json), Json),
                          pubsub_util:publish(State#state.session, State#state.room, "results", XML)
                  end, Results).
