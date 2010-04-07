%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_yahoo_answers).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-record(state, {session, room}).

init(Session) ->
    #state{session=Session}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(message, _Message, State) ->
    State;

update(question, Question, State) ->
    ?INFO_MSG("bot yahoo answers got a question: ~p~n", [Question]),
    Results = yahoo:search(Question, 1),
    
    lists:foreach(fun(X) ->
                          pubsub_util:publish(State#state.session, State#state.room, "results", pubsub_util:build_results_xml(bot_util:get_hash(X), X))
                  end, Results);

update(result, {_Id, _Json}, State) ->
    State.
