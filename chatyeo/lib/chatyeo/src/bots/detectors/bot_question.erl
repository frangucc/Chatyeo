%%%-------------------------------------------------------------------
%%% @author Jordan Wilberding
%%% @copyright (C) 2009, Jordan Wilberding
%%% @doc Detect when a proper question is asked
%%%              
%%% @end
%%% Created : 15 Sep 2009 by Jordan Wilberding
%%%-------------------------------------------------------------------
-module(bot_question).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-record(state, {session, room, regex}).

init(Session) ->
    {ok, QuestionRegExp} = re:compile("^.*\\?$", [caseless]),
    #state{session=Session, regex=QuestionRegExp}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(message, Message, State) ->
    {ok, Json, _} = rfc4627:decode(Message),
    {ok, Body} = rfc4627:get_field(Json, "body"),
    case is_question(Body, State) of
        true ->
            ?INFO_MSG("question: ~p~n", [Body]),
            XML = pubsub_util:build_question_xml(Body),
            pubsub_util:publish(State#state.session, State#state.room, "questions", XML);
        false ->
            do_nothing
    end,
    State.
    
is_question(Body, State) ->
    QuestionRegExp = State#state.regex,
    Results = re:run(Body, QuestionRegExp, [global]),
    case Results of
        {match, _List} ->
            true;
        nomatch ->
            false
    end.
