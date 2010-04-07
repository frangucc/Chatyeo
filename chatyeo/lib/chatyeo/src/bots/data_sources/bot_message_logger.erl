%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_message_logger).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-record(state, {session, room}).

init(Session) ->
    #state{session=Session}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(message, Message, State) ->
    {ok, JSON, _} = rfc4627:decode(Message),
    {ok, User} = rfc4627:get_field(JSON, "username"),
    db_interface:insert_message (State#state.room, User, 1, Message),
    State;

update(topics, _Topics, State) ->
    State;

update(result, _Results, State) ->
    State.
