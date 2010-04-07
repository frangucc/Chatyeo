%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_storage).


-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-record(state, {session, room}).

init(Session) ->
    #state{session=Session}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(topics, _, _) ->
    ok;

update(message, _Message, State) ->
    State;

update(result, {Id, Json}, State) ->
    %io:format ("~n~p~n", [Id]),
    storage:add_item(State#state.room, Id, Json),    
    State.
