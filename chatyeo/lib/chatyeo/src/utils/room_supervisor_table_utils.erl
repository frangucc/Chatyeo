%%%-------------------------------------------------------------------
%%% File    : room_supervisor_table_utils.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 29 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(room_supervisor_table_utils).
-export([get_room_supervisor/1, add_room_supervisor/2]).

get_room_supervisor (Room) ->
    case ets:lookup (room_supervisor_table, Room) of
        [{Room, RS}] ->            
            {ok, RS};
        [] ->
            undefined
    end.

add_room_supervisor (Room, EM) ->
    ets:insert (room_supervisor_table, {Room, EM}).
