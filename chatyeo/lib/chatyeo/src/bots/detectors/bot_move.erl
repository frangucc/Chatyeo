%%%-------------------------------------------------------------------
%%% File    : bot_move.erl
%%% Author  : 
%%% Description : 
%%%
%%% Created : 
%%%-------------------------------------------------------------------
-module(bot_move).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").

-define(ROOM_CHANGE_NS, "http://www.chatyeo.com/2009/RoomChange").

-record(state, {session, room, room_id}).

init(Session) ->
    #state{session=Session}.

moving_to_room(Room, State) ->
    {ok, RoomId} = db_interface:get_room_id(Room),
    State#state{room=Room, room_id=RoomId}.

update(topics, _, _) ->
    ok;

update(message, Message, State) ->
    {ok, Json, _} = rfc4627:decode(Message),
    {ok, Body} = rfc4627:get_field(Json, "body"),
    {ok, Username} = rfc4627:get_field(Json, "username"),
    case check_move(Username, Body, State#state.room_id) of                    
        {ok, Channel} ->
            jabber_utils:send_message(State#state.session, Username, 
                                      move_room_xml(Channel));
        _ ->
            nothing
    end,
    State.

move_room_xml(Room) ->
    exmpp_xml:element(undefined, move_room, [],
                      [exmpp_xml:element(undefined, room, [], [exmpp_xml:cdata(Room)])]).

check_move(_Username, Body, CurrentRoomId) ->
    {ok, Topics} = topic_detector:detect_topics(Body),
    case Topics of
        [{_WikiId, MainTopic, _Weight, _RTC, _RTO, _Generality} | _] ->
            {ok, Results} = db_interface:check_for_room_on_topic(CurrentRoomId, MainTopic),
            case Results of
                [[Room]|_] ->
                    {ok, binary_to_list(Room)};
                _ ->
                    none
            end;
        _ ->
            none
    end.

