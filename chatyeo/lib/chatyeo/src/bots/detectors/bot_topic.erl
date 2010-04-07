%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2009, Tristan
%%% @doc 
%%%              
%%% @end
%%% Created : 10 Aug 2009 by Jay
%%%-------------------------------------------------------------------
-module(bot_topic).

-export([init/1, moving_to_room/2, update/3]).

-include("config.hrl").
-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-define(TOPIC_NS, "http://www.chatyeo.com/2009/Topics").


-record(state, {session, window=[], room, room_id}).

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
    {ok, RegEx} = re:compile("\\bhttp\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,4}(/\\S*)?", [caseless]),
    Results = re:run(Body, RegEx, [global]),
    case Results of
        {match, _List} -> State;
        _ ->
            if 
                length(State#state.window) == 2 ->
                    [Msg1, Msg2] = State#state.window,
                    Msgs = [Msg1, Msg2, {Username, Body}],
                    ?INFO_MSG("topic msgs: ~p~n", [Msgs]),
                    Reply = topic_detector:query_string(State#state.room_id, Msgs),
                    ?INFO_MSG("topic reply: ~p~n", [Reply]),
                    case Reply of
                        [] ->
                            State#state{window=[]};
                        ignore ->
                            State#state{window=[{Username, Body}]};
                        Topics ->
                            SubTopics = lists:sublist(Topics, 3),
                            XML = build_topics_xml(SubTopics),
                            pubsub_util:publish(State#state.session, State#state.room, "topics", XML),
                            db_interface:add_topics_for_room(SubTopics, State#state.room, State#state.room_id),
                            State#state{window=[]}
                    end;
                true ->
                    State#state{window=[{Username, Body} | State#state.window]}
            end
    end.

build_topics_xml(Topics) ->
    exmpp_xml:element(?TOPIC_NS, topics, [],
                      lists:map(fun(Topic) ->
                                        exmpp_xml:element(?TOPIC_NS, topic, [],
                                                          [exmpp_xml:cdata(element(2, Topic))])
                                end, Topics)).
