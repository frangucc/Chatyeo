%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 21 Sep 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(pubsub_util).

-compile(export_all).

-include("config.hrl").

-define(NODE_TYPES, ["results", "topics", "questions"]).

%% @spec create_room_nodes(#exmpp_session(), string()) -> void()
%%
%% @doc Creates the necessary pubsub nodes for a room 
create_room_nodes(Session, Room) ->
    %% Create base node
    DeletePacket1 = exmpp_client_pubsub:delete_node(?JABBER_PUBSUB_HOST, get_node(Room,"")),
    exmpp_session:send_packet(Session, DeletePacket1),
    
    BasePacket = exmpp_client_pubsub:create_node(?JABBER_PUBSUB_HOST, get_node(Room, "")),    
    exmpp_session:send_packet(Session, BasePacket),
    
    lists:foreach(fun(X) ->
                          DeletePacket = exmpp_client_pubsub:delete_node(?JABBER_PUBSUB_HOST, get_node(Room, X)),
                          exmpp_session:send_packet(Session, DeletePacket),
                          Packet = exmpp_client_pubsub:create_node(?JABBER_PUBSUB_HOST, get_node(Room, X)),
                          exmpp_session:send_packet(Session, Packet)
                  end, ?NODE_TYPES).

%% @spec subscribe_to_node(#exmpp_session(), string(), string()) -> void()
%%
%% @doc Subscribes an active user to a pubsub node
subscribe(Session, Jid, Room, NodeType) ->
    exmpp_session:send_packet(Session, exmpp_client_pubsub:subscribe(Jid, ?JABBER_PUBSUB_HOST, get_node(Room, NodeType))).

%% @spec unsubscribe_from_node(#exmpp_session(), string(), string()) -> void()
%%
%% @doc Unsubscribes an active user to a pubsub node
unsubscribe(Session, Jid, Room, NodeType) ->
    exmpp_session:send_packet(Session, exmpp_client_pubsub:unsubscribe(Jid, ?JABBER_PUBSUB_HOST, get_node(Room, NodeType))).

%% @spec publish_to_node(#exmpp_session(), string(), string()) -> void()
%%
%% @doc Publishes content to a pubsub node
publish(Session, Room, NodeType, Contents) ->
    exmpp_session:send_packet(Session, exmpp_client_pubsub:publish(?JABBER_PUBSUB_HOST, get_node(Room, NodeType), Contents)).

get_node(Room, NodeType) ->
    "/home/"++?JABBER_HOST++"/"++Room++"/"++NodeType++"/".

%% @spec results_node(string) -> string()
%%
%% @doc Returns the node string for the results node of a room
results_node(Room) ->
    get_node(Room, "results").

%% @spec topics_node(string) -> string()
%%
%% @doc Returns the node string for the topics node of a room
topics_node(Room) ->
    get_node(Room, "topics").

build_results_xml(Id, Json) ->
    exmpp_xml:element(?RESULT_NS, result, [],
                      [exmpp_xml:element(?RESULT_NS, id, [], [exmpp_xml:cdata(Id)]),
                       exmpp_xml:element(?RESULT_NS, json, [],
                                         [exmpp_xml:cdata(Json)])]).

build_question_xml(Question) ->
    exmpp_xml:element(?QUESTION_NS, question, [], [exmpp_xml:cdata(Question)]).
