%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(db_interface).

%% API
-compile(export_all).

-include_lib("nitrogen/include/wf.inc").

init() ->
    mysql:prepare(add_tag_query, 
                  <<"INSERT INTO tags (tag) VALUES (?)">>),

    mysql:prepare(add_topic_query, 
                  <<"INSERT IGNORE INTO topics (topic) VALUES (?)">>),
    
    mysql:prepare(add_topic_for_room_query,
                  <<"INSERT INTO topic_room (topic_id, room_id, timestamp) VALUES ((select id from topics where topic=?), ?, unix_timestamp()) ON DUPLICATE KEY UPDATE timestamp=unix_timestamp()">>),
    
    mysql:prepare(add_room_query, 
                  <<"INSERT INTO rooms (room, category) VALUES (?, ?)">>),

    mysql:prepare(room_has_started_query, 
                  <<"UPDATE rooms WHERE room=? SET has_started=1">>),

    mysql:prepare(has_room_started_query, 
                  <<"SELECT has_started FROM rooms WHERE room=?">>),
    
    mysql:prepare(add_item_query, 
                  <<"INSERT INTO items (guid, type, page) VALUES (uuid(), ?, ?)">>),

    mysql:prepare(create_table_query, 
                  <<"CREATE TABLE ?">>),
  
    mysql:prepare(insert_message_query, 
                  <<"INSERT INTO messages (user, who_from, time, type, message, message_body) VALUES (?, ?, unix_timestamp(), ?, ?, ?)">>),

    mysql:prepare(get_messages_from_query,
                  <<"SELECT messages.message FROM messages, rooms WHERE messages.user=rooms.id AND rooms.room=? AND messages.time>? ORDER BY messages.time">>),

    mysql:prepare(get_messages_from_to_query,
                  <<"SELECT messages.message FROM messages, rooms WHERE messages.user=rooms.id AND rooms.room=? AND messages.time>=? AND messages.time<=? ORDER BY messages.time">>),

    mysql:prepare(get_topics_for_tag_cloud_query,
                  <<"SELECT topics.id, topics.topic, count(topic_room.topic_id) FROM topics, topic_room, rooms WHERE topic_room.timestamp > ? and topic_room.timestamp < ? AND rooms.room=? AND topic_room.room_id=rooms.id AND topics.id=topic_room.topic_id GROUP BY topic_room.topic_id">>),

    mysql:prepare(get_topics_for_tag_cloud_query_2,
                  <<"SELECT topics.id, topics.topic, count(topic_room.topic_id) FROM topics, topic_room, rooms, (SELECT min(timestamp)+(max(timestamp)-min(timestamp)) as y FROM topic_room) AS x WHERE topic_room.timestamp > ((x.y) * (?/100)) and topic_room.timestamp < ((x.y) * (?/100)) AND rooms.room=? AND topic_room.room_id=rooms.id AND topics.id=topic_room.topic_id GROUP BY topic_room.topic_id">>),

    mysql:prepare(get_topics_for_room_query,
                  <<"SELECT DISTINCT topics.topic FROM topics, topic_room, rooms WHERE rooms.room=? AND topic_room.room_id=rooms.id AND topics.id=topic_room.topic_id ORDER BY topic_room.timestamp DESC LIMIT 5">>),
    
    mysql:prepare(add_user_query, 
                  <<"INSERT INTO users (username, email, fullname, password, date_joined, last_logged_in) VALUES (?, ?, ?, PASSWORD(?), DATE(NOW()), NOW())">>),

    mysql:prepare(change_anon_username_query, 
                  <<"UPDATE users SET anon_username=? WHERE username=?">>),
    
    mysql:prepare(validate_user_query, 
                  <<"SELECT 1 FROM users WHERE (username=? OR email=?) AND password=PASSWORD(?)">>),

    mysql:prepare(update_last_logged_in_query, 
                  <<"UPDATE users SET last_logged_in=datetime()">>),

    %%%
    %%% Need to remove from friends and other tables....
    %%%
    mysql:prepare(delete_user_query, 
                  <<"DELETE FROM users WHERE username=?">>),
    
    mysql:prepare(is_username_used_query, 
                  <<"SELECT 1 FROM users WHERE username=?">>),
    
    mysql:prepare(is_email_used_query, 
                  <<"SELECT 1 FROM users WHERE email_address=?">>),
    
    mysql:prepare(get_email_address_query, 
                  <<"SELECT email FROM users WHERE username=?">>),

    mysql:prepare(get_user_id_query, 
                  <<"SELECT id FROM users WHERE username=? OR anon_username=?">>),

    mysql:prepare(get_room_id_query, 
                  <<"SELECT id FROM rooms WHERE room=?">>),

    mysql:prepare(get_category_id_query, 
                  <<"SELECT id FROM categories WHERE category=?">>),

    mysql:prepare(get_cat_for_room_query,
                  <<"SELECT categories.category  FROM rooms, categories WHERE rooms.room=? AND rooms.category=categories.id">>),

    mysql:prepare(add_friend_query,
                  <<"INSERT INTO friends (user_id, friend_id) VALUES (?, ?)">>),

    mysql:prepare(remove_friend_query,
                  <<"DELETE FROM friends WHERE user_id=? AND friend_id=?">>),

    mysql:prepare(get_friends_query,
                  <<"SELECT friend_id FROM friends, users WHERE friends.user_id=users.id AND users.username=?">>),

    mysql:prepare(get_users_in_room_query,
                  <<"SELECT user_id FROM user_room, rooms WHERE user_room.room_id=rooms.id AND rooms.room=?">>),

    mysql:prepare(get_rooms_user_in_query,
                  <<"SELECT room_id FROM user_room, users WHERE user_room.user_id=users.id AND users.username=?">>), 

    mysql:prepare(add_user_in_room_query,
                  <<"INSERT INTO user_room (user_id, room_id, last_entered, in_currently) VALUES (?, ?, unix_timestamp(), TRUE)">>),

    mysql:prepare(add_future_room_query,
                  <<"INSERT INTO user_room (user_id, room_id, in_currently) VALUES (?, ?, FALSE)">>),

    mysql:prepare(cleanup_user_room_query,
                  <<"UPDATE user_room SET in_currently=0">>),

    mysql:prepare(cleanup_rooms_query,
                  <<"UPDATE rooms SET has_started=0">>),    

    mysql:prepare(update_user_in_room_query,
                  <<"UPDATE user_room SET last_entered=unix_timestamp(), in_currently=TRUE WHERE user_id=? and room_id=?">>),

    mysql:prepare(check_user_in_room_query, 
                  <<"SELECT 1 FROM user_room WHERE user_id=? AND room_id=?">>),       
    
    mysql:prepare(user_leaving_room_query,
                  <<"UPDATE user_room SET in_currently=FALSE WHERE user_id=? and room_id=?">>),

    mysql:prepare(number_of_users_in_room_query,
                  <<"SELECT COUNT(*) FROM user_room, rooms WHERE user_room.room_id=rooms.id AND rooms.room=? AND in_currently=1">>),       

    mysql:prepare(how_active_is_room_query,
                  <<"SELECT unix_timestamp()-(max(messages.time))<3600, unix_timestamp()-(max(messages.time))<86400 FROM messages, rooms WHERE messages.user=rooms.id and rooms.room=?">>),

    mysql:prepare(get_matching_rooms_query,
                  <<"SELECT room FROM rooms WHERE id != ? AND room regexp ?">>),

    mysql:prepare(check_for_room_on_topic_query,
                  <<"SELECT rooms.room FROM rooms, topics, topic_room WHERE rooms.id!=? AND topics.topic=? AND topic_room.topic_id=topics.id AND rooms.id=topic_room.room_id">>),
    
    mysql:prepare(get_all_rooms_query,
                  <<"SELECT room FROM rooms, categories WHERE categories.category!=\"Modal\" AND rooms.category=categories.id">>),

    mysql:prepare(get_past_rooms_query, 
                  <<"SELECT rooms.room, categories.category, categories.image_url FROM user_room, users, rooms, categories WHERE user_room.user_id=users.id AND user_room.room_id=rooms.id AND users.username=? AND in_currently=0 AND categories.category!=\"Modal\" AND user_room.last_entered IS NOT NULL AND rooms.category=categories.id ORDER BY last_entered DESC LIMIT 10">>),

    mysql:prepare(get_future_rooms_query, 
                  <<"SELECT rooms.room, categories.category, categories.image_url FROM user_room, users, rooms, categories WHERE user_room.user_id=users.id AND user_room.room_id=rooms.id AND users.username=? AND in_currently=0 AND categories.category!=\"Modal\" AND user_room.last_entered IS NULL AND rooms.category=categories.id ORDER BY last_entered DESC LIMIT 10">>),
    
   mysql:prepare(get_last_room_query, 
                  <<"SELECT rooms.room FROM user_room, users, rooms, categories WHERE user_room.user_id=users.id AND user_room.room_id=rooms.id AND categories.category=\"Modal\" AND rooms.category!=categories.id AND users.username=? ORDER BY last_entered DESC LIMIT 1">>),

   mysql:prepare(add_friend_request_query, 
                  <<"INSERT IGNORE INTO pending_friends (user_id, pending_friend_id) VALUES (?, ?)">>),

   mysql:prepare(delete_friend_request_query, 
                  <<"DELETE FROM pending_friends WHERE user_id=? AND pending_friend_id=?">>),

    mysql:prepare(get_all_friend_requests_query, 
                  <<"SELECT requester.username FROM pending_friends, users user, users requester WHERE pending_friends.pending_friend_id=user.id AND user.username=? AND requester.id=pending_friends.user_id">>),

    mysql:prepare(convo_search_query,
                  <<"SELECT messages.id, categories.category, messages.message_body, categories.class, popularity.image_html, rooms.room FROM users, messages, messages_index, categories, rooms, popularity WHERE messages_index.message=? AND messages.id=messages_index.id AND users.username=? AND messages.who_from=users.id AND rooms.id=messages.user AND categories.id=rooms.category AND popularity=1">>),

    mysql:prepare(room_search_query,
                  <<"SELECT messages.id, categories.category, messages.message_body, categories.class, popularity.image_html, rooms.room FROM users, messages, messages_index, categories, rooms, popularity WHERE messages_index.message=? AND messages.id=messages_index.id AND rooms.room=? AND messages.user=rooms.id AND categories.id=rooms.category AND popularity=1">>),
    
    mysql:prepare(search_chatyeo_query,
                  <<"SELECT messages.id, users.username, users.email, messages.message_body, categories.class, popularity.image_html, rooms.room FROM users, messages, messages_index, categories, rooms, popularity WHERE messages_index.message=? AND messages.id=messages_index.id AND messages.who_from=users.id AND rooms.id=messages.user AND categories.id=rooms.category AND popularity=1">>).
 
add_tag(Tag) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, add_tag_query, [Tag])
                      end).

add_topics_for_room(Topics, Room, RoomId) ->
    mysql:transaction(p1,
                      fun() ->
                              lists:foreach(fun({_,Topic,_,_,_,_}) ->
                                                    mysql:execute(p1, add_topic_query, [Topic]),
                                                    mysql:execute(p1, add_topic_for_room_query, [Topic, RoomId]),
                                                    chatyeo_hbase:add_room_topic(Room, Topic)
                                            end, Topics)
                      end).
 
add_room(Room, Category) when is_number(Category) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, add_room_query, [Room, Category])
                      end);
add_room(Room, Category) ->
    mysql:transaction(p1,
                      fun() ->
                              {ok, CategoryID} = get_category_id (Category),
                              mysql:execute(p1, add_room_query, [Room, CategoryID])
                      end).

room_has_started(Room) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, room_has_started_query, [Room])
                      end).

has_room_started(Room) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, has_room_started_query, [Room])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            if
                hd(hd(Rows)) == 1 -> true;
                true -> false
            end;
        _ ->
            false
    end.

add_item(Type, Page) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, add_item_query, [Type, Page])                     
                      end).

cleanup_user_room() ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, cleanup_user_room_query, [])                     
                      end).    

cleanup_rooms() ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, cleanup_rooms_query, [])                     
                      end).    

create_table(Name) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, create_table_query, [Name])
                      end).

insert_message(Room, Who, Type, Msg) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    {ok, MessageBody} = get_message_body(Msg),
    mysql:transaction(p1,
                      fun() ->
                              {ok, UserID} = get_user_id (Who),
                              if
                                  Type == 1 ->
                                      {ok, RoomID} = get_room_id (RoomParsed);
                                  true ->
                                      {ok, RoomID} = get_user_id (RoomParsed)
                              end,
                              
                              mysql:execute(p1, insert_message_query, [RoomID, UserID, Type, Msg, MessageBody])
                      end).

add_user(Username, FullName, Email, Password) ->
    case mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, add_user_query, [Username, Email, FullName, Password])
                      end) of
        {atomic, {updated, _}} ->
            ok;
        {aborted, {{error, {mysql_result,[],[],0,0, Reason}}, _}} ->
            {aborted, Reason}
    end.

change_anon_username(UserName, NewUserName) ->
    case mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, change_anon_username_query, [NewUserName, UserName])
                      end) of
        {atomic, {updated, _}} ->
            ok;
        {aborted, {{error, {mysql_result,[],[],0,0, Reason}}, _}} ->
            {aborted, Reason}
    end.

validate_user(Username, Password) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, validate_user_query, [Username, Username, Password])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if 
                length (Rows) == 1 -> 
                    {ok, valid};
                true -> 
                    {error, false}
            end;
        _ ->
            {error, false}
    end.    

update_last_logged_in(Username) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, update_last_logged_query, [Username])
                      end).

delete_user(Username) ->
    mysql:transaction(p1,
                      fun() ->
                              mysql:execute(p1, delete_user_query, [Username])
                      end).

is_username_used(Username) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, is_username_used_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if 
                length (Rows) == 1 ->            
                    false;
                true -> 
                    true
            end;
        _ ->
            true
    end.

is_email_used(Email) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, is_email_used_query, [Email])
                           end) of
        {atomic, {data, MySQLResults}} -> 
            Rows = mysql:get_result_rows(MySQLResults),
            if
                length (Rows) == 1 -> 
                    false;
                true -> 
                    true
            end;
        _ ->
            true
    end.

get_email_address(Username) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_email_address_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if
                length(Rows) == 1 ->
                   {ok, hd(hd(Rows))};
                true ->
                    {error, unknown_user}
            end;
        _ ->
            {error, unknown_user}
    end.

add_user_in_room(User, Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    mysql:transaction(p1,
                      fun() ->
                              {ok, UserId} = get_user_id(User),
                              {ok, RoomId} = get_room_id(RoomParsed),
                              {data, MySQLResults} = mysql:execute (p1, check_user_in_room_query, [UserId, RoomId]),
                              Rows = mysql:get_result_rows(MySQLResults),
                              if    
                                  length(Rows) == 1 -> 
                                      mysql:execute(p1, update_user_in_room_query, [UserId, RoomId]);
                                  true -> 
                                      mysql:execute(p1, add_user_in_room_query, [UserId, RoomId])
                              end                                                              
                      end).
    
add_future_room(User, Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    mysql:transaction(p1,
                      fun() ->
                              {ok, UserId} = get_user_id(User),
                              {ok, RoomId} = get_room_id(RoomParsed),
                              mysql:execute(p1, add_future_room_query, [UserId, RoomId])
                      end).

user_leaving_room(User, Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    mysql:transaction(p1,
                      fun() ->
                              {ok, UserId} = get_user_id(User),
                              {ok, RoomId} = get_room_id(RoomParsed),
                              mysql:execute(p1, user_leaving_room_query, [UserId, RoomId])
                      end).

number_of_users_in_room(Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, number_of_users_in_room_query, [RoomParsed])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            {ok, hd(hd(Rows))};
        _ ->
            {error, "not a room"}
    end.

how_active_is_room(Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, how_active_is_room_query, [RoomParsed])
                           end) of
        {atomic, {data, MySQLResults}} ->
            [Active, Dormant] = hd(mysql:get_result_rows(MySQLResults)),
            
            if
                Active == 1 ->
                    {ok, 1};
                Dormant == 1 ->
                    {ok, 2};
                true ->
                    {ok, 3}
            end;
        _ ->
            {error, "how active is room"}
    end.

get_matching_rooms(CurrentRoomId, Query) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_matching_rooms_query, [CurrentRoomId, Query])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            {ok, Rows};
        _ ->
            {error, "get matching rooms"}
    end.

check_for_room_on_topic(CurrentRoomId, Query) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, check_for_room_on_topic_query, [CurrentRoomId, Query])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            {ok, Rows};
        _ ->
            {error, "check for room on topic"}
    end.

get_cat_for_room(Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_cat_for_room_query, [RoomParsed])
                           end) of
        {atomic, {data, MySQLResults}} ->
            case mysql:get_result_rows(MySQLResults) of
                [] ->
                    {ok, 1};            
                [[Result]] ->            
                    {ok, Result}
            end;
        _ ->
            {error, "get cat for room"}
    end.

get_all_rooms() ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_all_rooms_query, [])
                           end) of
        {atomic, {data, MySQLResults}} ->
            Rows = mysql:get_result_rows(MySQLResults),
            
            Results = lists:map(fun([Room]) ->
                                        {obj, [{id, Room}, {text, jabber_utils:unparse_room_name(Room)}]}
                                end, Rows),
            
            {ok, rfc4627:encode(Results)};
        _ ->
            {error, "get all rooms"}
    end.

get_past_rooms(Username) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_past_rooms_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            
            Results = lists:map(fun([Room, Cat, ImgUrl]) ->
                                        {obj, [{id, Room}, 
                                               {'ImgUrl', ImgUrl}, 
                                               {topicCat, Cat}, 
                                               {topicName, jabber_utils:unparse_room_name(Room)}]}
                                end, Rows),
            
            {ok, rfc4627:encode(Results)};
        _ ->
            {error, "get past rooms"}
    end.

get_future_rooms(Username) ->
    case mysql:transaction(p1,
                           fun() ->
                                   mysql:execute(p1, get_future_rooms_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            
            Results = lists:map(fun([Room, Cat, ImgUrl]) ->
                                        {obj, [{id, Room}, 
                                               {'ImgUrl', ImgUrl}, 
                                               {topicCat, Cat}, 
                                               {topicName, jabber_utils:unparse_room_name(Room)}]}
                                end, Rows),
            
            {ok, rfc4627:encode(Results)};
        _ ->
            {error, "get future rooms"}
    end.

get_last_room(Username) ->
    case mysql:transaction(p1,
                           fun () -> 
                                   mysql:execute(p1, get_last_room_query, [Username])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            case Rows of
                [[Room]] ->
                    {ok, jabber_utils:unparse_room_name(binary_to_list(Room))};
                _ ->
                    {ok, "Lounge"}
            end;
        _ ->
            {error, "get last room"}
    end.

convo_search(UserName, Query) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, convo_search_query, [Query++"*", UserName])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            create_json_from_convos (Rows);
        _ ->
            {error, "convo search"}
    end.

room_search(Room, Query) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, convo_search_query, [Query++";sort=time_segments;limit=10;", Room])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            create_json_from_convos (Rows);
        _ ->
            {error, "room search"}
    end.

search_chatyeo(Query) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, search_chatyeo_query, [Query++";sort=time_segments;limit=10;"])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            create_json_from_convos (Rows);
        _ ->
            {error, "search chatyeo"}
    end.

add_friend_request(UserName, Friend) -> 
    case mysql:transaction(p1,
                           fun () ->
                                   {ok, UserId} = get_user_id(UserName),
                                   {ok, FriendId} = get_user_id(Friend),
                                   if
                                       UserId =/= FriendId ->
                                           mysql:execute(p1, add_friend_request_query, [UserId, FriendId]);
                                       true ->
                                           failed
                                   end
                           end) of
        {atomic, {updated, {mysql_result,_,_,1,_,_}}} ->
            ok;
        _ ->
            failed
    end.

remove_friend(UserName, Friend) ->
    mysql:transaction(p1,
                      fun () ->
                              {ok, UserId} = get_user_id(UserName),
                              {ok, FriendId} = get_user_id(Friend),
                              mysql:execute(p1, remove_friend_query, [UserId, FriendId])
                      end).

accept_friend_request(UserName, Requester) ->
    mysql:transaction(p1,
                      fun () ->
                              {ok, UserId} = get_user_id(UserName),
                              {ok, RequesterId} = get_user_id(Requester),
                              mysql:execute(p1, add_friend_query, [UserId, RequesterId]),
                              mysql:execute(p1, delete_friend_request_query, [RequesterId, UserId])
                      end).

deny_friend_request(UserName, Requester) ->
    mysql:transaction(p1,
                      fun () ->
                              {ok, UserId} = get_user_id(UserName),
                              {ok, RequesterId} = get_user_id(Requester),
                              mysql:execute(p1, delete_friend_request_query, [RequesterId, UserId])
                      end).

get_all_friend_requests(UserName) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, get_all_friend_requests_query, [UserName])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            Rows;
        _ ->
            {error, "get all friend requests"}
    end.

get_messages_from(Room, From) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, get_messages_from_query, [Room, From])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults), 
            rfc4627:encode(lists:map(fun([Message]) -> Message end, Rows));
        _ ->
            {error, "get messages from"}
    end.

get_messages_from_to(Room, From, To) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, get_messages_from_to_query, [Room, From, To])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            rfc4627:encode(lists:map(fun([Message]) -> Message end, Rows));
        _ ->
            {error, "get messages from to"}
    end.

get_topics_for_tag_cloud (Room, Start, End) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, get_topics_for_tag_cloud_query_2, [Start, End, Room])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            Rows = mysql:get_result_rows(MySQLResults),
            topics_to_json(Rows);
        _ ->
            {error, "get topics for tag cloud"}
    end.
            
get_topics_for_room (Room) ->
    case mysql:transaction(p1,
                           fun () ->
                                   mysql:execute(p1, get_topics_for_room_query, [Room])
                           end) of
        {atomic, {data, MySQLResults}} ->            
            mysql:get_result_rows(MySQLResults);
        _ ->
            {error, "get topics for room"}
    end.

get_user_id (User) ->
    {data, {mysql_result, _, [[UserID]|_], _, _, _}} = 
        mysql:execute(p1, get_user_id_query, [User, User]),
    {ok, UserID}.

get_room_id (Room) ->
    RoomParsed = jabber_utils:parse_room_name(Room),
    {data, {mysql_result, _, [[RoomID]|_], _, _, _}} = 
        mysql:execute(p1, get_room_id_query, [list_to_binary(RoomParsed)]),
    {ok, RoomID}.

get_category_id (Category) ->
    case mysql:execute(p1, get_category_id_query, [Category]) of
        {data, MySQLResults} ->
            Rows = mysql:get_result_rows(MySQLResults),
            if 
                length(Rows) == 1 ->
                    CategoryId = hd(hd(Rows));
                true ->
                    CategoryId = 1
            end
    end,
    {ok, CategoryId}.

get_message_body (Message) ->
    {ok, DecodedMsg, _} = rfc4627:decode(Message),
    rfc4627:get_field(DecodedMsg, "body").
 
create_json_from_convos(Convos) ->
    rfc4627:encode(lists:map (fun([ID, NameTag, Text, Image, Extra, Room]) ->
                                      {obj, [{'id', ID}, {'nameTag', NameTag}, {'text', Text}, {'image', << <<"\"/images/spacer.gif\" class=\"">>/binary, Image/binary>>}, {'extra', Extra}, {'room', Room}]}
                              end, Convos)).

create_json_for_search_chatyeo(Convos) ->
    rfc4627:encode(lists:map (fun([ID, NameTag, Email, Text, _Image, Extra, Room]) ->
                                      Gravatar = element_gravatar:gravatar_icon(#gravatar{email=Email, size="35"}),
                                      {obj, [{'id', ID}, {'nameTag', NameTag}, {'text', Text}, {'image',  list_to_binary(Gravatar)}, {'extra', Extra}, {'room', Room}]}
                              end, Convos)).

topics_to_json(Topics) ->
    rfc4627:encode(lists:map (fun([TopicId, Topic, Count]) ->
                                          {obj, [{'id', TopicId}, {'topic', Topic},
                                                 {'popularity', Count}]}
                                  end, Topics)).
