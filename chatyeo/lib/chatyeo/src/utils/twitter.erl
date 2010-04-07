-module(twitter).
-export([init/0, log_timeline/2, search/2, search/3, test_xml/0, test_json/0, request_url/1, write_consult/2, dump_data/2, load_dump_to_mysql/1, test_insert/0]).

%%-include_lib("xmerl/include/xmerl.hrl").

-include("config.hrl").

-define(DB_SERVER, "localhost").
-define(CHATYEO_DB, "chatyeo").
-define(CHATYEO_DB_USER, "chatyeo").
-define(CHATYEO_DB_PASSWORD, "temp4now").

%% @doc Make sure inets is initialized before using
%% @spec init() -> ok

init() ->
    inets:start().

log_timeline(File, Time) ->
    io:format("Logging to ~p or ~p minutes~n", [File, Time]).

%% This converts the json returned from twitter into json that chatyeo accepts

convert_json(GoodJSON, Row, Depth) ->
    {ok, Text} = rfc4627:get_field(GoodJSON, "text"),
    {ok, FromUser} = rfc4627:get_field(GoodJSON, "from_user"),
    {ok, ID} = rfc4627:get_field(GoodJSON, "id"),
    {ok, ProfileImageURL} = rfc4627:get_field(GoodJSON, "profile_image_url"),    
    URL = list_to_binary(io_lib:format("http://twitter.com/~s/status/~p", [FromUser, ID])),
    Time = bot_utils:get_unix_timestamp(),     
    BadJSON = {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?TWEET_JS}, {'Row', Row}, {'Depth', Depth}, {'User', FromUser}, {'Url', URL}, {'ProfileImg', ProfileImageURL}, {'Message', <<FromUser/binary,<<" ">>/binary,Text/binary>>}]},
    BadJSON.


%% @doc Uses search.twitter.com's api to issue a query, returns default number of results
%% @spec search(Query::string()) -> List
%% List = [json_term()]

search(Query, Row) ->
    case request_url(io_lib:format("http://search.twitter.com/search.json?lang=en&q=~s", [ibrowse_lib:url_encode(Query)])) of
        {error, _} -> [];
        Res ->
            case rfc4627:decode(Res) of
                {error, _} -> [];
                {ok, TupleList, _Rest} ->  
                    {obj,Results} = TupleList,
                    {"results", Messages} = hd(Results),
                    {Data, _} = lists:mapfoldl(fun(X, Depth) -> 
                                                       {convert_json (X, Row, Depth), Depth+1}
                                               end, 1, Messages),
                    Data
            end
    end.

%% @doc Uses search.twitter.com's api to issue a query, returns Max number of results
%% @spec search(Query::string(), Max::integer()) -> List
%% List = [json_term()]

search(Query, Row, Max) ->
    lists:sublist(search(Query, Row), Max).

add_to_mysql(_Connection, []) -> ok;
add_to_mysql(Connection, [H|T]) ->
    {Text, ScreenName, FriendsCount, AccountCreatedTime, RealName, StatusesCount, ProfileImageURL, Location, UserID, FollowersCount, MessageCreatedTime, MessageID} = H,
    mysql:transaction(Connection,
                      fun() ->
                              mysql:execute(Connection, add_tweet_query, [Text, ScreenName, FriendsCount, AccountCreatedTime, RealName, StatusesCount, ProfileImageURL, Location, UserID, FollowersCount, MessageCreatedTime, MessageID])
                      end),
    add_to_mysql(Connection, T).

test_xml() ->
    Res = request_url("http://twitter.com/statuses/public_timeline.xml"),
    {XML, _Rest} = xmerl_scan:string(Res, [{quiet, true}]),
    xmerl_xpath:string("//statuses/status/id", XML).
 
test_json() ->
    Res = request_url("http://twitter.com/statuses/public_timeline.json"),
    io:format("Res: ~p~n", [Res]),
    case rfc4627:decode(Res) of
        {error, _} -> TupleList = [];
        {ok, TupleList, _Rest} -> ok
    end,
    [H1, H2 | _T] =  TupleList,
    io:format("~p~n~n~n~p~n", [H1,H2]).

load_dump_to_mysql(JsonDumpFile) ->
    mysql:start_link(tweet1, ?DB_SERVER, ?CHATYEO_DB_USER, ?CHATYEO_DB_PASSWORD, ?CHATYEO_DB),
    mysql:prepare(add_tweet_query, 
                  <<"INSERT INTO tweets (text, screen_name, friends_count, account_created_time, real_name, statuses_count, profile_image_url, location, user_id, followers_count, message_created_time, message_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>),
    {ok, List} = file:consult(JsonDumpFile),
    ParsedResults = lists:flatmap(fun(X) -> parse_tweets(X) end, List),
    io:format("Adding to mysql!!!!~n"),
    add_to_mysql(tweet1, ParsedResults).

test_insert() ->
    mysql:start_link(tweet1, ?DB_SERVER, ?CHATYEO_DB_USER, ?CHATYEO_DB_PASSWORD, ?CHATYEO_DB),
    mysql:prepare(add_tweet_query, 
                  <<"INSERT INTO tweets (text, screen_name, friends_count, account_created_time, real_name, statuses_count, profile_image_url, location, user_id, followers_count, message_created_time, message_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>),
    mysql:transaction(tweet1,
                      fun() ->
                              mysql:execute(tweet1, add_tweet_query, [<<"text">>, <<"screename">>, 1, <<"accountcreatedtime">>, <<"realname">>, 2,<<"profileimgurl">>, <<"location">>, 3, 4, <<"messagecreatedtime">>, 5])
                      end).
    

%% parses tweet that is in json->tuple format

parse_tweet(Tweet) ->
    {ok, Text} = rfc4627:get_field(Tweet, "text"),
    {ok, UserObject} = rfc4627:get_field(Tweet, "user"),
    {ok, ScreenName} = rfc4627:get_field(UserObject, "screen_name"),
    {ok, FriendsCount} = rfc4627:get_field(UserObject, "friends_count"),
    {ok, AccountCreatedTime} = rfc4627:get_field(UserObject, "created_at"),
    {ok, RealName} = rfc4627:get_field(UserObject, "name"),
    {ok, StatusesCount} = rfc4627:get_field(UserObject, "statuses_count"),
    {ok, ProfileImageURL} = rfc4627:get_field(UserObject, "profile_image_url"),
    {ok, Location} = rfc4627:get_field(UserObject, "location"),
    {ok, UserID} = rfc4627:get_field(UserObject, "id"),
    {ok, FollowersCount} = rfc4627:get_field(UserObject, "followers_count"),
    {ok, MessageCreatedTime} = rfc4627:get_field(Tweet, "created_at"),
    {ok, MessageID} = rfc4627:get_field(Tweet, "id"),
    {Text, ScreenName, FriendsCount, AccountCreatedTime, RealName, StatusesCount, ProfileImageURL, Location, UserID, FollowersCount, MessageCreatedTime, MessageID}.

parse_tweets(List) ->
    lists:map(fun(X) -> parse_tweet(X) end, List).

dump_data(File, Count) -> dump_data([], File, Count).
dump_data(Data, File, 0) -> write_consult(Data, File);
dump_data(Data, File, Count) ->
    case request_url("http://twitter.com/statuses/public_timeline.json") of
        {error, _} ->
            Res = [];
        Res -> ok
    end,
    case rfc4627:decode(Res) of
        {error, _} -> TupleList = [];
        {ok, TupleList, _Rest} -> ok
    end,
    io:format("Count: ~p~n", [Count]),
    timer:sleep(30000),
    dump_data([TupleList | Data], File, Count-1).

% write our recovery file
write_consult(EventList, Filename) ->
     file:delete(Filename),
     lists:map(fun(X) -> file:write_file(Filename, io_lib:format("~w.~n", [X]), [append]) end, EventList).

%% @private
request_url(Url) ->
    HTTPResult = http:request(get, {Url, []}, [{timeout, 6000}], []),
    case HTTPResult of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error, HTTPResult}
    end.
