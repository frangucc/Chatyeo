%%%-------------------------------------------------------------------
%%% File    : bot_utils.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created :  3 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(bot_utils).
-compile(export_all).

-include("config.hrl").

%% @spec get_hash(string()) -> string()
%%
%% @doc Returns the string representative of the Sha1 hash of the given data.
get_hash(Data) ->
    base64:encode_to_string(crypto:sha(Data)).

%% @spec get_meta_reader_name(string()) -> string()
%%
%% @doc Takes a parsed room name and returns the name of the reader that is logged in
%% to its meta room.
get_meta_reader_name(Room) ->
    "_meta_reader_"++jabber_utils:parse_room_name (Room).

%% @spec get_chat_reader_name(string()) -> string()
%%
%% @doc Takes a parsed room name and returns the name of the reader that is logged in
%% to that room.
get_chat_reader_name(Room) ->
    "_chat_reader_"++jabber_utils:parse_room_name (Room).

%% @spec get_unix_timestamp() -> integer()
%%
%% @doc Retusn the current time as a unix timestamp in seconds.
get_unix_timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))-719528*24*3600.

make_json_google_images (Row, Depth, Url, ImgUrl, Title) ->
    Time = bot_utils:get_unix_timestamp(),
    {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?IMAGES_JS}, {'Row', Row}, {'Depth', Depth}, {'Url', Url}, {'ImgUrl', ImgUrl}, {'Title', Title}]}.

make_json_sites (Row, Depth, Url, Title, Message) ->
    Time = bot_utils:get_unix_timestamp(),    
    {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?SITES_JS}, {'Row', Row}, {'Depth', Depth}, {'Url', Url}, {'Title', Title}, {'Message', Message}]}.

make_json_google_videos (Row, Depth, Site, ImgUrl, Title) ->
    Time = bot_utils:get_unix_timestamp(),     
    {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?VIDEO_JS}, {'Row', Row}, {'Depth', Depth}, {'Title', Title}, {'ImgUrl', ImgUrl}, {'Site', Site}]}.

make_json_docs(Row, Depth, Url, Title, Message) ->
    Time = bot_utils:get_unix_timestamp(),    
    {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?DOCS_JS}, {'Row', Row}, {'Depth', Depth}, {'Url', Url}, {'Title', Title}, {'Message', Message}]}.


