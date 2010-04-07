%%%-------------------------------------------------------------------
%%% File    : google-image-search.erl
%%% Author  : Tristan Sloughter <tristan@kfgx61>
%%% Description : 
%%%
%%% Created : 26 Jul 2009 by Tristan Sloughter <tristan@kfgx61>
%%%-------------------------------------------------------------------
-module(google_search).

-compile (export_all).

-define(GOOGLE_URL, "ajax.googleapis.com/ajax/services/search").
-define(IMAGE_TYPE, "images").
-define(WEB_TYPE, "web").
-define(VIDEO_TYPE, "video").
-define(VERSION, "1.0").
-define(REFERER, "http://www.my-ajax-site.com").

start () ->    
    application:start(inets).

image_search (Query) ->
    Url = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s", [?GOOGLE_URL, ?IMAGE_TYPE, ?VERSION, ibrowse_lib:url_encode(Query)])),
    {ok, Results, _} = rfc4627:decode(http_g_request(Url)),
    image_parse (Results).

web_search (Query) ->
    Url = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s", [?GOOGLE_URL, ?WEB_TYPE, ?VERSION, ibrowse_lib:url_encode(Query)])),
    {ok, Results, _} = rfc4627:decode(http_g_request(Url)),
    web_parse (Results).

video_search (Query) ->
    Url = lists:flatten(io_lib:format("http://~s/~s?v=~s&q=~s", [?GOOGLE_URL, ?VIDEO_TYPE, ?VERSION, ibrowse_lib:url_encode(Query)])),
    {ok, Results, _} = rfc4627:decode(http_g_request(Url)),
    video_parse (Results).

image_parse({obj,[{"responseData", {obj,[{"results", Results}, _]}}, _, _]}) ->
    rfc4627:encode(lists:map (fun(X) ->
                                      {ok, URL} = rfc4627:get_field(X, "url"),
                                      {ok, Title} = rfc4627:get_field(X, "title"),
                                      make_json_google_images(URL, URL, Title)
                              end, Results)).

web_parse({obj,[{"responseData", {obj,[{"results", Results}, _]}}, _, _]}) ->
    rfc4627:encode(lists:map (fun(X) ->
                                      {ok, URL} = rfc4627:get_field(X, "url"),
                                      {ok, Title} = rfc4627:get_field(X, "title"),
                                      {ok, Content} = rfc4627:get_field(X, "content"),
                                      make_json_sites(URL, Title, Content)
                              end, Results)).

video_parse({obj,[{"responseData", {obj,[{"results", Results}, _]}}, _, _]}) ->
        rfc4627:encode(lists:map (fun(X) ->
                                      {ok, URL} = rfc4627:get_field(X, "url"),
                                      {ok, Image} = rfc4627:get_field(X, "tbUrl"),
                                      {ok, Title} = rfc4627:get_field(X, "title"),
                                      make_json_google_videos(URL, Image, Title)
                              end, Results)).

http_g_request(Url) ->
    case http:request(get, {Url, [{"Referer", ?REFERER}]}, [], []) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.

make_json_google_images (Url, ImgUrl, Title) ->
    {obj, [{'Url', Url}, {'ImgUrl', ImgUrl}, {'Title', Title}]}.

make_json_google_videos (Site, ImgUrl, Title) ->
    {obj, [{'Title', Title}, {'ImgUrl', ImgUrl}, {'Site', Site}]}.

make_json_sites (Url, Title, Message) ->
    {obj, [{'Url', Url}, {'Title', Title}, {'Message', Message}]}.
