%%%-------------------------------------------------------------------
%%% @author Jay
%%% @copyright (C) 2009, Tristan
%%% @doc Detect Urls, youtube links, and links to pictures, video
%%%              
%%% @end
%%% Created : 10 Aug 2009 by Jay
%%%-------------------------------------------------------------------
-module(bot_regexp).

-export([init/1, moving_to_room/2, update/3]).

-record(state, {session, room, url, regexps=[]}).

-include("config.hrl").

init(Session) ->    
    {ok, Video} = re:compile("\\bhttp\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,4}(/\\S*)?/\\S+\\.(wmv|asf|avi|ogv)\\b", [caseless]),
    {ok, Picture} = re:compile("\\bhttp\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,4}(/\\S*)?/\\S+\\.(jpeg|jpg|gif|png|tiff|tif|bmp)\\b", [caseless]),
    {ok, Flash} = re:compile("\\bhttp\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,4}(/\\S*)?/\\S+\\.swf\\b", [caseless]),
    %{ok, Youtube} = re:compile("\\b(http://)?(www\\.)?youtube.com/watch\\?v=[a-zA-Z0-9]+\\b", [caseless]),
    {ok, Youtube} = re:compile("\\b(http://)?(www\\.)?youtube.com/watch\\?v=(?P<VIDEO>[a-zA-Z0-9]+)\\b", [caseless]),
    {ok, Url} = re:compile("\\bhttp\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,4}(/\\S*)?", [caseless]),
    RegExp = [
              {regex_video, {Video, [0]}}, 
              {regex_picture, {Picture, [0]}}, 
              {regex_flash, {Flash,[0]}},
              {regex_youtube, {Youtube, [0, 'VIDEO']}} 
             ],
    #state{session=Session, url=Url, regexps=RegExp}.

moving_to_room(Room, State) ->
    State#state{room=Room}.

update(topics, _, _) ->
    ok;

update(result, {_Id, _Json}, State) ->
    State;

update(message, Message, State) ->
    {ok, Json, _} = rfc4627:decode(Message),
    {ok, Body} = rfc4627:get_field(Json, "body"),
    Reply = detect(Body, State),
    %% Adapt this to check for youtube, etc. and make it real info(grep from the other bots maybe)
    lists:foreach(fun(Result) ->
                          case Result of
                              ignore ->
                                  ignore;
                              _ ->
                                  ?INFO_MSG("RegExp sending msg via pubsub: ~p~n", [Result]),
                                  XML = pubsub_util:build_results_xml(bot_utils:get_hash(Result), Result),
                                  pubsub_util:publish(State#state.session, State#state.room, "results", XML)
                          end
                  end, Reply),
    State.

detect(Body, State) ->
    ?INFO_MSG("Detecting if ~p is a URL~n", [Body]),
    Results = detect_urls(Body, State#state.url),
    ?INFO_MSG("RegExp Results: ~p~n", [Results]),
    lists:map(fun(Element) ->
                      get_type(Element, State#state.regexps)
              end, Results).

get_type(Value, [Head | Tail]) ->
    {Type, {Re, Capture}} = Head,
    case re:run(Value, Re, [{capture, Capture}]) of
        {match, List} ->            
            S = binary_to_list(Value),
            Value2 = lists:reverse(lists:foldl(fun({Index, Size}, AccIn) ->
                                                    [list_to_binary(lists:sublist(S, Index + 1, Size)) | AccIn] end, [], List)),
            build_json(Type, Value2);
            %{Type, Value};
        nomatch ->
            get_type(Value, Tail)
    end;

get_type(Value, []) ->
    build_json(regex_url, Value).

accumulate_urls(_Body, []) ->
    [];
accumulate_urls(Body, [[{Index, Size} | _] | Tail]) ->
    Rem = size(Body) - Index - Size,
    <<_Garbage:Index/binary, Result:Size/binary, _Remaining:Rem/binary>> = Body,
    [Result | accumulate_urls(Body, Tail)].

				
detect_urls(Body, Url) ->
    Results = re:run(Body, Url, [global]),
    case Results of
        {match, List} ->
            accumulate_urls(Body, List);
        nomatch ->
            []
    end.

build_json(regex_picture, Url) ->    
    rfc4627:encode(bot_utils:make_json_google_images (1, 1, Url, Url, Url));
build_json(regex_url, Url) ->
    R = ibrowse:send_req(binary_to_list(Url),[], get),
    case R of
        {ok, _S, _ResponseHeader, Html} ->
            T = html_utils:parse_title(Html),
            case T of
                {ok, Title} ->
                    rfc4627:encode(bot_utils:make_json_sites(1, 1, Url, Title, Title));
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end;
build_json(regex_video, Value) ->
    [VideoUrl, VideoId] = Value,
    VideoImg = "http://i.ytimg.com/vi/" ++ binary_to_list(VideoId) ++ "/1.jpg",
    R = ibrowse:send_req(binary_to_list(VideoUrl),[], get),
    case R of
        {ok, _S, _RH, Html} ->
            VideoTitle = html_utils:parse_title(Html),
            case VideoTitle of
                {ok, Title} ->
                    rfc4627:encode(bot_utils:make_json_google_videos(1, 1, VideoUrl, list_to_binary(VideoImg), Title));
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end;
build_json(regex_flash, _Url) ->
    ignore.

%make_json_sites (Row, Url, Title, Message) ->
    %{obj, [{'Function', ?SITES_JS}, {'Row', Row}, {'Url', Url}, {'Title', Title}, {'Message', Message}]}.
    

