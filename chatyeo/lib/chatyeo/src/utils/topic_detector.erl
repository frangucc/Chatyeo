-module(topic_detector).
-export([query_string/2, get_admin_data/1, detect_topics/1, clear_all/0]).

-include("config.hrl").

%% Topic detector server message codes
-define(GET_TOPICS, 1).
-define(GET_ADMIN_TOPICS, 2).
-define(GET_TOPICS_BY_USER, 3).
-define(GET_TOPICS_EXCLUDING_USER, 4).
-define(DETECT_TOPICS, 5).
-define(CLEAR_ALL, 6).
-define(MWFP, 7).
-define(MWFS, 8).
-define(MCFP, 9).
-define(MCFS, 10).
-define(MRTOFP, 11).
-define(MRTOFS, 12).
-define(SET_DEFAULT_WS, 13).
-define(SET_WS, 14).

%% Full address of topic detector server
-define(TD_SERVER, 'topicdetectorserver1@cloud-laptop').

%% Topic bot returns the following
%% {CT_WIKI_ID, CT_NAME, CT_WEIGHT, CT_REL_TO_CTXT, CT_REL_TO_OTHER, CT_GENERALITY, 6}

%% @doc Query the topic detector
%% @spec query(Q:string()) -> string()

query_string(_ID, [{_, <<"hi">>},{_, <<"hi">>},{_, <<"hi">>}]) ->
    [{148707,"RZA", 0.56, 0.5, 0.5, 0.6875}, 
     {2121624, "Rap", 0.56, 0.5, 0.5, -1.0},
     {33135, "wu-tang", 0.55, 0.5, 0.5, 0.625}];


query_string(ID, Q) ->
    ?INFO_MSG("Send query to topic detector server: ~p~n", [Q]),
    {topicdetector_server, ?TD_SERVER} ! {self(), 1, ID, Q},
    io:format("Send query to topic detector server: ~p~n", [Q]),
    receive
        {ok, {Res1, Res2}} ->
            lists:sort(fun(E1, E2) -> element(?CT_WEIGHT, E1) >= element(?CT_WEIGHT, E2) end, Res1) ++
                lists:sort(fun(E1, E2) -> element(?CT_WEIGHT, E1) >= element(?CT_WEIGHT, E2) end, Res2);
        {error, ignore} ->
            ignore;
        {error, Res} ->
            ?ERROR_MSG("~p returned error: ~p~n", [?MODULE, Res]),
            []
    after 20000 ->
            ignore
    end.

get_admin_data(ID) ->
    {topicdetector_server, ?TD_SERVER} ! {self(), 2, ID},
    receive
        {ok, Res} ->
            Res;
        {error, Res} ->
            ?ERROR_MSG("~p returned error: ~p~n", [?MODULE, Res]),
            nil
    after 60000 ->
            nil
    end.

set_min_weight_for_primary(Weight) ->
    set(?MWFP, Weight).
set_min_weight_for_secondary(Weight) ->
    set(?MWFS, Weight).
set_min_context_for_primary(Weight) ->
    set(?MCFP, Weight).
set_min_context_for_secondary(Weight) ->
    set(?MCFS, Weight).
set_min_rel_to_others_for_primary(Weight) ->
    set(?MRTOFP, Weight).
set_min_rel_to_other_for_secondary(Weight) ->
    set(?MRTOFS, Weight).

set(What, Weight) ->
    {topicdetector_server, ?TD_SERVER} ! {self(), What, Weight},
	receive
		{ok, Res} ->
			Val = Res;
		{error, Res} ->
			?ERROR_MSG("~p returned error: ~p~n", [?MODULE, Res]),
			Val = nil
	after 20000 ->
			Val = nil
	end,
	Val.

%get_topics_by_user(ChannelID, User) ->
    %{topicdetector_server, ?TD_SERVER} ! {self(), ?GET_TOPICS_BY_USER, ChannelID, User},
    %receive
        %{ok, {MacroTopics, MicroTopics}} ->
            %Val = {lists:reverse(lists:sort(?CT_WEIGHT,MacroTopics)), lists:reverse(lists:sort(?CT_WEIGHT,MicroTopics))};
        %{error, Reason} ->
            %?ERROR_MSG("~p returned error: ~p~n", [?MODULE, Reason]),
            %Val = {[], []}
    %after 20000 ->
            %Val = {[], []}
    %end,
    %Val.
    
%get_topics_by_user(ChannelID, User) ->
    %{topicdetector_server, ?TD_SERVER} ! {self(), ?GET_TOPICS_EXCLUDING_USER, ChannelID, User},
    %receive
        %{ok, {MacroTopics, MicroTopics}} ->
            %Val = {lists:reverse(lists:sort(?CT_WEIGHT,MacroTopics)), lists:reverse(lists:sort(?CT_WEIGHT,MicroTopics))};
        %{error, Reason} ->
            %?ERROR_MSG("~p returned error: ~p~n", [?MODULE, Reason]),
            %Val = {[], []}
    %after 20000 ->
            %Val = {[], []}
    %end,
    %Val.
detect_topics(Message) ->
    {topicdetector_server, ?TD_SERVER} ! {self(), ?DETECT_TOPICS, Message},
    receive
        {ok, Topics} ->
            {ok, lists:sort(fun(E1, E2) -> element(?CT_WEIGHT, E1) >= element(?CT_WEIGHT, E2) end,Topics)};
        {error, Reason} ->
            ?ERROR_MSG("~p returned error: ~p~n", [?MODULE, Reason]),
            {ok, []}
    after 20000 ->
            {ok, []}
    end.

clear_all() ->
    {topicdetector_server, ?TD_SERVER} ! {self(), ?CLEAR_ALL}.
