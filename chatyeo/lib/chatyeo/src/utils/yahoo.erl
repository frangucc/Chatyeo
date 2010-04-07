-module(yahoo).
-export([init/0, search/1, search/2]).  
-define(APP_ID, "GafQbBTV34Hj2t7M55hhzsqPNBCvy89GYPcoS4Egj05KJArddpOkiuOEAE_bvXY-").

-include("config.hrl").

%% @doc Make sure inets is initialized before using
%% @spec init() -> ok

init() ->
    inets:start().

%% @doc Uses yahoo questions's api to issue a query, returns default number of results,
%% a result being the top questions that match the question along with their best answer
%% @spec search(Query::string()) -> List
%% List = [json_term()]

search(Query) ->
    Url = "http://answers.yahooapis.com/AnswersService/V1/questionSearch?" ++
        "appid=" ++ ibrowse_lib:url_encode(?APP_ID) ++
        "&query=" ++ ibrowse_lib:url_encode(Query) ++ "&output=json",
    ?INFO_MSG("URL: ~s~n", [Url]),

    case request_url(Url) of
        {error, _} -> [];
        Res ->
            case rfc4627:decode(Res) of
                {error, _} -> [];
                {ok, TupleList, _Rest} ->
                    Depth = 1,
                    {ok, All} = rfc4627:get_field(TupleList, "all"),
                    {obj, QuestionsObject} = All,
                    [{"count", _QuestionsCount} | [{"questions", QuestionsList}]]  = QuestionsObject,
                    {Data, _} = lists:mapfoldr(fun(X, Row) -> 
                                                       {convert_json (X, Row, Depth), Row+1}
                                               end, 1, QuestionsList),
                    Data            
            end
    end.


%% @doc Uses yahoo questions's api to issue a query, returns Max number of results
%% @spec search(Query::string(), Max::integer()) -> List
%% List = [json_term()]

search(Query, Max) ->
    lists:sublist(search(Query), Max).


%% @private
request_url(Url) ->
    HTTPResult = http:request(get, {Url, []}, [{timeout, 6000}], []),
    case HTTPResult of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error, HTTPResult}
    end.

%% @private
convert_json(OriginalJSON, Row, Depth) ->
    Time = bot_utils:get_unix_timestamp(),
    {ok, URL} = rfc4627:get_field(OriginalJSON, "Link"),
    {ok, User} = rfc4627:get_field(OriginalJSON, "UserNick"),
    {ok, UserImage} = rfc4627:get_field(OriginalJSON, "UserPhotoURL"),
    {ok, Question} = rfc4627:get_field(OriginalJSON, "Content"),
    {ok, FromUser} = rfc4627:get_field(OriginalJSON, "ChosenAnswererNick"),
    {ok, Answer} = rfc4627:get_field(OriginalJSON, "ChosenAnswer"),
    
    {obj, [{'key', [Time, <<"\\u9999">>]}, {'Function', ?YAHOOANSWERS_JS}, {'Row', Row}, {'Depth', Depth}, {'Url', URL}, {'User', User}, {'ProfileImg', UserImage}, {'Question', Question} , {'FromUser', FromUser}, {'Answer', Answer}]}.
