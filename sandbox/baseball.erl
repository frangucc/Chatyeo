-module(baseball).
-export([init/0, test/0,request_url/1, cubs/0, game_by_team/1]).

init() ->
    inets:start().

test() ->
    io:format("Let's do some baseball shit~n"),
    Res = request_url("http://msn.foxsports.com/nugget/9240_49"),
    Res.


%% make json for frontend with a Baseball column

cubs() ->
    inets:start(),
    A = request_url("http://msn.foxsports.com/nugget/9240_49"),
    {XML, _Rest} = xmerl_scan:string(A),
    xmerl_xpath:string("//game[@awayTeam=\"ChC\"]/@gameType", XML).

game_by_team(Team) ->
    TeamAbv = convert_name(Team),
    {_,_,_,_,_,_,_,GameAttributes,_,_,_,_} = get_xml(TeamAbv),
    [{xmlAttribute,gameId,[],[],[],[],1,[],"290811116",false},
     {xmlAttribute,gameType,[],[],[],[],2,[],"Regular Season",false},
     {xmlAttribute,awayTeam,[],[],[],[],3,[],"Phi",false},
     {xmlAttribute,homeTeam,[],[],[],[],4,[],"ChC",false},
     {xmlAttribute,awayName,[],[],[],[],5,[],"Phillies",false},
     {xmlAttribute,homeName,[],[],[],[],6,[],"Cubs",false},
     {xmlAttribute,awayId,[],[],[],[],7,[],"22",false},
     {xmlAttribute,homeId,[],[],[],[],8,[],"16",false},
     {xmlAttribute,conference,[],[],[],[],9,[],"National League",false},
     {xmlAttribute,awayScore,[],[],[],[],10,[],[],false},
     {xmlAttribute,homeScore,[],[],[],[],11,[],[],false},
     {xmlAttribute,gameStatus,[],[],[],[],12,[],"8:05 ET",false},
     {xmlAttribute,pregame,[],[],[],[],13,[],"1",false},
     {xmlAttribute,ingame,[],[],[],[],14,[],"0",false},
     {xmlAttribute,postgame,[],[],[],[],15,[],"0",false},
     {xmlAttribute,delayed,[],[],[],[],16,[],"0",false},
     {xmlAttribute,redZone,[],[],[],[],17,[],"0",false},
     {xmlAttribute,mob1,[],[],[],[],18,[],"0",false},
     {xmlAttribute,mob2,[],[],[],[],19,[],"0",false},
     {xmlAttribute,mob3,[],[],[],[],20,[],"0",false}] = GameAttributes.

get_xml(TeamAbv) ->
    inets:start(),
    A = request_url("http://msn.foxsports.com/nugget/9240_49"),
    {XML, _Rest} = xmerl_scan:string(A),
    QueryString = lists:flatten(io_lib:format("//game[@homeTeam=\"~s\"]", [TeamAbv])),
    hd(xmerl_xpath:string(QueryString, XML)).

%% @private
request_url(Url) ->
    HTTPResult = http:request(get, {Url, []}, [{timeout, 6000}], []),
    case HTTPResult of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error, HTTPResult}
    end.


convert_name("Chicago Cubs") -> "ChC";
convert_name("Philadelphia Phillies") -> "Phi".
