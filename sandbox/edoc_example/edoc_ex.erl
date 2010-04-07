-module(edoc_ex).
-export([fun0/0, fun1/1, fun2/2, fun3/3]).

%% @doc This is a <em>very</em> useful module. It takes no args and returns ok.
%% @spec fun0() -> ok

fun0() ->
    ok.

%% @doc This is a little more useful..
%% @spec fun1(Arg::integer()) -> integer()

fun1(Arg) ->
    io:format("Arg: ~p~n", [Arg]),
    Arg.

%% @doc This is even more useful
%% @spec fun2(Arg1::term(), Arg2::term()) -> List
%%       List = [term()]

fun2(Arg1, Arg2) ->
    io:format("Args: ~p :: ~p~n", [Arg1, Arg2]),
    [Arg1, Arg2].

fun3(Arg1, Arg2, Arg3) ->
    io:format("Args: ~p :: ~p :: ~p~n", [Arg1, Arg2, Arg3]),
    ok.

%% comment with no at

% comment with one percent

% %% @TODO Finish writing the documentation.

fun4(Arg1, Arg2, Arg3, Arg4) ->
    io:format("Args: ~p :: ~p :: ~p :: ~p~n", [Arg1, Arg2, Arg3, Arg4]),
    ok.
