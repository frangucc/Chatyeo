-module(myrpc).

-export([f/1,g/1,h/1,setup/0,server/0,facLoop/0,fac/1]).

setup() ->
    %% spawn('bar@pannepot',myrpc,server,[]).
    %% spawn('bar@pannepot.diginux.net',myrpc,server,[]).
    spawn('ubuntu@domU-12-31-39-0C-34-03',myrpc,server,[]).

server() ->
    register(facserver,self()),
    facLoop().

facLoop() ->
    receive
	{Pid, N} ->
	    Pid ! {ok, fac(N)}
    end,
    facLoop().

f(N) ->
   %% {searcher_server, 'topicdetectorserver1@pannepot.diginux.net'} ! {self(), 1, N},
    {searcher_server, 'ubuntu@domU-12-31-39-0C-34-03'} ! {self(), 1, N},
%%    {facserver, 'bar@pannepot.diginux.net'} ! {self(), N},
    receive
        {ok, Res} ->
            Val = Res;
        {error, Res} ->
            Val = Res
    end,
    io:format("Echo of ~p is ~p.~n", [N,Val]).

g(N) ->
    %% {searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 2, N},
    {searcher_server, 'ubuntu@domU-12-31-39-0C-34-03'} ! {self(), 2, N},
%%    {facserver, 'bar@pannepot.diginux.net'} ! {self(), N},
    receive
        {ok, Res} ->
            Val = Res;
        {error, Res} ->
            Val = Res
    end,
    io:format("Echo of ~p is ~p.~n", [N,Val]).

h(N) ->
    %%{searcher_server, 'searcherserver1@pannepot.diginux.net'} ! {self(), 3, N},
    {searcher_server, 'ubuntu@domU-12-31-39-0C-34-03'} ! {self(), 3, N},
%%    {facserver, 'bar@pannepot.diginux.net'} ! {self(), N},
    receive
        {ok, Res} ->
            Val = Res;
        {error, Res} ->
            Val = Res
    end,
    io:format("Echo of ~p is ~p.~n", [N,Val]).

fac(N) when N==0 ->
    1;
fac(N) ->
    N * fac(N-1).
