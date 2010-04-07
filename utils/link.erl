-module(link).
-export([start/0, stop/0]).
-export([process/1]).

start() ->
    spawn(fun() ->
		  register(link, self()),
		  process_flag(trap_exit, true),
		  Port = open_port({spawn, "./link"}, [{packet, 2}]),
		  loop(Port)
	  end).

stop() ->
    link ! stop.

process(S) -> call_port({link, S}).

call_port(Msg) ->
    io:format("Call port sending message~n"),
    link ! {call, self(), Msg},
    receive
        {link, Result} ->
            io:format("Here is the result!~n"),
            Result
    end.

loop(Port) ->
    receive
        {call, Caller, Msg} ->
            io:format("Erlang sending message..~n"),
            Port ! {self(), {command, encode(Msg)}}, 
            receive
                {Port, {data, Data}} ->
                    io:format("Erlang got response..~n"),
                    Caller ! {link, Data};
                X ->
                    io:format("I got some other response ~p~n", [X])
            end,
            loop(Port);
        stop ->
            Port ! {self(), close},
            receive
                {Port, closed} ->
                    exit(normal)
            end;
        {'EXIT', Port, Reason} ->
            exit({port_terminated,Reason})
    end.
 
encode({link, S}) -> [1, S].
