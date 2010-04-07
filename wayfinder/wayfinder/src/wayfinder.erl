%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(wayfinder).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the wayfinder server.
start() ->
    wayfinder_deps:ensure(),
    ensure_started(crypto),
    application:start(wayfinder).

%% @spec stop() -> ok
%% @doc Stop the wayfinder server.
stop() ->
    Res = application:stop(wayfinder),
    application:stop(crypto),
    Res.
