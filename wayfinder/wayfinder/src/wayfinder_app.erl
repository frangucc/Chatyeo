%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the wayfinder application.

-module(wayfinder_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for wayfinder.
start(_Type, _StartArgs) ->
    wayfinder_deps:ensure(),
    wayfinder_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for wayfinder.
stop(_State) ->
    ok.
