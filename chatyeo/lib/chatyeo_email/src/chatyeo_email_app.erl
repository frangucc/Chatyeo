-module(chatyeo_email_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    chatyeo_email_sup:start_link().
stop(_State) ->
    ok.
