%%%-------------------------------------------------------------------
%%% File    : restful_querying.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 27 Jul 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(restful_querying).
-export([http_g_request/1]).

-include("config.hrl").

http_g_request(Url) ->
    case http:request(get, {Url, [{"Referer", ?REFERER}]}, [], []) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
