-module(string_term).
-export([encode/1, decode/1]).

-include("config.hrl").

%% We encode to base64 to make it easier for jabber's xml to handle

%% @doc Encode term into base64 string
%% @spec encode(Term::term()) -> string()

encode(Term) ->
    Str = lists:flatten(io_lib:format("~p.", [Term])),
    base64:encode_to_string(list_to_binary(Str)).

%% @doc Decode base64 string to term
%% @spec decode(String::string()) -> term()

decode(String) ->
    L = binary_to_list(base64:decode(String)),
    {ok, Tokens,_} = erl_scan:string(L),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.
