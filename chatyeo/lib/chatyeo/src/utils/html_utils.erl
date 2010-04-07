%%%-------------------------------------------------------------------
%%% File    : html_utils.erl
%%% Author  : Jay<jdmundrawala@gmail.com>
%%% Description : This module contains utilities to manipulate html_utils
%%%               files
%%% Created : 4 Sep 2009
%%%-------------------------------------------------------------------
%%% Example Usage:
%%%

-module(html_utils).

-export([
        parse_title/1
    ]).


parse_title(Html) when is_binary(Html) ->
    %{ok, TitleRegex} = re:compile("<title(^\>)>((.|\n)*?)</title>", [multiline, caseless]),
    {ok, TitleRegex} = re:compile("<title( [^>^%]*)?>(?P<T>(.|\\n)*?)</title>", [multiline, caseless]),
    Result = re:run(Html, TitleRegex, [{capture, ['T']}]),
    case Result of 
        {match, List} ->
            [{Index, Size}|_] = List,
            Rem = size(Html) - Index - Size,
            <<_Garbage:Index/binary, Title:Size/binary, _Remaining:Rem/binary>> = Html,
            {ok, Title};
        nomatch ->
            nomatch
    end;
parse_title(Html) when is_list(Html) ->
    parse_title(list_to_binary(Html)).
