-module(text_analysis).
-export([rank_topics/1, is_string/1]).

%input
%{TermA,TermB,score,connection}

%internal ranking
%{TermTokenCount,Score,{tuple from above}}


count_tokens(TermA,TermB)->
    WordsA = string:tokens(TermA," \n"),
    WordsB = string:tokens(TermB," \n"),
    length(WordsA)+length(WordsB).

%% @doc Takes a topic list and ranks them
%% @spec rank_topics(TopicList::list()) -> list()

rank_topics(TopicList)->
  WeightList = [ {count_tokens(TermA,TermB)*Score,{TermA,TermB,Score,Connection}} || {TermA,TermB,Score,Connection} <- TopicList],
  Sorted = lists:reverse( lists:keysort(1,WeightList)),
  [ {TermA,TermB,TotalScore,Connection} || {TotalScore,{TermA,TermB,_Score,Connection}} <- Sorted].


is_string(S) ->
    lists:all(fun(X) -> is_char(X) end, S).
is_char(C) when C >= 0, C =< 255 -> true;
is_char(_) -> false.
