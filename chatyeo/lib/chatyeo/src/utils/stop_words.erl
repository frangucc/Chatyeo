-module(stop_words).
-export([clean/1,clean_stems/1]).
-import(porter).


stop_word_list()->["a","an","the","be","am","are","do","does","how","i","need",
                   "know","think","broke","fix"].

%is_stop_word(Word)->lists:member(Word,stop_word_list()).

stop_terms(WordList,[StopWord|StopWordList])-> stop_terms(lists:delete(StopWord,WordList),StopWordList);
stop_terms(WordList,[])-> WordList.
stop_terms(WordList)-> stop_terms(WordList,stop_word_list()). 



stem_split_line(Line)->
    Words = string:tokens(Line," \n"),
    Cleaned = stop_terms(Words),
    [porter:stem(string:to_lower(Word)) || Word <- Cleaned].


split_line(Line)->
    Words = string:tokens(Line," \n"),
    Cleaned = stop_terms(Words),
    [ Word   || Word <- Cleaned].

%% @doc Remove stop words from string
%% @spec clean(Phrase::string()) -> string()

clean(Phrase)->
     WordList=split_line(Phrase),
     Clean = stop_terms(WordList),
     string:join(Clean," ").

%% @doc Remove stop words from string and stop word stems
%% @spec clean_stems(Phrase::string()) -> string()

clean_stems(Phrase)->
     WordList=stem_split_line(Phrase),
     Clean = stop_terms(WordList),
     string:join(Clean," ").
