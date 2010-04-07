-module(text_vector).
-export([test/0,test_ngrams/0,vector_text/2,vectorspace_add_text/3]).

%% @TODO Have Mike document this

is_stop_word(Word)->lists:member(Word, ["the","a","an"] ).

clean_stop_words (Words)->
    [ Word || Word <- Words, is_stop_word(Word)/=true].
    

split_line(Line)->
    Words = string:tokens(Line," \n"),
    Cleaned = clean_stop_words(Words),
    [porter:stem(string:to_lower(Word)) || Word <- Cleaned].
 
clean_line(Line)-> %[^A-Za-z0-9\']
    lists:filter(fun(X) -> lists:member(X, 
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ \n") end, Line).

stem_line(Line)-> split_line(clean_line(Line)).

 
build_word_frequency(Word,[{CWord,Freq}|Wordlist])->
    if CWord==Word -> [{CWord,Freq+1} | Wordlist];   
       CWord/=Word -> [{CWord,Freq} | build_word_frequency(Word,Wordlist)]
    end ;
build_word_frequency(Word,[])->[{Word,1}].

build_frequency([Word|Words],Wordlist)->
    NewWordlist =  build_word_frequency(Word,Wordlist),
    build_frequency(Words,NewWordlist);
build_frequency([],Wordlist)-> Wordlist.


build_ngrams([Word|Words],NGramSize,Wordlist)->
    if 
        length(Words)>=NGramSize-1 ->
            build_ngrams(Words,NGramSize,[[Word|lists:sublist(Words,NGramSize-1)]|Wordlist]);
        length(Words)<NGramSize-1->
            build_ngrams(Words,NGramSize,Wordlist)
    end;
build_ngrams([],_NGramSize,Wordlist)->Wordlist.
build_ngrams(Words,NGramSize)->build_ngrams(Words,NGramSize,[]).

build_vector_from_frequency (VectorID,FrequencyList) ->
    V = vectorspace:create_vector(VectorID), 
    Dims = [vectorspace:vector_dimension(Term,Frequency)  ||  {Term,Frequency} <- FrequencyList],
      vectorspace:vector_dimensions(V,Dims).

vector_text(VectorID,Input)->
    FrequencyList = build_frequency(stem_line(Input),[]),
    build_vector_from_frequency(VectorID,FrequencyList).

vectorspace_add_text (Vectorspace,VectorID,Text)->
    V=vector_text(VectorID,Text),
    vectorspace:vectorspace_add(V,Vectorspace).

test_ngrams()->
    Input="conditions were difficult that morning, with a sunrise all aflame.
Conditions align the sunrise of the soul.",
    {stem_line(Input), build_ngrams(stem_line(Input),2), build_ngrams(stem_line(Input),3)}.


test()->
    Input="conditions were difficult that morning, with a sunrise all aflame.
Conditions align the sunrise of the soul.",
    %  io:format("~p~n~p",[Input, vector_text("vector1",Input)]),
    vectorspace:create_idf_map([vector_text("vector1",Input)]) .
