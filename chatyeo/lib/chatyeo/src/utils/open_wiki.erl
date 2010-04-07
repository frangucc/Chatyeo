-module(open_wiki).
-export([test/3,init/0,topics/1]).

-include("config.hrl").

-record(open_wiki_data,{term,links}).
  

insert_term_links (Term,Links) ->
%%    Tran = fun()->  
%%                   mnesia:write(#open_wiki_data{term=Term,links=Links})
%%           end,
%%    mnesia:transaction(Tran).
    
    %% Do dirty write, since we are only writing unique vals, and not reading during
    %% this process
    ok = mnesia:dirty_write(#open_wiki_data{term=Term,links=Links}).

eval_term(S)->
   case erl_scan:string(S) of
       {ok,Scanned,_} ->
           {ok,Parsed}= erl_parse:parse_exprs(Scanned),
           {value,Term,_}= erl_eval:exprs(Parsed,[]),
           {result, Term};
       {error, Error, Error2} ->
           ?ERROR_MSG ("Unable to process ~p:~p:~p~n", [S,Error,Error2]),
           {error}
   end.
   
read_database(Filename)->
    %_Out=[  insert_term_links(Term,Links) ||  {Term,Links} <- Cyc],
    {ok,Device} = file:open(Filename,[read]),
    read_database_line(Device).

read_database_line(Device)->
    case io:get_line(Device,"") of
        eof-> file:close(Device);
        Line-> 
            case eval_term(Line) of 
                {result, {Term,Links}} ->
                    insert_term_links(Term,Links),
                    read_database_line(Device);
                {error} -> read_database_line(Device)
            end
    end.

%% @doc Initialize open_wiki data, must be called before any other function
%% @spec init() -> ok

init()->
    try
        mnesia:table_info(open_wiki_data,type)
    catch
        exit: _ ->
            mnesia:create_table(open_wiki_data,[{attributes, record_info(fields,open_wiki_data)},{type,bag},{disc_copies,[node()|nodes()]}]),
            read_database("/opt/chatyeo/data/wiki.dat"),
            ok
    end.
 

clean_term(Term)->
      Parsed = string:tokens(Term,":"),
      if length(Parsed)=<1-> hd(Parsed);
         length(Parsed)==1-> [_Depth,Word]=Parsed,Word;
         length(Parsed)>1->  string:join(tl(Parsed),":")
      end.  

locate_term(Term,_Cyc)->
    CleanTerm = clean_term(Term), 
    %% Tran = fun()-> 
%%                    Match = #open_wiki_data{term=CleanTerm,links='$1'},
%%                    %% Guard = [{'==','$1',CleanTerm}],
%%                    Result= ['$_'],
%%                    MatchSpec = [{Match, [], Result}],
%%                    mnesia:select(open_wiki_data,MatchSpec)
%%             end, 
%%     {atomic,Results} = mnesia:transaction(Tran),
    
    Match = #open_wiki_data{term=CleanTerm,links='$1'},
    Result= ['$_'],  
    MatchSpec = [{Match, [], Result}],
    %% Do dirty select, since no data is changing
    Results = mnesia:dirty_select(open_wiki_data,MatchSpec),
    if 
        length(Results)>=1 -> 
            {open_wiki_data,Word,Links} = hd(Results), {Word,Links};
        length(Results)<1 -> 
            {CleanTerm,[]}                         
    end.

 
stop_terms(WordList,[StopWord|StopWordList])-> stop_terms(lists:delete(StopWord,WordList),StopWordList);
stop_terms(WordList,[])-> WordList.

stop_terms(WordList)-> stop_terms(WordList,["thing","agent","living people"]). 
   

 
expand_one_step(Word,Cyc )->
    {_Term,WordList} = locate_term(Word,Cyc),
    stop_terms(WordList).
      
extract_children(Word,Cyc)->
    expand_one_step(Word,Cyc ).

extract_depth([Word|WordList],Cyc,Depth,Output) when Depth>0->
      Children= extract_children(Word,Cyc),  
      DepthChildren = [ string:join([integer_to_list(Depth),Child],":")  || Child <- Children],
      extract_depth(WordList,Cyc,Depth,lists:merge(DepthChildren,extract_depth(DepthChildren,Cyc,Depth-1,Output)));
extract_depth([],_Cyc,_Depth,Output)->Output;
extract_depth(_Word,_Cyc,_Depth,Output)->Output.
     




match_lists([WordA|ClassA],ClassB,Output)->
    case lists:member(WordA,ClassB) of
       true-> case lists:member(WordA,Output) of
                true->match_lists(ClassA,ClassB,Output);
                false->match_lists(ClassA,ClassB,[WordA|Output])
              end;
       false-> match_lists(ClassA,ClassB,Output)
    end;
match_lists([],_ClassB,Output)->Output.
      


seek_match(WordA,WordB,Cyc,MaxDepth)->
   ClassA = extract_depth([WordA],Cyc,MaxDepth,[]),
   ClassB = extract_depth([WordB],Cyc,MaxDepth,[]),
   {_CombinedTerm , Children }= locate_term(string:join([WordA,WordB]," "),Cyc),
   Combined = [ string:join([integer_to_list(MaxDepth),Child],":")  || Child <- Children],

   {_RCombinedTerm , RChildren }= locate_term(string:join([WordB,WordA]," "),Cyc),
   RCombined = [ string:join([integer_to_list(MaxDepth),RChild],":")  || RChild <- RChildren],

   Match = match_lists(ClassA,ClassB,[]), 
   lists:reverse(lists:sort(lists:umerge([RCombined,Combined,Match]))).


phrase_found([{WordA,WordB,FoundList}|SearchList],Output)->
    if length(FoundList)=<0 -> phrase_found(SearchList,Output);
       length(FoundList)>0 -> phrase_found(SearchList,[{WordA,WordB,FoundList}|Output])
    end;        
phrase_found([],Output)->Output.
 
build_ngrams([Word|Words],NGramSize,Wordlist)->
    if 
        length(Words)>=NGramSize-1 ->
            build_ngrams(Words,NGramSize,[[Word|lists:sublist(Words,NGramSize-1)]|Wordlist]);
        length(Words)<NGramSize-1->
            build_ngrams(Words,NGramSize,Wordlist)
    end;
build_ngrams([],_NGramSize,Wordlist)->Wordlist.
build_ngrams(Words,NGramSize)->build_ngrams(Words,NGramSize,[]).


merged_tokens(Text)->
   TokensA = string:tokens(string:to_lower(Text)," \n"),
   TokensB = TokensA,
   Grid1 = [ {A,B} || A<-TokensA,B<-TokensB,  A/=B],
   Grid2grams = build_ngrams(TokensB,2),
   Grid2 = [ {string:join(C," "),string:join(D," ")} || C<-Grid2grams,D<-Grid2grams,  C/=D], 
   Grid3grams = build_ngrams(TokensB,3),
   Grid3 = [ {string:join(E," "),string:join(F," ")} || E<-Grid2grams,F<-Grid3grams,  E/=F],
   Grid = lists:merge([Grid1,Grid2,Grid3]),
   Grid. 

phrase_search(Text,Cyc,MaxDepth)->
   Grid = merged_tokens(Text), 
 % Found = [ {WordA,WordB,seek_match(WordA,WordB,Cyc,MaxDepth)} || {WordA,WordB} <- Sorted],
   Found = plists:map(fun({WordA,WordB})->
                          {WordA,WordB,seek_match(WordA,WordB,Cyc,MaxDepth)}
                      end, Grid ,{nodes,[node()|nodes()]}),
   lists:usort(phrase_found(Found,[])).

extract_related([{WordA,WordB,Relation}|Phrases],Output)->  
    TestA = {WordA,WordB,hd(Relation)},
    TestB = {WordB,WordA,hd(Relation)},
  case ( lists:member(TestA,Output) ) of
     true -> extract_related(Phrases,Output);   
     false -> case (lists:member(TestB,Output) ) of
                 true -> extract_related(Phrases,Output);   
                 false->extract_related(Phrases,[TestA|Output])
              end
  end;
extract_related([],Output)-> lists:usort(Output).
extract_related(Phrases)->extract_related(Phrases,[]).   

%topics(Phrase,Cyc)-> 
%   extract_related( phrase_search(Phrase,Cyc,3)).

topic_to_arrity_four({TermA,TermB,Link})->
      Parsed = string:tokens(Link,":"),
      Depth= hd(Parsed) ,
      LinkTerm= hd(tl(Parsed)), 
      {Dint,_List} = string:to_integer(Depth),
      {TermA,TermB,Dint,LinkTerm}.

%% @doc Find all topics for a phrase and/or group of sentences
%% @spec topics(Phrase::string()) -> List
%% List = [term()]

topics(Phrase)->
   Topics = extract_related( phrase_search(Phrase,[],4)),
   [topic_to_arrity_four(Topic)|| Topic  <- Topics].

test(WordA,WordB,Depth)->
  Cyc = init(),
 %locate_term("Chinese food",Cyc). 
  seek_match(WordA,WordB,Cyc,Depth).
%phrase_search("What do you want for lunch  How about Chinese").
