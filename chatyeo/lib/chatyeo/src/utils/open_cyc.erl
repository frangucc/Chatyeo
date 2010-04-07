-module(open_cyc).
-import(lists).
-import(plists).
-export([test/3,init/0,topics/1]).

-include("config.hrl").

-record(open_cyc_data,{term,links}).

loadfile(File) ->
          case file:consult(File) of
              {ok, Term} -> 
                  ?INFO_MSG ("Opened cyc.dat", []), Term;
              _ -> 
                  ?ERROR_MSG ("Could not load ~p", [File]),
                   []
          end.

filter_cyc_list( [{BWord,Rel}|CycA] ,Output)->
           Parsed = string:tokens(BWord," "),
           if length(Parsed)=<3  -> filter_cyc_list(CycA,[{BWord,Rel}|Output]); 
              length(Parsed)>3 -> filter_cyc_list(CycA,Output)
           end;
filter_cyc_list( [], Output)-> Output.

insert_term_links (Term,Links) ->
    Tran = fun()->  
                   mnesia:write(#open_cyc_data{term=Term,links=Links})
           end,
    mnesia:transaction(Tran).

%% @doc Initialize open_cyc data, must be called before any other function
%% @spec init() -> ok

init()->
    CycI = loadfile("/opt/chatyeo/data/cyc.dat"), 
    Cyc = filter_cyc_list(CycI,[]),
    
    mnesia:create_table(open_cyc_data,[{attributes, record_info(fields,open_cyc_data)},{type,bag},{disc_copies,[node()|nodes()]}]),
    _Out=[  insert_term_links(Term,Links) ||  {Term,Links} <- Cyc].
  
clean_term(Term)->
      Parsed = string:tokens(Term,":"),
      if length(Parsed)=<1-> hd(Parsed);
         length(Parsed)==1-> [_Depth,Word]=Parsed,Word;
         length(Parsed)>1->  string:join(tl(Parsed),":")
      end.  

locate_term(Term,_Cyc)->
    CleanTerm = clean_term(Term), 
     Tran = fun()-> 
                   Match = #open_cyc_data{term=CleanTerm,links='$1'},
                  %% Guard = [{'==','$1',CleanTerm}],
                   Result= ['$_'],
                   MatchSpec = [{Match, [], Result}],
                   mnesia:select(open_cyc_data,MatchSpec)
           end, 
    {atomic,Results} = mnesia:transaction(Tran),
    if length(Results)>=1 -> 
                        {open_cyc_data,Word,Links} = hd(Results), {Word,Links};
       length(Results)<1 -> {CleanTerm,[]}                         
    end.

stop_terms(WordList,[StopWord|StopWordList])-> stop_terms(lists:delete(StopWord,WordList),StopWordList);
stop_terms(WordList,[])-> WordList.

stop_terms(WordList)-> stop_terms(WordList,["thing","agent","the union of { event, artifact }","container-underspecified","event","mental situation","solid object","intelligent agent activity","the union of { person, animal }","organic substance","expression",
"temporally existing thing","action","the union of { action, generic artifact, written or spoken work }"]).
   
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

phrase_search(Text,Cyc,MaxDepth)->
   TokensA = string:tokens(string:to_lower(Text)," \n"),
   TokensB = TokensA,
   Grid = [ {A,B} || A<-TokensA,B<-TokensB,  A/=B], 
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
   Topics = extract_related( phrase_search(Phrase,[],3)),
   [topic_to_arrity_four(Topic)|| Topic  <- Topics].

test(WordA,WordB,Depth)->
  Cyc = init(),
 %locate_term("Chinese food",Cyc). 
  seek_match(WordA,WordB,Cyc,Depth).
%phrase_search("What do you want for lunch  How about Chinese").

