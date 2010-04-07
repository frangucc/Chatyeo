-module(open_ai).
-import(lists).
-export([test/0]).


%% nth_head(N,Depth,[Popped|List],Output)->
%%      if  
%%         Depth<N -> nth_head(N,Depth+1,List,[Popped|Output]);
%%         Depth>=N -> nth_head(N,Depth+1,List,Output)
%%      end;
%% nth_head(_N,_Depth,[],Output)->Output.
%% nth_head(N,List)-> nth_head(N,0,List,[]).

%% suffix_position(Suffix,Phrase)->
%%       case lists:suffix(Suffix,Phrase) of
%%         true-> length(Phrase)-length(Suffix);
%%         false-> 0
%%       end.

%% extract_to_suffix(List,Suffix)->   %pull out everything up to the suffix
%%        SPOS = suffix_position(Suffix,List),
%%        if 
%%           SPOS=<0 -> [];
%%           SPOS>0 -> nth_head(SPOS,List)
%%        end.
 

lookup_reply({pattern,["Test1"]},Context,Detail)-> lookup_reply({answer,"A"},Context,Detail);
lookup_reply({pattern,["Test2"]},Context,Detail)-> lookup_reply({answer,"B"},Context,Detail);


% _ BLUE FISH in AIML
lookup_reply({pattern,[_ | ["BLUE", "FISH"] ]},Context,Detail)-> lookup_reply({answer,"Crazy Parsing rules!"},Context,Detail);  
    
% DEFINE A * DOG WHISTLE
lookup_reply({'DEFINE A * DOG WHISTLE',["DOG","WHISTLE"]},Context,Detail)-> lookup_reply({answer,"DEFINE A * DOG WHISTLE"},Context,Detail);
lookup_reply({'DEFINE A * DOG WHISTLE',[H|Phrase]},Context,Detail)-> 
         lookup_reply({'DEFINE A * DOG WHISTLE',Phrase},[H|Context],Detail); 
lookup_reply({'DEFINE A * DOG WHISTLE',[]},Context,Detail)-> lookup_reply({no_answer},Context,Detail);
lookup_reply({pattern,["DEFINE" | ["A"| Phrase]]},Context,Detail)-> 
         lookup_reply({'DEFINE A * DOG WHISTLE',Phrase},Context,Detail); 

lookup_reply({pattern,[_POPTERM|Terms]},Context,Detail)-> lookup_reply({pattern,Terms},Context,Detail); %keep pulling off the front until we find something. 
lookup_reply({pattern,_},Context,Detail)-> lookup_reply({no_answer},Context,Detail); 

lookup_reply({answer,Reply},Context,Detail)->{answer,Reply,lists:reverse(Context),Detail}; 
lookup_reply({no_answer},_Context,_Detail)->{no_answer}.


%Now how the hell to match :  DEFINE A * DOG WHISTLE   (so define a big red dog whistle responds with the * in place, but "big red" in extra info)

%% @doc This does something, who knows what, ask Mike!
%% @spec test() -> string()

test()-> lookup_reply({pattern,["DEFINE","A","BIG","YELLOW","DOG","WHISTLE"]},[],[]).
