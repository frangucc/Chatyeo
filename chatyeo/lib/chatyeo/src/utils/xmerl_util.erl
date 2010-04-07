-module(xmerl_util).
-import(xmerl).
-export([getElementsByTagName/2,extractSingleTextValue/2,extractAttribute/2]).
-include_lib("xmerl/include/xmerl.hrl").


%% TODO: Make this tail recursive. Not sure how to do it exactly as I am not sure
%% what the end case of thise code is. Whatever it is, it should return Result 
%% at the end, where Result is the accumulation of the elements
%% Brief example below.. -JW

%% getElementsByTagName(List, Item) ->
%%     getElementsByTagName(List, Item, []).

%% getElementsByTagName([H|T], Item, Result) when H#xmlElement.name == Item ->
%%     getElementsByTagName(T, Item, [H | Result]);

%% @doc Get elements based on their tag name
%% @spec getElementsByTagName(ElementList::list(), Tag::string()) -> Element::string()

getElementsByTagName([H|T], Item) when H#xmlElement.name == Item ->
    [H | getElementsByTagName(T, Item)];
getElementsByTagName([H|T], Item) when record(H, xmlElement) ->
    getElementsByTagName(H#xmlElement.content, Item) ++
        getElementsByTagName(T, Item);                                                                  
getElementsByTagName(X, Item) when record(X, xmlElement) ->
    getElementsByTagName(X#xmlElement.content, Item);
getElementsByTagName([_|T], Item) ->
    getElementsByTagName(T, Item);
getElementsByTagName([], _) ->
    [].

%% @doc Extract attribute from a list of attributes
%% @spec extractAttribute(AttributeList::list(), String::string()) -> Attribute::string()

extractAttribute(AttributeList,LookFor)-> extractAttribute(AttributeList,LookFor,[]).
extractAttribute([],_LookFor,Found)-> Found;
extractAttribute([ {xmlAttribute,NAME,_,_,_,_,_,_,FIELD,_} |AttributeList],LookFor,Found)->
    case NAME of
      LookFor-> extractAttribute(AttributeList,LookFor,FIELD);
      _-> extractAttribute(AttributeList,LookFor,Found)
    end.


extract_hd([H|_],_Type)->H;
extract_hd([],xmlElement)-> #xmlElement{};
extract_hd([],xmlText)-> #xmlText{value=""}.

%% @doc Extract single text value from XML element
%% @spec extractSingleTextValue(XMLElement::term(), String::string()) -> Value::string()

extractSingleTextValue(XMLElement,Tag) ->
    ASINelm = extract_hd(getElementsByTagName( XMLElement#xmlElement.content ,Tag),xmlElement),
    ASINtxt = extract_hd(ASINelm#xmlElement.content,xmlText),
    ASINtxt#xmlText.value.

