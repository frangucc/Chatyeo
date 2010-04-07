-module(rss).
-export([init/0,data/1,sample_feeds/0,broken_feeds/0,rsslist_to_rssml/2]).
-include_lib("xmerl/include/xmerl.hrl").
-import(xmerl_util).

%% @doc Initialize rss module, must be called before any other function
%% @spec init() -> ok

init() ->
    inets:start(),
    results_cache:init().

%% @doc Sample feeds to get RSS data from
%% @spec sample_feeds() -> URLs
%% URLs = [string()] 

sample_feeds() ->
    ["http://rss.slashdot.org/Slashdot/slashdot",
     "http://www.osnews.com/feeds",
     "http://rss.cnn.com/rss/cnn_latest.rss",
     "http://rss.cnn.com/rss/cnn_topstories.rss",
     "http://rss.cnn.com/rss/cnn_world.rss",
     "http://rss.cnn.com/rss/cnn_us.rss",
     "http://feeds.foxnews.com/foxnews/latest",
     "http://feeds.foxnews.com/foxnews/national",
     "http://feeds.foxnews.com/foxnews/world"].

%% @doc Feeds that are currently broken and need to be fixed
%% @spec broken_feeds() -> URLs
%% URLs = [string()] 

broken_feeds() ->
    ["http://feeds.huffingtonpost.com/huffingtonpost/raw_feed",
     "http://news.google.com/news?pz=1&ned=us&hl=en&output=rss"].

clean_xml_tag({feed,_,_})-> "link />";
clean_xml_tag(Tag)->
    % UTF8 = xmerl_ucs:from_utf8(Tag),
    Fixed = re:replace(Tag, " \& ", " \\&amp; ",[{return,list},global]),
    Fixed2 = re:replace(Fixed, "\r", "",[{return,list},global]),
    Fixed3 = re:replace(Fixed2, "\">", "\" />",[{return,list},global]),
    Fixed4 = re:replace(Fixed3, " >", " />",[{return,list},global]),
    Fixed4.

examine_link_for_rss(Tag)->
    Cleaned = "<"++clean_xml_tag(Tag),
    %io:format("~p~n",[Cleaned]),
                      try xmerl_scan:string(Cleaned) of
                         {Doc,_Misc}->
                             LinkType = xmerl_util:extractAttribute(Doc#xmlElement.attributes,type),
                             case LinkType of
                               "application/rss+xml"-> embedded_rss;
                               _ -> not_a_feed
                             end;
                         _->not_a_feed
                      catch
                         throw:_X->not_a_feed;
                         exit:_X->not_a_feed;
                         fatal:_X->#xmlElement{};
                         error:_X->not_a_feed
                      end.

extract_rss_link(Tag)->
    Cleaned = "<"++clean_xml_tag(Tag),
   % io:format("~p~n",[Cleaned]),
                      try xmerl_scan:string(Cleaned) of
                         {Doc,_Misc}->
                              Found = xmerl_util:extractAttribute(Doc#xmlElement.attributes,href),
                              Found;
                         _->not_a_feed
                      catch
                         throw:_X->not_a_feed;
                         exit:_X->not_a_feed;
                         fatal:_X->#xmlElement{};
                         error:_X->not_a_feed
                      end.


feed_type_tokens(_Tag,_Tags,[])-> not_a_feed;
feed_type_tokens(Tag,Tags,Tokens)->
  %io:format("~p~n",[hd(Tokens)]),
  case hd(Tokens) of
        "rss"-> rss_2;
        "rdf:RDF"-> rss_1;
        "feed"-> atom;
        "link"->
              case examine_link_for_rss(Tag) of
                embedded_rss-> embedded_rss;
                embedded_atom-> embedded_atom;
                no_rss-> feed_type(Tags);
                _-> feed_type(Tags)
              end;
        _-> feed_type(Tags)
      end.

feed_type([])-> not_a_feed;
feed_type([[]|Tags])-> feed_type(Tags);
feed_type([Tag|Tags])->
      Tokens = string:tokens(Tag++" "," \n"),
      feed_type_tokens(Tag,Tags,Tokens).

rss_classify(URL)->
    CHECK = cached_web:fetch("RSS","RAW",URL,URL, 10*60*60*24),
    Tags = string:tokens(CHECK,"<"),
    case feed_type(Tags) of
       rss_1-> rss_1;
       rss_2-> rss_2;
       atom->atom;
       embedded_rss-> embedded_rss;
       embedded_atom-> embedded_atom;
       _-> not_a_feed
    end.

classify(URL)-> 
    CleanURL = clean_url(URL),
    case CleanURL of
     {ok,CURL}->
        rss_classify(CURL);
     {error,Type}->
         {error, Type};  %need to parse page for links
     {not_supported,Type}->
         {not_supported,Type}
    end.





feed_link_rss_tokens(_Tag,_Tags,[])-> {error,not_a_feed};
feed_link_rss_tokens(Tag,Tags,Tokens)->
      case hd(Tokens) of
        "link"->
              case examine_link_for_rss(Tag) of
                embedded_rss-> {ok, extract_rss_link(Tag)};
                embedded_atom-> {not_supported, embedded_atom};
                no_rss-> feed_link_rss(Tags);
                _-> feed_link_rss(Tags)
              end;
        _-> feed_link_rss(Tags)
      end.


feed_link_rss([])-> {error,not_a_feed};
feed_link_rss([Tag|Tags])->
      Tokens = string:tokens(Tag," \n"),
      feed_link_rss_tokens(Tag,Tags,Tokens).


clean_url(URL)->
    CHECK = cached_web:fetch("RSS","RAW",URL,URL, 10*60*60*24),
    Tags = string:tokens(CHECK,"<"),
    case feed_type(Tags) of
       rss_2-> {ok,URL};
       rss_1-> {ok,URL};
       atom-> {ok,URL};
       embedded_rss-> feed_link_rss(Tags);
       embedded_atom-> {not_supported,embedded_atom};
       _-> {error,not_a_feed}
    end.

empty_channel()->"<?xml version=\"1.0\" encoding=\"utf-8\"?><channel />".

raw(URL)-> cached_web:fetch("RSS","FEED",URL,URL, 10*60*60*24) .

xml({ok,URL})->xml(URL);
xml({error,_URL})-> empty_channel();
xml({not_supported,_URL})-> empty_channel();
xml(URL)->
    CleanURL = clean_url(URL),
    case CleanURL of
     {ok,CURL}->
        raw(CURL);
     {error,Type}->
         {error, Type};  %need to parse page for links
     {not_supported,Type}->
         {not_supported,Type}
    end.

xml_list(XML)-> xml_list(XML,[]).
xml_list([],Docs)-> Docs;
xml_list(XML,Docs)->
               try xmerl_scan:string(XML) of
                         {Doc,Misc}->
                             xml_list(Misc,[Doc|Docs]);
                         _->
                             xml_list([],Docs)
                      catch
                         throw:_X->#xmlElement{};
                         fatal:_X->#xmlElement{};
                         exit:_X->#xmlElement{};
                         error:_X->#xmlElement{}
                      end.


locate_feed(Class,[Doc|DocList],ItemsList,Header)->
    DocItems = xmerl_util:getElementsByTagName(Doc#xmlElement.content,item),
    Title = xmerl_util:extractSingleTextValue(Doc,'title'),
    case Title of
      []->     locate_feed(Class,DocList,[DocItems|ItemsList],Header);
      Title->
            Link = xmerl_util:extractSingleTextValue(Doc,'link'),
            Description = xmerl_util:extractSingleTextValue(Doc,'description'),
            Language = xmerl_util:extractSingleTextValue(Doc,'language'),
            PubDate = xmerl_util:extractSingleTextValue(Doc,'pubDate'),
            ThisHeader =  {Title,Link,Description,Language,PubDate},
            locate_feed(Class,DocList,[DocItems|ItemsList],[ThisHeader|Header])
    end;
locate_feed(rss_2,[],ItemsList,Header)->     Items = lists:merge(ItemsList),
    {Header,
     [
     {
      xmerl_util:extractSingleTextValue(Item,'title'),
      xmerl_util:extractSingleTextValue(Item,'guid'),
      xmerl_util:extractSingleTextValue(Item,'link'),
      xmerl_util:extractSingleTextValue(Item,'author'),
      xmerl_util:extractSingleTextValue(Item,'image'),
      xmerl_util:extractSingleTextValue(Item,'pubDate'),
      xmerl_util:extractSingleTextValue(Item,'category'),
      xmerl_util:extractSingleTextValue(Item,'comments'),
      xmerl_util:extractSingleTextValue(Item,'description')   }
      || Item <- Items]
    };
locate_feed(rss_1,[],ItemsList,Header)->
    Items = lists:merge(ItemsList),
    {Header,
     [
     {
      xmerl_util:extractSingleTextValue(Item,'title'),
      xmerl_util:extractSingleTextValue(Item,'link'),
      xmerl_util:extractSingleTextValue(Item,'link'),
      xmerl_util:extractSingleTextValue(Item,'dc:creator'),
      xmerl_util:extractSingleTextValue(Item,'image'), 
      xmerl_util:extractSingleTextValue(Item,'dc:date'),
      xmerl_util:extractSingleTextValue(Item,'subject'),
      xmerl_util:extractSingleTextValue(Item,'comments'),
      xmerl_util:extractSingleTextValue(Item,'description')   }
      || Item <- Items]
    }.


%% @doc Get RSS feed of a url
%% @spec data(URL) -> List
%% URLs = [term()]

data(URL)->
     Class = classify(URL),
     XML = xml(URL),
     DocList = xml_list(XML),
     locate_feed(Class,DocList,[],[]).


%% @doc Take list of RSS Results and convert o RSSML(homebrewed) format
%% @spec rsslist_to_rssml(RSSList::list(), RSSMLFile::filename()) -> ok

rsslist_to_rssml(RSSList, RSSMLFile) ->
    lists:map(fun(RSSURL) -> result_to_rssml(rss:data(RSSURL), RSSMLFile) end, RSSList).

result_to_rssml(Result, RSSMLFile) ->
    {_MetaInfo, Feed} = Result,
    lists:map(fun(Post) -> write_rssml(Post, RSSMLFile) end, Feed), 
    ok.

write_rssml(Post, RSSMLFile) ->
    io:format("Trying to match~n"),
    {Title, Author, _Link, _, Link, _Date, _, _, Body} = Post,
    io:format("Got a match~n"),
    file:write_file(RSSMLFile, io_lib:format("<DOC>~n<TITLE>~s</TITLE>~n<AUTHOR>~s</AUTHOR>~n<LINK>~s</LINK>~n<TEXT>~n~n~s~n~n</TEXT>~n</DOC>~n", [Title, Author, Link, Body]), [append]).
