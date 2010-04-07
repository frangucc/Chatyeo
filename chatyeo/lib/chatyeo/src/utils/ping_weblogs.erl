-module(ping_weblogs).
-export([init/0,check/0,extract/1,feed/1,data_basic/1]).
-include_lib("xmerl/include/xmerl.hrl").


init() ->
    inets:start(),
    results_cache:init().

check()->
  URL="http://rpc.weblogs.com/shortChanges.xml",
  ChangesXML = cached_web:fetch("Weblogs.com","ShortChanges","",URL, 60*60*24), %change to 5 minutes.
  {Doc,_Misc} = xmerl_scan:string(ChangesXML),
  Items = xmerl_util:getElementsByTagName(Doc,'weblog'),
  [ xmerl_util:extractAttribute(Item#xmlElement.attributes,url) || Item <- Items].
 

grab_feed(_Type,[])-> "";
grab_feed(_Type,URL)->
  rss:data(URL),
  URL.
 
feed([])->[]; 
feed(URL)-> 
    CleanURL = rss:clean_url(URL), 
    grab_feed(rss:classify(CleanURL),CleanURL).

extract(URLList)->
      plists:map(fun(URL)->
                      io:format("Downloading:{~p} ~n",[URL]),
   		      try feed(URL) of
                         XML->XML
                      catch
                         throw:_X->ok;
                         exit:_X->ok;
                         error:_X->ok
                      end 
                      end, URLList ,{processes, 5}).  %10 threads? right? original used all nodes {nodes,[node()|nodes()]}

imageOrRssImage([])-> "rss.png";
imageOrRssImage(Image)-> Image.

guidOrLink([],Link)->Link;
guidOrLink(Guid,_Link)->Guid.

data_basic(ExtractedRSSFeeds)->
     [ {
       "RSS",
       guidOrLink(Guid,Link),
       Link,
       "<b>"++PubDate++"</b> "++Title,
       imageOrRssImage(Image),
       Description
       } ||      {
      Title,
      Guid,
      Image,
      Link,
      PubDate,
      _Category,
      _CommentsLink,
      Description   } <- ExtractedRSSFeeds].
      
      
