-module(amazon).
-export([init/0,product_basic/1,product_xml/1,recall_item/1,recall_basic/1]).  
-include_lib("xmerl/include/xmerl.hrl").

amazon_query_URL (Query)->
    "http://webservices.amazon.com/onca/xml?" ++
        "Service=AWSECommerceService" ++
        "&AWSAccessKeyId=1W4Y8GHZYHBSDER8Y2G2" ++ 
        "&Operation=ItemSearch" ++ 
        "&SearchIndex=All" ++ 
        "&ResponseGroup=Request,Large" ++ 
        "&Version=2005-10-13" ++ 
        "&t=dillyeo-20" ++ 
        "&tag=dillyeo-2" ++ 
        "&Keywords=" ++ edoc_lib:escape_uri(Query).
 
%% @doc Initialize amazon module, must be called before any other function
%% @spec init() -> ok

init() ->
    inets:start(),
    results_cache:init().

product_xml(Query)->
    cached_web:fetch("Amazon","ProductQuery",Query, amazon_query_URL(Query), 60*60*24).

extractItemContent(Item)->
    {xmlText,_Parent,_C,_ATTR,Content,text} = hd(Item#xmlElement.content),
    Content.

extractItemAttributes(Item)->
    xmerl_util:getElementsByTagName(Item,'ItemAttributes').

extractItemAttributeContent(Item,Tag)->
    Title = hd(xmerl_util:getElementsByTagName(extractItemAttributes(Item),Tag)),
    Content = extractItemContent(Title),
    Content.


extractItemChildContent(Item,Tag)->
    Title = hd(xmerl_util:getElementsByTagName(Item,Tag)),
    Content = extractItemContent(Title),
    Content.

extractLink(Item)-> extractItemChildContent(Item,'DetailPageURL').

extractTitle(Item)-> extractItemAttributeContent(Item,'Title').
extractASIN(Item)->extractItemChildContent(Item,'ASIN'). 

extractImageIfFound([])->"";
extractImageIfFound([Image|_Elems])-> extractItemChildContent(Image,'URL').

extractImage(Item)->
    Elems = xmerl_util:getElementsByTagName(Item,'MediumImage'),
    extractImageIfFound(Elems). 
 
cache_item({Source,Id,Link,Title,Image,Description})->
    results_cache:store(Source,"ITEM",Id,{Source,Id,Link,Title,Image,Description}),
    {Source,Id,Link,Title,Image,Description}.

%% @doc recall item from amazon cache
%% @spec recall_item(Id::integer()) -> string()

recall_item(Id)->    
     results_cache:recall("AMAZON","ITEM",Id,90*90*90*90).
 
%% @doc Recall result from amazon:product_basic/1 call, use same query as used for query
%% @spec recall_basic(Query::string()) -> string()

recall_basic(Query)->    
     results_cache:recall("AMAZON","RESULTSET",Query,60*60*24).

%% @doc Performs product query and stores results in cache
%% @spec product_basic(Query::string()) -> ok

product_basic(Query)->
    % {"AMAZON", ASIN, link, title, image, description}
    XML = product_xml(Query),
    {Doc,_Misc} = xmerl_scan:string(XML),
    Items = xmerl_util:getElementsByTagName(Doc,'Item'),
    Resultset = [ cache_item({
       "AMAZON",
       extractASIN(Item),
       extractLink(Item),
       extractTitle(Item),
       extractImage(Item),
       ""
      }) || Item <- Items],
      results_cache:store("AMAZON","RESULTSET",Query,Resultset),
    Resultset .
