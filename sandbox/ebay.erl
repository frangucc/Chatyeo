%%%-------------------------------------------------------------------
%%% File    : ebay.erl
%%% Author  : Jay<jdmundrawala@gmail.com>
%%% Description : 
%%%
%%% Created : 09 Oct 2009
%%%-------------------------------------------------------------------
%%% Example Usage:
%%% ebay:find_items_by_keywords(["egyptian", "hookah"]).
%%%

-module(ebay).
-export([
        find_items_by_keywords/1,
        find_items_by_keywords/2
    ]).


-define(BASE, "http://svcs.ebay.com/services/search/FindingService/v1?").
-define(SERVICE_VERSION, "&SERVICE-VERSION=1.0.0").
-define(SECURITY_APPNAME, "&SECURITY-APPNAME=JayMundr-6e0e-4bb5-8a03-8b4642489c05").
-define(RESPONSE_DATA_FORMAT, "&RESPONSE-DATA-FORMAT=JSON").
-define(REST_PAYLOAD, "&REST-PAYLOAD").
-define(FIND_ITEMS_BY_KEYWORDS, "OPERATION-NAME=findItemsByKeywords").

-define(INFO_MSG(Format, Args), error_logger:info_msg ("I(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).

%"searchResult":[{"@count":"100","item":[{"itemId":["390103747078"],"title":["Hookah FUNNEL Bowl EGYPTIAN Clay Large SHISHA Head"],"globalId":["EBAY-US"],"primaryCategory":[{"categoryId":["133"],"categoryName":["Other"]}],"galleryURL":["http:\/\/thumbs2.ebaystatic.com\/pict\/3901037470788080_1.jpg"],"viewItemURL":["http:\/\/cgi.ebay.com\/Hookah-FUNNEL-Bowl-EGYPTIAN-Clay-Large-SHISHA-Head_W0QQitemZ390103747078QQcmdZViewItemQQptZLH_DefaultDomain_0"],"paymentMethod":["PayPal","VisaMC","AmEx","Discover"],"autoPay":["false"],"postalCode":["92833"],"location":["Fullerton,CA,USA"],"country":["US"],"shippingInfo":[{"shippingServiceCost":[{"@currencyId":"USD","__value__":"4.0"}],"shippingType":["FlatDomesticCalculatedInternational"],"shipToLocations":["Worldwide"]}],"sellingStatus":[{"currentPrice":[{"@currencyId":"USD","__value__":"9.95"}],"convertedCurrentPrice":[{"@currencyId":"USD","__value__":"9.95"}],"sellingState":["Active"],"timeLeft":["P6DT0H59M51S"]}],"listingInfo":[{"bestOfferEnabled":["false"],"buyItNowAvailable":["false"],"startTime":["2009-10-08T18:49:27.000Z"],"endTime":["2009-10-15T18:49:27.000Z"],"listingType":["FixedPrice"],"gift":["false"]}]},{"itemId":["220487596515"],"title":["32\" EGYPTIAN 100% HAND MADE HOOKAH w\/GLASS BASE"],"globalId":["EBAY-US"],"subtitle":["Includes LOTS of accessories to get you started"],"primaryCategory":[{"categoryId":["112485"],"categoryName":["Hookahs & Water Pipes"]}],"galleryURL":["http:\/\/thumbs1.ebaystatic.com\/pict\/2204875965158080_1.jpg"],"viewItemURL":["http:\/\/cgi.ebay.com\/32-EGYPTIAN-100-HAND-MADE-HOOKAH-w-GLASS-BASE_W0QQitemZ220487596515QQcmdZViewItemQQptZLH_DefaultDomain_0"],"paymentMethod":["PayPal"],"autoPay":["false"],"postalCode":["75057"],"location":["Lewisville,TX,USA"],"country":["US"],"shippingInfo":[{"shippingServiceCost":[{"@currencyId":"USD","__value__":"0.0"}],"shippingType":["Free"],"shipToLocations":["US"]}],"sellingStatus":[{"currentPrice":[{"@currencyId":"USD","__value__":"39.0"}],"convertedCurrentPrice":[{"@currencyId":"USD","__value__":"39.0"}],"bidCount":["0"],"sellingState":["Active"],"timeLeft":["P0DT8H10M36S"]}],"listingInfo":[{"bestOfferEnabled":["false"],"buyItNowAvailable":["false"],"startTime":["2009-10-05T02:00:12.000Z"],"endTime":["2009-10-10T02:00:12.000Z"],"listingType":["Auction"],"gift":["false"]}]},{"itemId":["390103746956"],"title":["22\" 2-Hose GREEN ALIF Authentic Egyptian Hookah Shisha"],"globalId":["EBAY-US"],"primaryCategory":[{"categoryId":["10911"],"categoryName":["Egyptian"]}],"galleryURL":["http:\/\/thumbs3.ebaystatic.com\/pict\/3901037469568080_1.jpg"],"viewItemURL":["http:\/\/cgi.ebay.com\/22-2-Hose-GREEN-ALIF-Authentic-Egyptian-Hookah-Shisha_W0QQitemZ390103746956QQcmdZViewItemQQptZLH_DefaultDomain_0"],"paymentMethod":["CashOnPickup","PayPal","VisaMC","AmEx","Discover"],"autoPay":["false"],"postalCode":["92833"],"

%find_items_by_keywords expected output:
%"searchResult":[{
%   "@count":"100",
%       "item":[
%       {
%           "itemId":["390103747078"],
%           "title":["Hookah FUNNEL Bowl EGYPTIAN Clay Large SHISHA Head"],
%           "globalId":["EBAY-US"],
%           "primaryCategory":[{"categoryId":["133"],"categoryName":["Other"]}],
%           "galleryURL":["http:\/\/thumbs2.ebaystatic.com\/pict\/3901037470788080_1.jpg"],
%           "viewItemURL":["http:\/\/cgi.ebay.com\/Hookah-FUNNEL-Bowl-EGYPTIAN-Clay-Large-SHISHA-Head_W0QQitemZ390103747078QQcmdZViewItem
%           "paymentMethod":["PayPal","VisaMC","AmEx","Discover"],
%           "autoPay":["false"],
%           "postalCode":["92833"],
%           "location":["Fullerton,CA,USA"],
%           "country":["US"],
%            "shippingInfo":[{
%                "shippingServiceCost":[{"@currencyId":"USD","__value__":"4.0"}],
%                "shippingType":["FlatDomesticCalculatedInternational"],
%                "shipToLocations":["Worldwide"]
%            }],
%            "sellingStatus":[{"currentPrice":[{"@currencyId":"USD","__value__":"9.95"}],
%                "convertedCurrentPrice":[{"@currencyId":"USD","__value__":"9.95"}],
%                "sellingState":["Active"],
%                "timeLeft":["P6DT0H59M51S"]}],
%            "listingInfo":[{
%                "bestOfferEnabled":["false"],
%                "buyItNowAvailable":["false"],
%                "startTime":["2009-10-08T18:49:27.000Z"],
%                "endTime":["2009-10-15T18:49:27.000Z"],
%                "listingType":["FixedPrice"],
%                "gift":["false"]
%            }]
%        },
%        {
%            "itemId":["220487596515"],
%         .
%         .
%         .
find_items_by_keywords(Keywords) ->
    find_items_by_keywords(Keywords, []).
find_items_by_keywords(Keywords, Options) ->
    AccKeywords = lists:foldr(fun(X, AccIn) -> X  ++ " " ++ AccIn end,
        "", Keywords),
    AccOptions = lists:foldl(fun({X,Y}, AccIn) -> AccIn ++ "&" ++ X ++ "=" ++ ibrowse_lib:url_encode(Y) end,
        "", Options),
    Url = ?BASE ++ ?FIND_ITEMS_BY_KEYWORDS ++ ?SERVICE_VERSION ++ ?SECURITY_APPNAME ++
          ?RESPONSE_DATA_FORMAT ++ ?REST_PAYLOAD ++ "&keywords=" ++ ibrowse_lib:url_encode(AccKeywords) ++ AccOptions,
    ?INFO_MSG("ebay url: ~s~n" ,[Url]),
    {ok, _S, _ResponseHeader, Results} = ibrowse:send_req(Url, [], get),
    {ok, Json, []} = rfc4627:decode(Results),
    {ok, [OperationResult]} = rfc4627:get_field(Json, "findItemsByKeywordsResponse"),
    Ack = rfc4627:get_field(OperationResult, "ack"),

    case Ack of
        {ok, [<<"Success">>]} ->
            {ok, [Res]} = rfc4627:get_field(OperationResult, "searchResult"),
            Res;
        true ->
            []
    end
    .

