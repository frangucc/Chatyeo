-ifndef(chatyeo_config_hrl).
-define(chatyeo_config_hrl, ok).

-define(RESULT_NS, "http://www.chatyeo.com/2009/Results").
-define(QUESTION_NS, "http://www.chatyeo.com/2009/Question").

-define(IMAGES_JS, <<"client.onImages">>).
-define(TWEET_JS, <<"client.onTwitter">>).
-define(TEXT_JS, <<"client.onTextFindings">>).
-define(YAHOOANSWERS_JS, <<"client.onYahooAnswers">>).
-define(VIDEO_JS, <<"client.onVideos">>).
-define(BOOK_JS, <<"client.onBooks">>).
-define(WAYFAIR_JS, <<"client.onWayFair">>).
-define(DOCS_JS, <<"client.onDocs">>).
-define(SITES_JS, <<"client.onSites">>).
-define(TOPIC_JS, <<"client.onTopic">>).

-define(calltimeout, 10000).

-define(MUC, <<"http://jabber.org/protocol/muc">>).
-define(PUBSUB, <<"http://jabber.org/protocol/pubsub">>).
-define(PUBSUB_OWNER, <<"http://jabber.org/protocol/pubsub#owner">>).
-define(JABBER_HOST, "localhost").
-define(JABBER_MUC_HOST, "conference.localhost").
-define(JABBER_PUBSUB_HOST, <<"pubsub.localhost">>).
-define(JABBER_PORT, 5222).
-define(ATOM, <<"http://www.w3.org/2005/Atom">>).
 
-define(ERROR_MSG(Format, Args),                
        error_logger:error_msg ("E(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).
                                                                                                        
-define(INFO_MSG(Format, Args),
        error_logger:info_msg ("I(~p:~p:~p): "++Format++"~n", [self(),?MODULE,?LINE]++Args)).

-define(CONTENT_DIR, "/var/www/content").

-define(GOOGLE_URL, "ajax.googleapis.com/ajax/services/search").
-define(IMAGE_TYPE, "images").
-define(WEB_TYPE, "web").
-define(VIDEO_TYPE, "video").
-define(VERSION, "1.0").
-define(REFERER, "http://www.my-ajax-site.com").

%% Indicies for tuple returned by TopicDetectorServer because im not allowed to create records
-define(CT_WIKI_ID, 1).
-define(CT_NAME, 2).
-define(CT_WEIGHT, 3).
-define(CT_REL_TO_CTXT, 4).
-define(CT_REL_TO_OTHER, 5).
-define(CT_GENERALITY, 6).

-endif.
