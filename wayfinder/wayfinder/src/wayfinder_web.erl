%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Web server for wayfinder.

-module(wayfinder_web).
-author('author <author@example.com>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) -> 
    application:start(inets),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                _ -> 
                   ProxiedURL = "http://www.defyned.com/"++ Path,
                   io:format("~p~n",[ProxiedURL]),
                   {ContentType,Doc} = makeProxiedRequest(ProxiedURL,[]),
                 %  io:format("~p~n",[ContentType]),
                   Req:ok(handle_output(ContentType, Doc))
            end;
        'POST' ->
            case Path of
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

handle_output("text/html; charset=utf-8",Doc)->{"text/html; charset=utf-8",insert_header(binary_to_list(Doc))};
handle_output("text/html",Doc)->{"text/html",insert_header(binary_to_list(Doc))};
handle_output(ContentType,Doc)->{ContentType,Doc}.




locate_body([Tag|Tags],Rebuilt)->
     InsertSeg = "div style=\"height:50px;background:#000;color:#fff;\"> Chat this Back! </div>", 
   %  
     case lists:sublist(Tag,1,4) of
        "body"->  locate_body(Tags,[Tag|[InsertSeg|Rebuilt]]);
        _ -> locate_body(Tags,[Tag|Rebuilt])
     end; 
locate_body([],Rebuilt)-> lists:reverse(Rebuilt).

string_join(Items, Sep) ->
    lists:flatten(lists:reverse(string_join1(Items, Sep, []))).
string_join1([Head | []], _Sep, Acc) ->
    [Head | Acc];
string_join1([Head | Tail], Sep, Acc) ->
    string_join1(Tail, Sep, [Sep, Head | Acc]).

       

insert_header(Body)->
           Tags = string:tokens(Body,"<"),
           Inserted = locate_body(Tags,[]),
           io:format("~p~n",[Inserted]),
           string_join(Inserted,$<).  



locate_content_type([],Found)-> Found;
locate_content_type([{"content-type",ContentType}|Headers],Found)-> locate_content_type(Headers,ContentType);
locate_content_type([Header|Headers],Found)-> locate_content_type(Headers,Found).
 

makeProxiedRequest(ProxiedURL,Headers)->
                    Headers = [], 
                    HTTPOptions=[],
                    Options=[], 
                    TokenProxied = string:tokens(ProxiedURL," "),
                    EscapedSpacesURL = string:join(TokenProxied,"%20"),
                    Response=http:request(get,{EscapedSpacesURL,Headers}, HTTPOptions, [{body_format, binary}]), 
                  case Response of 
                      { ok, {_Status, ProxiedHeaders, Body }}-> 
                               %io:format("~p~n",[ProxiedHeaders]), 
                               {locate_content_type(ProxiedHeaders,[]),Body};
                      Err-> io:format("~p~n",[Err]), {"text/html","Error."}
                  end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


