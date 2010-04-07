%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(storage).

-behaviour(gen_server).

%% API
-export([start_link/0, add_item/3, expire_item/4, get_items/2, get_items/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).

-include("config.hrl").

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
add_item (Database, Name, Data) ->
    couchdb_server:add_item(Database, Name, Data).

expire_item (Database, Id, StartTime, EndTime) ->
    DatabaseName = string:to_lower(Database),
    {ok, Doc} = couchdb:retrieve_document(DatabaseName, Id),
    NewDoc = rfc4627:set_field(Doc, key, [StartTime, EndTime]),
    couchdb:update_doc(DatabaseName, Id, NewDoc).

get_items(Database, Time) ->
    case couchdb_server:run_view(Database, "items", "all", [{"startkey","[0,"++Time++"]"}, {"endkey", "["++Time++"]"}]) of
        {ok, Results, _} ->
            case rfc4627:get_field(Results, "rows") of
                {ok, Rows} ->
                    lists:map(fun(X) -> element(2, rfc4627:get_field(X, "value")) end, Rows);
                not_found ->
                    []
            end;
        _ ->
            []
    end.

get_items(Database, Start, End) ->
    case couchdb_server:run_view(Database, "items", "all", [{"startkey","[0,"++Start++"]"}, {"endkey", "["++End++"]"}]) of
        {ok, Results, _} ->
            case rfc4627:get_field(Results, "rows") of
                {ok, Rows} ->
                    lists:map(fun(X) -> element(2, rfc4627:get_field(X, "value")) end, Rows);
                not_found ->
                    []
            end;
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
create_dir_from_uuid (UUID) ->
    Path = ?CONTENT_DIR ++ "/" ++ re:replace(UUID, "-", "/", [global, {return, list}]),
    case filelib:is_file(Path) of
        true ->
            exists;
        false ->
            filelib:ensure_dir(Path),
            Path
    end.

file_name_from_uuid (UUID) ->
    hd(lists:reverse(re:split(UUID, "-", [{return, list}]))).

write_file_to_uuid (UUID, Binary) ->
    case create_dir_from_uuid(UUID) of
        exists ->
            UUID;
        Path ->
            file:write_file(Path, Binary),            
            UUID
    end.
