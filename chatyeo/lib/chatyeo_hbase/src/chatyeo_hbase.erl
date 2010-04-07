%%%-------------------------------------------------------------------
%%% @author Tristan <tristan@kfgyeo>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(chatyeo_hbase).

-behaviour(gen_server).

%% API
-export([start_link/1, add_user_topic_of_interest/2, add_user_topics_of_interest/2, get_user_topics_of_interest/1, user_has_topic_of_interest/2, add_room_topic/2, get_room_topics/1, room_has_topic/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("config.hrl").
-include("hbase_thrift.hrl").
-include("hbase_types.hrl").
-include("hbase_constants.hrl").

-define(SERVER, ?MODULE). 

-record(state, {hbase}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Client) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Client], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

add_user_topic_of_interest(UserName, Topic) ->
    gen_server:cast(?SERVER, {add_user_topic_of_interest, UserName, Topic}).

add_user_topics_of_interest(UserName, Topics) ->
    gen_server:cast(?SERVER, {add_user_topics_of_interest, UserName, Topics}).

get_user_topics_of_interest(UserName) ->
    gen_server:cast(?SERVER, {get_user_topics_of_interest, UserName}).

user_has_topic_of_interest(UserName, Topic) -> 
    gen_server:call(?SERVER, {user_has_topic_of_interest, UserName, Topic}).

add_room_topic(RoomName, Topic) ->
    gen_server:cast(?SERVER, {add_room_topic, RoomName, Topic}).

get_room_topics(RoomName) ->
    gen_server:call(?SERVER, {get_room_topics, RoomName}).

room_has_topic(RoomName, Topic) ->
    gen_server:call(?SERVER, {room_has_topic, RoomName, Topic}).

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
init([Client]) ->
    try
        thrift_client:call(Client, createTable, [?HBASE_USER_TABLE, [#columnDescriptor{name="topic:"}]])
    catch
         throw:Reason ->
            ?INFO_MSG ("Thrift table ~p couldn't be created because of ~p", [?HBASE_USER_TABLE, Reason])
    end,   

    try
        thrift_client:call(Client, createTable, [?HBASE_ROOM_TABLE, [#columnDescriptor{name="topic:"}]])
    catch
        throw:Reason2 ->
            ?INFO_MSG ("Thrift table ~p couldn't be created because of ~p", [?HBASE_ROOM_TABLE, Reason2])
    end,
    
    try         
        thrift_client:call(Client, createTable, [?HBASE_MESSAGES_TABLE, [#columnDescriptor{name="room:"},
                                                                         #columnDescriptor{name="message:"},
                                                                         #columnDescriptor{name="user:"}]])
    catch
        throw:Reason3 ->
            ?INFO_MSG ("Thrift table ~p couldn't be created because of ~p", [?HBASE_MESSAGES_TABLE, Reason3])
    end,

    {ok, #state{hbase=Client}}.

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
handle_call({get_user_topics_of_interest, UserName}, _From, State) ->
    Reply = hbase_users:get_user_topics_of_interest(State#state.hbase, UserName),    
    {reply, Reply, State};
handle_call({user_has_topic_of_interest, UserName, Topic}, _From, State) ->
    Reply = hbase_users:user_has_topic_of_interest(State#state.hbase, UserName, Topic),   
    {reply, Reply, State};
handle_call({get_room_topics, RoomName}, _From, State) ->
    Reply = hbase_rooms:get_room_topics(State#state.hbase, RoomName),
    {reply, Reply, State};
handle_call({room_has_topic, RoomName, Topic}, _From, State) ->
    Reply = hbase_rooms:room_has_topic(State#state.hbase, RoomName, Topic),
    {reply, Reply, State}.

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
handle_cast({add_user_topic_of_interest, UserName, Topic}, State) ->
    hbase_users:add_user_topic_of_interest(State#state.hbase, UserName, Topic),
    {noreply, State};
handle_cast({add_user_topics_of_interest, UserName, Topics}, State) ->
    lists:foreach(fun(Topic) ->
                          hbase_users:add_user_topic_of_interest(State#state.hbase, UserName, Topic)
                  end, Topics),                          
    {noreply, State};
handle_cast({add_room_topic, RoomName, Topic}, State) ->
    hbase_rooms:add_room_topic(State#state.hbase, RoomName, Topic),
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

%%--------------------------------------------------------------------
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
