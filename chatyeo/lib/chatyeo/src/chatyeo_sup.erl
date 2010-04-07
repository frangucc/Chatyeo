%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan@defyned.com>
%%% @doc
%%% @end
%%% @copyright 2009 defyned
%%%----------------------------------------------------------------
-module(chatyeo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, jabber_logout/2,
         jabber_login/1, room_sup_start/1]).

%% Supervisor callbacks
-export([init/1]).

-include("config.hrl").

-define(SERVER, ?MODULE).
-define(USER_JABBER_PASSWORD, "temp4now").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
jabber_login (UserName) ->
    case supervisor:start_child (?SERVER, {{user_sup, UserName}, {user_simple_one_for_one, start_link, []}, transient, 2000, supervisor, [user_simple_one_for_one]}) of
        {ok, Pid} ->
            %{ok, Pid} = user_simple_one_for_one:start_link(),
            user_server:start(Pid, UserName, ?USER_JABBER_PASSWORD);
        _ ->
            ?INFO_MSG("Unable to start ~p supervisor", [UserName])
    end.

%% kill_user_supervisor(UserName) ->
%%     case supervisor:terminate_child(?SERVER, {user_sup, UserName}) of
%%         ok ->
%%             supervisor:delete_child(?SERVER, {user_sup, UserName});
%%         {error, Error} ->
%%             ?INFO_MSG("Unable to kill ~p supervisor: ~p", [UserName, Error])
%%     end.            

%% jabber_anon_login (UserName) ->
%%     {ok, Pid} = user_simple_one_for_one:start_link(),
%%     user_server:start(Pid, UserName, "").

jabber_logout (Pid, UserName) ->
    user_server:logout (Pid),
    case supervisor:terminate_child(?SERVER, {user_sup, UserName}) of
        ok ->
            supervisor:delete_child(?SERVER, {user_sup, UserName});
        {error, Error} ->
            ?INFO_MSG("Unable to kill ~p supervisor: ~p", [UserName, Error])
    end.            

room_sup_start(Room) ->
    supervisor:start_child (?SERVER, {{room_sup, Room}, {bot_simple_one_for_one, start_link, []}, transient, 2000, supervisor, [bot_simple_one_for_one]}).

%% kill_room_supervisor(Room) ->
%%     case supervisor:terminate_child(?SERVER, {room_sup, Room}) of
%%         ok ->
%%            supervisor:delete_child(?SERVER, {room_sup, Room});
%%         {error, Error} ->
%%             ?INFO_MSG("Unable to kill ~p supervisor: ~p", [Room, Error])
%%    end.            

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, {SupFlags, [{storage, {storage, start_link, []},
                      Restart, Shutdown, Type, [storage]}]}}. 

%%%===================================================================
%%% Internal functions
%%%===================================================================


