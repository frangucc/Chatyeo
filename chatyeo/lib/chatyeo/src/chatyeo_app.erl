%%%----------------------------------------------------------------
%%% @author Tristan Sloughter <tristan@defyned.com>
%%% @copyright (C) 2009, Tristan
%%% @doc
%%%
%%% @end
%%% @copyright 2009 defyned
%%%----------------------------------------------------------------,
-module(chatyeo_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("config.hrl").

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    ets:new(room_supervisor_table, [public, named_table]),
    randoms:start(),
    case chatyeo_sup:start_link() of
        {ok, Pid} ->
            jabber_utils:jabber_register("_create_room_nodes_user", "temp4now"),
            db_interface:cleanup_user_room(),
            db_interface:cleanup_rooms(),
            {ok, Pid};
        Error ->
            ?ERROR_MSG ("Chatyeo could not start: ~p~n", [Error]),
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

