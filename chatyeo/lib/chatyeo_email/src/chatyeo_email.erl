%%%-------------------------------------------------------------------
%%% File    : erlgmail.erl
%%% @author  Russell Brown <russell@ossme.net>
%%% @doc  Q n D gen_server email sender using gmail. See the send/n functions below. See the configuration section of the overview for profiles and configuration information.
%%% @reference <a href="http://21ccw.blogspot.com/2009/05/how-to-send-email-via-gmail-using.html">Benjamin Nortier's 21st C code works blog post </a>
%%% @version 1
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(chatyeo_email).

-behaviour(gen_server).

%% API
-export([start_link/0, send/2, send/3, send/4, psend/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {socket, host, port, username, password, from, to, header_to}).
        
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------  
%% @doc Sends an email with subject Subject and message body Body to the recepients configured in the default profile. Uses the default profile.
%% @spec send(Subject::string(), Body::string()) -> ok
%% @equiv erlgmail:send("Subject", "Body", default)
%% @end
%%--------------------------------------------------------------------
send(Subject, Body) ->
    psend(Subject, Body, [], []).

%%--------------------------------------------------------------------
%% @doc Sends an email with subject Subject and message body Body to the email address To
%% @spec send(Subject::string(), Body::string(), To::email()) -> ok
%% @type email() = string(). An email address (there is no parameter validation)
%% @end
%%--------------------------------------------------------------------
send(Subject, Body, To) when is_list(To) ->
    psend(Subject, Body, To, []).

%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recipients in To with the display names in HeaderTo using the default profile
%%--------------------------------------------------------------------
send(Subject, Body, To, HeaderTo) ->
    psend(Subject, Body, To, HeaderTo).

%%--------------------------------------------------------------------
%% Function: send  
%% Description: sends the  mail to recipients in To using with display names in HeaderTo using Profile
%%--------------------------------------------------------------------
psend(Subject, Body, To, HeaderTo) ->
    gen_server:cast(?SERVER, {mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, HeaderTo}}, 0}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, Host} = gas:get_env(chatyeo_email, host, []),
    {ok, Port} = gas:get_env(chatyeo_email, port, []),
    {ok, UserName} = gas:get_env(chatyeo_email, username, []),
    {ok, Password} = gas:get_env(chatyeo_email, password, []),
    {ok, From} = gas:get_env(chatyeo_email, from, []),
    {ok, To} = gas:get_env(chatyeo_email, to, []),
    {ok, HeaderTo} = gas:get_env(chatyeo_email, header_to, []),
    
    Socket = nil,%new_smtp:connect({config, Host, Port, UserName, Password}),
    
    {ok, #state{socket=Socket, host=Host, port=Port, username=UserName, password=Password, from=From, to=To, header_to=HeaderTo}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({mail, Message, Times}, State) when Times > 3 ->
    %% Log it, move on
    error_logger:error_msg("Failed to send message ~p~n", [Message]),
    {noreply, State};
handle_cast({mail, {email, {subject, Subject}, {body, Body}, {to, To}, {header_to, HeaderTo}}=Message, Times}, State) ->
    #state {
             socket    = Socket,
             username  = UserName,
             to        = DefaultTo,
             header_to = DefaultHeaderTo,
             port      = Port,
             host      = Host,
             password  = Password,
             from      = From
            } = State,        
    
    Recipient = case To of
                    [] -> DefaultTo;
                    _ -> To
                end,

    HeaderRecipient = case HeaderTo of
                          [] -> DefaultHeaderTo;
                          _ -> HeaderTo
                      end,
    
    try new_smtp:send(Socket,  {message, Recipient, HeaderRecipient, From, Subject, Body}) of
        Socket ->
            {noreply, State}
    catch
        exit:_ ->
            S = new_smtp:connect({config, Host, Port, UserName, Password}),
            handle_cast({mail, Message, Times+1}, State#state{socket=S})
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @spec terminate(Reason::term(), State::state()) -> ok
%% @doc Politely says bye to gmail for each Socket and closes it
%% @type state() = {sockets(), configs()}
%% @type sockets() = dict(profile(), socket()). A dictionary using profile() as key and a socket() as value
%% @type configs() = dict(profile(), config()). A dictionary using profile() as key and a config() as value
%% @type config() = {config, {host, port, username, password, from, to, header_to}}. The config record defined in config.hrl
%% @type socket() = term(). http socket
%% @type dict(). An Erlang dictionary (see dict)
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    new_smtp:disconnect(State#state.socket).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


