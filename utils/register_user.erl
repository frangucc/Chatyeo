%%%-------------------------------------------------------------------
%%% File    : register_user.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 27 May 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(register_user).
-compile(export_all).

run (Username, Password) ->
    application:start (chatyeo),
    mnesia:start(),
    crypto:start(),
    jabber_proxy_sup:start_link (create_user),
    jabber_proxy:register_user (create_user, Username, Password), 
    application:stop(chatyeo),
    mnesia:stop(),
    crypto:stop().
    
