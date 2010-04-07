%%%-------------------------------------------------------------------
%%% File    : element_chatbox.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 29 Jun 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(element_chatbox_message_wrapper).
-compile(export_all).
-include_lib("nitrogen/include/wf.inc").
-include("elements.inc").

render(ControlId, R) ->
    wf_tags:emit_tag('div', [#chatbox_message{body=R#chatbox_message_wrapper.body, email=R#chatbox_message_wrapper.email, user=R#chatbox_message_wrapper.user}], [{id, ControlId},{class, ["chatbox_message_wrapper"]}]).

