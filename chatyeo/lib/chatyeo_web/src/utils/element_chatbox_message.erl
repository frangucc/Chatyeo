%%%-------------------------------------------------------------------
%%% File    : element_chatbox.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 29 Jun 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(element_chatbox_message).
-compile(export_all).
-include_lib("nitrogen/include/wf.inc").
-include("elements.inc").

render(ControlId, R) ->
    wf_tags:emit_tag('div', [#chatbox_image{email=R#chatbox_message.email}, #span{class="message_username", text=R#chatbox_message.user++": "}, R#chatbox_message.body], [{id, ControlId}, {class, ["chatbox_message"]}]).

