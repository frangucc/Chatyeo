%%%-------------------------------------------------------------------
%%% File    : element_chatbox.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 29 Jun 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(element_chatboxfeed).
-compile(export_all).
-include_lib("nitrogen/include/wf.inc").
-include("elements.inc").

render(ControlId, _R) ->
    wf_tags:emit_tag('div', [], [
                                      {id, ControlId},
                                      {class, ["chatbox_feed"]}
                                     ]).

