%%%-------------------------------------------------------------------
%%% File    : element_chatbox.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 29 Jun 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(element_chatbox).
-compile(export_all).
-include_lib("nitrogen/include/wf.inc").
-include("elements.inc").

render(ControlId, _R) ->
    Content = wf:render([#chatboxfeed{id=chatboxfeed}]),
    wf_tags:emit_tag('div', Content, [
                                      {id, ControlId},
                                      {class, ["scroll-pane"]},
                                      {style, ["height: 435px; width: 435px;"]}
                                     ]).
  
