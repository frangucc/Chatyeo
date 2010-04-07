%%%-------------------------------------------------------------------
%%% File    : element_chatbox.erl
%%% Author  : Tristan <tristan@kfgyeo>
%%% Description : 
%%%
%%% Created : 29 Jun 2009 by Tristan <tristan@kfgyeo>
%%%-------------------------------------------------------------------
-module(element_chatbox_image).
-compile(export_all).
-include_lib("nitrogen/include/wf.inc").
-include("elements.inc").

render(ControlId, R) ->
    wf_tags:emit_tag('div', [#gravatar{email=R#chatbox_image.email}], [
                                                                         {id, ControlId},
                                                                         {class, ["chatbox_image"]}
                                                                        ]).