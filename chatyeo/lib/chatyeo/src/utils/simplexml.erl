-module(simplexml).
-import(lists).
-export([test_xml/0]).
 
leaf_nodes(Xml,_Result)->
    [ string:tokens(Item,">")   ||   Item <- string:tokens(Xml,"<")].
    
test_xml()-> 
    Xml="<tag>test<inside>up</inside></tag>", 
    leaf_nodes(Xml,[]).
