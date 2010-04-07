-module (web_display).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> #template { file="./wwwroot/display.html", bindings=[
	{'Group', learn},
	{'Item', samples}
]}.

title() -> "Chatyeo".
headline() -> "Chatyeo".
 
param_Source(Args)->
   case lists:keysearch("source",1,Args) of
     false -> [];
     {value,{"source",Source}} -> Source
   end.


param_ID(Args)->
   case lists:keysearch("id",1,Args) of
     false -> [];
     {value,{"id",ID}} -> ID
   end.

body() ->
    Args = wf_platform:parse_get_args(),
    Source = param_Source(Args),
    ID = param_ID(Args), 
    amazon:init(), 
     {Source,_Id,Link,Title,Image,_Description} = amazon:recall_item(ID),
    Body=[    
              #h1{text=Title},#p{}, #link{ url=Link, body=[#image{image=Image}] } , #p{}, ID
             ],	
    wf:render(Body).
	 
event(_) -> ok.
 
