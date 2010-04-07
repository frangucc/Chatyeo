-module (web_modal_timeline).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file=nitrogen:get_wwwroot()++"/modal_timeline.html"}.

title() ->
	"web_modal_timeline".

body() ->
	#label{text="web_modal_timeline body."}.
	
event(_) -> ok.
