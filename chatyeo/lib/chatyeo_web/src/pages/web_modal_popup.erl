-module (web_modal_popup).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file=nitrogen:get_wwwroot()++"/modal_popup.html"}.

title() ->
	"web_modal_popup".

body() ->
	#label{text="web_modal_popup body."}.
	
event(_) -> ok.
