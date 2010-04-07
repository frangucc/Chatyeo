-module(web_wayfinder).
-include_lib("nitrogen/include/wf.inc").
%-include_lib("include/config.hrl").
-compile(export_all).

main() -> 
	#template {file="./wwwroot/wayfair/wayfinder.html"}.

