-module(html).
-export([encode/1]).

encode([],EndHtml) ->
	EndHtml;
encode([Ch|StartHtml],EndHtml) ->
	if 
		Ch==hd("<") -> encode(StartHtml,lists:append([lists:reverse("&lt;"),EndHtml]));
		Ch==hd(">") -> encode(StartHtml,lists:append([lists:reverse("&gt;"),EndHtml]));
		true -> encode(StartHtml,[Ch|EndHtml])
	end.
encode(Html) ->
	lists:reverse(encode(Html,[])).
