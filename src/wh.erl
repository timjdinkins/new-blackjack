%%
% Web Helper
% Common functions for packaging up messages to send back in an http response,
% as well as functions for getting name params from a query string and cleaning
% up the path in a URL.
%%
-module(wh).

-export([get_param/2, clean_path/1, ltb/1, atl/1, atb/1]).
-export([json_cards/1, enc/2, enc/3]).

get_param(Req, ValName) ->
	proplists:get_value(ValName, Req:parse_qs()).
	
clean_path(Path) ->
	case string:str(Path, "?") of
		0 ->
			Path;
		N ->
			string:substr(Path, 1, string:len(Path) - (N + 1))
	end.

ltb(Val) ->
	list_to_binary(Val).

atl(Val) ->
	atom_to_list(Val).

% atom_to_binary
atb(Val) ->
	ltb(atl(Val)).

enc_card(V) when is_integer(V) ->
	V;
enc_card(V) ->
	atb(V).

json_cards(Cards) ->
	[{obj, [{Suit, enc_card(Card)}]} || {Suit, Card, _Val} <- Cards].

enc(K, V, Ms) ->
	L = integer_to_list(length(Ms)),
	SVal =  case V of
						V when is_list(V) -> web_helper:ltb(V);
					 	_Else -> V
				 	end,
	[{"update" ++ L, {obj, [{K, SVal}]}} | Ms].

enc(List, Ms) ->
	L = integer_to_list(length(Ms)),
	[{"update" ++ L, {obj, List}}].