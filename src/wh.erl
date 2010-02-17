%%
% Web Helper
% Common functions for packaging up messages to send back in an http response,
% as well as functions for getting name params from a query string and cleaning
% up the path in a URL.
%%
-module(wh).

-include_lib("game.hrl").

-export([get_param/2, clean_path/1, ltb/1, atl/1, atb/1]).
-export([generate_sid/2, bin_to_hexstr/1]).
-export([json_cards/1, enc/2, enc/3]).
-export([enc_msg/1, enc_msg/2, enc_error/1, enc_error/2, enc_new_cards/3]).
-export([enc_bust/2, enc_result/1, enc_result/4, enc_update/1]).
-export([enc_dealer_msg/2, enc_initial_state/1, enc_registered/1]).

get_param(Req, ValName) ->
	proplists:get_value(ValName, Req:parse_post()).
	
clean_path(Path) ->
	case string:str(Path, "?") of
		0 ->
			Path;
		N ->
			string:substr(Path, 1, string:len(Path) - (N + 1))
	end.

generate_sid(Name, IP) ->
	Hash = crypto:sha_mac([Name, IP], "blackjack"),
	bin_to_hexstr(Hash).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

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
	[{obj, [{suit, Suit}, {val, enc_card(Card)}]} || {Suit, Card, _Val} <- Cards].

json_obj(List) ->
	{obj, [List]}.

enc(K, V, Ms) ->
	SVal =  case V of
						V when is_list(V) -> web_helper:ltb(V);
					 	_Else -> V
				 	end,
	[json_obj([{K, SVal}]) | Ms].

enc(List, Ms) ->
	[json_obj(List)|Ms].

enc_msg(Str) ->
	[{obj, [{type, <<"msg">>}, {val, ltb(Str)}]}].

enc_msg(Str, Ms) ->
	[{obj, [{type, <<"msg">>}, {val, ltb(Str)}]} | Ms].

% These messages are broadcast to all players at the table so they can all see what
% the dealer is saying to a player.
enc_dealer_msg(Seat, Str) ->
	[{obj, [{type, <<"dealer-msg">>}, {seat, Seat}, {val, ltb(Str)}]}].

enc_error(Str) ->
	[{obj, [{type, <<"error">>}, {val, ltb(Str)}]}].

enc_error(Str, Ms) ->
	[{obj, [{type, <<"error">>}, {val, ltb(Str)}]} | Ms].

enc_registered(Seat) ->
	[{obj, [{type, <<"registered">>}, {seat, Seat}]}].

enc_new_cards(Cs, Sc, Ms) ->
	[{obj, [{type, <<"state_update">>}, {cards, json_cards(Cs)}, {score, Sc}]} | Ms].

enc_bust(Score, Ms) ->
	[{obj, [{type, <<"bust">>}, {score, Score}]} | Ms].

enc_result(L) ->
	[{obj, [{type, <<"result">>} | L]}].

enc_result(R, Amt, Stack, Ms) ->
	[{obj, [{type, <<"result">>}, {result, atb(R)}, {amt, Amt}, {stack, Stack}]} | Ms].

enc_update(L) ->
	[{obj, [{type, <<"table_update">>} | L]}].

enc_initial_state(Seats) ->
	Ss = [{obj, [{seat, Sn}, {name, wh:ltb(Nm)}, {stack, Stk}]} || {Sn, _P, #seat{name=Nm, stack=Stk}} <- Seats],
	[{obj, [{type, <<"initial_state">>}, {seats, Ss}]}].