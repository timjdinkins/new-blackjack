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
-export([enc/2, enc_initial_state/1, enc_cards/1]).

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

enc_obj(Ts) ->
	{struct, enc_value(Ts)}.

enc_value({cards, V}) ->
	{cards, enc_cards(V)};
enc_value({K, [{_K, _V}|_T]=Obj}) ->
	{K, enc_obj(Obj)};
enc_value({K, V}) ->
	{K, enc_value(V)};
enc_value([H|T]) when is_tuple(H) ->
	[enc_value(H)|enc_value(T)];
enc_value([]) -> [];
enc_value(V) when is_list(V) ->
	ltb(V);
enc_value(V) when is_atom(V) ->
	atb(V);
enc_value(V) -> V.

enc_cards(Cards) ->
	[{struct, [{suit, Suit}, {val, enc_card(Card)}]} || {Suit, Card, _Val} <- Cards].

enc_card(V) when is_integer(V) ->
	V;
enc_card(V) ->
	atb(V).

%%
% enc(display, [{...}, {...}])
% Type = display | state | initial_state | reg | error
% Msgs = [tuple()] | tuple()
%%
enc(Type, [_H|_T]=Msgs) ->
	enc_obj([{type, Type}| Msgs]);
enc(Type, Msg) ->
	enc_obj([{type, Type}, Msg]).

enc_initial_state(Seats) ->
	Ss = [{struct, [{seat, Sn}, {name, ltb(Nm)}, {stack, Stk}]} || {Sn, _P, #seat{name=Nm, stack=Stk}} <- Seats],
	{struct, [{type, state}, {table, reset}, {seats, Ss}]}.