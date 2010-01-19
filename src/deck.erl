-module(deck).

-export([shuffled/0, cards/0, draw/2]).

-define (SUITS, [hearts, diamonds, clubs, spades]).
-define (CARDS, [{2,2},{3,3},{4,4},{5,5},{6,6},{7,7},{8,8},{9,9},{10,10}, {jack,10}, {queen,10}, {king, 10}, {ace, 11}]).

shuffled() ->
	{ok, Cards} = cards(),
	{ok, shuffle(Cards)}.

cards() ->
	[{S, C, V} || S <- ?SUITS, {C, V} <- ?CARDS].

draw(N, [Card|Deck]) ->
	draw(N - 1, [Card], Deck).
draw(0, Drawn, Deck) ->
	{ok, Drawn, Deck};
draw(N, Drawn, [Card|Deck]) ->
	draw(N - 1, [Card|Drawn], Deck).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

shuffle(List) ->
	%% Determine the log n portion then randomize the list.
	random:seed(now()),
	randomize(round(math:log(length(List)) + 0.5), List).

randomize(1, List) ->
 	randomize(List);
randomize(T, List) ->
	randomize(randomize(T - 1, List)).

randomize(List) ->
 	Zipped = lists:map(fun(A) -> {random:uniform(), A} end, List),
  	{_Rands, D} = lists:unzip(lists:keysort(1, Zipped)), 
  	D.