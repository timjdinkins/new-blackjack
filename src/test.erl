-module(test).

-export([setup/0]).

setup() ->
	casino_sup:start_link(),
	{ok, Player1} = player:start(tim, 100),
	% {ok, Player2} = player:start(jim, 100),
	
	player:find_table(Player1),
	player:join_table(Player1, seat1),
	
	% player:find_table(Player2),
	% player:join_table(Player2, seat6),
	
	Player1.