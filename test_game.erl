-module(test_game).

-export([setup/0]).

setup() ->
	casino_sup:start_link(),
	{ok, Player} = player:start(tim, 100),
	player:find_table(Player),
	player:join_table(Player, seat1),
	Player.