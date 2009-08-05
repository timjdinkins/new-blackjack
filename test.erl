-module(test).

-export([setup/0]).

setup() ->
	casino_sup:start_link(),
	{ok, Player} = player:start(tim, 100),
	{ok, Player1} = player:start(jim, 100),
	
	player:find_table(Player),
	player:join_table(Player, seat1),
	
	player:find_table(Player1),
	player:join_table(Player1, seat6),
	
	{Player, Player1}.