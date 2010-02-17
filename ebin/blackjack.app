{
	application,
	blackjack,
 	[
		{description, "Multiplayer BlackJack System"},
  	{vsn, "0.1.0"},
		{modules, [casino_sup, table_sup]},
		{registered, [casino_sup, table_sup]},
		{applications, [kernel, stdlib, crypto]},
		{mod, {blackjack, []}}
	]
}.