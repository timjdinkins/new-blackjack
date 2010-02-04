-module(bj_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	CasinoSup = {
								casino_sup,
								{casino_sup, start_link, []},
								permanent, 2000, supervisor, [casino_sup]
							},
	TableSup  = {
								table_sup,
								{table_sup, start_link, []},
								permanent, 2000, supervisor, [table_sup]
							},
	PlayerSup = {
								player_sup,
								{player_sup, start_link, []},
								permanent, 2000, supervisor, [player_sup]
							},
	{ok,{{one_for_one, 0, 1}, [TableSup, PlayerSup, CasinoSup]}}.