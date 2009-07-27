-module(casino_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	CasinoChild = {casino, {casino, start_link, []}, permanent, 2000, worker, [casino]},
	TableSupChild = {table_sup, {table_sup, start_link, []}, permanent, 2000, supervisor, [table_sup]},
	{ok, {{one_for_one, 1, 1}, [CasinoChild, TableSupChild]}}.