-module(table_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_table/0]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Table = {null, {table, start_link, []}, transient, 2000, worker, [table]},
	{ok, {{simple_one_for_one, 1, 1}, [Table]}}.

start_table() ->
	supervisor:start_child(table_sup, []).