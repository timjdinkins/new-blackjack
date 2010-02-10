-module(registry_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	process_flag(trap_exit, true),
	Spec = {registry, {registry, start_link, []}, permanent, 2000, worker, [registry]},
	{ok, {{one_for_one, 1, 1}, [Spec]}}.