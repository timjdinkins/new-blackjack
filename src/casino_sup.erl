-module(casino_sup).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Spec = {casino, {casino, start_link, []}, permanent, 2000, worker, [casino]},
	{ok, {{one_for_one, 1, 1}, [Spec]}}.