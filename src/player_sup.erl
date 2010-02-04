-module(player_sup).

-behavior(supervisor).

-define(SERVER, ?MODULE).

-export([start_link/0, stop/0]).
-export([init/1, start_player/2]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
	ok.

init([]) ->
	Player = {null, {player, start_link, []}, temporary, brutal_kill, worker, [player]},
	{ok, {{simple_one_for_one, 1, 1}, [Player]}}.

start_player(Name, Stack) ->
	supervisor:start_child(?SERVER, [Name, Stack]).