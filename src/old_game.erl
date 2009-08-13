-module(game).
-behavior(gen_server).
-include("player.hrl").

-export([start/1, start_link/1, init/1, stop/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([deal/1,notify_players_of_start/1]).

start(Players) ->
	gen_server:start(?MODULE, [Players], []).
start_link(Players) ->
	gen_server:start_link(?MODULE, [Players], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([Players]) ->
	{ok, Players}.

terminate(_Reason, _LoopData) ->
	ok.

%% Public API
deal(Pid) ->
	gen_server:call(?MODULE, deal).

notify_players_of_start(Pid) ->
	gen_server:call(Pid, notify_players).

handle_call(notify_players, _From, Players) ->
	notify_players(Players),
	{reply, ok, Players}.

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

%% Private API
notify_players([]) -> ok;
notify_players([#player{pid=Pid}|Players]) ->
	player:starting_game(Pid),
	notify_players(Players).