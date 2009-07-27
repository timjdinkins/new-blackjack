-module(player).
-behavior(gen_server).

-include("player.hrl").

-export([start/2, start_link/2, init/1, stop/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).

-export([join_table/1, starting_game/1, table_pid/1]).

start(Name, Stack) ->
	gen_server:start(?MODULE, [Name, Stack], []).
start_link(Name, Stack) ->
	gen_server:start_link(?MODULE, [Name, Stack], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([Name, Stack]) ->
	{ok, {table_pid, #player{name=Name,stack=Stack,pid=self()}}}.

terminate(_Reason, _Game) ->
	ok.

%% Public API
join_table(Pid) ->
	gen_server:call(Pid, join_table).

starting_game(Pid) ->
	gen_server:cast(Pid, starting_game).

notify(Pid, Msg) ->
	gen_server:cast(Pid, {notify, Msg}).

table_pid(Pid) ->
	gen_server:call(Pid, table_pid).

%% Callbacks
handle_call(join_table, _From, {OldTablePid, Player}) ->
	case casino:seat(Player) of
		{table, TablePid} -> 
			Msg = "Successfully joined the table";
		_Error ->
			Msg = "Something went wrong, try again!",
			TablePid = OldTablePid
	end,
	{reply, Msg, {TablePid, Player}};

handle_call(table_pid, _From, {TablePid, Player}) ->
	{reply, TablePid, {TablePid, Player}}.

handle_cast(starting_game, Player) ->
	io:format("Player ~p: the game is starting~n", [self()]),
	{noreply, Player};

handle_cast({notify, Msg}, Player) ->
	io:format("~p~n", [Msg]),
	{noreply, Player};

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.