-module(player).
-behavior(gen_server).

-include("player.hrl").

-export([start/2, start_link/2, init/1, stop/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).

-export([find_table/1, table_pid/1, join_table/2, place_bet/2, notify/2]).

start(Name, Stack) ->
	gen_server:start(?MODULE, [Name, Stack], []).
start_link(Name, Stack) ->
	gen_server:start_link(?MODULE, [Name, Stack], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([Name, Stack]) ->
	{ok, {table_pid, seat, #player{name=Name,stack=Stack,pid=self()}}}.

terminate(_Reason, _Game) ->
	ok.

%% Public API
find_table(Pid) ->
	gen_server:cast(Pid, find_table).

join_table(Pid, Seat) ->
	gen_server:call(Pid, {join_table, Seat}).

place_bet(Pid, Amt) ->
	gen_server:call(Pid, {place_bet, Amt}).

notify(Pid, Msg) ->
	gen_server:cast(Pid, {notify, Msg}).

table_pid(Pid) ->
	gen_server:call(Pid, table_pid).

%% Callbacks
handle_call({join_table, Seat}, _From, {TablePid, _Seat, Player}) ->
	Message = case table:seat(TablePid, Seat, self()) of
		{ok, seated} -> "Successfully joined the table";
		{error, Msg} -> Msg
	end,
	{reply, Message, {TablePid, Seat, Player}};

handle_call({place_bet, Amt}, _From, {TablePid, Seat, Player}) ->
	table:bet(TablePid, Seat, Amt),
	{reply, ok, {TablePid, Seat, Player}};

handle_call(table_pid, _From, {TablePid, Seat, Player}) ->
	{reply, TablePid, {TablePid, Seat, Player}}.

handle_cast(find_table, {_OldTablePid, _Seat, Player}) ->
	{table, TablePid} = casino:find_table(),
	{noreply, {TablePid, seat, Player}};

handle_cast({notify, Msg}, State) ->
	io:format("~p~n", [Msg]),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State}.