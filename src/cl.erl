-module(cl).

-behaviour(gen_server).

-export([start_link/2, stop/1, terminate/2, init/1, handle_cast/2]).
-export([join_table/1, bet/2, hit/1, stay/1]).

start_link(Name, Stack) ->
	gen_server:start_link(?MODULE, [Name, Stack], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

terminate(_Reason, _State) ->
	ok.

init([Name, Stack]) ->
	{ok, PlayerPid} = player_sup:start_player(Name, Stack),
	player:join_table(PlayerPid),
	register(PlayerPid),
	{ok, PlayerPid}.

%% Public API

join_table(Pid) ->
	gen_server:cast(Pid, join_table).

bet(Pid, Amt) ->
	gen_server:cast(Pid, {bet, Amt}).

hit(Pid) ->
	gen_server:cast(Pid, hit).

stay(Pid) ->
	gen_server:cast(Pid, stay).


%% OTP API

handle_cast(join_table, Pid) ->
	player:join_table(Pid),
	{noreply, Pid};

handle_cast({bet, Amt}, Pid) ->
	player:bet(Pid, Amt),
	{noreply, Pid};

handle_cast(hit, Pid) ->
	player:hit(Pid),
	{noreply, Pid};

handle_cast(stay, Pid) ->
	player:stay(Pid),
	{noreply, Pid};

handle_cast(Payload, Pid) when is_list(Payload) ->
	Printer = fun({Tag, Pl}) -> io:format("~p: ~p~n", [Tag, Pl]) end,
	lists:foreach(Printer, Payload),
	register(Pid),
	{noreply, Pid};

handle_cast(Any, Pid) ->
	io:format("Unhandled message: ~p~n", [Any]),
	{noreply, Pid}.

%% Private API

register(Pid) ->
	player:register_proxy(Pid, self()).