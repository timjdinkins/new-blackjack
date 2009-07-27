-module(table).
-behavior(gen_fsm).

-include("player.hrl").
-record(game, {pid=null, players=dict:new()}).

-export([start/0, start_link/0, init/1, stop/1, terminate/2]).
-export([handle_event/3]).

-export([empty_table/2, playing/2]).

-export([join/3, players/1, open_seats/1]).

start() ->
	gen_fsm:start(?MODULE, [], []).
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:cast(Pid, stop).

init([]) ->
	process_flag(trap_exit, true),
	{ok, GamePid} = game:start_link(),
	{ok, empty_table, #game{pid=GamePid}}.

terminate(_Reason, #game{pid=Pid}) ->
	game:stop(Pid),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

join(Pid, Seat, Player) ->
	gen_fsm:call(Pid, {add_player, Seat, Player}).

players(Pid) ->
	gen_fsm:call(Pid, print_players).

open_seats(Pid) ->
	gen_fsm:call(Pid, open_seats).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_table({add_player, Seat, Player}, #game{pid=Pid, players=Players}=Game) ->
	NewPlayers = dict:store(Seat, Player, Players),
	NewGame = Game#game{players=NewPlayers},
	ok = game:start_game(Pid, NewPlayers),
	{reply, {ok, seated}, playing, NewGame}.
	
playing({add_player, Seat, Player}, #game{players=Players}=Game) ->
	case seat_available(Seat, Players) of
		true -> 
			NewPlayers = dict:store(Seat, Player, Players),
			{reply, playing, {ok, seated}, Game#game{players=NewPlayers}};
		false ->
			{reply, {error, seat_taken}, playing, Game}
	end;

playing({game_complete, Players, Quiters}, #game{pid=Pid}=Game) ->
	NewPlayers = remove_quiters(Players, Quiters),
	ok = game:start_game(Pid, NewPlayers),
	{next_state, playing, Game#game{players=NewPlayers}}.

handle_event(print_players, StateName, #game{players=Players}=Game) ->
	io:format("~p~n", [Players]),
	{next_state, StateName, Game};

handle_event(open_seats, StateName, #game{players=Players}=Game) ->
	{reply, {ok, find_open_seats(Players)}, StateName, Game};

handle_event(stop, _StateName, #game{pid=Pid, players=_Players}=Game) ->
	%% Tell the players we stopped this table
	game:stop(Pid),
	{stop, normal, Game}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
seat_available(Seat, Players) ->
	lists:member(find_open_seats(Players), Seat).

find_open_seats(Players) ->
	Seats = dict:keys(Players),
	Possible = [seat1, seat2, seat3, seat4, seat5, seat6],
	lists:subtract(Possible, Seats).

remove_quiters(Players, []) ->
	Players;
remove_quiters(Players, [Quiter|Quiters]) ->
	remove_quiters(dict:erase(Quiter, Players), Quiters).