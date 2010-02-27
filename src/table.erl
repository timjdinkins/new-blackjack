-module(table).
-behavior(gen_fsm).

-include_lib("game.hrl").

-record(game, {pid=null, seats=[], new_players=[]}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([empty_table/3, playing/2, playing/3]).

-export([seat_player/2, open_seats/1, game_complete/2]).

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

terminate(_Reason, _StateName, #game{pid=Pid}) ->
	game:stop(Pid),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

seat_player(Pid, {PlayerPid, Name, Stack}) ->
	gen_fsm:sync_send_event(Pid, {seat_player, {PlayerPid, Name, Stack}}).

open_seats(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, open_seats).

game_complete(Pid, Seats) ->
	gen_fsm:send_event(Pid, {game_complete, Seats}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Player is {Pid, Name}
%%
empty_table({seat_player, {Pid, Name, Stack}}, _From, #game{pid=GamePid}=Game) ->
	NSeat = #seat{name=Name, stack=Stack},
	NewGame = Game#game{seats=[{1, Pid, NSeat}]},
	ok = game:start_hand(GamePid, self(), [{1, Pid, NSeat}]),
	{reply, {ok, 1, GamePid}, playing, NewGame}.
	
playing({seat_player, {Pid, Name, Stack}}, _From, #game{pid=GamePid, seats=Seats}=Game) ->
	SeatN = find_seat_number(Seats, Game#game.new_players),
	NewPlayers = [{SeatN, Pid, #seat{name=Name, stack=Stack}} | Game#game.new_players],
	{reply, {ok, SeatN, GamePid}, playing, Game#game{new_players=NewPlayers}}.

playing({game_complete, Seats}, #game{pid=Pid, new_players=NewPlayers}=Game) ->
	{NSeats, Quiters} = players_and_quiters(Seats),
	lists:foreach(fun({FSn, _Fp, FSeat}) -> io:format("Seat: ~p, Stack: ~p~n", [FSn, FSeat#seat.stack]) end, NSeats),
	ok = quit_table(Quiters),
	NNSeats = lists:keysort(1, NSeats ++ NewPlayers),
	NewSeats = [{SN, P, Seat#seat{bet=0, cards=[]}} || {SN, P, Seat} <- NNSeats],
	lists:foreach(fun({FSn, _Fp, FSeat}) -> io:format("Seat: ~p, Stack: ~p~n", [FSn, FSeat#seat.stack]) end, NewSeats),
	case length(NewSeats) of
		L when L > 0 ->
			ok = game:start_hand(Pid, self(), NewSeats),
			{next_state, playing, Game#game{seats=NewSeats, new_players=[]}};
		_Else ->
			{next_state, empty_table, Game#game{seats=[], new_players=[]}}
	end;

playing(timeout, #game{pid=Pid, seats=Seats}=Game) ->
	ok = game:start_hand(Pid, self(), Seats),
	{next_state, playing, Game}.

handle_event(stop, _StateName, #game{pid=Pid}=Game) ->
	%% Tell the players we stopped this table
	game:stop(Pid),
	%% Tell the casino this table it emptying out.
	casino:close_table(self()),
	{stop, normal, Game}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
quit_table(Seats) ->
	lists:foreach(fun({_SN, PPid, _Stk}) -> ok = casino:leave_table({self(), PPid}) end, Seats),
	ok.

% Add the player to the list of players tuple, returning {the_seat, list_of_player_tuples}
find_seat_number(Seats, NewPlayers) ->
	NSeats = lists:keysort(1, NewPlayers ++ Seats),
	[Fst|_] = lists:subtract(lists:seq(1,6), [I || {I,_,_} <- NSeats]),
	Fst.

players_and_quiters(Seats) ->
	Pred = fun({_SeatN, _Pid, #seat{bet=Bet}}) -> Bet > 0 end,
	lists:partition(Pred, Seats).