-module(game).
-behavior(gen_fsm).

-record(seat, {pid, cards, bet=0}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([waiting/2, betting/2, dealing/2, playing_hands/2]).

-export([start_hand/2, stop_hand/1, notify_players/2]).

start() ->
	gen_fsm:start(?MODULE, [], []).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
	{ok, Timer} = game_timer:start_link(),
	{ok, waiting, {Timer, seats}}.

terminate(_Reason, _StateName, _Timer) ->
	io:format("Terminating!~n", []),
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hand(Pid, Players) ->
	{ok, Seats} = setup_seats(dict:to_list(Players)),
	gen_fsm:send_event(Pid, {start_hand, Seats}).

stop_hand(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop_hand).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

waiting({start_hand, Seats}, {Timer, seats}) ->
	ok = notify_players("Starting a new game", Seats),
	send_self(0, start_betting, Timer),
	{next_state, betting, {Timer, Seats}}.

betting(start_betting, {Timer, Seats}) ->
	ok = notify_players("Place your bets", Seats),
	send_self(10, end_betting, Timer),
	{next_state, betting, {Timer, Seats}};

betting({place_bet, Seat, Amt}, {Timer, Seats}) ->
	{ok, SeatN} = dict:find(Seat, Seats),
	NewSeats = dict:store(Seat, SeatN#seat{bet=Amt}),
	{next_state, betting, {Timer, NewSeats}};

betting(end_betting, {Timer, Seats}) ->
	io:format("Betting is closed, dealing cards now.~n", []),
	send_self(0, start_dealing, Timer),
	{next_state, dealing, {Timer, Seats}}.

dealing(start_dealing, [Timer]) ->
	io:format("Dealing the cards.~n", []),
	send_self(0, play_hand, Timer),
	{next_state, playing_hands, [Timer]}.

playing_hands(play_hand, [Timer]) ->
	io:format("Playing a hand.~n", []),
	{next_state, playing_hands, [Timer]}.

handle_event(stop_game, StateName, Timer) ->
	case StateName of
		waiting ->
			ok;
		_AnyState ->
			io:format("Sorry, I have to end this game right now~n", [])
	end,
	{next_state, waiting, Timer};

handle_event(stop, _StateName, Timer) ->
	{stop, normal, Timer}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_self(Secs, Msg, Timer) ->
	MyPid = self(),
	ok = game_timer:set_timeout(Timer, {Secs, fun() -> gen_fsm:send_event(MyPid, Msg) end}).

notify_players(Msg, Dict) ->
	do_notify(Msg, dict:to_list(Dict)).

do_notify(_Msg, []) -> ok;
do_notify(Msg, [{_Seat,#seat{pid=Pid}}|Seats]) ->
	player:notify(Pid, Msg),
	do_notify(Msg, Seats).

setup_seats([{Seat, Pid}|Players]) ->
	Seats = dict:new(),
	setup_seats(Players, dict:store(Seat, #seat{pid=Pid}, Seats)).
setup_seats([], Seats) ->
	{ok, Seats};
setup_seats([{Seat, Pid}|Players], Seats) ->
	setup_seats(Players, dict:store(Seat, #seat{pid=Pid}, Seats)).