-module(game).
-behavior(gen_fsm).

-include("player.hrl").

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([waiting/2, betting/2, dealing/2, playing_hands/2]).

-export([start_game/1, stop_game/1]).

start() ->
	gen_fsm:start(?MODULE, [], []).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
	{ok, Timer} = game_timer:start_link(),
	{ok, waiting, {Timer, players, bets}}.

terminate(_Reason, _StateName, _Timer) ->
	io:format("Terminating!~n", []),
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hand(Pid, Players) ->
	gen_fsm:send_event(Pid, {start_hand, Players}).

stop_hand(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop_hand).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

waiting({start_hand, Players}, {Timer, players, bets}) ->
	notify_players("Starting a new game", dict:to_list(Players)),
	send_self(0, start_betting, Timer),
	Bets = create_bets(dict:to_list(Players)),
	{next_state, betting, {Timer, Players, Bets}}.

betting(start_betting, {Timer, Players}) ->
	notify_players("Place your bets", Players),
	send_self(10, end_betting, Timer),
	{next_state, betting, {Timer, Players}};

betting({place_bet, Seat, Amt}, {Timer, Players}) ->
	{next_state, betting, {Timer, Players}}

betting(end_betting, [Timer]) ->
	io:format("Betting is closed, dealing cards now.~n", []),
	send_self(0, start_dealing, Timer),
	{next_state, dealing, [Timer]}.

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
	ok = test_timer:set_timeout(Timer, {Secs, fun() -> gen_fsm:send_event(MyPid, Msg) end}).

notify_players(Msg, []) -> ok;
notify_players(Msg, [{_Seat,#player{pid=Pid}}|Players]) ->
	player:notify(Pid, Pid),
	notify_players(Players).

create_bets([{Seat, _Player}Players]) ->
	Bets = dict:new(),
	create_bets(Players, dict:store(Seat, 0, Bets)).
create_bets([], Bets) ->
	Bets;
create_bets([{Seat, _Player}, Players], Bets) ->
	create_bets(Players, dict:store(Seat, 0, Bets)).