-module(game).
-behavior(gen_fsm).

-record(seat, {pid, cards, bet=0}).
-record(state, {timer, seats, deck, hand}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([waiting/2, betting/2, betting/3, dealing/2, playing_hands/2, playing_hands/3, dealer/2]).

-export([start_hand/2, stop_hand/1, notify_players/2, bet/3, hit/2, stay/2]).

start() ->
	gen_fsm:start(?MODULE, [], []).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
	{ok, Timer} = game_timer:start_link(),
	{ok, waiting, #state{timer=Timer}}.

terminate(_Reason, _StateName, _State) ->
	io:format("Terminating!~n", []),
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hand(Pid, Players) ->
	{ok, Seats} = setup_seats(dict:to_list(Players)),
	{ok, Deck}  = deck:shuffled(),
	gen_fsm:send_event(Pid, {start_hand, Seats, Deck}).

stop_hand(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop_hand).

bet(Pid, Seat, Amt) ->
	gen_fsm:sync_send_event(Pid, {place_bet, Seat, Amt}).

hit(Pid, Seat) ->
	gen_fsm:sync_send_event(Pid, {hit, Seat}).

stay(Pid, Seat) ->
	gen_fsm:sync_send_event(Pid, {stay, Seat}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

waiting({start_hand, Seats, Deck}, #state{timer=Timer}=State) ->
	ok = notify_players("Starting a new game", Seats),
	send_self(0, start_betting, Timer),
	{next_state, betting, State#state{seats=Seats, deck=Deck}}.

betting(start_betting, #state{timer=Timer, seats=Seats}=State) ->
	ok = notify_players("Place your bets", Seats),
	send_self(10, end_betting, Timer),
	{next_state, betting, State};

betting(end_betting, #state{timer=Timer}=State) ->
	io:format("Betting is closed, dealing cards now.~n", []),
	send_self(0, start_dealing, Timer),
	{next_state, dealing, State}.

betting({place_bet, Seat, Amt}, _From, #state{seats=Seats}=State) ->
	{ok, #seat{bet=Bet}=SeatN} = dict:find(Seat, Seats),
	NewSeats = dict:store(Seat, SeatN#seat{bet= Bet + Amt}, Seats),
	{reply, {ok, Bet + Amt}, betting, State#state{seats=NewSeats}}.

dealing(start_dealing, #state{timer=Timer, seats=Seats, deck=Deck}=State) ->
	%% This will deal the cards and inform the users of their hands
	{ok, NewSeats, NewDeck} = initial_deal(Seats, Deck),
	send_self(0, play_hand, Timer),
	{next_state, playing_hands, State#state{seats=NewSeats, deck=NewDeck, hand=next_hand(0, Seats)}}.

%% If we have played through all 6 hands, move on to the dealer.
playing_hands(play_hand, #state{timer=Timer, hand=Hand}=State) when Hand > 6 ->
	send_self(0, play_hand, Timer),
	{next_state, dealer, State};

playing_hands(play_hand, #state{timer=Timer, seats=Seats, hand=Hand}=State) ->
	SeatN = seat_from_hand(Hand),
	{ok, #seat{pid=Pid}} = dict:find(SeatN, Seats),
	player:notify(Pid, "Hit or stay?"),
	send_self(30, next_hand, Timer),
	{next_state, playing_hands, State};

playing_hands(next_hand, #state{timer=Timer, seats=Seats, hand=Hand}=State) ->
	NextHand = next_hand(Hand, Seats),
	send_self(0, play_hand, Timer),
	{next_state, playing_hands, State#state{hand=NextHand}}.

playing_hands({stay, SeatN}, _From, #state{timer=Timer, seats=Seats, hand=Hand}=State) ->
	case seat_from_hand(Hand) of
		SeatN ->
			NextHand = next_hand(Hand, Seats),
			send_self(0, next_hand, Timer),
			{reply, ok, playing_hands, State#state{hand=NextHand}};
		_Else ->
			{reply, {error, "It's not your turn!"}, playing_hands, State}
	end;

playing_hands({hit, SeatN}, _From, #state{timer=Timer, seats=Seats, hand=Hand, deck=Deck}=State) ->
	case seat_from_hand(Hand) of
		SeatN ->
			{ok, #seat{cards=OldCards}=Seat} = dict:find(SeatN, Seats),
			{ok, Cards, NewDeck} = deck:draw(1, Deck),
			NewCards = OldCards ++ Cards,
			NewSeats = dict:store(SeatN, Seat#seat{cards=NewCards}, Seats),
			
			case hand:compute(NewCards) of
				Score when Score >= 21 ->
					NextHand = next_hand(Hand, NewSeats),
					send_self(0, next_hand, Timer),
					NewState = State#state{seats=NewSeats, deck=NewDeck, hand=NextHand};
				_Score ->
					send_self(0, next_hand, Timer),
					NewState = State#state{seats=NewSeats, deck=NewDeck}
			end,
			{reply, {ok, Cards}, playing_hands, NewState};
		_Else ->
			{reply, {error, "It's not your turn!"}, playing_hands, State}
	end.

dealer(play_hand, State) ->
	{next_hand, dealer, State}.

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

initial_deal(SeatHash, Deck) ->
	initial_deal(dict:to_list(SeatHash), SeatHash, Deck).

initial_deal([], SeatHash, Deck) ->
	{ok, SeatHash, Deck};
initial_deal([{SeatN, #seat{pid=Pid}=Seat}|Seats], SeatHash, Deck) ->
	{ok, Cards, NewDeck} = deck:draw(2, Deck),
	player:new_cards(Pid, Cards),
	initial_deal(Seats, dict:store(SeatN, Seat#seat{cards=Cards}, SeatHash), NewDeck).

next_hand(0, Seats) -> next_hand(1, Seats);
next_hand(N, Seats) ->
	SeatN = seat_from_hand(N),
	case dict:find(SeatN, Seats) of
		{ok, _Seat} -> N;
		error	   -> next_hand(N + 1, Seats)
	end.

seat_from_hand(N) ->
	list_to_atom("seat" ++ integer_to_list(N)).