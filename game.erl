-module(game).
-behavior(gen_fsm).

-record(seat, {pid, cards, bet=0}).
-record(state, {tablepid, timer, seats, deck, hand}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([waiting/2, betting/2, betting/3, dealing/2, playing_hands/2, playing_hands/3, dealer/2]).
-export([finish_game/2]).

-export([start_hand/3, stop_hand/1, notify_players/2, bet/3, hit/2, stay/2]).

start() ->
	gen_fsm:start(?MODULE, [], []).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
	{ok, Timer} = game_timer:start_link(),
	{ok, waiting, #state{timer=Timer}}.

terminate(Reason, StateName, _State) ->
	io:format("Terminating in state ~p: ~p~n", [StateName, Reason]),
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hand(Pid, GamePid, Players) ->
	{ok, Seats} = setup_seats(dict:to_list(Players)),
	{ok, Deck}  = deck:shuffled(),
	gen_fsm:send_event(Pid, {start_hand, GamePid, Seats, Deck}).

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

waiting({start_hand, TablePid, Seats, Deck}, #state{timer=Timer}=State) ->
	ok = notify_players("Starting a new game", Seats),
	send_self(0, start_betting, Timer),
	{next_state, betting, State#state{tablepid=TablePid, seats=Seats, deck=Deck}}.

betting(start_betting, #state{timer=Timer, seats=Seats}=State) ->
	ok = notify_players("Place your bets", Seats),
	send_self(30, end_betting, Timer),
	{next_state, betting, State};

betting(end_betting, #state{timer=Timer, seats=Seats}=State) ->
	io:format("Betting is closed, dealing cards now.~n", []),
	case anyone_playing(Seats) of
		yes ->
			send_self(0, start_dealing, Timer),
			{next_state, dealing, State};
		no  ->
			send_self(0, payout_hands, Timer),
			{next_state, finish_game, State}
	end.

betting({place_bet, Seat, Amt}, _From, #state{timer=Timer, seats=Seats}=State) ->
	{ok, #seat{bet=Bet}=SeatN} = dict:find(Seat, Seats),
	NewSeats = dict:store(Seat, SeatN#seat{bet= Bet + Amt}, Seats),
	case all_bets_in(NewSeats) of
		yes ->
			send_self(0, end_betting, Timer),
			{reply, {ok, Bet + Amt}, betting, State#state{seats=NewSeats}};
		no  ->
			{reply, {ok, Bet + Amt}, betting, State#state{seats=NewSeats}}
	end.

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
	case next_hand(Hand, Seats) of
		{ok, all_hands_played} ->
			send_self(0, play_hand, Timer),
			{next_state, dealer, State};
		NextHand ->
			send_self(0, play_hand, Timer),
			{next_state, playing_hands, State#state{hand=NextHand}}
	end.

playing_hands({stay, SeatN}, _From, #state{timer=Timer, hand=Hand}=State) ->
	case seat_from_hand(Hand) of
		SeatN ->
			send_self(0, next_hand, Timer),
			{reply, ok, playing_hands, State};
		_Else ->
			{reply, {error, "It's not your turn!"}, playing_hands, State}
	end;

playing_hands({hit, SeatN}, _From, #state{timer=Timer, seats=Seats, hand=Hand, deck=Deck}=State) ->
	case seat_from_hand(Hand) of
		SeatN ->
			{ok, #seat{pid=Pid, cards=Cards}=Seat} = dict:find(SeatN, Seats),
			{ok, [Card], NewDeck} = deck:draw(1, Deck),
			NewCards = [Card|Cards],
			NewSeats = dict:store(SeatN, Seat#seat{cards=NewCards}, Seats),
			
			case bj_hand:compute(NewCards) of
				Score when Score >= 21 ->
					send_self(0, next_hand, Timer),
					NewState = State#state{seats=NewSeats, deck=NewDeck};
				_Score ->
					player:notify(Pid, "Hit or stay?"),
					send_self(30, next_hand, Timer),
					NewState = State#state{seats=NewSeats, deck=NewDeck}
			end,
			{reply, {ok, NewCards}, playing_hands, NewState};
		_Else ->
			{reply, {error, "It's not your turn!"}, playing_hands, State}
	end.

dealer(play_hand, #state{timer=Timer, seats=Seats, deck=Deck}=State) ->
	case everyone_busted(Seats) of
		true ->
			send_self(0, payout_hands, Timer),
			{next_state, finish_game, State};
		_Else ->
			{ok, #seat{cards=Cards}=Dealer} = dict:find(dealer, Seats),
			io:format("The dealer's hand is ~p.~n", [Cards]),
			{ok, NewDeck, NewCards} = play_dealer_hand(Deck, Cards),
			io:format("Dealer hand played~n", []),
			NewSeats = dict:store(dealer, Dealer#seat{cards=NewCards}, Seats),
			io:format("The dealer finishes with ~p.~n", [bj_hand:compute(NewCards)]),
			send_self(2, payout_hands, Timer),
			{next_state, finish_game, State#state{seats=NewSeats, deck=NewDeck}}
	end.

finish_game(payout_hands, #state{tablepid=TablePid, seats=Seats}=State) ->
	io:format("Paying out hands~n", []),
	case anyone_playing(Seats) of
		yes ->
			ok = payout_hands(Seats);
		no ->
			ok
	end,
	io:format("Done paying out hands~n", []),
	{ok, Quiters} = find_quiters(Seats),
	io:format("Telling table to start a new game~n", []),
	table:game_complete(TablePid, Quiters),
	{next_state, waiting, State}.

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
	{ok, Cards, NewDeck} = deck:draw(2, Deck),
	{ok, dict:store(dealer, #seat{cards=Cards}, SeatHash), NewDeck};
initial_deal([{SeatN, #seat{pid=Pid,bet=Bet}=Seat}|Seats], SeatHash, Deck) ->
	if
		Bet > 0 ->
			{ok, Cards, NewDeck} = deck:draw(2, Deck),
			player:new_cards(Pid, Cards),
			initial_deal(Seats, dict:store(SeatN, Seat#seat{cards=Cards}, SeatHash), NewDeck);
		true ->
			initial_deal(Seats, SeatHash, Deck)
	end.

play_dealer_hand(Deck, Cards) ->
	play_dealer_hand(Deck, Cards, bj_hand:compute(Cards)).

play_dealer_hand(Deck, Cards, Score) when Score >= 17 orelse length(Cards) > 4 ->
	{ok, Deck, Cards};
play_dealer_hand(Deck, Cards, _Score) ->
	{ok, [NewCard], NewDeck} = deck:draw(1, Deck),
	play_dealer_hand(NewDeck, [NewCard|Cards], bj_hand:compute([NewCard|Cards])).

next_hand(N, _Seats) when N > 6 -> {ok, all_hands_played};
next_hand(N, Seats) ->
	Next = N + 1,
	SeatN = seat_from_hand(Next),
	case dict:find(SeatN, Seats) of
		{ok, #seat{bet=Bet}} when Bet > 0 ->
			Next;
		{ok, _Seat} -> next_hand(Next, Seats);
		error       -> next_hand(Next, Seats)
	end.

anyone_playing(Seats) ->
	Fun = fun(_Seat, #seat{bet=Bet}) -> Bet > 0 end,
	Rem = dict:filter(Fun, Seats),
	case length(dict:to_list(Rem)) of
		L when L > 0 -> yes;
		_Else        -> no
	end.

all_bets_in(Seats) ->
	Fun = fun(_Seat, #seat{bet=Bet}) -> Bet == 0 end,
	Rem = dict:filter(Fun, Seats),
	case length(dict:to_list(Rem)) of
		L when L == 0 -> yes;
		_Else        -> no
	end.

seat_from_hand(N) ->
	list_to_atom("seat" ++ integer_to_list(N)).

payout_hands(Seats) ->
	{ok, #seat{cards=Cards}} = dict:find(dealer, Seats),
	DealerScore = bj_hand:compute(Cards),
	payout_hands(next_hand(0, Seats), Seats, DealerScore).

payout_hands(N, Seats, DealerScore) ->
	SeatN = seat_from_hand(N),
	{ok, #seat{pid=Pid, cards=Cards, bet=Bet}} = dict:find(SeatN, Seats),
	case bj_hand:compute(Cards) of
		Score when Score > 21 ->
			player:lost(Pid, Bet);
		Score when Score =< 21, DealerScore > 21 ->
			player:paid(Pid, Bet);
		Score when Score == DealerScore ->
			player:paid(Pid, 0);
		Score when Score >= DealerScore, Score == 21 ->
			player:paid(Pid, Bet * 2);
		Score when Score >= DealerScore ->
			player:paid(Pid, Bet);
		_Score ->
			player:lost(Pid, Bet)
	end,
	case next_hand(N, Seats) of
		{ok, all_hands_played} ->
			ok;
		N1 ->
			payout_hands(N1, Seats, DealerScore)
	end.

find_quiters(Seats) ->
	Fun = fun(_Seat, #seat{bet=Bet}) -> Bet == 0 end,
	{ok, dict:filter(Fun, Seats)}.

everyone_busted(Seats) ->
	Fun = fun(_Seat, #seat{cards=Cards}) -> 
			  (Cards /= undefined) and (bj_hand:compute(Cards) =< 21)
	end,
	length(dict:to_list(dict:filter(Fun, dict:erase(dealer, Seats)))) == 0.