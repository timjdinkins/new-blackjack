-module(game).
-behavior(gen_fsm).

-include_lib("game.hrl").

-record(state, {tablepid, timer, seats, deck, hand, dealer=#seat{}}).

-export([start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([waiting/2, betting/2, betting/3, dealing/2, playing_hands/2, playing_hands/3, dealer/2]).
-export([finish_game/2]).

-export([start_hand/3, stop_hand/1, notify_players/2, bet/2, hit/2, stay/2]).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->		
	gen_fsm:send_all_state_event(Pid, stop).

init([]) ->
	process_flag(trap_exit, true),
	{ok, Timer} = game_timer:start_link(),
	{ok, waiting, #state{timer=Timer}}.

terminate(Reason, StateName, _State) ->
	io:format("Terminating in state ~p: ~p~n", [StateName, Reason]),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_hand(Pid, TablePid, Seats) ->
	gen_fsm:send_event(Pid, {start_hand, TablePid, Seats}).

stop_hand(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop_hand).

bet(Pid, {PlayerPid, Amt}) ->
	gen_fsm:send_event(Pid, {place_bet, PlayerPid, Amt}).

hit(Pid, PlayerPid) ->
	gen_fsm:send_event(Pid, {hit, PlayerPid}).

stay(Pid, PlayerPid) ->
	gen_fsm:send_event(Pid, {stay, PlayerPid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

waiting({start_hand, TablePid, Seats}, #state{timer=Timer}=State) ->
	io:format("Game is starting a new hand.~n", []),
	ok = notify_players(Seats, wh:enc_initial_state(Seats)),
	ok = notify_players(Seats, wh:enc_msg("Starting a new game")),
	send_self(1, start_betting, Timer),
	{next_state, betting, State#state{tablepid=TablePid, seats=Seats, deck=deck:shuffled()}}.

betting(start_betting, #state{timer=Timer, seats=Seats}=State) ->
	ok = notify_players(Seats, wh:enc_msg("Place your bets")),
	send_self(30, end_betting, Timer),
	{next_state, betting, State};

betting(end_betting, #state{timer=Timer, seats=Seats}=State) ->
	ok = notify_players(Seats, wh:enc_msg("Betting is closed.")),
	case anyone_playing(Seats) of
		yes ->
			ok = notify_players(Seats, wh:enc_msg("Dealing hands.")),
			send_self(2, start_dealing, Timer),
			{next_state, dealing, State};
		no  ->
			ok = notify_players(Seats, wh:enc_msg("No one wants to play.  Damn you!")),
			send_self(2, payout_hands, Timer),
			{next_state, finish_game, State}
	end;
	
betting({place_bet, Pid, Amt}, #state{timer=Timer, seats=Seats}=State) ->
	{SeatN, Pid, #seat{bet=Bet}=Seat} = lists:keyfind(Pid, 2, Seats),
	NewSeats = lists:keystore(SeatN, 1, Seats, {SeatN, Pid, Seat#seat{bet=Bet + Amt}}),
	case all_bets_in(NewSeats) of
		yes -> send_self(0, end_betting, Timer);
		no  -> ok
	end,
	notify_players(Seats, wh:enc_update([{seat, SeatN}, {bet, Bet + Amt}])),
	{next_state, betting, State#state{seats=NewSeats}};

betting(Any, State) ->
	io:format("Unhandled message: ~p~n", [Any]),
	{next_state, betting, State}.

betting(_Any, _From, State) ->
	{reply, {error, unexpected_request}, betting, State}.

dealing(start_dealing, #state{timer=Timer, seats=Seats, deck=Deck}=State) ->
	%% This will deal the cards and inform the users of their hands
	{ok, NewSeats, Dealer, NewDeck} = initial_deal(Seats, Deck),
	send_self(2, play_hand, Timer),
	{next_state, playing_hands, State#state{seats=NewSeats, dealer=Dealer, deck=NewDeck, hand=next_hand(0, Seats)}}.

%% If we have played through all 6 hands, move on to the dealer.
playing_hands(play_hand, #state{timer=Timer, hand=Hand}=State) when Hand > 6 ->
	send_self(2, play_hand, Timer),
	{next_state, dealer, State};

playing_hands(play_hand, #state{timer=Timer, seats=Seats, hand=Hand}=State) ->
	notify_players(Seats, wh:enc_dealer_msg(Hand, "Hit or stay?")),
	send_self(30, next_hand, Timer),
	{next_state, playing_hands, State};

playing_hands(next_hand, #state{timer=Timer, seats=Seats, hand=Hand}=State) ->
	case next_hand(Hand, Seats) of
		{ok, all_hands_played} ->
			send_self(2, play_hand, Timer),
			{next_state, dealer, State};
		NextHand ->
			send_self(2, play_hand, Timer),
			{next_state, playing_hands, State#state{hand=NextHand}}
	end;

playing_hands({stay, Pid}, #state{timer=Timer, seats=Seats, hand=SeatN}=State) ->
	% Make sure the correct player is playing
	case lists:keyfind(Pid, 2, Seats) of
		{SeatN, Pid, _} ->
			notify_players(Seats, wh:enc_dealer_msg(SeatN, " staying")),
			notify_players(Seats, wh:enc_update([{seat, SeatN}, {action, <<"stay">>}])),
			send_self(2, next_hand, Timer);
		_ ->
			notify(Pid, {msg, "Sorry, it's not your turn."})
	end,
	{next_state, playing_hands, State};

playing_hands({hit, Pid}, #state{timer=Timer, seats=Seats, hand=SeatN, deck=Deck}=State) ->
	case lists:keyfind(Pid, 2, Seats) of
		% Make sure the correct player is playing
		{SeatN, Pid, #seat{cards=Cards}=Seat} ->
			{ok, [Card], NewDeck} = deck:draw(1, Deck),
			NewCards = [Card|Cards],
			NewSeats = lists:keystore(Pid, 2, Seats, {SeatN, Pid, Seat#seat{cards=NewCards}}),
			% Compute the hand
			case bj_hand:compute(NewCards) of
				Score when Score > 21 ->
					player:new_cards(Pid, NewCards, Score),
					player:busted(Pid, NewCards, Score),
					notify_players(Seats, wh:enc_update([{seat, SeatN}, {action, <<"busted">>}])),
					notify_players(Seats, wh:enc_update([{seat, SeatN}, {cards, wh:json_cards(NewCards)}, {score, Score}])),
					send_self(1, next_hand, Timer),
					NewState = State#state{seats=NewSeats, deck=NewDeck};
				Score ->
					player:new_cards(Pid, NewCards, Score),
					notify_players(Seats, wh:enc_update([{seat, SeatN}, {cards, wh:json_cards(NewCards)}, {score, Score}])),
					notify_players(Seats, wh:enc_dealer_msg(SeatN, "Hit or stay?")),
					send_self(30, next_hand, Timer),
					NewState = State#state{seats=NewSeats, deck=NewDeck}
			end,
			{next_state, playing_hands, NewState};
		_ ->
			notify(Pid, wh:enc_msg("Sorry, it's not your turn.")),
			{next_state, playing_hands, State}
	end;

playing_hands(_Any, State) ->
	{next_state, playing_hands, State}.

playing_hands(_Any, _From, State) ->
	{reply, {error, unexpected_request}, playing_hands, State}.

dealer(play_hand, #state{timer=Timer, seats=Seats, dealer=Dealer, deck=Deck}=State) ->
	case everyone_busted(Seats) of
		true ->
			send_self(0, payout_hands, Timer),
			{next_state, finish_game, State};
		_Else ->
			Cards = Dealer#seat.cards,
			notify_players(Seats, wh:enc_update([{seat, <<"dealer">>}, {cards, wh:json_cards(Cards)}])),
			{ok, NewDeck, NewCards} = play_dealer_hand(Deck, Cards),
			notify_players(Seats, wh:enc_update([{seat, <<"dealer">>}, {cards, wh:json_cards(NewCards)}, {score, bj_hand:compute(NewCards)}])),
			send_self(2, payout_hands, Timer),
			{next_state, finish_game, State#state{dealer=Dealer#seat{cards=NewCards}, deck=NewDeck}}
	end.

finish_game(payout_hands, #state{tablepid=TablePid, seats=Seats, dealer=Dealer}=State) ->
	case anyone_playing(Seats) of
		yes ->
			{ok, NewSeats} = payout_hands(Seats, Dealer);
		no ->
			NewSeats = Seats
	end,
	table:game_complete(TablePid, NewSeats),
	{next_state, waiting, State#state{seats=NewSeats}}.

handle_event(stop_game, _StateName, Timer) ->
	{next_state, waiting, Timer};

handle_event(stop, _StateName, Timer) ->
	{stop, normal, Timer}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_self(Secs, Msg, Timer) ->
	Pid = self(), % I have to reference this this way so the Pid is my procs pid, not the timer's.
	ok = game_timer:set_timeout(Timer, {Secs, fun() -> gen_fsm:send_event(Pid, Msg) end}).

notify(Pid, Msg) ->
	player:notify(Pid, Msg).

notify_players(Players, Msg) ->
	Notifier = fun({_S, Pid, _Seat}) -> notify(Pid, Msg) end,
	lists:foreach(Notifier, Players),
	ok.

initial_deal(Seats, Deck) ->
	initial_deal([], Seats, Deck).

initial_deal(Consumed, [], Deck) ->
	{ok, Cards, NewDeck} = deck:draw(2, Deck), % The dealers hand
	Seats = lists:reverse(Consumed),
	Pred = fun({S, _P, Seat}) ->
		wh:enc_update([{seat, S}, {cards, wh:json_cards(Seat#seat.cards)}])
	end,
	notify_players(Seats, lists:map(Pred, Seats)),
	{ok, Seats, #seat{cards=Cards}, NewDeck};

initial_deal(Consumed, [{SeatN, Pid, #seat{bet=Bet}=Seat}=S|Seats], Deck) ->
	if
		Bet > 0 ->
			{ok, Cards, NewDeck} = deck:draw(2, Deck),
			initial_deal([{SeatN, Pid, Seat#seat{cards=Cards}}|Consumed], Seats, NewDeck);
		true ->
			initial_deal([S|Consumed], Seats, Deck)
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
	case lists:keyfind(N + 1, 1, Seats) of
		{N1, _Pid, #seat{bet=Bet}} when Bet > 0 ->
			N1;
		false -> next_hand(N + 1, Seats)
	end.

anyone_playing(Seats) ->
	Fun = fun({_Seat, _Pid, #seat{bet=Bet}}) -> Bet > 0 end,
	Rem = lists:filter(Fun, Seats),
	if
		length(Rem) > 0 -> yes;
		true -> no
	end.

all_bets_in(Seats) ->
	Fun = fun({_Seat, _Pid, #seat{bet=Bet}}) -> Bet == 0 end,
	Rem = lists:filter(Fun, Seats),
	if
		length(Rem) == 0 -> yes;
		true -> no
	end.
	

payout_hands(Seats, Dealer) ->
	DealerScore = bj_hand:compute(Dealer#seat.cards),
	payout_hands(next_hand(0, Seats), Seats, DealerScore).

payout_hands(N, Seats, DealerScore) ->
	{SeatN, Pid, SeatRec} = lists:keyfind(N, 1, Seats),
	{Bet, Stack} = {SeatRec#seat.bet, SeatRec#seat.stack},
	Action = case bj_hand:compute(SeatRec#seat.cards) of
						 Score when Score > 21 ->
							 player:lost(Pid, SeatRec#seat.bet),
						   {lost, Bet, Stack - Bet};
						 Score when Score =< 21, DealerScore > 21 ->
							player:won(Pid, SeatRec#seat.bet),
							 {won, Bet, Bet + Stack};
						 Score when Score == DealerScore ->
							 player:tied(Pid),
							 {tie, 0, Stack};
						 Score when Score >= DealerScore, Score == 21 ->
							 player:won(Pid, SeatRec#seat.bet * 2),
							 {won, Bet * 2, (Bet * 2) + Stack};
						 Score when Score >= DealerScore ->
							 player:won(Pid, SeatRec#seat.bet),
							 {won, Bet, Bet + Stack};
						 _Score ->
							 player:lost(Pid, SeatRec#seat.bet),
							 {lost, Bet, Stack - Bet}
					 end,
	{Res, Amt, NewStack} = Action,
	
	NewSeats = lists:keyreplace(1, SeatN, Seats, {SeatN, Pid, SeatRec#seat{stack=NewStack, result=Res}}),
	
	Msg = case Res of
					won -> "You won " ++ integer_to_list(Amt);
					lost -> "You lost " ++ integer_to_list(Amt);
					tie -> "You tied"
				end,
	notify_players(Seats, wh:enc_dealer_msg(SeatN, Msg)),
	
	case next_hand(N, Seats) of
		{ok, all_hands_played} ->
			{ok, NewSeats};
		N1 ->
			timer:sleep(2000),
			payout_hands(N1, NewSeats, DealerScore)
	end.

everyone_busted(Seats) ->
	Fun = fun({_Seat, _Pid, #seat{cards=Cards}}) -> 
			  	(Cards /= undefined) and (bj_hand:compute(Cards) =< 21)
				end,
	length(lists:filter(Fun, Seats)) == 0.