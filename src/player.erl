-module(player).
-behavior(gen_server).

-record(table, {pid, seat}).
-record(game, {pid, cards=[], bet=0}).
-record(state, {sid, name, stack=0, table=#table{}, game=#game{}}).

-export([start/3, start_link/3, init/1, stop/1, terminate/2]).
-export([handle_cast/2]).

-export([find_table/1, join_table/2, bet/2, stay/1, new_cards/2, hit/1, notify/2]).
-export([paid/2, lost/2]).

start(SID, Name, Stack) ->
	gen_server:start(?MODULE, [SID, Name, Stack], []).
start_link(SID, Name, Stack) ->
	gen_server:start_link(?MODULE, [SID, Name, Stack], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([SID, Name, Stack]) ->
	{ok, #state{sid=SID, name=Name, stack=Stack}}.

terminate(_Reason, _Game) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

join_table(Pid) ->
	gen_server:call(Pid, join_table).

bet(Pid, Amt) ->
	gen_server:call(Pid, {bet, Amt}).

hit(Pid) ->
	gen_server:call(Pid, hit).

stay(Pid) ->
	gen_server:cast(Pid, stay).

new_cards(Pid, Cards) ->
	gen_server:cast(Pid, {new_cards, Cards}).

paid(Pid, Amt) ->
	gen_server:cast(Pid, {paid, Amt}).

lost(Pid, Amt) ->
	gen_server:cast(Pid, {lost, Amt}).

notify(Pid, Msg) ->
	gen_server:cast(Pid, {notify, Msg}).

msg_from_web_client(Pid, Msg) ->
	case Msg of
		{bet, Amt} -> bet(Pid, Amt);
		hit 			 -> hit(Pid);
		stay 			 -> stay(Pid)
	end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_call(join_table, _From, #state{name=Name}=State) ->
	case casino:join_table(self(), Name) of
		{table, Table, Game, Seat} ->
			{reply, Msg, State#state{table=#table{pid=Table, seat=Seat}, game=#game{pid=Game}}};
		{error, Msg}  ->
			{reply, Msg, State}
	end;

handle_call({bet, Amt}, _From, #state{game=Game, table=Table, stack=Stack}=State) ->
	NewBet = Amt + Game#game.bet,
	case NewBet > Stack of
		false ->
			case game:bet(Game#game.pid, Table#table.seat, Amt) of
				{ok, NewBet} ->
					{reply, {ok, NewBet}, State#state{game=Game#game{bet=NewBet}}};
				Any ->
					{reply, Any, State}
			end;
		true ->
			{reply, {error, overbet}, State}
	end;

handle_call(hit, _From, #state{game=Game, table=Table}=State) ->
	case game:hit(Game#game.pid, Table#table.seat) of
		{ok, NewCards} ->
			io:format("Your hand is now: ~p~n", [NewCards]),
			io:format("Your hands score is: ~p~n", [bj_hand:compute(NewCards)]),
			{reply, ok, State#state{game=Game#game{cards=NewCards}}};
		{error, Msg} ->
			io:format("Error: ~p~n", [Msg]),
			{reply, error, State}
	end.
	
handle_cast(stay, #state{game=Game, table=Table}=State) ->
	game:stay(Game#game.pid, Table#table.seat),
	{noreply, State};

handle_cast({new_cards, Cards}, #state{game=Game}=State) ->
	io:format("Your hand is now: ~p~n", [Cards]),
	io:format("Your hands score is: ~p~n", [bj_hand:compute(Cards)]),
	{noreply, State#state{game=Game#game{cards=Cards}}};

handle_cast({paid, Amt}, #state{game=Game, stack=Stack}=State) ->
	io:format("I won $~p~n", [Amt]),
	io:format("Stack is now ~p~n", [Stack+Amt]),
	{noreply, State#state{stack=Stack+Amt, game=Game#game{bet=0, cards=[]}}};

handle_cast({lost, Amt}, #state{game=Game, stack=Stack}=State) ->
	io:format("I lost $~p~n", [Amt]),
	io:format("Stack is now ~p~n", [Stack-Amt]),
	{noreply, State#state{stack=Stack-Amt, game=Game#game{bet=0, cards=[]}}};

handle_cast({notify, Msg}, State) ->
	io:format("~p~n", [Msg]),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State}.