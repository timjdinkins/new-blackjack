-module(player).
-behavior(gen_server).

-record(state, {tablepid, gamepid, seat, name, stack, cards=[], bet=0}).

-export([start/2, start_link/2, init/1, stop/1, terminate/2]).
-export([handle_call/3, handle_cast/2]).

-export([find_table/1, join_table/2, bet/2, stay/1, new_cards/2, hit/1, notify/2]).
-export([paid/2, lost/2]).

start(Name, Stack) ->
	gen_server:start(?MODULE, [Name, Stack], []).
start_link(Name, Stack) ->
	gen_server:start_link(?MODULE, [Name, Stack], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([Name, Stack]) ->
	{ok, #state{name=Name, stack=Stack}}.

terminate(_Reason, _Game) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

find_table(Pid) ->
	gen_server:cast(Pid, find_table).

join_table(Pid, Seat) ->
	gen_server:call(Pid, {join_table, Seat}).

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({join_table, Seat}, _From, #state{tablepid=TablePid}=State) ->
	case table:seat(TablePid, Seat, self()) of
		{ok, GamePid} -> Msg = "Successfully joined the table";
		{error, Msg}  -> GamePid = undef
	end,
	{reply, Msg, State#state{gamepid=GamePid, seat=Seat}};

handle_call({bet, Amt}, _From, #state{gamepid=GamePid, seat=Seat, stack=Stack, bet=Bet}=State) when Stack >= (Bet + Amt) ->
	NewBet = Amt + Bet,
	{ok, NewBet} = game:bet(GamePid, Seat, Amt),
	{reply, {ok, NewBet}, State#state{bet=NewBet}};

handle_call(hit, _From, #state{gamepid=GamePid, seat=Seat}=State) ->
	case game:hit(GamePid, Seat) of
		{ok, NewCards} ->
			io:format("Your hand is now: ~p~n", [NewCards]),
			io:format("Your hands score is: ~p~n", [bj_hand:compute(NewCards)]),
			{reply, ok, State#state{cards=NewCards}};
		{error, Msg} ->
			io:format("Error: ~p~n", [Msg]),
			{reply, error, State}
	end.
	
handle_cast(stay, #state{gamepid=GamePid,seat=Seat}=State) ->
	game:stay(GamePid, Seat),
	{noreply, State};

handle_cast(find_table, State) ->
	{table, TablePid} = casino:find_table(),
	{noreply, State#state{tablepid=TablePid}};

handle_cast({new_cards, Cards}, State) ->
	io:format("Your hand is now: ~p~n", [Cards]),
	io:format("Your hands score is: ~p~n", [bj_hand:compute(Cards)]),
	{noreply, State#state{cards=Cards}};

handle_cast({paid, Amt}, #state{stack=Stack}=State) ->
	io:format("I won $~p~n", [Amt]),
	io:format("Stack is now ~p~n", [Stack+Amt]),
	{noreply, State#state{stack=Stack+Amt, bet=0}};

handle_cast({lost, Amt}, #state{stack=Stack}=State) ->
	io:format("I lost $~p~n", [Amt]),
	io:format("Stack is now ~p~n", [Stack-Amt]),
	{noreply, State#state{stack=Stack-Amt, bet=0}};

handle_cast({notify, Msg}, State) ->
	io:format("~p~n", [Msg]),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State}.