-module(player).
-behavior(gen_server).

-record(table, {pid, seat}).
-record(game, {pid, cards=[], bet=0}).
-record(state, {proxy_pid, name, stack=0, table=#table{}, game=#game{}, messages=[]}).

-export([start_link/2, init/1, stop/1, terminate/2]).
-export([handle_cast/2]).

-export([join_table/1, bet/2, stay/1, hit/1, notify/2]).
-export([won/2, lost/2, tied/1, new_cards/3]).

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

join_table(Pid) ->
	gen_server:cast(Pid, join_table).

bet(Pid, Amt) ->
	gen_server:cast(Pid, {bet, Amt}).

hit(Pid) ->
	gen_server:cast(Pid, hit).

stay(Pid) ->
	gen_server:cast(Pid, stay).

new_cards(Pid, Cards, Score) ->
	gen_server:cast(Pid, {new_cards, Cards, Score}).

won(Pid, Amt) ->
	gen_server:cast(Pid, {paid, Amt}).

lost(Pid, Amt) ->
	gen_server:cast(Pid, {lost, Amt}).

tied(Pid) ->
	gen_server:cast(Pid, tie).

notify(Pid, Msg) ->
	gen_server:cast(Pid, {notify, Msg}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_cast(join_table, #state{name=Name}=State) ->
	case casino:join_table(self(), Name) of
		{ok, Table, Game, Seat} ->
			{noreply, State#state{table=#table{pid=Table, seat=Seat}, game=#game{pid=Game}}};
		{error, _Msg}  ->
			notify_proxy(State, {msg, "Joining the table failed.  Try again later."}),
			{noreply, State}
	end;

handle_cast({bet, Amt}, #state{game=Game, table=Table, stack=Stack}=State) ->
	NewBet = Amt + Game#game.bet,
	case NewBet > Stack of
		false ->
			game:bet(Game#game.pid, Table#table.seat, Amt),
			{noreply, State#state{game=Game#game{bet=NewBet}}};
		true ->
			notify_proxy(State, {msg, "Your stack is too small for that bet."}),
			{noreply, State}
	end;

handle_cast(hit, #state{game=Game, table=Table}=State) ->
	game:hit(Game#game.pid, Table#table.seat),
	{noreply, State};
	
handle_cast(stay, #state{game=Game, table=Table}=State) ->
	game:stay(Game#game.pid, Table#table.seat),
	{noreply, State};

handle_cast({new_cards, Cards, Score}, #state{game=Game}=State) ->
	notify_proxy(State, [{new_cards, Cards}, {score, Score}]),
	{noreply, State#state{game=Game#game{cards=Cards}}};

handle_cast({busted, Cards}, #state{game=Game}=State) ->
	notify_proxy(State, {action, busted}),
	{noreply, State#state{game=Game#game{cards=Cards}}};

handle_cast({paid, Amt}, #state{game=Game, stack=Stack}=State) ->
	notify_proxy(State, [{result, win}, {amt, Amt}, {stack, Stack + Amt}]),
	{noreply, State#state{stack=Stack+Amt, game=Game#game{bet=0, cards=[]}}};

handle_cast({lost, Amt}, #state{game=Game, stack=Stack}=State) ->
	notify_proxy(State, [{result, lost}, {amt, Amt}, {stack, Stack - Amt}]),
	{noreply, State#state{stack=Stack-Amt, game=Game#game{bet=0, cards=[]}}};

handle_cast(tie, #state{game=Game, stack=Stack}=State) ->
	notify_proxy(State, [{result, tie}, {stack, Stack}]),
	{noreply, State#state{game=Game#game{bet=0, cards=[]}}};

%%
% Whenever we send a message to the proxy, we remove it's pid.
% After a proxy receives a message, it has to reregister it's pid.
%%
handle_cast({notify, Msg}, #state{proxy_pid=Pid, messages=Ms}=State) ->
	NewMessages = [Msg|Ms],
	case Pid of
		undefined ->
			{noreply, State#state{messages=NewMessages}};
		_  				->
			% Reverse the list as the newest will be first otherwise
			notify_proxy(State, lists:reverse(NewMessages)),
			{noreply, State#state{proxy_pid=undefined}}
	end;

handle_cast({register_proxy, Pid}, #state{proxy_pid=Pid, messages=Ms}=State) ->
	case Ms of
		[] ->
			{noreply, State#state{proxy_pid=Pid}};
		_  ->
			notify_proxy(State, []),
			{notify_proxy, State#state{messages=[]}}
	end;

handle_cast(stop, State) ->
	{stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_proxy(#state{proxy_pid=Pid, messages=Ms}=State, Msg) ->
	NewMs = case Msg of
						[] -> Ms;
						_  -> [Msg|Ms]
					end,
	case proxy_pid of
		undefined ->
			State#state{messages=NewMs};
		Pid ->
			Pid ! lists:reverse(NewMs),
			State#state{messages=[]}
	end.