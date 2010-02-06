-module(player).
-behavior(gen_server).

-record(table, {pid, seat}).
-record(game, {pid, cards=[], bet=0}).
-record(state, {proxy_pid, name, stack=0, table=#table{}, game=#game{}, messages=[]}).

-export([start_link/2, init/1, stop/1, terminate/2]).
-export([handle_cast/2]).

-export([join_table/1, register_proxy/2, bet/2, stay/1, hit/1, notify/2]).
-export([won/2, lost/2, tied/1, new_cards/3, busted/3]).

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

register_proxy(Pid, ProxyPid) ->
	gen_server:cast(Pid, {register_proxy, ProxyPid}).

bet(Pid, Amt) ->
	gen_server:cast(Pid, {bet, Amt}).

hit(Pid) ->
	gen_server:cast(Pid, hit).

stay(Pid) ->
	gen_server:cast(Pid, stay).

new_cards(Pid, Cards, Score) ->
	gen_server:cast(Pid, {new_cards, Cards, Score}).

busted(Pid, Cards, Score) ->
	gen_server:cast(Pid, {busted, Cards, Score}).

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


handle_cast(join_table, #state{proxy_pid=Pid, name=Name, messages=Messages}=State) ->
	case casino:join_table(self(), Name) of
		{ok, Table, Game, Seat} ->
			{noreply, State#state{table=#table{pid=Table, seat=Seat}, game=#game{pid=Game}}};
		{error, _Msg}  ->
			Ms = notify_proxy(Pid, [{msg, "Joining the table failed.  Try again later."}|Messages]),
			{noreply, State#state{messages=Ms}}
	end;

handle_cast({bet, Amt}, #state{proxy_pid=Pid, game=Game, stack=Stack, messages=Messages}=State) ->
	case Amt + Game#game.bet of
		NewBet when NewBet =< Stack ->
			game:bet(Game#game.pid, {self(), Amt}),
			{noreply, State#state{game=Game#game{bet=NewBet}}};
		_Any ->
			Ms = notify_proxy(Pid, [{msg, "Your stack is too small for that bet."}|Messages]),
			{noreply, State#state{messages=Ms}}
	end;

handle_cast(hit, #state{game=Game}=State) ->
	game:hit(Game#game.pid, self()),
	{noreply, State};
	
handle_cast(stay, #state{game=Game}=State) ->
	game:stay(Game#game.pid, self()),
	{noreply, State};

handle_cast({new_cards, Cards, Score}, #state{proxy_pid=Pid, game=Game, messages=Messages}=State) ->
	Ms = notify_proxy(Pid, [{new_cards, Cards}, {score, Score}|Messages]),
	{noreply, State#state{game=Game#game{cards=Cards}, messages=Ms}};

handle_cast({busted, Cards, Score}, #state{proxy_pid=Pid, game=Game, messages=Messages}=State) ->
	Ms = notify_proxy(Pid, [{action, busted}, {score, Score}|Messages]),
	{noreply, State#state{game=Game#game{cards=Cards}, messages=Ms}};

handle_cast({paid, Amt}, #state{proxy_pid=Pid, game=Game, stack=Stack, messages=Messages}=State) ->
	Ms = notify_proxy(Pid, [{result, won}, {amt, Amt}, {stack, Stack + Amt}|Messages]),
	{noreply, State#state{stack=Stack+Amt, game=Game#game{bet=0, cards=[]}, messages=Ms}};

handle_cast({lost, Amt}, #state{proxy_pid=Pid, game=Game, stack=Stack, messages=Messages}=State) ->
	Ms = notify_proxy(Pid, [{result, lost}, {amt, Amt}, {stack, Stack - Amt}|Messages]),
	{noreply, State#state{stack=Stack-Amt, game=Game#game{bet=0, cards=[]}, messages=Ms}};

handle_cast(tie, #state{proxy_pid=Pid, game=Game, stack=Stack, messages=Messages}=State) ->
	Ms = notify_proxy(Pid, [{result, tie}, {stack, Stack}|Messages]),
	{noreply, State#state{game=Game#game{bet=0, cards=[]}, messages=Ms}};

%%
% Whenever we send a message to the proxy, we remove it's pid.
% After a proxy receives a message, it has to reregister it's pid.
%%
handle_cast({notify, Msg}, #state{proxy_pid=Pid, messages=Messages}=State) ->
	NMs = case Msg of
					Msg when is_list(Msg) ->
						Msg ++ Messages;
					Msg ->
						[Msg|Messages]
				end,
	Ms = notify_proxy(Pid, NMs),
	{noreply, State#state{messages=Ms}};

handle_cast({register_proxy, Pid}, #state{proxy_pid=_Pid, messages=Ms}=State) ->
	case Ms of
		[] ->
			{noreply, State#state{proxy_pid=Pid}};
		_  ->
			notify_proxy(Pid, State#state.messages),
			{notify_proxy, State#state{proxy_pid=undefined, messages=[]}}
	end;

handle_cast(stop, State) ->
	{stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_proxy(Pid, Ms) ->
	case Pid of
		undefined ->
			Ms;
		Pid ->
			gen_server:cast(Pid, lists:reverse(Ms)),
			[]
	end.