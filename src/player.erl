-module(player).
-behavior(gen_server).

-record(table, {pid, seat}).
-record(game, {pid, cards=[], bet=0}).
-record(state, {proxy_pid, name, stack=0, table=#table{}, game=#game{}, messages=[]}).

-export([start_link/2, init/1, stop/1, terminate/2]).
-export([handle_cast/2, handle_call/3]).

-export([join_table/1, register_proxy/2, bet/2, stay/1, hit/1, notify/2]).
-export([won/2, lost/2, tied/1]).

start_link(Name, Stack) ->
	io:format("Starting player, with name: ~p~n", [Name]),
	gen_server:start_link(?MODULE, [Name, Stack], []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

init([Name, Stack]) ->
	process_flag(trap_exit, true),
	{ok, #state{name=Name, stack=Stack}}.

terminate(_Reason, _Game) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

join_table(Pid) ->
	gen_server:call(Pid, join_table).

register_proxy(Pid, ProxyPid) ->
	gen_server:cast(Pid, {register_proxy, ProxyPid}).

bet(Pid, Amt) ->
	gen_server:cast(Pid, {bet, Amt}).

hit(Pid) ->
	gen_server:cast(Pid, hit).

stay(Pid) ->
	gen_server:cast(Pid, stay).

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


handle_call(join_table, _From, #state{proxy_pid=Pid, name=Name, stack=Stack, messages=Messages}=State) ->
	case casino:join_table(self(), Name, Stack) of
		{ok, Table, Game, Seat} ->
			{reply, {ok, Seat}, State#state{table=#table{pid=Table, seat=Seat}, game=#game{pid=Game}}};
		{error, _Msg}  ->
			{NPid, Ms} = notify_proxy(Pid, [wh:enc(reg, [{action, join_table}, {status, failed}]) | Messages]),
			{reply, {error, join_failed}, State#state{proxy_pid=NPid, messages=Ms}}
	end.

handle_cast({bet, Amt}, #state{proxy_pid=Pid, game=Game, stack=Stack, messages=Messages}=State) ->
	case Amt + Game#game.bet of
		NewBet when NewBet =< Stack ->
			game:bet(Game#game.pid, {self(), Amt}),
			{noreply, State#state{game=Game#game{bet=NewBet}}};
		_Any ->
			{NPid, Ms} = notify_proxy(Pid, [wh:enc(state, {action, retry_bet}) | Messages]),
			{noreply, State#state{proxy_pid=NPid, messages=Ms}}
	end;

handle_cast(hit, #state{game=Game}=State) ->
	game:hit(Game#game.pid, self()),
	{noreply, State};
	
handle_cast(stay, #state{game=Game}=State) ->
	game:stay(Game#game.pid, self()),
	{noreply, State};

handle_cast({paid, Amt}, #state{game=Game, stack=Stack}=State) ->
	{noreply, State#state{stack=Stack+Amt, game=Game#game{bet=0, cards=[]}}};

handle_cast({lost, Amt}, #state{game=Game, stack=Stack}=State) ->
	{noreply, State#state{stack=Stack-Amt, game=Game#game{bet=0, cards=[]}}};

handle_cast(tie, #state{game=Game}=State) ->
	{noreply, State#state{game=Game#game{bet=0, cards=[]}}};

%%
% Whenever we send a message to the proxy, we remove it's pid.
% After a proxy receives a message, it has to reregister it's pid.
%%
handle_cast({notify, [_H|_T]=Msg}, #state{proxy_pid=Pid, messages=Messages}=State) ->
	{NPid, Ms} = notify_proxy(Pid, Msg ++ Messages),
	{noreply, State#state{proxy_pid=NPid, messages=Ms}};

handle_cast({notify, Msg}, #state{proxy_pid=Pid, messages=Messages}=State) ->
	{NPid, Ms} = notify_proxy(Pid, [Msg|Messages]),
	{noreply, State#state{proxy_pid=NPid, messages=Ms}};

handle_cast({register_proxy, Pid}, #state{proxy_pid=_Pid, messages=Ms}=State) ->
	case Ms of
		[] ->
			{noreply, State#state{proxy_pid=Pid}};
		Ms  ->
			notify_proxy(Pid, Ms),
			{noreply, State#state{proxy_pid=undefined, messages=[]}}
	end;

handle_cast(stop, State) ->
	{stop, normal, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_proxy(Pid, Ms) ->
	case Pid of
		undefined ->
			{Pid, Ms};
		Pid ->
			gen_server:cast(Pid, lists:reverse(Ms)),
			{undefined, []}
	end.