-module(table).
-behavior(gen_fsm).

-record(game, {pid=null, players=[]}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3]).

-export([empty_table/3, playing/2, playing/3]).

-export([seat_player/2, open_seats/1, game_complete/2]).

start() ->
	gen_fsm:start(?MODULE, [], []).
start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

stop(Pid) ->
	gen_fsm:cast(Pid, stop).

init([]) ->
	process_flag(trap_exit, true),
	{ok, GamePid} = game:start_link(),
	{ok, empty_table, #game{pid=GamePid}}.

terminate(_Reason, _StateName, #game{pid=Pid}) ->
	game:stop(Pid),
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

seat_player(Pid, Player) ->
	gen_fsm:sync_send_event(Pid, {seat_player, Player}).

open_seats(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, open_seats).

game_complete(Pid, Quiters) ->
	gen_fsm:send_event(Pid, {game_complete, Quiters}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Player is {Pid, Name}
%%
empty_table({seat_player, {Pid, Name}}, _From, #game{pid=GamePid}=Game) ->
	NewGame = Game#game{players=[{1, Pid, Name}]},
	ok = game:start_hand(GamePid, self(), [{1, Pid, Name}]),
	{reply, {ok, 1, GamePid}, playing, NewGame}.
	
playing({seat_player, {Pid, Name}}, _From, #game{pid=GamePid, players=Players}=Game) ->
	{Seat, Ps} = add_player({Pid, Name}, Players),
	{reply, {ok, Seat, GamePid}, playing, Game#game{players=Ps}}.

playing({game_complete, Quiters}, #game{pid=Pid, players=Players}=Game) ->
	NewPlayers = remove_quiters(Players, Quiters),
	case length(NewPlayers) of
		L when L > 0 ->
			io:format("Number of players at table: ~p~n.", [L]),
			ok = game:start_hand(Pid, self(), NewPlayers),
			{next_state, playing, Game#game{players=NewPlayers}};
		_Else ->
			{next_state, empty_table, Game#game{players=[]}}
	end;

playing(timeout, #game{pid=Pid, players=Players}=Game) ->
	ok = game:start_hand(Pid, self(), Players),
	{next_state, playing, Game}.

handle_event(stop, _StateName, #game{pid=Pid, players=_Players}=Game) ->
	%% Tell the players we stopped this table
	game:stop(Pid),
	%% Tell the casino this table it emptying out.
	casino:close_table(self()),
	{stop, normal, Game}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_quiters(Players, []) ->
	Players;
remove_quiters(Players, [Pid|Quiters]) ->
	remove_quiters(lists:keydelete(Pid, 2, Players), Quiters).

add_player({Pid, Name}, Ps) ->
	[Fst|_] = lists:subtract(lists:seq(1,6), [I || {I,_} <- Ps]),
	{Fst, lists:keysort(1, [{Fst, Pid, Name}|Ps])}.