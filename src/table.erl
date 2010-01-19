-module(table).
-behavior(gen_fsm).

-record(game, {pid=null, players=[]}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3, handle_sync_event/4]).

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
empty_table({seat_player, Player}, _From, #game{pid=Pid}=Game) ->
	NewGame = Game#game{players=[Player]},
	ok = game:start_hand(Pid, self(), [Player]),
	{reply, {ok, Pid}, playing, NewGame}.
	
playing({seat_player, Player}, _From, #game{pid=Pid, players=Players}=Game) ->
	{reply, {ok, Pid}, playing, Game#game{players=[Player|Players]}}.

playing({game_complete, Quiters}, #game{pid=Pid, players=Players}=Game) ->
	NewPlayers = remove_quiters(Players, Quiters),
	case length(NewPlayers) of
		L when L > 0 ->
			ok = game:start_hand(Pid, self(), NewPlayers),
			{next_state, playing, Game#game{players=NewPlayers}};
		_Else ->
			{next_state, empty_table, Game#game{players=[]}}
	end;

playing(timeout, #game{pid=Pid, players=Players}=Game) ->
	ok = game:start_hand(Pid, self(), Players),
	{next_state, playing, Game}.

handle_sync_event(print_players, _From, StateName, #game{players=Players}=Game) ->
	io:format("~p~n", Players),
	{next_state, StateName, Game}.

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
remove_quiters(Players, [Player|Quiters]) ->
	remove_quiters(lists:delete(Player, Players), Quiters).