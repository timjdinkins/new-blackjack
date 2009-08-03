-module(table).
-behavior(gen_fsm).

-record(game, {pid=null, players=dict:new()}).

-export([start/0, start_link/0, init/1, stop/1, terminate/3]).
-export([handle_event/3, handle_sync_event/4]).

-export([empty_table/3, playing/2, playing/3]).

-export([seat/3, open_seats/1, game_complete/3]).

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

seat(Pid, Seat, Player) ->
	gen_fsm:sync_send_event(Pid, {seat_player, Seat, Player}).

open_seats(Pid) ->
	gen_fsm:sync_send_all_state_event(Pid, open_seats).

game_complete(Pid, Players, Quiters) ->
	gen_fsm:send_event(Pid, {game_complete, Players, Quiters}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
empty_table({seat_player, Seat, Player}, _From, #game{pid=Pid, players=Players}=Game) ->
	NewPlayers = dict:store(Seat, Player, Players),
	NewGame = Game#game{players=NewPlayers},
	ok = game:start_hand(Pid, self(), NewPlayers),
	{reply, {ok, Pid}, playing, NewGame}.
	
playing({seat_player, Seat, Player}, _From, #game{pid=Pid, players=Players}=Game) ->
	case seat_available(Seat, Players) of
		true -> 
			NewPlayers = dict:store(Seat, Player, Players),
			{reply, {ok, Pid}, playing, Game#game{players=NewPlayers}};
		false ->
			{reply, {error, seat_taken}, playing, Game}
	end.

playing({game_complete, Players, Quiters}, #game{pid=Pid}=Game) ->
	NewPlayers = remove_quiters(Players, Quiters),
	case length(dict:to_list(NewPlayers)) of
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
	io:format("~p~n", [Players]),
	{next_state, StateName, Game}.

handle_event(stop, _StateName, #game{pid=Pid, players=_Players}=Game) ->
	%% Tell the players we stopped this table
	game:stop(Pid),
	{stop, normal, Game}.

% handle_info({'EXIT', _Pid, _Reason}, _StateName, Game) ->
% 	{ok, GamePid} = game:start_link(),
% 	{next_state, playing, Game#game{pid=GamePid}, 0}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
seat_available(Seat, Players) ->
	lists:member(find_open_seats(Players), Seat).

find_open_seats(Players) ->
	Seats = dict:fetch_keys(Players),
	Possible = [seat1, seat2, seat3, seat4, seat5, seat6],
	lists:subtract(Possible, Seats).

remove_quiters(Players, []) ->
	Players;
remove_quiters(Players, [{SeatN, _Pid}|Quiters]) ->
	remove_quiters(dict:erase(SeatN, Players), Quiters);
remove_quiters(Players, Quiters) ->
	remove_quiters(Players, dict:to_list(Quiters)).