-module(casino).
-behavior(gen_server).

-export([start_link/0, init/1, stop/0, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([join_table/1, leave_table/1, close_table/1]).

-record(state, {full_tables=[], open_tables=[]}).

%% Public API
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

join_table({Pid, Name}) ->
	gen_server:call(?MODULE, {join_table, Pid, Name}).

leave_table({Table, Player}) ->
	gen_server:call(?MODULE, {leave_table, Table, Player}).

close_table(Pid) ->
	gen_server:call(?MODULE, {close_table, Pid}).

%% Internal API
init([]) ->
	{ok, #state{}, 0}.

terminate(_Reason, _LoopData) ->
	ok.

%% Callbacks
handle_call({join_table, Pid, Name}, _From, #state{open_tables=[T1|OpenTables]}=State) ->
	case join_table({Pid, Name}, T1) of
		{open, Table} ->
			NewState = State#state{open_tables=[Table|OpenTables]};
		{full, Table} ->
			FullTables = State#state.full_tables,
			case OpenTables of
				[] ->
					NewState = State#state{full_tables=[Table|FullTables], open_tables=[new_table()]};
				_  ->
					NewState = State#state{full_tables=[Table|FullTables], open_tables=OpenTables}
			end
	end,
	{TPid, _, _} = Table,
	table:seat_player(TPid, {Pid, Name}),
	{reply, {table, Table}, NewState};

handle_call({leave_table, Table, Player}, _From, #state{open_tables=OpenTables, full_tables=FullTables}=State) ->
	case leave_table(Table, Player, OpenTables) of
		{ok, NewTables} ->
			{reply, ok, #state{open_tables=NewTables}};
		no_match ->
			case leave_table(Table, Player, FullTables) of
				{ok, NewTables} ->
					{reply, ok, #state{full_tables=NewTables}};
				no_match ->
					{reply, {error, no_match}, State}
			end
	end;

handle_call({close_table, TablePid}, _From, #state{open_tables=OpenTables, full_tables=FullTables}=State) ->
	case close_table(TablePid, OpenTables) of
		{ok, []} ->
			{reply, ok, State#state{open_tables=[new_table()]}};
		{ok, NewTables} ->
			{reply, ok, State#state{open_tables=NewTables}};
		no_match ->
			{ok, NewTables} = close_table(TablePid, FullTables),
			{reply, ok, State#state{full_tables=NewTables}}
	end.

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

handle_info(timeout, State) ->
	{noreply, State#state{open_tables=[new_table()]}}.

%% Private API
join_table(Player, {Pid, Cnt, Players}) ->
	NTable = {Pid, Cnt + 1, [Player|Players]},
	case Cnt + 1 of
		I when I > 7 ->
			{full, NTable};
		_ ->
			{open, NTable}
	end.

leave_table(TablePid, Player, Tables) ->
	case lists:keyfind(TablePid, 1, Tables) of
		{Pid, Cnt, Players} ->
			NPlayers = lists:delete(Player, Players),
			{ok, lists:keyreplace(TablePid, 1, Tables, {Pid, Cnt -1, NPlayers})};
		_ ->
			no_match
	end.

close_table(Pid, Tables) ->
	case lists:keymember(Pid, 1, Tables) of
		true -> {ok, lists:keydelete(Pid, 1, Tables)};
		false -> no_match
	end.

new_table() -> 
	{ok, Pid} = table_sup:start_table(),
	{Pid, 0, []}.