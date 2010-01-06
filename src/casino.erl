-module(casino).
-behavior(gen_server).

-export([start/0, start_link/0, init/1, stop/0, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([find_table/0]).

% -record(state, {tables, players, open_tables}).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
	{ok, []}.

terminate(_Reason, _LoopData) ->
	ok.

%% Public API
find_table() ->
	gen_server:call(?MODULE, find_table).

% join_table(PlayerPid, Table, Seat) ->
% 	gen_server:call(?MODULE, {join_table, PlayerPid, Table, Seat}).

% table_from_player(Pid) ->
% 	gen_server:call(?MODULE, table_from_player).

%% Callbacks
handle_call(find_table, _From, Tables) ->
	case Tables of
		[TablePid] ->
			{reply, {table, TablePid}, [Tables]};
		[] ->
			{ok, TablePid} = table:start_link(),
			{reply, {table, TablePid}, [TablePid]}
	end.

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

%% Private API
% first_open_table([]) ->
% 	{error, no_open_tables};
% first_open_table(Tables) ->
% 	{ok, lists:nth(1,lists:sort(Tables))}.
% 
% first_unused_table_name(Tables) ->
% 	first_unused_table_name(1, lists:sort(Tables)).
% 
% first_unused_table_name(N, Tables) ->
% 	TableN = list_to_atom("table" ++ integer_to_list(N)),
% 	case lists:member(TableN, Tables) of
% 		true -> TableN;
% 		false -> first_unused_table_name(N + 1, Tables)
% 	end.
% 
% start_table(Tables) ->
% 	TableNames = ets:match(Tables, {'$1', '_', '_'}),
% 	TableN = first_unused_table_name(lists:flatten(TableNames)),
% 	{ok, Pid} = table:start_link(TableN),
% 	{ok, TableN}.