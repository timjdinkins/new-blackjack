-module(casino).
-behavior(gen_server).

-export([start/0, start_link/0, init/1, stop/0, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([seat/1]).

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
seat(Player) ->
	gen_server:call(?MODULE, {seat, Player}).

%% Callbacks
handle_call({seat, Player}, _From, Tables) ->
	case Tables of
		[] ->
			{ok, TablePid} = table:start_link(),
			ok = table:join(TablePid, Player),
			{reply, {table, TablePid}, [TablePid]};
		[TablePid] ->
			ok = table:join(TablePid, Player),
			{reply, {table, TablePid}, [TablePid]}
	end.

handle_cast(stop, LoopData) ->
	{stop, normal, LoopData}.

%% Private API