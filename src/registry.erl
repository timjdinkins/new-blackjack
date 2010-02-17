-module(registry).
-behavior(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0, stop/0, terminate/2]).
-export([init/1, handle_call/3]).
-export([register_player/2, get_pid/1]).

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?SERVER, stop).

init([]) ->
	process_flag(trap_exit, true),
	{ok, []}.

terminate(_Reason, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

register_player(SID, Name) ->
	gen_server:call(?SERVER, {register, SID, Name}).

get_pid(SID) ->
	gen_server:call(?SERVER, {get_pid, SID}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({register, SID, Name}, _From, State) ->
	case already_registered(SID, Name, State) of
		yes ->
			{reply, {error, "already_registered"}, State};
		no ->
			{ok, Pid} = player_sup:start_player(Name, 200),
			{reply, {ok, Pid}, [{SID, Name, Pid}|State]}
	end;

handle_call({get_pid, SID}, _From, State) ->
	case lists:keyfind(SID, 1, State) of
		{SID, _Name, Pid} ->
			{reply, {ok, Pid}, State};
		false ->
			{reply, {error, name_not_found}, State}
	end;

handle_call(Unknown, _From, State) ->
	{reply, {error, {unkown_message, Unknown}}, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Private API %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

already_registered(SID, N, Ps) ->
	case lists:keyfind(SID, 1, Ps) of
		{SID, N, _Pid} -> yes;
		false   -> no
	end.