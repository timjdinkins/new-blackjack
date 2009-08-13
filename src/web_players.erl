-module(web_players).

-behavior(gen_server).

-export([start_link/0, stop/0, terminate/2]).
-export([init/1, handle_call/3]).
-export([register/1, get_pid/1]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
	{ok, dict:new()}.

terminate(_Reason, _State) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

register(Name) ->
	gen_server:call(?MODULE, {register, Name}).

get_pid(Name) ->
	gen_server:call(?MODULE, {get_pid, Name}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({register, Name}, _From, State) ->
	case dict:find(Name, State) of
		{ok, _Pid} ->
			{reply, {error, already_registered}, State};
		error ->
			{ok, Pid} = player:start_link(Name, 200),
			NewState = dict:store(Name, Pid, State),
			{reply, {ok, Pid}, NewState}
	end;

handle_call({get_pid, Name}, _From, State) ->
	case dict:find(Name, State) of
		{ok, Pid} ->
			{reply, {ok, Pid}, State};
		error ->
			{reply, {error, name_not_found}, State}
	end;

handle_call(Unknown, _From, State) ->
	{reply, {error, {unkown_message, Unknown}}, State}.