-module(player_proxy).

-behaviour(gen_server).

-record(state, {players, session_id=1}).

-export([start/0, start_link/0, init/1, stop/0, terminate/2]).
-export([handle_call/3, handle_cast/2]).
-export([register/1, listen/2, stop_listening/1, msg_web_client/2, msg_player/2]).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

init([]) ->
	{ok, #state{players=dict:new()}}.

terminate(_Reason, _Any) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%% Public API %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


register(Name) ->
	gen_server:call(?MODULE, {register, Name}).

listen(SID, Pid) ->
	gen_server:cast(?MODULE, {listen, SID, Pid}).

stop_listening(SID) ->
	gen_server:cast(?MODULE, {stop_listening, SID}).

msg_web_client(SID, Msg) ->
	gen_server:cast(?MODULE, {msg_web_client, SID, Msg}).

msg_player(SID, Msg) ->
	gen_server:cast(?MODULE, {msg_player, SID, Msg}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%% Callbacks %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_call({register, Name}, _From, #state{players=Players, session_id=SessionID}=State) ->
		NewSID = SessionID + 1,
		{ok, Pid} = player:start_link(Name, NewSID, 200),
		NewPlayers = dict:store(NewSID, {Pid, Name, [], undefined}, Players),
		{reply, {ok, NewSID}, State#state{players=NewPlayers, session_id=NewSID}};

handle_call(Any, _From, State) ->
	{reply, {error, {unexpected_request, Any}}, State}.

handle_cast({listen, SID, WebPid}, #state{players=Players}=State) ->
	case dict:find(SID, Players) of
		{ok, {PlayerPid, Name, Messages, _OldWebPid}} ->
			NewPlayers = dict:store(SID, {PlayerPid, Name, Messages, WebPid}),
			{noreply, State#state{players=NewPlayers}};
		error ->
			{noreply, State}
	end;

handle_cast({stop_listening, SID}, #state{players=Players}=State) ->
	case dict:find(SID, Players) of
		{ok, {PlayerPid, Name, Messages, _OldWebPid}} ->
			NewPlayers = dict:store(SID, {PlayerPid, Name, Messages, undefined}),
			{noreply, State#state{players=NewPlayers}};
		error ->
			{noreply, State}
	end;

handle_cast({msg_web_client, SID, Msg}, #state{players=Players}=State) ->
	case dict:find(SID, Players) of
		{ok, {PlayerPid, Name, Messages, WebPid}} ->
			case WebPid of
				undefined ->
					NewPlayers = dict:store(SID, {PlayerPid, Name, [Msg|Messages], WebPid}),
					{noreply, State#state{players=NewPlayers}};
				WebPid ->
					WebPid ! {messages, [Msg|Messages]},
					NewPlayers = dict:store(SID, {PlayerPid, Name, [], WebPid}),
					{noreply, State#state{players=NewPlayers}}
			end;
		error ->
			{noreply, State}
	end;

handle_cast({msg_player, SID, Msg}, #state{players=Players}=State) ->
	case dict:find(SID, Players) of
		{ok, {PlayerPid, _Name, _Messages, _WebPid}} ->
			player:msg_from_web_client(PlayerPid, Msg),
			{noreply, State};
		error ->
			{noreply, State}
	end;

handle_cast({notify, Msg}, State) ->
	io:format("~p~n", [Msg]),
	{noreply, State};

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(Any, State) ->
	io:format("Unexpected request ~s~n", [Any]),
	{noreply, State}.